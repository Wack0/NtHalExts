// HAL Extension API.

#include "halext.h"
#include <ntimage.h>

static void GetSecurityCookie(
	__out PULONG *pSecurityCookie,
	__out PULONG *pSecurityCookieComplement
) {
	static ULONG cookie, complement;
	*pSecurityCookie = &cookie;
	*pSecurityCookieComplement = &complement;
}

static OS_EXTENSION_HAL_EXTENSION_EXPORTS s_Exports = {
	AddResourceGroup,
	GetSecurityCookie
};

static POS_EXTENSION_HAL_EXTENSION_IMPORTS s_Imports = NULL;

typedef NTSTATUS (*tfpInterruptRegisterController)(
	PVOID InitialisationBlock,
	PUNICODE_STRING ResourceIdString,
	PVOID* InternalDataPointer
);

typedef void (*tfpSetInterruptProblem)(
	PVOID InternalData,
	ULONG ProblemCode,
	NTSTATUS ProblemStatus
);

typedef NTSTATUS (*tfpInterruptRegisterLine)(
	PINTERRUPT_LINE_DESCRIPTION_M3 Lines
);

typedef enum _HEXT_INTERRUPT_VERSION {
	VERSION_UNKNOWN,
	VERSION_M2, // 789x - 799x, can register interrupt controllers and lines via normal API
	VERSION_M3, // 799x - unknown, need to determine function addresses, uses M3 structures
	VERSION_LATER, // unknown - th1+, need to determine function addresses, uses later structures
} HEXT_INTERRUPT_VERSION;
static HEXT_INTERRUPT_VERSION s_Version = VERSION_UNKNOWN;
static BOOLEAN s_IsLateM2 = FALSE; // 795x - 799x, M2 era but uses the M3 def of INTERRUPT_LINE_DESCRIPTION
static tfpInterruptRegisterController HalpInterruptRegisterController;
static tfpSetInterruptProblem HalpSetInterruptProblem;
static tfpInterruptRegisterLine HalpInterruptRegisterLine;

typedef struct _ARMV7_UNWIND_PDATA {
	ULONG RvaStart;
	union {
		ULONG RvaUnwind;
		struct {
			ULONG Flag : 2;
			ULONG WordLength : 11;
			ULONG Ret : 2;
			ULONG H : 1;
			ULONG Reg : 3;
			ULONG R : 1;
			ULONG L : 1;
			ULONG C : 1;
			ULONG StackAdjust : 10;
		};
	};
} ARMV7_UNWIND_PDATA, *PARMV7_UNWIND_PDATA;

typedef struct _ARMV7_UNWIND_XDATA {
	ULONG WordLength : 18;
	ULONG Vers : 2;
	ULONG X : 1;
	ULONG E : 1;
	ULONG F : 1;
	ULONG EpilogueCount : 5;
	ULONG CodeWords : 4;
	ULONG ExtendedEpilogueCount : 16;
	ULONG ExtendedCodeWords : 8;
	ULONG Reserved : 8;
} ARMV7_UNWIND_XDATA, *PARMV7_UNWIND_XDATA;

static PUCHAR HepPatternMatch( UCHAR* Buffer, ULONG Length, const UCHAR* Pattern, const UCHAR* Mask, ULONG PatternLength) {
	for (ULONG i = 0; i < (Length - PatternLength); i++) {
		BOOLEAN Found = TRUE;
		for (ULONG PatternIdx = 0; PatternIdx < PatternLength; PatternIdx++) {
			UCHAR MaskByte = Mask[PatternIdx];
			UCHAR Expected = Pattern[PatternIdx] & MaskByte;
			UCHAR Calculated = Buffer[i + PatternIdx] & MaskByte;
			if (Expected != Calculated) {
				Found = FALSE;
				break;
			}
		}
		if (Found) return &Buffer[i];
	}
	return NULL;
}

// Boyer-Moore Horspool algorithm adapted from http://www-igm.univ-mlv.fr/~lecroq/string/node18.html#SECTION00180
static PUCHAR HepMemSearch(PUCHAR startPos, const void *pattern, ULONG size, ULONG patternSize)
{
    const UCHAR *patternc = (const UCHAR *)pattern;
    ULONG table[256];

    //Preprocessing
    for (ULONG i = 0; i < 256; i++)
        table[i] = patternSize;
    for (ULONG i = 0; i < patternSize - 1; i++)
        table[patternc[i]] = patternSize - i - 1;

    //Searching
    ULONG j = 0;
    while(j <= size - patternSize)
    {
        UCHAR c = startPos[j + patternSize - 1];
        if(patternc[patternSize - 1] == c && memcmp(pattern, startPos + j, patternSize - 1) == 0)
            return startPos + j;
        j += table[c];
    }

    return NULL;
}

static PUCHAR HepGetFunctionBounds(PVOID AddressInFunction, ULONG BaseAddress, PIMAGE_DATA_DIRECTORY ExceptionDirectory, PULONG FunctionLength) {
	ULONG ExDirLength = ExceptionDirectory->Size;
	ULONG ExDirCount = ExDirLength / sizeof(ARMV7_UNWIND_PDATA);
	ULONG RvaAddress = (ULONG)AddressInFunction - BaseAddress;
	PARMV7_UNWIND_PDATA Pdata = (PARMV7_UNWIND_PDATA)(BaseAddress + ExceptionDirectory->VirtualAddress);
	PARMV7_UNWIND_PDATA FuncPdata = NULL;
	ULONG RvaAddressNext = 0;
	for (ULONG i = 0; i < ExDirCount; i++) {
		// We are looking for the first entry where RvaStart >= RvaAddress.
		if (i != 0 && Pdata[i].RvaStart > RvaAddress) {
			FuncPdata = &Pdata[i - 1];
			RvaAddressNext = Pdata[i].RvaStart;
			break;
		} else if (Pdata[i].RvaStart == RvaAddress) {
			FuncPdata = &Pdata[i];
			if (i != (ExDirCount - 1)) RvaAddressNext = Pdata[i + 1].RvaStart;
			break;
		}
	}
	if (FuncPdata == NULL) return NULL;
	
	// Got it. Make sure.
	ULONG ChunkLength = 0;
	if (FuncPdata->Flag != 0) {
		ChunkLength = FuncPdata->WordLength;
	} else {
		PARMV7_UNWIND_XDATA Xdata = (PARMV7_UNWIND_XDATA)( BaseAddress + FuncPdata->RvaUnwind );
		ChunkLength = Xdata->WordLength;
	}
	ChunkLength *= sizeof(USHORT);
	if (FunctionLength != NULL) *FunctionLength = ChunkLength;
	return (PUCHAR)(BaseAddress + (FuncPdata->RvaStart - 1));
}

static PUSHORT HepFindCall(PUSHORT PointerStart, PVOID PointerEnd) {
	USHORT Insn = 0;
	BOOLEAN Backwards = (PointerEnd < PointerStart);
	PUSHORT Pointer = PointerStart;
	
	if (Backwards) {
		for (; ; Pointer--) {
			Insn = *Pointer;
			if (Pointer == PointerEnd) return NULL;
			if ((Insn & 0xD000) != 0xD000) continue;
			Pointer--;
			Insn = *Pointer;
			if ((Insn & 0xF800) == 0xF000) return Pointer;
		}
	} else {
		for (; ; Pointer ++) {
			Insn = *Pointer;
			if (Pointer == PointerEnd) return NULL;
			if ((Insn & 0xF800) != 0xF000) continue;
			Insn = Pointer[1];
			if ((Insn & 0xD000) == 0xD000) return Pointer;
			Pointer++;
		}
	}
	return NULL;
}

static PVOID HepDecodeCall(PUSHORT Pointer) {
	LONG Sign = (Pointer[0] >> 10) & 1;
	ULONG OffsetU = 0;
	if (Sign != 0) OffsetU = 0xFF800000u;
	OffsetU |= (Pointer[0] & 0x3FF) << 11;
	OffsetU |= (Pointer[1] & 0x7FF);
	ULONG S = (Pointer[0] >> 10) & 1;
	ULONG I1 = (Pointer[1] >> 13) & 1;
	ULONG I2 = (Pointer[1] >> 11) & 1;
	I1 = (~(I1 ^ S)) & 1;
	I2 = (~(I2 ^ S)) & 1;
	OffsetU |= (I2 << 21) | (I1 << 22);
	return (PVOID)((ULONG)&Pointer[2] + (OffsetU * 2));
}

static NTSTATUS HepFindInterruptApis(BOOLEAN ForM2) {
	// go grab a function in hal
	ULONG HalBase = (ULONG)s_Imports->RegisterResourceDescriptor & ~0xfff;
	// walk memory until we find base address
	PIMAGE_DOS_HEADER Mz = NULL;
	PIMAGE_NT_HEADERS Pe = NULL;
	PIMAGE_SECTION_HEADER Sections = NULL;
	for (; Mz == NULL; HalBase -= 0x1000) {
		while (*(PUSHORT)HalBase != IMAGE_DOS_SIGNATURE) { HalBase -= 0x1000; }
		Mz = (PIMAGE_DOS_HEADER)HalBase;
		Pe = (PIMAGE_NT_HEADERS)(HalBase + Mz->e_lfanew);
		if (Mz->e_lfanew >= 0x1000 || Pe->Signature != IMAGE_NT_SIGNATURE) {
			Mz = NULL;
			continue;
		}
		Sections = (PIMAGE_SECTION_HEADER)(
			(ULONG)&Pe->OptionalHeader + Pe->FileHeader.SizeOfOptionalHeader
		);
		if (Pe->FileHeader.SizeOfOptionalHeader >= (0x1000 - Mz->e_lfanew)) {
			Mz = NULL;
			continue;
		}
		break;
	}
	
	// Assumption: first section is .text, double check
	if (Pe->FileHeader.NumberOfSections == 0 || Sections[0].Characteristics != 0x68000020) {
		KeBugCheckEx(0x5C, 0x1337, 0, Pe->FileHeader.NumberOfSections, Sections[0].Characteristics);
		return STATUS_UNSUCCESSFUL;
	}
	
	PIMAGE_DATA_DIRECTORY ExceptionDirectory = &Pe->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXCEPTION];
	if (ExceptionDirectory->VirtualAddress == 0 || ExceptionDirectory->Size == 0) {
		
		KeBugCheckEx(0x5C, 0x1337, 1, ExceptionDirectory->VirtualAddress, ExceptionDirectory->Size);
		return STATUS_UNSUCCESSFUL;
	}
	
	PUCHAR TextBase = (PUCHAR)(HalBase + Sections[0].VirtualAddress);
	ULONG TextSize = Sections[0].Misc.VirtualSize;
	
	if (ForM2) {
		// Find HalpInterruptRegisterLine by NTSTATUS code.
		// This is the only match for this NTSTATUS for the entire period.
		static UCHAR s_PatternBytes[] = { 0x82, 0x02, 0x00, 0xc0 };
		PUCHAR cpHalpInterruptRegisterLine = (PUCHAR)HepMemSearch(TextBase, s_PatternBytes, TextSize, sizeof(s_PatternBytes));
		if (cpHalpInterruptRegisterLine == NULL) {
			KeBugCheckEx(0x5C, 0x31337, 0, (ULONG)TextBase, TextSize);
			return STATUS_UNSUCCESSFUL;
		}
		// Walk back the size of the entire const pool, and find the start of the function from there.
		cpHalpInterruptRegisterLine -= 0x20;
		ULONG FunctionLength = 0;
		PUCHAR HalpInterruptRegisterLine = HepGetFunctionBounds(cpHalpInterruptRegisterLine, HalBase, ExceptionDirectory, &FunctionLength);
		if (HalpInterruptRegisterLine == NULL) {
			KeBugCheckEx(0x5C, 0x31337, 1, (ULONG)cpHalpInterruptRegisterLine - HalBase, HalBase);
			return STATUS_UNSUCCESSFUL;
		}
		// MS compiler did assign same registers for this for the entire period we are looking at.
		// That makes this easier, just memmem for the pattern.
		UCHAR s_PatternNew[] = { 0xD6, 0xF8, 0x14, 0x80, 0xB8, 0xF1, 0xFF, 0x3F };
		if (HepMemSearch(HalpInterruptRegisterLine, s_PatternNew, FunctionLength, sizeof(s_PatternNew)) != NULL) {
			// Found it, we must be new style
			s_IsLateM2 = TRUE;
			return STATUS_SUCCESS;
		}
		// Double check, search for old style
		s_PatternNew[2] = 0x18;
		if (HepMemSearch(HalpInterruptRegisterLine, s_PatternNew, FunctionLength, sizeof(s_PatternNew)) != NULL) {
			// Found it, we must be old style
			return STATUS_SUCCESS;
		}
		// Didn't find old or new style, just bugcheck
		KeBugCheckEx(0x5C, 0x31337, 2, (ULONG)HalpInterruptRegisterLine - HalBase, FunctionLength);
		return STATUS_UNSUCCESSFUL;
	}
	
	// Find HalpInterruptRegisterController. Get version at the same time.
	HEXT_INTERRUPT_VERSION Version = VERSION_UNKNOWN;
	{
	static UCHAR s_PatternMask[] = { 0xFF, 0x00, 0xFF, 0x00, 0xFF };
	static UCHAR s_Lengths[] = { sizeof(INTERRUPT_INITIALIZATION_BLOCK_M3), sizeof(INTERRUPT_INITIALIZATION_BLOCK_LATER) };
	UCHAR PatternBytes[] = { 0, 0, 0x01, 0, 0x68 };
	_Static_assert(sizeof(PatternBytes) == sizeof(s_PatternMask), "patterns not same length");
	PVOID FunctionData = NULL;
	for (ULONG i = 0; i < sizeof(s_Lengths); i++) {
		PatternBytes[0] = s_Lengths[i];
		FunctionData = HepPatternMatch(TextBase, TextSize, PatternBytes, s_PatternMask, sizeof(PatternBytes));
		if (FunctionData != NULL) {
			Version = i + VERSION_M3;
			break;
		}
	}
	if (FunctionData == NULL) {
		KeBugCheckEx(0x5C, 0x1337, 2, (ULONG)TextBase, TextSize);
		return STATUS_UNSUCCESSFUL;
	}
	// We are somewhere in HalpGicRegisterIoUnit. Get the function bounds.
	ULONG FunctionLength = 0;
	PUCHAR HalpGicRegisterIoUnit = HepGetFunctionBounds(FunctionData, HalBase, ExceptionDirectory, &FunctionLength);
	if (HalpGicRegisterIoUnit == NULL) {
		KeBugCheckEx(0x5C, 0x1337, 3, (ULONG)FunctionData - HalBase, HalBase);
		return STATUS_UNSUCCESSFUL;
	}
	
	PUCHAR HalpGicRegisterIoUnitEnd = &HalpGicRegisterIoUnit[FunctionLength - 2];
	// Find last bl
	PUSHORT CallRegisterInterruptController = (PUSHORT)HalpGicRegisterIoUnitEnd;
	CallRegisterInterruptController = HepFindCall(CallRegisterInterruptController, HalpGicRegisterIoUnit);
	// bl __security_check_cookie or __security_pop_cookie
	CallRegisterInterruptController --;
	CallRegisterInterruptController = HepFindCall(CallRegisterInterruptController, HalpGicRegisterIoUnit);
	// We are now at bl HalpInterruptRegisterController.
	ULONG pHalpInterruptRegisterController = (ULONG)HepDecodeCall(CallRegisterInterruptController);
	HalpInterruptRegisterController = (tfpInterruptRegisterController)(pHalpInterruptRegisterController | 1);
	}
	
	// Find HalSetInterruptProblem.
	{
	static UCHAR s_PatternBytes[] = { 0x00, 0x22, 0x04, 0x21 };
	PUSHORT FunctionData = (PUSHORT)HepMemSearch(TextBase, s_PatternBytes, TextSize, sizeof(s_PatternBytes));
	if (FunctionData == NULL) {
		KeBugCheckEx(0x5C, 0x1337, 4, (ULONG)TextBase, TextSize);
		return STATUS_UNSUCCESSFUL;
	}
	// next bl, somewhere in the region of 10 instructions away
	PUSHORT CallSetInterruptProblem = HepFindCall(FunctionData, &FunctionData[10]);
	if (CallSetInterruptProblem == NULL) {
		KeBugCheckEx(0x5C, 0x1337, 5, (ULONG)FunctionData - HalBase, HalBase);
		return STATUS_UNSUCCESSFUL;
	}
	ULONG pHalSetInterruptProblem = (ULONG)HepDecodeCall(CallSetInterruptProblem);
	// bugcheck for analysing the pattern match result
	//KeBugCheckEx(0x1337, (ULONG)CallSetInterruptProblem - HalBase, (ULONG)pHalSetInterruptProblem - HalBase, HalBase, 0);
	HalpSetInterruptProblem = (tfpSetInterruptProblem)(pHalSetInterruptProblem | 1);
	}
	
	// Find HalpInterruptRegisterLine.
	{
	static UCHAR s_PatternBytes[] = { 0xf0, 0x00, 0xd0, 0x06, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02 };
	static UCHAR s_PatternMask[] =  { 0xf8, 0x00, 0xd0, 0xff, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0xff };
	_Static_assert(sizeof(s_PatternBytes) == sizeof(s_PatternMask), "patterns not same length");
	PUCHAR FunctionData = (PUCHAR)HepPatternMatch(TextBase, TextSize, s_PatternBytes, s_PatternMask, sizeof(s_PatternBytes));
	if (FunctionData == NULL) {
		KeBugCheckEx(0x5C, 0x1337, 6, (ULONG)TextBase, TextSize);
		return STATUS_UNSUCCESSFUL;
	}
	FunctionData += 3;
	// next bl, about 30 instructions away
	PUSHORT Call = HepFindCall((PUSHORT)FunctionData, &FunctionData[30 * 2]);
	if (Call == NULL) {
		KeBugCheckEx(0x5C, 0x1337, 7, (ULONG)FunctionData - HalBase, HalBase);
		return STATUS_UNSUCCESSFUL;
	}
	ULONG pHalpInterruptRegisterLine = (ULONG)HepDecodeCall(Call);
	// bugcheck for analysing the pattern match result
	//KeBugCheckEx(0x1337, (ULONG)Call - HalBase, (ULONG)pHalpInterruptRegisterLine - HalBase, HalBase, 0);
	HalpInterruptRegisterLine = (tfpInterruptRegisterLine)(pHalpInterruptRegisterLine | 1);
	}
	
#if 0 // bugcheck for analysing all discovered variables
	KeBugCheckEx(0x1337, Version,
		(ULONG)HalpInterruptRegisterController - HalBase,
		(ULONG)HalpSetInterruptProblem - HalBase,
		(ULONG)HalpInterruptRegisterLine - HalBase
	);
#endif
	// All done, set the version on the way out.
	s_Version = Version;
	return STATUS_SUCCESS;
}

NTSTATUS HalExtensionInit(
	__in OS_EXTENSION_INTERFACE Interface,
	__out POS_EXTENSION_HAL_EXTENSION_EXPORTS* Exports,
	__in POS_EXTENSION_HAL_EXTENSION_IMPORTS Imports
) {
	if (Interface != OsExtensionHalExtension) return STATUS_NOINTERFACE;
	*Exports = &s_Exports;
	s_Imports = Imports;
	return STATUS_SUCCESS;
}

typedef struct {
	USHORT Type, Subtype;
} RESOURCE_DESCRIPTOR_TYPES;

static NTSTATUS
HepRegisterResourceDescriptor(
	__in ULONG Handle,
	__in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
	__in RESOURCE_DESCRIPTOR_TYPES Types,
	__in PVOID Data
) {
	static ULONG s_Uid = 0;
	CSRT_RESOURCE_DESCRIPTOR_HEADER Header;
	Header.Type = Types.Type;
	Header.Subtype = Types.Subtype;
	Header.Length = sizeof(Header);
	Header.Uid = s_Uid;
	s_Uid += 1;
	NTSTATUS Status = s_Imports->RegisterResourceDescriptor(Handle, ResourceGroup, &Header, Data);
	// if this function fails then it leaves the uid resource in place and that uid can't be used again
	//if (!NT_SUCCESS(Status)) s_Uid -= 1;
	return Status;
}

#define REGISTER_RESOURCE_DESCRIPTOR(Type, Subtype, Data) do {\
	RESOURCE_DESCRIPTOR_TYPES Types = { Type, Subtype };\
	return HepRegisterResourceDescriptor(Handle, ResourceGroup, Types, Data);\
} while (0)

NTSTATUS
HextRegisterTimer (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PTIMER_INITIALIZATION_BLOCK NewTimer
) {
	REGISTER_RESOURCE_DESCRIPTOR(
		CSRT_RD_TYPE_TIMER,
		CSRT_RD_SUBTYPE_TIMER,
		NewTimer
	);
}

NTSTATUS
HalUpdateTimerCapabilities (
    __in PVOID TimerData,
    __in ULONG SetNewCapabilities,
    __in ULONG ClearNewCapabilities
    ) {
	return s_Imports->UpdateTimerCapabilities(TimerData, SetNewCapabilities, ClearNewCapabilities);
}

static NTSTATUS HepRegisterInterruptControllerM2(
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PINTERRUPT_INITIALIZATION_BLOCK NewController
    ) {
	REGISTER_RESOURCE_DESCRIPTOR(
		CSRT_RD_TYPE_INTERRUPT,
		CSRT_RD_SUBTYPE_INTERRUPT_CONTROLLER,
		NewController
	);
}

NTSTATUS
HextRegisterInterruptController (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PINTERRUPT_INITIALIZATION_BLOCK NewController
) {
	NTSTATUS Status;
	INTERRUPT_INITIALIZATION_BLOCK_UNION NewApi;
	switch (s_Version) {
	case VERSION_M2:
		return HepRegisterInterruptControllerM2(Handle, ResourceGroup, NewController);
	case VERSION_M3:
		// Create the M3 structure.
		memset(&NewApi, 0, sizeof(NewApi));
		NewApi.M3.Header = NewController->Header;
		NewApi.M3.Header.TableSize = sizeof(NewApi.M3);
		NewApi.M3.Footer = ((PINTERRUPT_INITIALIZATION_BLOCK_M2)NewController)->Footer;
		NewApi.M3.FunctionTable.Base = NewController->FunctionTable;
		// Pass it to HalpInterruptRegisterController.
		return HalpInterruptRegisterController(&NewApi.M3, NULL, NULL);
	case VERSION_LATER:
		// Create the later structure.
		memset(&NewApi, 0, sizeof(NewApi));
		NewApi.Later.Header = NewController->Header;
		NewApi.Later.Header.TableSize = sizeof(NewApi.Later);
		NewApi.Later.Footer = ((PINTERRUPT_INITIALIZATION_BLOCK_M2)NewController)->Footer;
		NewApi.Later.FunctionTable.Base = NewController->FunctionTable;
		// Pass it to HalpInterruptRegisterController.
		return HalpInterruptRegisterController(&NewApi.M3, NULL, NULL);
	case VERSION_UNKNOWN:
		Status = HepRegisterInterruptControllerM2(Handle, ResourceGroup, NewController);
		if (NT_SUCCESS(Status)) {
			s_Version = VERSION_M2;
			return HepFindInterruptApis(TRUE);
		}
		// Not M2, we need to find the HALSoC interrupt entry points
		Status = HepFindInterruptApis(FALSE);
		if (!NT_SUCCESS(Status)) return Status;
		// Found the entry points and the correct version.
		if (s_Version == VERSION_UNKNOWN) return STATUS_UNSUCCESSFUL;
		return HextRegisterInterruptController(Handle, ResourceGroup, NewController);
	}
	return STATUS_UNSUCCESSFUL;
}

static NTSTATUS
HepRegisterInterruptLinesM2Impl (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PINTERRUPT_LINE_DESCRIPTION Lines
    ) {
	REGISTER_RESOURCE_DESCRIPTOR(
		CSRT_RD_TYPE_INTERRUPT,
		CSRT_RD_SUBTYPE_INTERRUPT_LINES,
		Lines
	);
}

static NTSTATUS
HepRegisterInterruptLinesM2 (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PINTERRUPT_LINE_DESCRIPTION Lines
    ) {
	INTERRUPT_LINE_DESCRIPTION_M3 M3;
	if (s_IsLateM2) {
		// Function got changed to M3-style in late M2
		// Set up same structure as M3.
		M3.ControllerIdentifier = Lines->ControllerIdentifier;
		M3.MinLine = Lines->MinLine;
		M3.MaxLine = Lines->MaxLine;
		M3.Type = Lines->Type;
		M3.OutputUnitId = Lines->OutputUnitId;
		M3.GsiBase = Lines->GsiBase;
		Lines = (PINTERRUPT_LINE_DESCRIPTION)&M3;
	}
	return HepRegisterInterruptLinesM2Impl(Handle, ResourceGroup, Lines);
}

NTSTATUS
HextRegisterInterruptLines (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PINTERRUPT_LINE_DESCRIPTION Lines
    ) {
	if (s_Version == VERSION_M2) return HepRegisterInterruptLinesM2(Handle, ResourceGroup, Lines);
	if (s_Version == VERSION_UNKNOWN) return STATUS_UNSUCCESSFUL;
	INTERRUPT_LINE_DESCRIPTION_M3 M3;
	M3.ControllerIdentifier = Lines->ControllerIdentifier;
	M3.MinLine = Lines->MinLine;
	M3.MaxLine = Lines->MaxLine;
	M3.Type = Lines->Type;
	M3.OutputUnitId = Lines->OutputUnitId;
	M3.GsiBase = Lines->GsiBase;
	return HalpInterruptRegisterLine(&M3);
}

NTSTATUS
HalFixInterruptLine (
    __in PINTERRUPT_LINE Line,
    __in PINTERRUPT_LINE_STATE State
    ) {
	if (s_Version != VERSION_UNKNOWN) return STATUS_UNSUCCESSFUL;
	return s_Imports->FixInterruptLine(Line, State);
}

VOID
HalSetInterruptProblem (
    __in PVOID InternalData,
    __in ULONG ProblemCode,
    __in NTSTATUS ProblemStatus
    ) {
	if (s_Version == VERSION_UNKNOWN) return;
	if (s_Version == VERSION_M2)
		s_Imports->SetInterruptProblem(InternalData, ProblemCode, ProblemStatus);
	else HalpSetInterruptProblem(InternalData, ProblemCode, ProblemStatus);
}

NTSTATUS
HextRegisterDmaController(
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PDMA_INITIALIZATION_BLOCK ControllerDescription
    ) {
	REGISTER_RESOURCE_DESCRIPTOR(
		CSRT_RD_TYPE_DMA,
		CSRT_RD_SUBTYPE_DMA_CONTROLLER,
		ControllerDescription
	);
}

NTSTATUS
HextRegisterDmaChannel(
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PDMA_CHANNEL_INITIALIZATION_BLOCK ChannelDescription
    ) {
	REGISTER_RESOURCE_DESCRIPTOR(
		CSRT_RD_TYPE_DMA,
		CSRT_RD_SUBTYPE_DMA_CHANNEL,
		ChannelDescription
	);
}

PVOID
HalMapIoSpace (
    __in PHYSICAL_ADDRESS PhysicalAddress,
    __in SIZE_T NumberOfBytes,
    __in MEMORY_CACHING_TYPE CacheType
    ) {
	return s_Imports->HalMapIoSpace(PhysicalAddress, NumberOfBytes, CacheType);
}

VOID
HalUnmapIoSpace (
    __in PVOID VirtualAddress,
    __in SIZE_T NumberOfBytes
    ) {
	s_Imports->HalUnmapIoSpace(VirtualAddress, NumberOfBytes);
}

NTSTATUS
HalRegisterPermanentAddressUsage (
    __in PHYSICAL_ADDRESS PhysicalAddress,
    __in ULONG SizeInBytes
    ) {
	return s_Imports->HalRegisterPermanentAddressUsage(PhysicalAddress, SizeInBytes);
}

PVOID
HalSocGetAcpiTable (
    __in ULONG Signature
    ) {
	// For GetAcpiTable, OemId and OemTableId are optional and Handle is unused,
	// so:
	return s_Imports->GetAcpiTable(0, Signature, NULL, NULL);
}

NTSTATUS
RegisterResourceDescriptor (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PCSRT_RESOURCE_DESCRIPTOR_HEADER ResourceDescriptor,
    __in_opt PVOID ResourceDescriptorInfo
    ) {
	return s_Imports->RegisterResourceDescriptor(Handle, ResourceGroup, ResourceDescriptor, ResourceDescriptorInfo);
}

PCSRT_RESOURCE_DESCRIPTOR_HEADER
GetNextResourceDescriptor (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PCSRT_RESOURCE_DESCRIPTOR_HEADER ResourceDescriptor,
    __in USHORT ResourceType,
    __in USHORT ResourceSubtype,
    __in ULONG ResourceID
    ) {
	return s_Imports->GetNextResourceDescriptor(
		Handle,
		ResourceGroup,
		ResourceDescriptor,
		ResourceType,
		ResourceSubtype,
		ResourceID
	);
}

PVOID
GetAcpiTable (
    __in ULONG Handle,
    __in ULONG Signature,
    __in_opt PCSTR OemId,
    __in_opt PCSTR OemTableId
    ) {
	return s_Imports->GetAcpiTable(
		Handle,
		Signature,
		OemId,
		OemTableId
	);
}

VOID
HalSetTimerProblem (
    __in PVOID TimerData,
    __in ULONG ProblemCode,
    __in NTSTATUS ProblemStatus
    ) {
	s_Imports->SetTimerProblem(TimerData, ProblemCode, ProblemStatus);
}

VOID
RtlRaiseException (
    __in PEXCEPTION_RECORD ExceptionRecord
    ) {
	s_Imports->RtlRaiseException(ExceptionRecord);
}

BOOLEAN HextVersionIsM2(void) { return s_Version == VERSION_M2; }