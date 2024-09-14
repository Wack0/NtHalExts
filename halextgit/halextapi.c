// HAL Extension API.

#include "halext.h"

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
	__inout PULONG Uid,
	__in PVOID Data
) {
	CSRT_RESOURCE_DESCRIPTOR_HEADER Header;
	Header.Type = Types.Type;
	Header.Subtype = Types.Subtype;
	Header.Length = sizeof(Header);
	Header.Uid = *Uid;
	*Uid += 1;
	return s_Imports->RegisterResourceDescriptor(Handle, ResourceGroup, &Header, Data);
}

#define REGISTER_RESOURCE_DESCRIPTOR(Type, Subtype, Data) do {\
	static ULONG s_Uid = 0;\
	RESOURCE_DESCRIPTOR_TYPES Types = { Type, Subtype };\
	return HepRegisterResourceDescriptor(Handle, ResourceGroup, Types, &s_Uid, Data);\
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

NTSTATUS
HextRegisterInterruptController (
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
HextRegisterInterruptLines (
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

NTSTATUS
HalFixInterruptLine (
    __in PINTERRUPT_LINE Line,
    __in PINTERRUPT_LINE_STATE State
    ) {
	return s_Imports->FixInterruptLine(Line, State);
}

VOID
HalSetInterruptProblem (
    __in PVOID InternalData,
    __in ULONG ProblemCode,
    __in NTSTATUS ProblemStatus
    ) {
	s_Imports->SetInterruptProblem(InternalData, ProblemCode, ProblemStatus);
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