// HAL Extension header
#pragma once

#include "nthalext.h"

// Interrupt function table changed many times, so:
typedef NTSTATUS (*tfpDeinitializeLocalUnit)(PVOID Data);
typedef NTSTATUS (*tfpDeinitializeIoUnit)(PVOID Data);
typedef PVOID tfpQueryAndGetSource;
typedef struct _INTERRUPT_FUNCTION_TABLE_M3 {
	INTERRUPT_FUNCTION_TABLE Base;
	tfpDeinitializeLocalUnit DeinitializeLocalUnit;
	tfpDeinitializeIoUnit DeinitializeIoUnit;
} INTERRUPT_FUNCTION_TABLE_M3, *PINTERRUPT_FUNCTION_TABLE_M3;

typedef struct _INTERRUPT_FUNCTION_TABLE_LATER {
	INTERRUPT_FUNCTION_TABLE Base;
	tfpDeinitializeLocalUnit DeinitializeLocalUnit;
	tfpDeinitializeIoUnit DeinitializeIoUnit;
	tfpQueryAndGetSource QueryAndGetSource;
} INTERRUPT_FUNCTION_TABLE_LATER, *PINTERRUPT_FUNCTION_TABLE_LATER;

typedef struct _INTERRUPT_INITIALIZATION_BLOCK_FOOTER {
    PVOID InternalData;
    ULONG InternalDataSize;
    KNOWN_CONTROLLER_TYPE KnownType;
    ULONG Identifier;
    ULONG Capabilities;
    ULONG MaxPriority;
    ULONG MaxClusterSize;
    ULONG MaxClusters;
    ULONG InterruptReplayDataSize;
} INTERRUPT_INITIALIZATION_BLOCK_FOOTER, *PINTERRUPT_INITIALIZATION_BLOCK_FOOTER;

typedef struct _INTERRUPT_INITIALIZATION_BLOCK_M2 {
    SOC_INITIALIZATION_HEADER Header;
    INTERRUPT_FUNCTION_TABLE FunctionTable;
    INTERRUPT_INITIALIZATION_BLOCK_FOOTER Footer;
} INTERRUPT_INITIALIZATION_BLOCK_M2, *PINTERRUPT_INITIALIZATION_BLOCK_M2;
_Static_assert(sizeof(INTERRUPT_INITIALIZATION_BLOCK_M2) == sizeof(INTERRUPT_INITIALIZATION_BLOCK), "bad M2 interrupt initialization block");

typedef struct _INTERRUPT_INITIALIZATION_BLOCK_M3 {
    SOC_INITIALIZATION_HEADER Header;
    INTERRUPT_FUNCTION_TABLE_M3 FunctionTable;
    INTERRUPT_INITIALIZATION_BLOCK_FOOTER Footer;
} INTERRUPT_INITIALIZATION_BLOCK_M3, *PINTERRUPT_INITIALIZATION_BLOCK_M3;

typedef struct _INTERRUPT_INITIALIZATION_BLOCK_LATER {
    SOC_INITIALIZATION_HEADER Header;
    INTERRUPT_FUNCTION_TABLE_LATER FunctionTable;
    INTERRUPT_INITIALIZATION_BLOCK_FOOTER Footer;
} INTERRUPT_INITIALIZATION_BLOCK_LATER, *PINTERRUPT_INITIALIZATION_BLOCK_LATER;

typedef union _INTERRUPT_INITIALIZATION_BLOCK_UNION {
	INTERRUPT_INITIALIZATION_BLOCK_M3 M3;
	INTERRUPT_INITIALIZATION_BLOCK_LATER Later;
} INTERRUPT_INITIALIZATION_BLOCK_UNION, *PINTERRUPT_INITIALIZATION_BLOCK_UNION;

typedef struct _INTERRUPT_LINE_DESCRIPTION_M3 {
    ULONG ControllerIdentifier;
    LONG MinLine;
    LONG MaxLine;
    INTERRUPT_LINE_TYPE Type;
    ULONG OutputUnitId;
    ULONG GsiBase;
} INTERRUPT_LINE_DESCRIPTION_M3, *PINTERRUPT_LINE_DESCRIPTION_M3;

// Interface
typedef enum _OS_EXTENSION_INTERFACE {
	OsExtensionHalExtension = 0
} OS_EXTENSION_INTERFACE;

// Functions
typedef NTSTATUS
(*tfpAddResourceGroup)(
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup
    );

typedef void
(*tfpGetSecurityCookie)(
	__out PULONG *pSecurityCookie,
	__out PULONG *pSecurityCookieComplement
);

#define HALEXT_API(name) tfp##name name

typedef struct _OS_EXTENSION_HAL_EXTENSION_EXPORTS
{
  HALEXT_API(AddResourceGroup);
  HALEXT_API(GetSecurityCookie);
} OS_EXTENSION_HAL_EXTENSION_EXPORTS, *POS_EXTENSION_HAL_EXTENSION_EXPORTS;

typedef NTSTATUS
(*tfpRegisterResourceDescriptor) (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PCSRT_RESOURCE_DESCRIPTOR_HEADER ResourceDescriptor,
    __in_opt PVOID ResourceDescriptorInfo
    );

typedef PCSRT_RESOURCE_DESCRIPTOR_HEADER
(*tfpGetNextResourceDescriptor) (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PCSRT_RESOURCE_DESCRIPTOR_HEADER ResourceDescriptor,
    __in USHORT ResourceType,
    __in USHORT ResourceSubtype,
    __in ULONG ResourceID
    );

typedef PVOID
(*tfpGetAcpiTable) (
    __in ULONG Handle,
    __in ULONG Signature,
    __in_opt PCSTR OemId,
    __in_opt PCSTR OemTableId
    );

typedef PVOID
(*tfpHalMapIoSpace) (
    __in PHYSICAL_ADDRESS PhysicalAddress,
    __in SIZE_T NumberOfBytes,
    __in MEMORY_CACHING_TYPE CacheType
    );

typedef VOID
(*tfpHalUnmapIoSpace) (
    __in PVOID VirtualAddress,
    __in SIZE_T NumberOfBytes
    );

typedef NTSTATUS
(*tfpHalRegisterPermanentAddressUsage) (
    __in PHYSICAL_ADDRESS PhysicalAddress,
    __in ULONG SizeInBytes
    );

typedef VOID
(*tfpSetTimerProblem) (
    __in PVOID TimerData,
    __in ULONG ProblemCode,
    __in NTSTATUS ProblemStatus
    );

typedef VOID
(*tfpSetInterruptProblem) (
    __in PVOID InternalData,
    __in ULONG ProblemCode,
    __in NTSTATUS ProblemStatus
    );

typedef NTSTATUS
(*tfpFixInterruptLine) (
    __in PINTERRUPT_LINE Line,
    __in PINTERRUPT_LINE_STATE State
    );

typedef NTSTATUS
(*tfpUpdateTimerCapabilities) (
    __in PVOID TimerData,
    __in ULONG SetNewCapabilities,
    __in ULONG ClearNewCapabilities
    );

typedef VOID
(*tfpRtlRaiseException) (
    __in PEXCEPTION_RECORD ExceptionRecord
    );


typedef struct _OS_EXTENSION_HAL_EXTENSION_IMPORTS {
	HALEXT_API(RegisterResourceDescriptor);
	HALEXT_API(GetNextResourceDescriptor);
	HALEXT_API(GetAcpiTable);
	HALEXT_API(HalMapIoSpace);
	HALEXT_API(HalUnmapIoSpace);
	HALEXT_API(HalRegisterPermanentAddressUsage);
	HALEXT_API(SetTimerProblem);
	HALEXT_API(SetInterruptProblem);
	HALEXT_API(FixInterruptLine);
	HALEXT_API(UpdateTimerCapabilities);
	HALEXT_API(RtlRaiseException);
} OS_EXTENSION_HAL_EXTENSION_IMPORTS, *POS_EXTENSION_HAL_EXTENSION_IMPORTS;

NTSTATUS HalExtensionInit(
	__in OS_EXTENSION_INTERFACE Interface,
	__out POS_EXTENSION_HAL_EXTENSION_EXPORTS* Exports,
	__in POS_EXTENSION_HAL_EXTENSION_IMPORTS Imports
);

NTSTATUS
HextRegisterTimer (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PTIMER_INITIALIZATION_BLOCK NewTimer
    );

NTSTATUS
HextRegisterInterruptController (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PINTERRUPT_INITIALIZATION_BLOCK NewController
    );

NTSTATUS
HextRegisterInterruptLines (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PINTERRUPT_LINE_DESCRIPTION Lines
    );

NTSTATUS
HextRegisterDmaController(
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PDMA_INITIALIZATION_BLOCK ControllerDescription
    );

NTSTATUS
HextRegisterDmaChannel(
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PDMA_CHANNEL_INITIALIZATION_BLOCK ChannelDescription
    );

BOOLEAN HextVersionIsM2(void);