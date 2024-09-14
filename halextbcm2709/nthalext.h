/*++

Copyright (c) 2009  Microsoft Corporation

Module Name:

    nthalext.h

Abstract:

    Header file containing the HAL Extensions API. This file should be included
    by each HAL Extension.

Author:

    Andy Raffman (andyraf) 20-Sep-2010

--*/

//
// -------------------------------------------------------------------- Pragmas
//

#pragma once

//
// ------------------------------------------------------------------- Includes
//

#include <wdm.h>
#include <acpitabl.h>
#include <ntsoc.h>

//
// -------------------------------------------------------- Function Prototypes
//

//
// Functions exported from the HAL Extension to the HAL
//

NTSTATUS
AddResourceGroup (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup
    );

//
// Functions exported from the HAL to the HAL Extension
//

NTSTATUS
RegisterResourceDescriptor (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PCSRT_RESOURCE_DESCRIPTOR_HEADER ResourceDescriptor,
    __in_opt PVOID ResourceDescriptorInfo
    );

PCSRT_RESOURCE_DESCRIPTOR_HEADER
GetNextResourceDescriptor (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup,
    __in PCSRT_RESOURCE_DESCRIPTOR_HEADER ResourceDescriptor,
    __in USHORT ResourceType,
    __in USHORT ResourceSubtype,
    __in ULONG ResourceID
    );

PVOID
GetAcpiTable (
    __in ULONG Handle,
    __in ULONG Signature,
    __in_opt PCSTR OemId,
    __in_opt PCSTR OemTableId
    );

/*++

Routine Description:

    This routine describes the current state an interrupt input and notifies
    the HAL that its routing is fixed and must be preserved.

Arguments:

    ControllerIdentifier - Supplies the interrupt controller identifier
        associated with these lines.

    Line - Stores the input line number to fix.

    State - Stores a pointer to the current state of that line.

Return Value:

    STATUS_SUCCESS on success.

    STATUS_NOT_FOUND if a registered interrupt controller matching that
    identifier could not be found.

--*/

PVOID
HalMapIoSpace (
    __in PHYSICAL_ADDRESS PhysicalAddress,
    __in SIZE_T NumberOfBytes,
    __in MEMORY_CACHING_TYPE CacheType
    );

/*++

Routine Description:

    This routine maps physical memory into the area of virtual memory
    reserved for the HAL.  It does this by directly inserting the PTE
    into the Page Table which the OS Loader has provided.

Arguments:

    PhysicalAddress - Supplies the physical address of the start of the
        area of physical memory to be mapped.

    NumberOfBytes - Supplies the number of bytes that should be mapped.

    CacheType - Supplies the caching attributes of the mapping.

Return Value:

    PVOID - Virtual address at which the requested block of physical memory
            was mapped

    NULL - The requested block of physical memory could not be mapped.

--*/

VOID
HalUnmapIoSpace (
    __in PVOID VirtualAddress,
    __in SIZE_T NumberOfBytes
    );

/*++

Routine Description:

    This routine unmaps a virtual address.

Arguments:

    VirtualAddress  - Supplies a valid virtual address to be unmapped.

    NumberOfBytes - Supplies the number of bytes to unmap.

Return Value:

    None.

--*/

NTSTATUS
HalRegisterPermanentAddressUsage (
    __in PHYSICAL_ADDRESS PhysicalAddress,
    __in ULONG SizeInBytes
    );

/*++

Routine Description:

    This routine permanently stakes out a piece of physical memory for use by
    the SoC module. This routine does not map that memory anywhere, it just
    claims it. This registration cannot be undone.

Arguments:

    PhysicalAddress - Supplies the physical address to register.

    SizeInBytes - Supplies the size of the physical allocation, in byte.

Return Value:

    STATUS_SUCCESS on success.

    STATUS_NO_MEMORY if the allocation could not be reserved.

    STATUS_TOO_LATE if the call is made after the HAL initialization phase is
    complete.

--*/

