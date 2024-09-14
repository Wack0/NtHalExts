#include "halext.h"

#pragma pack(push)
#pragma pack(1)
typedef struct _EGIT_TABLE
{
	DESCRIPTION_HEADER Header;
	ULONGLONG MemoryMappedPhysicalAddress;
	ULONG SecurePhysicalTimerGsiv;
	ULONG NonSecurePhysicalTimerGsiv;
	ULONG VirtualTimerEventGsiv;
	ULONG HypervisorTimerEventGsiv;
	UCHAR SecurePhysicalTimerFlags;
	UCHAR NonSecurePhysicalTimerFlags;
	UCHAR VirtualTimerEventFlags;
	UCHAR HypervisorTimerEventFlags;
	ULONG GlobalFlags;
} EGIT_TABLE, *PEGIT_TABLE;

typedef struct _GTDT_TABLE
{
	DESCRIPTION_HEADER Header;
	ULONGLONG MemoryMappedPhysicalAddress;
	ULONG GlobalFlags;
	ULONG SecurePhysicalTimerGsiv;
	ULONG SecurePhysicalTimerFlags;
	ULONG NonSecurePhysicalTimerGsiv;
	ULONG NonSecurePhysicalTimerFlags;
	ULONG VirtualTimerEventGsiv;
	ULONG VirtualTimerEventFlags;
	ULONG NonSecurePhysicalTimer2Gsiv;
	ULONG NonSecurePhysicalTimer2Flags;
} GTDT_TABLE, *PGTDT_TABLE;
#pragma pack(pop)

typedef enum _GIT_TIMER_TYPE {
	GitTimerInvalid,
	GitTimerCounter,
	GitTimerInterrupts
} GIT_TIMER_TYPE;

typedef struct _GIT_INTERNAL_DATA
{
  GIT_TIMER_TYPE Type;
  TIMER_MODE Mode;
  ULONGLONG LastCompareValue;
  ULONGLONG PeriodicInterval;
} GIT_INTERNAL_DATA, *PGIT_INTERNAL_DATA;

static ULONG s_GitFrequency = 0;

#define __mcr(coproc, opcode1, value, crn, crm, opcode2) \
	_MoveToCoprocessor(value, coproc, opcode1, crn, crm, opcode2)

#define __mrc _MoveFromCoprocessor

NTSTATUS GitInitialize(PGIT_INTERNAL_DATA TimerData) {
	// CNTKCTL = CNTKCTL_PL0VCTEN
	__mcr(15, 0, 2u, 14, 1, 0);
	if (TimerData->Type == GitTimerInterrupts) {
		// CNTV_CTL = 0
		__mcr(15, 0, 0, 14, 3, 1);
	}
	// Frequency = CNTFRQ
	ULONG Frequency = __mrc(15, 0, 14, 0, 0);
	if (Frequency != s_GitFrequency) return STATUS_DEVICE_NOT_READY;
	return STATUS_SUCCESS;
}

static ULONGLONG GitQueryCounterImpl(void) {
	// return CNTVCT
	return _MoveFromCoprocessor64(15, 1, 14);
}

ULONGLONG GitQueryCounter(PGIT_INTERNAL_DATA TimerData) {
	(void)TimerData;
	return GitQueryCounterImpl();
}

void GitSetTimerCompareValue(ULONGLONG Value) {
	// CNTV_CVAL = Value
	_MoveToCoprocessor64(Value, 15, 3, 14);
}

NTSTATUS GitArmTimer(PGIT_INTERNAL_DATA TimerData, TIMER_MODE Mode, ULONGLONG TickCount) {
	TimerData->PeriodicInterval = TickCount;
	ULONGLONG CompareValue = GitQueryCounterImpl() + TickCount;
	TimerData->LastCompareValue = CompareValue;
	TimerData->Mode = Mode;
	GitSetTimerCompareValue(CompareValue);
	// CNTV_CTL = ENABLE;
	__mcr(15, 0, 1, 14, 3, 1);
	return STATUS_SUCCESS;
}

void GitAcknowledgeInterrupt(PGIT_INTERNAL_DATA TimerData) {
	if (TimerData->Mode != TimerModePseudoPeriodic) return;
	
	ULONGLONG Counter = GitQueryCounterImpl();
	ULONGLONG PeriodicInterval = TimerData->PeriodicInterval;
	ULONGLONG LastCompareValue = TimerData->LastCompareValue;
	ULONGLONG CompareValue = 0;
	if ( ((10 * PeriodicInterval) + LastCompareValue) >= Counter ) {
		CompareValue = LastCompareValue + PeriodicInterval;
	} else {
		CompareValue = PeriodicInterval + Counter;
	}
	TimerData->LastCompareValue = CompareValue;
	GitSetTimerCompareValue(CompareValue);
}

void GitStop(PGIT_INTERNAL_DATA TimerData) {
	// CNTV_CTL = 0;
	__mcr(15, 0, 0, 14, 3, 1);
	TimerData->Mode = TimerModeInvalid;
}

void GitSetInterruptVector(PGIT_INTERNAL_DATA TimerData, ULONG Vector) {
	// No operation. This is only implemented to fix a bug in win8m3 and below.
	(void)TimerData;
	(void)Vector;
}

NTSTATUS
AddResourceGroup (
    __in ULONG Handle,
    __in PCSRT_RESOURCE_GROUP_HEADER ResourceGroup
    ) {
	TIMER_INITIALIZATION_BLOCK NewTimer = {0};
	GIT_INTERNAL_DATA Data = {0};
	ULONG TimerEventGsiv = 0;
	BOOLEAN TimerIsActiveHigh = FALSE;
	KINTERRUPT_MODE TimerInterruptMode = LevelSensitive;
	
	// Try the GTDT table
	PGTDT_TABLE Gtdt = HalSocGetAcpiTable('TDTG');
	if (Gtdt != NULL) {
		TimerEventGsiv = Gtdt->VirtualTimerEventGsiv;
		TimerIsActiveHigh = (Gtdt->VirtualTimerEventFlags & 2) != 0;
		if ((Gtdt->VirtualTimerEventFlags & 1) != 0) TimerInterruptMode = Latched;
	} else {
		PEGIT_TABLE Egit = HalSocGetAcpiTable('TIGE');
		if (Egit == NULL) return STATUS_UNSUCCESSFUL;
		TimerEventGsiv = Egit->VirtualTimerEventGsiv;
		TimerIsActiveHigh = (Egit->VirtualTimerEventFlags & 2) == 0;
		if ((Egit->VirtualTimerEventFlags & 1) != 0) TimerInterruptMode = Latched;
	}
	
	s_GitFrequency = __mrc(15, 0, 14, 0, 0);
	
	BOOLEAN UseAlwaysOn = TRUE;
	NewTimer.Header.TableVersion = 1;
	NewTimer.Header.TableSize = sizeof(NewTimer);
	NewTimer.Capabilities = TIMER_PER_PROCESSOR | TIMER_COUNTER_READABLE; // 8061 sets TIMER_ALWAYS_ON here, 7915 doesn't implement that flag
	// set TIMER_ALWAYS_ON, if we fail, unset and try again
	NewTimer.Capabilities |= 0x8000;
	NewTimer.CounterBitWidth = 64;
	NewTimer.CounterFrequency = s_GitFrequency;
	NewTimer.FunctionTable.Initialize = (PTIMER_INITIALIZE)GitInitialize;
	NewTimer.FunctionTable.QueryCounter = (PTIMER_QUERY_COUNTER)GitQueryCounter;
	NewTimer.InternalData = &Data;
	NewTimer.InternalDataSize = sizeof(Data);
	//NewTimer.KnownType = TimerUnknown; // TimerGit doesn't exist until ~799x
	NewTimer.KnownType = 0xB; // TimerGit
	NewTimer.MaxDivisor = 1;
	NewTimer.Identifier = 0;
	Data.Type = GitTimerCounter;
	NTSTATUS Status = HextRegisterTimer(Handle, ResourceGroup, &NewTimer);
	if (!NT_SUCCESS(Status)) {
		NewTimer.Capabilities &= ~0x8000;
		NewTimer.KnownType = TimerUnknown;
		Status = HextRegisterTimer(Handle, ResourceGroup, &NewTimer);
		if (!NT_SUCCESS(Status)) return Status;
		UseAlwaysOn = FALSE;
	}
	NewTimer.Interrupt.Gsi = TimerEventGsiv;
	NewTimer.Interrupt.Polarity = TimerIsActiveHigh ? InterruptActiveHigh : InterruptActiveLow;
	NewTimer.Interrupt.Mode = TimerInterruptMode;
	NewTimer.Capabilities = TIMER_PER_PROCESSOR | TIMER_PSEUDO_PERIODIC_CAPABLE | TIMER_ONE_SHOT_CAPABLE | TIMER_GENERATES_LINE_BASED_INTERRUPTS;
	NewTimer.FunctionTable.QueryCounter = NULL;
	NewTimer.FunctionTable.ArmTimer = (PTIMER_CORE_TIMER)GitArmTimer;
	NewTimer.FunctionTable.AcknowledgeInterrupt = (PTIMER_ACKNOWLEDGE_INTERRUPT)GitAcknowledgeInterrupt;
	NewTimer.FunctionTable.SetInterruptVector = (PTIMER_SET_INTERRUPT_VECTOR)GitSetInterruptVector;
	NewTimer.FunctionTable.Stop = (PTIMER_STOP)GitStop;
	NewTimer.Identifier = 1;
	Data.Type = GitTimerInterrupts;
	return HextRegisterTimer(Handle, ResourceGroup, &NewTimer);
}