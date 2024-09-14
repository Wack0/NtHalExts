/*++

Copyright (c) 2009  Microsoft Corporation

Module Name:

    ntsoc.h

Abstract:

    Header file containing the HAL System on a Chip (SoC) API.

Author:

    Evan Green (evgreen) 29-Jul-2009

--*/

//
// -------------------------------------------------------------------- Pragmas
//

#pragma once

//
// Disable warnings of features used by the standard headers
//
// Disable warning C4214: nonstandard extension used : bit field types other
// than int.
// Disable warning C4201: nonstandard extension used : nameless struct/union
// Disable warning C4115: named type definition in parentheses
// Disable warning C4127: conditional expression is constant.
// Disable warning C4152: nonstandard extension used : function/data pointer
// conversion in expression.
// Disable warning C4200: zero-sized array in struct/union
//

#pragma warning(disable:4214 4201 4115 4127 4152 4200)

//
// ---------------------------------------------------------------- Definitions
//

//
// Timer capabilities
//

//
// Set if the timer is local to a processor, or clear if the timer is globally
// visible.
//

#define TIMER_PER_PROCESSOR 0x00000001

//
// Set if the counter can be read without stopping or destroying its value.
//

#define TIMER_COUNTER_READABLE 0x00000002

//
// Set if the counter can be written to. It's fine if the counter has to be
// stopped to write to it.
//

#define TIMER_COUNTER_WRITABLE 0x00000004

//
// Set if the timer has a configurable divisor.
//

#define TIMER_HAS_DIVISOR 0x00000008

//
// Set if the timer is capable of generating a periodic interrupts, but only by
// re-arming the timer on every firing. Only deadline-based timers are able to
// support this capability, as normal timers would lose time during the re-arm
// phase.
//

#define TIMER_PSEUDO_PERIODIC_CAPABLE 0x00000010

//
// Set if the timer is capable of stopping when the interrupt fires.
//

#define TIMER_ONE_SHOT_CAPABLE 0x00000020

//
// Set if the timer is capable of automatically reloading when the interrupt
// fires.
//

#define TIMER_PERIODIC_CAPABLE 0x00000040

//
// Set if the timer can only generate rollover interrupts, not configurable
// interrupts.
//

#define TIMER_ROLLOVER_INTERRUPTS_ONLY 0x00000080

//
// Set if the timer can generate legacy interrupts.
//

#define TIMER_GENERATES_8259_INTERRUPTS 0x00000100

//
// Set if the timer can generate normal line-based interrupts.
//

#define TIMER_GENERATES_LINE_BASED_INTERRUPTS 0x00000200

//
// Set if the timer can generate MSIs.
//

#define TIMER_GENERATES_MSI_INTERRUPTS 0x00000400

//
// Set if the timer is integrated into the system interrupt controller and
// generates interrupts internally.
//

#define TIMER_GENERATES_INTERNAL_INTERRUPTS 0x00000800

//
// Used to determine if the timer is capable of generating any sort of
// interrupt.
//

#if defined(_AMD64_) || defined(_X86_)

#define TIMER_INTERRUPT_MASK (TIMER_GENERATES_8259_INTERRUPTS |       \
                              TIMER_GENERATES_LINE_BASED_INTERRUPTS | \
                              TIMER_GENERATES_MSI_INTERRUPTS |        \
                              TIMER_GENERATES_INTERNAL_INTERRUPTS)


#endif

//
// Set if the timer is able to change the periodic interrupt rate without
// introducing any delay.
//

#define TIMER_LOSSLESS_RATE_CHANGE 0x00001000

//
// Set if the timer's counter frequency may change with performance states
// (Dynamic Voltage and Frequency Scaling).
//

#define TIMER_PERFORMANCE_STATE_VARIANT 0x00002000

//
// Set if the timer's counter frequency may change with processor core idle
// states.
//

#define TIMER_IDLE_STATE_VARIANT 0x00004000

//
// Set if the timer frequency may vary with deep processor idle states or
// performance states. This value is equivalent to specifying the Performance
// State Variant and Idle State Variant flags.
//

#define TIMER_VARIANT (TIMER_PERFORMANCE_STATE_VARIANT | \
                       TIMER_IDLE_STATE_VARIANT)

//
// Defines the mask of capabilities that can be updated post-registration.
//

#define TIMER_UPDATABLE_CAPABILITIES (TIMER_VARIANT)

//
// Defines the total valid timer capability flags. Timer plugins must not set
// flags outside this mask, as they are reserved for future use.
//

#define TIMER_VALID_CAPABILITIES 0x00007FFF

//
// Set if the interrupt controller has a local interrupt unit per-processor
// as well as the I/O unit. The Local APIC or Local GIC unit are examples of
// local units.
//

#define INTERRUPT_CONTROLLER_HAS_LOCAL_UNIT 0x00000001

//
// Set if the interrupt controller implements interrupt prioritization.
// Setting the priority is expected to be a per-processor operation on
// controllers with local units, and an I/O unit operation on controllers
// without a local unit.
//

#define INTERRUPT_CONTROLLER_HAS_PRIORITY_SCHEME 0x00000002

//
// Set if the interrupt controller supports logical flat mode. Only valid on
// controllers with a local unit.
//

#define INTERRUPT_CONTROLLER_SUPPORTS_LOGICAL_FLAT 0x00000004

//
// Set if the interrupt controller supports logical clustered mode. Only valid
// on controllers with a local unit.
//

#define INTERRUPT_CONTROLLER_SUPPORTS_LOGICAL_CLUSTERED 0x00000008

//
// Set if the interrupt controller supports all including self shorthand.
//

#define INTERRUPT_CONTROLLER_ALL_INCLUDING_SELF_SHORTHAND 0x00000010

//
// Set if the interrupt controller supports all excluding self shorthand.
//

#define INTERRUPT_CONTROLLER_ALL_EXCLUDING_SELF_SHORTHAND 0x00000020

//
// Set if the interrupt controller supports the self shorthand.
//

#define INTERRUPT_CONTROLLER_SELF_SHORTHAND 0x00000040

//
// Defines the mask of all shorthand types.
//

#define INTERRUPT_CONTROLLER_SHORTHAND_MASK                               \
    (INTERRUPT_CONTROLLER_ALL_INCLUDING_SELF_SHORTHAND |                  \
     INTERRUPT_CONTROLLER_ALL_EXCLUDING_SELF_SHORTHAND |                  \
     INTERRUPT_CONTROLLER_SELF_SHORTHAND)

//
// Set if the interrupt controller supports interrupt remapping.
//

#define INTERRUPT_CONTROLLER_SUPPORTS_INTERRUPT_REMAPPING 0x00000080

//
// Set if the interrupt controller requires interrupt remapping to be enabled.
//

#define INTERRUPT_CONTROLLER_REQUIRES_INTERRUPT_REMAPPING 0x00000100

//
// Defines the mask of all supported capabilities. The interrupt controller
// must not set any bits outside this range.
//

#define INTERRUPT_CONTROLLER_VALID_CAPABILITIES 0x000001FF

//
// This defines a reserved Controller ID representing a CPU interrupt. This
// unit ID is used to describe intrinsic CPU interrupt lines (ie inputs that
// actually cause the CPU to interrupt).
//

#define INTERRUPT_CPU_UNIT_ID 0xFFFFFFFF

//
// These defines describe the various CPU interrupt lines built in to the PC
// processor.
//

#define PC_CPU_INTERRUPT_PIN 1
#define PC_CPU_NMI_PIN 2
#define PC_CPU_SMI_PIN 3
#define PC_CPU_EXTINT_PIN 4
#define PC_CPU_LINT0_PIN 5
#define PC_CPU_LINT1_PIN 6
#define PC_CPU_INIT_PIN 7


//
// Define the architecture-agnostic interrupt pin for CPUs.
//

#if defined(_X86_) || defined(_AMD64_)

#define STANDARD_CPU_INTERRUPT_PIN PC_CPU_INTERRUPT_PIN


#endif

#define TIMER_INITIALIZATION_VERSION 1
#define INTERRUPT_INITIALIZATION_VERSION 1

//
// Interrupt line state flags.
//

#define INTERRUPT_LINE_ENABLED                 0x00000001
#define INTERRUPT_LINE_DELIVER_LOWEST_PRIORITY 0x00000002

//
// Built-in lines assumed for the APIC.
//

#define LAPIC_TIMER_PIN               (-1)
#define LAPIC_THERMAL_PIN             (-2)
#define LAPIC_PERFORMANCE_COUNTER_PIN (-3)
#define LAPIC_LINT0_PIN               (-4)
#define LAPIC_LINT1_PIN               (-5)
#define LAPIC_ERROR_PIN               (-6)
#define LAPIC_CMCI_PIN                (-7)
#define LAPIC_IPI_PIN                 (-10)

//
// Define baked definitions for PC 8259A interrupt controllers.
//

#define PIC1_IDENTIFIER 0xB000
#define PIC2_IDENTIFIER 0xB001
#define PIC_PIN_COUNT   8

typedef enum _TIMER_MODE {
    TimerModeInvalid,
    TimerModePseudoPeriodic,
    TimerModePeriodic,
    TimerModeOneShot,
    MaxTimerModes
} TIMER_MODE, *PTIMER_MODE;

typedef enum _KNOWN_TIMER_TYPE {
    TimerInvalid,
    TimerAcpi,
    TimerCmosRtc,
    TimerHpet,
    Timer8254,
    TimerProcessor,
    TimerSfi,
    TimerApic,
    TimerHypervisor,
    TimerBrokenAcpi,
    TimerGp,
    TimerSp804,
    TimerCycleCounter,
    TimerUnknown = 0x1000
} KNOWN_TIMER_TYPE, *PKNOWN_TIMER_TYPE;

//
// Routines that need to be implemented by each timer implementation.
//

typedef
NTSTATUS
(*PTIMER_INITIALIZE) (
    __in PVOID TimerData
    );

/*++

Routine Description:

    This routine initializes the timer hardware.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

Return Value:

    NT Status code.

--*/

typedef
ULONGLONG
(*PTIMER_QUERY_COUNTER) (
    __in PVOID TimerData
    );

/*++

Routine Description:

    This routine queries the timer hardware and retrieves the current counter
    value. Timers are assumed to always be count-up timers.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

Return Value:

    Returns the hardware's current count.

--*/

typedef
VOID
(*PTIMER_ACKNOWLEDGE_INTERRUPT) (
    __in PVOID TimerData
    );

/*++

Routine Description:

    This routine performs any actions necessary to acknowledge and quiesce a
    timer interrupt.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

Return Value:

    None.

--*/

typedef
VOID
(*PTIMER_SET_DIVISOR) (
    __in PVOID TimerData,
    __in ULONG NewDivisor
    );

/*++

Routine Description:

    This routine sets the timer's clock divisor, controlling the speed at which
    the counter updates.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

    NewDivisor - Supplies the new divisor rate to set. This value will be a
        power of two and will be less than or equal to the timer's specified
        maximum divisor.

Return Value:

    None.

--*/

typedef
NTSTATUS
(*PTIMER_CORE_TIMER) (
    __in PVOID TimerData,
    __in TIMER_MODE Mode,
    __in ULONGLONG TickCount
    );

/*++

Routine Description:

    This routine arms a timer to fire an interrupt after a given period of time.
    For timers that only interrupt on rollovers, this arms the timer under the
    assumption that any divisors have already been set accordingly.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

    Mode - Supplies the desired mode to arm the timer with (periodic or
        one-shot).

    TickCount - Supplies the number of ticks from now that the timer should
        interrupt in. For timers that only interrupt on rollovers, this
        parameter is ignored.

Return Value:

    NT Status code.

--*/

typedef
NTSTATUS
(*PTIMER_SET_MESSAGE_INTERRUPT_ROUTING) (
    __in PVOID TimerData,
    __in BOOLEAN Enable,
    __in PHYSICAL_ADDRESS MessageAddress,
    __in ULONG MessageData
    );

/*++

Routine Description:

    This routine enables or disables message signalled interrupts on the given
    timer and configures the address and data of the message to generate on
    interrupts. This function only needs to be implemented on timers that have
    indicated they are capable of generating message signalled interrupts.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

    Enable - Supplies a flag indicating whether or not MSI delivery should be
        enabled (TRUE) or disabled (FALSE).

    MessageAddress - Supplies the physical address to send the message to. This
        parameter should be ignored when MSIs are being disabled.

    MessageData - Supplies the value to write to the specified address. This
        parameter should be ignored when MSIs are being disabled.

Return Value:

    NT Status Code.

--*/

typedef
ULONG
(*PTIMER_LOSSLESS_RATE_CHANGE) (
    __in PVOID TimerData,
    __in ULONGLONG NewTickCount
    );

/*++

Routine Description:

    This routine changes the rate of a timer that is already operating in
    periodic mode. If this function is implemented, it must support changing
    the periodic interrupt rate without introducing any delay/skew to the rate.
    Traditionally only deadline-based timers are able to support this
    capability. If this function is not supported, the traditional arming
    mechanism will be used to set a new periodic rate.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

    NewTickCount - Supplies the new periodic rate, in hardware timer ticks.

Return Value:

    Returns the number of interrupts that will occur at the old rate. This is
    the way the timer can let the system know that the rate change is buffered.

--*/

typedef
VOID
(*PTIMER_SET_INTERRUPT_VECTOR) (
    __in PVOID TimerData,
    __in ULONG Vector
    );

/*++

Routine Description:

    This routine informs the timer of the vector it's going to be generating
    interrupts on. Most timers do not need to implement this function, it is
    only necessary for timers built in to interrupt controllers.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

    Vector - Supplies the vector that the timer should configure itself to
        generate interrupts on.

Return Value:

    None.

--*/

typedef
ULONGLONG
(*PTIMER_FIXED_STALL) (
    __in PVOID TimerData,
    __in ULONGLONG MinimumStallIn100ns
    );

/*++

Routine Description:

    This routine accurately stalls execution. This routine will be used when a
    timer is capable of being an accurate stall source but its counter is
    unreadable or its stall durations are fixed. This routine will always be
    called twice in quick succession: once to allow the timer to adjust to its
    fixed-stall cadence, and then once to actually perform the stall.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

    MinimumStallIn100ns - Supplies the minimum amount of time to stall for
        in 100 nanoseconds, or 0 to allow the timer to adjust to the fixed-stall
        cadence. If this parameter is 0, the return value is ignored.

Return Value:

    0 if the stall length is unknown (used when 0 is passed in to the minimum
    stall length).

    Returns the number of 100 nanosecond units the stall lasted. This number
    should be no less than the minimum stall parameter.

--*/

typedef
VOID
(*PTIMER_STOP) (
    __in PVOID TimerData
    );

/*++

Routine Description:

    This routine stops a timer from ticking. After this function returns, the
    timer should not generate any more interrupts, and reads to its counter
    might return the same value every time.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data.

Return Value:

    None, this function must succeed.

--*/

//
// Interrupt controller definitions.
//

typedef enum _KNOWN_CONTROLLER_TYPE {
    InterruptControllerInvalid,
    InterruptControllerPic,
    InterruptControllerApic,
    InterruptControllerGic,
    InterruptControllerUnknown = 0x1000
} KNOWN_CONTROLLER_TYPE, *PKNOWN_CONTROLLER_TYPE;

typedef enum _INTERRUPT_CONTROLLER_PROBLEM {
    InterruptControllerProblemNone,
    InterruptProblemNullParameter,
    InterruptProblemLateRegistration,
    InterruptProblemInvalidHeader,
    InterruptProblemInvalidType,
    InterruptProblemInvalidId,
    InterruptProblemMemoryAllocationFailure,
    InterruptProblemNoInitializeLocalUnit,
    InterruptProblemNoInitializeIoUnit,
    InterruptProblemNoSetPriority,
    InterruptProblemNoMaxPriority,
    InterruptProblemNoSetLineState,
    InterruptProblemNoRequestInterrupt,
    InterruptProblemNoStartProcessor,
    InterruptProblemNoReplayFunctions,
    InterruptProblemNoAcceptFunction,
    InterruptProblemShouldNotImplementAccept,
    InterruptProblemInvalidCapabilities,
    InterruptProblemInvalidLines,
    InterruptProblemControllerNotFound,
    InterruptProblemConflictingLines,
    InterruptProblemConflictingGsis,
    InterruptProblemInsufficientResources,
} INTERRUPT_CONTROLLER_PROBLEM, *PINTERRUPT_CONTROLLER_PROBLEM;

typedef enum _INTERRUPT_TARGET_TYPE {
    InterruptTargetInvalid,
    InterruptTargetAllIncludingSelf,
    InterruptTargetAllExcludingSelf,
    InterruptTargetSelfOnly,
    InterruptTargetPhysical,
    InterruptTargetLogicalFlat,
    InterruptTargetLogicalClustered,
    InterruptTargetRemapIndex
} INTERRUPT_TARGET_TYPE, *PINTERRUPT_TARGET_TYPE;

typedef enum _INTERRUPT_LINE_TYPE {
    InterruptLineInvalidType,
    InterruptLineUnusable,
    InterruptLineStandardPin,
    InterruptLineProcessorLocal,
    InterruptLineSoftwareOnly,
    InterruptLineSoftwareOnlyProcessorLocal,
    InterruptLineOutputPin
} INTERRUPT_LINE_TYPE, *PINTERRUPT_LINE_TYPE;

typedef enum _INTERRUPT_RESULT {
    InterruptBeginFatalError,
    InterruptBeginLine,
    InterruptBeginSpurious,
} INTERRUPT_RESULT, *PINTERRUPT_RESULT;

//
// Forward declarations used by interrupt SoC routines.
//

typedef struct _INTERRUPT_LINE INTERRUPT_LINE, *PINTERRUPT_LINE;
typedef struct _INTERRUPT_TARGET INTERRUPT_TARGET, *PINTERRUPT_TARGET;
typedef struct _INTERRUPT_LINE_STATE
    INTERRUPT_LINE_STATE, *PINTERRUPT_LINE_STATE;

//
// Prototypes for routines used by interrupt SoC modules.
//

typedef
NTSTATUS
(*PINTERRUPT_INITIALIZE_LOCAL_UNIT) (
    __in PVOID ControllerData,
    __in ULONG ProcessorNumber,
    __in ULONG SpuriousVector,
    __in ULONG StubVector,
    __in ULONG LocalErrorVector,
    __out PULONG LocalId
    );

/*++

Routine Description:

    This routine initializes a processor local interrupt unit. This routine
    should enable the unit and raise to the highest priority to mask all
    interrupts from the I/O Unit. Any locally-defined interrupts should be
    masked and pointed at the spurious vector.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    ProcessorNumber - Supplies the zero-based processor number that this code
        is running on.

    SpuriousVector - Supplies the vector to route spurious interrupts to. For
        non-vectored interrupt controllers or interrupt controllers whose
        spurious vectors cannot be configured, this parameter can be safely
        ignored.

    StubVector - Supplies the vector to route any interrupts to that have yet
        to be set up. The difference between the stub vector and the spurious
        vector is that the stub vector will EOI the interrupt.

    LocalErrorVector - Supplies the vector to route any local unit error
        interrupts to.

    LocalId - Supplies a pointer where the controller should return its local
        unit ID. This ID will be used when targeting interrupts in physical
        mode.

Return Value:

    NT Status code. Failure codes will result in a system bugcheck.

--*/

typedef
NTSTATUS
(*PINTERRUPT_INITIALIZE_IO_UNIT) (
    __in PVOID ControllerData
    );

/*++

Routine Description:

    This routine initializes an interrupt controller. It is responsible for
    masking all interrupt lines on the controller. Also by this time this
    function returns, all interrupt lines on the controller and any associated
    local units should be registered with the HAL. Controller plugins should
    also report any fixed interrupt lines at this point. This routine will be
    called once on boot and resume.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.
        Unused.

Return Value:

    NT Status code. Failing codes result in bugchecks.

--*/

typedef
VOID
(*PINTERRUPT_SET_PRIORITY) (
    __in PVOID ControllerData,
    __in ULONG NewPriority
    );

/*++

Routine Description:

    This routine sets the current interrupt priority level on the given
    processor (for controllers with a local unit) or I/O unit (for controllers
    that have no local unit). This routine will be used for IRQL management,
    but may not necessarily get called at the beginning and ending of every
    interrupt. It is assumed that receiving an interrupt automatically raises
    the priority, and EOIing an interrupt automatically lowers it.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    NewPriority - Supplies the new interrupt priority. Priorities are assumed
        to go from 0, the lowest priority, to MaxPriority, the highest,
        inclusive. This routine is responsible for munging that number into a
        value the interrupt controller can understand.

Return Value:

    None.

--*/

typedef
ULONG
(*PINTERRUPT_GET_LOCAL_UNIT_ERROR) (
    __in PVOID ControllerData
    );

/*++

Routine Description:

    This routine returns a 32-bit error code associated with the local unit.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

Return Value:

    Returns a 32-bit value representing the local unit's current error
    condition.

--*/

typedef
VOID
(*PINTERRUPT_CLEAR_LOCAL_UNIT_ERROR) (
    __in PVOID ControllerData
    );

/*++

Routine Description:

    This routine clears any errors in the local unit.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

Return Value:

    None.

--*/

typedef
NTSTATUS
(*PINTERRUPT_GET_LOGICAL_ID) (
    __in PVOID ControllerData,
    __out PINTERRUPT_TARGET Target
    );

/*++

Routine Description:

    This routine read the logical ID from the local unit and returns the
    interrupt target data for this processor. This routine is only implemented
    for controllers with a local unit.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    Target - Supplies a pointer where the interrupt target information will be
        returned. This information may either be a logical ID or cluster ID and
        mask.

Return Value:

    STATUS_SUCCESS on success.

    STATUS_UNSUCCESSFUL if the processor does not support logical mode
        addressing.

--*/

typedef
NTSTATUS
(*PINTERRUPT_SET_LOGICAL_ID) (
    __in PVOID ControllerData,
    __in PINTERRUPT_TARGET Target
    );

/*++

Routine Description:

    This routine sets the logical ID in the local unit of the current processor.
    This routine should only be implemented by controllers that have a local
    unit.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    Target - Supplies a pointer to the interrupt target information to program
        into the local unit. A physical target destination should indicate to
        the controller to remove itself from the logical destination pool if
        possible.

Return Value:

    STATUS_SUCCESS on success.

    STATUS_NOT_SUPPORTED if the controller cannot program that mode.

    STATUS_UNSUCCESSFUL if the relevant registers cannot be written to.

--*/

typedef
INTERRUPT_RESULT
(*PINTERRUPT_ACCEPT_AND_GET_SOURCE) (
    __in PVOID ControllerData,
    __out PLONG Line,
    __out PULONG OpaqueData
    );

/*++

Routine Description:

    This routine is called on non-vectored architectures when an interrupt
    occurs. It is responsible for doing any initial acknowledgement (such that
    interrupts can be re-enabled at the processor without taking the same
    interrupt again), and retrieving the line number that generated the
    interrupt. This routine will not get called on vectored architectures.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    Line - Supplies a pointer where the interrupting line will be returned by
        the plugin.

    OpaqueData - Supplies a pointer where the plugin can store opaque data
        regarding this interrupt.

Return Value:

    Interrupt Result code indicating whether the interrupt is a valid line,
    spurious, or whether a catastrophic error occurred.

--*/

//#if defined(_X86_) || defined(_AMD64_)

typedef
VOID
(*PINTERRUPT_END_OF_INTERRUPT) (
    __in PVOID ControllerData,
    __in ULONG OpaqueToken
    );

/*++

Routine Description:

    This routine signals to the I/O unit (or the local unit if the controller
    has one) that processing on the current interrupt has completed.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

Return Value:

    None.

--*/


//#endif

typedef
VOID
(*PINTERRUPT_FAST_END_OF_INTERRUPT) (
    VOID
    );

/*++

Routine Description:

    This routine implements an optimized path for EOIing a controller. It
    takes no arguments, thus it may not be supportable by controllers
    requiring a pointer to their private data.

Arguments:

    None.

Return Value:

    None.

--*/

typedef
NTSTATUS
(*PINTERRUPT_SET_LINE_STATE) (
    __in PVOID ControllerData,
    __in PINTERRUPT_LINE Line,
    __in PINTERRUPT_LINE_STATE NewState
    );


/*++

Routine Description:

    This routine enables, disables, or changes the configuration of a given
    interrupt input.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    Line - Supplies a pointer to a structure describing which line to
        configure.

    NewState - Supplies a pointer to a structure describing how the line
        should be configured.

Return Value:

    NT Status code.

--*/

typedef
NTSTATUS
(*PINTERRUPT_REQUEST_INTERRUPT) (
    __in PVOID ControllerData,
    __in PINTERRUPT_LINE Line,
    __in PINTERRUPT_TARGET Target,
    __in ULONG Vector,
    __in PINTERRUPT_LINE OutputLine
    );

/*++

Routine Description:

    This routine requests an interrupt on the given line at the given vector.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    Line - Supplies a pointer to a structure describing which line to
        trigger.

    Target - Supplies the destination processors to target this interrupt at.

    Vector - For software lines, describes which vector to generate the
        interrupt on. Controllers that do not support vectoring can safely
        ignore this parameter.

    OutputLine - Supplies a pointer to the output line to generate this
        interrupt on.

Return Value:

    NT Status code.

--*/

typedef
NTSTATUS
(*PINTERRUPT_START_PROCESSOR) (
    __in PVOID ControllerData,
    __in ULONG LocalUnitId,
    __in PVOID StartupCodeVirtual,
    __in ULONG StartupCodePhysical
    );

/*++

Routine Description:

    This routine starts an application processor executing at the given
    address.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    LocalUnitId - Supplies the local interrupt controller unit ID of the
        processor to start.

    StartupCodeVirtual - Supplies the virtual address of the code to start
        executing. This address will always be page aligned.

    StartupCodePhysical - Supplies the physical address of the code to start
        executing. This address will always be page aligned.

Return Value:

    NT Status code. Success means that the processor startup command was
    successfully sent, not that the processor necessarily responded to the
    request.

--*/

typedef
NTSTATUS
(*PINTERRUPT_GENERATE_MESSAGE) (
    __in PVOID ControllerData,
    __in PINTERRUPT_LINE_STATE LineState,
    __out PULONGLONG MessageAddress,
    __out PULONGLONG MessageData
    );

/*++

Routine Description:

    This routine generates a message address/data pair that will generate an
    interrupt at the desired local units with the given characteristics.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    LineState - Supplies characteristics about what kind of interrupt the
        message should generate at the local unit(s).

    MessageAddress - Supplies a pointer where the 64-bit address portion of
        the message will be returned. The interrupt controller extension fills
        this with the appropriate information to generate the interrupt.

    MessageData - Supplies a pointer where the 64-bit data portion of the
        message will be returned. The interrupt controller extension fills this
        with the appropriate information to generate the interrupt.

Return Value:

    STATUS_SUCCESS if a message with the desired characteristics could be
        generated.

    STATUS_NOT_SUPPORTED if the given characteristics could not be met.

    Other error codes on failure.

--*/

typedef
NTSTATUS
(*PINTERRUPT_CONVERT_ID) (
    __in PVOID ControllerData,
    __inout PULONG PhysicalId,
    __inout PINTERRUPT_TARGET LogicalTarget,
    __in BOOLEAN ToLogical
    );

/*++

Routine Description:

    This routine converts a physical ID to a logical ID, or a logical ID to a
    physical ID. This is only used on systems where there is a set mapping
    between physical and logical ID.

Arguments:

    ControllerData - Supplies a pointer to the controller's private data.

    PhysicalId - Supplies a pointer to the physical ID to convert to a logical
        ID, or a pointer where the physical ID should be returned.

    LogicalTarget - Supplies a pointer to the logical target to convert to a
        physical ID, or a pointer where the logical target should be returned.

    ToLogical - Supplies a boolean indicating the direction to translate. If
        TRUE, the physical ID should be converted to a logical target. If FALSE,
        the logical target should be turned into a physical ID.

Return Value:

    NT Status code.

--*/

typedef
NTSTATUS
(*PINTERRUPT_SAVE_LOCAL_INTERRUPTS) (
    __in PVOID ControllerData,
    __inout PVOID SaveBuffer
    );

/*++

Routine Description:

    This routine save the contents of the interrupts pending in the local unit
    so that it can be replayed once another processor has been swapped in.
    This routine is only used for hot replace of processors with hardware
    quiesce support.

Arguments:

    ControllerData - Supplies a pointer to the controller's internal data.

    SaveBuffer - Supplies a pointer to a buffer where the state of this
        processor's local unit should be saved. The size of the buffer is
        specified by the controller upon registration, and its contents are
        opaque to the HAL.

Return Value:

    NT Status code.

--*/

typedef
NTSTATUS
(*PINTERRUPT_REPLAY_LOCAL_INTERRUPTS) (
    __in PVOID ControllerData,
    __in PVOID SaveBuffer
    );

/*++

Routine Description:

    This routine issues a series of self-IPIs to replay edge-triggered
    interrupts on the current processor based on stored local unit state.

Arguments:

    ControllerData - Supplies a pointer to the controller's internal data.

    SaveBuffer - Supplies a buffer containing the state of the interrupt
        controller as saved by the SaveLocalInterrupts function.

Return Value:

    NT Status code.

--*/

//
// Forward declaration used by DMA extension interface.
//

typedef enum _DMA_INTERRUPT_TYPE DMA_INTERRUPT_TYPE, *PDMA_INTERRUPT_TYPE;
typedef struct _DMA_REQUEST_LINE_BINDING_DESCRIPTION
               DMA_REQUEST_LINE_BINDING_DESCRIPTION,
               *PDMA_REQUEST_LINE_BINDING_DESCRIPTION;

//
// Prototypes for routines used by DMA exntension modules.
//

typedef
VOID
(*PDMA_INITIALIZE_CONTROLLER) (
    __in PVOID ControllerContext
    );

/*++

Routine Description:

    This routine provides an opportunity for DMA controllers to initialize.

Arguments:

    ControllerContext - Supplies a pointer to the controller's internel data.

Return Value:

    None.

--*/

typedef
BOOLEAN
(*PDMA_VALIDATE_REQUEST_LINE_BINDING) (
    __in PVOID ControllerContext,
    __in PDMA_REQUEST_LINE_BINDING_DESCRIPTION BindingDescription
    );

/*++

Routine Description:

    This routine queries a DMA controller extension to test the validity of a
    request line binding.

Arguments:

    ControllerContext - Supplies a pointer to the controller's internal data.

    DeviceDescription - Supplies a pointer to the request information.

Return Value:

    TRUE if the request line binding is valid and supported by the controller.

    FALSE if the binding is invalid.

Environment:

    PASSIVE_LEVEL.

--*/

typedef
VOID
(*PDMA_PROGRAM_CHANNEL) (
    __in PVOID ControllerContext,
    __in ULONG ChannelNumber,
    __in ULONG RequestLine,
    __in PHYSICAL_ADDRESS MemoryAddress,
    __in PHYSICAL_ADDRESS DeviceAddress,
    __in ULONG Length,
    __in BOOLEAN WriteToDevice,
    __in ULONG PortWidth
    );

/*++

Routine Description:

    This routine programs a DMA controller channel for a specific transfer.

Arguments:

    ControllerContext - Supplies a pointer to the controller's internal data.

    ChannelNumber - Supplies the number of the channel to program.

    RequestLine - Supplies the request line number to program.  This request
        line number is system-unique (as provided to the HAL during
        registration) and must be translated by the extension.

    MemoryAddress - Supplies the address to be programmed into the memory
        side of the channel configuration.

    DeviceAddress - Supplies the address to be programmed into the device
        side of the channel configuration.

    Length - Supplies the length of the transfer.

    WriteToDevice - Supplies the direction of the transfer.

    PortWidth - Supplies device address wrapping width.

Return Value:

    None.

--*/

typedef
NTSTATUS
(*PDMA_CONFIGURE_CHANNEL) (
    __in PVOID ControllerContext,
    __in ULONG ChannelNumber,
    __in ULONG FunctionNumber,
    __in PVOID Context
    );

/*++

Routine Description:

    This routine configures the channel for a DMA extension specific operation.

Arguments:

    ControllerContext - Supplies a pointer to the controller's internal data.

    ChannelNumber - Supplies the channel to configure.

    FunctionNumber - Supplies the ID of the operation to perform.

    Context - Supplies parameters for this operation.

Return Value:

    NTSTATUS code.

--*/

typedef
VOID
(*PDMA_FLUSH_CHANNEL) (
    __in PVOID ControllerContext,
    __in ULONG ChannelNumber
    );

/*++

Routine Description:

    This routine flushes a previous transfer from a channel and returns the
    channel to a state ready for the next ProgramChannel call.

Arguments:

    ControllerContext - Supplies a pointer to the controller's internal data.

    ChannelNumber - Supplies the channel to flush.

Return Value:

    None.

--*/

typedef
BOOLEAN
(*PDMA_HANDLE_INTERRUPT) (
    __in PVOID ControllerContext,
    __out PULONG ChannelNumber,
    __out PDMA_INTERRUPT_TYPE InterruptType
    );

/*++

Routine Description:

    This routine probes a controller for interrupts, clears any interrupts
    found, fills in channel and interrupt type information.

Arguments:

    ControllerContext - Supplies a pointer to the controller's internal data.

    ChannelNumber - Supplies a placeholder for the extension to fill in which
        channel is interrupting.

    InterruptType - Supplies a placeholder for the extension to fill in the
        interrupt type.

Return Value:

    TRUE if an interrupt was found on this controller.

    FALSE otherwise.

--*/

//
// ------------------------------------------------- Data Structure Definitions
//

typedef struct _TIMER_FUNCTION_TABLE {
    PTIMER_INITIALIZE Initialize;
    PTIMER_QUERY_COUNTER QueryCounter;
    PTIMER_ACKNOWLEDGE_INTERRUPT AcknowledgeInterrupt;
    PTIMER_CORE_TIMER ArmTimer;
    PTIMER_STOP Stop;
    PTIMER_SET_DIVISOR SetDivisor;
    PTIMER_SET_MESSAGE_INTERRUPT_ROUTING SetMessageInterruptRouting;
    PTIMER_LOSSLESS_RATE_CHANGE LosslessRateChange;
    PTIMER_SET_INTERRUPT_VECTOR SetInterruptVector;
    PTIMER_FIXED_STALL FixedStall;
} TIMER_FUNCTION_TABLE, *PTIMER_FUNCTION_TABLE;

/*++

Timer Function Table Description :

    Exposes a set of basic operations that can be performed on timer hardware.

Fields:

    Initialize - Stores a pointer to the initialization function, which performs
        any actions necessary to get the timer to an operable state.

    QueryCounter - Stores a pointer to the query count routine, which returns
        the current hardware counter value.

    AcknowledgeInterrupt - Stores a pointer to the acknowledge interrupt
        routine, which performs any actions necessary to quiesce an interrupt.

    ArmTimer - Stores a pointer to the timer arming routine, which programs the
        timer to fire an interrupt after the specified number of ticks (if
        supported).

    Stop - Stores a pointer to a function that stops the timer.

    SetDivisor - Stores a pointer to the set divisor routine, which reprograms
        the hardware counter's divisor rate.

    SetMessageInterruptRouting - Stores a pointer to the routine for
        configuring a timer to generate message based interrupts. This function
        only needs to be implemented for timers that have indicated they can
        generate MSIs.

    LosslessRateChange - Stores a pointer to the routine that allows the
        system to change the clock rate without introducing timer programming
        delay.

    SetInterruptVector - Stores a pointer to the routine that configures the
        timer to generate an interrupt at a certain vector.

    FixedStall - Stores a pointer to the routine that generates stalls of
        fixed increments of time.

--*/

typedef struct _SOC_INITIALIZATION_HEADER {
    ULONG TableVersion;
    ULONG TableSize;
} SOC_INITIALIZATION_HEADER, *PSOC_INITIALIZATION_HEADER;

/*++

SoC Initialization Header Description:

    Stores the header for the SoC initialization block that is passed from a
    SoC module to the HAL.

Fields:

    TableVersion - Stores the version of the table.

    TableSize - Stores the total size, including the header, of the table.

--*/

typedef struct _TIMER_INTERRUPT {
    LONG LegacyIrq;
    LONG Gsi;
    KINTERRUPT_POLARITY Polarity;
    KINTERRUPT_MODE Mode;
} TIMER_INTERRUPT, *PTIMER_INTERRUPT;

/*++

Timer Initialization Block Description:

    Stores the information required to describe a timer's interrupt.

Fields:

    LegacyIrq - Stores the IRQ pin the timer's interrupt comes in on if it
        generates interrupts on a legacy 8259 interrupt controller.

    Gsi - Stores the global system interrupt number of the interrupt this timer
        generates, if it generates line-based interrupts.

    Polarity - Stores the polarity of the interrupt this timer generates, if it
        generates interrupts.

    Mode - Stores whether or not this timer generates edge or levelctriggered
        interrupts, if it generates interrupts at all.

--*/

typedef struct _TIMER_INITIALIZATION_BLOCK {
    SOC_INITIALIZATION_HEADER Header;
    TIMER_FUNCTION_TABLE FunctionTable;
    PVOID InternalData;
    ULONG InternalDataSize;
    ULONG CounterBitWidth;
    ULONGLONG CounterFrequency;
    ULONG MaxDivisor;
    ULONG Capabilities;
    TIMER_INTERRUPT Interrupt;
    KNOWN_TIMER_TYPE KnownType;
    ULONG Identifier;
} TIMER_INITIALIZATION_BLOCK, *PTIMER_INITIALIZATION_BLOCK;

/*++

Timer Initialization Block Description:

    Stores the information required of the timer module by the HAL for the HAL
    to register a new timer source.

Fields:

    Header - Stores the table header, used to verify that HAL can interpret the
        table contents correctly.

    FunctionTable - Stores pointers to the various functions implemented by the
        timer module for timer-specific tasks like initializing the hardware,
        quiescing interrupts, etc.

    InternalData - Stores a pointer to the individual timer's internal data.
        This pointer is passed to the timer module at each of its function
        table calls.

    InternalDataSize - Stores the size, in bytes, of the timer module's
        internal data strucuture.

    CounterBitWidth - Stores the width of the counter, in bits. This field is
        required even if the counter is not accessible to software, as its
        value can affect rollover interrupt rates.

    CounterFrequency - Stores the frequency of the hardware counter, in Hertz.
        This is used even if a timer's counter is not readable, since
        potentially interrupt rates depend on the divisor, counter width, and
        counter frequency. Set to 0 to indicate that the timer frequency is
        unknown.

    MaxDivisor - Stores the highest counter divisor supported by the hardware.

    Capabilities - Stores the capabilities of the current timer, such as the
        ability to generate interrupts, the ability to have its counter read or
        written, etc.

    Interrupt - Stores information about the interrupt this timer generates,
        if applicable.

    KnownType - Stores an identifier that marks this as a known timer type to
        the HAL. The HAL may choose to make certain decisions differently if it
        is familiar with this type of timer.

    Identifier - Stores a unique identifier for this timer.

--*/

typedef struct _INTERRUPT_FUNCTION_TABLE {
    PINTERRUPT_INITIALIZE_LOCAL_UNIT InitializeLocalUnit;
    PINTERRUPT_INITIALIZE_IO_UNIT InitializeIoUnit;
    PINTERRUPT_SET_PRIORITY SetPriority;
    PINTERRUPT_GET_LOCAL_UNIT_ERROR GetLocalUnitError;
    PINTERRUPT_CLEAR_LOCAL_UNIT_ERROR ClearLocalUnitError;
    PINTERRUPT_GET_LOGICAL_ID GetLogicalId;
    PINTERRUPT_SET_LOGICAL_ID SetLogicalId;
    PINTERRUPT_ACCEPT_AND_GET_SOURCE AcceptAndGetSource;
    PINTERRUPT_END_OF_INTERRUPT EndOfInterrupt;
    PINTERRUPT_FAST_END_OF_INTERRUPT FastEndOfInterrupt;
    PINTERRUPT_SET_LINE_STATE SetLineState;
    PINTERRUPT_REQUEST_INTERRUPT RequestInterrupt;
    PINTERRUPT_START_PROCESSOR StartProcessor;
    PINTERRUPT_GENERATE_MESSAGE GenerateMessage;
    PINTERRUPT_CONVERT_ID ConvertId;
    PINTERRUPT_SAVE_LOCAL_INTERRUPTS SaveLocalInterrupts;
    PINTERRUPT_REPLAY_LOCAL_INTERRUPTS ReplayLocalInterrupts;
} INTERRUPT_FUNCTION_TABLE, *PINTERRUPT_FUNCTION_TABLE;

/*++

Interrupt Function Table Description :

    Exposes a set of basic operations that can be performed by an interrupt
    controller.

Fields:

    InitializeLocalUnit - Stores a pointer to a function that is responsible
        for initializing the local interrupt controller on the current
        processor. This function is required for controllers that indicate an
        attached processor-local unit.

    InitializeIoUnit - Stores a pointer to a function that is responsible for
        initializing the interrupt controller unit. This function is required.

    SetPriority - Stores a pointer to a function that is responsible for
        setting the interrupt priority. This function is required for
        controllers that express an interrupt priority capability.

    GetLocalUnitError - Stores a pointer to a function that returns a 32-bit
        value representing the local unit's error condition. This function
        should not be implemented for controllers without a local unit, and is
        optional for controllers with a local unit.

    ClearLocalUnitError - Stores a pointer to a function that clears any error
        code in the local unit. This function should not be implemented for
        controllers that do not have a local unit, and is optional for
        controllers that do have a local unit.

    GetLogicalId - Stores a pointer to a function that returns the logical
        destination information for the local unit on the current processor.
        This should only be implemented for controllers with a local unit.

    SetLogicalId - Stores a pointer to a function that sets the logical
        destination information for the local unit on the current processor.
        This should only be implemented for controllers with a local unit. The
        absence of this function indicates that the logical information cannot
        be changed.

    AcceptAndGetSource - Stores a pointer to a function that is called on
        every interrupt. It is responsible for acknowledging the interrupt
        such that interrupts at the processor can be re-enabled, and returning
        the source of the interrupt. This routine will not be called on
        vectored architectures.

    EndOfInterrupt - Stores a pointer to a function that sends the End Of
        Interrupt message to the given controller. This routine is required.

    FastEndOfInterrupt - Stores a pointer to a function that sends an End Of
        Interrupt message. The only difference between this function and the
        standard End Of Interrupt function is that this routine takes no
        parameters. This routine is optional, but recommended for performance
        reasons.

    SetLineState - Stores a pointer to a function that enables, disables, or
        modifies the configuration of an interrupt input. This function is
        required for all interrupt controller implementations.

    RequestInterrupt - Stores a pointer to a function that generates an
        interrupt at the given line. Required for all controllers.

    StartProcessor - Stores a pointer to a function that sends startup commands
        to application processors.

    GenerateMessage - Stores a pointer to a function that generates a message
        Address/Data pair for generating an interrupt at one or more local
        units. This function is required only if Message Signalled [sic]
        Interrupts (MSIs) are expected to function.

    ConvertId - Stores a pointer to a function that converts between a logical
        target and a physical ID, and back. The presence of this function
        indicates that the mapping between logical and physical IDs is set
        (ie modifying one modifies the other).

    SaveLocalInterrupts - Stores a pointer to a function that saves the local
        unit interrupt state so it can be replayed on another processor
        during a hot-replace operation. This routine is only needed for
        controllers that support hot-replace with hardware quiesce.

    ReplayLocalInterrupts - Stores a pointer to a function that replays all
        interrupts saved during a call to save local interrupts. This function
        is only needed for controllers that support hot-replace with hardware
        quiesce.

--*/

typedef struct _INTERRUPT_INITIALIZATION_BLOCK {
    SOC_INITIALIZATION_HEADER Header;
    INTERRUPT_FUNCTION_TABLE FunctionTable;
    PVOID InternalData;
    ULONG InternalDataSize;
    KNOWN_CONTROLLER_TYPE KnownType;
    ULONG Identifier;
    ULONG Capabilities;
    ULONG MaxPriority;
    ULONG MaxClusterSize;
    ULONG MaxClusters;
    ULONG InterruptReplayDataSize;
} INTERRUPT_INITIALIZATION_BLOCK, *PINTERRUPT_INITIALIZATION_BLOCK;

/*++

Interrupt Controller Initialization Block Description:

    Stores the information required of the interrupt controller module by the
    HAL for the HAL to register a new interrupt controller unit.

Fields:

    Header - Stores the table header, used to verify that HAL can interpret the
        table contents correctly.

    FunctionTable - Stores pointers to the various functions implemented by the
        interrupt controller module for tasks like initializing the hardware,
        setting interrupt masks, etc.

    InternalData - Stores a pointer to the individual interrupt controller's
        internal data. This pointer is passed to the interrupt controller
        module at each of its function table calls.

    InternalDataSize - Stores the size, in bytes, of the interrupt module's
        internal data strucuture.

    KnownType - Stores an identifier that marks this as a known interrupt
        controller type to the HAL. The HAL may choose to make certain
        decisions differently if it is familiar with this type of interrupt
        controller.

    Identifier - Stores a 32-bit identifier for the controller. This must be
        unique across all interrupt controllers registered in the system.

    Capabilities - Stores a set of flags indicating the various capabilities
        and properties of this controller. See INTERRUPT_CONTROLLER_*
        definitions.

    MaxPriority - Stores the maximum priority value for interrupt controllers
        that include a priority scheme. Priority numbering is assumed to start
        at 0 (lowest priority) to MaxPriority (highest priority), inclusive.

    MaxClusterSize - Stores the maximum number of processors per cluster. This
        field is ignored if the controller doesn't have a local unit or
        doesn't support clustered interrupt targeting.

    MaxClusters - Stores the maximum number of clusters for this interrupt
        controller. This field is ignored if the controller doesn't have a
        local unit or doesn't support clustered interrupt targeting.

    InterruptReplayDataSize - Stores the number of bytes required for
        interrupt replay state storage, in bytes. Controllers should set this
        to 0 unless they support hot replace of processors with hardware
        quiescence (interrupt replay at the local unit). If this value is
        non-zero, then controllers *must* fill out the interrupt replay
        function pointers.

--*/

struct _INTERRUPT_LINE {
    ULONG UnitId;
    LONG Line;
};

/*++

Interrupt Line Structure:

    Stores the information uniquely identifying an interrupt line.

Fields:

    UnitId - Supplies the unit ID of the interrupt line.

    Line - Supplies the interrupt line number relative to the controller.

--*/

typedef struct _INTERRUPT_LINE_DESCRIPTION {
    ULONG ControllerIdentifier;
    LONG MinLine;
    LONG MaxLine;
    INTERRUPT_LINE_TYPE Type;
    ULONG Capabilities;
    ULONG OutputUnitId;
    ULONG GsiBase;
} INTERRUPT_LINE_DESCRIPTION, *PINTERRUPT_LINE_DESCRIPTION;

/*++

Interrupt Line Description Structure:

    Stores the information describing one or more interrupt lines.

Fields:

    ControllerIdentifier - Supplies the interrupt controller identifier
        associated with these lines.

    MinLine - Stores the minimimum interrupt line number this describes,
        relative to the controller, inclusive. For output pins, describes the
        minimum line on the *destination) controller that this pin can connect
        to, inclusive.

    MaxLine - Stores one after the maximum interrupt line number this describes,
        relative to the controller. For example, if a controller had 20 pins,
        MinLine would be 0 and MaxLine would be 20. This must not be equal to
        MinLine, otherwise 0 lines would be being described. For output pins,
        this describes maximum line on the *destination* controller that this
        output can connect to, exclusive.

    Type - Stores the type of interrupt line these line(s) represent.

    Capabilities - Stores various properties of the interrupt line(s). See
        INTERRUPT_LINE_* definitions.

    OutputUnitId - Stores the unit ID of the output pin(s). This parameter is
        only relevant for interrupt lines with a type of output pin, as it
        describes what unit that output pin is wired to.

    GsiBase - Stores the Global System Interrupt base of this set of pins. Set
        this to MAX_ULONG if there is no mapping to GSI or the mapping is
        unknown.

--*/

struct _INTERRUPT_TARGET {
    INTERRUPT_TARGET_TYPE Target;
    union {
        ULONG PhysicalTarget;
        ULONG LogicalFlatTarget;
        ULONG RemapIndex;
        struct {
            ULONG ClusterId;
            ULONG ClusterMask;
        };
    };
};

/*++

Interrupt Target Description :

    This structure defines the set of processors an individual interrupt line
    is targeted at.

Fields:

    Target - Stores the interrupt target, which may be either be shorthand or
        specify a targeting mode.

    PhysicalTarget - Stores the physical destination of the interrupt, if it
        is targeted in physical mode.

    LogicalFlatTarget - Stores the logical destination mask of the interrupt,
        if it is targeted in logical flat mode.

    RemapIndex - Stores the interrupt remapping table index of the interrupt,
        if it is targeting an interrupt remapping table entry.

    ClusterId - Stores the cluster ID of the interrupt target, if it is
        targeted in logical clustered mode.

    ClusterMask - Stores the mask within the cluster of the interrupt target,
        if it is targeted in logical clustered mode.

--*/

struct _INTERRUPT_LINE_STATE {
    KINTERRUPT_POLARITY Polarity;
    KINTERRUPT_MODE TriggerMode;
    ULONG Flags;
    INTERRUPT_LINE Routing;
    INTERRUPT_TARGET ProcessorTarget;
    ULONG Vector;
    ULONG Priority;
};

/*++

Interrupt Line State Definition:

    This structure describes the state of one interrupt input.

Fields:

    Polarity - Stores the line's interrupt polarity.

    TriggerMode - Stores the line's interrupt trigger mode (edge or level).

    Flags - Stores flags associated with the interrupt line state. See
        INTERRUPT_LINE_* definitions.

    Routing - Stores the interrupt output this input is routed to.

    ProcessorTarget - Stores the set of processors this line interrupts on.

    Vector - Stores the vector this interrupt comes in on. This is ignored for
        non-vectored controllers or routings.

--*/

//
// Define the DMA Extension interface table.
//

typedef struct _DMA_FUNCTION_TABLE {
    PDMA_INITIALIZE_CONTROLLER InitializeController;
    PDMA_VALIDATE_REQUEST_LINE_BINDING ValidateRequestLineBinding;
    PDMA_PROGRAM_CHANNEL ProgramChannel;
    PDMA_CONFIGURE_CHANNEL ConfigureChannel;
    PDMA_FLUSH_CHANNEL FlushChannel;
    PDMA_HANDLE_INTERRUPT HandleInterrupt;
} DMA_FUNCTION_TABLE, *PDMA_FUNCTION_TABLE;

/*++

DMA Function Table Description :

    Exposes a set of basic operations that can be performed by a DMA
    controller.

Fields:

    InitializeController - Perform any one-time initialization needed
        by the DMA controller.

    ValidateRequestLineBinding - Validate the request to bind a request
        line.

    ProgramChannel - Program a DMA channel and ready it for transfer.

    ConfigureChannel - Provides mechanism for DMA controller custom features.

    FlushChannel - Flushes the specified channel and returns the channel to a
        state ready to be programmed.

    HandleInterrupt - Probe DMA controller for interrupt.  Acknowledge the
        interrupt and return which channel interrupted.

--*/

typedef struct _DMA_INITIALIZATION_BLOCK {
    ULONG ControllerId;
    ULONG ChannelCount;
    BOOLEAN CacheCoherent;
    ULONG DmaAddressWidth;
    ULONG SupportedPortWidths;
    ULONG MinimumRequestLine;
    ULONG MaximumRequestLine;
    BOOLEAN GeneratesInterrupt;
    LONG Gsi;
    KINTERRUPT_POLARITY InterruptPolarity;
    KINTERRUPT_MODE InterruptMode;
    PVOID InternalData;
    ULONG InternalDataSize;
    PDMA_FUNCTION_TABLE Operations;
} DMA_INITIALIZATION_BLOCK, *PDMA_INITIALIZATION_BLOCK;

/*++

DMA Controller Initialization Block Description:

    Stores the information required of the DMA controller module by the
    HAL for the HAL to register a new DMA controller unit.

Fields:

    ControllerId - System-wide unique ID identifying controller.

    ChannelCount - Number of channels supported by teh controller.

    CacheCoherent - Describes whether this controller is cache coherent.

    DmaAddressWidth - Describes the width of this controller's address bus.
        This determiens the maximum address accessible by this controller.

    SupportedPortWidths - Describes which wrapping modes are supported by this
        controller.  This is a bitfield.

    MaximumRequestLine - System-unique maximum request line supported by
        this controller.

    MinimumRequestLine - System-unique minimum request line supported by this
        controller.

    GeneratesInterrupt - Describes whether this controller generates an
        interrupt to signal DMA completion.

    Gsi - Stores the global system interrupt number of the interrupt this DMA
        controller generates, if it generates line-based interrupts.

    InterruptPolarity - Stores the polarity of the interrupt this DMA
        controller generates, if it generates interrupts.

    InterruptMode - Stores whether or not this DMA controller generates edge
        or level triggered interrupts, if it generates interrupts at all.

    InternalData - Stores a pointer to the individual DMA controller's
        internal data. This pointer is passed to the DMA controller
        module at each of its function table calls.

    InternalDataSize - Stores the size, in bytes, of the DMA module's
        internal data strucuture.

--*/

typedef struct _DMA_CHANNEL_INITIALIZATION_BLOCK {
    ULONG ControllerId;
    BOOLEAN GeneratesInterrupt;
    LONG Gsi;
    KINTERRUPT_POLARITY InterruptPolarity;
    KINTERRUPT_MODE InterruptMode;
    ULONG ChannelNumber;
} DMA_CHANNEL_INITIALIZATION_BLOCK, *PDMA_CHANNEL_INITIALIZATION_BLOCK;

/*++

DMA Channel Initialization Block Description:

    Stores the information required of the DMA channel module by the
    HAL for the HAL to register a new DMA channel.

Fields:

    ControllerId - System-wide unique ID identifying controller which
        implements channel..

    GeneratesInterrupt - Describes whether this channel generates an
        interrupt to signal DMA completion.

    Gsi - Stores the global system interrupt number of the interrupt this DMA
        channel generates, if it generates line-based interrupts.

    InterruptPolarity - Stores the polarity of the interrupt this DMA channel
        generates, if it generates interrupts.

    InterruptMode - Stores whether or not this DMA channel generates edge or
        level triggered interrupts, if it generates interrupts at all.

    ChannelNumber - The number of the channel being described.  Channel numbers
        must be zero-based and not contain holes.

--*/

typedef enum _DMA_INTERRUPT_TYPE {
    InterruptTypeCompletion,
    InterruptTypeError
} DMA_INTERRUPT_TYPE, *PDMA_INTERRUPT_TYPE;

typedef struct _DMA_REQUEST_LINE_BINDING_DESCRIPTION {
    ULONG RequestLine;
    ULONG ChannelNumber;
} DMA_REQUEST_LINE_BINDING_DESCRIPTION, *PDMA_REQUEST_LINE_BINDING_DESCRIPTION;

//
// Flags used for DMA Port Widths.
//

#define DMA_UNLIMITED_PORT_WIDTH 1
#define DMA_8_BIT_PORT_WIDTH     2
#define DMA_16_BIT_PORT_WIDTH    4
#define DMA_32_BIT_PORT_WIDTH    8
#define DMA_64_BIT_PORT_WIDTH    16

//
// --------------------------------------------------------------------- Macros
//

//
// This macro performs a basic check on the SoC initialization headers,
// ensuring that the table size and version number are correct.
//

#define CHECK_TIMER_INITIALIZATION_HEADER(_Pointer)                          \
    (((_Pointer)->Header.TableVersion == TIMER_INITIALIZATION_VERSION) &&    \
     ((_Pointer)->Header.TableSize == sizeof(TIMER_INITIALIZATION_BLOCK)))

#define CHECK_INTERRUPT_INITIALIZATION_HEADER(_Pointer)                       \
    (((_Pointer)->Header.TableVersion == INTERRUPT_INITIALIZATION_VERSION) && \
     ((_Pointer)->Header.TableSize == sizeof(INTERRUPT_INITIALIZATION_BLOCK)))

//
// These macros initialize SoC initialization header structures.
//

#define INITIALIZE_TIMER_HEADER(_Pointer)                                    \
    ((_Pointer)->Header.TableVersion = TIMER_INITIALIZATION_VERSION),        \
    ((_Pointer)->Header.TableSize = sizeof(TIMER_INITIALIZATION_BLOCK))

#define INITIALIZE_INTERRUPT_HEADER(_Pointer)                                \
    ((_Pointer)->Header.TableVersion = INTERRUPT_INITIALIZATION_VERSION),    \
    ((_Pointer)->Header.TableSize = sizeof(INTERRUPT_INITIALIZATION_BLOCK))

//
// Common useful definitions.
//

#define ALIGN_RANGE_DOWN(_range, _alignment) \
    ((_range) & ~((ULONG64)(_alignment) - 1))

#define ALIGN_RANGE_UP(_range, _alignment) \
    ALIGN_RANGE_DOWN((_range) + (_alignment) - 1, _alignment)

#define Add2Ptr(_Ptr, _Value) ((PVOID)((PUCHAR)(_Ptr) + (_Value)))

//
// ----------------------------------------------------------------- Prototypes
//

NTSTATUS
HalRegisterTimer (
    __in PTIMER_INITIALIZATION_BLOCK NewTimer
    );

/*++

Routine Description:

    This routine is called by an individual timer module to make the HAL aware
    of a system timer. Registration of a timer does not necessarily mean that
    the system will choose to use the timer, but it does make the system aware
    of its existence.

Arguments:

    NewTimer - Supplies a pointer to the data used to register a new timer.
        The pointer passed here does not need to point to permanent memory.
        This routine will make a copy of the data here (including the internal
        data), which will be passed in to timer module routines. Callers of this
        function should not make modifications to internal data after this call,
        because the copy is the official version.

Return Value:

    NT Status Code.

--*/

NTSTATUS
HalUpdateTimerCapabilities (
    __in PVOID TimerData,
    __in ULONG SetNewCapabilities,
    __in ULONG ClearNewCapabilities
    );

/*++

Routine Description:

    This routine allows the timer module to update certain of its capabilities
    if new information comes to light. The current capabilities that may be
    updated post-registration are:

        TIMER_PERFORMANCE_STATE_VARIANT
        TIMER_IDLE_STATE_VARIANT
        TIMER_VARIANT

Arguments:

    TimerData - Supplies a pointer to the timer's internal data. This is used
        only to identify the timer, no data is modified.

    SetNewCapabilities - Supplies the new capabilities to set. This parameter
        will be ORed with the current capabilities. It must not contain bits
        set outside the capabilities that are allowed to change.

    ClearNewCapabilities - Supplies the capabilities to clear. This parameter
        will be negated and ANDed with the current capabilities. It must not
        contain bits set outside the capabilities that are allowed to change.

Return Value:

    STATUS_SUCCESS on success.

    STATUS_INVALID_PARAMETER if the caller attempted to set or clear a
        capability outside the allowed list.

    STATUS_NOT_FOUND if the timer could not be found in the list of registered
        timers.

--*/

NTSTATUS
HalRegisterInterruptController (
    __in PINTERRUPT_INITIALIZATION_BLOCK NewController
    );

/*++

Routine Description:

    This routine registers a new interrupt controller with the HAL.

Arguments:

    NewController - Supplies a pointer to the initialization structure
        describing the interrupt controller.

Return Value:

    STATUS_SUCCESS on success.

    STATUS_INVALID_PARAMETER if the controller initialization block was NULL,
        or filled out incorrectly.

    STATUS_DUPLICATE_NAME if a controller with the given identifier already
        exists.

--*/

NTSTATUS
HalRegisterInterruptLines (
    __in PINTERRUPT_LINE_DESCRIPTION Lines
    );

/*++

Routine Description:

    This routine registers new interrupt lines on an interrupt controller. This
    routine must be called after an interrupt controller is registered.

Arguments:

    Lines - Supplies the description of the interrupt lines.

Return Value:

    STATUS_SUCCESS on success.

    STATUS_INVALID_PARAMETER if the lines description is invalid.

    STATUS_NOT_FOUND if a registered interrupt controller matching that
    identifier could not be found.

    STATUS_RANGE_LIST_CONFLICT if the given interrupt lines have already been
    described.

    STATUS_INSUFFICIENT_RESOURCES if memory could not be allocated to satisfy
    the request.

--*/

NTSTATUS
HalFixInterruptLine (
    __in PINTERRUPT_LINE Line,
    __in PINTERRUPT_LINE_STATE State
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

VOID
HalSetInterruptProblem (
    __in PVOID InternalData,
    __in ULONG ProblemCode,
    __in NTSTATUS ProblemStatus
    );

/*++

Routine Description:

    This routine sets a problem code on an interrupt controller. This has no
    functional impact but is useful for debugging.

Arguments:

    InternalData - Supplies a pointer to the interupt controller's internal
        data. This is used to identify the controller.

    ProblemCode - Supplies an unsigned integer representing a user-defined
        problem code.

    ProblemStatus - Supplies an optional NT status code associated with the
        problem.

Return Value:

    None.

--*/

NTSTATUS
HalRegisterDmaController(
    __in PDMA_INITIALIZATION_BLOCK ControllerDescription
    );

/*++

Routine Description:

    This routine registers a new DMA controller with the system.  This is
    called when a resource descriptor for a new DMA controller is processed.

    This routine is responsible for saving information about the controller
    and adding the controller to the list of DMA controllers.

Arguments:

    ControllerDescription - Supplies pointer to description of DMA controller.
        This description includes the dispatch table of routines required to
        program and manage the controller.

Return Value:

    STATUS_SUCCESS if the controller has been successfully registered.

    STATUS_INVALID_PARAMETER if ControllerDescription is missing.

    STATUS_INSUFFICIENT_RESOURCES if memory cannot be allocated for the
        controller.

Environment:

    DISPATCH_LEVEL.

--*/

NTSTATUS
HalRegisterDmaChannel(
    __in PDMA_CHANNEL_INITIALIZATION_BLOCK ChannelDescription
    );

/*++

Routine Description:

    This routine registers a new DMA channel and attaches it to an existing
    DMA controller.

Arguments:

    ChannelDescription - Supplies pointer to information regarding the DMA
        channel being registered.

Return Value:

    STATUS_SUCCESS if the channel has been registered.

    STATUS_INVALID_PARAMETER if ChannelDescription is missing or if the
        channel cannot be linked to an existing DMA controller.

    STATUS_INSUFFICIENT_RESOURCES if memory cannot be allocated for the
        channel.

Environment:

    DISPATCH_LEVEL.

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

PVOID
HalSocGetAcpiTable (
    __in ULONG Signature
    );

/*++

Routine Description:

    This routine searches for and returns an ACPI table matching the given
    signature if one exists in the system. This function is only available
    during HAL initialization. Calls made after system initialization will
    always fail.

Arguments:

    Signature - Supplies the signature of the table.

Return Value:

    Returns a pointer to the table if one could be found.

    NULL if the table does not exist or the call is too late.

--*/

VOID
HalSetTimerProblem (
    __in PVOID TimerData,
    __in ULONG ProblemCode,
    __in NTSTATUS ProblemStatus
    );

/*++

Routine Description:

    This routine sets a problem code on a HAL timer extension. This has no
    functional impact but is quite useful for debugging.

Arguments:

    TimerData - Supplies a pointer to the timer's internal data. This is used
        only to identify the timer.

    ProblemCode - Supplies an unsigned integer representing a user-defined
        problem code.

    ProblemStatus - Supplies an optional NT status code associated with the
        problem.

Return Value:

    None.

--*/

VOID
RtlRaiseException (
    __in PEXCEPTION_RECORD ExceptionRecord
    );

/*++

Routine Description:

    This routine raises an exception.

Arguments:

    ExceptionRecord - Supplies an exception record identifying the location
        of the exception.

Return Value:

    None.

--*/


