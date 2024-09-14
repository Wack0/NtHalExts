# NT HAL extensions
This repository is for HAL extensions I write, for ARMv7 NT. Please note that headers and libraries from (Win8 M2-era) NT DDK are reproduced here for ease of building. To build, run `build.cmd` in MSVC cross-compile command prompt.

You need correct `CSRT` ACPI tables to load these.

## halextgit
HAL extension for ARM generic timers. Parses both the old-style (`EGIT`) and new-style (`GTDT`) ACPI tables.

Should work on build 789x+ but only tested on 7957+ for now.

`SetInterruptVector` function is provided as a no-op to work around a bug in Windows 8 M3 and below that can cause boot time bugchecks on some systems when using the HAL-provided Generic Interrupt Timer implementation.

Will not work on builds 8441-9200, due to a bad check in the HAL. For those builds, bring in [working prodsigned HAL from build 8400](https://msdl.microsoft.com/download/symbols/hal.dll/4fb707aa3a000/hal.dll).

## halextbcm2709
HAL extension for BCM2709 interrupt controllers (BCM2709 ARM local interrupt controller, and BCM2708 VC4 global interrupt controller). Backported from rs1-era HAL.

Interrupt controller HAL extensions have an interesting history, they were added in 789x but removed again in 799x, for builds since then we have to pattern match to find the functions that are no longer exported. Additionally various data structures changed over time, so that must be detected too.

All of this is done in `halextapi.c`, bugchecks happen on any kind of failure.

Tested HAL builds 7957, 8061, 8330, 8400; should work up to 9600 but untested.

Some M2-era builds are unstable, setting BCD `numproc` to use only 1 core helps.

Checked builds appear to be broken although patterns seem to work there.