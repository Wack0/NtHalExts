@echo off
cl /O1 /I . /std:c17 /D _ARM_=1 /GS- halextbcm2709.c halextapi.c /link /DRIVER /DLL /SUBSYSTEM:native /OUT:halextbcm2709.dll /ENTRY:HalExtensionInit /NODEFAULTLIB ntoskrnl.lib