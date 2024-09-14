@echo off
cl /O1 /I . /D _ARM_=1 /GS- halextgit.c halextapi.c /link /DRIVER /DLL /SUBSYSTEM:native /OUT:halextgit.dll /ENTRY:HalExtensionInit /NODEFAULTLIB ntoskrnl.lib