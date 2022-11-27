@echo off
echo *** flashAVR.bat ***
echo.
REM  <>
REM  This batchfile is called from G+Stool.exe if an AVR-chipmodel is detected and
REM  no "FlashThis.bat" is present in the sourcefile-folder.
REM  You can adapt this file to your needs.
REM  %1 is replaced with the hexfile in double quotes i.e. "C:\My Folder\nice file.hex"
REM  %2 is replaced with the chipmodel i.e. tiny44
REM  G+Stool.exe makes the GCB@Syn root-folder the current folder.


REM Changes @ Jan 2016
REM Added revised Mega2560 command and added some other commentry.
REM Changes @ August 2015
REM Changed AVRDude calls.  The first call prevents errors when trying to flash AVRs in delivery status (1MHz).
REM The second gives the maximum speed.
REM Both does not affect the speed of the firmware in most cheapy china programmers. But those with the actual
REM USBASP firmware (2011-05-28) benefit from. Actually most of the chinese USBASPs can be updated (needs two programmers).
REM Changes @Feb 2017 added timeout to MicroNucleus
REM Changes @Oct 2017 changed REM Call AVRdude for Arduino_Nano or Arduino_Pro_Mini bootloader

REM  --- Edit below and/or add your code below: ---------------------------------------

REM  Call AVRdude for USBASP programmer - moderate speed (187.5 kHz) for 1 MHz devices:
 "AVRdude\avrdude.exe" -B 5 -c USBasp -P Usb -p AT%2 -U flash:w:%1:i

REM  Call AVRdude for USBASP programmer - fastest mode (1500 kHz):
 rem "AVRdude\avrdude.exe" -B 0.5 -c USBasp -P Usb -p AT%2 -U flash:w:%1:i

REM  Call AVRdude for STK500v2 programmer:
 REM You will have to confirm the com (communications) port - currently set for com6
 rem "AVRdude\avrdude.exe" -c STK500v2 -P Com6 -p AT%2 -U flash:w:%1:i

REM  Call AVRdude for DASA3 programmer:
 REM You will have to confirm the com (communications) port - currently set for com6
 rem "AVRdude\avrdude.exe" -i 1 -c DASA3 -P Com6 -p AT%2 -U flash:w:%1:i

REM Call AVRdude for Arduino_Nano or Arduino_Pro_Mini bootloader:
 REM You will have to confirm the com (communications) port - currently set for com6
 rem "AVRdude\avrdude.exe" -c Arduino -P Com6 -b 115200 -p AT%2 -U flash:w:%1:i

REM Call AVRdude for Arduino_Nano or Arduino_Pro_Mini bootloader:
 REM You will have to confirm the com (communications) port - currently set for com6
 rem "AVRdude\avrdude.exe" -c Arduino -P Com6 -b 57600 -p AT%2 -U flash:w:%1:i

REM Call AVRdude for Arduino_Mega2560 bootloader - Alternative #1, see also below
 REM You will have to confirm the com (communications) port - currently set for com6
 rem "AVRdude\avrdude.exe" -c wiring -p m2560 -P COM6 -b 115200 -D -U flash:w:%1:i

REM  Call AVRdude for Arduino_Mega2560 bootloader  - Alternative #2, see above
 REM You will have to confirm the com (communications) port - currently set for com6
 rem "AVRdude\avrdude.exe" -c Wiring -P Com6 -b 115200 -p AT%2 -U flash:w:%1:i

Rem Call for Digispark bootloader:  remove the 'rem ' from the next three lines
 rem Echo ReInsert Digispark USB now
 rem "Micronucleus\micronucleus.exe" -run  %1 --timeout 5
 rem timeout 5 > nul

REM  --- End of your code. ------------------------------------


REM  Creating "G+Stools\FlashError.txt" makes G+Stool to leave the message-window open:

:FIN
if not errorlevel 1 goto END
 echo ERROR running: %0  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9 >"G+Stools\FlashError.txt"
 echo ERROR running: %0  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9
:END
