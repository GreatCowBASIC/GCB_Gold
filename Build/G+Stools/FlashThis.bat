@echo off
echo *** flashThis.bat ***
echo.
REM  <>
REM  "FlashThis.bat" is called from G&Stool.exe if present in the same folder as the
REM  sourcecode-file. If not, "G+Stools\flashAVR.bat" or "G+Stools\flashAVR.bat" is
REM  called (depending on detected chipmodel). "FlashThis.bat" is useful for flashing
REM  specific projects a particular way.
REM  Copy this file to a sourcecode-folder and adapt to your needs.
REM  %1 is replaced with the hexfile in double quotes i.e. "C:\My Folder\nice file.hex"
REM  %2 is replaced with the chipmodel i.e. tiny44
REM  G+Stool.exe makes the GCB@Syn root-folder the current folder.


REM  Call your programming software:

REM  --- Your code here: ---------------------------------------

"AVRdude\avrdude.exe" -c USBasp -P usb -p AT%2 -U flash:w:%1:i

REM  --- End of your code. -------------------------------------


REM  Creating "G+Stools\FlashError.txt" makes G+Stool to leave the message-window open:
if not errorlevel 1 goto END
 echo ERROR running: %0  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9 >"G+Stools\FlashError.txt"
 echo ERROR running: %0  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9
:END
