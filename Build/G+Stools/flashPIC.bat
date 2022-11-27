@echo off
echo *** flashPIC.bat ***
echo.
REM  <> - Added chip checking for PicKit2
REM  This batchfile is called from G+Stool.exe if an PIC-chipmodel is detected and
REM  no "FlashThis.bat" is present in the sourcefile-folder.
REM  You can adapt this file to your needs.
REM  %1 is replaced with the hexfile in double quotes i.e. "C:\My Folder\nice file.hex"
REM  %2 is replaced with the chipmodel i.e. 16F688
REM  G+Stool.exe always makes the GCB@Syn root-folder the current folder.


REM  --- Your code here: ---------------------------------------


REM Call XpressLoader for the Xpress Evaluation Board
REM A tool developed as part of Great Cow BASIC
 rem "XpressLoader\XpressLoader.exe" %1

REM Call the Northern Lights Programmer
REM A fast and effective programmer for all PICs
REM You will need to install the appropiate software to the ..ns folder.  See http://www.northernsoftware.com/nsdsp/ for latest software
 rem  "ns\nsprog.exe" p -d "PIC%2" -i %1 -v

REM Call uploader for TinyBootLoader+ bootloader:
 rem "TinyBootLoader\TinyMultiBootloader+.exe " %1 exit

REM  Call PICpgm-programming software:
 rem "PICPgm\picpgm.exe" -p %1
 rem "PICPgm\picpgm.exe" -delay 1 -if "5PiP" -port COM1 -pic PIC%2 -p %1
 rem "PICPgm\picpgm.exe" -delay 1 -if "JDM Programmer" -port COM1 -pic PIC%2 -p %1
 rem "PICPgm\picpgm.exe" -delay 1 -if "FT232Board-Programmer" -port USB -pic PIC%2 -p %1
 rem "PICPgm\picpgm.exe" -delay 1 -if "FT232Adapter-Programmer" -port USB -pic PIC%2 -p %1

REM PicKit Device Check
REM Remove the rem's from the next indented section to add PICKIT2 chip test check. Lines 27 thru 35.
 rem         "PicKit2\pk2cmd.exe" -I -b"PicKit2" -pPIC%2 > "PicKit2\pictest.txt"
 rem         findstr /C:"Device Name = PIC%2" "PicKit2\pictest.txt"
 rem         IF ERRORLEVEL 1 goto IncorrectChip
 rem         goto OKChipTest1
 rem         :IncorrectChip
 rem         echo Incorrect Chip Found: Not a %2  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9 >"G+Stools\FlashError.txt"
 rem         echo Incorrect Chip Found: Not a %2  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9
 rem         goto END
 rem         :OKChipTest1

REM  Call PicKit2 command-line programming Software leaving power on the chip
REM  and always exit with MCLR on. Optionally remove -r for MCLR off
 "PicKit2\pk2cmd.exe" -b"PicKit2" -pPIC%2 -f%1 -m -j -t -r

REM  Call PicKit2 command-line programming Software without leaving the power on the chip
REM  and always exit with MCLR on. Optionally remove -r for MCLR off
 rem "PicKit2\pk2cmd.exe" -b"PicKit2" -pPIC%2 -f%1 -m -r

REM Call pk3cmd.exe WITH RELEASE FROM RESET
 rem if exist "MPLAB_IPE\PK3cmd.exe" cd MPLAB_IPE & "PK3cmd.exe"  -P%2 -F%1 -L

REM Call pk3cmd.exe WITH RELEASE FROM RESET
 rem if exist "MPLAB_IPE\IPEcmd.exe" cd MPLAB_IPE & "IPEcmd.exe"   -TPPK3  -P%2 -M -F%1 -OL -Z

REM Call pk3cmd.exe WITH RELEASE FROM RESET - DO NOT PROGRAM EEPROM
REM if exist "MPLAB_IPE\PK3cmd.exe"  "MPLAB_IPE\PK3cmd.exe"  -P%2 -F%1 -L -M PICB



REM  Call PicKit3 command-line programming software:
REM  note, that the Java Runtime Environment (jre) has to be installed!
 rem java -jar "C:\Program Files\Microchip\IPE\ipecmd.jar" -TPPK3 -P%2 -M -F%1

REM  Use PicKit3 Integrated Programming Environment GUI (IPE):
REM  note, that IPE must run and a programmer and a PIC is connected!
 rem  "PicKit3\IPE-Pusher.exe" %1 0

REM  Call uploader for diolan bootloader:
 rem  "Bootloader\picboot" -r %1

REM  Call mikroProgSuitePIC-programming software:
 rem  "C:\WinSpr\mikroProgSuitePIC\mikroProg Suite for PIC.exe" -q -w -pPIC%2 -v -f%1

REM  Call MicroBrn K150 programming software:
 rem "K150\microbrn.exe" %1 %2 /f /Q 1

REM  --- End of your code. -------------------------------------
:FIN
REM  Creating "G+Stools\FlashError.txt" makes G+Stool to leave the message-window open:
if not errorlevel 1 goto END

 echo ERROR running: %0  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9 >"G+Stools\FlashError.txt"
 echo ERROR running: %0  with parameters  %1 %2 %3 %4 %5 %6 %7 %8 %9
:END
