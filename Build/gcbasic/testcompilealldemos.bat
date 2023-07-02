echo off
REM  Version 0.98.* or greater
REM  Adapted to test for compiler crash
REM  added AVR check

rem set AS6=C:\Program Files (x86)\Atmel\Atmel Toolchain\AVR Assembler\Native\2.1.1175\avrassembler\

set FileType=gcb
echo Started @ %time% >test.log
echo Started @ %time% >invalidsources.log
echo Started @ %time% >skipped.log

for /R demos\ %%F in (*.%FileType%) do (
  findstr /i "#chip" "%%F" >nul 2>nul
  if errorlevel 1 (
      echo "%%F: Invalid GCB Source" >> invalidsources.log
  ) ELSE (

REM  Commented out to only show errors in the log.
REM  echo "%%F">>test.log

  if exist errors.txt (
    del errors.txt > nul
  )

  ..\G+Stools\gawk.exe -f ..\G+Stools\preprocess.awk "%%F"
  if exist errors.txt (
    copy /b test.log+errors.txt test.log
  )
  del compiled.* /Q

  findstr /i "#Skiptest" "%%F" >nul 2>nul
  if errorlevel 1 (
    GCBASIC "%%F" /NP /O:compiled.asm /A:GCASM  /K:A /WX
  ) ELSE (
    echo Ignored from test >compiled.asm
    echo Ignored from test >compiled.hex
    echo "%%F">>skipped.log

  )

  if exist errors.txt (
    echo "%%F">>test.log
    copy /b test.log+errors.txt test.log

  ) else (

       if "%AS6%" == "" (
          rem
            ) else (

          setlocal ENABLEDELAYEDEXPANSION
         if exist compiled.asm (
            if exist %temp%\AVRErrors.txt del %temp%\AVRErrors.txt
            findstr /I "SREG" compiled.asm  >nul 2>nul
            rem echo !errorlevel!

            if !errorlevel! EQU 0  (
            "%AS6%\avrasm2.exe"  -v0 "compiled.asm" -I "%AS6%\Include"   1>%temp%\AVRErrors.txt 2>&1

            findstr /I "error" %temp%\AVRErrors.txt  >nul 2>nul

            if !errorlevel! EQU 0 (
                    echo "%%F">>test.log
                    copy /b test.log+%temp%\AVRErrors.txt test.log  >nul 2>nul
                  )

            )



         )

         endlocal
      )
      if not exist compiled.asm (
        echo "%%F">>test.log
        echo "Error: No ASM created" >> test.log
      )
      :checkhex
      echo off
      if not exist compiled.hex (
        echo "%%F">>test.log
        echo "Error: No HEX created" >> test.log
      )

  )


  )



)

echo Completed @ %time% >>test.log
echo Completed @ %time% >>invalidsources.log
echo Completed @ %time% >>skipped.log

del compiled.asm  >nul 2>nul
del compiled.hex >nul 2>nul
del compiled.html >nul 2>nul
del compiled.lst >nul 2>nul
