REM testcompilealldemos.bat
REM Version: GCBASIC.EXE build 1491 * or greater
REM Purpose:
REM   Compiles all .GCB files in the Demos subdirectory to test for compiler crashes
REM   and validate successful compilation to ASM and HEX files. Logs errors and
REM   invalid sources for debugging and verification.
REM
REM Assumptions:
REM   - Script is executed from the ..\GCStudio\GCBASIC directory.
REM   - Demos folder exists as a subdirectory (..\GCStudio\GCBASIC\Demos).
REM   - GCBASIC compiler is accessible in the system PATH or current directory.
REM   - Optional: Atmel AVR Toolchain (avrasm2.exe) path can be set in AS6 variable
REM     for additional AVR assembly validation.
REM
REM Inputs:
REM   - All .GCB files in the Demos folder and its subdirectories.
REM   - Files are considered valid if they contain:
REM     - '#chip' directive, OR
REM     - '#include <Uno_mega328p.h>' or '#include "Uno_mega328p.h"', OR
REM     - '#include <LGT8F328P.h>' or '#include "LGT8F328P.h"'.
REM   - Files with '#SKIPTEST' are skipped.
REM
REM Outputs:
REM   - test.log: Logs compilation errors, missing ASM/HEX files, and AVR errors (if AS6 is set).
REM   - invalidsources.log: Lists files lacking valid #chip or #include directives.
REM   - skipped.log: Lists files skipped due to #SKIPTEST.
REM   - compiled.asm, compiled.hex, compiled.html, compiled.lst: Temporary output files (deleted after each run).
REM   - %temp%\AVRErrors.txt: Temporary file for AVR assembler errors (if AS6 is set).
REM
REM Usage:
REM   1. Ensure GCBASIC is in PATH or the current directory.
REM   2. Optionally, uncomment and set AS6 to the Atmel AVR Toolchain path.
REM   3. Run the script from ..\GCStudio\GCBASIC:
REM      testcompilealldemos.bat
REM   4. Review test.log, invalidsources.log, and skipped.log for results.
REM
REM Notes:
REM   - Extended to support Uno_mega328p.h and LGT8F328P.h includes for files without explicit #chip.
REM   - Deletes temporary files (compiled.*) after each compilation.
REM   - Uses USE.INI for GCBASIC configuration (/S:USE.INI).
REM   - AVR validation checks for SREG in ASM and runs avrasm2.exe if AS6 is set.
REM
@echo off
cls
setlocal enabledelayedexpansion
REM set AS6=C:\Program Files (x86)\Atmel\Atmel Toolchain\AVR Assembler\Native\2.1.1175\avrassembler\

set FileType=gcb
echo Started @ %time% >test.log
echo Started @ %time% >invalidsources.log
echo Started @ %time% >skipped.log

set count=0

for /r %%f in (*.GCB) do (
    set /a count+=1
)

for /R demos\ %%F in (*.%FileType%) do (
  
  set "isValid=0"
  findstr /i "#chip" "%%F" >nul 2>nul && set "isValid=1"
  findstr /i "#include.*Uno_mega328p\.h" "%%F" >nul 2>nul && set "isValid=1"
  findstr /i "#include.*LGT8F328P\.h" "%%F" >nul 2>nul && set "isValid=1"
  if !isValid! equ 0 (
      echo %%F >> invalidsources.log
  ) ELSE (

REM  Commented out to only show errors in the log.
REM  echo "%%F">>test.log

  if exist errors.txt (
    del errors.txt > nul
  )

  del compiled.* /Q

  findstr /i "#SKIPTEST" "%%F" >nul 2>nul

  if errorlevel 1 (
    echo !count! files remaining

    GCBASIC "%%F" /NP /O:compiled.asm /A:GCASM /K:A /WX /S:USE.INI /P:
  ) ELSE (
    echo Ignored from test >compiled.asm
    echo Ignored from test >compiled.hex
    echo "%%F">>skipped.log
    del errors.txt  
  )

  if exist errors.txt (
    echo "%%F">>test.log
    REM copy /b test.log+errors.txt test.log
    for /f "tokens=* delims=" %%a in (errors.txt) do echo.    %%a>>test.log
  ) else (
    if "%AS6%" == "" (
        rem
    ) else (
        if exist compiled.asm (
            if exist %temp%\AVRErrors.txt del %temp%\AVRErrors.txt
            findstr /I "SREG" compiled.asm  >nul 2>nul
            if errorlevel 0 (
                "%AS6%\avrasm2.exe"  -v0 "compiled.asm" -I "%AS6%\Include"   1>%temp%\AVRErrors.txt 2>&1

                findstr /I "error" %temp%\AVRErrors.txt  >nul 2>nul

                if errorlevel 0 (
                    echo "%%F">>test.log
                    copy /b test.log+%temp%\AVRErrors.txt test.log  >nul 2>nul
                )
            )
        )
    )
    if not exist compiled.asm (
        echo "%%F">>test.log
        echo "Error: No ASM created" >> test.log
    )
    :checkhex
    if not exist compiled.hex (
        echo "%%F">>test.log
        echo "Error: No HEX created" >> test.log
    )
  )

  set /a count-=1
  
  )
)

echo Completed @ %time% >>test.log
echo Completed @ %time% >>invalidsources.log
echo Completed @ %time% >>skipped.log

del compiled.asm  >nul 2>nul
del compiled.hex >nul 2>nul
del compiled.html >nul 2>nul
del compiled.lst