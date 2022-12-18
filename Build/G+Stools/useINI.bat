@echo off
CLS
REM echo Make Hex and Program (useINI)
REM echo.

rem Unrem ATMELStudio variable below to enable AVR validation - this folder must point to the exe called avrasm2.exe
REM set ATMELStudio=C:\Program Files (x86)\Atmel\Atmel Toolchain\AVR Assembler\Native\2.1.1175\avrassembler\


REM  Great Cow BASIC Copyright 2007..2022
REM  Updated Feb 2022
REM  Updated to support GCCODE IDE
REM  This batchfile is called from G+Stool.exe to run the CGBasic-Compiler.
REM  %1 is replaced with the sourcecode-file in double quotes i.e. "C:\My Folder\nice file.gcb"
REM  G+Stool.exe makes the GreatCowBasic folder the current folder.
REM echo Great Cow BASIC Pre-processing (Copyright 2007..2022)

REM revised Feb 2020 to improve performance.  Used dummy .h file called Inspecting_Libraries.h to ensure no error message is issued

rem *** set the filename and test
set filename=%1
set tmpstr=%filename:~-1%
rem *** If last char is double quote then the parameter is good
if '^%tmpstr% == '^" goto FilenameGood

:rebuildfilename
shift
if "%1"=="" goto endoffilerebuild
setlocal enableDelayedExpansion
Set filename=%filename% %1
goto rebuildfilename

:endoffilerebuild

Set filename="%filename%"

:FilenameGood

REM Tidyup any existing files
if exist errors.txt del errors.txt
if exist preprocesserror.txt del preprocesserror.txt
if exist filesofinterest.txt del filesofinterest.txt
if exist include\Inspecting_Libraries.h del include\Inspecting_Libraries.h


REM  --- edit command below (don't delete /NP) -----------------
:compilecode

  REM Handle HEX&FLASH
REM  echo Make Hex and Program
  gcbasic.exe  %filename% /NP /S:use.ini /F:N

setlocal enableDelayedExpansion

set file=%filename%
FOR /F "delims=" %%i IN ("%file%") DO (
set filedrive=%%~di
set filepath=%%~pi
set filename=%%~ni
set fileextension=%%~xi
)

if "%ATMELStudio%" == "" (
  rem DO NOTHING
 ) else (
findstr "SREG" "%filedrive%%filepath%%filename%.asm"
if errorlevel 1 goto fin1

echo Validation in ATMEL Studio
"%ATMELStudio%\avrasm2.exe"  -v0 "%filedrive%%filepath%%filename%.asm" -I "%ATMELStudio%\Include"   1>>Errors.txt 2>&1
rem @timeout 2 > nul
 )
REM time to read the messages
::@timeout 2 > nul

:FIN1
REM  --- edit END ----------------------------------------------

REM  Add a minimum errorfile for others than compilererrors:
if %errorlevel% == 0 goto END
 echo An error occured - exit code: %errorlevel%
 echo An error occured - exit code: %errorlevel% >>Errors.txt
:END
if exist filesofinterest.txt del filesofinterest.txt
if exist include\Inspecting_Libraries.h del include\Inspecting_Libraries.h
