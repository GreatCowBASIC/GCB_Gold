@echo off
CLS
REM echo *** makeASM V2 ***
REM echo.

rem Unrem ATMELStudio variable below to enable AVR validation - this folder must point to the exe called avrasm2.exe
REM set ATMELStudio=C:\Program Files (x86)\Atmel\Atmel Toolchain\AVR Assembler\Native\2.1.1175\avrassembler\

REM  Great Cow BASIC Copyright 2007..2022
REM  This batchfile is called from G+Stool.exe to run the CGBasic-Compiler.
REM  %1 is replaced with the sourcecode-file in double quotes i.e. "C:\My Folder\nice file.gcb"
REM  G+Stool.exe makes the GreatCowBasic folder the current folder.
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

REM  Call GCBasic to make an asm-file only:

REM Tidyup any existing files
if exist errors.txt del errors.txt
if exist preprocesserror.txt del preprocesserror.txt
if exist filesofinterest.txt del filesofinterest.txt
if exist include\Inspecting_Libraries.h del include\Inspecting_Libraries.h

:CurrentFile

REM  --- edit command below (don't delete /NP) -----------------
:compilecode
REM  --- edit command below (don't delete /NP) -----------------
echo off
gcbasic.exe /NP /S:use.ini %filename% /P: /H:N

setlocal enableDelayedExpansion
set file=%filename%
FOR /F "delims=" %%i IN ("%file%") DO (
set filedrive=%%~di
set filepath=%%~pi
set filename=%%~ni
set fileextension=%%~xi
)

findstr "SREG" "%filedrive%%filepath%%filename%.asm" > NUL
if errorlevel 1 goto end



rem Echo "%var:~1,-5%.asm"
if "%ATMELStudio%" == "" (
  rem
 ) else (
echo Validation in ATMEL Studio
"%ATMELStudio%\avrasm2.exe"  -v0 "%filedrive%%filepath%%filename%.asm" -I "%ATMELStudio%\Include"   1>>Errors.txt 2>&1
rem @timeout 2 > nul
)
REM  --- edit END ----------------------------------------------


REM  Add a minimum errorfile for others than compilererrors:
if not errorlevel 1 goto END
 echo.>>Errors.txt
:END
if exist filesofinterest.txt del filesofinterest.txt
if exist include\Inspecting_Libraries.h del include\Inspecting_Libraries.h
