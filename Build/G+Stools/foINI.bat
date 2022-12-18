@echo off
echo *** foINI ***
echo.
REM  <>
REM  This batchfile is called from G+Stool.exe to run the CGBasic-Compiler.
REM  %1 is replaced with the surcecode-file in double quotes i.e. "C:\My Folder\nice file.gcb"
REM  G+Stool.exe makes the GreatCowBasic folder the current folder.


rem *** set the filename and test
set filename=%1
set tmpstr=%filename:~-1%
rem *** If last char is double quote then the parameter is good
if '^%tmpstr% == '^" goto FilenameGood

rem *** build up the parameters back to a single parameter
If NOT "%2"=="" (
setlocal enableDelayedExpansion
Set filename=%filename% %2
)

If NOT "%3"=="" (
setlocal enableDelayedExpansion
Set filename=%filename% %3%
)

If NOT "%4"=="" (
setlocal enableDelayedExpansion
Set filename=%filename% %4%
)

If NOT "%5"=="" (
setlocal enableDelayedExpansion
Set filename=%filename% %5%
)

Set filename="%filename%"

:FilenameGood

REM  --- edit command below (don't delete /NP) -----------------
:compilecode

  REM Handle FLASH
  echo Search for existing hexFile, compile if hexFile is not found, then program.
  gcbasic.exe  %filename% /NP /S:use.ini /FO

REM time to read the messages
@timeout 2 > nul

REM  --- edit END ----------------------------------------------

REM  Add an minimum errorfile for others than compilererrors:
if %errorlevel% == 0 goto END
 echo An error occured - exit code: %errorlevel%
 echo An error occured - exit code: %errorlevel% >>Errors.txt
:END
