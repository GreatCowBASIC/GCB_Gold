@ECHO Off
REM 1.02
set prompt=
SET FileToDelete="%temp%\PICTEST.TXT"
IF EXIST %FileToDelete% del /F %FileToDelete%

"..\PicKit2\pk2cmd.exe" -I -b"..\PicKit2" -pPIC%1 > %FileToDelete%
IF ERRORLEVEL 10 goto NoPK2
findstr PIC%1 %FileToDelete%

IF ERRORLEVEL 1 goto incorrectChip
goto GoodChip

echo what!

:incorrectChip
echo Incorrect PIC%1 microcontroller attached to PicKit2
echo Found
type %FileToDelete%
type %FileToDelete% >> errors.txt
goto fin

:NoPK2
echo Programmer Error no PicKit2.
echo Programmer Error. >> errors.txt
type %FileToDelete% >> errors.txt

type %FileToDelete%
GOTO FIN

:GoodChip

REM  Call PicKit2 command-line programming Software leaving power on the chip
"..\PicKit2\pk2cmd.exe" -b"..\PicKit2" -pPIC%1 -f%2 %3 %4 %5 %6 %7

:fin
IF EXIST %FileToDelete% del /F %FileToDelete%
exit /b %ERRORLEVEL%
