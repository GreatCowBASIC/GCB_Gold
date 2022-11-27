echo off
Rem Batch to add Date and Time to user code
Rem     '#include "DateStamp.pls"
Rem     '_MyInit
Rem     'wait 100 ms
Rem     'HSerPrintCRLF 2
Rem     'HSerPrint "Build "
Rem     'HSerPrint buildString
Rem     'HSerPrintCRLF
Rem
Rem
Rem     Do not forget to delete the include.gcb file each time.
Rem
echo Processing
echo %~d1%~p1%~n1.gcb

cd  "%~d1%~p1"
echo dim buildNumber  as byte > %~n1.gcb
echo dim buildString  as string >> %~n1.gcb
echo Creating Date and Time Stamp
echo ' %date% %time% >> %~n1.gcb
echo Sub _MyInit >> %~n1.gcb
echo buildNumber=0 >> %~n1.gcb
echo buildString="%date% %time%" >> %~n1.gcb
echo End Sub>> %~n1.gcb
echo %date% %time%

