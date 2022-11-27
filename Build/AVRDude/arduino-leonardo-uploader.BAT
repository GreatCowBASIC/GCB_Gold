@echo off
echo Great Cow BASIC 2018
echo Arduino Leonardo firmware upload starting.
rem based in https://stackoverflow.com/questions/43016993/how-can-i-force-a-leonardo-to-reset-with-avrdude?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

if %1.==. goto error
set hexfile=%1
set comportA=NONE
set comportB=NONE
if not exist %hexfile% goto error
for /f "usebackq" %%B in (`wmic path Win32_SerialPort Where "Caption LIKE '%%Leonardo%%'" Get DeviceID ^| FIND "COM"`) do set comportA=%%B
if %comportA%==NONE goto nodevice
echo Com Port for Arduino Leonardo device is detected as %comportA%.
echo Reset Leonardo Arduino into bootloader
mode %comportA%: baud=12 > nul
timeout  %2 > nul
for /f "usebackq" %%B in (`wmic path Win32_SerialPort Where "Caption LIKE '%%Leonardo%%'" Get DeviceID ^| FIND "COM"`) do set comportB=%%B
if %comportB%==NONE goto nobldevice
echo Com Port for Arduino Leonardo  bootloader device is detected as %comportB%.
echo.
echo Starting AVR Downloader/UploaDEr.....
avrdude -pm32u4 -cavr109 -D -P%comportB% -b57600 -Uflash:w:"%1"
goto upgradedone
:nodevice
echo Arduino Leonardo not found, connect a Arduino Leonardo.
goto end
:nobldevice
echo Reset into Arduino Leonardo bootloader failed, please try again...
goto end
:error
Echo Missing parameter or file, you should provide the full filename of an existing .hex file you want to use.
goto end
:upgradedone
echo.
echo Arduino Leonardo firmware upload completed.
:end
