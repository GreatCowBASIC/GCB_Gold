What is it?

Terminal is a simple serial port (COM) terminal emulation program. It can be used for communication with different devices such as modems, routers, embedded uC systems, GSM phones, GPS modules... It is very useful debugging tool for serial communication applications.

Features
?without installation, only single and small .exe file ~300KB
?simple file send
?Rx and Tx characters counter 
?baudrate up to 256kbps & custom baudrate

?up to 64 COM ports

?log to file (hex & string)
?24 custom transmit macros with auto repeat function

?scripting (with graph/visualization support)
?remote control over TCP/IP - telnet
?run applications from macro commands
?open www sites from macro commands
?request/response macros
?CSV Graph - As a little 2012 New Year's Gift ;)
?4th/black graph and scale factors for CSV values

?auto scroll checkbox - to prevent auto scrolling
?WriteToFile() procedure in scripting
?%SCRS"script.tsc" and %SCRE commands for macros - check help

?offset for %SUM and %XOR macro commands
?time stamp, scale factors and offsets for CSV graph
?Quit() function in scripting
?parameter/argument to open script file at start up
?Tx char delay strategy changed (real zero delay if value=0) 
?hot key changes: DEL=RXClear, ESC=send code 27
?flush buffers on disconnect to avoid "not responding"
?NEW: macro up to 256 characters
?FIX: lost chars problem...better but not fixed
?FIX: ComSetDTR,ComSetRTS,...LED turn on/off bug

!!! If you have problems with new version you can still download older releases on the bottom of the page !!!


Macros

In macros you can use all characters from keyboard and any ASCII char if you use $xx or #xxx. Where $xx is hex and #xxx dec format of ascii code. If you want to use # or $ char in macro you should type it twice ($$=$ and ##=#).To calculate XOR checksum byte use %XORxx command. To calculate SUM (1byte sum) checksum byte use %SUMxx command. Where xx is offset of first byte for calculation.
 To insert delay in macro string use %DLYxxxx, where xxxx is value 0000-9999 in ms.

You can store macro in macro file. Active macro is "saved" even if you don't save it and will be available next time when you'll start Terminal. Macro string can be up to 256 characters long.

some examples:

X1##Y2##Z3##$0D$0A - this will send 'X1#Y2#Z3#"CR""LF"'


ABC%DLY1000DEF - this will send ABC and after ~1000ms DEF


%RUN"cmd.exe" - this will run command line prompt


%URL"http://www.google.com" - guess what?! ;)

%URL"mailto:xyz@xymail.com?subject=terminal&body=test" - 
compose mail (without sending)

%M03 - this will send/run macro #3 (there are some limits when using this)

%LOGS - start logging

%LOGE - stop logging

%SCRS"script.tsc" - load and start script 




Scripting
Simple pascal syntax scripting is possible. You can write short scripts to do some simple tasks. More about supported command you can find if you click "Help" button.
Below in attachment section you can find few sample scripts to check and test functionality.


TCP/IP remote control
Terminal can also act like telnet server and listen on selected TCP port. You can connect to it with any telnet client program from another computer in network (or over internet from different location) and see what's going on in terminal and send commands etc. 