IPE-Pusher                  readme                               2015-11-10

 - Automizes programming PIC microprocessors with Microchips
   "Integrated Programming Einvironment"  IPE.

 - Tested with IPE 3.10 & 3.15 and PicKit3 (China-Clone).

 - IPE-Pusher works only, if 
   + IPE is running, 
   + a programmer and
   + a PIC is connected.

 - IPE-Pusher reads two commandline parameters:
   1. Path_to_hexfile 
   2. Keypress-delay_in_ms (maybe necessary on slower computers)
      The second parameter is optional, default is 0.
   Example:
   IPE-Pusher.exe "C:\My PIC Programs\GCB\BestProgEver.hex" 30

 - For use with GCB@Syn simply uncomment the related lines in
   ...\GCB@Syn\G@Stools\flashPIC.bat

 - Written with AutoIt 3   http://www.autoitscript.com
 - Exe compiled with AutoIt 3.11
