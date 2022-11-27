#Region ;**** Directives created by AutoIt3Wrapper_GUI ****
#AutoIt3Wrapper_Icon=IPE-Pusher.ico
#AutoIt3Wrapper_Run_Au3Stripper=y
#EndRegion ;**** Directives created by AutoIt3Wrapper_GUI ****
;- IPE-Pusher.au3 --------------------------------------------- >>fst'15<< -
; - Automizes programming PIC microprocessors with Microchips
;   "Integrated Programming Einvironment"  IPE.
; - It needs, that IPE is running, a programmer and a PIC is connected.
; - Start with commandline parameters:
;    Path_to_hexfile    Keypress-delay_in_ms (optional; default=0)
;    i.e. IPE-Pusher.exe "C:\My PIC Programs\GCB\BestProgEver.hex" 30
; - Written for AutoIt 3   http://www.autoitscript.com
; - Exe compiled with AutoIt 3.11
;- V 20151110 -------------------------------- http://www.FrankSteinberg.de -

$sTitel = "IPE-Pusher    20151110    >>fst'15<<"

;*** Errorhandling, if no commandline-parameter:
If $CmdLine[0] = 0 Then
  MsgBox(64, $sTitel, 'IPE-Pusher (re)programs a PIC microcontroller using a RUNNING' & @CRLF & _
                      'copy of Microchips "Integrated Programming Environment":'  & @CRLF & @CRLF & _
					  '  a) start ipe.jar' & @CRLF & _
					  '  b) choose PIC' & @CRLF & _
					  '  c) connect programmer' & @CRLF & _
					  '  d) run IPE-Pusher.exe with commandline parameters:' & @CRLF & _
					  '       Path_to_hexfile     Keypress-delay_in_ms (optional; default=0)' & @CRLF & _
					  '         OR' & @CRLF & _
					  '       use GCB@Syns programming batchfile flashPIC.bat' & @CRLF & _
					  '  e) repeat d) for reprogramming your PIC')
  Exit(1)
EndIf

;*** Handling for Keypress-Delay:
$iDelay = 0
If $CmdLine[0] > 1 Then $iDelay = $CmdLine[2]
If $iDelay > 1000 Then $iDelay = 1000
AutoItSetOption ("SendKeyDelay" ,$iDelay)

;*** Test for IPE running:
$iResult = WinExists("Integrated Programming Environment", "")
If $iResult = 0 Then
	MsgBox(16, $sTitel, 'Please start Microchips "Integrated Programming Environment" ' & @CRLF & 'and connect to PIC & PicKit3 !')
	Exit(2)
EndIf

;*** Activate IPE window:
WinActivate("Integrated Programming Environment", "")

;*** Wait for IPE detecting a modified hexfile (new in IPE version 3.05):
$iResult = WinWaitActive("File modified", "", 2) ;wait max. 2 Sec. for Window "File modified"
If $iResult Then                                 ;if exist ...
  Send ("!j")
  WinWaitClose("File modified", "", 2)           ;wait for additional window to close
 ;*** Otherwise put path_to_hexfile to IPE and start programming:
Else
  ;*** Call IPE-mainwindow:
  WinWaitActive("Integrated Programming Environment", "", 2)
  Send ("!b")                                    ;send Alt+B to browse for hexfile
  WinWaitActive("[CLASS:SunAwtDialog]", "", 2)   ;wait for additional window to open
  Send ($CmdLine[1])                             ;put in path and filename
  Send("{Enter}")                                ;send Enter
  WinWaitClose("[CLASS:SunAwtDialog]", "", 2)    ;wait for additional window to close                                       ;programming is finished here
 EndIf

;*** Start programming:
WinActivate("Integrated Programming Environment", "")
WinWaitActive("Integrated Programming Environment", "", 2)
Send ("!{F5}")                                   ;send Alt+F5 to program PIC
Exit(0)