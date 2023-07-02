'-- G+Stool.bas ------------------------------------------------ >>fst'22<< -
' - Tool for Great Cow BASIC SynWrite-IDE (named GCB@Syn)
' - calls different BatchFiles for compiling with gcbasic.exe
' - and flashing with customizable programmer/software
' - G+Stool is intended to be called from SynWrites
'   'Tools->External Tools' menu
' - does text output to console and a message-window
' - compiled with FreeBasic 1.09.0 (parameter -s console)
' - needs library https://sourceforge.net/projects/guiwindow9/
'-- V 20220127 ------------------------------- https://www.FrankSteinberg.de -
' Changes:
' 2018-05-12:
' - added handler for GCBProgrammer in the programmer log file
' 2019-11-03:
' - added parsing/displaying Oscillator-Info from GCB logfile
' 2019-12-23
' - added: Store/reset last window position (..\G+Stools\G+Stool.ini)
' 2020-04-10
' - complete rewrite of displaying text on output window
' - small bugfix related to handling of programmer errors
' 2020-09-21
' - complete rewrite of calculating AVR program memory from hexfile
'   (now identical to avrdude message)
' - displaying available program memory for AVR
' - errormessage if program memory for AVR insufficient
' 2022-01-27
' - FreeBasic's internal Beep command replaced by API-call
' - some code cosmetic
' - compiled with FreeBasic 1.09.0 and "Gui Library window9" 2022-01-23
' - parse reportfile for random promotion messages and show in output window
' 2023-07-1
' GCBASIC port

#Include "file.bi"
#Include "vbcompat.bi"
#include "window9.bi"

Declare Sub MakeWindow()
Declare Sub WriteToWindow()
Declare Sub ErrorHandling(ByVal As String)
Declare Sub runGCB(ByVal As String)
Declare Sub PathAsmFile()
Declare Sub PathHexFile()
Declare Sub CheckWinVer()
Declare Sub CheckForHex()
Declare Sub CheckForNewHex()
Declare Sub CheckForParameters()
Declare Sub CheckForIni()
Declare Sub ParseReportFile()
Declare Sub runProgrammer()
Declare Sub CheckGCBErrors()
Declare Sub CheckFlashError()
Declare Sub DelProgammerLog()
Declare Sub ShowProgammerLog()
Declare Sub StayOpen()
Declare Function BytesInHex() As UInteger
Declare Function StringReplace(ByVal As String, ByVal As String, ByVal As String) As String

Dim Shared As String sTitle, sWinVer, sPara, sSource, sAsmFile, sHexFile, sChipModel, sPathGCBSyn, sRunGCBbat, sFlashXbat, sReport, sProgLog
Dim Shared As Double dTimerStart, dHexFileTime
Dim Shared As Integer iEvent, iStayOpen
Dim Shared As Byte bErrorFlag
Dim Shared As HWND hWnd

'*** Title of this program:
sTitle = " G+Stool      V20230701      www.FrankSteinberg.de"

'*** Store the GCB@Syn root-folder:
sPathGCBSyn = Left(ExePath, (InStrRev(ExePath, "\") - 1))
'MessageBox(null, WStr(sPathGCBSyn), "debug", MB_OK Or MB_ICONINFORMATION)

'*** First argument is sourcefile, second is Chipmodel,
'    third is "stayopen" (if messagewindow shall always stay open at program end)
sPara = LCase(Command(1))
sSource = Command(2)
If LCase(Command(3)) = "stayopen" Then IStayOpen = TRUE

'*** Check G+Stool.exe commandline-arguments:
CheckForParameters()

'*** Errormessage, if gcbasic.ini is present (incompatible):
'CheckForIni()

'*** Create Message Window ***
MakeWindow()

'** Read Windows (main-) version from console command "ver":
CheckWinVer()

'** Store start time:
dTimerStart = Timer

'*** Mainprogram:
Select Case sPara
  Case "asm"               'user wants to compile to asm only ...
    runGCB("makeASM.bat")  'run "makeASM.bat" and do some console output
    WriteToWindow()        'print the programs output to messagewindow
    PathAsmFile()          'determine asm-file path/name
    ParseReportFile()      'get info from CGBs reportfile
    CheckGCBErrors()       'if errors, let messagewindow open and do some console output
  Case "hex"               'user wants to compile to hex ...
    runGCB("makeHEX.bat")  'run "makeHEX.bat" and do some console output
    WriteToWindow()        'print the programs output to messagewindow
    PathAsmFile()          'determine asm-file path/name
    PathHexFile()          'determine hex-file path/name
    ParseReportFile()      'get info from CGBs reportfile
    CheckGCBErrors()       'if errors, let messagewindow open and do some console output
  Case "hexflash"          'user wants to compile to hex and flash the hex then ...
    PathAsmFile()          'determine asm-file path/name
    PathHexFile()          'determine hex-file path/name
    dHexFileTime = FileDateTime(sHexFile)  'get last-modified-time of hexfile
    runGCB("makeHEX.bat")  'run "makeHEX.bat" and do some console output
    WriteToWindow()        'print the programs output to messagewindow
    ParseReportFile()      'get info from CGBs reportfile
    CheckGCBErrors()       'if errors, let messagewindow open and do some console output
    CheckForNewHex()       'check, if a NEW hexfile is created
    CheckForHex()          ' and check if exist
    DelProgammerLog()      'delete old Programmer log- and errorfiles
    runProgrammer()        'call one of the batchfiles that run the programer-software
    WriteToWindow()        'print the programs output to messagewindow
    ShowProgammerLog()     'show info from Programmer.log file (generated by PicKitPlus CL)
    CheckFlashError()      'if errors, let messagewindow open and do some console output
  Case "flash"              'user wants to flash a previous made hex ...
    PathHexFile()          'determine hex-file path/name
    CheckForHex()          'determine hexfile-name and check if exist
    ParseReportFile()      'get info from CGBs reportfile
    DelProgammerLog()      'delete old Programmer log- and errorfiles
    runProgrammer()        'call one of the batchfiles that run the programer-software
    WriteToWindow()        'print the programs output to messagewindow
    ShowProgammerLog()     'show info from Programmer.log file (generated by PicKitPlus CL)
    CheckFlashError()      'if errors, let messagewindow open and do some console output
  Case "useini"            'user wants to compile and flash using the prefs.dat ...
    DelProgammerLog()      'delete old Programmer.log file (generated by PicKitPlus CL)
    runGCB("useINI.bat")   'run "useINI.bat" and do some console output
    WriteToWindow()        'print the programs output to messagewindow
    PathAsmFile()          'determine asm-file path/name
    PathHexFile()          'determine hex-file path/name
    ParseReportFile()      'get info from CGBs reportfile
    CheckGCBErrors()       'if errors, let messagewindow open and do some console output
    ShowProgammerLog()     'show info from Programmer.log file (generated by PicKitPlus CL)
    CheckFlashError()      'if errors, let messagewindow open and do some console output
  Case "hexini"            'user wants to compile and flash using the prefs.dat ...
    runGCB("hexINI.bat")   'run "hexINI.bat" and do some console output
    WriteToWindow()        'print the programs output to messagewindow
    PathAsmFile()          'determine asm-file path/name
    PathHexFile()          'determine hex-file path/name
    ParseReportFile()      'get info from CGBs reportfile
    CheckGCBErrors()       'if errors, let messagewindow open and do some console output
  Case "foini"             'user wants to compile and flash using the prefs.dat ...
    DelProgammerLog()      'delete old Programmer.log file (generated by PicKitPlus CL)
    runGCB("foINI.bat")    'run "foINI.bat" and do some console output
    WriteToWindow()        'print the programs output to messagewindow
    PathAsmFile()          'determine asm-file path/name
    PathHexFile()          'determine hex-file path/name
    ParseReportFile()      'get info from CGBs reportfile
    CheckGCBErrors()       'if errors, let messagewindow open and do some console output
    ShowProgammerLog()     'show info from Programmer.log file (generated by PicKitPlus CL)
    CheckFlashError()      'if errors, let messagewindow open and do some console output
End Select

'*** Print duration of programm execution:
Print Using "Duration: ###.#  Seconds."; Timer - dTimerStart

'*** Let messagewindow stay open after an error, or if commandline argumet = "stayopen"
If iStayOpen Then StayOpen()

'*** Store window position to file:
Open sPathGCBSyn + "\G+Stools\G+Stool.ini" For Output As #4
Print #4, Str(WindowX(hwnd))
Print #4, Str(WindowY(hwnd))
Close #4

'*** Mainprogram END
Close_Window(hwnd)
End


'*** Create the messagewindow (using VANYAs window9 library):
Sub MakeWindow()

 Dim As String sOneLine
 Dim As Integer PosX, PosY

 '** Restore last window position:
 Open sPathGCBSyn + "\G+Stools\G+Stool.ini" For Input As #4
 If Err = 0 Then
   Line Input #4, sOneLine
   PosX = Val(sOneLine)
   Line Input #4, sOneLine
   PosY = Val(sOneLine)
   Close #4
   If (PosX > 1800) Or (PosX < -400) Then PosX = 210:PosY = 26
   If (PosY > 900)  Or (PosY < 0)    Then PosX = 210:PosY = 26
  Else
   PosX = 210
   PosY = 26
 EndIf


 '** Create window:
 hWnd=OpenWindow(sTitle,PosX,PosY,618,458)            'mainwindow
 ButtonGadget(2,5,386,594,30, "c l o s e")           'make close-button
 EditorGadget(1,5,5,594,374)                         'make textwindow
 'SetGadgetFont(1,LoadFont("Courier New",8))          'use nonproportional font
 SetGadgetFont(1,CINT(LoadFont("Courier New",8)))
 SetGadgetColor(1,&HFFFFFF,0,1)                      'white background
 ReadOnlyEditor(1,1)                                 'readonly, no writing
 'SetGadgetText(1,sTitle+Chr(10)+" Please wait ...")  'put text on textwindow
 PasteEditor(1, "***" + sTitle + "  ***" +Chr(10)+Chr(10), FALSE)

End Sub


'*** Write programm-output to messagewindow:
Sub WriteToWindow()

 Static As String sLine          'recent read line
 Static As String*1 sCharOld     'char from last loop
 Dim As String*1 sChar           'the recent read char
 '** Define signs that cause a write to window:
 Const TextEndSigns As String = Chr(10) + Chr(13) + "#*+=-.:-_/\|<>"

 ReadOnlyEditor(1,0)             'undo works only whithout readonly

 Do While Not Eof(1)             'output comes from open pipe

   sLine=""
   sCharOld = sChar              'save line-end sign from last loop
   Do
     sChar = Input (1, #1)       'read one char
     sLine = sLine + sChar       'put recent char to line
   'Loop Until sChar=Chr(10) Or sChar=Chr(13)
   Loop Until InStr(TextEndSigns, sChar)

   '** simulate Carriage-Return behavior like in console window:
   If sCharOld = Chr(13) Then UndoEditor(1)

   '** Add line to window:
   'PasteEditor(1, "[" + Str(Len(sLine)) + "]" + "<" + Str(Asc(sChar)) + ">" + sLine, TRUE)
   PasteEditor(1, sLine, TRUE)

   '** Scroll to bottom, after new line:
   If sChar = Chr(10) Then
     LineScrollEditor(1, 2)  'scroll to bottom line
     'sleepw9(10)
   EndIf

  iEvent=WindowEvent                 'look for user input
  If iEvent=EventClose Then Exit Do  'make possible to break
  If iEvent=EventGadget And EventNumber()=2 Then Exit Do
  If EventKEY = 27 Then Exit Do

 Loop

 ReadOnlyEditor(1,1)  'make readonly again

 Close #1             'close pipe

End Sub


'*** Call the batchfile that runs the compiler:
Sub runGCB(ByVal sBatFileName As String)

  'make full path of batchfile:
  sRunGCBbat = sPathGCBSyn+"\G+Stools\"+sBatFileName
  'console output:
  Print Time + "    G+Stool started with parameter '" + sPara + "'   ->   processing   " + sRunGCBbat
  Print "Source-File  =  " + sSource
  'errormessage, if no batchfile:
  If Not FileExists(sRunGCBbat) then  ErrorHandling("Missing  '" + sRunGCBbat + "'")
  'go to GreatCowBasic - folder:
  ChDir sPathGCBSyn + "\GCBASIC"
  'kill old errormessage-file, created by gcbasic.exe:
  Kill "Errors.txt"
  'start timer:
  'dTimerStart = Timer
  'run compiler-batchfile with argument1 = sourcefile, pipe output to G+Stool:
  'pipe output to G+Stool. " 2>&1" redirects the screen-output from STDERR to STDOUT
  '(needed for avrdude):
  If sWinVer = "4" Then  'path with single double-quotes for older Windows
    Open Pipe Chr(34)+sRunGCBbat+Chr(34)+" "+Chr(34)+sSource+Chr(34)+" 2>&1" For Input As #1
   Else                   'path with double-double-quotes for Windows > WinME
    Open Pipe Chr(34)+Chr(34)+sRunGCBbat+Chr(34)+" "+Chr(34)+sSource+Chr(34)+Chr(34)+" 2>&1" For Input As #1
  EndIf
  'go back to GCB@Syn-rootfolder:
  ChDir sPathGCBSyn

End Sub


'*** If GCB reports an error, do console output and let messagewindow stay open:
Sub CheckGCBErrors()
  'gcbasic.exe reports no errorlevel, so the only way to detect an compile-error is,
  'to check, if the file Errors.txt is created:
  '(Hint: Under Win98 maybe a nullbyte-Errors.txt is created!)

  'If FileExists(sPathGCBSyn+"\GCBASIC\Errors.txt") Then
  If FileLen(sPathGCBSyn+"\GCBASIC\Errors.txt") Then
    MessageBeep (&h00000010)
    'Print Using "###.#  Sec."; Timer - dTimerStart;  'show duration for programm execution
    Print ">>>  WARNINGs / ERRORs reported by Great Cow BASIC  (if Syntax Error, doubleclick on the errormessage below)  <<<"
     'a 2-byte errorfile is made by the batchfiles itself, so no "Doubleclick ..." message:
     'If FileLen(sPathGCBSyn+"\GCBASIC\Errors.txt") > 3 Then
     'Print "Doubleclick on blue errormessage below to go to sourcecode-line:"
     'print Errors.txt to console, so that we can analyze is in Synwrites output-window:
     Open sPathGCBSyn+"\GCBASIC\Errors.txt" For Input As #3
     Do While Not Eof(3)
      Print Input(1, #3);
     Loop
     Close #3
     iStayOpen = TRUE  'let messagewindow stay open
     bErrorFlag = 1    'set flag, that marks an error from GCB
     'EndIf
    Else                         'if no compile-error ...
     'Print Using "###.# Sec."; Timer - dTimerStart;  'show duration for programm execution
     'Print "    compiling o.k."  'print message to console
     Print "Target-File  =  ";
     If sPara = "asm" Then Print sAsmFile Else Print sHexFile
     'Print Using "###.#  Sec."; Timer - dTimerStart;  'show duration for programm execution
     Print sReport
  End If

End Sub


'*** Check, if GCB made a new hexFile; end otherwise:
Sub CheckForNewHex()

  'Print FileDateTime(sHexFile)
  'Print dHexFileTime
  'Print Using "##.#############"; FileDateTime(sHexFile) - dHexFileTime

  'End if no new hexfile is created:
 If (FileDateTime(sHexFile) - dHexFileTime) < 0.000001  Then
   Print "FLASHing aborted; no new hex-file created!"
   iStayOpen = TRUE  'let messagewindow stay open
   StayOpen()        'call messagewindow
   End               'end G+Stool, no further commands
 End If

End Sub


'*** Determine chipmodel and other info from GCBs reportfile (html or txt)
'    (most programming software needs chipmodel to flash the microcontroller):
Sub ParseReportFile()

  Dim As String sOneLine, sRepFile, sRepFile2, sFile2Compare, sComp, sPromo, sMem, sRam, sOSC, sError
  Dim As Double dRepFileTime, dRepFileTime2

  '** get report-filename:
  sRepFile  = Left(sSource, (InStrRev(sSource, "."))) + "html"
  sRepFile2 = Left(sSource, (InStrRev(sSource, "."))) + "report.txt"

  '** get reportfile-times:
  dRepFileTime  = FileDateTime(sRepFile)
  dRepFileTime2 = FileDateTime(sRepFile2)

  'default message, if no reportfile
  'sReport = "    compiling o.k."
  'If dRepFileTime + dRepFileTime2 <= 0 Then Exit Sub

  '** choose newer reportfile:
  If dRepFileTime < dRepFileTime2 Then
    dRepFileTime = dRepFileTime2
    sRepFile = sRepFile2
  End If

  '** determine, if reportfile has same time as ASMfile (can be older max. 3 sec.):
  If sPara = "asm" Then sFile2Compare = sAsmFile Else sFile2Compare = sHexFile  'no hexfile if compiled to asm only
  If FileDateTime(sFile2Compare) - dRepFileTime > 0.00003 Then
    'errormessage, when not exist:
    ErrorHandling("GCB-reportfile not found or outdated!")
  EndIf

  '** parse reportfile for resource-info
    Open sRepFile For Input As #3

  Do Until Eof(3)   'read lines of reportfile

   Dim As Integer iStartComp, iStartMem, iStartRam, iStartChip, iStartOSC, iStartPromo
   Dim As UInteger iMemUsed, iMemExist

      Line Input #3, sOneLine                            'get a line
      iStartComp  = InStr(sOneLine, "Compiler Version ")  'search for Compiler-Version info
      iStartChip  = InStr(sOneLine, "Chip Model:")        'search for Chip-Model info
      iStartMem   = InStr(sOneLine, "Program Memory: ")   'search for Program-Memory info
      iStartRam   = InStr(sOneLine, "RAM: ")              'search for RAM-Memory info
      iStartOSC   = InStr(sOneLine, "OSC: ")              'search for Oscillator info

      If iStartComp Then   'if Compiler-Version-Info found ...
        iStartPromo = InStr(sOneLine, "</p><p>")               'search for promo-text in line
        If iStartPromo Then
          iStartPromo = iStartPromo + 6
          sPromo = Right(sOneLine, Len(sOneLine)-iStartPromo)  'store promo text to separate string
          sPromo = StringReplace(sPromo, "</p>", "" )          'delete html-tag
          sOneLine = Left(sOneLine, iStartPromo)               'remove promo text
          sPromo = Chr(10) + sPromo + Chr(10) + Chr(10)        'add empty lines
        End If
      End If

      If iStartComp Or iStartMem Or iStartRam Or iStartChip Or iStartOSC Then  'if line has one of the wanted infos ...
        sOneLine = StringReplace(sOneLine, "<p>", "" )                         'delete html-tags
        sOneLine = StringReplace(sOneLine, "</p>", "" )                        ' -*-
      EndIf

      If iStartComp Then   'if Compiler-Version-Info found ...
        sComp = StringReplace(sOneLine, " (YYYY-MM-DD)", "" )   'store info without (DD-MM-YYYY)
      EndIf

      If iStartRam Then    'if RAM info found ...
          sRam = sOneLine  'store unchanged info from reportfile
      EndIf

      If iStartMem Then     'if program-memory info found ...
          'If PIC:
          If Left(sChipModel, 1) = "1" Then
             sMem = sOneLine           'store unchanged info from report-file
           'If AVR:
           Else
            sMem = Right(sOneLine, Len(sOneLine) - InStr(sOneLine, "/"))  'get flashmemory value left justified
            iMemExist = ValUInt(sMem) * 2  'get available flash from reportfile and convert from words to bytes
            iMemUsed = BytesInHex()        'the program memory value from reportfile is wrong, so do calculation from hexfile
            sMem = "Program Memory: " + Str(iMemUsed) + "/" + Str(iMemExist) + Format(iMemUsed/iMemExist, " \b\y\t\e\s (###.##%)")
            If iMemUsed > iMemExist Then
             sError = sError + Chr(13) + Chr(10) + ">>> ERROR:  Program Memory usage too high !"
             MessageBeep (&h00000010)
            EndIf
          EndIf
      EndIf

      If iStartChip Then   'if chip info found ...
        sChipModel = StringReplace(sOneLine, "Chip Model: ", "" )   'store pure chipname
      EndIf

      If iStartOSC Then        'if oscillator-info found ...
        sOSC = Trim(sOneLine)  'store info
      EndIf

     If iStartOSC Then Exit Do  'end loop, after found oscillator-info (which is the last one in file)

  Loop

  Close #3

  '** build report-string:
  'sReport = "    " + sComp + "   " + sMem + "   " + sRam + "   Chip: " + sChipModel
   sReport = sPromo + sComp + "   " + sMem + "   " + sRam + "   " + sOSC + "   Chip: " + sChipModel + sError

   '** add leading and trailing zeros to report info (beautify):
   sReport = StringReplace(sReport, "(," , "(0," )
   sReport = StringReplace(sReport, ",%" , ",0%" )
   sReport = StringReplace(sReport, "(." , "(0." )
   sReport = StringReplace(sReport, ".%" , ".0%" )

End Sub

Sub DelProgammerLog()

  'delete old errorfiles:
  Kill sPathGCBSyn+"\GCBASIC\Programmer.log"
  Kill sPathGCBSyn+"\G+Stools\FlashError.txt"

End Sub

'*** Show one line of info from logfile generated by PICKitCommandline utility:
Sub ShowProgammerLog()

  If Not FileExists(sPathGCBSyn+"\GCBASIC\Programmer.log") Then Exit Sub

  '** parse logfile for info to display:
  Open sPathGCBSyn+"\GCBASIC\Programmer.log" For Input As #3
  Do Until Eof(3)                                              'read lines of logfile
    Line Input #3, sProgLog                                    'get a line
    If Left(sProgLog, 18) = "PICKitCommandline:" Then Exit Do  'store message
    If Left(sProgLog, 14) = "GCBProgrammer:" Then
      sProgLog = Mid( sProgLog, 15 )
      Exit Do  'store message
    End If
    sProgLog = "No info from PICKitCommandline found!"         'message, if content not found
  Loop
  Close #3
  Print sProgLog

End Sub


'*** Determine Path/Name of ASM file:
Sub PathAsmFile()

  'AsmFile has the same name and path as the sourcefile with another suffix:
  sAsmFile = Left(sSource, (InStrRev(sSource, "."))) + "asm"

End Sub


'*** Determine Path/Name of HEX file:
Sub PathHexFile()

  'HexFile has the same name and path as the sourcefile with another suffix:
  sHexFile = Left(sSource, (InStrRev(sSource, "."))) + "hex"

End Sub


'*** Determine name an path of hexfile:
Sub CheckForHex()

  'errormessage, if not exist:
  If Not FileExists(sHexFile) Then ErrorHandling("Missing  '" + sHexFile + "'")

End Sub


'*** Call a batchfile that runs the programmer-software:
Sub runProgrammer()

 Dim As String sFlashThis

  'choose flashPIC.bat if ChipModel is a PIC or flashAVR.bat if is an AVR
  If Left(sChipModel, 1) = "1" Then          'PIC sChipModel name always begins with "1"
   sFlashXbat = sPathGCBSyn + "\G+Stools\flashPIC.bat"
  Else                                       '... otherwise must be an AVR
   sFlashXbat = sPathGCBSyn + "\G+Stools\flashAVR.bat"
  End If

 'use FlashThis.bat for flashing, if present in sourcefile-folder:
  sFlashThis = Left(sHexFile, (InStrRev(sHexFile, "\"))) + "FlashThis.bat"
  If FileExists(sFlashThis) Then sFlashXbat = sFlashThis

  'print path/filename to console:
  Print Time + "    G+Stool-FLASH,  processing    " + sFlashXbat
  Print "Hex-File:  " + sHexFile + "    Chip:  " + sChipModel

  'check, if batchfile for flashing is present:
  If Not FileExists(sFlashXbat) Then ErrorHandling("Missing  '" + sFlashXbat + "'")

  'delete old errorfile:
  'Kill sPathGCBSyn+"\G+Stools\FlashError.txt"
  'go to CGB@Syn-rootfolder:
  ChDir sPathGCBSyn
  'start timer:
  'dTimerStart = Timer
  'run programmer-batchfile with  argument1 = sourcefile,  argument 2 = chipmodel and
  'pipe output to G+Stool. " 2>&1" redirects the screen-output from STDERR to STDOUT
  '(needed for avrdude):
  If sWinVer = "4" Then  'path with single double-quotes for older Windows
    Open Pipe Chr(34)+sFlashXbat+Chr(34)+" "+Chr(34)+sHexFile+Chr(34)+" "+sChipModel+" 2>&1" For Input As #1
  Else                    'path with double-double-quotes for Windows > WinME
    Open Pipe Chr(34)+ Chr(34)+sFlashXbat+Chr(34)+" "+Chr(34)+sHexFile+Chr(34)+" "+sChipModel+" 2>&1" +Chr(34) For Input As #1
  EndIf

  'Open Pipe Chr(34)+Chr(34)+sFlashXbat+Chr(34)+" "+Chr(34)+sHexFile+Chr(34)+" "+sChipModel+" 2>&1"+Chr(34)  For Input As #1

  'Shell Chr(34)+ Chr(34)+sFlashXbat+Chr(34)+" "+Chr(34)+sHexFile+Chr(34)+" "+sChipModel +Chr(34)

End Sub


'*** Determine the Windows main version (one char),
'    due to different pipe commands in the run*** subs
Sub CheckWinVer()

  Open Pipe "ver" For Input As #2                    'call shell command "ver"
  sWinVer = Input(50, #2)                            'read result
  sWinVer = Mid(sWinVer, InStr(sWinVer,".") - 1, 1)  'main version is one char left from first dot
  Close #2                                           'close pipe

End Sub


'*** If the programmer-software reports an error, do console output and let messagewindow stay open:
Sub CheckFlashError()

   If bErrorFlag Then Exit Sub  'nor further reporting when an GCB-compile error occured

  'Print Using "###.#  Sec."; Timer - dTimerStart;  'show duration for programm execution

   'the programmer batchfiles (flashXXX.bat) put the file "FlashError.txt" to folder "G+Stools"
   'if an errorlevel is given;
   If FileLen(sPathGCBSyn + "\G+Stools\FlashError.txt") Then
    MessageBeep (&h00000010)
    Print ">>>  ERRORs while flashing!  <<<"
     'print file to console (= Synwrites output-window)
     Open sPathGCBSyn+"\G+Stools\FlashError.txt" For Input As #3
     Do While Not Eof(3)
     Print Input(1, #3);
     Loop
     Close #3
    iStayOpen = TRUE           'let messagewindow stay open
    Else                        'if no compile-error ...
      Print "Downloading / Flashing  o.k."  'print message to console
   End If

End Sub


'*** Don't automatic-close messagewindow:
Sub StayOpen()

  Do
    iEvent=WaitEvent()
    If iEvent=EventClose Then Exit Do                      'end with cross-button
    If iEvent=EventGadget And EventNumber()=2 Then Exit Do 'end with "close" button
    If EventKEY = 27 Then Exit Do                          'end with Esc key
    If EventKEY = 13 Then Exit Do                          'end with Enter key
 Loop

End Sub

'*** Check, if G&Ctool.exe is called with enough commandline arguments:
Sub CheckForParameters()

 '** First check, if 2 parameters:
 If sSource = "" Then
  ErrorHandling(Chr(10) + _
                "G+Stool.exe ist part of the GCB[at]Syn-IDE for the Great Cow BASIC compiler. " + _
                "It is useful only in conjunction with the SynWrite-Editor, which is " + _
                "the main part of the GCB[at]Syn-IDE." + Chr(10) + Chr(10) + _
                "For further information see 'readme.txt' in the GCB[at]Syn root folder!")
 End If

 '** Second check, if second parameter is valid:
 '* Errormessage, if called with illegal commandline arguments:
 Select Case sPara
  Case "asm", "hex", "hexflash", "flash", "useini", "foini", "hexini"  'these are valid
                                                                         '... so we can go ahead
  Case Else                                                            'if not, errormessage and end
    ErrorHandling("Wrong G+Stool.exe commandline! Use the following parameters, separated by blanks: " + Chr(10) + Chr(10) + _
                 "  1.  hexflash  OR  hex  OR  asm  OR  flash  OR" + Chr(10) + _
                 "       useini  OR  foini  OR  hexini" + Chr(10) + _
                 "  2.  "+Chr(34)+"{FileName}"+Chr(34)+"  (with doublequotes!)" + Chr(10) + _
                 "  3.  stayopen  (optional, if you want to close"  + Chr(10) + _
                 "       the messagewindow manually)" + Chr(10) + Chr(10) + _
                 "Put them into SynWrite Editor using menu:"  + Chr(10) + _
                 "Tools  ->  Customize external tools...  ->  Parameters")
 End Select


 '** Third check for source code filetype:
 '* Errormessage, if called with wrong file extension:
 Dim sExt As String
 Dim iPPos As Integer
 iPPos = InStrRev(sSource, ".")   'find last point in sourcefilename
 iPPos = Len(sSource) - iPPos     '
 sExt = Right(sSource, iPPos)     'get file extension
 sExt = LCase(sExt)               'lowercase file extension
 Select Case sPara
   Case "asm"
   If sExt <> "gcb" And sExt <> "bas" And sExt <> "pbs" And sExt <> "h" Then
     ErrorHandling("Use  'Make ASM'  with file extensions  .gcb .bas .pbs .h")
   End If
  Case "hex", "hexflash"
   If sExt <> "gcb" And sExt <> "bas" And sExt <> "pbs" Then
      ErrorHandling("Use  'Make HEX'  with file extensions  .gcb .bas .pbs")
   End If
 End Select

End Sub

'*** Avoid use of gcbasic.ini:
Sub CheckForIni()

  'gcbasic.ini may contain commands, that conflict with GCB@Syn:
  If FileExists(sPathGCBSyn+"\GCBASIC\gcbasic.ini") Then
    ErrorHandling("gcbasic.ini  is not compatible with GCB[at]Syn!" + Chr(10) + _
                  "Edit batchfiles in folder 'G+Stool' instead." + Chr(10) + Chr(10) + _
                  "Please delete or rename:"+ Chr(10) + _
                  sPathGCBSyn+"\GCBASIC\gcbasic.ini")
  End If

End Sub

'*** Do errormessage in messagebox and exit G+Stool:
Sub ErrorHandling(ByVal sMessage As String)

 'output errormessage to a messagebox:
 MessageBox(null, "E R R O R" + Chr(10) + Chr(10) + sMessage, sTitle, MB_ICONEXCLAMATION)
 'small errormessage for console (= Synwrites output-window)
 Print "ERROR!  >>>   " + sMessage
 'Print "    <<<   ERROR!"
 'exit G+Stool:
 End

End Sub

'*** replace for adding trailing and leading zero to resource report:
Function StringReplace(ByVal sText As String, ByVal sLookFor As String, ByVal sReplaceWith As String) As String

  Dim sNew As String
  Dim i As Integer

  For i = 1 To Len(sText)
    If Mid(sText, i, Len(sLookFor)) = sLookFor Then
      sNew = sNew + sReplaceWith
      i = i - 1 + Len(sLookFor)
    Else
      sNew = sNew + Mid(sText, i, 1)
    End If
  Next
  StringReplace = sNew

End Function

'*** Get program-size in bytes for AVRs from hexfile:
Function BytesInHex() As UInteger

  Dim iHexNr As Integer
  iHexNr = FreeFile
  If Open(sHexFile For Input as #iHexNr) Then Exit Function

  Dim As String sLine1, sLine2
  Dim As ULong iNumOfBytes

  While Not Eof(iHexNr)
   sLine1 = sLine2              'store line before the current line
   Line Input #iHexNr, sLine2   'current line
   sLine2 = Trim(sLine2)

   If sLine2 = ":00000001FF" Then  'if current line is 'end of file record'
     'Flash usage is the byte-count of the last record plus the start-address of the last record.
     'Byte-Count is char 2,3 (one byte hex-value)- Start-Address is char 4,5,6,7 (2 byte hex-value):
     iNumOfBytes = ValUInt("&H" + Mid(sLine1, 2, 2)) + ValUInt("&H" + Mid(sLine1, 4, 4))
     Exit While
   EndIf
  Wend

  Close #iHexNr
  Return iNumOfBytes

End Function
