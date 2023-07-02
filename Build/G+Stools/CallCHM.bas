'-- CallCHM.bas ----------------------------------------------- >>fst'16<< -
' - gives context-sensitive help to Great-Cow-Basic SynWrite-IDE
' - parses ...\SynWrite\Data\autocomplete\GreatCowBasic.acp for 
'   Keywords and html-filenames
' - uses Microsofts hh.exe to call chm-file
' - compiled with FreeBasic 0.24 (parameter -s gui)
'-- V 20160211 ------------------------------- http://www.FrankSteinberg.de -

#Include once "file.bi"
#include once "windows.bi"

Dim Shared As String Title, PathCHM, PathHH, PathACP, Row, OneChar, KeyWord, HTMfile, Para1
Dim Shared As Integer Position
Dim Shared As HWND hWnd
Dim Shared As LRESULT res1, res2
Dim Shared As Double T1

Title = "CallCHM    V20160211    www.FrankSteinberg.de    >>fst'16<<"

'*** Get full path and filenames:
PathCHM = ExePath + "\..\GCBasic\gcbasic.chm"
PathACP = ExePath + "\..\SynWrite\Data\autocomplete\GCBasic.acp"
PathHH =  Environ("windir") + "\HH.exe"

'*** Errorhandling for missing files:
If Not FileExists(PathCHM) Then
	 MessageBox NULL, PathCHM + "  not found!", Title, 16
  End
End If
If Not FileExists(PathACP) Then
	 MessageBox NULL, PathACP + "  not found!", Title, 16
  End
End If
If Not FileExists(PathHH) Then
	 MessageBox NULL, PathHH + "  not found!", Title, 16
  End
End If

'*** Direct call of HH.exe, when no keyword given: 
If Command(1) = "" Then GoTo RunHH

If Open (PathACP For Input As #1) Then
  MessageBox NULL, " Error opening  " + PathACP, Title, 16
  End
End If

'*** Parse ...\SysWrite\HL\GreatCowBasic.acp for keywords 
'    and corresponding html-file:
Do Until EOF(1)                        'search til end of file
   Line Input #1, Row                  'get one row
   Position = InStr(Row, " ")          'search first space in row
   Do                                  'skip following spaces  
   	Position += 1                      '
   	OneChar = Mid(Row, Position, 1)    '
   Loop Until OneChar <> " "           '
   KeyWord = ""                        
   Do                                  '*** parse KEYWORD
  		OneChar = Mid(Row, Position, 1)    'get one char after another
  		If OneChar = " " Then Exit Do      '... until next space
  		If OneChar = "|" Then Exit Do      '... or next |
  		If OneChar = "(" Then Exit Do      '... or next (  		
  		If OneChar = "" Then Exit Do       '... or no more char
  		KeyWord = KeyWord + OneChar        'build Keyword
  		KeyWord = Trim(KeyWord, " ")       'delete leading & trailing spaces
  		KeyWord = LTrim(KeyWord, "#")      'due to bug in SynWrite ('#' sometimes deleted)
  		Position += 1                      'next char
   Loop
   KeyWord = LCase(KeyWord)            'we need no case sensitivity 
   'Print KeyWord
   Para1 = Command(1)
   Para1 = LTrim(Para1, "#")           'due to bug in SynWrite ('#' sometimes deleted)
   If LCase(Para1) = KeyWord Then      '*** if keyword matches, then parse htm-filename
   	Position = InStrRev(Row, "\")      'search last backslash
   	'Print Position
   	If Position < 1 Then                'if no backslash 
   		HTMfile = "_" + LCase(KeyWord) + ".html"  '... KeyWord matches html-filename
   		Exit Do                            'htm-file found
   	EndIf
   	Position = Len(Row) - Position      'filename is from backslash to the end of the row
  		HTMfile = Right(Row, Position)      ' ...  to the end of the row
  		HTMfile = Trim(HTMfile)             'delete leading & trailing spaces
  		'Print "found: " + HTMFile
  		Exit Do   		                        'abort searching in acp-file
   EndIf
Loop

'*** Close acp-file:
Close #1

'*** Run chm-file via HH.exe:
RunHH:
T1 = Timer                                               'start timeout

'** Close older help-windows:
Do 
	hWnd = FindWindow("HH Parent", "GCBASIC Help")  'search help-window
 postMessage(hWnd, WM_SYSCOMMAND, SC_CLOSE, 0)           'close old help-window
 If Timer - T1 > 2 Then Exit Do                          'end CallCHM after 2 Sec. timeout
 Sleep(10)                                     
Loop While hWnd                                          'contiue, when help-window found

'** Open new help-window:
Sub mythread(param As Any Ptr) 
 Exec(PathHH, "-MyID " + PathCHM + "::" + HTMfile)
End Sub
If HTMfile = "" Then HTMfile = "_introducing_gcbasic.html"  'standard html-file when nothing else found:
ThreadCreate(@mythread, 0)                               'start help-window in separate thread

'** Wait for window and maximize it:
Do 
	hWnd = FindWindow("HH Parent", "GCBASIC Help")  'search help-window
 If Timer - T1 > 2 Then end                              'end CallCHM after 2 Sec. timeout
 Sleep(10)                                     
Loop Until hWnd                                          'continue, when help-window found
If InStr(LCase(Command(2)), "max") Then                  'if parameter 2 "max(imize)" found ...
 postMessage(hWnd, WM_SYSCOMMAND, SC_MAXIMIZE, 0)        'maximize help-window
End If