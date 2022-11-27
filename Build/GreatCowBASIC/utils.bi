' GCBASIC - A BASIC Compiler for microcontrollers
'  Miscellaneous routines
' Copyright (C) 2006 - 2021 Hugh Considine
'
' This program is free software; you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation; either version 2 of the License, or
' (at your option) any later version.
'
' This program is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with this program; if not, write to the Free Software
' Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'
'If you have any questions about the source code, please email me: hconsidine at internode.on.net
'Any other questions, please email me or see the GCBASIC forums.

'Windows API
'Needed for ShortFileName
#Ifdef __FB_WIN32__
  '#Include Once "windows.bi"
  type LPCWSTR as wstring Ptr
  type LPCSTR as zstring Ptr
  type LPWSTR as wstring Ptr
  type LPSTR as zstring Ptr
  type DWORD as UInteger

  extern "windows" lib "kernel32"
    #ifdef UNICODE
      declare function GetShortPathName alias "GetShortPathNameW" (byval as LPCWSTR, byval as LPWSTR, byval as DWORD) as DWORD
    #Else
      declare function GetShortPathName alias "GetShortPathNameA" (byval as LPCSTR, byval as LPSTR, byval as DWORD) as DWORD
    #EndIf
  End Extern
#EndIf

Function AddFullPath(CurrPath As String, FullPathIn As String = "") As String
  'Will return a fully qualified path from CurrPath.
  'FullPathIn is that path that should be added to CurrPath if not FQ

  Dim As String OutPath, FullPath
  OutPath = CurrPath

  'Get full path
  If FullPathIn = "" Then
    FullPath = CurDir
  Else
    FullPath = FullPathIn
  End If

  #Ifdef __FB_UNIX__
    'Remove trailing slash from full path
    If Right(FullPath, 1) = "/" Then FullPath = Left(FullPath, Len(FullPath) - 1)
    If Right(FullPath, 1) = "\" Then FullPath = Left(FullPath, Len(FullPath) - 1)
    'If CurrPath not FQ, add full path
    If Left(OutPath, 1) <> "/" And Left(OutPath, 1) <> "~" Then
      OutPath = FullPath + "/" + CurrPath
    End If

  #Else
    'Remove trailing slash from full path
    If Right(FullPath, 1) = "\" Then FullPath = Left(FullPath, Len(FullPath) - 1)
    'If CurrPath not FQ, add full path
    If Mid(OutPath, 2, 1) <> ":" And Left(OutPath, 2) <> "\\" Then
      OutPath = FullPath + "\" + CurrPath
    End If
  #EndIf

  Return OutPath
End Function

SUB Calculate (SUM As String)

  Dim As String Act, ACT2, CTemp, Temp
  Dim As String OSS, CLDO, TS, N1S, N2S, OS, Ans, M, SUMoriginal
  Dim As Single Res, N1, N2
  Dim As Integer CA, PD, T, FD, AP, FS, SOL, CO, SC

  SUMoriginal = SUM
  SUM = UCase(SUM)
  REM Replace AND, OR, NOT, XOR with symbols
  REPLOGIC:
  'IF INSTR(SUM, "AND") <> 0 THEN Replace SUM, "AND", "&": GOTO REPLOGIC
  'IF INSTR(SUM, "OR") <> 0 THEN Replace SUM, "OR", "|": GOTO REPLOGIC
  'IF INSTR(SUM, "XOR") <> 0 THEN Replace SUM, "XOR", "#": GOTO REPLOGIC
  WholeReplace SUM, "AND", "&"
  WholeReplace SUM, "OR", "|"
  WholeReplace SUM, "XOR", "#"

  REM Replace High and Low
  DO WHILE INSTR(SUM, "UPPER") <> 0: Replace SUM, "UPPER", "U": Loop
  DO WHILE INSTR(SUM, "HIGH") <> 0: Replace SUM, "HIGH", "H": LOOP
  DO WHILE INSTR(SUM, "LOW") <> 0: Replace SUM, "LOW", "L": LOOP

  BEGINCALC:
  REM Remove all Spaces
  Replace SUM, " ", ""
  IF INSTR(SUM, " ") <> 0 THEN GOTO BEGINCALC
  SUM = " " + LCase(SUM)

  IF INSTR(SUM, "%") <> 0 THEN GOTO PERCDIFF

  REM Calculate Sine, Cosine, Tangent, Arctangent and Pi
  PISWAP:
  IF INSTR(SUM, "pi") <> 0 THEN
    Replace SUM, "pi", "3.14159"
    GOTO PISWAP
  END IF

  INSMULT:
  IF INSTR(SUM, ")(") <> 0 THEN
    Replace SUM, ")(", ")*("
    GOTO INSMULT
  END IF

  FILE(1) = "cos("
  FILE(2) = "sin("
  FILE(3) = "tan("
  FILE(4) = "atn("
  FILE(5) = "sqr("
  FILE(6) = "abs("
  FILE(7) = "sgn("
  FILE(8) = "int("
  FILE(9) = "log("
  FILE(10) = "rnd("

  FOR CA = 1 TO 10
    CALCLOOP:
    IF INSTR(LCase(SUM), FILE(CA)) <> 0 THEN

      CTemp = ""
      PD = INSTR(LCase(SUM), FILE(CA)) + 4
      T = 1
      GETFUNCTION: 'Is this label still used?
      Do
        IF PD > LEN(SUM) THEN SUM = SUM + ")"
        CTemp = CTemp + Mid(SUM, PD, 1)
        IF Mid(SUM, PD, 1) = "(" THEN T = T + 1
        IF Mid(SUM, PD, 1) = ")" THEN T = T - 1
        PD = PD + 1
      Loop While T > 0
      IF T = 0 Then CTemp = Left(CTemp, LEN(CTemp) - 1)

      OSS = FILE(CA) + CTemp + ")"
      CLDO = CLD
      CLD = "no"
      Calculate CTemp
      CLD = CLDO

      Select Case CA
        Case 1: CTemp = Str(COS(VAL(CTemp)))
        Case 2: CTemp = Str(SIN(VAL(CTemp)))
        Case 3: CTemp = Str(TAN(VAL(CTemp)))
        Case 4: CTemp = Str(ATN(VAL(CTemp)))
        Case 5: CTemp = Str(SQR(VAL(CTemp)))
        Case 6: CTemp = Str(ABS(VAL(CTemp)))
        Case 7: CTemp = Str(SGN(VAL(CTemp)))
        Case 8: CTemp = Str(INT(VAL(CTemp)))
        Case 9: CTemp = Str(LOG(VAL(CTemp)))
        Case 10: CTemp = Str(RND(VAL(CTemp)))
      End Select
      SCICONV CTemp
      IF Left(CTemp, 1) = " " THEN CTemp = Mid(CTemp, 2)
      Replace SUM, OSS, CTemp
      'IF CLD = "" THEN PRINT SUM
      GOTO CALCLOOP
    END IF
  NEXT

  REM Solve Brackets using recursion
  BRACKETS:
  IF INSTR(SUM, "(") = 0 AND INSTR(SUM, ")") = 0 THEN GOTO ENDBRACKET
  TS = ""
  PD = INSTR(SUM, "(")
  T = 0
  Do
    TS = TS + Mid(SUM, PD, 1)
    IF Mid(SUM, PD, 1) = "(" THEN T = T + 1
    IF Mid(SUM, PD, 1) = ")" THEN T = T - 1
    PD = PD + 1
  Loop While T > 0
  OSS = TS
  Replace TS, "(", "": Replace TS, ")", ""

  CLDO = CLD
  CLD = "no"
  Calculate TS
  SCICONV TS
  CLD = CLDO
  DO WHILE INSTR(TS, " ") <> 0: Replace TS, " ", "": LOOP
  Replace SUM, OSS, TS
  GOTO BRACKETS
  ENDBRACKET:

  REM Find high/low
  HIGHLOW:
  'Added "lf" to resolve bug in handling compares than two string where the SUM could contain a chip like an LF chip
  'My thoughts on what this means.
  ' When  ( l,h,u are all NOT present in SUM ) or ( lf IS present ) or ( mega IS present )  jump to label
  IF ( INSTR(LCase(SUM), "l") = 0 AND INSTR(LCase(SUM), "h") = 0 AND INSTR(LCase(SUM), "u") = 0 ) OR INSTR(LCase(SUM), "lf") <> 0 OR INSTR(LCase(SUM), "mega") <> 0 THEN GOTO ENDHIGHLOW

  FD = 0
  Do
    FD = FD + 1
    Temp = LCase(Mid(SUM, FD, 1))
  Loop While Temp <> "u" And Temp <> "h" AND Temp <> "l"
  Act = Temp: AP = FD

  FOR FS = AP - 1 TO 1 STEP -1
    Temp = Mid(SUM, FS, 1)
    IF (Temp <> Str(VAL(Temp)) AND Temp <> ".") OR Temp = " " THEN FS = FS + 1: EXIT For
    M = Mid(SUM, FS - 1, 1)
    IF Temp = "-" AND (M <> LTrim(Str(VAL(M))) AND M <> "l" AND M <> "h" And M <> "u" And M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN FS = FS - 1: EXIT FOR
  NEXT

  N2S = Mid(SUM, AP + 1)

  'PRINT N2, Act

  IF INSTR(N2S, "u") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "u") - 1)
  IF INSTR(N2S, "h") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "h") - 1)
  IF INSTR(N2S, "l") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "l") - 1)
  IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
  IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
  IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
  IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)

  N2 = VAL(N2S)
  OS = Act + N2S
  DO WHILE INSTR(OS, " ") <> 0
    Replace OS, " ", ""
  LOOP
  'IF CLD = "" THEN PRINT SUM

  RES = 0
  IF Act = "l" THEN RES = N2 AND 255
  IF Act = "h" THEN RES = (N2 AND 65280) / 256
  IF Act = "u" THEN RES = (N2 AND &HFF0000) / 65536

  ANS = Str(RES)
  SCICONV ANS
  DO WHILE INSTR(ANS, " ") <> 0: Replace ANS, " ", "": LOOP
  Replace SUM, OS, ANS
  GOTO HIGHLOW
  ENDHIGHLOW:

  'Find rotate, translate into multiply/divide by power
  Do While InStr(Sum, "<<") <> 0
    Replace Sum, "<<", "*2^"
  Loop
  Do While InStr(Sum, ">>") <> 0
    Replace Sum, ">>", "/2^"
  Loop

REM Find Exponentiation
EXPONENT:
IF INSTR(SUM, "^") = 0 THEN GOTO ENDEXPONENT
FD = 0
FINDEXP:
FD = FD + 1
Temp = Mid(SUM, FD, 1)
IF Temp <> "^" THEN GOTO FINDEXP
Act = Temp: AP = FD

FOR FS = AP - 1 TO 1 STEP -1
 Temp = Mid(SUM, FS, 1)
 IF (Temp <> Right(Str(VAL(Temp)), 1) AND Temp <> "." AND Temp <> "-" AND Temp <> "E") OR Temp = " " THEN FS = FS + 1: EXIT FOR
 M = Mid(SUM, FS - 1, 1)
 IF Temp = "-" AND (M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN EXIT FOR
NEXT

N1S = Mid(SUM, FS, AP - FS)
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

If INSTR(N2S, "^") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "^") - 1)
IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)
IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)

N2 = VAL(N2S)
OS = N1S + Act + N2S
DO WHILE INSTR(OS, " ") <> 0
Replace OS, " ", ""
LOOP

IF N1 = 0 AND N2 = 0 THEN Replace SUM, OS, "0": GOTO EXPONENT

ANS = ""
'IF CLD = "" THEN PRINT SUM

RES = N1 ^ N2

ANS = Str(RES)
IF Left(ANS, 1) = " " THEN ANS = Mid(ANS, 2)
SCICONV ANS
IF Left(ANS, 1) = " " THEN ANS = Mid(ANS, 2)
Replace SUM, OS, ANS
GOTO EXPONENT
ENDEXPONENT:

  'Find Division and Multiplication
  DIVMULT:
  IF INSTR(SUM, "/") = 0 AND INSTR(SUM, "*") = 0 THEN GOTO ENDDIVMULT
  FD = 0
  FINDDIV:
  FD = FD + 1
  Temp = Mid(SUM, FD, 1)
  IF Temp <> "/" AND Temp <> "*" THEN GOTO FINDDIV
  Act = Temp: AP = FD

  FOR FS = AP - 1 TO 1 STEP -1
    Temp = Mid(SUM, FS, 1)
    If (Temp <> Str(VAL(Temp)) AND Temp <> ".") OR Temp = " " THEN FS = FS + 1: EXIT FOR
    M = Mid(SUM, FS - 1, 1)
    If Temp = "-" AND (M <> LTrim(Str(VAL(M))) AND M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN FS = FS - 1: EXIT FOR
  NEXT

  N1S = Mid(SUM, FS, AP - FS)
  N1 = VAL(N1S)
  N2S = Mid(SUM, AP + 1)

  IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
  IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)
  IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
  IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)

  N2 = VAL(N2S)
  OS = N1S + Act + N2S
  DO WHILE INSTR(OS, " ") <> 0
    Replace OS, " ", ""
  LOOP

  RES = 0
  IF Act = "/" AND N2 <> 0 THEN RES = N1 / N2
  IF Act = "*" THEN RES = N1 * N2
  ANS = Str(RES)
  SCICONV ANS
  DO WHILE INSTR(ANS, " ") <> 0: Replace ANS, " ", "": LOOP
  Replace SUM, OS, ANS
  GOTO DIVMULT
  ENDDIVMULT:

REM Replace all occurances of "--" with "+"
REMMINUS:
IF INSTR(SUM, "--") <> 0 THEN
 Replace SUM, "--", "+"
 GOTO REMMINUS
END IF
IF INSTR(SUM, "+-") <> 0 THEN
 Replace SUM, "+-", "-"
 GOTO REMMINUS
END IF

REM Addition and Subtraction
FD = 0
ADDSUB:
IF INSTR(SUM, "+") = 0 AND INSTR(FD + 1, SUM, "-") = 0 THEN GOTO ENDADDSUB
FINDADD:
 FD = FD + 1
 Temp = Mid(SUM, FD, 1)
IF Temp <> "+" AND Temp <> "-" THEN GOTO FINDADD

Act = Temp: AP = FD
FOR FS = AP - 1 TO 1 STEP -1
  Temp = Mid(SUM, FS, 1)
  'IF (Temp <> Right(Str(VAL(Temp)), 1) AND Temp <> "." AND Temp <> "-" AND Temp <> "E") OR Temp = " " Then
  IF IsCalcDivider(Temp) And Temp <> "-" Then
    FS = FS + 1
    Exit For
  End If
  M = Mid(SUM, FS - 1, 1)
  'If Temp = "-" AND (M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") Then
  If Temp = "-" AND Not IsCalcDivider(M) Then
    FS = FS - 1
    EXIT For
  End If
NEXT

N1S = Mid(SUM, FS, AP - FS)
IF N1S = " " OR N1S = "" THEN GOTO ADDSUB
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
IF INSTR(N2S, "&") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "&") - 1)
IF INSTR(N2S, "|") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "|") - 1)
IF INSTR(N2S, "#") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "#") - 1)

N2 = VAL(N2S)
OS = N1S + Act + N2S
Do WHILE INSTR(OS, " ") <> 0
  Replace OS, " ", ""
LOOP

IF Act = "+" THEN ANS = Str(N1 + N2)
IF Act = "-" THEN ANS = Str(N1 - N2)
IF Left(ANS, 1) = " " THEN ANS = Mid(ANS, 2)
IF N1 = 0 AND N2 <> 0 AND Act = "-" THEN FD = FD + 1: GOTO ADDSUB
SCICONV ANS
Replace SUM, OS, ANS
FD = FD + LEN(OS) - LEN(ANS) + 1
FD = 0

MINUS:
IF INSTR(SUM, "--") <> 0 THEN
 Replace SUM, "--", "+"
 GOTO MINUS
END IF
GOTO ADDSUB
ENDADDSUB:

'Test Sum
TESTSUM:
IF INSTR(SUM, "=") = 0 AND INSTR(SUM, "~") = 0 AND InStr(SUM, "<") = 0 AND INSTR(SUM, ">") = 0 AND INSTR(SUM, "{") = 0 AND INSTR(SUM, "}") = 0 THEN GOTO ENDTESTSUM
FD = 0
FINDTEST:
  FD = FD + 1
  Temp = Mid(SUM, FD, 1)
IF Temp <> "=" AND Temp <> "~" And Temp <> "<" AND Temp <> ">" AND Temp <> "{" AND Temp <> "}" THEN GOTO FINDTEST
Act = Temp: AP = FD

'Find value before op
FOR FS = AP - 1 TO 1 STEP -1
  Temp = Mid(SUM, FS, 1)
  'IF (Temp <> Str(VAL(Temp)) AND Temp <> "." AND Temp <> "-" AND Temp <> "E") OR Temp = " " Then
  IF IsCalcDivider(Temp) And Temp <> "-" Then
    FS = FS + 1
    EXIT For
  End If
  M = Mid(SUM, FS - 1, 1)
  IF Temp = "-" AND (M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+") Then
     FS = FS - 1
     EXIT For
  End If
NEXT

N1S = Mid(SUM, FS, AP - FS)
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

'IF INSTR(N2S, "=") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "=") - 1)
'IF INSTR(N2S, "<") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "<") - 1)
'IF INSTR(N2S, ">") <> 0 THEN N2S = Left(N2S, INSTR(N2S, ">") - 1)
'IF INSTR(N2S, "{") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "{") - 1)
'IF INSTR(N2S, "}") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "}") - 1)
'IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
'IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
For FS = 2 To Len(N2S)
  If IsCalcDivider(Mid(N2S, FS, 1)) Then
    N2S = Left(N2S, FS - 1)
    Exit For
  End If
Next

N2 = VAL(N2S)
OS = N1S + Act + N2S
DO WHILE INSTR(OS, " ") <> 0
Replace OS, " ", ""
LOOP
'IF CLD = "" THEN PRINT SUM

If IsConst(N2S) And IsConst(N1S) Then
  If Act = "=" THEN RES = (N1 = N2) * -1
  If Act = "~" Then RES = (N1 <> N2) * -1
  If Act = "<" THEN RES = (N1 < N2) * -1
  If Act = ">" THEN RES = (N1 > N2) * -1
  If Act = "{" THEN RES = (N1 <= N2) * -1
  If Act = "}" THEN RES = (N1 >= N2) * -1
Else
  If Act = "=" THEN RES = (N1S = N2S) * -1
  If Act = "~" Then RES = (N1S <> N2S) * -1
  If Act = "<" THEN RES = (N1S < N2S) * -1
  If Act = ">" THEN RES = (N1S > N2S) * -1
  If Act = "{" THEN RES = (N1S <= N2S) * -1
  If Act = "}" THEN RES = (N1S >= N2S) * -1
End If

ANS = Str(RES)
SCICONV ANS
IF Left(ANS, 1) = " " THEN ANS = Right(ANS, LEN(ANS) - 1)
Replace SUM, OS, ANS
GoTo TESTSUM
ENDTESTSUM:

REM Find Logical operation results (AND, OR, XOR)
LOGICALOP:
IF INSTR(SUM, "&") = 0 AND INSTR(SUM, "|") = 0 AND INSTR(SUM, "#") = 0 THEN GOTO ENDLOGICALOP
FD = 0
FINDLOGIC:
FD = FD + 1
Temp = Mid(SUM, FD, 1)
IF Temp <> "&" AND Temp <> "|" AND Temp <> "#" THEN GOTO FINDLOGIC
Act = Temp: AP = FD

FOR FS = AP - 1 TO 1 STEP -1
 Temp = Mid(SUM, FS, 1)
 IF (Temp <> Str(VAL(Temp)) AND Temp <> ".") OR Temp = " " THEN FS = FS + 1: EXIT FOR
 M = Mid(SUM, FS - 1, 1)
 IF Temp = "-" AND (M <> LTrim(Str(VAL(M))) AND M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN FS = FS - 1: EXIT FOR
NEXT

N1S = Mid(SUM, FS, AP - FS)
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)
IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
IF INSTR(N2S, "&") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "&") - 1)
IF INSTR(N2S, "|") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "|") - 1)
IF INSTR(N2S, "#") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "#") - 1)

N2 = VAL(N2S)
OS = N1S + Act + N2S
DO WHILE INSTR(OS, " ") <> 0
Replace OS, " ", ""
LOOP
'IF CLD = "" THEN PRINT SUM

RES = 0
IF Act = "&" AND N2 <> 0 THEN RES = N1 AND N2
IF Act = "|" THEN RES = N1 OR N2
IF Act = "#" THEN RES = N1 XOR N2
ANS = Str(RES)
SCICONV ANS
DO WHILE INSTR(ANS, " ") <> 0: Replace ANS, " ", "": LOOP
Replace SUM, OS, ANS
GOTO LOGICALOP
ENDLOGICALOP:

Rem End of normal calculate
SOL = VAL(SUM)
EXIT SUB

REM Percentage Section
PERCDIFF:
FD = 0
FINDSIGN:
FD = FD + 1
IF FD > Len(SUM) Then
    LogError "Fatal in Calculation(). Error at " + SUMoriginal
    Exit Sub
End If
Temp = Mid(SUM, FD, 1)
IF Temp <> "+" AND Temp <> "." AND Temp <> "-" AND Temp <> "x" AND Temp <> "*" AND Temp <> "/" THEN GOTO FINDSIGN
Act = Temp: AP = FD

FOR FS = AP - 1 TO 1 STEP -1
Temp = Mid(SUM, FS, 1)
IF Temp <> Right(Str(VAL(Temp)), 1) OR Temp = " " AND Temp <> "." THEN EXIT FOR
NEXT

N1 = VAL(Mid(SUM, FS, AP - FS))
N2 = VAL(Right(SUM, LEN(SUM) - AP))
ACT2 = Right(SUM, 1)

IF Act = "+" THEN SOL = (N2 + N1) / N2 * 100
IF Act = "-" THEN SOL = (N2 - N1) / N2 * 100
IF Act = "/" THEN SOL = N1 / N2 * 100
IF Act = "x" OR Act = "*" THEN
 SOL = N1 * (N2 / 100)
 IF ACT2 = "+" THEN SOL = SOL + N1
 IF ACT2 = "-" THEN SOL = SOL - N1
END IF
SOL = SOL * SGN(SOL)
SUM = Str(SOL)
END SUB

Function CountOccur (Source As String, Search As String, SearchWhole As Integer = 0) As Integer
  Dim As String Temp, SourceLower
  Dim As Integer T, CO, SC, LC, RC

  T = 0

  'Find whole string
  IF Left(Search, 2) <> "';" THEN
    FOR CO = 1 TO LEN(Source) - (1 - LEN(Search))
      IF Mid(UCase(Source), CO, LEN(Search)) = UCase(Search) Then
        If SearchWhole Then
          'Check left of find
          LC = 0
          If CO = 1 Then
            LC = -1
          ElseIf IsDivider(Mid(Source, CO - 1, 1)) Then
            LC = -1
          End If
          'Check right of find
          RC = 0
          If CO = LEN(Source) - (1 - LEN(Search)) Then
            RC = -1
          ElseIf IsDivider(Mid(Source, CO + Len(Search), 1)) Then
            RC = -1
          End If
          'If left and right clear, have found
          If LC And RC Then
            T = T + 1
          End If
        Else
          T = T + 1
        End If
      End If
    NEXT

  'Find one of the searched chars
  Else
    Temp = LCase(Mid(Search, 3))
    SourceLower = LCase(Source)

    FOR SC = 1 To LEN(Temp)
      FOR CO = 1 TO LEN(Source)
        IF Asc(Source, CO) = Asc(Temp, SC) THEN T = T + 1
      NEXT
    Next
  END IF

  CountOccur = T
END FUNCTION

Function DelType (InString As String) As String
  If Instr(InString, "[") = 0 Then Return InString

  Dim As String Temp, DataSource
  DataSource = UCase(InString)
  Temp = UCase(InString)

  If INSTR(Temp, "[BYTE]") <> 0 Then Replace DataSource, "[BYTE]", ""
  If INSTR(Temp, "[WORD]") <> 0 Then Replace DataSource, "[WORD]", ""
  If INSTR(Temp, "[INTEGER]") <> 0 Then Replace DataSource, "[INTEGER]", ""
  If INSTR(Temp, "[LONG]") <> 0 Then Replace DataSource, "[LONG]", ""
  If INSTR(Temp, "[SINGLE]") <> 0 Then Replace DataSource, "[SINGLE]", ""

  Return DataSource
End Function

Sub DisplayProgram
  'Debugging sub
  'Will show all program code when called

  Dim CurrSub As Integer
  For CurrSub = 0 To SBC
    Print Subroutine(CurrSub)->Name
    If Subroutine(CurrSub)->Required Then
      LinkedListPrint(Subroutine(CurrSub)->CodeStart)
      Print
    Else
      Print "Not used, so not shown"
      Print
    End If
  Next

End Sub

Sub DisplayProgramPurged
  'Debugging sub
  'Will show all program code when called

  Dim CurrSub As Integer
  For CurrSub = 0 To SBC
    If Subroutine(CurrSub)->Required Then
      Print Subroutine(CurrSub)->Name
      LinkedListPrintwithComments(Subroutine(CurrSub)->CodeStart)
      Print
    End If
  Next

End Sub


FUNCTION GetByte (DataSource As String, BS As Integer) As String

  Dim As String Temp
  Dim As LongInt OutVal
  Dim As Integer PD

  'BS is 0 for low, 1 for high

  'Check if asm constant
  If Left(DataSource, 1) = "@" Then
    If UCase(Left(DataSource, 11)) = "@SYSASMCALC" Then
      Return DataSource
    End If
    '11/4/2012: Changed from "low VAR" to "low (VAR)" on PIC
    'Same format as AVR, MPASM is ok, need to check gputils

    'If on AVR, and dealing with I/O variable, add 32 to address
    If ModeAVR Then
      If IsIOReg(Mid(DataSource, 2)) Then
        DataSource = DataSource + " + 32"
      End If
    EndIf

    If BS = 0 Then
      Return "low(" + DataSource + ")"
    ElseIf BS = 1 Then
      Return "high(" + DataSource + ")"
    ElseIf BS = 2 Then
      Return "upper(" + DataSource + ")"
    Else
      Return "0"
    End If
  End If

  'Check if variable
  IF NOT IsConst(DataSource) THEN
    IF BS = 0 THEN Return DataSource
    IF BS = 1 THEN Return DataSource + "_H"
    IF BS = 2 THEN Return DataSource + "_U"
    IF BS = 3 THEN Return DataSource + "_E"

    IF BS = 4 THEN Return DataSource + "_A"
    IF BS = 5 THEN Return DataSource + "_B"
    IF BS = 6 THEN Return DataSource + "_C"
    IF BS = 7 THEN Return DataSource + "_D"
  END IF

  OutVal = 0

  'Convert to decimal
  IF DataSource = Str(ValLng(DataSource)) THEN OutVal = VALLng(DataSource)

  IF INSTR(LCase(DataSource), "b'") <> 0 THEN
    Temp = Mid(DataSource, INSTR(LCase(DataSource), "b'") + 2)
    Replace Temp, "'", ""
    OutVal = 0
    FOR PD = 1 TO LEN(Temp)
      OutVal = OutVal + 2 ^ (LEN(Temp) - PD) * VAL(Mid(Temp, PD, 1))
    NEXT
  END IF

  IF INSTR(LCase(DataSource), "0x") <> 0 THEN
    Temp = DataSource
    Replace Temp, "0x", "&h"
    OutVal = ValLng(Temp)
  END IF

  GetByte = Str(OutVal Shr (8 * BS) And 255)
End Function

Function GetVarByteNumber(VarName As String) As Integer
  If Mid(VarName, Len(VarName) - 1, 1) = "_" Then
    Select Case UCase(Right(VarName, 2))
      Case "_H": Return 1
      Case "_U": Return 2
      Case "_E": Return 3

      Case "_A": Return 4
      Case "_B": Return 5
      Case "_C": Return 6
      Case "_D": Return 7
    End Select
  End If

  Return 0
End Function

Function GetFileLine(Origin As String) As String
  Dim As String Temp
  Dim As Integer F, L, S, I, T

  Temp = UCase(Origin)
  IF INSTR(Temp, ";?F") = 0 Then Return ""

  'Get values
  Replace Temp, "?", ""
  F = VAL(Mid(Temp, INSTR(Temp, "F") + 1)) 'File
  L = VAL(Mid(Temp, INSTR(Temp, "L") + 1)) 'Line

  'If F and L = 0, location isn't known
  If F = 0 And L = 0 Then
    Return ""

  Else
    'Trim filename of path
    Fi = SourceFile(F).FileName
    FOR T = LEN(Fi) to 1 step -1
      IF Mid(Fi, T, 1) = "\" Or Mid(Fi, T, 1) = "/" Then Fi = Mid(Fi, T + 1): Exit For
    NEXT
    'Prepare message
    Return Fi + " (" + Str(L) + ")"

  End If
End Function

Function GetNextTempVar(CurrVar As String) As String
  'Return next name for temporary variable if CurrVar is invalid
  Dim As String BeforeNumber, Number, AfterNumber
  Dim As Integer CurrCharNo, CurrChar, State

  'If name is known and has a number in it, increment number and return that
  State = 0
  BeforeNumber = ""
  Number = ""
  AfterNumber = ""
  For CurrCharNo = 1 To Len(CurrVar)
    CurrChar = Asc(Mid(CurrVar, CurrCharNo, 1))
    If CurrChar >= 48 And CurrChar <= 57 Then
      If State = 0 Or State = 1 Then
        State = 1
        Number += Mid(CurrVar, CurrCharNo, 1)
      Else
        'Found another number, use this one
        State = 1
        BeforeNumber = BeforeNumber + Number + AfterNumber
        Number = Mid(CurrVar, CurrCharNo, 1)
        AfterNumber = ""
      End If
    Else
      If State = 0 Then
        BeforeNumber += Mid(CurrVar, CurrCharNo, 1)
      ElseIf State = 1 Or State = 2Then
        State = 2
        AfterNumber += Mid(CurrVar, CurrCharNo, 1)
      End If
    End If
  Next

  Return BeforeNumber + Trim(Str(Val(Number) + 1)) + AfterNumber

End Function

Function GetOriginString(OriginIn As OriginType Pointer) As String
  If OriginIn = 0 Then Return ""

  Return ";?F" + Str(OriginIn->FileNo) + "L" + Str(OriginIn->LineNo) + "S" + Str(OriginIn->SubNo) + "?"
End Function

Function GetDoubleBytes (InValue As Double) As ULongInt
  'Get raw bytes for a double precision float
  Return *CPtr(ULongInt Pointer, @InValue)
End Function

Function GetSingleBytes (InValue As Single) As UInteger
  'Get raw bytes for a single precision float
  Return *CPtr(UInteger Pointer, @InValue)
End Function

Function GetString (StringName As String, UsedInProgram As Integer = -1) As String

  Dim As Integer ST

  If Instr(UCASE(StringName), ";STRING") = 0 Then Return ""
  ST = VAL(Mid(StringName, 8))

  If UsedInProgram Then
    StringStore(ST).Used = -1
  End If

  Return StringStore(ST).Value

End Function

Sub GetTokens(InData As String, OutArray() As String, ByRef OutSize As Integer, DivChar As String = "", IncludeDividers As Integer = 0)

  'Return list of tokens from InData
  'DivChar is list of characters that separate tokens

  Dim As Integer GetChar, DivMode, CharIsDiv
  Dim As String CurrToken, CurrChar

  DivMode = 0
  If DivChar = "" Then DivMode = 1

  OutSize = 0

  CurrToken = ""
  For GetChar = 1 To Len(InData)
    CurrChar = Mid(InData, GetChar, 1)
    CharIsDiv = 0
    If DivMode = 0 Then
      If InStr(DivChar, CurrChar) <> 0 Then CharIsDiv = -1
    ElseIf DivMode = 1 Then
      If IsDivider(CurrChar) Then CharIsDiv = -1
    End If

    If CharIsDiv Then
      If CurrToken <> "" Then
        OutSize += 1: OutArray(OutSize) = Trim(CurrToken)
      End If
      If IncludeDividers Then
        OutSize += 1: OutArray(OutSize) = CurrChar
      End If
      CurrToken = ""
    Else
      CurrToken += CurrChar
    End If
  Next

  If CurrToken <> "" Then
    OutSize += 1: OutArray(OutSize) = Trim(CurrToken)
  End If

End Sub

Function GetElements(InData As String, DivChar As String = "", IncludeDividers As Integer = 0) As LinkedListElement Pointer

  'Return list of tokens from InData
  'DivChar is list of characters that separate tokens

  Dim As Integer GetChar, DivMode, CharIsDiv
  Dim As String CurrToken, CurrChar
  Dim As LinkedListElement Pointer OutList, CurrPos

  OutList = LinkedListCreate
  CurrPos = OutList

  DivMode = 0
  If DivChar = "" Then DivMode = 1

  CurrToken = ""
  For GetChar = 1 To Len(InData)
    CurrChar = Mid(InData, GetChar, 1)
    CharIsDiv = 0
    If DivMode = 0 Then
      If InStr(DivChar, CurrChar) <> 0 Then CharIsDiv = -1
    Else
      If IsDivider(CurrChar) Then CharIsDiv = -1
    End If

    If CharIsDiv Then
      If CurrToken <> "" Then
        CurrPos = LinkedListInsert(CurrPos, Trim(CurrToken))
      End If
      If IncludeDividers Then
        CurrPos = LinkedListInsert(CurrPos, CurrChar)
      End If
      CurrToken = ""
    Else
      CurrToken += CurrChar
    End If
  Next

  If CurrToken <> "" Then
    CurrPos = LinkedListInsert(CurrPos, Trim(CurrToken))
  End If

  Return OutList
End Function

Function GetTypeLetter(InType As String) As String
  Select Case UCase(InType)
    Case "BIT", "BYTE", "WORD", "INTEGER", "LONG", "ULONGINT", "LONGINT", "SINGLE", "DOUBLE", "STRING": Return UCase(InType) + ":"
    Case Else: Return "*:"
  End Select

End Function

Function GetTypeSize(InType As String) As Integer
  'Returns the amount of RAM that a given data type will use

  Select Case UCase(InType)
    'Byte variables take 1 byte of RAM
    Case "BYTE": Return 1
    'Word and Integer variables take 2 bytes
    Case "WORD", "INTEGER": Return 2
    'Long variables take 4 bytes
    Case "LONG": Return 4

    'Single variables take 4 bytes
    Case "SINGLE": Return 4
    'Double variables take 8 bytes
    Case "DOUBLE": Return 8

    'Long Int variables take 8 bytes
    Case "ULONGINT", "LONGINT": Return 8

    'String variables have a different default size depending on available RAM
    Case "STRING":
      If ChipRam < 16 Then
        Return 10
      ElseIf ChipRam < 368 Then
        Return 20
      Else
        Return 40
      End If
  End Select

  Return 1
End Function

FUNCTION IsCalc (Temp As String) As Integer
  Dim As String DataSource

  DataSource = Temp
  IF Left(DataSource, 1) = "-" And Mid(DataSource, 2, 1) = Str(VAL(Mid(DataSource, 2, 1))) Then DataSource = Mid(DataSource, 2)

  IsCalc = 0
  IF INSTR(DataSource, "+") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "-") <> 0 THEN IsCalc = -1

  IF INSTR(DataSource, "*") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "/") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "%") <> 0 THEN IsCalc = -1

  IF INSTR(DataSource, "=") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "~") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "<") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, ">") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "{") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "}") <> 0 THEN IsCalc = -1

  IF INSTR(DataSource, "&") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "|") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "!") <> 0 THEN IsCalc = -1
  IF INSTR(DataSource, "#") <> 0 THEN IsCalc = -1

  IF WholeINSTR(DataSource, "low") = 2 THEN IsCalc = -1
  IF WholeINSTR(DataSource, "high") = 2 THEN IsCalc = -1
  IF WholeINSTR(DataSource, "upper") = 2 THEN IsCalc = -1
  IF WholeINSTR(DataSource, "and") = 2 THEN IsCalc = -1
  IF WholeINSTR(DataSource, "or") = 2 THEN IsCalc = -1
  IF WholeINSTR(DataSource, "xor") = 2 THEN IsCalc = -1
  IF WholeINSTR(DataSource, "not") = 2 THEN IsCalc = -1

END FUNCTION

FUNCTION IsCalcDivider (Temp As String) As Integer

  Select Case Temp
    Case "", " ", "(", ")", "+", "-", "*", "/", "%", "=", "!", "<", ">", "{", "}", "~", "&", "|", "#":
      Return -1
    Case Else:
      Return 0
  End Select

END FUNCTION

FUNCTION IsConst (DataSource As String) As Integer
  'Checks for a constant (literal) value
  'Accepted as literals:
  ' - Decimal numbers
  ' - Binary or hex, denoted with b' or 0x
  ' - Addresses, denoted with @
  ' - Strings, denoted with ;STRING
  ' - Any of the above with type cast in square brackets
  'Not accepted as literals:
  ' - Anything not listed above
  ' - Anything above but with a calculation operator

  Dim As Double StartTime
  StartTime = Timer

  Dim As Integer CurrChar, CurrLoc, CurrState
  CurrState = 0
  'States:
  '0 - could be literal
  '1 - not literal, quit
  '2 - inside type cast, stay in 2 until closing bracket
  '3 - definite literal
  'For CurrLoc = 1 To Len(DataSource)
  ' CurrChar = Asc(DataSource, CurrLoc)
  ' If CurrChar = Asc("[") Then
  '   CurrState = 2
  ' ElseIf CurrChar = Asc("]") And CurrState = 2 Then
  '   CurrState = 0
  ' ElseIf CurrState = 0 Then
  '   If CurrChar = Asc("B") And Asc(DataSource, CurrLoc + 1) = Asc("'") Then
  '
  '   EndIf
  ' End If
  'Next
  '
  Dim As String Temp

  Temp = UCase(Trim(DelType(DataSource)))
  IF Left(Temp, 1) = "-" THEN Temp = Mid(Temp, 2)

  IsConst = 0
  IF Trim(Temp) = Trim(Str(VAL(Temp))) THEN IsConst = -1
  IF Left(Temp, 2) = "B'" THEN IsConst = -1
  IF Left(Temp, 2) = "0X" THEN IsConst = -1

  IF INSTR(Temp, "@") <> 0 THEN IsConst = -1
  If INSTR(Temp, ";STRING") <> 0 Then IsConst = -1

  IF INSTR(Temp, "+") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "-") > 1 THEN IsConst = 0
  IF INSTR(Temp, "*") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "/") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "%") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "&") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "|") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "!") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "#") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "=") <> 0 THEN IsConst = 0
  IF INSTR(Temp, "<") <> 0 THEN IsConst = 0
  IF INSTR(Temp, ">") <> 0 THEN IsConst = 0

  'DebugTime += (Timer - StartTime)

END FUNCTION

FUNCTION IsDivider (Temp As String) As Integer
  Select Case Asc(Temp)
    Case Asc("A") To Asc("Z"), Asc("a") To Asc("z"), Asc("0") To Asc("9"): Return 0
    Case Asc(" "), Asc("("), Asc(")"), Asc(","), Asc("."), Asc(":"), Asc(";"), Asc("+"), Asc("-"), Asc("*"), Asc("/"), Asc("%"): Return -1
    Case Asc("="), Asc("!"), Asc("<"), Asc(">"), Asc("{"), Asc("}"), Asc("~"), Asc("&"), Asc("|"), Asc("#"): Return -1
    Case Asc("["), Asc("]"), 9, 160: Return -1
    Case Else: Return 0
  End Select

END FUNCTION

Function IsFloatType(InType As String) As Integer
  'Returns true (-1) if input type is a floating point variable
  Dim ThisType As String
  ThisType = LCase(InType)

  Select Case ThisType
    Case "single", "double": Return -1
    Case Else: Return 0
  End Select

End Function

Function IsIntType(InType As String) As Integer
  'Returns true (-1) if input type is an integer variable
  Dim ThisType As String
  ThisType = LCase(InType)

  Select Case ThisType
    Case "const", "byte", "word", "integer", "long", "ulongint", "longint": Return -1
    Case Else: Return 0
  End Select

End Function

FUNCTION IsLet (DataSource As String) As Integer

  Dim As String Temp
  Dim As Integer EqLoc, CD, SpaceCount

  IF INSTR(DataSource, "=") = 0 Then Return 0
  Temp = DataSource

  Do While INSTR(Temp, "=") <> 0: Replace Temp, "=", Chr(27): Loop
  Do While INSTR(Temp, Chr(27)) <> 0: Replace Temp, Chr(27), " = ": Loop
  Do While INSTR(Temp, "  ") <> 0: Replace Temp, "  ", " ": Loop

  EqLoc = INSTR(Temp, "=") - 1
  SpaceCount = 0
  For CD = EqLoc To 1 Step -1
    If Mid(Temp, CD, 1) = " " Then SpaceCount += 1
  Next
  If SpaceCount = 1 Then IsLet = -1

END Function

Function IsSysTemp(VarNameIn As String) As Integer
  'Check if a variable name is a system temp variable
  '(Check if name is SysTempn, where n is a number, possibly followed by _H, _U or _E)
  Dim StartPos As Integer = 8
  Dim CurrChar As UInteger
  Dim VarName As String

  VarName = UCase(Trim(VarNameIn))
  If Left(VarName, 7) <> "SYSTEMP" Then Return 0
  If Len(VarName) < 8 Then Return 0

  Do While StartPos < Len(VarName)
    'Check for a digit
    CurrChar = Asc(Mid(VarName, StartPos, 1))
    If CurrChar >= 48 And CurrChar <= 57 Then
      StartPos += 1
      GoTo ValidChar
    EndIf
    'Check for a byte specifier
    Select Case Mid(VarName, StartPos, 2)
      Case "_H", "_U", "_E"
        StartPos += 2
        GoTo ValidChar
    End Select
    'Found something invalid, so not SysTemp variable
    Return 0
    ValidChar:
  Loop

  'Got to the end of the name without anything invalid, so must be valid!
  Return -1
End Function

Function IsValidName(InName As String) As Integer
  'Check if a name is a valid name for a variable, label or subroutine
  Dim CurrCharNo As Integer
  Dim CurrChar As String
  For CurrCharNo = 1 To Len(InName)
    CurrChar = Mid(InName, CurrCharNo, 1)
    If CurrChar >= "A" And CurrChar <= "Z" Then GoTo ThisCharValid
    If CurrChar >= "a" And CurrChar <= "z" Then GoTo ThisCharValid
    If CurrCharNo > 1 And (CurrChar >= "0" And CurrChar <= "9") Then GoTo ThisCharValid
    If CurrChar = "_" Then GoTo ThisCharValid

    'Accept $ as last character (old string variables
    If CurrCharNo = Len(InName) And CurrChar = "$" Then Goto ThisCharValid

    'Character wasn't valid
    Return 0

    ThisCharValid:
  Next

  Return -1
End Function

Function IsValidValue(InValue As LongInt, TypeIn As String) As Integer
  'Check if a value is allowed for the given data type
  Dim ValType As String
  ValType = LCase(TypeIn)
  Dim As Double MinVal, MaxVal

  'Assume float types can handle pretty much anything
  If IsFloatType(TypeIn) Then Return -1

  'Find allowed range based on data type
  Select Case ValType
    Case "bit"
      MinVal = 0
      MaxVal = 1
    Case "byte"
      MinVal = 0
      MaxVal = 255
    Case "word"
      MinVal = 0
      MaxVal = 65535
    Case "integer"
      MinVal = -32768
      MaxVal = 32767
    Case "long"
      MinVal = 0
      MaxVal = 2 ^ 32 - 1
    Case "ulongint":
      MinVal = 0
      MaxVal = 2 ^ 64 - 1
    Case "longint":
      MinVal = - (2 ^ 63)
      MaxVal = 2 ^ 63 - 1
    Case Else
      'Unknown type, assume not compatible
      Return 0
  End Select

  'Is value within range?
  If InValue < MinVal Or InValue > MaxVal Then
    Return 0
  Else
    Return -1
  End If

End Function

FUNCTION MakeDec (Temp As String) As LongInt

  Dim As String DataSource, StrTemp
  Dim As Integer CB
  Dim As LongInt T

  DataSource = Trim(UCase(Temp))

  If InStr(DataSource, "@") Then
    'Print "Trying to make " + Temp + " into a decimal!"
  End If

  If Instr(DataSource, "[BYTE]") <> 0 Then Replace DataSource, "[BYTE]", ""
  If Instr(DataSource, "[WORD]") <> 0 Then Replace DataSource, "[WORD]", ""
  If Instr(DataSource, "[INTEGER]") <> 0 Then Replace DataSource, "[INTEGER]", ""
  If Instr(DataSource, "[LONG]") <> 0 Then Replace DataSource, "[LONG]", ""
  If Instr(DataSource, "[SINGLE]") <> 0 Then Replace DataSource, "[SINGLE]", ""
  If Instr(DataSource, "[DOUBLE]") <> 0 Then Replace DataSource, "[DOUBLE]", ""

  IF INSTR(DataSource, "0X") <> 0 THEN
    DataSource = Mid(DataSource, INSTR(DataSource, "0X") + 2)
    Return ValLng("&H" + DataSource)
  END IF

  IF DataSource = Str(VAL(DataSource)) THEN
    Return ValLng(DataSource)
  END IF

  IF INSTR(DataSource, "B'") <> 0 THEN
    DataSource = Mid(DataSource, INSTR(DataSource, "B'") + 2)
    DataSource = Left(DataSource, INSTR(DataSource, "'") - 1)
    T = 0
    FOR CB = LEN(DataSource) TO 1 STEP -1
      T = T + VAL(Mid(DataSource, LEN(DataSource) - CB + 1, 1)) * 2 ^ (CB - 1)
    NEXT
    Return T
  END If

  StrTemp = GetString(DataSource, 0)
  IF Len(StrTemp) = 1 Then Return ASC(StrTemp)

End Function

FUNCTION MakeDecFloat (Temp As String) As Double

  Dim As String DataSource, StrTemp

  DataSource = Trim(UCase(Temp))

  If InStr(DataSource, "@") Then
    'Print "Trying to make " + Temp + " into a decimal!"
  End If

  If Instr(DataSource, "[BYTE]") <> 0 Then Replace DataSource, "[BYTE]", ""
  If Instr(DataSource, "[WORD]") <> 0 Then Replace DataSource, "[WORD]", ""
  If Instr(DataSource, "[INTEGER]") <> 0 Then Replace DataSource, "[INTEGER]", ""
  If Instr(DataSource, "[LONG]") <> 0 Then Replace DataSource, "[LONG]", ""
  If Instr(DataSource, "[SINGLE]") <> 0 Then Replace DataSource, "[SINGLE]", ""
  If Instr(DataSource, "[DOUBLE]") <> 0 Then Replace DataSource, "[DOUBLE]", ""

  IF DataSource = Str(VAL(DataSource)) THEN
    Return Val(DataSource)
  END If

  StrTemp = GetString(DataSource, 0)
  IF Len(StrTemp) = 1 Then Return ASC(StrTemp)

  Return 0
End Function

Function NCase(InValue As String) As String
  Return UCase(Left(InValue, 1)) + LCase(Mid(InValue, 2))
End Function

SUB Replace (DataVar As String, Find As String, Rep As String)
  Dim As String VarTemp, FindTemp, NewData

  VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
  IF INSTR(VARTemp, FINDTemp) = 0 THEN DataVar = DataVar + Rep: EXIT SUB

  NewData = Left(DataVar, INSTR(VARTemp, FINDTemp) - 1)
  NewData = NewData + Rep
  NewData = NewData + Mid(DataVar, INSTR(VARTemp, FINDTemp) + LEN(Find))

  DataVar = NewData
END Sub

SUB ReplaceAll (DataVar As String, Find As String, Rep As String)

  Do While InStr(UCase(DataVar), UCase(Find)) <> 0
    Replace DataVar, Find, Rep
  Loop

End Sub

Function ReplaceToolVariables(InData As String, FNExtension As String = "", FileNameIn As String = "", Tool As ExternalTool Pointer = 0) As String

  Dim As String FileName, FileNameNoExt, OutData
  Dim As String FileNoPath, PathNoFile
  Dim As Integer PD
  Dim As String portstring, temp
  Dim As LinkedListElement Pointer CurrToolVar
  OutData = InData

  If FileNameIn = "" Then
    FileName = OFI
  Else
    FileName = FileNameIn
  End If

  'Get filename without extension
  For PD = Len(FileName) To 1 Step -1
    If Mid(FileName, PD, 1) = "." Then
      FileNameNoExt = Left(FileName, PD - 1)
      Exit For
    End If
  Next
  'If requested, change filename extension
  If FNExtension <> "" Then
    FileName = FileNameNoExt + "." + FNExtension
  End If

  'Functions
  Dim As String FunctionParams, FunctionResult, FunctionName
  Dim As Integer BL, FStart, FParams, FEnd, FindSlash

  Dim As String FunctionList(100)
  Dim As Integer FunctionCount, CurrFn
  FunctionCount = 2
  FunctionList(1) = "NAMEPART"
  FunctionList(2) = "PATHPART"

  For CurrFn = 1 To FunctionCount
    FunctionName = FunctionList(CurrFn)

    Do While InStr(UCase(OutData), "%" + FunctionName + "%") <> 0
      'Find parameters
      FStart = InStr(UCase(OutData), "%" + FunctionName + "%")
      FParams = FStart + 2 + Len(FunctionName)
      For PD = FParams To Len(OutData)
        If Mid(OutData, PD, 1) = "(" Then BL += 1
        If Mid(OutData, PD, 1) = ")" Then BL -= 1
        If BL = 0 Then
          FEnd = PD

          'Get and tidy parameters
          FunctionParams = Mid(OutData, FParams + 1, FEnd - FParams - 1)
          FunctionParams = ReplaceToolVariables(FunctionParams, FNExtension)

          'Perform function
          Select Case FunctionName
            Case "PATHPART":
              'Get path part of filename
              FunctionResult = CurDir
              For FindSlash = Len(FunctionParams) To 1 Step -1
                If Mid(FunctionParams, FindSlash, 1) = "/" Or Mid(FunctionParams, FindSlash, 1) = "\" Then
                  FunctionResult = Left(FunctionParams, FindSlash - 1)
                  Exit For
                End If
              Next

            Case "NAMEPART":
              'Get name part of filename
              FunctionResult = FunctionParams
              For PD = Len(FunctionParams) To 1 Step -1
                If Mid(FunctionParams, PD, 1) = "/" Or Mid(FunctionParams, PD, 1) = "\" Then
                  FunctionResult = Mid(FunctionParams, PD + 1)
                  Exit For
                End If
              Next

            Case Else:
              FunctionResult = ""
          End Select

          'Put result back into output
          OutData = Left(OutData, FStart - 1) + FunctionResult + Mid(OutData, FEnd + 1)
          Exit For
        End If
      Next
    Loop
  Next

  'Extra tool variables
  If Tool <> 0 Then
    With *Tool
      For PD = 1 To .ExtraParams
        Do While INSTR(LCase(OutData), "%" + .ExtraParam(PD, 1) + "%") <> 0

          'Check specified Serial Port is avaiable
          If Lcase(.ExtraParam(PD, 1)) = "port" And Left(Trim(Lcase(.ExtraParam(PD, 2))),3) = "com"  Then
              portstring = Trim(.ExtraParam(PD, 2))+":115200,N,8,1"
              On Error Goto PortOpenHandler
              Open Com portstring as 99
              PortOpenHandler:
              On Error Goto 0
              If ERR > 1 Then
                  temp = Message("SerialPortLockedorNotAvailable")
                  Replace temp, "%port%", Trim(.ExtraParam(PD, 2))
                  LogError temp
                  Exit Function
              End If
               '1 if no device attached
              On Error Goto PortCloseHandler
              Close 99
              PortCloseHandler:
              On Error Goto 0
          End If

          Replace OutData, "%" + .ExtraParam(PD, 1) + "%", .ExtraParam(PD, 2)
        Loop
      Next
    End With
  End If

  'Global tool variables
  CurrToolVar = ToolVariables->Next
  Do While CurrToolVar <> 0
    Do While INSTR(LCase(OutData), "%" + CurrToolVar->Value + "%") <> 0
      Replace OutData, "%" + CurrToolVar->Value+ "%", *CPtr(String Pointer, CurrToolVar->MetaData)
    Loop
    CurrToolVar = CurrToolVar->Next
  Loop

  'Items typically found in parameters
  Do While INSTR(UCase(OutData), "%FILENAME%") <> 0: Replace OutData, "%FILENAME%", FileName: Loop
  Do While INSTR(UCase(OutData), "%SHORTNAME%") <> 0: Replace OutData, "%SHORTNAME%", ShortFileName(FileName): Loop
  Do While INSTR(UCase(OutData), "%FN_NOEXT%") <> 0: Replace OutData, "%FN_NOEXT%", FileNameNoExt: Loop
  Do While INSTR(UCase(OutData), "%CHIPMODEL%") <> 0: Replace OutData, "%CHIPMODEL%", ChipName: Loop
  Do While INSTR(UCase(OutData), "%CHIPPROGRAMMERNAME%") <> 0: Replace OutData, "%CHIPPROGRAMMERNAME%", Chipprogrammername: Loop




  'Items typically found in paths
  Do While InStr(LCase(OutData),"%appdata%") <> 0: Replace OutData, "%appdata%", Environ("APPDATA"): Loop
  Do While InStr(LCase(OutData),"%allusersappdata%") <> 0: Replace OutData, "%allusersappdata%", Environ("ALLUSERSPROFILE") + "\Application Data": Loop
  Do While InStr(LCase(OutData),"%temp%") <> 0: Replace OutData, "%temp%", Environ("TEMP"): Loop
  Do While InStr(LCase(OutData),"%instdir%") <> 0: Replace OutData, "%instdir%", ID: Loop

  Return OutData
End Function

SUB SCICONV (STemp As String)
  Dim As String Temp, DataSource
  Dim As String V, PS, VI, VF
  Dim As Integer P, S, DV, M, A, AddZero

  DO WHILE INSTR(STemp, " ") <> 0: Replace STemp, " ", "": LOOP
  STemp = UCase(STemp)

  IF INSTR(UCase(STemp), "E") <> 0 THEN
    'Temp = "00000000000000000000"
    'V = Left(STemp, INSTR(STemp, "E") - 1)
    'PS = Mid(STemp, INSTR(STemp, "E") + 1)
    'Replace V, ".", ""
    'P = VAL(PS)
    'S = 1: IF INSTR(V, "-") <> 0 THEN S = -1: Replace V, "-", ""
    'IF SGN(P) = -1 THEN STemp = "0." + Left(Temp, P * -1 - 1) + V
    'IF SGN(P) = 1 THEN STemp = V + Left(Temp, P)
    'IF S = -1 THEN STemp = "-" + STemp

    V = Left(STemp, INSTR(STemp, "E") - 1)
    P = Val(Mid(STemp, INSTR(STemp, "E") + 1))
    S = 1: If INSTR(V, "-") <> 0 THEN S = -1: Replace V, "-", ""

    IF SGN(P) = -1 Then
      STemp = "0."
      For AddZero = 1 To (-P - 1)
        STemp = STemp + "0"
      Next
      Replace V, ".", ""
      STemp = STemp + V
    Else
      'Get integer and fractional part of number
      If InStr(V, ".") <> 0 Then
        VI = Left(V, InStr(V, ".") - 1)
        VF = Mid(V, InStr(V, ".") + 1)
      Else
        VI = V
        VF = ""
      End If
      'Combine without decimal
      STemp = VI + VF
      'Add zeros
      For AddZero = 1 To P - Len(VF)
        STemp = STemp + "0"
      Next
    End If

    IF S = -1 THEN STemp = "-" + STemp

  END IF

  IF INSTR(UCase(STemp), "D") <> 0 THEN
    Temp = "00000000000000000000"
    DV = VAL(Mid(STemp, INSTR(STemp, "D") + 1))
    STemp = Left(STemp, INSTR(STemp, "D") - 1)
    DV = DV * SGN(DV)
    IF DV > 1 THEN DV = DV - 1
    DataSource = STemp
    Replace DataSource, ".", ""
    Replace DataSource, "-", ""
    Replace DataSource, " ", ""
    M = 0: IF INSTR(STemp, "-") <> 0 THEN M = 1
    IF DV > 7 THEN STemp = ".0": EXIT SUB
    A = 7 - DV
    STemp = "." + Left(Temp, DV)
    STemp = STemp + Left(DataSource, A)
    IF M = 1 THEN STemp = "-" + STemp
  END IF

  DO WHILE INSTR(STemp, " ") <> 0: Replace STemp, " ", "": LOOP

END SUB

Function ShortFileName(InName As String) As String

  #Ifdef __FB_WIN32__
    Dim As String OutName = Space(256)
    GetShortPathName(InName, OutName, 256)
    Return Trim(OutName)
  #Else
    'No need for such silliness on Linux
    Return InName
  #endif

End Function

FUNCTION ShortName (NameIn As String) As String

  Dim As String TempData
  TempData = NameIn

  'Get rid of "s
  IF INSTR(TempData, Chr(34)) <> 0 THEN
    TempData = Mid(TempData, INSTR(TempData, Chr(34)) + 1)
    TempData = Left(TempData, INSTR(TempData, Chr(34)) - 1)
  END IF

  'UNIX filename conversion
  #IFDEF __FB_UNIX__
    DO WHILE INSTR(TempData, "\") <> 0: Replace TempData, "\", "/": LOOP
  #ENDIF

  ShortName = Trim(TempData)

END Function

Function HashMapCreate As HashMap Pointer
  'Create new hash map
  Dim As HashMap Pointer OutMap
  OutMap = Callocate(SizeOf(HashMap))

  Return OutMap
End Function

Sub HashMapDestroy(Map As HashMap Pointer)
  Dim CurrBucket As Integer

  If Map = 0 Then Exit Sub

  'Clear every bucket
  For CurrBucket = 0 To HASH_MAP_BUCKETS
    If Map->Buckets(CurrBucket) <> 0 Then
      LinkedListDeleteList(Map->Buckets(CurrBucket), 0)
    End If
  Next

  'Deallocate map
  DeAllocate Map
End Sub

Function HashMapCalcHash(Key As String) As Integer
  'Calculate hash
  Dim As UInteger HashOut, CurrChar

  HashOut = 2166136261
  For CurrChar = 1 To Len(Key)
    HashOut = (HashOut XOR Asc(Key, CurrChar)) * 16777619
  Next

  HashOut = (HashOut Shr 16) Xor HashOut

  Return HashOut Mod HASH_MAP_BUCKETS
End Function

Sub HashMapDelete(Map As HashMap Pointer, Key As String, DeleteMeta As Integer = -1)
  Dim Bucket As Integer
  Dim CurrItem As LinkedListElement Pointer

  If Map = 0 Then Exit Sub

  'If nothing in the appropriate bucket, item isn't in the map
  Bucket = HashMapCalcHash(Key)
  If Map->Buckets(Bucket) = 0 Then
    Exit Sub
  End If

  'Look in the bucket
  CurrItem = Map->Buckets(Bucket)->Next
  Do While CurrItem <> 0
    If CurrItem->Value = Key Then
      LinkedListDelete(CurrItem, DeleteMeta)
      Exit Sub
    End If
    CurrItem = CurrItem->Next
  Loop
End Sub

Function HashMapGet(Map As HashMap Pointer, Key As String) As Any Pointer
  Dim Bucket As Integer
  Dim CurrItem As LinkedListElement Pointer

  If Map = 0 Then Return 0

  'If nothing in the appropriate bucket, item isn't in the map
  Bucket = HashMapCalcHash(Key)
  If Map->Buckets(Bucket) = 0 Then
    Return 0
  End If

  'Look in the bucket
  CurrItem = Map->Buckets(Bucket)->Next
  Do While CurrItem <> 0
    If CurrItem->Value = Key Then
      Return CurrItem->MetaData
    End If
    CurrItem = CurrItem->Next
  Loop

  'Item not in bucket
  Return 0
End Function

Function HashMapGetStr(Map As HashMap Pointer, Key As String) As String
  Dim As String Pointer TempStr

  TempStr = HashMapGet(Map, Key)
  If TempStr = 0 Then
    Return ""
  Else
    Return *TempStr
  EndIf
End Function

Function HashMapSet(Map As HashMap Pointer, Key As String, Value As String, ReplaceExisting As Integer = -1) As Integer

  Dim As String Pointer TempStr
  TempStr = Callocate(SizeOf(String))
  *TempStr = Value

  Return HashMapSet(Map, Key, TempStr, ReplaceExisting)

End Function

Function HashMapSet(Map As HashMap Pointer, Key As String, Value As Any Pointer, ReplaceExisting As Integer = -1) As Integer
  'Returns -1 if set succeeded, or 0 if it did not (duplicate key and not replacing)
  Dim Bucket As Integer
  Dim As LinkedListElement Pointer CurrElement, NewElement

  If Map = 0 Then Return 0

  'Get bucket to put item in
  Bucket = HashMapCalcHash(Key)
  If Map->Buckets(Bucket) = 0 Then
    Map->Buckets(Bucket) = LinkedListCreate
  End If

  'Put new item in bucket
  CurrElement = Map->Buckets(Bucket)->Next
  'Is item with same key in bucket? If so, update
  Do While CurrElement <> 0
    If CurrElement->Value = Key Then
      If ReplaceExisting Then
        CurrElement->MetaData = Value
        Return -1
      End If
      Return 0
    End If
    CurrElement = CurrElement->Next
  Loop
  'Item wasn't in the bucket, so add it
  NewElement = LinkedListInsert(Map->Buckets(Bucket), Value)
  NewElement->Value = Key

  Return -1
End Function

Function HashMapToList(Map As HashMap Pointer, Sorted As Integer = 0) As LinkedListElement Pointer
  'Copy items from hashmap to a linked list
  'Sorted flag should be set to -1 to sort alphabetically by key
  Dim As LinkedListElement Pointer NewList, InsertLoc, BucketLoc
  Dim As Integer CurrBucket

  NewList = LinkedListCreate
  InsertLoc = NewList

  For CurrBucket = 0 To HASH_MAP_BUCKETS - 1
    If Map->Buckets(CurrBucket) <> 0 Then
      'Found bucket, copy contents
      BucketLoc = Map->Buckets(CurrBucket)->Next
      Do While BucketLoc <> 0
        If Sorted Then
          'Find location to insert at
          InsertLoc = NewList
          Do While InsertLoc->Next <> 0
            InsertLoc = InsertLoc->Next
            If InsertLoc->Value > BucketLoc->Value Then
              InsertLoc = InsertLoc->Prev
              Exit Do
            End If
          Loop
        End If
        InsertLoc = LinkedListInsert(InsertLoc, BucketLoc->Value)
        InsertLoc->MetaData = BucketLoc->MetaData
        BucketLoc = BucketLoc->Next
      Loop
    End If
  Next

  Return NewList
End Function

Function LinkedListCreate As LinkedListElement Pointer

  Dim As LinkedListElement Pointer NewElement
  NewElement = Callocate(SizeOf(LinkedListElement))
  NewElement->Prev = 0
  NewElement->Next = 0
  NewElement->MetaData = 0

  Return NewElement
End Function

Function LinkedListInsert (Location As LinkedListElement Pointer, NewLine As String, NewNumVal As Integer = 0) As LinkedListElement Pointer
  Dim As LinkedListElement Pointer NewCodeLine
  NewCodeLine = Callocate(SizeOf(LinkedListElement))

  If Location = 0 Then
    Print "Internal error in LinkedListInsert: No location for " + NewLine
    Return 0
  End If

  With *NewCodeLine
    .Value = NewLine
    .NumVal = NewNumVal
    .Prev = Location
    .Next = Location->Next
    Location->Next = NewCodeLine
    If .Next <> 0 Then .Next->Prev = NewCodeLine
  End With

  Return NewCodeLine
End Function

Function LinkedListInsert (Location As LinkedListElement Pointer, NewData As Any Pointer) As LinkedListElement Pointer
  Dim As LinkedListElement Pointer NewCodeLine
  NewCodeLine = Callocate(SizeOf(LinkedListElement))

  With *NewCodeLine
    .Value = "-"
    .MetaData = NewData
    .Prev = Location
    .Next = Location->Next
    Location->Next = NewCodeLine
    If .Next <> 0 Then .Next->Prev = NewCodeLine
  End With

  Return NewCodeLine
End Function


Function LinkedListInsertList (Location As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
  Dim As LinkedListElement Pointer NewListEnd, AfterLocation, CurrPos, StartNode

  'If no list passed in, do nothing
  If NewList = 0 Then Return Location

  'No location passed in, return null
  If Location = 0 Then Return 0

  'Don't do anything if new list is empty
  '(Need to check next because lists always have empty sentinel node at start)
  If NewList->Next = 0 Then Return Location

  'Get next element after location
  AfterLocation = Location->Next

  'Add new list after location
  Location->Next = NewList->Next
  NewList->Next->Prev = Location

  'Find end of new list
  If NewListEndIn = 0 Then
    CurrPos = NewList
    StartNode = 0
    Do While (CurrPos->Next <> 0 And StartNode <> CurrPos->Next)
      'Detect circular loops, should not happen!
      If StartNode = 0 Then StartNode = NewList->Next
      CurrPos = CurrPos->Next
    Loop
    If StartNode = CurrPos->Next Then
      Print "Internal error: circular or damaged list"
    End If
    NewListEnd = CurrPos
  Else
    NewListEnd = NewListEndIn
  End If

  'Found end, reattach rest of old list
  If AfterLocation <> 0 Then
    NewListEnd->Next = AfterLocation
    AfterLocation->Prev = NewListEnd
  End If

  Return NewListEnd
End Function

Function LinkedListAppend (ListIn As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
  Dim As LinkedListElement Pointer NewListEnd, CurrPos, Location

  'Get end of list
  Location = ListIn
  Do While Location->Next <> 0
    Location = Location->Next
  Loop

  'Don't do anything if new list is empty
  '(Need to check next because lists always have empty sentinel node at start)
  If NewList->Next = 0 Then Return Location

  'Add new list after location
  Location->Next = NewList->Next
  NewList->Next->Prev = Location

  'Find end of new list
  If NewListEndIn = 0 Then
    CurrPos = NewList
    Do While CurrPos->Next <> 0
      CurrPos = CurrPos->Next
    Loop
    NewListEnd = CurrPos
  Else
    NewListEnd = NewListEndIn
  End If

  Return NewListEnd
End Function

Function LinkedListDelete (Location As LinkedListElement Pointer, DeleteMeta As Integer = -1) As LinkedListElement Pointer
  'Removes Location from the list
  'Returns the list element before Location
  Dim As LinkedListElement Pointer OldPrev, OldNext

  With *Location
    OldPrev = .Prev
    OldNext = .Next
    If .Prev <> 0 Then
      .Prev->Next = OldNext
    End If
    If .Next <> 0 Then
      .Next->Prev = OldPrev
    End If
    If .MetaData <> 0 And DeleteMeta Then
      DeAllocate .MetaData
    End If
  End With
  DeAllocate Location

  Return OldPrev
End Function

Function LinkedListDeleteList (StartLoc As LinkedListElement Pointer, EndLoc As LinkedListElement Pointer) As LinkedListElement Pointer
  Dim As LinkedListElement Pointer CurrLine, DeleteLine, LastLine

  If StartLoc = 0 Then
    Print "Internal error in LinkedListDeleteList: No start location"
    Return 0
  End If

  'Remove items from list
  'Fix next pointer of node before start
  If StartLoc->Prev <> 0 Then
     If EndLoc <> 0 Then
      StartLoc->Prev->Next = EndLoc->Next
     Else
      StartLoc->Prev->Next = 0
     End If
  End If
  LastLine = StartLoc->Prev
  'Fix prev pointer of node after end
  'If EndLoc = 0, delete the rest of the list
  If EndLoc <> 0 Then
    If EndLoc->Next <> 0 Then EndLoc->Next->Prev = StartLoc->Prev
    EndLoc->Next = 0
  End If
  'Clear prev of first node
  StartLoc->Prev = 0

  'Delete items from memory
  CurrLine = StartLoc
  Do While CurrLine <> 0
    DeleteLine = CurrLine
    CurrLine = CurrLine->Next
    DeAllocate DeleteLine
  Loop

  Return LastLine
End Function

Function LinkedListFind(StartNode As LinkedListElement Pointer, SearchValue As String) As LinkedListElement Pointer
  'Search list for item with matching metadata value
  Dim As LinkedListElement Pointer CurrPos
  Dim As String SearchUpper

  If StartNode = 0 Then Return 0
  CurrPos = StartNode->Next
  SearchUpper = UCase(SearchValue)

  Do While CurrPos <> 0
    If UCase(CurrPos->Value) = SearchUpper Then
      Return CurrPos
    End If
    CurrPos = CurrPos->Next
  Loop

End Function

Function LinkedListFind(StartNode As LinkedListElement Pointer, SearchMeta As Any Pointer) As LinkedListElement Pointer
  'Search list for item with matching metadata value
  Dim As LinkedListElement Pointer CurrPos

  If StartNode = 0 Then Return 0
  CurrPos = StartNode->Next

  Do While CurrPos <> 0
    If CurrPos->MetaData = SearchMeta Then
      Return CurrPos
    End If
    CurrPos = CurrPos->Next
  Loop

End Function

Sub LinkedListPrint(StartNode As LinkedListElement Pointer)
  Dim As LinkedListElement Pointer CurrPos

  'Check for null pointer
  If StartNode = 0 Then Exit Sub
  CurrPos = StartNode->Next

  Do While CurrPos <> 0
    Print CurrPos->Value    
    CurrPos = CurrPos->Next
  Loop
End Sub

Sub LinkedListPrintwithComments(StartNode As LinkedListElement Pointer)
  Dim As LinkedListElement Pointer CurrPos

  'Check for null pointer
  If StartNode = 0 Then Exit Sub
  CurrPos = StartNode->Next

  Do While CurrPos <> 0

    If Left(CurrPos->Value, 8) = "PRESERVE" Then
      Dim as Integer PresPos
      PresPos = VAL(Mid(CurrPos->Value, 10))
      Print PreserveCode(PresPos)
    Else
      Print CurrPos->Value
    End if

  
    
    CurrPos = CurrPos->Next
  Loop
End Sub


Sub LinkedListPrint2(StartNode As LinkedListElement Pointer, EndNode As LinkedListElement Pointer)
  Dim As LinkedListElement Pointer CurrPos
  Dim as Integer LastLineNumber, CurrentLineNumber = 0

  'CDF output file
  print #CDFFileHandle, ""

  'Check for null pointer
  If StartNode = 0 Then Exit Sub
  CurrPos = StartNode->Next

  Do While CurrPos <> 0

    If CurrPos->Prev->Value = EndNode->Value Then
      CurrentLineNumber = Val(Mid(GetMetaData ( EndNode)->OrgLine, INSTR(  Instr(GetMetaData ( EndNode)->OrgLine, "?"),  UCase(GetMetaData ( EndNode)->OrgLine), "L") + 1))
      If Instr(Ucase( GetMetaData ( EndNode)->OrgLine ), ";?F") and CurrentLineNumber <> LastLineNumber and left( GetMetaData ( EndNode)->OrgLine , 6) <> "#ENDIF" Then
        print #CDFFileHandle, Str(Val(Mid(GetMetaData ( EndNode)->OrgLine, INSTR(  Instr(GetMetaData ( EndNode)->OrgLine, "?"),  UCase(GetMetaData ( EndNode)->OrgLine), "L") + 1))) + ": " +Trim(Left( GetMetaData ( EndNode)->OrgLine, instr(GetMetaData ( EndNode)->OrgLine, ";?F" ) -1 ))
      End if
      Exit Sub
    End if
  
    LastLineNumber =Val(Mid(GetMetaData ( CurrPos)->OrgLine, INSTR(  Instr(GetMetaData ( CurrPos)->OrgLine, "?"),  UCase(GetMetaData ( CurrPos)->OrgLine), "L") + 1))
    'display cached data - need to do this first
    If Instr(Ucase( GetMetaData ( CurrPos)->OrgLine ), ";?F") and left( GetMetaData ( CurrPos)->OrgLine , 6) <> "#ENDIF" Then
      print #CDFFileHandle, ,Str(Val(Mid(GetMetaData ( CurrPos)->OrgLine, INSTR(  Instr(GetMetaData ( CurrPos)->OrgLine, "?"),  UCase(GetMetaData ( CurrPos)->OrgLine), "L") + 1))) + ": " +Trim(Left( GetMetaData ( CurrPos)->OrgLine, instr(GetMetaData ( CurrPos)->OrgLine, ";?F" ) -1 ))
    ElseIf left( CurrPos->Value, 1) = "#" AND left( CurrPos->Value, 6) <> "#ENDIF" Then
      Print #CDFFileHandle,,CurrPos->Value
    ElseIf Instr(Ucase( CurrPos->Value ), ";?F") Then
      print #CDFFileHandle, ,Str(Val(Mid(CurrPos->Value, INSTR(CurrPos->Value, "L") + 1))) + ": " +Trim(Left( CurrPos->Value, instr(CurrPos->Value, ";?F" ) -1 ))
    ElseIf Left(CurrPos->Value, 8) = "PRESERVE" Then
      Dim as Integer PresPos
      PresPos = VAL(Mid(CurrPos->Value, 10))
      print #CDFFileHandle,, PreserveCode(PresPos)
    ElseIf Left(CurrPos->Value, 8) = "REPROCES" Then
      Dim as Integer PresPos
      PresPos = VAL(Mid(CurrPos->Value, 10))
      If Instr( PreserveCode(PresPos) , "':") Then
        print #CDFFileHandle,, Str(Val(Mid(PreserveCode(PresPos), INSTR(PreserveCode(PresPos), "L") + 1))) + ": " +mid( PreserveCode(PresPos), 2, Instr( PreserveCode(PresPos), "':")-2) 
      Else
        print #CDFFileHandle,, Str(Val(Mid(PreserveCode(PresPos), INSTR(PreserveCode(PresPos), "L") + 1))) + ": " +mid( PreserveCode(PresPos), 2, Instr( PreserveCode(PresPos), ";?")-2) 
      End if

    End if
    
    CurrPos = CurrPos->Next
    Loop



End Sub

Function LinkedListSize(StartNode As LinkedListElement Pointer) As Integer
  Dim As LinkedListElement Pointer CurrPos
  Dim As Integer OutSize
  OutSize = 0
  CurrPos = StartNode->Next

  Do While CurrPos <> 0
    OutSize += 1
    CurrPos = CurrPos->Next
  Loop

  Return OutSize
End Function

Function NextCodeLine(CodeLine As LinkedListElement Pointer) As LinkedListElement Pointer
  'Get next line of code after the specified line
  'Skip over any PRESERVE lines
  Dim NextLine As LinkedListElement Pointer

  If CodeLine = 0 Then Return 0
  NextLine = CodeLine->Next
  Do While NextLine <> 0
    If Left(NextLine->Value, 9) <> "PRESERVE " Then Exit Do
    NextLine = NextLine->Next
  Loop

  Return NextLine
End Function

Function PrefIsYes(CheckVal As String, YesVal As Integer) As Integer
  Select Case LCase(Left(CheckVal, 1))
    Case "y", "t", "1": Return YesVal
    Case "n", "f", "0": Return 0
  End Select

  Return 0
End Function

Function SubSigMatch (SubSigIn As String, CallSigIn As String) As Integer

  'Print "Finding match for " + SubSigIn + " and " + CallSigIn

  Dim As Integer OutScore
  Dim As String SubSig, CallSig
  Dim As String CurrSub, CurrCall

  'Force match
  '1000000 should be way more than any other score
  If CallSigIn = "ForceMatch" Then Return 1000000

  SubSig = SubSigIn
  CallSig = CallSigIn

  'If signatures match exactly, return highest possible value
  If SubSig = CallSig Then
    If Len(SubSig) <> 0 Then
      Return 100 * Len(SubSig)
    Else
      Return 100
    End If
  End If

  'Calculate a match score
  'If parameters don't match, 0, if they can be converted, 100 / intermediate sizes, if they match, 100.
  OutScore = 0
  Do While Len(SubSig) > 0
    CurrSub = Left(SubSig, InStr(SubSig, ":") - 1)
    CurrCall = Left(CallSig, InStr(CallSig, ":") - 1)

    'Deal with optional parameters
    If CurrCall = "" Then
      If CurrSub = LCase(CurrSub) Then
        'OutScore += 2
        OutScore += 100
        GoTo CheckNextParam
      Else
        Return 0 'Can't match if default parameter not specified
      End If

    ElseIf CurrCall <> "" And CurrSub = "" Then
      Return 0
    End If

    'Parameter in call matches that in sub
    If UCase(CurrCall) = UCase(CurrSub) Then
      OutScore += 100 'was 2
      GoTo CheckNextParam
    End If

    'Parameter in call is of lower type than that in sub
    If CastOrder(CurrCall) < CastOrder(CurrSub) Then
      'OutScore += 1
      OutScore += 100 / (1 + CastOrder(CurrSub) - CastOrder(CurrCall))
      GoTo CheckNextParam
    End If

    'Parameter in call is of higher type than in sub, cannot convert
    If CastOrder(CurrCall) > CastOrder(CurrSub) Then Return 0

    CheckNextParam:

    SubSig = Mid(SubSig, InStr(SubSig, ":") + 1)
    CallSig = Mid(CallSig, InStr(CallSig, ":") + 1)
  Loop

  Return OutScore

End Function

Sub WaitForKeyOrTimeout

  If PauseTimeout = 0 Then Exit Sub

  Print Message("AnyKey")

  If PauseTimeout = -1 Then
    GetKey
  Else
    Sleep PauseTimeout * 1000
  End If
End Sub

FUNCTION WholeINSTR (DataIn As String, FindIn As String, SearchAgain As Integer = -1) As Integer
  Dim As String DataSource, Temp, Find
  Dim As Integer T

  DataSource = UCase(DataIn): Find = UCase(FindIn)

  IF INSTR(DataSource, Find) = 0 THEN WholeINSTR = 0: EXIT FUNCTION
  IF LEN(DataSource) = LEN(Find) THEN WholeINSTR = 2: EXIT FUNCTION

  DoWholeINSTRAgain:
  T = 0

  IF INSTR(DataSource, Find) = 1 THEN T = 1
  IF T = 0 THEN
    Temp = Mid(DataSource, INSTR(DataSource, Find) - 1, 1)
    IF IsDivider(Temp) THEN T = 1
  END IF

  IF INSTR(DataSource, Find) + LEN(Find) - 1 = LEN(DataSource) THEN T = T + 1
  IF T < 2 THEN
    Temp = Mid(DataSource, INSTR(DataSource, Find) + LEN(Find), 1)
    IF IsDivider(Temp) THEN T = T + 1
  END If

  IF T = 1 And SearchAgain THEN
    Replace DataSource, Find, Chr(30)
    IF INSTR(DataSource, Find) <> 0 THEN GOTO DoWholeINSTRAgain
  END IF

  WholeINSTR = T

END FUNCTION

Function WholeInstrLoc(DataSource As String, FindTemp As String) As Integer

  Dim As Integer P, StartOK

  For P = 1 To Len(DataSource)
    'Check start
    StartOK = 0
    If P = 1 Then
      StartOK = -1
    Else
      If IsDivider(Mid(DataSource, P - 1, 1)) Then StartOK = -1
    EndIf
    If StartOK Then
      'Check end
      If IsDivider(Mid(DataSource, P + Len(FindTemp), 1)) Or P + Len(FindTemp) = Len(DataSource) Then
        'Check middle
        If Mid(DataSource, P, Len(FindTemp)) = FindTemp Then
          Return P
        End If
      End If
    End If
  Next

  Return 0
End Function

SUB WholeReplace (DataVar As String, Find As String, Rep As String)

  Dim As String VarTemp, FindTemp, Temp, NewData
  Dim As Integer T, RepAll

  RepAll = -1
  If Left(Find, 3) = "[1]" Then
    RepAll = 0
    Find = Mid(Find, 4)
  End If

  VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
  IF INSTR(VARTemp, FINDTemp) = 0 THEN EXIT SUB
  IF VARTemp = FINDTemp THEN DataVar = Rep: EXIT SUB

  Do
    'Check for occurance of whole string
    'T: 0 not found, 1 left matches, 2 right matches (all found), 3 = found but not whole
    T = 0
    'Check start
    IF INSTR(UCase(DataVar), FINDTemp) = 1 THEN T = 1
    IF T = 0 THEN
      Temp = Mid(DataVar, INSTR(UCase(DataVar), FINDTemp) - 1, 1)
      IF IsDivider(Temp) THEN T = 1
    END IF
    'Check end
    IF INSTR(UCase(DataVar), FINDTemp) + LEN(FINDTemp) - 1 = LEN(DataVar) AND T = 1 THEN T = 2
    IF T = 1 THEN
      Temp = Mid(DataVar, INSTR(UCASE(DataVar), FINDTemp) + LEN(FINDTemp), 1)
      IF IsDivider(Temp) THEN T = 2
    END IF
    'If search string present but part of another word, T = 3
    IF T = 0 AND INSTR(UCase(DataVar), FINDTemp) <> 0 THEN T = 3

    'Search string not found at all, clean up and exit
    IF T = 0 Then Exit Do

    'If search string found but part of another word, convert to Chr(26)
    If T = 1 OR T = 3 THEN
      Replace DataVar, FINDTemp, Chr(26)

    'If whole string found, replace
    ElseIf T = 2 THEN
      NewData = Left(DataVar, INSTR(UCase(DataVar), FINDTemp) - 1)
      NewData = NewData + Rep
      DataVar = NewData + Mid(DataVar, INSTR(UCase(DataVar), FINDTemp) + LEN(Find))
      'If only replacing single occurance, exit
      If Not RepAll Then Exit Do
    END IF

  Loop

  'Tidy and exit
  Do While InStr(DataVar, Chr(26)) <> 0
    Replace DataVar, Chr(26), Find
  Loop

  Exit Sub

END SUB


Sub StringSplit(Text As String, Delim As String = " ", Count As Long = -1, Ret() As String)

   Dim As Long x, p
   If Count < 1 Then
      Do
         x = InStr(x + 1, Text, Delim)
         p += 1
      Loop Until x = 0
      Count = p - 1
   ElseIf Count = 1 Then
      ReDim Ret(Count - 1)
      Ret(0) = Text
   Else
      Count -= 1
   End If
   Dim RetVal(Count) As Long
   x = 0
   p = 0
   Do Until p = Count
      x = InStr(x + 1,Text,Delim)
      RetVal(p) = x
      p += 1
   Loop
   ReDim Ret(Count)
   Ret(0) = Left(Text, RetVal(0) - 1 )
   p = 1
   Do Until p = Count
      Ret(p) = Mid(Text, RetVal(p - 1) + 1, RetVal(p) - RetVal(p - 1) - 1 )
      p += 1
   Loop
   Ret(Count) = Mid(Text, RetVal(Count - 1) + 1)

End Sub
