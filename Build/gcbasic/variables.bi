' GCBASIC - A BASIC Compiler for microcontrollers
'  Variable handling routines
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

Function AddFinalVar(VarName As String, VarLoc As String, VarIsArray As Integer = 0) As Integer
  'Add new item to FinalVarList
  'Returns:
  ' 0 on success
  ' 1 if variable list not ready
  If FinalVarList = 0 Then Return 1

  Dim As VariableListElement Pointer NewVar
  NewVar = Callocate(SizeOf(VariableListElement))
  NewVar->Name = VarName
  NewVar->Value = VarLoc
  NewVar->IsArray = VarIsArray

  HashMapSet(FinalVarList, VarName, NewVar)
  Return 0
End Function

Sub AddVar(VarNameIn As String, VarTypeIn As String, VarSizeIn As Integer, VarSubIn As SubType Pointer, VarPointerIn As String, OriginIn As String, FixedLocation As Integer = -1, ExplicitDeclaration As Integer = 0, Used As Integer = -1)

  Dim As String VarName, VarType, VarPointer, Origin, Temp, VarAlias, ConstName
  Dim As String AliasList(16)
  Dim As Integer VarSize, CL, TempSize, PD, VarSearchStart, T, VarFixedSize
  Dim As Integer CurrFile, ALC, ParentByte, CheckSub, UndeclaredError
  Dim As VariableType Pointer VarFound, MainVarFound
  Dim As SubType Pointer VarSub, MainSub
  Dim As LinkedListElement Pointer SearchConstPos
  Dim As Single StartTime
  dim AS Integer AllocMem = 0

  VarName = UCase(VarNameIn)
  VarType = VarTypeIn
  VarSize = VarSizeIn
  VarPointer = VarPointerIn
  Origin = OriginIn
  VarSub = VarSubIn
  MainSub = Subroutine(0)
  UndeclaredError = 0

  'Do this to prevent null pointer access
  If VarSub = 0 Then
    'Print "Internal error in AddVar"
    VarSub = MainSub
  End If

  'Find file that variable was added in
  If InStr(Origin, "F") <> 0 Then
    CurrFile = Val(Mid(Origin, InStr(Origin, "F") + 1))
  Else
    CurrFile = VarSub->SourceFile
  End If

  'Do not add SysPointerX pseudo var
  If UCase(VarName) = "SYSPOINTERX" Then Exit Sub
  If UCase(VarName) = "SYSPOINTERXINC" Then Exit Sub

  'If VarName is a constant, exit
  If IsConst(VarName) Then Exit Sub
  If INSTR(VarName, ";STRING") <> 0 Then Exit Sub
  If INSTR(VarName, ".") <> 0 Then Exit Sub

  'Remove $, () from var name
  IF INSTR(VarName, "()") <> 0 Then
    VarName = Left(VarName, INSTR(VarName, "()") - 1)
    If VarType = "" THEN VarType = "STRING"
    If VarSize <= 1 Then VarSize = 20
  End If
  IF INSTR(VarName, "$") <> 0 Then
    VarName = Left(VarName, INSTR(VarName, "$") - 1)
    If VarType = "" THEN VarType = "STRING"
    If VarSize <= 1 Then VarSize = 20
  End If
  VarName = Trim(VarName)
  IF VarName = "" THEN EXIT SUB

  'Remove casts from var name
  If InStr(VarName, "[") <> 0 And InStr(VarName, "]") <> 0 Then
    Temp = Mid(VarName, InStr(VarName, "["))
    Temp = Left(Temp, InStr(Temp, "]"))
    Replace VarName, Temp, ""
    VarType = Mid(Temp, 2, Len(Temp) - 2)
  End If

  'Special treatment for strings  and alloc
  VarFixedSize = 0
  If Left(VarType, 6) = "STRING" or Left(VarType, 5) = "ALLOC"  Then

    'Change an ALLOC to a string and set to AllocMem to -1 - used later to upadate variable
    If Left(VarType, 5) = "ALLOC" Then
      AllocMem = -1
      Replace Vartype, "ALLOC", "STRING"
    End if
    VarSize = -1

    'Check for StringSize constant
    Dim As ConstMeta Pointer StringSizeConst
    StringSizeConst = HashMapGet(Constants, "STRINGSIZE")
    If StringSizeConst <> 0 And VarSize < 2 Then
      TempSize = MakeDec(StringSizeConst->Value)
      If TempSize > 0 Or TempSize < ChipRam Then VarSize = TempSize
    End If

    'Defaults
    If VarSize = -1 Then
      VarSize = GetTypeSize("STRING")
    End If

    IF INSTR(VarType, "*") <> 0 Then
      VarSize = VAL(Trim(Mid(VarType, INSTR(VarType, "*") + 1)))
      VarType = "STRING"
      If VarSize = 1 Then VarSize = 2 ' Needed to make some older code in compiler behave
      VarFixedSize = -1
    End If
  End If

  'Reduce the size of the VarSize -1 if this is an ALLOC
  If AllocMem = -1 Then VarSize = VarSize - 1

  'Defaults
  If VarSize = 0 Then VarSize = 1
  If VarPointer = "" Then VarPointer = "REAL"

  'Don't try to redefine system registers
  If ModePIC Or ModeAVR Then
    IF UCase(Left(VarName, 11)) = "SYSWAITTEMP" OR UCase(Left(VarName, 9)) = "DELAYTEMP" Then
      VarPointer = "REGISTER@-2"
    End If
    IF UCase(Left(VarName, 11)) = "SYSCALCTEMP" And InStr("ABXabx", Mid(VarName, 12, 1)) <> 0 Then VarPointer = "REGISTER@-2"
    IF UCase(Left(VarName, 11)) = "SYSBYTETEMP" And InStr("ABXabx", Mid(VarName, 12, 1)) <> 0 Then VarPointer = "REGISTER@-2"
    IF UCase(Left(VarName, 11)) = "SYSWORDTEMP" And InStr("ABXabx", Mid(VarName, 12, 1)) <> 0 Then VarPointer = "REGISTER@-2"
    IF UCase(Left(VarName, 14)) = "SYSINTEGERTEMP" And InStr("ABXabx", Mid(VarName, 15, 1)) <> 0 Then VarPointer = "REGISTER@-2"
    IF UCase(Left(VarName, 11)) = "SYSLONGTEMP" And InStr("ABXabx", Mid(VarName, 12, 1)) <> 0 Then VarPointer = "REGISTER@-2"
    If UCase(Left(VarName, 10)) = "SYSDIVMULT" Then VarPointer = "REGISTER@-2"
    If VarName = "SYSREADA" Or VarName = "SYSREADA_H" Then VarPointer = "REGISTER@-2"
    If UCase(Left(VarName, 9)) = "SYSSTRING" Then
      Temp = UCase(Mid(VarName, 10))
      If Temp = "LENGTH" Or Temp = "A" Or Temp = "A_H" Or Temp = "B" Or Temp = "B_H" Then
        VarPointer = "REGISTER@-2"
      End If
    End If
    IF UCase(VarName) = "SYSDIVLOOP" Then VarPointer = "REGISTER@-2"
    IF UCase(VarName) = "SYSSIGNBYTE" Then
      'Create this register in GPRs only when there is sufficient GPRs. 16 system vars in GPR is the maximum for a 16 GPR chip.
      'Create this register in GPRs only when there is sufficient GPRs. 16 system vars in GPR is the maximum for a 16 GPR chip.
      IF ( CHIPGPR > 16 AND ModeAVR ) or ModePIC then
        VarPointer = "REGISTER@-2"
      End If
    End If
    'Some are PIC Or AVR specific
    If ModePIC Then
      If UCase(VarName) = "SYSW" Then VarPointer = "REGISTER@-2"
      If UCase(VarName) = "SYSSTATUS" Then VarPointer = "REGISTER@-2"
    ElseIf ModeAVR Then
      IF UCase(VarName) = "SYSVALUECOPY" Then VarPointer = "REGISTER@-2"
      If UCase(VarName) = "SYSBITTEST" Then VarPointer = "REGISTER@-2"
      IF IsSysTemp(VarName) Then
        If VarPointer = "REAL" Then VarPointer = "REGISTER@-2"
      End If
    End If
  End If

  'Convert "float" to "single" (although neither is yet supported)
  If VarType = "FLOAT" Then VarType = "SINGLE"

  'Deal with aliases
  VarAlias = ""
  If Left(VarPointer, 6) = "ALIAS:" Then
    VarAlias = Trim(Mid(VarPointer, 7))
    VarPointer = "POINTER"
    'Print VarName, "Alias:", VarAlias
  End If

  'Tidy input
  VarType = Trim(UCase(VarType))

  'Check Name
  T = 0
  If Not IsValidName(VarName) Then T = 1
  'Names for status flags or destination locations, cannot reuse
  IF VarName = "B" THEN T = 1
  IF VarName = "C" THEN T = 1
  IF VarName = "F" THEN T = 1
  IF VarName = "H" Then T = 1
  IF ModeAVR And VarName = "I" Then T = 1
  IF VarName = "N" Then T = 1
  IF VarName = "S" Then T = 1
  IF VarName = "T" Then T = 1
  IF VarName = "V" Then T = 1
  IF VarName = "W" THEN T = 1
  IF VarName = "Z" THEN T = 1

  IF T = 1 THEN
    Temp = Message("BadVarName")
    Replace Temp, "%var%", VarName
    LogError Temp, Origin
  END IF
  'Some one-letter names should be allowed without warnings
  If VarName = "I" THEN T = -1
  IF VarName = "J" THEN T = -1
  IF VarName = "K" THEN T = -1
  IF VarName = "L" THEN T = -1
  IF VarName = "M" THEN T = -1

  If Len(VarName) = 1 And VarFound = 0 And T = 0 Then
    Temp = Message("WarningShortName")
    Replace Temp, "%type%", "variable"
    Replace Temp, "%name%", VarName
    LogWarning Temp, Origin
  END IF

  'Check Type
  T = -1
  Select Case VarType
    Case "BIT", "BYTE", "WORD", "INTEGER", "LONG" 
      T = 0
    Case "STRING"
      T = 0
    Case "LONGINT"  
      If ( (floatcapability and 4) = 4 ) OR CurrFile <> 1 Then
        T = 0
      End if
    Case "ULONGINT"   
      If ( (floatcapability and 8) = 8 )  OR CurrFile <> 1 Then 
        T = 0
      End If
    Case "SINGLE"
      if ( (floatcapability and 1) = 1 ) OR CurrFile <> 1 Then
        T = 0
      End If
    Case "DOUBLE"
      If ( (floatcapability and 2) = 2 )  OR CurrFile <> 1 Then 
        T = 0
      End If
    Case Else
      T = -1
  End Select

  ' Override variable check when SYStem variables
  If Left( VarName, 3 ) = "SYS" Then T = 0
  'print VarName, VarType, T, CurrFile
  
  IF T = -1 THEN
    Temp = Message("BadVarType")
    Replace Temp, "%type%", VarType
    LogError Temp, Origin
  END If

  'Check to see if the var is a SFR
  If GetSysVar(UCase(VarName)) <> 0 Then Exit Sub

  'Check to see if var exists
  VarFound = HashMapGet(@(VarSub->Variables), UCase(Trim(VarName)))
  If VarSub = MainSub Then
    MainVarFound = VarFound
  Else
    MainVarFound = HashMapGet(@(MainSub->Variables), UCase(Trim(VarName)))
  End If

  'If this variable or main variable are bit, add to main
  'Bit in main sub, so remove from this sub and exit
  If MainVarFound <> 0 Then
    If MainVarFound->Type = "BIT" And MainVarFound->ExplicitDeclaration Then
      'Bit found in main, remove from current sub
      If VarFound <> 0 And VarFound <> MainVarFound Then
        HashMapDelete(@(VarSub->Variables), UCase(Trim(VarName)))
      End If
      Exit Sub
    End If
  End If

  'If dealing with a higher byte of large variable, may not need to add
  If VarFound = 0 Then
    Temp = UCase(Right(VarName, 2))
    ParentByte = 0
    If Temp = "_H" Then ParentByte = 1
    If Temp = "_U" Then ParentByte = 2
    If Temp = "_E" Then ParentByte = 3

    If ParentByte > 0 Then
      'Check to see if var exists
      Temp = Left(UCase(Trim(VarName)), Len(Trim(VarName)) - 2)
      VarFound = HashMapGet(@(VarSub->Variables), Temp)
      If VarFound <> 0 Then
        If GetTypeSize(VarFound->Type) > ParentByte Then
          VarFound->Used = -1
          Exit Sub
        End If
      EndIf
      If VarSub <> MainSub Then
        VarFound = HashMapGet(@(MainSub->Variables), Temp)
        If VarFound <> 0 Then
          If GetTypeSize(VarFound->Type) > ParentByte Then
            VarFound->Used = -1
            Exit Sub
          End If
        End If
      End If

      VarFound = 0
    End If
  End If

  'If variable not found, make a new one
  If VarFound = 0 Then
    'Implicit declaration of new variable
    If (Not ExplicitDeclaration) And MainVarFound = 0 And Not IsSysTemp(VarName) And VarPointer = "REAL" And _
      Left(UCase(VarName), 9) <> "SYSBITVAR" And Left(UCase(VarName), 15) <> "SYSINTSTATESAVE" Then
      If SourceFile(CurrFile).OptionExplicit Then
        'If explicit declaration required, request error message if it remains undeclared
        UndeclaredError = -1
      End If
    End If

    'Is variable name used for an SFR bit (this will cause confusion)
    If HasSFRBit(VarName) Then
      Temp = Message("WarningNameUsedSystem")
      Replace Temp, "%name%", VarName
      LogWarning Temp, Origin
    End If

    'Is variable name used for a subroutine/function?
    For CheckSub = 0 To SBC
      If Subroutine(CheckSub)->IsFunction Then
        'Used for function, this is allowed
        If UCase(Subroutine(CheckSub)->Name) = VarName Then
          Exit For
        End If
      Else
        'Used for subroutine, not allowed
        If UCase(Subroutine(CheckSub)->Name) = VarName Then
          Temp = Message("SubAndVarNameConflict")
          Replace Temp, "%var%", VarName
          LogError Temp, Origin
          Exit For
        End If
      End If
    Next

    With *VarSub
      VarFound = Callocate(SizeOf(VariableType))
      VarFound->Name = UCase(VarName)
      VarFound->Type = UCase(VarType)
      VarFound->Size = VarSize
      VarFound->FixedLocation = -1
      VarFound->FixedSize = VarFixedSize
      VarFound->ExplicitDeclaration = ExplicitDeclaration
      VarFound->Alias = ""
      VarFound->Used = 0
      VarFound->UndeclaredError = UndeclaredError
      VarFound->NeedsSequentialLoc = 0
      VarFound->AllocOnly = AllocMem        'set to 0 to -1 if ALLOC

      HashMapSet(@(VarSub->Variables), UCase(VarName), VarFound)
    End With
  End If

  'Choose Type
  'Explicit declarations override implicit ones - so manually added bits override automatically added bytes
  If CastOrder(VarFound->Type) > CastOrder(VarType) Or (VarFound->ExplicitDeclaration And Not ExplicitDeclaration) Then VarType = VarFound->Type

  'If found var was explicitly declared, clear undeclared error
  If ExplicitDeclaration Then
    VarFound->UndeclaredError = 0
  End If

  'Choose Size
  If (VarSize = 1 And VarFound->Size <> 1) Or (VarSize <> 1 And VarFound->Size = 1) Then
    If VarType <> "STRING" And VarFound->Type <> "STRING" Then
      Temp = Message("DupDef")
      Replace Temp, "%var%", VarName
      'Print VarName, VarSize, VarFound->Size, VarType, VarFound->Type
      LogError Temp, Origin
    End If
  End If
  'Upgrade size?
  'If new size is fixed, but current variable size isn't, force change
  If Not VarFound->FixedSize And VarFixedSize Then
    'Size will be changed below, but mark as fixed now
    VarFound->FixedSize = -1
  Else
    'New size is not fixed, old size might be
    If (VarFound->FixedSize And Not VarFixedSize) Or (VarFound->Size > VarSize) Then
      VarSize = VarFound->Size
    End If
  End If

  'Choose alias
  If (VarAlias = "" And VarFound->Alias <> "") Or (CountOccur(VarAlias, ",") < CountOccur(VarFound->Alias, ",")) Then
    'Print "Changing " + VarName + " from " + VarAlias + " to " + VarSub->Variable(VarFound).Alias
    VarAlias = VarFound->Alias
  End If
  If VarAlias <> "" Then
    VarPointer = "POINTER"
    'Ensure alias variable is also declared
    GetTokens (VarAlias, AliasList(), ALC, ",")
    For PD = 1 To ALC
      If Not IsConst(AliasList(PD)) And Not IsCalc(AliasList(PD)) Then
        If InStr(AliasList(PD), "(") = 0 Then
          AddVar AliasList(PD), "BYTE", 1, VarSub, "REAL", Origin, -1, -1
        Else
          AddVar Left(AliasList(PD), InStr(AliasList(PD), "(") - 1), "BYTE", 2, VarSub, "REAL", Origin, -1, -1
        End If
      End If
    Next
  End If

  'Choose Origin
  If VarFound->Origin <> "" Then Origin = VarFound->Origin

  'Choose location
  If VarFound->FixedLocation <> -1 Then FixedLocation = VarFound->FixedLocation

  'Explicitely defined?
  If VarFound->ExplicitDeclaration Then ExplicitDeclaration = VarFound->ExplicitDeclaration

  'Apply new settings
  'Print VarName, VarType, VarSubOut, VarAlias
  With *VarFound
    .Name = VarName
    .Type = VarType
    .Size = VarSize
    .Pointer = VarPointer
    .Alias = VarAlias
    .Origin = Origin
    .FixedLocation = FixedLocation
    .ExplicitDeclaration = ExplicitDeclaration

    If .Type = "BIT" And .BitVarLocation = "" Then
      .BitVarLocation = "SYSBITVAR" + Str(INT(BVC / 8)) + "." + Str(BVC MOD 8)
      BVC = BVC + 1
    End If

    'Mark as used
    If Used = -1 Then
      .Used = -1
    End If

  End With

  'If variable is bit, need to move to main
  If VarFound->Type = "BIT" And VarFound->ExplicitDeclaration Then
    HashMapDelete(@(VarSub->Variables), UCase(VarName), 0)
    HashMapSet(@(MainSub->Variables), UCase(VarName), VarFound)
  End If

END Sub

SUB AllocateRAM

  Dim As String TempData, Origin, Temp
  Dim As String VarName, VarType
  Dim As Integer SV, ListSorted, PD, SkipVar, CD, ArraySize, SR, VLC
  Dim As Integer HighReg, AD, VarSize, ED, FreeStart, FreeSize, FirstLoc
  Dim As String OccursInSub(50)
  Dim As Integer OccursInSubs, CurrSub, SearchVarList
  Dim As Integer CurrByte, SRStart, SREnd, SRDir, UnallocatedVars, AllocAttempts
  Dim As Integer UseLinear, CurrBlockSize
  Dim As Integer RegisterUsed(32)

  Dim As LinkedListElement Pointer CurrLine

  Dim As HashMap Variables
  Dim As LinkedListElement Pointer SubVars, SubVarLoc
  Dim As VariableType Pointer SubVar, FinalVar, SearchVar

  Dim As LinkedListElement Pointer AllVarsUnsorted, AllVars, CurrVarItem, InsertPos

  'Add calc vars to list
  For SV = 1 to TCVC
    With CalcVars(SV)
      If .MaxType <> "" Then
        VarName = "SysTemp" + Str(SV)
        VarType = .MaxType
        If ModePIC Then AddVar VarName, VarType, 1, Subroutine(0), "REAL", ""
        If ModeAVR Then
          'High reg?
          If .High Then
            AddVar VarName, VarType, 1, Subroutine(0), "REGISTERH", ""
          Else
            AddVar VarName, VarType, 1, Subroutine(0), "REGISTER", ""
          End If
        End If
      End If
    End With
  Next

  'Copy all variables from all used subs into a single list
  For CurrSub = 0 To SBC
    'If sub is required, get its variables
    With *Subroutine(CurrSub)
      If .Required Then
        SubVars = HashMapToList(@(Subroutine(CurrSub)->Variables))
        SubVarLoc = SubVars->Next

        Do While SubVarLoc <> 0
          'Check to see if variable with same name already added
          SubVar = SubVarLoc->MetaData
          FinalVar = HashMapGet(@Variables, UCase(Trim(SubVar->Name)))

          'Var not found, create a new one
          If FinalVar = 0 Then
            HashMapSet(@Variables, UCase(Trim(SubVar->Name)), SubVar)
            'Mark location as unknown - must be assigned
            SubVar->Location = -1

          'Apply new settings to existing variable
          Else
            'Type
            'Explicit declarations override implicit ones
            If (CastOrder(FinalVar->Type) < CastOrder(SubVar->Type) And FinalVar->ExplicitDeclaration = SubVar->ExplicitDeclaration) Or _
                (SubVar->ExplicitDeclaration And Not FinalVar->ExplicitDeclaration) Then
              FinalVar->Type = SubVar->Type
            End If
            'Size
            'Do not copy if smaller size is fixed
            If Not FinalVar->FixedSize And SubVar->FixedSize Then
              'Existing found is not fixed, new one is, use new size
              FinalVar->Size = SubVar->Size
            ElseIf FinalVar->FixedSize And Not SubVar->FixedSize Then
              'Existing found is fixed, new one is not fixed, use existing

            Else
              'Both fixed size, use larger
              'Or neither fixed, use larger
              If FinalVar->Size < SubVar->Size Then
                FinalVar->Size = SubVar->Size
              End If
            End If

            'Pointer
            If FinalVar->Pointer = "" And SubVar->Pointer <> "" Then
              FinalVar->Pointer = SubVar->Pointer
            End If
            'Alias
            If (CountOccur(FinalVar->Alias, ",") < CountOccur(SubVar->Alias, ",")) Or _
              (FinalVar->Alias = "" And SubVar->Alias <> "") Then
              FinalVar->Alias = SubVar->Alias
            End If
            'Fixed Location
            If FinalVar->FixedLocation = -1 And SubVar->FixedLocation <> -1 Then
              FinalVar->FixedLocation = SubVar->FixedLocation
            End If
            'Explicit declaration
            If SubVar->ExplicitDeclaration Then
              FinalVar->ExplicitDeclaration = SubVar->ExplicitDeclaration
            End If
            'Used
            If SubVar->Used Then
              FinalVar->Used = SubVar->Used
            End If
            'Origin
            If FinalVar->Origin = "" And SubVar->Origin <> "" Then
              FinalVar->Origin = SubVar->Origin
            End If
            'Requires sequential locations?
            If SubVar->NeedsSequentialLoc Then
              FinalVar->NeedsSequentialLoc = SubVar->NeedsSequentialLoc
            End If
          End If

          SubVarLoc = SubVarLoc->Next
        Loop
      End If
    End With
  Next

  'Mark all registers as available (AVR)
  If ModeAVR Then
    For PD = 0 To 31
      RegisterUsed(PD) = 0
      ' If number of GPR =16 then we need to mark the Register as Used to prevent usage.
      if ChipGPR = 16 and PD < 16 then RegisterUsed(PD) = -1
    Next
  End If

  'Put all variables into a list
  AllVarsUnsorted = HashMapToList(@Variables, -1)

  'Remove unused variables from list
  CurrVarItem = AllVarsUnsorted->Next
  Do While CurrVarItem <> 0
    FinalVar = CurrVarItem->MetaData
	'Only delete is NOT used and NOT an ALLOC variable
    If Not FinalVar->Used  and FinalVar->AllocOnly = 0 Then
      CurrVarItem = LinkedListDelete(CurrVarItem, 0)
    ElseIf Left(Right(CurrVarItem->Value, 2), 1) = "_" Then
      'Is this a byte of a larger variable? Prevent add if it is
      CurrByte = GetVarByteNumber(CurrVarItem->Value)
      If CurrByte > 0 Then
        VarName = Left(CurrVarItem->Value, Len(CurrVarItem->Value) - 2)
        SearchVar = HashMapGet(@Variables, VarName)
        If SearchVar <> 0 Then
          If GetTypeSize(SearchVar->Type) > CurrByte Then
            CurrVarItem = LinkedListDelete(CurrVarItem, 0)
          End If
        End If
      End If
    End If
    CurrVarItem = CurrVarItem->Next
  Loop

  'Sort list into correct order
  ' - Fixed location variables must be before movable ones
  ' - Larger variables should be before smaller ones
  ' - Variables should be in alphabetical order
  AllVars = LinkedListCreate
  CurrVarItem = AllVarsUnsorted->Next
  Do While CurrVarItem <> 0
    'Find where to put the item
    FinalVar = CurrVarItem->MetaData

    InsertPos = AllVars
    Do While InsertPos->Next <> 0
      InsertPos = InsertPos->Next
      SearchVar = InsertPos->MetaData

      If FinalVar->FixedLocation <> -1 And SearchVar->FixedLocation = -1 Then InsertPos = InsertPos->Prev: Exit Do
      If (FinalVar->FixedLocation = -1) = (SearchVar->FixedLocation = -1) Then
        If FinalVar->Size > SearchVar->Size Then InsertPos = InsertPos->Prev: Exit Do
        If FinalVar->Size = SearchVar->Size Then
          If FinalVar->Name < SearchVar->Name Then InsertPos = InsertPos->Prev: Exit Do
        End If
      End If
    Loop
    LinkedListInsert(InsertPos, FinalVar)

    CurrVarItem = CurrVarItem->Next
  Loop

  'Create final variable list
  FinalVarList = HashMapCreate

  'Allocate common (non-banked) RAM or register space to system variables
  Dim As Integer DesiredLoc, RegBytesLocated, FinalRegLoc
  RegCount = 0
  CurrVarItem = AllVars->Next
  Do While CurrVarItem <> 0
    FinalVar = CurrVarItem->MetaData

    With *FinalVar
      IF Left(.Pointer, 8) = "REGISTER" And .Location = -1 Then
        If ModePIC Then
          'We have a register
          'Allocate the individual bytes
          VarSize = GetTypeSize(.Type) - 1
          RegBytesLocated = 0
          For CurrByte = 0 To VarSize
            'Need to allocate it something from the shared bank
            DesiredLoc = GetRegisterLoc(GetByte(.Name, CurrByte))
            'DesiredLoc will be -1 if no location found for variable
            'Or the location otherwise
            If DesiredLoc <> -1 Then
              FinalRegLoc = NoBankLoc(1).StartLoc + DesiredLoc
              AddFinalVar(GetByte(.Name, CurrByte), Str(FinalRegLoc))
              If .Location = -1 Then
                .Location = FinalRegLoc
              End If
              RegBytesLocated += 1
              RegBytesUsed  += 1
              'Then mark that shared bank location as off limits to other variables
              For SR = 1 To FreeRAM
                If VarLoc(SR) = FinalRegLoc Then
                  FreeRAM = FreeRAM - 1
                  For CD = SR To FreeRAM
                    VarLoc(CD) = VarLoc(CD + 1)
                  Next
                  Exit For
                End If
              Next

            End If
          Next

          'If all bytes located, mark variable as in shared bank
          If RegBytesLocated = (VarSize + 1) Then
            .IsSharedBank = -1
          Else
            'If out of room, put it in non-shared bank
            'Might result in problems, but at least it will work sometimes
            .IsSharedBank = 0
            .Location = -1
            .Pointer = "REAL" 'Mark as not a register
            LogWarning Message("WarningLowCommonRam")
          End If

        ElseIf ModeAVR Then
          'Is this a system register?
          AD = -1
          IF INSTR(.Pointer, "@") <> 0 Then AD = VAL(Mid(.Pointer, INSTR(.Pointer, "@") + 1))
          If AD = -2 Then
            'If address = -2, it's a system register
            'Look up proper location
            AD = GetRegisterLoc(.Name)

            'A location should now have been requested, give it to the variable
            If AD >= 0 Then
              'Allocate the individual bytes
              VarSize = GetTypeSize(.Type) - 1
              'Mark registers as used
              For SR = AD To AD + VarSize
                RegisterUsed(SR) = -1
              Next
              'Assign locations
              For CurrByte = 0 To VarSize
                RegCount += 1
                RegList(RegCount, 1) = GetByte(.Name, CurrByte)
                RegList(RegCount, 2) = Str(AD + CurrByte)
              Next
              .Location = AD
            Else
              'Error, system variable doesn't have a location

            End If

          End If

        End If
      End If
    End With

    CurrVarItem = CurrVarItem->Next
  Loop

  'Allocate RAM to single element vars and arrays
  Dim AliasList(10) As String
  ALC = 0

  'May need to iterate through list several times, alias may not be set on the first run
  '(They may refer to variables yet to be allocated)
  AllocAttempts = 0
  UnallocatedVars = -1
  Do While UnallocatedVars And AllocAttempts < 20
    UnallocatedVars = 0
    AllocAttempts += 1

    CurrVarItem = AllVars->Next
    Do While CurrVarItem <> 0
      FinalVar = CurrVarItem->MetaData
      With *FinalVar

        'Don't try allocating RAM to high byte of alias
        If InStr(.Name, "_") <> 0 Then
          'Get name of full variable
          TempData = UCase(Left(.Name, InStrRev(.Name, "_") - 1))
          'Does it match an alias?
          SubVar = HashMapGet(@Variables, TempData)
          If SubVar <> 0 Then
            If SubVar->Alias <> "" Then
              'Name of current var matches name of alias, don't allocate
              .Location = 0
            End If
          End If
        End If

        'Don't allocate RAM twice
        If .Location = -1 Then

          'Decide how much RAM to allocate
          'Note: Some types here may not be implemented properly (yet)
          'Allocate RAM to register variables
          IF Left(.Pointer, 8) = "REGISTER" THEN
            'Does the variable need to go into the high register space?
            HighReg = 0
            If Right(.Pointer, 1) = "H" Then HighReg = -1

            'Has a particular location been requested for the register?
            AD = -1
            IF INSTR(.Pointer, "@") <> 0 Then AD = VAL(Mid(.Pointer, INSTR(.Pointer, "@") + 1))
            'Location has not been requested, so assign one
            IF AD = -1 Then
              'Get var size
              VarSize = GetTypeSize(.Type)

              'Allocate low registers
              If Not HighReg Then
                'Example: LRS is 14, need to allocate 2 bytes, r14 and r15. Can do
                'If LRS is 15, need to allocate 2 bytes, r15 and r16, can't do
                'Search RAM for free locations
                FreeStart = -1
                FreeSize = 0
                For SR = 0 To 15
                  'Check for a continuous block of available RAM
                  IF RegisterUsed(SR) Then
                    FreeSize = 0
                    FreeStart = -1
                  Else
                    FreeSize += 1
                    If FreeStart = -1 Then FreeStart = SR
                  End If

                  If FreeSize = VarSize Then Exit For
                Next

                If FreeSize = VarSize Then
                  AD = FreeStart
                  For SR = FreeStart To FreeStart + FreeSize - 1
                    RegisterUsed(SR) = -1
                  Next
                Else
                  HighReg = -1
                End If
              End If

              'Allocate high registers/excess low registers
              If HighReg Then
                'If HRS = 17, need 2 bytes, r17 and r16, can do.
                'If HRS = 16 and need 2 bytes, r16 and r15, can't.
                'Need to allocate high registers backwards
                'Search RAM for free locations
                FreeStart = -1
                FreeSize = 0
                For SR = 16 To 31
                  'Check for a continuous block of available RAM
                  IF RegisterUsed(SR) Then
                    FreeSize = 0
                    FreeStart = -1
                  Else
                    FreeSize += 1
                    If FreeStart = -1 Then FreeStart = SR
                  End If

                  If FreeSize = VarSize Then Exit For
                Next

                If FreeSize = VarSize Then
                  AD = FreeStart
                  For SR = FreeStart To FreeStart + FreeSize - 1
                    RegisterUsed(SR) = -1
                  Next
                Else
                  If ChipFamily = 121 then
                    LogError Message("OutOfRegSpaceAVRrc")
                  Else
                    LogError Message("OutOfRegSpace")
                  End if
                End If
              End If
            End If

            'A location should now have been requested, give it to the variable
            If AD >= 0 Then
              'Allocate the individual bytes
              VarSize = GetTypeSize(.Type) - 1
              For CurrByte = 0 To VarSize
                RegCount += 1
                RegList(RegCount, 1) = GetByte(.Name, CurrByte)
                RegList(RegCount, 2) = Str(AD + CurrByte)
              Next
              .Location = AD
            End If

          'Variable is an alias
          ElseIf .Alias <> "" Then
            'Unused aliases don't get RAM
            SkipVar = -1
            For CD = 0 To SBC
              If Subroutine(CD)->Required Then
                CurrLine = Subroutine(CD)->CodeStart->Next
                Do While CurrLine <> 0
                  If WholeINSTR(CurrLine->Value, .Name) = 2 Then SkipVar = 0: Exit For
                  CurrLine = CurrLine->Next
                Loop
              End If
            Next
            'Print .Name, SkipVar
            If Not SkipVar Then

              'Get SFR(s) being referred to
              TempData = .Alias
              ALC = 0
              Do While INSTR(TempData, ",") <> 0
                ALC += 1: AliasList(ALC) = Trim(Left(TempData, INSTR(TempData, ",") - 1))
                TempData = Mid(TempData, INSTR(TempData, ",") + 1)
              Loop
              ALC += 1: AliasList(ALC) = Trim(TempData)

              'Create alias
              VarSize = GetTypeSize(.Type)
              If ALC < VarSize Then
                'If the alias is too small for the variable type, show error
                Temp = Message("BadAliasSize")
                Replace Temp, "(%size%)", "["+.Type+" - "+Str(VarSize)+" byte(s)]"+.Name
                Replace Temp, "%locations%", Str(ALC) +" byte(s)"
                Replace Temp, "%target%", .Alias
                LogError Temp, Origin
                
              Else
                'Define alias
                .Location = -1
                For CurrByte = 0 To VarSize - 1
                  'Calculate location for alias
                  .Location = CalcAliasLoc(AliasList(ALC - CurrByte))

                  'Check for alias to register
                  If ModeAVR And .Location = -1 And IsRegister(AliasList(ALC - CurrByte)) Then
                    'Print "Found alias to register " + AliasList(ALC - CurrByte)
                    .Location = GetRegisterLoc(AliasList(ALC - CurrByte))
                    FinalAliasList(FALC + CurrByte + 1).Name = GetByte(.Name, CurrByte)
                    FinalAliasList(FALC + CurrByte + 1).Value = AliasList(ALC - CurrByte)

                  'Allocate
                  ElseIf .Location <> -1 Then
                    MakeSFR GetByte(.Name, CurrByte), .Location
                    FinalAliasList(FALC + CurrByte + 1).Name = GetByte(.Name, CurrByte)
                    FinalAliasList(FALC + CurrByte + 1).Value = Str(.Location)
                  Else
                    'Could not find location, try again
                    UnallocatedVars = -1
                    GoTo NextVarAdd
                  End If

                  'Ensure sequential bytes if needed
                  If .NeedsSequentialLoc Then
                    If CurrByte = 0 Then
                      FirstLoc = .Location
                    Else
                      If .Location <> FirstLoc + CurrByte Then
                        'Error, byte n is not at first location + n
                        Temp = Message("AliasLocNotSequential")
                        Replace Temp, "%var%", .Name
                        LogError Temp, .Origin
                      End If
                    End If
                  End If
                Next
                If .Location <> -1 Then FALC += VarSize
              End If
            End If

          'Pointers get no RAM at present
          ElseIf .Pointer = "POINTER" Then

          'Bit variables don't get allocated, their parent should be
          ElseIf .Type = "BIT" Then

          Else
            'Variable isn't alias, isn't register, so must be normal RAM variable or array
            'Show error if adding variable that wasn't explicitly declared but should have been
            If .UndeclaredError And Not .ExplicitDeclaration Then
              Temp = Message("UndeclaredVar")
              Replace Temp, "%var%", .Name
              LogError Temp, .Origin
            End If

            'Array or normal variable?
            If .Size > 1 Then
              'Array
              'Calculate array size
              If .Type = "STRING" Then
                VarSize = .Size + 1
              Else
                VarSize = (.Size + 1) * GetTypeSize(.Type)
              End If

              'Decide which direction to search RAM for free location
              'PICs need to have arrays put at the end of RAM to reduce banking
              'AVRs need arrays at start, so as to leave end of RAM free for stack
              If ModePIC Then
                SRStart = FreeRAM - (VarSize - 1)
                SREnd = 1
                SRDir = -1
                If ChipFamily = 15 Then
                  UseLinear = -1
                End If
              Else
                SRStart = 1
                SREnd = FreeRAM - (VarSize - 1)
                SRDir = 1
              End If
            Else
              'Scalar
              'Get variable size
              VarSize = GetTypeSize(.Type)
              'Set location search direction and start point
              SRStart = 1
              SREnd = FreeRAM - (VarSize - 1)
              SRDir = 1
              UseLinear = 0
            End If

            'Search RAM for suitable contiguous free locations
            For SR = SRStart To SREnd Step SRDir
              'Check for a continuous block of available RAM
              If UseLinear Then
                CurrBlockSize = GetLinearLoc(VarLoc(SR + VarSize - 1)) - GetLinearLoc(VarLoc(SR))
              Else
                CurrBlockSize = VarLoc(SR + VarSize - 1) - VarLoc(SR)
              End If
              IF CurrBlockSize = VarSize - 1 And (.FixedLocation = -1 Or GetNonLinearLoc(.FixedLocation) = VarLoc(SR)) Then
                'Allocate RAM to variable
                If UseLinear Then
                  .Location = GetLinearLoc(VarLoc(SR))
                Else
                  .Location = GetNonLinearLoc(VarLoc(SR))
                End If
                If .Size > 1 Or LCase(.Type) = "string" Then
                  'For an array, need to name first location only
                  AddFinalVar(.Name, Str(.Location), -1)
                Else
                  'For a scalar, need to name every byte
                  For CurrByte = 0 To VarSize - 1
                    AddFinalVar(GetByte(.Name, CurrByte), Str(VarLoc(SR + CurrByte)))
                  Next
                End If

                'Remove the now occupied RAM from the free list
                FreeRAM = FreeRAM - VarSize
                For CD = SR To FreeRAM
                  VarLoc(CD) = VarLoc(CD + VarSize)
                Next
                StatsUsedRam += VarSize
                Exit For
              End If
            Next

            'Error for large variable that cannot be allocated RAM
            If .Location = -1 Then
              If .FixedLocation <> -1 Then
                'Fixed location was not free, allocate anyway but show warning
                .Location = .FixedLocation
                If .Size > 1 Then
                  'For an array, need to name first location only
                  AddFinalVar(.Name, Str(.FixedLocation), -1)
                Else
                  'For a scalar, need to name every byte
                  For CurrByte = 0 To VarSize - 1
                    AddFinalVar(GetByte(.Name, CurrByte), Str(.FixedLocation + CurrByte))
                  Next
                End If
                TempData = Message("WarningFixedLocBad")
                Replace TempData, "%var%", .Name
                LogWarning TempData, Origin

              ElseIf .Size > 1 Then
                'Error for oversized array
                TempData = Message("ArrayTooBig")
                Replace TempData, "%array%", .Name
                LogError TempData, Origin
              Else
                'Error for too many variables
                LogError Message("ExcessVars")
              End If
            End If

          End If

        End If
      End With


      NextVarAdd:
      CurrVarItem = CurrVarItem->Next
    Loop
  Loop

  'Display error here if UnallocatedVars = -1

  'Create FinalRegList on AVR
  If ModeAVR Then
    FRLC = 0
    If RegCount > 0 Then
      For PD = 1 to RegCount
        FRLC += 1
        With FinalRegList(FRLC)
           .Name = RegList(PD, 1)
           .Value = "r" + RegList(PD, 2)
        End With
      Next
    End If
  End If

END SUB

Function CalcAliasLoc(LocationIn As String) As Integer
  'Calculate memory location required for an alias
  'Returns -1 if an error occurs and the location cannot be found

  'Can handle:
  ' - SFR names
  ' - Normal variable names
  ' - Variable names with brackets immediately after, which are assumed to be arrays
  ' - literals
  ' - calculations containing a mix of those

  Dim As String LineToken(100), OutTemp
  Dim As Integer FC, LineTokens, CurrToken
  Dim As SysVarType Pointer TempVar
  Dim As VariableListElement Pointer FinalVar

  'Split line into tokens
  GetTokens LocationIn, LineToken(), LineTokens, , -1

  'Process each token, looking for SFR or variable name
  OutTemp = ""
  For CurrToken = 1 To LineTokens
    If Not IsConst(LineToken(CurrToken)) Then
      'Check if token is a divider
      If IsDivider(LineToken(CurrToken)) Then GoTo AliasConstReplaced

      LineToken(CurrToken) = UCase(LineToken(CurrToken))

      'Check if LocationIn is SFR
      TempVar = GetSysVar(LineToken(CurrToken))
      If TempVar <> 0 Then
        LineToken(CurrToken) = Str(TempVar->Location)
        GoTo AliasConstReplaced
      End If

      'Check if LocationIn is a variable
      FinalVar = HashMapGet(FinalVarList, LineToken(CurrToken))
      If FinalVar <> 0 Then
        LineToken(CurrToken) = FinalVar->Value
        'Array? If so, and if it's followed by brackets, add whatever is in the brackets
        If LineToken(CurrToken + 1) = "(" Then
          LineToken(CurrToken) += "+"
        End If
        GoTo AliasConstReplaced
      End If

      'If we reach this point, there is a variable name still lurking
      Return -1
    End If

    AliasConstReplaced:
    'Should have only numbers and dividers at this point

    OutTemp += LineToken(CurrToken)
  Next

  'Calculate location, return
  Calculate OutTemp
  Return GetNonLinearLoc(Val(OutTemp))
End Function

Function GetWholeSFR(BitName As String) As String
  Dim As SysVarType Pointer FoundVar
  Dim As VariableType Pointer FoundUserVar

  'Find SFR var bit
  FoundVar = HashMapGet(SysVarBits, UCase(BitName))
  If FoundVar <> 0 Then
    Return UCASE(FoundVar->Parent + "." + BitName)
  End If

  'Also deal with bit variables (which are all stored in main)
  FoundUserVar = HashMapGet(@(Subroutine(0)->Variables), BitName)
  If FoundUserVar <> 0 Then
    Return UCASE(FoundUserVar->BitVarLocation)
  End If

  Return UCASE(BitName)
End Function


Function GetSFRBitValue(BitName As String) As String
  Dim As SysVarType Pointer FoundVar
  Dim As VariableType Pointer FoundUserVar

  'Find SFR var bit
  FoundVar = HashMapGet(SysVarBits, UCase(BitName))
  If FoundVar <> 0 Then
    Return UCASE(str(FoundVar->Location))
  End If

  'Also deal with bit variables (which are all stored in main)
  FoundUserVar = HashMapGet(@(Subroutine(0)->Variables), BitName)
  If FoundUserVar <> 0 Then
    Return UCASE(FoundUserVar->BitVarLocation)
  End If

  Return UCASE(BitName)
End Function


Function GetReversePICASIncFileLookupValue( address As integer ) As String
  Dim as integer searchnow

  'can be improved - slow as this searches all the array until it finds something
  for searchnow = 0 to ubound(ReverseIncFileLookup)
    if ReverseIncFileLookup( searchnow ).NumVal =  address then
        Return ReverseIncFileLookup( searchnow ).Value
    end if
  next

  'this function will NOT find aliases
  Return ""

End Function


Function HasSFR(SFRName As String) As Integer
  Dim As String TidiedName

  'Search system variable list to find register
  TidiedName = UCase(Trim(SFRName))
  If GetSysVar(TidiedName) <> 0 Then Return -1

  Return 0
End Function

Function HasSFRBit(BitName As String) As Integer

  Return HashMapGet(SysVarBits, Trim(UCase(BitName))) <> 0

End Function

Sub MakeSFR (UserVar As String, SFRAddress As Integer, AVRAlias As Integer = 0 )
  'Make UserVar an SFR, assign location

  'Create new sysvar
  Dim As SysVarType Pointer NewVar
  NewVar = Callocate(SizeOf(SysVarType))

  NewVar->Name = UserVar
  NewVar->Location = SFRAddress
  NewVar->AVRAlias = AVRAlias

  HashMapSet(SysVars, UserVar, NewVar)

End Sub

Sub RequestVariable(VarName As String, CurrSub As SubType Pointer)
  'Find requested sub, either in current sub or main, and mark as used
  'Will get names from inline assembly, so also need to request vars if high byte used
  '(or if _H is appended to var name but no matching high byte exists)

  Dim As Integer ParentByte
  Dim As String Temp
  Dim As VariableType Pointer VarFound
  Dim As SubType Pointer MainSub

  MainSub = Subroutine(0)

  Temp = UCase(Right(VarName, 2))
  ParentByte = 0
  If Temp = "_H" Then ParentByte = 1
  If Temp = "_U" Then ParentByte = 2
  If Temp = "_E" Then ParentByte = 3

  'Check parent of this variable
  If ParentByte > 0 Then
    'Check to see if var exists
    Temp = Left(UCase(Trim(VarName)), Len(Trim(VarName)) - 2)
    VarFound = HashMapGet(@(CurrSub->Variables), Temp)
    If VarFound <> 0 Then
      If GetTypeSize(VarFound->Type) > ParentByte Then
        VarFound->Used = -1
        Exit Sub
      End If
    End If
    If CurrSub <> MainSub Then
      VarFound = HashMapGet(@(MainSub->Variables), Temp)
      If VarFound <> 0 Then
        If GetTypeSize(VarFound->Type) > ParentByte Then
          VarFound->Used = -1
          Exit Sub
        End If
      End If
    End If
  End If

  'Check for variable
  Temp = UCase(Trim(VarName))
  VarFound = HashMapGet(@(CurrSub->Variables), Temp)
  If VarFound <> 0 Then
    VarFound->Used = -1
    Exit Sub
  End If
  If CurrSub <> MainSub Then
    VarFound = HashMapGet(@(MainSub->Variables), Temp)
    If VarFound <> 0 Then
      VarFound->Used = -1
      Exit Sub
    End If
  End If

End Sub
