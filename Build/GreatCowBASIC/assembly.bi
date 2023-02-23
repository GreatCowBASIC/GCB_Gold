' GCBASIC - A BASIC Compiler for microcontrollers
'  Built-in assembler
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

Sub AddAsmSymbol(SymName As String, SymValue As String)
  Dim As String Temp

  'Add symbol to symbol table
  If Not HashMapSet(ASMSymbols, SymName, SymValue, 0) Then
    'Generate error if can't due to conflict
    If HashMapGetStr(ASMSymbols, SymName) <> SymValue Then
      Temp = Message("DupDef")
      Replace Temp, "%var%", UCase(SymName)
      LogError "GCASM: " + Temp
    End If
  End If

End Sub

Sub AsmOptimiser (CompSub As SubType Pointer)

  Dim As String Temp, BranchLine, NewBranch, OldBranch, ClearVar, OtherParam, VarName, RegName
  Dim As String EndIfLabel
  Dim As Integer LL, TRISSupport, LocValid, SL, ScanDist, SingleSkip
  Dim As LinkedListElement Pointer CurrLine, LastLine, CheckLine, DelEndIfLoc

  'Optimise asm code, and make final adjustments

  If ModePIC Then
    'Variable clearing ops
    CurrLine = CompSub->CodeStart->Next
    If CurrLine <> 0 Then
      Do While CurrLine->Next <> 0

        IF CurrLine->Value = " movlw 0" AND Left(CurrLine->Next->Value, 7) = " movwf " THEN
          ClearVar = Mid(CurrLine->Next->Value, 7)
          CurrLine->Value = " clrf" + ClearVar
          LinkedListDelete(CurrLine->Next)
        END If
        CurrLine = CurrLine->Next
      Loop
    End If

    'Delete all "bankisel" commands on PICs with <= 256 addresses
    If MemSize <= 256 Then
      CurrLine = CompSub->CodeStart->Next
      Do While CurrLine <> 0

        IF Left(CurrLine->Value, 10) = " bankisel " THEN
          CurrLine = LinkedListDelete(CurrLine)
        END If
        CurrLine = CurrLine->Next
      Loop
    End If

    'Don't copy something back into W if it's just been copied out
    CurrLine = CompSub->CodeStart->Next
    Do While CurrLine <> 0
      If Left(CurrLine->Value, 6) = " movf " And Right(CurrLine->Value, 1) = "W" Then
        Temp = Trim(Mid(CurrLine->Value, 6))
        Temp = LCase(Left(Temp, INSTR(Temp, ",") - 1))
        LastLine = CurrLine->Prev
        Do While (Left(LastLine->Value, 1) = ";" Or Left(LastLine->Value, 8) = "PRESERVE") AND LastLine->Prev <> 0: LastLine = LastLine->Prev: Loop
        If LCase(LastLine->Value) = " movwf " + Temp Then
          'Print "Deleting line reloading "; Temp
          CurrLine = LinkedListDelete(CurrLine)
        End If
      End If
      CurrLine = CurrLine->Next
    Loop

    'return > retlw 0(12 bit core)
    IF ChipFamily = 12 THEN
      CurrLine = CompSub->CodeStart->Next
      Do While CurrLine <> 0
        IF INSTR(LCase(CurrLine->Value), "return") <> 0 THEN CurrLine->Value = " retlw 0"
        CurrLine = CurrLine->Next
      Loop
    END IF

    'Replace 16F mnemonics with 18F ones
    IF ChipFamily = 16 Then
      CurrLine = CompSub->CodeStart->Next
      Do While CurrLine <> 0
        IF LCase(Trim(CurrLine->Value)) = "clrw" THEN CurrLine->Value = " clrf WREG"
        IF INSTR(LCase(CurrLine->Value), " rlf ") <> 0 THEN Replace CurrLine->Value, " rlf ", " rlcf "
        IF INSTR(LCase(CurrLine->Value), " rrf ") <> 0 THEN Replace CurrLine->Value, " rrf ", " rrcf "
        CurrLine = CurrLine->Next
      Loop
    END IF

  End If

  If ModeAVR Then
    'Conditional branches
    CurrLine = CompSub->CodeStart->Next
    Do While CurrLine <> 0

      If Left(CurrLine->Value, 6) = " rjmp " And Left(CurrLine->Prev->Value, 3) = " br" Then

        SingleSkip = 0
        DelEndIfLoc = 0
        If Right(CurrLine->Prev->Value, 6) = "PC + 2" Then
          SingleSkip = -1
        ElseIf InStr(CurrLine->Prev->Value, "ENDIF") <> 0 Then
          If CurrLine->Next <> 0 Then
            If Left(CurrLine->Next->Value, 5) = "ENDIF" Then
              EndIfLabel = Left(CurrLine->Next->Value, Len(CurrLine->Next->Value) - 1)
              If Right(CurrLine->Prev->Value, Len(EndIfLabel) + 1) = " " + EndIfLabel Then
                SingleSkip = -1
                DelEndIfLoc = CurrLine->Next
              End If
            End If
          End If
        End If

        If SingleSkip Then
          'Get old branch condition, rjmp destination
          OldBranch = Mid(CurrLine->Prev->Value, 2, 4)
          BranchLine = Trim(Mid(CurrLine->Value, 6))

          'Check that destination is with approx 63 words of origin
          LocValid = 0
          'Check last 63 instructions before current
          ScanDist = 63
          CheckLine = CurrLine->Prev
          Do While ScanDist > 0 And CheckLine <> 0
            IF CheckLine->Value = BranchLine + ":" Then
              LocValid = -1
              Exit Do
            End If
            ScanDist -= CalcLineSize(CheckLine->Value, 1)
            CheckLine = CheckLine->Prev
          Loop
          If LocValid = 0 Then
            'Check next 63 instructions after current
            ScanDist = 63
            CheckLine = CurrLine->Next
            Do While ScanDist > 0 And CheckLine <> 0
              IF CheckLine->Value = BranchLine + ":" Then
                LocValid = -1
                Exit Do
              End If
              ScanDist -= CalcLineSize(CheckLine->Value, 1)
              CheckLine = CheckLine->Next
            Loop
          End If

          If LocValid Then
            NewBranch = ""
            OtherParam = " "
            Select Case OldBranch
              Case "breq": NewBranch = "brne"
              Case "brne": NewBranch = "breq"
              Case "brlo": NewBranch = "brsh"
              Case "brsh": NewBranch = "brlo"
              Case "brlt": NewBranch = "brge"
              Case "brge": NewBranch = "brlt"
              Case "brbs"
                NewBranch = "brbc"
                OtherParam = " " + Mid(CurrLine->Prev->Value, 7, 1) + ","
              Case "brbc"
                NewBranch = "brbs"
                OtherParam = " " + Mid(CurrLine->Prev->Value, 7, 1) + ","
            End Select

            CurrLine = LinkedListDelete(CurrLine)
            CurrLine->Value = " " + NewBranch + OtherParam + BranchLine
            If DelEndIfLoc <> 0 Then
              LinkedListDelete(DelEndIfLoc)
            End If
          End If
        End If
      End If

      'Change ldi/mov to plain ldi where possible (AVR)
      Dim As String SourceVal, TempVar, DestVar
      If Left(CurrLine->Value, 5) = " mov " And Left(CurrLine->Prev->Value, 5) = " ldi " Then
        TempVar = Trim(Mid(CurrLine->Prev->Value, 5))
        TempVar = Left(TempVar, InStr(TempVar, ",") - 1)
        If Right(CurrLine->Value, Len(TempVar)) = TempVar Then
          SourceVal = CurrLine->Prev->Value
          SourceVal = Trim(Mid(SourceVal, InStr(SourceVal, ",") + 1))
          DestVar = Mid(CurrLine->Value, 5)
          DestVar = Trim(Left(DestVar, InStr(DestVar, ",") - 1))

          If Not IsLowRegister(DestVar) Then
            CurrLine = LinkedListDelete(CurrLine)
            CurrLine->Value = " ldi " + DestVar + "," + SourceVal
          End If
        End If
      End If

      CurrLine = CurrLine->Next
    Loop

    'Don't copy something back into register if it's just been copied out
    CurrLine = CompSub->CodeStart->Next
    Do While CurrLine <> 0
      If Left(CurrLine->Value, 5) = " lds " Then
        Temp = Mid(CurrLine->Value, 5)
        RegName = Trim(LCase(Left(Temp, INSTR(Temp, ",") - 1)))
        VarName = Trim(LCase(Mid(Temp, InStr(Temp, ",") + 1)))
        LastLine = CurrLine->Prev
        Do While (Left(LastLine->Value, 1) = ";" Or Left(LastLine->Value, 8) = "PRESERVE") AND LastLine->Prev <> 0: LastLine = LastLine->Prev: Loop
        If LCase(LastLine->Value) = " sts " + VarName + "," + RegName Then
          'Print "Deleting line reloading "; VarName; " into "; RegName
          CurrLine = LinkedListDelete(CurrLine)
        End If
      End If
      CurrLine = CurrLine->Next

    Loop

  End If

  'Check for subs that have a call as the last instruction
  'Turn call into a goto
  'If call is last instruction and always runs, no need for a return at the end either
  'Get last line of sub
  CompSub->HasFinalGoto = 0
  CompSub->FinalGotoDest = ""
  'Restart the code list
  CurrLine = CompSub->CodeStart
  'Do while has data in code list
  Do While CurrLine->Next <> 0
    'While has code in code list, set to next code line
    CurrLine = CurrLine->Next
  Loop

  'Handle last line... it could be a comment therefore PRESERVEd
  'Do while the current code line is PRESERVEd
  Do while Left(CurrLine->Value, 8) = "PRESERVE"
   'if PREVious code is NOT a PRESERVEd line then get the PREVious line as the current line, and, check that we are not at the top of the list
    If Left(CurrLine->Prev->Value, 8) <> "PRESERVE" and CurrLine->Prev <> 0 then
      CurrLine = CurrLine->Prev
    Else
      Exit Do
    End if
  Loop

  LastLine = CurrLine
  'Get line before last
  CheckLine = LastLine->Prev
  If CheckLine <> 0 Then
    Do While (Left(CheckLine->Value, 1) = ";" Or Left(CheckLine->Value, 8) = "PRESERVE") AND CheckLine->Prev <> 0
      CheckLine = CheckLine->Prev
    Loop
  End If

  If ModePIC Then
    If Left(LastLine->Value, 6) = " call " Then
      'Record presence of goto and destination for page selection later
      CompSub->HasFinalGoto = -1
      CompSub->FinalGotoDest = Trim(Mid(LastLine->Value, 6))
      LastLine->Value = " goto " + CompSub->FinalGotoDest
      If Left(CheckLine->Value, 5) <> " btfs" Then
        CompSub->NoReturn = -1
      End If
    End If
  Else
    If Left(LastLine->Value, 7) = " rcall " Then
      CompSub->HasFinalGoto = -1
      'No need to record goto destination for page selection
      LastLine->Value = " rjmp " + Trim(Mid(LastLine->Value, 7))
      If CheckLine = 0 Then
        CompSub->NoReturn = -1
      ElseIf Left(CheckLine->Value, 3) <> " sb" And Left(CheckLine->Value, 3) <> " br" Then
        CompSub->NoReturn = -1
      End If

    ElseIf Left(LastLine->Value, 6) = " call " Then
      CompSub->HasFinalGoto = -1
      LastLine->Value = " jmp " + Trim(Mid(LastLine->Value, 6))
      If CheckLine = 0 Then
        CompSub->NoReturn = -1
      ElseIf Left(CheckLine->Value, 3) <> " sb" And Left(CheckLine->Value, 3) <> " br" Then
        CompSub->NoReturn = -1
      End If
    End If
  End If

End Sub

Function ConfigTidy (DataSource As String ) As String
  'checks and resolve case sensitivity in config

  Dim searchIndex as Integer = 0
  Dim searchState as Integer
  Dim localDataSource  as String
  Dim localDataSourceValue  as String
  Dim localDataSourceState  as String
  Dim adaptedConfig as String
  Dim localpatchCounter as Integer
  Dim configtidycode as String = "C1"
  Dim as String sourcestringraw, sourcestring, deststring

  if AFISupport = 1 then
     ' print trim(DataSource)
     'Apply patches in INI file


     for localpatchCounter = 0 to patchCounter
          sourcestring = ucase(trim(Patches( localpatchCounter ).Source))
          sourcestringraw = DataSource
          deststring   = ucase(trim(DataSource))

          replaceall ( sourcestring, " ","")
          replaceall ( deststring, " ", "" )

          'print sourcestring, deststring

          if sourcestring =  deststring then
              DataSource = Patches( localpatchCounter ).Destination
              LogWarning ( sourcestringraw + " now " + DataSource, "Patched .S source using INI file: Was ")
              exit for
          end if
      next

     localDataSource = trim ( DataSource )
     ReplaceAll ( localDataSource, " ", "")


     localDataSourceValue = left( localDataSource, instr( localDataSource, "=")-1)
     localDataSourceState = mid( localDataSource, instr( localDataSource, "=")+1)

     'transform string K to numbers
     Select Case UCase(localDataSourceState)
        Case "32K": localDataSourceState = "32768": configtidycode = "C2"
        Case "16K": localDataSourceState = "16384": configtidycode = "C2"
        Case "8K": localDataSourceState = "8192"  : configtidycode = "C2"
        Case "4K": localDataSourceState = "4096"  : configtidycode = "C2"
        Case "2K": localDataSourceState = "2048"  : configtidycode = "C2"
        Case "1K": localDataSourceState = "1024"  : configtidycode = "C2"
     End Select

     for searchIndex = 0 to ubound( ReverseCfgFileLookup ) - 1

        ' print ucase(ReverseCfgFileLookup( searchIndex ).value) = localDataSourceValue ,ReverseCfgFileLookup( searchIndex ).value , "<"+localDataSourceValue+">"
        ' ReverseCfgFileLookup( findex ).State
        ' print ReverseCfgFileLookup( searchIndex ).State

        'search thru the state(s) looking for a match.. the issue to be resolved is the case sensitive nature of .S
        For searchState = 0 to ubound( ReverseCfgFileLookup ) - 1
          if ucase(trim(ReverseCfgFileLookup( searchState ).State)) = ucase(trim(localDataSourceState)) Then
            'replace with a case sensitive state
            localDataSourceState = ReverseCfgFileLookup( searchState ).State
            Exit For
          End if
        Next

        if ReverseCfgFileLookup( searchIndex ).Config = "" then exit for

        if ucase(ReverseCfgFileLookup( searchIndex ).Value) = localDataSourceValue then
          adaptedConfig  = " CONFIG "+ReverseCfgFileLookup( searchIndex ).Value + "="+localDataSourceState+ Pad32
          adaptedConfig  = Left (  adaptedConfig + Pad32+ Pad32, 48 ) + " ;"+ configtidycode +" Reverselookup " + DataSource
          ConfigTidy = adaptedConfig
          Exit Function
        End if

     next

      adaptedConfig  = " CONFIG "+DataSource+ Pad32
      adaptedConfig  = Left (  adaptedConfig + Pad32, 48 ) + " ;C2 No reverselookup "+ " was '" + DataSource+"'"
      ConfigTidy = "; " + adaptedConfig

 end if

End Function

Function AsmTidy (DataSource As String ,  StoredGCASM as integer ) As String

  Dim As LinkedListElement Pointer CurrLine
  Dim As LinkedListElement Pointer VarList

  If AFISupport = 1 then
    'Ensure all variables are the same case
    'failed as replace was TOO slow!! and was not replacing seoond instance.
        '      VarList = HashMapToList(FinalVarList, -1)
        '      IF VarList <> 0 AndAlso VarList->Next <> 0 THEN
        '
        '        CurrLine = VarList->Next
        '        Do While CurrLine <> 0
        '          With *CPtr(VariableListElement Pointer, CurrLine->MetaData)
        '              if (  instr( ucase( DataSource), ucase(.name) ) <> 0 ) then
        '                  'replace to match case
        '                  if instr(   DataSource, "," ) > 0 then
        '                    replace ( DataSource, .name+",", ucase(.name)+"," )
        '                    replace ( DataSource, ","+.name, ","+ucase(.name) )
        '                  else
        '                    replace ( DataSource, .name, ucase(.name) )
        '                  end if
        '              End if
        '          End With
        '          CurrLine = CurrLine->Next
        '        Loop

        '      END IF
    'just make upper case
    If Instr( DataSource, "CLCn") = 0 AND Instr( DataSource, "DMAn") = 0  then

        DataSource = ucase(DataSource)

    End if

  END IF



  Dim As String Temp, DSTemp
  Dim As Integer T, FS, AsmSize

  Temp = DataSource

  'Replace CLS sub and calls to it sith lbCLS on AVR
  If ModeAVR And INSTR(UCase(Temp), "CLS") <> 0 Then
    If Left(Temp, 4) <> " cls" Then WholeReplace Temp, "CLS", "lbCLS"
  End If

  'Remove @ if not inside data statement
  If Left(Temp, 4) <> " dw " Then
    Do While INSTR(Temp, "@") <> 0: Replace Temp, "@", "": Loop
  End If

  'Replace PRESERVE with value
  IF Left(Temp, 9) = "PRESERVE " THEN
    T = VAL(Mid(Temp, 10))
    Temp = PreserveCode(T)
    If Mid(Temp, 2, 1) = Chr(8) Then Temp = Mid(Temp, 3)
  END IF

  'Do not process comments
  IF Left(Temp, 1) = ";" AND Left(LCase(Temp), 5) <> ";sub'" THEN AsmTidy = Temp: EXIT FUNCTION

  'Store asm program for GCASM
  IF UCase(ASMExe) = "GCASM" AND StoredGCASM = -1 THEN

    IF Trim(Temp) <> "" THEN
      DSTemp = Temp
      DO WHILE INSTR(DSTemp, Chr(9)) <> 0: Replace DSTemp, Chr(9), " ": LOOP
      IF INSTR(DSTemp, ";") <> 0 THEN DSTemp = Left(DSTemp, INSTR(DSTemp, ";") - 1)
      DSTemp = Trim(DSTemp)
      IF Left(UCase(DSTemp), 3) <> "DT " AND Left(UCase(DSTemp), 3) <> "DW " THEN DSTemp = UCase(DSTemp)
      IF Left(UCase(DSTemp), 3) = "DT " OR Left(UCase(DSTemp), 3) = "DW " THEN DSTemp = UCase(Left(DSTemp, 3)) + Mid(DSTemp, 4)
      If Left(UCase(DSTemp), 4) = ".DB " Then DSTemp = Mid(DSTemp, 2)
      If Left(UCase(DSTemp), 5) = ".ORG " Then DSTemp = Mid(DSTemp, 2)
      If ModePIC Or (Left(DSTemp, 1) <> "." And Left(DSTemp, 1) <> "#") Then AsmProgLoc = LinkedListInsert(AsmProgLoc, DSTemp)
    Else
      AsmProgLoc = LinkedListInsert(AsmProgLoc, "")
    END IF
  END IF

  If ModePIC Then
    'Resolves case where BITS have been named by GETCHIPDATA.bas as "register_bit" - these are not recognised by MPASM 
    ' example 
        ' Set TX2STA_SYNC Off
        '	bcf	TX2STA,SYNC_TX2STA,BANKED
        '
      IF Instr( Temp, "_") <> 0 Then
        IF CountOccur ( Temp, ",") Then
            Dim as string ElementsArray(), InstructionArray()
            ' Split the Temp string into the array ElementsArray() 
            StringSplit ( Temp, ",",-1,ElementsArray() )

            ' Ensure SFRBit exists
            If HasSFRBit ( ElementsArray(1) ) Then
              ' Split the first array element ElementsArray(0) into ElementsArray()
              StringSplit ( ElementsArray(0), " ", -1, InstructionArray() )
              ' Look for Bit with suffix of '_Register' within in ElementsArray(1)
              If Instr( ElementsArray(1), InstructionArray(2)+"_" )   > 0 Then
              
                ' Replace with null string when matched.  
                Replace ( ElementsArray(1), InstructionArray(2)+"_", "" )
                '18F
                IF CountOccur ( Temp, ",") = 2 Then Temp = ElementsArray(0)+","+ ElementsArray(1)+","+ElementsArray(2)
                '10F..16F
                IF CountOccur ( Temp, ",") = 1 Then Temp = ElementsArray(0)+","+ ElementsArray(1)
                
              End if
            End if
        End If
      End If 
  End If
  
  If AFISupport = 1 and UserCodeOnlyEnabled = -1 then
    'Check for Reservedword case
    dim ReservedWordCounter as Integer
    For ReservedWordCounter = 0 to RESERVED_WORDS
        If Instr(Ucase(Trim(Temp)), Ucase(trim(ReservedWords(ReservedWordCounter)))) > 0 And LEN(ReservedWords(ReservedWordCounter))> 2 THEN
            replace( Temp, Ucase(ReservedWords(ReservedWordCounter)),  ReservedWords(ReservedWordCounter)  )
            exit for
        End if
    Next
    'remove MPASM errorlevel setting from .S source
    If Instr( ucase(temp), "ERRORLEVEL") > 0 Then
      temp = ";"+temp
    End if

  End if

  'Format command, output result
  IF  INSTR(Temp, " EQU " ) = 0 then
    IF INSTR(Temp, " ") <> 0 THEN Replace Temp, " ", Chr(9)
    IF INSTR(Temp, " ") <> 0 THEN Replace Temp, " ", Chr(9)
  END IF
  AsmTidy = Temp

END FUNCTION

SUB AssembleProgram
  'GCASM Assembler

  'Variables
  Dim As String Temp, NewData, DataSource, NDTemp, ROMData, Reg, NameTemp
  Dim As String DSTemp, HRTemp, NewRecordData, Address, RecType, RecCheck, ValueTemp
  Dim As String RecSize, HexAddress, HexContents, Byte1, Byte2, HTemp, NewCmd, Cmd
  Dim As String CurrentParam, ParamStart, ParamName, ParamValue, CurrentBit, ConfName
  Dim As String CTO, PSO

  Dim As Integer CD, PD, IsSFR, SFL, SP1, CSB, CurrentLocation, T, DT, DataSize
  Dim As Integer RP1, DWC, RSC, DWIC, SS, CS, RepeatBanksel, B, FB, RepeatPagesel
  Dim As Integer PRC, KeepReplacing, AW, PB, FP, PVT, TBT, HT, PV, UseBSR
  Dim As Integer FCO, COI, CCI, HRC, OIR, RCC, DataBlockSize, ConfigBaseLoc
  Dim As LongInt CL, OA, FRA, CHA
  Dim As AsmCommand Pointer CurrCmd, LDSLoc, STSLoc

  Dim As Single CurrPerc, PercAdd, PercOld

  Dim As String ParamValues(100)
  Dim As Integer ParamWasLabel(100)
  Dim As String ParamElement(100)
  Dim As Integer PValueCount, CurrentLine, ParamElements, ElementNo

  Dim As LinkedListElement Pointer AsmLine, AsmSymbolList, CurrItem
  Dim As LinkedListElement Pointer OutProgram, CurrProgramLoc
  Dim As LinkedListElement Pointer HexRecords, CurrHexRecord

  If VBS = 1 then
    Temp = Message("Assembler")
    Replace Temp, "%ASMEXE%", ASMEXE
    PRINT Temp
  End if
  'Clear compiled program
  OutProgram = LinkedListCreate
  CurrProgramLoc = OutProgram

  'Check for RP1 bit
  If ModePIC Then
    'Should banksel set 1 or 2 STATUS bits?
    RP1 = HasSFRBit("RP1")
    If MemSize <= 256 Then RP1 = 0 'If 256 or less addresses, there is no RP1
  End If
  'Is this a 16F5 with a BSR register
  UseBSR = 0
  If ModePIC Then
    If IsASM("movlb", 1) Then
      UseBSR = -1
    End If
  End If

  'Get index of LDS and STS commands (AVR)
  If ModeAVR Then
    LDSLoc = IsASM("lds", 0)
    STSLoc = IsASM("sts", 0)
  End If

  'Get config base location (18F)
  ConfigBaseLoc = GetConfigBaseLoc

  'Build symbol table
  If VBS = 1 Then Print SPC(5); Message("SymbolTable")
  BuildAsmSymbolTable

  'Convert asm > binary > hex
  IF VBS = 1 Then Print SPC(5); Message("MachineCode")
  PercOld = 0
  CurrPerc = 0.5
  PercAdd = 1 / LinkedListSize(AsmProg) * 100

  'Open list file
  Open ReplaceToolVariables("%filename%", "lst") For Output As #1
  Print #1, "GCASM List File (GCBASIC " + Version + ")"
  Print #1, ""
  Print #1, "Symbols:"
  AsmSymbolList = HashMapToList(ASMSymbols, -1)
  CurrItem = AsmSymbolList->Next
  Do While CurrItem <> 0
    Print #1, CurrItem->Value + " " + Chr(9) + "EQU " + Chr(9) + *CPtr(String Pointer, CurrItem->MetaData)
    CurrItem = CurrItem->Next
  Loop
  Print #1, ""
  Print #1, "Code:"
  Print #1, "Loc" + Chr(9) + "Obj Code" + Chr(9) + "Original Assembly"

  'Convert line at a time
  CurrentLine = 0
  AsmLine = AsmProg->Next
  Do While AsmLine <> 0

    'Show percentage assembled
    IF VBS = 1 And ShowProgressCounters Then
      CurrPerc += PercAdd
      If Int(CurrPerc) > Int(PercOld) Then
        PercOld = CurrPerc
        LOCATE , 60
        Print Int(CurrPerc); "%";
      End If
    End If

    'Get the line
    DataSource = AsmLine->Value
    RepeatBanksel = 0
    IF INSTR(DataSource, ":") <> 0 THEN
      CurrentLine = Val(Left(DataSource, INSTR(DataSource, ":") - 1))
      DataSource = Mid(DataSource, INSTR(DataSource, ":") + 1)
    END IF
    IF DataSource <> "" THEN
      'Get the index of the assembly command
      CurrCmd = IsASM(DataSource)

      'Get current line for list file
      Dim As String DebugInput, DebugOutput, DebugLoc
      DebugInput = Mid(AsmLine->Value, InStr(AsmLine->Value, ":") + 1)
      DebugOutput = ""

      'Special Cases
      If CurrCmd = 0 Then
        IF Left(DataSource, 8) = "BANKSEL " THEN
          Temp = UCase(Trim(Mid(DataSource, 8)))
          B = Val(HashMapGetStr(ASMSymbols, Temp))

          IF ChipFamily = 12 Or ChipFamily = 14 THEN
            If UseBSR Then
              B = (B AND 96) / 32
              DataSource = "MOVLB " + Str(B)
              AsmLine->Value = DataSource
              CurrCmd = IsASM(DataSource)
            Else
              If ChipFamily = 12 Then
                B = (B AND 96) / 32
              Else
                B = (B AND 384) / 128
              End If
              RepeatBanksel = 0
              AddMoreBanksel:
              RepeatBanksel = RepeatBanksel + 1
              If RepeatBanksel = 2 THEN
                CurrentLine = CurrentLine + 1
                DataSource = "BCF STATUS,RP1"
                IF (B AND 2) > 0 THEN DataSource = "BSF STATUS,RP1"
                AsmLine->Value = DataSource
                CurrCmd = IsASM(DataSource)
              END IF
              If RepeatBanksel = 1 THEN
                IF RP1 = 0 THEN RepeatBanksel = 2
                DataSource = "BCF STATUS,RP0"
                IF (B AND 1) > 0 THEN DataSource = "BSF STATUS,RP0"
                AsmLine->Value = DataSource
                CurrCmd = IsASM(DataSource)
              END If
            End If

          ElseIf ChipFamily = 15 Then
            'Print DataSource + ": Location " + Hex(B) + " Bank ";
            If ChipFamilyVariant = 0 Then
              B = (B AND 3968) / 128
            Else
              B = (B AND 8064) / 128
            End If
            'Print B
            DataSource = "MOVLB " + Trim(Str(B))
            AsmLine->Value = DataSource
            CurrCmd = IsASM(DataSource)

          ElseIf ChipFamily = 16 THEN
            B = (B AND 65280) / 256
            DataSource = "MOVLB " + Str(B)
            AsmLine->Value = DataSource
            CurrCmd = IsASM(DataSource)
          END IF

        ElseIF Left(DataSource, 8) = "PAGESEL " THEN
          B = 0
          Temp = Trim(Mid(DataSource, 8))
          IF Temp = "$" THEN B = CurrentLine
          IF B = 0 THEN
            B = Val(HashMapGetStr(ASMSymbols, Temp))
          END IF

          'Remove pagesel on chips with under 2048 words
          If PCUpper = 0 Then
            CurrCmd = 0
          Else
            RepeatPagesel = 0
            AddMorePagesel:

            If ChipFamily = 12 Then
              B = (B AND 1536) / 512
              RepeatPagesel = RepeatPagesel + 1
              If RepeatPagesel = 2 THEN
                CurrentLine = CurrentLine + 1
                DataSource = "BCF STATUS,PA1"
                IF (B AND 2) > 0 THEN DataSource = "BSF STATUS,PA1"
                AsmLine->Value = DataSource
                CurrCmd = IsASM(DataSource)
              ElseIf RepeatPagesel = 1 THEN
                If PCUpper = 1 THEN RepeatPagesel = 2
                DataSource = "BCF STATUS,PA0"
                IF (B AND 1) > 0 THEN DataSource = "BSF STATUS,PA0"
                AsmLine->Value = DataSource
                CurrCmd = IsASM(DataSource)
              END If

            ElseIf ChipFamily <> 15 Then


              RepeatPagesel = RepeatPagesel + 1
              If RepeatPagesel = 2 THEN
                CurrentLine = CurrentLine + 1
                DataSource = "BCF PCLATH,4"
                IF (B AND 2) > 0 THEN DataSource = "BSF PCLATH,4"  ' PageSel 2,3, 6 etc would be valid
                AsmLine->Value = DataSource
                CurrCmd = IsASM(DataSource)
              END IF
              If RepeatPagesel = 1 THEN
                'First time thru, there is a GOTO much later that processes the second PageSel via the label AddMorePagesel:
                'So, take the address and select the page - now B is the PageSel.  B is reused on the second process ( see IF-THEN above)
                B = (B AND 6144) / 2048
                If PCUpper = 1 THEN RepeatPagesel = 2  'If PCUPPER =1 therefore a small PROGMEM chip.  So, set RepeatPagesel=2 to stop the processing of BxF PCLATH,4 as the second process will increment RepeatPagesel to 3...
                DataSource = "BCF PCLATH,3"
                IF (B AND 1) > 0 THEN DataSource = "BSF PCLATH,3"
                AsmLine->Value = DataSource
                CurrCmd = IsASM(DataSource)
              END If
            Else
              B = (B And 65280) / 256
              DataSource = "MOVLP " + Str(B)
              AsmLine->Value = DataSource
              CurrCmd = IsASM(DataSource)
            End If
          End If

        ElseIF Left(DataSource, 9) = "BANKISEL " THEN
          B = 0
          Temp = Trim(Mid(DataSource, 9))
          IF Temp = "$" THEN B = CurrentLine
          IF B = 0 THEN
            B = Val(HashMapGetStr(ASMSymbols, Temp))
          END IF

          IF B < 256 Then DataSource = " bcf STATUS,IRP"
          IF B > 255 Then DataSource = " bsf STATUS,IRP"
          CurrCmd = IsASM(DataSource)

        'Raw hex code
        ElseIF Left(DataSource, 4) = "RAW " THEN
          DataSource = Trim(Mid(DataSource, 4))

          DebugLoc = Hex(CurrentLine)
          DebugOutput = ""
          Do While Len(DebugLoc) < 6
            DebugLoc = "0" + DebugLoc
          Loop

          DO WHILE INSTR(DataSource, ",") <> 0

            CurrProgramLoc = LinkedListInsert(CurrProgramLoc, Left(DataSource, INSTR(DataSource, ",") - 1), CurrentLine)
            DebugOutput += (Left(DataSource, INSTR(DataSource, ",") - 1) + " ")
            If ChipFamily = 16 Then
              CurrentLine = CurrentLine + 2
            Else
              CurrentLine = CurrentLine + 1
            End If
            DataSource = Mid(DataSource, INSTR(DataSource, ",") + 1)
          LOOP
          CurrProgramLoc = LinkedListInsert(CurrProgramLoc, DataSource, CurrentLine)

          DebugOutput += DataSource

          Print #1, DebugLoc + Chr(9) + Trim(DebugOutput) + Chr(9) + DebugInput

          CurrCmd = 0

        'Label
        Else
          Print #1, DebugLoc + Chr(9) + Trim(DebugOutput) + Chr(9) + DebugInput

        END IF
      End If

      'Standard assembly command
      IF CurrCmd <> 0 THEN

        'If AVR, convert CLR r to EOR r,r and TST r to AND r,r
        If ModeAVR Then
          If Left(DataSource, 4) = "CLR " Then
            Reg = Trim(Mid(DataSource, 5))
            DataSource = "EOR " + Reg + "," + Reg
            CurrCmd = IsASM(DataSource)

          ElseIf Left(DataSource, 4) = "TST " Then
            Reg = Trim(Mid(DataSource, 5))
            DataSource = "AND " + Reg + "," + Reg
            CurrCmd = IsASM(DataSource)

          ElseIf Left(DataSource, 4) = "LSL " Then
            Reg = Trim(Mid(DataSource, 5))
            DataSource = "ADD " + Reg + "," + Reg
            CurrCmd = IsASM(DataSource)

          ElseIf Left(DataSource, 4) = "ROL " Then
            Reg = Trim(Mid(DataSource, 5))
            DataSource = "ADC " + Reg + "," + Reg
            CurrCmd = IsASM(DataSource)

          End If
        End If

        'Get command
        ParamValue = UCase(Trim(DataSource))
        IF INSTR(ParamValue, " ") = 0 Then
          ParamValue = ""
        Else
          ParamValue = Trim(Mid(ParamValue, INSTR(ParamValue, " ") + 1))
        End If

        'Get parameters
        GetTokens (ParamValue, ParamValues(), PValueCount, ",")
        PRC = CurrCmd->Params
        If CurrCmd->Params > PValueCount Then
          For CD = PValueCount + 1 To CurrCmd->Params
            ParamValues(CD) = "0"
          Next
        End If
        'TD = Name, CT = Value

        'Replace symbols in parameters
        FOR CD = 1 TO PRC
          ParamWasLabel(CD) = 0
          CTO = ParamValues(CD)
          'Replace current address
          If ModePIC Then
            Do While InStr(ParamValues(CD), "$") <> 0: Replace ParamValues(CD), "$", Str(CurrentLine): Loop
          ElseIf ModeAVR Then
            WholeReplace ParamValues(CD), "PC", Str(CurrentLine)
          End If
          'Replace symbols
          Do
            KeepReplacing = 0
            ParamValue = HashMapGetStr(ASMSymbols, ParamValues(CD))
            If ParamValue <> "" Then
              If Not IsConst(ParamValue) Then
                KeepReplacing = -1
              Else
                'On AVR, need to add 32 to SFR addresses for LDS/STS
                If ModeAVR And ((CD = 2 And CurrCmd = LDSLoc) Or (CD = 1 And CurrCmd = STSLoc)) Then
                  If IsIOReg(ParamValues(CD)) Then ParamValue = Str(Val(ParamValue) + 32)
                End If
              End If
              ParamValues(CD) = ParamValue

            ElseIf Not IsConst(ParamValues(CD)) Then

              ParamElements = 0
              GetTokens(ParamValues(CD), ParamElement(), ParamElements, , -1)

              ParamValues(CD) = ""
              For ElementNo = 1 To ParamElements
                If Not IsDivider(ParamElement(ElementNo)) Then
                  ParamValue = HashMapGetStr(ASMSymbols, ParamElement(ElementNo))
                  If ParamValue <> "" Then
                    ParamElement(ElementNo) = ParamValue
                    ParamWasLabel(CD) = -1
                    If Not IsConst(ParamValue) Then KeepReplacing = -1
                  End If
                End If
                ParamValues(CD) += ParamElement(ElementNo)
              Next
            End If
          Loop While KeepReplacing

          'Some AVR only operations
          If ModeAVR Then
            'Correct address for br* and rjmp/rcall instructions
            If UCase(Left(DataSource, 2)) = "BR" Or UCase(Left(DataSource, 4)) = "RJMP" OR UCase(Left(DataSource, 5)) = "RCALL" Then
              If LCase(CurrCmd->Param(CD)) = "k" Then
                'Calculate value
                If IsCalc(ParamValues(CD)) Then Calculate ParamValues(CD)

                'Error if invalid location
                If NOT IsConst(ParamValues(CD)) THEN
                Temp = Message("SymbolNotDefined")
                Replace Temp, "%symbol%", ParamValues(CD)
                LogError "GCASM: " + Temp + " "+ Message("At") + " " + DataSource
              END If
                ParamValues(CD) = Trim(ParamValues(CD))
                ParamValues(CD) = Str(MakeDec(ParamValues(CD)) - CurrentLine - 1)
              End If
            End If
            'Replace register names
            ParamValues(CD) = Trim(ParamValues(CD))
            If LCASE(LEFT(ParamValues(CD), 1)) = "r" And IsConst(MID(ParamValues(CD), 2)) Then
              ParamValues(CD) = MID(ParamValues(CD), 2)
            End If

          'PIC only operations
          ElseIf ModePIC Then
            If ChipFamily = 16 Then
              'Fix branches
              If UCase(Left(DataSource, 3)) = "BRA" Or UCase(Left(DataSource, 6)) = "RCALL " Or _
                 UCase(Left(DataSource, 3)) = "BC " Or UCase(Left(DataSource, 4)) = "BNC " Or _
                 UCase(Left(DataSource, 3)) = "BN " Or UCase(Left(DataSource, 4)) = "BNN " Or _
                 UCase(Left(DataSource, 3)) = "BZ " Or UCase(Left(DataSource, 4)) = "BNZ " Or _
                 UCase(Left(DataSource, 4)) = "BOV " Or UCase(Left(DataSource, 5)) = "BNOV " Then
                ParamValues(CD) = Str((MakeDec(ParamValues(CD)) - CurrentLine) / 2 - 1)
              End If
            End If
          End If

          'Calculate value
          If IsCalc(ParamValues(CD)) Then Calculate ParamValues(CD)
          ParamValues(CD) = Trim(ParamValues(CD))
        NEXT

        'Get command binary, and add in parameters
        Cmd = CurrCmd->Word(1)
        'IF VAL(ASMCommanDataSource(T, 2)) = 2 THEN Cmd = Cmd + ":" + ASMCommanDataSource(T, 4)
        IF ChipFamily <> 16 Or Left(UCase(DataSource), 5) = "LFSR " Or Left(UCase(DataSource), 7) = "MOVFFL " Then
          With *CurrCmd
            For AW = 2 To .Words
              Cmd = Cmd + ":" + .Word(AW)
            Next
          End With
        Else
           With *CurrCmd
            For AW = 2 To .Words
              Cmd = .Word(AW) + ":" + Cmd
            Next
          End With
        END IF

        FOR CD = 1 TO PRC

          'Count the number of bits available to store the parameter
          PB = 0
          CurrentParam = CurrCmd->Param(CD)

          FOR FP = 1 TO LEN(Cmd)
            IF Mid(Cmd, FP, 1) = CurrentParam THEN
              If FP > 1 And Cmd = "'" Then Exit For
              PB = PB + 1
            End If
            NEXT

          'Handle negative parameter
          If LEFT(ParamValues(CD), 1) = "-" Then
            ParamValues(CD) = STR(VAL(ParamValues(CD)) And (2 ^ PB - 1))
          End If

          'Check parameter is not an undefined symbol
          If NOT IsConst(ParamValues(CD)) AND IsASM(ParamValues(CD)) = 0 THEN
            Temp = Message("SymbolNotDefined")
            Replace Temp, "%symbol%", ParamValues(CD)
            'Print DebugInput
            DebugLoc = Hex(Val(AsmLine->Value))
            Do While Len(DebugLoc) < 6
              DebugLoc = "0" + DebugLoc
            Loop
            LogError "GCASM: " + Temp + " "+Message("At")+" `" +DataSource+"` Hexfile address(see lst file): "+DebugLoc
          END If

          'Get data
          ParamStart = ""
          T = 1
          PVT = MakeDec(ParamValues(CD))

          'Halve address for call, goto on 18F
          If ChipFamily = 16 AND (Left(UCase(DataSource), 5) = "CALL " OR Left(UCase(DataSource), 5) = "GOTO ") AND CurrentParam = "K" Then
            PVT = PVT / 2
          End If

          'Convert parameter from decimal to binary
          DO WHILE PVT > 0
            TBT = PVT AND T
            IF TBT = 0 THEN ParamStart = "0" + ParamStart
            IF TBT <> 0 THEN ParamStart = "1" + ParamStart
            PVT = PVT - TBT
            T = T * 2
          LOOP
          'Adjust length
          DO WHILE LEN(ParamStart) < PB
            ParamStart = "0" + ParamStart
          LOOP
          'PRINT ParamStart, CurrentParam, DataSource, Cmd
          IF LEN(ParamStart) > PB AND CurrentParam = "K" AND INSTR(UCase(DataSource), "GOTO") = 0 AND INSTR(UCase(DataSource), "CALL") = 0 THEN
              If ChipFamily = 121 AND (Left(UCase(DataSource), 4) = "LDS " OR Left(UCase(DataSource), 4) = "STS ")  Then
                    'Issue no warning as this is valid
                    'PRINT ParamStart, PB, CurrentParam, DataSource, Cmd
              ELSE
                    'PRINT ParamStart, PB, CurrentParam, DataSource, Cmd
                    LogWarning "GCASM: " + Message("ValueOutOfRange")
                    Print #1, Message("ValueOutOfRange")
                    ParamStart = Right(ParamStart, PB)
              END IF
          END IF
          ParamStart = Right(ParamStart, PB)

          'Add in binary parameter to binary command
          PSO = ParamStart 'Save original parameter, may be needed
          FP = 0
          'Print #1, Chr(9) + CurrentParam + Chr(9) + Cmd + Chr(9) + ParamValues(CD)
          Do While FP <= LEN(Cmd)
            FP += 1
            IF Mid(Cmd, FP, 1) = CurrentParam THEN
              CurrentBit = "": IF FP > 1 THEN CurrentBit = Mid(Cmd, FP - 1, 1)
              'Inverted parameter bit
              If CurrentBit = "_" Then
                Temp = Left(ParamStart, 1)
                If Temp = "1" Then Temp = "0" Else Temp = "1"
                Cmd = Left(Cmd, FP - 2) + Temp + Mid(Cmd, FP + 1)
                FP -= 1

              'Start new copy of parameter
              ElseIf CurrentBit = "'" Then
                ParamStart = PSO
                Mid(Cmd, FP) = Left(ParamStart, 1)

              'Normal parameter bit
              Else
                Mid(Cmd, FP) = Left(ParamStart, 1)

              End If
              ParamStart = Mid(ParamStart, 2)
            END IF
          'NEXT
          Loop
          'Print #1, Chr(9) + Chr(9) + Cmd
        NEXT

        'Convert binary > hex
        'Color 9: Print DataSource, Cmd: Color 7
        DO WHILE INSTR(Cmd, " ") <> 0: Replace Cmd, " ", "": LOOP
        DO WHILE INSTR(Cmd, ":") <> 0
          IF ChipFamily <> 16 Or Left(UCase(DataSource), 5) = "LFSR " Or Left(UCase(DataSource), 7) = "MOVFFL " THEN
            NewCmd = Mid(Cmd, INSTR(Cmd, ":") + 1)
            Cmd = Left(Cmd, INSTR(Cmd, ":") - 1)
          Else
            NewCmd = Left(Cmd, InStrRev(Cmd, ":") - 1)
            Cmd = Mid(Cmd, InStrRev(Cmd, ":") + 1)
          END IF

          HT = 0
          PV = 1
          FOR T = LEN(Cmd) TO 1 STEP -1
            HT = HT + VAL(Mid(Cmd, T, 1)) * PV
            PV = PV * 2
          NEXT
          HTemp = Hex(HT)
          DO WHILE LEN(HTemp) < 4
            HTemp = "0" + HTemp
          LOOP
          DebugOutput = DebugOutput + HTemp + " "
          CurrProgramLoc = LinkedListInsert(CurrProgramLoc, HTemp, CurrentLine)
          If ChipFamily = 16 Then
            CurrentLine += 2
          Else
            CurrentLine += 1
          End If
          Cmd = NewCmd
        LOOP

        HT = 0
        PV = 1
        FOR T = LEN(Cmd) TO 1 STEP -1
          HT = HT + VAL(Mid(Cmd, T, 1)) * PV
          PV = PV * 2
        NEXT
        HTemp = Hex(HT)
        DO WHILE LEN(HTemp) < 4
          HTemp = "0" + HTemp
        Loop
        DebugOutput = DebugOutput + HTemp + " "
        DebugLoc = Hex(Val(AsmLine->Value))
        Do While Len(DebugLoc) < 6
          DebugLoc = "0" + DebugLoc
        Loop
        Print #1, DebugLoc + Chr(9) + Trim(DebugOutput) + Chr(9) + DebugInput
        CurrProgramLoc = LinkedListInsert(CurrProgramLoc, HTemp, CurrentLine)

        'PRINT DataSource, Cmd, HTemp

        If RepeatBanksel = 1 THEN GOTO AddMoreBanksel
        If RepeatPagesel = 1 THEN GOTO AddMorePagesel

      END IF

    'Blank line
    Else
      Print #1, ""
    END If

    AsmLine = AsmLine->Next
  Loop
  IF VBS = 1 And ShowProgressCounters Then PRINT


  'Close list file
  Close #1

  'Add config
  Dim As Integer CurrConfWord, ConfWord(ConfWords)
  If ModePIC And Not ConfigDisabled Then 'No config needed for AVR
    If ChipFamily <> 16 THEN

      For CurrConfWord = 1 To ConfWords
        'Get default word value
        If ChipFamily = 12 Then
          ConfWord(CurrConfWord) = 2 ^ 12 - 1
        Else
          ConfWord(CurrConfWord) = 2 ^ 14 - 1
        End If

        'Apply settings
        DataSource = OutConfig(CurrConfWord)
        DO WHILE INSTR(DataSource, "&") <> 0
          Temp = UCase(Trim(Left(DataSource, INSTR(DataSource, "&") - 1)))
          IF Left(Temp, 1) = "_" THEN Temp = Mid(Temp, 2)
          DataSource = Trim(Mid(DataSource, INSTR(DataSource, "&") + 1))
          FOR FCO = 1 TO COC
            With ConfigOps(FCO)
              IF Temp = UCase(.Op) THEN
                ConfWord(CurrConfWord) = ConfWord(CurrConfWord) AND .Val
                EXIT FOR
              END IF
            End With
          NEXT
        LOOP
        IF DataSource <> "" THEN
          Temp = UCase(Trim(DataSource))
          IF Left(Temp, 1) = "_" THEN Temp = Mid(Temp, 2)
          FOR FCO = 1 TO COC
            With ConfigOps(FCO)
              IF Temp = UCase(.Op) THEN
                ConfWord(CurrConfWord) = ConfWord(CurrConfWord) AND .Val
                EXIT FOR
              END IF
            End With
          NEXT
        END If

        'Store to hex file
        If ChipFamily = 12 Then
          CurrProgramLoc = LinkedListInsert(CurrProgramLoc, "0" + HEX(ConfWord(CurrConfWord)), &HFFF)
        Else
          CurrProgramLoc = LinkedListInsert(CurrProgramLoc, HEX(ConfWord(CurrConfWord)), GetConfigBaseLoc - 1 + CurrConfWord)
        End If
      Next

    End IF

    If ChipFamily = 16 THEN

      'Use TempData to store config words
      Dim ConfWordCount As Integer
      ConfWordCount = ConfWords 'was 20

      'Initialise TempData()
      FOR CD = 1 to ConfWordCount
        TempData(CD) = Str(ConfigMask(CD))
      NEXT

      'Split up OutConfig(1), store in CheckTemp()
      DataSource = OutConfig(1)
      T = 0
      DO WHILE INSTR(DataSource, ",") <> 0
        T += 1
        CheckTemp(T) = Trim(Left(DataSource, INSTR(DataSource, ",") - 1))
        DataSource = Trim(Mid(DataSource, INSTR(DataSource, ",") + 1))
      LOOP
      IF DataSource <> "" THEN
        T += 1
        CheckTemp(T) = DataSource
      END IF

      'Add default settings where needed
      'Check every default setting
      FOR CD = 1 TO DCOC
        ConfName = Left(DefCONFIG(CD), INSTR(DefCONFIG(CD), "=") - 1)
        'Is setting overriden anywhere?
        For PD = 1 to T
          If ConfigNameMatch(CheckTemp(PD), ConfName) Then
            'Yes, setting overridden
            GoTo ApplyNextDefault
          End If
        Next

        'Add default, since it is not overridden
        T += 1
        CheckTemp(T) = DefConfig(CD)

        ApplyNextDefault:
      Next

      'Process values
      FOR PD = 1 to T
        DataSource = CheckTemp(PD)
        COI = 0

        'Find exact match
        Temp = UCase(DataSource)
        DO WHILE INSTR(Temp, "=") <> 0: Replace Temp, "=", "_": Loop
        ReplaceAll (Temp, " ", "")
        FOR FCO = 1 TO COC
          IF Temp = UCase(ConfigOps(FCO).Op) THEN
            COI = FCO
            EXIT FOR
          END IF
        NEXT

        'If no exact match, find close match
        IF COI = 0 THEN
          ConfName = UCase(Trim(Left(DataSource, INSTR(DataSource, "=") - 1)))
          ValueTemp = UCase(Trim(Mid(DataSource, INSTR(DataSource, "=") + 1)))
          FOR FCO = 1 TO COC
            With ConfigOps(FCO)
              IF INSTR(UCase(.Op), ConfName) <> 0 AND INSTR(UCase(.Op), ValueTemp) <> 0 THEN
                COI = FCO
                EXIT FOR
              END IF
            End With
          NEXT
        END IF

        'If still nothing, then we have a problem
        IF COI = 0 THEN
          Temp = Message("BadConfig")
          Replace Temp, "%option%", DataSource
          LogError "GCASM: " + Temp
        End IF

        'Apply setting
        With ConfigOps(COI)
          TempData(.Loc) = Str(VAL(TempData(.Loc)) AND .Val)
        End With

      NEXT

      'Store settings
      CCI = 0
      Do
        CCI += 1
        Byte1 = Hex(VAL(TempData(CCI)))
        Do While LEN(Byte1) < 2: Byte1 = "0" + Byte1: LOOP

        CCI += 1
        Byte2 = Hex(VAL(TempData(CCI)))
        Do While LEN(Byte2) < 2: Byte2 = "0" + Byte2: LOOP
        CurrProgramLoc = LinkedListInsert(CurrProgramLoc, Byte2 + Byte1, ConfigBaseLoc + CCI - 2)

      Loop While CCI < ConfWordCount

    END IF
  End If

  'Prepare hex records
  'Dim As Single OA, FRA
  HexRecords = LinkedListCreate
  CurrHexRecord = HexRecords
  OA = -1
  OIR = 0
  RCC = 0
  HRTemp = ""
  FRA = -1
  CHA = 0 'High address word
  CurrProgramLoc = OutProgram->Next
  Do While CurrProgramLoc <> 0

    CL = CurrProgramLoc->NumVal
    IF ChipFamily <> 16 THEN CL = CL * 2
    If OA = -1 Then OA = CL
    NewRecordData = CurrProgramLoc->Value
    Do While LEN(NewRecordData) < 4: NewRecordData = "0" + NewRecordData: Loop

    IF OIR >= 16 OR OA < (CL - 2) Or OA > CL THEN
      'Save record, add new one

      'Format: :SSAAAATTDDDDDDDDCC
      'Size
      RecSize = Right(Hex(OIR), 2)
      DO WHILE LEN(RecSize) < 2: RecSize = "0" + RecSize: LOOP
      RCC = RCC + OIR
      'Start Address
      Address = Right(Hex(FRA), 4)
      DO WHILE LEN(Address) < 4: Address = "0" + Address: LOOP
      RCC = RCC + (FRA AND 255) + (FRA AND 65280) \ 256
      'Type
      RecType = "00"
      'Checksum
      RecCheck = Right(Hex(256 - (RCC AND 255)), 2)
      DO WHILE LEN(RecCheck) < 2: RecCheck = "0" + RecCheck: LOOP

      'Add high address?
      IF ((FRA \ 65536) AND 65535) <> CHA THEN
        CHA = ((FRA \ 65536) AND 65535)
        ':02 0000 04 0030 CA
        HexAddress = Right(HEX(CHA), 4)
        DO WHILE LEN(HexAddress) < 4: HexAddress = "0" + HexAddress: LOOP
        If ModePIC Then
          HexContents = Hex(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 6) AND 255))
        ElseIf ModeAVR Then
          HexContents = Hex(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 4) AND 255))
        End If
        DO WHILE LEN(HexContents) < 2: HexContents = "0" + HexContents: LOOP
        If ModePIC Then
          CurrHexRecord = LinkedListInsert(CurrHexRecord, ":02000004" + HexAddress + HexContents)
        ElseIf ModeAVR Then
          CurrHexRecord = LinkedListInsert(CurrHexRecord, ":02000002" + HexAddress + HexContents)
        End IF
      END IF

      CurrHexRecord = LinkedListInsert(CurrHexRecord, ":" + RecSize + Address + RecType + HRTemp + RecCheck)

      HRTemp = Mid(NewRecordData, 3, 2) + Mid(NewRecordData, 1, 2)
      RCC = VAL("&H" + Mid(NewRecordData, 3, 2)) + VAL("&H" + Mid(NewRecordData, 1, 2))
      OIR = 2
      OA = CL
      FRA = CL

    ELSE
      'Add to current record
      HRTemp = HRTemp + Mid(NewRecordData, 3, 2) + Mid(NewRecordData, 1, 2)
      RCC = RCC + VAL("&H" + Mid(NewRecordData, 3, 2)) + VAL("&H" + Mid(NewRecordData, 1, 2))
      OIR += 2
      OA = CL
      IF FRA = -1 THEN FRA = CL
    END IF

    CurrProgramLoc = CurrProgramLoc->Next
  Loop

  'Save last record

  'Format: :SSAAAATTDDDDDDDDCC
  'Size
  RecSize = Right(Hex(OIR), 2)
  DO WHILE LEN(RecSize) < 2: RecSize = "0" + RecSize: LOOP
  RCC = RCC + OIR
  'Start Address
  Address = Right(Hex(FRA), 4)
  DO WHILE LEN(Address) < 4: Address = "0" + Address: LOOP
  RCC = RCC + (FRA AND 255) + (FRA AND 65280) / 256
  'Type
  RecType = "00"
  'Checksum
  RecCheck = Right(Hex(256 - (RCC AND 255)), 2)
  DO WHILE LEN(RecCheck) < 2: RecCheck = "0" + RecCheck: LOOP

  'Add high address?
  IF ((FRA \ 65536) AND 65535) <> CHA THEN
    CHA = ((FRA \ 65536) AND 65535)
    ':02 0000 04 0030 CA
    HexAddress = Right(HEX(CHA), 4)
    DO WHILE LEN(HexAddress) < 4: HexAddress = "0" + HexAddress: LOOP
    If ModePIC Then
      HexContents = HEX(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 6) AND 255))
    ElseIf ModeAVR Then
      HexContents = HEX(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 4) AND 255))
    End If
    DO WHILE LEN(HexContents) < 2: HexContents = "0" + HexContents: LOOP
    If ModePIC Then
      CurrHexRecord = LinkedListInsert(CurrHexRecord, ":02000004" + HexAddress + HexContents)
    ElseIf ModeAVR Then
      CurrHexRecord = LinkedListInsert(CurrHexRecord, ":02000002" + HexAddress + HexContents)
    End IF
  END IF

  CurrHexRecord = LinkedListInsert(CurrHexRecord, ":" + RecSize + Address + RecType + HRTemp + RecCheck)

  'Write hex file
  'HFI = OFI
  'Replace HFI, ".asm", ".hex"
  HFI = ReplaceToolVariables("%filename%", "hex")
  OPEN HFI FOR OUTPUT AS #1
  If ModePIC Then
    PRINT #1, ":020000040000FA"
  ElseIf ModeAVR Then
    PRINT #1, ":020000020000FC"
  End If
  CurrHexRecord = HexRecords->Next
  Do While CurrHexRecord <> 0
    PRINT #1, CurrHexRecord->Value
    CurrHexRecord = CurrHexRecord->Next
  Loop
  PRINT #1, ":00000001FF"
  If ModePIC and HexAppend <> "" then
    PRINT #1, ";HEX file generated by Great Cow BASIC"
  End if
  Close

END SUB

Sub BuildAsmSymbolTable
  'Scan through program, build symbol table

  Dim As String Temp, RomData, NewData, NDTemp
  Dim As Integer PD, CSB, RP1, CurrentLocation, DT, OrgLocation, SS, CS
  Dim As Integer DataBlockSize, DataSize, DWC, RSC, DWIC, T, FoundDirective
  Dim As AsmCommand Pointer CurrCmd

  Dim As LinkedListElement Pointer TempList, CurrItem, AsmLine
  Dim As SysVarType Pointer TempVar

  ASMSymbols = HashMapCreate

  If ModePIC Then
    AddAsmSymbol("W", "0")
    AddAsmSymbol("F", "1")
    AddAsmSymbol("A", "0")
    AddAsmSymbol("B", "1")
    AddAsmSymbol("ACCESS", "0")
    AddAsmSymbol("BANKED", "1")

  ElseIf ModeAVR Then
    'Need to do X after X+ and -X
    AddAsmSymbol("X+", "29")
    AddAsmSymbol("-X", "30")
    AddAsmSymbol("X", "28")

    AddAsmSymbol("Y+", "25")
    AddAsmSymbol("-Y", "26")
    AddAsmSymbol("Y", "8")

    AddAsmSymbol("rZ", "0")
    AddAsmSymbol("Z+", "17")
    AddAsmSymbol("-Z", "18")

    Temp = MemRanges(MRC)
    Temp = "&H" + Trim(Mid(Temp, InStr(Temp, ":") + 1))
    HashMapSet(ASMSymbols, "RAMEND", STR(Val(Temp) - 1))
  End If

  'SFRs
  TempList = HashMapToList(SysVars)
  CurrItem = TempList->Next
  Do While CurrItem <> 0
    TempVar = CurrItem->MetaData
    AddAsmSymbol(Trim(UCase(TempVar->Name)), Trim(Str(TempVar->Location)))

    CurrItem = CurrItem->Next
  Loop
  LinkedListDelete(TempList, 0)

  'SFR bits
  TempList = HashMapToList(SysVarBits)
  CurrItem = TempList->Next
  Do While CurrItem <> 0
    TempVar = CurrItem->MetaData
    AddAsmSymbol(Trim(UCase(TempVar->Name)), Trim(Str(TempVar->Location)))

    CurrItem = CurrItem->Next
  Loop
  LinkedListDelete(TempList, 0)

  'User variables
  TempList = HashMapToList(FinalVarList)
  CurrItem = TempList->Next
  Do While CurrItem <> 0
    With *CPtr(VariableListElement Pointer, CurrItem->MetaData)
       AddAsmSymbol(UCase(.Name), .Value)  'ERV reverted 24012021
    End With
    CurrItem = CurrItem->Next
  Loop

  'User register variables (AVR only)
  If ModeAVR Then
    FOR PD = 1 to FRLC
      With FinalRegList(PD)
        AddAsmSymbol(UCase(.Name), .Value)
      End With
    Next
  End If
  'Alias variables
  FOR PD = 1 to FALC
    With FinalAliasList(PD)
      If Not HasSFR(.Value) Then
        AddAsmSymbol(UCase(.Name), .Value)
      End If
    End With
  NEXT

  'Symbols from compiler
  For PD = 1 To ToAsmSymbols
    AddAsmSymbol(ToAsmSymbol(PD, 1), ToAsmSymbol(PD, 2))
  Next

  'Labels
  'Check for RP1 bit
  If ModePIC Then
    'Should banksel set 1 or 2 STATUS bits?
    If ChipFamily = 15 Or ChipFamily = 16 Then
      RP1 = 0
    Else
      RP1 = HasSFRBit("RP1")
      If MemSize <= 256 Then RP1 = 0 'If 256 or less addresses, there is no RP1
    End If
  End If
  CurrentLocation = 0
  AsmLine = AsmProg->Next
  Do While AsmLine <> 0
    CurrCmd = IsASM(AsmLine->Value)
    FoundDirective = 0

    'Blank line
    If AsmLine->Value = "" Then

    'Assembly instruction
    ElseIf CurrCmd <> 0 THEN
      DT = CurrCmd->Words
      If ChipFamily = 16 THEN DT = DT * 2
      AsmLine->Value = Str(CurrentLocation) + ":" + AsmLine->Value
      CurrentLocation = CurrentLocation + DT

    'Not instruction
    Else
      'ORG directive
      IF Left(AsmLine->Value, 4) = "ORG " Then
        OrgLocation = MakeDec(Mid(AsmLine->Value, 4))
        If OrgLocation >= CurrentLocation Then
          CurrentLocation = OrgLocation
        Else
          Temp = Message("BadORG")
          Replace Temp, "%loc%", Str(OrgLocation)
          LogError("GCASM:" + Temp, "")
        End If
        FoundDirective = -1
        AsmLine->Value = ""

      'BANKSEL/PAGESEL directives
      ElseIF Left(AsmLine->Value, 8) = "BANKSEL " THEN
        AsmLine->Value = Str(CurrentLocation) + ":" + AsmLine->Value
        If ChipFamily = 16 Then
          CurrentLocation += 2
        ElseIf ChipFamily = 15 Then
          CurrentLocation += 1
        Else
          IF RP1 Then
            CurrentLocation += 2
          Else
            CurrentLocation += 1
          End If
        End If
        FoundDirective = -1

      ElseIF Left(AsmLine->Value, 9) = "BANKISEL " THEN
        AsmLine->Value = Str(CurrentLocation) + ":" + AsmLine->Value
        CurrentLocation += 1
        FoundDirective = -1

      ElseIF Left(AsmLine->Value, 8) = "PAGESEL " THEN
        If ChipFamily = 15 Then
          AsmLine->Value = Str(CurrentLocation) + ":" + AsmLine->Value
          If PCUpper > 0 Then CurrentLocation += 1
          FoundDirective = -1

        Else
          AsmLine->Value = Str(CurrentLocation) + ":" + AsmLine->Value
          CurrentLocation += PCUpper
          FoundDirective = -1

        End If

      'Data embedding instructions
      ElseIf Left(AsmLine->Value, 3) = "DW " OR Left(AsmLine->Value, 3) = "DB " Or Left(AsmLine->Value, 3) = "DE " THEN

        DataBlockSize = 1
        If (ChipFamily = 14 Or ChipFamily = 15) And Left(AsmLine->Value, 3) = "DE " Then DataBlockSize = 2
        ROMData = Trim(Mid(AsmLine->Value, 4))
        DataSize = 0
        DWC = 0
        RSC = 0
        DO WHILE INSTR(ROMData, Chr(34)) <> 0
          Temp = Mid(ROMData, INSTR(ROMData, Chr(34)) + 1)
          Temp = Chr(34) + Left(Temp, INSTR(Temp, Chr(34)))
          RSC = RSC + 1
          Replace ROMData, Temp, "%S" + Str(RSC) + "S"
          Temp = Mid(Temp, 2)
          Temp = Left(Temp, LEN(Temp) - 1)
          TempData(RSC) = Temp
        LOOP
        DWIC = 0
        DO WHILE INSTR(ROMData, ",") <> 0
          DWIC += 1: CheckTemp(DWIC) = Trim(Left(ROMData, INSTR(ROMData, ",") - 1))
          ROMData = Trim(Mid(ROMData, INSTR(ROMDATA, ",") + 1))
        LOOP
        IF ROMData <> "" Then
          DWIC += 1: CheckTemp(DWIC) = ROMData
        End If
        'Pad with 0
        CheckTemp(DWIC + 1) = "0"

        NewData = ""

        SS = 0
        Do While SS < DWIC
          SS += 1

          Temp = CheckTemp(SS)
          IF Temp <> "" THEN
            'Print Temp,

            'Temp contains integer
            IF IsConst(Temp) Then
              'Print "Int", MakeDec(Temp)
              If ModePIC Then
                DataSize += 2
              ElseIf ModeAVR Then
                DataSize += 1
              End If
              NDTemp = Hex(MakeDec(Temp))
              'Combine with next byte?
              If DataBlockSize = 1 Then
                IF LEN(NDTemp) = 1 THEN NDTemp = "0" + NDTemp
                IF LEN(NDTemp) <= 2 THEN
                  'CS = 0: IF MakeDec(CheckTemp(SS + 1)) <> 0 THEN CS = 1
                  CS = IsConst(CheckTemp(SS + 1))

                  IF CS Then
                    NDTemp = HEX(MakeDec(CheckTemp(SS + 1))) + NDTemp
                    SS += 1
                  Else
                    NDTemp = "00" + NDTemp
                  END IF
                END IF
                IF LEN(NDTemp) = 3 THEN NDTemp = "0" + NDTemp
              ElseIf DataBlockSize = 2 Then
                Select Case Len(NDTemp)
                  Case 1: NDTemp = "000" + NDTemp
                  Case 2: NDTemp = "00" + NDTemp
                  Case 3: NDTemp = "0" + NDTemp
                End Select
              End If
              'Print "Output as "; NDTemp
              NewData = NewData + "," + NDTemp

            'Temp contains string
            Else
              IF INSTR(Temp, "%S") <> 0 THEN
                T = VAL(Mid(Temp, INSTR(Temp, "%S") + 2))
                Temp = TempData(T)
              END If
              'Print "String:"; Temp

              DataSize += LEN(Temp)
              IF (LEN(Temp) / 2) <> INT(LEN(Temp) / 2) Then DataSize += 1: Temp = Temp + Chr(0)
              FOR CS = 1 to LEN(Temp) Step 2
                NDTemp = HEX(ASC(Mid(Temp, CS + 1, 1))) + HEX(ASC(Mid(Temp, CS, 1)))
                IF LEN(NDTemp) = 3 THEN NDTemp = "0" + NDTemp
                NewData = NewData + "," + NDTemp
              NEXT
            END If

          END IF
        Loop

        AsmLine->Value = Str(CurrentLocation) + ":RAW " + Mid(NewData, 2)
        CurrentLocation += DataSize
        FoundDirective = -1
      END IF

      'EQU directive
      IF INSTR(AsmLine->Value, " EQU ") <> 0 THEN FoundDirective = -1: AsmLine->Value = ""

'ERV'PSECT at PAGE boundary
IF INSTR(AsmLine->Value, "__PROGMEMPAGE") <> 0 THEN FoundDirective = -1: AsmLine->Value = ""

      'If nothing else, then line is label
      If CurrCmd = 0 And FoundDirective = 0 THEN
        IF Right(AsmLine->Value, 1) = ":" Then AsmLine->Value = Left(AsmLine->Value, LEN(AsmLine->Value) - 1)
        AddAsmSymbol(UCase(AsmLine->Value), Str(CurrentLocation))
        IF INSTR(AsmLine->Value, " ") <> 0 THEN
          Temp = Message("BadSymbol")
          Replace Temp, "%symbol%", AsmLine->Value
          LogError Temp
        END IF
      END IF
    END If

    AsmLine = AsmLine->Next
  Loop

End Sub

Function GetConfigBaseLoc As Integer

  Select Case ChipFamily
    Case 12: Return &HFFF
    Case 14: Return &H2007
    Case 15: Return &H8007
    Case 16
      Dim As ConstMeta Pointer ConfigBaseLocConst
      ConfigBaseLocConst = HashMapGet(Constants, "CHIPCONFIGBASELOC")
      If ConfigBaseLocConst <> 0 Then
        Return MakeDec(ConfigBaseLocConst->Value)
      Else
        Return 3145728
      End If
    Case Else
      Return -1
  End Select
End Function

Function IsASM (DataSource As String, ParamCount As Integer = -1) As AsmCommand Pointer
  'Returns 0 if instruction is not assembly
  'Returns instruction if it is asm
  'Instructions not valid on current chip variant will be treated as not existing

  Dim As String Temp, Params
  Dim As Integer CurrChar
  Dim As AsmCommand Pointer FoundCmd, SearchCmd

  Temp = Trim(UCase(DataSource))
  If INSTR(Temp, " ") <> 0 Then
    If ParamCount = -1 Then
      ParamCount = 1
      For CurrChar = 1 To Len(Temp)
        If Mid(Temp, CurrChar, 1) = "," Then ParamCount += 1
      Next
    End If
    Temp = Trim(Left(Temp, INSTR(Temp, " ") - 1))
  End If
  If ParamCount = -1 Then ParamCount = 0

  FoundCmd = HashMapGet(ASMCommands, Temp)
  If FoundCmd <> 0 AndAlso (FoundCmd->Params <> ParamCount Or Not IsForVariant(FoundCmd)) Then
    SearchCmd = FoundCmd->Alternative
    Do While SearchCmd <> 0
      If SearchCmd->Params = ParamCount And IsForVariant(SearchCmd) Then
        Return SearchCmd
      End If
      SearchCmd = SearchCmd->Alternative
    Loop
  End If

  If Not IsForVariant(FoundCmd) Then
    Return 0
  End If

  Return FoundCmd
END FUNCTION

Function IsASMConst (DataSource As String) As Integer

  If IsConst(DataSource) Then Return -1
  IF Left(Trim(DataSource), 1) = "@" Then Return -1
  Return 0

End Function

Function IsHexConfigValid(HexFile As String, ConfigSettings As String) As Integer

  Dim As UInteger ConfWordVal(ConfWords), CurrAddress, LineLen, LineAddr, LineType
  Dim As Integer f, ConfigWordLoc, CurrWord, CurrConfConst, BitsVal, DesiredVal
  Dim As String ReadLine, LineData, SettingName, SettingValue
  Dim As LinkedListElement Pointer SettingsList, CurrSetting

  'Not used for AVR, return true
  If ModeAVR Then Return -1

  'Guess config word start location
  ConfigWordLoc = GetConfigBaseLoc

  'Clear words
  For CurrWord = 0 To ConfWords
    ConfWordVal(CurrWord) = &HFFFF
  Next

  'Read config words from hex file
  f = FreeFile
  CurrAddress = 0
  Open HexFile For Input As #f
  Do While Not Eof(f)
    Line Input #f, ReadLine
    If Left(ReadLine, 1) = ":" Then
      LineLen = Val("&H" + Mid(ReadLine, 2, 2))
      LineAddr = Val("&H" + Mid(ReadLine, 4, 4))
      LineType = Val("&H" + Mid(ReadLine, 8, 2))

      Select Case LineType
        Case 0:
          LineAddr += CurrAddress
          If ChipFamily <> 16 Then
            LineAddr = LineAddr / 2
          End If
          If LineAddr >= ConfigWordLoc - 16 And LineAddr <= ConfigWordLoc + ConfWords + 16 Then
            LineData = Mid(ReadLine, 10)
            LineData = Left(LineData, Len(LineData) - 2)
            CurrWord = LineAddr - ConfigWordLoc
            Do While LineData <> ""
              If ChipFamily = 16 Then
                If CurrWord >= 0 And CurrWord <= ConfWords Then
                  ConfWordVal(CurrWord) = Val("&H" + Mid(LineData, 1, 2))
                End If
                If (CurrWord  + 1) >= 0 And (CurrWord + 1) <= ConfWords Then
                  ConfWordVal(CurrWord + 1) = Val("&H" + Mid(LineData, 3, 2))
                End If
                LineData = Mid(LineData, 5)
                CurrWord += 2
              Else
                If CurrWord >= 0 And CurrWord <= ConfWords Then
                  ConfWordVal(CurrWord) = Val("&H" + Mid(LineData, 3, 2) + Mid(LineData, 1, 2))
                End If
                LineData = Mid(LineData, 5)
                CurrWord += 1
              End If
            Loop
          End If

        Case 1: Exit Do
        Case 4: CurrAddress = Val("&H" + Mid(ReadLine, 10, 4)) Shl 16

      End Select

    End If

  Loop
  Close #f

  'For CurrWord = 0 To ConfWords - 1
  ' Print CurrWord, Hex(ConfigWordLoc + CurrWord), Hex(ConfWordVal(CurrWord))
  'Next

  'Config words known, check required settings
  SettingsList = GetElements(ConfigSettings, ",")
  CurrSetting = SettingsList->Next
  Do While CurrSetting <> 0
    'Print "Checking", CurrSetting->Value
    If InStr(CurrSetting->Value, "=") <> 0 Then
      SettingName = Trim(Left(CurrSetting->Value, InStr(CurrSetting->Value, "=") - 1))
      SettingValue = Trim(Mid(CurrSetting->Value, InStr(CurrSetting->Value, "=") + 1))

      BitsVal = -1
      DesiredVal = -1
      For CurrConfConst = 1 To COC
        With ConfigOps(CurrConfConst)
          'Print .Op, .Loc, .Val
          If ConfigNameMatch(.Op, SettingName) Then
            If BitsVal = -1 Then
              BitsVal = .Val
            Else
              BitsVal = BitsVal And .Val
            End If
            If ConfigValueMatch(.Op, SettingValue) Then
              DesiredVal = CurrConfConst
            End If
          End If
        End With
      Next
      'Ensure that bits masked by BitsVal match
      If DesiredVal <> -1 Then
        With ConfigOps(DesiredVal)
          If (ConfWordVal(.Loc - 1) And (Not BitsVal)) <> (.Val And (Not BitsVal)) Then
            'Print "Mismatch found"
            'Print .Op, Hex(.Val), Hex(BitsVal), Hex(ConfWordVal(.Loc - 1))
            'Print (ConfWordVal(.Loc - 1) And (Not BitsVal)), (.Val And (Not BitsVal))
            Return 0
          End If
        End With
      End If
    End If
    CurrSetting = CurrSetting->Next
  Loop

  'No issues found, assume settings are valid
  Return -1
End Function

Function IsForVariant(FoundCmd As AsmCommand Pointer) As Integer
  Dim SearchVariants As Integer

  'If no command, return false
  If FoundCmd = 0 Then Return 0

  'If for all variants, return true
  If FoundCmd->FamilyVariants = 0 Then Return -1

  'Search variants for current variant
  For SearchVariants = 1 To FoundCmd->FamilyVariants
    If FoundCmd->FamilyVariant(SearchVariants) = ChipFamilyVariant Then Return -1
  Next
  Return 0
End Function
