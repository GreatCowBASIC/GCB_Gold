' GCBASIC - A BASIC Compiler for microcontrollers
' Copyright (C) 2006 - 2024 Hugh Considine, Evan R. Venn and the GCBASIC team
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


'Array sizes
#Define MAX_PROG_PAGES 20
'Number of buckets to use in hash maps - increasing makes operations faster but uses more RAM
#Define HASH_MAP_BUCKETS 512

#Define RESERVED_WORDS 1024

#Define MAXTABLEITEMS 65536

Type PICASInc
  Value as String
  NumVal as Integer
End Type

Type PICASCfg
  Value as String
  State as String
  Config as String
End Type

Type PICASPatch
  Source as String
  Destination as String
End Type

Dim Shared ReverseIncFileLookup ( 5000) as PICASInc
Dim Shared ReverseCfgFileLookup ( 10000) as PICASCfg
Dim Shared Patches(100) as PICASPatch

'31k types
Type _31kCfg
  'Chip32Support_OSC,Chip32Support_Register, Chip32Support_ANDMask,Chip32Support_ORValue
  State as Integer
  OSCType as String
  OSCRegister as String
  Value as Integer
End Type
Dim Shared _31kSupported as _31kCfg

'Type for sections of code returned from subs
Type LinkedListElement
  Value As String
  NumVal As Integer
  MetaData As Any Pointer
  Prev As LinkedListElement Pointer
  Next As LinkedListElement Pointer
End Type

Type HashMap
  Buckets(HASH_MAP_BUCKETS) As LinkedListElement Pointer
End Type

'Meta data for program line
Type ProgLineMeta
  RequiredBank As Integer 'Bank chip needs for this icon, -1 if N/A
  VarInBank As String 'Variable in the bank that is to be changed to

  IsAutoPageSel As Integer 'Set to -1 if line is automatically added pagesel

  'List of commands that lead to this one
  PrevCommands As LinkedListElement Pointer
  'List of commands that this one leads to
  NextCommands As LinkedListElement Pointer

  AsmCommand As Integer 'Index of asm instruction on line. -1 if none.
  LineSize As Integer 'Size of line in words.

  isLabel as Integer
  isPICAS as Integer

  OrgLine as String

  RawConfig as string

  Conditional as Integer

End Type

Type ConstMeta
  Value As String
  Startup As String
End Type

Type OriginType
  FileNo As Integer
  LineNo As Integer
  SubNo As Integer
End Type

Type SourceFileType
  FileName As String
  RequiresConversion As Integer = 0

  InitSub As String
  InitSubUsed As Integer
  InitSubPriority As Integer = 100
  InitSubFound As Integer = 0

  IncludeOrigin As String

  OptionExplicit As Integer

  UserInclude as Integer = 0
  SystemInclude as Integer = 0
  Ignore as Integer = 0

  RequiredModules As LinkedListElement Pointer
End Type

Type VariableType
  Name As String
  Type As String
  Size As Integer
  Pointer As String
  Alias As String
  Origin As String

  FixedLocation As Integer
  Location As Integer
  BitVarLocation As String
  IsSharedBank As Integer
  ExplicitDeclaration As Integer

  FixedSize As Integer
  Used As Integer

  NeedsSequentialLoc As Integer ' Set if individual bits accessed, and must have bytes in sequential order
  UndeclaredError As Integer ' Set if option explicit used and this variable wasn't declared

  AllocOnly as Integer  'Set to -1 if this variable is an ALLOC (allocation memory only)

End Type

Type VariableListElement
  Name As String
  Value As String
  IsArray As Integer
End Type

Type IntData
  Vector As String
  VectorLoc As Integer
  EventName As String
  EnableBit As String
  FlagBit As String
  Handler As String
End Type

Type SubParam
  Name As String
  Type As String
  Dir As Integer '1 - in, 2 - out, 3 - in/out
  Default As String
End Type

Type SubType
  Name As String
  SourceFile As Integer
  Origin As String

  IsMacro As Integer
  IsFunction As Integer
  IsData As Integer
  ReturnType As String
  Params(50) As SubParam
  ParamCount As Integer
  Overloaded As Integer

  'Variables in subroutine
  Variables As HashMap
  VarsRead As Integer

  'Flags
  Required As Integer
  Compiled As Integer
  NoReturn As Integer
  HasFinalGoto As Integer

  'If ends in goto, record sub jumped to
  FinalGotoDest As String

  'If overloaded function, alias for return value
  ReturnAlias As String

  'Call tree
  CallList As LinkedListElement Pointer 'Outgoing calls
  CallerList As LinkedListElement Pointer 'Incoming calls

  'Assembly bit variable used to save IntOn/IntOff state
  IntStateSaveVar As String

  'Original BASIC size (for compilation report)
  OriginalLOC As Integer

  'Program memory page selection hints
  HexSize As Integer
  DestPage As Integer
  FirstPage As Integer
  LocationSet As Integer
  MaxHexSize As Integer 'Highest recorded size
  CallsFromPage(MAX_PROG_PAGES) As Integer 'Record calls from each page

  'Temporary variables used (replace if they overlap with variables in a calling subroutine)
  TemporaryVars As LinkedListElement Pointer

  'Sub code
  CodeStart As LinkedListElement Pointer

End Type

Type DataTableType
  Name As String
  Type As String
  Origin As String
  StoreLoc As Integer '0 = Flash, 1 = Data EEPROM
  Used As Integer
  SourceFile As String 'Used if table loaded from file

  RawItems As LinkedListElement Pointer
  CurrItem As LinkedListElement Pointer

  Items As Integer
  Item(MAXTABLEITEMS) As LongInt
  IsEEPromData As Integer
  IsData As Integer
  FixedLoc As Integer
End Type

Type ProgString
  Value As String
  Used As Integer
End Type

Type SubCallType
  Called As SubType Pointer
  Caller As SubType Pointer
  CalledID As Integer
  Origin As String
  Param(50, 2) As String
  Params As Integer
  CallSig As String
End Type

Type SysVarType
  Name As String
  Location As Integer
  Parent As String
  AVRAlias As Integer
End Type

Type AsmCommand
  Syntax As String
  Words As Integer
  Word(4) As String

  Cmd As String
  Params As Integer
  Param(25) As String

  FamilyVariant(10) As Integer
  FamilyVariants As Integer = 0

  Alternative As AsmCommand Pointer 'Need this to allow for multiple commands with same name
End Type

Type ConfigOp
  Op As String
  Loc As Integer
  Val As Integer
End Type

Type ConfigSetting
  Name As String 'Name of config setting
  Setting As LinkedListElement Pointer 'Points to current option
  Options As LinkedListElement Pointer 'List of potential settings
End Type

Type CalcVar
  MaxType As String
  Status As String
  CurrentType As String
  CurrentValue As String
  High As Integer
End Type

Type ExternalTool
  Name As String
  Type As String
  DispName As String
  Cmd As String
  Params As String
  WorkingDir As String
  ExtraParam(5, 2) As String
  ExtraParams As Integer

  'Conditions for use
  UseIf As String

  'Allow programmers to require config or option settings
  ProgConfig As String
  ProgOptions As String
End Type

Type FileConverterType
  Name As String
  Desc As String
  InFormat(10) As String
  InFormats As Integer
  OutFormat As String
  ExeName As String
  Params As String
  DeleteTarget As Integer
End Type

'Type to store generated code
Type CodeSection
  CodeList As LinkedListElement Pointer
  CodeEnd As LinkedListElement Pointer
End Type

'Type to store a program memory page
Type ProgMemPageType
  StartLoc As Integer
  EndLoc As Integer
  CodeSize As Integer
  MaxSize As Integer
  PageSize as Integer
End Type

'Type to store a RAM bank
Type DataMemBankType
  StartLoc As Integer 'Start address of bank
  EndLoc As Integer 'End address of bank
  DataSize As Integer 'Amount of bank currently used
End Type

'Type to store info on which page is best
Type AllocationOrderType
  Page As Integer
  Calls As Integer
End Type

'Type to store usage info on an I/O pin
Type PinDirType
  'Name of the pin stored in LinkedListElement
  'This type used as metadata
  'Record pin reads and writes
  WrittenTo As Integer
  ReadFrom As Integer
  'Set to -1 if whole port set, but direction may not be known
  WholePort As Integer
  'Record pin direction settings
  SetOut As Integer
  SetIn As Integer
  'Allowed directions (from chip data)
  AllowedDirections As String
  'If Dir command sets entire port to constant, store here
  'Set to -1 if set by a variable
  WholePortDir As Integer
End Type

'Subs in this file
declare SUB Add18FBanks(CompSub As SubType Pointer)
declare SUB AddBankCommands(CompSub As SubType Pointer)
declare SUB AddDataBlocks ( ByRef CurrLine As LinkedListElement Pointer, ByRef CurrPage As Integer, ByRef CurrPagePos As Integer, ByRef DataBlockCount as Integer, NonChipFamily16DataBlocksNotAdded As Integer )
declare Sub AddMainEndCode
declare Sub AddMainInitCode
declare Sub AddPageCommands(CompSub As SubType Pointer)
declare Sub AddInterruptCode
declare SUB AddSysVarBits (CompSub As SubType Pointer)
declare SUB BuildMemoryMap
declare SUB CalcConfig
declare Sub CalcOps (OutList As CodeSection Pointer, SUM As String, AV As String, Ops As String, OriginIn As String, NeverLast As Integer)
declare Function CalcLineSize(CurrLine As String, ThisSubPage As Integer, CallPos As AsmCommand Pointer = 0, GotoPos As AsmCommand Pointer = 0) As Integer
declare Sub CalcSubSize(CurrSub As SubType Pointer)
declare FUNCTION CastOrder (InType As String) As Integer
declare Sub CheckConstName (ConstName As String, Origin As String)
declare Sub CheckClockSpeed
declare Sub CompileProgram
declare Sub CompileSubroutine(CompSub As SubType Pointer)
declare Sub CompileCalc (SUM As String, AV As String, Origin As String, ByRef OutList As CodeSection Pointer = 0, NeverLast As Integer = 0)
declare FUNCTION CompileCalcAdd(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
declare FUNCTION CompileCalcCondition(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
declare FUNCTION CompileCalcLogic(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
declare FUNCTION CompileCalcMult(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
declare Function CompileCalcUnary(CodeSection As CodeSection Pointer, Act As String, V2 As String, Origin As String, AnswerIn As String) As String
declare Function CompileConditions (Condition As String, IfTrue As String, Origin As String, CompSub As SubType Pointer = 0) As LinkedListElement Pointer
declare Sub CompileDim (CurrSub As SubType Pointer)
declare SUB CompileDir (CompSub As SubType Pointer)
declare SUB CompileDo (CompSub As SubType Pointer)
declare SUB CompileExitSub (CompSub As SubType Pointer)
declare SUB CompileFor (CompSub As SubType Pointer)
declare Sub CompileGoto (CompSub As SubType Pointer)
declare SUB CompileIF (CompSub As SubType Pointer)
declare Sub CompileIntOnOff (CompSub As SubType Pointer)
declare SUB CompileOn (CompSub As SubType Pointer)
declare SUB CompilePot (CompSub As SubType Pointer)
declare SUB CompileReadTable (CompSub As SubType Pointer)
declare SUB CompileRepeat (CompSub As SubType Pointer)
declare Sub CompileReturn (CompSub As SubType Pointer)
declare SUB CompileRotate (CompSub As SubType Pointer)
declare SUB CompileSelect (CompSub As SubType Pointer)
declare SUB CompileSet (CompSub As SubType Pointer)
declare Function CompileString (InLine As String, Origin As String) As LinkedListElement Pointer
declare Function CompileSubCall (InCall As SubCallType Pointer) As LinkedListElement Pointer
declare Sub CompileSubCalls (CompSub As SubType Pointer)
declare SUB CompileTables
declare SUB CompileVars (CompSub As SubType Pointer)
declare Function CompileVarSet (SourceIn As String, Dest As String, Origin As String, InvertBitCopy As Integer = 0) As LinkedListElement Pointer
declare SUB CompileWait (CompSub As SubType Pointer)
declare Function CompileWholeArray (InLine As String, Origin As String) As LinkedListElement Pointer
declare Function ConfigNameMatch(ConfigIn As String, ConfigNameIn As String) As Integer
declare Function ConfigValueMatch(ConfigIn As String, ConfigValueIn As String, MatchAny As Integer = 0) As Integer
declare Sub CreateCallTree
declare Sub CreateReservedWordsList
declare Sub DisplayProgram
declare Sub DisplayCallTree
declare Sub ExtAssembler
declare Sub PICASAssembler
declare Sub ExtractParameters(ByRef NewSubCall As SubCallType, CalledSubName As String, CallParams As String, Origin As String)
declare Sub FinalOptimise
declare Sub FindAssembly (CompSub As SubType Pointer)
declare Function FindPotentialBanks(CurrLine As LinkedListElement Pointer, OutList As LinkedListElement Pointer = 0, CheckedLines As LinkedListElement Pointer = 0) As LinkedListElement Pointer
declare Sub FindUncompiledLines
declare Function FixBit (InBit As String, Origin As String) As String
declare SUB FixFunctions (CompSub As SubType Pointer)
declare Sub FixPointerOps (CompSub As SubType Pointer)
declare Sub FixSinglePinSet
declare Sub FixTemporaryVariables
declare Sub FreeCalcVar (VarName As String)
declare Function GenerateArrayPointerSet(DestVar As String, DestPtr As Integer, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer
declare Function GenerateAutoPinDir As LinkedListElement Pointer
declare Function GenerateBitSet(BitNameIn As String, NewStatus As String, Origin As String, CurrSub As SubType Pointer = 0, SetStatus As Integer = -1) As LinkedListElement Pointer
declare Function GenerateExactDelay(ByVal Cycles As Integer) As LinkedListElement Pointer
declare Function GenerateMultiSet(SourceData As String, DestVar As String, Origin As String, CurrSub As SubType Pointer, CanSkip As Integer) As LinkedListElement Pointer
declare Function GenerateVectorCode As LinkedListElement Pointer
declare Function GetCalcType(VT1 As String, Act As String, VT2 As String, AnswerType As String) As String
declare Function GetCalcVar (VarTypeIn As String) As String
declare Function GetCalledSubs(CurrSub As SubType Pointer, ExistingList As LinkedListElement Pointer = 0, FindCallers As Integer = 0) As LinkedListElement Pointer
declare FUNCTION GetDestSub(Origin As String) As Integer
declare Sub GetEqConfig
declare Function GetLabelList(CompSub As SubType Pointer) As LinkedListElement Pointer
declare Function GetLinearLoc(Location As Integer) As Integer
declare Function GetNonLinearLoc(Location As Integer) As Integer
declare Function GetMetaData(CurrLine As LinkedListElement Pointer) As ProgLineMeta Pointer
declare Function GetPinDirection(PinNameIn As String) As PinDirType Pointer
declare Function GetRealIOName(InName As String) As String
declare Function GetRegisterLoc(RegName As String) As Integer
declare Function GetSysVar(VarName As String) As SysVarType Pointer
declare Function GetSysVarName(Address as Integer) As String
declare Function GetSysVarAliasName(Lookup as String) As String
declare FUNCTION GetSub(Origin As String) As String
declare Function GetSubFullName(SubIndex As Integer) As String
declare FUNCTION GetSubID(Origin As String) As Integer
declare Function GetSubSig(CurrentSub As SubType) As String
declare Function GetSubParam (ParamIn As String, ForceIn As Integer) As SubParam
declare Function GetTool(ToolName As String) As ExternalTool Pointer
declare SUB InitCompiler
declare FUNCTION IsArray (VarName As String, CurrSub As SubType Pointer) As Integer
declare Function IsNonBanked(Location As Integer) As Integer
declare Function IsInAccessBank(VarNameIn As String) As Integer
declare Function IsIOPinName(PinName As String) As Integer
declare Function IsIOReg (RegNameIn As String) As Integer
declare Function IsIORegDX (RegNameIn As String) As Integer
declare Function IsLowIOReg (RegNameIn As String) As Integer
declare Function IsLowRegister(VarName As String) As Integer
declare Function IsRegister (VarName As String) As Integer
declare FUNCTION IsString (InData As String, CurrSub As SubType Pointer) As Integer
declare Function IsUnaryOp (InData As String) As Integer
declare Sub LoadConverters
declare FUNCTION LocationOfSub (SubNameIn As String, SubSigIn As String, Origin As String = "", AllowVague As Integer = 0) As Integer
declare Sub LogError(InMessage As String, Origin As String = "")
declare Sub LogOutputMessage(InMessage As String)
declare Sub LogWarning(InMessage As String, Origin As String = "")
declare Sub MergeSubroutines
declare FUNCTION Message (InData As String) As String
declare Function ModeAVR As Integer
declare Function ModePIC As Integer
declare Function ModeZ8 As Integer
declare Function NewCodeSection As CodeSection Pointer
declare Function NewProgLineMeta As ProgLineMeta Pointer
declare Function NewSubroutine(SubName As String) As SubType Pointer
declare Sub OptimiseCalls
declare SUB OptimiseIF(CompSub As SubType Pointer = 0)
declare Sub OptimiseIncrement(CompSub As SubType Pointer)
declare Sub PreparePageData
declare Sub PrepareProgrammer
declare SUB ProcessArrays (CompSub As SubType Pointer)
declare SUB ProcessWords ()
declare Function PutInRegister(ByRef OutList As LinkedListElement Pointer, SourceValue As String, RegType As String, Origin As String) As String
declare SUB ReadChipData
declare SUB ReadPICASChipData
declare Sub ReadOptions(OptionsIn As String)
declare Sub RecordSubCall(CompSub As SubType Pointer, CalledSub As SubType Pointer)
declare Function ReplaceFnNames(InName As String) As String
declare Function RequestSub(Requester As SubType Pointer, SubNameIn As String, SubSigIn As String = "") As Integer
declare Sub RetrySubRequests
declare Sub SetCalcTempType (CalcVar As String, NewType As String)
declare Function SetStringPointers (V1 As String, V2 As String, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer
declare SUB ShowBlock (BlockIn As String)
declare SUB SplitLines (CompSub As SubType Pointer)
declare Function TempRemove(Removed As String) As String
declare Sub TidyProgram
declare Sub TidySubroutine(CompSub As SubType Pointer)
declare Function TranslateFile(InFile As String) As String
declare FUNCTION TypeOfVar (VarName As String, CurrSub As SubType Pointer) As String
declare FUNCTION TypeOfValue (ValueNameIn As String, CurrentSub As SubType Pointer, SingCharString As Integer = 0) As String
declare Sub UpdateOutgoingCalls (CompSub As SubType Pointer)
declare Sub UpdateSubMap
declare Sub UpgradeCalcVar (VarName As String, VarType As String)
declare Sub ValueChanged(VarName As String, VarValue As String)
declare Sub ValidateParameterIsValid ( inline as String, FunctionParam as String, Origin as String )
declare FUNCTION VarAddress (ArrayNameIn As String, CurrSub As SubType Pointer) As VariableType Pointer
declare Sub WriteAssembly
declare Sub WriteCompilationReport
declare SUB WriteErrorLog

'Subs in assembly.bi
declare Sub AddAsmSymbol(SymName As String, SymValue As String)
declare Sub AsmOptimiser (CompSub As SubType Pointer)
declare FUNCTION AsmTidy (DataSource As String, StoredGCASM as integer = -1 ) As String
declare Function ConfigTidy (DataSource As String ) As String
declare SUB AssembleProgram
declare Sub BuildAsmSymbolTable
declare Function GetConfigBaseLoc As Integer
declare FUNCTION IsASM (DataSource As String, ParamCount As Integer = -1) As AsmCommand Pointer
declare Function IsASMConst (DataSource As String) As Integer
declare Function IsHexConfigValid(HexFile As String, ConfigSettings As String) As Integer
declare Function IsForVariant(FoundCmd As AsmCommand Pointer) As Integer

'Subs in variables.bi
declare Function AddFinalVar(VarName As String, VarLoc As String, VarIsArray As Integer = 0) As Integer
declare Sub AddVar(VarNameIn As String, VarTypeIn As String, VarSizeIn As Integer, VarSubIn As SubType Pointer, VarPointerIn As String, OriginIn As String, FixedLocation As Integer = -1, ExplicitDeclaration As Integer = 0, Used As Integer = -1)
declare SUB AllocateRAM
declare Function CalcAliasLoc(LocationIn As String) As Integer
declare Function GetWholeSFR(BitName As String) As String
declare Function GetSFRBitValue(BitName As String) As String
declare Function HasSFR(SFRName As String) As Integer
declare Function HasSFRBit(BitName As String) As Integer
declare Function IsNumberString(gstring As String) As Integer
declare Sub MakeSFR (UserVar As String, SFRAddress As Integer, AVRAlias As Integer = 0 )
declare Sub RequestVariable(VarName As String, CurrSub As SubType Pointer)
declare Function GetReversePICASIncFileLookupValue( address As integer ) As String

'Subs in preprocessor.bi
declare function LongToString(value as ulong) as string
declare Sub AddConstant(ConstName As String, ConstValue As String, ConstStartup As String = "", ReplaceExisting As Integer = -1)
declare Function CheckSysVarDef(ConditionIn As String) As String
declare Sub LoadTableFromFile(DataTable As DataTableType Pointer)
declare SUB PrepareBuiltIn ()
declare SUB PreProcessor ()
declare Sub ProcessSame (DirectiveIn As String)
declare Sub ReadTableValues
declare SUB RemIfDefs ()
declare Function RemoveSpacesfromCommands ( CO as String, Param as String ) As String
declare SUB ReplaceConstants ()
declare Function ReplaceConstantsLine (DataSourceIn As String, IncludeStartup As Integer) As String
declare SUB RunScripts ()
declare Sub TidyInputSource (CompSub As SubType Pointer)
declare Sub TableString (DataSource As String, TF As String )

'Subs in utils.bi
declare Function AddFullPath(CurrPath As String, FullPathIn As String = "") As String
declare SUB Calculate (SUM As String)
declare FUNCTION CountOccur (Source As String, Search As String, SearchWhole As Integer = 0) As Integer
declare FUNCTION CountSubstring (Source As String, Search As String) As Integer
declare Function DelType (InString As String) As String
declare FUNCTION GetByte (DataSource As String, BS As Integer) As String
declare Function GetElements(InData As String, DivChar As String = "", IncludeDividers As Integer = 0) As LinkedListElement Pointer
declare Function GetFileLine(Origin As String) As String
declare Function GetNextTempVar(CurrVar As String) As String
declare Function GetOriginString(OriginIn As OriginType Pointer) As String
declare Function GetDoubleBytes (InValue As Double) As ULongInt
declare Function GetSingleBytes (InValue As Single) As UInteger
declare Function GetString(StringName As String, UsedInProgram As Integer = -1) As String
declare Sub GetTokens(InData As String, OutArray() As String, ByRef OutSize As Integer, DivChar As String = "", IncludeDividers As Integer = 0)
declare Function GetTypeLetter(InType As String) As String
declare Function GetTypeSize(InType As String) As Integer
declare Function GetVarByteNumber(VarName As String) As Integer
declare Function HashMapCreate As HashMap Pointer
declare Function HashMapCalcHash(Key As String) As Integer
declare Sub HashMapDestroy(Map As HashMap Pointer)
declare Sub HashMapDelete(Map As HashMap Pointer, Key As String, DeleteMeta As Integer = -1)
declare Function HashMapGet(Map As HashMap Pointer, Key As String) As Any Pointer
declare Function HashMapGetStr(Map As HashMap Pointer, Key As String) As String
declare Function HashMapSet OverLoad (Map As HashMap Pointer, Key As String, Value As String, ReplaceExisting As Integer = -1) As Integer
declare Function HashMapSet OverLoad (Map As HashMap Pointer, Key As String, Value As Any Pointer, ReplaceExisting As Integer = -1) As Integer
declare Function HashMapToList(Map As HashMap Pointer, Sorted As Integer = 0) As LinkedListElement Pointer
declare FUNCTION IsCalc (Temp As String) As Integer
declare FUNCTION IsCalcDivider (Temp As String) As Integer
declare FUNCTION IsConst (Temp As String) As Integer
declare FUNCTION IsDivider (Temp As String) As Integer
declare Function IsFloatType(InType As String) As Integer
declare Function IsIntType(InType As String) As Integer
declare FUNCTION IsLet(Temp As String) As Integer
declare Function IsSysTemp(VarNameIn As String) As Integer
declare Function IsValidName(InName As String) As Integer
declare Function IsValidValue(InValue As LongInt, TypeIn As String) As Integer
declare Function LinkedListCreate As LinkedListElement Pointer
declare Function LinkedListFind OverLoad (StartNode As LinkedListElement Pointer, SearchMeta As Any Pointer) As LinkedListElement Pointer
declare Function LinkedListFind OverLoad (StartNode As LinkedListElement Pointer, SearchValue As String) As LinkedListElement Pointer
declare Function LinkedListInsert OverLoad (Location As LinkedListElement Pointer, NewLine As String, NewNumVal As Integer = 0) As LinkedListElement Pointer
declare Function LinkedListInsert OverLoad (Location As LinkedListElement Pointer, NewData As Any Pointer) As LinkedListElement Pointer
declare Function LinkedListInsertList (Location As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
declare Function LinkedListAppend (ListIn As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
declare Function LinkedListDelete (Location As LinkedListElement Pointer, DeleteMeta As Integer = -1) As LinkedListElement Pointer
declare Function LinkedListDeleteList (StartLoc As LinkedListElement Pointer, EndLoc As LinkedListElement Pointer) As LinkedListElement Pointer
declare Sub LinkedListPrint(StartNode As LinkedListElement Pointer)
declare Sub LinkedListPrintwithComments(StartNode As LinkedListElement Pointer)
declare Function LinkedListSize(StartNode As LinkedListElement Pointer) As Integer
declare FUNCTION MakeDec (DataSource As String) As LongInt
declare FUNCTION MakeDecFloat (DataSource As String) As Double
declare Function NCase(InValue As String) As String
declare Function NextCodeLine(CodeLine As LinkedListElement Pointer) As LinkedListElement Pointer
declare Function PrefIsYes(CheckVal As String, YesVal As Integer = -1) As Integer
declare SUB Replace (DataVar As String, Find As String, Rep As String)
declare SUB ReplaceAll (DataVar As String, Find As String, Rep As String)
declare Function ReplaceToolVariables(InData As String, FNExtension As String = "", FileNameIn As String = "", Tool As ExternalTool Pointer = 0) As String
declare SUB SCICONV (STemp As String)
declare Function ShortFileName(InName As String) As String
declare FUNCTION ShortName (NameIn As String) As String
declare Function SubSigMatch (SubSigIn As String, CallSigIn As String) As Integer
declare Sub WaitForKeyOrTimeout
declare Function WholeINSTR (DataIn As String, FindIn As String, SearchAgain As Integer = -1) As Integer
declare Function WholeInstrLoc(DataSource As String, FindTemp As String) As Integer
declare SUB WholeReplace (DataVar As String, Find As String, Rep As String)
declare Sub StringSplit(Text As String, Delim As String = " ", Count As Long = -1, Ret() As String)

'Initialise
'Misc Vars
DIM SHARED As Integer FRLC, FALC, SBC, WSC, FLC, DLC, SSC, SASC, POC, MainSBC, CompiledSBC, InsertLineNo
DIM SHARED As Integer COC, BVC, PCC, CVCC, TCVC, CAAC, ISRC, IISRC, RPLC, ILC, SCT
DIM SHARED As Integer CSC, CV, COSC, MemSize, FreeRAM, FoundCount, PotFound, IntLevel
DIM SHARED As Integer ChipGPR, ChipRam, ConfWords, DataPass, ChipFamily, ChipFamilyVariant, ChipSubFamily, PSP, ChipProg, IntOscSpeedValid, ChipMinimumBankSelect
Dim Shared As Integer ChipPins, UseChipOutLatches, AutoContextSave, LaxSyntax, PICASdebug, PICASDEBUGmessageShown, DATfileinspection, NoSummary, ConfigDisabled, UserCodeOnlyEnabled, ChipIO, ChipADC, methodstructuredebug, floatcapability, compilerdebug, ChipAVRDX, overridelowleveldatfileextextension, overridelowleveldatfileextextensionmessage, overridetestdatfilemessage
Dim Shared As Integer MainProgramSize, StatsUsedRam, StatsUsedProgram, RegBytesUsed = 0
DIM SHARED As Integer VBS, MSGC, PreserveMode, SubCalls, IntOnOffCount, ExitValue, OutPutConfigOptions
DIM SHARED As Integer UserInt, PauseOnErr, USDC, MRC, GCGB, ALC, DCOC, SourceFiles, IgnoreSourceFiles
Dim Shared As Integer WarningsAsErrors, FlashOnly, SkipHexCheck, ShowProgressCounters, muteBanners, ExtendedVerboseMessages, MuteDonate
DIM SHARED As Integer SubSizeCount, PCUpper, Bootloader, HighFSR, NoBankLocs
DIM SHARED As Integer RegCount, IntCount, AllowOverflow, SysInt, HMult, AllowInterrupt
Dim Shared As Integer ToolCount, ChipEEPROM, DataTables, ProgMemPages, PauseAfterCompile
Dim Shared As Integer gUSDelaysInaccurate, IntOscSpeeds, PinDirShadows, CompileSkipped
Dim Shared As Integer PauseTimeout, OldSBC, ReserveHighProg, HighTBLPTRBytes
Dim Shared As Single ChipMhz, ChipMaxSpeed, FileConverters
Dim Shared As Single StartTime, CompEndTime, AsmEndTime, ProgEndTime
Dim Shared As Double DebugTime
Dim Shared As String ChipPICASDataFile,ChipPICASConfigFile,ChipPICASRoot, Conditionaldebugfile
Dim Shared As Integer StringConCatLengthAdapted = 0
dim shared As integer WholeINSTRdebug = 0 

'Assembler vars
DIM SHARED As Integer ToAsmSymbols
DIM SHARED As String  SelectedAssembler

'Code Array
Dim Shared CompilerOutput As CodeSection Pointer
DIM SHARED As LinkedListElement Pointer AsmProg, AsmProgLoc

'Sub arrays
Dim Shared Subroutine(10000) As SubType Pointer: SBC = 0: MainSBC = 0: CompiledSBC = 0
Dim Shared Subroutines As HashMap Pointer

'Processing Arrays
DIM SHARED Constants As HashMap Pointer
Dim Shared SourceFile(100) As SourceFileType: SourceFiles = 0
Dim Shared IgnoreSourceFile(100) As SourceFileType: IgnoreSourceFiles = 0
DIM SHARED TempData(300) As String
DIM SHARED CheckTemp(300) As String
Dim SHARED SysVars As HashMap Pointer
DIM SHARED SysVarBits As HashMap Pointer
DIM SHARED FILE(300) As String
Redim SHARED FreeMem(1) As Integer
Redim SHARED VarLoc(1) As Integer
DIM SHARED MemRanges(100) As String: MRC = 0
Dim Shared NoBankLoc(10) As DataMemBankType: NoBankLocs = 0
Dim SHARED StringStore(0 To 1000) As ProgString: SSC = 0: SASC = 0
Dim Shared ConfigOps(700) As ConfigOp: COC = 0
Dim Shared ConfigSettings As LinkedListElement Pointer
Dim Shared EqConfigSettings As LinkedListElement Pointer
Dim Shared ChipConfigCode As CodeSection Pointer
DIM SHARED DefCONFIG(700) As String: DCOC = 0
DIM SHARED ConfigMask(20) As Integer
DIM SHARED DataTable(500) As DataTableType: DataTables = 0
DIM SHARED Messages(1 TO 2, 500) As String: MSGC = 0
DIM SHARED ASMCommands As HashMap Pointer
DIM SHARED ASMSymbols As HashMap Pointer
Dim Shared ToAsmSymbol(500, 1 To 2) As String: ToAsmSymbols = 0
DIM SHARED FinalVarList As HashMap Pointer
DIM Shared FinalRegList(100) As VariableListElement: FRLC = 0
DIM Shared FinalAliasList(8000) As VariableListElement: FALC = 0
DIM SHARED PreserveCode(60000) As String: PCC = 0
Dim Shared CalcVars(100) As CalcVar: TCVC = 0
Dim Shared As String AllSysSubs, UsedSysSubs
Dim Shared RegList(512, 2) As String: RegCount = 0
Dim Shared CalcAtAsm(2000) As String: CAAC = 0
Dim Shared CalcTempType(3) As String
Dim Shared Interrupts(200) As IntData: IntCount = 0
Dim Shared IntInitCode As CodeSection Pointer
Dim Shared IntHandlerCode As CodeSection Pointer
Dim Shared Tool(200) As ExternalTool: ToolCount = 0
Dim Shared ToolVariables As LinkedListElement Pointer
Dim Shared ProgMemPage(MAX_PROG_PAGES) As ProgMemPageType: ProgMemPages = 0
Dim Shared IntOscSpeed(20) As Double: IntOscSpeeds = 0
Dim Shared FileConverter(50) As FileConverterType: FileConverters = 0
Dim Shared AttemptedCallList As LinkedListElement Pointer
Dim Shared OutConfig(16) As String
Dim Shared PinDirections As LinkedListElement Pointer
Dim Shared PinDirShadow(20) As String
Dim Shared GlitchFreeOutputs As HashMap Pointer
Dim Shared ReservedWords( RESERVED_WORDS ) as String

Dim Shared As String Star80
Dim Shared As String Pad32

Dim Shared As String ChipName, OSCType, CONFIG, Intrpt, gcOPTION, ChipProgrammerName
Dim Shared As String ChipOscSource
Dim Shared As String CDF, AFI, FI, OFI, HFI, ID, Version, buildVersion, ProgDir, CLD, LabelEnd, VersionSuffix
Dim Shared As String PrgExe, PrgParams, PrgDir, AsmExe, AsmParams, PrgName, HexAppend
Dim Shared As ExternalTool Pointer AsmTool, PrgTool
Dim Shared As String CompReportFormat, globalSettingsFile

Dim Shared As Integer CDFSupport = 0
Dim Shared As Integer CDFFileHandle
Dim Shared As Integer AFISupport = 0
Dim Shared As Integer StoredGCASM = 0
Dim Shared As Integer MakeHexMode = 1
Dim Shared As Integer Columnwidth = 77
Dim Shared As UByte   ReservedwordC = 0
Dim Shared As Integer ChipMhzCalculated = 0

'Config correct code
Dim Shared as string adaptedConfigLine
Dim Shared as string configarray()
Dim Shared as Integer configarraycounter

'PICAS Converter
Dim Shared as string currentLineElements()
Dim Shared as Integer patchCounter = 0

#Define DISABLEOPTIMISATION "f122 jmp"
#Define INVALIDARRAYVALUE -9999

#Define MAX_OUTPUT_MESSAGES 200
Dim Shared As String OutMessage(MAX_OUTPUT_MESSAGES)
Dim Shared As Integer OutMessages, ErrorsFound

#Define TYPECHECKSIZE 40

Dim Shared As String UserDefineStartLabel

Dim As Integer CD, T, PD


const   ChipFamily18FxxQ10 as integer = 16100
const   ChipFamily18FxxQ43 as integer = 16101
const   ChipFamily18FxxQ41 as integer = 16102
const   ChipFamily18FxxK42 as integer = 16103
const   ChipFamily18FxxK40 as integer = 16104
const   ChipFamily18FxxQ40 as integer = 16105
const   ChipFamily18FxxQ84 as integer = 16106
const   ChipFamily18FxxK83 as integer = 16107
const   ChipFamily18FxxQ83 as integer = 16108
const   ChipFamily18FxxQ71 as integer = 16109
const   ChipFamily18FxxQ20 as integer = 16110
const   ChipFamily18FxxQ24 as integer = 16111

' Compiler debug constants
const   cCOMPILECALCADD     as integer = 1
const   cVAR_SET            as integer = 2
const   cCALCOPS            as integer = 4
const   cCOMPILECALCMULT    as integer = 8
const   cGENERATEAUTOPINDIR as integer = 16
const   cAVRDXDEBUG         as integer = 32
const   cGCASMDEBUG         as integer = 64



const   INSERTFILENOTOPEN = 1
const   INSERTFILEOPEN    = 2
const   INSERTFILEPROCESS = 3
const   INSERTFILEEOF     = 4



'Other GCBASIC source files
#include "utils.bi"
#include "assembly.bi"
#include "preprocessor.bi"
#include "variables.bi"

'FreeBASIC libraries
#include "file.bi"
#Include "string.bi"

'Close any open files, delete old error log
CLOSE
IF Dir("ERRORS.TXT") <> "" THEN KILL "ERRORS.TXT"

'Get random number seed
Randomize Timer

'Set version
Version = "2024.08.15"
buildVersion = "1409"


#ifdef __FB_DARWIN__  'OS X/macOS
  #ifndef __FB_64BIT__
    Version = Version + " (Darwin 32 bit)"
  #else
    Version = Version + " (Darwin 64 bit)"
  #endif
#elseif defined (__FB_FREEBSD__)
  #ifndef __FB_64BIT__
    Version = Version + " (FreeBSD 32 bit)"
  #else
    Version = Version + " (FreeBSD 64 bit)"
  #endif
#elseif defined (__FB_LINUX__)
  #ifndef __FB_64BIT__
    Version = Version + " (Linux 32 bit)"
  #else
    Version = Version + " (Linux 64 bit)"
  #endif
#elseif defined (__FB_WIN32__)  'Need to test first as WIN32 matches both 32 and 64 bit
   #ifdef __FB_64BIT__
       Version = Version + " (Windows 64 bit)"
   #else
       Version = Version + " (Windows 32 bit)"
   #endif
#endif

Version = Version + " : Build " + buildVersion


'Initialise assorted variables
Star80 = ";********************************************************************************"
Pad32 = "                                 "
SysInt = 0
IntLevel = 0
AllowInterrupt = 0
ErrorsFound = 0
PinDirShadows = 0
MainProgramSize = 0
StatsUsedRam = 0
StatsUsedProgram = 0
UseChipOutLatches = -1
AutoContextSave = -1
LaxSyntax = 0
PICASdebug = false
methodstructuredebug = false
floatcapability =  0 '  1 = singles, 2 = doubles, 4 = longint, 8 = uLongINT
compilerdebug = 0
overridelowleveldatfileextextension = 0
overridelowleveldatfileextextensionmessage = 0
overridetestdatfilemessage=0
PICASDEBUGmessageShown = false
DATfileinspection = true
NoSummary = 0
ReserveHighProg = 0
ConfigDisabled = 0
UserCodeOnlyEnabled = 0
ExitValue = 0
ToolVariables = LinkedListCreate
CompileSkipped = 0
OldSBC = -1
DebugTime = 0
MakeHexMode = 1
Conditionaldebugfile = ""
SelectedAssembler = "GCASM"
VersionSuffix = ""
ChipAVRDX = 0
AddConstant("CHIPASSEMBLER", SelectedAssembler )

ChipProgrammerName=""
_31kSupported.State = 0

'Various size counters
USDC = 0 'US delay loops
RPLC = 0 'Repeat loops
WSC = 0 'Wait Until/While loops
DLC = 0 'Do loops
SCT = 0 'Select Case
gUSDelaysInaccurate = 0 'Set if variable len US delays will be wrong
IntOnOffCount = 0 'Count IntOn/IntOff

'Show startup messages, and read COMMAND
StartTime = Timer
InitCompiler

'Initialise code sections and lists
CompilerOutput = NewCodeSection
IntInitCode = NewCodeSection
IntHandlerCode = NewCodeSection
ChipConfigCode = NewCodeSection

AttemptedCallList = LinkedListCreate
ConfigSettings = LinkedListCreate
PinDirections = LinkedListCreate
Constants = HashMapCreate
AsmProg = LinkedListCreate
AsmProgLoc = AsmProg
GlitchFreeOutputs = HashMapCreate

SysVars = HashMapCreate
SysVarBits = HashMapCreate
ASMCommands = HashMapCreate

CreateReservedWordsList()
If  ErrorsFound Then Goto Fin 
'Load files and tidy them up
PreProcessor

If Not ErrorsFound Then

  If FlashOnly Then
    Print Message("SkippingCompile")
    CompEndTime = Timer
    AsmEndTime = CompEndTime
    CompileSkipped = -1
    GoTo DownloadProgram
  End If

  'Compile
  CompileProgram

  'Allocate RAM
  IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("AllocateRAM")
  AllocateRAM

  IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("TidyCode")
  TidyProgram

  'Combine subs
  IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("LocatingSubs")
  MergeSubroutines

  'Final optimisation
  FinalOptimise

  If VBS = 1 THEN PRINT : PRINT SPC(5); Message("WritingASM")
  WriteAssembly
  CompEndTime = Timer

  'error check.. has program exceeded avialable progmem
  If StatsUsedProgram + ReserveHighProg > ChipProg Then
    Dim Temp As String

    PRINT
    PRINT Message("Summary")
    PRINT SPC(5); Message("DataRead")
    PRINT SPC(10); Message("InLines") + Str(MainProgramSize)
    PRINT SPC(10); Message("Subs" ) + " User: " + Str( MainSBC - 1 ) + " ; System: " + Str(CompiledSBC - MainSBC ) + " of " + Str(SBC -  MainSBC - 1 )+ " ; Total: " + Str( MainSBC - 1 + CompiledSBC - MainSBC )
    PRINT SPC(5); Message("ChipUsage")
    Temp = Message("UsedProgram")
    Replace Temp, "%used%", Str(StatsUsedProgram + ReserveHighProg)
    Replace Temp, "%total%", Str(ChipProg)
    If ChipProg <> 0 Then Temp += Format((StatsUsedProgram + ReserveHighProg) / ChipProg, " (###.##%)")
    PRINT SPC(10); Temp
    Temp = Message("UsedRAM")
    Replace Temp, "%used%", Str(StatsUsedRam+RegBytesUsed)
    Replace Temp, "%total%", Str(ChipRAM)
    If ChipRAM <> 0 Then Temp += Format(StatsUsedRAM / ChipRAM, " (###.##%)")
    PRINT SPC(10); Temp


    LogError Message ("OutOfProgMemExceeded")
  End if

  'If no errors, show success message and assemble
  IF Not ErrorsFound THEN
    'Success message
    PRINT Message("CompiledSuccess");
    '  IF VBS = 0 THEN
    '    PRINT
    '  Else
      Dim Temp As String
      Temp = Trim(Str(CompEndTime - StartTime))
      IF LEN(Temp) > 4 Then Temp = Left(Temp, 5)
      PRINT Message("CompTime") + Temp + Message("CompSecs")
      If NoSummary = 0 Then
        PRINT
        PRINT Message("Summary")

        PRINT SPC(5); Message("DataRead")

        PRINT SPC(10); Message("InLines") + Str(MainProgramSize)
        PRINT SPC(10); Message("Subs" ) + " User: " + Str( MainSBC - 1 ) + " ; System: " + Str(CompiledSBC - MainSBC ) + " of " + Str(SBC -  MainSBC - 1 )+ " ; Total: " + Str( MainSBC - 1 + CompiledSBC - MainSBC )
        PRINT SPC(5); Message("ChipUsage")
        Temp = Message("UsedProgram")
        Replace Temp, "%used%", Str(StatsUsedProgram + ReserveHighProg)
        Replace Temp, "%total%", Str(ChipProg)
        If ChipProg <> 0 Then Temp += Format((StatsUsedProgram + ReserveHighProg) / ChipProg, " (###.##%)")
        PRINT SPC(10); Temp
        Temp = Message("UsedRAM")
        Replace Temp, "%used%",  Str(StatsUsedRam+RegBytesUsed)
        Replace Temp, "%total%", Str(ChipRAM)
        If ChipRAM <> 0 Then 
          If StatsUsedRam+RegBytesUsed = 0 Then
            Temp += " (0%)"
          Else
            Temp += Format( (StatsUsedRAM+RegBytesUsed) / ChipRAM, " (###.##%)")
          End If
        End If


        PRINT SPC(10); Temp


        OscType = ""
        If ModePIC Then
          If HashMapGet(Constants, "CHIPUSINGINTOSC") <> 0 Then
            OscType = " (" + Message("CRIntOsc") + ")"
          Else
            OscType = " (" + Message("CRExtOsc") + ")"
          End If
        End If
        If ChipAVRDX Then
          If HashMapGet(Constants, "CHIPUSINGINTOSC") <> 0 Then
            OscType = " (" + Message("CRIntOsc") + ")"
          Else
            OscType = " (" + Message("CRExtOsc") + ")"
          End If
        End If

        PRINT SPC(10);
        If ModePIC Then
            Print "OSC: " + ChipOscSource + ", " + Str(ChipMhz) + "Mhz" + OscType;
        Else
            Print "OSC: " + Str(ChipMhz) + "Mhz" + OscType;
        End If
        If ChipMhzCalculated = 0 Then
          Print
        Else
          Print " : Frequency calculated by compiler"
        End If
      End If
  '  END IF

    If ModeAVR then
          AFISupport = 0
    End IF

    If AsmExe <> "" and MakeHexMode = 1 Then

      IF UCase(AsmExe) = "GCASM" THEN
        'Internal assembler
        PRINT
        PRINT Message("MakeASM")
        AssembleProgram

      ELSEIF instr(UCase(AsmExe),"PIC-AS") > 0 THEN

        'AssembleProgram
        PRINT
        PRINT Message("MakeS")
        PICASAssembler
        'IF Not ErrorsFound THEN PRINT Message("PICASMSuccess")


      Else
        ExtAssembler
      END If

      AsmEndTime = Timer
      'If VBS = 1 Then
      ' Dim Temp As String
      ' Temp = Trim(Str(AsmEndTime - CompEndTime))
      ' IF LEN(Temp) > 4 Then Temp = Left(Temp, 5)
      ' PRINT Message("AsmTime") + Temp + Message("CompSecs")
      'End If

        IF Not ErrorsFound THEN
          Dim Temp As String
          Temp = Trim(Str(AsmEndTime - CompEndTime))
          IF LEN(Temp) > 4 Then Temp = Left(Temp, 5)
          PRINT Message("ASMSuccess") + Message("AsmTime") + Temp + Message("CompSecs")
        End If

    End If




  End If
  AsmEndTime = Timer

  'Download program
  DownloadProgram:
    IF (  PrgExe <> "" AND  Ucase(RIGHT(PrgExe,4)) <> "NONE"  AND AsmExe <> "" ) AND ErrorsFound = 0 THEN
      PRINT
      Dim Temp As String
      Temp = Message("SendToPIC")
      Replace Temp, "%PrgName%", Trim(Str(PrgName))
      PRINT Temp
      PrgExe = ReplaceToolVariables(PrgExe, "hex",, PrgTool)
      PrgParams = ReplaceToolVariables(PrgParams, "hex",, PrgTool)

      #ifdef __FB_WIN32__  'Need to test as WIN32 matches both 32 and 64 bit
          if dir(PrgExe)="" then
              'Temp = "Programmer executable " + PrgExe + " does not exist"
              Temp = Message("ProgrammerNotFound")
              Replace Temp, "%PrgExe%", PrgExe
              LogError Temp
          End if
      #endif

      If ErrorsFound = 0 Then
        IF VBS = 1 THEN PRINT SPC(5);"Calling    : " + PrgExe
        IF VBS = 1 THEN PRINT SPC(5);  "Parameters : " + PrgParams


        Dim As String SaveCurrDir
        SaveCurrDir = CurDir
        If PrgDir <> "" Then ChDir ReplaceToolVariables(PrgDir, "hex")

        ExitValue = Exec(PrgExe, PrgParams)

        'Check for programmer success, should have 0 exit value
        If ExitValue <> 0 And (LCase(PrgExe) <> "none" And LCase(Right(PrgExe, 5)) <> "\none") Then
          Dim Temp As String
          Temp = Message("WarningProgrammerFail")
          Replace Temp, "%status%", Trim(Str(ExitValue))
          LogWarning Temp
        Else
          ExitValue = 0
          ProgEndTime = Timer

          If VBS = 1 Then
            Dim Temp As String
            Temp = Trim(Str(ProgEndTime - AsmEndTime))
            IF LEN(Temp) > 4 Then Temp = Left(Temp, 5)
            PRINT Message("ProgTime") + Temp + Message("CompSecs")
          End If
        End If
        ChDir SaveCurrDir
      End If
    END If
  ProgEndTime = Timer

  'Issue message
End If
If MuteDonate = -1 then
  IF Not ErrorsFound and MuteBanners = -1 THEN
      Randomize timer
      Select Case  int(Rnd * (10 - 1) + 1)
        Case 1,2,3:
      Print
      Print "Enjoying GCBASIC ?"
      Print
      Print "Please donate to help support the operational costs of the project.  Donate via http://paypal.me/gcbasic"

        Case 4,5,6:
      Print
      Print "Finding GCBASIC useful ?"
      Print
      Print "Please donate to help support the operational costs of the project.  Donate via http://paypal.me/gcbasic"

        Case 7,8,9:
      Print
      Print "Spreading the word about using GCBASIC ?"
      Print
      Print "Please donate to help support the operational costs of the project.  Donate via http://paypal.me/gcbasic"

        Case Else

      'Nothing

      End Select
  End if
Else  
  beep
    Print
    Print "Please support GCBASIC - donate to the operational costs of providing GCBASIC."
    Print
    Print "Goto to http://paypal.me/gcbasic and donate. $25 is the typical donation, but, donate what you can."
    sleep 2000
    Print
    Print
  Beep
End If
'Write compilation report
WriteCompilationReport

Fin:
'Write errors to file
WriteErrorLog

If VBS = 1 Then
  Dim Temp As String
  Temp = Trim(Str(ProgEndTime - StartTime))
  IF LEN(Temp) > 4 Then Temp = Left(Temp, 5)
  PRINT ""
  PRINT Message("TotalTime") + Temp + Message("CompSecs")
End If

If DebugTime > 0 Then
  Color 14
  Print "DebugTime:"; DebugTime; " s"
  Color 7
End If

'End of program
'Pause and wait for key at end of compilation?
If PauseAfterCompile Then
  Print
  WaitForKeyOrTimeout
End If

Dim Temp As String
Temp = Message("Success")
'Print
Print Temp

Finishs:
Close
End ExitValue

SUB Add18FBanks(CompSub As SubType Pointer)
  Dim As String TempData, First8, VarName
  Dim As Integer PD, ConstFound, FC

  Dim As LinkedListElement Pointer CurrLine

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    TempData = UCase(CurrLine->Value)
    First8 = Left(TempData, 8)
    'Add banking mode where needed
    IF INSTR(First8, "MOVFF") = 0 AND INSTR(First8, "LFSR") = 0 AND INSTR(First8, "RETFIE") = 0 AND INSTR(TempData, ",ACCESS") = 0 AND INSTR(TempData, ", ACCESS") = 0 AND TempData <> "" AND Left(TempData, 1) <> ";" AND ( Instr(TempData,"#DEFINE" ) = 0 ) THEN
      TempData = Trim(TempData)
      VarName = Mid(TempData, INSTR(TempData, " ") + 1)
      TempData = Left(TempData, INSTR(TempData, " ") - 1)
      IF INSTR(TempData, "F") <> 0 THEN
        IF INSTR(VarName, ",") <> 0 THEN VarName = Left(VarName, INSTR(VarName, ",") - 1)
        VarName = Trim(VarName)

        'permit raw ASM through the ASM source when is leading with #DEFINE.  All other cases should be treated as-was
        IF ( INSTR( trim(CurrLine->Value), "#DEFINE" ) <> 1  and Left( ucase(trim( GetMetaData(CurrLine)->OrgLine)), 4 ) <> "ASM " ) then
            'Check if the variable being accessed is a SFR, and add banking mode
            If IsInAccessBank(VarName) Then
              CurrLine->Value = CurrLine->Value + ",ACCESS"
            Else
              CurrLine->Value = CurrLine->Value + ",BANKED"
            End If
        Else
          'permit ASM with the format of asm #define  - used in MPASX
          'pointless assignment - just left for total clarity
          CurrLine->Value = CurrLine->Value
        End if


      END IF
    End If
    CurrLine = CurrLine->Next
  Loop

END SUB

SUB AddBankCommands(CompSub As SubType Pointer)
  'Scans through program, adds banksel instructions where needed

  'Not used for AVR
  If ModeAVR Then Exit Sub

  Dim As Integer BankMask, Bank, FindVar, BankselNeeded, ReturnAdded
  Dim As String TempData, OtherData, VarInBank, VarName, DestLabel
  Dim As LinkedListElement Pointer CurrLine, LabelList, LabelListPos, LabelLoc, BankList, BankListLoc, PrintLoc
  Dim As LinkedListElement Pointer RealNextLine
  Dim As ProgLineMeta Pointer CurrMeta, DestMeta
  Dim As String LineToken(100)
  Dim As Integer LineTokens, CurrToken, UseMOVFFL
  Dim As SysVarType Pointer FoundSFR
  Dim As VariableListElement Pointer FoundFinalVar

  'Get bank size
  Select Case ChipFamily
    Case 12: BankMask = 32
    Case 14: BankMask = 384
    Case 15: BankMask = 3968 : If ChipRAM >  4095 then BankMask = 8064
    Case 16: BankMask = 65280
  End Select

  'Add return to end of sub
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    If CurrLine->Next = 0 Then
      LinkedListInsert(CurrLine, " return")
      ReturnAdded = -1
      CurrLine = 0
    Else
      CurrLine = CurrLine->Next
    End If
  Loop

  'Get list of labels and locations in program
  LabelList = GetLabelList(CompSub)

  'Check required bank for each line
  'Also, ensure metadata attached
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    'Get metadata for line
    CurrMeta = GetMetaData(CurrLine)

    'Check to see what bank line requires
    TempData = CurrLine->Value

    Bank = -1
    VarInBank = ""
    If Left(Trim(TempData), 6) = "return" Then
      Bank = 0
    ElseIf Left(Trim(TempData), 5) = "call " Then
      Bank = 0

    Else
      'If line operates on file registers, check var usage
      OtherData = UCase(Trim(TempData))
      IF INSTR(OtherData, " ") <> 0 THEN OtherData = Left(OtherData, INSTR(OtherData, " ") - 1)
      IF INSTR(OtherData, "F") <> 0 And WholeINSTR(TempData, "lfsr") <> 2 And WholeINSTR(TempData, "movff") <> 2 Then

        'Split line into tokens
        GetTokens(TempData, LineToken(), LineTokens)
        For CurrToken = 1 To LineTokens
          'Check for SFRs outside of Bank 0
          FoundSFR = GetSysVar(UCase(LineToken(CurrToken)))

          If FoundSFR <> 0 Then
            'Found an SFR

            '18F code: check for SFRs outside of access bank
            If ChipFamily = 16 Then
              If Not IsNonBanked(FoundSFR->Location) Then
                Bank = FoundSFR->Location And BankMask
                VarInBank = FoundSFR->Name
              End If

            '16F1 code: Don't need bank selection for 12 core registers on 16F1xxx
            ElseIf ChipFamily = 15 Then
              If (FoundSFR->Location And Not BankMask) >= 12 Then
                Bank = FoundSFR->Location And BankMask
                VarInBank = FoundSFR->Name
              End If

            'Other PIC code: assume always need bank selection (unless dealing with STATUS, PCL, FSR or INDF)
            Else
              Select Case UCase(FoundSFR->Name)
                Case "STATUS", "PCL", "FSR", "INDF", "PCLATH", "INTCON"
                  Bank = -1
                Case Else
                  Bank = FoundSFR->Location And BankMask
                  VarInBank = FoundSFR->Name
              End Select

            End If
            GoTo RequiredBankFound
          End If

          'Check for user vars outside of bank 0
          FoundFinalVar = HashMapGet(FinalVarList, UCase(LineToken(CurrToken)))
          IF FoundFinalVar <> 0 Then
            With *FoundFinalVar
              'Have found a variable, is banking needed?
              IF Not .IsArray AND Not IsRegister(.Name) Then
                Bank = VAL(.Value) AND BankMask
                VarInBank = .Name
                GoTo RequiredBankFound
              End If
            End With
          End If
        Next

        RequiredBankFound:
      End If
    End If

    With *CurrMeta
      .RequiredBank = Bank
      .VarInBank = VarInBank
    End With

    CurrLine = CurrLine->Next
  Loop

  'Record jump to first line
  'Use null pointer to indicate it
  CurrLine = CompSub->CodeStart->Next
  If CurrLine <> 0 Then
    DestMeta = CurrLine->MetaData
    LinkedListInsert(DestMeta->PrevCommands, 0)
  End If

  'Generate graph of connections between lines
  CurrLine = CompSub->CodeStart->Next
  Dim As String CurrLine4, CurrLine5, CurrLine6, CurrLine8
  Do While CurrLine <> 0

    CurrLine4 = Left(CurrLine->Value, 4)
    CurrLine5 = Left(CurrLine->Value, 5)
    CurrLine6 = Left(CurrLine->Value, 6)
    CurrLine8 = Left(CurrLine->Value, 8)
    CurrMeta = CurrLine->MetaData

    'goto/bra
    If CurrLine6 = " goto " Or CurrLine5 = " bra " Then
      'Print CurrLine->Value; " to label"

      'Get label jumped to
      'Get label name
      If CurrLine6 = " goto " Then
        DestLabel = LCase(Trim(Mid(CurrLine->Value, 7)))
      Else
        DestLabel = LCase(Trim(Mid(CurrLine->Value, 6)))
      End If

      'Is this a delay jump?
      TempData = DestLabel
      Do While InStr(TempData, " ") <> 0
        Replace TempData, " ", ""
      Loop
      If TempData = "$+1" Or TempData = "$+2" Then
        'Record jump to next line
        LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next)
        If CurrLine->Next <> 0 Then
          DestMeta = CurrLine->Next->MetaData
          LinkedListInsert(DestMeta->PrevCommands, CurrLine)
        End If

      'Not a delay jump, find destination
      Else
        'Find label name in list
        LabelLoc = 0
        LabelListPos = LabelList->Next
        Do While LabelListPos <> 0
          If LCase(LabelListPos->Value) = DestLabel Then
            LabelLoc = LabelListPos->MetaData
            Exit Do
          End If
          LabelListPos = LabelListPos->Next
        Loop
        'Record jump
        If LabelLoc <> 0 Then
          'Record in list of next locations for current line
          LinkedListInsert(CurrMeta->NextCommands, LabelLoc)
          'Record in list of previous locations for destination line
          DestMeta = LabelLoc->MetaData
          LinkedListInsert(DestMeta->PrevCommands, CurrLine)
        End If

      End If

    'conditional branch
    ElseIf CurrLine4 = " bc " Or CurrLine4 = " bn " Or _
         CurrLine4 = " bz " Or CurrLine5 = " bnc " Or _
         CurrLine5 = " bnn " Or CurrLine5 = " bnz " Or _
         CurrLine5 = " bov " Or CurrLine6 = " bnov " Then
      'Print CurrLine->Value; " to next or label"
      'Show jump to next
      LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next)
      If CurrLine->Next <> 0 Then
        DestMeta = CurrLine->Next->MetaData
        LinkedListInsert(DestMeta->PrevCommands, CurrLine)
      End If

      'Get label jumped to
      'Get label name
      DestLabel = LCase(Trim(Mid(CurrLine->Value, 9)))
      'Find label name in list
      LabelLoc = 0
      LabelListPos = LabelList->Next
      Do While LabelListPos <> 0
        If LCase(LabelListPos->Value) = DestLabel Then
          LabelLoc = LabelListPos->MetaData
          Exit Do
        End If
        LabelListPos = LabelListPos->Next
      Loop
      'Record jump
      If LabelLoc <> 0 Then
        LinkedListInsert(CurrMeta->NextCommands, LabelLoc)
        DestMeta = LabelLoc->MetaData
        LinkedListInsert(DestMeta->PrevCommands, CurrLine)
      End If

    'btfsc/btfss/cpfseq/cpfslt/cpfsgt/decfsz
    ElseIf CurrLine5 = " btfs" Or CurrLine5 = " cpfs" Or _
          CurrLine8 = " decfsz " Or CurrLine8 = " dcfsnz " Or _
          CurrLine8 = " incfsz " Or CurrLine8 = " infsnz " Or _
          CurrLine8 = " tstfsz" Then
      'Print CurrLine->Value; " to next or after next"
      'Show jump to next
      RealNextLine = NextCodeLine(CurrLine)
      LinkedListInsert(CurrMeta->NextCommands, RealNextLine)
      If RealNextLine <> 0 Then
        'Show jump from this
        DestMeta = RealNextLine->MetaData
        LinkedListInsert(DestMeta->PrevCommands, CurrLine)

        'Show jump after next
        RealNextLine = NextCodeLine(RealNextLine)
        LinkedListInsert(CurrMeta->NextCommands, RealNextLine)
        If RealNextLine <> 0 Then
          DestMeta = RealNextLine->MetaData
          LinkedListInsert(DestMeta->PrevCommands, CurrLine)
        End If
      End If

    'Return
    ElseIf Left(CurrLine->Value, 7) = " return" Then
      'Don't add any jump

    'others
    Else
      'Print CurrLine->Value; " to next"
      'Show jump to next
      LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next)
      If CurrLine->Next <> 0 Then
        DestMeta = CurrLine->Next->MetaData
        LinkedListInsert(DestMeta->PrevCommands, CurrLine)
      End If
    End If

    CurrLine = CurrLine->Next
  Loop

  'Print prev/next
  /'
  If UCase(CompSub->Name) = "READAD" Then
    CurrLine = CompSub->CodeStart->Next
    Color 15
    Do While CurrLine <> 0
      Print "Line:"; CurrLine->Value
      CurrMeta = CurrLine->MetaData

      PrintLoc = CurrMeta->PrevCommands->Next
      Do While PrintLoc <> 0
        BankListLoc = PrintLoc->MetaData
        Print "    After:"; BankListLoc->Value
        PrintLoc = PrintLoc->Next
      Loop
      PrintLoc = CurrMeta->NextCommands->Next
      Do While PrintLoc <> 0
        BankListLoc = PrintLoc->MetaData
        Print "    Before:"; BankListLoc->Value
        PrintLoc = PrintLoc->Next
      Loop
      CurrLine = CurrLine->Next
    Loop
    Color 7
  End If
  '/

  'Find locations where current bank may not match required bank
  CurrLine = CompSub->CodeStart->Next
  'FinalBankselNeeded = 0
  Do While CurrLine <> 0

    'Decide if banksel needed
    BankselNeeded = 0
    VarInBank = ""

    CurrMeta = CurrLine->MetaData
    'Need to make sure metadata exists for line, banksels won't have it
    If CurrMeta <> 0 Then
      'Does line require a particular bank?
      If CurrMeta->RequiredBank <> -1 Then

        'Need to check what bank/s chip will be in
        BankList = FindPotentialBanks(CurrLine)

        'If it can only be in correct bank before line, no need for banksel
        If LinkedListSize(BankList) <> 1 Then
          If LinkedListSize(BankList) <> 0 Or CurrMeta->RequiredBank > 0 Then
            BankselNeeded = -1
          End If

        ElseIf BankList->Next->NumVal <> CurrMeta->RequiredBank Then
          BankselNeeded = -1
        End If

        If BankselNeeded Then
          VarInBank = CurrMeta->VarInBank
          If VarInBank = "" Then
            If CurrMeta->RequiredBank = 0 Then
              If ChipFamily = 16 Then
                VarInBank = "0"
              Else
                VarInBank = "STATUS"
              End If
            Else
              Print "Internal error in AddBankCommands"
            End If
          End If
        End If
      End If
    End If

    'Add banksel
    If BankselNeeded Then
      'Prevent BANKSEL being added UserCodeOnlyEnabled at end of program
      If UserCodeOnlyEnabled = 0  Then 
        LinkedListInsert(CurrLine->Prev, " banksel " + VarInBank)
      Else 
        If CurrLine->Next <> 0 Then
          'Must be UserCodeOnlyEnabled=-1, and not end of the user program
          LinkedListInsert(CurrLine->Prev, " banksel " + VarInBank)
        End if
      End If
    End If
    ''Add final banksel, need to return in bank 0
    'If CurrLine->Next = 0 And FinalBankselNeeded Then
    ' LinkedListInsert(CurrLine, " banksel STATUS")
    ' CurrLine = 0
    'End If

    If CurrLine->Next = 0 Then
      'Remove added return
      If ReturnAdded Then
        If CurrLine->Value = " return" Then
          LinkedListDelete(CurrLine)
          CurrLine = 0
        End If
      End If
    End If

    If CurrLine <> 0 Then
      CurrLine = CurrLine->Next
    End If
  Loop

  'On 18F with larger RAM, replace movff with movffl where necessary
  If ChipFamily = 16 And MemSize >= 4096 Then
    CurrLine = CompSub->CodeStart->Next
    Do While CurrLine <> 0
      If Left(CurrLine->Value, 7) = " movff " Then
        'Get variables
        VarName = Trim(Mid(CurrLine->Value, 8))
        LineToken(2) = Trim(Mid(VarName, InStr(VarName, ",") + 1))
        LineToken(1) = Trim(Left(VarName, InStr(VarName, ",") - 1))
        UseMOVFFL = 0

        'Is either above location 4096?
        For CurrToken = 1 To 2
          'Check for system variable
          FoundSFR = GetSysVar(UCase(LineToken(CurrToken)))
          If FoundSFR <> 0 Then
            If FoundSFR->Location > 4096 Then UseMOVFFL = -1
          Else
            'Check for user var
            FoundFinalVar = HashMapGet(FinalVarList, UCase(LineToken(CurrToken)))
            IF FoundFinalVar <> 0 Then
              With *FoundFinalVar
                If Val(.Value) >= 4096 Then UseMOVFFL = -1
              End With
            End If
          End If
        Next

        'Make replacement
        If UseMOVFFL Then
          CurrLine->Value = " movffl " + LineToken(1) + "," + LineToken(2)
        End If
      End If

      CurrLine = CurrLine->Next
    Loop
  End If

End Sub

Sub AddPageCommands(CompSub As SubType Pointer)

  Dim As LinkedListElement Pointer CurrLine, NextLine
  Dim As Integer CalledSub, CalledSubPage, ThisSubPage, CheckSub, TempDataCount
  Dim As Integer RestorePage, ForceRestorePage, NextCalledSub, NextCalledSubPage
  Dim As String TempData(100), CallTarget, ThisSubName, NextCallTarget
  Dim As SubType Pointer GotoSub

  If CompSub = 0 Then Exit Sub

  ThisSubPage = CompSub->DestPage

  'This is not needed on AVR or PIC18
  If ModeAVR Then Exit Sub
  If ModePIC And ChipFamily = 16 Then Exit Sub

  'Remove any existing page selection commands
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    'Get metadata for line
    If CurrLine->MetaData <> 0 Then
      'Line is automatic page sel, remove it
      If GetMetaData(CurrLine)->IsAutoPageSel Then
        CurrLine = LinkedListDelete(CurrLine)
      End If
    End If

    CurrLine = CurrLine->Next
  Loop

  'Search subroutine for calls
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    'Found a call, get destination and destination page
    If Left(CurrLine->Value, 6) = " call " Then
      GetTokens(CurrLine->Value, TempData(), TempDataCount)
      CallTarget = TempData(2)
      ThisSubName = CompSub->Name
      CalledSub = LocationOfSub(CallTarget, "")
      CalledSubPage = Subroutine(CalledSub)->DestPage

      'Check next line for a call, get dest and dest page
      '(Decide if page needs to be restored after this call)
      RestorePage = -1
      'Find next actual code line, ignore comments
      NextLine = CurrLine->Next
      Do While (Left(NextLine->Value, 1) = ";" Or Left(NextLine->Value, 8) = "PRESERVE") AND NextLine->Next <> 0
        NextLine = NextLine->Next
      Loop
      If NextLine <> 0 Then
        If Left(NextLine->Value, 6) = " call " Then
          'Next line is a call, find the sub it is calling and what page it is on
          GetTokens(NextLine->Value, TempData(), TempDataCount)
          NextCallTarget = TempData(2)
          NextCalledSub = LocationOfSub(NextCallTarget, "")
          NextCalledSubPage = Subroutine(NextCalledSub)->DestPage
          If NextCalledSubPage <> ThisSubPage Then
            RestorePage = 0
          End If
        End If
      End If

      'Is call in different page?
      If CalledSubPage = ThisSubPage Then
        RestorePage = 0
      End If

      'If called sub ends in goto, restore
      'May have linked list: called sub > goto sub > another goto sub
      'Need to check if called sub's goto, or any other goto will change page
      'GotoSub = Subroutine(NextCalledSub)
      GotoSub = Subroutine(CalledSub)
      Do While GotoSub -> HasFinalGoto
        GotoSub = Subroutine(LocationOfSub(GotoSub->FinalGotoDest, ""))
        If CompSub->DestPage <> GotoSub->DestPage Then
          RestorePage = -1
          Exit Do
        End If
      Loop

      'If called sub is IndCall, may need to restore page
      If UCase(CallTarget) = "INDCALL" Then
        'Any sub could be called from IndCall. So, if any subs off page 0, restore needed
        For CheckSub = 1 To SBC
          If Subroutine(CheckSub)->Required And Subroutine(CheckSub)->DestPage <> 0 Then
            RestorePage = -1
            Exit For
          End If
        Next
      End If

      'Alter call to correct instruction/s
      If ModePIC Then
        If ChipFamily = 12 Or ChipFamily = 14 Or ChipFamily = 15 Then
          'Remove call temporarily
          CurrLine = LinkedListDelete(CurrLine)
          If AFISupport = 0 Then
              'GCASM - this is the original method
              'Set page if needed
              If CalledSubPage <> ThisSubPage Then
                CurrLine = LinkedListInsert(CurrLine, " pagesel " + CallTarget)
                GetMetaData(CurrLine)->IsAutoPageSel = -1
              End If
              'Add call back
              CurrLine = LinkedListInsert(CurrLine, " call " + CallTarget)
              'Restore PCLATH if needed
              If RestorePage Then
                CurrLine = LinkedListInsert(CurrLine, " pagesel $")
                GetMetaData(CurrLine)->IsAutoPageSel = -1
              End If
          Else
            'PIC-AS mandates use of fcall... and, no matter how clever we are ... fcall is required
            If CalledSubPage <> ThisSubPage Then
              'Add call back as a fcall
               CurrLine = LinkedListInsert(CurrLine, " fcall " + CallTarget)
            Else
              'Add call back
              CurrLine = LinkedListInsert(CurrLine, " call " + CallTarget)
              'Restore PCLATH if needed
              If RestorePage Then
                CurrLine = LinkedListInsert(CurrLine, " pagesel $")
                GetMetaData(CurrLine)->IsAutoPageSel = -1
              End If
            End if
          End if


        End If
      ElseIf ModeZ8 Then
        CurrLine = LinkedListDelete(CurrLine)
        CurrLine = LinkedListInsert(CurrLine, " call " + CallTarget)
      End If

    'Also deal with gotos that jump to other subs
    ElseIf Left(CurrLine->Value, 6) = " goto " And CompSub->HasFinalGoto Then
      GetTokens(CurrLine->Value, TempData(), TempDataCount)

      'Have found goto, make sure it is a jump to another sub
      CallTarget = TempData(2)
      If LCase(CallTarget) = LCase(CompSub->FinalGotoDest) Then
        ThisSubName = CompSub->Name
        CalledSub = LocationOfSub(CallTarget, "")
        If CalledSub <> 0 Then
          'Have found a jump to a sub
          CalledSubPage = Subroutine(CalledSub)->DestPage

          'Alter call to correct instruction/s
          If ModePIC Then
            If ChipFamily = 12 Or ChipFamily = 14 Or ChipFamily = 15 Then
              CurrLine = LinkedListDelete(CurrLine)
              If AFISupport = 0 then
              'GCASM - this is the original method
                If CalledSubPage = ThisSubPage Then
                  CurrLine = LinkedListInsert(CurrLine, " goto " + CallTarget)
                Else
                  CurrLine = LinkedListInsert(CurrLine, " pagesel " + CallTarget)
                  GetMetaData(CurrLine)->IsAutoPageSel = -1
                  CurrLine = LinkedListInsert(CurrLine, " goto " + CallTarget)
                End If
              Else
              'PIC-AS mandates use of ljmp... and, no matter how clever we are ... ljmp is required
                If CalledSubPage = ThisSubPage Then
                  CurrLine = LinkedListInsert(CurrLine, " goto " + CallTarget)
                Else
                  CurrLine = LinkedListInsert(CurrLine, " ljmp " + CallTarget)
                End If
              End if

            End If
          End If
        End If
      End If
    End If


    CurrLine = CurrLine->Next
  Loop

End Sub

Sub AddMainEndCode
  Dim As LinkedListElement Pointer CurrPos

  'Prevent return being placed at end
  Subroutine(0)->NoReturn = -1

  'Get last location in main sub
  CurrPos = Subroutine(0)->CodeStart
  Do While CurrPos->Next <> 0
    CurrPos = CurrPos->Next
  Loop

  If ModePIC Then
    If UserCodeOnlyEnabled = 0 then
        CurrPos = LinkedListInsert(CurrPos, "BASPROGRAMEND")
        GetMetaData(CurrPos)->IsLabel = -1
        CurrPos = LinkedListInsert(CurrPos, " sleep")
        'CurrPos = LinkedListInsert(CurrPos, " goto $")
        CurrPos = LinkedListInsert(CurrPos, " goto BASPROGRAMEND")
    End If
  ElseIf ModeAVR Then
    CurrPos = LinkedListInsert(CurrPos, "BASPROGRAMEND:")
    CurrPos = LinkedListInsert(CurrPos, " sleep")
    'CurrPos = LinkedListInsert(CurrPos, "ProgEndLoop:")
    'CurrPos = LinkedListInsert(CurrPos, " rjmp ProgEndLoop")
    CurrPos = LinkedListInsert(CurrPos, " rjmp BASPROGRAMEND")
  ElseIf ModeZ8 Then

  Else
    Print "Unsupported family, Main"

  End If
End Sub

Sub AddMainInitCode
  'Add initialisation code to start of Main routine
  Dim As LinkedListElement Pointer CurrLine, AddPos, InitRoutineFiles
  Dim As SysVarType Pointer TempBit
  Dim As Integer CurrInc, SubLoc, OldVBS
  Dim As String Temp

  Dim As SubType Pointer AutoDirSub

  CurrLine = Subroutine(0)->CodeStart

  'Mark Main sub as destined for first page
  Subroutine(0)->FirstPage = -1

  'Set up stack (AVR and Z8)
  If ModeAVR Then
    CurrLine = LinkedListInsert(CurrLine, ";Initialise stack")
    If CHIPAVRDX Then
      'AVRDX support
      If (( compilerdebug and cAVRDXDEBUG ) = cAVRDXDEBUG )  Then
        CurrLine = LinkedListInsert(CurrLine, ";AVRDX #0 support")
      End If

      CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,high(RAMEND)")
      CurrLine = LinkedListInsert(CurrLine, " out "+GetSysVarAliasName ("ALIAS_CPU_SPH")+", SysValueCopy")
      CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,low(RAMEND)")
      CurrLine = LinkedListInsert(CurrLine, " out "+GetSysVarAliasName ("ALIAS_CPU_SPL")+", SysValueCopy") 
    Else
      'Pre AVRDX support code - unchanged
      If HasSFR("SPH") Then
        CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,high(RAMEND)")

        CurrLine = LinkedListInsert(CurrLine, " out SPH, SysValueCopy")
      End If
      If HasSFR("SPL") Then
        CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,low(RAMEND)")
        CurrLine = LinkedListInsert(CurrLine, " out SPL, SysValueCopy")
      ElseIf HasSFR("SP") Then
        CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,low(RAMEND)")
        CurrLine = LinkedListInsert(CurrLine, " out SP, SysValueCopy")
      End If
    End If
  ElseIf ModeZ8 Then
    CurrLine = LinkedListInsert(CurrLine, ";Initialise stack")
    CurrLine = LinkedListInsert(CurrLine, " ldx SPH, #HIGH(RAMEND)")
    CurrLine = LinkedListInsert(CurrLine, " ldx SPL, #LOW(RAMEND)")
  End If

  'Call init routines
  If UserCodeOnlyEnabled = 0 then
      CurrLine = LinkedListInsert(CurrLine, ";Call initialisation routines")
  End If
  InitRoutineFiles = LinkedListCreate
  FOR CurrInc = 1 TO SourceFiles
    With SourceFile(CurrInc)
      If .InitSub <> "" And .InitSubUsed Then
        'Need init sub, add to list
        AddPos = InitRoutineFiles
        Do
          If AddPos->Next = 0 OrElse .InitSubPriority < AddPos->Next->NumVal Then
            AddPos = LinkedListInsert(AddPos, .InitSub)
            AddPos->NumVal = .InitSubPriority
            Exit Do
          End If
          AddPos = AddPos->Next
        Loop While AddPos <> 0

      End If
    End With
  Next

  AddPos = InitRoutineFiles->Next
  Do While AddPos <> 0
    SubLoc = LocationOfSub(AddPos->Value, "")
    If ModeAVR Then
      CurrLine = LinkedListInsert(CurrLine, " rcall " + AddPos->Value)
    Else
      CurrLine = LinkedListInsert(CurrLine, " call " + AddPos->Value)
    End If

    AddPos = AddPos->Next
  Loop
  LinkedListDeleteList(InitRoutineFiles, 0)

  'Enable interrupts
  If UserInt Or SysInt Then
    CurrLine = LinkedListInsert(CurrLine, ";Enable interrupts")
    If ModePIC Then
      TempBit = HashMapGet(SysVarBits, "GIE")
      If TempBit <> 0 Then
        CurrLine = LinkedListInsert(CurrLine, " bsf " + TempBit->Parent + ",GIE")
      End If
      TempBit = HashMapGet(SysVarBits, "PEIE")
      If TempBit <> 0 Then
        CurrLine = LinkedListInsert(CurrLine, " bsf " + TempBit->Parent + ",PEIE")
      End If
    ElseIf ModeAVR Then
      CurrLine = LinkedListInsert(CurrLine, " sei")

    ElseIf ModeZ8 Then
      CurrLine = LinkedListInsert(CurrLine, " ei")

    End If

    'Ensure interrupts stay on - need int state save set when interrupts enabled in main
    If Subroutine(0)->IntStateSaveVar <> "" Then
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("1", Subroutine(0)->IntStateSaveVar, "S0"))
    End If
  End If

  'Automatic pin direction setting
  'Create new subroutine to store dir commands
  'Add commands, compile sub, inline code, delete sub
  AutoDirSub = NewSubroutine("SysInitPinDirs")
  LinkedListInsertList(AutoDirSub->CodeStart, GenerateAutoPinDir)
  'If code generated, add it
  If AutoDirSub->CodeStart->Next <> 0 Then
    'Disable verbose mode to stop any output from CompileSubroutine
    OldVBS = VBS: VBS = 0
    'Compile auto pin dir sub
    CompileSubroutine(AutoDirSub)
    VBS = OldVBS
    'Add code from it to main
    CurrLine = LinkedListInsert(CurrLine, ";Automatic pin direction setting")
    CurrLine = LinkedListInsertList(CurrLine, AutoDirSub->CodeStart)
  End If

  'Comment start of main program
  CurrLine = LinkedListInsert(CurrLine, "")
  CurrLine = LinkedListInsert(CurrLine, ";Start_of_the_main_program")

End Sub

Sub AddInterruptCode
  Dim As Integer IntSubLoc, CurrVect
  Dim As LinkedListElement Pointer CurrLine, SubStart, SubEnd

  'If no interrupts are used, no need for any of this
  If Not (UserInt Or SysInt) Then Return

  'Get list of variables to save/restore
  'Need to check every handler to see what registers it uses
  Dim As LinkedListElement Pointer HandlerSubs, SaveVars, CurrCalled, SaveVarPos
  Dim As LinkedListElement Pointer SearchA, SearchB
  Dim As SubType Pointer HandlerSub, CalledSub
  Dim As String RegItem(100), TempData(20), Temp
  Dim As Integer HandlerID, RegItems, CurrReg, SV, CurrBit, DataCount, PCHUsed

  'Update list of sub calls
  RetrySubRequests

  'Make list of all variables that may need saving
  RegItems = 0
  SaveVars = 0
  If AutoContextSave Then
    RegItems += 1: RegItem(RegItems) = "DelayTemp"
    RegItems += 1: RegItem(RegItems) = "DelayTemp2"
    RegItems += 1: RegItem(RegItems) = "SysDivMultX"
    RegItems += 1: RegItem(RegItems) = "SysWaitTempMS"
    RegItems += 1: RegItem(RegItems) = "SysStringB"
    RegItems += 1: RegItem(RegItems) = "SysDivMultX_H"
    RegItems += 1: RegItem(RegItems) = "SysWaitTempMS_H"
    RegItems += 1: RegItem(RegItems) = "SysStringB_H"
    RegItems += 1: RegItem(RegItems) = "SysDivLoop"
    RegItems += 1: RegItem(RegItems) = "SysWaitTemp10MS"
    RegItems += 1: RegItem(RegItems) = "SysWaitTempS"
    RegItems += 1: RegItem(RegItems) = "SysWaitTempUS"
    RegItems += 1: RegItem(RegItems) = "SysWaitTemp10US"
    RegItems += 1: RegItem(RegItems) = "SysWaitTempM"
    RegItems += 1: RegItem(RegItems) = "SysStringLength"
    RegItems += 1: RegItem(RegItems) = "SysWaitTempUS_H"
    RegItems += 1: RegItem(RegItems) = "SysWaitTempH"
    RegItems += 1: RegItem(RegItems) = "SysDivMultA"
    RegItems += 1: RegItem(RegItems) = "SysStringA"
    RegItems += 1: RegItem(RegItems) = "SysDivMultA_H"
    RegItems += 1: RegItem(RegItems) = "SysStringA_H"
    RegItems += 1: RegItem(RegItems) = "SysDivMultB"
    RegItems += 1: RegItem(RegItems) = "SysReadA"
    RegItems += 1: RegItem(RegItems) = "SysDivMultB_H"
    RegItems += 1: RegItem(RegItems) = "SysReadA_H"
    RegItems += 1: RegItem(RegItems) = "SysSignByte"

    RegItems += 1: RegItem(RegItems) = "DataPointer"

    'EEPROM/NVM registers
    If ModePIC Then
      If HasSFR("EEDATA") Then RegItems += 1: RegItem(RegItems) = "EEDATA"
      If HasSFR("EEDAT") Then RegItems += 1: RegItem(RegItems) = "EEDAT"
      If HasSFR("EEDATL") Then RegItems += 1: RegItem(RegItems) = "EEDATL"
      If HasSFR("EEADRH") Then
        RegItems += 1: RegItem(RegItems) = "EEADRH"
        RegItems += 1: RegItem(RegItems) = "EEADRL"
      ElseIf HasSFR("EEADR") Then
        RegItems += 1: RegItem(RegItems) = "EEADR"
      End If
      If HasSFR("NVMCON1") Then RegItems += 1: RegItem(RegItems) = "NVMCON1"
      If HasSFR("NVMADRU") Then RegItems += 1: RegItem(RegItems) = "NVMADRU"
      If HasSFR("NVMADRH") Then RegItems += 1: RegItem(RegItems) = "NVMADRH"
      If HasSFR("NVMADRL") Then RegItems += 1: RegItem(RegItems) = "NVMADRL"
      If HasSFR("NVMDAT") Then RegItems += 1: RegItem(RegItems) = "NVMDAT"
      If HasSFR("NVMCON1") Then RegItems += 1: RegItem(RegItems) = "NVMCON1"

    ElseIf ModeAVR Then
      RegItems += 1: RegItem(RegItems) = "EEDR"
      If HasSFR("EEARH") Then
        RegItems += 1: RegItem(RegItems) = "EEARH"
        RegItems += 1: RegItem(RegItems) = "EEARL"
      Else
        RegItems += 1: RegItem(RegItems) = "EEAR"
      End If
    End If

    'Temporary calculation registers
    'Need to do A, B, X for SysCalcTemp, SysByteTemp, SysWordTemp, SysIntTemp, SysLongTemp
    Dim TempVarLetter(3) As String = {"A", "B", "X"}
    Dim TempVarType(5) As String = {"Calc", "Byte", "Word", "Integer", "Long"}
    Dim As Integer CurrLetter, CurrTypeNo, CurrByte
    Dim As String CurrType, SaveRegName
    For CurrLetter = 0 To 2
      For CurrTypeNo = 0 To 4
        CurrType = TempVarType(CurrTypeNo)
        If CurrType = "Calc" Then CurrType = "Long"
        For CurrByte = 1 To GetTypeSize(CurrType)
          RegItems += 1: RegItem(RegItems) = GetByte("Sys" + TempVarType(CurrTypeNo) + "Temp" + TempVarLetter(CurrLetter), CurrByte - 1)
        Next
      Next
    Next

    If ModePIC Then
      RegItems += 1: RegItem(RegItems) = "SysIFTemp"

      If ChipFamily = 12 Or ChipFamily = 14 Then
        RegItems += 1: RegItem(RegItems) = "FSR"
        RegItems += 1: RegItem(RegItems) = "PCLATH"

      ElseIf ChipFamily = 16 And ChipFamilyVariant <> 1 Then
        '18FxxK42 chips are family variant 1 and automatically save these
        RegItems += 1: RegItem(RegItems) = "PCLATH"
        RegItems += 1: RegItem(RegItems) = "FSR0"
        RegItems += 1: RegItem(RegItems) = "FSR0H"
        RegItems += 1: RegItem(RegItems) = "FSR1"
        RegItems += 1: RegItem(RegItems) = "FSR1H"
      End If
    End If

    'Add calc vars to list
    For SV = 1 to TCVC
      With CalcVars(SV)
        If .MaxType <> "" Then
          For CurrBit = 0 To GetTypeSize(.MaxType) - 1
            RegItems += 1: RegItem(RegItems) = GetByte("SysTemp" + Str(SV), CurrBit)
          Next
        End If
      End With
    Next

    'Get list for all vectors
    SaveVars = LinkedListCreate
    SaveVarPos = SaveVars

    'Search all handlers for registers
    'When CurrVect = 0, check for INTERRUPT sub
    For CurrVect = 0 To IntCount
      HandlerID = 0
      If CurrVect = 0 Then
        HandlerID = LocationOfSub("INTERRUPT", "")   'was capitalised
      Else
        With Interrupts(CurrVect)
          If .Handler <> "" Then
            'Handler is defined for vector
            HandlerID = LocationOfSub(.Handler, "", "")
          End If
        End With
      End If

      If HandlerID > 0 Then
        'Get all subroutines called from handler
        HandlerSub = Subroutine(HandlerID)
        HandlerSubs = GetCalledSubs(HandlerSub)
        LinkedListInsert(HandlerSubs, HandlerSub)
        'Scan through all handler subs
        CurrCalled = HandlerSubs->Next
        Do While CurrCalled <> 0
          CalledSub = CurrCalled->MetaData
          'Search sub lines
          CurrLine = CalledSub->CodeStart->Next
          Do While CurrLine <> 0
            'Search line for variables to back up
            CurrReg = 1
            Do While CurrReg <= RegItems
              If WholeINSTR(CurrLine->Value, RegItem(CurrReg)) = 2 Then
                SaveVarPos = LinkedListInsert(SaveVarPos, RegItem(CurrReg))
                'Remove register from list, no need to keep looking for it
                RegItem(CurrReg) = RegItem(RegItems)
                RegItems -= 1
                CurrReg -= 1
              End If
              CurrReg += 1
            Loop

            'Some PIC only values that will need saving
            If ModePIC Then
              If WholeINSTR(CurrLine->Value, "lfsr") <> 0 Then
                GetTokens(CurrLine->Value, TempData(), DataCount)
                If LCase(TempData(1)) = "lfsr" Then
                  SaveVarPos = LinkedListInsert(SaveVarPos, "FSR" + TempData(2) + "L")
                  SaveVarPos = LinkedListInsert(SaveVarPos, "FSR" + TempData(2) + "H")
                End If
              ElseIf WholeINSTR(CurrLine->Value, "AFSR") = 1 Then
                Temp = Mid(CurrLine->Value, InStr(UCase(CurrLine->Value), "AFSR"))
                If Mid(Temp, 7, 1) = "H" Then
                  Temp = "FSR" + Mid(Temp, 5, 1) + Mid(Temp, 7, 1)
                Else
                  Temp = "FSR" + Mid(Temp, 5, 1) + "L"
                End If
                SaveVarPos = LinkedListInsert(SaveVarPos, Temp)
              End If
            End If

            CurrLine = CurrLine->Next
          Loop
          CurrCalled = CurrCalled->Next
        Loop

      End If
    Next

    'Always save PCLATH
    PCHUsed = 0
    If ChipFamily = 14 Or ChipFamily = 15 Then
      If HasSFR("PCLATH") Then
        SaveVarPos = LinkedListInsert(SaveVarPos, "PCLATH")
        PCHUsed = -1
      End If
    End If

    'Delete variables that share location with another saved var
    SearchA = SaveVars->Next
    Do While SearchA <> 0
      SearchB = SearchA->Next
      Do While SearchB <> 0
        'A and B share location, remove B
        If GetRegisterLoc(SearchA->Value) = GetRegisterLoc(SearchB->Value) Then
          If (Left(SearchA->Value, 7) <> "SysTemp" And GetRegisterLoc(SearchA->Value) <> -1) Or _
            LCase(SearchA->Value) = LCase(SearchB->Value) Then
            SearchB = LinkedListDelete(SearchB)
          End If
        End If

        SearchB = SearchB->Next
      Loop
      SearchA = SearchA->Next
    Loop
    'Should now have a list of variables that need to be backed up before interrupt
  End If

  If ModePIC Then
    'On PIC, need to add context save and On Int to start of interrupt routine
    'and add context restore at the end

    'Get interrupt sub
    IntSubLoc = LocationOfSub("INTERRUPT", "")   'was capitalised
    If IntSubLoc = 0 Then
      SBC += 1
      Subroutine(SBC) = NewSubroutine("INTERRUPT")   'was capitalised
      IntSubLoc = SBC
      'Mark as required and compiled (then only insert asm, no BASIC allowed)
      With *Subroutine(SBC)
        '.Required = -1
        .Compiled = -1
      End With
    End If
    With *Subroutine(IntSubLoc)
      'Interrupt sub must go on first page
      .FirstPage = -1
      'Interrupt sub doesn't need return added
      .NoReturn = -1
      'Subroutine is required
      .Required = -1

      'Get code start
      SubStart = .CodeStart
    End With
    CurrLine = SubStart

    'Add context save code
    If AutoContextSave Then
      AddVar "SysW", "BYTE", 1, 0, "REAL", "", , -1
      AddVar "SysSTATUS", "BYTE", 1, 0, "REAL", "", , -1

      'Variables to store registers
      SaveVarPos = SaveVars->Next
      Do While SaveVarPos <> 0
        AddVar("Save" + SaveVarPos->Value, "BYTE", 1, 0, "REAL", "", , -1)
        SaveVarPos = SaveVarPos->Next
      Loop
      If ChipFamily = 14 Or ChipFamily = 15 Then
        'Will need to put SysW, SysSTATUS into shared bank
        CurrLine = LinkedListInsert(CurrLine, ";Save Context")
        CurrLine = LinkedListInsert(CurrLine, " movwf SysW")
        CurrLine = LinkedListInsert(CurrLine, " swapf STATUS,W")
        CurrLine = LinkedListInsert(CurrLine, " movwf SysSTATUS")
        If ChipFamily = 15 Then
          AddVar "SysBSR", "BYTE", 1, 0, "REAL", "", , -1
          CurrLine = LinkedListInsert(CurrLine, " movf BSR,W")
          CurrLine = LinkedListInsert(CurrLine, " banksel STATUS")
          CurrLine = LinkedListInsert(CurrLine, " movwf SysBSR")
        Else
          CurrLine = LinkedListInsert(CurrLine, " banksel STATUS")
        End If
        CurrLine = LinkedListInsert(CurrLine, ";Store system variables")
        SaveVarPos = SaveVars->Next
        Do While SaveVarPos <> 0
          CurrLine = LinkedListInsert(CurrLine, " movf " + SaveVarPos->Value + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf Save" + SaveVarPos->Value)
          SaveVarPos = SaveVarPos->Next
        Loop
        If PCHUsed Then
          CurrLine = LinkedListInsert(CurrLine, " clrf PCLATH")
        End If
      ElseIf ChipFamily = 16 Then
        AddVar "SysBSR", "BYTE", 1, 0, "REAL", "", , -1

        If ChipFamilyVariant = 1 then
          CurrLine = LinkedListInsert(CurrLine, ";Use Automatic Context Save for K42 and K83 with MVECEN = OFF.  Interrupt priority not supported")
        Else
          CurrLine = LinkedListInsert(CurrLine, ";Save Context")
          CurrLine = LinkedListInsert(CurrLine, " movff WREG,SysW")
          CurrLine = LinkedListInsert(CurrLine, " movff STATUS,SysSTATUS")
          CurrLine = LinkedListInsert(CurrLine, " movff BSR,SysBSR")
        End If
        CurrLine = LinkedListInsert(CurrLine, ";Store system variables")
        SaveVarPos = SaveVars->Next
        Do While SaveVarPos <> 0
          CurrLine = LinkedListInsert(CurrLine, " movff " + SaveVarPos->Value + ",Save" + SaveVarPos->Value)
          SaveVarPos = SaveVarPos->Next
        Loop
      End If
    End If

    'Add On Interrupt generated code
    CurrLine = LinkedListInsert(CurrLine, ";On Interrupt handlers")
    CurrLine = LinkedListInsertList(CurrLine, IntHandlerCode->CodeList)
    CurrLine = LinkedListInsert(CurrLine, ";User Interrupt routine")

    'Get last line in routine
    Do While CurrLine <> 0
      SubEnd = CurrLine
      CurrLine = CurrLine->Next
    Loop

    'Add context restore code
    CurrLine = SubEnd
    CurrLine = LinkedListInsert(CurrLine, "INTERRUPTDONE")  'PIC
    GetMetaData(Currline)->IsLabel = -1

    If AutoContextSave Then
      CurrLine = LinkedListInsert(CurrLine, ";Restore Context")
      If ChipFamily = 14 Or ChipFamily = 15 Then
        CurrLine = LinkedListInsert(CurrLine, ";Restore system variables")
        SaveVarPos = SaveVars->Next
        Do While SaveVarPos <> 0
          CurrLine = LinkedListInsert(CurrLine, " movf Save" + SaveVarPos->Value + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + SaveVarPos->Value)
          SaveVarPos = SaveVarPos->Next
        Loop
        If ChipFamily = 15 Then
          AddVar "SysBSR", "BYTE", 1, 0, "REAL", ""
          CurrLine = LinkedListInsert(CurrLine, " movf SysBSR,W")
          CurrLine = LinkedListInsert(CurrLine, " movwf BSR")
        End If
        CurrLine = LinkedListInsert(CurrLine, " swapf SysSTATUS,W")
        CurrLine = LinkedListInsert(CurrLine, " movwf STATUS")
        CurrLine = LinkedListInsert(CurrLine, " swapf SysW,F")
        CurrLine = LinkedListInsert(CurrLine, " swapf SysW,W")
        CurrLine = LinkedListInsert(CurrLine, " retfie")
      ElseIf ChipFamily = 16 Then
        CurrLine = LinkedListInsert(CurrLine, ";Restore system variables")
        SaveVarPos = SaveVars->Next
        Do While SaveVarPos <> 0
          CurrLine = LinkedListInsert(CurrLine, " movff Save" + SaveVarPos->Value + "," + SaveVarPos->Value)
          SaveVarPos = SaveVarPos->Next
        Loop

        If ChipFamilyVariant = 1 then
          CurrLine = LinkedListInsert(CurrLine, " retfie 1")
        else
          CurrLine = LinkedListInsert(CurrLine, " movff SysW,WREG")
          CurrLine = LinkedListInsert(CurrLine, " movff SysSTATUS,STATUS")
          CurrLine = LinkedListInsert(CurrLine, " movff SysBSR,BSR")
          CurrLine = LinkedListInsert(CurrLine, " retfie 0")
        end if
      End If

    Else
      'Do not save context, return
      If ChipFamily = 14 Or ChipFamily = 15 Then
        CurrLine = LinkedListInsert(CurrLine, " retfie")
      ElseIf ChipFamily = 16 Then
        CurrLine = LinkedListInsert(CurrLine, " retfie 0")
      End If
    End If

  ElseIf ModeAVR Then
    'On AVR, need to add subs for context save/restore, and set up vectors to
    'call correct handlers

    If AutoContextSave Then
      'Add variables
      AddVar("SaveSysValueCopy", "BYTE", 1, 0, "REAL", "", , -1)
      AddVar("SaveSREG", "BYTE", 1, 0, "REAL", "", , -1)

      'Variables to store registers
      SaveVarPos = SaveVars->Next
      Do While SaveVarPos <> 0
        AddVar("Save" + SaveVarPos->Value, "BYTE", 1, 0, "REAL", "", , -1)
        SaveVarPos = SaveVarPos->Next
      Loop

      'Interrupt subs must go on first page
      'Create context save sub
      SBC += 1: Subroutine(SBC) = NewSubroutine("SysIntContextSave")
      'Set up flags (required, not compiled, needs first page, needs return)
      With *Subroutine(SBC)
        .Required = -1
        .Compiled = 0
        .FirstPage = -1
        .NoReturn = 0
        SubStart = .CodeStart
      End With
      CurrLine = SubStart
      'Add context save code
      CurrLine = LinkedListInsert(CurrLine, ";Store SysValueCopy")
      CurrLine = LinkedListInsert(CurrLine, " sts SaveSysValueCopy,SysValueCopy")
      CurrLine = LinkedListInsert(CurrLine, ";Store SREG")
      CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy,SREG")
      CurrLine = LinkedListInsert(CurrLine, " sts SaveSREG,SysValueCopy")
      'Store all registers
      CurrLine = LinkedListInsert(CurrLine, ";Store registers")
      SaveVarPos = SaveVars->Next
      Do While SaveVarPos <> 0
        CurrLine = LinkedListInsert(CurrLine, " sts Save" + SaveVarPos->Value + "," + SaveVarPos->Value)
        SaveVarPos = SaveVarPos->Next
      Loop
      CompileSubroutine(Subroutine(SBC))

      'Create context restore sub
      SBC += 1: Subroutine(SBC) = NewSubroutine("SysIntContextRestore")
      'Set up flags (required, not compiled, needs first page, no return)
      With *Subroutine(SBC)
        .Required = -1
        .Compiled = 0
        .FirstPage = -1
        .NoReturn = -1
        SubStart = .CodeStart
      End With
      CurrLine = SubStart
      'Add context restore code
      'Restore all registers
      CurrLine = LinkedListInsert(CurrLine, ";Restore registers")
      SaveVarPos = SaveVars->Next
      Do While SaveVarPos <> 0
        CurrLine = LinkedListInsert(CurrLine, " lds " + SaveVarPos->Value + ",Save" + SaveVarPos->Value)
        SaveVarPos = SaveVarPos->Next
      Loop
      CurrLine = LinkedListInsert(CurrLine, ";Restore SREG")
      CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy,SaveSREG")
      CurrLine = LinkedListInsert(CurrLine, " out SREG,SysValueCopy")
      CurrLine = LinkedListInsert(CurrLine, ";Restore SysValueCopy")
      CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy,SaveSysValueCopy")
      CurrLine = LinkedListInsert(CurrLine, " reti")
      'Compile the sub
      CompileSubroutine(Subroutine(SBC))
    End If

    'Create subs for On Interrupt events
    For CurrVect = 1 To IntCount
      With Interrupts(CurrVect)
        If .Handler <> "" Then
          SBC += 1: Subroutine(SBC) = NewSubroutine("Int" + UCase(.Vector))
          Subroutine(SBC)->Compiled = -1
          Subroutine(SBC)->FirstPage = -1
          Subroutine(SBC)->Required = -1
          Subroutine(SBC)->NoReturn = -1
          CurrLine = Subroutine(SBC)->CodeStart

          If AutoContextSave Then CurrLine = LinkedListInsert(CurrLine, " rcall SysIntContextSave")
          CurrLine = LinkedListInsert(CurrLine, " rcall " + .Handler)
          If .FlagBit <> "" Then
            CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(.FlagBit, "0", ""))
          End If
          If AutoContextSave Then
            CurrLine = LinkedListInsert(CurrLine," rjmp SysIntContextRestore")
          Else
            CurrLine = LinkedListInsert(CurrLine, " reti")
          End If
        End If
      End With
    Next

    'Add appropriate return and context save and restore to user Interrupt routine
    IntSubLoc = LocationOfSub("INTERRUPT", "")   'was capitalised
    If IntSubLoc <> 0 Then
      'No return needed
      Subroutine(IntSubLoc)->NoReturn = -1

      'Add save
      CurrLine = Subroutine(IntSubLoc)->CodeStart
      If AutoContextSave Then CurrLine = LinkedListInsert(CurrLine, " rcall SysIntContextSave")

      'Add context restore code
      Do While CurrLine <> 0
        SubEnd = CurrLine
        CurrLine = CurrLine->Next
      Loop
      CurrLine = SubEnd
      CurrLine = LinkedListInsert(CurrLine, "INTERRUPTDONE:")  'AVR
      If AutoContextSave Then
        CurrLine = LinkedListInsert(CurrLine," rjmp SysIntContextRestore")
      Else
        CurrLine = LinkedListInsert(CurrLine, " reti")
      End If

    End If

  ElseIf ModeZ8 Then
    'Z8 can use vectors, so same as AVR

  End If

  'Delete save list
  If SaveVars <> 0 Then
    LinkedListDeleteList(SaveVars, 0)
  End If

End Sub

Sub AddSysVarBits (CompSub As SubType Pointer)
  Dim As String TempData, BitName
  Dim As Integer PD, T, SV, LineChanged

  Dim As LinkedListElement Pointer CurrLine
  Dim As LinkedListElement Pointer LineElements, CurrElement
  Dim As SysVarType Pointer CurrVar

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    TempData = CurrLine->Value
    IF INSTR(TempData, ".") <> 0 And InStr(TempData, "=") = 0 Then GOTO AddNextLineBits
    IF Left(TempData, 1) = " " THEN GOTO AddNextLineBits

    'Only use for some commands
    T = 0
    IF Left(TempData, 4) = "SET " THEN T = 1
    IF Left(TempData, 4) = "DIR " THEN T = 1
    IF Left(TempData, 3) = "IF " THEN T = 1
    IF Left(TempData, 3) = "DO " THEN T = 1
    IF Left(TempData, 5) = "LOOP " THEN T = 1
    IF INSTR(TempData, "=") <> 0 THEN T = 1
    IF Left(TempData, 11) = "WAIT UNTIL " THEN T = 1
    IF Left(TempData, 11) = "WAIT WHILE " THEN T = 1
    IF T = 0 THEN GOTO AddNextLineBits

    LineElements = GetElements(TempData, , -1)
    LineChanged = 0
    CurrElement = LineElements->Next
    Do While CurrElement <> 0
      CurrVar = HashMapGet(SysVarBits, CurrElement->Value)
      If CurrVar <> 0 Then
        'Found SFR bit, should have . before it, or else we need to add it
        If CurrElement->Prev->Value <> "." Then
          IF UCase(CurrElement->Value) <> "DIR" AND Left(TempData, 4) <> "DIR " Then
            LinkedListInsert(CurrElement->Prev, CurrVar->Parent + ".")
            LineChanged = -1
          End If
        End If
      End If

      CurrElement = CurrElement->Next
    Loop

    If LineChanged Then
      CurrLine->Value = ""
      CurrElement = LineElements->Next
      Do While CurrElement <> 0
        CurrLine->Value = CurrLine->Value + CurrElement->Value
        CurrElement = CurrElement->Next
      Loop
    End If

    AddNextLineBits:

    CurrLine = CurrLine->Next
  Loop

END SUB

SUB BuildMemoryMap

  'Dimension arrays
  REDIM FreeMem (MemSize + 10) As Integer
  REDIM VarLoc (MemSize + 10) As Integer
  Dim As String TempData
  Dim As Integer PD, Range, Min, Max, L, T, CheckNonBanked

  'Mark all locations as used
  For PD = 0 To MemSize + 10
    FreeMem(PD) = 1
  Next


  'Mark locations as free if specified by a range
  'Do not mark non-banked locations as free yet
  For Range = 1 to MRC
    TempData = MemRanges(Range)
    Min = VAL("&h" + Left(TempData, INSTR(TempData, ":") - 1))
    Max = VAL("&h" + Mid(TempData, INSTR(TempData, ":") + 1))

    For L = Min To Max
      If DATfileinspection = True Then
        If L > MemSize then
            LogError "DAT file error - probable `MaxAddress = "+ str(MemSize)+"` is incorrect/too small. Check DAT with DATASHEET to correct error"
            exit sub
        End if
      End if
      'On 16F1 chips, keep non-banked locations at end of list to make allocation of linear memory simpler
      If ChipFamily = 15 Then

        For CheckNonBanked = 1 To NoBankLocs
          If L >= NoBankLoc(CheckNonBanked).StartLoc And L <= NoBankLoc(CheckNonBanked).EndLoc Then
            FreeMem(L) = 2
            GoTo LocationIsNonBanked
          End If
        Next
      End If

      FreeMem(L) = 0
      LocationIsNonBanked:
    Next
  Next

  'Produce list of free memory locations
  T = 0
  'Banked locations first

  For PD = 1 To MemSize
    If FreeMem(PD) = 0 Then
      T += 1
      VarLoc(T) = PD
      If T = ChipRam Then Exit For
    End If
  Next
  'Then non-banked locations
  If ChipFamily = 15 Then
    For PD = 1 To MemSize
      If FreeMem(PD) = 2 Then
        T += 1
        VarLoc(T) = PD
        If T = ChipRam Then Exit For
      End If
    Next
  End If
  FreeRAM = T

End SUB

SUB CalcConfig

  'Process config flags from CONFIG variable and programmer required settings
  'Store required settings in OutConfig array

  'No config registers on AVR that can be set by GCBASIC
  If ModeAVR Then
    If NOT CHIPAVRDX Then
      Exit Sub
    End If
  End If

  'Do not set CONFIG if TBL+
  If ConfigDisabled Then Exit Sub

  Dim As String CurrItem, CurrName, CurrVal, Temp, LFINTOSCString
  Dim As Integer CurrWord, CurrConfConst, CurrSpeed
  Dim As LinkedListElement Pointer UserSettingList, UserSettingLoc, CodeLoc
  Dim As LinkedListElement Pointer CurrSettingLoc, CurrSettingOptLoc
  Dim As ConfigSetting Pointer CurrSetting

  Dim As Integer configreport
  Dim As String ConfigReportFileName
  'Set default - this is needed as some dont use the same LF INT OSC
  LFINTOSCString = "LFINT"


  'AVRDX Support
    'Check for internal osc
    If IntOscSpeeds <> 0 Then
    For CurrSpeed = 1 To IntOscSpeeds
      If CSng(ChipMhz) = CSng(IntOscSpeed(CurrSpeed)) Then
        AddConstant("CHIPUSINGINTOSC", "TRUE")
        Exit For
      End If
    Next
    End If
    If CHIPAVRDX Then 
      Exit Sub
    End If


  'Read config
  Do WHILE INSTR(CONFIG, "&") <> 0: Replace CONFIG, "&", ",": Loop
  'Split line into elements, put into list
  UserSettingList = LinkedListCreate
  UserSettingLoc = UserSettingList
  Do While InStr(Config, ",") <> 0
    UserSettingLoc = LinkedListInsert(UserSettingLoc, Trim(Left(Config, InStr(Config, ",") - 1)))
    Config = Trim(Mid(Config, InStr(Config, ",") + 1))
  Loop
  If Config <> "" Then
    UserSettingLoc = LinkedListInsert(UserSettingLoc, Config)
  End If

  'Add config for programmer
  If PrgTool <> 0 Then
    If PrgTool->ProgConfig <> "" Then
      Config = PrgTool->ProgConfig
      Do While InStr(Config, ",") <> 0
        UserSettingLoc = LinkedListInsert(UserSettingLoc, Trim(Left(Config, InStr(Config, ",") - 1)))
        Config = Trim(Mid(Config, InStr(Config, ",") + 1))
      Loop
      If Config <> "" Then
        UserSettingLoc = LinkedListInsert(UserSettingLoc, Config)
      End If
    End If
  End If

  'Search through list of user entered config settings
  'Then find a match in the list of valid settings for the PIC
  'If no match found, show error

  'Get a user setting
  UserSettingLoc = UserSettingList->Next
  Do While UserSettingLoc <> 0
    'Get a setting name
    CurrSettingLoc = ConfigSettings->Next
    Do While CurrSettingLoc <> 0
      CurrSetting = CurrSettingLoc->MetaData
      With (*CurrSetting)
        'Check for setting name in user setting
        If ConfigNameMatch(UserSettingLoc->Value, .Name) Then
          'Print "Name match for " + .Name
          'If found, find matching setting value
          CurrSettingOptLoc = .Options
          Do While CurrSettingOptLoc <> 0

            If ConfigValueMatch(CurrSettingOptLoc->Value, UserSettingLoc->Value) <> 0 Then
              'Matching setting value found
              'Print "Found match for: "; UserSettingLoc->Value; ", "; .Name; " = "; CurrSettingOptLoc->Value
              .Setting = CurrSettingOptLoc
              If ConfigValueMatch(CurrSettingOptLoc->Value, UserSettingLoc->Value) = -2 Then GoTo ProcessNextSetting
            End If
            CurrSettingOptLoc = CurrSettingOptLoc->Next
          Loop

          If .Setting <> 0 Then GoTo ProcessNextSetting
        End If

      End With
      CurrSettingLoc = CurrSettingLoc->Next
    Loop

    'If setting has been found, will jump to ProcessNextSetting
    'Only way to get here is if a setting hasn't been found
    'Print "Bad setting: " + UserSettingLoc->Value
    Temp = Message("BadConfig")
    Replace Temp, "%option%", UserSettingLoc->Value
    LogWarning(Temp, "")

    ProcessNextSetting:
    UserSettingLoc = UserSettingLoc->Next
  Loop

  'Add default CONFIG options
  'Find settings with nothing specified
  Dim As String DesiredSetting
  'Check here as I could not find better place for it!
  IntOscSpeedValid = 0
  If CSng(ChipMHz) = CSng(0.031) Then
    If _31kSupported.State = -1 Then
      DesiredSetting = _31kSupported.OSCType
      IntOscSpeedValid = 1
    End if
  End If

  CurrSettingLoc = ConfigSettings->Next
  Do While CurrSettingLoc <> 0
    CurrSetting = CurrSettingLoc->MetaData
    With (*CurrSetting)
      If .Setting = 0 Then
        'Get the desired default setting
        DesiredSetting = ""
        If ConfigNameMatch(.Name, "MCLR") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "LVP") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "WDT") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "PLLEN") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "CPUDIV") Then
          DesiredSetting = "NOCLKDIV"
        ElseIf ConfigNameMatch(.Name, "MVECEN") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "FEXTOSC") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "CP") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "CPD") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "CLKOUTEN") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "VCAPEN") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "WRTD") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "XINST") Then
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "WRT") Then   'this was generating a config error in non GCASM implementations
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "FCMEN") Then   'this was generating a config error in non GCASM implementations
          DesiredSetting = "ON"
        ElseIf ConfigNameMatch(.Name, "WRTSAF") Then   'this was generating a config error in non GCASM implementations
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "WRTB") Then     'this was generating a config error in non PICAS implementations
          DesiredSetting = "OFF"
        ElseIf ConfigNameMatch(.Name, "VDDIO2MD") Then     'new for Q20 chips
          DesiredSetting = "STANDARD_RANGE"
        ElseIf ConfigNameMatch(.Name, "VDDIO3MD") Then     'new for Q20 chips
          DesiredSetting = "STANDARD_RANGE"

        ElseIf ConfigNameMatch(.Name, "OSC") Then
          'Get setting from #osc directive

          IF OSCType <> "" Then
            DesiredSetting = OSCType
          Else

            'No oscillator chosen, need to find best option
            'If ChipMhz = IntOscSpeed(x), use Int Osc
            'If ChipMhz > 4, use HS
            'If 4 > ChipMhz > ChipIntOsc, use XT
            'Check for internal osc
            If IntOscSpeeds <> 0 Then
              For CurrSpeed = 1 To IntOscSpeeds
                If CSng(ChipMhz) = CSng(IntOscSpeed(CurrSpeed)) Then
                  DesiredSetting = "INT"
                  Exit For
                End If
              Next
            End If

            If CSng(ChipMHz) = CSng(0.031) Then
              If _31kSupported.State = -1 Then
                DesiredSetting = _31kSupported.OSCType
                IntOscSpeedValid = 1
              End if
            End If

            'Can't use int osc, come up with suitable ext osc
            If DesiredSetting = "" Then
              If ChipMhz > 4 Then
                DesiredSetting = "HS"
              Else
                DesiredSetting = "XT"
              End If
            End If

            ChipOscSource = DesiredSetting
          End If

        End If

        'If there is a default, use it
        If DesiredSetting <> "" Then
          CurrSettingOptLoc = .Options
          Do While CurrSettingOptLoc <> 0      
            'Select correct LFINT.. if this chip is LFINTOSC
            'print CurrSettingOptLoc->Value, WholeINSTR(CurrSettingOptLoc->Value, "LFINTOSC" )
            If WholeINSTR(CurrSettingOptLoc->Value, "LFINTOSC" ) = 2 Then
              If DesiredSetting = "LFINT" then
                DesiredSetting = "LFINTOSC"
              end if
            end if
            If ConfigValueMatch(CurrSettingOptLoc->Value, DesiredSetting) Then
              'Matching setting value found
              .Setting = CurrSettingOptLoc
              If ConfigValueMatch(CurrSettingOptLoc->Value, DesiredSetting) = -2 Then Exit Do
                End If
            CurrSettingOptLoc = CurrSettingOptLoc->Next
          Loop
        End If

      End If
    End With
    CurrSettingLoc = CurrSettingLoc->Next
  Loop

  'Publish config
  If OutPutConfigOptions = 0 then
    configreport = FreeFile
    ConfigReportFileName = ReplaceToolVariables("%filename%", "config")
    Open ConfigReportFileName For Output As #configreport
    IF HasSFR ( "PPSLOCK" ) then
      Print #configreport, "[" + ChipName + "]"+str(ModePIC)+","+str(ModeAVR)+",PPS,"+str(chipfamily)
    else
      Print #configreport, "[" + ChipName + "]"+str(ModePIC)+","+str(ModeAVR)+",,"+str(chipfamily)
    end if
  End If

  'Check oscillator type, add constant
  CurrSettingLoc = ConfigSettings->Next
  Do While CurrSettingLoc <> 0
    CurrSetting = CurrSettingLoc->MetaData
    With (*CurrSetting)
      If ConfigNameMatch(.Name, "OSC") and not ( ConfigNameMatch(.Name, "SOSCSEL") or ConfigNameMatch(.Name, "LPT1OSC")  or ConfigNameMatch(.Name, "INTOSC_XT") ) Then
        ChipOscSource = .Setting->Value
        AddConstant("CHIPOSC", ChipOscSource)
        'Cache the ChipOscSource value @1121
        'The purpose is to support INT OSC with CLOCKOUT - the initsys block of code works for CLOCK and NOCLOCK so, cache the value, treat as NOCLOCK and restore the config setting
        Dim ChipOscSourceCache as String = ""
        If ChipOscSource = "INTRC_OSC_CLKOUT" Then
          ChipOscSourceCache = ChipOscSource
          ChipOscSource = "INTRC_OSC_NOCLKOUT"
        End if
        If ConfigValueMatch(ChipOscSource, "INT", -1) Then
          'Check for internal osc
          If IntOscSpeeds <> 0 Then
            For CurrSpeed = 1 To IntOscSpeeds
              If CSng(ChipMhz) = CSng(IntOscSpeed(CurrSpeed)) Then
                IntOscSpeedValid = 1
                Exit For
              End If
            Next
          End If
          'Restore the cached value @1121
          IF ChipOscSourceCache <> "" Then
            ChipOscSource = ChipOscSourceCache
          End If
          If IntOscSpeedValid = 1 then
            AddConstant("CHIPUSINGINTOSC", "TRUE")
          else
            LogWarning(Message("BadIntFreq"))
          End if
        End If
        Exit Do
      End If
    End With
    CurrSettingLoc = CurrSettingLoc->Next
  Loop


  'Store config
  'PIC 10/12/16 format
  If ChipFamily <> 16 Then

    'Find all settings
    CurrSettingLoc = ConfigSettings->Next
    Do While CurrSettingLoc <> 0
      CurrSetting = CurrSettingLoc->MetaData
      With (*CurrSetting)
        'If a setting has been set, need to write out
        If .Setting <> 0 Then

          'Find where setting came from
          'Print .Name, .Setting->Value
          For CurrConfConst = 1 To COC
            If ConfigNameMatch(ConfigOps(CurrConfConst).OP, .Name) And _
              ConfigValueMatch(ConfigOps(CurrConfConst).OP, .Setting->Value) Then
              'Store setting
              CurrWord = ConfigOps(CurrConfConst).Loc
              If OutConfig(CurrWord) = "" Then
                OutConfig(CurrWord) = "_" + ConfigOps(CurrConfConst).Op
                If OutPutConfigOptions = 0 then
                  Print #configreport, ConfigOps(CurrConfConst).Op
                End if
              Else
                OutConfig(CurrWord) = OutConfig(CurrWord) + " & _" + ConfigOps(CurrConfConst).Op
                If OutPutConfigOptions = 0 then
                  Print #configreport, ConfigOps(CurrConfConst).Op
                End if
              End If

              Exit For
            End If
          Next

        End If
      End With


      CurrSettingLoc = CurrSettingLoc->Next
    Loop

    'Write code
    'Single config word
    If ConfWords = 1 Then
      CodeLoc = LinkedListInsert(ChipConfigCode->CodeList, " __CONFIG " + OutConfig(1))
      GetMetaData(CodeLoc)->RawConfig = OutConfig(1)

    'Multiple config words
    Else
      CodeLoc = ChipConfigCode->CodeList
      For CurrWord = 1 To ConfWords
        'Only write non-blank words
        If OutConfig(CurrWord) <> "" Then
          CodeLoc = LinkedListInsert(CodeLoc, " __CONFIG _CONFIG" + Str(CurrWord) + ", " + OutConfig(CurrWord))
          GetMetaData(CodeLoc)->RawConfig = OutConfig(CurrWord)
        End If
      Next
    End If

  'PIC 18 format
  Else
    OutConfig(1) = ""

    'Use OSC or FOSC?
    Dim As Integer FOSC
    FOSC = 0
    For CurrConfConst = 1 To COC
      With ConfigOps(CurrConfConst)
        If Left(.Op, 5) = "FOSC_" Then
          FOSC = -1
          Exit For
        End If
      End With
    Next

    'Find all settings
    CurrSettingLoc = ConfigSettings->Next
    Do While CurrSettingLoc <> 0
      CurrSetting = CurrSettingLoc->MetaData
      With (*CurrSetting)
        'If a setting has been set, need to write out
        If .Setting <> 0 Then

          If FOSC Then
            If .Name = "OSC" Then .Name = "FOSC"
          End If
          ' as MVECEN is not supported (see Interrupt handler section).
          If ucase(trim(.Name)) = "MVECEN" and ucase(trim(.Setting->Value)) = "ON" Then
              Temp = Message("BadConfig")
              Replace Temp, "%option%", .Name + "=" + .Setting->Value + " not supported by compiler.  Use MVECEN = OFF or remove from config statement"
              LogWarning(Temp, "")
          End If

          If OutConfig(1) = "" Then
            OutConfig(1) = .Name + " = " + .Setting->Value
            If OutPutConfigOptions = 0 then
              Print  #configreport, .Name + " = " + .Setting->Value
            End if
          Else
            OutConfig(1) = OutConfig(1) + ", " + .Name + " = " + .Setting->Value
            If OutPutConfigOptions = 0 then
              Print #configreport, .Name + " = " + .Setting->Value
            End if
          End If
        End If
      End With
      CurrSettingLoc = CurrSettingLoc->Next
    Loop
    LinkedListInsert(ChipConfigCode->CodeList, " CONFIG " + OutConfig(1))

  End If

  If OutPutConfigOptions = 0 then
    Close #configreport
  End if

END SUB

Sub CalcOps (OutList As CodeSection Pointer, SUM As String, AV As String, Ops As String, OriginIn As String, NeverLast As Integer)

  Dim As String Origin, Temp
  Dim As String Answer, ActN, AnswerIn, V1, V2, TypeV1, TypeV2, CalcType, TypeAV, Act
  Dim As Integer UnaryMode, SearchStart, CalcisBad, OpPos, SD, SO
  Dim As Integer CalcStart, CalcEnd, LastCalc, NextSame, PD
  Dim As SubType Pointer CurrentSub, DestSub

  Dim As LinkedListElement Pointer NewCode

  If (( compilerdebug and cCALCOPS ) = cCALCOPS )  Then
    Print " 4 CALCOPS origin: " + OriginIn, SUM, ops, av
  End If
  Origin = OriginIn
  CurrentSub = Subroutine(GetSubID(Origin))
  IF INSTR(Origin, "D") <> 0 Then DestSub = Subroutine(GetDestSub(Origin)) Else DestSub = CurrentSub

  UnaryMode = 0
  If Left(Ops, 1) = "U" Then
    UnaryMode = -1
    Ops = Mid(Ops, 2)
  End If

  SearchStart = 1
SearchForOpAgain:
  'Initialise
  CalcIsBad = 0
  Answer = ""

  'Find operator
  OpPos = 0
  Act = ""
  FOR SD = SearchStart to LEN(SUM)
    Temp = "": IF SD > 1 Then Temp = Mid(SUM, SD - 1, 1)
    FOR SO = 1 to LEN(Ops)
      If (Not UnaryMode And Mid(SUM, SD, 1) = Mid(Ops, SO, 1)) Or (UnaryMode And Mid(SUM, SD, 1) = Mid(Ops, SO, 1) And IsCalcDivider(Temp)) Then
        Act = Mid(Ops, SO, 1)
        OpPos = SD
        'Exit For
        GoTo FoundOp
      End If
    Next
  Next
  FoundOp:

  'If operator not found, exit sub
  If OpPos = 0 Then Exit Sub

  'Get normal name for action
  ActN = Act
  If ActN = "{" Then ActN = "<="
  If ActN = "}" Then ActN = ">="
  If ActN = "~" Then ActN = "<>"

  'Get left operand
  V1 = ""
  CalcStart = 0
  If OpPos = 1 Or UnaryMode Then
    CalcStart = OpPos
  Else
    For SD = OpPos - 1 to 0 step -1
      If SD = 0 Then
        V1 = Left(SUM, OpPos - 1)
        CalcStart = 1
        Exit For
      End If
      If IsCalcDivider(Mid(SUM, SD, 1)) Then
        If Mid(SUM, SD, 1) <> "-" Or (Not IsCalcDivider(Mid(SUM, SD - 1, 1)) And SD > 1) Then
          V1 = Left(SUM, OpPos - 1)
          V1 = Mid(V1, SD + 1)
          CalcStart = SD + 1
          Exit For
        End If
      End If
    Next
  End If

  'Get right operand
  V2 = ""
  CalcEnd = OpPos
  For SD = OpPos + 2 to (LEN(SUM) + 1)
    If SD > LEN(SUM) Then
      V2 = Mid(SUM, OpPos + 1)
      CalcEnd = LEN(SUM)
      Exit For
    End If
    If IsCalcDivider(Mid(SUM, SD, 1)) Then
      V2 = Left(SUM, SD - 1)
      V2 = Mid(V2, OpPos + 1)
      CalcEnd = SD - 1
      Exit For
    End If
  Next

  'Check syntax
  CalcIsBad = 0
  If V1 = "" And Not UnaryMode Then
    If Act = "-" Then
      'If act is -, may have a negative literal
      SearchStart = OpPos + 1
      GoTo SearchForOpAgain
    Else
      Temp = Message("MissingOperand")
      Replace Temp, "%pos%", "before"
      Replace Temp, "%operator%", ActN
      LogError Temp, Origin
      CalcIsBad = -1
    End If
  End If
  If V2 = "" Then
    Temp = Message("MissingOperand")
    Replace Temp, "%pos%", "after"
    Replace Temp, "%operator%", ActN
    LogError Temp, Origin
    'Print SUM, Origin, Temp
    CalcIsBad = -1
  End If

  'If calc is bad, quit and try next operator
  If CalcIsBad Then
    SUM = Left(SUM, CalcStart - 1) + "SynErr" + Mid(SUM, CalcEnd + 1)
    Goto SearchForOpAgain
  End If

  'Get input types
  TypeV1 = TypeOfValue(V1, Subroutine(GetSubID(Origin)))
  TypeV2 = TypeOfValue(V2, Subroutine(GetSubID(Origin)))
  If UnaryMode Then TypeV1 = ""
  TypeAV = TypeOfValue(AV, Subroutine(GetDestSub(Origin)))

  '[1113]
  'Resolves match =  times_array(1) + 2 >= times_array(3) error
  ' where the times_array(1) + 2 is treated a SYSTEMP byte variable  when the array is a word element array .. thus messes up the ASM and the users code.
  ' the cause of the issue is that the 'TypeAV = TypeOfValue(AV, Subroutine(GetDestSub(Origin)))' above returns a byte not a word
  ' so, this code tests that TypeAV is not empty, when empty then the compiler has not determine a type, so, it is not of interest
  ' then, it test for > or M then this is a calulation that could impact the type cast.
  ' If all these tests are valid, then simply choose the largest of the two variables.


  If TypeAV <> "" Then
    If TypeV1 <> "BYTE" OR TypeV2 <> "BYTE" Then
      IF  ( Instr( SUM, "}" ) > 0  OR  Instr( SUM, "{" ) > 0 ) Then
        If  ( Instr(OPS, "+-") > 0 OR Instr(OPS, "*/%") > 0 ) Then
            dim temp as string
            If CastOrder( TypeV2 ) > CastOrder( TypeV1 ) Then
                TypeAV = TypeV2
            Else
                TypeAV = TypeV1
            End if
            temp = Message("WarningConditionalExpressionAssumesVarType")
            Replace temp,"%vartype%",TypeAV
            'Logwarning temp, Origin
            LinkedListInsert(OutList->CodeEnd, ";"+temp)
        End If
      End If
    End If
  End If

  CalcType = GetCalcType(TypeV1, Act, TypeV2, TypeAV)
  ' Potential debug point
  If (( compilerdebug and cCALCOPS ) = cCALCOPS ) Then
    Print " 4 CALCOPS       : " +V1, Act, V2, CalcType, AV, ": " + TypeV1 + " " + Act + " " +TypeV2, Origin
  End If
  'Decide output variable
  If CalcStart = 1 And CalcEnd = LEN(SUM) And AV <> "" And (Not NeverLast) Then
    AnswerIn = AV
    LastCalc = -1
  Else
    'If CalcType is BIT, get BYTE variable
    If CalcType = "BIT" Then CalcType = "BYTE"
    AnswerIn = GetCalcVar(CalcType)

    If ModeAVR and Instr( UCASE( Origin), ";?F1L" ) > 0 Then  'examine only user program ignoring libraries which are assumed to be safe
      'Ensure Systemp1 is NOT used when complex maths with MUL - causesSystemp1 (R0 and R1 ) being overwritten by the MUL results
      'So, dont use Systemp1 else we will get incorrect maths
      If CountOccur(Sum, "';+-*/%&|#!") > 2 And HMult and  Instr( Ucase(AnswerIn), "SYSTEMP1") Then
        AnswerIn = GetCalcVar(CalcType)
      End if
    End If

    LastCalc = 0
  End If

  'Does next calc use same system vars for result?
  NextSame = 0
  If CountOccur(SUM, "';*/%=~<>{}") > 1 And CountOccur(Act, "';*/%=~<>{}") > 0 Then NextSame = -1
  If NeverLast Then NextSame = -1
  'Print SUM, Act, NextSame

  'Define all vars found as byte
  'AddVar will ignore any constants/system vars/vars already defined
  AddVar DelType(V1), "BYTE", 1, CurrentSub, "REAL", Origin
  AddVar DelType(V2), "BYTE", 1, CurrentSub, "REAL", Origin
  AddVar AnswerIn, "BYTE", 1, DestSub, "REAL", Origin

  Color 3

  'Generate code for unary ops
  If UnaryMode Then
    Answer = CompileCalcUnary(OutList, Act, V2, Origin, AnswerIn)
    SearchStart += 1

  'Generate code for binary ops
  Else
    If Act = "*" Or Act = "/" Or Act = "%" Then Answer = CompileCalcMult(OutList, V1, Act, V2, Origin, AnswerIn)
    If Act = "+" Or Act = "-" Then Answer = CompileCalcAdd(OutList, V1, Act, V2, Origin, AnswerIn)
    If Act = "=" Or Act = "~" Or Act = "<" Or Act = ">" Or Act = "{" Or Act = "}" Then Answer = CompileCalcCondition(OutList, V1, Act, V2, Origin, AnswerIn)
    If Act = "&" Or Act = "|" Or Act = "#" Then Answer = CompileCalcLogic(OutList, V1, Act, V2, Origin, AnswerIn)
    'Note 13/2/2012: This error should never appear. Here just in case something stupid has happened!
    If Act = "!" Then Print "Internal error: Unary operator ! not handled properly"
  End If
  'Print SUM, V1, Act, V2, AnswerIn, Answer

  'Don't bother with NextSame if calc result in const
  If IsConst(Answer) Then NextSame = 0

  'Copy answer if needed
  If Answer <> AnswerIn And (LastCalc Or NextSame) Then
    NewCode = CompileVarSet(Answer, AnswerIn, Origin)
    OutList->CodeEnd = LinkedListInsertList(OutList->CodeEnd, NewCode)
    FreeCalcVar Answer
    Answer = AnswerIn
  End If

  'Release calc vars
  If Answer <> V1 And Not UnaryMode Then FreeCalcVar V1
  If Answer <> V2 Then FreeCalcVar V2
  If Answer <> AnswerIn And Not LastCalc Then FreeCalcVar AnswerIn
  Color 7

  'Store answer in sum
  'Print SUM,
  SUM = Left(SUM, CalcStart - 1) + Answer + Mid(SUM, CalcEnd + 1)
  'Print Answer, SUM

  Goto SearchForOpAgain

END SUB

Function CalcLineSize(CurrLine As String, ThisSubPage As Integer, CallPos As AsmCommand Pointer, GotoPos As AsmCommand Pointer) As Integer
  'Calculates the size in words of an assembly code line
  'CallPos is the location of the call instruction in the instruction list

  Dim As Integer InstSize, PresPos, TempDataCount, CalledSub, CalledSubPage
  Dim As String CurrLineVal, TempData(20), NextCallTarget
  Dim As Integer RestorePage, NextCalledSub, NextCalledSubPage
  Dim As SubType Pointer GotoSub
  Dim As AsmCommand Pointer InstIndex

  Dim As String ROMData, Temp
  Dim As Integer DataBlockSize, RSC, DWIC, SS

  CurrLineVal = CurrLine

  'Replace PRESERVE with whatever is being preserved, need to do this for string tables
  IF Left(CurrLineVal, 9) = "PRESERVE " THEN
    PresPos = VAL(Mid(CurrLineVal, 10))
    CurrLineVal = ";" + PreserveCode(PresPos)
    If Mid(CurrLineVal, 2, 1) = Chr(8) Then CurrLineVal = Mid(CurrLineVal, 3)
  END If

  InstSize = 0
  InstIndex = IsASM(CurrLineVal)

  If InstIndex = 0 Then
    'If line not recognised as instruction, check for directive
    If ModePIC Then

      'Bank selection directives
      If Left(CurrLineVal, 9) = " banksel " Then
        If ChipFamily = 12 Then
          InstSize = 1
        ElseIf ChipFamily = 14 Then
          If HasSFRBit("RP1") Then
            InstSize = 2
          Else
            InstSize = 1
          End If
        ElseIf ChipFamily = 15 Then
          InstSize = 1
        ElseIf ChipFamily = 16 Then
          InstSize = 1
        End If

      ElseIf Left(CurrLineVal, 10) = " bankisel " Then
        InstSize = 1

      ElseIf Left(CurrLineVal, 7) = " fcall " Then
          'added for PIC-AS support
          InstSize = 3

      ElseIf Left(CurrLineVal, 6) = " ljmp " Then
          'added for PIC-AS support
           InstSize = 3

      ElseIf Left(CurrLineVal, 9) = " pagesel " Then
        If ChipFamily = 12 Or ChipFamily = 15 Then
          InstSize = 1
        Else
          InstSize = 2
        End If

      'Data embedding directives
      ElseIf Left(CurrLineVal, 4) = " dw " OR Left(CurrLineVal, 4) = " db " Or Left(CurrLineVal, 4) = " de " THEN
        'Print "Data: "; CurrLineVal;
        DataBlockSize = 1
        If (ChipFamily = 14 Or ChipFamily = 15) And Left(CurrLineVal, 4) = " de " Then DataBlockSize = 2
        ROMData = Trim(Mid(CurrLineVal, 4))
        InstSize = 0
        'DWC = 0
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

        SS = 0
        InstSize = 0
        Do While SS < DWIC
          SS += 1

          Temp = CheckTemp(SS)
          IF Temp <> "" THEN
            'Temp contains integer
            IF IsConst(Temp) Then
              'Print "Int", MakeDec(Temp)
              If MakeDec(Temp) > 255 Then
                InstSize += 2
              Else
                InstSize += 1
              End If

            'Temp contains string
            Else
              IF INSTR(Temp, "%S") <> 0 THEN
                Temp = TempData(Val(Mid(Temp, INSTR(Temp, "%S") + 2)))
              END If
              InstSize += LEN(Temp)
            END If

          END IF
        Loop

        If ChipFamily = 16 Then
          InstSize = (InstSize + 1) \ 2
        End If
        'Print " Size:"; InstSize
      End If

    ElseIf ModeAVR Then
      'Data embedding directives
      If UCase(Left(Trim(CurrLineVal), 4)) = ".DB " THEN
        'Print "Data: "; CurrLineVal;
        ROMData = Trim(Mid(CurrLineVal, 4))
        InstSize = 0

        DO WHILE INSTR(ROMData, ",") <> 0
          InstSize += 1
          ROMData = Trim(Mid(ROMData, INSTR(ROMDATA, ",") + 1))
        LOOP
        IF ROMData <> "" Then
          InstSize += 1
        End If
        'Will be zero padded if odd length
        If InstSize Mod 2 = 1 Then InstSize += 1
        'Two bytes are packed in each location
        InstSize = InstSize \ 2

        'Print " Size:"; InstSize
      End If

    End If

  Else
    InstSize = InstIndex->Words
  End If

  Return InstSize
End Function

Sub CalcSubSize(CurrSub As SubType Pointer)

  Dim As Integer FinalSize, ThisSubPage
  Dim As LinkedListElement Pointer CurrLine, NextLine
  Dim As String NextLineValue
  Dim As AsmCommand Pointer CallPos, GotoPos, RetPos

  'Get the position of call instruction, saves work later
  CallPos = IsASM("call")

  'Get the position of return instruction
  If ModePIC Then
    If ChipFamily = 12 Then
      RetPos = IsASM("retlw")
    Else
      RetPos = IsASM("return")
    End If
    GotoPos = IsASM("goto")
  ElseIf ModeAVR Or ModeZ8 Then
    RetPos = IsASM("ret")
    GotoPos = 0
  EndIf

  'Add page selection commands to sub so that size is accurate
  '(Will remove and re-add all page selection commands)
  AddPageCommands(CurrSub)

  'Find the page this sub is intended for
  ThisSubPage = CurrSub->DestPage

  FinalSize = 0
  CurrLine = CurrSub->CodeStart->Next
  Do While CurrLine <> 0

    'Get next code line
    NextLine = CurrLine->Next
    If NextLine <> 0 Then
      Do While (Left(NextLine->Value, 1) = ";" Or Left(NextLine->Value, 8) = "PRESERVE") AND NextLine->Next <> 0
        NextLine = NextLine->Next
      Loop
    End If
    NextLineValue = ""
    If NextLine <> 0 Then
      NextLineValue = NextLine->Value
    End If

    FinalSize += CalcLineSize(CurrLine->Value, ThisSubPage, CallPos, GotoPos)
    CurrLine = CurrLine->Next
  Loop

  'Add size of return
  If Not CurrSub->NoReturn Then
    FinalSize += RetPos->Words
  End If

  CurrSub->HexSize = FinalSize

  If FinalSize > CurrSub->MaxHexSize Then
    CurrSub->MaxHexSize = FinalSize
  End If

End Sub

FUNCTION CastOrder (InType As String) As Integer
  Select Case UCase(Trim(InType))
    Case "BIT": Return 0
    Case "BYTE": Return 1
    Case "WORD": Return 2
    Case "INTEGER": Return 3
    Case "LONG": Return 4
    Case "ULONGINT": Return 5
    Case "LONGINT": Return 5
    Case "SINGLE": Return 6
    Case "DOUBLE": Return 7
    Case "STRING": Return 8
    Case Else: Return -1
  End Select
END FUNCTION

/'* \brief return a tag value
\param ConstName As String, Origin As String
\returns Nothing

This sub validates the constname against a set of rules.
Issues Warning if the constname is not valid.

'/
Sub CheckConstName (ConstName As String, Origin As String)

  Dim As String TempData
  Dim As Integer T

  'Names which will cause trouble
  T = 0
  IF INSTR(ConstName, " ") <> 0 THEN T = 1
  IF INSTR(ConstName, ",") <> 0 THEN T = 1
  IF INSTR(ConstName, "(") <> 0 THEN T = 1
  IF INSTR(ConstName, ")") <> 0 THEN T = 1
  IF ConstName = "W" THEN T = 1
  IF ConstName = "F" THEN T = 1
  IF ConstName = "B" THEN T = 1
  IF ConstName = "C" THEN T = 1
  IF ConstName = "Z" THEN T = 1
  IF T = 1 THEN
    TempData = Message("BadConstName")
    Replace TempData, "%const%", ConstName
    LogError TempData, Origin
  END IF

  'Names which may cause trouble
  If Len(ConstName) = 1 And T = 0 Then
    TempData = Message("WarningShortName")
    Replace TempData, "%type%", "constant"
    Replace TempData, "%name%", ConstName
    LogWarning TempData, Origin
  END If
End Sub

/'* \brief return a tag value
\param None
\returns Nothing

Check speed that has been selected for the system clock, using information from DAT.
The available clock speed are in the array IntOscSpeeds()
If it is 0 Mhz, need to set to highest possible
If it is too high, show a warning


'/
Sub CheckClockSpeed

  Dim As Integer CurrSpeed
  Dim As Double MaxSpeed
  Dim As String SpeedMessage

  If ChipMhz = 0 Then

    'Find highest int osc speed
    MaxSpeed = -1
    If IntOscSpeeds <> 0 Then
      For CurrSpeed = 1 To IntOscSpeeds
        If MaxSpeed < IntOscSpeed(CurrSpeed) Then
          MaxSpeed = IntOscSpeed(CurrSpeed)
        End If
      Next
    End If

    'No suitable int osc, use highest ext osc speed
    If MaxSpeed = -1 Then
      MaxSpeed = ChipMaxSpeed
    End If

    'Set speed
    ChipMHz = MaxSpeed
    SpeedMessage = Message("AutoClockSpeed")
    Replace SpeedMessage, "%speed%", Str(ChipMhz)
    IF VBS = 1 THEN PRINT SPC(5); SpeedMessage

  ElseIf ChipMhz > ChipMaxSpeed Then
    'Show warning if speed too high
    LogWarning(Message("WarningOverclocked"))
  End If

End Sub

/'* \brief return a tag value
\param None

This sub compiles the source program.
The preprocessor has aleady transformed the source files.

Checks every sub in program, compile those that need to be compiled.
The approach includes the need to check again once a sub has been compiled, because that sub may require other subs.


'/

Sub CompileProgram

  Dim As Integer CurrSub, CompileMore, IntLoc, CurrInc, SubLoc, TablesCompiled

  'Set constant here to ensure the preprocessor has examined the source
  If ModeAVR Then
    If HMult Then AddConstant("HARDWAREMULT", "")
  End If

  TablesCompiled = 0

  'Request initialisation routine
  If UserCodeOnlyEnabled = 0 then
      RequestSub(0, "InitSys")
      SubLoc = LocationOfSub("InitSys", "")
      If SubLoc > 0 Then
        SourceFile(Subroutine(SubLoc)->SourceFile).InitSubUsed = -1
      End If
  End if
  'If main source file has startup routine, also call that
  With SourceFile(1)
    If .InitSub <> "" Then
      RequestSub(0, .InitSub)
      .InitSubUsed = -1
    End If
  End With

  'Find Interrupt sub, if found mark as required and set UserInt flag
  IntLoc = LocationOfSub("INTERRUPT", "")   'was capitalised
  UserInt = 0
  If IntLoc <> 0 Then
    Subroutine(IntLoc)->Required = -1
    UserInt = -1
  End If

  'Add code to prevent program continuing into subs
  AddMainEndCode

  'Need to find all DIM statements before anything else
  For CurrSub = 0 To SBC
    CompileDim Subroutine(CurrSub)
  Next

  'Main Loop
  If VBS <> 0 Then
    Print Spc(5); Message("CompSubs")
  End If
  Do
    CompileMore = 0
    For CurrSub = 0 To SBC
      With *Subroutine(CurrSub)
        If .Required And Not .Compiled Then
          CompileSubroutine(Subroutine(CurrSub))
          CompileMore = -1
        End If
      End With
    Next

    'Check for required initialisation subs
    FOR CurrInc = 1 TO SourceFiles
      With SourceFile(CurrInc)
        If .InitSub <> "" And .InitSubUsed Then
          SubLoc = LocationOfSub(.InitSub, "")
          If SubLoc <> 0 Then
            Subroutine(SubLoc)->FirstPage = -1
            If Not Subroutine(SubLoc)->Required Then
              Subroutine(SubLoc)->Required = -1
              CompileMore = -1
            End If
          End If
        End If
      End With
    Next

    'On last loop through, compile tables and then force one more loop
    If Not CompileMore And Not TablesCompiled Then
      'Compile data/string tables
      CompileTables
      TablesCompiled = -1

      CompileMore = -1
    End If

  Loop While CompileMore

  'Add context save/restore and int handlers to Interrupt
  AddInterruptCode

  'Compile IntOn/IntOff
  '(Need to compile here, after all On Interrupt commands in all subs have been found, but before AddMainInitCode)
  For CurrSub = 0 To SBC
    If .Subroutine(CurrSub)->Required Then
      CompileIntOnOff (Subroutine(CurrSub))
    End If
  Next

  'Ensure no overlap between temporary variables
  FixTemporaryVariables

  'Add initialise code to start of main
  AddMainInitCode

  'Add code for individual pin dir set on PIC 12 bit
  If ChipFamily = 12 Then
    FixSinglePinSet
  End If

End Sub


/'* \brief return a tag value
\param CompSub as pointer

This sub compiles the subroutines.

Supports use of ExtendedVerboseMessages= n | y in INI file to enable extended complilation messages

'/
Sub CompileSubroutine(CompSub As SubType Pointer)

  If VBS = 1 Then
    Print ""
    Print Spc(5); "Commpiling method"; "  "; CompSub->Name
  End If

  'Split any lines at : (these may be inserted through constants)
  If ExtendedVerboseMessages Then Print Spc(15); "Splitting lines"
  SplitLines (CompSub)

  'Apply any tweaks
  TidyInputSource (CompSub)

  'Compile calls to other subroutines, insert macros
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling sub calls"

  CompileSubCalls (CompSub)

  'Compile DIMs again, in case any come through from macros
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling DIM"
  CompileDim (CompSub)

  'Compile various commands
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling For"
  CompileFor (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Processing arrays"
  ProcessArrays (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Adding sys var bits"
  AddSysVarBits (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling ReadTable"
  CompileReadTable (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Pot"
  CompilePot (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Do"
  CompileDo (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Dir"
  CompileDir (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Wait"
  CompileWait (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling On Interrupt"
  CompileOn (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Set"
  CompileSet (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Rotate"
  CompileRotate (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Repeat"
  CompileRepeat (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Select"
  CompileSelect (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Return"   'was Select
  CompileReturn (CompSub)
  'Compile If statements and variable assignments last
  'This allows other commands to generate IFs and assignments rather than having to produce assembly
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling If"
  CompileIF (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling var assignments"
  CompileVars (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Exit Sub"
  CompileExitSub (CompSub)
  If ExtendedVerboseMessages Then Print Spc(15); "Compiling Goto"
  CompileGoto (CompSub)

  'Recognise ASM
  If ExtendedVerboseMessages Then Print Spc(15); "Finding assembly"
  FindAssembly(CompSub)

  'Replace SysPointerX pseudo variable
  If ModeAVR Then FixPointerOps (CompSub)

  'CompileIntOnOff (CompSub) Need to do this after all subs compiled
  If ExtendedVerboseMessages Then Print Spc(15); "Done"
  CompSub->Compiled = -1
  CompiledSBC =  CompiledSBC + 1
End Sub


/'* \brief return a tag value
\param SUM As String, AV As String, Origin As String, ByRef OutList As CodeSection Pointer = 0, NeverLast As Integer = 0

This sub compiles the CALC maths

'/

  ' 'Check AV (the target variable) is not the source variable
  ' Dim sourcevariablepos as integer
  ' Dim lengthofsourcevariable as integer
  ' sourcevariablepos=  INSTR ( SUM, AV)
  ' lengthofsourcevariable = length ( AV )
  ' If sourcevariablepos+lengthofsourcevariable = length(SUM) then

  ' End if

Sub CompileCalc (SUM As String, AV As String, Origin As String, ByRef OutList As CodeSection Pointer = 0, NeverLast As Integer = 0)

  Dim As String TempData, InBrackets, OutTemp
  Dim As Integer T, BL, BC
  Dim As LinkedListElement Pointer FindLine

  If OutList = 0 Then
    OutList = NewCodeSection
  End If



  If ModeAVR Then

    'Turn off HMult when really complex as the register management just is too complex with SystempX variables being overwritten by the calc and MUL results
    'So, disable hardware maths and use long maths to ensure we have regiser management
    If CountOccur(Sum, "';+-*/%&|#!") > 3 And HMult Then
      'print Origin, Instr( UCASE( Origin), ";?F1L" )
      'Are we in the user program? Test for ;?F1L....  Because we dont change library code
      If Instr( UCASE( Origin), ";?F1L" ) > 0 Then
        ' LogWarning "Using AVR hardware maths - simplify statement to ensure correct ASM generation"
        If PreserveMode <> 0 Then OutList->CodeEnd = LinkedListInsert(OutList->CodeEnd, ";Complex math calculation.  AVR hardware MUL directive disabled  - simplify calculation to enable MUL for Byte math")
        HMult = 0
      End If
    End if

  End If


  'Simplify --, +-
  DO WHILE INSTR(SUM, "--") <> 0: Replace SUM, "--", "+": Loop
  DO WHILE INSTR(SUM, "+-") <> 0: Replace SUM, "+-", "-": LOOP

  'Calculate brackets
  Do While INSTR(SUM, "(") <> 0
    'Get the sum in brackets
    TempData = Mid(SUM, INSTR(SUM, "("))
    BL = 1
    FOR T = 2 TO LEN(TempData)
      IF Mid(TempData, T, 1) = "(" THEN BL = BL + 1
      IF Mid(TempData, T, 1) = ")" THEN BL = BL - 1
      IF BL = 0 THEN TempData = Left(TempData, T): EXIT FOR
    Next

    'Detect if whole sum is in brackets, remove brackets and try again if so
    If Len(TempData) = Len(SUM) Then
      SUM = Mid(Sum, 2, Len(Sum) - 2)
    Else

      'Use recursion to calculate sum in brackets
      InBrackets = Mid(TempData, 2)
      InBrackets = Left(InBrackets, LEN(InBrackets) - 1)
      OutTemp = ""
      CompileCalc InBrackets, OutTemp, Origin, OutList, -1
      If OutTemp = "" Then OutTemp = InBrackets
      Replace SUM, TempData, OutTemp
    End If
  Loop

  'Calculate unary operations
  CalcOps OutList, SUM, AV, "U!-", Origin, NeverLast

  'Calculate binary operations
  CalcOps OutList, SUM, AV, "*/%", Origin, NeverLast
  CalcOps OutList, SUM, AV, "+-", Origin, NeverLast
  CalcOps OutList, SUM, AV, "=~<>{}", Origin, NeverLast
  CalcOps OutList, SUM, AV, "&|!#", Origin, NeverLast

  'Re-add asm calculations
  FindLine = OutList->CodeList
  Do While FindLine <> 0
    TempData = UCase(FindLine->Value)
    IF INSTR(TempData, "SYSASMCALC") <> 0 Then
      T = VAL(Mid(TempData, INSTR(TempData, "SYSASMCALC") + 10))
      Replace FindLine->Value, "SYSASMCALC" + Str(T), CalcAtAsm(T)
    End If
    FindLine = FindLine->Next
  Loop

End Sub

/'* \brief return a tag value
\param OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String

This sub compiles the ADD and SUBTRACTION maths

'/
FUNCTION CompileCalcAdd(OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String

  Dim OutVal As LongInt
  Dim As String V1Type, V2Type, CalcType, DestType, AV, R1, R2, Temp, V1Org, V2Org
  Dim As Integer SourceSub, DestSub
  Dim As String CurrV1, CurrV2
  Dim As String Cmd, Ovr, TempVar
  Dim As Integer CD, CurrVarByte, CheckCarry, AddEndIf, UseAVByte

  Dim As LinkedListElement Pointer CurrLine, NewCode
  CurrLine = OutList->CodeEnd

  V1Org = V1
  V2Org = V2
 
  'Calculate +, -. Replace sum with variable containing answer

  'Notes for new code:
  ' - Only need to work with enough bytes to fill answer. If A = 257, B = 256,
  '   A - B = 1 (with byte ops). If high bytes don't match, it will be
  ' impossible to store result anyway. No point calculating more bytes!
  ' - Remember references! (@variable)
  ' - addlw/sublw not implemented on 12 bit core
  ' - in long addition, need incfsz instruction on 16F to handle carry properly
  'Optimisations:
  ' - A +/- 0 = A
  ' - B +/- 0 = B
  ' - If A = B, A - B = 0, A + B = A << 1
  ' - A + 1/A - 1/B + 1/B - 1: use incf/decf/inc/dec?
  ' - 0 +/- 0: store 0 directly in output

  'Remove casts from variable names
  'These should have already been read by CalcOps, and a suitable size temp
  'or output variable should have been set.
  V1 = DelType(V1): V2 = DelType(V2)

  'Get types
  SourceSub = GetSubID(Origin)
  If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
  V1Type = TypeOfValue(V1, Subroutine(SourceSub))
  V2Type = TypeOfValue(V2, Subroutine(SourceSub))
  
  'override type for Floats - this is OK as TypeOfValue() would handle if... V1 or V2 had not been stripped of CAST
  If Instr( V1Org, "[SINGLE]" ) Then
    V1Type = "SINGLE"
  End If
  If Instr( V1Org, "[DOUBLE]" ) Then
    V1Type = "DOUBLE"
  End If
  If Instr( V2Org, "[SINGLE]" ) Then
    V2Type = "SINGLE"
  End If
  If Instr( V2Org, "[DOUBLE]" ) Then
    V2Type = "DOUBLE"
  End If
  DestType = TypeOfVar(Answer, Subroutine(DestSub))
  CalcType = DestType

  If ( V1Type = "BIT" AND ( Instr( V1, ".") <> 0 ))  Or  ( V2Type = "BIT"  AND ( Instr( V2, ".") <> 0  ))  Then
      LogError Message("SynIllegalBitMaths"), Origin
  End if

  'Remove cast from output var
  Answer = DelType(Answer)

  'Get output var
  AV = Answer
  
  If (( compilerdebug and cCOMPILECALCADD ) = cCOMPILECALCADD ) Then
    Print " 1 COMPILECALCADD: " + AV + " = " + V1 + " " + Act + " " + V2 + " TYPE: " + V1Type + ACT + V2Type + " " +  " CONST: " + Str(IsConst(V1)) + " " +Str(IsConst(V2))+ " CalcType: " + CalcType
  End If

  'Check if both are constants
  IF IsConst(V1) AND IsConst(V2) Then
    If InStr(V1, "@") = 0 AND INSTR(V2, "@") = 0 Then
      If IsIntType(CalcType) Then
        OutVal = 0
        IF Act = "+" THEN OutVal = MakeDec(V1) + MakeDec(V2)
        IF Act = "-" THEN OutVal = MakeDec(V1) - MakeDec(V2)
        If CalcType = "BYTE" Then
          Do While OutVal < 0: OutVal += 256: Loop
          Do While OutVal > 255: OutVal -= 256: Loop
        ElseIf CalcType = "WORD" Then
          Do While OutVal < 0: OutVal += 65536: Loop
          Do While OutVal > 65535: OutVal -= 65536: Loop
        ElseIf CalcType = "INTEGER" Then
          Do While OutVal < -32768: OutVal += 65536: Loop
          Do While OutVal > 32767: OutVal -= 65536: Loop
        ElseIf CalcType = "LONG" Then
          Do While OutVal < 0: OutVal += 2^32: Loop
          Do While OutVal >= 2^32: OutVal -= 2^32: Loop
        End If
        AV = Str(OutVal)
      ElseIf IsFloatType(CalcType) Then
        IF Act = "+" THEN AV = Str(MakeDecFloat(V1) + MakeDecFloat(V2))
        IF Act = "-" THEN AV = Str(MakeDecFloat(V1) - MakeDecFloat(V2))
      End If
      GOTO AddSubAnswer
    Else
      'CalcAtAsm(T), CAAC
      '@SYSASMCALCn
      'Need to put calculation for assembler into CalcAtAsm(n)
      'Then put "@SYSASMCALCn" into the the line. It will be replace when suitable.
      For CurrVarByte = 0 To GetTypeSize(CalcType) - 1
        CAAC += 1
        CalcAtAsm(CAAC) = GetByte("@" + V1 + Act + V2, CurrVarByte)
        NewCode = CompileVarSet("@SYSASMCALC" + Str(CAAC), "[byte]" + GetByte(AV, CurrVarByte), Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
      Next
      GoTo AddSubAnswer
    End If
  END IF

  'Optimisations
  'Increment/Decrement (byte only)
  'V1 and V2 are the two values in the calculation step, and R1 and R2 are the registers that they have been temporarily copied into on AVR.
  If CalcType = "BYTE" Then
    If V2 = "1" And INSTR(V1, "@") = 0 Then
      If ModePIC Then
        If V1 <> AV Then
          If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " incf " + V1 + ",W")
          If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " decf " + V1 + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + AV)
        Else
          If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " incf " + V1 + ",F")
          If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " decf " + V1 + ",F")
        End If
      ElseIf ModeAVR Then
        NewCode = LinkedListCreate
        R1 = PutInRegister(NewCode, V1, "BYTE", Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " inc " + R1)
        If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " dec " + R1)
        AV = R1
      End If
      Goto AddSubAnswer
    End If
    If V1 = "1" And Act = "+" And INSTR(V2, "@") = 0 Then
      If ModePIC Then
        If V2 <> AV Then
          CurrLine = LinkedListInsert(CurrLine, " incf " + V2 + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + AV)
        Else
          CurrLine = LinkedListInsert(CurrLine, " incf " + V2 + ",F")
        End If
      ElseIf ModeAVR Then
        NewCode = LinkedListCreate
        R2 = PutInRegister(NewCode, V2, "BYTE", Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        CurrLine = LinkedListInsert(CurrLine, " inc " + R2)
        AV = R2
      End If
      Goto AddSubAnswer
    End If
  End If

  If V1 = V2 And INSTR(V1, "@") = 0 Then
    If Act = "-" Then
      AV = "0"
      Goto AddSubAnswer
    End If
  End If

  'Handle floating point addition here
  If IsFloatType(CalcType) Then

    'Check for zeroes that mean addition or subtraction can be swapped
    If IsConst(V2) Then
      If MakeDecFloat(V2) = 0 Then
        AV = V1
        GoTo AddSubAnswer
      End If
    ElseIf IsConst(V1) And Act = "+" Then
      If MakeDecFloat(V1) = 0 Then
        AV = V2
        GoTo AddSubAnswer
      End If
    End If

    'Copy V1 and V2 into temporary variables, call appropriate sub
    AddVar("Sys" + CalcType + "TempA", CalcType, 1, 0, "REAL", Origin, ,-1)
    AddVar("Sys" + CalcType + "TempB", CalcType, 1, 0, "REAL", Origin, ,-1)
    AddVar("Sys" + CalcType + "TempX", CalcType, 1, 0, "REAL", Origin, ,-1)
    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, "Sys" + CalcType + "TempA", Origin))
    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, "Sys" + CalcType + "TempB", Origin))
    If Act = "+" Then
      RequestSub(Subroutine(SourceSub), "SysAddSub" + CalcType, "")
      CurrLine = LinkedListInsert(CurrLine, " call SysAddSub" + CalcType)
    ElseIf Act = "-" Then
      RequestSub(Subroutine(SourceSub), "SysSubSub" + CalcType, "")
      CurrLine = LinkedListInsert(CurrLine, " call SysSubSub" + CalcType)
    End If
    AV = "Sys" + CalcType + "TempX"
    GoTo AddSubAnswer
  End If

  'Swap V1, V2 if mode is add and V1 is const
  If Act = "+" And IsConst(V1) Then
    'Swap names
    Swap V1, V2

    'Swap Types
    Swap V1Type, V2Type
  End If

  'Snippet of old code, can sublw be used below to improve generated code?
  '   If ChipFamily <> 12 And IsConst(V1) Then
  '     Cmd = " addlw ": IF Act = "-" THEN Cmd = " sublw "
  '     CurrLine = LinkedListInsert(CurrLine, Cmd + GetByte(V1, 0))
  '     CurrLine = LinkedListInsert(CurrLine, " movwf " + AV)
  '

  'Flag to indicate if carry/borrow check is needed
  CheckCarry = 0

  'Deal with each byte
  For CurrVarByte = 0 To GetTypeSize(CalcType) - 1

    AddEndIf = 0

    'Get current byte of V1 and V2 (if they exist) or 0
    If GetTypeSize(V1Type) > CurrVarByte Then
      CurrV1 = GetByte(V1, CurrVarByte)
    Else
      CurrV1 = "0"
    End If
    If GetTypeSize(V2Type) > CurrVarByte Then
      CurrV2 = GetByte(V2, CurrVarByte)
    Else
      CurrV2 = "0"
    End If

    'Shortcut for multiple byte increment
    '(Especially useful for pointers and For loops)
    If V1 = AV And V2 = "1" And INSTR(V1, "@") = 0 And Act = "+" Then
      If ModePIC Then
        If CurrVarByte > 0 Then
          CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
        End If
        CurrLine = LinkedListInsert(CurrLine, " incf " + CurrV1 + ",F")

      ElseIf ModeAVR Then
        NewCode = LinkedListCreate
        R1 = PutInRegister(NewCode, CurrV1, "BYTE", Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        If CurrVarByte > 0 Then
          CurrLine = LinkedListInsert(CurrLine, " brne PC + 2")
        End If
        CurrLine = LinkedListInsert(CurrLine, " inc " + R1)
        'Copy result to output variable
        NewCode = CompileVarSet("[byte]" + R1, "[byte]" + GetByte(AV, CurrVarByte), Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        FreeCalcVar R1
      End If
      Goto AddSubNextByte

    End If

    'Is a calculation needed?
    'If both inputs 0, output 0
    'If one input 0, copy or negate (0 - x) other input
    'If both inputs non 0, then calculate. Check carry if needed (CurrVarByte > 0 and potential overflow)

    'Both inputs 0, output 0
    If CurrV1 = "0" And CurrV2 = "0" Then
      'Need carry?
      If CheckCarry Then
        'Carry won't be needed next time
        CheckCarry = 0

        If ModePIC Then
          '0 + C > F, no need to update C
          CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(AV, CurrVarByte))
          If Act = "+" Then
            CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,C")
            CurrLine = LinkedListInsert(CurrLine, " incf " + GetByte(AV, CurrVarByte) + ",F")
          Else
            CurrLine = LinkedListInsert(CurrLine, " btfss STATUS,C")
            'CurrLine = LinkedListInsert(CurrLine, " addlw 255")
            CurrLine = LinkedListInsert(CurrLine, " decf " + GetByte(AV, CurrVarByte) + ",F")
          End If
          'CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
          GoTo AddSubNextByte

        ElseIf ModeAVR Then
          R1 = GetCalcVar("BYTE")
          CurrLine = LinkedListInsert(CurrLine, " clr " + R1)
          CurrLine = LinkedListInsert(CurrLine, " brcc PC + 2")
          If Act = "+" Then
            CurrLine = LinkedListInsert(CurrLine, " inc " + R1)
          Else
            CurrLine = LinkedListInsert(CurrLine, " dec " + R1)
          End If
          NewCode = CompileVarSet(R1, "[BYTE]" + GetByte(AV, CurrVarByte), Origin)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
          FreeCalcVar R1
          GoTo AddSubNextByte

        End If

      Else
        'No high bytes, no carry, so put 0 into output byte
        NewCode = CompileVarSet("0", "[BYTE]" + GetByte(AV, CurrVarByte), Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        GoTo AddSubNextByte
      End If

    End If

    'Need to calculate AV
    If ModePIC Then
      'On 16F1/18F, if V1 is const, put it into variable so subwfb can be used
      'On 12 bit core, V1 cannot be a constant due to lack of sublw instruction
      'If V1 is a constant, it needs to be put into a variable
      If IsConst(CurrV1) And (ChipFamily = 12 Or ((ChipFamily = 15 Or ChipFamily = 16) And CheckCarry)) Then
        CurrLine = LinkedListInsert(CurrLine, " movlw " + CurrV1)
        CurrV1 = GetCalcVar("BYTE")
        CurrLine = LinkedListInsert(CurrLine, " movwf " + CurrV1)
      END If

      'Need to put V1 into temporary variable if carry involved, or issue occurs adding 255 to 255 with carry
      UseAVByte = 0
      If Not IsConst(CurrV1) And V1 <> AV And CheckCarry And ChipFamily <> 15 And ChipFamily <> 16 Then
        CurrLine = LinkedListInsert(CurrLine, " movf " + CurrV1 + ",W")
        CurrV1 = GetByte(AV, CurrVarByte)
        CurrLine = LinkedListInsert(CurrLine, " movwf " + CurrV1)
        UseAVByte = -1
      End If

      'Put V2 into W
      Temp = " movf " + CurrV2 + ",W"
      IF IsConst(CurrV2) THEN Temp = " movlw " + CurrV2
      CurrLine = LinkedListInsert(CurrLine, Temp)

      'Add carry from low byte calc to W
      If CheckCarry And ChipFamily <> 15 And ChipFamily <> 16 Then
        Temp = " btfsc ": IF Act = "-" THEN Temp = " btfss "
        If ChipFamily = 12 Then
          TempVar = GetCalcVar("BYTE")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + TempVar)
          CurrLine = LinkedListInsert(CurrLine, Temp + "STATUS,C")
          CurrLine = LinkedListInsert(CurrLine, " incf " + TempVar + ",W")
          FreeCalcVar TempVar
        Else
          CurrLine = LinkedListInsert(CurrLine, Temp + "STATUS,C")
          'CurrLine = LinkedListInsert(CurrLine, " addlw 1")
          If IsConst(CurrV2) Then
            CurrLine = LinkedListInsert(CurrLine, " movlw " + CurrV2 + " + 1")
          Else
            CurrLine = LinkedListInsert(CurrLine, " incf " + CurrV2 + ",W")
            ILC += 1: AddEndIf = -1
            CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
            CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
          End If
        End If
      End If

      'Add W to V1, store - C already added, or 18F and no C
      If (ChipFamily <> 15 And ChipFamily <> 16) Or Not CheckCarry Then
        If (V1 <> AV Or IsConst(CurrV1)) And Not UseAVByte Then
          If IsConst(CurrV1) Then
            If Act = "+" And CurrV1 <> "0" Then CurrLine = LinkedListInsert(CurrLine, " addlw " + CurrV1)
            If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " sublw " + CurrV1)
          Else
            If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " addwf " + CurrV1 + ",W")
            If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " subwf " + CurrV1 + ",W")
          End If
          If AddEndIf Then
            AddEndIf = 0
            CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
            GetMetaData(Currline)->IsLabel = -1
          EndIf
          CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
        Else
          IF Act = "+" THEN CurrLine = LinkedListInsert(CurrLine, " addwf " + CurrV1 + ",F")
          IF Act = "-" THEN CurrLine = LinkedListInsert(CurrLine, " subwf " + CurrV1 + ",F")
          If AddEndIf Then
            AddEndIf = 0
            CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
            GetMetaData(Currline)->IsLabel = -1
          EndIf
        End If

      'Add W to V1, store - 18F, carry needed
      ElseIf ChipFamily = 15 Or ChipFamily = 16 Then
        If AV <> V1 Then
          IF Act = "+" THEN CurrLine = LinkedListInsert(CurrLine, " addwfc " + CurrV1 + ",W")
          IF Act = "-" THEN CurrLine = LinkedListInsert(CurrLine, " subwfb " + CurrV1 + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
        Else
          IF Act = "+" THEN CurrLine = LinkedListInsert(CurrLine, " addwfc " + CurrV1 + ",F")
          IF Act = "-" THEN CurrLine = LinkedListInsert(CurrLine, " subwfb " + CurrV1 + ",F")
        End If
      End If
      'Carry or borrow may now have occured, must check on next byte
      CheckCarry = -1

    ElseIf ModeAVR Then

      'Put parameters into registers
      'Overwrite V1?
      Ovr = "O:"
      If UCase(Left(V1, 7)) = "SYSTEMP" Then Ovr = ""
      If UCase(Left(V1, 11)) = "SYSCALCTEMP" Then Ovr = ""

      'Put V1 into register, keep name in R1
      NewCode = LinkedListCreate
      R1 = PutInRegister(NewCode, Ovr + CurrV1, "BYTE", Origin)
      CurrLine = LinkedListInsertList(CurrLine, NewCode)
      'Put V2 into register, keep name in R2
      NewCode = LinkedListCreate
      R2 = PutInRegister(NewCode, CurrV2, "BYTE", Origin)
      CurrLine = LinkedListInsertList(CurrLine, NewCode)
      If CurrVarByte = 0 Then
        AddVar V1, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
        AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
      End If

      If CheckCarry Then
        If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " adc " + R1 + "," + R2)
        If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " sbc " + R1 + "," + R2)
      Else
        If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " add " + R1 + "," + R2)
        If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " sub " + R1 + "," + R2)
      End If

      'Copy result to output variable
      NewCode = CompileVarSet("[byte]" + R1, "[byte]" + GetByte(AV, CurrVarByte), Origin)
      CurrLine = LinkedListInsertList(CurrLine, NewCode)
      FreeCalcVar R2
      FreeCalcVar R1
      'Carry or borrow may now have occured, must check on next byte
      CheckCarry = -1
    End If

    AddSubNextByte:

  Next

  'Replace sum with answer variable
 AddSubAnswer:
  OutList->CodeEnd = CurrLine
  Return AV

END FUNCTION

FUNCTION CompileCalcCondition(OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String

  Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2, AVH, CT1, CT2, SNT
  Dim As Integer SourceSub, DestSub
  Dim As String Cmd, CalcVarType, VarBit
  Dim As Integer OutVal, PD, IsBitTest
  Dim As LinkedListElement Pointer CurrLine
  CurrLine = OutList->CodeEnd

  'Calculate <,>,=,~. Replace sum with variable containing answer

  'Unusual symbols:
  ' ~ (not equal)
  ' } (equal or greater)
  ' { (less or equal)

  'Check if both are constants
  IF IsConst(V1) AND IsConst(V2) And INSTR(V1, "@") = 0 AND INSTR(V2, "@") = 0 THEN
    OutVal = 0
    IF Act = "=" THEN OutVal = MakeDec(V1) = MakeDec(V2)
    IF Act = "~" THEN OutVal = MakeDec(V1) <> MakeDec(V2)
    IF Act = "<" THEN OutVal = MakeDec(V1) < MakeDec(V2)
    IF Act = ">" THEN OutVal = MakeDec(V1) > MakeDec(V2)
    IF Act = "{" THEN OutVal = MakeDec(V1) <= MakeDec(V2)
    IF Act = "}" THEN OutVal = MakeDec(V1) >= MakeDec(V2)
    If OutVal Then OutVal = 255 Else OutVal = 0
    Return Str(OutVal)
  END IF

  'Get types
  SourceSub = GetSubID(Origin)
  If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
  V1Type = TypeOfValue(V1, Subroutine(SourceSub))
  V2Type = TypeOfValue(V2, Subroutine(SourceSub))
  If CastOrder(V1Type) > CastOrder(V2Type) Then
    CalcType = V1Type
  Else
    CalcType = V2Type
  End If
  DestType = "BYTE" 'Remove any doubt that may exist! (unlikely)
  V1 = DelType(V1): V2 = DelType(V2)
  V1O = V1: V2O = V2

  IsBitTest = 0
  If (TypeOfValue(V1, Subroutine(SourceSub)) = "BIT" Or IsConst(V1)) And (TypeOfValue(V2, Subroutine(SourceSub)) = "BIT" Or IsConst(V2)) Then
    IsBitTest = -1
  End If

  'Remove cast from output var
  Answer = DelType(Answer)

  'Generate asm code for sum

  'Special shortcut code for bit test
  If IsBitTest And IsConst(V2) Then

    'Translate less/more into equal/not equal
    If Act = "<" Or Act = ">" Then Act = "~"
    If Act = "{" Or Act = "}" Then Act = "="
    'Decide test status
    Dim As Integer TestFor = 0
    If Act = "=" And MakeDec(V2) <> 0 Then TestFor = 1
    If Act = "~" And MakeDec(V2) = 0 Then TestFor = 1

    'Get Bit name and number
    Dim As String BitName
    BitName = V1
    Replace BitName, ".", ","

    AddVar "SysByteTempX", "Byte", 1, Subroutine(SourceSub), "REAL", Origin, , -1
    If ModePIC Then
      'Bit referenced by variable?
      VarBit = Mid(V1, InStr(V1, ".") + 1)
      If Not IsConst(VarBit) And Not HasSFRBit(VarBit) Then
        CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V1 + "=" + Str(TestFor), "FALSE", Origin, Subroutine(SourceSub)))
        CurrLine = LinkedListInsert(CurrLine, " btfss STATUS,Z")
      Else
        CurrLine = LinkedListInsert(CurrLine, " clrf SysByteTempX")
        If TestFor = 1 Then
          CurrLine = LinkedListInsert(CurrLine, " btfsc " + BitName)
        Else
          CurrLine = LinkedListInsert(CurrLine, " btfss " + BitName)
        End If
      End If

      CurrLine = LinkedListInsert(CurrLine, " comf SysByteTempX,F")
      GoTo CompileConditionDone
    ElseIf ModeAVR Then
    
      'Incorrect handler - this just set the CLR!! - corrected #1377
      CurrLine = LinkedListInsert(CurrLine, " ldi	SysValueCopy,255")
      CurrLine = LinkedListInsert(CurrLine, " mov	SYSBYTETEMPX,SysValueCopy")        
      'Pre 1377 change
      '        CurrLine = LinkedListInsert(CurrLine, " clr SysByteTempX")
      'End if #1377 change

      CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V1 + "=" + Str(TestFor), "FALSE", Origin, Subroutine(SourceSub)))
      'Incorrect syntax, extra ,F... a tie over from PIC stuff... maybe Hugh made an error - corrected #1377 was " com SysByteTempX,F")
      CurrLine = LinkedListInsert(CurrLine, " com SysByteTempX")
      GoTo CompileConditionDone
    End If
  End If

  'Copy parameters
  If CalcType = "STRING" Then
    'Copy pointers
    'SetStringPointers (V1, V2, Origin)
    'Print "String comparison ", V1, V1Type, V2, V2Type, Origin
    CurrLine = LinkedListInsertList(CurrLine, SetStringPointers(V1, V2, Subroutine(SourceSub), Origin))
  Else
    CalcVarType = CalcType
    If CalcVarType = "BIT" Then CalcVarType = "BYTE"
    'declare SysCalcTempA, SysCalcTempB and SysCalcTempX
    AddVar "Sys" + CalcVarType + "TempA", CalcVarType, 1, Subroutine(SourceSub), "REAL", Origin, , -1
    AddVar "Sys" + CalcVarType + "TempB", CalcVarType, 1, Subroutine(SourceSub), "REAL", Origin, , -1
    AddVar "SysByteTempX", "BYTE", 1, Subroutine(SourceSub), "REAL", Origin, , -1

    'Copy values
  '   If Act = ">" Or Act = "}" Then
    If Act = ">" Or Act = "{" Then
      CT1 = "Sys" + CalcVarType + "TempB"
      CT2 = "Sys" + CalcVarType + "TempA"
    Else
      CT1 = "Sys" + CalcVarType + "TempA"
      CT2 = "Sys" + CalcVarType + "TempB"
    End If
    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, CT1, Origin))
    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, CT2, Origin))
  End If

  'Decide which sub to use
  IF Act = "=" Or Act = "~" THEN SNT = "SysCompEqual"
  'IF Act = "<" Or Act = ">" THEN SNT = "SysCompLessThan"
  'IF Act = "{" OR Act = "}" THEN SNT = "SysCompLessOrEqual"
  IF Act = "<" Or Act = ">" Or Act = "{" OR Act = "}" Then SNT = "SysCompLessThan"
  IF CalcType = "WORD" THEN SNT += "16"
  IF CalcType = "INTEGER" THEN
    If Act = "=" Or Act = "~" Then
      SNT += "16"
    Else
      SNT += "INT"
    End If
  End If
  If CalcType = "LONG" Then SNT += "32"
  IF CalcType = "SINGLE" THEN SNT += "Single"
  IF CalcType = "STRING" THEN SNT += "String"

  'Call calculation sub
  SNT = UCase( SNT )     'added as SNT was case sensitive
  CurrLine = LinkedListInsert(CurrLine, " call " + SNT)
  RequestSub (Subroutine(SourceSub), SNT)

  'Invert answer for not equal
  'If Act = "~" Then
  If Act = "~" Or Act = "{" Or Act = "}" Then
    If ModePIC Then
      CurrLine = LinkedListInsert(CurrLine, " comf SysByteTempX,F")
    ElseIf ModeAVR Then
      CurrLine = LinkedListInsert(CurrLine, " com SysByteTempX")
    End If
  End If

  'Write answer
  CompileConditionDone:
  AV = "SysByteTempX"
  OutList->CodeEnd = CurrLine
  Return AV

END FUNCTION

FUNCTION CompileCalcLogic (OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String

  Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2
  Dim As String CurrV1, CurrV2, CurrAct, Temp, Cmd, Ovr
  Dim As Integer CD, SourceSub, DestSub, CurrVarByte
  Dim As LinkedListElement Pointer CurrLine, NewCode
  CurrLine = OutList->CodeEnd

  'Delete casts (not needed for logic operations)
  V1 = DelType(V1): V2 = DelType(V2)

  'Get types
  'Print V1, Act, V2, AV
  SourceSub = GetSubID(Origin)
  If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
  V1Type = TypeOfValue(V1, Subroutine(SourceSub))
  V2Type = TypeOfValue(V2, Subroutine(SourceSub))
  DestType = TypeOfVar(Answer, Subroutine(DestSub))
  CalcType = DestType

  'Remove cast from output var
  Answer = DelType(Answer)

  'Check types
  If V1Type = "SINGLE" Or V1Type = "STRING" Then
    Temp = Message("OperandTypeMismatch")
    Replace Temp, "%operand%", Act
    Replace Temp, "%type%", V1Type
    LogError Temp, Origin
  End If
  If V2Type = "SINGLE" Or V2Type = "STRING" Then
    Temp = Message("OperandTypeMismatch")
    Replace Temp, "%operand%", Act
    Replace Temp, "%type%", V2Type
    LogError Temp, Origin
  End If

  'Convert bit vars to byte
  If V1Type = "BIT" And Not IsConst(V1) Then
    V1Type = "BYTE"
    V1O = V1
    V1 = GetCalcVar("BYTE")
    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1O, V1, Origin))
  End If
  If V2Type = "BIT" And Not IsConst(V2) Then
    V2Type = "BYTE"
    V2O = V2
    V2 = GetCalcVar("BYTE")
    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2O, V2, Origin))
  End If

  'Get output var
  AV = Answer

  'If V1 and V2 are constant, produce constant result
  If (Act <> "!" And IsConst(V1) And IsConst(V2)) OR (Act = "!" And IsConst(V2)) Then
    IF Act = "&" THEN Return Trim(Str(MakeDec(V1) AND MakeDec(V2)))
    IF Act = "|" THEN Return Trim(Str(MakeDec(V1) OR MakeDec(V2)))
    IF Act = "#" THEN Return Trim(Str(MakeDec(V1) XOR MakeDec(V2)))
  End If

  'If V1 not const, V2 const, swap
  If IsConst(V2) Then
    Swap V1, V2
    Swap V1Type, V2Type
  End If

  'Perform operation on each pair of bytes
  For CurrVarByte = 0 To GetTypeSize(CalcType) - 1

    'Get current byte of V1 and V2 (if they exist) or 0
    If GetTypeSize(V1Type) > CurrVarByte Then
      CurrV1 = GetByte(V1, CurrVarByte)
    Else
      CurrV1 = "0"
    End If
    If GetTypeSize(V2Type) > CurrVarByte Then
      CurrV2 = GetByte(V2, CurrVarByte)
    Else
      CurrV2 = "0"
    End If
    CurrAct = Act

    'Optimisations for when one var is 255
    'Print "Before:", CurrV1, CurrAct, CurrV2
    If CurrV1 = "255" Then
      '255 & x = 0 | x
      If CurrAct = "&" Then
        CurrV1 = "0"
        CurrAct = "|"
      '255 | x = 0 | 255
      ElseIf CurrAct = "|" Then
        CurrV1 = "0"
        CurrV2 = "255"
      End If
    ElseIf CurrV2 = "255" Then
      'x & 255 = x | 0
      If CurrAct = "&" Then
        CurrV2 = "0"
        CurrAct = "|"
      'x | 255 = 0 | 255 (need V1 = 0 for below)
      ElseIf CurrAct = "|" Then
        CurrV1 = "0"
      End If
    End If

    'Should always try to have constant in V1 only
    If CurrV2 = "0" And Not IsConst(CurrV1) Then
      'Can swap order of each byte with no consequences
      'Don't need to swap types (VxType) either.
      Swap CurrV1, CurrV2
    End If
    'Print "After:", CurrV1, CurrAct, CurrV2

    'Possible inputs:
    'V1 = 0, V2 = 0 > AV = 0
    'V1 = 0, V2 = x > AV = V2 (OR/XOR) or 0 (AND)
    'V1 = x, V2 = x > AV = V1 op V2

    'First case, no V1 or V2
    If CurrV1 = "0" And CurrV2 = "0" Then
      '0 | 0 = 0, 0 & 0 = 0, 0 # 0 = 0
      If ModePIC Then
        CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(AV, CurrVarByte))
      ElseIf ModeAVR Then
        NewCode = CompileVarSet("0", "[byte]" + GetByte(AV, CurrVarByte), Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
      End If

    'Second case, V2 only
    ElseIf CurrV1 = "0" And CurrV2 <> "0" Then
      'Anything AND 0 = 0, so clear result
      If CurrAct = "&" Then
        If ModePIC Then
          CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(AV, CurrVarByte))
        ElseIf ModeAVR Then
          'CurrLine = LinkedListInsert(CurrLine, " clr " + GetByte(AV, CurrVarByte))
          NewCode = CompileVarSet("0", "[byte]" + GetByte(AV, CurrVarByte), Origin)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
        End If

      'Anything OR/XOR 0 = Anything, so copy V2
      ElseIf CurrAct = "|" Or CurrAct = "#" Then
        NewCode = CompileVarSet("[byte]" + CurrV2, "[byte]" + GetByte(AV, CurrVarByte), Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
      End If

    'Third case, V1 and V2
    ElseIf CurrV1 <> "0" And CurrV2 <> "0" Then
      If ModePIC Then
        Cmd = " movf " + GetByte(V1, CurrVarByte) + ",W"
        If IsConst(V1) Then Cmd = " movlw " + GetByte(V1, CurrVarByte)
        CurrLine = LinkedListInsert(CurrLine, Cmd)
        IF CurrAct = "&" THEN Cmd = " andwf "
        IF CurrAct = "|" THEN Cmd = " iorwf "
        IF CurrAct = "#" THEN Cmd = " xorwf "
        If CurrVarByte = 0 Then
          AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
        End If
        If V2 <> AV Then
          CurrLine = LinkedListInsert(CurrLine, Cmd + GetByte(V2, CurrVarByte) + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
        Else
          CurrLine = LinkedListInsert(CurrLine, Cmd + GetByte(V2, CurrVarByte) + ",F")
        End If

      ElseIf ModeAVR Then
        'If AVR, put values in registers
        NewCode = LinkedListCreate
        R1 = PutInRegister(NewCode, CurrV1, "BYTE", Origin)
        If CurrVarByte = 0 Then AddVar V1, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        'Overwrite V2?
        Ovr = "O:"
        If UCase(Left(V2, 7)) = "SYSTEMP" Then Ovr = ""
        If UCase(Left(V2, 11)) = "SYSCALCTEMP" Then Ovr = ""

        NewCode = LinkedListCreate
        R2 = PutInRegister(NewCode, Ovr + CurrV2, "BYTE", Origin)
        If CurrVarByte = 0 Then AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
        CurrLine = LinkedListInsertList(CurrLine, NewCode)

        IF CurrAct = "&" THEN CurrLine = LinkedListInsert(CurrLine, " and " + R2 + "," + R1)
        IF CurrAct = "|" THEN CurrLine = LinkedListInsert(CurrLine, " or " + R2 + "," + R1)
        IF CurrAct = "#" THEN CurrLine = LinkedListInsert(CurrLine, " eor " + R2 + "," + R1)

        'Copy result to output variable
        NewCode = CompileVarSet(R2, "[byte]" + GetByte(AV, CurrVarByte), Origin)
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        If R1 <> AV Then FreeCalcVar R2
        If R2 <> AV Then FreeCalcVar R1
      End If

    End If

  Next

  OutList->CodeEnd = CurrLine
  Return AV

END FUNCTION

FUNCTION CompileCalcMult (OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String

  Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2, AVH, SNT
  Dim As String Temp
  Dim As Integer SourceSub, DestSub, CD, PD
  Dim As LinkedListElement Pointer CurrLine, NewCode
  CurrLine = OutList->CodeEnd

  'Calculate *, /, %. Replace sum with variable containing answer

  'Get types
  SourceSub = GetSubID(Origin)
  If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
  V1Type = TypeOfValue(V1, Subroutine(SourceSub))
  V2Type = TypeOfValue(V2, Subroutine(SourceSub))
  DestType = TypeOfVar(Answer, Subroutine(DestSub))
  CalcType = DestType
  If CastOrder(V1Type) > CastOrder(CalcType) Then CalcType = V1Type
  If CastOrder(V2Type) > CastOrder(CalcType) Then CalcType = V2Type
  V1 = DelType(V1): V2 = DelType(V2)
  V1O = V1: V2O = V2

  'Remove cast from output var
  Answer = DelType(Answer)

  'Get output var
  If DestType = CalcType Then
    AV = Answer
  Else
    AV = GetCalcVar(CalcType)
  End If

  'Generate asm code for sum
  'Check if both are constants
  IF IsConst(V1) AND IsConst(V2) And INSTR(V1, "@") = 0 AND INSTR(V2, "@") = 0 THEN
    If IsIntType(CalcType) Then
      IF Act = "*" THEN AV = Str(MakeDec(V1) * MakeDec(V2))
      IF Act = "/" THEN AV = Str(INT(MakeDec(V1) / MakeDec(V2)))
      IF Act = "%" THEN AV = Str(MakeDec(V1) MOD MakeDec(V2))
    ElseIf IsFloatType(CalcType) Then
      IF Act = "*" THEN AV = Str(MakeDecFloat(V1) * MakeDecFloat(V2))
      IF Act = "/" THEN AV = Str(MakeDecFloat(V1) / MakeDecFloat(V2))
      IF Act = "%" THEN AV = Str(MakeDecFloat(V1) MOD MakeDecFloat(V2))
    End If
    GOTO MultDivAnswer
  END IF

  'If multiply, make sure V2 is the constant
  If Act = "*" And IsConst(V1) And (Not IsConst(V2)) Then
    Swap V1, V2
    Swap V1O, V2O
    Swap V1Type, V2Type
  End If

  'Simplifications
  If Act = "*" And V2 = "0" Then AV = "0": Goto MultDivAnswer
  If (Act = "*" OR Act = "/") And V2 = "1" Then AV = V1: Goto MultDivAnswer
  If (Act = "/" Or Act = "%") And V1 = "0" Then AV = "0": Goto MultDivAnswer

  'Use hardware multiply instruction for byte*byte multiply
  If CalcType = "BYTE" And Act = "*" And ChipFamily = 16 Then
    If IsConst(V1) Then
      CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V1)))
    Else
      CurrLine = LinkedListInsert(CurrLine, " movf " + V1 + ",W")
    End If
    If IsConst(V2) Then
      CurrLine = LinkedListInsert(CurrLine, " mullw " + Str(MakeDec(V2)))
    Else
      CurrLine = LinkedListInsert(CurrLine, " mulwf " + V2)
    End If
    AV = "PRODL"
    Goto MultDivAnswer
  End If

  'Rotate left/right for mult or div by 2
  IF CalcType = "BYTE" And MakeDec(V2) = 2 Then

    AddVar V1, "BYTE", 1, 0, "REAL", Origin
    If ModeAVR Then
      Temp = ""
      If Act = "%" THEN Temp = "H:"
      R1 = PutInRegister(NewCode, "O:" + Temp + V1, CalcType, Origin)
      CurrLine = LinkedListInsertList(CurrLine, NewCode)
    End If

    If Act = "*" Then
      If ModePIC Then
        CurrLine = LinkedListInsert(CurrLine, " bcf STATUS,C")
        If V1 = Answer Then
          CurrLine = LinkedListInsert(CurrLine, " rlf " + V1 + ",F")
        Else
          CurrLine = LinkedListInsert(CurrLine, " rlf " + V1 + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + Answer)
        End If
        AV = Answer
      ElseIf ModeAVR Then
        CurrLine = LinkedListInsert(CurrLine, " lsl " + R1)
        AV = R1
      End If

    ElseIf Act = "/" Then
      If ModePIC Then
        CurrLine = LinkedListInsert(CurrLine, " bcf STATUS,C")
        If V1 = Answer Then
          CurrLine = LinkedListInsert(CurrLine, " rrf " + V1 + ",F")
        Else
          CurrLine = LinkedListInsert(CurrLine, " rrf " + V1 + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + Answer)
        End If
        AV = Answer
      ElseIf ModeAVR Then
        CurrLine = LinkedListInsert(CurrLine, " lsr " + R1)
        AV = R1
      End If

    ElseIf Act = "%" Then
      If ModePIC Then
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Answer)
        ILC += 1
        CurrLine = LinkedListInsert(CurrLine, " btfss " + V1 + ",0")
        CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
        CurrLine = LinkedListInsert(CurrLine, " incf " + Answer + ",F")
        CurrLine = LinkedListInsert(CurrLIne, "ENDIF" + Str(ILC))
        AV = Answer
      ElseIf ModeAVR Then
        CurrLine = LinkedListInsert(CurrLine, " andi " + R1 + ",1")
        AV = R1
      End If
    End If
    Goto MultDivAnswer
  End If

  'Declare variables needed for calculation
  AddVar "Sys" + CalcType + "TempA", CalcType, 1, 0, "REAL", Origin, , -1
  AddVar "Sys" + CalcType + "TempB", CalcType, 1, 0, "REAL", Origin, , -1
  AddVar "Sys" + CalcType + "TempX", CalcType, 1, 0, "REAL", Origin, , -1
  IF Act = "/" OR Act = "%" THEN AddVar "SysDivLoop", "BYTE", 1, 0, "REAL", Origin, , -1

  'Copy parameters
  CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, "Sys" + CalcType + "TempA", Origin))
  CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, "Sys" + CalcType + "TempB", Origin))

  'Use hardware multiply instruction for byte*byte multiply
  If CalcType = "BYTE" And Act = "*" And ModeAVR And HMult Then
    CurrLine = LinkedListInsert(CurrLine, " mul SysByteTempA,SysByteTempB")
    AV = "SysByteTempX"
    Goto MultDivAnswer
  End If

  'Decide which sub to call
  IF Act = "*" THEN SNT = "SysMultSub"
  IF Act = "/" THEN SNT = "SysDivSub"
  IF Act = "%" THEN SNT = "SysDivSub"
  IF CalcType = "WORD" THEN SNT += "16"
  IF CalcType = "INTEGER" THEN SNT += "INT"
  If CalcType = "LONG" Then SNT += "32"
  IF CalcType = "SINGLE" THEN SNT += "SINGLE"
  IF CalcType = "DOUBLE" THEN SNT += "DOUBLE"
  IF CalcType = "ULONGINT" THEN SNT += "64"

  'Special case for SINGLE
  If Act = "%" and CalcType = "SINGLE" Then SNT = "SYSMODSUBSINGLE"
  If Act = "%" and CalcType = "DOUBLE" Then SNT = "SYSMODSUBDOUBLE"
  

  'Call calculation sub
  SNT = Ucase( SNT )  'added was case sensistive
  CurrLine = LinkedListInsert(CurrLine, " call " + SNT)
  RequestSub (Subroutine(SourceSub), SNT) 'Mark as used

  'Write answer
  If Act = "*" Then AV = "Sys" + CalcType + "TempX"
  If Act = "/" Then AV = "Sys" + CalcType + "TempA"
  If Act = "%" Then AV = "Sys" + CalcType + "TempX"
  
  If (( compilerdebug and cCOMPILECALCMULT ) = cCOMPILECALCMULT ) Then
    Print " 8 COMPILECALCMULT :" + V1, V2, AV, CalcType
  End If    

  'Replace sum with answer variable
 MultDivAnswer:
  OutList->CodeEnd = CurrLine
  Return AV

END FUNCTION

Function CompileCalcUnary(OutList As CodeSection Pointer, Act As String, V2 As String, Origin As String, Answer As String) As String

  Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2, AVH, SNT
  Dim As String Temp, Ovr, OldCalcType
  Dim As Integer SourceSub, DestSub, CD, CurrVarByte
  Dim As LinkedListElement Pointer CurrLine, NewCode
  CurrLine = OutList->CodeEnd

  'Delete casts (not needed for logic operations)
  If Act = "!" Then V2 = DelType(V2)

  'Get types
  SourceSub = GetSubID(Origin)
  If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
  V2Type = TypeOfValue(V2, Subroutine(SourceSub))
  DestType = TypeOfVar(Answer, Subroutine(DestSub))
  CalcType = DestType
  V2 = DelType(V2)

  'Remove cast from output var
  Answer = DelType(Answer)

  'If op is negate, return an integer
  If Act = "-" And CastOrder(CalcType) < CastOrder("INTEGER") Then CalcType = "INTEGER"

  'Check types
  If V2Type = "STRING" Or (V2Type = "SINGLE" and Act = "!") Then
    Temp = Message("OperandTypeMismatch")
    Replace Temp, "%operand%", Act
    Replace Temp, "%type%", V2Type
    LogError Temp, Origin
  End If

  'Get output var
  AV = Answer
  OldCalcType = CalcType

  'Deal with bit variable input
  'Convert bit vars to byte
  If V2Type = "BIT" And Not IsConst(V2) Then
    'If answer is also a bit and operation is NOT, use copy with invert
    If DestType = "BIT" And Act = "!" And V2 <> AV Then
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, AV, Origin, -1))
      OutList->CodeEnd = CurrLine
      Return AV
    Else
      If V2 = AV Then
        V2Type = "BYTE"
        V2O = V2
        If IsSysTemp(AV) Then
          V2 = AV
        Else
          V2 = GetCalcVar("BYTE")
          AV = V2
        End If
        DestType = "BYTE"
        CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2O, V2, Origin))
      Else
        V2Type = "BYTE"
        V2O = V2
        If IsSysTemp(AV) Then
          V2 = AV
        Else
          V2 = GetCalcVar("BYTE")
        End If
        CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2O, V2, Origin))
      End If
    End If
  End If

  'If V2 is constant, produce constant result
  If IsConst(V2) And InStr(V2, "@") = 0 Then
    IF Act = "!" THEN
      If CalcType = "BYTE" Then Return Trim(Str((NOT MakeDec(V2)) AND 255))
      If CalcType = "WORD" OR CalcType = "INTEGER" Then Return Trim(Str((Not MakeDec(V2) And 65535)))
      If CalcType = "LONG" Then Return Trim(Str(NOT MakeDec(V2)))
    ElseIf Act = "-" THEN
      'Do nothing
      Return "-" + V2
    End If
  End If

  'On AVR, need to do negate on entire variable at once
  If ModeAVR And Act = "-" Then

    'Put value in register
    'Overwrite V2?
    Ovr = "O:"
    If UCase(Left(V2, 7)) = "SYSTEMP" Then Ovr = ""
    If UCase(Left(V2, 11)) = "SYSCALCTEMP" Then Ovr = ""

    NewCode = LinkedListCreate
    R2 = PutInRegister(NewCode, Ovr + V2, CalcType, Origin)
    AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
    CurrLine = LinkedListInsertList(CurrLine, NewCode)

    'Perform operation
    For CurrVarByte = 0 To GetTypeSize(CalcType) - 1
      CurrLine = LinkedListInsert(CurrLine, " com " + GetByte(R2, CurrVarByte))
    Next

    CurrLine = LinkedListInsert(CurrLine, " inc " + GetByte(R2, 0))
    For CurrVarByte = 1 To GetTypeSize(CalcType) - 1
      CurrLine = LinkedListInsert(CurrLine, " brne PC + 2")
      CurrLine = LinkedListInsert(CurrLine, " inc " + GetByte(R2, CurrVarByte))
    Next

    'Copy result to output variable
    If R2 <> AV Then
      NewCode = CompileVarSet(R2, AV, Origin)
      CurrLine = LinkedListInsertList(CurrLine, NewCode)
      FreeCalcVar R2
    End If

  Else

    'Process each byte of variable one at a time
    For CurrVarByte = 0 To GetTypeSize(CalcType) - 1

      'NOT (Byte and Word)

      'No matching byte in input, so set output byte to 255
      If CurrVarByte >= GetTypeSize(V2Type) Then
        If ModePIC Then
          If ChipFamily <> 16 Then
            CurrLine = LinkedListInsert(CurrLine, " movlw 255")
            CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
          End If
          If ChipFamily = 16 Then CurrLine = LinkedListInsert(CurrLine, " setf " + GetByte(AV, CurrVarByte))
        ElseIf ModeAVR Then
          'R2 = GetCalcVar("BYTE")
          'CurrLine = LinkedListInsert(CurrLine, " clr " + R2)
          'CurrLine = LinkedListInsert(CurrLine, " com " + R2)
          R2 = "255"
        End If

      'Matching byte in input for current output byte, so invert input
      Else
        If ModePIC Then
          If AV <> V2 Then
            CurrLine = LinkedListInsert(CurrLine, " comf " + GetByte(V2, CurrVarByte) + ",W")
            CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
            If CurrVarByte = 0 Then
              AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
              AddVar AV, "BYTE", 1, Subroutine(DestSub), "REAL", Origin
            End If
          Else
            CurrLine = LinkedListInsert(CurrLine, " comf " + GetByte(V2, CurrVarByte) + ",F")
            If CurrVarByte = 0 Then
              AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
            End If
          End If

        ElseIf ModeAVR Then
          'If AVR, put current byte in register
          'Overwrite V2?
          Ovr = "O:"
          If UCase(Left(V2, 7)) = "SYSTEMP" Then Ovr = ""
          If UCase(Left(V2, 11)) = "SYSCALCTEMP" Then Ovr = ""

          NewCode = LinkedListCreate
          R2 = PutInRegister(NewCode, Ovr + GetByte(V2, CurrVarByte), "BYTE", Origin)
          AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
          CurrLine = LinkedListInsertList(CurrLine, NewCode)

          'Perform operation
          CurrLine = LinkedListInsert(CurrLine, " com " + R2)

        End If
      End If

      If ModeAVR Then
        'Copy result to output variable
        If OldCalcType = "BIT" And R2 <> Answer Then
          NewCode = CompileVarSet(R2, Answer, Origin)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
          FreeCalcVar R2
          AV = Answer
        ElseIf AV <> R2 Then
          NewCode = CompileVarSet(R2, "[byte]" + GetByte(AV, CurrVarByte), Origin)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
          FreeCalcVar R2
        End If
      End If

    Next

    If Act = "-" And CalcType = "INTEGER" Or CalcType = "LONGINT" Then
      '(PIC only, can't reach here on AVR)
      CurrLine = LinkedListInsert(CurrLine, " incf " + GetByte(AV, 0) + ",F")
      For CurrVarByte = 1 To GetTypeSize(CalcType) - 1
        CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
        CurrLine = LinkedListInsert(CurrLine, " incf " + GetByte(AV, CurrVarByte) + ",F")
      Next
    End If

  End If

  OutList->CodeEnd = CurrLine
  Return AV

End Function

Function CompileConditions (Condition As String, IfTrue As String, Origin As String, CompSub As SubType Pointer = 0) As LinkedListElement Pointer

  Dim As String V1, V2, Op, R1, R2, Temp, Cmd, BI, VarName, Sum, Reg, CondType
  Dim As String BitName, VarBit
  Dim As Integer PD, Complex, Value1, Value2, OutVal, R2Literal, T, S, WD
  Dim As Integer IsBitTest, IsInverted, BitAndValue
  Dim As SubType Pointer CurrSub
  Dim As LinkedListElement Pointer OutList, CurrLine, NewCode
  Dim As VariableType Pointer FoundUserVar

  'If Condition = IfTrue, execute next instruction

  'Initialise
  OutList = LinkedListCreate
  CurrLine = OutList

  'Get origin
  'Origin = ""
  IF INSTR(Condition, ";?F") <> 0 THEN
    Origin = Mid(Condition, INSTR(Condition, ";?F"))
    Condition = Left(Condition, INSTR(Condition, ";?F") - 1)
  END If
  Condition = Trim(Condition)
  If CompSub <> 0 Then
    CurrSub = CompSub
  Else
    CurrSub = Subroutine(GetSubID(Origin))
  End If
  'IF Origin = "" THEN PRINT "No Origin!", Condition

  'Trim extra brackets on Condition
  If Left(Condition, 1) = "(" And Right(Condition, 1) = ")" Then
    If CountOccur(Condition, "';()") = 2 Then
      Condition = Trim(Mid(Condition, 2, Len(Condition) - 2))
    End If
  End If

  'Decide whether to compile inline or using CompileCalc
  'Complex =
  ' 0: One condition, byte, compile inline
  ' 1: More than one condition or not byte, so compile as calculation
  ' -1: No condition, single variable, check if 0 or not 0.

  CheckConditionAgain:
  Complex = 0
  CondType = TypeOfValue(Condition, Subroutine(GetSubID(Origin)))
  IF CountOccur(Condition, "';=~<>{}") = 0 THEN Complex = -1
  IF CountOccur(Condition, "';=~<>{}+-*/%&|#!") >= 2 THEN Complex = 1
  IF CountOccur(Condition, "';+-*/%&|#!") >= 1 THEN Complex = 1
  If CondType <> "BIT" And CondType <> "BYTE" THEN Complex = 1
  'PRINT Condition, GetSub(Origin), CondType, Complex

  'No conditions - check if variable is 0 or non-0
  'NOT is not taken into account above, need to detect it here
  IF Complex = -1 THEN
    If IsConst(Condition) Then
      If MakeDec(Condition) = 0 Then
        Condition = "AlwaysFalse"
      Else
        Condition = "AlwaysTrue"
      End If
      Return OutList
    Else
      If CondType = "BYTE" Then

        IsInverted = 0
        If Left(Condition, 1) = "!" Then
          Condition = Trim(Mid(Condition, 2))
          IsInverted = -1
        End If

        If ModePIC Then
          CurrLine = LinkedListInsert(CurrLine, " movf " + Condition + ",F")
          If IsInverted Then
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfsc STATUS,Z"
            IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfss STATUS,Z"
          Else
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfss STATUS,Z"
            IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfsc STATUS,Z"
          End If
          CurrLine = LinkedListInsert(CurrLine, Cmd)
        ElseIf ModeAVR Then
          NewCode = LinkedListCreate
          Reg = PutInRegister(NewCode, Condition, "BYTE", Origin)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
          CurrLine = LinkedListInsert(CurrLine, " tst " + Reg)
          If IsInverted Then
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " brne PC + 2"
            IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " breq PC + 2"
          Else
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " breq PC + 2"
            IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " brne PC + 2"
          End If
          CurrLine = LinkedListInsert(CurrLine, Cmd)
        End If

      Else
        'Checking a single bit
        'Can do this just as efficiently using bit test code in section below
        If InStr(Condition, " ") = 0 Then Condition = Condition + "=1"
        Complex = 0
      End If
    End If
  END If

  'One condition and byte, so compile inline
  IF Complex = 0 THEN
    OP = ""
    If INSTR(Condition, "=") <> 0 THEN OP = "="
    IF INSTR(Condition, "~") <> 0 THEN OP = "~"
    IF INSTR(Condition, "<") <> 0 THEN OP = "<"
    IF INSTR(Condition, ">") <> 0 THEN OP = ">"
    IF INSTR(Condition, "{") <> 0 THEN OP = "{"
    IF INSTR(Condition, "}") <> 0 THEN OP = "}"

    'Read input variables
    If OP = "" Then
      V1 = UCase(Left(Condition, INSTR(Condition, " ") - 1))
      V2 = UCase(Mid(Condition, INSTR(Condition, " ") + 1))
      OP = "="
    Else
      V1 = UCase(Left(Condition, INSTR(Condition, OP) - 1))
      DO WHILE INSTR(V1, " ") <> 0: Replace V1, " ", "": LOOP
      V2 = UCase(Mid(Condition, INSTR(Condition, OP) + 1))
      DO WHILE INSTR(V2, " ") <> 0: Replace V2, " ", "": Loop
    End If

    IsBitTest = 0
    If (TypeOfValue(V1, CurrSub) = "BIT" Or IsConst(V1)) And (TypeOfValue(V2, CurrSub) = "BIT" Or IsConst(V2)) Then
      IsBitTest = -1
    End If
    'Print Condition, ":"; V1, OP, V2, IsBitTest

    'If both V1 and V2 are constants, evaluate condition now
    If IsConst(V1) And IsConst(V2) Then
      Value1 = MakeDec(V1)
      Value2 = MakeDec(V2)
      OutVal = 0
      Select Case OP
      Case "=": OutVal = Value1 = Value2
      Case "~": OutVal = Value1 <> Value2
      Case "<": OutVal = Value1 < Value2
      Case ">": OutVal = Value1 > Value2
      Case "{": OutVal = Value1 <= Value2
      Case "}": OutVal = Value1 >= Value2
      End Select

      If OutVal Then
        Condition = "AlwaysTrue"
      Else
        Condition = "AlwaysFalse"
      End If

      Return OutList
    End If

    'Code for a bit test
    IF IsBitTest THEN
      'If reading from a pin, record it
      Dim As PinDirType Pointer CurrPinDir
      CurrPinDir = GetPinDirection(V1)
      If CurrPinDir <> 0 Then
        CurrPinDir->ReadFrom = -1
      End If

      'If V1 is const and V2 is not, swap
      If IsConst(V1) And Not IsConst(V2) Then
        Swap V1, V2
      End If

      'Compare bit variable to bit constant
      If IsConst(V2) Then
        Dim As String BitTestTemp = "SysBitTest"

        T = 1
        'Get var and bit
        BI = FixBit(V1, Origin)
        If INSTR(BI, ".") <> 0 Then Replace BI, ".", ","
        'Get state to test
        S = 1: If V2 = "0" THEN S = 0
        If OP = "~" Or OP = ">" Or OP = "<" Then S = 1 - S

        'Is bit referenced by byte?
        'If no var, might be dealing with an SFR bit
        BitName = V1
        If InStr(BitName, ".") = 0 Then
          BitName = GetWholeSFR(BitName)
        End If
        'Should now have a var!
        If InStr(BitName, ".") <> 0 Then
          VarName = Left(BitName, InStr(BitName, ".") - 1)
          VarBit = Mid(BitName, InStr(BitName, ".") + 1)
        Else
          'This is an error
          Temp = Message("InvalidBitRead")
          Replace Temp, "%bit%", V1
          LogError(Temp, Origin)
          VarBit = "0"
        End If

        If Not IsConst(VarBit) And Not HasSFRBit(VarBit) Then
          'Bit number isn't a number or SFR, must be referenced by variable
          'Get maximum allowed bit number
          BitAndValue = GetTypeSize(TypeOfVar(VarName, CurrSub)) * 8 - 1

          'Variable to operate on specified by FSR (or FSR0) on PIC or SysReadA on AVR
          'Bit to operate on comes from SysByteTempX
          'New state of bit is in bit 0 of SysByteTempB
          If ChipFamily = 12 Or ChipFamily = 14 Then
            CurrLine = LinkedListInsert(CurrLine, " movlw low " + VarName)
            CurrLine = LinkedListInsert(CurrLine, " movwf FSR")
            If HighFSR Then CurrLine = LinkedListInsert(CurrLine, " bankisel " + VarName)

          ElseIf ChipFamily = 15 Then
            CurrLine = LinkedListInsert(CurrLine, " movlw low " + VarName)
            CurrLine = LinkedListInsert(CurrLine, " movwf FSR0L")
            CurrLine = LinkedListInsert(CurrLine, " movlw high " + VarName)
            CurrLine = LinkedListInsert(CurrLine, " movwf FSR0H")

          ElseIf ChipFamily = 16 Then
            CurrLine = LinkedListInsert(CurrLine, " lfsr 0," + VarName)

          End If

          If ModePIC Then
            CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(BitAndValue))
            CurrLine = LinkedListInsert(CurrLine, " andwf " + VarBit + ", W")
            CurrLine = LinkedListInsert(CurrLine, " movwf SysByteTempX")
            CurrLine = LinkedListInsert(CurrLine, " call SysReadBit")
            BI = "STATUS,Z" ' Code below can do checking of Z. Need to invert S, Z is set when test bit clear
            S = 1 - S
            RequestSub(CurrSub, "SysReadBit")
            If Not IsIOReg(VarName) Then AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin

          ElseIf ModeAVR Then
            VarName = GetRealIOName(VarName)
            CurrLine = LinkedListInsert(CurrLine, " ldi SysStringA, low(" + VarName + ")")
            CurrLine = LinkedListInsert(CurrLine, " ldi SysStringA_H, high(" + VarName + ")")

            CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(VarBit, "SysReadA", Origin))
            CurrLine = LinkedListInsert(CurrLine, " andi SysReadA, " + Str(BitAndValue))

            CurrLine = LinkedListInsert(CurrLine, " rcall SysReadBit")
            BI = "SREG,Z"
            S = 1 - S

            RequestSub(CurrSub, "SysReadBit")
            If Not IsIOReg(VarName) Then AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin
            AddVar "SysStringA", "WORD", 1, 0, "REAL", Origin, , -1 'Needs to be global

          End If
        End If

        If ModePIC Then
          IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
            Cmd = " btfss "
            IF S = 1 THEN Cmd = " btfsc "
          END IF
          IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
            Cmd = " btfsc "
            IF S = 1 THEN Cmd = " btfss "
          END IF

          CurrLine = LinkedListInsert(CurrLine, Cmd + BI)
          If INSTR(BI, ",") <> 0 Then
            VarName = Left(BI, INSTR(BI, ",") - 1)
            AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin
          End If
        ElseIf ModeAVR Then
          VarName = GetRealIOName(Left(BI, INSTR(BI, ",") - 1))

          If ChipAVRDX Then
            If IsIORegDX(VarName) Then
              'Map legacy megaAVR register to DX register using the Alias Addressing
              If (( compilerdebug and cAVRDXDEBUG ) = cAVRDXDEBUG )  Then
                CurrLine = LinkedListInsert(CurrLine, ";IOAVRDX #3")
              End if
              VarName = GetSysVarAliasName ( VarName ) 
            End If          
          End If

          BI = Mid(BI, INSTR(BI, ",") + 1)
          If VarName = "SREG" Then
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
              Cmd = " brbs "
              IF S = 1 THEN Cmd = " brbc "
            END IF
            IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
              Cmd = " brbc "
              IF S = 1 THEN Cmd = " brbs "
            END IF
            CurrLine = LinkedListInsert(CurrLine, Cmd + BI + ",PC + 2")

          ElseIf IsRegister(VarName) Then
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
              Cmd = " sbrs "
              IF S = 1 THEN Cmd = " sbrc "
            END IF
            IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
              Cmd = " sbrc "
              IF S = 1 THEN Cmd = " sbrs "
            END IF
            CurrLine = LinkedListInsert(CurrLine, Cmd + VarName + "," + BI)

          ElseIf IsLowIOReg(VarName) Then
          
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
              Cmd = " sbis "
              IF S = 1 THEN Cmd = " sbic "
            END IF
            IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
              Cmd = " sbic "
              IF S = 1 THEN Cmd = " sbis "
            END IF
            CurrLine = LinkedListInsert(CurrLine, Cmd + VarName + "," + BI)

          ElseIf IsIOReg(VarName) Then
            AddVar BitTestTemp, "BYTE", 1, CurrSub, "REAL", Origin, , -1
            CurrLine = LinkedListInsert(CurrLine, " in " + BitTestTemp + "," + VarName)
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
              Cmd = " sbrs " + BitTestTemp + ","
              IF S = 1 THEN Cmd = " sbrc " + BitTestTemp + ","
            ElseIF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
              Cmd = " sbrc " + BitTestTemp + ","
              IF S = 1 THEN Cmd = " sbrs " + BitTestTemp + ","
            END IF
            CurrLine = LinkedListInsert(CurrLine, Cmd + BI)

          Else
            AddVar BitTestTemp, "BYTE", 1, CurrSub, "REAL", Origin, , -1
            CurrLine = LinkedListInsert(CurrLine, " lds " + BitTestTemp + "," + VarName)
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
              Cmd = " sbrs " + BitTestTemp + ","
              IF S = 1 THEN Cmd = " sbrc " + BitTestTemp + ","
            ElseIF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
              Cmd = " sbrc " + BitTestTemp + ","
              IF S = 1 THEN Cmd = " sbrs " + BitTestTemp + ","
            END IF
            CurrLine = LinkedListInsert(CurrLine, Cmd + BI)
            AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin

          End If
        End If

      'Need to compare 2 bit variables
      Else
        'Check operator
        If Op <> "=" And Op <> "~" Then
          LogError(Message("BadBitCompareOp"), Origin)
        End If

        If ModePIC Then
          'Is indirect mode needed?
          R1 = V1
          If InStr(V1, ".") <> 0 Then R1 = Mid(V1, InStr(V1, ".") + 1)
          R2 = V2
          If InStr(V2, ".") <> 0 Then R2 = Mid(V2, InStr(V2, ".") + 1)
          If (Not IsConst(R1) And Not HasSFRBit(R1)) Or (Not IsConst(R2) And Not HasSFRBit(R2)) Then
            'one or both variables use indirect mode
            CurrLine = LinkedListInsert(CurrLine, " clrf SysByteTempB")
            CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V1 + "=1", "TRUE", Origin, CurrSub))
            CurrLine = LinkedListInsert(CurrLine, " comf SysByteTempB,F")
            CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V2 + "=1", "TRUE", Origin, CurrSub))
            CurrLine = LinkedListInsert(CurrLine, " comf SysByteTempB,F")
            AddVar "SysByteTempB", "BYTE", 1, CurrSub, "REAL", Origin, , -1

          Else
            'Get variables
            R1 = FixBit(V1, Origin)
            If INSTR(R1, ".") <> 0 Then Replace R1, ".", ","
            R2 = FixBit(V2, Origin)
            If INSTR(R2, ".") <> 0 Then Replace R2, ".", ","

            'Add to resolve undeclared variable, as an undeclared variable causes ASM error and the memory has not be allocated
            FoundUserVar =  HashMapGet( @(Subroutine(0)->Variables) , Left(R2,Instr(R2,",")-1) )
            If FoundUserVar = 0 Then
              'Create a variable but do not mark as used - this will then throw an Explict error as expected
              AddVar Left(R2,Instr(R2,",")-1), TypeOfValue( Left(R2,Instr(R2,",")-1) , CurrSub), 1, CurrSub, "REAL", Origin, -1 , 0
            Else
              'variable has been located just validate its use
              AddVar Left(R2,Instr(R2,",")-1), TypeOfValue( Left(R2,Instr(R2,",")-1) , CurrSub), 1, CurrSub, "REAL", Origin, -1 , -1
            End If

            'clear w, xor if each var set
            CurrLine = LinkedListInsert(CurrLine, " clrw")
            CurrLine = LinkedListInsert(CurrLine, " btfsc " + R1)
            CurrLine = LinkedListInsert(CurrLine, " xorlw 255")
            CurrLine = LinkedListInsert(CurrLine, " btfsc " + R2)
            CurrLine = LinkedListInsert(CurrLine, " xorlw 255")
          End If
          'If W is 0, variables are equal (inverted 0 or 2 times)
          If Op = "=" Then
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfsc STATUS,Z"
            If INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfss STATUS,Z"
          Else
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfss STATUS,Z"
            If INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfsc STATUS,Z"
          End If
          CurrLine = LinkedListInsert(CurrLine, Cmd)

        ElseIf ModeAVR Then
          'Use recursion, easier!
          'Otherwise, same algorithm as PIC: clr, if, com, if, com, test
          'clear w, xor if each var set
          AddVar "SysCalcTempX", "BYTE", 1, CurrSub, "REAL", Origin, , -1
          CurrLine = LinkedListInsert(CurrLine, " clr SysCalcTempX")
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V1 + "=1", "TRUE", Origin, CurrSub))
          CurrLine = LinkedListInsert(CurrLine, " com SysCalcTempX")
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V2 + "=1", "TRUE", Origin, CurrSub))
          CurrLine = LinkedListInsert(CurrLine, " com SysCalcTempX")
          'If W is 0, variables are equal (inverted 0 or 2 times)
          If Op = "=" Then
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " sbrs SysCalcTempX,0"
            If INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " sbrc SysCalcTempX,0"
          Else
            IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " sbrc SysCalcTempX,0"
            If INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " sbrs SysCalcTempX,0"
          End If
          CurrLine = LinkedListInsert(CurrLine, Cmd)

        End If

      End If

    'If the condition is not a bit test, then run this code:
    Else
      'Add needed variables
      'Generate warning if comparing bit variable to other type of variable
      'If using PIC and a bit variable is found, put bit variable into temporary variable
      If Not IsConst(V1) Then
        If TypeOfValue(V1, CurrSub) = "BIT" Then
          Temp = Message("WarningBitByteCompare")
          Replace Temp, "%nonbit%", V2
          LogWarning(Temp, Origin)
          If ModePIC Then
            AddVar "SYSCALCTEMPA", "BYTE", 1, CurrSub, "REAL", Origin, , -1
            CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, "SysCalcTempA", Origin))
            V1 = "SysCalcTempA"
          End If
        End If
        AddVar V1, "BYTE", 1, CurrSub, "REAL", Origin
      End If
      If Not IsConst(V2) Then
        If TypeOfValue(V2, CurrSub) = "BIT" Then
          Temp = Message("WarningBitByteCompare")
          Replace Temp, "%nonbit%", V1
          LogWarning(Temp, Origin)

          If ModePIC Then
            AddVar "SYSCALCTEMPB", "BYTE", 1, CurrSub, "REAL", Origin, , -1
            CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, "SysCalcTempB", Origin))
            V2 = "SysCalcTempB"
          End If

        End If
        AddVar V2, "BYTE", 1, CurrSub, "REAL", Origin
      End If

      'Remove any casts
      If InStr(V1, "]") <> 0 Then
        V1 = Trim(Mid(V1, InStr(V1, "]") + 1))
      End If
      If InStr(V2, "]") <> 0 Then
        V2 = Trim(Mid(V2, InStr(V2, "]") + 1))
      End If

      'Code to run comparison
      'Used by all AVR compare modes
      If ModeAVR Then
        'Swap V1/V2 as needed
        IF ((OP = "=" Or OP = "~") And IsConst(V1)) OR OP = ">" OR OP = "{" Then
          Swap V1, V2
        End If

        'Put V1 in SysCalcTempA
        If IsRegister(V1) Then
          R1 = V1
        Else
          R1 = "SysCalcTempA"
          AddVar R1, "BYTE", 1, CurrSub, "REAL", Origin, , -1
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, "SysCalcTempA", Origin))
        End If
        'ElseIf IsIOReg(V1) Then
        ' R1 = "SysCalcTempA"
        ' CurrLine = LinkedListInsert(CurrLine, " in SysCalcTempA," + V1)
        'ElseIf IsConst(V1) Then
        ' R1 = "SysCalcTempA"
        ' CurrLine = LinkedListInsert(CurrLine, " ldi SysCalcTempA," + Str(MakeDec(V1)))
        'Else
        ' R1 = "SysCalcTempA"
        ' CurrLine = LinkedListInsert(CurrLine, " lds SysCalcTempA," + V1)
        ' AddVar V1, "BYTE", 1, 0, "REAL", Origin
        'End If

        'Put V2 in SysCalcTempB
        R2Literal = 0
        If IsRegister(V2) Then
          R2 = V2
        ElseIf IsIOReg(V2) Then
          R2 = "SysCalcTempB"
          AddVar R2, "BYTE", 1, CurrSub, "REAL", Origin, , -1
          CurrLine = LinkedListInsert(CurrLine, " in SysCalcTempB," + V2)
        ElseIf IsConst(V2) Then
          R2 = Str(MakeDec(V2))
          R2Literal = -1
        Else
          R2 = "SysCalcTempB"
          AddVar R2, "BYTE", 1, CurrSub, "REAL", Origin, , -1
          CurrLine = LinkedListInsert(CurrLine, " lds SysCalcTempB," + V2)
          AddVar V2, "BYTE", 1, CurrSub, "REAL", Origin
        End If

        'Compare
        If R2Literal Then
          If Val(R2) = 0 And (OP = "=" Or OP = "~") Then
            CurrLine = LinkedListInsert(CurrLine, " tst " + R1)
          Else
            CurrLine = LinkedListInsert(CurrLine, " cpi " + R1 + "," + R2)
          End If
        Else
          CurrLine = LinkedListInsert(CurrLine, " cp " + R1 + "," + R2)
        End If

      End If

      'Code for equal/not equal
      IF OP = "=" OR OP = "~" THEN

        If ModePIC Then
          'V1 must always be a variable. V2 can be variable or constant.
          IF IsConst(V1) Then
            Temp = V1
            V1 = V2
            V2 = Temp
          End If

          If IsConst(V2) And MakeDec(V2) = 0 Then
            CurrLine = LinkedListInsert(CurrLine, " movf " + V1 + ",F")
          ElseIf IsConst(V2) And MakeDec(V2) = 1 Then
            CurrLine = LinkedListInsert(CurrLine, " decf " + V1 + ",W")
          ElseIf IsConst(V2) And MakeDec(V2) = 255 Then
            CurrLine = LinkedListInsert(CurrLine, " incf " + V1 + ",W")
          Else
            IF IsConst(V2) Then
              CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V2)))
            Else
              CurrLine = LinkedListInsert(CurrLine, " movf " + V2 + ",W")
            End If
            CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
          End If
          IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
            Cmd = " btfss STATUS, Z"
            IF OP = "=" THEN Cmd = " btfsc STATUS, Z"
          END IF
          IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
            Cmd = " btfsc STATUS, Z"
            IF OP = "=" THEN Cmd = " btfss STATUS, Z"
          END IF
          CurrLine = LinkedListInsert(CurrLine, Cmd)
        ElseIf ModeAVR Then

          'Compare is inserted above

          'Branch
          IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
            Cmd = " breq PC + 2"
            IF OP = "=" THEN Cmd = " brne PC + 2"
          END IF
          IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
            Cmd = " brne PC + 2"
            IF OP = "=" THEN Cmd = " breq PC + 2"
          END IF
          CurrLine = LinkedListInsert(CurrLine, Cmd)

        END IF
      END IF

      'Code for greater than/less than
      IF OP = "<" OR OP = ">" THEN
        'COSC = COSC + 1: CheckTemp(COSC) = " bsf STATUS, C"
        If ModePIC Then
          IF OP = ">" THEN Swap V1, V2
          'Take bigger from smaller. V2 should be bigger. Take V2 from V1
          'mov small into W
          'sub big from W
          IF IsConst(V1) And ChipFamily = 12 THEN
            If InStr(V1, "@") <> 0 Then
              CurrLine = LinkedListInsert(CurrLine, " movlw " + V1)
            Else
              CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V1)))
            End If
            CurrLine = LinkedListInsert(CurrLine, " movwf SysIFTemp")
            V1 = "SysIFTemp": AddVar V1, "BYTE", 1, 0, "REAL", Origin, , -1
          END If

          IF IsConst(V2) THEN Cmd = " movlw " + Str(MakeDec(V2))
          IF NOT IsConst(V2) THEN Cmd = " movf " + V2 + ",W"
          CurrLine = LinkedListInsert(CurrLine, Cmd)

          If IsConst(V1) Then
            CurrLine = LinkedListInsert(CurrLine, " sublw " + Str(MakeDec(V1)))
          Else
            CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
          End If

          IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfss STATUS, C"
          IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfsc STATUS, C"
          CurrLine = LinkedListInsert(CurrLine, Cmd)

        ElseIf ModeAVR Then
          IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brsh PC + 2")
          IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brlo PC + 2")

        End If
      END IF

      'Code for less or equal/more or equal
      IF OP = "{" OR OP = "}" THEN
        If ModePIC Then
          IF OP = "{" THEN Swap V1, V2
          'Take smaller from bigger. V1 should be bigger or equal. Take V2 from V1
          'mov small into W
          'sub big from W

          IF IsConst(V1) And ChipFamily = 12 THEN
            If InStr(V1, "@") <> 0 Then
              CurrLine = LinkedListInsert(CurrLine, " movlw " + V1)
            Else
              CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V1)))
            End If
            CurrLine = LinkedListInsert(CurrLine, " movwf SysIFTemp")
            V1 = "SysIFTemp": AddVar V1, "BYTE", 1, 0, "REAL", Origin, , -1
          END If

          IF IsConst(V2) Then
            If InStr(V2, "@") <> 0 Then
              Cmd = " movlw " + V2
            Else
              Cmd = " movlw " + Str(MakeDec(V2))
            End If
          End If
          IF NOT IsConst(V2) THEN Cmd = " movf " + V2 + ",W"
          CurrLine = LinkedListInsert(CurrLine, Cmd)

          'CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
          If IsConst(V1) Then
            CurrLine = LinkedListInsert(CurrLine, " sublw " + Str(MakeDec(V1)))
          Else
            CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
          End If

          IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfsc STATUS, C"
          IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfss STATUS, C"
          CurrLine = LinkedListInsert(CurrLine, Cmd)

        ElseIf ModeAVR Then
          IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brlo PC + 2")
          IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brsh PC + 2")
        End If
      END IF

    END IF

  End IF

  'More than one condition or not byte, so compile as calculation
 ComplexCondition:
  IF Complex = 1 THEN

    DO WHILE INSTR(Condition, " ") <> 0: Replace Condition, " ", "": LOOP
    T = 0
    Sum = Condition


    If HashMapGet(Constants, "DISABLE1173" ) = 0 then
      If ModeAVR  Then
        'resolve System bit tests silent failures where complex bit tests with =1 or -=0 fail
        Dim as Integer SysByteAddress, SysBitAddress, SysByteCounter,SysBitCounter, SysFound, Conditions
        Dim SysTempSearchString as String
        SysFound = 0
        Conditions = 0
        For SysByteCounter = 0 to 8 'max of 8 assumed... could be more. I have no way of knowing as this point of the process
          For SysBitCounter = 0 to 7
            SysTempSearchString = "SYSBITVAR"+Str( SysByteCounter )+"."+Str(SysBitCounter)
            If Instr( Sum, SysTempSearchString ) <> 0  Then
                SysFound = SysFound + 1
            End if
          Next
        Next
        'Find some chars that offend
        Conditions = CountOccur(Sum, "<")
        Conditions = Conditions + CountOccur(Sum, ">")
        Conditions = Conditions + CountOccur(Sum, "=")
        Conditions = Conditions + CountOccur(Sum, "{")
        Conditions = Conditions + CountOccur(Sum, "}")
        Conditions = Conditions + CountOccur(Sum, "~")


        If SysFound > 1 and Conditions > 0 Then LogError "Potential invalid BIT comparison error when using complex Bitwise logic.  Reduce complexity of Bitwise comparison", Origin
      End if
    End If

    FreeCalcVar ""

    Dim As CodeSection Pointer CodeDest
    CodeDest = 0

    CompileCalc(Sum, "", Origin, CodeDest)
    NewCode = CodeDest->CodeList
    If NewCode->Next <> 0 Then
      CurrLine = LinkedListInsertList(CurrLine, NewCode)
      If ModePIC Then
        IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfsc " + Sum + ",0"
        IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfss " + Sum + ",0"
      ElseIf ModeAVR Then
        IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " sbrc " + Sum + ",0"
        IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " sbrs " + Sum + ",0"
      End If
      CurrLine = LinkedListInsert(CurrLine, Cmd)
    Else
      If MakeDec(Sum) = 0 Then
        Condition = "AlwaysFalse"
      Else
        Condition = "AlwaysTrue"
      End If
      Return OutList
    End If
  END IF

  Return OutList
End Function

Sub CompileDim (CurrSub As SubType Pointer)
  Dim As String VarName, VarType, VarAlias, VarFixedLocIn, Origin, InLine, SiStr, Temp
  Dim As String AliasList(16)
  Dim As Integer PD, IsAlias, Si, CD, VarFixedLoc, NewVarCount, ALC
  Dim As LinkedListElement Pointer CurrLine

  'Don't try to read dims from macro
  'Only read from macro invocation
  If CurrSub->IsMacro Then Return

  Dim NewVarList(100) As String
  NewVarCount = 0

  'Mark sub as having had variables read
  'Haven't actually read them yet (won't until end of this sub)
  'But might cause an infinite loop if this isn't set
  CurrSub->VarsRead = -1

  CurrLine = CurrSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = CurrLine->Value

    If Left(InLine, 4) = "DIM " THEN

      InLine = Mid(InLine, 5)
      CurrLine = LinkedListDelete(CurrLine)

      'Get origin
      Origin = ""
      If INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      'Get name
      VarName = InLine
      If INSTR(VarName, " AS ") <> 0 THEN VarName = Left(VarName, INSTR(VarName, " AS ") - 1)
      If INSTR(VarName, " ALIAS ") <> 0 THEN VarName = Left(VarName, INSTR(VarName, " ALIAS ") - 1)
      If INSTR(VarName, " AT ") <> 0 THEN VarName = Left(VarName, INSTR(VarName, " AT ") - 1)
      VarName = Trim(VarName)
      If INSTR(VarName, ",") = 0 Then
        NewVarList(1) = VarName
        NewVarCount = 1
      Else
        NewVarCount = 0
        Do While INSTR(VarName, ",") <> 0
          NewVarCount += 1
          NewVarList(NewVarCount) = Trim(Left(VarName, INSTR(VarName, ",") - 1))
          VarName = Trim(Mid(VarName, INSTR(VarName, ",") + 1))
        Loop
        NewVarCount += 1
        NewVarList(NewVarCount) = VarName
      End If

      'Get type
      VarType = "BYTE"
      IF INSTR(InLine, " AS ") <> 0 Then
        VarType = Mid(InLine, INSTR(InLine, " AS ") + 3)
        IF INSTR(VarType, "(") <> 0 Then VarType = Left(VarType, INSTR(VarType, "(") - 1)
        IF INSTR(VarType, " ALIAS ") <> 0 Then VarType = Left(VarType, INSTR(VarType, " ALIAS ") - 1)
        IF INSTR(VarType, " AT ") <> 0 Then VarType = Left(VarType, INSTR(VarType, " AT ") - 1)
        VarType = Trim(UCase(VarType))
      END IF

      'Check if alias
      IsAlias = 0
      VarAlias = ""
      IF INSTR(InLine, " ALIAS ") <> 0 Then
        IsAlias = -1
        VarAlias = Trim(Mid(InLine, INSTR(InLine, " ALIAS ") + 7))
        IF INSTR(VarAlias, " AS ") <> 0 Then VarAlias = Left(VarAlias, INSTR(VarAlias, " AS ") - 1)
        IF INSTR(VarAlias, " AT ") <> 0 Then VarAlias = Left(VarAlias, INSTR(VarAlias, " AT ") - 1)
      End If

      'Check if fixed location specified
      VarFixedLoc = -1
      If InStr(InLine, " AT ") <> 0 Then
        VarFixedLocIn = Trim(Mid(InLine, INSTR(InLine, " AT ") + 4))
        IF INSTR(VarFixedLocIn, " AS ") <> 0 Then VarFixedLocIn = Left(VarFixedLocIn, INSTR(VarFixedLocIn, " AS ") - 1)
        IF INSTR(VarFixedLocIn, " ALIAS ") <> 0 Then VarFixedLocIn = Left(VarFixedLocIn, INSTR(VarFixedLocIn, " ALIAS ") - 1)

        'Is a calculation needed to get location?
        If IsCalc(VarFixedLocIn) Then
          Calculate VarFixedLocIn
        End If

        'Is fixed location given as a constant?.
        If IsConst(VarFixedLocIn) Then
          VarFixedLoc = MakeDec(VarFixedLocIn)
        'Is fixed location given as a reference to an SFR?
        Else
          'Not supported, but can be added later if needed
        End If
        'Print "Var loc:"; VarFixedLoc

        'If also an alias, show an error. Can't be in two locations at once
        If IsAlias Then
          LogError(Message("VarAliasAndAt"), Origin)
        End If
      End If

      'Ensure alias variable is also declared
      If IsAlias Then
        GetTokens (VarAlias, AliasList(), ALC, ",")
        For PD = 1 To ALC
          If Not IsConst(AliasList(PD)) And Not IsCalc(AliasList(PD)) Then
            'Is variable an alias to itself?
            Temp = Right(AliasList(PD), 2)
            If Temp = "_H" Or Temp = "_U" Or Temp = "_E" Then
              Temp = Left(AliasList(PD), Len(AliasList(PD)) - 2)
            Else
              Temp = AliasList(PD)
            End If
            If Temp = VarName Then
              'Trying to add self as alias, prevent
              LogError(Message("AliasToSelf"), Origin)
              VarAlias = ""
              IsAlias = 0
            End If
          End If
        Next
      End If

      For CV = 1 to NewVarCount
        InLine = NewVarList(CV)
        'Get size
        Si = 1

        'added error handling for arrays and processing of arrays using constant
        IF INSTR(InLine, "(") <> 0 Then
          If CountOccur( InLine, "(" ) <> CountOccur( InLine, ")" ) Then
            SiStr = Message("BadVarName")
            Replace SiStr, "%var%", InLine
            LogError SiStr, Origin
            goto process_next_line:
          End If
          SiStr = Mid(InLine, INSTR(InLine, "(") )

          If IsCalc(SiStr) Then
            Calculate SiStr
            Si = VAL(SiStr)
          Else
            Si = VAL(Mid(InLine, INSTR(InLine, "(") + 1))
          End If

          InLine = Trim(Left(InLine, INSTR(InLine, "(") - 1))
        End If

        'Check variable name
        Dim ValidVarName As Integer = -1
        If VarType = "BIT" AndAlso Left(InLine, 9) = "SYSBITVAR" AndAlso InStr(InLine, ".") <> 0 Then
          'Redefining bit var, ignore
          ValidVarName = 0

        ElseIf IsConst(InLine) Or Not IsValidName(InLine) Then
          Temp = Message("BadVarName")
          If Left(InLine, 7) = ";STRING" Then
            Replace Temp, "%var%", Chr(34) + GetString(InLine, 0) + Chr(34)
          Else
            Replace Temp, "%var%", InLine
          End If
          If InStr(Origin, ";STARTUP") <> 0 Then
            Origin = RTrim(Left(Origin, InStr(Origin, ";STARTUP") - 1))
          EndIf
          LogError Temp, Origin
          ValidVarName = 0
        END If

        'Add var
        If ValidVarName Then
          If IsAlias Then
            AddVar(InLine, VarType, Si, CurrSub, "ALIAS:" + VarAlias, Origin, , -1, 0)
          Else
            AddVar(InLine, VarType, Si, CurrSub, "REAL", Origin, VarFixedLoc, -1, 0)
          End If
        End If
      Next
    End If
    process_next_line:
    CurrLine = CurrLine->Next
  Loop

End Sub

SUB CompileDir (CompSub As SubType Pointer)

  Dim As String Origin, InLine, VarName, Port, Temp, Direction
  Dim As String TrisPort, PortLetter
  Dim As Integer DirPort, NotIOPort, SearchPos, FoundPos
  Dim As LinkedListElement Pointer CurrLine
  Dim As PinDirType Pointer CurrPinDir
  Dim As SysVarType Pointer CurrVar

  FoundCount = 0

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = CurrLine->Value
    IF Left(InLine, 4) = "DIR " Then

      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      FoundCount += 1
      InLine = Trim(Mid(InLine, 4))
      Direction = LTrim(Mid(InLine, INSTR(InLine, " ") + 1))
      Port = UCase(RTrim(Left(InLine, INSTR(InLine, " ") - 1)))

      'Handle original (early 2006) form of Dir command (ie. Dir B0 Output)
      IF INSTR(Port, "PORT") = 0 AND (Left(ChipName, 2) = "16" Or Left(ChipName, 2) = "18") AND LEN(Port) = 2 THEN
        Port = "PORT" + Left(Port, 1) + "." + Mid(Port, 2)
      'early 2006 version of full port (ie. Dir B number)
      ElseIf Len(Port) = 1 AND (Left(ChipName, 2) = "16" Or Left(ChipName, 2) = "18") Then
        Port = "PORT" + Port
      End If
      If Left(ChipName, 2) = "10" Or Left(ChipName, 2) = "12" Then
        If Port = "" Or Port = "IO" Then
          Port = "GPIO"
        End If
      End If
      Replace Direction, "PUT", ""

      'Should now have PORTx, GPIO, PORTx.y or GPIO.y in Port. If not, something is wrong
      'Check for sys var bit that refers to port bit
      CurrVar = HashMapGet(SysVarBits, Port)
      If CurrVar <> 0 Then
        Port = CurrVar->Parent + "." + Str(CurrVar->Location)
      End If

      'Determine if DIR sets 1 bit or whole port
      DirPort = -1
      IF INSTR(Port, ".") <> 0 THEN DirPort = 0

      'Get the variable that sets port direction
      PortLetter = Port
      Replace PortLetter, "PORT", ""
      Replace PortLetter, "GP", ""
      Replace PortLetter, "LAT", ""
      Replace PortLetter, "PIN", ""
      TrisPort = PortLetter
      If InStr(TrisPort, ".") <> 0 Then TrisPort = Left(TrisPort, InStr(TrisPort, ".") - 1)
      If ModePIC Then
        VarName = "TRIS" + PortLetter
      ElseIf ModeAVR Then
        VarName = "DDR" + PortLetter
      End If

      'Check that a pin is being set
      'TrisPort should be a single letter or IO, no bits
      NotIOPort = 0

      IF LEN(TrisPort) <> 1 AND TrisPort <> "IO" THEN 
        NotIOPort = -1
      '  print 1
      END IF
      If TrisPort < "A" Or TrisPort > "Z" Then 
        NotIOPort = -1
       ' print 2
      END IF
      'SFR for variable should exist, unless 12 bit core PIC
      Temp = VarName
      If Instr(Temp, ".") <> 0 Then 
        Temp = Left(Temp, Instr(Temp, ".") - 1)
        'print 3
      END IF
      If ChipFamily <> 12 And GetSysVar(Temp) = 0 Then 
        NotIOPort = -1
        'print 4
      END IF
      'Error if port is not valid
      If NotIOPort Then
        Temp = Message("NotIO")
        Replace Temp, "%var%", Port
        LogError Temp, Origin
      End If

      'Add code
      'Whole port mode
      IF DirPort THEN

        'Record settings
        CurrPinDir = GetPinDirection(Port)
        If CurrPinDir <> 0 Then
          If Direction = "IN" Then
            CurrPinDir->SetIn = -1
            CurrPinDir->WholePort = -1
            CurrPinDir->WholePortDir = 255
          ElseIf Direction = "OUT" Then
            CurrPinDir->SetOut = -1
            CurrPinDir->WholePort = -1
            CurrPinDir->WholePortDir = 0
          Else
            'If port is set to mixed direction, store direction
            If IsConst(Direction) Then
              CurrPinDir->WholePort = -1
              CurrPinDir->WholePortDir = MakeDec(Direction)
            Else
              CurrPinDir->WholePort = -1
              CurrPinDir->WholePortDir = -1
            End If
          End If
        End If

        'Generate code
        IF Direction = "IN" THEN
          If ModePIC Then
            If ChipFamily = 12 And Not IsIOReg(VarName) Then
              CurrLine->Value = " movlw 255"
              CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
            Else
              If PSP = 1 AND VarName = "TRISE" Then
                CurrLine->Value = VarName + "=239"
              Else
                CurrLine->Value = VarName + "=255"
              End If
            End If
          ElseIf ModeAVR Then
            CurrLine = LinkedListDelete(CurrLine)
            CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("0", VarName, Origin))
          End If

        ElseIf Direction = "OUT" THEN
          If ModePIC Then
            If ChipFamily = 12 And Not IsIOReg(VarName) Then
              CurrLine->Value = " clrw"
              CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
            Else
              CurrLine->Value = " clrf " + VarName
            End If
          ElseIf ModeAVR Then
            CurrLine = LinkedListDelete(CurrLine)
            CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("255", VarName, Origin))
          End If

        Else
          If ChipFamily = 12 And Not IsIOReg(VarName) Then
            If IsConst(Direction) Then
              CurrLine->Value = " movlw " + Direction
              CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
            Else
              CurrLine->Value = " movf " + Direction + ",W"
              CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
            End If
          Else
            CurrLine->Value = VarName + "=" + Direction
          End If
        End If

      'Single pin mode
      Else
        'On 12 bit PIC, need to create shadow register
        'Can't set individual pin directions otherwise
        If ChipFamily = 12 Then
          FoundPos = -1
          For SearchPos = 1 To PinDirShadows
            If PinDirShadow(SearchPos) = TrisPort Then
              FoundPos = SearchPos
              Exit For
            End If
          Next
          If FoundPos = -1 Then
            PinDirShadows += 1
            PinDirShadow(PinDirShadows) = TrisPort
          End If
        End If

        'Record setting
        CurrPinDir = GetPinDirection(Port)
        If CurrPinDir <> 0 Then
          If Direction = "IN" Then
            CurrPinDir->SetIn = -1

            'Check that pin can be input
            If CurrPinDir->AllowedDirections <> "" And InStr(CurrPinDir->AllowedDirections, "i") = 0 Then
              Temp = Message("WarningPinNotInput")
              Replace Temp, "%pin%", Port
              LogWarning Temp, Origin
            End If

          ElseIf Direction = "OUT" Then
            CurrPinDir->SetOut = -1

            'Check that pin can be output
            If CurrPinDir->AllowedDirections <> "" And InStr(CurrPinDir->AllowedDirections, "o") = 0 Then
              Temp = Message("WarningPinNotOutput")
              Replace Temp, "%pin%", Port
              LogWarning Temp, Origin
            End If

          End If
        End If

        'Generate code
        If ModePIC Then
          Replace VarName, ".", ","
          If Direction = "IN" Then
            CurrLine->Value = " bsf " + VarName
          ElseIf Direction = "OUT" Then
            CurrLine->Value = " bcf " + VarName
          Else
            LogError Message("BadDirection"), Origin
          End If
        ElseIf ModeAVR Then
          CurrLine = LinkedListDelete(CurrLine)
          If Direction = "IN" Then
            CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(VarName, "0", Origin))
          ElseIf Direction = "OUT" Then
            CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(VarName, "1", Origin))
          Else
            LogError Message("BadDirection"), Origin
          End If
        End If
      End If

    End If
    CurrLine = CurrLine->Next
  Loop
END SUB

SUB CompileDo (CompSub As SubType Pointer)
  FoundCount = 0
  Dim As String InLine, Temp, Origin, Mode, Condition, LoopOrigin
  Dim As Integer DL, CP, ExitFound, WD
  Dim As LinkedListElement Pointer CurrLine, FindLoop, LoopLoc, NewCode, OldLoopLoc

  DL = 0
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = UCase(CurrLine->Value)
    If Left(InLine, 3) = "DO " OR InLine = "DO" THEN

      'Get origin
      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      'Get position of condition
      CP = 0
      IF Left(InLine, 3) = "DO " THEN CP = 1
      DL = 1 'Nesting level
      ExitFound = 0 'Exit Do found?
      DLC = DLC + 1 'Increment do loop count
      CurrLine->Value = "SysDoLoop_S" + Str(DLC) + LabelEnd 'Add label to mark start
      GetMetaData(Currline)->IsLabel = -1

      'Find exit do, loop
      LoopLoc = 0
      FindLoop = CurrLine->Next
      Do While FindLoop <> 0
        Temp = UCase(FindLoop->Value)
        IF Left(Temp, 5) = "LOOP " OR Temp = "LOOP" THEN DL = DL - 1
        IF Left(Temp, 3) = "DO " OR Temp = "DO" THEN DL = DL + 1
        IF Left(Temp, 7) = "EXIT DO" AND DL = 1 Then
          ExitFound = 1
          If ModePIC Then
            FindLoop->Value = " goto SysDoLoop_E" + Str(DLC)
          End if
          If ModeAVR Then FindLoop->Value = " rjmp SysDoLoop_E" + Str(DLC)
          If ModeZ8 Then FindLoop->Value = " jp SysDoLoop_E" + Str(DLC)
        End If
        IF DL = 0 THEN LoopLoc = FindLoop: Exit Do
        FindLoop = FindLoop->Next
      Loop

      'Show error if no loop
      If LoopLoc = 0 Then
        LogError(Message("DoWithoutLoop"), Origin)

      'Otherwise, compile normally
      Else
        'Get origin of LOOP command
        LoopOrigin = ""
        IF INSTR(LoopLoc->Value, ";?F") <> 0 THEN
          LoopOrigin = Mid(LoopLoc->Value, INSTR(LoopLoc->Value, ";?F"))
          LoopLoc->Value = RTrim(Left(LoopLoc->Value, INSTR(LoopLoc->Value, ";?F") - 1))
        END If
        If LoopOrigin = "" Then LoopOrigin = Origin

        'Is the condition after the LOOP?
        IF Left(LoopLoc->Value, 5) = "LOOP " THEN CP = 2

        'Get mode {UNTIL | WHILE}, condition
        IF CP = 1 THEN
          Mode = LTrim(Mid(InLine, 4))
          If InStr(Mode, " ") <> 0 Then
            Condition = LCase(Mid(Mode, INSTR(Mode, " ") + 1))
            Mode = UCase(Left(Mode, INSTR(Mode, " ") - 1))
          End If

          'Is mode valid?
          If Mode <> "UNTIL" And Mode <> "WHILE" Then
            Temp = Message("InvalidDoMode")
            Replace Temp, "%mode%", Mode
            LogError Temp, Origin
          Else
            'Is condition present?
            If Condition = "" Then
              Temp = Message("DoWithoutCondition")
              Replace Temp, "%mode%", Mode
              LogError Temp, Origin
            End If
          End If
        ElseIf CP = 2 THEN
          Mode = LTrim(Mid(LoopLoc->Value, 6))
          If InStr(Mode, " ") <> 0 Then
            Condition = LCase(Mid(Mode, INSTR(Mode, " ") + 1))
            Mode = UCase(Left(Mode, INSTR(Mode, " ") - 1))
          End If

          If Mode <> "UNTIL" And Mode <> "WHILE" Then
            Temp = Message("InvalidDoMode")
            Replace Temp, "%mode%", Mode
            LogError Temp, LoopOrigin
          Else
            'Is condition present?
            If Condition = "" Then
              Temp = Message("DoWithoutCondition")
              Replace Temp, "%mode%", Mode
              LogError Temp, LoopOrigin
            End If
          End If
        END IF

        OldLoopLoc = LoopLoc

        'Compile with no condition
        If CP = 0 Then
          If ModePIC Then Temp = " goto SysDoLoop_S" + Str(DLC)
          If ModeAVR Then Temp = " rjmp SysDoLoop_S" + Str(DLC)
          If ModeZ8 Then Temp = " jp SysDoLoop_S" + Str(DLC)
          LoopLoc = LinkedListInsert(LoopLoc, Temp)

        'Compile with condition after DO
        ElseIf CP = 1 THEN
          Temp = "TRUE": IF Mode = "WHILE" THEN Temp = "FALSE"
          NewCode = CompileConditions(Condition, Temp, Origin)
          If (Mode = "WHILE" And Condition <> "AlwaysTrue") Or (Mode = "UNTIL" And Condition <> "AlwaysFalse") Then
            CurrLine = LinkedListInsertList(CurrLine, NewCode)
            If ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto SysDoLoop_E" + Str(DLC))
            If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp SysDoLoop_E" + Str(DLC))
            If ModeZ8 Then CurrLine = LinkedListInsert(CurrLine, " jp SysDoLoop_E" + Str(DLC))
          End If

          If ModePIC Then Temp = " goto SysDoLoop_S" + Str(DLC)
          If ModeAVR Then Temp = " rjmp SysDoLoop_S" + Str(DLC)
          If ModeZ8 Then Temp = " jp SysDoLoop_S" + Str(DLC)
          LoopLoc = LinkedListInsert(LoopLoc, Temp)

        'Compile with condition after LOOP
        ElseIf CP = 2 THEN
          Temp = "TRUE": IF Mode = "UNTIL" THEN Temp = "FALSE"
          NewCode = CompileConditions(Condition, Temp, LoopOrigin)
          If (Mode = "WHILE" And Condition <> "AlwaysFalse") Or (Mode = "UNTIL" And Condition <> "AlwaysTrue") Then
            LoopLoc = LinkedListInsertList(LoopLoc, NewCode)

            If ModePIC Then Temp = " goto SysDoLoop_S" + Str(DLC)
            If ModeAVR Then Temp = " rjmp SysDoLoop_S" + Str(DLC)
            If ModeZ8 Then Temp = " jp SysDoLoop_S" + Str(DLC)
            LoopLoc = LinkedListInsert(LoopLoc, Temp)
          END IF
        End If

        'Create label for end of loop
        LoopLoc = LinkedListInsert(LoopLoc, "SysDoLoop_E" + Str(DLC) + LabelEnd)
        GetMetaData(LoopLoc)->IsLabel = -1
        'Delete Loop statement
        LinkedListDelete(OldLoopLoc)
      End IF
    End If

    CurrLine = CurrLine->Next
  Loop
  FoundCount = DLC

END SUB

SUB CompileExitSub (CompSub As SubType Pointer)

  Dim As String Temp
  Dim As LinkedListElement Pointer CurrLine
  FoundCount = 0

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    IF Left(UCase(CurrLine->Value), 8) = "EXIT SUB" OR Left(UCase(CurrLine->Value), 13) = "EXIT FUNCTION" THEN
      If ModePIC Then CurrLine->Value = " return"
      If ModeAVR Then CurrLine->Value = " ret"
      If ModeZ8 Then CurrLine->Value = " ret"
      FoundCount = FoundCount + 1
    End If

    IF Left(UCase(CurrLine->Value), 4) = "END " THEN
      Temp = CurrLine->Value
      IF INSTR(Temp, ";") <> 0 THEN Temp = Trim(Left(Temp, INSTR(Temp, ";") - 1))
      IF Temp = "END" THEN
        If ModePIC Then
          CurrLine->Value = " goto BASPROGRAMEND"
        End If
        If ModeAVR Then CurrLine->Value = " rjmp BASPROGRAMEND"
        If ModeZ8 Then CurrLine->Value = " jp BASPROGRAMEND"
        FoundCount = FoundCount + 1
      END IF
    END IF
    CurrLine = CurrLine->Next
  Loop

END SUB

SUB CompileFor (CompSub As SubType Pointer)

  Dim As String InLine, Origin, Temp, ErrorOrigin
  Dim As String LoopVar, StartValue, EndValue, StepValue, LoopVarType
  Dim As Integer FL, CD, StepIntVar, StepExists
  Dim As LinkedListElement Pointer CurrLine, FindLoop, LoopLoc
  Dim As Integer IgnoreWarning, IntegerEndVariableCreated, EndValueConstantInitialValue

  'force use at RC43 + RC44
  If HashMapGet(Constants, "USELEGACYFORNEXT" ) = 0 then
    ADDCONSTANT("NEWNEXTFORHANDLER","")
  end if

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0

    InLine = CurrLine->Value

    IF Left(InLine, 4) = "FOR " THEN

      FL = 0
      StepExists = 0
      IgnoreWarning = 0
      IntegerEndVariableCreated = 0
      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      Replace InLine, " TO ", Chr(30)

      IF INSTR(InLine, " STEP ") <> 0 THEN
        Replace InLine, " STEP ", Chr(31)
      END IF

      IF INSTR(InLine, "#IGNOREWARNING") <> 0 THEN
        Replace InLine, "#IGNOREWARNING", ""
        IgnoreWarning = -1
      END IF

      'enable new handler  NewNextForhandler force though new handler!
      If HashMapGet(Constants, "NEWNEXTFORHANDLER" ) then
        StepExists = -1
      End if

      DO WHILE INSTR(InLine, " ") <> 0: Replace InLine, " ", "": LOOP
      LoopVar = Mid(InLine, 4)
      LoopVar = Left(LoopVar, INSTR(LoopVar, "=") - 1)
      StartValue = Mid(InLine, INSTR(InLine, "=") + 1)
      StartValue = Left(StartValue, INSTR(StartValue, Chr(30)) - 1)
      EndValue = Mid(InLine, INSTR(InLine, Chr(30)) + 1)
      StepValue = "1"
      StepIntVar = 0
      IF INSTR(InLine, Chr(31)) <> 0 THEN
        EndValue = Left(EndValue, INSTR(EndValue, Chr(31)) - 1)
        StepValue = Mid(InLine, INSTR(InLine, Chr(31)) + 1)
        If Not IsConst(StepValue) And TypeOfVar(StepValue, CompSub) = "INTEGER" Then
          StepIntVar = -1
          'Print "Found Integer step variable:" + StepValue
        EndIf

        If HashMapGet(Constants, "NEWNEXTFORHANDLER" )  then
          'create an integer from the constant, only not ABS(1)
          If IsConst(StepValue) and ABS(Val(StepValue)) <> 1 then
             AddVar "SysForLoopStep" + Str(FLC), "INTEGER", 1, 0, "REAL", "", , -1

             CurrLine = LinkedListInsert(CurrLine,  "SysForLoopStep" + Str(FLC) + " = " + StepValue)
             CurrLine = CurrLine->Prev
             CurrLine = LinkedListInsert(CurrLine,  ";Init SysForLoopStep" + Str(FLC)+" :#0" )
             CurrLine = CurrLine->Prev
             'replace the constant with the integer string name
             StepValue = "SysForLoopStep" + Str(FLC)
             StepIntVar = -1
          End if
        End if

      END IF
'Print "For variable:"; LoopVar; " start:"; StartValue; " end:"; EndValue; " step:"; StepValue

      EndValueConstantInitialValue = val(EndValue)

      'Check that variable can hold start and end values
      LoopVarType = TypeOfValue(LoopVar, CompSub)
      If CastOrder(TypeOfValue(StartValue, CompSub)) = -1 Then
        LogError(Message("ForBadStart2"), Origin)
        Exit Sub
      End If
      If CastOrder(LoopVarType) < CastOrder(TypeOfValue(StartValue, CompSub))  Then
        LogError(Message("ForBadStart"), Origin)
      End If
      If CastOrder(LoopVarType) < CastOrder(TypeOfValue(EndValue, CompSub)) Then
        LogError(Message("ForBadEnd"), Origin)
      End If

      If HashMapGet(Constants, "NEWNEXTFORHANDLER" ) = 0 then
      'this is bonkers when you want to use the overflow!  255 to 10 for instant
          'Negate step value if necessary -
          IF VAL(EndValue) < VAL(StartValue) AND IsConst(EndValue) AND IsConst(StartValue) AND (InStr(StepValue, "-") = 0 And Not StepIntVar) THEN
            CurrLine = LinkedListInsert(CurrLine, ";For-next step value was automaticaly negated by compiler")
            CurrLine = CurrLine->Prev
            StepValue = "-" + StepValue
          End if
      End if

      'Use step value 1 as default when variables are used to force through new code - NOT NEEDED....
      IF ( ( NOT IsConst(EndValue) OR  NOT IsConst(StartValue) )  And Not StepIntVar) THEN
        If HashMapGet(Constants, "NEWNEXTFORHANDLER" ) then
           StepExists = -1
        End if
      End if

      If HashMapGet(Constants, "NEWNEXTFORHANDLER" ) THEN
        If Not IsConst(StepValue) And TypeOfVar(StepValue, CompSub) <> "INTEGER" THEN
      'Prevent using a negate with a variable... we cannot handle
            If Left(StepValue,1) <> "-" then
              LogError(Message("ForBadStepVariable"), Origin)
            Else
              LogError(Message("ForBadStepNegate"), Origin)
            End if
        End if
      End if


      FLC = FLC + 1
      FL = 1

      'And, finally determine if we can use the old method for specific use cases of constants and step=1, belt and braces check
      IF ( VAL(EndValue) > VAL(StartValue) ) AND IsConst(EndValue) AND IsConst(StartValue) AND (InStr(StepValue, "-") = 0 And Val(StepValue)=1 ) THEN
          StepExists  = 0
      End if

      ' A final, final check to try and force legacy....
      If StepExists = -1 Then
        If ABS(Val(Stepvalue)) = 1 Then 
          If  ( TypeOfValue(LoopVar, Subroutine(GetSubID(Origin)))= "BYTE" or _ 
                TypeOfValue(LoopVar, Subroutine(GetSubID(Origin)))= "WORD" or _ 
                TypeOfValue(LoopVar, Subroutine(GetSubID(Origin)))= "LONG" or _ 
                TypeOfValue(LoopVar, Subroutine(GetSubID(Origin)))= "INTEGER" ) Then
            StepExists = 0
          End If
        End If
      End If

      If StepExists Then

          'New Starting code. This is common code to the negative or positive StepValue value
          CurrLine->Value = LoopVar + " =" + StartValue +  Origin + "[ao]"  'need to add [ao] to ensure the loop value type is correct
          'Create a Loopx (see Pseudo code)
          CurrLine = LinkedListInsert(CurrLine, "SysForLoop" + Str(FLC) + LabelEnd)
          GetMetaData(Currline)->IsLabel = -1

      Else
          'Old code - still used for NO STEP
          'old Pseudo code:

          'V = SV - ST
          'SysForLoop(n):
          'V += ST
          '...
          'if V < EV then goto SysForLoop(n)

          'Changed to this: (27/6/2010)
          'V = SV
          'SysForLoop(n):
          'if V > EV then goto SysForLoopEnd(n)
          '...
          'V += ST
          'goto SysForLoop(n)
          '(New version more in line with For in other dialects)

          'Then to this: (27/8/2010)
          'V = SV - ST
          'If SV not const or EV not const, if V > EV then goto SysForLoopEnd(n)
          'SysForLoop(n):
          'V += ST
          '...
          'if V <= EV then goto SysForLoop(n)
          'SysForLoopEnd(n):


          'Starting code
          If PreserveMode <> 0 then
            CurrLine = LinkedListInsert(CurrLine->Prev, ";Legacy method")
            CurrLine = CurrLine->Next
          End If
          CurrLine->Value = LoopVar + "=" + StartValue + "-" + StepValue + Origin + "[ao]"

          'If StartValue or EndValue are variables, need to check values at runtime to see if loop should run
          If Not IsConst(EndValue) Or Not IsConst(StartValue) Then
            'If Step is an integer variable, need to check +/- first
            If StepIntVar Then
              CurrLine = LinkedListInsert(CurrLine, "IF " + StepValue + ".15 = 1 THEN" + Origin)
              CurrLine = LinkedListInsert(CurrLine, "IF " + StartValue + " < " + EndValue + " THEN" + Origin)
              IF ModePIC Then
                CurrLine = LinkedListInsert(CurrLine, " goto SysForLoopEnd" + Str(FLC))
              ElseIf ModeAVR Then
                CurrLine = LinkedListInsert(CurrLine, " rjmp SysForLoopEnd" + Str(FLC))
              End If
              CurrLine = LinkedListInsert(CurrLine, "END IF")
              CurrLine = LinkedListInsert(CurrLine, "ELSE" + Origin)
              CurrLine = LinkedListInsert(CurrLine, "IF " + StartValue + " > " + EndValue + " THEN" + Origin)
              IF ModePIC Then
                CurrLine = LinkedListInsert(CurrLine, " goto SysForLoopEnd" + Str(FLC))
              ElseIf ModeAVR Then
                CurrLine = LinkedListInsert(CurrLine, " rjmp SysForLoopEnd" + Str(FLC))
              End If
              CurrLine = LinkedListInsert(CurrLine, "END IF")
              CurrLine = LinkedListInsert(CurrLine, "END IF")
            'Step is a constant, check appropriately
            Else
              If INSTR(StepValue, "-") = 0 THEN
                CurrLine = LinkedListInsert(CurrLine, "IF " + StartValue + " > " + EndValue + " THEN" + Origin)
              Else
                CurrLine = LinkedListInsert(CurrLine, "IF " + StartValue + " < " + EndValue + " THEN" + Origin)
              End If
              IF ModePIC Then
                CurrLine = LinkedListInsert(CurrLine, " goto SysForLoopEnd" + Str(FLC))
              ElseIf ModeAVR Then
                CurrLine = LinkedListInsert(CurrLine, " rjmp SysForLoopEnd" + Str(FLC))
              End If
              CurrLine = LinkedListInsert(CurrLine, "END IF")
            End If

          End If
          CurrLine = LinkedListInsert(CurrLine, "SysForLoop" + Str(FLC) + LabelEnd)
          GetMetaData(Currline)->IsLabel = -1
          CurrLine = LinkedListInsert(CurrLine, LoopVar + "=" + LoopVar + "+" + StepValue + Origin)

      End if

      'End code
      'Find matching Next
      LoopLoc = 0
      FindLoop = CurrLine->Next
      Do While FindLoop <> 0
        InLine = UCase(FindLoop->Value)

        'Nested For, increment level counter, ensure different counter var used
        IF Left(InLine, 4) = "FOR " THEN
          FL += 1
          Temp = Mid(InLine, 5)
          Temp = Trim(Left(Temp, INSTR(Temp, "=") - 1))
          IF Temp = UCase(LoopVar) Then
            ErrorOrigin = ""
            IF INSTR(InLine, ";?F") <> 0 THEN ErrorOrigin = Mid(InLine, INSTR(InLine, ";?F"))
            Temp = Message("ForIndexInUse")
            Replace Temp, "%index%", LoopVar
            LogError Temp, ErrorOrigin
          END IF

        ElseIf Left(InLine, 8) = "EXIT FOR" AND FL = 1 Then
          IF ModePIC Then FindLoop->Value = " goto SysForLoopEnd" + Str(FLC)
          IF ModeAVR Then FindLoop->Value = " rjmp SysForLoopEnd" + Str(FLC)

        ElseIF InLine = "NEXT" Or Left(InLine, 5) = "NEXT " THEN
          FL -= 1
        END IF

        IF FL = 0 THEN LoopLoc = FindLoop: EXIT Do
        FindLoop = FindLoop->Next
      Loop

      IF LoopLoc = 0 THEN
        LogError Message("NoNext"), Origin

      Else
        LoopLoc = LinkedListDelete(LoopLoc)

        If StepExists Then

 'new NEXT code

          '#1 only add the new logic handle an integer... code only needed if an integer
          If TypeOfVar(StepValue, CompSub) = "INTEGER" then

            If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Integer negative Step Handler in For-next statement")
            LoopLoc = LinkedListInsert(LoopLoc, "IF " + StepValue + ".15 = 1 THEN" + Origin+" ;here")

                'Pseudo code - negative ops
                '                 if LoopVar - EndValue >= abs(StepValue) then
                '                   LoopVar = LoopVar + StepValue
                '                   goto loopx
                '                 end if

                If StepIntVar Then


                  'create an tmp integer long variable - stepvalue and handle LONGs.  This prevents the maths overrun
                  'Long psuedo code
                  'Dim tmpStepValue as Integer
                  'tmpStepValue = -StepValue
                  '
                  'if LoopVar - EndValue >= tmpStepValue then
                  ' LoopVar = LoopVar - tmpStepValue
                  '
                  ' goto negloop
                  'end if


                  If TypeOfVar(LoopVar, CompSub) = "LONG" or TypeOfVar(LoopVar, CompSub) = "WORD"  then
                    AddVar "SysForLoopABsValue" + Str(FLC), "INTEGER", 1, 0, "REAL", "", , -1
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set SysForLoopABsValue to -StepValue :#1 ")
                    LoopLoc = LinkedListInsert(LoopLoc,  "SysForLoopABsValue" + Str(FLC) + " = -([integer]" + StepValue+")")


                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";IF ( " + LoopVar + " - " + EndValue + ") } [WORD]" + "SysForLoopABsValue" + Str(FLC)  + " THEN  :#1n ")
                    LoopLoc = LinkedListInsert(LoopLoc,  "IF ( " + LoopVar + " - " + EndValue + ") } [WORD]" + "SysForLoopABsValue" + Str(FLC)  + " THEN" + Origin)

                  ElseIf TypeOfVar(LoopVar, CompSub) = "INTEGER"   then

                    'create a temp variable so that we can do the tests, keeping the original value as-is for maths
                    AddVar "SysForLoopABsValue" + Str(FLC), "INTEGER", 1, 0, "REAL", "", , -1
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set SysForLoopABsValue to -StepValue#2 ")
                    LoopLoc = LinkedListInsert(LoopLoc,  "SysForLoopABsValue" + Str(FLC) + " = -([integer]" + StepValue+")")

                    if IsConst(EndValue) then
                        'create a variable to enable casting - added to resolve negative constant issue
                        AddVar "SysForLoopEndValue" + Str(FLC), "INTEGER", 1, 0, "REAL", "", , -1
                        If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set SysForLoopEndValue to EndValue ")
                        LoopLoc = LinkedListInsert(LoopLoc,  "SysForLoopEndValue" + Str(FLC) + " = " + str(EndValueConstantInitialValue) )
                        EndValue = "SysForLoopEndValue" + Str(FLC)
                        IntegerEndVariableCreated = -1
                    End if


                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";IF ( [WORD]" + LoopVar + " - [WORD]" + EndValue + ") } [WORD]" + "SysForLoopABsValue" + Str(FLC)  + " THEN :#2n ")
                    LoopLoc = LinkedListInsert(LoopLoc,     "IF ( [WORD]" + LoopVar + " - [WORD]" + EndValue + ") } [WORD]" + "SysForLoopABsValue" + Str(FLC)  + " THEN" + Origin)

                  Else
                    'If statement for NOT longs
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";IF ( " + LoopVar + " - " + EndValue + ") } -" + StepValue + " THEN :#3n")
                    LoopLoc = LinkedListInsert(LoopLoc, "IF ( " + LoopVar + " - " + EndValue + ") } -" + StepValue + " THEN" + Origin)

                  End if


                else
                  'handle a constant
                  If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";IF " + EndValue + " - " + LoopVar + " ~ 0 THEN" + Origin + ":#4n")
                  LoopLoc = LinkedListInsert(LoopLoc, "IF " + EndValue + " - " + LoopVar + " ~ 0 THEN" + Origin)

                end if

                  If TypeOfVar(LoopVar, CompSub) = "LONG" then
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set SysForLoopABsValue to LoopVar - SysForLoopABsValue  ")
                    LoopLoc = LinkedListInsert(LoopLoc, "[LONG]" + LoopVar + " = [LONG]" + LoopVar + " - " + "SysForLoopABsValue" + Str(FLC) + Origin)
                  else
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set LoopVar to LoopVar + StepValue where StepValue is a negative value")
                    LoopLoc = LinkedListInsert(LoopLoc, LoopVar + " = " + LoopVar + " + " + StepValue + Origin + "[ao]")
                  end if

                  IF ModePIC Then
                    LoopLoc = LinkedListInsert(LoopLoc, " goto SysForLoop" + Str(FLC))
                  ElseIf ModeAVR Then
                    LoopLoc = LinkedListInsert(LoopLoc, " rjmp SysForLoop" + Str(FLC))
                  End If

                If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";END IF")
                LoopLoc = LinkedListInsert(LoopLoc, "END IF")

            LoopLoc = LinkedListInsert(LoopLoc, "ELSE" + Origin)

 'Positive handler starts here with the encoded "ELSE" above
          End if  '#1  do not handle an integer... code above is needed only when an integer

          'Pseudo code - positive or assumed constant 1
                '                  if EndValue-LoopVar >= StepValue then
                '                    LoopVar = LoopVar + StepValue
                '                    goto loopx
                '                  end if



                If StepIntVar Then
                  If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Integer positive Step Handler in For-next statement")

                  If TypeOfVar(LoopVar, CompSub) = "WORD"  then
                    'Need to set the type to ensure operation works for integer
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";IF ([WORD]" + EndValue + " - [WORD]" + LoopVar + ") } [WORD]" + StepValue + " THEN :#1p ")
                    LoopLoc = LinkedListInsert(LoopLoc,  "IF ([WORD]" + EndValue + " - [WORD]" + LoopVar + ") } [WORD]" + StepValue + " THEN" + Origin)

                 ElseIf TypeOfVar(LoopVar, CompSub) = "INTEGER" then

                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";INTEGER :#2pa")
                    'create a temp variable so that we can do the tests, keeping the original value as-is for maths
                    AddVar "SysForLoopABsValue" + Str(FLC), "INTEGER", 1, 0, "REAL", "", , -1
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set SysForLoopABsValue to StepValue ")
                    LoopLoc = LinkedListInsert(LoopLoc,  "SysForLoopABsValue" + Str(FLC) + " = " + StepValue )

                    if IsConst(EndValue) or IntegerEndVariableCreated = -1 then
                        'create a variable to enable casting - added to resolve negative constant issue
                        AddVar "SysForLoopEndValue" + Str(FLC), "INTEGER", 1, 0, "REAL", "", , -1
                        If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set SysForLoopEndValue to EndValue :#2pb")
                        LoopLoc = LinkedListInsert(LoopLoc,  "SysForLoopEndValue" + Str(FLC) + " = " + str(EndValueConstantInitialValue))
                        EndValue = "SysForLoopEndValue" + Str(FLC)
                    End if


                    'Need to set the type to ensure operation works for integer
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";IF [LONG]([INTEGER]" + EndValue + " - [INTEGER]" + LoopVar + ") } [LONG]" + "SysForLoopABsValue" + Str(FLC) + " THEN :#2pc")
                    LoopLoc = LinkedListInsert(LoopLoc,  "IF [LONG]([INTEGER]" + EndValue + " - [INTEGER]" + LoopVar + ") } [LONG]" + "SysForLoopABsValue" + Str(FLC) + " THEN" + Origin)


                  else
                    If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";IF (" + EndValue + " - " + LoopVar + ") } " + StepValue + " THEN :#3p")
                    LoopLoc = LinkedListInsert(LoopLoc, "IF (" + EndValue + " - " + LoopVar + ") } " + StepValue + " THEN" + Origin)
                  end if
                else

                  If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Positive value Step Handler in For-next statement :#4p ")
                  LoopLoc = LinkedListInsert(LoopLoc, "IF " + EndValue + " - " + LoopVar + " ~ 0 THEN" + Origin)
                end if

                  'Finally - add the STEPVALUE!!
                  If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";Set LoopVar to LoopVar + StepValue where StepValue is a positive value")
                  LoopLoc = LinkedListInsert(LoopLoc, LoopVar + " = " + LoopVar + " + " + StepValue + Origin+ "[ao]")
                  IF ModePIC Then
                    LoopLoc = LinkedListInsert(LoopLoc, " goto SysForLoop" + Str(FLC))
                  ElseIf ModeAVR Then
                    LoopLoc = LinkedListInsert(LoopLoc, " rjmp SysForLoop" + Str(FLC))
                  End If

                If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";END IF")
                LoopLoc = LinkedListInsert(LoopLoc, "END IF")

          '#2 do not handle an integer... if an integer - close out the if statement
          If TypeOfVar(StepValue, CompSub) = "INTEGER" then
            If PreserveMode <> 0 Then LoopLoc = LinkedListInsert(LoopLoc, ";END IF")
            LoopLoc = LinkedListInsert(LoopLoc, "END IF")
          End if

        Else

            'old method - still AVAILABLE !!!
            If StepIntVar Then
              LoopLoc = LinkedListInsert(LoopLoc, "IF " + StepValue + ".15 = 1 THEN" + Origin)
              LoopLoc = LinkedListInsert(LoopLoc, "IF " + LoopVar + " > " + EndValue + " THEN" + Origin)
              IF ModePIC Then
                LoopLoc = LinkedListInsert(LoopLoc, " goto SysForLoop" + Str(FLC))
              ElseIf ModeAVR Then
                LoopLoc = LinkedListInsert(LoopLoc, " rjmp SysForLoop" + Str(FLC))
              End If
              LoopLoc = LinkedListInsert(LoopLoc, "END IF")
              LoopLoc = LinkedListInsert(LoopLoc, "ELSE" + Origin)
              LoopLoc = LinkedListInsert(LoopLoc, "IF " + LoopVar + " < " + EndValue + " THEN" + Origin)
              IF ModePIC Then
                LoopLoc = LinkedListInsert(LoopLoc, " goto SysForLoop" + Str(FLC))
              ElseIf ModeAVR Then
                LoopLoc = LinkedListInsert(LoopLoc, " rjmp SysForLoop" + Str(FLC))
              End If
              LoopLoc = LinkedListInsert(LoopLoc, "END IF")
              LoopLoc = LinkedListInsert(LoopLoc, "END IF")
            Else
              If INSTR(StepValue, "-") = 0 THEN
                LoopLoc = LinkedListInsert(LoopLoc, "IF " + LoopVar + " < " + EndValue + " THEN" + Origin)
              Else
                LoopLoc = LinkedListInsert(LoopLoc, "IF " + LoopVar + " > " + EndValue + " THEN" + Origin)
              End If
              IF ModePIC Then
                LoopLoc = LinkedListInsert(LoopLoc, " goto SysForLoop" + Str(FLC))
              ElseIf ModeAVR Then
                LoopLoc = LinkedListInsert(LoopLoc, " rjmp SysForLoop" + Str(FLC))
              End If
              LoopLoc = LinkedListInsert(LoopLoc, "END IF" + Str(FLC) + LabelEnd)
              GetMetaData(LoopLoc)->IsLabel = -1
            End If
        End If
        LoopLoc = LinkedListInsert(LoopLoc, "SysForLoopEnd" + Str(FLC) + LabelEnd)
        GetMetaData(LoopLoc)->IsLabel = -1
      End IF
    END If

    CurrLine = CurrLine->Next
  Loop

  FoundCount = FLC
End SUB

Sub CompileGoto (CompSub As SubType Pointer)
  'Compile GOSUBs, GOTOs and labels
  Dim As String InLine, DestLabel, Origin, TempData
  'Dim As Integer PD
  Dim As LinkedListElement Pointer CurrLine, Labels
  Dim ReservedWordCounter as Integer


  FoundCount = 0

  'Find labels, alter on PICs
  If ModePIC Then
    CurrLine = CompSub->CodeStart->Next
    Do While CurrLine <> 0
      IF Right(CurrLine->Value, 1) = ":" THEN
        Replace CurrLine->Value, ":", ""
        GetMetaData(Currline)->IsLabel = -1
        FoundCount += 1
      End If
      CurrLine = CurrLine->Next
    Loop
  End If

  'Find and translate everything else
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = UCASE(CurrLine->Value)

    If Left(InLine, 5) = "GOTO " THEN
      IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
      Else
        Origin = ""
      End If
      DestLabel = Trim(MID(CurrLine->Value, 6))

      'Should check label!
      If Right(DestLabel, 1) = ":" Then DestLabel = Trim(Left(DestLabel, Len(DestLabel) - 1))

      'Check for Reservedword as label
      For ReservedWordCounter = 0 to RESERVED_WORDS
          If Ucase(Trim(DestLabel)) = Ucase(trim(ReservedWords(ReservedWordCounter)))  AND LaxSyntax = 0 THEN
              TempData = Message("CannotUseReservedWords")
              Replace TempData, "%label%", DestLabel
              LogError TempData, Origin
              exit for
          End if
      Next
      If ModePIC Then
        CurrLine->Value = " goto " + DestLabel
      ElseIf ModeAVR Then
        CurrLine->Value = " rjmp " + DestLabel
      ElseIf ModeZ8 Then
        CurrLine->Value = " jp " + DestLabel
      End If
      FoundCount += 1

    ElseIf Left(InLine, 6) = "GOSUB " THEN
      IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
      Else
        Origin = ""
      End If
      DestLabel = Trim(MID(CurrLine->Value, 7))

      CurrLine->Value = " call " + DestLabel
      FoundCount += 1
    END IF
    CurrLine = CurrLine->Next
  Loop

End Sub

SUB CompileIF (CompSub As SubType Pointer)
  FoundCount = 0
  Dim As String Origin, Temp, L1, L2, Condition, EndBlock, LastIfOrigin
  Dim As Integer FoundIF, IL, ELC, IT, TL, FE, DelSection, DelEndIf, EndIfUsed, PrevSectionSkipped
  Dim As Integer WD, CurrIfNo

  Dim As LinkedListElement Pointer CurrLine, NewCode, FindEnd, StartDel

  'Needs to loop through program several times, compile nested IFs one level at a time
  COMPILEIFS:

  FoundIF = 0
  IL = 0

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0

    'Signalling:
    ' - DelEndIf means that one condition is always true, so all elses after should be removed
    ' - DelSection means that one condition is always false, so everything before the next else should be removed
    '   - When DelSection is used, StartDel is the first line that should be deleted by the next section

    'IF
    IF UCase(Left(CurrLine->Value, 3)) = "IF " Then

      IL = IL + 1
      IF IL = 1 THEN
        ILC = ILC + 1
        CurrIfNo = ILC
        ELC = 0

        'Get origin
        Origin = ""
        IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
          Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
          CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
          LastIfOrigin = Origin
        END IF

        'Decide whether to jump to else or end if
        IT = 0
        TL = 1
        FindEnd = CurrLine->Next
        Do While FindEnd <> 0
          Temp = FindEnd->Value
          IF Left(Temp, 3) = "IF " Then TL += 1
          IF Left(Temp, 6) = "END IF" Then TL -= 1
          IF TL = 1 AND Left(Temp, 4) = "ELSE" Then IT = 1: Exit Do
          If TL = 0 Then Exit Do
          FindEnd = FindEnd->Next
        Loop
        'If no end found, error and quit
        If FindEnd = 0 Then
          LogError(Message("NoEndIf"), Origin)
          'Remove faulty If and continue compilation (to find further errors)
          CurrLine = LinkedListDelete(CurrLine)
          GoTo CompileIfs
        End If

        'Generate code to test and jump
        Condition = Mid(CurrLine->Value, 4)
        Condition = Trim(Left(Condition, INSTR(UCase(Condition), "THEN") - 1))
        NewCode = CompileConditions(Condition, "FALSE", Origin, CompSub)
        DelSection = 0
        DelEndIf = 0
        StartDel = 0
        EndIfUsed = 0
        If Condition = "AlwaysTrue" Then
          CurrLine = LinkedListDelete(CurrLine)
          DelEndIf = -1

        ElseIf Condition = "AlwaysFalse" Then
          DelSection = -1
          StartDel = CurrLine

        Else
          CurrLine = LinkedListDelete(CurrLine)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
          If IT = 0 Then EndBlock = "ENDIF" + Str(CurrIfNo): EndIfUsed = -1
          If IT = 1 Then EndBlock = "ELSE" + Str(CurrIfNo) + "_" + Str(ELC + 1)
          If ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto " + EndBlock)
          If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp " + EndBlock)
        End If

        FoundIF = -1
      END IF

    'Else If
    ElseIF Left(CurrLine->Value, 7) = "ELSE IF" And IL = 1 Then
      ELC += 1

      'Get origin
      Origin = ""
      IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
        Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
        CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
      END If

      'Decide whether to jump to else or end if
      IT = 0
      TL = 1
      FindEnd = CurrLine->Next
      Do While FindEnd <> 0
        Temp = FindEnd->Value
        IF Left(Temp, 3) = "IF " Then TL += 1
        IF Left(Temp, 6) = "END IF" Then TL -= 1
        IF TL = 1 AND Left(Temp, 4) = "ELSE" Then IT = 1: Exit Do
        If TL = 0 Then Exit Do
        FindEnd = FindEnd->Next
      Loop
      'If no end found, error and quit
      If FindEnd = 0 Then
        'Print "No End If"
        LogError(Message("NoEndIf"), Origin)
        'Remove faulty If and continue compilation (to find further errors)
        CurrLine = LinkedListDelete(CurrLine)
        GoTo CompileIfs
      End If

      'Add Code
      PrevSectionSkipped = DelSection
      If DelSection Then
        CurrLine = LinkedListDeleteList(StartDel, CurrLine->Prev)->Next
        DelSection = 0
      End If
      If DelEndIf Then
        If StartDel = 0 Then
          StartDel = CurrLine
        End If
      Else
        Condition = Mid(CurrLine->Value, 8)
        Condition = Left(Condition, INSTR(UCase(Condition), "THEN") - 1)
        If PrevSectionSkipped Then
          CurrLine = LinkedListDelete(CurrLine)
        Else
          EndIfUsed = -1
          If ModePIC Then
            L1 = " goto ENDIF" + Str(CurrIfNo)
            L2 = "ELSE" + Str(CurrIfNo) + "_" + Str(ELC)

          ElseIf ModeAVR Then
            L1 = " rjmp ENDIF" + Str(CurrIfNo)
            L2 = "ELSE" + Str(CurrIfNo) + "_" + Str(ELC) + ":"

          End If
          CurrLine->Value = L1
          CurrLine = LinkedListInsert(CurrLine, L2)
          GetMetaData(Currline)->IsLabel = -1
        End If

        'Generate code to test and jump
        NewCode = CompileConditions(Condition, "FALSE", Origin, CompSub)
        DelSection = 0
        DelEndIf = 0
        StartDel = 0
        If Condition = "AlwaysTrue" Then
          'CurrLine = LinkedListDelete(CurrLine)
          DelEndIf = -1

        ElseIf Condition = "AlwaysFalse" Then
          DelSection = -1
          StartDel = CurrLine->Next

        Else
          'CurrLine = LinkedListDelete(CurrLine)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
          If IT = 0 Then EndBlock = "ENDIF" + Str(CurrIfNo): EndIfUsed = -1
          If IT = 1 Then EndBlock = "ELSE" + Str(CurrIfNo) + "_" + Str(ELC + 1)
          If ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto " + EndBlock)
          If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp " + EndBlock)
        End If

      End If

    'Else
    ElseIF (CurrLine->Value = "ELSE" Or (Left(CurrLine->Value, 4) = "ELSE" And IsDivider(Mid(CurrLine->Value, 5, 1)))) And IL = 1 Then
      'Get origin
      Origin = ""
      IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
        Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
        CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
      END IF

      'Add Code
      PrevSectionSkipped = DelSection
      If DelSection Then
        CurrLine = LinkedListDeleteList(StartDel, CurrLine->Prev)->Next
        DelSection = 0
      End If
      If DelEndIf Then
        If StartDel = 0 Then
          StartDel = CurrLine
        End If

      Else
        If PrevSectionSkipped Then
          CurrLine = LinkedListDelete(CurrLine)

        Else
          EndIfUsed = -1
          If ModePIC Then
            L1 = " goto ENDIF" + Str(CurrIfNo)
            L2 = "ELSE" + Str(CurrIfNo) + "_" + Str(ELC + 1)

          ElseIf ModeAVR Then
            L1 = " rjmp ENDIF" + Str(CurrIfNo)
            L2 = "ELSE" + Str(CurrIfNo) + "_" + Str(ELC + 1) + ":"
          End If
          CurrLine->Value = L1
          CurrLine = LinkedListInsert(CurrLine, L2)
          GetMetaData(Currline)->IsLabel = -1
        End If

      End If

    'End If
    ElseIF UCase(Left(CurrLine->Value, 6)) = "END IF" THEN
      'Get origin
      Origin = ""
      IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
        Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
        CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
      END IF

      'Decrement level counter, add end if label if needed
      IL = IL - 1
      IF IL = 0 Then
        If (DelSection Or DelEndIf) And StartDel <> 0 Then
          LinkedListDeleteList(StartDel, CurrLine->Prev)
        End If

        If EndIfUsed Then
          CurrLine->Value = "ENDIF" + Str(CurrIfNo) + LabelEnd
          GetMetaData(CurrLine)->IsLabel = -1
        Else
          CurrLine = LinkedListDelete(CurrLine)
        End If


      'If there are too many END IFs, display error
      ElseIf IL < 0 THEN
        IF Origin <> "" THEN
          LogError Message("ExtraENDIF"), Origin
        END IF
        IL = 0

      END IF

    END IF

    CurrLine = CurrLine->Next
  Loop

  'Not enough end ifs?
  If IL > 0 Then
    LogError(Message("NoEndIf"), LastIfOrigin)
  End If

  'Need to scan through program over and over until no more new IFs are found
  IF FoundIF THEN GOTO COMPILEIFS

  FoundCount = ILC
END SUB

Sub CompileIntOnOff (CompSub As SubType Pointer)
  Dim As String LineTemp, TempData, Origin
  Dim As SysVarType Pointer GIEBit
  Dim NewCode(10) As String
  Dim As LinkedListElement Pointer CurrLine

  GIEBit = HashMapGet(SysVarBits, "GIE")

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    LineTemp = CurrLine->Value
    IF LEFT(LineTemp, 5) = "INTON" Or LEFT(LineTemp, 6) = "INTOFF" THEN
      Origin = ""
      IF INSTR(LineTemp, ";?F") <> 0 THEN
        Origin = Mid(LineTemp, INSTR(LineTemp, ";?F"))
        LineTemp = RTRIM(LEFT(LineTemp, INSTR(LineTemp, ";?F") - 1))
      END IF
      If LineTemp <> "INTON" And LineTemp <> "INTOFF" Then Continue Do

      If SysInt Or UserInt Then

        'Get status save variable to use (if not already found)
        If CompSub->IntStateSaveVar = "" Then
          CompSub->IntStateSaveVar = "SYSINTSTATESAVE" + Str(IntOnOffCount \ 8) + "." + Str(IntOnOffCount Mod 8)
          IntOnOffCount += 1

        End If

        CurrLine = LinkedListDelete(CurrLine)

        If ModePIC Then
          If GIEBit <> 0 Then
            If LineTemp = "INTON" Then
              'Restore state
              CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(CompSub->IntStateSaveVar, GIEBit->Parent + ".GIE", Origin))
            Else
              'Save state
              CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(GIEBit->Parent + ".GIE", CompSub->IntStateSaveVar, Origin))
              'Turn off interrupt
              CurrLine = LinkedListInsert(CurrLine, " bcf " + GIEBit->Parent + ",GIE")
            End If
          End If
        ElseIf ModeAVR Then
          If LineTemp = "INTON" Then
            'Restore state
            CurrLine = LinkedListInsertList(CurrLine, CompileConditions(CompSub->IntStateSaveVar + "=1", "TRUE", Origin))
            CurrLine = LinkedListInsert(CurrLine, " sei")
          Else
            'Save state
            CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SREG.I", CompSub->IntStateSaveVar, Origin))
            'Turn off interrupt
            CurrLine = LinkedListInsert(CurrLine, " cli")
          End If

        End If

      Else
        'If no user or system interrupt routine, delete IntOn and IntOff
        CurrLine = LinkedListDelete(CurrLine)

      End If
    End If

    CurrLine = CurrLine->Next
  Loop

End Sub

SUB CompileOn (CompSub As SubType Pointer)
  Dim As String LineTemp, Origin, TempData, EnableBit, FlagBit
  Dim As String OnType, OnCondition, OnJumpTo
  Dim As String IntSource, IntEvent
  Dim As Integer IntIndex, FindIndex, HandlerSubLoc
  Dim As LinkedListElement Pointer CurrLine, OutLine
  FoundCount = 0

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0

    LineTemp = CurrLine->Value
    IF Left(LineTemp, 3) = "ON " THEN

      Origin = ""
      IF INSTR(LineTemp, ";?F") <> 0 THEN
        Origin = Mid(LineTemp, INSTR(LineTemp, ";?F"))
        LineTemp = RTRIM(LEFT(LineTemp, INSTR(LineTemp, ";?F") - 1))
      END IF

      'Get condition, type
      OnType = Mid(LineTemp, INSTR(LineTemp, " ") + 1)
      OnCondition = Trim(Mid(OnType, INSTR(OnType, " ") + 1))
      OnType = Trim(Left(OnType, INSTR(OnType, " ") - 1))

      'Get target
      If Instr(OnCondition, " CALL ") <> 0 Then
        OnJumpTo = Trim(Mid(OnCondition, Instr(OnCondition, " CALL ") + 6))
        OnCondition = Trim(Left(OnCondition, Instr(OnCondition, " CALL ") - 1))

      ElseIf Instr(OnCondition, " GOSUB ") <> 0 Then
        OnJumpTo = Trim(Mid(OnCondition, Instr(OnCondition, " GOSUB ") + 7))
        OnCondition = Trim(Left(OnCondition, Instr(OnCondition, " GOSUB ") - 1))

      ElseIf Instr(OnCondition, " IGNORE") <> 0 Then
        OnJumpTo = "IGNORE"
        OnCondition = Trim(Left(OnCondition, Instr(OnCondition, " IGNORE") - 1))

      Else
        'Error, no target
        LogError Message("MissingTarget"), Origin
        CurrLine = LinkedListDelete(CurrLine)
        Continue Do
      End If

      'On Error ...
      If OnType = "ERROR" Then
        'Implement in future version

      'On Interrupt
      ElseIf OnType = "INTERRUPT" Then
        AllowInterrupt = -1
        SysInt = -1

        'Get index
        IntIndex = 0
        FOR FindIndex = 1 to IntCount
          If UCase(Interrupts(FindIndex).EventName) = OnCondition Then IntIndex = FindIndex: Exit For
        Next
        If IntIndex = 0 Then
          TempData = Message("BadIntEvent")
          Replace TempData, "%event%", OnCondition
          LogError TempData, Origin
          CurrLine = LinkedListDelete(CurrLine)
          Continue Do
        End If

        With Interrupts(IntIndex)
          'Check handler sub, request
          If OnJumpTo <> "IGNORE" Then
            HandlerSubLoc = RequestSub(0, OnJumpTo)
            If HandlerSubLoc = -1 Then
              'Error, handler sub not found
              TempData = Message("SubNotFound")
              Replace TempData, "%sub%", OnJumpTo
              LogError TempData, Origin

              CurrLine = LinkedListDelete(CurrLine)
              Continue Do
            Else
              'If trying to jump to a function, fix call
              If Subroutine(HandlerSubLoc)->IsFunction Then
                OnJumpTo = GetSubFullName(HandlerSubLoc)
              End If
            End If
          End If

          'Show error if conflicting handler found
          If .Handler <> "" And .Handler <> OnJumpTo And OnJumpTo <> "IGNORE" Then
            TempData = Message("HandlerConflict")
            Replace TempData, "%event%", OnCondition
            LogError TempData, Origin

            CurrLine = LinkedListDelete(CurrLine)
            Continue Do
          End If

          'On PIC, generate handler routine in IntCode()
          If ModePIC Then
            If OnJumpTo = "IGNORE" Then
              If .EnableBit <> "" Then
                CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 0"
              Else
                CurrLine = LinkedListDelete(CurrLine)
              End If
            Else
              If .EnableBit <> "" Then
                EnableBit = GetWholeSFR(.EnableBit)
                FlagBit = GetWholeSFR(.FlagBit)
                Replace EnableBit, ".", ","
                Replace FlagBit, ".", ","

                CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 1"
                If .Handler = "" Then
                  If .FlagBit = "" Then
                    CurrLine = LinkedListDelete(CurrLine)
                    TempData = Message("WarningNoIntFlag")
                    Replace TempData, "%event%", .EventName
                    LogWarning TempData, Origin
                  Else
                    OutLine = IntHandlerCode->CodeEnd
                    OutLine = LinkedListInsert(OutLine, " btfss " + EnableBit)
                    OutLine = LinkedListInsert(OutLine, " goto Not" + UCase(.FlagBit))
                    OutLine = LinkedListInsert(OutLine, " btfss " + FlagBit)
                    OutLine = LinkedListInsert(OutLine, " goto Not" + UCase(.FlagBit))
                    OutLine = LinkedListInsert(OutLine, " call " + OnJumpTo)
                    OutLine = LinkedListInsert(OutLine, " bcf " + FlagBit)
                    OutLine = LinkedListInsert(OutLine, " goto INTERRUPTDONE")
                    OutLine = LinkedListInsert(OutLine, "Not" + UCase(.FlagBit))
                    IntHandlerCode->CodeEnd = OutLine
                    GetMetaData(IntHandlerCode->CodeEnd)->IsLabel = -1
                  End If
                  .Handler = OnJumpTo
                End If
              Else
                CurrLine = LinkedListDelete(CurrLine)
                TempData = Message("WarningNoIntEnable")
                Replace TempData, "%event%", .EventName
                LogWarning TempData, Origin
              End If

            End If

          'On AVR, set Interrupt().Handler
          ElseIf ModeAVR Then
            If OnJumpTo = "IGNORE" Then
              'Prog(PD) = "SET " + GetWholeSFR(.EnableBit) + " 0"
              If .EnableBit <> "" Then
                CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 0"
                If .FlagBit = "" Then
                  CurrLine = LinkedListDelete(CurrLine)
                  TempData = Message("WarningNoIntFlag")
                  Replace TempData, "%event%", .EventName
                  LogWarning TempData, Origin
                End If
              Else
                CurrLine = LinkedListDelete(CurrLine)
              End If
            Else
              'Prog(PD) = "SET " + GetWholeSFR(.EnableBit) + " 1"
              If .EnableBit <> "" Then
                CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 1"
              Else
                CurrLine = LinkedListDelete(CurrLine)
                TempData = Message("WarningNoIntEnable")
                Replace TempData, "%event%", .EventName
                LogWarning TempData, Origin
              End If
              .Handler = OnJumpTo
            End If

          End If
        End With

      'Error, bad type
      Else
        TempData = Message("BadOnType")
        Replace TempData, "%found%", OnType
        LogError TempData, Origin
        CurrLine = LinkedListDelete(CurrLine)
      End If

      FoundCount = FoundCount + 1
    END If

    CurrLine = CurrLine->Next
  Loop

END SUB

SUB CompilePot (CompSub As SubType Pointer)
  FoundCount = 0

  Dim As String LineTemp, Port, OutVar, Origin
  Dim As LinkedListElement Pointer CurrLine

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0

    LineTemp = CurrLine->Value
    IF Left(LineTemp, 4) = "POT " THEN
      Port = Mid(LineTemp, 5)

      Origin = ""
      IF INSTR(Port, ";?F") <> 0 THEN
        Origin = Mid(Port, INSTR(Port, ";?F"))
        Port = RTrim(Left(Port, INSTR(Port, ";?F") - 1))
      END IF

      OutVar = LTRIM(Mid(Port, INSTR(Port, ",") + 1))
      Port = Trim(Left(Port, INSTR(Port, ",") - 1))
      POC = POC + 1

      CurrLine = LinkedListDelete(CurrLine)
      CurrLine = LinkedListInsert(CurrLine, "DIR " + Port + " OUT" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "SET " + Port + " 0" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "WAIT 5 MS" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "INTOFF")
      CurrLine = LinkedListInsert(CurrLine, "DIR " + Port + " IN" + Origin)
      CurrLine = LinkedListInsert(CurrLine, OutVar + "=0" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "DO WHILE " + Port + " = 0" + Origin)
      CurrLine = LinkedListInsert(CurrLine, OutVar + "=" + OutVar + "+1" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "IF " + OutVar + " = 0 THEN" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "GOTO ENDPOT" + Str(POC) + Origin)
      CurrLine = LinkedListInsert(CurrLine, "END IF" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "WAIT 10 10US" + Origin)
      CurrLine = LinkedListInsert(CurrLine, "LOOP " + Origin)
      CurrLine = LinkedListInsert(CurrLine, "ENDPOT" + Str(POC) + ":")
      CurrLine = LinkedListInsert(CurrLine, "INTON")

      FoundCount += 1

    END If

    CurrLine = CurrLine->Next
  Loop

END SUB

SUB CompileReadTable (CompSub As SubType Pointer)
  Dim As String InLine, Origin, TableName, TableLoc, OutVar, Temp
  Dim As Integer CD, TableID, TableBytes, OutBytes, CurrTableByte, ItemLoc, ItemVal
  Dim As Integer LargeTable
  Dim As LinkedListElement Pointer CurrLine
  Dim As PinDirType Pointer CurrPinDir
  FoundCount = 0

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = CurrLine->Value
    IF Left(InLine, 10) = "READTABLE " THEN

      'Get origin of line
        Origin = ""
        IF INSTR(InLine, ";?F") <> 0 THEN
          Origin = Mid(InLine, INSTR(InLine, ";?F"))
          InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
        END IF

         IF CountOccur ( Inline, ",") <> 2  THEN
            Temp = Message("InCorrectNumberofParameters")
            LogError Temp, Origin
            exit sub
         END IF

        'Get table name
        InLine = LTrim(Mid(InLine, 11))
        TableName = Trim(Left(InLine, INSTR(InLine, ",") - 1))
        InLine = Mid(InLine, INSTR(InLine, ",") + 1)

        'Get table location
        TableLoc = Trim(Left(InLine, INSTR(InLine, ",") - 1))
        InLine = Mid(InLine, INSTR(InLine, ",") + 1)

        'Get output variable
        OutVar = Trim(InLine)
        AddVar OutVar, "BYTE", 1, CompSub, "REAL", Origin

        'Check that table exists
        TableID = 0
        FOR CD = 1 to DataTables
		' Improved isolation for EEPROM datasets
          IF DataTable(CD).Name = UCase(Trim(TableName)) Then
            IF DataTable(CD).IsEEPromData = 0 Then
              TableID =CD
            Else
              'Must be EEPROM dataset
              TableID = 0                   
            End If
            Exit For
          End If
        NEXT

        'Table not found, show error
        IF TableID = 0 THEN
          IF DataTable(CD).IsEEPromData = 0 Then
            Temp = Message("TableNotFound")
            Replace Temp, "%Table%", TableName
            LogError Temp, Origin
            CurrLine = LinkedListDelete(CurrLine)
          Else
            Temp = Message("DataFound")
            Replace Temp, "%Table%", TableName
            LogError Temp, Origin
            CurrLine = LinkedListDelete(CurrLine)
          End If
          CurrLine = LinkedListInsert(CurrLine, origin)
        'Table found, continue compile
        Else
          'Get size of data in table
          LargeTable = 0
          If DataTable(TableID).Items > 255 Then
            LargeTable = -1
            'Upgrade type of table if necessary due to size
            If CastOrder(DataTable(TableID).Type) < CastOrder("WORD") Then
              DataTable(TableID).Type = "WORD"
            End If
          End If
          TableBytes = GetTypeSize(DataTable(TableID).Type)
          OutBytes = GetTypeSize(TypeOfVar(OutVar, CompSub))

          'Remove input line
          CurrLine = LinkedListDelete(CurrLine)

          'Write assembly
          'If reading from fixed location, remove call to table
          If IsConst(TableLoc) Then
            ItemLoc = MakeDec(TableLoc)
            With DataTable(TableID)
              If ItemLoc = 0 Then
                ItemVal = .Items
              ElseIf ItemLoc <= .Items Then
                ItemVal = .Item(ItemLoc)
              Else
                ItemVal = 0
              End If

              CurrLine = LinkedListInsert(CurrLine, OutVar + "=" + Str(ItemVal) + Origin)
            End With
          Else
            'Request table
            DataTable(TableID).Used = -1

            'Pseudo code:
            ' movf/movlw TableLoc
            ' call TableName
            ' movwf OutVar
            If ModePIC Then
              If DataTable(TableID).StoreLoc = 0 Then
                'Store in program memory
                For CurrTableByte = 1 To OutBytes
                  If CurrTableByte > TableBytes Then
                    CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(OutVar, CurrTableByte - 1))
                  Else
                    If LargeTable Then
                      AddVar "SysStringA", "WORD", 1, CompSub, "REAL", Origin, , -1
                      CurrLine = LinkedListInsert(CurrLine, "[WORD]SYSSTRINGA=" + TableLoc + Origin)
                    Else
                      AddVar "SysStringA", "BYTE", 1, CompSub, "REAL", Origin, , -1
                      CurrLine = LinkedListInsert(CurrLine, "[BYTE]SYSSTRINGA=" + TableLoc + Origin)
                    End If

                    CurrLine = LinkedListInsert(CurrLine, " call " + GetByte(TableName, CurrTableByte - 1))
                    CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(OutVar, CurrTableByte - 1))
                  End If
                Next

                'If writing to a port, record it
                CurrPinDir = GetPinDirection(OutVar)
                If CurrPinDir <> 0 Then
                  CurrPinDir->WrittenTo = -1
                End If

              ElseIf DataTable(TableID).StoreLoc = 1 Then
                'Store in EEPROM
  '                RequestSub(CompSub, "SysEPRead")
  '                CurrLine = LinkedListInsert(CurrLine, "EEAddress=" + TableLoc + "+@Table" + TableName)
  '                CurrLine = LinkedListInsert(CurrLine, " call SysEPRead")
  '                CurrLine = LinkedListInsert(CurrLine, OutVar + "=EEDataValue")
                'Change SysEPRead to read the CONSTANT as this may have been redefined to an alternative method() ie K42, Q43 to NVMADR_EPREAD

                RequestSub(CompSub, ReplaceConstantsLine ( "SysEPRead", 0 ))
				'This ensures chip that require NVMADRH:NVMADRL use the WORD address
                If HasSFR("NVMADRL") Then
                  CurrLine = LinkedListInsert(CurrLine, "[WORD]SysEEAddress=" + TableLoc + "+@TABLE" + TableName)
                Else
                  CurrLine = LinkedListInsert(CurrLine, "EEAddress=" + TableLoc + "+@TABLE" + TableName)
                End if
                CurrLine = LinkedListInsert(CurrLine, " call " + ReplaceConstantsLine ( "SysEPRead", 0 ))
                CurrLine = LinkedListInsert(CurrLine, OutVar + "=EEDataValue")
              End If

          ElseIf ModeAVR Then
              If DataTable(TableID).StoreLoc = 0 Then
                'Store in program memory
                For CurrTableByte = 1 To OutBytes
                  If CurrTableByte > TableBytes Then
                    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("0", "[BYTE]" + GetByte(OutVar, CurrTableByte - 1), Origin))
                  Else

                    If Instr( OutVar, "SysPointerX" ) > 0 Then
                      AddVar("sysbytetempa", "BYTE", 1, CompSub, "REAL", Origin, , -1)
                      AddVar("sysbytetempb", "BYTE", 1, CompSub, "REAL", Origin, , -1)
                      CurrLine = LinkedListInsert(CurrLine, ";Cache X")
                      CurrLine = LinkedListInsert(CurrLine, "[BYTE]sysbytetempa=SYSSTRINGA")
                      CurrLine = LinkedListInsert(CurrLine, "[BYTE]sysbytetempb=SYSSTRINGA_H")
                      CurrLine = LinkedListInsert(CurrLine, ";End of Cache X")
                    End If

                    AddVar("SysByteTempX", "BYTE", 1, CompSub, "REAL", Origin, , -1)

                    If LargeTable Then
                      AddVar "SysStringA", "WORD", 1, CompSub, "REAL", Origin, , -1
                      CurrLine = LinkedListInsert(CurrLine, "[WORD]SYSSTRINGA=" + TableLoc + Origin)
                    Else
                      AddVar "SysStringA", "BYTE", 1, CompSub, "REAL", Origin, , -1
                      CurrLine = LinkedListInsert(CurrLine, "[BYTE]SYSSTRINGA=" + TableLoc + Origin)
                    End If

                    CurrLine = LinkedListInsert(CurrLine, " call " + GetByte(TableName, CurrTableByte - 1))

                    If Instr( OutVar, "SysPointerX" ) > 0 Then
                      CurrLine = LinkedListInsert(CurrLine, ";Restore X")
                      CurrLine = LinkedListInsert(CurrLine, "[BYTE]SYSSTRINGA=sysbytetempa")
                      CurrLine = LinkedListInsert(CurrLine, "[BYTE]SYSSTRINGA_H=sysbytetempb")
                      CurrLine = LinkedListInsert(CurrLine, ";End of Restore X")
                    End if

                    CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysByteTempX", "[BYTE]" + GetByte(OutVar, CurrTableByte - 1), Origin))

                  End If
                Next

              ElseIf DataTable(TableID).StoreLoc = 1 Then
                'Store in EEPROM
                RequestSub(CompSub, "SysEPRead")
                CurrLine = LinkedListInsert(CurrLine, "EEAddress=" + TableLoc + "+@TABLE" + TableName)
                CurrLine = LinkedListInsert(CurrLine, " call SysEPRead")
                CurrLine = LinkedListInsert(CurrLine, OutVar + "=EEDataValue")
              End If

            End If
          End If
        END IF

    End IF

    CurrLine = CurrLine->Next
  Loop
END SUB

SUB CompileRepeat (CompSub As SubType Pointer)

  Dim As String InLine, Origin, Temp, RepCount, NewOrigin, RepValType
  Dim As Integer RVN, RL, EV, FE, FS, RepNone, CheckZero, CurrByte
  Dim As LinkedListElement Pointer CurrLine, EndLoc, FindEnd
  Dim As Longint RepCountVal

  'Was RVN = 0
  'Quick and dirty fix to stop variables getting reused badly
  'set initial var number to number of repeat loops
  RVN = RPLC
  FoundCount = 0

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = CurrLine->Value
    RL = 0

    IF Left(InLine, 7) = "REPEAT " THEN

      'Get origin
      Origin = ""
      NewOrigin = ";?F0L0"
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
        NewOrigin = Left(Origin, InStr(Origin, "S") - 1)
      END IF
      For FS = 0 To SBC
        If Subroutine(FS) = CompSub Then
          NewOrigin += "S" + Str(FS) + "?"
          Exit For
        End If
      Next

      'Get parameter
      RepCount = Trim(Mid(InLine, 7))
      If RepCount = "" Then
        LogError Message("RepeatMissingCount"), Origin
      End If

      RepValType = TypeOfValue(RepCount, CompSub)
      'If told to repeat once, counter should be byte rather than bit
      If RepValType = "BIT" Then RepValType = "BYTE"
      
      If RepValType = "SINGLE" or RepValType = "DOUBLE" Then
        LogError Message("CannotUseFloatforRepeatOperation"), Origin
      End If

      RepNone = 0
      CheckZero = -1

      'Is count zero?
      If IsConst(RepCount) Then
        CheckZero = 0
        If MakeDec(RepCount) = 0 Then
          RepNone = -1
        End If
      End If

      RL = 1
      If RepNone = 0 Then
        RPLC += 1
        RVN += 1
        AddVar "SysRepeatTemp" + Str(RVN), RepValType, 1, CompSub, "REAL", Origin, , -1
      End If

      'Pseudo code:
      'V = RV
      'SysRepeatLoop(n):
      '...
      'decfsz RV, F
      'goto SysRepeatLoop(n)

      'Starting code
      If RepNone = 0 Then
        CurrLine = LinkedListDelete(CurrLine)

        'Need to increment all outer loop counts by 1 to ensure that they repeat the correct number of times
        'If inner loop is 0, it will loop 256 times
        'Need to decrement next outer loop in this situation

        If IsConst(RepCount) And InStr(RepCount, "@") = 0 Then
          'Increment counters as needed
          RepCountVal = MakeDec(RepCount)
          For CurrByte = 1 To GetTypeSize(RepValType) - 1
            RepCountVal += (2 ^ (8 * CurrByte))
          Next
          'Decrement again to take into account 0 = 256
          For CurrByte = 0 To GetTypeSize(RepValType) - 1
            If (RepCountVal And 255 * 2 ^ (8 * CurrByte)) = 0 Then
              RepCountVal -= (2 ^ (8 * (CurrByte + 1)))
            End If
          Next
          RepCount = Str(RepCountVal)
          CurrLine = LinkedListInsert(CurrLine, "[" + RepValType + "]SysRepeatTemp" + Str(RVN) + "=" + RepCount + NewOrigin)
        Else
          CurrLine = LinkedListInsert(CurrLine, "[" + RepValType + "]SysRepeatTemp" + Str(RVN) + "=" + RepCount + NewOrigin)

          'Check for zero, don't enter loop if 0 found
          If CheckZero Then
            If ModePIC And RepValType = "BYTE" And ChipFamily <> 16 Then
              'Can't use this on 18F, movff is used for copy, which doesn't set Z
              CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
            Else
              If RepValType <> "BYTE" Then
                CurrLine = LinkedListInsertList(CurrLine, CompileConditions("[" + RepValType + "]SysRepeatTemp" + Str(RVN) + "<1", "TRUE", NewOrigin))
              Else
                CurrLine = LinkedListInsertList(CurrLine, CompileConditions("[byte]SysRepeatTemp" + Str(RVN) + "=0", "TRUE", NewOrigin))
              End If
            End If
            If ModePIC Then
              CurrLine = LinkedListInsert(CurrLine, " goto SysRepeatLoopEnd" + Str(RPLC))
            ElseIf ModeAVR Then
              CurrLine = LinkedListInsert(CurrLine, " jmp SysRepeatLoopEnd" + Str(RPLC))
            End If
          End If

          'If byte is not zero, increment next byte
          For CurrByte = 0 To GetTypeSize(RepValType) - 2
            'Can use IF and assignments here, Repeat compiled before If
            CurrLine = LinkedListInsert(CurrLine, "IF [byte]" + GetByte("SysRepeatTemp" + Str(RVN), CurrByte) + " ~ 0 THEN" + NewOrigin)
            CurrLine = LinkedListInsert(CurrLine, GetByte("SysRepeatTemp" + Str(RVN), CurrByte + 1) + "=" + GetByte("SysRepeatTemp" + Str(RVN), CurrByte + 1) + "+1" + NewOrigin)
            CurrLine = LinkedListInsert(CurrLine, "END IF" + NewOrigin)
          Next
        End If

        CurrLine = LinkedListInsert(CurrLine, "SysRepeatLoop" + Str(RPLC) + LabelEnd)
        GetMetaData(Currline)->IsLabel = -1
      End If

      'Find End
      EndLoc = 0
      FindEnd = CurrLine->Next
      Do While FindEnd <> 0
        IF Left(FindEnd->Value, 7) = "REPEAT " Then
          RL += 1
        ElseIf Left(FindEnd->Value, 10) = "END REPEAT" Then
          RL -= 1
        ElseIf Left(FindEnd->Value, 11) = "EXIT REPEAT" AND RL = 1 Then
          IF ModePIC Then FindEnd->Value = " goto SysRepeatLoopEnd" + Str(RPLC)
          IF ModeAVR Then FindEnd->Value = " rjmp SysRepeatLoopEnd" + Str(RPLC)
        End If

        IF RL = 0 THEN EndLoc = FindEnd: Exit Do
        FindEnd = FindEnd->Next
      Loop
      'Match not found error
      IF EndLoc = 0 THEN
        LogError Message("NoEndRepeat"), Origin
        Goto NextRepeat
      END IF

      'Add end code
      If RepNone Then
        'If no repeating, delete entire loop
        CurrLine = LinkedListDeleteList(CurrLine, EndLoc)

      Else
        EndLoc = LinkedListDelete(EndLoc)
        If ModePIC Then
          For CurrByte = 0 To GetTypeSize(RepValType) - 1
            EndLoc = LinkedListInsert(EndLoc, " decfsz " + GetByte("SysRepeatTemp" + Str(RVN), CurrByte) + ",F")
            EndLoc = LinkedListInsert(EndLoc, " goto SysRepeatLoop" + Str(RPLC))
          Next

        ElseIf ModeAVR Then
          Dim As String RegName
          Dim As LinkedListElement Pointer CopyToReg
          CopyToReg = LinkedListCreate
          RegName = PutInRegister(CopyToReg, "[" + RepValType + "]SysRepeatTemp" + Str(RVN), RepValType, Origin)
          EndLoc = LinkedListInsertList(EndLoc, CopyToReg)

          For CurrByte = 0 To GetTypeSize(RepValType) - 1
            EndLoc = LinkedListInsert(EndLoc, " dec " + GetByte(RegName, CurrByte))
            EndLoc = LinkedListInsertList(EndLoc, CompileVarSet("[byte]" + GetByte(RegName, CurrByte), "[byte]" + GetByte("SysRepeatTemp" + Str(RVN), CurrByte), Origin))
            EndLoc = LinkedListInsert(EndLoc, " breq PC + 2")
            EndLoc = LinkedListInsert(EndLoc, " rjmp SysRepeatLoop" + Str(RPLC))
          Next
          FreeCalcVar RegName
        End If
        EndLoc = LinkedListInsert(EndLoc, "SysRepeatLoopEnd" + Str(RPLC) + LabelEnd)
        GetMetaData(EndLoc)->IsLabel = -1
      End If

    End IF

    IF ModePIC And (INSTR(InLine, " decfsz SysRepeatTemp") <> 0 AND INSTR(InLine, "_H") = 0 AND INSTR(InLine, "_U") = 0 AND INSTR(InLine, "_E") = 0) Then RVN = RVN - 1
    If ModeAVR And (INSTR(InLine, " sts SysRepeatTemp") <> 0 AND INSTR(InLine, "_H") = 0 AND INSTR(InLine, "_U") = 0 AND INSTR(InLine, "_E") = 0) Then RVN = RVN - 1

    NextRepeat:
    CurrLine = CurrLine->Next
  Loop
  FoundCount = RPLC
END SUB

Sub CompileReturn (CompSub As SubType Pointer)
  Dim As String InLine, Origin
  Dim As LinkedListElement Pointer CurrLine

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = UCASE(CurrLine->Value)

    'Added Instr(InLine," ") = 7  to permitted variables that start with RETURN
    'This bug was introduced at 98.03, fixes in 98.07
    'A dormat bug for a while
    If Left(InLine, 6) = "RETURN" AND Instr(Trim(InLine)," ") = 7  THEN

      'Remove origin
      IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
      Else
        Origin = ""
      End If

      'If in a function, check for value after return
      Dim Value As String
      Value = Trim(Mid(CurrLine->Value, 7))

      If Value <> "" Then
        If CompSub->IsFunction Then
          LinkedListInsert(CurrLine->Prev, CompSub->Name + "=" + Value + Origin)
        Else
          LogWarning(Message("WarningReturnValueInSub"), Origin)
        End If
      End If

      If ModePIC Then
        CurrLine->Value = " return"
      ElseIf ModeAVR Then
        CurrLine->Value = " ret"
      End If

      'If this is the last line, no need to add return later
      If CurrLine->Next = 0 Then
        CompSub->NoReturn = -1
      End If

    END IF
    CurrLine = CurrLine->Next
  Loop

End Sub

SUB CompileRotate (CompSub As SubType Pointer)

  FoundCount = 0
  Dim As String InLine, Origin, Temp, VarName, VarType, Direction, RotateReg
  Dim As String CalcVar, AsmDir, AsmMode
  Dim As Integer UseC, OldType, FirstShift, LastShift, ShiftDir, CurrByte
  Dim As LinkedListElement Pointer CurrLine

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = UCase(CurrLine->Value)
    IF Left(InLine, 7) = "ROTATE " Then

      'Delete BASIC command, makes it a bit neater below
      CurrLine = LinkedListDelete(CurrLine)

      'Get origin
      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      'Get var, var type, direction, mode
      VarName = Mid(InLine, INSTR(InLine, " ") + 1)
      Direction = Mid(VarName, INSTR(VarName, " ") + 1)
      VarName = Left(VarName, INSTR(VarName, " ") - 1)
      VarType = TypeOfVar(VarName, CompSub)
      CalcVar = "Sys" + VarType + "TempA"
      IF Direction = "LC" Then Direction = "LEFT"
      IF Direction = "RC" Then Direction = "RIGHT"
      IF Direction = "L" Then Direction = "LEFT SIMPLE"
      IF Direction = "R" Then Direction = "RIGHT SIMPLE"

      'Remove cast from var name
      If InStr(VarName, "]") <> 0 Then
        VarName = LTrim(Mid(VarName, InStr(VarName, "]") + 1))
        'Detect error if cast to larger type, this will result in invalid assembly
        If GetTypeSize(TypeOfVar(VarName, CompSub)) < GetTypeSize(VarType) Then
          Temp = Message("BadCast")
          Replace Temp, "%from%", TypeOfVar(VarName, CompSub)
          Replace Temp, "%to%", VarType
          LogError Temp, Origin
          Goto CompileNextRotate
        End If
      End If

      'Check var type
      If Not IsIntType(VarType) Then 'Error
        Temp = Message("BadCommandType")
        Replace Temp, "%command%", "Rotate"
        Replace Temp, "%type%", VarType
        LogError Temp, Origin
        Goto CompileNextRotate
      End If

      'Use carry flag?
      UseC = -1
      IF INSTR(Direction, "SIMPLE") <> 0 Then
        UseC = 0
        Replace Direction, "SIMPLE", ""
        Direction = TRIM(Direction)
      End If

      'Decide how to shift
      If Direction = "LEFT" Then
        FirstShift = 0
        LastShift = GetTypeSize(VarType) - 1
        ShiftDir = 1
        AsmDir = "l"
      Else
        FirstShift = GetTypeSize(VarType) - 1
        LastShift = 0
        ShiftDir = -1
        AsmDir = "r"
      End If

      If ChipFamily = 16 Then
        AsmMode = "cf"
      Else
        AsmMode = "f"
      End If

      'Add variable
      AddVar VarName, "BYTE", 1, CompSub, "REAL", Origin

      'Pre-set C bit?
      If (Not UseC) And Not (ChipFamily = 16 And VarType = "BYTE") Then
        RotateReg = GetByte(VarName, LastShift)
        If ModeAVR And Not IsRegister(RotateReg) Then
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(RotateReg, "SysValueCopy", Origin))
          RotateReg = "SysValueCopy"
        End If
        If ModePIC Then
          CurrLine = LinkedListInsert(CurrLine, " r" + AsmDir + AsmMode + " " + RotateReg + ",W")
        ElseIf ModeAVR Then
          CurrLine = LinkedListInsert(CurrLine, " ro" + AsmDir + " " + RotateReg)
        End If
      End If

      'Do shift
      For CurrByte = FirstShift To LastShift Step ShiftDir
        RotateReg = GetByte(VarName, CurrByte)
        If ModeAVR And Not IsRegister(RotateReg) Then
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(RotateReg, "SysValueCopy", Origin))
          RotateReg = "SysValueCopy"
        End If

        'Do without carry? (PIC18F and byte only)
        If ChipFamily = 16 And VarType = "BYTE" And Not UseC Then
          CurrLine = LinkedListInsert(CurrLine, " r" + AsmDir + "ncf " + RotateReg + ",F")

        'Do with carry
        Else
          If ModePIC Then
            CurrLine = LinkedListInsert(CurrLine, " r" + AsmDir + AsmMode + " " + RotateReg + ",F")
          ElseIf ModeAVR Then
            CurrLine = LinkedListInsert(CurrLine, " ro" + AsmDir + " " + RotateReg)
            If RotateReg <> GetByte(VarName, CurrByte) Then
              CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(RotateReg, "[byte]" + GetByte(VarName, CurrByte), Origin))
            End If
          End If
        End If

      Next
      FoundCount = FoundCount + 1
    END IF

    CompileNextRotate:
    CurrLine = CurrLine->Next
  Loop

END SUB

Function CompileString (InLine As String, Origin As String) As LinkedListElement Pointer
  Dim As String DestVar, Temp, ArrayName, StringToCopy
  Dim As String Source, SourceType, SourceTable, DestArrayName
  Dim As Integer ArrayType, CD, TC, BL, SS, SArrayType, ST, CurrSubID
  Dim As VariableType Pointer SourcePtr

  Dim StringTemp(100) As String

  Dim As LinkedListElement Pointer OutList, CurrLine
  Dim As SubType Pointer CurrSub

  OutList = LinkedListCreate
  CurrLine = OutList

  'Find subroutine that contains array
  CurrSubID = GetSubID(Origin)
  CurrSub = Subroutine(CurrSubID)

  'Disable interrupt
  'CurrLine = LinkedListInsert(CurrLine, "INTOFF")

  'Get destination array
  DestVar = Trim(Left(InLine, INSTR(InLine, "=") - 1))
  ArrayName = DestVar
  If INSTR(DestVar, "(") <> 0 Then ArrayName = RTrim(Left(DestVar, INSTR(DestVar, "(") - 1))
  If INSTR(DestVar, "$") <> 0 Then ArrayName = RTrim(Left(DestVar, INSTR(DestVar, "$") - 1))
  DestArrayName = ArrayName

  'Add code to set destination array RAM pointer
  CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, 1, CurrSub, Origin))

  'Get string to copy
  StringToCopy = Trim(Mid(InLine, INSTR(InLine, "=") + 1))
  TC = 0
  BL = 0
  Temp = ""
  For SS = 1 TO Len(StringToCopy)
    Select Case Mid(StringToCopy, SS, 1)
    Case "(":
      BL += 1
    Case ")":
      BL -= 1
    Case "+":
      If BL = 0 Then
        TC += 1: StringTemp(TC) = Trim(Temp)
        Temp = ""
      Else
        Temp = Temp + "+"
      End If
    Case Else:
      Temp = Temp + Mid(StringToCopy, SS, 1)
    End Select
  Next
  TC += 1: StringTemp(TC) = Trim(Temp)

  'Add code to clear length counter
  If TC > 1 Then
    If ModePIC Then
      CurrLine = LinkedListInsert(CurrLine, " clrf SysStringLength")
    ElseIf ModeAVR Then
      CurrLine = LinkedListInsert(CurrLine, " clr SysStringLength")
      CurrLine = LinkedListInsert(CurrLine, " ld SysValueCopy, Y+") 'Need to pre-increment Y pointer
    End If
    AddVar "SysStringLength", "BYTE", 1, 0, "REAL", Origin, , -1
  End If

  'Set string
  FOR CD = 1 to TC
    Source = StringTemp(CD)

    'Get source type
    SourceType = TypeOfValue(Source, Subroutine(GetSubID(Origin)))
    If IsConst(Source) Then SourceType = "CONST"
    IF INSTR(Source, ";STRING") <> 0 THEN SourceType = "SCONST"
    'Color 14: Print SourceType: Color 7
    Select Case SourceType

    'String
    Case "STRING":
      CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(Source, 0, CurrSub, Origin))
      RequestSub(CurrSub, "SysCopyString")
      IF TC = 1 Then CurrLine = LinkedListInsert(CurrLine, " call SYSCOPYSTRING")   'was Camelcase
      IF TC > 1 Then CurrLine = LinkedListInsert(CurrLine, " call SYSCOPYSTRINGPART")   'was Camelcase

    'String const
    Case "SCONST":
      'Add code to set ROM pointer
      ST = VAL(Mid(Source, 8))
      SourceTable = "StringTable" + Str(ST)
      StringStore(ST).Used = -1
      If ModePIC Then
        If ChipFamily <> 16 Then
          AddVar "SysStringA", "WORD", 1, 0, "REAL", Origin, , -1
          CurrLine = LinkedListInsert(CurrLine, " movlw low " + SourceTable)
          CurrLine = LinkedListInsert(CurrLine, " movwf SysStringA")
          If ChipFamily = 15 Then
            CurrLine = LinkedListInsert(CurrLine, " movlw (high " + SourceTable + ") & 127")
          Else
            CurrLine = LinkedListInsert(CurrLine, " movlw high " + SourceTable)
          End If
          CurrLine = LinkedListInsert(CurrLine, " movwf SysStringA_H")
        Else
          CurrLine = LinkedListInsert(CurrLine, " movlw low " + SourceTable)
          CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRL")
          CurrLine = LinkedListInsert(CurrLine, " movlw high " + SourceTable)
          CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRH")
          If HighTBLPTRBytes > 2 Then
            CurrLine = LinkedListInsert(CurrLine, " movlw upper " + SourceTable)
            CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRU")
          End If
        End If
      ElseIf ModeAVR Then
        AddVar "SysReadA", "WORD", 1, 0, "REAL", Origin, , -1
        CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA,low(" + SourceTable + "<<1)")
        CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA_H,high(" + SourceTable + "<<1)")
      End If
      RequestSub(CurrSub, "SysReadString")
      If TC = 1 Then CurrLine = LinkedListInsert(CurrLine, " call SYSREADSTRING")   'was camelcase
      If TC > 1 Then CurrLine = LinkedListInsert(CurrLine, " call SYSREADSTRINGPART") 'was camelcase

    ' single > string   floats2024
    Case "SINGLE":
          LogError "Cannot assign Single to String, use SingleToString() method", Origin

        ' Following code works... just there is no SYSSINGLETOSTRING functionality in SYSTEM.H
        'Dim As String DType, SType
        'DType = "STRING"
        'SType = SourceType

        'AddVar "Sys" + SType + "Temp", SType, 1, 0, "REAL", "", , -1
        'AddVar "Sys" + DType + "Temp", DType, 1, 0, "REAL", "", , -1
        'RequestSub(CurrSub, "Sys" + SType + "To" + DType, "")
        'CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(Source, "Sys" + SType + "Temp", Origin))
        'CurrLine = LinkedListInsert(CurrLine, " call Sys" + SType + "To" + DType)
        'CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("Sys" + DType + "Temp", Source, Origin))

    'Anything else, show error
    Case Else:
      Temp = Message("CannotConcat")
      Replace Temp, "%value%", Source
      Replace Temp, "%type%", NCase(SourceType)
      LogError Temp, Origin

    End Select

  Next

  'Set length
  IF TC > 1 Then

    CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestArrayName, -1, CurrSub, Origin))
    If ModePIC Then
      If ChipFamily = 16 Then
        CurrLine = LinkedListInsert(CurrLine, " movff SysStringLength, INDF0")
      ElseIf ChipFamily = 15 Then
        CurrLine = LinkedListInsert(CurrLine, " movf SysStringLength,W")
        CurrLine = LinkedListInsert(CurrLine, " movwf INDF0")
      Else
        CurrLine = LinkedListInsert(CurrLine, " movf SysStringLength,W")
        CurrLine = LinkedListInsert(CurrLine, " movwf INDF")
      End If
    ElseIf ModeAVR Then
      CurrLine = LinkedListInsert(CurrLine, " st X, SysStringLength")
    End If

  End If

  'Enable interrupt
  'CurrLine = LinkedListInsert(CurrLine, "INTON")

  Return OutList
End Function

SUB CompileSelect (CompSub As SubType Pointer)
  FoundCount = 0
  Dim As String InLine, Origin, Temp, SelectValue, Condition, NextCaseLabel
  Dim As String ElseCaseLabel, RepValType, SCastType
  Dim As Integer PD, CC, AL, SCL, MinValue, MaxValue, AllNumeric, UseJumpTable
  Dim As Integer NumVal, HasElse, ElseCaseNo
  Dim As LinkedListElement Pointer CurrLine, NewCode, FindCase
  Dim As LinkedListElement Pointer CaseStatements, CurrCase, TablePos, SelectPos

  SCL = 0
  CaseStatements = 0

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = CurrLine->Value
    IF Left(InLine, 12) = "SELECT CASE " THEN

      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      FoundCount += 1

      CaseStatements = LinkedListCreate
      CurrCase = CaseStatements

      SelectValue = Trim(Mid(InLine, 12))
      If LEN(TRIM(SelectValue)) = 0 then
          LogError Message("NoSelectVariableParameter"), Origin
      End if
        
      ' Ensure SelectValue is NOT decimal
      SCastType = Mid(InLine, InStr(InLine, "[") + 1)
      SCastType = UCase(Left(SCastType, InStr(SCastType, "]") - 1))
      RepValType = TypeOfValue(SelectValue, CompSub)

      If IsConst(SelectValue) Then
          LogWarning Message("ShouldNotUseConstant"), Origin
      End If

      If CountOccur( inLine, "[") > 1 Then
        'Error handler for use of Singles.
        Dim temp As String
        temp = Message("CannotUseFloatforSelectOperation")
        Replace ( temp, "%varerror%", InLine )
        LogError temp, Origin
      End If


      SCL = 1
      CC = 0
      CurrLine = LinkedListDelete(CurrLine)

      SelectPos = CurrLine
      SCT = SCT + 1

      'Find case statements in this select case
      HasElse = 0
      ElseCaseNo = -1
      FindCase = CurrLine->Next
      Do While FindCase <> 0
        InLine = FindCase->Value

        IF Left(InLine, 12) = "SELECT CASE " THEN
          SCL += 1

        ElseIf SCL = 1 AND Left(InLine, 5) = "CASE " THEN

          'Trim trailing : from line
          If Right(InLine, 1) = ":" Then
            InLine = Left(InLine, Len(InLine) - 1)
          End If

          Condition = Trim(Mid(InLine, 5))
          IF INSTR(Condition, ";?F") <> 0 THEN
            Origin = Mid(Condition, INSTR(Condition, ";?F"))
            Condition = RTrim(Left(Condition, INSTR(Condition, ";?F") - 1))
          END If

          'Store found condition and reference to line containing case

          CurrCase = LinkedListInsert(CurrCase, Condition)
          CurrCase->MetaData = FindCase
          CC += 1

          'Anything after case else gets ignored
          If Condition = "ELSE" Then
            HasElse = -1
            ElseCaseNo = CC
          ElseIf HasElse Then
            'TODO: Warn about ignored case and remove dead code
            Color 14
            Print "Select ignoring code after Case Else"
            Color 7
          End If

        ElseIf Left(InLine, 10) = "END SELECT" THEN

          IF SCL = 1 THEN
            FindCase->Value = "SysSelectEnd" + Str(SCT) + LabelEnd
            GetMetaData(FindCase)->IsLabel = -1
          END IF
          SCL = SCL - 1

        END IF

        'Quit once End Select found
        If SCL = 0 Then Exit Do
        FindCase = FindCase->Next
      Loop

      'Empty select? Skip if yes
      If CaseStatements->Next <> 0 Then

        'Compile as jump table or as series of tests?
        'If all conditions are a number, and range is limited, use jump table
        MinValue = 65535
        MaxValue = -32768
        AllNumeric = -1
        HasElse = 0
        CurrCase = CaseStatements->Next
        Do While CurrCase <> 0
          CurrCase->NumVal = -32768
          If IsConst(CurrCase->Value) And Left(CurrCase->Value, 1) <> "@" Then
            NumVal = MakeDec(CurrCase->Value)
            If NumVal < MinValue Then MinValue = NumVal
            If NumVal > MaxValue Then MaxValue = NumVal
            CurrCase->NumVal = NumVal
          ElseIf InStr(CurrCase->Value, " TO ") <> 0 Then
            Temp = RTrim(Left(CurrCase->Value, InStr(CurrCase->Value, " TO ") - 1))
            If IsConst(Temp) And Left(Temp, 1) <> "@" Then
              NumVal = MakeDec(Temp)
              If NumVal < MinValue Then MinValue = NumVal
              If NumVal > MaxValue Then MaxValue = NumVal
            Else
              AllNumeric = 0
              Exit Do
            End If

            Temp = LTrim(Mid(CurrCase->Value, InStr(CurrCase->Value, " TO ") + 4))
            If IsConst(Temp) And Left(Temp, 1) <> "@" Then
              NumVal = MakeDec(Temp)
              If NumVal < MinValue Then MinValue = NumVal
              If NumVal > MaxValue Then MaxValue = NumVal
              CurrCase->NumVal = NumVal
            Else
              AllNumeric = 0
              Exit Do
            End If

          ElseIf CurrCase->Value = "ELSE" Then
            HasElse = -1
          Else
            AllNumeric = 0
            Exit Do
          End If

          CurrCase = CurrCase->Next
        Loop
        UseJumpTable = 0
        'Jump table takes one word per value in range plus about 14 words overhead
        'Checking individually takes about 4 words per case - which is smaller?
        If AllNumeric And (MaxValue - MinValue) < 127 And (14 + MaxValue - MinValue) < CC * 4 Then UseJumpTable = -1

        'Keep jump table mode off while problem with large Select blocks (over 2047 words on AVR, over 1023 on PIC 18F)
        If HashMapGet(Constants, "SELECT_USE_JUMP_TABLES") = 0 Then UseJumpTable = 0

        'Compile case statements (using jump table)
        If UseJumpTable Then

          'Prepare
          If ElseCaseNo = -1 Then
            ElseCaseLabel = "SysSelectEnd" + Str(SCT)

          Else
            ElseCaseLabel = "SysSelect" + Str(SCT) + "Case" + Str(ElseCaseNo)
          End If

          'Generate jump table
          TablePos = SelectPos
          'Check select value, jump to else if too high
          If ModePIC Then
            If MinValue = 0 Then
              TablePos = LinkedListInsert(TablePos, "SYSBYTETEMPA=" + SelectValue)
            Else
              TablePos = LinkedListInsert(TablePos, "SYSBYTETEMPA=" + SelectValue + "-" + Str(MinValue))
            End If
            TablePos = LinkedListInsertList(TablePos, CompileConditions("SYSBYTETEMPA>" + Str(MaxValue - MinValue), "TRUE", Origin, CompSub))
            TablePos = LinkedListInsert(TablePos, " goto " + ElseCaseLabel)
          ElseIf ModeAVR Then
            If MinValue = 0 Then
              TablePos = LinkedListInsert(TablePos, "SYSSTRINGLENGTH=" + SelectValue)
            Else
              TablePos = LinkedListInsert(TablePos, "SYSSTRINGLENGTH=" + SelectValue + "-" + Str(MinValue))
            End If
            TablePos = LinkedListInsertList(TablePos, CompileConditions("SYSSTRINGLENGTH>" + Str(MaxValue - MinValue), "TRUE", Origin, CompSub))
            TablePos = LinkedListInsert(TablePos, " rjmp " + ElseCaseLabel)
          End If
          'Generate computed goto
          If ModePIC Then
            If ChipFamily = 12 THEN
              TablePos = LinkedListInsert(TablePos, " movf SYSBYTETEMPA, W")
              TablePos = LinkedListInsert(TablePos, " addwf PCL, F")

            ElseIf ChipFamily = 15 Then
              TablePos = LinkedListInsert(TablePos, " movf SYSBYTETEMPA, W")
              TablePos = LinkedListInsert(TablePos, " brw")

            ElseIf ChipFamily = 14 Or ChipFamily = 16 Then
              If ChipFamily = 14 Then
                TablePos = LinkedListInsert(TablePos, " movf SYSBYTETEMPA, W")
              Else
                TablePos = LinkedListInsert(TablePos, " rlcf SYSBYTETEMPA, W")
              End If
              TablePos = LinkedListInsert(TablePos, " addlw low SysSelect" + Str(SCT) + "Vectors")
              TablePos = LinkedListInsert(TablePos, " movwf SYSBYTETEMPA")
              TablePos = LinkedListInsert(TablePos, " movlw high SysSelect" + Str(SCT) + "Vectors")
              TablePos = LinkedListInsert(TablePos, " btfsc STATUS, C")
              TablePos = LinkedListInsert(TablePos, " addlw 1")
              TablePos = LinkedListInsert(TablePos, " movwf PCLATH")
              TablePos = LinkedListInsert(TablePos, " movf SYSBYTETEMPA, W")
              TablePos = LinkedListInsert(TablePos, " movwf PCL")
              TablePos = LinkedListInsert(TablePos, "SysSelect" + Str(SCT) + "Vectors")
            End If
          ElseIf ModeAVR Then
            TablePos = LinkedListInsert(TablePos, " ldi SysReadA, low(SysSelect" + Str(SCT) + "Vectors)")
            TablePos = LinkedListInsert(TablePos, " ldi SysReadA_H, high(SysSelect" + Str(SCT) + "Vectors)")
            TablePos = LinkedListInsert(TablePos, " add SysReadA, SYSSTRINGLENGTH")
            TablePos = LinkedListInsert(TablePos, " brcc PC + 2")
            TablePos = LinkedListInsert(TablePos, " inc SysReadA_H")

            TablePos = LinkedListInsert(TablePos, " ijmp")
            TablePos = LinkedListInsert(TablePos, "SysSelect" + Str(SCT) + "Vectors:")

            AddVar "SysStringA", "BYTE", 1, 0, "REAL", "", , -1
            AddVar "SysReadA", "WORD", 1, 0, "REAL", "", , -1
          End If

          'Generate labels to jump to
          For NumVal = MinValue To MaxValue
            CC = 0
            CurrCase = CaseStatements->Next
            Do While CurrCase <> 0
              CC += 1
              FindCase = CurrCase->MetaData
              Condition = CurrCase->Value

              If InStr(Condition, " TO ") <> 0 Then
                Temp = RTrim(Left(CurrCase->Value, InStr(CurrCase->Value, " TO ") - 1))
                Condition = LTrim(Mid(CurrCase->Value, InStr(CurrCase->Value, " TO ") + 4))
                If NumVal < MakeDec(Temp) Or NumVal > MakeDec(Condition) Then GoTo CheckNextCase
              Else
                If NumVal <> CurrCase->NumVal Then GoTo CheckNextCase
              End If

              'Found correct case
              'TODO: Make this work if one of the jumps has to go a long way
              If ModePIC Then
                If ChipFamily = 16 Then
                  TablePos = LinkedListInsert(TablePos, " bra SysSelect" + Str(SCT) + "Case" + Str(CC))
                Else
                  TablePos = LinkedListInsert(TablePos, " goto SysSelect" + Str(SCT) + "Case" + Str(CC))
                End If
              ElseIf ModeAVR Then
                TablePos = LinkedListInsert(TablePos, " rjmp SysSelect" + Str(SCT) + "Case" + Str(CC))
              End If

              Exit Do
              CheckNextCase:
              CurrCase = CurrCase->Next
            Loop
          Next

          'Process case statements
          CC = 0
          CurrCase = CaseStatements->Next
          Do While CurrCase <> 0
            CC += 1
            FindCase = CurrCase->MetaData

            If CC = 1 THEN
              FindCase->Value = "SysSelect" + Str(SCT) + "Case" + Str(CC) + LabelEnd
            Else
              If ModePIC Then
                FindCase->Value = " goto SysSelectEnd" + Str(SCT)
                FindCase = LinkedListInsert(FindCase, "SysSelect" + Str(SCT) + "Case" + Str(CC))
              ElseIf ModeAVR Then
                FindCase->Value = " rjmp SysSelectEnd" + Str(SCT)
                FindCase = LinkedListInsert(FindCase, "SysSelect" + Str(SCT) + "Case" + Str(CC) + ":")
              End If
            END If

            CurrCase = CurrCase->Next
          Loop

        'Compile case statements (using sequential tests)
        Else
          CC = 0
          CurrCase = CaseStatements->Next
          Do While CurrCase <> 0
            CC += 1
            FindCase = CurrCase->MetaData
            Condition = CurrCase->Value
            If CurrCase->Next = 0 Then
              NextCaseLabel = "SysSelectEnd" + Str(SCT) 'seems to be for select greater than 1
            Else
              NextCaseLabel = "SysSelect" + Str(SCT) + "Case" + Str(CC + 1)
              GetMetaData(FindCase)->IsLabel = -1
            End If

            If CC = 1 THEN
              FindCase->Value = "SysSelect" + Str(SCT) + "Case" + Str(CC) + LabelEnd
              GetMetaData(FindCase)->IsLabel = -1
            Else
              If ModePIC Then
                FindCase->Value = " goto SysSelectEnd" + Str(SCT)
                GetMetaData(FindCase)->IsLabel = 0
                FindCase = LinkedListInsert(FindCase, "SysSelect" + Str(SCT) + "Case" + Str(CC))
                GetMetaData(FindCase)->IsLabel = -1
              ElseIf ModeAVR Then
                FindCase->Value = " rjmp SysSelectEnd" + Str(SCT)
                FindCase = LinkedListInsert(FindCase, "SysSelect" + Str(SCT) + "Case" + Str(CC) + ":")
              End If
            END If

            IF Condition <> "ELSE" THEN
              'Prepare condition test
              If InStr(Condition, " TO ") <> 0 Then
                'Test range
                Temp = RTrim(Left(CurrCase->Value, InStr(CurrCase->Value, " TO ") - 1))
                Condition = LTrim(Mid(CurrCase->Value, InStr(CurrCase->Value, " TO ") + 4))

                'Test lower
                NewCode = CompileConditions(SelectValue + "<" + Temp, "TRUE", Origin, CompSub)
                FindCase = LinkedListInsertList(FindCase, NewCode)
                If ModePIC Then
                  FindCase = LinkedListInsert(FindCase, " goto " + NextCaseLabel)
                ElseIf ModeAVR Then
                  FindCase = LinkedListInsert(FindCase, " rjmp " + NextCaseLabel)
                End If
                'Test upper
                NewCode = CompileConditions(SelectValue + ">" + Condition, "TRUE", Origin, CompSub)
                FindCase = LinkedListInsertList(FindCase, NewCode)
                If ModePIC Then
                  FindCase = LinkedListInsert(FindCase, " goto " + NextCaseLabel)
                ElseIf ModeAVR Then
                  FindCase = LinkedListInsert(FindCase, " rjmp " + NextCaseLabel)
                End If

              Else
                'Only one value to test
                IF CountOccur(Condition, "';=~<>{}") = 0 THEN Condition = "=" + Condition
                Condition = SelectValue + Condition
                NewCode = CompileConditions(Condition, "FALSE", Origin, CompSub)
                FindCase = LinkedListInsertList(FindCase, NewCode)
                If ModePIC Then
                  FindCase = LinkedListInsert(FindCase, " goto " + NextCaseLabel)
                ElseIf ModeAVR Then
                  FindCase = LinkedListInsert(FindCase, " rjmp " + NextCaseLabel)
                End If
              End If

            END If

            CurrCase = CurrCase->Next
          Loop
        End If
      END If

      If CaseStatements <> 0 Then
        LinkedListDeleteList(CaseStatements, 0)
      EndIf

    End If

    CurrLine = CurrLine->Next
  Loop

END SUB

SUB CompileSet (CompSub As SubType Pointer)
  FoundCount = 0
  Dim As String InLine, Origin, Temp
  Dim As String VarName, VarBit, Status

  Dim As String Token(20)
  Dim As Integer Tokens

  Dim As LinkedListElement Pointer CurrLine
  Dim As PinDirType Pointer CurrPinDir

  'Find SET commands
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0

    InLine = CurrLine->Value
    IF UCase(Left(InLine, 4)) = "SET " THEN

      'Get origin
      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      'Get Tokens from line
      GetTokens(InLine, Token(), Tokens)
      'Should be: 1 = SET, 2 = Var, 3 = Bit, 4 = Status

      'Delete line from program, will add asm later
      CurrLine = LinkedListDelete(CurrLine)

      'Do we have a bit by itself?
      If Tokens = 3 Then
        VarName = GetWholeSFR(Token(2))
        If InStr(VarName, ".") <> 0 Then
          'Found valid bit, correct command
          Tokens = 4
          Token(4) = Token(3)
          Token(2) = Left(VarName, InStr(VarName, ".") - 1)
          Token(3) = Mid(VarName, InStr(VarName, ".") + 1)
        End If
      End If

      'Get var/port and bit
      If Tokens = 4 Then
        VarName = Token(2)
        VarBit = Token(3)
        Status = Token(4)

        'If writing to a pin, record it
        CurrPinDir = GetPinDirection(VarName + "." + VarBit)
        If CurrPinDir <> 0 Then
          CurrPinDir->WrittenTo = -1
        End If

      Else
        'Wrong number of input parameters
        Temp = Message("BadParamsSet")
        LogError Temp, Origin
        GOTO BADSET
      End If

      'Generate code
      CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(VarName + "." + VarBit, Status, Origin, CompSub))

      FoundCount = FoundCount + 1
    END IF

  BADSET:
    CurrLine = CurrLine->Next
  Loop
END SUB

Function CompileSubCall (InCall As SubCallType Pointer) As LinkedListElement Pointer

  Dim As Integer CD, C, F, L, S, AF, StringConstCount, RP, ParamIsFn, LocOfFn, ST
  Dim As Integer StringConstLen
  Dim As String Temp, SendOrigin, ReceiveOrigin, SourceArray, DestArray
  Dim As String ArrayHandler, SourceArrayHandler, TempData, NextLine, GenName
  Dim As String CallCmd, MacroLineOrigin, SourceFunction
  Dim As LinkedListElement Pointer BeforeCode, AfterCode, BeforePos, AfterPos
  Dim As VariableType Pointer SourcePtr, SourceArrayPtr

  Dim As LinkedListElement Pointer OutList, CurrPos, MacroLine
  OutList = LinkedListCreate

  'Temporary code lists
  BeforeCode = LinkedListCreate
  AfterCode = LinkedListCreate
  BeforePos = BeforeCode
  AfterPos = AfterCode

  StringConstCount = 0

  'Load code to call sub into OutList
  With (*InCall)
    If .Called->IsMacro Then
      'Dealing with a macro
      'Adjust source as needed
      TidyInputSource(.Called)

      'Copy lines over, replacing params where needed
      CurrPos = OutList
      If PreserveMode = 2 Then
        PCC += 1
        PreserveCode(PCC) = ";Macro Source: " + GetFileLine(.Called->Origin)
        CurrPos = LinkedListInsert(CurrPos, "PRESERVE " + Str(PCC))
      End If

      MacroLine = .Called->CodeStart->Next
      Do While MacroLine <> 0
        TempData = MacroLine->Value
        'Remove original origin
        MacroLineOrigin = ""
        IF INSTR(TempData, ";?F") <> 0 THEN
          MacroLineOrigin = " " + Mid(TempData, INSTR(TempData, ";?F"))
          TempData = RTrim(Left(TempData, INSTR(TempData, ";?F") - 1))
          'If macro line has an origin, check if dest line also has one
          If .Origin <> "" Then
            'If dest line has origin, use that instead of macro line's original
            MacroLineOrigin = " " + Trim(.Origin)
          End If
        END If
        'Replace parameters
        For RP = 1 to .Params
          'Change 1205 add the IF check to stop recursive replacement  - so, the IF and ENDIF
          If .Called->Params(RP).Name <> .Param(RP, 1) Then
            WholeReplace TempData, .Called->Params(RP).Name, .Param(RP, 1)   'this was the original line. When the two params are the same then the compiler locked up as it never exits WholeReplace()
          Else
            Temp = Message("UndeclaredMacroVar")
            Replace Temp, "%var%", .Param(RP, 1)
            LogError Temp, .Origin
          End If
        Next
        'Hide macro name (if macro is a function)
        If .Called->IsFunction Then
          Do While InStr(TempData, .Called->Name) <> 0
            Replace (TempData, .Called->Name, Chr(31) + Str((*InCall).CalledID) + Chr(31))
          Loop
        End If
        'Insert line (or lines, divided by :)
        If TempData <> "" Then

          Do While InStr(TempData, ":") <> 0
            NextLine = Trim(Mid(TempData, InStr(TempData, ":") + 1))
            'If line is label, : should be last character
            If NextLine <> "" Then
              CurrPos = LinkedListInsert(CurrPos, Trim(Left(TempData, InStr(TempData, ":") - 1)) + MacroLineOrigin)
              TempData = NextLine
            End If
          Loop

          'Append insertion origin
          If TempData <> "" Then
            CurrPos = LinkedListInsert(CurrPos, TempData + MacroLineOrigin)
          End If
        End If
        MacroLine = MacroLine->Next
      Loop
      Return OutList

    Else
      'Not dealing with a macro, so set parameters and call
      'Get call command
      With (*.Called)
        If .IsFunction Then
          'GenName = "FN_" + .Name
          GenName = "FN_" + Chr(31) + Str((*InCall).CalledID) + Chr(31)
        Else
          'GenName = .Name
          GenName = Chr(31) + Str((*InCall).CalledID) + Chr(31)
        End If
        IF .Overloaded Then
          CallCmd = " call " + GenName + Str((*InCall).CalledID)
        Else
          CallCmd = " call " + GenName
        End If
        'Print "Compiling call to " + .Name + " (from " + InCall->Caller->Name + ")"
      End With

      'Optional params
      If (*.Called).ParamCount > .Params Then
        For CD = .Params + 1 To (*.Called).ParamCount
          .Param(CD, 1) = ""
        Next
        .Params = (*.Called).ParamCount
      End If

      'Make code to copy parameters
      FOR CD = 1 TO .Params

        'Load defaults into optional parameters
        IF (*.Called).Params(CD).Default <> "" And .Param(CD, 1) = "" Then
          .Param(CD, 1) = (*.Called).Params(CD).Default

          'Constants won't have been replaced in default values
          .Param(CD, 1) = ReplaceConstantsLine(.Param(CD, 1), 0)
          .Param(CD, 2) = TypeOfValue(.Param(CD, 1), Subroutine(GetSubID(.Origin)))
        End If

        'Decide how to pass parameter
        'C = 0: Copy variable
        'C = 1: Copy constant value
        'C = 2: Copy calculation result
        'C = 3: Copy string/array reference
        'C = 4: Copy string into temp array then pass reference
        'C = 5: Concatenate strings, store in temp array, pass reference
        'C = -1: Error
        If .Param(CD, 1) <> "" Then
          C = 0
          IF IsConst(.Param(CD, 1)) THEN C = 1
          IF IsCalc(.Param(CD, 1)) THEN C = 2
          IF INSTR((*.Called).Params(CD).Type, "$") <> 0 Or INSTR((*.Called).Params(CD).Type, "()") <> 0 Or (*.Called).Params(CD).Type = "STRING" Then
            C = 3
            If InStr(.Param(CD, 1), "+") Then C = 5
          End If
          'IF INSTR(.Param(CD, 1), ";STRING") <> 0 THEN
          'Only search for ;STRING at start, not in middle!
          IF INSTR(.Param(CD, 1), ";STRING") = 1 Then
            Temp = StringStore(Val(MID(.Param(CD, 1), Instr(.Param(CD, 1), ";STRING") + 7))).Value
            If Len(Temp) > 1 Or (INSTR((*.Called).Params(CD).Type, "$") <> 0 Or INSTR((*.Called).Params(CD).Type, "()") <> 0 Or (*.Called).Params(CD).Type = "STRING") Then
              C = 4
              If InStr(.Param(CD, 1), "+") Then C = 5
            Else
              C = 1
            End If
          End If
        Else
          Temp = Message("MissingSubParam")
          Replace Temp, "%param%", (*.Called).Params(CD).Name
          LogError Temp, .Origin
          C = -1
        End If
        'Print , "Copying "; .Param(CD, 1); " to "; *.Called.Params(CD).Name; " using mode "; C

        'Pass by copying
        If C = 0 OR C = 1 OR C = 2 THEN
          'Calculate new origins
          'Important so that variable types are detected correctly
          SendOrigin = ""
          ReceiveOrigin = ""
          If .Origin <> "" Then
            F = VAL(Mid(.Origin, INSTR(.Origin, "F") + 1))
            L = VAL(Mid(.Origin, INSTR(.Origin, "L") + 1))
            Temp = Mid(.Origin, INSTR(.Origin, "S") + 1)
            IF INSTR(Temp, "D") <> 0 Then Temp = Left(Temp, INSTR(Temp, "D") - 1)
            S = VAL(Temp)
          Else
            'If no origin present, make one up so variable assignments work
            'As long as sub (S) is correct, it will work properly
            '(except for maybe giving misleading error locations)
            F = 0
            L = 0
            S = 0
            For LocOfFn = 0 To SBC
              If Subroutine(LocOfFn) = .Caller Then
                S = LocOfFn
                Exit For
              End If
            Next
          End If
          SendOrigin = ";?F" + Str(F) + "L" + Str(L) + "S" + Str(S) + "D" + Str(.CalledID) + "?"
          ReceiveOrigin = ";?F" + Str(F) + "L" + Str(L) + "S" + Str(.CalledID) + "D" + Str(S) + "?"

          If ((*.Called).Params(CD).Dir And 1) <> 0 Then
            BeforePos = LinkedListInsert(BeforePos, (*.Called).Params(CD).Name + " = " + .Param(CD, 1) + SendOrigin)
          End If
          'Detect parameters that are functions, don't try returning values in them
          '(Unless parameter is the current function, in which case treat it as a variable)
          ParamIsFn = 0
          LocOfFn = LocationOfSub(.Param(CD, 1), "", .Origin, -1)
          If LocOfFn <> 0 And LocOfFn <> S Then
            If Subroutine(LocOfFn)->IsFunction Then ParamIsFn = -1
          End If
          IF C = 0 AND ((*.Called).Params(CD).Dir And 2) <> 0 And Not ParamIsFn THEN
            AfterPos = LinkedListInsert(AfterPos, .Param(CD, 1) + " = " + (*.Called).Params(CD).Name + ReceiveOrigin)
          ElseIf (*.Called).Params(CD).Dir = 2 And C <> 0 Then
            'Error, need to supply output
            Temp = Message("SubParamNotVar")
            Replace Temp, "%value%", .Param(CD, 1)
            Replace Temp, "%param%", (*.Called).Name
            LogError Temp, .Origin
          End IF
        END IF

        'Pass string using SYSTEMPARRAY
        IF C = 4 Or C = 5 THEN
          'On 16F1, should pass reference to string location if sub will not be changing it
          If C = 4 And ChipFamily = 15 And (*.Called).Params(CD).Dir = 1 Then
            'Print "Opportunity for optimisation: "; *.Called.Params(CD).Name; " = "; .Param(CD, 1)
            'Destination array
            DestArray = (*.Called).Params(CD).Name
            IF INSTR(DestArray, "()") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "()") - 1)
            IF INSTR(DestArray, "$") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "$") - 1)
            'Create destination array if necessary
            SourcePtr = VarAddress(DestArray, .Called)
            AddVar DestArray, "BYTE", 2, .Called, "POINTER", .Origin
            'Set handler name
            ArrayHandler = "Sys" + DestArray + "Handler"
            AddVar ArrayHandler, "WORD", 1, 0, "REAL", .Origin, , -1 'Make handler global
            'Copy reference
            ST = VAL(Mid(.Param(CD, 1), 8))
            StringStore(ST).Used = -1
            BeforePos = LinkedListInsert(BeforePos, " movlw low StringTable" + Str(ST))
            BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler)
            BeforePos = LinkedListInsert(BeforePos, " movlw (high StringTable" + Str(ST) + ") | 128")
            BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler + "_H")

          'In other situations, copy string constant into temporary variable, then pass reference to that
          Else
            StringConstLen = Len(GetString(.Param(CD, 1), 0))
            StringConstCount += 1

           'Start of #1039 - Concat changes
           'print StringConstLen, *.Called.Params(CD).Name, .Param(CD, 1)
           'Need to ensure a concat is resolved
            StringConCatLengthAdapted = 0
           'Declare a local variables
            Dim As String  StringConCatString = ""

            If HashMapGet(Constants, "SYSDEFAULTCONCATSTRING" ) > 0  then
              'Always use this if defined constant
              StringConstLen = val(HashMapGetStr(Constants, "SYSDEFAULTCONCATSTRING" ))
              StringConCatString = ";Used SYSDEFAULTCONCATSTRING constant to create SYSSTRINGPARAM"+ str(StringConstCount)+" string of length " + str(StringConstLen) + " to cater for constant_string + constant_string Concatenation"
              StringConCatLengthAdapted = -1

            End If

            If PreserveMode <> 0 Then
              If StringConCatLengthAdapted = -1 then
                  LinkedListInsert(BeforePos,StringConCatString)
              Else
                  LinkedListInsert(BeforePos,";Using SYSSTRINGPARAM"+Str(StringConstCount)+"*"+str(StringConstLen))
              End if
            End If
            'End of #1039 - Concat changes

            BeforePos = LinkedListInsert(BeforePos, "SYSSTRINGPARAM" + Str(StringConstCount) + "=" + .Param(CD, 1) + .Origin)
            .Param(CD, 1) = "SYSSTRINGPARAM" + Str(StringConstCount)
            .Param(CD, 2) = "STRING"
            AddVar("SYSSTRINGPARAM" + Str(StringConstCount), "STRING*" + Str(StringConstLen), StringConstLen, Subroutine(0), "REAL", .Origin, , -1)
            'Record use of temporary variable
            LinkedListInsert(.Caller->TemporaryVars, "SYSSTRINGPARAM" + Str(StringConstCount))
            C = 3
          End If
        END IF

        'Pass array by reference (set pointer)
        IF C = 3 THEN
          'Source
          SourceArray = .Param(CD, 1)
          'IF INSTR(SourceArray, "(") <> 0 THEN SourceArray = Left(SourceArray, INSTR(SourceArray, "(") - 1)
          IF INSTR(SourceArray, "()") <> 0 THEN SourceArray = RTrim(Left(SourceArray, INSTR(SourceArray, "()") - 1))
          IF INSTR(SourceArray, "$") <> 0 THEN SourceArray = RTrim(Left(SourceArray, INSTR(SourceArray, "$") - 1))

          'Is SourceArray a function?
          LocOfFn = LocationOfSub(.Param(CD, 1), "", .Origin, -1)

          'SourceArray is function
          If LocOfFn <> 0 Then
            SourceFunction = SourceArray
            IF INSTR(SourceArray, "(") <> 0 THEN SourceArray = RTrim(Left(SourceArray, INSTR(SourceArray, "(") - 1))
            SourceArrayPtr = VarAddress(ReplaceFnNames(SourceArray), .Caller)
            If SourceArrayPtr = Cast( VariableType Pointer, INVALIDARRAYVALUE )Then
              'this traps the error in VarAddress()
              GoTo CompileNextParam
            End If
            If SourceArrayPtr = 0 Then
              'This should never run
'              Color 12
'              Print "Internal error in CompileSubCall: A GCBASIC library method " + ReplaceFnNames(SourceArray) + " exists but the return variable does not. Is this a call to a Subroutine rather than a Function? "
'              Print "(in " + .Caller->Name + ", calling " + SourceArray + ")"
'              Color 7
              Dim temp as String
              Temp = Message( "CannotHandleFunctionCall")
              Replace ( Temp, "%fn" , ReplaceFnNames(SourceArray) )
              LogError Temp, .Origin
              GoTo CompileNextParam
            End If

            'Remove function name from SourceArray
            SourceArray = Chr(31) + Str(LocOfFn) + Chr(31)

          'SourceArray is not a function
          Else
            SourceFunction = SourceArray
            SourceArrayPtr = VarAddress(ReplaceFnNames(SourceArray), .Caller)
            If Cast( Integer, SourceArrayPtr ) = INVALIDARRAYVALUE Then
                Dim Temp as String
                Temp = Message("CannotHandleArrayConstruction")
                LogError Temp, .Origin
                GoTo CompileNextParam
            End If
            'If source array not found, show error
            If SourceArrayPtr = 0 Then
              Temp = Message("ArrayNoDec")
              Replace Temp, "%name%", ReplaceFnNames(SourceArray)
              LogError Temp, .Origin
              GoTo CompileNextParam
            Else
              if left( ucase( trim(ReplaceFnNames(SourceArray) )),1)  = "(" AND right( ucase( trim(ReplaceFnNames(SourceArray) )),1)  = ")" then
                  'This should never run
                   Temp = Message("DoubleBracesError")
                   Replace Temp, "%source%", ReplaceFnNames(SourceArray)
                   LogError Temp, .Origin
                   GoTo CompileNextParam
              End if
              'Mark source variable as used
              RequestVariable(ReplaceFnNames(SourceArray), .Caller)
            End If
          End If

          'Destination
          DestArray = (*.Called).Params(CD).Name
          'IF INSTR(DestArray, "(") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "(") - 1)
          IF INSTR(DestArray, "()") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "()") - 1)
          IF INSTR(DestArray, "$") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "$") - 1)

          'Create destination array if necessary
          SourcePtr = VarAddress(DestArray, .Called)
          AddVar DestArray, "BYTE", 2, .Called, "POINTER", .Origin

          'Set handler name
          ArrayHandler = "Sys" + DestArray + "Handler"
          AddVar ArrayHandler, "WORD", 1, 0, "REAL", .Origin, , -1 'Make handler global

          'May need to hide function names in array names
          Do While InStr(SourceArray, .Called->Name) <> 0
            Replace (SourceArray, .Called->Name, Chr(31) + Str((*InCall).CalledID) + Chr(31))
          Loop
          Do While InStr(ArrayHandler, .Called->Name) <> 0
            Replace (ArrayHandler, .Called->Name, Chr(31) + Str((*InCall).CalledID) + Chr(31))
          Loop
          SourceArrayHandler = "Sys" + SourceArray + "Handler"

          If ModePIC Then
            If SourceArrayPtr->Pointer = "POINTER" Then
              If ChipFamily = 16 Then
                BeforePos = LinkedListInsert(BeforePos, " movff " + SourceArrayHandler + "," + ArrayHandler)
                BeforePos = LinkedListInsert(BeforePos, " movff " + SourceArrayHandler + "_H," + ArrayHandler + "_H")
              Else
                BeforePos = LinkedListInsert(BeforePos, " movf " + SourceArrayHandler + ",W")
                BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler)
                BeforePos = LinkedListInsert(BeforePos, " movf " + SourceArrayHandler + "_H,W")
                BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler + "_H")
              End If
            Else
              BeforePos = LinkedListInsert(BeforePos, " movlw low " + SourceFunction)
              BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler)
              BeforePos = LinkedListInsert(BeforePos, " movlw high " + SourceArray)
              BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler + "_H")
            End If
          ElseIf ModeAVR Then
            If SourceArrayPtr->Pointer = "POINTER" Then
              BeforePos = LinkedListInsert(BeforePos, " lds SysValueCopy," + SourceArrayHandler)
              BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + ",SysValueCopy")
              BeforePos = LinkedListInsert(BeforePos, " lds SysValueCopy," + SourceArrayHandler + "_H")
              BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + "_H,SysValueCopy")
            Else
              BeforePos = LinkedListInsert(BeforePos, " ldi SysValueCopy,low(" + SourceFunction + ")")
              BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + ",SysValueCopy")
              BeforePos = LinkedListInsert(BeforePos, " ldi SysValueCopy,high(" + SourceArray + ")")
              BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + "_H,SysValueCopy")
            End If
            AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
          End If
        END If

        CompileNextParam:
      Next

      'Mark sub as required
      .Called->Required = -1
      'Mark file sub came from as used
      SourceFile(.Called->SourceFile).InitSubUsed = -1

    End If

  End With

  'Copy code to output array
  CurrPos = OutList
  CurrPos = LinkedListInsertList(CurrPos, BeforeCode)
  CurrPos = LinkedListInsert(CurrPos, CallCmd)
  CurrPos = LinkedListInsertList(CurrPos, AfterCode)

  Return OutList
End Function

Sub CompileSubCalls(CompSub As SubType Pointer)

  Dim As String TempLine, LineFromFile, BeforeFn, FunctionName, FunctionParams, AfterFn
  Dim As String FunctionType, Origin, NewOrigin, TempVarName, TempData
  Dim As String ReturnVar
  Dim As Integer CD, DS, S, E, BL, FB, F, PD, FoundFunction, MatchScore, BetterMatch
  Dim As Integer L, D, SL, Temp, UseTempVar, FunctionTypeID, CurrSub
  Dim As Integer ParamsInBrackets, CurrAliasByte, FirstBracketLoc
  Dim As Integer LastBracketLoc, BracketsRequired

  Dim As LinkedListElement Pointer CurrLine, NewCallCode, NewCallLine, LineBeforeCall
  Dim As LinkedListElement Pointer LineElements, CurrElement, MatchingSubs, MatchingSub
  Dim As LinkedListElement Pointer FindMatch

  Dim As SubCallType NewSubCall

  Dim TempVarCount(10) As Integer

  If CompSub->Name = "Main" or  Left(CompSub->Origin,5) = ";?F1L" Then
      MainSBC = MainSBC + 1
  End if


  'Prepare hash map of subs
  UpdateSubMap

  'Find functions
  CurrLine = CompSub->CodeStart->Next
  Origin = ""
  Do While CurrLine <> 0

    'Check if startup sub is needed for the line because of a constant
    DO WHILE INSTR(UCase(CurrLine->Value), ";STARTUP") <> 0
      Temp = VAL(Mid(CurrLine->Value, INSTR(UCase(CurrLine->Value), ";STARTUP") + 8))
      TempData = ";STARTUP" + Str(Temp)
      Replace CurrLine->Value, TempData, ""
      SourceFile(Temp).InitSubUsed = -1
      RequestSub(0, SourceFile(Temp).InitSub)
    Loop

    For PD = 1 to 10
      TempVarCount(PD) = 0
    Next

    FindFunctionsAgain:
    TempLine = CurrLine->Value
    NewOrigin = ""
    IF INSTR(TempLine, ";?F") <> 0 Then
      NewOrigin = MID(TempLine, INSTR(TempLine, ";?F"))
      If NewOrigin <> "" Then
        Origin = NewOrigin
      End If
    End If

    'Don't check "ON " statements for functions
    If UCASE(Left(TempLine, 3)) = "ON " Then Goto NextLineFunctions

    'Split up line
    LineElements = GetElements(TempLine)

    'Run through list of functions
    CurrElement = LineElements->Next
    Do While CurrElement <> 0

      MatchingSubs = HashMapGet(Subroutines, UCase(CurrElement->Value))
      MatchingSub = MatchingSubs
      Do While MatchingSub <> 0
        'Line contains call to this subroutine
        CurrSub = MatchingSub->NumVal

        FunctionName = Subroutine(CurrSub)->Name
        FunctionType = Subroutine(CurrSub)->ReturnType
        FunctionTypeID = CastOrder(FunctionType)

        'Check to see if a line contains a function
        If INSTR(UCASE(TempLine), UCase(FunctionName)) = 0 THEN Goto CheckForNextFunction

        SearchLineAgain:
        FoundFunction = WholeINSTR(TempLine, FunctionName, 0)

        'Avoid calling functions from themselves
        'Note: ignores overloaded sub and function with same name, need to detect that later
        If FoundFunction = 2 And Subroutine(CurrSub)->IsFunction Then
          If CompSub->Name = Subroutine(CurrSub)->Name Then FoundFunction = 0
        END IF

        'Have already dealt with line
        If NewOrigin = "" And Left(TempLine, 6) = " call " Then
          FoundFunction = 0
        End If

        IF FoundFunction <> 2 And INSTR(UCase(TempLine), UCase(FunctionName)) <> 0 THEN
          Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
          Goto SearchLineAgain
        END IF

        'If it does, call a sub, and get the value after
        IF FoundFunction = 2 THEN
          'Print "Found function: "; FunctionName
          'Check if a temp variable should be used
          UseTempVar = 0
          If Subroutine(CurrSub)->IsFunction Then
            If CountOccur(TempLine, FunctionName, -1) > 1 Then
              UseTempVar = -1
              TempVarCount(FunctionTypeID) += 1
              TempVarName = "SysFn" + FunctionType + Str(TempVarCount(FunctionTypeID))
              AddVar TempVarName, FunctionType, 1, CompSub, "REAL", Origin, , -1
            End If
            'Add a variable for the function result
            AddVar Subroutine(CurrSub)->Name, FunctionType, 1, Subroutine(CurrSub), "REAL", Origin, , -1
            AddVar Subroutine(CurrSub)->Name, FunctionType, 1, CompSub, "REAL", Origin, , -1
          End If

          'If line starts with call or GOSUB, remove call
          If Left(TempLine, 5) = "CALL " Then
            TempLine = Trim(Mid(TempLine, 6))
          ElseIf Left(TempLine, 6) = "GOSUB " Then
            TempLine = Trim(Mid(TempLine, 7))
          End If

          'Replace the function in the line
          'Get whatever is to the left of the function
          BeforeFn = LEFT(TempLine, INSTR(UCase(TempLine), FunctionName) - 1)

          'If there's something before, make sure we actually have a function
          If BeforeFn <> "" And Not Subroutine(CurrSub)->IsFunction Then
            Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
            Goto SearchLineAgain
          End If

          'Get the parameters of the function and the text after it
          FunctionParams = ""

          AfterFn = MID(TempLine, INSTR(UCase(TempLine), UCase(FunctionName)) + LEN(FunctionName))

          'Check to see if parameters are in brackets
          'FunctionName(params) - yes
          'SubName(param) - yes
          'SubName param - no
          'SubName (param1 + x), param2 - no
          'FunctionName(param) ms - yes
          'So, check for comma inside or outside brackets

          'Are brackets required?
          BracketsRequired = BeforeFn <> ""
          'Parse parameters
          BL = 0
          FirstBracketLoc = -1
          LastBracketLoc = -1
          For FB = 1 To Len(AfterFn)
            Select Case Mid(AfterFn, FB, 1)
              Case "("
                'Note position of opening bracket
                BL += 1
                If BL = 1 And FirstBracketLoc = -1 Then
                  FirstBracketLoc = FB
                End If
              Case ")"
                'Note position of closing bracket
                BL -= 1
                If BL = 0 Then
                  'If match found and all parameters must be in brackets, this is the end of these parameters
                  If BracketsRequired Then
                    LastBracketLoc = FB
                    Exit For
                  End If
                  'If we haven't seen a comma yet, this could be the end of the parameters
                  If LastBracketLoc <> -2 Then
                    LastBracketLoc = FB
                  End If
                End If
              Case " "
                'Do nothing
              Case ","
                'If a comma is found outside of brackets, the brackets do not mark the parameters
                If BL = 0 Then
                  LastBracketLoc = -2
                End If
              Case Else
                'If any other character is found before brackets, these brackets do not mark parameters
                'Example: SomeSub SomeFunction(param)
                If BL = 0 And FirstBracketLoc = -1 Then
                  LastBracketLoc = -2
                End If

                'If a mathematical operator is found after brackets, brackets do not mark parameters
                If BL = 0 And IsCalcDivider(Mid(AfterFn, FB, 1)) Then
                  LastBracketLoc = -2
                End If
            End Select
          Next
          'Extract parameters from brackets if brackets used
          If FirstBracketLoc <> -1 And LastBracketLoc > 0 Then
            FunctionParams = Trim(Mid(AfterFn, FirstBracketLoc + 1, LastBracketLoc - FirstBracketLoc - 1))
            
            'For a subroutine, remove anything after parameters
            If Subroutine(CurrSub)->IsFunction Then
              AfterFn = Mid(AfterFn, LastBracketLoc + 1)
            Else
              AfterFn = ""
            End If
          Else
            'No brackets - nothing or everything after sub/function name is a parameter
            If BracketsRequired Then
              'If we have a function without brackets, no parameters
              FunctionParams = ""
            Else
              'If we have a sub without brackets, everything after the name is a parameter
              FunctionParams = Trim(AfterFn)
              AfterFn = ""
            End If
          End If

          ValidateParameterIsValid( Subroutine(CurrSub)->Name, FunctionParams, Origin )

          'Remove origin from FunctionParams
          IF INSTR(FunctionParams, ";?F") <> 0 Then
            FunctionParams = RTrim(Left(FunctionParams, InStr(FunctionParams, ";?F") - 1))
          End If

          'Detect cases where overloaded sub and function have same name, and return of function is set
          'Need to detect here or the sub will be called with = return value as a parameter.
          If Subroutine(CurrSub)->Overloaded And Left(FunctionParams, 1) = "=" Then
            Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
            Goto SearchLineAgain
          End If

          'Prepare sub call
          'Print "Calling:"; FunctionName, "Params:"; FunctionParams
          ExtractParameters(NewSubCall, FunctionName, FunctionParams, Origin)
          With NewSubCall
            .Called = Subroutine(CurrSub)
            .Caller = CompSub
            .CalledID = CurrSub
            .Origin = Origin
          End With

          'Check function being called matches current function best
          If Subroutine(CurrSub)->Overloaded Then
            MatchScore = SubSigMatch(GetSubSig(*Subroutine(CurrSub)), NewSubCall.CallSig)
            BetterMatch = 0
            'Check for better matches
            FindMatch = MatchingSub->Next
            Do While FindMatch <> 0
              If Subroutine(CurrSub)->Name = Subroutine(FindMatch->NumVal)->Name Then
                If SubSigMatch(GetSubSig(*Subroutine(FindMatch->NumVal)), NewSubCall.CallSig) > MatchScore Then
                  BetterMatch = -1
                  Exit Do
                End If
              End If

              FindMatch = FindMatch->Next
            Loop

            'Skip this sub if it's not the best
            If BetterMatch Then
              Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
              Goto SearchLineAgain
            End If
          End If

          'Get return variable
          ReturnVar = ""
          If NewSubCall.Called->IsFunction Then
            If NewSubCall.Called->Overloaded Then
              If UCase(NewSubCall.Called->ReturnType) = "STRING" Then
                Dim Temp as String
                Temp = Message("OverLoadFunctionsNotSupported")
                Replace ( Temp, "%functionname%", NewSubCall.Called->Name)
                LogError Temp, Origin
                WriteErrorLog
                End

            Else
                ReturnVar = "SYS" + Chr(31) + Str(CurrSub) + CHR(31) + UCase(NewSubCall.Called->ReturnType)

                'Add alias for return variable with appropriate type
                If NewSubCall.Called->ReturnAlias = "" Then
                  For CurrAliasByte = GetTypeSize(NewSubCall.Called->ReturnType) - 1 To 0 Step -1
                    If NewSubCall.Called->ReturnAlias = "" Then
                      NewSubCall.Called->ReturnAlias = GetByte(NewSubCall.Called->Name, CurrAliasByte)
                    Else
                      NewSubCall.Called->ReturnAlias += "," + GetByte(NewSubCall.Called->Name, CurrAliasByte)
                    End If
                  Next
                  AddVar("SYS" + UCase(NewSubCall.Called->Name + NewSubCall.Called->ReturnType), NewSubCall.Called->ReturnType, 1, 0, "ALIAS:" + NewSubCall.Called->ReturnAlias, Origin,, -1)
                End If
              End If
            Else
              ReturnVar = CHR(31) + Str(CurrSub) + CHR(31)
            End If
          End If

          'Adjust line
          'Use 31 where name needs removal to prevent trying to handle it twice
          If UseTempVar Then
            TempLine = BeforeFn + TempVarName + AfterFn
          Else
            TempLine = BeforeFn + ReturnVar + AfterFn
          End If

          'Write back code
          'Print DS, BeforeFn, FunctionName, FunctionParams, AfterFn
          If Subroutine(CurrSub)->IsFunction Then

            'If vars are empty we can assume the function as no assignment.
            If Len(Trim(BeforeFn)) = 0 and Len(Trim(AfterFn)) = 0 and Len(Trim(FunctionParams)) = 0 Then
              Dim Temp as String
              Temp = Message("MissingFunctionAssignment")
              LogError Temp, Origin
            End If

            If NewSubCall.Called->Overloaded Then
              CurrLine->Value = ";FNSTART," + CHR(31) + Str(CurrSub) + CHR(31) + Str(CurrSub)
            Else
              CurrLine->Value = ";FNSTART," + CHR(31) + Str(CurrSub) + CHR(31)
            End If
          Else
            CurrLine = LinkedListDelete(CurrLine)
          End If
          LineBeforeCall = CurrLine
          CurrLine = LinkedListInsertList(CurrLine, CompileSubCall(@NewSubCall))

          'Record call
          If Not NewSubCall.Called->IsMacro Then
            RecordSubCall(CompSub, NewSubCall.Called)
          End If

          If TempLine <> "" Then
            If UseTempVar Then
              CurrLine = LinkedListInsert(CurrLine, TempVarName + "=" + ReturnVar)
              LinkedListInsert(CurrLine, TempLine)
            Else
              LinkedListInsert(CurrLine, TempLine)
            End If
          End If

          'Need to check line again, in case of nested functions
          CurrLine = LineBeforeCall
          GoTo NextLineFunctions

        END If

        CheckForNextFunction:
        'Use 30 for temporarily removing name, ie when close match occurs
        Dim OldName As String
        Do While Instr(TempLine, Chr(30)) <> 0
          OldName = MID(TempLine, Instr(TempLine, Chr(30)) + 1)
          OldName = Left(OldName, INSTR(OldName, Chr(30)) - 1)
          Replace TempLine, Chr(30) + OldName + Chr(30), Subroutine(Val(OldName))->Name
        Loop

        MatchingSub = MatchingSub->Next
      Loop

      CurrElement = CurrElement->Next
    Loop

    NextLineFunctions:
    CurrLine = CurrLine->Next
  Loop

  'Deal with any missed names
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0

    'Delete any lines with just a sub name
    If Left(CurrLine->Value, 1) = Chr(31) And InStr(CurrLine->Value, "=") = 0 Then
      CurrLine = LinkedListDelete(CurrLine)
    Else
      'Replace tokens with names
      CurrLine->Value = ReplaceFnNames(CurrLine->Value)
    End If
    CurrLine = CurrLine->Next
  Loop

End Sub

Sub CompileTables
  Dim As String Temp, Source, Table, ThisItem
  Dim As Integer PD, SP, CurrTableByte, StringTablesUsed, LargeTable
  Dim As Integer LastNonZeroElement, CurrElement

  Dim As LinkedListElement Pointer CurrLine
  Dim As SubType Pointer TableSub

  'Add string lookup tables
  StringTablesUsed = 0
  For PD = 1 To SSC
    If StringStore(PD).Used Then
      StringTablesUsed = -1
      Exit For
    End If
  Next

  IF StringTablesUsed THEN
    IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("StringTable")

    'Create sub for string tables
    TableSub = NewSubroutine("SysStringTables")
    SBC += 1: Subroutine(SBC) = TableSub
    CurrLine = TableSub->CodeStart
    TableSub->Compiled = -1
    TableSub->Required = 0
    TableSub->NoReturn = -1

    AddVar "StringPointer", "BYTE", 1, Subroutine(0), "REAL", "", , -1
    AddVar "SysStringA", "WORD", 1, Subroutine(0), "REAL", "", , -1

    If ModePIC And ChipFamily <> 16 Then
      If ChipFamily = 12 Then
        CurrLine = LinkedListInsert(CurrLine, " movf SysStringA,W")
        CurrLine = LinkedListInsert(CurrLine, " incf SysStringA,F")
        CurrLine = LinkedListInsert(CurrLine, " movwf PCL")
        LogWarning Message("WarningUnsafeString")
      Else
        'Get current pointer location
        CurrLine = LinkedListInsert(CurrLine, " movf SysStringA_H,W")
        CurrLine = LinkedListInsert(CurrLine, " movwf PCLATH")
        CurrLine = LinkedListInsert(CurrLine, " movf SysStringA,W")
        'Code will increment pointer for next access
        CurrLine = LinkedListInsert(CurrLine, " incf SysStringA,F")
        CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
        CurrLine = LinkedListInsert(CurrLine, " incf SysStringA_H,F")
        'Alter PCL
        CurrLine = LinkedListInsert(CurrLine, " movwf PCL")
      End If

    End If

    For PD = 1 TO SSC
      Table = "StringTable" + Str(PD) + LabelEnd
      Source = StringStore(PD).Value
      If StringStore(PD).Used Then
        CurrLine = LinkedListInsert(CurrLine, "")
        CurrLine = LinkedListInsert(CurrLine, Table)
        GetMetaData(Currline)->IsLabel = -1
        If ModePIC Then
          'Table on low/mid range chip, use retlw
          If ChipFamily <> 16 THEN
              CurrLine = LinkedListInsert(CurrLine, " retlw " + Str(LEN(Source)))
            For SP = 1 TO LEN(Source)
              Temp = " retlw " + Str(ASC(Mid(Source, SP, 1)))
              IF Mid(Source, SP, 1) <> "=" THEN Temp = Temp + Chr(9) + ";" + Mid(Source, SP, 1)
              IF Mid(Source, SP, 1) = "=" THEN Temp = Temp + Chr(9) + "; (equals)"
              CurrLine = LinkedListInsert(CurrLine, Temp)
            Next
          'Table on high end, can use db and then read with tblrd
          Else
            Temp = ""
            FOR SP = 0 TO LEN(Source)
              If SP = 0 Then
                ThisItem = Str(Len(Source))
              Else
                ThisItem = Str(Asc(Mid(Source, SP, 1)))
              End If
              If Len(Temp) + Len(ThisItem) >= 79 And Temp <> "" And (SP Mod 2) = 0 Then
                CurrLine = LinkedListInsert(CurrLine, " db " + Temp)
                Temp = ThisItem
              Else
                If Temp = "" Then
                  Temp = ThisItem
                Else
                  Temp = Temp + "," + ThisItem
                End If
              End If
            NEXT
            If Temp <> "" Then CurrLine = LinkedListInsert(CurrLine, " db " + Temp)
          End If

        ElseIf ModeAVR Then
          Temp = Str(LEN(Source))
          For SP = 1 TO LEN(Source)
            Temp = Temp + "," + Str(ASC(Mid(Source, SP, 1)))
          Next
          'Need to pad with 0 to make even length
          If Len(Source) Mod 2 = 0 Then
            Temp = Temp + ",0"
          End If
          CurrLine = LinkedListInsert(CurrLine, ".DB " + Temp)
        End If

        TableSub->Required = -1
        CurrLine = LinkedListInsert(CurrLine, "")

        IF AFISupport = 1  and ModePIC and ChipFamily = 16 Then
          CurrLine = LinkedListInsert(CurrLine, " ALIGN 2;X3")
        End if

      End If

    Next


  End If

  'Add data tables (program memory)
  IF DataTables > 0 Then
    If VBS = 1 THEN PRINT : PRINT SPC(5); Message("DataTable")
    For PD = 1 TO DataTables

      'If no data memory, move all tables to program memory
      If DataTable(PD).Used And DataTable(PD).StoreLoc = 1 And ChipEEPROM = 0 Then
        LogWarning Message("WarningNoEEPROMTable")
        DataTable(PD).StoreLoc = 0
      End If

      If DataTable(PD).Used And DataTable(PD).StoreLoc = 0 THEN
        If VBS = 1 Then Print Spc(10); DataTable(PD).Name
        DataTable(PD).Item(0) = DataTable(PD).Items

        'Is this a large data table?
        LargeTable = 0
        If DataTable(PD).Items > 255 Then
          LargeTable = -1
        End If

        'Create sub for data tables
        'Need to create a table for each byte in data
        For CurrTableByte = 0 To GetTypeSize(DataTable(PD).Type) - 1
          Table = GetByte("TABLE" + DataTable(PD).Name, CurrTableByte)   'was capitalised
          TableSub = NewSubroutine(GetByte(DataTable(PD).Name, CurrTableByte))

          SBC += 1: Subroutine(SBC) = TableSub
          CurrLine = TableSub->CodeStart




          TableSub->Compiled = -1
          TableSub->Required = -1
          TableSub->NoReturn = -1

          'Find last element in table that is non-zero
          LastNonZeroElement = -1
          For CurrElement = DataTable(PD).Items To 0 Step -1
            If ((DataTable(PD).Item(CurrElement) Shr CurrTableByte * 8) And 255) <> 0 Then
              LastNonZeroElement = CurrElement
              Exit For
            End If
          Next

          If ModePIC Then
            If LargeTable Then
              AddVar "SysStringA", "WORD", 1, Subroutine(0), "REAL", "", , -1
            Else
              AddVar "SysStringA", "BYTE", 1, Subroutine(0), "REAL", "", , -1
            End If

            If ChipFamily = 12 THEN
              CurrLine = LinkedListInsert(CurrLine, " movf SysStringA, W")
              CurrLine = LinkedListInsert(CurrLine, " addwf PCL, F")

            ElseIf ChipFamily = 14 Or ChipFamily = 15 Then

              'Check item number, return 0 if number is too big
              If LargeTable Then
                CurrLine = LinkedListInsertList(CurrLine, CompileConditions("SYSSTRINGA<" + Str(LastNonZeroElement + 1), "FALSE", ""))
              Else
                CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(LastNonZeroElement + 1))
                CurrLine = LinkedListInsert(CurrLine, " subwf SysStringA, W")
                CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS, C")
              End If
              CurrLine = LinkedListInsert(CurrLine, " retlw 0")
              'Retrieve item
              If LastNonZeroElement >= 0 Then
                CurrLine = LinkedListInsert(CurrLine, " movf SysStringA, W")
                CurrLine = LinkedListInsert(CurrLine, " addlw low " + Table)
                CurrLine = LinkedListInsert(CurrLine, " movwf SysStringA")
                If ChipFamily = 15 Then
                  CurrLine = LinkedListInsert(CurrLine, " movlw (high " + Table + ") & 127")
                Else
                  CurrLine = LinkedListInsert(CurrLine, " movlw high " + Table)
                End If
                CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS, C")
                CurrLine = LinkedListInsert(CurrLine, " addlw 1")
                If LargeTable Then
                  CurrLine = LinkedListInsert(CurrLine, " addwf SysStringA_H, W")
                End If
                CurrLine = LinkedListInsert(CurrLine, " movwf PCLATH")
                CurrLine = LinkedListInsert(CurrLine, " movf SysStringA, W")
                CurrLine = LinkedListInsert(CurrLine, " movwf PCL")
                CurrLine = LinkedListInsert(CurrLine, Table)
                GetMetaData(CurrLine)->IsLabel = -1
              End If

            ElseIf ChipFamily = 16 Then
              If LargeTable Then
                CurrLine = LinkedListInsertList(CurrLine, CompileConditions("SYSSTRINGA<" + Str(LastNonZeroElement + 1), "FALSE", ""))
              Else
                CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(LastNonZeroElement + 1))
                CurrLine = LinkedListInsert(CurrLine, " cpfslt SysStringA")
              End If
              CurrLine = LinkedListInsert(CurrLine, " retlw 0")

              If LastNonZeroElement >= 0 Then
                CurrLine = LinkedListInsert(CurrLine, " movf SysStringA, W")
                CurrLine = LinkedListInsert(CurrLine, " addlw low " + Table)
                CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRL")
                CurrLine = LinkedListInsert(CurrLine, " movlw high " + Table)
                If LargeTable Then
                  CurrLine = LinkedListInsert(CurrLine, " addwfc SysStringA_H, W")
                Else
                  CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS, C")
                  CurrLine = LinkedListInsert(CurrLine, " addlw 1")
                End If
                CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRH")
                If HighTBLPTRBytes > 2 Then
                  CurrLine = LinkedListInsert(CurrLine, " movlw upper " + Table)
                  CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS, C")
                  CurrLine = LinkedListInsert(CurrLine, " addlw 1")
                  CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRU")
                End If
                CurrLine = LinkedListInsert(CurrLine, " tblrd*")
                CurrLine = LinkedListInsert(CurrLine, " movf TABLAT, W")
                CurrLine = LinkedListInsert(CurrLine, " return")
                CurrLine = LinkedListInsert(CurrLine, Table)
                GetMetaData(CurrLine)->IsLabel = -1
                Temp = ""
                FOR SP = 0 TO LastNonZeroElement
                  ThisItem = GetByte(Str(DataTable(PD).Item(SP)), CurrTableByte)
                  If Len(Temp) + Len(ThisItem) >= 79 And Temp <> "" And (SP Mod 2) = 0 Then
                    CurrLine = LinkedListInsert(CurrLine, " db " + Temp)
                    Temp = ThisItem
                  Else
                    If Temp = "" Then
                      Temp = ThisItem
                    Else
                      Temp = Temp + "," + ThisItem
                    End If
                  End If
                NEXT
                If Temp <> "" Then CurrLine = LinkedListInsert(CurrLine, " db " + Temp)
              Else
                CurrLine = LinkedListInsert(CurrLine, " retlw 0")
              End If
            END IF

            If ChipFamily <> 16 Then
              FOR SP = 0 TO LastNonZeroElement
                CurrLine = LinkedListInsert(CurrLine, " retlw " + GetByte(Str(DataTable(PD).Item(SP)), CurrTableByte))
              NEXT
            End If

            IF AFISupport = 1  and ModePIC and ChipFamily = 16 Then
              CurrLine = LinkedListInsert(CurrLine, " ALIGN 2;X4")
            End if

          ElseIf ModeAVR Then
            'Check requested location
            If LargeTable Then
              CurrLine = LinkedListInsertList(CurrLine, CompileConditions("SYSSTRINGA<" + Str(LastNonZeroElement + 1), "FALSE", ""))
              'If SysStringA < max size, then SysByteTempX = 255, and this instruction is skipped
              'If it's the same, then SysByteTempX = 0, and this instruction is executed
              CurrLine = LinkedListInsert(CurrLine, " ret")
            Else
              CurrLine = LinkedListInsert(CurrLine, " cpi SysStringA, " + Str(LastNonZeroElement + 1))
              CurrLine = LinkedListInsert(CurrLine, " brlo PC + 3")
              CurrLine = LinkedListInsert(CurrLine, " clr SysByteTempX")
              CurrLine = LinkedListInsert(CurrLine, " ret")
            End If

            '4/1/2010: added << 2 to ldi
            CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA, low(" + Table + "<<1)")
            CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA_H, high(" + Table + "<<1)")
            CurrLine = LinkedListInsert(CurrLine, " add SysReadA, SysStringA")
            If LargeTable Then
              CurrLine = LinkedListInsert(CurrLine, " adc SysReadA_H, SysStringA_H")
            Else
              CurrLine = LinkedListInsert(CurrLine, " brcc PC + 2")
              CurrLine = LinkedListInsert(CurrLine, " inc SysReadA_H")
            End If

            If ChipFamily = 121 then
              CurrLine = LinkedListInsert(CurrLine, " sbr  SysReadA_H,1<<6") 'added 0x4000 to address PROGMEM by setting the one bit
              CurrLine = LinkedListInsert(CurrLine, " ld sysbytetempx, z") 'Value will be read, stored in R16
            Else
              CurrLine = LinkedListInsert(CurrLine, " lpm") 'Value will be read, stored in R0

            End if
            CurrLine = LinkedListInsert(CurrLine, " ret")

            CurrLine = LinkedListInsert(CurrLine, Table + ":")
            Temp = ""

            FOR SP = 0 TO LastNonZeroElement
              ThisItem = GetByte(Str(DataTable(PD).Item(SP)), CurrTableByte)
              If Len(Temp) + Len(ThisItem) >= 79 And Temp <> "" And (SP Mod 2) = 0 Then
                CurrLine = LinkedListInsert(CurrLine, " .DB " + Temp)
                Temp = ThisItem
              Else
                If Temp = "" Then
                  Temp = ThisItem
                Else
                  Temp = Temp + "," + ThisItem
                End If
              End If
            NEXT
            If Temp <> "" Then CurrLine = LinkedListInsert(CurrLine, " .DB " + Temp)

            If LargeTable Then
              AddVar "SysStringA", "WORD", 1, 0, "REAL", "", , -1
            Else
              AddVar "SysStringA", "BYTE", 1, 0, "REAL", "", , -1
            End If
            AddVar "SysReadA", "WORD", 1, 0, "REAL", "", , -1
          End If

        Next
      END If

    NEXT
  End IF

END SUB

Function CompileVarSet (SourceIn As String, Dest As String, Origin As String, InvertBitCopy As Integer = 0) As LinkedListElement Pointer

  Dim As String SType, DType, Temp, DestTemp, SourceTemp, CSource, SCastType
  Dim As String LTemp, HTemp, UTemp, ETemp, STemp, Source, ReferencedSub
  Dim As Integer CurrVarByte, LastConst, ThisConst, DestIsDouble
  Dim As Integer DestReg, DestIO, SourceReg, SourceIO, L, H, U, E, CD, DestIOAVRDx
  Dim As Integer RequiresGlitchFree, DestVarBitNo, UseIndirectBitSet
  Dim As LongInt S
  Dim As PinDirType Pointer CurrPinDir
  Dim As SysVarType Pointer SysVarBit
  Dim As SubType Pointer CurrentSub, DestSub

  Dim As LinkedListElement Pointer CurrLine, OutList
  OutList = LinkedListCreate
  CurrLine = OutList

  'Check parameters
  if Len( SourceIn ) = 0  then
      LogWarning( Message("NoSourceParam"), Origin)
  End if
  if Len( Dest ) = 0 then
      LogWarning( Message("NoDestParam"), Origin)
  End if

  'Initialise
  Source = SourceIn
  CurrentSub = Subroutine(GetSubID(Origin))
  IF INSTR(Origin, "D") <> 0 Then DestSub = Subroutine(GetDestSub(Origin)) Else DestSub = CurrentSub

  'Get types
  If Instr(Source, "[") <> 0 And InStr(Source, "]") <> 0 Then
    'Remove cast from source name
    'Show warning if it's not the first thing
    If Left(Source, 1) <> "[" Then
      LogWarning(Message("WarningCastBadPlace"), Origin)
    End If
    SCastType = Mid(Source, InStr(Source, "[") + 1)
    Source = Left(Source, InStr(Source, "[") - 1)
    Source = Source + Mid(SCastType, InStr(SCastType, "]") + 1)
    SCastType = UCase(Left(SCastType, InStr(SCastType, "]") - 1))
    If IsConst(Source) Then
      SType = "CONST"

    Else
      'Do not allow cast to increase source type - this causes problems
      SType = TypeOfVar(Source, CurrentSub)
      If CastOrder(SCastType) <= CastOrder(SType) Then
        SType = SCastType
      End If
    End If

  Else
    SType = TypeOfVar(Source, CurrentSub)
    If IsConst(Source) Then SType = "CONST"
    IF INSTR(Source, ";STRING") <> 0 THEN
      If Len(GetString(Source)) = 1 Then
        SType = "CONST"
        Source = Str(ASC(GetString(Source)))
      Else
        SType = "SCONST"
      End If
    End If
  End If

  If Instr(Dest, "[") <> 0 And InStr(Dest, "]") <> 0 Then
    'Remove cast from source name
    'Show warning if it's not the first thing
    If Left(Dest, 1) <> "[" Then
      LogWarning(Message("WarningCastBadPlace"), Origin)
    End If
    DType = Mid(Dest, InStr(Dest, "[") + 1)
    Dest = Left(Dest, InStr(Dest, "[") - 1)
    Dest = Dest + Mid(DType, InStr(DType, "]") + 1)
    DType = UCase(Left(DType, InStr(DType, "]") - 1))
    If IsConst(Dest) Then SType = "CONST"

  Else
    DType = TypeOfVar(Dest, DestSub)
  End If

  'Don't copy var to itself
  If Source = Dest Then
    Return OutList
  End If

  If ChipAVRDX Then
    If IsIORegDX(Dest) Then
    ' print dest, GetSysVarAliasName ( Dest ), IsIORegDX(Dest)
      'Map legacy megaAVR register to DX register using the Alias Addressing
      If (( compilerdebug and cAVRDXDEBUG ) = cAVRDXDEBUG )  Then
        CurrLine = LinkedListInsert(CurrLine, ";IOAVRDX #5 Dest: " + Dest)
      End if
      Dest = GetSysVarAliasName ( Dest ) 
    End If   

    If IsIORegDX(Source) Then
      'Map legacy megaAVR register to DX register using the Alias Addressing
      If (( compilerdebug and cAVRDXDEBUG ) = cAVRDXDEBUG )  Then
        CurrLine = LinkedListInsert(CurrLine, ";IOAVRDX #6 Source: " + Source)
      End if
      Source = GetSysVarAliasName ( Source ) 
    End If   
  End If

  DestIOAVRDx = IsIORegDX(Dest)
  DestReg = IsRegister(Dest) or IsIORegDX(Dest)
  DestIO = IsIOReg(Dest)
  SourceReg = IsRegister(Source)
  SourceIO = IsIOReg(Source)

  ' Correct numeric defintions
  If Stype = "CONST" and ( Dtype = "SINGLE" or Dtype = "DOUBLE" ) Then

    If CountOccur( Source, ".") > 1 Then
      Temp = Message("BadValueType")
      Replace Temp, "%value%", Source
      Replace Temp, "%type%", LCase(DType)
      Replace Temp, "%var%", Dest
      LogError Temp, Origin
    End If

    If Val(Trim(Source)) = 0 Then 
      Source = "0.0"
    ElseIf Right(Trim(Source),1) = "." Then
      'Always add 0 to nn. to give nn.0
      Source = Source + "0"  
    Else 
      If Left(Trim(Source),1) = "." Then
        'Always add 0 to . to give 0.nnn
        Source = "0" + Source
      End if

      'All add .0 when nn to give nn.0
      If Instr(Source,".") = 0 And Trim(Source) = Trim(Str(VAL(Source))) Then
        Source = Source + ".0"
      End if
    End If

    'Remove Leading 0
    Do While Left(Source,1) = "0" And Mid( Source, 2, 1) <> "."
      Source = Mid(Source, 2)
    Loop 

    'Remove trailing 0
      Do While Right(Source,1) = "0" And Mid( Source, Len(Source)-1, 1) <> "."
      Source = Mid(Source, 1, Len(Source) - 1)
    Loop 
  ElseIf SType = "BIT" and ( Dtype = "SINGLE" or Dtype = "DOUBLE" ) Then
      'check for double decimal point
      If CountOccur( Source, ".") > 1 Then
        Temp = Message("BadValueType")
        Replace Temp, "%value%", Source
        Replace Temp, "%type%", LCase(DType)
        Replace Temp, "%var%", Dest
        LogError Temp, Origin
      End If
  End if 

  If (( compilerdebug and cVAR_SET ) = cVAR_SET ) Then
    Print " 2 VAR SET       : " + Dest + "[" + DType + "] = " + Source + "[" + SType + "]"
  End If

  'Record reads and writes (for auto pin direction setting)
  If DType = "BIT" Or DType = "BYTE" Then
    'If writing to a pin/port, record it
    'Unless setting port to 0 - this is done by InitSys and should not be recorded
    CurrPinDir = GetPinDirection(Dest)
    If CurrPinDir <> 0 Then
      If Not IsConst(Source) Or MakeDec(Source) <> 0 Or DType = "BIT" Then
        CurrPinDir->WrittenTo = -1
      End If
    End If
  End If
  If SType = "BIT" Or SType = "BYTE" Then
    'If reading from a pin/port, record it
    CurrPinDir = GetPinDirection(Source)
    If CurrPinDir <> 0 Then
      CurrPinDir->ReadFrom = -1
    End If
  End If

  'Record references to subroutines (for indirect calling)
  If Left(Source, 1) = "@" Then
    'Print "Reference found:" + Source
    ReferencedSub = Mid(Source, 2)
    RequestSub(0, ReferencedSub, "")
  End If

  'Generate copy code
  'Copies between integers of same or different size
  If IsIntType(SType) And IsIntType(DType) Then
    'Print "New: "; Dest; " = "; Source
    'Don't add variables here, will result in duplicate definitions
    '(if setting VAR_H here, then VAR is defined as word later)
    'If DType = "BYTE" Then AddVar Dest, "BYTE", 1, 0, "REAL", Origin
    'If SType = "BYTE" Then AddVar Source, "BYTE", 1, 0, "REAL", Origin

    'Need to check if big constant is getting put into a small variable
    If SType = "CONST" And Not AllowOverflow Then

      ' Check that a decimal point is only assigned to double or single
      If InStr(Source, ".") <> 0 and ( UCase(DType) <> "DOUBLE" or UCase(DType) <> "SINGLE" )Then
          Temp = Message("BadValueType")
          Replace Temp, "%value%", Source
          Replace Temp, "%type%", LCase(DType)
          Replace Temp, "%var%", Dest
          LogError Temp, Origin
          Source = "0"
      End if

      If InStr(Source, "@") = 0 Then
        S = MakeDec(Source)
        If Not IsValidValue(S, DType) Then
          Temp = Message("BadValueType")
          Replace Temp, "%value%", Source
          Replace Temp, "%type%", LCase(DType)
          Replace Temp, "%var%", Dest
          LogError Temp, Origin
          Source = "0"
        End If
      End If
    End If

    'Remember last value loaded in to W/SysValueCopy
    '-1 = nothing loaded, or unknown
    LastConst = -1

    For CurrVarByte = 0 To GetTypeSize(DType) - 1

      'Copy each byte
      'Source is a constant, or source is too small
      If SType = "CONST" Or GetTypeSize(SType) <= CurrVarByte Then
        'For integer:
        'MakeTwosComp
        'Get value to load in to current byte
        If SType = "CONST" Then
          If Instr(Source, "@") <> 0 Then
            S = 1
            'Is "high" or "low" already present?
            If Left(Source, 1) <> "@" Then
              STemp = Source
              Replace STemp, "@", ""
            Else
              STemp = GetByte(Source, CurrVarByte)
            End If
            ThisConst = -1
          Else
            STemp = GetByte(Source, CurrVarByte)
            S = Val(STemp)
            ThisConst = S
          End If
        Else
          'Trying to load high byte of smaller variable
          S = 0
          STemp = "0"
          ThisConst = 0
        End If

        'Set byte
        If ModePIC Then
          If ChipFamily = 16 And S = 255 Then
            CurrLine = LinkedListInsert(CurrLine, " setf " + GetByte(Dest, CurrVarByte))
          ElseIf S > 0 Then
            If LastConst = -1 Or LastConst <> ThisConst Then
              CurrLine = LinkedListInsert(CurrLine, " movlw " + STemp)
              LastConst = ThisConst
            End If
            CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(Dest, CurrVarByte))
          Else
            CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(Dest, CurrVarByte))
          End If
        ElseIf ModeAVR Then
          If DestReg Then
            AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
            CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy," + STemp)
            If DestIOAVRDx Then
              If (( compilerdebug and cAVRDXDEBUG ) = cAVRDXDEBUG )  Then
                  CurrLine = LinkedListInsert(CurrLine, ";IOAVRDX #1")
              End If  
              CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(GetSysVarAliasName ( Dest), CurrVarByte) + ",SysValueCopy")
            Else
              ' CurrLine = LinkedListInsert(CurrLine, "; IOAVR #1")
              CurrLine = LinkedListInsert(CurrLine, " mov " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
            End If
          Else
            'Note: If writing to a register, ldi then mov must be used
            'This will be optimised to ldi only later
            'If no ldi is present, will have just a mov later
            If LastConst = -1 Or LastConst <> ThisConst Then
              CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy," + STemp)
              AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
              LastConst = ThisConst
            End If
            If DestIO Then
              CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
            Else
              CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
             End If
          End If
        End If

      'Source is a variable
      Else
        'Copy byte from variable
        If ModePIC Then
          If ChipFamily = 16 Then
            CurrLine = LinkedListInsert(CurrLine, " movff " + GetByte(Source, CurrVarByte) + "," + GetByte(Dest, CurrVarByte))
          Else
            CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(Source, CurrVarByte) + ",W")
            CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(Dest, CurrVarByte))
          End If
        ElseIf ModeAVR Then
          If DestReg Then
            If SourceReg Then
              CurrLine = LinkedListInsert(CurrLine, " mov " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
            ElseIf SourceIO Then
              CurrLine = LinkedListInsert(CurrLine, " in " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
            Else
              CurrLine = LinkedListInsert(CurrLine, " lds " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
            End If
          ElseIf DestIO Then
            If SourceReg Then
              CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
            ElseIf SourceIO Then
              CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy," + GetByte(Source, CurrVarByte))
              CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
              AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
            Else
              CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + GetByte(Source, CurrVarByte))
              CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
              AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
            End If
          Else
            If SourceReg Then
              CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
            ElseIf SourceIO Then
              CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy," + GetByte(Source, CurrVarByte))
              CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
              AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
            Else
              CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + GetByte(Source, CurrVarByte))
              CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
              AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
            End If
          End If
        End If

      End If

    Next

    'Variable copied, all done!
    Return OutList
  End If

  'Copies to or from non-integer types
  'Print "Old: "; Dest; " = "; Source,
  'Print "Source type =";  SType
  Select Case DType
  'Copy to bit
  Case "BIT":
    DestTemp = FixBit(Dest, Origin)
    If InStr(DestTemp, ".") <> 0 Then Replace DestTemp, ".", ","

    'Split dest into var and bit, get source
    Dim As String DestVarName, DestVarBit, IndDestVarName, IndDestVarBit, IndDestTemp
    If InStr(DestTemp, ",") <> 0 Then
      DestVarName = Trim(Left(DestTemp, InStr(DestTemp, ",") - 1))
      DestVarBit = Trim(Mid(DestTemp, InStr(DestTemp, ",") + 1))
    Else
      Print "Internal error in CompileVarSet bit > bit: " + DestTemp
    End If

    'Glitch free output or indirect mode needed?
    DestVarBitNo = -1
    UseIndirectBitSet = 0
    If IsConst(DestVarBit) Then
      DestVarBitNo = MakeDec(DestVarBit)
    Else
      SysVarBit = HashMapGet(SysVarBits, DestVarBit)
      If SysVarBit <> 0 Then
        DestVarBitNo = SysVarBit->Location
      Else
       'Test for indirect mode needed

       'add a test to set if the source is is an array element
        If instr( Source, "INDF" ) > 0 then
          Temp = Message("CannotUseArayBitasSource")

          LogError Temp, Origin
        End If

        If instr( Dest, "INDF" ) > 0 then
          Temp = Message("CannotUseArayBitasDestination")

          LogError Temp, Origin
        End If


        'Check the source is a bit or const - 060621#
        If SType = "BIT" Then
          'Need to use indirect bit setting
          UseIndirectBitSet = -1
        ElseIf SType = "CONST" And ( trim(Source) = "1" Or trim(Source) = "0" )  Then
          UseIndirectBitSet = -1
        Else
          Temp = Message("NotABitORAConst")
          Replace Temp, "%value%", Source
          LogError Temp, Origin
        End If
      End If
    End If
    RequiresGlitchFree = 0
    If DestVarBitNo <> -1 Then
      RequiresGlitchFree = HashMapGet(GlitchFreeOutputs, DestVarName + "." + Str(DestVarBitNo)) <> 0
    End If

    'Redirect PORTx writes to LATx
    If UseChipOutLatches And Left(DestTemp, 4) = "PORT" And Mid(DestTemp, 6, 1) = "," Then
      If HasSFR(DestVarName) Then
        DestTemp = "LAT" + Right(DestVarName, 1) + Mid(DestTemp, InStr(DestTemp, ","))
      End If
    End If

    'Record setting of individual OPTION_REG bits (PIC12x5/16x5)
    If ChipFamily = 12 Then
      If DestVarName = "OPTION_REG" And Not HasSFR(DestVarName) Then
        For CD = 1 To PinDirShadows
          If PinDirShadow(CD) = DestVarName Then GoTo OptionShadowFound
        Next
        PinDirShadows += 1
        PinDirShadow(PinDirShadows) = DestVarName
        OptionShadowFound:
      End If
    End If

    'Add var that contains bit
    IF INSTR(DestTemp, ",") <> 0 Then
      AddVar DestVarName, "BYTE", 1, DestSub, "REAL", Origin
    End If

    Select Case SType
    'bit > bit
      Case "BIT":
      SourceTemp = FixBit(Source, Origin)
      IF INSTR(SourceTemp, ".") <> 0 Then
        Temp = Left(SourceTemp, InStr(SourceTemp, ".") - 1)
        AddVar Temp, "BYTE", 1, CurrentSub, "REAL", Origin
      End If

      If ModePIC Then
        If UseIndirectBitSet Then
          IndDestTemp = DestTemp
          Replace IndDestTemp, ",", "."
          DestTemp = "SysByteTempB,0"
        End If

        'If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","
        If RequiresGlitchFree Then
          ILC += 1
          'CurrLine = LinkedListInsert(CurrLine, " btfsc " + SourceTemp)
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
          CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
          Else
            CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
          End If
          CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
          GetMetaData(CurrLine)->IsLabel = -1
        Else
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
          Else
            CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
          End If
        End If
        ILC += 1
        'CurrLine = LinkedListInsert(CurrLine, " btfss " + SourceTemp)
        CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=0", "TRUE", Origin))
        CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
        If InvertBitCopy Then
          CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
        Else
          CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
        End If
        CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
        GetMetaData(CurrLine)->IsLabel = -1

        If UseIndirectBitSet Then
          CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(IndDestTemp, "", Origin, DestSub, 0))
        End If

      ElseIf ModeAVR Then

        If UseIndirectBitSet Then
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " sbr SysByteTempB,1")
          Else
            CurrLine = LinkedListInsert(CurrLine, " cbr SysByteTempB,1")
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cbr SysByteTempB,1")
          Else
            CurrLine = LinkedListInsert(CurrLine, " sbr SysByteTempB,1")
          End If
          CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(DestVarName + "." + DestVarBit, "", Origin, DestSub, 0))
          If Not IsIOReg(DestVarName) Then AddVar DestVarName, "BYTE", 1, 0, "REAL", Origin

        ElseIf IsRegister(DestVarName) Then
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " sbr " + DestVarName + ",1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " cbr " + DestVarName + ",1<<" + DestVarBit)
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cbr " + DestVarName + ",1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " sbr " + DestVarName + ",1<<" + DestVarBit)
          End If

        ElseIf IsLowIOReg(DestVarName) Then
          If RequiresGlitchFree Then
            CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=0", "TRUE", Origin))
          End If
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " sbi " + DestVarName + "," + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " cbi " + DestVarName + "," + DestVarBit)
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cbi " + DestVarName + "," + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " sbi " + DestVarName + "," + DestVarBit)
          End If

        ElseIf LCase(DestVarName) = "sreg" Then
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " se" + LCase(Left(DestVarBit, 1)))
          Else
            CurrLine = LinkedListInsert(CurrLine, " cl" + LCase(Left(DestVarBit, 1)))
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cl" + LCase(Left(DestVarBit, 1)))
          Else
            CurrLine = LinkedListInsert(CurrLine, " se" + LCase(Left(DestVarBit, 1)))
          End If

        Else
          If ChipFamily <> 121 then
            CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + DestVarName)
          Else
            'Assumes the DestVarName address is 0 = A = 63
            CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy," + DestVarName)
          End if
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " sbr SysValueCopy,1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " cbr SysValueCopy,1<<" + DestVarBit)
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cbr SysValueCopy,1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " sbr SysValueCopy,1<<" + DestVarBit)
          End If
          If ChipFamily <> 121 then
            CurrLine = LinkedListInsert(CurrLine, " sts " + DestVarName + ",SysValueCopy")
          else
            'Assumes the DestVarName is 0 = A = 63
            CurrLine = LinkedListInsert(CurrLine, " out " + DestVarName + ",SysValueCopy")
          End if
          If Not IsIOReg(DestVarName) Then AddVar DestVarName, "BYTE", 1, 0, "REAL", Origin
          AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
        End If

      End If

    'byte > bit / word > bit / integer > bit
      Case "BYTE", "WORD", "INTEGER", "LONG":
      If ModePIC Then
        If Not RequiresGlitchFree Then
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
          Else
            CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
          End If
        End If
        ILC += 1
        CurrLine = LinkedListInsert(CurrLine, " btfss " + Source +",0")
        CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
        If InvertBitCopy Then
          CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
        Else
          CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
        End If
        CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
        GetMetaData(Currline)->IsLabel = -1
        If RequiresGlitchFree Then
          ILC += 1
          CurrLine = LinkedListInsert(CurrLine, " btfsc " + Source +",0")
          CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
          Else
            CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
          End If
          CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
          GetMetaData(Currline)->IsLabel = -1
        End If
      ElseIf ModeAVR Then
        SourceTemp = Source

        If ChipAVRDX Then        
          If IsIORegDX(DestVarName) Then
            'Map legacy megaAVR register to DX register using the Alias Addressing
            If (( compilerdebug and cAVRDXDEBUG ) = cAVRDXDEBUG )  Then
              CurrLine = LinkedListInsert(CurrLine, ";IOAVRDX #4")
            End if
            DestVarName = GetSysVarAliasName ( DestVarName ) 
          End If   
        End If
        
        If IsRegister(DestVarName) Then
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " sbr " + DestVarName + ",1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " cbr " + DestVarName + ",1<<" + DestVarBit)
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + ".0~0", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cbr " + DestVarName + ",1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " sbr " + DestVarName + ",1<<" + DestVarBit)
          End If

        ElseIf IsLowIOReg(DestVarName) Then
          If RequiresGlitchFree Then
            CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + ".0=0", "TRUE", Origin))
          End If
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " sbi " + DestVarName + "," + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " cbi " + DestVarName + "," + DestVarBit)
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + ".0~0", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cbi " + DestVarName + "," + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " sbi " + DestVarName + "," + DestVarBit)
          End If

        ElseIf LCase(DestVarName) = "sreg" Then
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " se" + LCase(Left(DestVarBit, 1)))
          Else
            CurrLine = LinkedListInsert(CurrLine, " cl" + LCase(Left(DestVarBit, 1)))
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + ".0~0", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cl" + LCase(Left(DestVarBit, 1)))
          Else
            CurrLine = LinkedListInsert(CurrLine, " se" + LCase(Left(DestVarBit, 1)))
          End If

        Else
          CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + DestVarName)
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " sbr SysValueCopy,1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " cbr SysValueCopy,1<<" + DestVarBit)
          End If
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + ".0~0", "TRUE", Origin))
          If InvertBitCopy Then
            CurrLine = LinkedListInsert(CurrLine, " cbr SysValueCopy,1<<" + DestVarBit)
          Else
            CurrLine = LinkedListInsert(CurrLine, " sbr SysValueCopy,1<<" + DestVarBit)
          End If
          CurrLine = LinkedListInsert(CurrLine, " sts " + DestVarName + ",SysValueCopy")
          If Not IsIOReg(DestVarName) Then AddVar DestVarName, "BYTE", 1, 0, "REAL", Origin
          AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
        End If
      End If

    'single > bit
    Case "SINGLE":
      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    'string > bit
    Case "STRING":
      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    'const > bit
    Case "CONST":
      If MakeDec(Source) = 0 Then
        CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(FixBit(Dest, Origin), "0", Origin))
      Else
        CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(FixBit(Dest, Origin), "1", Origin))
      End If

    'sconst > bit
    Case "SCONST":
      ' support could be added this to catch silent compilation with no ASM  
      'Temp = Message("SynErrIncorrectBitDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    End Select

  'Copy to byte
  Case "BYTE":
    AddVar Dest, "BYTE", 1, DestSub, "REAL", Origin
    Select Case SType
    'bit > byte
    Case "BIT"
      SourceTemp = FixBit(Source, Origin): If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","

      If ModePIC Then
        ILC += 1
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest)
        CurrLine = LinkedListInsert(CurrLine, " btfss " + SourceTemp)
        CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
        CurrLine = LinkedListInsert(CurrLine, " incf " + Dest + ",F")
        CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
        GetMetaData(CurrLine)->IsLabel = -1
      ElseIf ModeAVR Then
        If IsRegister(Dest) Then
          CurrLine = LinkedListInsert(CurrLine, " clr " + Dest)
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
          CurrLine = LinkedListInsert(CurrLine, " inc " + Dest)
        Else
          CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
          CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysValueCopy", Dest, Origin))
          AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
        End If
      End If

    'single > byte
    Case "SINGLE", "DOUBLE":
      DType = "LONG"
      AddVar "Sys" + SType + "Temp", SType, 1, 0, "REAL", "", , -1
      AddVar "Sys" + DType + "Temp", DType, 1, 0, "REAL", "", , -1
      RequestSub(CurrentSub, "SysConv" + SType + "To" + DType, "")
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(Source, "Sys" + SType + "Temp", Origin))
      CurrLine = LinkedListInsert(CurrLine, " call SysConv" + SType + "To" + DType)
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("Sys" + DType + "Temp", Dest, Origin))

    'string > byte
    Case "STRING":

      'Temp = Message("SynErrIncorrectByteDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    'sconst > byte
    Case "SCONST":
      ' support could be added this to catch silent compilation with no ASM  
      'Temp = Message("SynErrIncorrectByteDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    End Select

  'Copy to word or integer
  Case "WORD", "INTEGER":
    Select Case SType
    'bit > word
    Case "BIT":
      SourceTemp = FixBit(Source, Origin): If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","

      If ModePIC Then
        ILC += 1
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest)
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_H")
        CurrLine = LinkedListInsert(CurrLine, " btfss " + SourceTemp)
        CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
        CurrLine = LinkedListInsert(CurrLine, " incf " + Dest + ",F")
        CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
      ElseIf ModeAVR Then
        If IsRegister(Dest) Then
          CurrLine = LinkedListInsert(CurrLine, " clr " + Dest)
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
          CurrLine = LinkedListInsert(CurrLine, " inc " + Dest)
          CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_H")
        Else
          CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
          CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysValueCopy", Dest, Origin))
          AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
        End If
      End If

    'single > word
    Case "SINGLE", "DOUBLE":
      If DType = "WORD" Then DType = "LONG"
      AddVar "Sys" + SType + "Temp", SType, 1, 0, "REAL", "", , -1
      AddVar "Sys" + DType + "Temp", DType, 1, 0, "REAL", "", , -1
      RequestSub(CurrentSub, "SysConv" + SType + "To" + DType, "")
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(Source, "Sys" + SType + "Temp", Origin))
      CurrLine = LinkedListInsert(CurrLine, " call SysConv" + SType + "To" + DType)
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("Sys" + DType + "Temp", Dest, Origin))

    'string > word
    Case "STRING":

      'Temp = Message("SynErrIncorrectWordDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    'sconst > word
    Case "SCONST":
      ' support could be added this to catch silent compilation with no ASM  
      'Temp = Message("SynErrIncorrectWordDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    End Select

  'Copy to long
  Case "LONG":
    Select Case SType
    'bit > long
    Case "BIT":
      SourceTemp = FixBit(Source, Origin): If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","

      If ModePIC Then
        ILC += 1
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest)
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_H")
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_U")
        CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_E")
        CurrLine = LinkedListInsert(CurrLine, " btfss " + SourceTemp)
        CurrLine = LinkedListInsert(CurrLine, " goto ENDIF" + Str(ILC))
        CurrLine = LinkedListInsert(CurrLine, " incf " + Dest + ",F")
        CurrLine = LinkedListInsert(CurrLine, "ENDIF" + Str(ILC))
      ElseIf ModeAVR Then
        If IsRegister(Dest) Then
          CurrLine = LinkedListInsert(CurrLine, " clr " + Dest)
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
          CurrLine = LinkedListInsert(CurrLine, " inc " + Dest)
          CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_H")
          CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_U")
          CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_E")
        Else
          CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
          CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
          CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysValueCopy", Dest, Origin))
          AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
        End If
      End If

    'single > long
    Case "SINGLE", "DOUBLE":
      AddVar "Sys" + SType + "Temp", SType, 1, 0, "REAL", "", , -1
      AddVar "Sys" + DType + "Temp", DType, 1, 0, "REAL", "", , -1
      RequestSub(CurrentSub, "SysConv" + SType + "To" + DType, "")
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(Source, "Sys" + SType + "Temp", Origin))
      CurrLine = LinkedListInsert(CurrLine, " call SysConv" + SType + "To" + DType)
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("Sys" + DType + "Temp", Dest, Origin))

    'string > long
    Case "STRING":

      'Temp = Message("SynErrIncorrectLongDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    'sconst > long
    Case "SCONST":
      ' support could be added this to catch silent compilation with no ASM  
      'Temp = Message("SynErrIncorrectLongDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    End Select

  'Copy to floating point type (single or double)
  Case "SINGLE", "DOUBLE"
    DestIsDouble = DType = "DOUBLE"
    Select Case SType
    'bit > single
    Case "BIT":
      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    'byte/word/long/integer > single/double
    Case "BYTE", "WORD", "LONG", "INTEGER":
      'Use long code for word, byte
      If SType = "BYTE" Or SType = "WORD" Then SType = "LONG"
      'Need to convert
      AddVar "Sys" + SType + "Temp", SType, 1, 0, "REAL", "", , -1
      AddVar "Sys" + DType + "Temp", DType, 1, 0, "REAL", "", , -1
      RequestSub(CurrentSub, "SysConv" + SType + "To" + DType, "")
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(Source, "Sys" + SType + "Temp", Origin))
      CurrLine = LinkedListInsert(CurrLine, " call SysConv" + SType + "To" + DType)
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("Sys" + DType + "Temp", Dest, Origin))

    'single/double > single/double
    Case "SINGLE", "DOUBLE":
      If SType = DType Then
        For CurrVarByte = 0 To GetTypeSize(SType) - 1
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("[byte]" + GetByte(Source, CurrVarByte), "[byte]" + GetByte(Dest, CurrVarByte), Origin))
        Next
      Else
        'Need to convert
        AddVar "SysSingleTemp", "SINGLE", 1, 0, "REAL", "", , -1
        AddVar "SysDoubleTemp", "DOUBLE", 1, 0, "REAL", "", , -1
        RequestSub(CurrentSub, "SysConv" + SType + "To" + DType, "")
        CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(Source, "Sys" + SType + "Temp", Origin))
        CurrLine = LinkedListInsert(CurrLine, " call SysConv" + SType + "To" + DType)
        CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("Sys" + DType + "Temp", Dest, Origin))
      End If

    'string > single
    Case "STRING":

      'Temp = Message("SynErrIncorrectSingleDestination")
      'LogError Temp, Origin

      ' HANDLED ELSEWHERE    LogError "ErrorTemp 1", Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    'const > single/double
    Case "CONST":
      If DestIsDouble Then
        Dim TempDoubleBytes As ULongInt
        TempDoubleBytes = GetDoubleBytes(Val(Source))
        For CurrVarByte = 0 To 7
          CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(Str((TempDoubleBytes Shr (CurrVarByte * 8)) And 255), "[byte]" + GetByte(Dest, CurrVarByte), Origin))
        Next
      Else
        OutList = CompileVarSet(Str(GetSingleBytes(Val(Source))), "[long]" + Dest, Origin)
      End If

    'sconst > single
    Case "SCONST":

      ' support could be added this to catch silent compilation with no ASM  
      'Temp = Message("SynErrIncorrectSingleDestination")
      'LogError Temp, Origin

      If ModePIC Then

      ElseIf ModeAVR Then

      End If

    End Select

  'Copy to string
  Case "STRING":
    OutList = CompileString(Dest + "=" + SourceIn, Origin)

  End Select

  Return OutList
End Function

SUB CompileVars (CompSub As SubType Pointer)
  Dim As String InLine, Origin, Temp, DestVar, SourceData
  Dim As String VarType, SourceOld, ErrorTemp, OriginalDest
  Dim As Integer CD, PD, T, DisableInt, CTR, AIC, CanSkip
  Dim As PinDirType Pointer CurrPinDir
  Dim As SubType Pointer SourceSub, DestSub

  Dim As LinkedListElement Pointer CurrLine, NewCode, FindLine
  Dim As CodeSection Pointer NewSection

  CurrLine = CompSub->CodeStart->Next
  FoundCount = 0
  Do While CurrLine <> 0
    InLine = CurrLine->Value

    If INSTR(InLine, "=") <> 0 THEN
      'Don't try to do anything if = follows "dw" or "retlw"
      IF INSTR(LCase(InLine), " dw ") <> 0 THEN GOTO CompNextVar
      IF INSTR(LCase(InLine), " retlw ") <> 0 THEN GOTO CompNextVar
      IF Left(InLine, 4) = "ASM " Then GOTO CompNextVar

      'Allow overflow?
      AllowOverflow = 0
      If INSTR(LCase(InLine), "[ao]") <> 0 Then
        Replace InLine, "[ao]", ""
        AllowOverflow = 1
      End If

      'Only try to set, skip if trying to set non-existent register
      CanSkip = 0
      If INSTR(LCase(InLine), "[canskip]") <> 0 Then
        Replace InLine, "[canskip]", ""
        CanSkip = -1
      End If

      'Get origin and dest sub
      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
        CurrLine->Value = InLine
      END IF
      If Origin <> "" Then
        SourceSub = Subroutine(GetSubID(Origin))
      Else
        SourceSub = CompSub
      End If
      IF INSTR(Origin, "D") = 0 Then
        DestSub = SourceSub
      Else
        DestSub = Subroutine(GetDestSub(Origin))
      End If

      'Remove ALL spaces
      DO WHILE INSTR(InLine, " ") <> 0: Replace InLine, " ", "": LOOP

      'Get source and destination
      DestVar = Left(InLine, INSTR(InLine, "=") - 1)
      SourceData = Mid(InLine, INSTR(InLine, "=") + 1)

      'Ensure that a variable is being set
      Temp = DestVar
      IF INSTR(Temp, ".") <> 0 THEN Temp = Left(Temp, INSTR(Temp, ".") - 1)
      If IsConst(Temp) THEN
        ErrorTemp = Message("NotAVariable")
        Replace ErrorTemp, "%value%", Temp
        LogError ErrorTemp, Origin
        GoTo CompNextVar
      END IF

      'Load an array with a list of values?
      If INSTR(SourceData, ",") <> 0 And IsArray(DestVar, DestSub) Then
        CurrLine = LinkedListDelete(CurrLine)
        CurrLine = LinkedListInsertList(CurrLine, CompileWholeArray(InLine, Origin))
        GOTO CompNextVar
      END If

      'Load multiple bits?
      If InStr(DestVar, ",") <> 0 Then
        CurrLine = LinkedListDelete(CurrLine)
        CurrLine = LinkedListInsertList(CurrLine, GenerateMultiSet(SourceData, DestVar, Origin, CompSub, CanSkip))
        GoTo CompNextVar
      EndIf

      'Check if command involves setting a string
      IF IsString(DestVar, DestSub) Or INSTR(DestVar, "()") <> 0 Then
            CurrLine = LinkedListDelete(CurrLine)
            CurrLine = LinkedListInsertList(CurrLine, CompileString(InLine, Origin))
            GOTO CompNextVar
      END IF

      'Call another sub to generate code
      If IsCalc(SourceData) Then

        'Use temporary variable? (Needed for bit destinations)
        OriginalDest = ""
        If InStr(DestVar, ".") <> 0 Then
          OriginalDest = DestVar
          DestVar = GetCalcVar("BYTE")
        End If

        '@1094 add assignment check to prevent target var being corrupted when same var as target is in the calcluation
        'Delay_time_i = (Descend_start_i - Delay_time_i)
        'See AssignmentDeductionTest.gcb or AssignmentAddititionTest.gcb

        If HashMapGet(Constants, "DISABLE1094" ) = 0 then

          If ModePIC and ChipFamily=14 THEN
            'resolves Family=14 maths error

            Dim as integer sourcevariablepos, pospos, negpos, nextchar
            Dim as string ParamArray()
            sourcevariablepos=  INSTR ( SourceData, DestVar)
            If CountOccur(SourceData, "';=~<>{}*/%&|#!")  = 0 AND sourcevariablepos <> 0 AND LEFT(DestVar,3)<>"SYS" AND LEFT(DestVar,3)<>"AFR"   THEN
              If  TypeOfVar(DestVar, Subroutine(GetSubID(Origin))) <> "BYTE" THEN
                  pospos = INSTR( SourceData ,"+")
                  negpos = INSTR( SourceData ,"-")

                  nextchar = 48
                  If pospos > 1 AND CountOccur(SourceData, "+")  = 1 THEN
                      StringSplit ( SourceData, "+",-1,ParamArray() )
                      replace( ParamArray(0),"(","" )
                      replace( ParamArray(1),"(","" )
                      replace( ParamArray(0),")","" )
                      replace( ParamArray(1),")","" )
                      If NOT (DestVar = ParamArray(0) or DestVar = ParamArray(1) ) then Goto NoMatchofParameter

                      If pospos < sourcevariablepos THEN
                        nextchar = ASC(Mid ( SourceData, pospos +1,1 ))
                      End if

                      'If nextchar <> 43 or nextchar <> 45 Then nextchar = 48  'assumed not to be a perfect match but a substring
                  ElseIf negpos > 1 AND CountOccur(SourceData, "-")  = 1 THEN
                      StringSplit ( SourceData, "-",-1,ParamArray() )
                      replace( ParamArray(0),"(","" )
                      replace( ParamArray(1),"(","" )
                      replace( ParamArray(0),")","" )
                      replace( ParamArray(1),")","" )
                      If NOT ( DestVar = ParamArray(0) or DestVar = ParamArray(1) ) then Goto NoMatchofParameter

                      If negpos < sourcevariablepos THEN
                        nextchar = ASC(Mid ( SourceData, negpos +1,1 ))
                      End if
                      'If nextchar <> 43 or nextchar <> 45 Then nextchar = 48  'assumed not to be a perfect match but a substring
                  Else
                      nextchar = 48  'a numberic or complex
                  End if


                  if nextchar < 48 OR nextchar > 58  Then
                      OriginalDest = DestVar

                      If HashMapGet(Constants, "DEBUG1094" ) <> 0 then
                        LogWarning TypeOfVar(DestVar, Subroutine(GetSubID(Origin)))+" " +Destvar + " = " +SourceData, origin
                      End if
                      DestVar = GetCalcVar( TypeOfVar(DestVar, Subroutine(GetSubID(Origin))) )
                  End if
              End if
            End If
          End if
        End if
        NoMatchofParameter:


        'Add var
        If InStr(DestVar, "[") = 0 Then
          AddVar DestVar, "BYTE", 1, DestSub, "REAL", Origin
          VarType = TypeOfVar(DestVar, DestSub)
        Else
          VarType = Mid(DestVar, InStr(DestVar, "[") + 1)
          VarType = Left(VarType, InStr(VarType, "]") - 1)
        End If

        'Compile calculation
        FreeCalcVar ""
        SourceOld = SourceData
        NewSection = 0
        CompileCalc SourceData, DestVar, Origin, NewSection

        'Finish Calculation
        'If no code has been produced, set var to final value
        IF NewSection->CodeList = 0 THEN
          NewCode = CompileVarSet(SourceData, DestVar, Origin)
        Else
          NewCode = NewSection->CodeList

          CurrPinDir = GetPinDirection(DestVar)
          If CurrPinDir <> 0 Then
            CurrPinDir->WrittenTo = -1
          End If
        END IF

        If ModePIC Then
          'Get last line
          FindLine = NewCode->Next
          If FindLine <> 0 Then
            Do While FindLine->Next <> 0
              FindLine = FindLine->Next
            Loop
            'Change destination of last operation
            Temp = FindLine->Prev->Value
            IF WholeINSTR(Temp, DelType(DestVar)) = 2 AND INSTR(Temp, "wf") <> 0 And INSTR(Temp, " movwf ") = 0 And INSTR(Temp, " mulwf ") = 0 Then
              FindLine = LinkedListDelete(FindLine)
              'The test below was added to protect the incorrection replacement operation 1260
              If INSTR( FindLine->Value, ",W") Then
                Replace FindLine->Value, ",W", ",F"
              End If
            END If
          End If
        End If

        'Copy temp var to output (if needed)
        If OriginalDest <> "" Then
          'Get last line
          FindLine = NewCode->Next
          If FindLine <> 0 Then
            Do While FindLine->Next <> 0
              FindLine = FindLine->Next
            Loop

            LinkedListInsertList(FindLine, CompileVarSet(DestVar, OriginalDest, Origin))
          End If
        End If

        'Write the assembly code
        If NewCode->Next <> 0 Then
          CurrLine = LinkedListDelete(CurrLine)
          'Isolation of 1094 fix for PIC only
          If HashMapGet(Constants, "DISABLE1094" ) = 0 then
            If ModePic  and ChipFamily=14 THEN
              If PreserveMode <> 0 Then
                ' Commented out to remove clutter from ASM.. no one has complained so change 1094 must work ok
                ' CurrLine = LinkedListInsert(CurrLine, ";Using temp var method implemented at #1094. Use a constant DISABLE1094 to disable")
              End If
            End if
          End if
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
        Else
          CurrLine = LinkedListDelete(CurrLine)
        End If

      Else
        'Not a calculation, compile the variable setting operation
        'Trim brackets from SourceData
        If Left(SourceData, 1) = "(" And Right(SourceData, 1) = ")" And CountOccur(SourceData, "';()") = 2 Then
          SourceData = Mid(SourceData, 2, Len(SourceData) - 2)
        End If

        'Add variables
        AddVar SourceData, "BYTE", 1, SourceSub, "REAL", Origin
        AddVar DestVar, "BYTE", 1, DestSub, "REAL", Origin

        'Get code
        NewCode = CompileVarSet(SourceData, DestVar, Origin)

        'Add code
        If NewCode->Next <> 0 Then
          CurrLine = LinkedListDelete(CurrLine)
          CurrLine = LinkedListInsertList(CurrLine, NewCode)
        Else
          CurrLine = LinkedListDelete(CurrLine)
        End If

      End If

      FoundCount = FoundCount + 1
    END IF

  CompNextVar:
    CurrLine = CurrLine->Next
  Loop

END SUB

SUB CompileWait (CompSub As SubType Pointer)

  'Time Intervals: us, 10us, ms, 10ms, s, m, h
  Dim As String InLine, Origin, Temp, Value, Unit, Condition
  Dim As Integer UP, T, Cycles, DS, ExpandedValueLen, lUSDelaysInaccurate, overrideUSDelaysInaccurateWarning
  Dim as Integer lValueASC, LessOneCycle
  Dim As LinkedListElement Pointer CurrLine, NewCode
  Dim ExpandedValue as String
  Dim minDelay as String

  FoundCount = 0
  
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = UCase(CurrLine->Value)

    IF Left(InLine, 5) = "WAIT " or Left(InLine, 7) = "WAITL1 " THEN

      overrideUSDelaysInaccurateWarning = 0

      IF Left(InLine, 7) = "WAITL1 " Then
        Replace Inline, "WAITL1", "WAIT"
        LessOneCycle = 1
      Else
        LessOneCycle = 0
      End if

      If INSTR( Inline, "#OVERRIDEWARNING") > 0 Then
        overrideUSDelaysInaccurateWarning = -1
        Replace ( Inline, "#OVERRIDEWARNING", "" )
      End If

      Origin = ""
      IF INSTR(InLine, ";?F") <> 0 THEN
        Origin = Mid(InLine, INSTR(InLine, ";?F"))
        InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
      END IF

      Value = Mid(InLine, 6)
      IF Left(UCase(Value), 6) = "WHILE " OR Left(UCase(Value), 6) = "UNTIL " THEN
        Unit = LCase(Mid(Value, INSTR(Value, " ") + 1))
        Value = UCase(Left(Value, INSTR(Value, " ") - 1))
      Else
        Value = Trim(Value)
        FOR UP = LEN(Value) TO 1 STEP -1
          IF Mid(Value, UP, 1) = " " THEN
            Unit = LCase(Trim(Mid(Value, UP + 1)))
            Value = Trim(Left(Value, UP - 1))
            EXIT FOR
          END IF
        NEXT
      END IF

      'Time-based wait
      IF Value <> "UNTIL" AND Value <> "WHILE" THEN
        IF INSTR(Unit, "second") <> 0 THEN Replace Unit, "second", "s"
        Select Case Unit
        Case "us", "usec":
          Unit = "Delay_US"
        Case "10us", "10usec":
          Unit = "Delay_10US"
        Case "ms", "msec":
          Unit = "Delay_MS"
        Case "10ms", "10msec":
          Unit = "Delay_10MS"
        Case "s", "sec":
          Unit = "Delay_S"
        Case "m", "min":
          Unit = "Delay_M"
        Case "h", "hour":
          Unit = "Delay_H"
        Case Else:
          LogError Message("NoDelayUnits"), Origin
          CurrLine = LinkedListDelete(CurrLine)
          GoTo EndWaitCompile
        End Select

        Temp = Unit: Replace Temp, "Delay_", ""

        'Add word variables (PIC only)
        'If ModePIC Then
          If Unit = "Delay_US" Then AddVar "SysWaitTempUS", "WORD", 1, 0, "REAL", Origin,, -1
          If Unit = "Delay_MS" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin,, -1
          'Delays which depend on Delay_MS
          If Unit = "Delay_10MS" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin,, -1
          If Unit = "Delay_S" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin,, -1
          If Unit = "Delay_M" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin,, -1
          If Unit = "Delay_H" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin,, -1
        'End If

        'Check for delay of 0
        IF Trim(Value) = "0" OR VAL(Value) < 0 THEN
          CurrLine = LinkedListDelete(CurrLine)
          GOTO EndWaitCompile
        END If
        'Check for delay with floating point value
        If InStr(Value, ".") <> 0 Then
          If Trim(Value) = Trim(Str(Val(Value))) Then
            LogError Message("FloatInDelay"), Origin
            CurrLine = LinkedListDelete(CurrLine)
            GoTo EndWaitCompile
          End If
        End If
        'Patch #1041 10/10/2021
        'Check the syntax of the Value... does it have a space or other error?
        ExpandedValue = ReplaceConstantsLine(trim(Value),0)
        ExpandedValueLen = LEN(ExpandedValue)
        If InStr(ExpandedValue, " ") <> 0 Then  'The Value has spaces but this cound be a calc like 'show_timer / 4 ms'
          For UP = 1 to ExpandedValueLen
            lValueASC = ASC(MID(ExpandedValue,UP,1))
            'walk the Value looking for NON numeric values.. if non numeric then we can assume that we have numbers only and numbers can be tested
            If ( lValueASC < 47 OR  lValueASC > 57 ) and lValueASC <> 32 Then 'this is NOT a numeric constant
                Goto NoTNumericConstant
            End if
          Next
          'Test a numeric value of Value .. if Value <> Str(Val(Value)) then we have something like '10 10'... error!
          If trim(ExpandedValue) <> trim(Str(Val(ExpandedValue))) Then
              LogError Message("IncorrectWaitParameter"), Origin
              CurrLine = LinkedListDelete(CurrLine)
              GoTo EndWaitCompile
          End If
        End if
         NoTNumericConstant:
        'End of Patch #1041

        'Compile longer delays as subs
        IF Unit <> "Delay_US" OR Not IsConst(Value) Then
          CurrLine->Value = "SysWaitTemp" + Temp + "=" + Value + Origin
          CurrLine = LinkedListInsert(CurrLine, " call " + Unit)
          RequestSub(CompSub, Unit)

          'Generate error when using US delay on slow chips
          IF Unit = "Delay_US" AND gUSDelaysInaccurate  AND overrideUSDelaysInaccurateWarning = 0 THEN
            LogWarning Message("WarningUSDelay"), Origin
          END If

        'Compile microsecond delays as inline when given constant length
        Else
          If ModePIC Then
            Cycles = MakeDec(Value) * ChipMHz / 4
          ElseIf ModeAVR Then
            Cycles = MakeDec(Value) * ChipMHz
          End If

          NewCode = GenerateExactDelay(Cycles - LessOneCycle)
          CurrLine = LinkedListDelete(CurrLine)

          'add message regarding minimum delay
          If Len( Trim(NewCode->Next->Value) ) = 0 Then
            If  ChipFamily <> 16 Then
                minDelay = str(INT((1/( ChipMhz /4)+2)/2))
            Else
                minDelay = str(INT((1/( ChipMhz /4)+2)/2))
            End if
            CurrLine = LinkedListInsert(CurrLine, ";GCASM note: wait unit and value too slow for chip frequency, no ASM added. Minimum delay is " + minDelay + "us")
          End if
          CurrLine = LinkedListInsertList(CurrLine, NewCode)

        END IF

        FoundCount = FoundCount + 1

      'Condition-based Wait
      Else
        Condition = UCase(Unit)
        WSC = WSC + 1
        CurrLine->Value = "SysWaitLoop" + Str(WSC) + LabelEnd
        GetMetaData(Currline)->IsLabel = -1
        'Get code
        Temp = "TRUE": IF Value = "UNTIL" THEN Temp = "FALSE"
        NewCode = CompileConditions(Condition, Temp, Origin)
        'Add
        CurrLine = LinkedListInsertList(CurrLine, NewCode)
        'Append appropriate goto
        If Condition <> "AlwaysFalse" Then
          If ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto SysWaitLoop" + Str(WSC))
          If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp SysWaitLoop" + Str(WSC))
        End If

        FoundCount = FoundCount + 1
      END IF

    END IF

  EndWaitCompile:
    CurrLine = CurrLine->Next
  Loop

END SUB

Function CompileWholeArray (InLine As String, Origin As String) As LinkedListElement Pointer

  Dim As String DestVar, Source, Temp, TempData
  Dim As String ArrayHandler, DestArrayHandler, ArrayName, DestArrayName
  Dim StringTemp(256) As String
  Dim As Integer AF, ArrayType, TDC, PD, MaxArrayElements
  Dim As LinkedListElement Pointer OutList, CurrLine
  Dim As SubType Pointer CurrSub

  OutList = LinkedListCreate
  CurrLine = OutList

  'Get subroutine
  CurrSub = Subroutine(GetSubID(Origin))

  'Fix extra ' as end of string
  Inline = trim(InLine)
  if right(Inline,1) = "," then
      Inline = MID(Inline,1,LEN(Inline)-1)
  end if


  'Find array address, display error if not found
  DestVar = Trim(Left(InLine, INSTR(InLine, "=") - 1))
  Source = Trim(Mid(InLine, INSTR(InLine, "=") + 1))

  'Set indirect pointer
  If ModePIC Then
    If ChipFamily = 15 Or ChipFamily = 16 Then
      CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, 0, CurrSub, Origin))
    Else
      CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, -1, CurrSub, Origin))
    End If
  ElseIf ModeAVR Then
    CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, 0, CurrSub, Origin))
  End If

  'Prevent silent failure of the overrun of the StringTemp array
  If CHIPRAM < 2048 Then MaxArrayElements = 250 Else MaxArrayElements = 255

  'Get source data
  TDC = 0
  Do While INSTR(Source, ",") <> 0
    TDC += 1
    'Prevents silent failure of the overrun of the StringTemp array
    if TDC > MaxArrayElements - 1 then
        'Error for oversized array
        TempData = Message("TooManyArrayElements")
        Replace TempData, "%Array%", DestVar
        Replace TempData, "%Max%", str(MaxArrayElements)
        LogError TempData, Origin
    end if
    StringTemp(TDC) = Trim(Left(Source, INSTR(Source, ",") - 1))
    Source = Mid(Source, INSTR(Source, ",") + 1)

  Loop
  TDC += 1
  StringTemp(TDC) = Trim(Source)
  StringTemp(0) = Str(TDC)

  'Store source data
  For PD = 0 to TDC
    If ModePIC Then
      If ChipFamily = 12 Or ChipFamily = 14 Then
        'CurrLine = LinkedListInsert(CurrLine, "INDF = " + StringTemp(PD))
        CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(StringTemp(PD), "INDF", Origin))
        If PD < TDC Then CurrLine = LinkedListInsert(CurrLine, " incf FSR,F")
      ElseIf ChipFamily = 15 Then
        CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(StringTemp(PD), "INDF0", Origin))
        If PD < TDC Then CurrLine = LinkedListInsert(CurrLine, " incf FSR0L,F")
      ElseIf ChipFamily = 16 Then
        'CurrLine = LinkedListInsert(CurrLine, "POSTINC0 = " + StringTemp(PD))
        CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(StringTemp(PD), "POSTINC0", Origin))
      End If
    ElseIf ModeAVR Then
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(StringTemp(PD), "SysValueCopy", Origin))
      CurrLine = LinkedListInsert(CurrLine, " st X+, SysValueCopy")
    End If
  Next

  Return OutList
End Function

Function ConfigNameMatch(ConfigIn As String, ConfigNameIn As String) As Integer

  'Ensure Equivalent config settings are ready
  If EqConfigSettings = 0 Then
    GetEqConfig
  End If

  'Checks whether an input config setting (ie MCLR_OFF) matches a name (ie MCLR)
  Dim As String Config, ConfigName
  Config = ConfigIn
  ConfigName = ConfigNameIn
  ReplaceAll Config, "_", " "
  ReplaceAll ConfigName, "_", " "

  'If = found, get whatever is before it
  If InStr(Config, "=") <> 0 Then Config = Trim(Left(Config, InStr(Config, "=") - 1))
  If InStr(ConfigName, "=") <> 0 Then ConfigName = Trim(Left(ConfigName, InStr(ConfigName, "=") - 1))

  'If name of setting in input config, name matches
  If WholeINSTR(Config, ConfigName) = 2 Then Return -1
  If WholeINSTR(ConfigName, Config) = 2 Then Return -1

  Dim As LinkedListElement Pointer CurrItem, Look1, Look2

  'Find equivalent names
  CurrItem = EqConfigSettings->Next
  Do While CurrItem <> 0

    'Look at every item in the list
    'Compare to every other item in the list
    Look1 = CurrItem->Metadata
    Look1 = Look1->Next
    Do While Look1 <> 0
      Look2 = CurrItem->Metadata
      Look2 = Look2->Next
      Do While Look2 <> 0
        If Look1 <> Look2 Then
          'Have two different but equivalent settings, check for them
          If ConfigName = Look1->Value And WholeINSTR(Config, Look2->Value) = 2 Then Return -1

        End If
        Look2 = Look2->Next
      Loop
      Look1 = Look1->Next
    Loop

    CurrItem = CurrItem->Next
  Loop

  If ConfigName = "OSC" And InStr(Config, "OSC") <> 0 Then Return -1

  'No match found
  Return 0
End Function

Function ConfigValueMatch(ConfigIn As String, ConfigValueIn As String, MatchAny As Integer = 0) As Integer
  'MatchAny is set when checking if ConfigValueIn is any internal oscillator, not just the fastest one

  'Checks whether an input config setting (ie MCLR_OFF) matches a value (ie OFF)
  Dim As String Config, ConfigValue, CurrValue
  Dim As Integer CurrOption
  Config = ConfigIn
  ConfigValue = ConfigValueIn
  ReplaceAll Config, "_", " "
  ReplaceAll ConfigValue, "_", " "

  'If = in value, remove it and anything before it
  '(ie convert OSC = HS to HS)
  If InStr(ConfigValue, "=") <> 0 Then ConfigValue = Trim(Mid(ConfigValue, InStr(ConfigValue, ("=")) + 1))
  If InStr(Config, "=") <> 0 Then Config = Trim(Mid(Config, InStr(Config, ("=")) + 1))

  'Return -2 for best match
  If Config = ConfigValue Then Return -2


  'If value of setting in input config, value matches
  If WholeINSTR(Config, ConfigValue) = 2 Then Return -1
  If WholeINSTR(ConfigValue, Config) = 2 Then Return -1

  If Config = "DISABLED" And ConfigValue = "OFF" Then Return -1
  If Config = "ENABLED" And ConfigValue = "ON" Then Return -1

  'MCLR on some newer PICs
  If Config = "INTMCLR" And ConfigValue = "OFF" Then Return -1
  If Config = "EXTMCLR" And ConfigValue = "ON" Then Return -1

  'Internal oscillator
  If ConfigValue = "INT" Or ConfigValue = "INT IO" Then

    'Use non-clockout mode
    If WholeINSTR(Config, "CLKOUT") = 2 Then Return 0
    If WholeINSTR(Config, "IRCCLKOUT") = 2 Then Return 0

    'Possible matches
    If InStr(Config, "INTRC IO") <> 0 Then Return -1

    If InStr(Config, "INTRC OSC") <> 0 Then Return -1

    If Config = "INTRC" Then Return -1

    'If "INTOSC" found, make sure it's the option with IO
    If InStr(Config, "INTOSC") <> 0  Then
      If InStr(Config, "IO") <> 0 Then Return -1
      If MatchAny Then Return -1
      'This option doesn't have IO, return false if one with IO is found
      For CurrOption = 1 To COC
        ' Build 1285.  The Q20 chips have a CONFIG that has a substring of `IO` so added additional check to isolate  
        If InStr(ConfigOps(CurrOption).OP, "IO") <> 0 and  InStr(ConfigOps(CurrOption).OP, "VDDIO") = 0 Then
          Return 0
        End If
      Next
      Return -1
    End If

    If InStr(Config, "INTIO67") <> 0 Then Return -1
    If InStr(Config, "INTIO2") <> 0 Then Return -1
    If InStr(Config, "IRCIO67") <> 0 Then Return -1
    If InStr(Config, "IRC") <> 0 Then Return -1
    If InStr(Config, "HFINT32") <> 0 Then Return -1
    If InStr(Config, "HFINTOSC 1MHZ") <> 0 Then Return -1
    'Normally only want the highest to match, but if match any then match any internal oscillator
    If MatchAny Then
      If InStr(Config, "HFINT") <> 0 Then Return -1
      If InStr(Config, "LFINTOSC") <> 0 Then Return -1

      If InStr(Config, "LFINT") <> 0 Then Return -1

      If InStr(Config, "INTIO") <> 0 Then Return -1
    EndIf

  End If

  'No match found
  Return 0

End Function

Sub CreateCallTree

End Sub

Sub DisplayCallTree

  'Debugging sub
  'Will show all calls in program

  Dim CurrSub As Integer
  Dim CurrCall As LinkedListElement Pointer
  Dim CalledSub As SubType Pointer

  For CurrSub = 0 To SBC
    If Subroutine(CurrSub)->Required Then
      Print Subroutine(CurrSub)->Name

      CurrCall = Subroutine(CurrSub)->CallList->Next
      Do While CurrCall <> 0
        If CurrCall->MetaData <> 0 Then
          CalledSub = CurrCall->MetaData
          Print , CalledSub->Name
        End If

        CurrCall = CurrCall->Next
      Loop
      Print

    End If
  Next

End Sub

Sub ExtAssembler
  Dim As String DataSource, ErrFile, Temp
  Dim As Integer PD, Result, compiledOK

  compiledOK = -1

  AsmExe = ReplaceToolVariables(AsmExe,,, AsmTool)
  AsmParams = ReplaceToolVariables(AsmParams, "asm",, AsmTool)

  PRINT
  'Assemble program with external assembler including MPASM
  Temp = Message("MakeExternalTool")
  Replace Temp, "%tool%", Trim(Str(AsmExe))
  PRINT Temp


  IF VBS = 1 THEN PRINT SPC(5); Message("Calling") + AsmExe
  Result = Exec(AsmExe, AsmParams)
  'SHELL MakeASM

  'Check that the assembly was successful
  If Result = -1 Then
    LogError Message("AssemblerNotFound")
    compiledOK = 0

  ELSEIF OPEN(Left(OFI, Instr(OFI, ".")) + "err" For Input As #1) = 0 THEN

    DO WHILE NOT EOF(1)
      LINE INPUT #1, DataSource
      IF INSTR(UCase(DataSource), "ERROR") <> 0 THEN
        LogError "ASM: " + Trim(DataSource)
        compiledOK = 0
      END IF
    LOOP
    CLOSE
  END IF

  IF VBS = 1 AND compiledOK = -1 then
    Print "Assembled successfully using "+AsmExe
  END IF

  'Get name of hex file to download
  HFI = OFI
  For PD = Len(HFI) To 1 Step -1
    If Mid(HFI, PD, 1) = "." Then
      HFI = Left(HFI, PD) + "hex"
      Exit For
    End If
  Next

End Sub


'Call PIC-AS
Sub PICASAssembler
  Dim As String DataSource, ErrFile, Temp
  Dim As Integer PD, Result, LBracePos, RBracePos
  Dim as string ErrorArray()

  AsmExe = ReplaceToolVariables(AsmExe,,, AsmTool)

  AsmParams = ReplaceToolVariables(AsmParams, "asm",, AsmTool)

  IF VBS = 1 THEN PRINT SPC(5); Message("Calling") + AsmExe

  Result = Shell( chr(34)+AsmExe+" "+AsmParams+" 2>&1"+chr(34))

  'Check that the assembly was successful
  If Result = -1 then
    Temp = Message("PICASFailtoLaunch")
    Replace Temp, "%var%", "Failed to Launch"
    LogError Temp

  elseif OPEN(ReplaceToolVariables("%filename%", "err")  For Input As #1) = 0 THEN
    DO WHILE NOT EOF(1)
      LINE INPUT #1, DataSource

      ' A warning that is really a serious error!!
      IF INSTR(UCase(DataSource), "WARNING:") <> 0 and INSTR(UCase(DataSource), "(1343)") <> 0 THEN
        'xcAshfc.4::: warning: (1343) hexfile data at address 0x1000 (0x08) overwritten with 0xF3

        'just format the output. Oh my... a real issue to sort this out!
        if instr( DataSource, "\\" ) > 0 then
          StringSplit ( DataSource, "\\",-1,ErrorArray() )
        else
          StringSplit ( DataSource, "\",-1,ErrorArray() )
        end if

        Temp  = mid ( ErrorArray( ubound(ErrorArray)), 2 )

        Replace( Temp, ":: WARNING", ": Error" )
        LBracePos = Instr ( Temp, "(" )-1
        RBracePos = Instr ( Temp, ")" )+2

        Temp = Left( Temp, LBracePos) + mid( temp, RBracePos, 255   )

        replace ( temp, ":", " (")
        replace ( temp, ":", "): ")

        ErrorsFound = -1
        LogOutputMessage Temp+" "

      END IF



      IF INSTR(UCase(DataSource), "ERROR") <> 0 THEN

        'Source:  10_use_pwm_via_ccp_and_adc_to_control_led_brightness.S:661:: error: (876) syntax error
        'or
        'Source:   D:\Great-Cow-BASIC-Demonstration-Sources.git\trunk\GLCD_Solutions\GLCD_Simple_Demonstration_Solutions\GLCD_Simple_Demonstration_Hardware_I2C_16f18855_for_SSD1306@32.o:172:: error: (1357) fixup overflow storing 0x825 in 2 bytes at 0xF98/0x2 -> 0x7CC (D:\Great-Cow-BASIC-Demonstration-Sources.git\trunk\GLCD_Solutions\GLCD_Simple_Demonstration_Solutions\GLCD_Simple_Demonstration_Hardware_I2C_16f18855_for_SSD1306@32.o 172/0x8)
        'or
        'Source:    C:\Users\admin\AppData\Local\Temp\xcAsdns.4::: warning: (1343) hexfile data at address 0x1000 (0x20) overwritten with 0xF3

        'Target:  10_use_pwm_via_ccp_and_adc_to_control_led_brightness.S (661):  Error: syntax error



'        'just format the output. Oh my... a real issue to sort this out!
'        if instr( DataSource, "\\" ) > 0 then
'          StringSplit ( DataSource, "\\",-1,ErrorArray() )
'        else
'          StringSplit ( DataSource, "\",-1,ErrorArray() )
'        end if
'
'        Temp  = mid ( ErrorArray( ubound(ErrorArray)), 2 )
'
'        Replace( Temp, ":: e", ": E" )
'        LBracePos = Instr ( Temp, "(" )-1
'        RBracePos = Instr ( Temp, ")" )+2
'
'        Temp = Left( Temp, LBracePos) + mid( temp, RBracePos, 255   )
'        replace ( temp, ":", " (")
'        replace ( temp, ":", "): ")

        ErrorsFound = -1
        LogOutputMessage DataSource+" "

      END IF


      IF INSTR(UCase(DataSource), ucase("cannot find the path")) <> 0 THEN

        Temp = Message("PICASFailtoLaunch")
        Replace Temp, "%var%", DataSource
        LogError Temp

      END IF

    LOOP
    CLOSE

  END IF

  'Get name of hex file to download
  HFI = OFI
  For PD = Len(HFI) To 1 Step -1
    If Mid(HFI, PD, 1) = "." Then
      HFI = Left(HFI, PD) + "hex"
      Exit For
    End If
  Next

End Sub


Sub ExtractParameters(ByRef NewSubCall As SubCallType, CalledSubName As String, CallParams As String, Origin As String)

  Dim As String TrimParams, SubSig, Temp
  Dim As Integer FP, PD
  Dim As Integer CurrPos, CurrLevel
  Dim As String CurrChar

  'Clear parameter list
  With NewSubCall
    .CalledID = 0
    .Params = 0
  End With

  'Get parameters
  'Find sub calls with parameters in program
  TrimParams = CallParams
  SubSig = ""

  If CallParams = "" Then
    With NewSubCall
      .Params = 0
      .CallSig = ""
    End With
    Exit Sub

  Else
    With NewSubCall

      'Get Origin
      .Origin = Origin

      'Parse values - split at commas, except if commas are inside brackets
      CurrLevel = 0
      .Params = 1
      .Param(.Params, 1) = ""
      For CurrPos = 1 To Len(TrimParams)
        CurrChar = Mid(TrimParams, CurrPos, 1)
        If CurrChar = "(" Then
          CurrLevel += 1
        ElseIf CurrChar = ")" Then
          CurrLevel -= 1
        End If
        If CurrChar = "," And CurrLevel = 0 Then
          'Found comma that splits parameters
          .Param(.Params, 1) = Trim(.Param(.Params, 1))
          .Param(.Params, 2) = TypeOfValue(ReplaceFnNames(.Param(.Params, 1)), Subroutine(GetSubID(Origin)), -1)
          'Print .Param(.Params, 1) + " is a " + .Param(.Params, 2)
          SubSig += GetTypeLetter(.Param(.Params, 2))
          .Params += 1
          .Param(.Params, 1) = ""
        Else
          .Param(.Params, 1) = .Param(.Params, 1) + CurrChar
        End If
      Next

      'Get type of final parameter
      .Param(.Params, 1) = Trim(.Param(.Params, 1))
      .Param(.Params, 2) = TypeOfValue(ReplaceFnNames(.Param(.Params, 1)), Subroutine(GetSubID(Origin)), -1)
      'Print .Param(.Params, 1) + " is a " + .Param(.Params, 2)
      SubSig += GetTypeLetter(.Param(.Params, 2))

      'Print
      .CallSig = SubSig
    End With
  End If
End Sub

Sub FinalOptimise

  'Replace call with rcall where possible (AVR and 18F)
  OptimiseCalls

  'Perform final optimisation on code, before writing assembly
  OptimiseIF

  'Check for uncompiled lines
  FindUncompiledLines

End Sub

Sub FindAssembly (CompSub As SubType Pointer)
  Dim As String Temp, CalledSub
  Dim As Integer PD
  Dim As LinkedListElement Pointer CurrLine, LineElements, CurrElement
  Dim As AsmCommand Pointer CallPos, RCallPos, ThisCmdPos

  CallPos = IsASM("call")
  RCallPos = IsASM("rcall")

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    'If "ASM" is at start of line, remove it

    IF Left(CurrLine->Value, 4) = "ASM " THEN
      CurrLine->Value = Trim(Mid(CurrLine->Value, 5))
      'Allow use of ASM REM x for debugging
      If Left(CurrLine->Value, 10) = "SHOWDEBUG " Then
        Replace ( CurrLine->Value, "SHOWDEBUG ", "")
        CurrLine->Value = ";" + LCase(Trim(Mid(CurrLine->Value, 11)))
      Else
        CurrLine->Value = " " + LCase(Left(CurrLine->Value, LEN(Temp))) + Mid(CurrLine->Value, LEN(Temp) + 1)
      End If
      IF INSTR(CurrLine->Value, ";?") <> 0 THEN CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?") - 1))
    Else
      ThisCmdPos = IsASM(CurrLine->Value)
      If ThisCmdPos <> 0 AND Left(CurrLine->Value, 1) <> " " THEN
        Temp = Trim(ThisCmdPos->Syntax)
        IF InStr(Temp, " ") <> 0 THEN Temp = Trim(Left(Temp, INSTR(Temp, " ") - 1))

        'Find variables
        LineElements = GetElements(CurrLine->Value)
        CurrElement = LineElements->Next
        If CurrElement <> 0 Then CurrElement = CurrElement->Next 'Don't try adding command as variable
        Do While CurrElement <> 0
          RequestVariable(CurrElement->Value, CompSub)
          CurrElement = CurrElement->Next
        Loop
        LinkedListDeleteList(LineElements, 0)

        'Record calls
        If ThisCmdPos = CallPos Or ThisCmdPos = RCallPos Then
          CalledSub = Trim(Mid(CurrLine->Value, LEN(Temp) + 1))
          IF InStr(CalledSub, ";?") <> 0 THEN CalledSub = RTrim(Left(CalledSub, INSTR(CalledSub, ";?") - 1))
          RequestSub(CompSub, CalledSub, "")
        End If

        CurrLine->Value = " " + LCase(Left(CurrLine->Value, LEN(Temp))) + Mid(CurrLine->Value, LEN(Temp) + 1)

        IF InStr(CurrLine->Value, ";?") <> 0 THEN CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?") - 1))
      End If
    End If

    CurrLine = CurrLine->Next
  Loop
End Sub

Function FindPotentialBanks(CurrLine As LinkedListElement Pointer, OutList As LinkedListElement Pointer, CheckedLines As LinkedListElement Pointer) As LinkedListElement Pointer
  'Will accept a code line and an optional output list
  'Will check to see what bank line will use, and add it to the list
  'Need to keep list of lines that have been checked to avoid infinite loops

  Dim As Integer CreatedCheckedList, LineNotChecked, BankIsNew
  Dim As LinkedListElement Pointer CurrPrev, CurrPrevPos, CheckListPos, OutListPos
  Dim As ProgLineMeta Pointer CurrMeta, CheckMeta

  'Make sure output list exists
  If OutList = 0 Then
    OutList = LinkedListCreate
  End If

  'Make sure check lines list exists
  CreatedCheckedList = 0
  If CheckedLines = 0 Then
    CreatedCheckedList = -1
    CheckedLines = LinkedListCreate
  End If

  'Check to see if line requires a bank
  CurrMeta = CurrLine->MetaData
  If CurrMeta <> 0 Then

    'Check all previous lines to see if they set a bank
    'MetaData in PrevCommands points to line
    'If MetaData in PrevCommands points to 0, line is first in sub
    CurrPrevPos = CurrMeta->PrevCommands->Next
    Do While CurrPrevPos <> 0

      CurrPrev = CurrPrevPos->MetaData

      'Is first line in sub?
      If CurrPrev = 0 Then
        'If so, it will always be in bank 0
        BankIsNew = -1
        OutListPos = OutList->Next
        Do While OutListPos <> 0
          If OutListPos->NumVal = 0 Then
            BankIsNew = 0
            Exit Do
          End If
          OutListPos = OutListPos->Next
        Loop
        If BankIsNew Then
          OutListPos = LinkedListInsert(OutList, "")
          OutListPos->NumVal = 0
        End If

      Else
        'Has line been checked already?
        LineNotChecked = -1
        CheckListPos = CheckedLines->Next
        Do While CheckListPos <> 0
          If CheckListPos->MetaData = CurrPrev Then
            LineNotChecked = 0
            Exit Do
          End If
          CheckListPos = CheckListPos->Next
        Loop
        'Line not checked, so check it and record the check
        If LineNotChecked Then
          LinkedListInsert(CheckedLines, CurrPrev)

          CheckMeta = CurrPrev->MetaData
          If CheckMeta <> 0 Then
            If CheckMeta->RequiredBank <> -1 Then
              'Have found a bank, add to the bank list
              BankIsNew = -1
              OutListPos = OutList->Next
              Do While OutListPos <> 0
                If OutListPos->NumVal = CheckMeta->RequiredBank Then
                  BankIsNew = 0
                  Exit Do
                End If
                OutListPos = OutListPos->Next
              Loop
              If BankIsNew Then
                OutListPos = LinkedListInsert(OutList, "")
                OutListPos->NumVal = CheckMeta->RequiredBank
              End If

            Else
              'Check lines before current previous line
              OutList = FindPotentialBanks(CurrPrev, OutList, CheckedLines)

            End If
          End If
        End If

      End If

      CurrPrevPos = CurrPrevPos->Next
    Loop

  End If

  'Delete checked lines list if created by this sub
  If CreatedCheckedList Then
    LinkedListDelete(CheckedLines)
  End If

  Return OutList
End Function

Sub FindUncompiledLines
  'Search the compiler output listing for uncompiled lines
  'Show error where found

  Dim As LinkedListElement Pointer CurrLine
  Dim As String TempLine, Origin, temp

  CurrLine = CompilerOutput->CodeList->Next
  Do While CurrLine <> 0
    IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
      Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
      TempLine = Trim(Left(CurrLine->Value, InStr(CurrLine->Value, ";?F") - 1))
      'Recognise some block ends
      'These are typically dealt with when the start is found
      'If left in, there's a missing start somewhere!
      If TempLine = "END REPEAT" Then
        LogError Message("EndWithoutRepeat"), Origin
      ElseIf TempLine = "LOOP" Or Left(TempLine, 5) = "LOOP " Then
        LogError Message("LoopWithoutDo"), Origin
      ElseIf TempLine = "ELSE" Then
        LogError Message("ElseWithoutIf"), Origin
      ElseIf TempLine = "NEXT" Then
        LogError Message("NextWithoutFor"), Origin

      'Anything else uncompiled at this point is a syntax error or RAISECOMPILERERROR
      Else
        If instr(ucase(TempLine), "RAISECOMPILERERROR") > 0 Then
            replace TempLine, "RAISECOMPILERERROR" , ""
            tempLine = GetString ( trim(tempLine) )
            If Instr( tempLine , "%") > 0 Then
              ReplaceAll tempLine, "%", ""
              tempLine = Message ( tempLine )
            End If
            temp = mid(TempLine,1 + InStr(TempLine,chr(34)))  'search for first double quote
            temp = Mid(temp,1,InStr(temp,chr(34))-1)            'search for next double quote
            LogError temp, Origin                                    'change message to desired
        Else
          LogError Message("SynErr"), Origin
        End if
      End If
    END If



    CurrLine = CurrLine->Next
  Loop
End Sub

Function FixBit (InBit As String, Origin As String) As String
  'Fix any references to bits > 7

  Dim As String DivChar, VarName, VarType, VarBit, Temp
  Dim As Integer B
  Dim As VariableType Pointer FoundUserVar

  'Get dividing character
  DivChar = "."
  IF INSTR(InBit, ".") = 0 AND INSTR(InBit, ",") <> 0 Then DivChar = ","

  'Deal with bit variables (which are all stored in main)
  If InStr(InBit, DivChar) = 0 Then
    FoundUserVar = HashMapGet(@(Subroutine(0)->Variables), InBit)
    If FoundUserVar <> 0 Then
      InBit = UCASE(FoundUserVar->BitVarLocation)
    End If
  End If

  'Exit if not a bit
  IF INSTR(InBit, DivChar) = 0 Then Return Inbit

  VarName = Trim(Left(InBit, INSTR(InBit, DivChar) - 1))
  VarBit = Trim(Mid(InBit, INSTR(InBit, DivChar) + 1))
  VarType = TypeOfVar(VarName, Subroutine(GetSubID(Origin)))
  B = VAL(VarBit)

  'If bit < 8, return without change
  If B < 8 Then Return InBit

  'Prepare error
  Temp = Message("BadVarBit")
  Replace Temp, "%var%", VarName
  Replace Temp, "%type%", VarType
  Replace Temp, "%bit%", VarBit

  'Is bit number valid?
  If B < 8 * GetTypeSize(VarType) Then
    Return GetByte(VarName, B \ 8) + DivChar + Trim(Str(B Mod 8))
  Else
    LogError Temp, Origin
  End If

  'Return unchanged
  Return InBit

End Function

SUB FixFunctions(CompSub As SubType Pointer)

  'Fix functions that are used by DO and WAIT, so they are updated inside
  'the loops
  Dim As String SubName
  Dim As Integer T, CD
  Dim As LinkedListElement Pointer CurrLine, FindStart

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0

    If Left(CurrLine->Value, 9) = " call FN_" THEN
      IF Left(CurrLine->Next->Value, 11) = "SysWaitLoop" OR Left(CurrLine->Next->Value, 11) = "SysDoLoop_S" THEN
        SubName = Mid(CurrLine->Value, 10)
        FindStart = CurrLine->Prev
        Do While FindStart <> 0

          IF FindStart->Value = ";FNSTART," + SubName THEN

            'Maintain the IsLabel as the LinkedListDelete is moving the code
            GetMetaData(FindStart)->IsLabel = GetMetaData(CurrLine->Next)->IsLabel

            FindStart->Value = CurrLine->Next->Value
            LinkedListDelete(CurrLine->Next)
            CurrLine = FindStart->Prev
            Exit Do
          END If

          FindStart = FindStart->Prev
        Loop
      'Not inside a loop, no change needed, just delete the ;FNSTART
      Else
        SubName = Mid(CurrLine->Value, 10)
        FindStart = CurrLine->Prev
        Do While FindStart <> 0
          IF FindStart->Value = ";FNSTART," + SubName THEN
            LinkedListDelete(FindStart)
            Exit Do
          END If
          FindStart = FindStart->Prev
        Loop
      END If

    ElseIf Left(CurrLine->Value, 10) = ";BLOCKEND," THEN
      IF Left(CurrLine->Next->Value, 11) = "SysWaitLoop" OR Left(CurrLine->Next->Value, 11) = "SysDoLoop_S" THEN
        SubName = Mid(CurrLine->Value, 11)
        FindStart = CurrLine->Prev
        Do While FindStart <> 0

          IF FindStart->Value = ";BLOCKSTART," + SubName THEN
            FindStart->Value = CurrLine->Next->Value

             'Maintain the IsLabel as this routine is moving the label about
            GetMetaData(FindStart)->IsLabel = GetMetaData(CurrLine->Next)->IsLabel

            LinkedListDelete(CurrLine->Next)
            CurrLine = LinkedListDelete(CurrLine)
            Exit Do
          END IF
          FindStart = FindStart->Prev
        Loop
      END IF

    END IF
    CurrLine = CurrLine->Next
  Loop


END SUB

Sub FixPointerOps (CompSub As SubType Pointer)

  'AVR Only

  'Replace operations on the SysPointerX pseudo variable with operations
  'using ld X/st x.

  Dim As String DataSource, DataReg
  Dim As LinkedListElement Pointer CurrLine

  Dim As Integer AccessType

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    If WholeINSTR(CurrLine->Value, "SysPointerX") > 0 Then

      AccessType = 0
      If WholeINSTR(CurrLine->Value, "SysPointerX") = 2 Then
        AccessType = 1
      ElseIf WholeINSTR(CurrLine->Value, "SysPointerXInc") = 2 Then
        AccessType = 2
      End If

      If AccessType > 0 Then
        DataSource = CurrLine->Value

        Select Case LCase(Left(Trim(DataSource), 3))
          Case "lds":
          DataReg = Mid(Trim(DataSource), 4)
          DataReg = Trim(Left(DataReg, Instr(DataReg, ",") - 1))
          If AccessType = 1 Then
            CurrLine->Value = " ld " + DataReg + ",X"
          Else
            CurrLine->Value = " ld " + DataReg + ",X+"
          End If


          Case "sts":
          DataReg = Trim(Mid(DataSource, Instr(DataSource, ",") + 1))
          If AccessType = 1 Then
            CurrLine->Value = " st X," + DataReg
          Else
            CurrLine->Value = " st X+," + DataReg
          End If

          Case Else:
          'If an error message has already happened then we dont want to issue this message
          If ErrorsFound <> -1 then
            LogError "Internal error in FixPointerOps"
          End if

        End Select
      End If
    End If

    CurrLine = CurrLine->Next
  Loop

End Sub

Sub FixSinglePinSet
  'Deal with pin direction setting on 12 bit PIC
  'Need to cache W into TRISIO before tris
  'Also need to copy TRISIO into tris after bcf/bsf on TRISIO
  'Need to operate on every subroutine

  'Also need to fix OPTION in the same way

  Dim As Integer CurrSub, NextSamePort, SearchPos, FoundPos, AddedShadows
  Dim As LinkedListElement Pointer CurrLine
  Dim As String Port, NextPort, TrisPort
  Dim As SubType Pointer InitSys
  AddedShadows = 0

  '12 bit PIC only
  If ChipFamily <> 12 Then Exit Sub

  'Only needed if individual pin dir is set
  If PinDirShadows = 0 Then Exit Sub

  'Check each sub
  For CurrSub = 0 To SBC
    With *Subroutine(CurrSub)
      If .Required And .Compiled Then

        'Check each line of sub
        CurrLine = .CodeStart->Next
        Do While CurrLine <> 0

          'Find tris x
          If Left(CurrLine->Value, 6) = " tris " Then
            'Get port
            Port = Trim(Mid(CurrLine->Value, 7))

            'Get tris cache
            TrisPort = ""
            If Port = "GPIO" Then
              TrisPort = "IO"
            ElseIf Left(Port, 4) = "PORT" And Len(Port) = 5 Then
              TrisPort = Mid(Port, 5)
            End If

            'Check if shadow needed
            FoundPos = -1
            For SearchPos = 1 To PinDirShadows
              If PinDirShadow(SearchPos) = TrisPort Then
                FoundPos = SearchPos
                Exit For
              End If
            Next

            If FoundPos <> -1 Then
              'Set buffer
              LinkedListInsert(CurrLine->Prev, " movwf TRIS" + TrisPort)

              'Add caching instructions
              AddVar("TRIS" + TrisPort, "BYTE", 1, 0, "REAL", "", , -1)
            End If

          'Find option
          ElseIf Trim(CurrLine->Value) = "option" Then

            'Check if shadow needed
            FoundPos = -1
            For SearchPos = 1 To PinDirShadows
              If PinDirShadow(SearchPos) = "OPTION_REG" Then
                FoundPos = SearchPos
                Exit For
              End If
            Next

            If FoundPos <> -1 Then
              'Set buffer
              LinkedListInsert(CurrLine->Prev, " movwf OPTION_REG")

              'Add caching instructions
              AddVar("OPTION_REG", "BYTE", 1, 0, "REAL", "", , -1)
            End If

          'Find bcf/bsf OPTION_REG
          ElseIf Left(CurrLine->Value, 16) = " bsf OPTION_REG," Or Left(CurrLine->Value, 16) = " bcf OPTION_REG," Then
            'Print "Found single dir set:", CurrLine->Value

            'If there is a block of dir commands, get last one for same port
            Do
              NextSamePort = 0

              'Is there another line?
              If CurrLine->Next <> 0 Then
                'Is the next line an option setting instruction?
                If Left(CurrLine->Next->Value, 16) = " bsf OPTION_REG," Or Left(CurrLine->Next->Value, 16) = " bcf OPTION_REG," Then
                  CurrLine = CurrLine->Next
                  NextSamePort = -1
                End If
              End If
            Loop While NextSamePort

            'Add caching instructions
            AddVar("OPTION_REG", "BYTE", 1, 0, "REAL", "", , -1)
            CurrLine = LinkedListInsert(CurrLine, " movf OPTION_REG,W")
            CurrLine = LinkedListInsert(CurrLine, " option")

          'Find bcf/bsf TRISx
          ElseIf Left(CurrLine->Value, 9) = " bsf TRIS" Or Left(CurrLine->Value, 9) = " bcf TRIS" Then
            'Print "Found single dir set:", CurrLine->Value

            'Get port
            Port = Mid(CurrLine->Value, 10)
            If InStr(Port, ",") <> 0 Then Port = Left(Port, InStr(Port, ",") - 1)
            Port = Trim(Port)

            'If there is a block of dir commands, get last one for same port
            Do
              NextSamePort = 0

              'Is there another line?
              If CurrLine->Next <> 0 Then
                'Is the next line a tris setting instruction?
                If Left(CurrLine->Next->Value, 9) = " bsf TRIS" Or Left(CurrLine->Next->Value, 9) = " bcf TRIS" Then
                  'What port does it set?
                  NextPort = Mid(CurrLine->Value, 10)
                  If InStr(NextPort, ",") <> 0 Then NextPort = Left(NextPort, InStr(NextPort, ",") - 1)
                  NextPort = Trim(NextPort)
                  'Is it the same?
                  If NextPort = Port Then
                    CurrLine = CurrLine->Next
                    NextSamePort = -1
                  End If
                End If
              End If
            Loop While NextSamePort

            If Len(Port) = 1 Or Port = "IO" Then
              'Calculate port name
              If Len(Port) = 1 Then
                TrisPort = "PORT" + Port
              Else
                TrisPort = "GPIO"
              End If

              'Add caching instructions
              AddVar("TRIS" + Port, "BYTE", 1, 0, "REAL", "", , -1)
              CurrLine = LinkedListInsert(CurrLine, " movf TRIS" + Port + ",W")
              CurrLine = LinkedListInsert(CurrLine, " tris " + TrisPort)

            End If

          End If

          CurrLine = CurrLine->Next
        Loop

      End If
    End With
  Next

  'Add pin dir shadow init code to end of InitSys
  If PinDirShadows > 0 Then
    SearchPos = LocationOfSub("InitSys", "")
    If SearchPos > 0 Then
      InitSys = Subroutine(SearchPos)
      If InitSys <> 0 Then

        'Get last line of sub
        CurrLine = InitSys->CodeStart
        Do While CurrLine->Next <> 0
          CurrLine = CurrLine->Next
        Loop

        'Add buffer setting code
        For SearchPos = 1 To PinDirShadows
          If PinDirShadow(SearchPos) <> "OPTION_REG" Then
            If AddedShadows = 0 Then
              CurrLine = LinkedListInsert(CurrLine, " movlw 255")
            End If
            AddedShadows += 1
            CurrLine = LinkedListInsert(CurrLine, " movwf TRIS" + PinDirShadow(SearchPos))
          End If
        Next

      End If
    End If
  End If

End Sub

Sub FixTemporaryVariables
  'Find any temporary variables that will be overwritten by subroutines, and rename to prevent
  Dim As LinkedListElement Pointer CurrVar, CheckVar, CallerList, CallerListPos, CalledListPos
  Dim As LinkedListElement Pointer UsedVarPos, VisitList, VisitListPos, SearchPos, CheckNewPos
  Dim As LinkedListElement Pointer CurrLine
  Dim As String CurrVarName, NewVarName
  Dim As SubType Pointer CurrSub, CallingSub
  Dim As Integer CurrSubID, KeepSearching, SearchIn
  Dim As HashMap Pointer UsedTempVars
  Dim As VariableType Pointer OldVar

  'Remove duplicate names from list
  For CurrSubID = 0 To SBC
    CurrSub = Subroutine(CurrSubID)
    If CurrSub->Required Then
      CurrVar = CurrSub->TemporaryVars
      Do While CurrVar <> 0
        CurrVarName = UCase(CurrVar->Value)
        CurrVar->Value = CurrVarName
        CheckVar = CurrVar->Next
        Do While CheckVar <> 0
          If CurrVarName = UCase(CheckVar->Value) Then
            CheckVar = LinkedListDelete(CheckVar)
          End If
          CheckVar = CheckVar->Next
        Loop

        CurrVar = CurrVar->Next
      Loop
    End If
  Next

  'Search for conflicts
  'Basic algorithm:
  'Create visit list, fill with main sub and everything called from it
  'For each item on visit list
  ' - Check that it does not steal any variables from its callers
  ' - Add all subroutines called to visit list
  ' - Subroutines may be visited multiple times if needed
  VisitList = LinkedListCreate
  VisitListPos = VisitList
  VisitListPos = LinkedListInsert(VisitListPos, Subroutine(0))
  Do While VisitListPos <> 0
    CurrSub = VisitListPos->MetaData

    'Check that this sub does not interfere with callers
    'Get all subroutines that call current sub, build hash map of used temporary vars
    UsedTempVars = HashMapCreate
    CallerList = GetCalledSubs(CurrSub, , -1)
    CallerListPos = CallerList->Next
    Do While CallerListPos <> 0
      CallingSub = CallerListPos->MetaData
      CurrVar = CallingSub->TemporaryVars->Next
      Do While CurrVar <> 0
        HashMapSet(UsedTempVars, CurrVar->Value, CallingSub->Name, -1)
        CurrVar = CurrVar->Next
      Loop
      CallerListPos = CallerListPos->Next
    Loop

    'Check if any temp variables in this sub are used by callers
    CheckVar = CurrSub->TemporaryVars->Next
    Do While CheckVar <> 0
      If HashMapGetStr(UsedTempVars, CheckVar->Value) <> "" Then
        'Found problem variable
        'Decide new temp variable name
        NewVarName = CheckVar->Value
        Do
          KeepSearching = 0
          'Get name to try
          NewVarName = GetNextTempVar(NewVarName)

          'Check for conflict in this sub
          If LinkedListFind(CurrSub->TemporaryVars, NewVarName) <> 0 Then
            'Name won't work
            KeepSearching = -1
            GoTo TryNextName
          End If

          'Check for conflict with callers
          If HashMapGetStr(UsedTempVars, NewVarName) <> "" Then
            KeepSearching = -1
            GoTo TryNextName
          End If

          'If we got here, new name is valid

          TryNextName:
        Loop While KeepSearching

        'Clone variable to new name
        OldVar = HashMapGet(@(CurrSub->Variables), CheckVar->Value)
        If OldVar <> 0 Then
          'Local variable
          If OldVar->Type = "STRING" Then
            AddVar(NewVarName, "STRING*" + Str(OldVar->Size), OldVar->Size, CurrSub, OldVar->Pointer, "", OldVar->FixedLocation, -1, OldVar->Used)
          Else
            AddVar(NewVarName, OldVar->Type, OldVar->Size, CurrSub, OldVar->Pointer, "", OldVar->FixedLocation, -1, OldVar->Used)
          End If
        Else
          'Must be global
          OldVar = HashMapGet(@(Subroutine(0)->Variables), CheckVar->Value)
          If OldVar <> 0 Then
            If OldVar->Type = "STRING" Then
              AddVar(NewVarName, "STRING*" + Str(OldVar->Size), OldVar->Size, 0, OldVar->Pointer, "", OldVar->FixedLocation, -1, OldVar->Used)
            Else
              AddVar(NewVarName, OldVar->Type, OldVar->Size, 0, OldVar->Pointer, "", OldVar->FixedLocation, -1, OldVar->Used)
            End If
          End If
        End If

        'Change code to use new variable
        CurrLine = CurrSub->CodeStart->Next
        Do While CurrLine <> 0
          WholeReplace(CurrLine->Value, CheckVar->Value, NewVarName)
          CurrLine = CurrLine->Next
        Loop

        'Record use of new temporary variable
        CheckVar = LinkedListInsert(CheckVar, NewVarName)
      End If
      CheckVar = CheckVar->Next
    Loop

    'Add called subs to visit list
    CalledListPos = CurrSub->CallList->Next
    Do While CalledListPos <> 0
      'Search visit list, add if not already on list after current sub
      SearchPos = VisitListPos->Next
      If SearchPos = 0 Then
        LinkedListInsert(VisitListPos, CalledListPos->MetaData)
      Else
        Do While SearchPos <> 0
          If SearchPos->MetaData = CalledListPos->MetaData Then Exit Do
          If SearchPos->Next = 0 Then
            'Called sub is not later in visit list already, so add
            '(Unless a called sub calls this one, then we have recursion and adding will lock up compiler)
            If LinkedListFind(CallerList, CurrSub) = 0 Then
              LinkedListInsert(SearchPos, CalledListPos->MetaData)
            End If
          End If
          SearchPos = SearchPos->Next
        Loop
      End If

      CalledListPos = CalledListPos->Next
    Loop

    'Delete caller list
    HashMapDestroy(UsedTempVars)
    LinkedListDelete(CallerList, 0)

    VisitListPos = VisitListPos->Next
  Loop

End Sub

Sub FreeCalcVar (VarName As String)
  'Mark a calc var as available for reuse
  Dim As String Temp
  Dim As Integer CV, CD

  If VarName <> "" Then
    'If name specified, only mark that var

    'Exit if input is not a calc var
    IF Left(UCase(VarName), 7) <> "SYSTEMP" Then Exit Sub

    Temp = Mid(Trim(VarName), 8)
    CV = VAL(Temp)

    CalcVars(CV).Status = "A"
  Else
    'If no name, mark all
    For CD = 1 to TCVC
      CalcVars(CD).Status = "A"
    Next
  End If
End Sub

Function GenerateArrayPointerSet(DestVar As String, DestPtr As Integer, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer

  'DestPtr:
  ' 0: SysStringA on 16F, FSR0 on enhanced 16F/18F
  ' 1: SysStringB on 16F, FSR1 on enhanced 16F/18F
  '-1: FSR on 16F, FSR0 on 18F

  Dim As String ArrayHandler, Temp, ArrayName, WholePtrVar, DestPtrL, DestPtrH
  Dim As Integer ArrayType
  Dim As VariableType Pointer ArrayPtr

  'Create output list
  Dim As LinkedListElement Pointer CurrLine, OutList
  OutList = LinkedListCreate
  CurrLine = OutList

  'Get array name (no ( or $ at the end)
  ArrayName = Trim(UCase(DestVar))
  If INSTR(ArrayName, "(") <> 0 Then ArrayName = RTrim(Left(ArrayName, INSTR(ArrayName, "(") - 1))
  If INSTR(ArrayName, "$") <> 0 Then ArrayName = RTrim(Left(ArrayName, INSTR(ArrayName, "$") - 1))

  'Get array data, exit if not declared
  ArrayPtr = VarAddress(DestVar, CurrSub)
  IF ArrayPtr = 0 THEN
    Temp = Message("UndeclaredArray")
    Replace Temp, "%array%", ArrayName
    LogError Temp, Origin
    Return OutList
  END IF

  IF ArrayPtr = Cast( VariableType Pointer, INVALIDARRAYVALUE ) THEN
    Temp = Message("CannotHandleConstruction")
    Replace Temp, "%var%", DestVar
    Replace Temp, "%sub%", CurrSub->Name    
    LogError Temp, Origin
    Return OutList
  END IF

  'Find array type, get source location
  IF ArrayPtr->Pointer = "REAL" THEN
    ArrayPtr->Used = -1
    ArrayHandler = ArrayPtr->Name
    ArrayType = 0
  ElseIF ArrayPtr->Pointer = "POINTER" THEN
    ArrayHandler = "Sys" + ArrayName + "Handler"
    AddVar ArrayHandler, "WORD", 1, CurrSub, "REAL", Origin, , -1
    ArrayType = 1
  Else
    Print "Unknown array type, "; ArrayPtr->Name; " is "; ArrayPtr->Pointer
  END IF

  'Get dest pointer
  If ModePIC Then
    If ChipFamily = 15 Or ChipFamily = 16 Then
      If DestPtr = -1 Then DestPtr = 0
      DestPtrL = "FSR" + Str(DestPtr) + "L"
      DestPtrH = "FSR" + Str(DestPtr) + "H"
    Else
      If DestPtr = -1 Then
        WholePtrVar = "FSR"
      ElseIf DestPtr = 0 Then
        WholePtrVar = "SysStringA"
      ElseIf DestPtr = 1 Then
        WholePtrVar = "SysStringB"
      Else
        WholePtrVar = "SysStringC"
      End If
      AddVar WholePtrVar, "WORD", 1, 0, "REAL", Origin, , -1 'Needs to be global
      DestPtrL = GetByte(WholePtrVar, 0)
      DestPtrH = GetByte(WholePtrVar, 1)
    End If
  ElseIf ModeAVR Then
    If DestPtr = 0 Then
      WholePtrVar = "SysStringA"
    ElseIf DestPtr = 1 Then
      WholePtrVar = "SysStringB"
    Else
      WholePtrVar = "SysStringA"
    End If
    AddVar WholePtrVar, "WORD", 1, 0, "REAL", Origin, , -1 'Needs to be global
    DestPtrL = GetByte(WholePtrVar, 0)
    DestPtrH = GetByte(WholePtrVar, 1)
  ElseIf ModeZ8 Then

  End If

  If ModePIC Then
    'Warn about unsafe strings
    If ChipFamily = 12 Then
      LogWarning Message("WarningUnsafeString")
    End If

    If ChipFamily = 12 Or ChipFamily = 14 Then
      'DestPtr = -1: Set FSR directly
      If DestPtr = -1 Then
        'ArrayType = 0: Have a constant address
        If ArrayType = 0 Then
          CurrLine = LinkedListInsert(CurrLine, " movlw low " + ArrayHandler)
          CurrLine = LinkedListInsert(CurrLine, " movwf FSR")
          If HighFSR Then CurrLine = LinkedListInsert(CurrLine, " bankisel " + ArrayHandler)
        Else
          'ArrayType = 1: Have another pointer
          CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 0) + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf FSR")
          If HighFSR Then
            CurrLine = LinkedListInsert(CurrLine, " bcf STATUS, IRP")
            CurrLine = LinkedListInsert(CurrLine, " btfsc " + GetByte(ArrayHandler, 1) + ",0")
            CurrLine = LinkedListInsert(CurrLine, " bsf STATUS, IRP")
          End If
        End If
      Else
        If ArrayType = 0 Then
          CurrLine = LinkedListInsert(CurrLine, " movlw low " + ArrayHandler)
          CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
          CurrLine = LinkedListInsert(CurrLine, " movlw high " + ArrayHandler)
          CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
        Else
          CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 0) + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
          CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 1) + ",W")
          CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
        End If
      End If

    ElseIf ChipFamily = 15 Then
      If ArrayType = 0 Then
        CurrLine = LinkedListInsert(CurrLine, " movlw low " + ArrayHandler)
        CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
        CurrLine = LinkedListInsert(CurrLine, " movlw high " + ArrayHandler)
        CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
      Else
        CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 0) + ",W")
        CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
        CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 1) + ",W")
        CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
      End If

    ElseIf ChipFamily = 16 Then
      If ArrayType = 0 Then
        CurrLine = LinkedListInsert(CurrLine, " lfsr " + Str(DestPtr) + "," + ArrayHandler)
      Else
        CurrLine = LinkedListInsert(CurrLine, " movff " + GetByte(ArrayHandler, 0) + "," + DestPtrL)
        CurrLine = LinkedListInsert(CurrLine, " movff " + GetByte(ArrayHandler, 1) + "," + DestPtrH)
      End If
    End If

  ElseIf ModeAVR Then
    If ArrayType = 0 Then
      CurrLine = LinkedListInsert(CurrLine, " ldi " + DestPtrL + ",low(" + ArrayHandler + ")")
      CurrLine = LinkedListInsert(CurrLine, " ldi " + DestPtrH + ",high(" + ArrayHandler + ")")
    ElseIf ArrayType = 1 Then
      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(ArrayHandler, WholePtrVar, Origin))
    End If
  End If

  Return OutList
End Function

Function GenerateAutoPinDir As LinkedListElement Pointer
  'Generates automatic pin direction setting code
  'Returns a linked list containing the Dir commands

  Dim As LinkedListElement Pointer OutList, CurrLine, CurrPin, SearchLine
  Dim As PinDirType Pointer CurrPinDir, CurrPortDir
  Dim As String WholePortName, Temp

  'Create output list
  OutList = LinkedListCreate
  CurrLine = OutList

  'Get pin directions/
  If (( compilerdebug and cGenerateAutoPinDir ) = cGenerateAutoPinDir ) Then
    Print "16: AUTOPINDIR","Pin", "In", "Out", "Read", "Written"
  End If
  CurrPin = PinDirections->Next
  Do While CurrPin <> 0
    CurrPinDir = CurrPin->MetaData
    If (( compilerdebug and cGenerateAutoPinDir ) = cGenerateAutoPinDir ) Then
      Print ,,CurrPin->Value, CurrPinDir->SetIn, CurrPinDir->SetOut, CurrPinDir->ReadFrom, CurrPinDir->WrittenTo
    End If
    'If direction not set manually, and required direction is known, set direction
    With (*CurrPinDir)
      'Checking a pin
      If InStr(CurrPin->Value, ".") <> 0 Then

        'If pin direction hasn't been set but is obvious, set it automatically
        If Not .SetIn And Not .SetOut Then

          'Make sure no conflicting whole port direction set used
          WholePortName = Left(CurrPin->Value, InStr(CurrPin->Value, ".") - 1)
          SearchLine = PinDirections->Next
          Do While SearchLine <> 0
            If SearchLine->Value = WholePortName Then
              'Have found entire port setting
              'If pin and port setting conflict, don't set
              CurrPortDir = SearchLine->MetaData
              'If whole port dir set, don't set automatically
              If CurrPortDir->WholePort <> 0 Then GoTo SkipPinDirSet
              'If whole port set at all, don't set automatically
              If CurrPortDir->SetIn Or CurrPortDir->SetOut Then GoTo SkipPinDirSet
              'If whole port dir clashes with single bit dir, don't set
              If Not (CurrPortDir->ReadFrom Or .ReadFrom) Xor (CurrPortDir->WrittenTo Or .WrittenTo) Then
                'Pin directions not set, and cannot be guessed - show warning
                If .ReadFrom And .WrittenTo Then
                  Temp = Message("WarningNoAutoPinDir")
                  Replace Temp, "%pin%", CurrPin->Value
                  LogWarning(Temp)
                End If
                GoTo SkipPinDirSet
              End If

              'No conflict, so stop looking for conflict and set dir if needed
              Exit Do
            End If
            SearchLine = SearchLine->Next
          Loop

          'Pin is read only
          If .ReadFrom Then
            CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " IN")
          'Pin is written only
          ElseIf .WrittenTo Then
            CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " OUT")
          End If
        End If

        SkipPinDirSet:

      'Checking a port
      Else

        'If pin direction hasn't been set but is obvious, set it automatically
        If (Not .SetIn And Not .SetOut And .WholePort = 0) And (.ReadFrom Xor .WrittenTo) Then

          'Make sure no conflicting single pin direction set used
          SearchLine = PinDirections->Next
          Do While SearchLine <> 0
            If Left(SearchLine->Value, Len(CurrPin->Value)) = CurrPin->Value Then
              'Have found single setting
              'Don't set port if any bits are set
              'Port is actually pin
              CurrPortDir = SearchLine->MetaData
              'If whole port set at all, don't set automatically
              If CurrPortDir->SetIn Or CurrPortDir->SetOut Then GoTo SkipPortDirSet
              'If whole port dir clashes with single bit dir, don't set
              If Not (CurrPortDir->ReadFrom Or .ReadFrom) Xor (CurrPortDir->WrittenTo Or .WrittenTo) Then GoTo SkipPortDirSet

            End If
            SearchLine = SearchLine->Next
          Loop

          If .ReadFrom Then
            CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " IN")
          ElseIf .WrittenTo Then
            CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " OUT")
          End If
        End If

        SkipPortDirSet:
      End If

    End With

    CurrPin = CurrPin->Next
  Loop

  Return OutList
End Function

Function GenerateBitSet(BitNameIn As String, NewStatus As String, Origin As String, CurrSub As SubType Pointer = 0, SetStatus As Integer = -1) As LinkedListElement Pointer
  'Set a given bit either on or off
  'When bit is addressed by variable, can call with SetStatus = 0 (false).
  'This will prevent any setting of the bit status for SysSetBit, which must be set before the code from this routine

  Dim As String InLine, Temp, BitName
  Dim As String VarName, VarType, VarBit, Status, VarNameOld, VarBitOld
  Dim As Integer FindShadow, BitAndValue
  Dim As VariableType Pointer VarFound

  Dim As LinkedListElement Pointer OutList, CurrLine

  OutList = LinkedListCreate
  CurrLine = OutList

  'Ensure subroutine is known (or assume main)
  If CurrSub = 0 Then
    IF INSTR(Origin, "D") <> 0 Then
      CurrSub = Subroutine(GetDestSub(Origin))
    Else
      CurrSub = Subroutine(GetSubID(Origin))
    End If
  End If

  'Get Bit var and name
  BitName = BitNameIn
  'If no var, might be dealing with an SFR bit
  If InStr(BitName, ".") = 0 Then
    BitName = GetWholeSFR(BitName)
  End If
  'Should now have a var!
  If InStr(BitName, ".") <> 0 Then
    VarName = Left(BitName, InStr(BitName, ".") - 1)
    VarBit = Mid(BitName, InStr(BitName, ".") + 1)
  Else
    Return OutList
  End If
  Status = NewStatus
  VarType = TypeOfVar(VarName, CurrSub)
  VarNameOld = VarName: VarBitOld = VarBit

  'Show error if used on invalid type
  Select Case VarType
    Case "BYTE", "WORD", "INTEGER", "LONG", "ULONGINT", "LONGINT", "SINGLE", "DOUBLE":
    'Do nothing
  Case Else
    'Show error
    Temp = Message("BadCommandType")
    Replace Temp, "%command%", "SET"
    Replace Temp, "%type%", LCase(VarType)
    LogError Temp, Origin
    Return OutList
  End Select

  If VarType = "BYTE" and Instr(VarBit, "," ) > 0 Then
    'Trap things like DIR PORTB.0,1,2 OUT
    Temp = Message("InvalidDirCommand")
    Replace Temp, "%status%", Status
    LogError Temp, Origin
    Return OutList
  End If

  'Status should be 0 or 1
  If Status <> "0" And Status <> "1" And SetStatus Then
    Temp = Message("BadSetStatus")
    Replace Temp, "%status%", Status
    LogError Temp, Origin
    Return OutList
  End If

  'Is bit number fixed?
  If Not IsConst(VarBit) And Not HasSFRBit(VarBit) Then
    'Bit number isn't a number or SFR, must be referenced by variable
    'Get maximum allowed bit number
    BitAndValue = GetTypeSize(VarType) * 8 - 1

    'Variable to operate on specified by FSR (or FSR0) on PIC or SysReadA on AVR
    'Bit to operate on comes from SysByteTempX
    'New state of bit is in bit 0 of SysByteTempB
    If ChipFamily = 12 Or ChipFamily = 14 Then
      CurrLine = LinkedListInsert(CurrLine, " movlw low " + VarName)
      CurrLine = LinkedListInsert(CurrLine, " movwf FSR")
      If HighFSR Then CurrLine = LinkedListInsert(CurrLine, " bankisel " + VarName)

    ElseIf ChipFamily = 15 Then
      CurrLine = LinkedListInsert(CurrLine, " movlw low " + VarName)
      CurrLine = LinkedListInsert(CurrLine, " movwf FSR0L")
      CurrLine = LinkedListInsert(CurrLine, " movlw high " + VarName)
      CurrLine = LinkedListInsert(CurrLine, " movwf FSR0H")

    ElseIf ChipFamily = 16 Then
      CurrLine = LinkedListInsert(CurrLine, " lfsr 0," + VarName)

    End If

    If ModePIC Then
      CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(BitAndValue))
      CurrLine = LinkedListInsert(CurrLine, " andwf " + VarBit + ", W")
      CurrLine = LinkedListInsert(CurrLine, " movwf SysByteTempX")
      If SetStatus Then
        Temp = " bsf ": IF Status = "0" THEN Temp = " bcf "
        CurrLine = LinkedListInsert(CurrLine, Temp + "SysByteTempB, 0")
      End If
      CurrLine = LinkedListInsert(CurrLine, " call SysSetBit")

      RequestSub(CurrSub, "SysSetBit")
      If Not IsIOReg(VarName) Then AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin

    ElseIf ModeAVR Then
      CurrLine = LinkedListInsert(CurrLine, " ldi SysStringA, low(" + VarName + ")")
      CurrLine = LinkedListInsert(CurrLine, " ldi SysStringA_H, high(" + VarName + ")")

      CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(VarBit, "SysReadA", Origin))
      CurrLine = LinkedListInsert(CurrLine, " andi SysReadA, " + Str(BitAndValue))

      If SetStatus Then
        Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
        CurrLine = LinkedListInsert(CurrLine, Temp + "SysByteTempB, 1")
      End If
      CurrLine = LinkedListInsert(CurrLine, " rcall SysSetBit")

      RequestSub(CurrSub, "SysSetBit")
      If Not IsIOReg(VarName) Then AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin
      AddVar "SysStringA", "WORD", 1, 0, "REAL", Origin, , -1 'Needs to be global

    End If

    'Request sequential bytes
    VarFound = HashMapGet(@(CurrSub->Variables), VarName)
    If VarFound <> 0 Then
      VarFound->NeedsSequentialLoc = -1
    End If

    Return OutList
  End If

  'If bit > 7, operate on high byte
  If Val(VarBit) > 7 And Val(VarBit) < 8 * GetTypeSize(VarType) Then
    VarName = GetByte(VarName, Val(VarBit) \ 8)
    VarBit = Str(Val(VarBit) Mod 8)
  End If

  'If bit still > 7, bit is wrong
  If Val(VarBit) > 7 Then
    Temp = Message("BadVarBit")
    Replace Temp, "%var%", VarNameOld
    Replace Temp, "%type%", LCase(VarType)
    Replace Temp, "%bit", VarBitOld
    LogError Temp, Origin
    Return OutList
  End If

  If ModePIC Then
    'Redirect PORTx writes to LATx
    If UseChipOutLatches And Left(VarName, 4) = "PORT" And Len(VarName) = 5 Then
      If HasSFR(VarName) Then
        VarName = "LAT" + Right(VarName, 1)
      End If
    End If

    'Record individual setting of OPTION bits
    If ChipFamily = 12 Then
      If VarName = "OPTION_REG" And Not HasSFR(VarName) Then
        For FindShadow = 1 To PinDirShadows
          If PinDirShadow(FindShadow) = VarName Then GoTo OptionShadowFound
        Next
        PinDirShadows += 1
        PinDirShadow(PinDirShadows) = VarName
        OptionShadowFound:
      End If
    End If

    Temp = " bsf ": IF Status = "0" THEN Temp = " bcf "
    CurrLine = LinkedListInsert(CurrLine, Temp + VarName + "," + VarBit)
    If Not IsIOReg(VarName) Then AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin

  ElseIf ModeAVR Then
    IF VarName = "SREG" Then
      Temp = " se": IF Status = "0" THEN Temp = " cl"
      CurrLine = LinkedListInsert(CurrLine, Temp + LCase(Trim(VarBit)))
    ElseIf IsRegister(VarName) Then
      Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
      CurrLine = LinkedListInsert(CurrLine, Temp + " " + VarName + ",1<<" + VarBit)

    'Adapted to support chipfamily121
    'As chipfamily121 has USART registers that are NOT accessible using SBI and CBI instructions'
    'Luckily the USART registers start with U
    ElseIf IsLowIOReg(VarName) and not ( ChipFamily = 121 and Ucase(left(trim(VarName),1)) = "U" ) Then
      Temp = " sbi ": IF Status = "0" THEN Temp = " cbi "
      
      If ChipAVRDX and IsIORegDX(VarName) Then
        'AVRDX approach to set a bit
        'Map legacy AVR register to DX register using the Alias Addressing
        If (( compilerdebug and cAVRDXDEBUG ) = cAVRDXDEBUG )  Then
          CurrLine = LinkedListInsert(CurrLine, ";IOAVRDX #2")
        End if

        Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
        AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
        'Get the current value in z, copy z into SysValueCopy, then write back from Z
        CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy, " +  GetSysVarAliasName ( VarName ))
        CurrLine = LinkedListInsert(CurrLine, Temp + "SysValueCopy" + ",1<<" + VarBit)
        CurrLine = LinkedListInsert(CurrLine, " sts " + GetSysVarAliasName ( VarName ) + ", SysValueCopy")
      Else
        'Original MegaAVR approach to set a bit
        CurrLine = LinkedListInsert(CurrLine, Temp + VarName + "," + VarBit)
      End If
    ElseIf IsIOReg(VarName) Then
      Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
      CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy," + VarName)
      CurrLine = LinkedListInsert(CurrLine, Temp + "SysValueCopy,1<<" + VarBit)
      CurrLine = LinkedListInsert(CurrLine, " out " + VarName + ",SysValueCopy")
      AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1

    Else
      Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
      CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + VarName)
      CurrLine = LinkedListInsert(CurrLine, Temp + "SysValueCopy,1<<" + VarBit)
      CurrLine = LinkedListInsert(CurrLine, " sts " + VarName + ",SysValueCopy")
      AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", "", , -1
      AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin
    End If

  ElseIf ModeZ8 Then
    If Status = "0" Then
      CurrLine = LinkedListInsert(CurrLine, " orx " + VarName + ", #(1 <<" + Trim(VarBit) + ")" )
    Else
      CurrLine = LinkedListInsert(CurrLine, " andx " + VarName + ", #!(1 <<" + Trim(VarBit) + ")")
    End If

  End If

  Return OutList
End Function

Function GenerateExactDelay(ByVal Cycles As Integer) As LinkedListElement Pointer

  Dim As Integer DS ', AddDelayTemp, AddDelayTemp2
  Dim As LinkedListElement Pointer OutList, CurrPos
  OutList = LinkedListCreate
  CurrPos = OutList

  'AddDelayTemp = 0
  Do
    If ModePIC Then
      If Cycles >= 772 Then
        DS = (Cycles - 1) \ 771
        If DS > 255 Then DS = 255
        Cycles -= 1 + DS * 771
        CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP2 = " + Str(DS))
        CurrPos = LinkedListInsert(CurrPos, "DelayUSO" + Str(USDC))
        GetMetaData(CurrPos)->IsLabel = -1
        CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = 0")
        CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC))
        GetMetaData(CurrPos)->IsLabel = -1
        CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP,F")
        CurrPos = LinkedListInsert(CurrPos, " goto DelayUS" + Str(USDC))
        CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP2,F")
        CurrPos = LinkedListInsert(CurrPos, " goto DelayUSO" + Str(USDC))
        USDC += 1
      ElseIf Cycles >= 5 Then
        DS = Cycles \ 3
        IF DS > 255 THEN DS = 255
        Cycles -= (DS * 3 + 1) 'lose 1 cycle at loop end, but have 2 extra at start
        CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = " + Str(DS)) '2 cycles, movlw/movwf
        CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC))
        GetMetaData(CurrPos)->IsLabel = -1
        CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP,F")
        CurrPos = LinkedListInsert(CurrPos, " goto DelayUS" + Str(USDC))
        USDC += 1
      ElseIf Cycles >= 2 AND ChipFamily <> 16 THEN
        CurrPos = LinkedListInsert(CurrPos, " goto $+1")
        Cycles -= 2
      ElseIf Cycles >= 1 THEN
        CurrPos = LinkedListInsert(CurrPos, " nop")
        Cycles -= 1
      END If

    ElseIf ModeAVR Then
      If Cycles >= 771 Then
        'Each 1 in DS wastes 3 + 3 * 256 = 771 us
        DS = Cycles \ 771
        If DS > 255 Then DS = 255
        Cycles -= (DS * 771)
        CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP2 = " + Str(DS))
        CurrPos = LinkedListInsert(CurrPos, "DelayUSO" + Str(USDC) + LabelEnd)
        CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = 0")
        CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC) + LabelEnd)
        CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP")
        CurrPos = LinkedListInsert(CurrPos, " brne DelayUS" + Str(USDC))
        CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP2")
        CurrPos = LinkedListInsert(CurrPos, " brne DelayUSO" + Str(USDC))
        USDC += 1

      ElseIf Cycles >= 6 Then
        'This delay can do multiples of 3 cycles
        'But for 3 cycles, it's more efficient to have the rjmp then the nop
        DS = Cycles \ 3
        IF DS > 255 THEN DS = 255
        Cycles -= (DS * 3) 'lose 1 cycle at loop end, but have 1 extra at start
        CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = " + Str(DS))
        CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC) + LabelEnd)
        CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP")
        CurrPos = LinkedListInsert(CurrPos, " brne DelayUS" + Str(USDC))
        USDC += 1
      ElseIf Cycles >= 2 Then
        CurrPos = LinkedListInsert(CurrPos, " rjmp PC + 1") ' Was rjmp 0
        Cycles -= 2
      ElseIf Cycles >= 1 Then
        CurrPos = LinkedListInsert(CurrPos, " nop")
        Cycles -= 1
      End If

    Else

    End If
  Loop While Cycles > 0

  Return OutList
End Function

Function GenerateMultiSet(SourceData As String, DestVar As String, Origin As String, CurrSub As SubType Pointer, CanSkip As Integer) As LinkedListElement Pointer
  'Generate code to set multiple bits in destvar to appropriate values from sourcedata.
  'CanSkip is set if it is OK to skip non-existent bits without an error

  Dim As LinkedListElement Pointer OutList, CurrLine, OutBits, CurrOutBit
  Dim As Integer SourceValue, CurrBitNo
  Dim As String CurrBitVal, Temp

  'Prepare
  OutList = LinkedListCreate
  CurrLine = OutList

  'Source data must be a constant
  If Not IsConst(SourceData) Then
    'Error
    LogError Message("MultiSetLiteral"), Origin
    Return OutList
  Else
    SourceValue = MakeDec(SourceData)
  End If

  'Get individual output bits
  OutBits = GetElements(DestVar, ",")
  CurrBitNo = LinkedListSize(OutBits) - 1
  CurrOutBit = OutBits->Next
  Do While CurrOutBit <> 0
    'Set bit
    CurrBitVal = "0"
    If (SourceValue And 2 ^ CurrBitNo) <> 0 Then CurrBitVal = "1"
    If InStr(CurrOutBit->Value, ".") = 0 And Not CanSkip Then
      'Error, bit is invalid
      Temp = Message("InvalidBit")
      Replace Temp, "%bit%", CurrOutBit->Value
      LogError Temp, Origin
    Else
      CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(CurrOutBit->Value, CurrBitVal, Origin, CurrSub))
    End If

    CurrBitNo -= 1
    CurrOutBit = CurrOutBit->Next
  Loop

  Return OutList
End Function

Function GenerateVectorCode As LinkedListElement Pointer

  Dim As Integer VectsAdded, CurrentVect, IntLoc, PD

  'Prepare output list
  Dim As LinkedListElement Pointer OutList, CurrLine
  Dim As SubType Pointer IntSub
  OutList = LinkedListCreate
  CurrLine = OutList

  CurrLine = LinkedListInsert(CurrLine, ";Vectors")
  'Generate PIC vectors
  If ModePIC Then
    '12 and 14 bit instruction width cores
    If ChipFamily = 12 Or ChipFamily = 14 Or ChipFamily = 15 Then
      If HasSFR("INTCON") Then
        If UserCodeOnlyEnabled = 0 then
            CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader))
            CurrLine = LinkedListInsert(CurrLine, " pagesel BASPROGRAMSTART")
            CurrLine = LinkedListInsert(CurrLine, " goto BASPROGRAMSTART")
            CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader + 4))
            IntLoc = LocationOfSub("INTERRUPT", "")   'was capitalised
        Else
            CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader))
            CurrLine = LinkedListInsert(CurrLine, " pagesel " + UserDefineStartLabel)
            CurrLine = LinkedListInsert(CurrLine, " goto " + UserDefineStartLabel)
        End If

        If IntLoc = 0 Then
          If UserCodeOnlyEnabled = 0 then
              CurrLine = LinkedListInsert(CurrLine, " retfie")
          End If
        Else
          'Can't do this, must compile INTERRUPT inline
          'CurrLine = LinkedListInsert(CurrLine, " goto INTERRUPT")
          CurrLine = LinkedListInsert(CurrLine, "INTERRUPT")   'was capitalised
          GetMetaData(Currline)->IsLabel = -1
          'Inline later
        End If
        CurrLine = LinkedListInsert(CurrLine, "")
        CurrLine = LinkedListInsert(CurrLine, Star80)
        CurrLine = LinkedListInsert(CurrLine, "")
      Else
        'Do nothing, page 0 will start immediately
      End If

    '16 bit instruction width cores
    ElseIf ChipFamily = 16 Then
      If UserCodeOnlyEnabled = 0 then
          CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader))
          CurrLine = LinkedListInsert(CurrLine, " goto BASPROGRAMSTART")
          CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader + 8))
          If LocationOfSub("INTERRUPT", "") = 0 Then
            CurrLine = LinkedListInsert(CurrLine, " retfie")
          Else
            CurrLine = LinkedListInsert(CurrLine, " goto INTERRUPT")
          End If
          CurrLine = LinkedListInsert(CurrLine, "")
          CurrLine = LinkedListInsert(CurrLine, Star80)
          CurrLine = LinkedListInsert(CurrLine, "")
      else
          CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader))
          CurrLine = LinkedListInsert(CurrLine, " goto "+ UserDefineStartLabel)
          'No Interrupt handler is supported
'          CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader + 8))
'          If LocationOfSub("INTERRUPT", "") = 0 Then     'was capitalised
'            CurrLine = LinkedListInsert(CurrLine, " retfie")
'          Else
'            CurrLine = LinkedListInsert(CurrLine, " goto INTERRUPT")
'          End If
          CurrLine = LinkedListInsert(CurrLine, "")
          CurrLine = LinkedListInsert(CurrLine, Star80)
          CurrLine = LinkedListInsert(CurrLine, "")
      End if
    End If

  'AVR vectors
  ElseIf ModeAVR Then
    CurrLine = LinkedListInsert(CurrLine, ";Interrupt vectors")
    CurrLine = LinkedListInsert(CurrLine, ".ORG " + Str(Bootloader))
    If ChipFamily <> 122 then
      CurrLine = LinkedListInsert(CurrLine, " rjmp BASPROGRAMSTART ;Reset")
    Else
      If VBS = 1 then
        Print Message( "Family122OptimisationMessage" )
      End If
      CurrLine = LinkedListInsert(CurrLine, " jmp BASPROGRAMSTART ;Reset  " + DISABLEOPTIMISATION )

    End If

    'Add ON ... GOSUB code
    VectsAdded = 0
    CurrentVect = 0
    ISRC = 0
    Do While VectsAdded < IntCount
      CurrentVect += 1
      ' Print CurrentVect, IntCount, VectsAdded
      If CurrentVect > 1024 then 
          LogError "Interrupt vectors are overloading compiler - check [Interrupt] section of DAT file for duplicates"
          Return OutList
          Exit Function
      End If
      IntLoc = 0
      For PD = 1 to IntCount
        If Interrupts(PD).VectorLoc = CurrentVect Then IntLoc = PD: Exit For
      Next

      If IntLoc <> 0 Then
        With Interrupts(IntLoc)
          VectsAdded += 1
              CurrLine = LinkedListInsert(CurrLine, ".ORG " + Str(.VectorLoc + Bootloader))
              If .Handler = "" Then
                IF UserInt THEN
                  If ChipFamily <> 122 then
                      CurrLine = LinkedListInsert(CurrLine, " rjmp INTERRUPT ;" + UCase(.Vector) )
                  Else
                      CurrLine = LinkedListInsert(CurrLine, " jmp INTERRUPT ;" + UCase(.Vector) + left("                    ", 20-len(UCase(.Vector))) + DISABLEOPTIMISATION)
                  End If
                Else
                  If ChipFamily <> 122 then
                    CurrLine = LinkedListInsert(CurrLine, " reti ;" + UCase(.Vector))
                  Else
                    CurrLine = LinkedListInsert(CurrLine, " jmp 0  ;" + UCase(.Vector)  + left("                    ", 20-len(UCase(.Vector))) + DISABLEOPTIMISATION)
                  End If
                End If
              Else
                If ChipFamily <> 122 then
                  CurrLine = LinkedListInsert(CurrLine, " rjmp Int" + UCase(.Vector) + " ;" + UCase(.Vector))
                Else
                  CurrLine = LinkedListInsert(CurrLine, " jmp Int" + UCase(.Vector) + " ;" + UCase(.Vector)+ left("                    ", 20-len(UCase(.Vector))) + DISABLEOPTIMISATION)
                End if
              End If
        End With
      End If

    Loop

    CurrLine = LinkedListInsert(CurrLine, "")
    CurrLine = LinkedListInsert(CurrLine, Star80)
    CurrLine = LinkedListInsert(CurrLine, "")

  'Z8 vectors
  ElseIf ModeZ8 Then
    'Add ON ... GOSUB code
    For IntLoc = 1 To IntCount
      With Interrupts(IntLoc)
        If UCase(.Vector) = "RESET" Then
          CurrLine = LinkedListInsert(CurrLine, " VECTOR " + .Vector + " = BASPROGRAMSTART")
        Else
          If .Handler = "" Then
            IF UserInt THEN
              CurrLine = LinkedListInsert(CurrLine, " VECTOR " + .Vector + " = INTERRUPT")
            End If
          Else
            CurrLine = LinkedListInsert(CurrLine, " VECTOR " + .Vector + " = Int" + UCase(.Vector))
          End If
        End If
      End With
    Next

  End If

  Return OutList
End Function

Function GetCalcType(VT1 As String, Act As String, VT2 As String, AnswerType As String) As String
  'Decide which type a calculation returns

  'Comparision operations return byte
  If Act = "=" Then Return "BYTE"
  If Act = "~" Then Return "BYTE"
  If Act = "<" Then Return "BYTE"
  If Act = ">" Then Return "BYTE"
  If Act = "{" Then Return "BYTE"
  If Act = "}" Then Return "BYTE"

  'Negate returns at least an integer
  If Act = "-" And VT1 = "" And CastOrder(VT2) < CastOrder("INTEGER") Then Return "INTEGER"

  'Multiply, divide and remainder operations return highest of two operands
  'Also return highest of two input types if no answer type
  If Act = "*" Or Act = "/" Or Act = "%" Or AnswerType = "" Then
    If CastOrder(VT1) > CastOrder(VT2) Then Return VT1
    Return VT2
  End If

  'Other operations return answer type if known
  Return AnswerType
End Function

Function GetCalcVar (VarTypeIn As String) As String
  'Get a calc var, and return its name
  Dim As String VarType, Temp
  Dim As Integer OutVar, HighReg, SV

  'Status can be A (available), U (used) or empty

  OutVar = -1
  HighReg = 0
  VarType = UCase(Trim(VarTypeIn))
  If Mid(VarType, 2, 1) = ":" Then
    Temp = Left(VarType, 1)
    VarType = Mid(VarType, 3)
    IF ModeAVR And Temp = "H" Then HighReg = -1
  End If

  'Find old, unused var
  'Aim to achieve highest of following:
  '= Type, = area
  '> Type, = area
  '* Type, = area
  '= Type, > area
  '>= Type, > area
  '* Type, > area
  'If still none, make new

  'Correct type, correct area
  For SV = 1 to TCVC
    With CalcVars(SV)
      If .MaxType = VarType And .Status = "A" And HighReg = .High Then
        OutVar = SV
        Exit For
      End If
    End With
  Next

  'Any type that is bigger, correct area
  If OutVar = -1 Then
    For SV = 1 to TCVC
      With CalcVars(SV)
        If CastOrder(.MaxType) > CastOrder(VarType) And .Status = "A" And HighReg = .High Then
          OutVar = SV
          Exit For
        End If
      End With
    Next
  End If

  'Any type, correct area
  If OutVar = -1 Then
    For SV = 1 to TCVC
      With CalcVars(SV)
        If .Status = "A" And HighReg = .High Then
          OutVar = SV
          .MaxType = VarType 'Increase type
          Exit For
        End If
      End With
    Next
  End If

  'Correct type, higher area
  If Not HighReg Then
    If OutVar = -1 Then
      For SV = 1 to TCVC
        With CalcVars(SV)
          If .MaxType = VarType And .Status = "A" Then
            OutVar = SV
            Exit For
          End If
        End With
      Next
    End If

    'Any type that is bigger, higher area
    If OutVar = -1 Then
      For SV = 1 to TCVC
        With CalcVars(SV)
          If CastOrder(.MaxType) > CastOrder(VarType) And .Status = "A" Then
            OutVar = SV
            Exit For
          End If
        End With
      Next
    End If

    'Any type, higher area
    If OutVar = -1 Then
      For SV = 1 to TCVC
        With CalcVars(SV)
          If .Status = "A" Then
            OutVar = SV
            .MaxType = VarType 'Increase type
            Exit For
          End If
        End WIth
      Next
    End If
  End If

  'If no unused vars, make new one
  If OutVar = -1 Then
    TCVC += 1
    OutVar = TCVC
    With CalcVars(OutVar)
      .MaxType = VarType
      .High = 0
    End With
  End If

  'Mark var as used, set type and return name
  With CalcVars(OutVar)
    .Status = "U" 'Mark used
    .CurrentType = VarType 'Set type
    .CurrentValue = "" 'Clear value
    If HighReg Then
      .High = -1
    End If
  End With
  Return "SysTemp" + Str(OutVar)

End Function

Function GetCalledSubs(CurrSub As SubType Pointer, ExistingList As LinkedListElement Pointer, FindCallers As Integer) As LinkedListElement Pointer
  'If FindCallers is 0, gets a list of all subs called from CurrSub, and any subs called from those.
  'If FindCallers is -1, work in reverse - find all subs that call CurrSub, and any subs that call those.

  Dim As LinkedListElement Pointer OutList, CurrPos, SearchList, CurrCall
  Dim As Integer CallFound

  If ExistingList = 0 Then
    OutList = LinkedListCreate
  Else
    OutList = ExistingList
  End If
  CurrPos = OutList
  Do While CurrPos->Next <> 0
    CurrPos = CurrPos->Next
  Loop

  With *CurrSub
    If FindCallers Then
      CurrCall = .CallerList->Next
    Else
      CurrCall = .CallList->Next
    End If
    Do While CurrCall <> 0

      'Check to see if call is already in output list
      SearchList = OutList->Next
      CallFound = 0
      Do While SearchList <> 0
        If SearchList->MetaData = CurrCall->MetaData Then
          CallFound = -1
          Exit Do
        End If
        SearchList = SearchList->Next
      Loop
      If CallFound = 0 Then
        CurrPos = LinkedListInsert(CurrPos, CurrCall->MetaData)
        GetCalledSubs(CurrCall->MetaData, OutList, FindCallers)
        Do While CurrPos->Next <> 0
          CurrPos = CurrPos->Next
        Loop
      End If

      CurrCall = CurrCall->Next
    Loop
  End With

  Return OutList
End Function

FUNCTION GetDestSub(Origin As String) As Integer
  Dim As String Temp

  If Origin = "ALL" OR Origin = "" Then Return 0
  If InStr(Origin, "D") = 0 Then Return GetSubID(Origin)
  Return VAL(Trim(Mid(Origin, INSTR(Origin, "D") + 1)))

END FUNCTION

Sub GetEqConfig
  'Generate a list of all config names/settings that are equivalent

  Dim As LinkedListElement Pointer CurrLoc, EqSettingsLoc

  If ModePIC Then
    'EqConfigSettings is a linked list of linked lists
    'Each sub list in the list contains all equivalent settings
    EqConfigSettings = LinkedListCreate
    EqSettingsLoc = EqConfigSettings

    'MCLR/MCLRE
    CurrLoc = LinkedListCreate
    EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
    CurrLoc = LinkedListInsert(CurrLoc, "MCLR")
    CurrLoc = LinkedListInsert(CurrLoc, "MCLRE")
    CurrLoc = LinkedListInsert(CurrLoc, "EXTMCLR")

    'OSC/FOSC/EXTRC/INTRC/INTIO
    CurrLoc = LinkedListCreate
    EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
    CurrLoc = LinkedListInsert(CurrLoc, "OSC")
    CurrLoc = LinkedListInsert(CurrLoc, "FOSC")
    CurrLoc = LinkedListInsert(CurrLoc, "EXTRC")
    CurrLoc = LinkedListInsert(CurrLoc, "INTRC")
    CurrLoc = LinkedListInsert(CurrLoc, "INTIO")
    CurrLoc = LinkedListInsert(CurrLoc, "RSTOSC")

    'BODEN/BOREN/BOD/BOR
    CurrLoc = LinkedListCreate
    EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
    CurrLoc = LinkedListInsert(CurrLoc, "BODEN")
    CurrLoc = LinkedListInsert(CurrLoc, "BOREN")
    CurrLoc = LinkedListInsert(CurrLoc, "BOD")
    CurrLoc = LinkedListInsert(CurrLoc, "BOR")

    'WDT/WDTEN
    CurrLoc = LinkedListCreate
    EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
    CurrLoc = LinkedListInsert(CurrLoc, "WDT")
    CurrLoc = LinkedListInsert(CurrLoc, "WDTE")
    CurrLoc = LinkedListInsert(CurrLoc, "WDTEN")

    'PWRT/PWRTE
    CurrLoc = LinkedListCreate
    EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
    CurrLoc = LinkedListInsert(CurrLoc, "PWRT")
    CurrLoc = LinkedListInsert(CurrLoc, "PWRTE")

    'CCP1/CCPMX
    CurrLoc = LinkedListCreate
    EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
    CurrLoc = LinkedListInsert(CurrLoc, "CCP1")
    CurrLoc = LinkedListInsert(CurrLoc, "CCPMX")

  End If

End Sub

Function GetLabelList(CompSub As SubType Pointer) As LinkedListElement Pointer

  'Search a subroutine, return a list of all labels in it

  Dim As LinkedListElement Pointer LabelList, LabelListPos, CurrLine

  LabelList = LinkedListCreate
  LabelListPos = LabelList

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    If IsASM(CurrLine->Value, 1) = 0 Then
      If LabelEnd <> "" Then
        If Right(CurrLine->Value, 1) = LabelEnd Then
          LabelListPos = LinkedListInsert(LabelListPos, UCase(Left(CurrLine->Value, Len(CurrLine->Value) - 1)))
          LabelListPos->MetaData = CurrLine
        End If
      Else
        If CurrLine->Value <> "" And Left(CurrLine->Value, 1) <> ";" And Left(CurrLine->Value, 1) <> " " And Left(CurrLine->Value, 9) <> "PRESERVE " Then
          LabelListPos = LinkedListInsert(LabelListPos, UCase(CurrLine->Value))
          LabelListPos->MetaData = CurrLine
        End If
      End If
    End If
    CurrLine = CurrLine->Next
  Loop

  Return LabelList
End Function

Function GetLinearLoc(Location As Integer) As Integer
  'On 16F1 chips, convert a non-linear address to a linear address
  'Mapping: 0x20 to 0x6F > 0x2000 to 0x204F
  '         0xA0 to 0xEF > 0x2050 to 0x209F
  '         0x120 to 0x16F > 0x20A0 to 0x20EF

  If ChipFamily <> 15 Then Return Location
  Dim As Integer Bank, BankLoc
  'Get bank and location in bank
  Bank = Location \ 128
  BankLoc = Location And 127
  'If location isn't available in linear memory, return unchanged
  If BankLoc < 32 Then Return Location
  If BankLoc > 111 Then Return Location

  Return &H2000 + Bank * 80 + (BankLoc - 32)
End Function

Function GetNonLinearLoc(Location As Integer) As Integer
  'On 16F1 chips, convert a linear address to a non-linear address
  'Mapping: 0x20 to 0x6F < 0x2000 to 0x204F
  '         0xA0 to 0xEF < 0x2050 to 0x209F
  '         0x120 to 0x16F < 0x20A0 to 0x20EF

  'If not 16F1, or not linear location, return unchanged
  If ChipFamily <> 15 Then Return Location
  If Location >= &H8000 Then Return Location
  If Location < &H2000 Then Return Location

  Dim As Integer TempLoc, Bank, BankLoc

  'Get bank and location in bank
  TempLoc = Location - &H2000
  Bank = TempLoc \ 80
  BankLoc = (TempLoc - 80 * Bank) + 32

  Return Bank * 128 + BankLoc
End Function

Function GetMetaData(CurrLine As LinkedListElement Pointer) As ProgLineMeta Pointer
  'Get the ProgLineMeta object for the current line
  'If none exists, create one

  'If CurrLine is null pointer, return null pointer
  If CurrLine = 0 Then Return 0

  'Check for metadata, create if needed
  If CurrLine->MetaData = 0 Then
    CurrLine->MetaData = NewProgLineMeta
  End If

  Return CurrLine->MetaData

End Function

Function GetPinDirection(PinNameIn As String) As PinDirType Pointer
  'Get the PinDirections list element metadata for the pin
  'Will add the pin to the list if no data found for it

  Dim As String PinName, CheckPin, PinDirVar, PinBit
  Dim As LinkedListElement Pointer CurrItem, PinListItem
  Dim As PinDirType Pointer PinDirItem
  Dim As SysVarType Pointer PinBitVar

  PinName = UCase(Trim(PinNameIn))

  'Check that pin is a valid I/O pin
  'Return 0 if it isn't
  'Should be in form {PORT | GPIO | LAT | PIN}[A|B|C|D|E]
  'Can also accept an entire port
  If InStr(PinName, ".") = 0 Then
    CheckPin = PinName
  Else
    CheckPin = Left(PinName, InStr(PinName, ".") - 1)
  End If

  'If CheckPin is not PORTx Or PORTxx,
  If Left(CheckPin, 4) <> "PORT" Or (Len(CheckPin) <> 5 And Len(CheckPin) <> 6) Then
    'and not GPIO,
    If CheckPin <> "GPIO" Then
      'and not LATx,
      If Left(CheckPin, 3) <> "LAT" Or Len(CheckPin) <> 4 Then
        'and not PINx,
        If Left(CheckPin, 3) <> "PIN" Or Len(CheckPin) <> 4 Then
          'then CheckPin is not a valid pin
          Return 0
        End If
      End If
    End If
  End If
  'Not IO, not a pin
  If GetSysVar(CheckPin) = 0 Then Return 0

  'Check that direction of pin can be set
  'Get the variable that sets port direction
  PinDirVar = CheckPin
  Replace PinDirVar, "PORT", ""
  Replace PinDirVar, "GP", ""
  Replace PinDirVar, "LAT", ""
  Replace PinDirVar, "PIN", ""
  If ModePIC Then
    PinDirVar = "TRIS" + PinDirVar
  ElseIf ModeAVR Then
    PinDirVar = "DDR" + PinDirVar
  End If
  'TRISIO will not be IO on 12 bit PIC, but will be used as direction buffer
  If ChipFamily <> 12 And GetSysVar(PinDirVar) = 0 Then Return 0

  'If pin name starts with LAT or PIN, replace with PORT
  If Left(PinName, 3) = "LAT" Then PinName = "PORT" + Mid(PinName, 4)
  If Left(PinName, 3) = "PIN" Then PinName = "PORT" + Mid(PinName, 4)

  If InStr(PinName, ".") <> 0 Then
    PinBit = Mid(PinName, InStr(PinName, ".") + 1)
    If Not IsConst(PinBit) Then
      PinBitVar = HashMapGet(SysVarBits, PinBit)
      If PinBitVar <> 0 Then
        PinName = CheckPin + "." + Str(PinBitVar->Location)
      Else
        'Bit is unknown, cannot set
        Return 0
      End If
    End If
  End If

  'Check for pin in existing pin direction list
  PinListItem = 0
  CurrItem = PinDirections->Next
  Do While CurrItem <> 0
    If CurrItem->Value = PinName Then
      PinListItem = CurrItem
    End If
    CurrItem = CurrItem->Next
  Loop

  'If pin isn't in list, add it
  If PinListItem = 0 Then
    PinListItem = LinkedListInsert(PinDirections, PinName)
  End If

  'Get pin direction data
  PinDirItem = PinListItem->MetaData
  'If no direction data exists, create it
  If PinDirItem = 0 Then
    PinDirItem = Callocate(SizeOf(PinDirType))
    PinListItem->MetaData = PinDirItem
  End If

  'Return direction data
  Return PinDirItem

End Function

Function GetRealIOName(InName As String) As String
  'Used on AVR to improve compatibility with existing GCBASIC programs
  'Typically called when generating pin reading code
  'Changes PORTx to PINx, and LATx to PORTx, making AVR and PIC18 compatible

  'AVR only
  If Not ModeAVR Then Return InName

  Dim As String OutName
  'Do replacements
  If Left(UCase(InName), 4) = "PORT" And Len(InName) = 5 Then
    OutName = "PIN" + Mid(InName, 5)
  ElseIf Left(UCase(InName), 3) = "LAT" And Len(InName) = 4 Then
    OutName = "PORT" + Mid(InName, 4)
  Else
    OutName = InName
  End If

  Return OutName
End Function

Function GetRegisterLoc(RegName As String) As Integer

  'Returns the location for a system variable
  'Returns -1 if no location found

  Dim RegNameTidy As String
  Dim As Integer DestLoc, CurrBank
  DestLoc = -1

  If ModePIC Then
    'Get the location for the register
    RegNameTidy = LCase(Trim(RegName))
    Select Case RegNameTidy

      Case "syscalctempx", "sysbytetempx", "syswordtempx", "sysintegertempx", "syslongtempx", "delaytemp": DestLoc = 0
      Case "syscalctempx_h", "syswordtempx_h", "sysintegertempx_h", "syslongtempx_h", "delaytemp2": DestLoc = 1
      Case "syscalctempx_u", "syslongtempx_u", "sysdivmultx", "syswaittempms", "sysstringb": DestLoc = 2
      Case "syscalctempx_e", "syslongtempx_e", "sysdivmultx_h", "syswaittempms_h", "sysstringb_h": DestLoc = 3
      Case "sysdivloop", "syswaittemp10ms", "syswaittemps": DestLoc = 4
      Case "syscalctempa", "sysbytetempa", "syswordtempa", "sysintegertempa", "syslongtempa": DestLoc = 5
      Case "syswaittempus", "syswaittemp10us", "syswaittempm": DestLoc = 5
      Case "syscalctempa_h", "syswordtempa_h", "sysintegertempa_h", "syslongtempa_h": DestLoc = 6
      Case "sysstringlength", "syswaittempus_h", "syswaittemph": DestLoc = 6
      Case "syscalctempa_u", "syslongtempa_u", "sysdivmulta", "sysstringa": DestLoc = 7
      Case "syscalctempa_e", "syslongtempa_e", "sysdivmulta_h", "sysstringa_h": DestLoc = 8
      Case "syscalctempb", "sysbytetempb", "syswordtempb", "sysintegertempb", "syslongtempb": DestLoc = 9
      Case "syscalctempb_h", "syswordtempb_h", "sysintegertempb_h", "syslongtempb_h": DestLoc = 10
      Case "syscalctempb_u", "syslongtempb_u", "sysdivmultb", "sysreada": DestLoc = 11
      Case "syscalctempb_e", "syslongtempb_e", "sysdivmultb_h", "sysreada_h": DestLoc = 12
      Case "syssignbyte": DestLoc = 13
      Case "sysw": DestLoc = 14
      Case "sysstatus": DestLoc = 15
    End Select

    'Check that DestLoc actually exists
    If DestLoc <> -1 Then
      For CurrBank = 1 To NoBankLocs
        With NoBankLoc(CurrBank)
          'If the location is in a shared bank, return location
          If .StartLoc + DestLoc <= .EndLoc Then
            Return DestLoc
          End If
        End With
      Next
      DestLoc = -1
    End If

  ElseIf ModeAVR Then
    'Get the location for the register
    RegNameTidy = LCase(Trim(RegName))
    Select Case RegNameTidy
      Case "syscalctempx", "sysbytetempx", "syswordtempx", "sysintegertempx", "syslongtempx": 'DestLoc = 0
          Select Case ChipGPR
            Case 16
              DestLoc = 16
            Case Else
              DestLoc = 0
          End Select

      Case "syscalctempx_h", "syswordtempx_h", "sysintegertempx_h", "syslongtempx_h":'DestLoc = 1
          Select Case ChipGPR
            Case 16
              DestLoc = 17
            Case Else
              DestLoc = 1
          End Select
      Case "syscalctempx_u", "syslongtempx_u", "sysdivmultx": 'DestLoc = 2
          Select Case ChipGPR
            Case 16
              DestLoc = 18
            Case Else
              DestLoc = 2
          End Select

      Case "syscalctempx_e", "syslongtempx_e", "sysdivmultx_h": 'DestLoc = 3
          Select Case ChipGPR
            Case 16
              DestLoc = 19
            Case Else
              DestLoc = 3
          End Select

      Case "syssignbyte": 'DestLoc = 4
          Select Case ChipGPR
            Case 16
              'usE ram
              'DestLoc = 4
            Case Else
              DestLoc = 4
          End Select

      Case "sysdivloop", "sysbittest": 'DestLoc = 5
          Select Case ChipGPR
            Case 16
              DestLoc = 20
            Case Else
              DestLoc = 5
          End Select


      Case "sysvaluecopy": DestLoc = 21
      Case "syscalctempa", "sysbytetempa", "syswordtempa", "sysintegertempa", "syslongtempa": DestLoc = 22
      Case "syscalctempa_h", "syswordtempa_h", "sysintegertempa_h", "syslongtempa_h": DestLoc = 23
      Case "syscalctempa_u", "syslongtempa_u", "sysdivmulta": DestLoc = 24
      Case "delaytemp", "syscalctempa_e", "syslongtempa_e", "sysdivmulta_h", "sysstringlength": DestLoc = 25
      Case "delaytemp2", "sysstringa": DestLoc = 26
      Case "syswaittempus", "syswaittemp10us", "syswaittempm": DestLoc = 27
      Case "sysstringa_h": DestLoc = 27
      Case "syswaittempus_h", "syswaittemph", "syscalctempb", "sysbytetempb", "syswordtempb", "sysintegertempb", "syslongtempb", "sysstringb": DestLoc = 28
      Case "syswaittempms", "syscalctempb_h", "syswordtempb_h", "sysintegertempb_h", "syslongtempb_h", "sysstringb_h": DestLoc = 29
      Case "syswaittempms_h", "syscalctempb_u", "syslongtempb_u", "sysdivmultb", "sysreada": DestLoc = 30
      Case "syswaittemp10ms", "syswaittemps", "syscalctempb_e", "syslongtempb_e": DestLoc = 31
      Case "sysdivmultb_h", "sysreada_h": DestLoc = 31

    End Select
  End If

  Return DestLoc

End Function

Function GetSysVar(VarName As String) As SysVarType Pointer
  'Look up system variable in hash map
  Return CPtr(SysVarType Pointer, HashMapGet(SysVars, VarName))
End Function

Function GetSysVarAliasName( Lookup as String ) As String
  'Look up system alias in hash map, if the retrun string has leadng "ALIAS_" then remove
  'This enables AVR DX return lookups
  
  Dim As LinkedListElement Pointer TempList, CurrItem
  Dim As SysVarType Pointer SysVar
  Dim As Integer TempVar
  Dim As Integer Address
  Dim As String ReturnString, Bits

  'Remove BITs as bits are not part of the ALIAS
  Bits = ""
  If Instr( Lookup, ".") > 0 Then
    Bits = Trim(Mid(Lookup, InStr(Lookup, ".") ))
    Lookup = Trim(Left(Lookup, InStr(Lookup, ".") - 1))
  End If

  If GetSysVar( Lookup ) = 0 Then Return ""
  

  Address = GetSysVar( Lookup )->location

  TempList = HashMapToList(SysVars, -1)
  CurrItem = TempList->Next
  Do While CurrItem <> 0
    SysVar = CurrItem->MetaData
    If SysVar->AVRAlias = -1 Then
      ' print "ADDRESS:",address , CurrItem->Value, SysVar->Name , SysVar->Location, SysVar->AVRAlias
      If SysVar->Location = Address and Trim(SysVar->Name) <> Trim(Lookup) Then
        ReturnString = SysVar->Name
        If Left( SysVar->Name, 6 ) = "ALIAS_" Then 
          ReturnString =  Mid( SysVar->Name, 7)
        End If
        ' print "2 Return Alias" + ReturnString+Bits
        Return ReturnString+Bits
        Exit Function
      End If
    End If
    CurrItem = CurrItem->Next
  Loop
  Return ""
End Function

Function GetSysVarName(Address as Integer) As String
  'Look up system variable in hash map
  
  Dim As LinkedListElement Pointer TempList, CurrItem
  Dim As SysVarType Pointer SysVar
  Dim As Integer TempVar

  TempList = HashMapToList(SysVars, -1)
  CurrItem = TempList->Next
  Do While CurrItem <> 0
    SysVar = CurrItem->MetaData
    ' print "ADDRESS:",address, SysVar->Location , SysVar->Name , SysVar->Location = Address
    If SysVar->Location = Address and SysVar->AVRAlias = 0 Then
      'print "Return" + SysVar->Name
      Return SysVar->Name
    End If

    CurrItem = CurrItem->Next
  Loop
  Return ""
End Function


Function GetSub(Origin As String) As String
  Dim As String Temp
  Dim As Integer T

  IF Origin = "ALL" OR Origin = "" Then Return "ALL"
  Temp = UCase(Trim(Mid(Origin, INSTR(Origin, "S") + 1)))
  IF INSTR(Temp, "D") <> 0 THEN Temp = Left(Temp, INSTR(Temp, "D") - 1)
  T = VAL(Temp)
  IF T = 0 THEN Return "ALL"

  Temp = Trim(Subroutine(T)->Name)
  IF INSTR(Temp, "(") <> 0 Then Temp = Left(Temp, INSTR(Temp, "(") - 1)
  IF INSTR(Temp, " ") <> 0 Then Temp = Left(Temp, INSTR(Temp, " ") - 1)
  IF Temp = "" THEN Temp = "ALL"
  GetSub = Trim(UCase(Temp))
END Function

Function GetSubFullName(SubIndex As Integer) As String

  Dim As String SubNameOut
  Dim As SubType Pointer CompSub
  CompSub = Subroutine(SubIndex)

  'Get the name that the sub will receive when written to the asm
  If CompSub->Overloaded Then
    SubNameOut = CompSub->Name + Str(SubIndex)
  Else
    SubNameOut = CompSub->Name
  End If
  If CompSub->IsFunction Then
    SubNameOut = "FN_" + SubNameOut
  End If

  Return SubNameOut
End Function

FUNCTION GetSubID(Origin As String) As Integer
  Dim As String Temp

  If Origin = "ALL" OR Origin = "" Then Return 0
  Temp = UCase(Trim(Mid(Origin, INSTR(Origin, "S") + 1)))
  IF INSTR(Temp, "D") <> 0 THEN Temp = Left(Temp, INSTR(Temp, "D") - 1)
  Return VAL(Temp)

END Function

Function GetSubSig(CurrentSub As SubType) As String
  Dim As String OutData, NewData
  Dim As Integer PD

  OutData = ""
  With CurrentSub
    For PD = 1 to .ParamCount
      With .Params(PD)
        NewData = ""
        Select Case UCASE(.Type)
          Case "BIT", "BYTE", "WORD", "INTEGER", "LONG", "FLOAT", "STRING": NewData = UCase(.Type) + ":"
        End Select
        If .Default <> "" Then NewData = LCase(NewData)
        OutData += NewData
      End With
    Next
  End With

  Return OutData
End Function

Function GetSubParam (ParamIn As String, ForceIn As Integer) As SubParam
  'Parse a text representation of a single sub parameter, and output a
  'matching SubParam item
  'If ForceIn = -1, parameter will be treated as input

  Dim ParamOut As SubParam
  Dim As String TempData, ParamData

  ParamData = UCase(Trim(ParamIn))

  With ParamOut
    .Dir = 3
    .Default = ""

    'In/Out, OPTIONAL
    CheckStartAgain:
    'Print ParamData
    If Left(ParamData, 3) = "IN " Then
      .Dir = 1
      ParamData = Mid(ParamData, 4)
      Goto CheckStartAgain
    ElseIf Left(ParamData, 4) = "OUT " Then
      .Dir = 2
      ParamData = Mid(ParamData, 5)
      Goto CheckStartAgain
    ElseIf Left(ParamData, 9) = "OPTIONAL " Then
      .Default = Trim(Mid(ParamData, Instr(ParamData, "=") + 1))
      ParamData = Trim(Left(ParamData, Instr(ParamData, "=") - 1))
      ParamData = Trim(Mid(ParamData, 10))
      Goto CheckStartAgain
    'Optional is optional, might just have the default value
    ElseIf InStr(ParamData, "=") <> 0 Then
      .Default = Trim(Mid(ParamData, Instr(ParamData, "=") + 1))
      ParamData = Trim(Left(ParamData, Instr(ParamData, "=") - 1))
      Goto CheckStartAgain
    End If

    'Type
    If INSTR(ParamData, " AS ") <> 0 Then
      .Type = Mid(ParamData, Instr(ParamData, " AS ") + 4)
      ParamData = Left(ParamData, Instr(ParamData, " AS ") - 1)
    Else
      .Type = "BYTE"
    End If

    'Name
    .Name = Trim(ParamData)

    'Type detection for () and $
    If Right(.Name, 2) = "()" Then
      .Name = Left(.Name, Len(.Name) - 2)
      .Type = .Type + "()"
    ElseIf Right(.Name, 1) = "$" Then
      .Name = Left(.Name, Len(.Name) - 1)
      .Type = "STRING"
    End If

    'Force input?
    If ForceIn Then
      .Dir = 1
    End If

  End With

  Return ParamOut

End Function

Function GetTool(ToolName As String) As ExternalTool Pointer
  'Returns a pointer to the tool named by ToolName, or 0 if nothing found

  If ToolName = "" Then Return 0

  Dim As Integer FindTool
  For FindTool = 1 To ToolCount
    If Tool(FindTool).Name = LCase(ToolName) Then
      Return @Tool(FindTool)
    End If
  Next

  Return 0
End Function

SUB InitCompiler

  'Misc temp vars
  Dim As String Temp, DataSource, MessagesFile, LangMessagesFile, LangName, MsgName, MsgVal
  Dim As String SettingsFile(20), CurrentTag, ParamUpper, LeftThree, NewFileName
  Dim As Integer T, Block, CD, FCB, IniNotSet, SettingsFileMode, FindTool, FindMsg
  Dim As Integer SettingsFiles, CurrSettingsFile

  Dim As Integer PICAsmNotSet, AsmNotSet, ProgNotSet, OutNotSet, PresNotSet, VbsNotSet, WarnErrorNotSet
  Dim As Integer PauseNotSet, ReportNotSet, FlashOnlyNotSet

  'Detect GCBASIC install directory
  ID = ExePath
  If ID = "" Or ID = "." THEN
    ID = CURDIR
    #IFDEF __FB_UNIX__
      If Right(ID, 1) = "/" Then ID = Left(ID, Len(ID) - 1)
    #ELSE
      If Right(ID, 1) = "\" Then ID = Left(ID, Len(ID) - 1)
    #ENDIF
  End If

  'Clear parameters
  OFI = ""
  AsmExe = ""
  PrgExe = ""
  HexAppend = ""
  AsmTool = 0
  PrgTool = 0
  VBS = 0
  PauseOnErr = 1
  WarningsAsErrors = 0
  MuteBanners = -1
  MuteDonate = -1
  ExtendedVerboseMessages= 0
  PreserveMode = 0
  PauseAfterCompile = 0
  ShowProgressCounters = -1
  FlashOnly = 0
  SkipHexCheck = 0
  GCGB = 0
  CompReportFormat = "html"
  PauseTimeout = -1

  SettingsFile(1) = "gcbasic.ini"
  SettingsFiles = 1
  AsmNotSet = -1
  PICAsmNotSet = -1
  ProgNotSet = -1
  OutNotSet = -1
  PresNotSet = -1
  VbsNotSet = -1
  PauseNotSet = -1
  WarnErrorNotSet = -1
  IniNotSet = -1
  ReportNotSet = -1
  FlashOnlyNotSet = -1
  OutPutConfigOptions = -1

  'Read parameters
  CD = 1
  Do While COMMAND(CD) <> ""
    DataSource = COMMAND(CD)
    ParamUpper = Ucase(COMMAND(CD))
    LeftThree = Left(ParamUpper, 3)

    'Simple options
    'Verbose mode
    If ParamUpper = "/V" Or ParamUpper = "-V" Then
      VBS = 1
      VbsNotSet = 0

    'Show licence
    ElseIf ParamUpper = "/L" Or ParamUpper = "-L" Then
      ShowBlock "License"
      END

    'Pause on errors?
    ElseIf ParamUpper = "/NP" Or ParamUpper = "-NP" Then
      PauseOnErr = 0
      PauseNotSet = 0

    ElseIf ParamUpper = "/WX" Or ParamUpper = "-WX" Then
      WarningsAsErrors = -1
      WarnErrorNotSet = 0

    ElseIf ParamUpper = "/M" Or ParamUpper = "-M" Then
      MuteBanners = 0

    ElseIf ParamUpper = "/F" Or ParamUpper = "-F" Then
      FlashOnly = -1
      FlashOnlyNotSet = 0

    ElseIf ParamUpper = "/FO" Or ParamUpper = "-FO" Then
      SkipHexCheck = -1
      FlashOnly = -1
      FlashOnlyNotSet = 0

    'Great Cow Graphical BASIC Mode?
    '(Alters the error listing format)
    ElseIf ParamUpper = "/GCGB" Then
      GCGB = 1

    ElseIf ParamUpper = "/CLS" Then  
      CLS
    ElseIf ParamUpper = "/VERSION" Then
      Print Version
      End

    'Complex options
    'Allow verbose mode to be turned off
    ElseIf LeftThree = "/V:" Or LeftThree = "-V:" Then
      VBS = PrefIsYes(Mid(ParamUpper, 4), 1)
      VbsNotSet = 0

    'Allow flash only to be turned off
    ElseIf LeftThree = "/F:" Or LeftThree = "-F:" Then
      FlashOnly = PrefIsYes(Mid(ParamUpper, 4))
      FlashOnlyNotSet = 0

    'Settings file
    ElseIf LeftThree = "/S:" Or LeftThree = "-S:" Then
      SettingsFile(1) = Mid(DataSource, 4)
      IniNotSet = 0

    'Assembler command
    ElseIf LeftThree = "/A:" Or LeftThree = "-A:" Then


     AsmExe = Trim(Mid(DataSource, 4))
     if instr ( AsmExe, "PIC-AS") = 0 then
        'as was code
        AsmParams = ""
        If Left(AsmExe, 1) = Chr(34) Then
          For FCB = 2 to Len(AsmExe)
            If Mid(AsmExe, FCB, 1) = Chr(34) Then
              AsmParams = Trim(Mid(AsmExe, FCB + 1, 1))
              AsmExe = Trim(Left(AsmExe, FCB))
            End If
          Next
        ElseIf Instr(AsmExe, " ") <> 0 Then
          AsmParams = Mid(AsmExe, Instr(AsmExe, " ") + 1)
          AsmExe = Left(AsmExe, Instr(AsmExe, " ") - 1)
        End If

        AsmNotSet = 0
        'Do While INSTR(MakeASM, Chr(34)) <> 0: Replace MakeASM, Chr(34), "": Loop
      else

        'PIC-AS Assembler command
        AsmExe = Trim(Mid(DataSource, 4))
        AsmParams = ""
        If Left(AsmExe, 1) = Chr(34) Then
          For FCB = 2 to Len(AsmExe)
            If Mid(AsmExe, FCB, 1) = Chr(34) Then
              AsmParams = Trim(Mid(AsmExe, FCB + 1, 1))
              AsmExe = Trim(Left(AsmExe, FCB))
            End If
          Next
        ElseIf Instr(AsmExe, " ") <> 0 Then
          AsmParams = Mid(AsmExe, Instr(AsmExe, " ") + 1) +  " > "+chr(34)+"%Fn_NoExt%.err"+chr(34)
          AsmExe = Left(AsmExe, Instr(AsmExe, " ") - 1)
        End If

        AFISupport = 1
        PICASMNotSet = 0

      End if




    'Programmer command
    ElseIf LeftThree = "/P:" Or LeftThree = "-P:" Then
      PrgExe = Trim(Mid(DataSource, 4))
      PrgParams = ""
      PrgDir = ""
      If Left(PrgExe, 1) = Chr(34) Then
        For FCB = 2 to Len(PrgExe)
          If Mid(PrgExe, FCB, 1) = Chr(34) Then
            PrgParams = Trim(Mid(PrgExe, FCB + 1, 1))
            PrgExe = Trim(Left(PrgExe, FCB))
          End If
        Next
      ElseIf Instr(PrgExe, " ") <> 0 Then
        PrgParams = Mid(PrgExe, Instr(PrgExe, " ") + 1)
        PrgExe = Left(PrgExe, Instr(PrgExe, " ") - 1)
      End If
      ProgNotSet = 0
      'Do While INSTR(SendToPIC, Chr(34)) <> 0: Replace SendToPIC, Chr(34), "": Loop

    'Output filename
    ElseIf LeftThree = "/O:" Or LeftThree = "-O:" Then
      OFI = Mid(DataSource, 4)
      OutNotSet = 0
      'Do While INSTR(OFI, Chr(34)) <> 0: Replace OFI, Chr(34), "": Loop

    'Make hex mode
    ElseIf LeftThree = "/H:" Or LeftThree = "-H:" Then
      Temp = Right(ParamUpper, 1)
      IF Temp = "Y" OR Temp = "1" Then MakeHexMode = 1
      IF Temp = "N" OR Temp = "0" Then MakeHexMode = 0

    'Preserve mode
    ElseIf LeftThree = "/K:" Or LeftThree = "-K:" Then
      Temp = Right(ParamUpper, 1)
      IF Temp = "C" OR Temp = "1" Then PreserveMode = 1
      IF Temp = "A" OR Temp = "2" Then PreserveMode = 2
      If Temp = "L" OR Temp = "3" Then PreserveMode = 3
      PresNotSet = 0

    'Report mode
    ElseIf LeftThree = "/R:" Or LeftThree = "-R:" Then
      CompReportFormat = LCase(Mid(DataSource, 4))
      ReportNotSet = 0

    ElseIf LeftThree = "/CP" Or LeftThree = "-CP" Then
      OutPutConfigOptions = 0

    'Deprecated options
    'Clear screen
    ElseIf ParamUpper = "/NC" or ParamUpper = "-NC" Then
      'CLS

    'Compiler directory
    ElseIf LeftThree = "/D:" Or LeftThree = "-D:" Then

    'Input filename
    Else
      FI = ShortName(DataSource)

    End If

    CD += 1
  Loop

  'Read settings file
  CurrSettingsFile = 0
  Do
    CurrSettingsFile += 1

    #IFDEF __FB_UNIX__
      If Instr(SettingsFile(CurrSettingsFile), "/") = 0 Then SettingsFile(CurrSettingsFile) = ID + "/" + SettingsFile(CurrSettingsFile)
    #ELSE
      If Instr(SettingsFile(CurrSettingsFile), "\") = 0 Then SettingsFile(CurrSettingsFile) = ID + "\" + SettingsFile(CurrSettingsFile)
    #ENDIF
    If Dir(SettingsFile(CurrSettingsFile)) = "" Then
      If VBS = 1 And IniNotSet = 0 Then Print "Cannot find settings file " + SettingsFile(CurrSettingsFile)
    Else
      Open SettingsFile(CurrSettingsFile) For Input As #1
      globalSettingsFile = SettingsFile(CurrSettingsFile)
      'Decide mode to read file in
      SettingsFileMode = 0
      Do While Not Eof(1)
        Line Input #1, DataSource
        DataSource = Trim(DataSource)
        If Left(DataSource, 1) = "[" And Right (DataSource, 1) = "]" Then
          SettingsFileMode = 1
          Exit Do
        End If
      Loop
      Seek #1, 1
      'Read actual file
      CurrentTag = ""
      If SettingsFileMode = 0 Then CurrentTag = "gcbasic"
      Do While Not EOF(1)
        Line Input #1, DataSource
        Do While InStr(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": Loop
        DataSource = Trim(DataSource)

        If DataSource <> "" And Left(DataSource, 1) <> "'" Then

          If Left(DataSource, 8) = "include " Then
            NewFileName = Trim(Mid(DataSource, 9))

            'Replace %APPDATA% with APPDATA environment var
            NewFileName = ReplaceToolVariables(NewFileName)

            Dim As Integer CheckFile, FileFound
            FileFound = 0
            For CheckFile = 1 To SettingsFiles
              If SettingsFile(CheckFile) = NewFileName Then FileFound = -1: Exit For
            Next
            If Not FileFound Then
              SettingsFiles += 1
              SettingsFile(SettingsFiles) = NewFileName
            End If

          ElseIf SettingsFileMode = 1 And Left(DataSource, 1) = "[" And Right(DataSource, 1) = "]" Then
            CurrentTag = LCase(Mid(DataSource, 2, Len(DataSource) - 2))

            If Left(CurrentTag, 4) = "tool" Then
              ToolCount += 1
              With Tool(ToolCount)
                If InStr(CurrentTag, "=") <> 0 Then
                  .Name = LCase(Trim(Mid(CurrentTag, InStr(CurrentTag, "=") + 1)))
                Else
                  .Name = ""
                End If
                .Type = ""
                .Cmd = ""
                .Params = ""
                .ExtraParams = 0
                .ProgConfig = ""
                .ProgOptions = ""
                .UseIf = ""
              End With
            EndIf

          Else
            MsgName = LCase(Trim(Left(DataSource, INSTR(DataSource, "=") - 1)))
            MsgVal = Trim(Mid(DataSource, INSTR(DataSource, "=") + 1))

            If CurrentTag = "gcbasic" Then
              Select Case MsgName
                Case "assembler"
                If AsmNotSet Then
                  AsmExe = MsgVal
                  SelectedAssembler = MsgVal
                  if instr( ucase(MsgVal), "PIC-AS") > 0 then
                          AFISupport = 1
                  End If
                  Replace ( SelectedAssembler, "-", "" )
                End if

                Case "conditionaldebugfile"
                  Conditionaldebugfile = MsgVal
                  if Len(Conditionaldebugfile) > 0 Then
                    CDFSupport = 1
                  End if

                Case "columnwidth"
                  Columnwidth = Val(Trim(MsgVal))
                  if Columnwidth = 0 Then
                    Columnwidth = 77
                  End if



                Case "assemblerparams"
                If AsmNotSet Then AsmParams = MsgVal

                Case "programmer"
                If ProgNotSet Then PrgExe = MsgVal

                Case "programmerparams"
                If ProgNotSet Then PrgParams = MsgVal

                Case "output"
                If OutNotSet Then OFI = MsgVal

                Case "preserve"
                If PresNotSet Then
                  Select Case LCase(Left(MsgVal, 1))
                    Case "c", "1": PreserveMode = 1
                    Case "a", "2": PreserveMode = 2
                    Case "l", "3": PreserveMode = 3
                  End Select
                End If

                Case "workingdir"
                If LCase(MsgVal) = "tempdir" Or LCase(MsgVal) = "instdir" Then
                  MsgVal = "%" + MsgVal + "%"
                End If
                MsgVal = ReplaceToolVariables(MsgVal)
                ChDir MsgVal

                Case "verbose"
                If VbsNotSet Then
                  VBS = PrefIsYes(MsgVal, 1)
                End If

                Case "pauseonerror"
                If PauseNotSet Then
                  PauseOnErr = PrefIsYes(MsgVal, 1)
                End If

                Case "showprogresscounters":
                ShowProgressCounters = PrefIsYes(MsgVal)

                Case "warningsaserrors"
                If WarnErrorNotSet Then
                  WarningsAsErrors = PrefIsYes(MsgVal)
                End If

                Case "mutebanners"
                  MuteBanners = NOT PrefIsYes(MsgVal)

                Case "mutedonateplyx"
                  MuteDonate = PrefIsYes(MsgVal)

                Case "evbs"
                  if PrefIsYes(MsgVal, 1 ) = 1   Then
                    ExtendedVerboseMessages= -1
                  End if

                Case "laxsyntax"
                  if PrefIsYes(MsgVal, 1 ) = 1   Then
                    laxsyntax = -1
                  End if

                Case "picasdebug"
                  if PrefIsYes(MsgVal, 1 ) = 1   Then
                    PICASdebug = true
                  End if
                
                Case "methodstructuredebug"
                  if PrefIsYes(MsgVal, 1 ) = 1   Then
                    methodstructuredebug = true
                  End if

                Case "floatcapability"
                    If floatcapability = 0 Then  'so, not set by #OPTION FLOATS 
                      floatcapability = val(msgval)
                      VersionSuffix = Str(floatcapability)
                    End if

                Case "compilerdebug"
                    compilerdebug = val(msgval)

                Case "overridelowleveldatfileextextension"
                    overridelowleveldatfileextextension = val(msgval)
                
                Case "overridelowleveldatfileextextensionmessage"
                    overridelowleveldatfileextextensionmessage = val(msgval)
                
                Case "overridetestdatfilemessage"
                    overridetestdatfilemessage = val(msgval)

                Case "datfileinspection"
                  if PrefIsYes(MsgVal, 1 ) = 1   Then
                    DATfileinspection = true
                  else
                    DATfileinspection = false
                  End if                

                Case "nosummary"
                  if PrefIsYes(MsgVal, 1 ) = 1   Then
                    NoSummary = -1
                  End if

                Case "flashonly"
                If FlashOnlyNotSet Then
                  FlashOnly = PrefIsYes(MsgVal)
                End If

                Case "pauseaftercompile"
                  PauseAfterCompile = PrefIsYes(MsgVal)

                Case "pausetimeout"
                  PauseTimeout = Val(MsgVal)

                Case "language"
                  LangName = MsgVal

                Case "hexappendgcbmessage"
                  Select Case LCase(Left(MsgVal, 1))
                    Case "Y","y", "1": HexAppend = ";HEX file generated by GCBASIC"
                  End Select


                Case "report"
                  If ReportNotSet Then
                    CompReportFormat = LCase(MsgVal)
                  End If

              End Select
            ElseIf CurrentTag = "libaries" Then
              Select Case MsgName
                Case "disabled"
                  IgnoreSourceFiles = IgnoreSourceFiles + 1
                  IgnoreSourceFile(IgnoreSourceFiles).FileName = MsgVal
                  IgnoreSourceFile(IgnoreSourceFiles).Ignore = -1
                Case "enabled"
                  'do nothing
              End Select
            ElseIf CurrentTag = "toolvariables" Then
              Dim As String Pointer TempStr
              TempStr = Callocate(SizeOf(String))
              *TempStr = MsgVal
              LinkedListInsert(ToolVariables, MsgName)->MetaData = TempStr

            ElseIf CurrentTag= "patch=asm2picas" Then
              'create patches from

              Dim localCharPosition as Integer
              'strip off first quote
              localCharPosition = instr( DataSource, chr(34) )
              DataSource = Mid( DataSource, localCharPosition + 1 )

              localCharPosition = instr( DataSource, chr(34) )
              MsgName = left ( DataSource, localCharPosition - 1 )

              Patches( patchCounter ).Source = ucase(MsgName)

              DataSource = mid ( DataSource , localCharPosition )

              localCharPosition = instr( DataSource, "=" )
              DataSource = Mid ( DataSource,localCharPosition + 1 )


              localCharPosition = instr( DataSource, chr(34) )  'find the next char
              DataSource = mid ( DataSource , localCharPosition + 1 )

              localCharPosition = instr( DataSource, chr(34) )  'find the next char
              MsgVal = mid ( DataSource ,1, localCharPosition -1 )
              Patches( patchCounter ).Destination = MsgVal
              patchCounter = patchCounter + 1

            ElseIf Left(CurrentTag, 4) = "tool" Then
              Select Case MsgName
                Case "name"
                  Tool(ToolCount).Name = LCase(MsgVal)
                Case "desc"
                  Tool(ToolCount).DispName = MsgVal
                Case "type"
                  Tool(ToolCount).Type = LCase(MsgVal)
                Case "command"
                  Tool(ToolCount).Cmd = MsgVal
                  if instr( ucase(MsgVal), "PIC-AS") > 0 then
                      if left( MsgVal, 1 )  =  chr(34) then MsgVal = mid( MsgVal, 1)
                      if right( MsgVal, 1 ) =  chr(34) then MsgVal = right( MsgVal, 1)
                      Tool(ToolCount).Cmd  = chr(34)+MsgVal+chr(34)  'PICASDriver
                  End if

                Case "params"
                  Tool(ToolCount).Params = MsgVal
                  if instr( ucase(Tool(ToolCount).Cmd), "PIC-AS") > 0 then
                        Tool(ToolCount).Params = MsgVal +  " > "+chr(34)+"%Fn_NoExt%.err"+chr(34)  'PICAsmParams
                  End if
                Case "workingdir"
                  Tool(ToolCount).WorkingDir = MsgVal
                Case "progconfig"
                  Tool(ToolCount).ProgConfig = MsgVal
                Case "progoptions"
                  Tool(ToolCount).ProgOptions = MsgVal
                Case "useif"
                  Tool(ToolCount).UseIf = MsgVal
                Case Else
                  With Tool(ToolCount)
                    If .ExtraParams < 5 Then
                      .ExtraParams += 1
                      .ExtraParam(.ExtraParams, 1) = MsgName
                      .ExtraParam(.ExtraParams, 2) = MsgVal
                    End If
                  End With
              End Select


            End If
          End If
        End If
      Loop
      Close
    End If
  Loop While CurrSettingsFile < SettingsFiles

  'Read message list
  #IFDEF __FB_UNIX__
    MessagesFile = ID + "/messages.dat"
    LangMessagesFile = ID + "/messages-" + LCase(LangName) + ".dat"
  #ELSE
    MessagesFile = ID + "\messages.dat"
    LangMessagesFile = ID + "/messages-" + LCase(LangName) + ".dat"
  #ENDIF
  IF Dir(MessagesFile) = "" THEN
    PRINT "Cannot find " + MessagesFile
    Print
    PRINT "GCBASIC cannot operate without this file"
    If PauseOnErr = 1 THEN
      PRINT
      PRINT "Press any key to continue"
      Sleep
    END IF
    END
  END IF

  'Load localised messages file
  If LangName <> "" Then
    If Dir(LangMessagesFile) = "" Then
      Print "Could not find localised message file " + LangMessagesFile
    Else
      OPEN LangMessagesFile FOR INPUT AS #1
      Block = 0
      DO WHILE NOT EOF(1)
        LINE INPUT #1, DataSource
        DataSource = Trim(DataSource)
        IF Left(UCase(DataSource), 6) = "BLOCK " THEN
          Block = 1
        ElseIF Left(UCase(DataSource), 9) = "END BLOCK" THEN
          Block = 0
        ElseIF Left(DataSource, 1) = "'" THEN
          DataSource = ""
        ElseIF INSTR(DataSource, "=") <> 0 THEN
          MsgName = Left(DataSource, InStr(DataSource, "=") - 1)
          MsgVal = Mid(DataSource, INSTR(DataSource, "=") + 1)
          IF INSTR(MsgVal, Chr(34)) <> 0 THEN
            MsgVal = Mid(MsgVal, INSTR(MsgVal, Chr(34)) + 1)
            MsgVal = Left(MsgVal, INSTR(MsgVal, Chr(34)) - 1)
          END IF
          'Add message if it doesn't exist already
          MsgName = Trim(UCase(MsgName))
          T = 0
          For FindMsg = 1 To MSGC
            If MsgName = Messages(1, FindMsg) Then T = FindMsg: Exit For
          Next
          If T = 0 Then
            MSGC = MSGC + 1
            Messages(1, MSGC) = MsgName
            Messages(2, MSGC) = MsgVal
          End If
        END IF
      LOOP
      CLOSE #1
    End If
  End If

  OPEN MessagesFile FOR INPUT AS #1
  Block = 0
  DO WHILE NOT EOF(1)
    LINE INPUT #1, DataSource
    DataSource = Trim(DataSource)
    IF Left(UCase(DataSource), 6) = "BLOCK " THEN
      Block = 1
    ElseIF Left(UCase(DataSource), 9) = "END BLOCK" THEN
      Block = 0
    ElseIF Left(DataSource, 1) = "'" THEN
      DataSource = ""
    ElseIF INSTR(DataSource, "=") <> 0 THEN
      MsgName = Left(DataSource, INSTR(DataSource, "=") - 1)
      MsgVal = Mid(DataSource, INSTR(DataSource, "=") + 1)
      IF INSTR(MsgVal, Chr(34)) <> 0 THEN
        MsgVal = Mid(MsgVal, INSTR(MsgVal, Chr(34)) + 1)
        MsgVal = Left(MsgVal, INSTR(MsgVal, Chr(34)) - 1)
      END IF
      'Add message if it doesn't exist already
      MsgName = Trim(UCase(MsgName))
      T = 0
      For FindMsg = 1 To MSGC
        If MsgName = Messages(1, FindMsg) Then T = FindMsg: Exit For
      Next
      If T = 0 Then
        MSGC = MSGC + 1
        Messages(1, MSGC) = MsgName
        Messages(2, MSGC) = MsgVal
      End If
    END IF
  LOOP
  CLOSE #1

  'Message if no filename specified
  IF FI = "" THEN
    ShowBlock "NoPrompt"
    If PauseOnErr = 1 THEN
      WaitForKeyOrTimeout
    END IF
    END
  END IF

  'Decide name for output file if not specified
  'If name is specified, do not allow flashing only
  IF OFI = "" THEN
    OFI = FI
    For CD = LEN(OFI) TO 1 STEP -1
      IF Mid(OFI, CD, 1) = "." THEN OFI = Left(OFI, CD - 1): EXIT FOR
    Next

    CDF = OFI + ".cdf"
    AFI = OFI + ".s"
    OFI = OFI + ".asm"


  Else
    FlashOnly = 0
  END IF

  'Find directory of source file (used for relative include)
  ProgDir = CURDIR
  IF INSTR(FI, "\") <> 0 THEN
    FOR T = LEN(FI) TO 1 STEP -1
      IF Mid(FI, T, 1) = "\" Or Mid(FI, T, 1) = "/" Then EXIT FOR
    NEXT
    ProgDir = Left(FI, T - 1)
  END IF
  IF Right(ProgDir, 1) = "\" THEN ProgDir = Left(ProgDir, LEN(ProgDir) - 1)
  IF Right(ProgDir, 1) = "/" THEN ProgDir = Left(ProgDir, LEN(ProgDir) - 1)

  'Load file converters
  LoadConverters

  'Show version
  PRINT "GCBASIC (" + Version + ")"
  Print

  IF Dir(FI) = "" THEN
    PRINT
    PRINT Message("NoFile")
    If PauseOnErr = 1 THEN
      WaitForKeyOrTimeout
    END IF
    END
  END IF

  'Start Compile
  IF VBS = 0 Then   PRINT Message("CompilingShortname") Else  PRINT Message("Compiling")

END SUB

Function IsArray (VarNameIn As String, CurrSub As SubType Pointer) As Integer
  IsArray = 0
  Dim As VariableType Pointer FoundVar
  Dim As String VarName
  'If () after variable name, remove and check variable
  VarName = Trim(UCase(VarNameIn))
  If Right(VarName, 2) = "()" Then
    VarName = RTrim(Left(VarName, Len(VarName) - 2))
  End If

  'Array var?
  FoundVar = HashMapGet(@(CurrSub->Variables), VarName)
  If FoundVar <> 0 Then
    If FoundVar->Size > 1 Then
      Return -1
    End If
  End If

  'Main sub
  FoundVar = HashMapGet(@(Subroutine(0)->Variables), VarName)
  If FoundVar <> 0 Then
    If FoundVar->Size > 1 Then
      Return -1
    End If
  End If

End FUNCTION

Function IsNonBanked(Location As Integer) As Integer
  'Returns -1 if the location does not require banking
  'PIC only, return true on all other chips

  Dim As Integer CurrBank

  If ModePIC Then
    'Check all shared banks
    For CurrBank = 1 To NoBankLocs
      With NoBankLoc(CurrBank)
        'If the location is in a shared bank, return true
        If Location >= .StartLoc And Location <= .EndLoc Then
          Return -1
        End If
      End With
    Next
    Return 0

  Else
    Return -1
  End If

End Function

Function IsInAccessBank(VarNameIn As String) As Integer
  'Check if a specified variable is located in the access bank
  Dim As Integer FindVar
  Dim As String VarName
  Dim As SysVarType Pointer FoundVar

  '18F only at this stage
  If ChipFamily <> 16 Then Return 0

  VarName = UCase(Trim(VarNameIn))

  'Check for SFRs and Registers
  'Need to get location, then check if it is a non-banked location
  'Search system variable list
  FoundVar = GetSysVar(VarName)
  If FoundVar <> 0 Then
    Return IsNonBanked(FoundVar->Location)
  End If

  'Check if the variable being accessed is a SFR
  If IsRegister(VarNameIn) Then
    Return -1
  End If
  Return 0
End Function

Function IsIOPinName(PinNameIn As String) As Integer
  Dim PinName As String
  PinName = PinNameIn

  'Checks if PinName is that of an IO pin.
  'Format: [RP][A-Z][0-7] Or GP[0-7]
  If Len(PinName) <> 3 Then Return 0
  PinName = UCase(PinName)

  If Left(PinName, 1) = "R" Or Left(PinName, 1) = "P" Then
    If Mid(PinName, 2, 1) < "A" Or Mid(PinName, 2, 1) > "Z" Then Return 0
    If InStr("01234567", Mid(PinName, 3, 1)) = 0 Then Return 0
  Else
    If Left(PinName, 2) <> "GP" Then Return 0
    If InStr("01234567", Mid(PinName, 3, 1)) = 0 Then Return 0
  End If

  Return -1
End Function

Function IsIORegDX (RegNameIn As String) As Integer
  Dim RegName As String
  Dim As Integer SD
  Dim As SysVarType Pointer FoundVar

  'Check if a register is in the IO space
  RegName = TRIM(UCASE(RegNameIn))
  'Only check the register name not the bit
  If Instr( RegName, ".") > 0 Then
    RegName = Left(RegName, InStr(RegName, ".") - 1)
  End If

  'Search, return true if found
  FoundVar = GetSysVar(RegName)
  If FoundVar <> 0 Then
    'print RegName, FoundVar->AVRAlias, FoundVar->Location, FoundVar->Name
    If FoundVar->AVRAlias = -1 Then
      Return -1
    End If
  End If

  Return 0
End Function

Function IsIOReg (RegNameIn As String) As Integer
  Dim RegName As String
  Dim As Integer SD
  Dim As SysVarType Pointer FoundVar

  'Check if a register is in the IO space
  RegName = TRIM(UCASE(RegNameIn))

  'Search, return true if found
  FoundVar = GetSysVar(RegName)
  If FoundVar <> 0 Then
    If ModePIC Then
      Return -1
    ElseIf ModeAVR Then
      If FoundVar->Location > 63 Then
        Return 0
      Else
        Return -1
      End If
    End If
  End If

  'Check aliases
  'May have an alias to an IO register
  Dim As String Source, Temp
  Dim As Integer CurrSub
  Dim As VariableType Pointer FoundUserVar
  Source = Trim(UCase(RegName))
  Temp = ""
  For CurrSub = 0 To SBC
    FoundUserVar = HashMapGet(@(Subroutine(CurrSub)->Variables), Source)
    If FoundUserVar <> 0 THEN
      'Have found an alias?
      Temp = FoundUserVar->Alias
      If Temp <> "" Then
        If InStr(Temp, ",") <> 0 Then Temp = Trim(Left(Temp, InStr(Temp, ",") - 1))
        If IsIOReg(Temp) Then
          Return -1
        End If
      End If
    End If

  Next

  Return 0
End Function

Function IsLowIOReg (RegNameIn As String) As Integer
  Dim RegName As String
  Dim As Integer SD
  Dim As SysVarType Pointer FoundVar

  'Check if a register is in the IO space
  RegName = TRIM(UCASE(RegNameIn))

  'Search, return true if found
  FoundVar = GetSysVar(RegName)
  If FoundVar <> 0 Then
    If ModePIC Then
      Return -1
    ElseIf ModeAVR Then
      If FoundVar->Location > 31 Then
        Return 0
      Else
        Return -1
      End If
    End If
  End If

  'Check aliases
  'May have an alias to a low IO register
  Dim As String Source, Temp
  Dim As Integer CurrSub
  Dim As VariableType Pointer FoundUserVar
  Source = Trim(UCase(RegName))
  Temp = ""
  For CurrSub = 0 To SBC
    FoundUserVar = HashMapGet(@(Subroutine(CurrSub)->Variables), Source)
    If FoundUserVar <> 0 THEN
      'Have found an alias?
      Temp = FoundUserVar->Alias
      If Temp <> "" Then
        If InStr(Temp, ",") <> 0 Then Temp = Trim(Left(Temp, InStr(Temp, ",") - 1))
        Return IsLowIOReg(Temp)
      End If
    End If

  Next

  Return 0
End Function

Function IsLowRegister(VarName As String) As Integer
  'Note, this can only be run accurately after AllocateRAM has run
  'AVR only
  'Will return true if register is low (r0 to r15 inclusive)
  Dim As String RealName
  Dim As Integer CurrItem, RegNo

  If Not ModeAVR Then Return 0

  'If we have an alias, get the real name
  RealName = UCase(VarName)
  SearchAliasAgain:
  For CurrItem = 1 To FALC
    If RealName = FinalAliasList(CurrItem).Name Then
      RealName = FinalAliasList(CurrItem).Value
      GoTo SearchAliasAgain 'May have a nested alias
    End If
  Next
  RealName = UCase(RealName)

  'Is it low?
  For CurrItem = 1 To FRLC
    If RealName = FinalRegList(CurrItem).Name Then
      RegNo = Val(Mid(FinalRegList(CurrItem).Value, 2))
      If RegNo < 16 Then
        Return -1
      Else
        Return 0
      End If
    End If
  Next

  'Default to most restrictive but safest option
  Return -1
End Function

Function IsRegister (VarName As String) As Integer
  Dim As String Temp, Source, SourceLowByte
  Dim As Integer CurrSub, MinAliasSize
  Dim As VariableType Pointer FoundVar
  Dim As VariableListElement Pointer FoundFinalVar

  'System vars that are always registers
  'SysTemp vars are only registers on AVR, no room to be registers on PIC
  If ModeAVR Then
    If IsSysTemp(VarName) Then Return -1
    IF UCase(VarName) = "SYSVALUECOPY" Then Return -1
    If UCase(VarName) = "SYSBITTEST" Then Return -1
  End If
  If ModePIC Then
    If UCase(VarName) = "SYSW" Then Return -1
    If UCase(VarName) = "SYSSTATUS" Then Return -1
  End If
  IF UCase(Left(VarName, 11)) = "SYSCALCTEMP" Then Return -1

  IF UCase(Left(VarName, 11)) = "SYSBYTETEMP" And InStr("ABXabx", Mid(VarName, 12, 1)) <> 0 Then Return -1
  IF UCase(Left(VarName, 11)) = "SYSWORDTEMP" And InStr("ABXabx", Mid(VarName, 12, 1)) <> 0 Then Return -1
  IF UCase(Left(VarName, 14)) = "SYSINTEGERTEMP" And InStr("ABXabx", Mid(VarName, 15, 1)) <> 0 Then Return -1
  IF UCase(Left(VarName, 11)) = "SYSLONGTEMP" And InStr("ABXabx", Mid(VarName, 12, 1)) <> 0 Then Return -1

  IF UCase(Left(VarName, 10)) = "SYSDIVMULT" Then Return -1
  IF UCase(Left(VarName, 11)) = "SYSWAITTEMP" Then Return -1
  IF UCase(Left(VarName, 11)) = "SYSWAITTEMP" OR UCase(Left(VarName, 9)) = "DELAYTEMP" Then Return -1
  If UCase(VarName) = "SYSREADA" Or UCase(VarName) = "SYSREADA_H" Then Return -1
  If UCase(Left(VarName, 9)) = "SYSSTRING" Then
    Temp = UCase(Mid(VarName, 10))
    If Temp = "A" Or Temp = "A_H" Or Temp = "B" Or Temp = "B_H" Or Temp = "LENGTH" Then
      Return -1
    End If
    Return 0
  End If
  IF UCase(VarName) = "SYSDIVLOOP" Then Return -1
  IF UCase(VarName) = "SYSSIGNBYTE" Then Return -1

  'Is location known and in access bank?
  If ModePIC And FinalVarList <> 0 Then
    FoundFinalVar = HashMapGet(FinalVarList, UCase(VarName))
    If FoundFinalVar <> 0 Then
      If Val(FoundFinalVar->Value) = 0 And FoundFinalVar->Value <> "0" Then
        Return 0
      Else
        Return IsNonBanked(Val(FoundFinalVar->Value))
      End If
    End If
  End If

  'User defined register vars
  'If it's a register in one sub, it will be in all
  Source = Trim(UCase(VarName))
  Temp = ""
  For CurrSub = 0 To SBC
    FoundVar = HashMapGet(@(Subroutine(CurrSub)->Variables), Source)
    IF FoundVar <> 0 THEN
      Temp = FoundVar->Pointer
      If Left(Temp, 8) = "REGISTER" Then
        Return -1
      End If
    END IF
  Next

  'Check aliases
  'May have an alias to a register
  Source = Trim(UCase(VarName))
  SourceLowByte = Source
  MinAliasSize = 1
  If Right(Source, 2) = "_H" Then
    SourceLowByte = Left(Source, Len(Source) - 2)
    MinAliasSize = 2
  ElseIf Right(Source, 2) = "_U" Or Right(Source, 2) = "_E" Then
    SourceLowByte = Left(Source, Len(Source) - 2)
    MinAliasSize = 4
  End If
  Temp = ""
  For CurrSub = 0 To SBC
    FoundVar = HashMapGet(@(Subroutine(CurrSub)->Variables), SourceLowByte)

    If FoundVar <> 0 THEN
      Temp = FoundVar->Alias
      'Have found an alias?
      If Temp <> "" Then
        If InStr(Temp, ",") <> 0 Then Temp = Trim(Left(Temp, InStr(Temp, ",") - 1))
        If Temp <> Source Then
          Return IsRegister(Temp)
        Else
          'Alias to self
          Return 0
        End If
      End If
    End If

  Next

  Return 0
End Function

FUNCTION IsString (InData As String, CurrSub As SubType Pointer) As Integer
  IsString = 0
  Dim As Integer PD

  'String constant?
  IF INSTR(InData, "$") <> 0 OR INSTR(InData, ";STRING") <> 0 THEN Return -1

  'String var?
  If TypeOfVar(InData, CurrSub) = "STRING" Then Return -1
END Function

Function IsUnaryOp (InData As String) As Integer

  If InData = "-" Then Return -1
  If InData = "!" Then Return -1
  Return 0

End Function

Sub LoadConverters
  'Loads file format converters
  Dim As String SaveDir, ConvDir, CurrFile, InLine, InName, InVal
  Dim As Integer f

  'Change to directory
  SaveDir = CurDir
  #Ifdef __FB_UNIX__
    ConvDir = ID + "/converters"
  #ELSE
    ConvDir = ID + "\converters"
  #EndIf
  If ChDir(ConvDir) = 0 Then
    'Read each .ini file in directory
    CurrFile = Dir("*.ini")
    Do While CurrFile <> ""

      'Create new FileConverter
      FileConverters += 1
      With FileConverter(FileConverters)
        .Name = CurrFile
        .InFormats = 0
        .Params = ""
        .DeleteTarget = 0

        f = FreeFile
        Open CurrFile For Input As #f
        Do While Not Eof(f)
          Line Input #f, InLine
          If InStr(InLine, "=") <> 0 Then
            InName = LCase(Trim(Left(InLine, InStr(InLine, "=") - 1)))
            InVal = Trim(Mid(InLine, InStr(InLine, "=") + 1))

            'Process line
            If InName = "desc" Then
              .Desc = InVal
            ElseIf InName = "in" Then
              GetTokens(InVal, .InFormat(), .InFormats)
            ElseIf InName = "out" Then
              .OutFormat = InVal
            ElseIf InName = "exe" Then
              .ExeName = InVal
            ElseIf InName = "params" Then
              .Params = InVal
            ElseIf InName = "deletetarget" Then
              Select Case LCase(Left(InVal, 1))
                Case "y", "t", "1": .DeleteTarget = -1
                Case "n", "f", "0": .DeleteTarget = 0
              End Select
            End If
          End If
        Loop
        Close #f
      End With

      CurrFile = Dir
    Loop

  End If

  'Restore directory
  ChDir SaveDir

End Sub

FUNCTION LocationOfSub (SubNameIn As String, SubSigIn As String, Origin As String = "", AllowVague As Integer = 0) As Integer

  'SubNameIn - name of sub to find
  'SubSigIn - signature of sub to find, leave blank to find all. If blank, may
  '           need to also set AllowVague to suppress error if multiple
  '           overloaded subs match name
  'Origin - Location sub is called from. Used for error reporting. Optional
  'AllowVague - Set to -1 to suppress error if multiple overloaded subs are
  '             found and no signature provided. First matching sub will be
  '             returned.

  'Sub name coming in can be just the name with parameters seperate or missing,
  'or can be output name (FN_READAD54, for example)

  'Returns:
  ' Location in Subroutines() of match
  ' 0 if no match
  ' -1 if overloaded, and couldn't determine which overload matches

  Dim As String SubName, SubSig, Temp, ParamTemp, ErrorTemp
  Dim As Integer T, FoundSameName, BestMatch, BestMatchLoc, ThisScore
  Dim As LinkedListElement Pointer SubElement

  SubName = UCase(LTrim(SubNameIn))
  SubSig = SubSigIn
  IF Left(SubName, 5) = "CALL " THEN SubName = Trim(Mid(SubName, 6))
  LocationOfSub = 0

  'If a var is being set, don't bother checking to see if the var is a sub
  IF Mid(SubName, INSTR(SubName, " ") + 1, 1) = "=" THEN EXIT FUNCTION

  'Remove any parameters from the name
  IF INSTR(SubName, "(") <> 0 THEN SubName = Left(SubName, INSTR(SubName, "(") - 1)
  IF INSTR(SubName, " ") <> 0 THEN SubName = Left(SubName, INSTR(SubName, " ") - 1)

  'Exit if SubName is a known, common command
  'This code is meant to save time, so no point in checking for common commands if they are not subs
  IF SubName = "IF" THEN EXIT FUNCTION
  IF SubName = "DO" THEN EXIT FUNCTION
  IF SubName = "SET" THEN EXIT FUNCTION
  IF SubName = "DIR" THEN EXIT FUNCTION
  IF SubName = "FOR" THEN EXIT FUNCTION
  IF SubName = "DIM" THEN EXIT FUNCTION
  IF SubName = "END" THEN EXIT FUNCTION
  IF SubName = "LOOP" THEN EXIT FUNCTION
  IF SubName = "NEXT" THEN EXIT FUNCTION
  IF SubName = "WAIT" THEN EXIT FUNCTION
  If Left(SubName, 5) = "ENDIF" Then Exit Function

  'Dodgy hack for SysReadString and SysCopyString to allow entry to middle of subroutine
  If SubName = "SYSREADSTRINGPART" Or SubName = "SYSCOPYSTRINGPART" Then
    SubName = Left(SubName, Len(SubName) - 4)
  End If

  'Returns the position of DataSource in SUBDATA()
  SubName = Trim(SubName)
  FoundSameName = 0

  'Prepare hash map for searching
  UpdateSubMap

  'Prepare to find best match
  BestMatch = 0
  BestMatchLoc = -2

  SubElement = HashMapGet(Subroutines, SubName)
  Do While SubElement <> 0
    T = SubElement->NumVal
    Temp = UCase(Trim(Subroutine(T)->Name))

    'Early exit if sub not overloaded or vague allowed, return first found sub
    If Not Subroutine(T)->Overloaded Or AllowVague Then
      Return T
    End If

    'Is the supplied name the same name as in output code?
    'Full name will be unique, so have found the right one
    If SubName = UCase(GetSubFullName(T)) or Left(UCase(GetSubFullName(T)),2) = "FN" Then
      Return T
    End If

    'Record that a sub was found with the right name
    FoundSameName = -1

    'Check if found sub signature matches call signature
    If SubSig <> "" Then
      ThisScore = SubSigMatch(GetSubSig(*Subroutine(T)), SubSig)
      If ThisScore > BestMatch Then
        BestMatch = ThisScore
        BestMatchLoc = T
      End If

    'No call signature supplied
    Else
      'If this happens, there is a bug in the compiler
      'Or there is a sub with no params

      'Found sub has no parameters, so it matches
      If Subroutine(T)->ParamCount = 0 Then
        Return T

      'Sub does have parameters, but call does not so cannot find the best match
      Else
        ErrorTemp = Message("DuplicateSub")
        Replace ErrorTemp, "%sub%", SubName
        LogError ErrorTemp, Origin
        Return -1
      End If
    End If

    SubElement = SubElement->Next
  Loop

  'Found same name but none with valid sig, return -1 for error
  If BestMatchLoc = -2 And FoundSameName Then
    Return -1

  'Didn't find anything with the right name
  ElseIf FoundSameName = 0 Then
    Return 0

  'Found a match that may/should work
  Else
    Return BestMatchLoc
  End If

END FUNCTION

Sub LogError(InMessage As String, Origin As String = "")

  'Error handler to issue more meaning messages to users.

  'Determine source file
    Dim F as integer
    Dim temp as string
    F = VAL(Mid(Origin, INSTR(Origin, "F") + 1)) 'File

  If UCase(Left(InMessage, Len("RAISECOMPILERERROR:"))) =  "RAISECOMPILERERROR:"  Then
        Origin = Left(Origin, Instr(Origin, "L"))+""+Mid(Origin, Instr(Origin, "S"))
        'Remove CATCH: from string
        InMessage = Mid(InMessage,Len("RAISECOMPILERERROR:")+1)
  End If

  'Handle error messages from incorrect LCD setup
  If InStr(ucase( SourceFile(F).FileName ), "LCD.H") > 0  Then
      If instr(InMessage, "SYSLCDTEMP") > 0  or _
        instr(UCASE(InMessage), "LCD_ENABLE") > 0 or _
        instr(UCASE(InMessage), Ucase(Message("BadParamsSet"))) > 0 or _
        ( instr(UCASE(InMessage), Ucase(Message("NotIONOTVALID"))) > 0 and instr(UCASE(InMessage), Ucase("LCD_EB")) )   _
        Then
          If Instr(Subroutine(GetSubID(Origin))->Name, "BACKLIGHT") > 0 Then
            InMessage = Message( "LCD_Not_Setup" ) + " " + Message("LCD_NO_BACKLITECONSTANT") 
          Else
            InMessage = Message( "LCD_Not_Setup" ) + " Calling " + Subroutine(GetSubID(Origin))->Name + "(). " + InMessage  '"LCD Parameters not setup correctly - please correct the LCD setup. " + Subroutine(GetSubID(Origin))->Name
          End If
          Origin = ""
      End if
  End if


  ErrorsFound = -1
  LogOutputMessage Origin + InMessage + "E"
End Sub

Sub LogOutputMessage(InMessage As String)

  Dim As Integer FindDup

  'Check if message is duplicate
  For FindDup = 1 To OutMessages
    If InMessage = OutMessage(FindDup) Then
      'Duplicate found, exit
      Exit Sub
    End If
  Next

  'Error if too many messages
  If OutMessages >= MAX_OUTPUT_MESSAGES - 1 Then
    'If too many warnings, just exit
    If Right(InMessage, 1) = "W" And Not WarningsAsErrors Then Exit Sub

    'If too many errors, error and quit
    If OutMessages = (MAX_OUTPUT_MESSAGES - 1) Then
      OutMessages += 1
      OutMessage(OutMessages) = Message("TooManyErrors") + "E"
    End If

    Return
  End If

  'Log message
  OutMessages += 1
  OutMessage(OutMessages) = InMessage

End Sub

Sub LogWarning(InMessage As String, Origin As String = "")
  LogOutputMessage Origin + InMessage + "W"
End Sub

Sub OptimiseCalls

  'Ensure that rcall/call and rjmp/jmp/goto are used appropriately
  'Will measure the distance between the jump and the target. If it's small
  'enough for a relative call, the non-relative jump will be replaced with a
  'relative one to make the program smaller. If there is already a relative
  'jump, but the distance is too great, a non-relative jump will be inserted
  'instead.

  'This routine is AVR and PIC18F compatible only
  If ChipFamily <> 16 And Not ModeAVR Then Exit Sub

  Dim As LinkedListElement Pointer CurrLine
  Dim As ProgLineMeta Pointer CurrMeta
  Dim As HashMap Pointer Labels
  Dim As Integer Pointer LabelPos
  Dim As String JumpTarget, ProperCmd, CheckTarget, NextLine, TempLoc
  Dim As Integer IsRelative, UseRelative, IsJump, LineSize, CurrLinePos
  Dim As Integer JumpSize, IsFirstLine
  Dim As Integer CallChanged, ProgramScans

  ProgramScans = 0
  Do
    'Get list of labels and locations in program
    CallChanged = 0
    ProgramScans += 1
    CurrLine = CompilerOutput->CodeList->Next
    CurrLinePos = 0
    Labels = HashMapCreate
    Do While CurrLine <> 0

      CurrMeta = GetMetaData(CurrLine)

      LineSize = CalcLineSize(CurrLine->Value, 1)
      CurrMeta->LineSize = LineSize
      If LineSize = 0 Then
        If ModeAVR Then
          If Right(CurrLine->Value, 1) = LabelEnd Then
            LabelPos = Callocate(SizeOf(Integer))
            *LabelPos = CurrLinePos
            HashMapSet(Labels, UCase(Left(CurrLine->Value, Len(CurrLine->Value) - 1)), LabelPos)
          End If
        ElseIf ChipFamily = 16 Then
          If CurrLine->Value <> "" And Left(CurrLine->Value, 1) <> ";" And Left(CurrLine->Value, 1) <> " " And Left(CurrLine->Value, 9) <> "PRESERVE " Then
            LabelPos = Callocate(SizeOf(Integer))
            *LabelPos = CurrLinePos
            HashMapSet(Labels, UCase(CurrLine->Value), LabelPos)
          End If
        End If
        If Left(CurrLine->Value, 5) = ".ORG " Then
          CurrLinePos = MakeDec(Trim(Mid(CurrLine->Value, 5)))
        End If
      Else
        CurrLinePos += LineSize
      End If

      CurrLine = CurrLine->Next
    Loop

    'Check program, make changes where needed
    CurrLine = CompilerOutput->CodeList->Next
    CurrLinePos = 0
    Do While CurrLine <> 0

      CurrMeta = GetMetaData(CurrLine)

      'Get current position
      If CurrLinePos = 0 Then
        IsFirstLine = -1
      End If
      If Left(CurrLine->Value, 5) = ".ORG " Then
        CurrLinePos = MakeDec(Trim(Mid(CurrLine->Value, 5)))
      End If
      'Print Hex(CurrLinePos), CurrLine->Value

      'Check to see if command is call or goto
      IsJump = 0
      If ModeAVR Then
        If Left(CurrLine->Value, 7) = " rcall " Or Left(CurrLine->Value, 6) = " call " Then
          IsJump = 1
        ElseIf Left(CurrLine->Value, 6) = " rjmp " Or Left(CurrLine->Value, 5) = " jmp " Then
          IsJump = 2
        End If
      ElseIf ChipFamily = 16 Then
        If Left(CurrLine->Value, 7) = " rcall " Or Left(CurrLine->Value, 6) = " call " Then
          IsJump = 1
        ElseIf Left(CurrLine->Value, 6) = " goto " Or Left(CurrLine->Value, 5) = " bra " Then
          IsJump = 2
        End If
      End If

      If IsJump <> 0 Then
        'Get jump target
        JumpTarget = Trim(CurrLine->Value)

        JumpTarget = Mid(JumpTarget, InStr(JumpTarget, " ") + 1)

        If InStr(JumpTarget, ";") <> 0 Then JumpTarget = Trim(Left(JumpTarget, InStr(JumpTarget, ";") - 1))
        'Check if relative
        IsRelative = 0
        If ModeAVR Then
          If Left(CurrLine->Value, 2) = " r" Then IsRelative = -1
        ElseIf ChipFamily = 16 Then
          If Left(CurrLine->Value, 2) = " r" Or Left(CurrLine->Value, 2) = " b" Then IsRelative = -1
        End If

        'If chip has 4096 or less words program memory, use r* exclusively
        'Otherwise, alter as needed
        If (ModeAVR And ChipProg <= 4096) Or (ChipFamily = 16 And ChipProg < 2048) Then
          UseRelative = -1
        Else
          UseRelative = 0

          'Get label
          CheckTarget = Trim(UCase(JumpTarget))
          'Search 2048 words either way from current location
          'If label found, then can use relative mode
          If CheckTarget = "$" Then
            UseRelative = -1

          ElseIf ModeAVR And WholeInstr(CheckTarget, "PC") = 2 Then
            TempLoc = CheckTarget
            WholeReplace TempLoc, "PC", "0"
            Calculate TempLoc
            If Abs(Val(TempLoc)) < 2048 Then
              'Print "Using relative for "; CheckTarget
              UseRelative = -1
            End If

          'For bootloader compatibility, treat first line as non-relative on all
          'PIC 18F chips.
          ElseIf IsFirstLine And ModePIC Then
            UseRelative = 0

          Else
            LabelPos = HashMapGet(Labels, CheckTarget)
            If LabelPos <> 0 Then

              JumpSize = Abs(*LabelPos - CurrLinePos)

              If (ModeAVR And JumpSize < 2048) Or (ChipFamily = 16 And JumpSize < 1024) Then
                If Instr( Ucase( CurrLine->Value), Ucase(DISABLEOPTIMISATION) ) <> 0 then
                  ' DO NOT OPTIMISE SPECIFIC CODE FOR LGT CHIPS
                  UseRelative = 0
                Else
                  UseRelative = -1
                Endif
              End If
            End If
          End If

        End If

        'If using relative when shouldn't or not when should, change line
        If IsRelative <> UseRelative Then
          CallChanged = -1
          If ModeAVR Then
            If UseRelative Then
              ProperCmd = " r"
            Else
              ProperCmd = " "
            End If
            If IsJump = 1 Then
              ProperCmd += "call "
            Else
              ProperCmd += "jmp "
            End If

          ElseIf ChipFamily = 16 Then
            If UseRelative Then
              If IsJump = 1 Then
                ProperCmd = " rcall "
              Else
                ProperCmd = " bra "
              End If
            Else
              If IsJump = 1 Then
                ProperCmd = " call "
              Else
                ProperCmd = " goto "
              End If
            End If
          End If
          CurrLine->Value = ProperCmd + JumpTarget
          CurrMeta->LineSize = CalcLineSize(CurrLine->Value, 1)
        End If

      End If

      CurrLinePos += CurrMeta->LineSize
      CurrLine = CurrLine->Next
      IsFirstLine = 0
    Loop

    HashMapDestroy(Labels)
  Loop While CallChanged
  'Print "Optimised calls in "; ProgramScans; " attempts"
End Sub

SUB OptimiseIF (CompSub As SubType Pointer = 0)
  'Optimise compiled IF statements. Replace this:
  ' conditional skip
  ' goto LABEL
  ' single instruction
  ' LABEL
  'With this:
  ' opposite conditional skip
  ' single instruction
  'This is called at the end of the program, as the final step of compilation.
  '(CompSub = 0, output code optimised)
  'If may also be called before page selection has been added. If this happens,
  'it is possible that single call instructions will become multiple instruction
  'page select, call, page select. If being called before page selection,
  'CompSub will be non-zero, and so calls should not be optimised yet.

  'Should leave labels, in case there is an else if

  Dim As String Temp, InLine, SearchLabel
  Dim As Integer T, EndIfNotUsed
  Dim As LinkedListElement Pointer CurrLine, PL(3), SearchBack

  If CompSub = 0 Then
    CurrLine = CompilerOutput->CodeList->Next
  Else
    CurrLine = CompSub->CodeStart->Next
  End If

  Do While CurrLine <> 0
    Temp = CurrLine->Value

    'Find skip > goto > other > label, replace with opposite skip, other

    IF Left(Temp, 5) = "ENDIF" THEN
      If Right(Temp, 1) = ":" THEN Temp = Left(Temp, LEN(Temp) - 1)

      'Get 3 lines before CurrLine
      PL(0) = CurrLine
      For T = 1 To 3
        If PL(T - 1) <> 0 Then PL(T) = PL(T - 1)->Prev
        'Remove comments from list
        Do While Left(PL(T)->Value, 9) = "PRESERVE "
          If PL(T)->Prev <> 0 Then
            PL(T) = PL(T)->Prev
          Else
            Exit Do
          End If
        Loop
      Next

      'Before:    After:
      '3 btfsc    btfss
      '2 goto   inst
      '1 inst
      '0 label < Looking at this

      'If compiling a subroutine, check if instruction is call
      If CompSub <> 0 Then
        If Left(PL(1)->Value, 6) = " call " Or Left(PL(1)->Value, 7) = " rcall " Then
          'Call found, need to skip
          GoTo OptimiseNextIf
        EndIf
      End If

      If ModePIC Then
        IF PL(2)->Value = " goto " + Temp Or PL(2)->Value = " bra " + Temp Then
          InLine = Left(PL(3)->Value, 6)
          IF InLine = " btfsc" THEN T = 0
          IF InLine = " btfss" THEN T = 1
          IF T = 0 THEN Replace PL(3)->Value, "btfsc", "btfss"
          IF T = 1 THEN Replace PL(3)->Value, "btfss", "btfsc"
          LinkedListDelete(PL(2))
          If Left(PL(1)->Value, 4) <> "ELSE" Then
            'CurrLine = LinkedListDelete(CurrLine)
            'Check that ENDIF isn't used
            EndIfNotUsed = -1
            SearchLabel = " " + CurrLine->Value
            SearchBack = CurrLine->Prev
            Do While SearchBack <> 0
              If Right(SearchBack->Value, Len(SearchLabel)) = SearchLabel Then
                EndIfNotUsed = 0
                Exit Do
              End If
              SearchBack = SearchBack->Prev
            Loop
            If EndIfNotUsed Then
              CurrLine = LinkedListDelete(CurrLine)
            End If
          End If
        END If

      ElseIf ModeAVR Then
        IF PL(2)->Value = " rjmp " + Temp Or PL(2)->Value = " jmp " + Temp THEN
          InLine = Left(PL(3)->Value, 5)
          T = -1
          IF InLine = " sbrc" THEN T = 0
          IF InLine = " sbrs" THEN T = 1
          IF InLine = " sbic" THEN T = 2
          IF InLine = " sbis" THEN T = 3
          IF InLine = " brbc" THEN T = 4
          IF InLine = " brbs" THEN T = 5
          IF T = 0 THEN Replace PL(3)->Value, "sbrc", "sbrs"
          IF T = 1 THEN Replace PL(3)->Value, "sbrs", "sbrc"
          IF T = 2 THEN Replace PL(3)->Value, "sbic", "sbis"
          IF T = 3 THEN Replace PL(3)->Value, "sbis", "sbic"
          IF T = 4 THEN Replace PL(3)->Value, "brbc", "brbs"
          IF T = 5 THEN Replace PL(3)->Value, "brbs", "brbc"
          If T <> -1 Then
            LinkedListDelete(PL(2))
            'CurrLine = LinkedListDelete(CurrLine)
            If Left(PL(1)->Value, 4) <> "ELSE" Then
              'CurrLine = LinkedListDelete(CurrLine)
              'Check that ENDIF isn't used
              EndIfNotUsed = -1
              SearchLabel = " " + Left(CurrLine->Value, Len(CurrLine->Value) - 1)
              SearchBack = CurrLine->Prev
              Do While SearchBack <> 0
                If Right(SearchBack->Value, Len(SearchLabel)) = SearchLabel Then
                  EndIfNotUsed = 0
                  Exit Do
                End If
                SearchBack = SearchBack->Prev
              Loop
              If EndIfNotUsed Then
                CurrLine = LinkedListDelete(CurrLine)
              End If

            End If
          End If
        END IF
      End If

      OptimiseNextIf:
    END If

    CurrLine = CurrLine->Next
  Loop

END SUB

Sub OptimiseIncrement(CompSub As SubType Pointer)
  'PIC only
  'Find uses of incf followed by btfss STATUS, Z, replace with single incfsz
  'Mainly for output of CompileCalcAddition, which would use an incfsz but must allow for bank selection
  If Not ModePIC Then Exit Sub

  Dim As LinkedListElement Pointer CurrLine

  CurrLine = CompSub->CodeStart->Next

  Do While CurrLine <> 0

    If Left(CurrLine->Value, 6) = " incf " Then
      If CurrLine->Next <> 0 Then
        If CurrLine->Next->Value = " btfss STATUS,Z" Then
          CurrLine->Value = " incfsz " + Mid(CurrLine->Value, 7)
          LinkedListDelete(CurrLine->Next)
        End If
      EndIf
    End If

    CurrLine = CurrLine->Next
  Loop

End Sub

Sub PreparePageData
  'Generate prog mem page data
  Dim As Integer PageSize, FirstPageStart, FindLastVector, LastVector, LastLoc, IsFirstPage
  Dim As Integer AvailableChipProg

  PageSize = 0
  AvailableChipProg = ChipProg - ReserveHighProg

  If ModePIC Then
    'On low end, have 512 word pages
    If ChipFamily = 12 Then
      PageSize = 512
      If HasSFR("INTCON") Then
        FirstPageStart = 5
      Else
        FirstPageStart = 0
      End If

    'Midrange, 2048 words
    'First page starts at location 5, after interrupt
    ElseIf ChipFamily = 14 Or ChipFamily = 15 Then
      PageSize = 2048
      If HasSFR("INTCON") Then
        FirstPageStart = 5
      Else
        FirstPageStart = 0
      End If

    '18F, single page
    'First page starts at location 12, after interrupt
    ElseIf ChipFamily = 16 Then
      FirstPageStart = 12

    End If
  ElseIf ModeAVR Then
    'No pages
    PageSize = 0
    'Find last interrupt vector
    LastVector = 0
    For FindLastVector = 1 To IntCount
      With Interrupts(FindLastVector)
        If .VectorLoc > LastVector Then LastVector = .VectorLoc
      End With
    Next
    FirstPageStart = LastVector + 2

  ElseIf ModeZ8 Then
    'Single page?
    PageSize = 0
    LastVector = 0
    For FindLastVector = 1 To IntCount
      With Interrupts(FindLastVector)
        If .VectorLoc > LastVector Then LastVector = .VectorLoc
      End With
    Next
    FirstPageStart = LastVector + 1
  End If
  'Generate page info
  ProgMemPage(0).PageSize = PageSize
  LastLoc = 0
  Do While LastLoc < AvailableChipProg
    ProgMemPages += 1
    With ProgMemPage(ProgMemPages)
      'Set first location in page
      If ProgMemPages = 1 Then
        .StartLoc = FirstPageStart + Bootloader
      Else
        .StartLoc = LastLoc
      End If

      'Calc and set last location
      If PageSize = 0 Then
        LastLoc = AvailableChipProg
      Else
        LastLoc += PageSize
        If LastLoc > AvailableChipProg Then LastLoc = AvailableChipProg
      End If
      .EndLoc = LastLoc - 1
      .CodeSize = 0
      .MaxSize = .EndLoc - .StartLoc + 1
      .PageSize = PageSize
    End With
  Loop

End Sub

Sub PrepareProgrammer
  'Prepare programmer tool
  'Need to select appropriate programmer for current chip model
  'If programmer specified but assembler isn't, use gcasm
  Dim As LinkedListElement Pointer ProgrammerList, CurrProg
  Dim As ExternalTool Pointer CurrTool
  Dim As String Cmd, OldCmd, Temp
  Dim As Integer RecDetect

  'Trim quotes from exe names
  AsmExe = Trim(AsmExe, Chr(34))
  PrgExe = Trim(PrgExe, Chr(34))

  'If there is a list of programmers, choose the first appropriate one
  PrgTool = 0
  If InStr(PrgExe, ",") <> 0 Then
    If VBS = 1 Then
      Temp = Message("ChoosingProgrammer")
      Print Spc(5); Temp
    End If
    ProgrammerList = GetElements(PrgExe, ",")
    CurrProg = ProgrammerList->Next
    Do While CurrProg <> 0
      CurrTool = GetTool(CurrProg->Value)
      If CurrTool <> 0 Then
        With *CurrTool
          If .UseIf = "" Then
            'Found programmer with no conditions, use
            If VBS = 1 Then
              Temp = Message("ProgrammerSelected")
              Replace Temp, "%prog%", .DispName
              Print Spc(10); Temp
            End If
            PrgTool = CurrTool
            Exit Do
          Else
            'Found programmer with conditions, check
            Cmd = Ucase(.UseIf)

'            If VBS = 1 Then
'                Print Spc(10);
'                Print "USEIF " + Cmd;
'            End If
            OldCmd = ""
            RecDetect = 0
            Do While OldCmd <> Cmd
              OldCmd = Cmd
              Cmd = ReplaceConstantsLine(CheckSysVarDef(Cmd), 0)
              RecDetect += 1
              'If have looped too many times, there is probably a recursive define
              If RecDetect > 100 Then Exit Do
            Loop
            'Change <> to ~
            If Instr( Cmd, "<>" ) > 0 then Replace Cmd, "<>","~"
'            If VBS = 1 Then
'                Print Spc(4);
'                Print " equates to '" + Cmd+"' = ";
'            End If
            Calculate Cmd
            If Val(Cmd) <> 0 Then
              'Condition is true, use programmer
              'Found programmer with no conditions, use
              If VBS = 1 Then
'                Print " = -" + Trim(Cmd)
                Print Spc(10);
                Temp = Message("ProgrammerSelected")
                Replace Temp, "%prog%", .DispName
                Print Temp
              End If

              PrgTool = CurrTool
              Exit Do
            Else

              'Found programmer with no conditions, use
              If VBS = 1 Then
                Print Spc(10);
                Temp = Message("ProgrammerSkipped")
                Replace Temp, "%prog%", .DispName
                Print Temp
              End If
            End If

          End If
        End With
      End If

      CurrProg = CurrProg->Next
    Loop

    'If PrgTool is still zero, no programmers were valid
    If PrgTool = 0 Then
      PrgExe = "none"
      LogWarning(Message("WarningNoValidProgrammer"))
    End If

  End If

  'If tool specified for assembler or programmer, use it
  AsmTool = GetTool(AsmExe)
  If AsmTool <> 0 Then
    With *AsmTool
      AsmExe = ReplaceToolVariables(.Cmd, , , AsmTool)

      AsmParams = .Params
    End With
  End If


  If PrgTool = 0 Then PrgTool = GetTool(PrgExe)
  If PrgTool <> 0 Then
    With *PrgTool
      PrgName = .DispName
      PrgExe = ReplaceToolVariables(.Cmd, , , PrgTool)
      PrgParams = .Params
      PrgDir = .WorkingDir
    End With
  End If

  'If we have a programmer but not an assembler, use gcasm
  If AsmExe = "" And PrgExe <> "" Then
    AsmExe = "gcasm"
  End If

  'Add full path to assembler and programmer names
  #IFDEF __FB_UNIX__
    If AsmExe <> "" And Left(AsmExe, 1) = "." Then AsmExe = ID + Mid(AsmExe, 2)
    If PrgExe <> "" And Left(PrgExe, 1) = "." Then PrgExe = ID + Mid(PrgExe, 2)
  #ELSE

    If AsmExe <> "" And Mid(AsmExe, 2, 1) <> ":" And UCASE(AsmExe) <> "GCASM" Then
      If instr(  UCASE(AsmExe), "PIC-AS" ) = 0 then
        If Left(AsmExe, 1) = "\" Then AsmExe = Mid(AsmExe, 2)
        AsmExe = ID + "\" + AsmExe
      End if
    End If

    If PrgExe <> "" And Mid(PrgExe, 2, 1) <> ":" Then
      If Left(PrgExe, 1) = "\" Then PrgExe = Mid(PrgExe, 2)
      PrgExe = ID + "\" + PrgExe
    End If
  #EndIf

End Sub

SUB ProcessArrays (CompSub As SubType Pointer)
  Dim As String InLine, Origin, Temp, AV, ArrayName, ArrayType, ArrayPosition
  Dim As String ArrayHandler, AppendArrayPosition, AliasLoc, OldLine
  Dim As Integer ATV, ArraysInLine, CD, SS, UseTempVar, ArrayElementSize, CurrByte
  Dim As Integer ArrayDir, L, P, ArrayPointer, NCO, MarkBlock
  Dim As LinkedListElement Pointer CurrLine, NewCodeList, NewCodeLine, LastArray, ArrayFound

  Dim As SubType Pointer MainSub
  MainSub = Subroutine(0)

  'Combine local arrays and global arrays into single list
  Dim As LinkedListElement Pointer Variables, CurrVarLoc
  Dim As VariableType Pointer CurrVar
  Variables = HashMapToList(@(MainSub->Variables))
  LinkedListInsertList(Variables, HashMapToList(@(CompSub->Variables)))
  CurrVarLoc = Variables->Next
  Do While CurrVarLoc <> 0
    'Remove any single element variables
    CurrVar = CurrVarLoc->MetaData
    If CurrVar->Size < 2 Then
      CurrVarLoc = LinkedListDelete(CurrVarLoc, 0)
    End If
    CurrVarLoc = CurrVarLoc->Next
  Loop

  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    InLine = CurrLine->Value
    'Color 1
    'Print ,, InLine
    'Color 7

    'No brackets means no arrays in line
    If Instr(InLine, "(") = 0 Then Goto CompileArraysNextLine

    'Some things will look like arrays but are not
    If (WholeINSTR(InLine, "low") = 2 And InStr(InLine, "low(") <> 0) Or (WholeINSTR(InLine, "high") = 2 And InStr(InLine, "high(") <> 0) Then Goto CompileArraysNextLine

    Origin = ""
    IF INSTR(InLine, ";?F") <> 0 THEN
      Origin = Mid(InLine, INSTR(InLine, ";?F"))
      InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
    END IF
    ATV = 0 'Number of array temporary variables

    'Mark array operation as block?
    MarkBlock = 0
    If Left(InLine, 3) = "DO " Or Left(InLine, 5) = "WAIT " Or Left(InLine, 5) = "LOOP " Then MarkBlock = -1

    'Count the number of arrays in the line
    ArraysInLine = 0
    Temp = InLine
    'Local arrays
    CurrVarLoc = Variables->Next
    Do While CurrVarLoc <> 0
      CurrVar = CurrVarLoc->MetaData
      With *CurrVar
        If .Size > 1 Then
          Do While WholeINSTR(Temp, .Name) = 2 AND INSTR(Temp, "(") <> 0
            ArraysInLine += 1
            WholeReplace Temp, "[1]" + .Name, ""
            'Print .Name, ArraysInLine, Temp
          Loop
        END IF
      End With

      CurrVarLoc = CurrVarLoc->Next
    Loop

    'Does the line contain an array?
    LastArray = Variables->Next
CheckArrayAgain:
    ArrayFound = 0
    CurrVarLoc = LastArray
    Do While CurrVarLoc <> 0
      CurrVar = CurrVarLoc->MetaData
      With *CurrVar
        If .Size > 1 Then
'WholeINSTRdebug=-1
'print origin; "{"+str(WholeINSTR(InLine, .Name ))+"} "
          IF ( WholeINSTR(InLine, .Name ) = 2 AND INSTR(InLine, "(") <> 0)  THEN
            IF WholeINSTR(InLine, .Name + "()") = 2 THEN
              ArrayFound = 0
             
              IF Instr( Inline, .Name ) <> 1 Then
                'This permits the support of  array population such as array() = 1,2,3,4....
                Temp = Message("ArrayAssignmentIncorrect")
                Replace Temp, "%Name%", .Name
                LogError Temp, Origin
              END IF
              
            Else
              ArrayFound = CurrVarLoc
              EXIT Do
            End If
          END IF
'WholeINSTRdebug=0          
        END IF
      End With

      CurrVarLoc = CurrVarLoc->Next
    Loop

    'Show error if array has not been found but brackets have
    IF ArrayFound = 0 AND INSTR(InLine, "(") <> 0 AND INSTR(InLine, "()") = 0 AND INSTR(InLine, "$") = 0 AND Left(InLine, 6) <> " retlw" THEN
      InLine = Left(InLine, INSTR(InLine, "(") - 1)
      'Get name of just array/function
      FOR SS = LEN(InLine) TO 1 STEP -1
        IF Mid(InLine, SS, 1) = " " THEN InLine = Mid(InLine, SS + 1): Exit For
      NEXT
      'High and Low are built in assembler function on AVR, ignore them
      If ModeAVR And (UCase(Trim(InLine)) = "LOW" OR UCase(Trim(InLine)) = "HIGH") Then Goto CompileArraysNextLine
      'Casting operations
      If Left(InLine, 1) = "[" And Right(InLine, 1) = "]" Then Goto CompileArraysNextLine

      'Check if brackets follow a calculation symbol, show error if they don't
      IF InLine <> "" THEN
        IF Not IsCalc(RIGHT(InLine, 1)) And InLine <> "IF" Then
          Temp = Message("ArrayNoDec")
          Replace Temp, "%Name%", InLine
          LogError Temp, Origin
        End IF
      END IF
    END IF

    'Array has been found, so generate code to access it
    IF ArrayFound <> 0 THEN

      'Print "Array found in line " + CurrLine->Value

      'Get array type
      CurrVar = ArrayFound->MetaData
      With *CurrVar
        ArrayName = .Name
        ArrayType = .Type
      End With
      If ArrayType = "STRING" Then ArrayType = "BYTE"
      ArrayElementSize = GetTypeSize(ArrayType)
      UseTempVar = -1
      If ArraysInLine = 1 And ArrayElementSize = 1 Then UseTempVar = 0

      NewCodeList = LinkedListCreate
      NewCodeLine = NewCodeList

      If MarkBlock Then
        NewCodeLine = LinkedListInsert(NewCodeLine, ";BLOCKSTART," + ArrayName)
      End If

      'Mark array as used
      CurrVar->Used = -1

      'Is array being read or set?
      ArrayDir = 0 '0 = read, 1 = set
      If UseTempVar Then
        'These commands set variables:
        IF Left(InLine, 4) = "SET " THEN ArrayDir = 1
        'IF INSTR(InLine, "=") > INSTR(InLine, ArrayName) THEN ArrayDir = 1
        IF INSTR(InLine, "=") <> 0 Then
          'If array name before =, is being set
          Temp = Left(InLine, InStr(InLine, "=") - 1)
          If WholeINSTR(Temp, ArrayName) = 2 Then ArrayDir = 1
        End If
        If LCase(Left(LTrim(InLine), 6)) = "movwf " THEN ArrayDir = 1
        'These commands read variables, but may appear to set them based on above
        IF Left(InLine, 3) = "IF " THEN ArrayDir = 0
        IF Left(InLine, 3) = "DO " THEN ArrayDir = 0
        IF Left(InLine, 5) = "WAIT " THEN ArrayDir = 0
        IF Left(InLine, 10) = "READTABLE " Then
          Temp = InLine
          Replace Temp, ",", ""
          IF INSTR(Temp, ",") < INSTR(Temp, ArrayName) THEN ArrayDir = 1
        END IF
      Else
        ArrayDir = 0
      End If

      'Get the array name and index
      Temp = Mid(InLine, WholeInstrLoc(InLine, ArrayName))

      L = 1
      P = INSTR(Temp, "(")
      Do
        P += 1
        IF Mid(Temp, P, 1) = "(" THEN L = L + 1
        IF Mid(Temp, P, 1) = ")" THEN L = L - 1
      Loop While L <> 0 AND P < LEN(Temp)
      IF L <> 0 THEN
        LogError Message("BadBrackets"), Origin
        GoTo CompileArraysNextLine
      Else
        Temp = Left(Temp, P)

        'Get array index
        ArrayPosition = Mid(Temp, INSTR(Temp, "(") + 1)
        ArrayPosition = Left(ArrayPosition, Len(ArrayPosition) - 1)
        'Print "Accessing "; ArrayName; " index "; ArrayPosition

        'Get array type (real/pointer)
        IF CurrVar->Pointer = "REAL" THEN
          ArrayHandler = "@" + ArrayName
          ArrayPointer = 0
        END IF
        IF CurrVar->Pointer = "POINTER" THEN
          ArrayHandler = "Sys" + ArrayName + "Handler"
          ArrayPointer = -1
        END If

        'Access location directly?
        If IsConst(ArrayPosition) And Not ArrayPointer Then
          'If using a fixed element of a real array, treat it as a single element variable
          'Create alias to refer to the exact memory location directly and use that
          AV = "SYS" + ArrayName + "_" + Str(MakeDec(ArrayPosition))
          AliasLoc = ""
          For CurrByte = ArrayElementSize - 1 To 0 Step -1
            If AliasLoc = "" Then
              AliasLoc = ArrayName + " + " + Str(MakeDec(ArrayPosition) * ArrayElementSize + CurrByte)
            Else
              AliasLoc += ", " + ArrayName + " + " + Str(MakeDec(ArrayPosition) * ArrayElementSize + CurrByte)
            End If
          Next
          AddVar (AV, ArrayType, 1, 0, "ALIAS:" + AliasLoc, Origin, , -1)
          'Can't put array name back into line, causes problem if array accessed twice
          WholeReplace InLine, "[1]" + Temp, TempRemove(AV)
          CurrLine->Value = InLine + Origin

        'Need to use indirect addressing for array
        Else

          'Alter line
          If UseTempVar Then
            ATV = ATV + 1
            AV = "SysArrayTemp" + Str(ATV)
            AddVar AV, ArrayType, 1, CompSub, "REAL", Origin, , -1
          Else
            If ChipFamily = 12 Or ChipFamily = 14 Then AV = "INDF"
            If ChipFamily = 15 Or ChipFamily = 16 Then AV = "INDF0"
            If ModeAVR Then AV = "SysPointerX"
          End If
          OldLine = InLine
          WholeReplace InLine, "[1]" + Temp, AV
          If OldLine = InLine Then
            'Replace failed, line is invalid
            Temp = Message("SynErr")
            LogError Temp, Origin
            CurrLine = LinkedListDelete(CurrLine)
            GoTo CompileArraysNextLine
          End If
          CurrLine->Value = InLine + Origin
          If ArrayPointer Then AddVar ArrayHandler, "WORD", 1, 0, "REAL", Origin, , -1

          'Get position to set
          If IsConst(ArrayPosition) And MakeDec(ArrayPosition) = 0 Then
            AppendArrayPosition = ""
          Else
            If IsCalc(ArrayPosition) Then
              AppendArrayPosition = "+(" + ArrayPosition + ")*" + Str(ArrayElementSize)
            Else
              AppendArrayPosition = "+" + ArrayPosition + "*" + Str(ArrayElementSize)
            End If
          End If

          'Add code to read/set array
          If ModePIC And (ChipFamily = 12 Or ChipFamily = 14) Then
            If ArrayPointer = 0 Then
              NewCodeLine = LinkedListInsert(NewCodeLine, "FSR = [byte]" + ArrayHandler + AppendArrayPosition + Origin)
              If HighFSR Then
                NewCodeLine = LinkedListInsert(NewCodeLine, " bankisel " + ArrayName)
              End If
            Else
              NewCodeLine = LinkedListInsert(NewCodeLine, "FSR = " + ArrayHandler + AppendArrayPosition + Origin)
              If HighFSR Then
                NewCodeLine = LinkedListInsert(NewCodeLine, " bcf STATUS, IRP")
                NewCodeLine = LinkedListInsert(NewCodeLine, " btfsc " + ArrayHandler + "_H,0")
                NewCodeLine = LinkedListInsert(NewCodeLine, " bsf STATUS, IRP")
              End If
            End If
            If UseTempVar Then
              For CurrByte = 0 To ArrayElementSize - 1
                If ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, GetByte(AV, CurrByte) + " = INDF")
                IF ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "INDF = " + GetByte(AV, CurrByte))
                If CurrByte < ArrayElementSize - 1 Then
                  NewCodeLine = LinkedListInsert(NewCodeLine, " incf FSR,F")
                End If
              Next
            End If

          ElseIf ModePIC And ChipFamily = 15 Then
            NewCodeLine = LinkedListInsert(NewCodeLine, "AFSR0 = " + ArrayHandler + AppendArrayPosition + Origin)
            If UseTempVar Then
              For CurrByte = 0 To ArrayElementSize - 1
                If ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, GetByte(AV, CurrByte) + " = INDF0")
                If ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "INDF0 = " + GetByte(AV, CurrByte))
                If CurrByte < ArrayElementSize - 1 Then
                  NewCodeLine = LinkedListInsert(NewCodeLine, " addfsr 0,1")
                End If
              Next
            End If

          ElseIf ModePIC And ChipFamily = 16 Then
            If ArrayPointer = 0 Then
              If ArrayElementSize > 1 Then
                If IsCalc(ArrayPosition) Then
                  ArrayPosition = "(" + ArrayPosition + ")*" + Str(ArrayElementSize)
                Else
                  ArrayPosition += "*" + Str(ArrayElementSize)
                End If
              End If
              If IsConst(ArrayPosition) Then
                NewCodeLine = LinkedListInsert(NewCodeLine, " lfsr 0," + ArrayName + "+" + ArrayPosition)
              Else
                NewCodeLine = LinkedListInsert(NewCodeLine, " lfsr 0," + ArrayName)
                NewCodeLine = LinkedListInsert(NewCodeLine, "AFSR0 = AFSR0 +" + ArrayPosition + Origin)
              End If
            Else
              NewCodeLine = LinkedListInsert(NewCodeLine, "AFSR0 = " + ArrayHandler + AppendArrayPosition + Origin)
            End If
            If UseTempVar Then
              For CurrByte = 0 To ArrayElementSize - 1
                If ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, GetByte(AV, CurrByte) + " = POSTINC0")
                If ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "POSTINC0 = " + GetByte(AV, CurrByte))
              Next
            End If

          ElseIf ModeAVR Then
            AddVar "SysStringA", "WORD", 1, 0, "REAL", Origin, , -1
            If ArrayPointer = 0 Then
              NewCodeLine = LinkedListInsert(NewCodeLine, "SysStringA = @" + ArrayName + AppendArrayPosition + Origin)
            Else
              NewCodeLine = LinkedListInsert(NewCodeLine, "SysStringA = " + ArrayHandler + AppendArrayPosition + Origin)
            End If
            If UseTempVar Then
              For CurrByte = 0 To ArrayElementSize - 1
                If ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, GetByte(AV, CurrByte) + " = SysPointerXInc")
                IF ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "SysPointerXInc = " + GetByte(AV, CurrByte))
              Next
            End If
          End If

        END IF

        If MarkBlock Then
          NewCodeLine = LinkedListInsert(NewCodeLine, ";BLOCKEND," + ArrayName)
        End If

        'Add array access code
        'Add after line when ArrayDir = 1, before when = 0
        If ArrayDir = 0 Then
          LinkedListInsertList(CurrLine->Prev, NewCodeList)
        Else
          LinkedListInsertList(CurrLine, NewCodeList)
        End If
      End If

      'Check the line again to see if it contains another array
      LastArray = ArrayFound
      GoTo CheckArrayAgain
    END IF

  CompileArraysNextLine:
    CurrLine = CurrLine->Next
  Loop

  'Deal with any missed names
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    'Replace tokens with names
    CurrLine->Value = ReplaceFnNames(CurrLine->Value)
    CurrLine = CurrLine->Next
  Loop

END SUB

Function PutInRegister(ByRef OutList As LinkedListElement Pointer, SourceValue As String, RegType As String, Origin As String) As String
  'Take any value, and put it into a register
  'Return name of register
  Dim As String Temp, OutVar, CurrRegType
  Dim As Integer Overwrite, ForceHighReg, UpgradeType

  'Try to create new list
  If OutList = 0 Then OutList = LinkedListCreate

  Overwrite = 0
  ForceHighReg = 0
  Do While Mid(SourceValue, 2, 1) = ":"
    'Check if register will be overwritten
    If Left(SourceValue, 2) = "O:" Then
      SourceValue = Mid(SourceValue, 3)
      Overwrite = -1
    End If
    'Check if register needs to be high
    If Left(SourceValue, 2) = "H:" Then
      SourceValue = Mid(SourceValue, 3)
      ForceHighReg = -1
    End If
  Loop

  'AVR Only
  If Not ModeAVR Then
    Return SourceValue
  End If

  'If no type, decide one
  CurrRegType = TypeOfValue(SourceValue, Subroutine(GetSubID(Origin)))
  If RegType = "" Then RegType = CurrRegType

  'Might need to upgrade type
  UpgradeType = 0
  If CastOrder(RegType) > CastOrder(CurrRegType) Then UpgradeType = -1

  'If already a register, then return name
  If IsRegister(SourceValue) And Not Overwrite And Not ForceHighReg Then
    'Might need to upgrade type of register
    If UpgradeType Then
      IF Left(UCase(SourceValue), 7) = "SYSTEMP" Then
        'Need to upgrade neatly
        If CurrRegType = "BYTE" And RegType = "WORD" Then
          LinkedListInsert(OutList, " clr " + GetByte(SourceValue, 1))
        End If
        UpgradeCalcVar SourceValue, RegType
        Return SourceValue
      End If
    Else
      Return SourceValue
    End If
  End If

  'Create temp var
  Temp = ""
  If IsConst(SourceValue) Or ForceHighReg Then Temp = "H:"
  OutVar = GetCalcVar(Temp + RegType)

  'Copy value
  'OutList = CompileVarSet(SourceValue, OutVar, Origin)
  LinkedListInsertList(OutList, CompileVarSet(SourceValue, OutVar, Origin))
  Return OutVar

End Function

Sub ReadPICASChipData

  'Get filename
#IFDEF __FB_UNIX__
  'not supported
#ELSE

  'read the INC in and put into ARRAY   ReverseIncFileLookup(
  dim as string InLine, tmpfilename
  dim as integer ffile, fresult,findex
  dim as string SearchExe
  dim as integer F, Result, ChipSupported

'ChipPICASConfigFile,ChipPICASRoot

  ChipPICASRoot = Left( AsmExe, InStrRev( ReplaceToolVariables(AsmExe,,, AsmTool), "\"))
  ChipPICASRoot = Left( ChipPICASRoot , InStrRev( ChipPICASRoot , "\")-1)
  ChipPICASRoot = Left( ChipPICASRoot , InStrRev( ChipPICASRoot , "\")-1)
  ChipPICASDataFile = Left( ChipPICASRoot , InStrRev( ChipPICASRoot , "\"))+"pic\include\proc\pic"+LCase(ChipName) + ".inc"
  ChipPICASDataFile = mid(ChipPICASDataFile, 2)

  ChipPICASConfigFile = Left( ChipPICASRoot , InStrRev( ChipPICASRoot , "\"))+"pic\dat\cfgmap\"+LCase(ChipName) + ".cfgmap"
  ChipPICASConfigFile = mid( ChipPICASConfigFile , 2)

  SearchExe =  AsmExe
  ReplaceAll ( SearchExe, Chr(34), "" )
  if trim(dir(SearchExe)) = "" Then
    LogError Message("PICASAssemblerNotFound")
    LogError ChipPICASRoot+"\bin\pic-as.exe " + Message( "PICASNotFound" )
    
    WriteErrorLog
    ErrorsFound = - 1
    end
  end If

  'Now check if chip is supported
  tmpfilename = FI
  replace ( tmpfilename, ".GCB", ".tmp")
  kill tmpfilename
  F = FreeFile
  
  'Execute the PIC-AS compiler to provide a list of the chips
  Result = Shell( chr(34)+chr(34)+SearchExe+chr(34)+" -mprint-devices > " + chr(34) + tmpfilename + chr(34) + chr(34) )
  Open tmpfilename For Input As #F
  ChipSupported = 0

  DO WHILE NOT EOF(F)
    LINE INPUT #F, InLine
    If Instr( Ucase(Inline), Ucase(ChipName) ) > 0 Then
        ChipSupported = -1
    End if
  Loop
  Close #F
  kill tmpfilename
  if ChipSupported = 0 Then
    LogError Message("PICASChipNotSupported")
    WriteErrorLog
    ErrorsFound = - 1
    End
  End if

 
  'now check that the chip data file is present
    findex = 0
    ' Find the first free file number.
    ffile = FreeFile
     if trim(dir(ChipPICASDataFile)) = "" Then
        LogError Message("PICASChipNotSupported")
        WriteErrorLog
        ErrorsFound = - 1
        end
    End if

    fresult = OPEN ( ChipPICASDataFile For Input As ffile )
    If fresult <> 0 then
        LogError Message("PICASNotFound")
        WriteErrorLog
        ErrorsFound = - 1
        end
    End if

    DO WHILE NOT EOF(ffile)
      LINE INPUT #ffile, InLine
      if left( InLine,7) =  "#define" and instr( inline, "BANKMASK") = 0  <> 0 then
        'read next line
        LINE INPUT #ffile, InLine

        If instr( ucase(inline), "EQU") > 0 then
          ReverseIncFileLookup( findex ).Value = trim(mid(inline, 1, instr(inline," equ ")-5))
          ReverseIncFileLookup( findex ).NumVal = val("&h"+trim( mid( inline,instr(inline," equ ")+5,  InStr( inline,chr(34) ) -1 )  ))
          findex = findex +1
        end if
      end if

    LOOP
    Close #ffile


   'Check that the cfg data is present
    findex = 0
    ' Find the first free file number.
    ffile = FreeFile
    fresult = OPEN ( ChipPICASConfigFile For Input As ffile )
    DO WHILE NOT EOF(ffile)
      LINE INPUT #ffile, InLine
      if instr( inline, ":") <> 0 and instr( inline, "=") <> 0 then
         'read next line
         'PLLSEL_PLL3X:PLLSEL=PLL3X
         ReverseCfgFileLookup( findex ).Value = trim(mid(inline, instr(inline,":")+1, instr(inline,"=")-instr(inline,":")-1 ))
         ReverseCfgFileLookup( findex ).State = trim(mid(inline, instr(inline,"=")+1))
         ReverseCfgFileLookup( findex ).Config = ReverseCfgFileLookup( findex ).Value+"="+ReverseCfgFileLookup( findex ).State
         '  ReverseCfgFileLookup( findex ).Config

         findex = findex +1

      end if

    LOOP
    Close #ffile


   'AS inc file not found, show error and quit
  '  LogError Message("PICASChipNotSupported")

#ENDIF

End Sub

SUB ReadChipData
  Dim As String TempData, TempList(20)
  Dim As String ReadDataMode, ChipDataFile, InLine, SFRName
  Dim As String PinName, PinDir, ConstName, ConstValue
  Dim As PinDirType Pointer PinDirData
  Dim As Integer TDC, CW, PD, FirstSFR, FindSFR
  Dim As Integer SFRLoc
  Dim As ConfigSetting Pointer ThisSetting
  Dim As SysVarType Pointer NewSysVar
  Dim As LinkedListElement Pointer CurrLoc
  Dim As AsmCommand Pointer NewAsmCommand, FindAsmCommand

  'Get filename
#IFDEF __FB_UNIX__
  ChipDataFile = ID + "/chipdata/" + LCase(ChipName) + ".dat"
#ELSE
  ChipDataFile = ID + "\chipdata\" + ChipName + ".dat"
#ENDIF

      'Check that the chip data is present
      If OPEN(ChipDataFile For Input As #1) <> 0 Then
        'Chip file not found. If ChipName contains LF, try loading F file
        If InStr(LCase(ChipName), "lf") <> 0 Then
          TempData = ChipName
          Replace TempData, "lf", "f"

          #IFDEF __FB_UNIX__
            ChipDataFile = ID + "/chipdata/" + LCase(TempData) + ".dat"
          #ELSE
            ChipDataFile = ID + "\chipdata\" + TempData + ".dat"
          #EndIf
          If Open(ChipDataFile For Input As #1) = 0 Then GoTo ChipDataFileOpened
        End If

        'Chip data still not found, show error and quit
        LogError Message("ChipNotSupported")
        WriteErrorLog
        END
      End If
  ChipDataFileOpened:
  IF VBS = 1 THEN PRINT SPC(10); ChipDataFile

  FirstSFR = &HFFFF
  ChipFamilyVariant = 0
  ChipMinimumBankSelect = 0

  ReadDataMode = ""
  DO WHILE NOT EOF(1)
    LINE INPUT #1, InLine
    InLine = Trim(LCase(InLine))
    IF InLine = "" THEN Goto ReadNextChipInfoLine
    IF Left(InLine, 1) = "'" THEN Goto ReadNextChipInfoLine
    IF Left(InLine, 1) = ";" THEN Goto ReadNextChipInfoLine
    IF Left(InLine, 2) = "\\" THEN Goto ReadNextChipInfoLine

    IF Left(InLine, 1) = "[" AND Right(InLine, 1) = "]" THEN
      ReadDataMode = InLine

    'Chip Data
    ElseIF ReadDataMode = "[chipdata]" AND INSTR(InLine, "=") <> 0 THEN
      TempData = Trim(Mid(InLine, INSTR(InLine, "=") + 1))
      ConstName = Trim(Left(InLine, INSTR(InLine, "=") - 1))
      ConstValue = TempData

      'Set appropriate internal variable
      Select Case ConstName
        Case "prog": ChipProg = VAL(TempData): ConstName = "WORDS"
        Case "eeprom": ChipEEPROM = Val(TempData)
        Case "ram": ChipRam = VAL(TempData)
        Case "i/o": ChipIO = Val(TempData): ConstName = "IO"
        Case "adc": ChipADC = Val(TempData)
        Case "maxmhz": ChipMaxSpeed = Val(TempData)
        Case "intosc"
          'ChipIntOsc = Val(TempData)
          IntOscSpeeds = 0
          Do While InStr(TempData, ",") <> 0
            IntOscSpeeds += 1
            IntOscSpeed(IntOscSpeeds) = Val(TempData)
            TempData = Trim(Mid(TempData, InStr(TempData, ",") + 1))
          Loop
          If TempData <> "" Then
            If Val(TempData) <> 0 Then
              IntOscSpeeds += 1
              IntOscSpeed(IntOscSpeeds) = Val(TempData)
            End If
          End If
          ConstValue = Str(IntOscSpeed(1))
        Case "pins": ChipPins = Val(TempData)
        Case "family": ChipFamily = Val(TempData)
        Case "familyvariant": ChipFamilyVariant = Val(TempData)
        Case "subfamily": ChipSubFamily = Val(TempData)
        Case "configwords": ConfWords = VAL(TempData)
        Case "psp": PSP = VAL(TempData)
        Case "maxaddress": MemSize = VAL(TempData)
        Case "gpr": ChipGPR = Val(TempData)
        Case "hardwaremult":
          HMult = 0: If TempData = "y" Then HMult = -1
          ConstValue = Str(-HMult)
        Case "nottested"
          If overridetestdatfilemessage = 0 then 
            If ( val(TempData) and 1 ) = 1 Then
              LogWarning "This is a development chip definition file (.DAT) and the chip has not been validated by the developers of the compiler. "
              LogWarning "    There may be errors in the ASM and/or the generated HEX file - please be aware that the libraries may or may not support this chip."+chr(13)+chr(10)
            End If

            If ( val(TempData) and 2 ) = 2 Then
              LogWarning "This is a development chip definition file (.DAT) and the chip and the interrupt section of the DAT is not completed."
              LogWarning "    Please resolve the interrupt section and contact the developers via the GCBASIC forum"+chr(13)+chr(10)
            End If
          End if
        Case "programmername":
          ChipProgrammerName = TempData

        Case "31ksupport"
        '31kSupport is exposed as Chip31kConfig, Chip31kRegister & Chip31kValue
        'Read four parameters

          _31kSupported.State = 0
          Do While _31kSupported.State <= 3

            _31kSupported.State += 1

            Select Case _31kSupported.State

              Case 1:
                _31kSupported.OSCType = Trim(Mid(TempData, 1, InStr(TempData, ",") - 1))
                AddConstant("CHIP" + UCase("31kConfig"), UCASE(_31kSupported.OSCType))

              Case 2:
                _31kSupported.OSCRegister = Trim(Mid(TempData,1, InStr(TempData, ",") - 1))
                AddConstant("CHIP" + UCase("31kRegister"), UCASE(_31kSupported.OSCRegister))

              Case 3:
                _31kSupported.Value = Val(TempData)
                AddConstant("CHIP" + UCase("31kValue"), str(_31kSupported.Value))

            End Select
            TempData = Trim(Mid(TempData, InStr(TempData, ",") + 1))
          Loop
          _31kSupported.State = -1

        Case "minimumbankselect": ChipMinimumBankSelect = Val(TempData)

        Case "memorylock": AddConstant("CHIP" + UCase(ConstName), ucase(ConstValue) )
        Case "avrdx": AddConstant("CHIP" + UCase(ConstName), ucase(ConstValue) ): ChipAVRDX = -1
      End Select

      'Create constant for data item
      if ConstName <> "memorylock" then
              AddConstant("CHIP" + UCase(ConstName), ConstValue)
      end if

    'Interrupts
    ElseIf ReadDataMode = "[interrupts]" AND INSTR(InLine, ":") <> 0 THEN
      IntCount += 1
      With Interrupts(IntCount)
        .EventName = Trim(Left(InLine, INSTR(InLine, ":") - 1))
        TDC = 0
        TempData = Trim(Mid(InLine, INSTR(InLine, ":") + 1))
        Do While Instr(TempData, ",") <> 0
          TDC += 1: TempList(TDC) = TRIM(Left(TempData, Instr(TempData, ",") - 1))
          TempData = Trim(Mid(TempData, Instr(TempData, ",") + 1))
        Loop
        If TempData <> "" Then
          TDC += 1: TempList(TDC) = TempData
        End If

        If ModePIC Then
          .Vector = ""
          .VectorLoc = 0
          .EnableBit = TempList(1)
          .FlagBit = TempList(2)
          .Handler = ""

        ElseIf ModeAVR Then
          .Vector = TempList(1)
          .VectorLoc = Val(TempList(2))
          .EnableBit = TempList(3)
          .FlagBit = TempList(4)
          .Handler = ""

        ElseIf ModeZ8 Then
          .Vector = TempList(1)
          .VectorLoc = Val(TempList(2))
          .EnableBit = TempList(3)
          .FlagBit = TempList(4)
          .Handler = ""

        End If

      End With

    'AVR alaises
    ElseIf ReadDataMode = "[avralias]" AND INSTR(InLine, "=") <> 0 THEN
      InLine = UCase(InLine)
      SFRName = Trim(Left(InLine, INSTR(InLine, "=") - 1))
      SFRLoc = Val(Trim(Mid(InLine, INSTR(InLine, "=") + 1)))
      MakeSFR(SFRName, SFRLoc, -1 )

    'Registers
    ElseIf ReadDataMode = "[registers]" AND INSTR(InLine, ",") <> 0 THEN
      InLine = UCase(InLine)
      SFRName = Trim(Left(InLine, INSTR(InLine, ",") - 1))
      SFRLoc = Val(Trim(Mid(InLine, INSTR(InLine, ",") + 1)))
      MakeSFR(SFRName, SFRLoc, 0)

      'On 18F, need to find lowest SFR location
      If ChipFamily = 16 Then
        If SFRLoc < FirstSFR Then
          FirstSFR = SFRLoc
        End If
      End If

    'Bits
    ' 1 = name 3 = parent
    ' In file: name, parent, location
    ElseIf ReadDataMode = "[bits]" AND INSTR(InLine, ",") <> 0 THEN
      NewSysVar = Callocate(SizeOf(SysVarType))
      InLine = UCase(InLine)
      NewSysVar->Name = Trim(Left(InLine, INSTR(InLine, ",") - 1))
      TempData = Trim(Mid(InLine, INSTR(InLine, ",") + 1))
      NewSysVar->Parent = Trim(Left(TempData, INSTR(TempData, ",") - 1))
      NewSysVar->Location = Val(Trim(Mid(TempData, INSTR(TempData, ",") + 1)))
      HashMapSet(SysVarBits, NewSysVar->Name, NewSysVar)

    'FreeRAM
    ElseIf ReadDataMode = "[freeram]" AND INSTR(InLine, ":") <> 0 THEN
      MRC = MRC + 1
      MemRanges(MRC) = InLine

    'No bank RAM
    ElseIf ReadDataMode = "[nobankram]" And InStr(InLine, ":") <> 0 Then
      NoBankLocs += 1
      With NoBankLoc(NoBankLocs)
        .StartLoc = Val("&H" + Left(InLine, InStr(InLine, ":") - 1))
        .EndLoc = Val("&H" + Mid(InLine, InStr(InLine, ":") + 1))
      End With

    'Pinout data
    ElseIf Left(ReadDataMode, 6) = "[pins-" Then
      'Get first pin function and data direction from line
      TempData = Mid(InLine, InStr(InLine, ",") + 1)
      If InStr(TempData, ",") <> 0 Then
        TempData = Trim(Left(TempData, InStr(TempData, ",") - 1))
      End If
      'Split pin name and pin direction
      If InStr(TempData, "(") <> 0 Then
        PinName = Left(TempData, InStr(TempData, "(") - 1)
        PinDir = Mid(TempData, InStr(TempData, "(") + 1)
        If InStr(PinDir, ")") <> 0 Then
          PinDir = Left(PinDir, InStr(PinDir, ")") - 1)
        End If
      Else
        PinName = TempData
        PinDir = "P"
      End If

      If IsIOPinName(PinName) Then
        If Left(PinName, 2) = "gp" Then
          PinName = "GPIO." + Mid(PinName, 3)
        Else
          PinName = "PORT" + Mid(PinName, 2, 1) + "." + Mid(PinName, 3, 1)
        End If
        PinDirData = GetPinDirection(PinName)
        If PinDirData <> 0 Then
          PinDirData -> AllowedDirections = PinDir
        End If
      End If

    'Config
    ElseIF ReadDataMode = "[config]" THEN
      InLine = UCase(InLine)
      If INSTR(InLine, ":") <> 0 THEN InLine = Left(InLine, INSTR(InLine, ":") - 1)
      IF INSTR(InLine, ",") <> 0 THEN
        COC = COC + 1
        With ConfigOps(COC)
          .Op = Trim(Left(InLine, INSTR(InLine, ",") - 1))
          InLine = Trim(Mid(InLine, INSTR(InLine, ",") + 1))
          .Loc = VAL(InLine)
          IF INSTR(InLine, ",") <> 0 THEN
            .Val = VAL(Trim(Mid(InLine, INSTR(InLine, ",") + 1)))
          END IF
        End With
      END IF

    'Config options
    ElseIf ReadDataMode = "[configops]" Then
      InLine = UCase(InLine)
      If InStr(InLine, "=") <> 0 Then
        'Create new option
        ThisSetting = Callocate(SizeOf(ConfigSetting))
        LinkedListInsert(ConfigSettings, ThisSetting)
        ThisSetting->Options = LinkedListCreate
        CurrLoc = ThisSetting->Options

        With (*ThisSetting)
          'Get option name
          .Name = Trim(Left(InLine, InStr(InLine, "=") - 1))

          'Get option values
          TempData = Trim(Mid(InLine, InStr(InLine, "=") + 1))
          Do While InStr(TempData, ",") <> 0
            CurrLoc = LinkedListInsert(CurrLoc, Trim(Left(TempData, InStr(TempData, ",") - 1)))
            TempData = Trim(Mid(TempData, InStr(TempData, ",") + 1))
          Loop
          If TempData <> "" Then
            CurrLoc = LinkedListInsert(CurrLoc, TempData)
          End If

        End With
      End If

    'Default config settings (18 only)
    ElseIF ReadDataMode = "[asmconfig]" THEN
      DCOC = DCOC + 1
      DefCONFIG(DCOC) = UCase(InLine)

    'Config masks (18 only)
    ElseIF ReadDataMode = "[configmask]" THEN
      ConfWords += 1
      ConfigMask(ConfWords) = VAL(InLine)

    END IF

    ReadNextChipInfoLine:
  LOOP
  CLOSE




  'Misc initialisation
  If ModePIC Then
    'Calculate the number of high PC bits
    PCUpper = 0
    IF ChipFamily = 12 Then
      If ChipProg > 512 THEN PCUpper = 1
      If ChipProg > 1024 THEN PCUpper = 2
    ElseIf ChipFamily = 14 Or ChipFamily = 15 Then
      If ChipProg > 2048 THEN PCUpper = 1
      IF ChipProg > 4096 THEN PCUpper = 2
    END If

    'If there are no bank free ram locations defined, guess them
    'Typically expect 0:F on 12 bit, 70:7F on 14, and 0:5F on 16
    If NoBankLocs = 0 Then
      Select Case ChipFamily
        Case 12:
          NoBankLocs = 1
          With NoBankLoc(1)
            .StartLoc = 0
            .EndLoc = &HF
          End With
        Case 14, 15:
          NoBankLocs = 1
          With NoBankLoc(1)
            .StartLoc = &H70
            .EndLoc = &H7F
          End With
        Case 16:
          If MemSize >= 4096 Then
            '18F with larger memory ('K42 and others)
            'Guess where SFR access bank starts
            'Not earlier than 0xF60, but can be later (0xF80 on 18F2620, for example)
            If FirstSFR < &H3F60 Then FirstSFR = &H3F60

            NoBankLocs = 2
            With NoBankLoc(1)
              .StartLoc = 0
              .EndLoc = (FirstSFR - 1) And 255
            End With
            'Access RAM for SFRs
            With NoBankLoc(2)
              .StartLoc = FirstSFR
              .EndLoc = &H3FFF
            End With

          Else
            'Older 18F with smaller RAM
            'Guess where SFR access bank starts
            'Not earlier than 0xF60, but can be later (0xF80 on 18F2620, for example)
            If FirstSFR < &HF60 Then FirstSFR = &HF60

            'Assume that SFR access bank starts at a location that is a multiple of 16
            FirstSFR = FirstSFR And &HFF0

            NoBankLocs = 2
            With NoBankLoc(1)
              .StartLoc = 0
              .EndLoc = (FirstSFR - 1) And 255
            End With
            'Access RAM for SFRs
            With NoBankLoc(2)
              .StartLoc = FirstSFR
              .EndLoc = &HFFF
            End With
          End If

      End Select

      'If on an 18F and there is only one set of non-banked locations, add SFR
      If ChipFamily = 16 And NoBankLocs = 1 Then
        NoBankLocs = 2
        With NoBankLoc(2)
          .StartLoc = NoBankLoc(1).StartLoc + &HF01
          .EndLoc = &HFFF
        End With
      End If
    End If

  ElseIf ModeAVR Then

  End If

  'Load assembly commands

  #IFDEF __FB_UNIX__
    ChipDataFile = ID + "/chipdata/core" + Str(ChipFamily) + ".dat"
  #Else
    ChipDataFile = ID + "\chipdata\core" + Str(ChipFamily) + ".dat"
  #ENDIF
  If OPEN(ChipDataFile For INPUT AS #1) <> 0 Then
    TempData = Message("MissingCoreFile")
    Replace TempData, "%core%", ChipDataFile
    Replace TempData, "%chip%", ChipName
    LogError TempData
    WriteErrorLog
    END
  End If

  DO WHILE NOT EOF(1)
    LINE INPUT #1, InLine
    InLine = Trim(UCase(InLine))
    IF InLine <> "" AND Left(InLine, 1) <> "'" THEN
      DO WHILE INSTR(InLine, Chr(9)) <> 0: Replace InLine, Chr(9), " ": LOOP

      NewAsmCommand = Callocate(SizeOf(AsmCommand))

      With *NewAsmCommand

        'Get syntax and number of words
        .Syntax = Trim(Left(InLine, INSTR(InLine, ";") - 1))
        InLine = Mid(InLine, INSTR(InLine, ";") + 1)
        .Words = VAL(Trim(Left(InLine, INSTR(InLine, ";") - 1)))
        InLine = Mid(InLine, INSTR(InLine, ";") + 1)

        'Get binary instruction words and applicable variants
        CW = 0
        Do While INSTR(InLine, ";") <> 0
          If CW < .Words Then
            CW += 1
            .Word(CW) = TRIM(Left(InLine, INSTR(InLine, ";") - 1))
          Else
            .FamilyVariants += 1
            .FamilyVariant(.FamilyVariants) = Val(TRIM(Left(InLine, INSTR(InLine, ";") - 1)))
          End If
          InLine = Trim(Mid(InLine, INSTR(InLine, ";") + 1))
        Loop
        If InLine <> "" Then
          If CW < .Words Then
            CW += 1
            .Word(CW) = InLine
          Else
            .FamilyVariants += 1
            .FamilyVariant(.FamilyVariants) = Val(InLine)
          End If
        End If

        'Get command name
        .Cmd = Trim(UCase(.Syntax))
        If INSTR(.Cmd, " ") <> 0 THEN .Cmd = Trim(Left(.Cmd, INSTR(.Cmd, " ") - 1))

        'Get parameters
        Dim As String ParamName
        ParamName = .Syntax
        IF INSTR(ParamName, " ") = 0 Then
          ParamName = ""
        Else
          ParamName = Trim(Mid(ParamName, INSTR(ParamName, " ") + 1))
        End If
        GetTokens (ParamName, .Param(), .Params, ",")

      End With

      'Is command with same name already in hashmap?
      FindAsmCommand = HashMapGet(ASMCommands, NewAsmCommand->Cmd)
      If FindAsmCommand <> 0 Then
        'Yes, so add new command to existing one
        Do While FindAsmCommand->Alternative <> 0
          FindAsmCommand = FindAsmCommand->Alternative
        Loop
        FindAsmCommand->Alternative = NewAsmCommand
      Else
        'No, so add to map normally
        HashMapSet(ASMCommands, NewAsmCommand->Cmd, NewAsmCommand)
      End If

    END If
  Loop
  CLOSE

  'Check for presence of High FSR bit
  HighFSR = 0
  If ChipFamily = 12 Or ChipFamily = 14 Then
    HighFSR = HasSFRBit("IRP")
  End If

  'Check for LAT registers
  If ChipFamily <> 15 And ChipFamily <> 16 Then
    UseChipOutLatches = 0
  ElseIf Not (HasSFR("LATA") OrElse HasSFR("LATB") OrElse HasSFR("LATC")) Then
    UseChipOutLatches = 0
  End If

  'Ensure GIE is switched in glitch free mode
  If ModePIC Then
    NewSysVar = HashMapGet(SysVarBits, "GIE")
    If NewSysVar <> 0 Then
      HashMapSet(GlitchFreeOutputs, NewSysVar->Parent + "." + Str(NewSysVar->Location), "y")
    End If
  End If

  'Check for high table pointer bytes
  HighTBLPTRBytes = 0
  If ChipFamily = 16 Then
    'The Q10 has SECTOR RAM and therefore "TBLPTRU" must be managed
    If ( ChipProg > 32767 or  ( Instr( UCase(ChipName) , "Q10") <> 0 ) ) And HasSFR("TBLPTRU") Then
      HighTBLPTRBytes = 3
    Else
      HighTBLPTRBytes = 2
    End If
  End If

End Sub

Sub ReadOptions(OptionsIn As String)
  'Process #option statements

  Dim As String OutMessage, VolatileVar, RequiredVar
  Dim As LinkedListElement Pointer OptionElements, CurrElement
  Dim As String VarName, VarBit, MandatedStringMessage
  Dim As SysVarType Pointer SysVarBit
  Dim As Integer VarBitNo, TempVal

  'Set defaults
  Bootloader = 0

  'Get settings
  OptionElements = GetElements(OptionsIn, " ,")

  CurrElement = OptionElements->Next
  Do While CurrElement <> 0

    'Ignore Explicit (should have been read earlier)
    If CurrElement->Value = "EXPLICIT" Then
      'Do nothing

    'Get bootloader setting
    ElseIf CurrElement->Value = "BOOTLOADER" Then
      If  CurrElement->Next <> 0 Then
        If IsConst(CurrElement->Next->Value) Then
          Bootloader = MakeDec(CurrElement->Next->Value)
          CurrElement = CurrElement->Next
        End If
      End If

    'Reserve memory at the end of flash for HEF/bootloader?
    ElseIf CurrElement->Value = "RESERVEHIGHPROG" Then

      If CurrElement->Next <> 0 Then
        If IsConst(CurrElement->Next->Value) Then
          TempVal = Val(CurrElement->Next->Value)
        Else 
          TempVal = Val(HashMapGetStr(Constants, UCase(CurrElement->Next->Value)))
        End if
        If TempVal > ReserveHighProg Then
          ReserveHighProg = TempVal
        End If

        CurrElement = CurrElement->Next
      End If

    'Disable automatic use of output latches?
    ElseIf CurrElement->Value = "NOLATCH" Then
      UseChipOutLatches = 0

    'Disable automatic interrupt context save/restore?
    ElseIf CurrElement->Value = "NOCONTEXTSAVE" Then
      AutoContextSave = 0

    'Disable config (Used for TinyBootloader Support, possibly others)
    ElseIf CurrElement->Value = "NOCONFIG" Then
      ConfigDisabled = -1

    'Disable INITSYS and BASPROGRAMEND
    ElseIf CurrElement->Value = "USERCODEONLY" Then
      UserCodeOnlyEnabled = -1
      If InStr(CurrElement->Next->Value,":") <> 0 Then
          UserDefineStartLabel = Trim(Mid(CurrElement->Next->Value,1,  InStr(CurrElement->Next->Value,":")-1))
          CurrElement = CurrElement->Next
      Else
          OutMessage = Message("WarningUserCodeLabelIncorect")
          LogWarning OutMessage, ""
      End If

    'Volatile bit?
    ElseIf CurrElement->Value = "VOLATILE" Then
      If CurrElement->Next <> 0 Then
        VolatileVar = ReplaceConstantsLine(CurrElement->Next->Value, 0)

        'Split dest into var and bit, get source
        VarName = ""
        If InStr(VolatileVar, ".") <> 0 Then
          VarName = Trim(Left(VolatileVar, InStr(VolatileVar, ".") - 1))
          VarBit = Trim(Mid(VolatileVar, InStr(VolatileVar, ".") + 1))

          'Get name for map
          VarBitNo = -1
          If IsConst(VarBit) Then
            VarBitNo = MakeDec(VarBit)
          Else
            SysVarBit = HashMapGet(SysVarBits, VarBit)
            If SysVarBit <> 0 Then
              VarBitNo = SysVarBit->Location
            End If
          End If
          If VarBitNo <> -1 Then
            HashMapSet(GlitchFreeOutputs, VarName + "." + Str(VarBitNo), "y")
          Else
            OutMessage = Message("WarningVolatileBit")
            Replace OutMessage, "%bit%", VolatileVar
            LogWarning OutMessage, ""
          End If
        Else
          OutMessage = Message("WarningVolatileBit")
          Replace OutMessage, "%bit%", VolatileVar
          LogWarning OutMessage, ""
        End If

        CurrElement = CurrElement->Next
      End If

    'Unrecognised option
    Else
      OutMessage = Message("WarningBadOption")
      Replace OutMessage, "%option%", CurrElement->Value
      LogError (OutMessage, "")
    End If

    CurrElement = CurrElement->Next
  Loop

End Sub

Sub RecordSubCall(CompSub As SubType Pointer, CalledSub As SubType Pointer)

  Dim FindCall As LinkedListElement Pointer

  If CompSub = 0 Then Exit Sub
  If CalledSub = 0 Then Exit Sub

  'Record outgoing call in CompSub
  With *CompSub
    'Check for sub in list already
    FindCall = .CallList->Next
    Do While FindCall <> 0
      If FindCall->MetaData = CalledSub Then
        FindCall->NumVal += 1
        Exit Sub
      End If
      FindCall = FindCall->Next
    Loop

    'Sub not in list, add it
    '.Call(.Calls) = CalledSub
    FindCall = LinkedListInsert(.CallList, CalledSub)
    FindCall->NumVal = 1
  End With


  'Record incoming call in CalledSub
  With *CalledSub
    'Check for sub in list already
    FindCall = .CallerList->Next
    Do While FindCall <> 0
      If FindCall->MetaData = CompSub Then
        FindCall->NumVal += 1
        Exit Sub
      End If
      FindCall = FindCall->Next
    Loop

    'Sub not in list, add it
    '.Call(.Calls) = CalledSub
    FindCall = LinkedListInsert(.CallerList, CompSub)
    FindCall->NumVal = 1
  End With

End Sub

Function ReplaceFnNames(InName As String) As String
  'Replace tokens in sub names with actual names

  Dim As String OutName
  Dim As Integer Temp
  OutName = InName

  'Function name
  Do While Instr(OutName, Chr(31)) <> 0
    Temp = VAL(MID(OutName, Instr(OutName, Chr(31)) + 1))
    Replace OutName, Chr(31) + Str(Temp) + Chr(31), Subroutine(Temp)->Name
  Loop

  'Function name (used differently to 31)
  Do While Instr(OutName, Chr(30)) <> 0
    Temp = VAL(MID(OutName, Instr(OutName, Chr(30)) + 1))
    Replace OutName, Chr(30) + Str(Temp) + Chr(30), Subroutine(Temp)->Name
  Loop

  'Miscellaneous item. Used for array names, may be used for other things
  Do While Instr(OutName, Chr(29)) <> 0
    Temp = VAL(MID(OutName, Instr(OutName, Chr(29)) + 1))
    Replace OutName, Chr(29) + Str(Temp) + Chr(29), PreserveCode(Temp)
  Loop

  Return OutName
End Function

Function RequestSub(Requester As SubType Pointer, SubNameIn As String, SubSigIn As String = "") As Integer
  'Will return index of sub in Subroutine
  'Requester is used to log which sub calls which
  'If Requester is 0, it will be set to main sub
  'If sub not found, will return -1

  Dim As Integer CurrSub, BestMatchPos, BestMatch, ThisMatch, InReqList
  Dim As String SubName, SubSig
  Dim As LinkedListElement Pointer ReqListPos, ReqListData

  SubName = UCase(Trim(SubNameIn))
  BestMatchPos = -1
  BestMatch = -1

  If Requester = 0 Then
    Requester = Subroutine(0)
  End If

  'If no sub being called, exit before recording attempt
  If SubName = "" Then
    Return -1
  End If

  For CurrSub = 1 To SBC
    If UCase(Trim(Subroutine(CurrSub)->Name)) = SubName Then
      If Subroutine(CurrSub)->Overloaded Then
        ThisMatch = SubSigMatch(GetSubSig(*Subroutine(CurrSub)), SubSigIn)
        If ThisMatch > BestMatch Then
          BestMatch = ThisMatch
          BestMatchPos = CurrSub
        End If
      Else
        Subroutine(CurrSub)->Required = -1
        RecordSubCall(Requester, Subroutine(CurrSub))

        Return CurrSub
      End If
    End If
  Next

  If BestMatch <> -1 Then
    Subroutine(BestMatchPos)->Required = -1
    RecordSubCall(Requester, Subroutine(BestMatchPos))
    Return BestMatchPos
  End If

  'Record failed attempted call
  'May be able to grant request later
  'Only add to list if not already in it
  ReqListPos = AttemptedCallList->Next
  InReqList = 0
  Do While ReqListPos <> 0
    With *ReqListPos
      If .MetaData <> 0 Then
        ReqListData = .MetaData
        'Put sub name in .Value, sub sig in .MetaData->Value, caller in .MetaData->MetaData
        If ReqListData->MetaData = Requester Then
          If LCase(.Value) = LCase(SubName) And LCase(ReqListData->Value) = LCase(SubSigIn) Then
            InReqList = -1
            Exit Do
          End If
        End If
      End If
    End With
    ReqListPos = ReqListPos->Next
  Loop

  'Not in requested list, add
  If Not InReqList Then
    ReqListPos = LinkedListInsert(AttemptedCallList, SubName)
    ReqListData = LinkedListCreate
    ReqListPos->MetaData = ReqListData
    ReqListData->Value = SubSigIn
    ReqListData->MetaData = Requester
  End If

  Return -1
End Function

Sub RetrySubRequests
  'Check AttemptedCallList for calls that could not be recorded
  'Attempt to record them

  Dim As LinkedListElement Pointer ReqListPos, ReqListData

  ReqListPos = AttemptedCallList->Next
  Do While ReqListPos <> 0

    If ReqListPos->MetaData <> 0 Then
      ReqListData = ReqListPos->MetaData
      RequestSub(ReqListData->MetaData, ReqListPos->Value, ReqListData->Value)
    End If

    ReqListPos = ReqListPos->Next
  Loop

End Sub

Sub SetCalcTempType (CalcVar As String, NewType As String)

  'Don't allow calc vars to be bit
  If UCase(NewType) = "BIT" Then NewType = "BYTE"

  Select Case UCASE(RIGHT(TRIM(CalcVar), 1))
    Case "A": CalcTempType(1) = UCASE(TRIM(NewType))
    Case "B": CalcTempType(2) = UCASE(TRIM(NewType))
    Case "X": CalcTempType(3) = UCASE(TRIM(NewType))
  End Select

End Sub

Function SetStringPointers (V1 As String, V2 As String, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer

  Dim As String Temp

  'Prepare output list
  Dim As LinkedListElement Pointer OutList, CurrLine
  OutList = LinkedListCreate
  CurrLine = OutList

  'Remove $ from end of var name
  IF Right(V1, 1) = "$" Then V1 = Left(V1, LEN(V1) - 1)
  IF Right(V2, 1) = "$" Then V2 = Left(V2, LEN(V2) - 1)

  'Check input
  If INSTR(UCase(V1), ";STRING") <> 0 Then
    LogError Message("BadStringConst"), Origin
    Return OutList
  End If
  'If V2 is a string, put it into a temp array
  If INSTR(UCase(V2), ";STRING") <> 0 Then
    AddVar("SysTempArray", "STRING", 20, 0, "REAL", Origin, , -1)
    'CSC += 1: TempData(CSC) = "SYSTEMPARRAY()=" + V2
    CurrLine = LinkedListInsert(CurrLine, "SYSTEMPARRAY()=" + V2)
    V2 = "SYSTEMPARRAY"
  End If

  'Add code to set pointers
  'Load V1 into 0, V2 into 1
  CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(V1, 0, CurrSub, Origin))
  CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(V2, 1, CurrSub, Origin))

  Return OutList
End Function

SUB ShowBlock (BlockIn As String)

  Dim As String Block, Temp, LineIn, DispLine
  Dim As Integer InBlock
  Block = UCase(Trim(BlockIn))

  #IFDEF __FB_UNIX__
    OPEN ID + "/messages.dat" FOR INPUT AS #9
  #ELSE
    OPEN ID + "\messages.dat" FOR INPUT AS #9
  #ENDIF

  InBlock = 0
  DO WHILE NOT EOF(9)
    LINE INPUT #9, LineIn
    DispLine = LineIn
    LineIn = UCase(Trim(LineIn))
    IF LineIn = "BLOCK " + Block THEN
      InBlock = -1
    ElseIf InBlock Then
      If LineIn = "END BLOCK" THEN
        Exit Do
      Else
        Do While INSTR(UCase(DispLine), "%VERSION%") <> 0: Replace DispLine, "%VERSION%", Version: Loop
        Do While INSTR(UCase(DispLine), "%FILENAME%") <> 0: Replace DispLine, "%FILENAME%", FI: Loop
        Do While INSTR(UCase(DispLine), "%OUTPUTFILE%") <> 0: Replace DispLine, "%OUTPUTFILE%", OFI: Loop
        PRINT DispLine
      End If
    End If
  LOOP
  CLOSE #9

END SUB

SUB SplitLines (CompSub As SubType Pointer)

  Dim As String Temp, LastHalf
  Dim As LinkedListElement Pointer CurrLine

  'Search main program
  CurrLine = CompSub->CodeStart->Next
  Do While CurrLine <> 0
    IF INSTR(CurrLine->Value, ":") <> 0 AND Right(CurrLine->Value, 1) <> ":" THEN
      Temp = CurrLine->Value
      LastHalf = LTrim(Mid(Temp, INSTR(Temp, ":") + 1))
      CurrLine->Value = RTrim(Left(Temp, INSTR(Temp, ":") - 1))
      LinkedListInsert(CurrLine, LastHalf)
    Else
      CurrLine = CurrLine->Next
    End If
  Loop

END SUB

Function TempRemove(Removed As String) As String
  'Temporarily remove something from a line. Can be replaced later with ReplaceFnNames

  'Record removed item
  PCC += 1: PreserveCode(PCC) = Removed

  Return Chr(29) + Str(PCC) + Chr(29)
End Function

Sub TidyProgram
  Dim As Integer CurrSub

  For CurrSub = 0 To SBC
    With *Subroutine(CurrSub)
      If .Required Then
        TidySubroutine(Subroutine(CurrSub))
      End If
    End With
  Next

End Sub

Sub TidySubroutine(CompSub As SubType Pointer)

  'Fix function calls
  FixFunctions(CompSub)

  'Set Bank
  AddBankCommands(CompSub)

  'Perform some simple ASM optimisations
  AsmOptimiser(CompSub)

  'Add banking mode to 18F commands
  If ChipFamily = 16 Then
    Add18FBanks(CompSub)
  End If

  'Tidy up IFs
  OptimiseIF (CompSub)

  'Replace incf/btfss Z with incfsz (PIC)
  OptimiseIncrement (CompSub)

End Sub

Function TranslateFile(InFile As String) As String
  'Translates file to GCBASIC format

  Dim As String OutFile, ConvFile, CurrType, ConvExe, ConvParams, PathDiv
  Dim As String ConvDir, Temp
  Dim As Integer Converted, Converter, CurrForm, ConvertRequired
  Dim As Double InDate, OutDate
  OutFile = InFile

  'Get converter directory
  #Ifdef __FB_UNIX__
    ConvDir = ID + "/converters"
    PathDiv = "/"
  #ELSE
    ConvDir = ID + "\converters"
    PathDiv = "\"
  #EndIf

  'Might need to do multiple conversions
  Do
    Converted = 0

    'Find converter for current type
    CurrType = Trim(LCase(Mid(OutFile, InStrRev(OutFile, ".") + 1)))
    Converter = -1
    For Converter = 1 To FileConverters
      With FileConverter(Converter)
        'Check all formats for current converter
        For CurrForm = 1 To .InFormats
          If .InFormat(CurrForm) = CurrType Then
            'Have found a converter
            'Check for converted file
            ConvFile = ReplaceToolVariables("%filename%", .OutFormat, OutFile)
            ConvertRequired = 0
            'If no converted file, need to convert
            If Dir(ConvFile) = "" Then
              ConvertRequired = -1
            'If converted file older than source file, need to convert
            Else
              InDate = FileDateTime(OutFile)
              OutDate = FileDateTime(ConvFile)
              If InDate > OutDate Then ConvertRequired = -1
            End If

            'If DeleteTarget set, always convert
            If .DeleteTarget Then ConvertRequired = -1

            If VBS = 1 Then
              Temp = Message("Converting")
              Replace Temp, "%infile%", OutFile
              Print Spc(10); Temp;
            End If

            'If convert needed, convert
            If ConvertRequired Then
              ConvParams = InFile
              If .Params <> "" Then
                ConvParams = ConvParams+" "+ReplaceToolVariables(.Params, CurrType, InFile)
                'If chip model required but not known, cannot convert
                If InStr(LCase(.Params), "%chipmodel%") <> 0 And ChipName = "" Then
                  Return ""
                End If
              End If
              ConvExe = AddFullPath(.ExeName, ConvDir)
              If Exec(ConvExe, ConvParams) <> -1 Then
                If VBS = 1 Then Print Message("Success")
                OutFile = ConvFile
                Converted = -1
              Else
                Print Message("NoConverter")
              End If
            Else
              If VBS = 1 Then Print Message("ConvertedAlready")
              OutFile = ConvFile
              Converted = -1
            End If

            GoTo CheckNextFormat
          End If
        Next
      End With
    Next

    CheckNextFormat:
  Loop While Converted

  Return OutFile
End Function

Sub ValueChanged(VarName As String, VarValue As String)

  If VarName <> "" Then Exit Sub
  IF Left(UCase(VarName), 7) <> "SYSTEMP" Then Exit Sub

  CV = VAL(Mid(Trim(VarName), 8))

  CalcVars(CV).CurrentValue = VarValue
End Sub

Sub WriteAssembly
  Dim As String Temp, VarDecLine, PICASVarDecLine, AddedBits, BitName
  Dim As Integer PD, AddSFR, FindSREG, legacyConfigPublished
  Dim As LinkedListElement Pointer CurrLine
  Dim As LinkedListElement Pointer VarList
  Dim As LinkedListElement Pointer FinalConstantList 
  Dim As SysVarType Pointer SysVar

  'Force AVR to use GCASM if PIC-AS, or MPASM selected .. this is not supported
  If AFISupport = 1 Then
      If ModeAVR and AsmExe <> "GCASM" Then
          AsmExe = "GCASM"
          AFISupport = 0
          Print Message("SwappingAssembertoGCASMs")
      End if
  End IF

  If  ( UCASE(AsmExe) = "PIC-AS" or Instr(UCASE(AsmExe), "MPASM" ) > 0 ) and ModeAVR Then
    AsmExe = "gcasm"
    Print Message("SwappingAssembertoGCASMs")
  End if

  'Write .ASM program
  On error goto ASMLocked
  OPEN OFI FOR OUTPUT AS #1
  If err <> 0 then
      ASMLocked:
      LogError "ASM File locked/open error. Cannot create output ASM", ""
      ErrorsFound = -1
      exit Sub
  End if
  On Error goto 0

  if AFISupport = 1 then
      On error goto ASLocked
      OPEN AFI FOR OUTPUT AS #2
      If err <> 0 then
          ASLocked:
          LogError "AS File locked/open error. Cannot create output AS", ""
          ErrorsFound = -1
          exit Sub
      End if
      On Error goto 0
  Else
      Kill AFI
  End if

  ' ASM file
  If ModePIC Then
      PRINT #1, ";Program compiled by GCBASIC (" + Version + ") for Microchip MPASM/MPLAB-X Assembler using " +  __FB_SIGNATURE__ + "/" + __DATE_ISO__+" CRC"+STR(ReservedwordC)
  Else
      PRINT #1, ";Program compiled by GCBASIC (" + Version + ") for Microchip AVR Assembler using " +  __FB_SIGNATURE__ + "/" + __DATE_ISO__+" CRC"+STR(ReservedwordC)
  End If
  Print #1, ";Need help? "
  Print #1, ";  Please donate to help support the operational costs of the project.  Donate via http://paypal.me/gcbasic"
  Print #1, ";  "
  Print #1, ";  See the GCBASIC forums at http://sourceforge.net/projects/gcbasic/forums,"
  Print #1, ";  Check the documentation and Help at http://gcbasic.sourceforge.net/help/,"
  Print #1, ";or, email us:"
  Print #1, ";   w_cholmondeley at users dot sourceforge dot net"
  Print #1, ";   evanvennn at users dot sourceforge dot net"
  PRINT #1, Star80

  if AFISupport = 1 then
      ' AS file
      PRINT #2, ";Program compiled by GCBASIC (" + Version + ") for Microchip PIC-AS using " +  __FB_SIGNATURE__ + "/" + __DATE_ISO__+" CRC"+STR(ReservedwordC)
      Print #2, ";  Please donate to help support the operational costs of the project.  Donate via http://paypal.me/gcbasic"
      Print #2, ";  "
      Print #2, ";  See the GCBASIC forums at http://sourceforge.net/projects/gcbasic/forums,"
      Print #2, ";  Check the documentation and Help at http://gcbasic.sourceforge.net/help/,"
      Print #2, ";or, email:"
      Print #2, ";   evanvennn at users dot sourceforge dot net"
      PRINT #2, Star80
      Print #2, ";   Installation Dir : " + ID
      Print #2, ";   Source file      : " + SourceFile(1).FileName
      Print #2, ";   Setting file     : " + globalSettingsFile
      Print #2, ";   Preserve mode    : " + str(PreserveMode)
      Print #2, ";   Assembler        : " + ASMEXE
      Print #2, ";   Programmer       : " + PrgExe
      Print #2, ";   Output file      : " + OFI
      Print #2, ";   Float Capability : " + VersionSuffix
      Print #2, Star80
  
  End if

  Print #1, ";   Installation Dir : " + ID
  Print #1, ";   Source file      : " + SourceFile(1).FileName
  Print #1, ";   Setting file     : " + globalSettingsFile
  Print #1, ";   Preserve mode    : " + str(PreserveMode)
  Print #1, ";   Assembler        : " + ASMEXE
  Print #1, ";   Programmer       : " + PrgExe
  Print #1, ";   Output file      : " + OFI
  Print #1, ";   Float Capability : " + VersionSuffix
  Print #1, Star80
  Print #1, ""
  If ModePIC Then
    'asm
    PRINT #1, ";Set up the assembler options (Chip type, clock source, other bits and pieces)"
    PRINT #1, " LIST p=" + ChipName + ", r=DEC"
    'Workaround for 16C5x chips
    If Len(ChipName) = 5 And Left(ChipName, 2) = "16" And Mid(ChipName, 4, 1) = "5" Then
      PRINT #1, "#include <P16" + Mid(ChipName, 3, 1) + "5X.inc>"
    Else
      PRINT #1, "#include <P" + ChipName + ".inc>"
    End If

      if AFISupport = 1 then
          'as
          PRINT #2, ""
          PRINT #2, ";Set up the assembler options (Chip type, clock source, other bits and pieces)"
          PRINT #2, ";PROCESSOR   " + ChipName
          PRINT #2, " PAGEWIDTH   180"
          PRINT #2, " RADIX       DEC"
          PRINT #2, " TITLE       "+ CHR(34)+AFI+CHR(34)
          PRINT #2, " SUBTITLE    "+ CHR(34)+date+CHR(34)
          PRINT #2, ""
          PRINT #2, "; Reverse lookup file(s)"
          PRINT #2, "; "+ChipPICASDataFile
          PRINT #2, "; "+ChipPICASConfigFile
          PRINT #2, "; Alternative CONFIG file is to be found at C:\Program Files\Microchip\xc8\vX.YY\pic\dat\cfgdata as per Microchip support ticket #00730936 "

          PRINT #2, ""
          PRINT #2, Star80
          If ChipFamily = 16 Then
            PRINT #2, ";Explicit PIC-AS constants to resolve the crazyness of the PIC-AS syntax"
            PRINT #2, ";These are therefore the same as MPASM"
            PRINT #2, "#define BANKED b"
            PRINT #2, "#define ACCESS a"
            PRINT #2, "#define UPPER low highword"
          End if
          'if development workarounds... meant to be resolved at PIC-AS 2.35
          '          PRINT #2, ""
          '          PRINT #2, ";#define BTFSS mybtfss"
          '          PRINT #2, ";#define BTFSS mybtfsc"
          '
          '          PRINT #2, ""
          '          PRINT #2, "mybtfss MACRO arg1, arg2"
          '          PRINT #2, "local arg3, arg4"
          '          PRINT #2, " arg3 set ( arg2 << 7 )"
          '          PRINT #2, " arg4 set ( arg1 AND 127 )"
          '          PRINT #2, " dw arg3 + arg4 + 0x1C00"
          '          PRINT #2, "ENDM"
          '          PRINT #2, ""
          '          PRINT #2, "mybtfsc MACRO arg1, arg2"
          '          PRINT #2, "local arg3, arg4"
          '          PRINT #2, " arg3 set ( arg2 << 7 )"
          '          PRINT #2, " arg4 set ( arg1 AND 127 )"
          '          PRINT #2, " dw arg3 + arg4 + 0x1800"
          '          PRINT #2, "ENDM"
          '          PRINT #2, ""

            If ChipFamily = 12 Or ChipFamily = 14 and HighFSR Then
              ' Only create macro for old version of PIC-AS
              If Instr(Ucase(ChipPICASRoot),"2.32") > 0 then
                PRINT #2, ""
                PRINT #2, "BANKISEL MACRO reg"

                PRINT #2, "  ;BANKISEL macro to restore missing Directive."
                ' alternate see https://www.microchip.com/forums/FindPost/1173118 post #39
                'print #2, "dw (0x1000 | ((high(reg) and 01h)<<10) |(7<<7)|STATUS) // BCF/BSF STATUS,IRP"
                PRINT #2, "  if reg < 0x100 "
                PRINT #2, "   ;MESSG "+CHR(34)+"Bankvalue < 256 - bcf"+CHR(34)
                PRINT #2, "   bcf STATUS, 7"
                PRINT #2, "  else"
                PRINT #2, "   ;MESSG "+CHR(34)+"Bankvalue > 256 - bsf"+CHR(34)
                PRINT #2, "   bsf STATUS, 7"
                PRINT #2, "  endif"
                PRINT #2, "ENDM"
              End if
            End If

          PRINT #2, ""
      End if


      'Publish CONFIG for PIC
      if AFISupport = 1 and ConfigDisabled =0 then
        Print #2, Star80
        Print #2, ";Explicit CONFIG"
      end if

    CurrLine = ChipConfigCode->CodeList->Next
    legacyConfigPublished = 0

    Do While CurrLine <> 0

      'Purge duplicate configs for .S only
      dim as Integer CD
      dim ConfName as string
      FOR CD = 1 TO DCOC
        ConfName = trim(Left(DefCONFIG(CD), INSTR(DefCONFIG(CD), "=") - 1))
        if INSTR( ucase(CurrLine->value), ucase(ConfName)) > 0 then
          'we have a match; so comment the DefCONFIG, we cannot delete as we need to restore the value later.
          DefCONFIG(CD) = ";"+DefCONFIG(CD)
        end if
      Next

      'Publish CONFIGs
      If len(CurrLine->Value) < 196 and AFISupport = 0 then
          'less than 196 and not PICAS, so, legacy config statement
          Print #1, CurrLine->Value
          CurrLine = CurrLine->Next

      else
          'so, PICAS mode but do not want to multiple line the config for legacy chips
          if len(CurrLine->Value) < 196 and AFISupport = 1 then
              legacyConfigPublished = -1
              Print #1, CurrLine->Value
          end if

          'export CONFIG  onto on line per entry to stop MPASMx from crashing!
          adaptedConfigLine = CurrLine->Value
          'transform config for non ChipFamily16, only non ChipFamily16 will have GetMetaData(Currline)->RawConfig
          If GetMetaData(Currline)->RawConfig <> "" then

            If instr( GetMetaData(Currline)->RawConfig, "&" ) > 0 then
              StringSplit ( GetMetaData(Currline)->RawConfig, "&",-1,configarray() )
            Else
             redim configarray(1)
             configarray(0) = GetMetaData(Currline)->RawConfig
            End if

            'transform old ASM CONFIG to NEW PIC-AS CONFIG
            for configarraycounter = 0 to ubound(configarray)

               configarray(configarraycounter ) = trim( configarray(configarraycounter )  )  'remove the spaces
               if configarray(configarraycounter ) <> "" then
                 configarray(configarraycounter ) = mid ( configarray(configarraycounter ) , 2 ) 'renove the leading underscore
                 replace( configarray(configarraycounter ), "_", " = " ) 'replace the underscore with =

                 if AFISupport = 1 then print #2,  ConfigTidy( configarray(configarraycounter ) )
               end if

            next
            erase configarray
            'output the ASM, cus we have to maintain the ASM file for reference
            if legacyConfigPublished = 0 then Print #1, CurrLine->Value
            goto ConfigDone
          End if

          'Print multiple lines to ASM and S
          if left(trim(adaptedConfigLine), 6) = "CONFIG" then
              'must be a long 18f config line
              replace ( adaptedConfigLine, "CONFIG ","")
              StringSplit ( adaptedConfigLine, ",",-1,configarray() )
              for configarraycounter = 0 to ubound(configarray)
                if legacyConfigPublished = 0 then print #1, " CONFIG "+configarray(configarraycounter )
                if AFISupport = 1 then print #2, ConfigTidy( configarray(configarraycounter ) )
              next
          else

              if legacyConfigPublished = 0 then Print #1, CurrLine->Value
              if AFISupport = 1 then Print #2, CurrLine->Value
          end if


          ConfigDone:
          CurrLine = CurrLine->Next
          legacyConfigPublished = 0

      End if

    Loop

    'Output the other default configs - these are used to the the HEX config, so, we need them for PIC-AS to create the same HEX
    if AFISupport = 1 and ConfigDisabled = 0 then
      Print #2, ";Inferred CONFIG"
      dim as Integer CD
      FOR CD = 1 TO DCOC
        if left(DefCONFIG(CD),1) <> ";" then
          ' print #2, " CONFIG  " +DefCONFIG(CD)
          print #2, ConfigTidy(DefCONFIG(CD))
        end if
      Next
      print #2, ""
    end if

    'We have to restore Duplicate configs.  They may have been commented out but we need them back for GCASM,
    'So, this becomes a non-destructive operations.
    dim as Integer CD
    FOR CD = 1 TO DCOC
      if left(DefCONFIG(CD),1) = ";" then
        'restore the initial value
        DefCONFIG(CD)=mid( DefCONFIG(CD),2)
      end if
    Next

    if AFISupport = 1 then

      PRINT #2, ""
      PRINT #2, Star80
      PRINT #2, "; The XC8 xc.inc include file.  This MUST be placed after the CONFIG statements."
      PRINT #2, ""
      PRINT #2, " #include <xc.inc>"
      PRINT #2, ""

    End if

  ElseIf ModeAVR Then
    Temp = LCase(ChipName)
    If INSTR(Temp, "usb") <> 0 Then
      Temp = MID(Temp, INSTR(Temp, "usb"))
    ElseIf INSTR(Temp, "can") <> 0 Then
      Temp = MID(Temp, INSTR(Temp, "can"))
    ElseIf Left(Temp, 4) = "mega" THEN
      Temp = "m" + Mid(Temp, 5)
    ElseIf Left(Temp, 4) = "tiny" THEN
      Temp = "tn" + Mid(Temp, 5)
    ElseIf INSTR(Temp, "s") <> 0 Then
      Temp = Mid(Temp, INSTR(Temp, "s") + 1)
    End If
    PRINT #1, ";Chip Model: " + ChipName
    PRINT #1, ";Assembler header file"
    PRINT #1, ".INCLUDE " + Chr(34) + Temp + "def.inc" + Chr(34)
    Print #1, ""
    Print #1, ";SREG bit names (for AVR Assembler compatibility, GCBASIC uses different names)"
    VarList = HashMapToList(SysVarBits, -1)
    CurrLine = VarList->Next
    Do While CurrLine <> 0
      SysVar = CurrLine->MetaData
      If UCASE(SysVar->Parent) = "SREG" Then
        'Get letter of bit (C, Z, etc)
        BitName = Right(SysVar->Name, 1)
        If InStr(AddedBits, BitName) = 0 Then
          Print #1, "#define " + BitName + " " + Str(SysVar->Location)
        End If
        AddedBits += BitName
      End If
      CurrLine = CurrLine->Next
    Loop
    LinkedListDelete(VarList, 0)

  End If
  PRINT #1, ""

  'Out All constants
  If CDFSupport = 1 Then
    Print #CDFFileHandle, ""
    'use FinalConstantList
    Dim as String templine, explandedtempline 
    FinalConstantList = HashMapToList(Constants, -1)
    IF FinalConstantList <> 0 AndAlso FinalConstantList->Next <> 0 THEN
      CurrLine = FinalConstantList->Next
      Do While CurrLine <> 0
        templine = CurrLine->value
        explandedtempline = templine
        explandedtempline = ReplaceConstantsLine(explandedtempline,0)
        If InStr(UCase(explandedtempline), ";STRING") <> 0 Then
          templine = GetString(explandedtempline)
           Print #CDFFileHandle  ,Space(14) + "FINAL/CONSTANT :" +  Left( CurrLine->value+Space(40),40)  ,  chr(34)+templine+chr(34), ReplaceConstantsLine(explandedtempline,0)
        else
           Print #CDFFileHandle  ,Space(14) + "FINAL/CONSTANT :" +  Left(tempLine+Space(40),40)  ,  ReplaceConstantsLine(templine,0)
        end if
       
        CurrLine = CurrLine->Next
      Loop
    End If
  End If

  VarList = HashMapToList(FinalVarList, -1)
  IF VarList <> 0 AndAlso VarList->Next <> 0 THEN
    Dim localoutline as string

    PRINT #1, Star80
    PRINT #1, ""
    PRINT #1, ";Set aside memory locations for variables"

    If AFISupport = 1 then
      PRINT #2, Star80
      PRINT #2, ""
      PRINT #2, ";Set aside RAM memory locations for variables. All variables are global."
    End if

    CurrLine = VarList->Next
    Do While CurrLine <> 0
      With *CPtr(VariableListElement Pointer, CurrLine->MetaData)
        If ModePIC and AFISupport = 1 then
          'PICAS
          PICASVarDecLine = " "+left(.Name+Pad32, 32) + " EQU " + trim(.Value) +"          ; 0x"+ Hex(val(.Value))
          localoutline = "Global "+.Name
          'ASM
          VarDecLine = left(.Name+Pad32, 32) + " EQU " + right( Pad32+trim(.Value),7) +"          ; 0x"+ Hex(val(.Value))
        ElseIf ModePIC Or ModeZ8 Then
          VarDecLine = left(.Name+Pad32, 32) + " EQU " + right( Pad32+trim(.Value),7) +"          ; 0x"+ Hex(val(.Value))
        ElseIf ModeAVR Then
          VarDecLine = ".EQU " + .Name + "=" + trim(.Value) +"          ; 0x"+ Hex(val(.Value))
        Else
          VarDecLine = ";Write variable declaring code!"
        End If
      End With
      PRINT #1, AsmTidy(VarDecLine, -1 )
      if AFISupport = 1 then
        if PreserveMode <> 0 then PRINT #2, AsmTidy( localoutline, 0 )
        PRINT #2, AsmTidy(PICASVarDecLine, 0 )
      End if
      CurrLine = CurrLine->Next
    Loop
    PRINT #1, ""
  END IF
  PRINT #1, Star80
  PRINT #1, ""
  if AFISupport = 1 then
      PRINT #2, ""
      PRINT #2, Star80
      PRINT #2, ""
  End if


  'Registers
  If ModeAVR Then
    Print #1, AsmTidy(";Register variables", -1 )
    FOR PD = 1 TO FRLC
      With FinalRegList(PD)
        If ModePIC Or ModeZ8 Then
          VarDecLine = .Name + " EQU " + .Value
        ElseIf ModeAVR Then
          VarDecLine = ".DEF " + .Name + "=" + .Value
        Else
          VarDecLine = ";Write variable declaring code!"
        End If
      End With
      PRINT #1, AsmTidy(VarDecLine, -1 )
    NEXT
    PRINT #1, ""
    PRINT #1, Star80
    PRINT #1, ""

  End If

  'Aliases
  IF FALC > 0 THEN
    PRINT #1, AsmTidy(";Alias variables", -1 )
    PRINT #2, AsmTidy(";Alias variables", 0 )

    Dim localoutline as string

    FOR PD = 1 TO FALC
      With FinalAliasList(PD)

       If ModePIC and AFISupport = 1 then
          'PICAS
          PICASVarDecLine = " "+left(.Name+Pad32, 32) + " EQU " + trim(.Value)
          localoutline = "Global "+.Name
          'ASM
          VarDecLine = left(.Name+Pad32, 32) + " EQU " + .Value
        ElseIf ModePIC Or ModeZ8 Then
          VarDecLine = .Name + " EQU " + .Value
        ElseIf ModeAVR Then
          VarDecLine = "#define " + .Name + " " + .Value
        Else
          VarDecLine = ";Write variable declaring code!"
        End If
      End With
      PRINT #1, AsmTidy(VarDecLine, -1 )
      if AFISupport = 1 then
        if PreserveMode <> 0 then PRINT #2, AsmTidy( localoutline, 0 )
        PRINT #2, AsmTidy(PICASVarDecLine, 0 )
      End if
    NEXT
    PRINT #1, ""
    PRINT #1, Star80
    PRINT #1, ""
    PRINT #2, ""
    PRINT #2, Star80
    PRINT #2,
  END IF


  If ModePIC Then
      'Add the  Initial vector for PICAS
      if AFISupport = 1 then
        ' PIC10/12/16 devices use a delta of 2 and PIC18 devices use a delta of 1.
        Select Case ChipFamily
          Case 16
            PRINT #2, " PSECT   RESETVEC,delta=1, abs"
          Case Else
            PRINT #2, " PSECT   PROGMEM0,delta=2, abs"
        End Select
        PRINT #2, " RESETVEC:"
      end if
  END IF

  'Program code
  CurrLine = CompilerOutput->CodeList->Next
  if AFISupport = 1 then
    Print message ("PICASMStartDotS")
  end if

  Do While CurrLine <> 0

    IF GetMetaData(Currline)->IsPICAS = -1 then
     'This is specific line for PICAS
     'Implementation assumes only PSECT PAGE COMMANDS - not required for PIC18
     If ChipFamily <> 16 then
        If AFISupport = 1 then
          PRINT #2, AsmTidy(CurrLine->Value, -1 )
        Else
          'Comment out line as GCASM will barf, if, still in list.
          CurrLine->Value = ";"+CurrLine->Value
        End if
     End if
     CurrLine = CurrLine->Next
     Continue Do

    end if

    'If REPROCES line then do not geneate / process for ASM
    IF instr( UCase(CurrLine->Value), "REPROCES") <> 0 then
      CurrLine = CurrLine->Next
      Continue Do
    End If

    IF instr( UCase(CurrLine->Value), "ALIGN 2") = 0 then
      PRINT #1, AsmTidy(CurrLine->Value, -1 )
    end if

    'Output and transform ASM to PICAS code
    'This essentially transforms to ensure all registers are correct, and all register.bit(s) are numeric constants

    If AFISupport = 1 then

      If PICASDEBUG AND NOT PICASDEBUGmessageShown Then 
        Print "PICASDEBUG enabled - see .S output file"
        PICASDEBUGmessageShown = True  'this ensure the message is shown only once.
      End If

      if left(CurrLine->Value,1) <> ";" then

          'the now implies just PIC 
          'If NOT a label, adapt the register and the register.bit to ensure compilation by PIC-AS

          'Define vars and init vars
          dim outline as string
          
          dim ASMInstruction as string
          dim Param1 as string
          dim Param2 as string
          dim Param3 as string
          dim elements0() as string
          dim outstring as string
          dim preservedline as Integer
          dim showonceflag as Byte = 0
          outline = CurrLine->Value
          preservedline = 0

          if GetMetaData(Currline)->IsLabel = 0 then
            
            Select Case ChipFamily

            Case 16 'therefore 18Fs
                  'Some #RAWASM is actually hidden in PRESERVE lines, so, we need to extract and tranform
                  If Left(outline, 8) = "PRESERVE" Then
                      preservedline = -1
                      Dim as Integer PresPos
                      PresPos = VAL(Mid(outline, 10))                    
                      outline  = PreserveCode(PresPos)
                  End if

                  'This section reverse lookups into the PICAS INC file in there is comma....
                  If instr( outline, "," ) > 0 then

                      'Take the initial line of code and split into the array - so BCF OSCCON1,NDIV3,BANKED becomes 3 elements (BCF OSCCON1),(NDIV3),(BANKED)
                      ' but MOVFF TMRVALUE,TMR2 is....
                      ' but BCF OSCCON1,NDIV3,BANKED
                      erase currentLineElements

                      ' count the number of commas'.  This will identify the format of the outline
                      dim as integer charCount = 0
                      dim i as integer
                      for i = 0 to len(outline)-1
                          if outline[i] = ASC(",") then
                            charCount = charCount + 1
                          end if
                      next 

                      Select Case charCount 
                        Case 1
                          ' example "bsf      NVMCON1,WR"  | "MOVWF	OSCCON1,BANKED"
                          ' MOVFF TMRVALUE,TMR2
                          replaceall ( OutLine, "  ", " " )

                          StringSplit ( trim(outline), ",",-1,currentLineElements() )
                          Param3 = currentLineElements(1)

                          erase elements0
                          StringSplit( trim(currentLineElements(0)), " " ,3, elements0() )

                          ASMInstruction = " "+elements0(0)
                          Param2 = elements0(1)

                        Case 2

                          StringSplit ( trim(outline), ",",-1,currentLineElements() )
                          Param2 = currentLineElements(1)

                          Param3 = ""
                          if ubound(currentLineElements) > 1  then
                            Param3 = trim(currentLineElements(2))
                          end if
                      
                          'So, (BCF OSCCON1) becomes ((BCF) (OSCCON1)
                          erase elements0
                          StringSplit( trim(currentLineElements(0)), " " ,3, elements0() )

                          ASMInstruction = " "+elements0(0)
                          Param1 = elements0(1)

                        Case Else
                          ' items that are not processed
                          If Left(outline,1) <> ";" and Left(trim(outline),2) <> "DB" and Left(trim(outline),2) <> "DW" Then
                            LogWarning "GCBASIC compiler: PIC-AS conversion unhandled for "+outline
                          End If
                      End Select

                      'First test: Does the SECOND element exist as SYSVAR? and therefore a register   like  BCF 'OSCCON1',NDIV3,BANKED
                      if GetSysVar(Param1) <> 0 then
                        outstring = GetReversePICASIncFileLookupValue ( GetSysVar(Param1)->location )
                        if outstring <> "" then
                    
                              replace ( outline , trim(Param1) , outstring )
                              outline = outline
                              if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                if PICASDEBUG then Print #2, ";A2: ASM Source was: "+CurrLine->Value + "  Param1 = " + trim(Param1) + " - target = "+outstring + " now " + outline
                                CurrLine->Value = outline
                              end if

                              'assign here so we can see the debug
                              Param1 = outstring

                              'We know that we are dealing with a register, so, reverse lookup was correct
                              if Param3 = "" then
                                outline = ASMInstruction+" "+outstring+","+Param2
                                if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                  if PICASDEBUG then Print #2, ";A4: ASM Source was: "+CurrLine->Value
                                end if
                              else
                                outline = ASMInstruction+" "+outstring+","+Param2+","+Param3
                                if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                  if PICASDEBUG then Print #2, ";A5: ASM Source was: "+CurrLine->Value + " target = " + outline
                                end if

                              end if

                        else
                              replace ( outline , trim(Param1) , ucase( trim(Param1) ) )
                              outline = outline
                              if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                if PICASDEBUG then Print #2, ";A3: ASM Source was: "+CurrLine->Value
                              end if
                        end if

                      else

                          if GetSysVar(Trim(Param2)) <> 0 then
                              outstring = GetReversePICASIncFileLookupValue ( GetSysVar(Trim(Param2))->location )
                              if outstring <> "" then
                                  If Instr(Ucase(Param3), "BANKED") <> 0 or Instr(Ucase(Param3), "ACCESS") <> 0  Then                                 
                                    outline = ASMInstruction+" "+OutString+","+Param3
                                    if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                      if PICASDEBUG then Print #2, ";A6a: ASM Source was: "+CurrLine->Value
                                    end if
                                  Else
                                    'Test if Param3 is a Bit, if is convert, else, it is register.
                                    if trim(GetSFRBitValue(trim(Param3))) <> "" then
                                      outline = ASMInstruction+" "+Param2 + "," + GetSFRBitValue(trim(Param3))
                                    else
                                      outline = ASMInstruction+" "+Param2 + "," + trim(Param3)
                                    end if
                                    if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                      if PICASDEBUG then Print #2, ";A6b: ASM Source was: "+CurrLine->Value
                                    end if
                                  End if

                              end if
                          end if

                          if GetSysVar(trim(Param3)) <> 0 then
                              outstring = GetReversePICASIncFileLookupValue ( GetSysVar(Trim(Param3))->location )
                         


                              if outstring <> "" then
                                  If Instr(Ucase(Param3), "BANKED") <> 0 or Instr(Ucase(Param3), "ACCESS") <> 0  Then                                 
                                    outline = ASMInstruction+" "+Param2 + ","+OutString
                                    if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                      if PICASDEBUG then Print #2, ";A6c: ASM Source was: "+CurrLine->Value
                                    end if
                                  Else
                                    'Test if Param2 is a Bit, if is convert, else, it is register.
                                    if trim(GetSFRBitValue(trim(Param2))) <> "" then
                                      outline = ASMInstruction+" "+GetSFRBitValue(trim(Param2)) + "," + trim(outstring)
                                    else
                                      outline = ASMInstruction+" "+Param2 + "," + trim(outstring)
                                    end if
                                    if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                      if PICASDEBUG then Print #2, ";A6d: ASM Source was: "+CurrLine->Value
                                    end if
                                  End if

                              end if
                          end if

                      end if

                      'Second test for register.bit  transforms the BITs to a constant to resolve the reverse lookup
                      if HasSFRBit ( Param2 )  then
                          Param2 = GetSFRBitValue(trim( Param2 ))
                          if Param3 = "" then
                            outline = ASMInstruction+" "+Param1+","+Param2
                            if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                              if PICASDEBUG then Print #2, ";A7: ASM Source was: "+CurrLine->Value
                            end if
                          else
                            outline = ASMInstruction+" "+Param1+","+Param2+","+Param3
                            if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                if PICASDEBUG then Print #2, ";A8: ASM Source was: "+CurrLine->Value   + " target = " + outline
                            end if
                          end if
                      end if

                  else
                      'lots of code looks like this... "BANKSEL ADCON0"  where there is register.. check the register is valid and reverse
                      erase currentLineElements

                      'Inspect for a space - threfore, must be more that one paramater
                      if instr(trim(outline), " ") <> 0 then
                        Dim tmpOutLine as String = outline
                        replaceall ( tmpOutLine, "(", " " )
                        replaceall ( tmpOutLine, ")", " " )
                        replaceall ( tmpOutLine, "[", " " )
                        replaceall ( tmpOutLine, "]", " " )
                        replaceall ( tmpOutLine, ">>", " " )
                        replaceall ( tmpOutLine, "<<", " " )
                        replaceall ( tmpOutLine, "  ", " " )
                        
                        StringSplit ( trim(tmpOutLine), " ",-1,currentLineElements() )                          
                        If UserCodeOnlyEnabled = -1 then
                          Dim elementcounter as Byte
                          Dim registerbitlocation as String
                          'walk the elements replacing BIT constant will value using the reverse lookups
                          For elementcounter = 0 to ubound(currentLineElements)
                          
                              if GetSFRBitValue(trim(currentLineElements(elementcounter))) <> "" then
                                  registerbitlocation = GetSFRBitValue(trim(currentLineElements(elementcounter)))
                                  
                                  if registerbitlocation <> "" then           
                            
                                      If showonceflag = 0 and Instr(Ucase(outline), "#ASMRAW") = 0  and  Left(outline, 8) <> "PRESERVE" and trim(ucase(registerbitlocation)) <> trim(ucase(currentLineElements(elementcounter))) then
                                        Print #2, ";"+trim(outline)
                                        showonceflag = -1
                                      end if

                                      replace( outline, currentLineElements(elementcounter), registerbitlocation )

                                  end if

                              end if

                          Next
                        else
                          StringSplit ( trim(outline), " ",-1,currentLineElements() )
                          Param1 = currentLineElements(0)
                          Param2 = currentLineElements(1)

                          if GetSysVar(Param2) <> 0 then
                            outstring = GetReversePICASIncFileLookupValue ( GetSysVar(Param2)->location )

                            if outstring <> "" then
                                  outline = Param1+" "+outstring
                                  if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                    if PICASDEBUG then Print #2, ";A9: ASM Source was: "+CurrLine->Value
                                  end if
                            end if
                          end if
                        end if
                      end if
                  end if


            Case Else  '16F etc
                  If instr( outline, "," ) > 0 then
                      'Assumes that any ASM that has a command requires analysis and transformation for PIC-AS

                      'Take the initial line of code and split into the array into the array currentLineElements()
                      erase currentLineElements
                      StringSplit ( trim(outline), ",",-1,currentLineElements() )
                      Param2 = currentLineElements(1)

                      Param3 = ""
                      if ubound(currentLineElements) > 1  then
                        Param3 = trim(currentLineElements(2))
                      end if

                      'So, split the first element into elements0()
                      erase elements0
                      StringSplit( trim(currentLineElements(0)), " " ,3, elements0() )
                      ASMInstruction = " "+elements0(0)
                      Param1 = elements0(1)

                      'print outline, "=",ASMInstruction+" "+Param1+","+Param2+","+Param3+"           ;B1"

                      'First test: Does the SECOND element exist as getSYSVAR and therefore a register   like  BCF 'OSCCON1',NDIV3,BANKED
                      if GetSysVar(Param1) <> 0 then
                        outstring = GetReversePICASIncFileLookupValue ( GetSysVar(Param1)->location )
                        if outstring <> "" then

                              Param1 = outstring
                              outline = ASMInstruction+" "+Param1+","+Param2
                              if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                if PICASDEBUG then Print #2, ";B2: ASM Source was: "+CurrLine->Value
                              end if

                        else
                              replace ( outline , trim(Param1) , ucase( trim(Param1) ) )
                              outline = outline
                              if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                if PICASDEBUG then Print #2, ";B3: ASM Source was: "+CurrLine->Value
                              end if
                        end if

                        'We know that we are dealing with a register, so, reverse lookup to correct
                        outstring = GetReversePICASIncFileLookupValue ( GetSysVar(Param1)->location )
                        if outstring <> "" then
                          Param1 = outstring
                          if Param3 = "" then
                            outline = ASMInstruction+" "+Param1+","+Param2
                            if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                              if PICASDEBUG then Print #2, ";B4: ASM Source was: "+CurrLine->Value
                            end if
                          else
                            outline = ASMInstruction+" "+Param1+","+Param2+","+Param3
                            if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                              if PICASDEBUG then Print #2, ";B5: ASM Source was: "+CurrLine->Value
                            end if

                          end if
                        end if

                      else

                          if GetSysVar(Param2) <> 0 then
                              outstring = GetReversePICASIncFileLookupValue ( GetSysVar(Param2)->location )
                              if outstring <> "" then
                                  Param2 = outstring
                                  outline = chr(9)+ASMInstruction+" "+Param1+","+Param2
                                  if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                    if PICASDEBUG then Print #2, ";B6: ASM Source was: "+CurrLine->Value
                                  end if

                              end if
                          end if

                      end if

                      'Second test for register.bit  transforms the BITs to a constant to resolve the reverse lookup
                      'Section can be improved to replace the constant with he targtet bit - tested on 16/18f
                      if HasSFRBit ( Param2 )  then
                          'replace this with an explict look up into the .h file 
                          'meanwhile use a constant
                          Param2 = GetSFRBitValue(trim( Param2 ))
                          if Param3 = "" then
                            outline = ASMInstruction+" "+Param1+","+Param2
                            if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                              if PICASDEBUG then Print #2, ";B7: ASM Source was: "+CurrLine->Value
                            end if
                          else
                            outline = chr(9)+ASMInstruction+" "+Param1+","+Param2+","+Param3
                            if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                if PICASDEBUG then Print #2, ";B8: ASM Source was: "+CurrLine->Value
                            end if
                          end if
                      end if

                  else                  
                      'lots of code looks like this... "BANKSEL ADCON0"  where there is register.. check the register is valid and reverse
                      'code could be ( 1 << BIT ) or ( 1 << WR ) etc
                      erase currentLineElements

                      'Some #RAWASM is actually hidden in PRESERVE lines, so, we need to extract and tranform
                      If Left(outline, 8) = "PRESERVE" Then
                          preservedline = -1
                          Dim as Integer PresPos
                          PresPos = VAL(Mid(outline, 10))
                          outline = PreserveCode(PresPos)
                      End if
                                          
                      if instr(trim(outline), " ") <> 0 then
                          Dim tmpOutLine as String = outline
                          replaceall ( tmpOutLine, "(", " " )
                          replaceall ( tmpOutLine, ")", " " )
                          replaceall ( tmpOutLine, "[", " " )
                          replaceall ( tmpOutLine, "]", " " )
                          replaceall ( tmpOutLine, ">>", " " )
                          replaceall ( tmpOutLine, "<<", " " )
                          replaceall ( tmpOutLine, "  ", " " )
                          
                          StringSplit ( trim(tmpOutLine), " ",-1,currentLineElements() )
                          If UserCodeOnlyEnabled = -1 then
                            Dim elementcounter as Byte
                            Dim registerbitlocation as String
                            'walk the elements replacing BIT constant will value using the reverse lookups
                            For elementcounter = 0 to ubound(currentLineElements)
                            
                                if GetSFRBitValue(trim(currentLineElements(elementcounter))) <> "" then
                                    registerbitlocation = GetSFRBitValue(trim(currentLineElements(elementcounter)))
                                    
                                    if registerbitlocation <> "" then           
                              
                                        If showonceflag = 0 and Instr(Ucase(outline), "#ASMRAW") = 0  and  Left(outline, 8) <> "PRESERVE" and trim(ucase(registerbitlocation)) <> trim(ucase(currentLineElements(elementcounter))) then
                                          Print #2, ";"+trim(outline)
                                          showonceflag = -1
                                        end if

                                        replace( outline, currentLineElements(elementcounter), registerbitlocation )

                                    end if

                                end if

                            Next

                          else
                            ' reverse lookup Param2. Things like BANKSEL NVMCON1. This transformation is to ensure the register is the same name
                            Param1 = currentLineElements(0)
                            Param2 = currentLineElements(1)

                            if GetSysVar(Param2) <> 0 then
                              Param2 = GetReversePICASIncFileLookupValue ( GetSysVar(Param2)->location )

                              if Param2 <> "" then
                                    outline = chr(9)+Param1+" "+Param2
                                    if trim(CurrLine->Value) <> trim(outline)  and PreserveMode = 2 then
                                      if PICASDEBUG then Print #2, ";B9: ASM Source was: "+CurrLine->Value
                                    end if
                              end if
                            end if
                          end if
                          
                      end if

                  end if
            End Select

          else
            'must be a label

            if trim(outline) <> "" then
                if PreserveMode <> 0 then print #2,  AsmTidy( "Global " +ucase(outline) ,0 )
                outline= outline+":"
            End if


          end if

          'pad the tab
          if ucase(left(trim(outline),7)) =  "BANKSEL" then
              outline = chr(9)+trim(outline)
          End if

          print #2, AsmTidy( outline, 0 )
      else
        print #2, AsmTidy( CurrLine->Value, 0 )
      end if

    end if


    CurrLine = CurrLine->Next
  Loop
  PRINT #1, ""
  If ModePIC Then
      if AFISupport = 1 then
        PRINT #2, ";"
        PRINT #2, "; Declare Power-On-Reset entry point"
        PRINT #2, ";"
        PRINT #2, " END     RESETVEC"
        CLOSE #2

        'reopen for 2.35 fix - as the comments are corrupted
        'removed constraint check but this probable means that 2.32 and 2.35 will fail to compile.. use a version of PIC-AS from 2.4x or greater
        'If Instr(Ucase(ChipPICASRoot),"2.32") > 0 or Instr(Ucase(ChipPICASRoot),"2.35") > 0 then
            OPEN AFI FOR INPUT AS #2
            OPEN AFI+".tmp" FOR OUTPUT AS #3
            Dim DataSource as String
            Do While NOT EOF(2)
                Line Input #2,  DataSource
                If left( DataSource, 1 )=";" Then
                  If left( DataSource, 2 )=";"+Chr(34)  Then
                    Print #3, DataSource
                  ElseIf left( DataSource, 2 )=";'"  Then
                    Print #3, DataSource
                  Else
                    'muset be just the ;
                    DataSource = ";'"+mid( DataSource,2 )
                    Print #3, DataSource
                  End If
                Else
                    Print #3, DataSource
                End if
            Loop
            Close #2
            Close #3
            FileCopy ( AFI+".tmp", AFI )
            Kill ( AFI+".tmp")
        'End If



        PRINT #1, " END"

      else
        PRINT #1, " END"
      end if
  End if
  CLOSE #1

  if AFISupport = 1 then
    Print message ("PICASMWriteDotS")
  end if


End Sub

Sub WriteCompilationReport
  'Create a report on compilation process

  Dim As String RF, ReportFileName, OutData, Temp, CallText
  Dim As Integer F, CurrSubNo, CurrBank, CurrBankLoc
  Dim As SubType Pointer CurrSub, CalledSub
  Dim As LinkedListElement Pointer ListItem

  'Save typing
  RF = CompReportFormat

  'If format starts with "s_", then skip report if no compilation done
  If Left(RF, 2) = "s_" Then
    If CompileSkipped Then
      Exit Sub
    End If
    'Not skipping, remove s_ and continue
    RF = Mid(RF, 3)
  End If

  'Check requested format: valid options are HTML or plain text
  If RF <> "html" And RF <> "text" Then Exit Sub

  'Open file for output
  If RF = "html" Then
    ReportFileName = ReplaceToolVariables("%filename%", "html")
  ElseIf RF = "text" Then
    ReportFileName = ReplaceToolVariables("%filename%", "report.txt")
  End If
  F = FreeFile
  Open ReportFileName For Output As #F

  'Write header of file
  If RF = "html" Then
    Print #F, "<!DOCTYPE html>"
    Print #F, "<html>"
    Print #F, "<head>"
    Print #F, "<title>" + Message("CRTitle") + "</title>"
    Print #F, "</head>"
    Print #F, "<body>"
    Print #F, "<h1>" + Message("CRTitle") + "</h1>"
  ElseIf RF = "text" Then
    Print #F, Message("CRTitle")
    Print #F, ""
  End If

  'Format the version string
  If Right(Trim(Version),1) <> ")" Then Version = Version + ")"

  'Write compiler information
  If RF = "html" Then

    If MuteBanners = -1 Then

        Randomize timer
        Select Case  int(Rnd * (10 - 1) + 1)
          Case 1:
        Print #F, "<p>" + Message("CRVersion") + ": " + Version + "</p>"+Message("PromoMessage1")

          Case 2:
        Print #F, "<p>" + Message("CRVersion") + ": " + Version + "</p>"+Message("PromoMessage2")

          Case 3:
        Print #F, "<p>" + Message("CRVersion") + ": " + Version + "</p>"+Message("PromoMessage3")

          Case Else
        Print #F, "<p>" + Message("CRVersion") + ": " + Version + "</p>"

        End Select
    Else
      Print #F, "<p>" + Message("CRVersion") + ": " + Version + "</p>"
    End If

  ElseIf RF = "text" Then
    Print #F, Message("CRVersion") + ": " + Version
    Print #F, ""
    Print #F, Star80
    Print #F, ""
  End If

  'Write compilation time and messages produced
  If RF = "html" Then
    Print #F, "<h2>" + Message("CRCompTime") + "</h2>"
    Print #F, "<p>" + Message("CompTime") + Str(CompEndTime - StartTime) + Message("CompSecs") + "</p>"
    Print #F, "<p>" + Message("AsmTime") + Str(AsmEndTime - CompEndTime) + Message("CompSecs") + "</p>"
    Print #F, "<p>" + Message("ProgTime") + Str(ProgEndTime - AsmEndTime) + Message("CompSecs") + "</p>"
    Print #F, "<p>" + Message("TotalTime") + Str(ProgEndTime - StartTime) + Message("CompSecs") + "</p>"
  ElseIf RF = "text" Then
    Print #F, Message("CRCompTime") + ":"
    Print #F, Message("CompTime") + Str(CompEndTime - StartTime) + Message("CompSecs")
    Print #F, Message("AsmTime") + Str(AsmEndTime - CompEndTime) + Message("CompSecs")
    Print #F, Message("ProgTime") + Str(ProgEndTime - AsmEndTime) + Message("CompSecs")
    Print #F, Message("TotalTime") + Str(ProgEndTime - StartTime) + Message("CompSecs")
    Print #F, ""
    Print #F, Star80
    Print #F, ""
  End If

  'Write chip resource usage information
  Dim As String UsedProgram, UsedRAM, OscType
  UsedProgram = Message("UsedProgram")
  Replace UsedProgram, "%used%", Str(StatsUsedProgram + ReserveHighProg)
  Replace UsedProgram, "%total%", Str(ChipProg)
  If ChipProg <> 0 Then UsedProgram += Format((StatsUsedProgram + ReserveHighProg) / ChipProg, " (###.##%)")
  UsedRAM = Message("UsedRAM")
  Replace UsedRAM, "%used%", Str(StatsUsedRam+RegBytesUsed)
  Replace UsedRAM, "%total%", Str(ChipRAM)
  If ChipRAM <> 0 Then UsedRAM += Format(StatsUsedRAM / ChipRAM, " (###.##%)")

  OscType = ""
  If ModePIC Then
    If HashMapGet(Constants, "CHIPUSINGINTOSC") <> 0 Then
      OscType = " (" + Message("CRIntOsc") + ")"
    Else
      OscType = " (" + Message("CRExtOsc") + ")"
    End If
  End If

  If RF = "html" Then
    Print #F, "<h2>" + Message("ChipUsage") + "</h2>"
    'This needs to match g@Stools.bas
    Print #F, "<p>" + Message("ChipM") + ChipName + "</p>"
    Print #F, "<p>" + UsedProgram + "</p>"
    Print #F, "<p>" + UsedRAM + "</p>"
    If ModePIC Then
        Print #F, "<p> OSC: " + ChipOscSource + ", " + Str(ChipMhz) + "Mhz" + OscType + "</p>"
    Else
        Print #F, "<p> OSC: " + Str(ChipMhz) + "Mhz</p>"
    End if

  ElseIf RF = "text" Then
    Print #F, Message("ChipUsage")
    Print #F, Message("ChipM") + ChipName
    Print #F, UsedProgram
    Print #F, UsedRAM
    Print #F, ChipOscSource + OscType
    Print #F, ""
    Print #F, Star80
    Print #F, ""
  End If

  'Write chip pin usage information

  'Write variable sizes/locations
  'Display variable name, type and location
  If RF = "html" Then
    Print #F, "<h2>" + Message("CRRAM") + "</h2>"
    Print #F, "<table border=""1"">"
  Else
    Print #F, Message("CRRAM")
  End If

  If RF = "html" Then
    Print #F, "</table>"
  ElseIf RF = "text" Then
    Print #F, Star80
    Print #F, ""
  End If

  'Write subroutine sizes/locations/calls
  If RF = "html" Then
    Print #F, "<h2>" + Message("CRSubInfo") + "</h2>"
    Print #F, "<table border=""1"">"
    OutData = "<tr><th>" + Message("CRName") + "</th><th>" + Message("CROriginalSize")
    OutData = OutData + "</th><th>" + Message("CRSize") + "</th><th>"
    If ModePIC And (ProgMemPages > 1) Then
      OutData = OutData + Message("CRPage") + "</th><th>"
    End If
    OutData = OutData + Message("CRCalls") + "</th></tr>"
    Print #F, OutData

  ElseIf RF = "text" Then
    Print #F, Message("CRSubInfo")
  End If
  For CurrSubNo = 0 To SBC
    CurrSub = Subroutine(CurrSubNo)

    If CurrSub->Required Then
      'Get call list
      CallText = ""
      ListItem = CurrSub->CallList->Next
      Do While ListItem <> 0
        CalledSub = ListItem->Metadata
        If CallText = "" Then
          CallText = CalledSub->Name + "(" + Str(ListItem->NumVal) + ")"
        Else
          CallText = CallText + ", " + CalledSub->Name + "(" + Str(ListItem->NumVal) + ")"
        End If
        ListItem = ListItem->Next
      Loop

      'Write information
      If RF = "html" Then
        OutData = "<tr><td>" + CurrSub->Name + "</td>"
        OutData += "<td>" + Str(CurrSub->OriginalLOC) + "</td>"
        OutData += "<td>" + Str(CurrSub->HexSize) + "</td>"
        If ModePIC And (ProgMemPages > 1) Then
          OutData += "<td>" + Str(CurrSub->DestPage) + "</td>"
        End If
        OutData += "<td>" + CallText + "</td></tr>"
        Print #F, OutData

      ElseIf RF = "text" Then
        Print #F, CurrSub->Name
        Print #F, Message("CROriginalSize") + ": " + Str(CurrSub->OriginalLOC)
        Print #F, Message("CRSize") + ": " + Str(CurrSub->HexSize)
        If ModePIC And (ProgMemPages > 1) Then
          Print #F, Message("CRPage") + ": " + Str(CurrSub->DestPage)
        End If
        If CallText <> "" Then Print #F, Message("CRCalls") + ": " + CallText
        Print #F, ""
      End If
    End If
  Next

  'Finish
  If RF = "html" Then
    Print #F, "</table>"
    Print #F, "</body>"
    Print #F, "</html>"
  ElseIf RF = "text" Then
    Print #F, Star80
    Print #F, ""
  End If
  Close #F

End Sub

SUB WriteErrorLog
  Dim As String Temp, MessageType, NewErr, ErrorFileName
  Dim As Integer CD, CS, PD, F1, L1, F2, L2, F, L, S, I, T
  Dim As Integer ERC, WRC, SortEnd

  'Set error log filename
  ErrorFileName = "Errors.txt"

  'Exit sub if no errors are present
  IF OutMessages = 0 Then
    Kill ErrorFileName
    EXIT Sub
  End If

  'Sort error list
  'Print "Sort error list"
  SortEnd = OutMessages - 1
  If OutMessages = MAX_OUTPUT_MESSAGES Then SortEnd = OutMessages - 2
  Do
    CS = 0
    FOR PD = 1 TO SortEnd
      'Get file and line of two errors
      F1 = 0: L1 = 0: F2 = 0: L2 = 0
      IF INSTR(OutMessage(PD), ";?F") <> 0 THEN
        Temp = OutMessage(PD)
        F1 = VAL(Mid(Temp, INSTR(Temp, "F")+1))
        L1 = VAL(Mid(Temp, InStr(Temp, "L")+1))
      END IF
      IF INSTR(OutMessage(PD + 1), ";?F") <> 0 THEN
        Temp = OutMessage(PD + 1)
        F2 = VAL(Mid(Temp, INSTR(Temp, "F")+1))
        L2 = VAL(Mid(Temp, INSTR(Temp, "L")+1))
      END IF

      'Compare, swap if necessary
      IF (F1 > F2) Or (F1 = F2 AND L1 > L2) THEN
        Swap OutMessage(PD), OutMessage(PD + 1)
        CS = 1
      END IF
    NEXT
  Loop While CS = 1
  'Print "Finished sorting"

  'Generate file:line references
  FOR PD = 1 to OutMessages
    'Get type
    MessageType = Right(OutMessage(PD), 1)
    OutMessage(PD) = Left(OutMessage(PD), LEN(OutMessage(PD)) - 1)
    If MessageType = "W" Then MessageType = Message("TypeWarning"): WRC += 1
    If MessageType = "E" Then MessageType = Message("TypeError"): ERC += 1
    If LEN(MessageType) > 1 And UCase(LEFT(OutMessage(PD), 4)) <> "ASM:" Then
      MessageType = MessageType + ": "
    Else
      MessageType = ""
    End If

    Dim As String Me, Fi
    IF INSTR(OutMessage(PD), ";?F") <> 0 THEN
      Temp = OutMessage(PD)
      'Get values
      Replace Temp, "?", ""
      Me = Trim(Mid(Temp, INSTR(Temp, "?") + 1)) 'Message
      F = VAL(Mid(Temp, INSTR(Temp, "F") + 1)) 'File
      L = VAL(Mid(Temp, INSTR(Temp, "L") + 1)) 'Line
      S = VAL(Mid(Temp, INSTR(Temp, "S") + 1)) 'Subroutine
      I = VAL(Mid(Temp, INSTR(Temp, "I") + 1)) 'Subroutine line

      'If F and L = 0, location isn't known
      If F = 0 And L = 0 Then
        OutMessage(PD) = MessageType + OutMessage(PD)

      Else
        'Trim filename of path
        Fi = SourceFile(F).FileName
        FOR T = LEN(Fi) to 1 step -1
          IF Mid(Fi, T, 1) = "\" THEN Fi = Mid(Fi, T + 1): Exit For
        NEXT
        'Prepare message
        If GCGB = 0 Then
          'New handler for SynWrite and GC Code IDEs.
	      ' <file> ** ( ... a system include file (within lowlevel.dat)
	      ' <file> * ( ... an system include  file, like #include <>
	      ' <file> ** ( ... other files.
          Select Case SourceFile(F).SystemInclude
              Case -1
                OutMessage(PD) = Fi + " ** (" + Str(L) + "): " + MessageType + Me
                OutMessage(PD) = OutMessage(PD) +message("SystemIncludeMessage")
              Case 1
                OutMessage(PD) = Fi + " * (" + Str(L) + "): " + MessageType + Me
                OutMessage(PD) = OutMessage(PD) +message("LibraryIncludeMessage")
              Case Else
                'Standard message
                OutMessage(PD) = Fi + " (" + Str(L) + "): " + MessageType + Me
          End Select 
        Else
          'Use special error format for GCGB (or other IDE that breaks program into subs)
          OutMessage(PD) = Left(MessageType, 1) + "|" + Fi + "|" + Str(S) + "|" + Str(I) + "|" + Me
        End If
      End If
    Else
      OutMessage(PD) = MessageType + OutMessage(PD)
    END IF

  Next

  If ERC = 0 And WRC > 0 And WarningsAsErrors <> 0 Then
    'No errors, only warnings, but treating warnings as errors
    LogOutputMessage(Message("TypeError") + ": " + Message("WarningsAsErrors"))
    ERC += 1
  End If

  'Display message
  IF ERC = 1 THEN Temp = Message("Error")
  IF ERC > 1 THEN Temp = Message("Errors")
  IF ERC = 0 And WRC = 1 Then Temp = Message("Warning")
  IF ERC = 0 And WRC > 1 Then Temp = Message("Warnings")

  'If not being called by GCGB/other IDE, display errors
  IF GCGB = 0 THEN
    PRINT
    PRINT Temp
    PRINT
    FOR PD = 1 TO OutMessages
      Temp = OutMessage(PD)
      'Display errors in 77 columns neatly
      '77 chosen because it allows for the whole thing to fit in a Windows command
      'prompt, the screen is normally 80 columns but the scrollbar takes away 3.
ShowError:
      IF LEN(Temp) <= Columnwidth THEN PRINT SPC(1); Temp
      IF LEN(Temp) > Columnwidth THEN
        FOR T = Columnwidth TO 1 STEP -1
          IF Mid(Temp, T, 1) = " " THEN
            PRINT SPC(1); Left(Temp, T)
            Temp = Mid(Temp, T + 1)
            GOTO ShowError
          END IF
        NEXT
      END IF
    NEXT
  END If

  'Set return code
  If ERC > 0 And ExitValue = 0 Then
    ExitValue = &Hdeadbeef
  End If


  'Write error log
  If ERC > 0 Or( WarningsAsErrors = -1 and WRC > 0) Then
    PRINT
    PRINT Message("ErrorLogged") + ErrorFileName + "."
  End IF
  OPEN ErrorFileName FOR OUTPUT AS #1
  FOR PD = 1 TO OutMessages
    PRINT #1, OutMessage(PD)
  NEXT
  CLOSE
  PRINT

  'Pause if errors found and pause enabled
  IF PauseOnErr = 1 And ErrorsFound Then
    WaitForKeyOrTimeout
  END IF

END SUB

Sub MergeSubroutines

  'Combine subroutines into single program
  Dim As Integer CurrSub, TotalProgSize, FitAttempts, SubsAdded, SubQueueCount, Temp
  Dim As Integer SortPos, MovePos, ThisSub, RequiredSubs, CurrPage, OldTotalProgSize
  Dim As Integer SubsFit, FirstPageFull, ForceFit, CurrPagePos, SortMore, CurrListPage
  Dim As AllocationOrderType AllocationOrder(MAX_PROG_PAGES)
  Dim As SubType Pointer CurrSubPtr
  Dim As LinkedListElement Pointer CurrLine
  Dim As String SubNameOut, ErrTemp, SubLoc

  Dim As SubType Pointer IntSub
  Dim As Integer IntSubLoc

  Dim MaxDestPage as Integer = 0  
  Dim DataBlockCount as Integer = 0
  Dim CurrEPTable as Integer
  Dim OrgPosOffset as Integer
  Dim NonChipFamily16DataBlocksNotAdded As Integer = -1

  'Set ForceFit if on AVR or 18F
  'OptimiseCalls and OptimiseIf may reduce the code enough to fit, let the assembler worry if it doesn't
  ForceFit = 0
  If ModeAVR Or ChipFamily = 16 Then
    ForceFit = -1
  End If

  Dim As Integer SubQueue(SBC + 1)
  SubQueueCount = 0

  'Generate call tree of program
  CreateCallTree

  'Get location of interrupt sub (if it exists)
  IntSub = 0
  IntSubLoc = LocationOfSub("INTERRUPT", "")   'was capitalised
  If IntSubLoc <> 0 Then
    IntSub = Subroutine(IntSubLoc)
  End If

  If ForceFit Then
    'Force subs to fit
    'Put them all on page 1, assembler can deal with the mess
    For Temp = 0 To SBC
      With *Subroutine(Temp)
        If .Required Then
          .LocationSet = -1
          .DestPage = 1

          'Sub size is used for information only on 18F/AVR
          CalcSubSize(Subroutine(Temp))
        End If
      End With
    Next

  Else
    'Don't force subs to fit
    'Try to put them all in place, if they don't fit show an error
    'This should be used on 10F/12F/16F PIC, not on AVR or 18F

    'Measure size of subs
    TotalProgSize = 0
    For CurrSub = 0 To SBC
      If Subroutine(CurrSub)->Required Then
        CalcSubSize(Subroutine(CurrSub))
        TotalProgSize += Subroutine(CurrSub)->HexSize
        'Is subroutine too big to fit anywhere?
        If Subroutine(CurrSub)->HexSize > 2047 Then
          ErrTemp = Message("SubToBigForPage")
          Replace ErrTemp, "%sub%", Subroutine(CurrSub)->Name
          Replace ErrTemp, "%size%", Str(Subroutine(CurrSub)->HexSize)
          LogError ErrTemp, ""
        End If
        'Clear location set flag as well
        Subroutine(CurrSub)->LocationSet = 0
      End If
    Next

    'Get list of subs sorted by size, excluding those forced to page 0
    'Copy list of locations
    RequiredSubs = 0
    For Temp = 0 To SBC
      If Subroutine(Temp)->Required Then
        If Not Subroutine(Temp)->FirstPage Then
          SubQueueCount += 1
          SubQueue(SubQueueCount) = Temp
        End If
        RequiredSubs += 1
      End If
    Next
    'Sort by size
    '(Use insertion sort)
    For SortPos = 1 To SubQueueCount
      ThisSub = SubQueue(SortPos)
      MovePos = SortPos - 1
      'Do While MovePos > 0 And Subroutine(ThisSub)->HexSize > Subroutine(SubQueue(MovePos))->HexSize
      Do While MovePos > 0 And Subroutine(ThisSub)->MaxHexSize > Subroutine(SubQueue(MovePos))->MaxHexSize
        SubQueue(MovePos + 1) = SubQueue(MovePos)
        MovePos -= 1
      Loop
      SubQueue(MovePos + 1) = ThisSub
    Next

    'Clear all pages
    For CurrPage = 1 To ProgMemPages
      ProgMemPage(CurrPage).CodeSize = 0
    Next

    'Try moving subs and counting sizes until everything stabilises
    FitAttempts = 0
    Do
      FitAttempts += 1
      SubsAdded = 0

      'Prepare subroutines for allocation
      For CurrSub = 0 To SBC
        'Clear location set flags on subs
        Subroutine(CurrSub)->LocationSet = 0
        'Clear call counts
        For CurrPage = 0 To ProgMemPages
          Subroutine(CurrSub)->CallsFromPage(CurrPage) = 0
        Next
      Next

      'Clear size counters on pages
      For CurrPage = 1 To ProgMemPages
        ProgMemPage(CurrPage).CodeSize = 0
      Next

      'Position subroutines
      'Page 0 subs
      FirstPageFull = 0
      For CurrSub = 0 To SBC
        If Subroutine(CurrSub)->Required And Subroutine(CurrSub)->FirstPage Then
          Subroutine(CurrSub)->DestPage = 0
          'Print "Placing "; Subroutine(CurrSub)->Name; " on first page"
          ProgMemPage(1).CodeSize += Subroutine(CurrSub)->MaxHexSize
          If ProgMemPage(1).CodeSize < ProgMemPage(1).MaxSize Then
            SubsAdded += 1
            Subroutine(CurrSub)->LocationSet = -1
            Subroutine(CurrSub)->DestPage = 1
            'Record calls to other subroutines
            UpdateOutgoingCalls Subroutine(CurrSub)
          Else
            'Once the first page is full, no point trying to locate anything else
            FirstPageFull = -1
            GoTo StopTryingToFit
          End If
        End If
      Next

      'Other subs
      For CurrSub = 1 To SubQueueCount
        CurrSubPtr = Subroutine(SubQueue(CurrSub))
        'Print "Placing "; CurrSubPtr->Name; " anywhere"
        'Decide which pages are better for this subroutine
        For CurrPage = 1 To ProgMemPages
          AllocationOrder(CurrPage).Page = CurrPage
          AllocationOrder(CurrPage).Calls = CurrSubPtr->CallsFromPage(CurrPage)
        Next
        SortMore = -1
        Do While SortMore
          SortMore = 0
          For CurrPage = 1 To ProgMemPages - 1
            If AllocationOrder(CurrPage).Calls < AllocationOrder(CurrPage + 1).Calls Then
              Swap AllocationOrder(CurrPage), AllocationOrder(CurrPage + 1)
              SortMore = -1
            End If
          Next
        Loop

        'Where will this sub fit?
        'Note 2/6/2013: Implemented allocation order, and a program went from 7748 to 7761 words
        'Disabled again, more testing needed to see if it produces an overall benefit
        For CurrPage = 1 To ProgMemPages
        'For CurrListPage = 1 To ProgMemPages
        ' CurrPage = AllocationOrder(CurrListPage).Page

          With ProgMemPage(CurrPage)
            If .MaxSize >= .CodeSize + CurrSubPtr->MaxHexSize Then
              'Print , "page "; CurrPage; ", size "; CurrSubPtr->HexSize; ", curr filled "; .CodeSize; "/"; .MaxSize
              'Found a location
              CurrSubPtr->LocationSet = -1
              CurrSubPtr->DestPage = CurrPage
              .CodeSize += CurrSubPtr->MaxHexSize
              SubsAdded += 1
              'Record calls to other subroutines
              UpdateOutgoingCalls Subroutine(CurrSub)
              GoTo LocateNextSub
            End If
          End With
        Next

        LocateNextSub:
      Next

      'Calc new sizes
      OldTotalProgSize = TotalProgSize
      TotalProgSize = 0
      For CurrSub = 0 To SBC
        If Subroutine(CurrSub)->Required Then
          CalcSubSize(Subroutine(CurrSub))
          TotalProgSize += Subroutine(CurrSub)->HexSize
        End If
      Next

      'If everything still fits, can exit
      SubsFit = -1
      For CurrPage = 0 To ProgMemPages
        ProgMemPage(CurrPage).CodeSize = 0
        For CurrSub = 0 To SBC
          With *Subroutine(CurrSub)
            If .Required And .DestPage = CurrPage Then
              If .LocationSet Then
                ProgMemPage(CurrPage).CodeSize += .HexSize
                If ProgMemPage(CurrPage).CodeSize > ProgMemPage(CurrPage).MaxSize Then
                  SubsFit = 0
                  GoTo UnableToFit
                End If
              Else
                SubsFit = 0
                GoTo UnableToFit
              End If
            End If
          End With
        Next
      Next
      UnableToFit:
      If SubsFit And SubsAdded = RequiredSubs Then Exit Do

      'Print "Fit attempt"; FitAttempts; ", page sizes ";
      'For CurrPage = 1 To ProgMemPages
      ' Print Str(CurrPage); ":"; Str(ProgMemPage(CurrPage).CodeSize); " ";
      'Next
      'Print

    Loop While FitAttempts < 20
    StopTryingToFit:

    'If not all subs have been added, show an error
    If Not SubsFit Then
      If FirstPageFull Then
        LogError Message("FirstPageFull")
      Else
        LogError Message("OutOfProgMem")
      End If
    End If
  End If

  'Get a list of subs sorted into alphabetical order
  'Copy list of locations
  SubQueueCount = 0
  For Temp = 0 To SBC
    If Subroutine(Temp)->Required Then
      SubQueueCount += 1
      SubQueue(SubQueueCount) = Temp
    End If
  Next
  'Sort by name
  '(Use insertion sort)
  For SortPos = 2 To SubQueueCount
    ThisSub = SubQueue(SortPos)
    MovePos = SortPos - 1
    Do While MovePos > 1 And Subroutine(ThisSub)->Name < Subroutine(SubQueue(MovePos))->Name
      SubQueue(MovePos + 1) = SubQueue(MovePos)
      MovePos -= 1
    Loop
    SubQueue(MovePos + 1) = ThisSub
  Next

  'Add jumps in required vector locations
  CurrLine = CompilerOutput->CodeEnd
  CurrLine = LinkedListInsertList(CurrLine, GenerateVectorCode)

  'If on page 0 on 10/12/16, and interrupt exists, add it
  If ModePIC Then
    If IntSub <> 0 And ChipFamily <> 16 Then

      'Alter calls in sub (Use long/short calls as needed)
      AddPageCommands(IntSub)
      'Add sub code
      CurrLine = LinkedListInsertList(CurrLine, IntSub->CodeStart)
      'Blank, stars, blank at end
      CurrLine = LinkedListInsert(CurrLine, "")
      CurrLine = LinkedListInsert(CurrLine, Star80)
      CurrLine = LinkedListInsert(CurrLine, "")

      'Mark as no longer required so it isn't added again
      IntSub->Required = 0
    End If
  End If

  ' Calculate max page used and how many DATA blocks 
  For CurrSub = 1 To SubQueueCount
    If CurrSubPtr <> 0 Then
      If CurrSubPtr->DestPage > MaxDestPage then 
        MaxDestPage = CurrSubPtr->DestPage
      End If
    End If
  Next
  ' Print "Max Page Used " + Str(MaxDestPage)

  For CurrEPTable = 1 TO DataTables
    With DataTable(CurrEPTable)
      If .IsData = -1 Then
        DataBlockCount = DataBlockCount +1
      End If       
    End With
  Next
  'Add all subs to output code
  'Add subs that could be located
 
  For CurrPage = 1 To ProgMemPages
    
    If ( ChipFamily <> 16 And MaxDestPage = CurrPage - 1 and CurrPagePos <> 0 )   Then
          ' Handle the DATABLOCKs here for non 18F or AVR
          AddDataBlocks ( CurrLine, CurrPage, CurrPagePos, DataBlockCount, NonChipFamily16DataBlocksNotAdded  )
          NonChipFamily16DataBlocksNotAdded = 0
    End If

    If UserCodeOnlyEnabled = 0  Then
      If CurrPage > 0 and CurrPage <= ProgMemPages Then
        'Only output this line when there this is normal GCB compilation and there is no data blocks being processsed.
        CurrLine = LinkedListInsert(CurrLine, ";Program_memory_page: " + Str(CurrPage - 1) )
      End If
    End If
    
    If ModePIC Then
      If CurrPage <= ProgMemPages Then 
        If ChipFamily = 16 Or IntSub = 0 Or CurrPage > 1 Then
            If UserCodeOnlyEnabled = 0  Then

              If CurrPage > 1 then
                  CurrLine = LinkedListInsert(CurrLine, " PSECT Progmem" + Str(CurrPage - 1) + ",class=CODE,space=SPACE_CODE,delta=2, abs, ovrld " )
                  GetMetaData(CurrLine)->IsPICAS = -1
              End if

              CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(ProgMemPage(CurrPage).StartLoc))
              CurrPagePos = ProgMemPage(CurrPage).StartLoc

            Else
                'Output as comment
                CurrLine = LinkedListInsert(CurrLine, ";ORG " + Str(ProgMemPage(CurrPage).StartLoc))
            End If
        Else
          'On 10/12/16, Interrupt must go at very start of program
          'Allow it space in page 0.
          If UserCodeOnlyEnabled = 0  Then
              CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(ProgMemPage(CurrPage).StartLoc + IntSub->HexSize))
              CurrPagePos = ProgMemPage(CurrPage).StartLoc + IntSub->HexSize
          Else
            'only output once
            If CurrPage = 1 then
                CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(ProgMemPage(CurrPage).StartLoc + IntSub->HexSize))
                CurrPagePos = ProgMemPage(CurrPage).StartLoc + IntSub->HexSize
            Else
                CurrLine = LinkedListInsert(CurrLine, ";ORG " + Str(ProgMemPage(CurrPage).StartLoc + IntSub->HexSize))
            End  If
          End If
        End If
      End If

      'On 12 bit, insert list of GOTOs to allow calls to second half of pages
      If ChipFamily = 12 Then
        Dim GotoMainInserted As Integer = 0
        CurrLine = LinkedListInsert(CurrLine, ";Indirect jumps to allow calls to second half of page")
        For CurrSub = 1 To SubQueueCount
          CurrSubPtr = Subroutine(SubQueue(CurrSub))
          If CurrSubPtr->Required And CurrSubPtr->LocationSet And CurrSubPtr->DestPage = CurrPage Then
            'Need to put the sub here
            'Name of sub
            If SubQueue(CurrSub) <> 0 Then
              SubNameOut = GetSubFullName(SubQueue(CurrSub))
              CurrPagePos += 1
              StatsUsedProgram += 1

              If Not GotoMainInserted Then
                CurrLine = LinkedListInsert(CurrLine, " goto BASPROGRAMSTART")
                GotoMainInserted = -1
              End If

              CurrLine = LinkedListInsert(CurrLine, SubNameOut)
              GetMetaData(CurrLine)->IsLabel = -1
              CurrLine = LinkedListInsert(CurrLine, " goto SysInd_" + SubNameOut)
            End If

          End If
        Next
        CurrLine = LinkedListInsert(CurrLine, "")
      End If

    ElseIf ModeAVR Then
      CurrLine = LinkedListInsert(CurrLine, ".ORG " + Str(ProgMemPage(CurrPage).StartLoc))
      CurrPagePos = ProgMemPage(CurrPage).StartLoc
    ElseIf ModeZ8 Then
      CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(ProgMemPage(CurrPage).StartLoc))
      CurrPagePos = ProgMemPage(CurrPage).StartLoc
    End If

    'check for #REQUIRED PARAMETERS
    For CurrSub = 1 To SubQueueCount
      CurrSubPtr = Subroutine(SubQueue(CurrSub))
      If CurrSubPtr->Required And CurrSubPtr->LocationSet And CurrSubPtr->DestPage = CurrPage Then

        'Check for required parameters
        Dim RequiredString as String
        Dim MandatedStringMessage as String
        Dim OutMessage as String
        Dim RequiredCommand as String
        Dim As String ReqChip, ReqConst

        Dim As LinkedListElement Pointer RequiredListPos, ItemList, ItemListPos
        Dim ReqListItem As String

        'Get array of REQUIRED pieces from current file
        Dim SourceFileNo As Integer = CurrSubPtr->SourceFile
        RequiredListPos = 0
        If SourceFile(SourceFileNo).RequiredModules <> 0 Then
          RequiredListPos = SourceFile(SourceFileNo).RequiredModules->Next
        End If
        Do While RequiredListPos <> 0

          RequiredCommand = RequiredListPos->Value
          ItemList = GetElements(RequiredCommand, " ")
          ItemListPos = ItemList->Next
          If LinkedListSize(ItemList) >= 3 Then
            ' Req_Res(0)  will always be REQUIRED
            ItemListPos = ItemListPos->Next
            ' Req_Res(1)  PIC or AVR
            ReqChip = ItemListPos->Value
            ItemListPos = ItemListPos->Next
            ' Req_Res(2)  what we are looking for ... a constant in the DAT file
            ReqConst = ItemListPos->Value
            ItemListPos = ItemListPos->Next

            'Build the string  from element 2 onwards
            RequiredString = ""
            Do While ItemListPos <> 0
              RequiredString = RequiredString + ItemListPos->Value
              ItemListPos = ItemListPos->Next
            Loop

            IF (UCASE(ReqChip) = "PIC" AND ModePIC) or (UCASE(ReqChip) = "AVR" AND ModeAVR) Then
              'Do we have SFR?
              IF Not HasSFR(ReqConst) Then
                'Do we have no constant, or a constant that's zero?
                If Val(HashMapGetStr(Constants, UCase(ReqConst))) = 0 Then

                  If Instr(RequiredString, ";STRING" ) <> 0 Then
                      MandatedStringMessage = GetString(RequiredString)
                      OutMessage = Message("BadMandatedOption")
                      Replace OutMessage, "%string%", MandatedStringMessage
                  End if

                  If Left(RequiredString, 1 ) = "%"  Then
                    MandatedStringMessage = RequiredString
                    Replace MandatedStringMessage , "%", ""
                    Replace MandatedStringMessage , "%", ""
                    OutMessage = Message(MandatedStringMessage)
                  End if
                  ErrorsFound = -1
                  LogOutputMessage mid( SourceFile(SourceFileNo).filename, 1+instrRev(SourceFile(SourceFileNo).filename,"\")  )+" (0): Error: "+OutMessage
                End if
              End If
             end if

          Else
            'Malformed required list

          End If

          RequiredListPos = RequiredListPos->Next
        Loop

        'Need to put the sub here
        'Name of sub
        If SubQueue(CurrSub) = 0 Then
          If UserCodeOnlyEnabled = 0 Then
              SubNameOut = "BASPROGRAMSTART"
          End if
        Else
          SubNameOut = GetSubFullName(SubQueue(CurrSub))
          If ChipFamily = 12 Then
            SubNameOut = "SysInd_" + SubNameOut
          End If
        End If
        ' Print "Placing " + SubNameOut + " at 0x" + Hex(CurrPagePos) + " (size:" + Str(CurrSubPtr->HexSize) + ")"
        CurrPagePos += CurrSubPtr->HexSize
        StatsUsedProgram += CurrSubPtr->HexSize

        SubLoc = GetFileLine(CurrSubPtr->Origin)
        If PreserveMode = 2 Then
            If CurrSubPtr->Overloaded Then
              CurrLine = LinkedListInsert(CurrLine, ";Overloaded signature: " + GetSubSig(*CurrSubPtr) + ", Source: " + SubLoc)
            Else
              If SubLoc <> "" Then CurrLine = LinkedListInsert(CurrLine, ";Source: " + SubLoc)
            End If
        End If
        'CurrLine = LinkedListInsert(CurrLine, ";Subroutine size:" + Str(CurrSubPtr->HexSize) + " words")
        CurrLine = LinkedListInsert(CurrLine, SubNameOut + LabelEnd)
        GetMetaData(Currline)->IsLabel = -1
        'Alter calls in sub (Use long/short calls as needed)
        AddPageCommands(CurrSubPtr)
        'Add sub code
        CurrLine = LinkedListInsertList(CurrLine, CurrSubPtr->CodeStart)
        'Return
        If Not CurrSubPtr->NoReturn Then
          If ModePIC Then
            If ChipFamily = 12 Then
              CurrLine = LinkedListInsert(CurrLine, " retlw 0")
            Else
              CurrLine = LinkedListInsert(CurrLine, " return")
            End If
          ElseIf ModeAVR Then
            CurrLine = LinkedListInsert(CurrLine, " ret")
          ElseIf ModeZ8 Then
            CurrLine = LinkedListInsert(CurrLine, " ret")
          End If
        End If
        'Blank, stars, blank at end
        CurrLine = LinkedListInsert(CurrLine, "")
        CurrLine = LinkedListInsert(CurrLine, Star80)
        CurrLine = LinkedListInsert(CurrLine, "")
      End If
    Next

    'This is needed as in there is NO GCB source.
    If NonChipFamily16DataBlocksNotAdded And MaxDestPage = CurrPage - 1 Then 
      AddDataBlocks ( CurrLine, CurrPage, CurrPagePos, DataBlockCount, NonChipFamily16DataBlocksNotAdded  )
      NonChipFamily16DataBlocksNotAdded = 0
    End If

  Next
  

  'Add any subs that couldn't be located to end of file
  'User may be able to manually locate them
  Dim As Integer FirstUnsetSub
  FirstUnsetSub = -1
  'For CurrSub = 0 To SBC
  For CurrSub = 1 To SubQueueCount
    CurrSubPtr = Subroutine(SubQueue(CurrSub))
    If CurrSubPtr->Required And Not CurrSubPtr->LocationSet Then
      If FirstUnsetSub Then
        CurrLine = LinkedListInsert(CurrLine, ";Subs that could not be automatically placed")
        CurrLine = LinkedListInsert(CurrLine, ";It may be possible to manually locate them, but you should upgrade to a larger chip if possible")
        CurrLine = LinkedListInsert(CurrLine, "")
        FirstUnsetSub = 0
      End If
      'Need to put the sub here
      'Name of sub
      If SubQueue(CurrSub) = 0 Then
        SubNameOut = "BASPROGRAMSTART"
      Else
        SubNameOut = GetSubFullName(SubQueue(CurrSub))
      End If
      CurrLine = LinkedListInsert(CurrLine, SubNameOut + LabelEnd)
      GetMetaData(Currline)->IsLabel = -1
      CurrLine = LinkedListInsert(CurrLine, ";This sub size:" + Str(CurrSubPtr->HexSize))
      StatsUsedProgram += CurrSubPtr->HexSize
      'Add sub code
      CurrLine = LinkedListInsertList(CurrLine, CurrSubPtr->CodeStart)
      'Return
      If Not CurrSubPtr->NoReturn Then
        If ModePIC Then
          CurrLine = LinkedListInsert(CurrLine, " return")
        ElseIf ModeAVR Then
          CurrLine = LinkedListInsert(CurrLine, " ret")
        ElseIf ModeZ8 Then
          CurrLine = LinkedListInsert(CurrLine, " ret")
        End If
      End If
      'Blank, stars, blank at end
      CurrLine = LinkedListInsert(CurrLine, "")
      CurrLine = LinkedListInsert(CurrLine, Star80)
      CurrLine = LinkedListInsert(CurrLine, "")
    End If
  Next

  If ChipFamily = 16 or MODEAVR Then 
    AddDataBlocks ( CurrLine, CurrPage, CurrPagePos, DataBlockCount, NonChipFamily16DataBlocksNotAdded )
  End If

  'Add EEPROM data tables
  Dim As Integer EPDataHeader, EPDataLoc, CurrEPItem, TableAddressState, AVRAddressState
  Dim As String EPTempData
  Dim As Integer EPAddress, DataTableSwapped
  Dim As DataTableType Temp_DataTableType
  EPDataHeader = 0
  TableAddressState = 0
  AVRAddressState = 0
  IF DataTables > 0 Then

    'Sort table so GCASM handles TABLE and EEPROM Datasets ORG correctly.
    Do
      DataTableSwapped = 0 
      For CurrEPTable = 1 TO DataTables - 1
        If DataTable(CurrEPTable).Used And DataTable(CurrEPTable).StoreLoc = 1 Then
            If DataTable(CurrEPTable).FixedLoc > DataTable(CurrEPTable+1).FixedLoc Then
              Swap   DataTable(CurrEPTable) , DataTable(CurrEPTable+1)
              DataTableSwapped = -1
            End If
        End IF
      Next
    Loop While DataTableSwapped = -1


    For CurrEPTable = 1 TO DataTables
      If DataTable(CurrEPTable).Used And DataTable(CurrEPTable).StoreLoc = 1 Then

        'Add header (if not added)
        If Not EPDataHeader Then
          CurrLine = LinkedListInsert(CurrLine, Star80)
          CurrLine = LinkedListInsert(CurrLine, "")

          If ChipFamily = 12 Or ChipFamily = 14 Then
            CurrLine = LinkedListInsert(CurrLine, "; Data Tables (data memory)")
            'CurrLine = LinkedListInsert(CurrLine, " ORG 0x2100")
            EPAddress = &h2100

          ElseIf ChipFamily = 15 Then
            CurrLine = LinkedListInsert(CurrLine, "; Data Tables (data memory)")
            'CurrLine = LinkedListInsert(CurrLine, " ORG 0xF000")
            EPAddress = &hF000
          ElseIf ChipFamily = 16 Then
            If ( ChipSubFamily = ChipFamily18FxxQ43  )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ43 EEPROM Address 0x380000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x380000")
              EPAddress = &h380000
            ElseIf ( ChipSubFamily = ChipFamily18FxxQ41 )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ41 EEPROM Address 0x380000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x380000")
              EPAddress = &h380000
            ElseIf ( ChipSubFamily = ChipFamily18FxxK40  )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxK40 EEPROM Address 0x310000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x310000")
              EPAddress = &h310000
            ElseIf ( ChipSubFamily = ChipFamily18FxxQ10  )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ10 EEPROM Address 0x310000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x310000")
              EPAddress = &h310000
            ElseIf ( ChipSubFamily = ChipFamily18FxxQ40  )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ40 EEPROM Address 0x380000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x380000")
              EPAddress = &h380000
            ElseIf ( ChipSubFamily = ChipFamily18FxxQ71 )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ71 EEPROM Address 0x380000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x380000")
              EPAddress = &h380000
            ElseIf ( ChipSubFamily = ChipFamily18FxxQ83 )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ83 EEPROM Address 0x380000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x380000")
              EPAddress = &h380000    
            ElseIf ( ChipSubFamily = ChipFamily18FxxQ20 )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ20 EEPROM Address 0x380000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x380000")
              EPAddress = &h380000                     
            ElseIf ( ChipSubFamily = ChipFamily18FxxQ24 )  then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamily18FxxQ24 EEPROM Address 0x380000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x380000")
              EPAddress = &h380000                                         
            ElseIf ChipFamilyVariant = 1 then
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (ChipFamilyVariant EEPROM Address 0x310000)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0x310000")
              EPAddress = &h380000
            Else
              ' this does not support AVR, see https://microchip.my.site.com/s/article/EEPROM-Segment-Preloading-in-Assembly-code for correct format
              CurrLine = LinkedListInsert(CurrLine, "; Data Tables (Default EEPROM address)")
              'CurrLine = LinkedListInsert(CurrLine, " ORG 0xF00000")
              EPAddress = &hF00000
            End if
            
          End If
          EPDataHeader = -1
          EPDataLoc = 0
        End If
        
        With DataTable(CurrEPTable)

          If .Items > 0 OR .IsEEPromData = 0 Then

          'Construct string using a variable
          Dim EETempVal as integer
          EETempVal = EPAddress+.FixedLoc
          If ModePIC Then
            If TableAddressState = 0 or .IsEEPromData Then
              'only show the ORG for table address once.
              TableAddressState = - 1
              CurrLine = LinkedListInsert(CurrLine, "ORG 0x"+HEX(EETempVal))
            End If
          Else
            If AVRAddressState = 0 or .IsEEPromData Then
              'only show the ORG for table address once.
              AVRAddressState = - 1
              CurrLine = LinkedListInsert(CurrLine, ".ESEG")
              CurrLine = LinkedListInsert(CurrLine, ".ORG 0x"+Right("0"+HEX(EETempVal),2))
            End If
          End If


            'Get data
            
            If .IsEEPromData = 0 Then 
              'Output the size of the table, not, required for EEData
              EPTempData = "0x"+Right("0"+Hex(.Items),2)
            Else
              EPTempData = ""
            End If
            
            For CurrEPItem = 1 To .Items
              EPTempData = EPTempData + ", 0x" + Right("0"+HEX(.Item(CurrEPItem)),2)
            Next
            If Left(EPTempData, 2 ) = ", " Then EPTempData = Mid(EPTempData,3, Len( EPTempData ) ) 
            'Add table
            ToAsmSymbols += 1
            If .IsEEPromData = 0 then
              ToAsmSymbol(ToAsmSymbols, 1) = " TABLE" + Trim(.Name)
            Else
              ToAsmSymbol(ToAsmSymbols, 1) = " DATA" + Trim(.Name)
            End If
            'This test ensure the address of the DATA is ALIGNed to 2 for ChipFamily = 16 
            If (  EPDataLoc/2 <> Int(EPDataLoc/2) and ChipFamily = 16 ) Then
                EPDataLoc = EPDataLoc + 1
            End If
            ToAsmSymbol(ToAsmSymbols, 2) = Str(EPDataLoc)

            if instr(UCase(AsmExe),"PIC-AS") = 0 then
              If .IsEEPromData = 0 then
                If MODEPIC Then CurrLine = LinkedListInsert(CurrLine, " TABLE" + Trim(.Name) + " equ " + Str(EPDataLoc))
                If MODEAVR Then CurrLine = LinkedListInsert(CurrLine, " TABLE" + Trim(.Name) + ":")
              Else
                If MODEPIC Then CurrLine = LinkedListInsert(CurrLine, "DATA" + Trim(.Name) + " equ " + Str(EPDataLoc))
                If MODEAVR Then CurrLine = LinkedListInsert(CurrLine, " DATA" + Trim(.Name) + ":")
              End If
              GetMetaData(Currline)->IsLabel = -1
              If trim(EPTempData) <> "" Then 
                'Do not push out empty structure, as this will cause an error in MPASM etc
                If Instr(UCase(AsmExe), "MPASM") > 0 Then
                  If Len(EPTempData) < 129 Then
                    CurrLine = LinkedListInsert(CurrLine, "  de " + EPTempData)
                  Else
                    ' Only output width that MPASM can cope with
                    EPTempData = EPTempData +","  'add additional delimter to make this routine work

                    Dim as Integer EEStringCut, EELastDelimiter 
                    For EEStringCut = 1 to Len(EPTempData) step 86
                      EELastDelimiter = InStrRev( Mid ( EPTempData, EEStringCut, 85 ), "," ) - 1
                      'Found EELastDelimiter so adjust loop
                      if EELastDelimiter <> -1 Then 
                        CurrLine = LinkedListInsert(CurrLine, "  de " + Mid ( EPTempData, EEStringCut, EELastDelimiter ) )
                        EEStringCut =  EEStringCut - ( 86 - EELastDelimiter) + 2
                      End If
                    Next
                  End if
                Else
                  If ModePIC Then CurrLine = LinkedListInsert(CurrLine, "  de " + EPTempData)
                  If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, "  .DB " + EPTempData)
                End If
              End If
            else
              If .IsEEPromData = 0 then
                CurrLine = LinkedListInsert(CurrLine, " TABLE" + Trim(.Name))
              Else
                CurrLine = LinkedListInsert(CurrLine, " DATA" + Trim(.Name))
              End If

              GetMetaData(Currline)->IsLabel = -1
              CurrLine = LinkedListInsert(CurrLine, "  db " + EPTempData)
            end if

            EPDataLoc += (.Items + 1)
          Else
            'Delete the comment line
            LinkedListDelete(CurrLine)
          End IF
        End With
        'move down as we dont need to align on every table
        IF AFISupport = 1  and ModePIC and ChipFamily = 16 Then
          'There should only one table of this type, and, align may not be needed... but, I do not know if this is the only once....
          CurrLine = LinkedListInsert(CurrLine, " ALIGN 2 ;X1")
        End if


      End If

    Next

    IF AFISupport = 1  and ModePIC and ChipFamily = 16 Then
      'There should only one table of this type, and, align may not be needed... but, I do not know if this is the only once....
      CurrLine = LinkedListInsert(CurrLine, " ALIGN 2;X2")
    End if

  End If

  'Check for overlapping tables and datasets
  'Iterate through tables, if EE ( .StoreLoc = 1 ), take the base address add the items if less than the next valid address then all is good, else error message
  Dim as Integer EETempVal, EENextLocation
  For CurrEPTable = 1 TO DataTables - 1
    If DataTable(CurrEPTable).Used And DataTable(CurrEPTable).StoreLoc = 1 Then
      ' Extract next valid table that is EEdata
      EETempVal = CurrEPTable + 1
      EENextLocation = 0
      Do While DataTable(EETempVal).StoreLoc = 0 and EETempVal < DataTables
        EETempVal = EETempVal + 1
      Loop 
      'This is the next EE address
      EENextLocation = DataTable(EETempVal).FixedLoc
        'show the calcs
        ' print DataTable(CurrEPTable).FixedLoc,  DataTable(CurrEPTable).Items, EENextLocation, ( DataTable(CurrEPTable).FixedLoc + DataTable(CurrEPTable).Items )  <= EENextLocation 
      If ( NOT ( DataTable(CurrEPTable).FixedLoc + DataTable(CurrEPTable).Items )  <= EENextLocation ) AND EENextLocation <> 0 Then
          ErrTemp = Message("EEBadORG")
          Replace ErrTemp, "%loc%", "0x"+hex( EPAddress + DataTable(CurrEPTable+1).FixedLoc)
          LogError("GCASM:" + ErrTemp, "")
      End iF
    End If
  Next

  CompilerOutput->CodeEnd = CurrLine

End Sub

Function Message (InData As String) As String

  Dim As String Temp, MsgOut, MsgID
  Dim As Integer DS, T

  'Find message
  MsgID = UCase(InData)
  DS = 0
  FOR T = 1 TO MSGC
    IF Messages(1, T) = MsgID THEN DS = T: EXIT FOR
  NEXT

  'Retrieve message
  IF DS = 0 THEN
    Return "Message " + MsgID + " not defined in messages.dat!"
  Else
    MsgOut = Messages(2, DS)
  End If

  'Replace values
  IF INSTR(UCase(MsgOut), "%SHORTFILENAME%") <> 0 THEN 
    Replace MsgOut, "%SHORTFILENAME%", Mid(FI, InstrRev(FI, "\")+1) 
  End If
  IF INSTR(UCase(MsgOut), "%VERSION%") <> 0 THEN Replace MsgOut, "%VERSION%", Version
  IF INSTR(UCase(MsgOut), "%FILENAME%") <> 0 THEN Replace MsgOut, "%FILENAME%", FI
  IF INSTR(UCase(MsgOut), "%OUTPUTFILE%") <> 0 THEN Replace MsgOut, "%OUTPUTFILE%", OFI
  IF INSTR(UCase(MsgOut), "%CHIPNAME%") <> 0 THEN Replace MsgOut, "%CHIPNAME%", ChipName
  

  Return MsgOut
END FUNCTION

Function ModeAVR As Integer
  If ChipFamily >= 100 And ChipFamily < 200 Then Return -1
  Return 0
End Function

Function ModePIC As Integer
  If ChipFamily > 0 And ChipFamily < 100 Then Return -1
  Return 0
End Function

Function ModeZ8 As Integer
  If ChipFamily > 200 And ChipFamily < 300 Then Return -1
  Return 0
End Function

Function NewCodeSection As CodeSection Pointer
  Dim As CodeSection Pointer OutSection

  OutSection = Callocate(SizeOf(CodeSection))
  OutSection->CodeList = LinkedListCreate
  OutSection->CodeEnd = OutSection->CodeList

  Return OutSection
End Function

Function NewProgLineMeta As ProgLineMeta Pointer
  Dim As ProgLineMeta Pointer OutMeta

  OutMeta = Callocate(SizeOf(ProgLineMeta))

  With (*OutMeta)
    .RequiredBank = -1

    .IsAutoPageSel = 0

    .PrevCommands = LinkedListCreate
    .NextCommands = LinkedListCreate
  End With

  Return OutMeta
End Function

Function NewSubroutine(SubName As String) As SubType Pointer
  Dim As SubType Pointer OutSub

  OutSub = Callocate(SizeOf(SubType))
  With (*OutSub)
    .Name = SubName
    .CodeStart = LinkedListCreate
    .CallList = LinkedListCreate
    .CallerList = LinkedListCreate
    .TemporaryVars = LinkedListCreate

    .IntStateSaveVar = ""
    .ReturnAlias = ""
  End With

  Return OutSub
End Function

FUNCTION TypeOfVar (VarName As String, CurrSub As SubType Pointer) As String

  Dim As String Temp, TempType, Source
  Dim As String GlobalType, LocalType
  Dim As Integer SubLoc
  Dim As VariableType Pointer FoundVar
  Dim As SubType Pointer MainSub
  Dim As Single TempValue

  'Get pointer to main sub
  MainSub = Subroutine(0)
  'Do this to prevent null pointer access
  If CurrSub = 0 Then
    LogError( "'"+Varname+"'", Message("InternalError") )

    CurrSub = MainSub
  End If

  'Bit variables with . override all
  If INSTR(VarName, ".") <> 0 Then TypeOfVar = "BIT": Exit Function

  'Type overrides
  If INSTR(VarName, "[") <> 0 Then
    If INSTR(LCase(VarName), "[double]") <> 0 Then Return "DOUBLE"
    If INSTR(LCase(VarName), "[single]") <> 0 Then Return "SINGLE"
    If INSTR(LCase(VarName), "[long]") <> 0 Then Return "LONG"
    If INSTR(LCase(VarName), "[integer]") <> 0 Then Return "INTEGER"
    If INSTR(LCase(VarName), "[word]") <> 0 Then Return "WORD"
    If INSTR(LCase(VarName), "[byte]") <> 0 Then Return "BYTE"
  End If

  'Handle constants
  If IsConst(VarName) Then
    If InStr(VarName, "@") <> 0 Then
      Return "WORD"
    Else
      TempValue = MakeDec(VarName)
      If TempValue = 0 Or TempValue = 1 Then Return "BIT"
      If TempValue >= 2 And TempValue <= 255 Then Return "BYTE"
      If TempValue >= 256 And TempValue <= 65535 Then Return "WORD"
      If TempValue >= -32768 And TempValue <= -1 Then Return "INTEGER"
      IF TempValue <> INT(TempValue) Then Return "SINGLE"
    End If
  End If

  'Temporary Calculation vars
  IF Left(UCase(VarName), 7) = "SYSTEMP" Then
    Temp = CalcVars(Val(Mid(VarName, 8))).CurrentType
    If Temp = "" Then Temp = "BYTE"
    Return Temp
  End If

  'Element is a function
  If UCase(VarName) = UCase(CurrSub->Name) And CurrSub->IsFunction Then
    Return CurrSub->ReturnType

  Else
    SubLoc = LocationOfSub(VarName, "", , -1)
    'Don't use overloaded type
    If SubLoc > 0 Then
      If Subroutine(SubLoc)->IsFunction Then
        With *(Subroutine(SubLoc))
          If .ReturnType <> "" Then
            Return .ReturnType
          End If
        End With
      End If
    End If
  End If

  'Get local type
  LocalType = "BYTE"
  GlobalType = "BYTE"
  If CurrSub <> MainSub Then

    'To proceed any further, sub needs to have dim commands compiled
    If Not CurrSub->VarsRead Then CompileDim(CurrSub)

    'Search var list
    FoundVar = HashMapGet(@(CurrSub->Variables), UCase(VarName))
    If FoundVar <> 0 Then
      LocalType = Trim(UCase(FoundVar->Type))
    End If
  End If

  'Permanent calculation vars
  If UCase(Left(VarName, 11)) = "SYSCALCTEMP" And LEN(VarName) = 12 And LocalType = "BYTE" Then
    Temp = UCase(Mid(VarName, 12, 1))
    TempType = ""
    IF Temp = "A" Then TempType = CalcTempType(1)
    IF Temp = "B" Then TempType = CalcTempType(2)
    IF Temp = "X" Then TempType = CalcTempType(3)
    IF TempType <> "" THEN Return TempType
  End If

  'Get global type
  FoundVar = HashMapGet(@(MainSub->Variables), UCase(VarName))
  If FoundVar <> 0 Then
    GlobalType = Trim(UCase(FoundVar->Type))
  End If

  'Get highest of local and global type
  '(Unless type in main sub is BIT, then it is a bit)
  If CastOrder(LocalType) > CastOrder(GlobalType) And GlobalType <> "BIT" Then
    Return LocalType
  Else
    Return GlobalType
  EndIf

End FUNCTION

FUNCTION TypeOfValue (ValueNameIn As String, CurrentSub As SubType Pointer, SingCharString As Integer = 0) As String
  'SingCharString: 0 = treat single char as byte, -1 = treat single char as string

  Dim As String ThisType, FinalType, Temp, ValueTemp, CurrentItem, CurrChar
  Dim As String ValueName
  Dim As Integer TCC, SS, CD, FindSub, SubLoc
  Dim TempValue As Double
  Dim TypeCheck( TYPECHECKSIZE ) As String
  TCC = 0
  FinalType = ""

  'If no value, don't return a type
  If ValueNameIn = "" Then Return ""
  ValueName = ValueNameIn

  'Type overrides
  If Left(ValueName, 1) = "[" And InStr(ValueName, "]") <> 0 Then
    ThisType = Mid(ValueName, 2)
    ThisType = Left(ThisType, InStr(ThisType, "]") - 1)
    Return UCase(Trim(ThisType))
  End If

  'Is whole thing a constant?
  If IsConst(ValueName) Then
    If InStr(ValueName, "@") <> 0 Then
      ThisType = "WORD"
    Else
      TempValue = MakeDec(ValueName)
      If TempValue = 0 Or TempValue = 1 Then
        ThisType = "BIT"
      ElseIf TempValue >= 2 And TempValue <= 255 Then
         ThisType = "BYTE"
      ElseIf TempValue >= 256 And TempValue <= 65535 Then
        ThisType = "WORD"
      ElseIf TempValue >= -32768 And TempValue <= -1 Then
        ThisType = "INTEGER"
      ElseIf TempValue >= 0 And TempValue <= (2 ^ 32 - 1) Then
        ThisType = "LONG"
      End If

      IF TempValue <> INT(TempValue) Then ThisType = "SINGLE"
      'If InStr(UCase(ValueName), ";STRING") <> 0 Then ThisType = "STRING"
      If InStr(UCase(ValueName), ";STRING") <> 0 Then
        If Len(GetString(ValueName)) = 1 And Not SingCharString Then
          ThisType = "BYTE"
        Else
          ThisType = "STRING"
      End If
      End If
    End If
    Return ThisType
  End If

  'Remove function parameters and array indexes
  'Find (
  'Then check for an operator or start of string before (
  'If no operator, remove from ( to matching ) inclusive
  If InStr(ValueName, "(") Then
    Do While InStr(ValueName, " ") <> 0: Replace ValueName, " ", "": Loop
    Dim As Integer DelStart, DelEnd, DelLevel, DelPos
    DelPos = 1
    'Find (
    Do While DelPos < Len(ValueName)
      CurrChar = Mid(ValueName, DelPos, 1)
      If CurrChar = "(" Then
        'Is there a character before ( ?
        If DelPos <> 1 Then
          'Is there something other than a symbol before (
          If Not IsDivider(Mid(ValueName, DelPos - 1, 1)) Then
            'There is a something other than an operator before (
            'So we have found an array index or function param
            'Find end of index/params
            DelStart = DelPos
            DelLevel = 0
            DelEnd = DelStart
            Do While DelEnd <= Len(ValueName)
              CurrChar = Mid(ValueName, DelEnd, 1)
              If CurrChar = "(" Then
                DelLevel += 1
              ElseIf CurrChar = ")" Then
                DelLevel -= 1
                If DelLevel = 0 Then
                  Exit Do
                End If
              End If
              DelEnd += 1
            Loop
            'Remove everything from start to end
            ValueName = Left(ValueName, DelStart - 1) + Chr(31) + Mid(ValueName, DelEnd + 1)
          End If
        End If
      End If

      DelPos += 1
    Loop
  End If

  'Split into individual elements
  CurrentItem = ""
  For SS = 1 To LEN(ValueName)
    Temp = Mid(ValueName, SS, 1)
    If IsDivider(Temp) AND Temp <> "." AND Temp <> ";" AND Temp <> "$" And Temp <> "[" And Temp <> "]" Then
      IF Trim(CurrentItem) <> "" Then
        TCC += 1
        If TCC > TYPECHECKSIZE Then
            'fatal error
            LogError "TYPECHECKSIZE exceed - reduce complexity: "+ValueNameIn
            WriteErrorLog
            ErrorsFound = - 1
            end
        End if
        TypeCheck(TCC) = Trim(CurrentItem)
        CurrentItem = ""
      End If
    Else
      CurrentItem = CurrentItem + Temp
    End If
  Next
  If Trim(CurrentItem) <> "" Then
    TCC += 1
      If TCC > TYPECHECKSIZE Then
        'fatal error
        LogError "TYPECHECKSIZE exceed - reduce complexity: "+ValueNameIn
        WriteErrorLog
        ErrorsFound = - 1
        end
    End if    
    TypeCheck(TCC) = Trim(CurrentItem)
  End If

  'Check each element
  Dim HasIndex As Integer
  For CD = 1 To TCC
    ValueTemp = TypeCheck(CD)
    HasIndex = 0
    If Right(ValueTemp, 1) = Chr(31) Then
      HasIndex = -1
      ValueTemp = Left(ValueTemp, Len(ValueTemp) - 1)
    End If
    ThisType = ""

    'Element is a string const
    If INSTR(ValueTemp, ";STRING") <> 0 Then
      If Len(GetString(ValueTemp)) = 1 And Not SingCharString Then
        ThisType = "BYTE"
      Else
        ThisType = "STRING"
      End If

    'Element has a cast
    ElseIf InStr(ValueTemp, "[") <> 0 And InStr(ValueTemp, "]") <> 0 Then
      ValueTemp = Mid(ValueTemp, InStr(ValueTemp, "[") + 1)
      ValueTemp = Left(ValueTemp, InStr(ValueTemp, "]") - 1)
      ThisType = UCase(Trim(ValueTemp))

    'Element is a const
    ElseIf IsConst(ValueTemp) Then
      If InStr(ValueTemp, "@") <> 0 Then
        ThisType = "WORD"
      Else
        TempValue = MakeDec(ValueTemp)
        If TempValue = 0 Or TempValue = 1 Then
          ThisType = "BIT"
        ElseIf TempValue >= 2 And TempValue <= 255 Then
           ThisType = "BYTE"
        ElseIf TempValue >= 256 And TempValue <= 65535 Then
          ThisType = "WORD"
        ElseIf TempValue >= -32768 And TempValue <= -1 Then
          ThisType = "INTEGER"
        ElseIf TempValue >= 0 And TempValue <= (2 ^ 32 - 1) Then
          ThisType = "LONG"
        End If

        IF TempValue <> INT(TempValue) Then ThisType = "SINGLE"
      End If

    'Element is a string var, ending with $
    ElseIf Right(ValueTemp, 1) = "$" Then
      ThisType = "STRING"
      If HasIndex Then ThisType = "BYTE"

    'Element is a bit var
    ElseIf InStr(ValueTemp, ".") <> 0 And Not IsConst(Left(ValueTemp, InStr(ValueTemp, ".") - 1)) Then
      ThisType = "BIT"

    'Element is a var (or function result)
    Else
      ThisType = TypeOfVar(ValueTemp, CurrentSub)
      'May be accessing character of string
      If HasIndex And ThisType = "STRING" Then
        'Check for function
        SubLoc = LocationOfSub(ValueTemp, "", "", -1)

        If SubLoc = 0 Then
          'No function, so accessing char of string - type is byte
          ThisType = "BYTE"
        Else
          ThisType = Subroutine(SubLoc)->ReturnType
        End If
      End If
    End If

    'Set FinalType to highest value
    'Print ,, ValueTemp + " is " + ThisType
    If CastOrder(ThisType) > CastOrder(FinalType) Then FinalType = ThisType

  Next

  Return FinalType

END FUNCTION

Sub UpdateOutgoingCalls (CompSub As SubType Pointer)
  'Update call counts to other subroutines
  'Use after allocating a subroutine to a program memory page
  Dim As SubType Pointer CalledSub
  Dim As LinkedListElement Pointer ListItem

  'Make sure that this subroutine is on a valid page
  If CompSub->DestPage < 0 Or CompSub->DestPage > MAX_PROG_PAGES Then
    Exit Sub
  End If

  'Find all subroutines called from this one
  ListItem = CompSub->CallList->Next
  Do While ListItem <> 0
    CalledSub = ListItem->Metadata
    'Update call counts
    CalledSub->CallsFromPage(CompSub->DestPage) += ListItem->NumVal
    ListItem = ListItem->Next
  Loop
End Sub

Sub UpdateSubMap
  'Ensure that the hash map of subroutines is up to date
  Dim As LinkedListElement Pointer SubElement
  Dim As Integer T

  'Regenerate if new subroutines have been added and old map is out of date
  If OldSBC <> SBC Then
    Subroutines = HashMapCreate
    'Copy every sub into map
    For T = 1 To SBC
      'Add sub name to hash map, with reference to location in array
      SubElement = HashMapGet(Subroutines, UCase(Trim(Subroutine(T)->Name)))
      If SubElement = 0 Then
        SubElement = LinkedListCreate
        HashMapSet(Subroutines, UCase(Trim(Subroutine(T)->Name)), SubElement)
        SubElement->NumVal = T
      Else
        'Name already found in hash map, add this one to list of overloads
        SubElement = LinkedListInsert(SubElement, UCase(Subroutine(T)->Name), T)
      End If

      'Add name in output code as well
      SubElement = HashMapGet(Subroutines, UCase(GetSubFullName(T)))
      If SubElement = 0 Then
        SubElement = LinkedListCreate
        HashMapSet(Subroutines, UCase(GetSubFullName(T)), SubElement)
        SubElement->NumVal = T
      End If
    Next
    OldSBC = SBC
  End If
End Sub

Sub UpgradeCalcVar (VarName As String, VarType As String)
  'Upgrade the type of a calc var
  Dim As String Temp
  Dim As Integer CV, CD

  If VarName <> "" Then
    'If name specified, only mark that var

    'Exit if input is not a calc var
    IF Left(UCase(VarName), 7) <> "SYSTEMP" Then Exit Sub

    Temp = Mid(Trim(VarName), 8)
    CV = VAL(Temp)

    With CalcVars(CV)
      If CastOrder(.CurrentType) < CastOrder(VarType) Then
        .CurrentType = VarType
        'Also need to change max type
        If CastOrder(VarType) > CastOrder(.MaxType) Then
          .MaxType = VarType
        End If
      End If
    End With
  End If
End Sub

FUNCTION VarAddress (ArrayNameIn As String, CurrSub As SubType Pointer) As VariableType Pointer
  Dim As String ArrayName
  Dim As Integer LO
  Dim As SubType Pointer MainSub
  Dim As VariableType Pointer FoundVar
  MainSub = Subroutine(0)

  'Tidy name
  ArrayName = ArrayNameIn
  If INSTR(ArrayNameIn, "(") <> 0 Then ArrayName = RTrim(Left(ArrayNameIn, INSTR(ArrayNameIn, "(") - 1))
  If INSTR(ArrayNameIn, "$") <> 0 Then ArrayName = RTrim(Left(ArrayNameIn, INSTR(ArrayNameIn, "$") - 1))

  'Search local variables
  FoundVar = HashMapGet(@(CurrSub->Variables), ArrayName)
  If FoundVar <> 0 Then
    Return FoundVar
  End If

  'Search global variables
  If MainSub <> CurrSub Then
    FoundVar = HashMapGet(@(MainSub->Variables), ArrayName)
    If FoundVar <> 0 Then
      Return FoundVar
    End If
  End If

  'Check if variable is a function result
  LO = LocationOfSub(ArrayNameIn, "", "")
'  Print ArrayNameIn, LO
  If LO > 0 Then
    With *Subroutine(LO)
      If Subroutine(LO) = 0 Then
'        Print "Var " + ArrayName + " not found in sub " + CurrSub->Name
        Return Cast( VariableType Pointer, INVALIDARRAYVALUE )  'DuplicateFunction name - permitting higher level methods to handle the error
      ElseIf .IsFunction Then
        'Have found function, add a var and then return it
        AddVar ArrayNameIn, .ReturnType, 1, 0, "REAL", "", , -1
        FoundVar = HashMapGet(@(MainSub->Variables), ArrayName)
        Return FoundVar
      End If
    End With
  Else
    Return Cast( VariableType Pointer, INVALIDARRAYVALUE ) 'DuplicateFunction name - permitting higher level methods to handle the error
  End If

  'Nothing found, return null pointer
  'Print "Var " + ArrayName + " not found in sub " + CurrSub->Name
  Return 0
END FUNCTION

Sub CreateReservedWordsList
  dim ReservedWordFile as string
  dim ReservedWordIn as string
  dim ReservedWordCounter as Integer
  dim As Byte ReservedwordLoop = 0

  'Detect GCBASIC install directory
  ID = ExePath
  If ID = "" Or ID = "." THEN
    ID = CURDIR
    #IFDEF __FB_UNIX__
      If Right(ID, 1) = "/" Then ID = Left(ID, Len(ID) - 1)
    #ELSE
      If Right(ID, 1) = "\" Then ID = Left(ID, Len(ID) - 1)
    #ENDIF
  End If

  'Read list
  #IFDEF __FB_UNIX__
    ReservedWordFile = ID + "/reservedwords.dat"
  #ELSE
    ReservedWordFile = ID + "\reservedwords.dat"
  #ENDIF
  IF Dir(ReservedWordFile) = "" THEN
    PRINT "Cannot find " + ReservedWordFile
    Print
    PRINT "GCBASIC cannot operate without this file"
    If PauseOnErr = 1 THEN
      PRINT
      PRINT "Press any key to continue"
      Sleep
    END IF
    END
  END IF

  ReservedWordCounter = 0
  OPEN ReservedWordFile FOR INPUT AS #1
  Do While NOT EOF(1)
      Line Input #1,  ReservedWordIn
      ReservedWords( ReservedWordCounter )  = ReservedWordIn
'      print ReservedWords( ReservedWordCounter )
      ReservedWordCounter = ReservedWordCounter +1
      For ReservedwordLoop = 1 to len ( ReservedWordIn )
          If ReservedWordCounter > 1 then
            ReservedwordC = ReservedwordC + ASC( Mid(ReservedWordIn,ReservedwordLoop ,1) )
          End if
      Next 
  Loop
  CLOSE  #1
  If Val( Mid(ReservedWords( 0 ), Instr( ReservedWords( 0 )," ") ) ) + ReservedwordC <> 255 Then
      LogError "Invalid Reservedwords.dat - reinstall GCBASIC toolchain"
  End If
  ReservedwordC = Val( Mid(ReservedWords( 0 ), Instr( ReservedWords( 0 )," ") ) )     
End Sub

Sub AddDataBlocks ( ByRef CurrLine As LinkedListElement Pointer, ByRef CurrPage as Integer, ByRef CurrPagePos As Integer, ByRef DataBlockCount as Integer, NonChipFamily16DataBlocksNotAdded As Integer)

  'Only do this once.
  If NonChipFamily16DataBlocksNotAdded = 0 then Exit Sub
  If DataBlockCount > 0 Then CurrLine = LinkedListInsert(CurrLine, "; DATA blocks. DATA blocks are contiguous and may, or may not, overlap page boundary(ies)." )
  
  Dim As Integer EPDataHeader, EPDataLoc, CurrEPItem, TableAddressState, AVRAddressState, LogWarningCounter = 0, CurrEPTable, OrgPosOffset
  Dim as String ASMInstruction, Prefix, EPTempData

  For CurrEPTable = 1 TO DataTables
    'Examine all tables

    EPTempData = ""

    With DataTable(CurrEPTable)
      
      If .Items > 0 AND .IsData = -1 and DataBlockCount > 0 Then              
        'Get data. Where .items the number of items and ISDATA table
        OrgPosOffset = 0

        StatsUsedProgram = StatsUsedProgram + 1

        If .Type = "BYTE" Then
          'ORG counter
          OrgPosOffset = OrgPosOffset + ( .Items * 2 )
          If Instr(UCase(AsmExe), "GCASM") > 0 Then
            ASMInstruction = "de"
            ' GBASIC Assember needs 0x34 as the high byte
            Prefix ="34"
          ElseIf Instr(UCase(AsmExe), "MPASM") > 0 Then
            ASMInstruction = "de"
            Prefix ="00"
          Else
            ASMInstruction = "db"
            Prefix = ""
          End IF

          'iterate the items
          For CurrEPItem = 1 To .Items
            ' process to create data strng as BYTE
            EPTempData = EPTempData + ", 0x" + Prefix + Right("0"+HEX(.Item(CurrEPItem)),2)                  
          Next
        Else
          'ORG counter
          OrgPosOffset = OrgPosOffset + ( .Items * 1 )            
          ASMInstruction = "dw"
          'iterate the items
          For CurrEPItem = 1 To .Items Step 1
            ' process to check the data strng
            If MODEPIC Then
              If ChipFamily <> 16 Then
                If .Item(CurrEPItem) > 16383 Then
                    LogWarning message("DataBlockExceeds")
                    LogWarningCounter = LogWarningCounter + 1
                    If LogWarningCounter = 11 then
                      LogError message("DataBlockExceedsTooMany")
                    End If 
                End If
              End If
            Else
              ' AVR
              'AVR Check needs to be added
            End If
            ' process to create data strng as BYTE
            EPTempData = EPTempData + ", 0x" + Right("0000"+HEX(.Item(CurrEPItem)),4)
          Next
        End If

        ' Tidy DATABlock string
        If Left(EPTempData, 2 ) = ", " Then EPTempData = Mid(EPTempData,3, Len( EPTempData ) ) 
        
        'This test ensure the address of the DATA is ALIGNed to 2 for ChipFamily = 16 
        If (  EPDataLoc/2 <> Int(EPDataLoc/2) and ChipFamily = 16 ) Then
            EPDataLoc = EPDataLoc + 1
        End If

        'Remove ASMSymbol as this is not required.
          'ToAsmSymbols += 1
          'ToAsmSymbol(ToAsmSymbols, 2) = Str(CurrPagePos)
          'ToAsmSymbol(ToAsmSymbols, 1) = Trim(.Name)

        AddConstant(Trim(.Name), Str(CurrPagePos))

        'Create DATA block label
        If ModePIC Then  
          CurrLine = LinkedListInsert(CurrLine, Trim(.Name))
          GetMetaData(Currline)->IsLabel = -1
        Else
          CurrLine = LinkedListInsert(CurrLine, Trim(.Name)+":")
          GetMetaData(Currline)->IsLabel = -1
        End If

        If trim(EPTempData) <> "" Then 
          'Do not push out empty structure, as this will cause an error in MPASM etc

          EPTempData = EPTempData +","  'add additional delimter to make this routine work as we use the "," in the instrev()

          Dim as Integer FirstDelimiter, PosOfComma, LastValidPostOfComma, WordCounter, PageSize
          Dim as String StringCut, StringOut

          StringCut = EPTempData
          PageSize = ProgMemPage(CurrPage-1).PageSize
          StringOut = ""
          FirstDelimiter = 1
          WordCounter = 0
          
          'print "Address page", currpage, ProgMemPages, CurrPagePos

          Do
            ' Only output width that MPASM can cope with
            PosOfComma = InStr( FirstDelimiter,  StringCut, "," )
            ' Less the Maxsize AND not at end of string AND ( is NOT MPASM and LESS the 129 chars )
            ' Print CurrPagePos + 1 , PosOfComma  
            If PosOfComma <> 0 AND PosOfComma < 129 Then
                'update outstring
                StringOut = Mid ( StringCut, 1, PosOfComma )
                  WordCounter = WordCounter + 1
                'print "s1 = ",stringout +"| "+Str(PosOfComma)+"| "+ hex(CurrPagePos)
                FirstDelimiter = PosOfComma + 1
                LastValidPostOfComma = PosOfComma
            Else
  
                'Current ORG
                If PageSize <> 0 Then
                  CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(CurrPagePos) )
                End if
                'Remove any comma at the end
                If right( Trim(StringOut),1) = "," Then StringOut = Left( StringOut,Len(StringOut)-1)
                'Insert into list
                CurrLine = LinkedListInsert(CurrLine, "  " + ASMInstruction + " " + StringOut )

                'Update the CurrPagePos            
                CurrPagePos = CurrPagePos + WordCounter
                StatsUsedProgram = StatsUsedProgram + WordCounter              
                'Tidy the string
                StringCut = trim(Mid ( StringCut, LastValidPostOfComma +1 ))

                ' print CurrPagePos, CurrPagePos MOD .MaxSize, wordcounter, PosOfComma, Len(StringCut)
                
                FirstDelimiter = 1
                WordCounter = 0
                ' print CurrPagePos, Int ( CurrPagePos / PageSize ), CurrPage - 1 , Int ( CurrPagePos / PageSize ) <> CurrPage  - 1
                If PageSize <> 0  and CurrPage > 0 Then
                  If Int ( CurrPagePos / PageSize ) <> CurrPage - 1  Then
                    If CurrPage <= ProgMemPages Then
                      'Increment the page - this is required to page control and ORG statements ( later in this method)
                      CurrPage = CurrPage + 1
                      CurrLine = LinkedListInsert(CurrLine,  "; Page Increment ")
                    End if
                  End If
                End If
            End If
          Loop While PosOfComma > 0 And Len(StringCut) <> 0
        End If        
        CurrLine = LinkedListInsert(CurrLine, "" )
        EPDataLoc += (.Items + 1)
      End IF
    End With

    'move down as we dont need to align on every table
    IF AFISupport = 1 and ModePIC and ChipFamily = 16 Then
      'There should only one table of this type, and, align may not be needed... but, I do not know if this is the only once....
      CurrLine = LinkedListInsert(CurrLine, " ALIGN 2 ;X1")
    End if
    
    ' Unset .IsData to ensure this is specific table is not handled again
    If DataTable(CurrEPTable).IsData = -1 then 
      DataTable(CurrEPTable).IsData = 0
      DataTable(CurrEPTable).Used = 0
    End If
    ' We have processed a DataBlock, so, reduce the counter
    DataBlockCount = DataBlockCount - 1
  Next

End Sub