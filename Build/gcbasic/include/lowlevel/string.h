'    String routines for the GCBASIC compiler
'    Copyright (C) 2006-2024 Hugh Considine, Evan Venn, Immo Freudenberg & Domenic Cirone

'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.

'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.

'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

'********************************************************************************
'IMPORTANT:
'THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
'UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
'COMMANDS UNUSABLE!
'********************************************************************************

'Changes:
' 15/2/2011: First working versions of LCase, UCase, Str
' 18/2/2011: Added Val function
' 13/2/2013: Added Left, Right, Mid functions
' New functions  added Jan 2014 by 2014 Evan R Venn
' a built-in function, asc(), which takes a character and returns the numeric value for that character in the machine's character set. If the string passed to asc() has more than one character, only the first one is used.
' The inverse of this function is chr() (from the function of the same name in Pascal), which takes a number and returns the corresponding character. Both functions are written very nicely in GCB.
' 22/12/2014: Add PAD function
' 1/6/2016: Add Fill function
' 21/10/2016: Fixed Pad
' 19/12/2015: Addes Str32 AND Val32 for string handling of 32 bit numbers.  Immo Freudenberg <immo-freudenberg@t-online.de>
' 24/1/2016: Added LeftPad and correct PAD
' 20/9/2017: Revised StrInteger to correct labels clash with other methods
' 25/02/2021: Add VarToBin, VarWToBin, IntegerToBin, LongtoBin
' 01/03/2024: Add WordToHex, LongToHex and SingleToHex
' 01/03/2024: Add SingleToString
' 03/03/2024: Revise LTRIM to save 46 bytes of RAM
/*
  04/03/2025:
              Revised type to STRing, type to HEX and type to BINary functions with common naming[]
              Type to string functions

                ByteToString()       127
                Str()      Legacy    127
                WordToString()       32768
                Str()      Legacy    32768
                LongToString()       2147483647
                Str32()    Legacy    2147483647
                IntegerToString()    -16384
                StrInteger() Legacy  -16384
                SingleToString()     12345.67871093

              Type to hexidecimal string functions

                ByteToHex()          0x7F
                Hex()      Legacy    0x7F
                WordToHex()          0x8000
                LongToHex()          0x7FFFFFFF
                IntegerToHex()       0xC000
                SingleToHex()        0x4640E6B7

              Type to binary string functions

                ByteToBin()          0b01111111
                WordToBin()          0b1000000000000000
                LongToBin()          0b01111111111111111111111111111111
                IntegerToBin()       0b010000000000000
                SingleToBin()        0b01000110010000001110011010110111

  */

'Length/position
Function Len (LenTemp())
  Len = LENTemp(0)
End Function

Function Instr (SysInString(), SysFindString())
  Instr = 0

  'Check length of search and find strings
  'Exit if the find string cannot fit into the search string
  SysStrLen = SysInString(0)
  If SysStrLen = 0 Then Exit Function
  SysFindLen = SysFindString(0)
  If SysFindLen = 0 Then Exit Function
  If SysFindLen > SysStrLen Then Exit Function

  'Start searching
  SysSearchEnd = SysStrLen - SysFindLen
  For SysStringTemp = 0 to SysSearchEnd
    For SysStrData = 1 To SysFindLen
      'If a mismatch is found, search next position
      If SysInString(SysStringTemp + SysStrData) <> SysFindString(SysStrData) Then Goto SysInstrNextPos
    Next
    'If no mismatches were found, return current start position
    Instr = SysStringTemp + 1
    Exit Function
    SysInstrNextPos:
  Next

  'Have reached end of search string without finding anything, return 0
  Instr = 0
End Function

'String/number conversion
'Word > String
'(Max output will be 5 characters)
#DEFINE ByteToString Str
#DEFINE WordToString Str
Function Str(SysValTemp As Word) As String * 5

  SysCharCount = 0
  Dim SysCalcTempX As Word

  'Ten Thousands
  IF SysValTemp >= 10000 then
    SysStrData = SysValTemp / 10000
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    Str(SysCharCount) = SysStrData + 48
    Goto SysValThousands
  End If

  'Thousands
  IF SysValTemp >= 1000 then
    SysValThousands:
    SysStrData = SysValTemp / 1000
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    Str(SysCharCount) = SysStrData + 48
    Goto SysValHundreds
  End If

  'Hundreds
  IF SysValTemp >= 100 then
    SysValHundreds:
    SysStrData = SysValTemp / 100
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    Str(SysCharCount) = SysStrData + 48
    Goto SysValTens
  End If
  'Tens
  IF SysValTemp >= 10 Then
    SysValTens:
    SysStrData = SysValTemp / 10
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    Str(SysCharCount) = SysStrData + 48
  End If

  'Ones
  SysCharCount += 1
  Str(SysCharCount) = SysValTemp + 48
  SysValTemp = SysCalcTempX

  Str(0) = SysCharCount

End Function

#DEFINE LongToString Str32
function Str32(SysValTemp As long) As String * 10
'converts strings of 10 Digits to a long Var (32bit)
'max in = # 4.294.967.295 (h FFFF FFFF)

  SysCharCount = 0
  'SysCalcTempX stores remainder after division
  'has to be DIM'ed b/o issue in GCB
   Dim SysCalcTempX As long

  Str32 = ""

   'thousand millions
  IF SysValTemp >= 1000000000 then
    SysStrData = SysValTemp / 1000000000
    SysValTemp = SysCalcTempX
    SysCharCount ++      ' first char.
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValHundredMillions
  End If

   'hundred millions
  IF SysValTemp >= 100000000 then
   _SysValHundredMillions:
    SysStrData = SysValTemp / 100000000
    SysValTemp = SysCalcTempX
    SysCharCount ++      ' scnd char.
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValTenMillions
  End If

   'Ten millions
  IF SysValTemp >= 10000000 then
   _SysValTenMillions:
    SysStrData = SysValTemp / 10000000
    SysValTemp = SysCalcTempX
    SysCharCount ++      ' third char.
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValMillions
  End If

   'one millions
  IF SysValTemp >= 1000000 then
   _SysValMillions:
    SysStrData = SysValTemp / 1000000

    SysValTemp = SysCalcTempX
    SysCharCount ++
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValHundredThousands
  End If

   'hundred Thousands
  IF SysValTemp >= 100000 then
   _SysValHundredThousands:
    SysStrData = SysValTemp / 100000
    SysValTemp = SysCalcTempX
    SysCharCount ++
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValTenThousands
  End If

  'Ten Thousands
  IF SysValTemp >= 10000 then
   _SysValTenThousands:
    SysStrData = SysValTemp / 10000
    SysValTemp = SysCalcTempX
    SysCharCount ++
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValThousands
  End If

  'Thousands
  IF SysValTemp >= 1000 then
   _SysValThousands:
    SysStrData = SysValTemp / 1000
    SysValTemp = SysCalcTempX
    SysCharCount ++
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValHundreds
  End If

  'Hundreds
  IF SysValTemp >= 100 then
   _SysValHundreds:
    SysStrData = SysValTemp / 100
    SysValTemp = SysCalcTempX
    SysCharCount ++
    Str32(SysCharCount) = SysStrData + 48
    Goto _SysValTens
  End If

  'Tens
  IF SysValTemp >= 10 Then
   _SysValTens:
    SysStrData = SysValTemp / 10
    SysValTemp = SysCalcTempX
    SysCharCount ++
    Str32(SysCharCount) = SysStrData + 48
  End If

  'Ones
  SysCharCount ++
  Str32(SysCharCount) = SysValTemp + 48
  SysValTemp = SysCalcTempX

  Str32(0) = SysCharCount

end function

'String/number conversion
'Integer > String  -32767 to 32767
'(Max output will be 6 characters)
#DEFINE IntegerToString StrInteger
Function StrInteger(SysValTemp As Word) As String * 6

  SysCharCount = 0
  Dim SysCalcTempX As Word

  if SysValTemp.15 = 1 Then
    SysCharCount += 1
    StrInteger(SysCharCount) = 0x2D
    SysValTemp = ABS(SysValTemp)
  end if

  'Ten Thousands
  IF SysValTemp >= 10000 then
    SysStrData = SysValTemp / 10000
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    StrInteger(SysCharCount) = SysStrData + 48
    Goto __SysValThousands
  End If

  'Thousands
  IF SysValTemp >= 1000 then
    __SysValThousands:
    SysStrData = SysValTemp / 1000
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    StrInteger(SysCharCount) = SysStrData + 48
    Goto __SysValHundreds
  End If

  'Hundreds
  IF SysValTemp >= 100 then
    __SysValHundreds:
    SysStrData = SysValTemp / 100
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    StrInteger(SysCharCount) = SysStrData + 48
    Goto __SysValTens
  End If
  'Tens
  IF SysValTemp >= 10 Then
    __SysValTens:
    SysStrData = SysValTemp / 10
    SysValTemp = SysCalcTempX
    SysCharCount += 1
    StrInteger(SysCharCount) = SysStrData + 48
  End If

  'Ones
  SysCharCount += 1
  StrInteger(SysCharCount) = SysValTemp + 48
  SysValTemp = SysCalcTempX

  StrInteger(0) = SysCharCount


End Function


'String > Word
#define Val16 Val
Function Val(SysInString As String) As Word
  'Parse SysInString, convert to word
  'Stop parsing and exit on any non-number character

  'Clear output value
  Val = 0

  'Get input length
  SysCharCount = SysInString(0)
  If SysCharCount = 0 Then Exit Function

  'Parse
  For SysStringTemp = 1 to SysCharCount
    SysStrData = SysInString(SysStringTemp)
    'Exit if non-digit encountered
    If SysStrData < 48 Then Exit Sub
    If SysStrData > 57 Then Exit Sub
    'Add to output value
    Val = Val * 10 + SysStrData - 48
  Next

End Function

'String > Long
Function Val32(SysInString as String) as Long
'Converts a string to Long
'max in = "4294967295" (#4.294.967.295)
    Val32=0
    SysCharCount = SysInString(0)          'length of input string
    For SysStringTemp = 1 to SysCharCount
        SysStrData = SysInString(SysStringTemp)
        Val32 = Val32 * 10 + SysStrData - 48
    Next
end Function

'Decimal > Hex
#DEFINE ByteToHex Hex
Function Hex (In SysValTemp) As String * 2
  Hex(0) = 2

  'Low nibble
  SysStringTemp = SysValTemp And 0x0F
  If SysStringTemp > 9 Then SysStringTemp = SysStringTemp + 7
  Hex(2) = SysStringTemp + 48

  'Get high nibble
  For SysStringTemp = 1 to 4
    Rotate SysValTemp Right
  Next
  SysStringTemp = SysValTemp And 0x0F
  If SysStringTemp > 9 Then SysStringTemp = SysStringTemp + 7
  Hex(1) = SysStringTemp + 48

End Function

#DEFINE IntegerToHex WordToHex
Function WordToHex ( in SysValTemp as Word ) as String * 4
  WordToHex(0) = 4

  Repeat 4

    SysStringTemp = SysValTemp And 0x0F
    If SysStringTemp > 9 Then SysStringTemp = SysStringTemp + 7
    WordToHex(WordToHex(0)) = SysStringTemp + 48

    Repeat 4 
      Rotate SysValTemp Right
    End Repeat

    WordToHex(0)--

  End Repeat
  WordToHex(0) = 4

End Function

Function LongToHex ( in SysValTemp as Long ) as String * 8
  LongToHex(0) = 8

  Repeat 8

    SysStringTemp = SysValTemp And 0x0F
    If SysStringTemp > 9 Then SysStringTemp = SysStringTemp + 7
    LongToHex(LongToHex(0)) = SysStringTemp + 48

    Repeat 4 
      Rotate SysValTemp Right
    End Repeat

    LongToHex(0)--

  End Repeat
  LongToHex(0) = 8

End Function


Function SingleToHex ( in SnglToHex_Sngl as Single ) as String * 8

    SingleToHex = HEX([byte]SnglToHex_Sngl_E)
    SingleToHex += HEX([byte]SnglToHex_Sngl_U)
    SingleToHex += HEX([byte]SnglToHex_Sngl_H)
    SingleToHex += HEX([byte]SnglToHex_Sngl)

End Function

'Truncation
Function Left (SysInString(), SysCharCount) As String
  'Empty input?
  If SysInString(0) = 0 Then
    Left(0) = 0
    Exit Function
  End If

  'Input length too high?
  If SysInString(0) < SysCharCount Then
    SysCharCount = SysInString(0)
  End If

  'Copy leftmost characters
  For SysStringTemp = 1 To SysCharCount
    Left(SysStringTemp) = SysInString(SysStringTemp)
  Next
  Left(0) = SysCharCount
End Function

Function Right (SysInString(), SysCharCount) As String
  'Empty input?
  If SysInString(0) = 0 Then
    Right(0) = 0
    Exit Function
  End If

  'Input length too high?
  If SysInString(0) < SysCharCount Then
    SysCharCount = SysInString(0)
  End If

  'Copy rightmost characters
  SysCharStart = SysInString(0) - SysCharCount
  For SysStringTemp = 1 To SysCharCount
    Right(SysStringTemp) = SysInString(SysCharStart + SysStringTemp)
  Next
  Right(0) = SysCharCount
End Function

Function Mid (SysInString(), SysCharStart, Optional SysCharCount = 255) As String
  'Empty input?
  If SysInString(0) = 0 Then
    Mid(0) = 0
    Exit Function
  End If

  'Starting position too low?
  If SysCharStart < 1 Then SysCharStart = 1

  'Starting position too high?
  If SysCharStart > SysInString(0) Then
    Mid(0) = 0
    Exit Function
  End If

  'Input length too high?
  SysCharStart -= 1
  SysStringTemp = SysInString(0) - SysCharStart 'Max number of characters
  If SysCharCount > SysStringTemp Then
    SysCharCount = SysStringTemp
  End If

  'Copy characters
  For SysStringTemp = 1 To SysCharCount
    Mid(SysStringTemp) = SysInString(SysCharStart + SysStringTemp)
  Next
  Mid(0) = SysCharCount
End Function

'Trimming

Function RTrim (SysInString3 as string) As String
  'Get length of string, return empty string.  If you dont do this you will return an uninitialize value... ooops
  RTrim = ""
  SysCharRTCount = SysInString3(0)
  If SysCharRTCount = 0 Then Exit Function

  'find first non white space in the string from the Right
  SysCharRT = 0
  For SysTempRT = SysCharRTCount to 1 step -1

      ' was Asc(Mid (SysInString3, SysTempRT , 1))
      if SysInString3(SysTempRT) <> 32 then
         SysCharRT = SysTempRT
         goto FoundNonSpaceChar2
      end if
  Next

FoundNonSpaceChar2:
  if SysCharRT <> 0 then
     Rtrim = Mid (SysInString3, 1, SysCharRT )
     Exit Function
  end if


End Function


function Trim (SysInString()) As String
  trim = rtrim(ltrim(SysInString))
end function

Function LTrim ( in TrimCharLeftString as String , Optional in TrimCharLeftChar as Byte = 32 ) as String
  Dim TTrimCharLeftTemp as Byte

  Do While TrimCharLeftString(1) = TrimCharLeftChar
    For TTrimCharLeftTemp = 1 to TrimCharLeftString(0)-1
        TrimCharLeftString(TTrimCharLeftTemp) = TrimCharLeftString(TTrimCharLeftTemp+1) 
    Next
    TrimCharLeftString(0)--
  Loop
  Return TrimCharLeftString
End Function


function OLDLTrim (SysInString2 as string) As String

  'Get length of string, return empty string.  If you dont do this you will return an uninitialize value... ooops
  Ltrim = ""
  SysCharLTCount = SysInString2(0)
  If SysCharLTCount = 0 Then Exit Function


  'find first non white space in the string
  SysCharLT = 0
  For SysTempLT = 1 to SysCharLTCount

      if Asc(Mid (SysInString2, SysTempLT , 1)) <> 32 then
         SysCharLT = SysTempLT
         goto FoundNonSpaceChar1
      end if
  Next

  FoundNonSpaceChar1:
  if SysCharLT <> 0 then
     Ltrim = Mid (SysInString2, SysCharLT )
     Exit Function
  end if

end function





'Case conversion
Function UCase (SysInString()) As String

  'Get length of string, don't try copying if it's empty
  SysCharCount = SysInString(0)
  UCase(0) = SysCharCount
  If SysCharCount = 0 Then Exit Function

  'Search string for lowercase letters, change to uppercase
  For SysStringTemp = 1 to SysCharCount
    SysStrData = SysInString(SysStringTemp)
    If SysStrData >= 97 Then
      If SysStrData <= 122 Then SysStrData -= 32
    End If
    UCase(SysStringTemp) = SysStrData
  Next
End Function

Function LCase (SysInString()) As String

  'Get length of string, don't try copying if it's empty
  SysCharCount = SysInString(0)
  LCase(0) = SysCharCount
  If SysCharCount = 0 Then Exit Function

  'Search string for uppercase letters, drop to lowercase
  For SysStringTemp = 1 to SysCharCount
    SysStrData = SysInString(SysStringTemp)
    If SysStrData >= 65 Then
      If SysStrData <= 90 Then SysStrData += 32
    End If
    LCase(SysStringTemp) = SysStrData
  Next
End Function



Function Asc (in SysInStringA(), optional Syschar = 1) As Byte
  Asc = 0
  'Get length of string, don't try _asc ing if it's empty
  SysCharCount = SysInStringA(0)
  If SysCharCount = 0 Then Exit Function

  if Syschar > SysCharCount Then Exit Function

  'Get first char of string, only first character is of interest, Syschar if provided
  Asc = SysInStringA( Syschar )

End Function

Function Chr (SysChar ) As String
  'Empty input?
  If SysChar < 0 Then
    Chr(0) = 0
    Exit Function
  End If

  Chr(0) = 1
  'Copy characters
  Chr(1) = SysChar

End Function

' Return a binary strings

Function ByteToBin (In ByteNum as Byte ) as String * 8
  ByteToBin = ""
  Repeat 8
      If ByteNum.7 = 1 Then
         ByteToBin = ByteToBin +"1"
      Else
         ByteToBin = ByteToBin +"0"
      End If
      Rotate ByteNum Left
  End Repeat
End Function

Function WordToBin (In WordNum as Word ) as String * 16
  WordToBin = ""
  Repeat 16
      If WordNum.15 = 1 Then
         WordToBin = WordToBin +"1"
      Else
         WordToBin = WordToBin +"0"
      End If
      Rotate WordNum Left
  End Repeat
End Function



Function VarToBin (In __ByteNum as Byte ) as String * 8
  VarToBin = ""
  Repeat 8
      If __ByteNum.7 = 1 Then
         VarToBin = VarToBin +"1"
      Else
         VarToBin = VarToBin +"0"
      End If
      Rotate __ByteNum Left
  End Repeat
End Function

Function VarWToBin (In __WordNum as Word ) as String * 16
  VarWToBin = ""
  Repeat 16
      If __WordNum.15 = 1 Then
         VarWToBin = VarWToBin +"1"
      Else
         VarWToBin = VarWToBin +"0"
      End If
      Rotate __WordNum Left
  End Repeat
End Function

Function IntegerToBin (In __IntegerNum as Integer, optional in _IntegerSign as Bit = 0 ) as String * 16
  IntegerToBin = ""
  If _IntegerSign = 1 Then
    If __IntegerNum.15 = 1 Then
      IntegerToBin = "-"
    Else
       IntegerToBin = "+"
    End If
  End If
  Repeat 15
    If __IntegerNum.15 = 1 Then
      If __IntegerNum.14 = 1 Then
         IntegerToBin = IntegerToBin +"0"
      Else
        IntegerToBin = IntegerToBin +"1"
      End If
    Else
      If __IntegerNum.14 = 1 Then
         IntegerToBin = IntegerToBin +"1"
      Else
        IntegerToBin = IntegerToBin +"0"
      End If
    End if
      Rotate __IntegerNum Left
  End Repeat
End Function

Function LongToBin (In __LongNum as Long ) as String * 32
  LongToBin = ""
  Repeat 32
      If __LongNum.31 = 1 Then
         LongToBin = LongToBin +"1"
      Else
         LongToBin = LongToBin +"0"
      End If
      Rotate __LongNum Left
  End Repeat
End Function

Function SingleToBin (In __SingleNum as Single ) as String * 32
  SingleToBin = ""
  SingleToBinPTR = 31
  Repeat 32
      If __SingleNum.SingleToBinPTR = 1 Then
         SingleToBin = SingleToBin +"1"
      Else
         SingleToBin = SingleToBin +"0"
      End If
      SingleToBinPTR--
  End Repeat
End Function

'PAD(str,len,padchr )
'Description  - The PAD() function pads a specified string
'Parameters
'str - String to be padded.
'len - Length of str.
'padchr - Pad character, which can be any string. The first character is used to pad new space in the output string.
Function Pad ( in SysInString as string, SysStrLen, optional in SysInString3 as string = " " ) As String
  'Check length of SysInString
  'If SysInString(0) = longer or equal SysStrLen then
  'give back SysInString and exit function

  If SysInString(0) < SysStrLen Then
    SysCharCount = SysInString(0)
    'clear output string
    Pad=""
      'Copy leftmost characters
      For SysStringTemp = 1 To SysCharCount
        Pad(SysStringTemp) = SysInString(SysStringTemp)
      Next

      For SysStringTemp = SysCharCount+1 to SysStrLen
        Pad(SysStringTemp) = SysInString3(1)
      Next
      'set new length to PAD
      Pad(0) = SysStrLen
  else
      'SysInString is equal or longer than SysStrLen
      'give back old string; copy SysInString to Pad

      For SysStringTemp = 1 To SysInString(0)
        Pad(SysStringTemp) = SysInString(SysStringTemp)
      Next
      PAD(0) = SysInString(0)
  End If

End Function?



'Fill(len,padchr )
'Description  - The Fill() function returns a string
'Parameters
'len - Length of str.
'Fill - Pad characters, the second is used to pad new space in the output string.
Function Fill ( SysStrLen, optional SysInString3 as string = " " ) As String

      'Copy leftmost characters
      For SysStringTemp = 1 To SysStrLen
        Fill(SysStringTemp) = SysInString3(1)
      Next

  Fill(0) = SysStrLen

End Function

'LeftPad(str,len,padchr )  @ Immo Freudenberg
'Description
'The LeftPad() function pads a specified string left
'with a given string to its new length
'exits with given value, when new length <= old length
'Parameters
'str - string to be padded
'len - new length of string
'padchr - Pad character, which can be any string
'The first character is added left of incoming string
Function LeftPad ( in SysInString as string, SysStrLen, optional in SysInString2 as string = " " ) As String

  'check input length
  'return if too short or equal SysStrLen
  If SysStrLen > SysInString(0) Then

     LeftPad = ""
     LeftPad(0) = SysStrLen       'set string to new length
     SysCharCount = SysStrLen - SysInString(0) 'diff-length To incoming string


    'add sysInString2 to new String
     for SysStringTemp = 1 to SysCharCount
        LeftPad(sysStringTemp) = SysInString2(1)
     Next

     'add old content to new string
     For SysStringTemp = 1 To sysInString(0)
        LeftPad(sysCharCount + sysStringTemp) = SysInString(SysStringTemp)
     Next

  Else

     LeftPad = SysInString

  end if

End Function

// *********************** Advanced variables

Function SingleToString(in SingleNum as Single) as String * 16
  ' Source from Clint Koehn 01/03/2024
  '-----------------------------------------------------------
  '   SingleNum = Single variable or constant to HSerPrint
  '-----------------------------------------------------------
  dim SysULongIntTempA, SysULongIntTempB, SysULongIntTempX as ULongInt
  dim SysLongTempA, SysLongTempB, SysLongTempX as Long
  dim SysByte_STS_Sgn, SysByte_STS_Exp, SysByte_STS_Bin as Byte
  dim SysByte_STS_Ptr as Byte  

  'ExtractSingleParts
  SysLongTempB = [byte]SingleNum: SysLongTempB_H = [byte]SingleNum_H: SysLongTempB_U = [byte]SingleNum_U: SysLongTempB_E = [byte]SingleNum_E

  SysByte_STS_Sgn = SysLongTempB.31
  
  SysLongTempX = SysLongTempB & 0x7FFFFF  
  Repeat 23
    Set C Off
    Rotate SysLongTempB Right Simple
  End Repeat

  SysByte_STS_Exp = SysLongTempB & 0xFF  
  Repeat 8
    Set C Off
    Rotate SysLongTempB Right Simple
  End Repeat

  SysLongTempX.23 = 1      'add hidden Bit
  
  SysULongIntTempA = SysLongTempX  
  SysULongIntTempB = 100000000
  SysMultSub64

  SysULongIntTempA = SysULongIntTempX

  if SysByte_STS_Exp < 127 then
    'fractional - divide - shift right
    SysByte_STS_Bin = (127 - SysByte_STS_Exp) + 23
    
    SysLongTempB = 1
    Repeat SysByte_STS_Bin
      SysLongTempB = SysLongTempB * 2
    End Repeat

    SysULongIntTempB = SysLongTempB
    SysDivSub64    
  else
    'whole number - multiply - shift left
    SysByte_STS_Bin = SysByte_STS_Exp - 127    
    if SysByte_STS_Bin < 23 then
      SysByte_STS_Bin = 23 - SysByte_STS_Bin      
    else
      SysByte_STS_Bin = SysByte_STS_Bin - 23      
    end if
    
	'divide the mantissa by 2^(127 - exp) apparently.  Your suppose to multiply the mantissa by 2^(127 - exp)    
    SysLongTempB = 1
    Repeat SysByte_STS_Bin
      SysLongTempB = SysLongTempB * 2
    End Repeat

    SysULongIntTempB = SysLongTempB
    SysDivSub64		'SysULongIntTempA holds the quotient, SysULongIntTempX hold the remainder which is dicarded
  end if

  'init string to receive number
  SysByte_STS_Ptr = 16
  SingleToString = "      0.00000000"
  
  'have to do this because uLongInt has no comparison routines
  SysLongTempA = [byte]SysULongIntTempA_D + [byte]SysULongIntTempA_C +[byte]SysULongIntTempA_B +[byte]SysULongIntTempA_A +[byte]SysULongIntTempA_E +[byte]SysULongIntTempA_U +[byte]SysULongIntTempA_H +[byte]SysULongIntTempA 

  SysULongIntTempB = 10	'divide SysULongIntTempA by 10 till SysULongIntTempA = 0

  do while SysLongTempA > 0
    SysDivSub64
    SysByte_STS_Bin = [byte]SysULongIntTempX 	'remainder hold 0-9
    SingleToString(SysByte_STS_Ptr) = (SysByte_STS_Bin + 48)	'add 48 gives tha ASC value of 0 - 9    
    SysByte_STS_Ptr --

    if SysByte_STS_Ptr = 8 then	'multiplying by 100,000,000 gave it 8 decimal places      
      SingleToString(SysByte_STS_Ptr) = "."
      SysByte_STS_Ptr --
    end if
    
    'have to do this because uLongInt has no comparison routines
    SysLongTempA = [byte]SysULongIntTempA_D + [byte]SysULongIntTempA_C +[byte]SysULongIntTempA_B +[byte]SysULongIntTempA_A +[byte]SysULongIntTempA_E +[byte]SysULongIntTempA_U +[byte]SysULongIntTempA_H +[byte]SysULongIntTempA 
  loop

  If SysByte_STS_Ptr > 7 then
    SysByte_STS_Ptr = 7
  End If

  SingleToString = Right(SingleToString, (17 - SysByte_STS_Ptr))	'remove leading spaces

  If SysByte_STS_Sgn = 1 then
    'negative
    SingleToString(1) = "-"
  End If

End Function