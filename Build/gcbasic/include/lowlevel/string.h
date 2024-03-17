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
/*  04/03/2025:
              Revised type to S, type to HEX and type to BINary functions with common naming[]
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
                ULingInttoBIN()      0b01000110010000001110011010110111

    */
' 09/03/2024: Revised SingleToString
' 10/03/2024: Add StringToLong by renaming VAL32
' 10/03/2024: Add prototype StringToSingle
' 10/03/2024: Updated to SingleToStrijng to resolve 0.0 error
' 12/03/2024: Added StringToSingle
' 14/03/2024: Added ULongIntToBin. Help updated and GCODE. Updated SingletoString and StringToSingle 

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

'String > Integer
Function StringToInteger(SysInString as String) as Integer
'Converts a string to Long
'max in = "4294967295" (#4.294.967.295)
    StringToLong=0
    SysCharCount = SysInString(0)          'length of input string
    For SysStringTemp = 1 to SysCharCount
        SysStrData = SysInString(SysStringTemp)
        StringToLong = StringToLong * 10 + SysStrData - 48
    Next
end Function

'String > Long
#DEFINE VAL32 StringToLong
Function StringToLong(SysInString as String) as Long
'Converts a string to Long
'max in = "4294967295" (#4.294.967.295)
    SysInString = trim(SysInString)
    if SysInString(1)="-" Then 
      SysInString(1)=" "
      SysInString = trim(SysInString)
    End If
    

    StringToLong=0
    SysCharCount = SysInString(0)          'length of input string
    For SysStringTemp = 1 to SysCharCount 
        SysStrData = SysInString(SysStringTemp)
        if SysStrData = 32 Then Exit Function
        StringToLong = StringToLong * 10 + SysStrData - 48
    Next
end Function

// Prototpe by Evan R. Venn 10/02/2024
Function StringToSingleERV ( in inStringToSingle as String ) As Single
	
	Dim _LongSTS as Long
	// determine decimal value from string
    _LongSTS = StringToLong(Mid( inStringToSingle, Instr ( inStringToSingle, ".")+1, 3 ))

    'using GCBASIC method to assing the Long to Single
    StringToSingle = ( _LongSTS + (_LongSTS % 2) )
	
	StringToSingle = StringToSingle / 1000
	
    _LongSTS = StringToLong(Mid( inStringToSingle, 1, Instr ( inStringToSingle, ".")-1 ))

    // Add the two parts
    StringToSingle = StringToSingle + _LongSTS

	// Set to negative, if required
	If Instr(inStringToSingle, "-" ) Then StringToSingle.31 = 1

End Function

Function StringToSingle (in StrNum as String * 18) as Single
  // 14/03/2024 version 05
	'-----------------------------------------------------------
	'   SysByte_STS_Err.0 = 1 good - 0 - bad
	'   SysByte_STS_Err.1 = 1 decimals places to many chars,  0 = ok
	'   SysByte_STS_Err.2 = 1 integer places to many chars-out of range,   0 = ok
	'   SysByte_STS_Err.3 = 1 no decimal point, info only
	'   SysByte_STS_Err.4 = non numeric chars found	'   
	'-----------------------------------------------------------

	#define MaxDecimal 100000000

  	'-------------------------------
	' actual system variables
	'-------------------------------
	dim SysULongIntTempA as ulongint
	dim SysULongIntTempB as ulongint
	dim SysULongIntTempX as ulongint
	dim SysLongTempA as long
	dim SysLongTempB as long
	dim SysLongTempX as long	

	'-------------------------------
	' STS variables in string.h
	'-------------------------------
	dim SysByte_STS_Exp as Byte
	dim SysByte_STS_Bin as Byte
	dim SysByte_STS_Ptr as Byte
	dim SysByte_STS_Sgn as Byte
	dim SysByte_STS_Err as Byte

	'-------------------------------
	' STS variables unique here
	'-------------------------------	
	dim SysULongInt_STS_Num as uLongINT	
	dim SysLong_STS_Total as Long

	' Seperate int from fraction
	SysByte_STS_Ptr = 1
	SysByte_STS_Sgn = 0

	Do While (SysByte_STS_Ptr <= StrNum(0))	'as long as isn't a decimal and not over string length
		SysByte_STS_Bin = StrNum(SysByte_STS_Ptr)
		If Instr(".+-0123456789", Chr(SysByte_STS_Bin)) > 0 then
			Exit Do
		End If
		SysByte_STS_Ptr ++
	Loop

	SysByte_STS_Sgn = 0
	if StrNum(1) = "-" Then
		SysByte_STS_Sgn = 1
		SysByte_STS_Ptr ++
	else If StrNum(1) = "+" then
		SysByte_STS_Ptr ++
	end If

	SysULongInt_STS_Num = 0
	Do While (SysByte_STS_Ptr <= StrNum(0))	'as long as isn't a decimal and not over string length
		SysByte_STS_Bin = StrNum(SysByte_STS_Ptr)
		If Instr(".0123456789", Chr(SysByte_STS_Bin)) = 0 then
			'ERROR Should be a . or a number
			SysByte_STS_Err = 0
			SysByte_STS_Err.4 = 1	'non numeric chars found
			Exit Function
		Else
			'check for .
			If SysByte_STS_Bin = ASC(".") then
				Exit Do
			End if
		End If

		SysULongIntTempA = SysULongInt_STS_Num
		Set C Off
		Rotate SysULongIntTempA Left			'x 2
		SysULongIntTempB = 5					'x 5 = x 10 with less iterations
		SysMultSub64
		SysByte_STS_Bin -= 48
		SysULongInt_STS_Num = SysULongIntTempX + SysByte_STS_Bin

		'NEED TO CHECK FOR OVERFLOW
		If SysULongInt_STS_Num.63 = 1 then			
			SysByte_STS_Err = 0
			SysByte_STS_Err.2 = 1	'1 integer places to many chars-out of range,   0 = ok
			Exit Function
		end If

		SysByte_STS_Ptr ++
	Loop

	SysByte_STS_Ptr ++	'move off of the decimal
	SysLong_STS_Total = 1	'need to do this for .0xx number, otherwize .625 is the same as .00625

	Repeat 8
		If SysByte_STS_Ptr <= StrNum(0) then	'do while less than length of string
			SysLongTempA = SysLong_STS_Total
			Set C Off
			Rotate SysLongTempA Left	'x 2
			SysLongTempB = 5			'x 5 = x 10 with less iterations
			SysMultSub32	'multiply -> X = A * B

			SysLongTempB = StrNum(SysByte_STS_Ptr) - 48
			
			SysLong_STS_Total = SysLongTempX + SysLongTempB 'A = X + digit
			SysByte_STS_Ptr ++	'next digit
		else
			Exit Repeat
		end If
	End Repeat

	Repeat 8	'truancate to 8 decimals
		SysLongTempB = MaxDecimal		'100000000
		SysLongTempA = SysLong_STS_Total	' A < B	
		SysCompLessThan32	'<--- not implementedd in compiler

		If SysByteTempX = true Then		'A < B  multiply by 10	
			SysLongTempA = SysLong_STS_Total
			Set C Off
			Rotate SysLongTempA Left		'x 2
			SysLongTempB = 5				'x 5 = x 10 with less iterations		
			SysMultSub32
			SysLong_STS_Total = SysLongTempX
		else
			Exit Repeat						' No Exit, precision reached
		end if
	End Repeat

	SysLong_STS_Total = SysLong_STS_Total - MaxDecimal

	'---------------------------------------------
	'  SysULongInt_STS_Num = Integer
	'  SysLong_STS_Total = Fraction
	'---------------------------------------------

  '---- SysLong_STS_Total = fraction in 8 decimal places ---
  ' x 2^32 / 100000000 >> 23, ULongInt needs to be used
  '----------------------------------------------------------

	SysULongIntTempX = SysLong_STS_Total
	Repeat 32
    Set C Off
		Rotate SysULongIntTempX Left	'same as x 2
	End Repeat
  SysUlongIntTempA = SysULongIntTempX	'result of multiply
  SysULongIntTempB = MaxDecimal
  SysDivSub64  
  
  SysLong_STS_Total = SysULongIntTempA  ' store division result back into Total

	SysByte_STS_Exp = 127
	
	'if integer is 0, we handle this different
	SysULongIntTempA = SysULongInt_STS_Num
	SysULongIntTempB = 0
	SysCompEqual64

	If SysByteTempX = true Then	'Integer is 0, all we have is a fraction
		'move left until STATUS.C has a 1		
		Repeat 32
			If SysLong_STS_Total.31 = 1 then
				rotate SysLong_STS_Total left
				SysByte_STS_Exp --
				exit Repeat
      end If

			Set C Off
			rotate SysLong_STS_Total left
			SysByte_STS_Exp --			
		End Repeat
	else					'let's see if we have just an integer		
		'move right into SysLong_STS_Total till SysULongInt_STS_Num = 0
    
		Repeat 64	'if SysULongInt_STS_Num is never 0, integer is too big
			Set C Off
			Rotate SysLong_STS_Total right

			SysLong_STS_Total.31 = SysULongInt_STS_Num.0
			
			Set C Off
			Rotate SysULongInt_STS_Num right

			SysULongIntTempA = SysULongInt_STS_Num
			SysULongIntTempB = 1
			SysCompEqual64

			If SysByteTempX = True then
        SysByte_STS_Bin = SysByteTempX
				SysByte_STS_Exp ++
				Exit Repeat
			end If

			SysByte_STS_Exp ++
		End Repeat

		If SysByte_STS_Bin = False then
			'number too big      
			StringToSingle = 0
			SysByte_STS_Err = 0
			SysByte_STS_Err.0 = 0
			SysByte_STS_Err.2 = 1	'1 integer places to many chars-out of range,   0 = ok
			Exit Function
		End If
	end If

	Repeat 8	'move in exponent
		Set C Off
		rotate SysLong_STS_Total right
		SysLong_STS_Total.31 = SysByte_STS_Exp.0
		Set C Off
		rotate SysByte_STS_Exp right
	End Repeat
	
	Set C Off
	rotate SysLong_STS_Total right	'make room for sign

	SysLong_STS_Total.31 = SysByte_STS_Sgn.0

  '---- should round up 1 bit ---
  SysLong_STS_Total ++
  
	StringToSingle_E = [byte]SysLong_STS_Total_E 	
	StringToSingle_U = [byte]SysLong_STS_Total_U 
	StringToSingle_H = [byte]SysLong_STS_Total_H
	[byte]StringToSingle = [byte]SysLong_STS_Total
  
	SysByte_STS_Err = 0
	SysByte_STS_Err.0 = 1
End Function


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


Function ULongIntToBin (in __ULongInt as uLongINT) as String * 64
  'Client 14/03/2024 
  ULongIntToBin = ""
  Repeat 64
      If __ULongInt.63 = 1 Then
         ULongIntToBin = ULongIntToBin +"1"
      Else
         ULongIntToBin = ULongIntToBin +"0"
      End If
      Rotate __ULongInt Left
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

Function SingleToString(in SingleNum as Single) as String * 20
  ' 14/03/2024  Version 07
  '-----------------------------------------------------------
  '   SingleNum = Single variable or constant to HSerPrint
  '
  '   SysByte_STS_Err = 0 if no error
  '   SysByte_STS_Err.0 = 1 good - 0 - bad
  '   SysByte_STS_Err.1 = 1 decimals places to many chars,  0 = ok
  '   SysByte_STS_Err.2 = 1 integer places to many chars-out of range,   0 = ok
  '   SysByte_STS_Err.3 = 1 no decimal point, info only
  '   SysByte_STS_Err.4 = non numeric chars found
  '   
  '-----------------------------------------------------------
  dim SysULongIntTempA as ULongInt
  dim SysULongIntTempB as ULongInt
  dim SysULongIntTempX as ULongInt
  dim SysLongTempA as Long
  dim SysLongTempB as Long
  dim SysLongTempX as Long

  dim SysULongInt_STS_Num as ULongInt  
  dim SysLong_STS_Mantissa as Long  
  dim SysByte_STS_Sgn as Byte
  dim SysByte_STS_Exp as Byte
  dim SysByte_STS_Bin as Byte
  dim SysByte_STS_Ptr as Byte
  dim SysByte_STS_Err as Byte  

  'ExtractSingleParts
  SysLongTempB = [byte]SingleNum: SysLongTempB_H = [byte]SingleNum_H: SysLongTempB_U = [byte]SingleNum_U: SysLongTempB_E = [byte]SingleNum_E
  
  SysByte_STS_Sgn = SysLongTempB.31

  SysLong_STS_Mantissa = SysLongTempB & 0x7FFFFF  
  
  Rotate SysLongTempB Left
  
  SysByte_STS_Exp = [byte]SysLongTempB_E

  SysLong_STS_Mantissa.23 = 1      'add hidden Bit

  SysULongInt_STS_Num = 0
  
  Repeat 8   ' align 24 bit mantissa to left 32
    Set C Off
    rotate SysLong_STS_Mantissa Left
  End Repeat

  If SysByte_STS_Exp > 127 Then            'number is > 0
    SysByte_STS_Bin = SysByte_STS_Exp - 126 'needed because I'm including the 1.

    SysULongInt_STS_Num = 0

    Set C Off 
    Repeat SysByte_STS_Bin              'move integer into Num3  
      rotate SysLong_STS_Mantissa Left  'when finished will be remainder
      rotate SysULongInt_STS_Num Left
    End Repeat

  else if SysByte_STS_Exp = 0 Then      'if Exp is 0 then answer is 0
    SingleToString = "0.0"
    SysByte_STS_Err = 0
    SysByte_STS_Err.0 = 1     '1 good - 0 - bad
    exit Function

  else '<=127 - move decimal place to the left  answer is <= 0
    SysByte_STS_Bin = (126 - SysByte_STS_Exp)
    if (SysByte_STS_Bin > 23) Then ' resulting number will be less than 0.00000001
      SingleToString = "Error "
      SysByte_STS_Err = 0
      SysByte_STS_Err.0 = 0     '1 good - 0 - bad
      SysByte_STS_Err.1 = 1     '1 decimals places to many chars,  0 = ok      
      exit Function
    end If

    SysULongInt_STS_Num = 0
  End If

  'init string to receive number
  SysByte_STS_Ptr = 20
  SingleToString = "          0.00000000"
  
  If SysLong_STS_Mantissa > 0 Then
    ' m * 100,000,000 >> 32 bits results in 8 digit accuracy
    '<< 8  and x 390625 = x 100000000
    SysULongIntTempA = SysLong_STS_Mantissa
    'Repeat 8
    '  Rotate SysULongIntTempA Left
    'End Repeat
    SysULongIntTempB = 100000000  '390625
    SysMultSub64

    Repeat 32
      Set C Off
      Rotate SysULongIntTempX Right
    End Repeat

    SysLong_STS_Mantissa = SysULongIntTempX 'load m with fractional for safe keeping

    SysULongIntTempA = SysULongIntTempX
  
    'Starting with fraction

    SysULongIntTempA = SysLong_STS_Mantissa
    SysULongIntTempB = 0
    SysCompEqual64

    do while SysByteTempX = False
      SysULongIntTempB = 10	'divide SysULongIntTempA by 10 till SysULongIntTempA = 0
      SysDivSub64

      SysByte_STS_Bin = [byte]SysULongIntTempX 	'remainder hold 0-9
      SingleToString(SysByte_STS_Ptr) = (SysByte_STS_Bin + 48)	'add 48 gives tha ASC value of 0 - 9 

      SysByte_STS_Ptr --
      
      SysULongIntTempB = 0
      SysCompEqual64
    loop
  End If

  If SysByte_STS_Ptr < 12 Then
    SingleToString(SysByte_STS_Ptr - 1) = "0"
    SingleToString(SysByte_STS_Ptr) = "."
    SysByte_STS_Ptr --

  else
    If SysLong_STS_Mantissa = 0 then
      SingleToString = "                 0.0"
      SysByte_STS_Ptr = 18
    else
      SysByte_STS_Ptr = 11
    end if
  End If

  SysULongIntTempA = SysULongInt_STS_Num
  SysULongIntTempB = 0
  SysCompEqual64

  do while SysByteTempX = False  
    if SysByte_STS_Ptr < 1 Then
      'ERROR too big of number      
      SingleToString = "Error "
      SysByte_STS_Err = 0
      SysByte_STS_Err.0 = 0     '1 good - 0 - bad
      SysByte_STS_Err.2 = 1     '1 integer places to many chars-out of range,   0 = ok      
      Exit Function
    end If
    
    SysULongIntTempB = 10
    SysDivSub64
    SysByte_STS_Bin = [byte]SysULongIntTempX 	'remainder hold 0-9
    SingleToString(SysByte_STS_Ptr) = (SysByte_STS_Bin + 48)	'add 48 gives tha ASC value of 0 - 9    
    SysByte_STS_Ptr --
    
    SysULongIntTempB = 0
    SysCompEqual64
  loop

  '---- remove leading spaces -----------
  SysByte_STS_Err = 1 ' use as counter
  Repeat 20    
    If SingleToString(SysByte_STS_Err + 1) <> 32 then
      exit Repeat
    end If
    SysByte_STS_Err ++
  end Repeat
  
  '---- copy data to start of string ----
  SysByte_STS_Exp = 1
  SysByte_STS_Bin = 21 - SysByte_STS_Err
  Repeat SysByte_STS_Bin

    SingleToString(SysByte_STS_Exp) = SingleToString(SysByte_STS_Err)

    SysByte_STS_Exp ++
    SysByte_STS_Err ++
  End Repeat

  SingleToString(0) = SysByte_STS_Bin   'set new string size
  
  SysByte_STS_Ptr = SingleToString(0) 'remove trailing 0's except .0  

  Do While SysByte_STS_Ptr > 4  ' 0.0
    If SingleToString(SysByte_STS_Ptr - 1 ) ="." then
      exit Do
    end If

    If SingleToString(SysByte_STS_Ptr) <> "0" then
      exit do
    end If
    SysByte_STS_Ptr --
  loop
  
  SingleToString(0) = SysByte_STS_Ptr

  If SysByte_STS_Sgn = 1 then
    'negative
    SingleToString(1) = "-"    
  End If
  SysByte_STS_Err = 0
  SysByte_STS_Err.0 = 1     '1 good - 0 - bad
End Function