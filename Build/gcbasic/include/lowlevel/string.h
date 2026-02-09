'    String routines for the GCBASIC compiler
'    Copyright (C) 2006-2026 Hugh Considine, Evan Venn, Immo Freudenberg & Domenic Cirone

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

    */
' 09/03/2024: Revised SingleToString
' 10/03/2024: Add StringToLong by renaming VAL32
' 10/03/2024: Add prototype StringToSingle
' 10/03/2024: Updated to SingleToString to resolve 0.0 error
' 25/03/2024: Updated StringToSingle to remove CHR() and INSTR()
' 26/03/2024: Updated StringToSingle to optimise multiplications
' 27/03/2024: Updated StringToSingle for 12F/16F support
' 29/03/2024: Updated SingleToString for 12F/16F support
' 25/05/2024: Updated to include StringToByte, StringToWord - both support commas in string
' 10/08/2024: Resolve error in StringToInteger() where an ENDIF was missing


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
    If SysStrData <> "," Then
      If SysStrData < 48 Then Exit Function
      If SysStrData > 57 Then Exit Function
      'Add to output value
      Val = Val * 10 + SysStrData - 48
    End If
  Next

End Function

#define StringToByte StringToWord
Function StringToWord(SysInString As String) As Word
  'Parse SysInString, convert to word
  'Stop parsing and exit on any non-number character

  'Clear output value
  StringToWord = 0

  'Get input length
  SysCharCount = SysInString(0)
  If SysCharCount = 0 Then Exit Function

  'Parse
  For SysStringTemp = 1 to SysCharCount
    SysStrData = SysInString(SysStringTemp)
    'Exit if non-digit encountered
    If SysStrData <> "," Then
      If SysStrData < 48 Then Exit Function
      If SysStrData > 57 Then Exit Function
      'Add to output value
      StringToWord = StringToWord * 10 + SysStrData - 48
    End If
  Next

End Function

'String > Long
#DEFINE VAL32 StringToLong
Function StringToLong(SysInString as String * 14 ) as Long
'Converts a string to Long
'max in = "4294967295" (#4.294.967.295)
    StringToLong=0
    SysCharCount = SysInString(0)          'length of input string
    For SysStringTemp = 1 to SysCharCount
        SysStrData = SysInString(SysStringTemp)
        If SysStrData <> "," Then
          StringToLong = StringToLong * 10 + SysStrData - 48
        End If
    Next
end Function

'String > Integer
Function StringToInteger(SysInString as String) as Integer
'Converts a string to Integer

    Dim _signValue as Bit
    _signValue = 0
    SysInString = trim(SysInString)
    if SysInString(1)="-" Then 
      SysInString(1)=" "
      _signValue = 1
      SysInString = trim(SysInString)
    End If
    

    StringToInteger=0
    SysCharCount = SysInString(0)          'length of input string
    For SysStringTemp = 1 to SysCharCount 
        SysStrData = SysInString(SysStringTemp)
        If SysStrData <> "," Then
          If SysStrData = 32 Then Exit For
          StringToInteger = StringToInteger * 10 + SysStrData - 48
        End If
    Next
    If _signValue = 1 then StringToInteger = StringToInteger * -1
end Function

Function StringToSingle (in StrNum as String) as Single
// remove INSTR and CHR 25/03/2024 erv
// improved mutliplication Clint 27/03/2024
// improved support for 12F/16F chip and added debug levels    // #DEFINE DebugStr2Sngle 1..8 
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
  dim SysByteCache as Byte Alias SysByte_STS_Err  // Reuse this variable

    #IF DebugStr2Sngle = 1 
      // Show the incoming string
      HserSend 0x44 :HserSend 0x31: HserSend 32
      Dim debugloop as Byte
      For debugloop = 1 to StrNum(0)
        HserSend StrNum(debugloop)
      Next 
      HserPrintCRLF
    #Endif

	' Seperate int from fraction
	SysByte_STS_Ptr = 1
	SysByte_STS_Sgn = 0

	Do While (SysByte_STS_Ptr <= StrNum(0))	'as long as isn't a decimal and not over string length
		SysByte_STS_Bin = StrNum(SysByte_STS_Ptr)

    #IF DebugStr2Sngle = 2 
      // Show the character being processed - for a valid string... should see it all
      Dim debugloop as Byte
      For debugloop = 1 to StrNum(0)
        HserSend StrNum(debugloop)
      Next 
      HserPrintCRLF
    #Endif
    
    Select Case SysByte_STS_Bin
      Case 0x2D to 0X2E  
        // -.
        Exit Do
      Case 0x30 to 0x39
        // numbers
        Exit Do
      Case 0x2B    
        // +
        Exit Do
    End Select

		SysByte_STS_Ptr ++
	Loop
  // So, at this point the DO-LOOP has been exited when it find any valid of these .-+0123456789
  // but, there could be a leading character that is not valid

  // Check for a sign - or +; this is assumed to be the first character.
	SysByte_STS_Sgn = 0
	if StrNum(1) = 0x2D Then            // "-"  removal of string
		SysByte_STS_Sgn = 1
		SysByte_STS_Ptr ++
	else If StrNum(1) = 0x2B then       // "+"  removal of string
		SysByte_STS_Ptr ++
	end If

	SysULongInt_STS_Num = 0         'Integer part

	Do While (SysByte_STS_Ptr <= StrNum(0))	'as long as isn't a decimal and not over string length
		SysByte_STS_Bin = StrNum(SysByte_STS_Ptr)
    Select Case SysByte_STS_Bin
      Case 0x2E ' "."
        Exit Do
      Case 0x30 to 0x39
        NOP ' as this is good
      Case Else
        'ERROR Should be a . or a number
        SysByte_STS_Err = 0
        SysByte_STS_Err.4 = 1	'non numeric chars found
        Exit Function
    End Select

		SysULongIntTempA = SysULongInt_STS_Num  'Num = Num * 10 + (Char - 48)
    #IF DebugStr2Sngle = 3
      // Show the character being processed - for a valid string... should see it all
      HserSend 0x44 :HserSend 0x33: HserSend 32
      Dim debugloop as Byte
      For debugloop = 1 to StrNum(0)
        HserSend StrNum(debugloop)
      Next 
      HserPrintCRLF
    #Endif

    SysULongIntTempX = 0
    Repeat 10
        SysULongIntTempX = SysULongIntTempA + SysULongIntTempX
    End Repeat
    SysByte_STS_Bin -= 48
		SysULongInt_STS_Num = SysULongIntTempX + SysByte_STS_Bin

		'NEED TO CHECK FOR OVERFLOW
		If SysULongInt_STS_Num.63 = 1 then			
			SysByte_STS_Err = 0
			SysByte_STS_Err.2 = 1	'1 integer places to many chars-out of range,   0 = ok
          #IFDEF DebugStr2Sngle
            HserPrint "Overflow!"
          #ENDIF
			Exit Function
		end If

    #IF DebugStr2Sngle = 4
      // Show the character being processed until the Do- Loop exits when it finds a Decimal point
        HserSend SysByte_STS_Bin
    #Endif

		SysByte_STS_Ptr ++
	Loop

	If SysByte_STS_Bin = 0x2E then  // This should only be incremented if . was first character
    SysByte_STS_Ptr ++	      // So, move off of the decimal
  End If

  // DEBUG
      #IF DebugStr2Sngle = 5
        // Show the decimal component ONLY to be processed
        HserSend 0x44 :HserSend 0x35: HserSend 32
        Dim debugloop as Byte
        For debugloop = SysByte_STS_Ptr to StrNum(0)
          HserSend StrNum(debugloop)
        Next 
        HserPrintCRLF
      #Endif

      #IF DebugStr2Sngle = 6
        // Show the decimal char being processed - not the value!
        HserSend 0x44 :HserSend 0x36: HserSend 32
      #Endif

      #IF DebugStr2Sngle = 7
        // Show the decimal value of the  being processed
        HserSend 0x44 :HserSend 0x37: HserSend 32
      #Endif

  SysLong_STS_Total = 1  // Need to do this for .0xx number, otherwize .625 is the same as .00625
                         // This base line number is eradicated during a factorisation latter in the function

	Repeat 8
		If SysByte_STS_Ptr <= StrNum(0) then	'do while less than length of string
			SysLongTempA = SysLong_STS_Total      
      SysLongTempX = 0

      Repeat 10
        SysLongTempX = SysLongTempX + SysLongTempA 
      End Repeat
      SysLong_STS_Total = SysLongTempX

      SysByteCache = StrNum(SysByte_STS_Ptr) - 48
			
          #IF DebugStr2Sngle = 6
            // Show the decimal char being processed
            HserSend StrNum(SysByte_STS_Ptr)
          #Endif

          #IF DebugStr2Sngle = 7
            // Show the decimal value
            HserSend SysByte_STS_Ptr+48: HserSend 0x3C: HserSend  [Byte]SysByteCache + 48:HserSend 0x3E
          #Endif

			[Long]SysLong_STS_Total = [Long]SysLong_STS_Total + SysByteCache 'A = X + digit

			SysByte_STS_Ptr ++	'next digit
		else
      // Should never get here
			Exit Repeat
		end If
	End Repeat

  //DEBUG
    #IF DebugStr2Sngle = 6 or DebugStr2Sngle = 7 
      HserSend 13:      HserSend 10
    #Endif

	// At this point 
    //  SysULongInt_STS_Num  = the integer value
    //  SysLong_STS_Total    = the decimal value
  
  Repeat 8	'truncate to 8 decimals
		If SysLong_STS_Total < MaxDecimal Then		'A < B  multiply by 10	
      SysLong_STS_Total = SysLong_STS_Total * 10
		else
			Exit Repeat						' No Exit, precision reached
		end if
	End Repeat

	SysLong_STS_Total = SysLong_STS_Total - MaxDecimal

  // At this point
    // SysLong_STS_Total is a truncated to 8 Decimal points

  //DEBUG
          #IF DebugStr2Sngle = 8
            // Show the decimal component ONLY to be processed
            HserSend 0x44 :HserSend 0x38: HserSend 32
            HSerPrintStringCRLF LongToString( SysLong_STS_Total )
          #ENDIF

	// At this point 
  //  SysULongInt_STS_Num  = the integer value
  //  SysLong_STS_Total    = the decimal value

  // Check that integer <> 0, AND then, if SysByteTempX = True check decimal <> 0   
  
      #IFDEF ChipFamily 12,14,15,16
        //~ Prevent stack issues
        SysByteTempX = False
        If [BYTE]SysULongInt_STS_Num = 0 Then    // Check all is ZERO
        If SysULongInt_STS_Num_H = 0 Then
        If SysULongInt_STS_Num_U = 0 Then
        If SysULongInt_STS_Num_E = 0 Then
        If SysULongInt_STS_Num_A = 0 Then
        If SysULongInt_STS_Num_B = 0 Then
        If SysULongInt_STS_Num_C = 0 Then
        If SysULongInt_STS_Num_D = 0 Then
          SysByteTempX = True
        End if
        End if
        End if
        End if
        End if
        End if
        End if
        End if
      #ELSE
        SysULongIntTempA = SysULongInt_STS_Num
        SysULongIntTempB = 0      
        SysCompEqual64
      #ENDIF
  

  If SysByteTempX = True Then    
    If SysLong_STS_Total = 0 Then
      StringToSingle = 0
      SysByte_STS_Err = 0
      SysByte_STS_Err.0 = 1     '1 good - 0 - bad
      #IFDEF DebugStr2Sngle
        HSerPrint "Zero"
      #ENDIF
      Exit Function
    End If
  End If

	'---------------------------------------------
	'  SysULongInt_STS_Num = Integer
	'  SysLong_STS_Total = Fraction
	'---------------------------------------------

	SysULongIntTempX = SysLong_STS_Total
	Repeat 32
    Set C Off
		Rotate SysULongIntTempX Left	'same as x 2
	End Repeat
  SysUlongIntTempA = SysULongIntTempX	'result of multiply
  SysULongIntTempB = MaxDecimal
  Call SysDivSub64  
  
  SysLong_STS_Total = SysULongIntTempA  'store division result back into Total    '
  
	SysByte_STS_Exp = 127
	
	'if integer is 0, we handle this different

    #IFDEF ChipFamily 12,14,15
      //~ Prevent stack issues
      SysByteTempX = False
      If [BYTE]SysULongInt_STS_Num = 0 Then    // ERV check all are ZERO
      If SysULongInt_STS_Num_H = 0 Then
      If SysULongInt_STS_Num_U = 0 Then
      If SysULongInt_STS_Num_E = 0 Then
      If SysULongInt_STS_Num_A = 0 Then
      If SysULongInt_STS_Num_B = 0 Then
      If SysULongInt_STS_Num_C = 0 Then
      If SysULongInt_STS_Num_D = 0 Then
        SysByteTempX = True
      End if
      End if
      End if
      End if
      End if
      End if
      End if
      End if
    #ELSE
    	SysULongIntTempA = SysULongInt_STS_Num
	    SysULongIntTempB = 0
      SysCompEqual64
    #ENDIF

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

      #IFDEF ChipFamily 12,14,15
		
        //~ Prevent stack issues
        SysByteTempX = False
        If [BYTE]SysULongInt_STS_Num = 1 Then  // ERV is the correct - just check this byte ?
        If SysULongInt_STS_Num_H = 0 Then
        If SysULongInt_STS_Num_U = 0 Then
        If SysULongInt_STS_Num_E = 0 Then
        If SysULongInt_STS_Num_A = 0 Then
        If SysULongInt_STS_Num_B = 0 Then
        If SysULongInt_STS_Num_C = 0 Then
        If SysULongInt_STS_Num_D = 0 Then
          SysByteTempX = True
        End if
        End if
        End if
        End if
        End if
        End if
        End if
        End if
      #ELSE
			  SysULongIntTempA = SysULongInt_STS_Num     // ERV move to #ELSE once Clint as approved
			  SysULongIntTempB = 1
        SysCompEqual64
      #ENDIF

			If SysByteTempX = True then
        SysByte_STS_Bin = SysByteTempX
				'SysByte_STS_Exp ++
				Exit Repeat
			end If

			Set C Off
			Rotate SysLong_STS_Total right

			SysLong_STS_Total.31 = SysULongInt_STS_Num.0
			
			Set C Off
			Rotate SysULongInt_STS_Num right


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

  SysLong_STS_Total +=1 'round up
	
	Set C Off
	rotate SysLong_STS_Total right	'make room for sign

	SysLong_STS_Total.31 = SysByte_STS_Sgn.0

  '---- should round up 1 bit ---
  'SysLong_STS_Total ++
  
	StringToSingle_E = [byte]SysLong_STS_Total_E 	
	StringToSingle_U = [byte]SysLong_STS_Total_U 
	StringToSingle_H = [byte]SysLong_STS_Total_H
	[byte]StringToSingle = [byte]SysLong_STS_Total
  
	SysByte_STS_Err = 0
	SysByte_STS_Err.0 = 1
End Function

/*
// Prototpe by Evan R. Venn 10/02/2024
Function StringToSingle ( in inStringToSingle as String ) As Single
	
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
*/

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

Function ULongIntToBin (in __ULongInt as uLongINT) as String * 64
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


/*

  Convert 0x40400000 a single-precision IEEE-754 number to decimal. 
  1.	Convert Hexadecimal to Binary: Hexadecimal 0x40400000 converts to binary as:
  2.	0100 0000 0100 0000 0000 0000 0000 0000
  3.	Identify the Components: IEEE-754 single-precision format is composed of 1 sign bit, 8 exponent bits, and 23 fraction (mantissa) bits.
  4.	Sign bit:        0
  5.	Exponent:       10000000
  6.	Fraction:   10000000000000000000000
  7.	Determine the Sign: The sign bit is 0, which means the number is positive.
  8.	Calculate the Exponent: The exponent in binary is 10000000. Convert this to decimal:
  9.	10000000 (binary) = 128 (decimal)

  IEEE-754 format uses a bias of 127 for single-precision, so we subtract 127 from the exponent:
  128 - 127 = 1

  10.	Calculate the Fraction (Mantissa): The fraction (or mantissa) is 10000000000000000000000 in binary, which represents:
  11.	1.10000000000000000000000
  12.	Combine the Components: Using the formula for IEEE-754 floating-point numbers:
  13.	(-1)^sign * 1.fraction * 2^(exponent - bias)

  Plugging in the values:
  (-1)^0 * 1.10000000000000000000000 * 2^1

  14.	Simplify the Expression:
  15.	1.10000000000000000000000 (binary) = 1 + 1/2 = 1.5 (decimal)

  Thus, the number becomes:
  1.5 * 2^1 = 1.5 * 2 = 3
  So, the IEEE-754 single-precision floating-point representation 0x40400000 converts to the decimal number 3.

*/
Function SingleToString(in SingleNum as Single) as String * 20
  ' Source from Clint Koehn 25/03/2024
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
  dim SysWordTempX as Word

  dim SysULongInt_STS_Num as ULongInt  
  SysULongInt_STS_Num =0
  dim SysLong_STS_Mantissa as Long  
  SysLong_STS_Mantissa = 0
  dim SysByte_STS_Sgn as Byte
  dim SysByte_STS_Exp as Byte
  dim SysByte_STS_Bin as Byte
  dim SysByte_STS_Ptr as Byte
  dim SysByte_STS_Err as Byte 

  'ExtractSingleParts. Set bytes to ensure GCBASIC does not LONG convert.
  [byte]SysLongTempB = [byte]SingleNum: SysLongTempB_H = [byte]SingleNum_H: SysLongTempB_U = [byte]SingleNum_U: SysLongTempB_E = [byte]SingleNum_E


  /*
  Identify the Components: IEEE-754 single-precision format is composed of 1 sign bit, 8 exponent bits, and 23 fraction (mantissa) bits.
  Sign bit:   0
  Exponent:   10000000
  Fraction:   10000000000000000000000
  */

  // Extract sign bit - Determine the Sign: The sign bit is 0, which means the number is positive.
  SysByte_STS_Sgn = SysLongTempB.31

  // Determine the Fraction part by a using a mask
  SysLong_STS_Mantissa = (SysLongTempB & 0x7FFFFF)
  
  // Rotate the variable to the left to remove the sign bit 
  Rotate SysLongTempB Left
  
  // The variable still has 4 parts  SysLongTempB_E, SysLongTempB_U, SysLongTempB_H, SysLongTemp

  // Calculate the Exponent: The exponent in the 8 bits.
  SysByte_STS_Exp = [byte]SysLongTempB_E

  // Check if SysLong_STS_Mantissa + SysByte_STS_Exp are zero, set and exit. A quick method 
  If (SysLong_STS_Mantissa + SysByte_STS_Exp) = 0 then
    SingleToString = " 0.0"
    SysByte_STS_Err = 0
    SysByte_STS_Err.0 = 1     '1 good - 0 - bad    
    Exit Function
  End If

  SysLong_STS_Mantissa.23 = 1      'add hidden Bit

  SysULongInt_STS_Num = 0

  Repeat 8   ' align 24 bit mantissa to left 32
    Set C Off
    rotate SysLong_STS_Mantissa Left
  End Repeat

  If SysByte_STS_Exp >= 127 Then            'number is > 0
    SysByte_STS_Bin = SysByte_STS_Exp - 126 'needed because I'm including the 1.

    Set C Off 
    Repeat SysByte_STS_Bin              'move integer into Num  
      rotate SysLong_STS_Mantissa Left  'when finished will be remainder
      rotate SysULongInt_STS_Num Left
    End Repeat

    SysByte_STS_Bin = 32

  else if SysByte_STS_Exp = 0 Then      'if Exp is 0 then answer is 0
    SingleToString = "0.0"
    SysByte_STS_Err = 0
    SysByte_STS_Err.0 = 1     '1 good - 0 - bad
    exit Function

  else '<127 - move decimal place to the left  answer is <= 0
    SysByte_STS_Bin = (126 - SysByte_STS_Exp)
    if (SysByte_STS_Bin > 23) Then ' resulting number will be less than 0.00000001
      SingleToString = "Error "
      SysByte_STS_Err = 0
      SysByte_STS_Err.0 = 0     '1 good - 0 - bad
      SysByte_STS_Err.1 = 1     '1 decimals places to many chars,  0 = ok      
      exit Function
    end If
    SysByte_STS_Bin += 32
  End If

  'init string to receive number
  SysByte_STS_Ptr = 20
  SingleToString = "          0.00000000"
  
  If SysLong_STS_Mantissa > 0 Then
    ' m * 100,000,000 >> 32 bits results in 8 digit accuracy
    SysULongIntTempA = SysLong_STS_Mantissa
    SysULongIntTempX = 0
    SysULongIntTempB = 100000000
    
    SysMultSub64

    Repeat SysByte_STS_Bin
      Set C Off
      Rotate SysULongIntTempX Right
    End Repeat

    SysLong_STS_Mantissa = SysULongIntTempX 'load m with fractional for safe keeping    
    SysULongIntTempA = SysLong_STS_Mantissa
  
    'Starting with fraction
      #IFDEF ChipFamily 12,14,15,16
        //~ Prevent stack issues
        SysByteTempX = False
        If [BYTE]SysULongIntTempA = 0 Then    // Check all is ZERO
        If SysULongIntTempA_H = 0 Then
        If SysULongIntTempA_U = 0 Then
        If SysULongIntTempA_E = 0 Then
        If SysULongIntTempA_A = 0 Then
        If SysULongIntTempA_B = 0 Then
        If SysULongIntTempA_C = 0 Then
        If SysULongIntTempA_D = 0 Then
          SysByteTempX = True
        End if
        End if
        End if
        End if
        End if
        End if
        End if
        End if
      #ELSE    
        SysULongIntTempB = 0
        SysCompEqual64
      #ENDIF

    do while SysByteTempX = False
      SysULongIntTempB = 10	'divide SysULongIntTempA by 10 till SysULongIntTempA = 0      
      SysDivSub64

      SysByte_STS_Bin = [byte]SysULongIntTempX 	'remainder hold 0-9
      SingleToString(SysByte_STS_Ptr) = (SysByte_STS_Bin + 48)	'add 48 gives tha ASC value of 0 - 9 

      SysByte_STS_Ptr --    ' decrement pos in SingleToString for next char
      #IFDEF ChipFamily 12,14,15,16
        //~ Prevent stack issues
        SysByteTempX = False
        If [BYTE]SysULongIntTempA = 0 Then    // Check all is ZERO
        If SysULongIntTempA_H = 0 Then
        If SysULongIntTempA_U = 0 Then
        If SysULongIntTempA_E = 0 Then
        If SysULongIntTempA_A = 0 Then
        If SysULongIntTempA_B = 0 Then
        If SysULongIntTempA_C = 0 Then
        If SysULongIntTempA_D = 0 Then
          SysByteTempX = True
        End if
        End if
        End if
        End if
        End if
        End if
        End if
        End if
      #ELSE    
        SysULongIntTempB = 0
        SysCompEqual64
      #ENDIF
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

  #IFDEF ChipFamily 12,14,15,16
    //~ Prevent stack issues
    SysByteTempX = False
        If [BYTE]SysULongIntTempA = 0 Then    // Check all is ZERO
        If SysULongIntTempA_H = 0 Then
        If SysULongIntTempA_U = 0 Then
        If SysULongIntTempA_E = 0 Then
        If SysULongIntTempA_A = 0 Then
        If SysULongIntTempA_B = 0 Then
        If SysULongIntTempA_C = 0 Then
        If SysULongIntTempA_D = 0 Then
      SysByteTempX = True
    End if
    End if
    End if
    End if
    End if
    End if
    End if
    End if
  #ELSE    
    SysULongIntTempB = 0
    SysCompEqual64
  #ENDIF

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
    
    #IFDEF ChipFamily 12,14,15,16
      //~ Prevent stack issues
      SysByteTempX = False
          If [BYTE]SysULongIntTempA = 0 Then    // Check all is ZERO
          If SysULongIntTempA_H = 0 Then
          If SysULongIntTempA_U = 0 Then
          If SysULongIntTempA_E = 0 Then
          If SysULongIntTempA_A = 0 Then
          If SysULongIntTempA_B = 0 Then
          If SysULongIntTempA_C = 0 Then
          If SysULongIntTempA_D = 0 Then
        SysByteTempX = True
      End if
      End if
      End if
      End if
      End if
      End if
      End if
      End if
    #ELSE    
      SysULongIntTempB = 0
      SysCompEqual64
    #ENDIF
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

// Evan R. Venn 10/02/2024
Function StringToSingle3DP ( in inStringToSingle as String ) As Single
	
	Dim _LongSTS as Long
	// determine decimal value from string
    _LongSTS = StringToLong(Mid( inStringToSingle, Instr ( inStringToSingle, ".")+1, 3 ))

    'using GCBASIC method to assing the Long to Single
    StringToSingle3DP = ( _LongSTS + (_LongSTS % 2) )
	
	StringToSingle3DP = StringToSingle3DP / 1000
	
    _LongSTS = StringToLong(Mid( inStringToSingle, 1, Instr ( inStringToSingle, ".")-1 ))

    // Add the two parts
    StringToSingle3DP = StringToSingle3DP + _LongSTS

	// Set to negative, if required
	If Instr(inStringToSingle, "-" ) Then StringToSingle3DP.31 = 1

End Function

Dim SingleToString3DP as String * 14
Function SingleToString3DP (  In SingleNum as Single  ) as String * 14
 
    //! Show to three decimal points using psuedo_multiplication .. which is addition, many times

        // Dimension the variables
        Dim ExponentNumberInteger, MantissaNumberInteger as Integer
        Dim ResultNumber as Single

        // Extract exponent
        // Assign the Single to the Integer 
        ExponentNumberInteger = [single]SingleNum

        // Deduct the exponent from float
        SingleNum = [single]SingleNum - [integer]ExponentNumberInteger

        psuedo_multiplication:
            // Now SingleNum is just the mantissa
            // Factorise to 3 digits
            //
            // ResultNumber is the result
            //
            [single]ResultNumber = [single]SingleNum

            Repeat 1000
                If SingleNum.31 = 0 Then
                    [single]ResultNumber += [single]SingleNum
                Else
                    [single]ResultNumber -= [single]SingleNum
                End If
            End Repeat

            // Handle rounding
            If SingleNum.31 = 0 Then
                [single]ResultNumber--
            Else
                [single]ResultNumber++
            End If
        end_of_psuedo_multiplication:

        MantissaNumberInteger = ResultNumber
        // Factorisation can cause decimal rounding so, check for the integer value and add to exponent
        ExponentNumberInteger  = ExponentNumberInteger '+ (MantissaNumberInteger / 1000)


        // Show results
        SingleToString3DP = IntegerToString(ExponentNumberInteger)
        SingleToString3DP += "."
        SingleToString3DP += Right("000"+Str(MantissaNumberInteger),3)
    
    End Function
