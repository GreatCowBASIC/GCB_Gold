'    Some common BASIC commands/functions for GCBASIC
'    Copyright (C) 2006-2025 Hugh Considine, Chris Roper and Willaim Roth and Evan Venn

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
' 17/6/2009: AVR support added
' 10/2/2013: Indirect call added
' 08/02/2013: added to new functions _dectobcd and _bcdtodecot
' 26/09/2015: Added new methods lslBit, lsrBit and isNotBit - created by Chris Roper
' 06/10/2015: FnLSR, FnLSL, FnEQUBit and FnNOTBit - created by Chris Roper
' 24/10/2015: Fix for AVR handling for FnEQUBit and FnNotBit
' 01/11/2015: Fix for AVR handling for Fnlsl AND FNLSR
' 11/03/2016: Add LOCKPPS and UNLOCKPPS
' 16/01/2017: Modified PulseIn & PulseInInV for better functionality - WMR
' 29/01/2017: Added overloaded FnLSL and FnLSR to reduce memory overhead
' 19/05/2017: Revised and corrected DecToBcd_GCB.. overwriting system vars
' 04/03/2018: Added Scale() and a mapping to Map
' 30/01/2019: Added word Scale()
' 19/10/2021: Change PULSEx delays to one cycle less using WAITL1
' 04/03/2024: Add RoundSingle for Single support
' 07/03/2024: Add Int()
' 07/01/2024: Improved Int() messages
'Misc settings

'Bit rate delays
#define r300 64
#define r600 32
#define r1200 16
#define r2400 8
#define r4800 4
#define r9600 2
#define r19200 1
'Number to multiply above by to get correct delay length (us)
#define rBitRateUnit 52

'Moved from from i2c.h to ensure hwi2c.h is initialised correctly.
#define NAK             FALSE
#define NACK            FALSE     'permit alternative spelling
#define ACK             TRUE

'Indirect call
Sub IndCall(In SysCallAdr As Word)
  'Jump to a subroutine located at MemAdr
  #ifdef PIC
    Dim SysCallAdr As Word Alias SysWordTempX_H, SysWordTempX
    #ifdef Var(PCLATU)
      PCLATU = 0
    #endif
    PCLATH = SysCallAdr_H
    'Use inline assembly, or movff will be generated for 18F and this does not work
    movf SysCallAdr, W
    movwf PCL
  #endif
  #ifdef AVR
    Dim SysCallAdr As Word Alias SysReadA_H, SysReadA
    Dim SysReadA As Word
    ijmp
  #endif
End Sub

macro UNLOCKPPS
    dim IntState as bit
    IntState = GIE
    GIE = 0
    PPSLOCK = 0x55
    PPSLOCK = 0xAA
    PPSLOCKED = 0x00  'unlock PPS

end macro


macro LOCKPPS

    PPSLOCK = 0x55
    PPSLOCK = 0xAA
    PPSLOCKED = 0x01  'lock PPS
    GIE = IntState

end macro

'Direct memory access
Sub Poke (In MemAdr As Word, In MemData)
  #ifdef PIC
    #IFDEF ChipFamily 12,14
      SET STATUS.IRP OFF
      if MemAdr.8 ON then SET STATUS.IRP ON
      FSR = MemAdr
      INDF = MemData
    #ENDIF
    #IFDEF ChipFamily 15,16
      Dim MemAdr As Word Alias FSR0H, FSR0L
      'FSR0H = MemAdr_H
      'FSR0L = MemAdr
      INDF0 = MemData
    #ENDIF
  #endif
  #ifdef AVR
    Dim MemAdr As Word Alias SysStringA_H, SysStringA
    Dim MemData Alias SysValueCopy
    st X, MemData
  #endif
End Sub

Function Peek (MemAdr As Word)
  #ifdef PIC
    #IFDEF ChipFamily 12,14
      SET STATUS.IRP OFF
      if MemAdr.8 ON then SET STATUS.IRP ON
      FSR = MemAdr
      PEEK = INDF
    #ENDIF
    #IFDEF ChipFamily 15,16
      FSR0H = MemAdr_H
      FSR0L = MemAdr
      PEEK = INDF0
    #ENDIF
  #endif
  #ifdef AVR
    Dim MemAdr As Word Alias SysStringA_H, SysStringA
    Dim Peek Alias SysValueCopy
    ld Peek, X
  #endif
End Function

'Software PWM
'Cannot be put in pwm.h as then the InitPWM subroutine will be called even though it is not needed

'Duty is /255, Dur is ms
sub PWMOut(PWMChannel, SoftPWMDuty, SoftPWMCycles) #NR
  Dim PWMDur, DOPWM as Byte
  For PWMDur = 1 to SoftPWMCycles 
    For DOPWM = 1 to 255
      if SoftPWMDuty > DOPWM then
        #IFDEF PWM_Out1
          if PWMChannel = 1 then set PWM_Out1 ON
        #ENDIF
        #IFDEF PWM_Out2
          if PWMChannel = 2 then set PWM_Out2 ON
        #ENDIF
        #IFDEF PWM_Out3
          if PWMChannel = 3 then set PWM_Out3 ON
        #ENDIF
        #IFDEF PWM_Out4
          if PWMChannel = 4 then set PWM_Out4 ON
        #ENDIF
      Else
        #IFDEF PWM_Out1
          if PWMChannel = 1 then set PWM_Out1 OFF
        #ENDIF
        #IFDEF PWM_Out2
          if PWMChannel = 2 then set PWM_Out2 OFF
        #ENDIF
        #IFDEF PWM_Out3
          if PWMChannel = 3 then set PWM_Out3 OFF
        #ENDIF
        #IFDEF PWM_Out4
          if PWMChannel = 4 then set PWM_Out4 OFF
        #ENDIF
      end if
      #IFDEF PWM_Delay
        Wait PWM_Delay
      #ENDIF
    next DOPWM
  next PWMDur
end sub

'PulseOut
macro Pulseout (Pin, Time)
  Set Pin On
  WaitL1 Time
  Set Pin Off
end macro

'PulseOutInv (inverted PulseOut)
macro PulseOutInv (Pin, Time)
  Set Pin Off
  WaitL1 Time
  Set Pin On
end macro

'PulseInInv
'PulseIn
macro PulseInInv (PulseInPin, PulseTime as WORD, TimeUnit)
  PulseTime = 0
  Do While PulseInPin = Off
    WaitL1 1 TimeUnit
    PulseTime += 1
    If PulseTime = 0 Then Exit Do
  Loop
end macro

macro PulseIn (PulseInPin, PulseTime as Word, TimeUnit)
  PulseTime = 0
  Do While PulseInPin = On
    WaitL1 1 TimeUnit
    PulseTime += 1
    If PulseTime  = 0 Then Exit Do
  Loop
end macro

'Delay
'macro Sleep (time)
' Wait Time s
'end macro

macro Pause (time)
 Wait Time ms
end macro

'Provides a fast way to calculate the average of two 8 bit numbers
function Average(SysCalcTempA, SysCalcTempB)
  SET C OFF
  Average = SysCalcTempA + SysCalcTempB
  ROTATE Average RIGHT
end function


; Re-Scales a number from one range to another. That is, a value of fromLow would get Scaled to toLow, a value of fromHigh to toHigh, values in-between to values in-between, etc.
; Does not constrain values to within the range, because out-of-range values are sometimes intended and useful. The constrain() function may be used either before or after this function, if limits to the ranges are desired.
; Note that the "lower bounds" of either range may be larger or smaller than the "upper bounds" so the Scale() function may be used to reverse a range of numbers, for example
;       myvalue = Scale( InValue , 0, 255, -30, 30 )
;
; The function also handles negative numbers well, so that this example
;      myvalue = Scale( inValue, 1, 1023, 50, -100) is also valid and works well.
;
; The Scale() function uses integer maths.
;
; You can calibrate the scale also with an optiona calibration parameter
;      myvalue = Scale( InValue , 0, 255, -30, 30, 2 )  'will always offset the result by a calibration offset of 2.
;
;    27.02.2018    Second release to the world, faster, smarter and with optional calibration
#define map scale
function scale ( in l_map as word, in l_fromLow as integer, in l_fromHigh as integer, in l_toLow as integer, in l_toHigh  as integer, Optional in l_calibrate as integer = 0) as integer

  dim l_syscalc as integer
  dim l_syscalcF as long

  l_syscalcf =  [long]( l_map - l_fromLow ) * [long](l_toHigh   - l_toLow)
  l_syscalc = (  l_fromHigh - l_fromLow )
  scale = (l_syscalcf / l_syscalc) + l_toLow + l_calibrate

end function


'word implementation
function scale ( in l_map as word, in l_fromLow as word, in l_fromHigh as word, in l_toLow as word, in l_toHigh  as word, Optional in l_calibrate as integer = 0) as word

  dim l_syscalc as word
  dim l_syscalcF as long

  l_syscalcf =  [long]( l_map - l_fromLow ) * [long](l_toHigh   - l_toLow)
  l_syscalc = (  l_fromHigh - l_fromLow )
  scale = (l_syscalcf / l_syscalc) + l_toLow + l_calibrate

end function


'Miscellaneous Variable handling subs

'Swap SysTempA and SysTempB
Sub Swap(SysCalcTempA, SysCalcTempB)
  SysCalcTempX = SysCalcTempA
  SysCalcTempA = SysCalcTempB
  SysCalcTempB = SysCalcTempX
End Sub

Sub Swap(SysCalcTempA As Word, SysCalcTempB As Word)
  Dim SysCalcTempX As Word
  SysCalcTempX = SysCalcTempA
  SysCalcTempA = SysCalcTempB
  SysCalcTempB = SysCalcTempX
End Sub

Function Abs(SysCalcTempA As Integer) As Integer
  If SysCalcTempA.15 Then
    Abs = -SysCalcTempA
  Else
    Abs = SysCalcTempA
  End If
End Function

'Swap nibbles (4-byte blocks)
Function Swap4(Swap4In)
  #ifdef PIC
    swapf Swap4In, W
    movwf swap4
  #endif
  #ifdef AVR
    Dim Swap4In Alias SysCalcTempA
    Dim Swap4 Alias SysCalcTempX
    mov Swap4, Swap4In
    asm swap Swap4
  #endif
End Function

' Dec to BCD
Function DecToBcd_GCB( in SysInVal as Byte  ) as Byte
         DecToBcd_GCB = (SysInVal /10)*16+SysInVal%10
End Function


' BDC to Dec
Function BcdToDec_GCB( in SysInVal as byte ) as byte
         BcdToDec_GCB = (SysInVal/16)*10
         BcdToDec_GCB = BcdToDec_GCB + SysInVal%16
End Function

'' Bitwise Functions and Macros by Chris Roper
'
'' BitOut != BitIn
'Macro isNotBit(BitOut, BitIn)
'  If BitIn then
'     BitOut = 0
'  Else
'     BitOut = 1
'  End If
'End Macro
'
'' BitsOut = BitsIn << NumBits
'Macro lslBit(BitsOut, BitsIn, NumBits)
'  BitsOut = BitsIn
'  Repeat NumBits
'    STATUS.C = 0
'    Rotate BitsOut Left
'  End Repeat
'End Macro
'
'' BitsOut = BitsIn >> NumBits
'Macro lsrBit(BitBitsOut, BitsIn, NumBits)
'  BitsOut = BitsIn
'  Repeat NumBits
'    STATUS.C = 0
'    Rotate BitsOut Right
'  End Repeat
'End Macro
'
''eqBit(BitOut, BitIn)
'macro equBit(BitOut, BitIn)
'  if BitIn then
'     BitOut = 1
'  else
'     BitOut = 0
'  end if
'End macro

' 06/10/15@08:41

' Assign a Variable, Constant or Function to a Bit
' Compliments SET Method and Addresses Port Glitch Issues
' Note: Now deprecated - use #option volatile TargetBit and normal GCBASIC
' Set command or assignment operator.
macro SetWith(TargetBit, Source)
  if Source then
    TargetBit = 1
  else
    TargetBit = 0
  end if
end macro

' BitsOut = BitsIn >> NumBits
Function FnLSR(in SysLongTempB as long, in NumBits as byte) as long
  Repeat NumBits
    Set C Off
    Rotate SysLongTempB Right
  End Repeat
  FnLSR = SysLongTempB
End Function

Function FnLSR(in SysWordTempB as Word, in NumBits as byte) as Word
  Repeat NumBits
    Set C Off
    Rotate SysWordTempB Right
  End Repeat
  FnLSR = SysWordTempB
End Function


Function FnLSR(in SysByteTempB as Byte, in NumBits as byte) as Byte
  Repeat NumBits
    Set C Off
    Rotate SysByteTempB Right
  End Repeat
  FnLSR = SysByteTempB
End Function





' BitsOut = BitsIn << NumBits
Function FnLSL(in SysLongTempB as long, in NumBits as byte) as long
  Repeat NumBits
    Set C Off
    Rotate SysLongTempB Left
  End Repeat
  FnLSL = SysLongTempB
End Function

Function FnLSL(in SysWordTempB as Word, in NumBits as byte) as Word
  Repeat NumBits
    Set C Off
    Rotate SysWordTempB Left
  End Repeat
  FnLSL = SysWordTempB
End Function

Function FnLSL(in SysByteTempB as Byte, in NumBits as byte) as Byte
  Repeat NumBits
    Set C Off
    Rotate SysByteTempB Left
  End Repeat
  FnLSL = SysByteTempB
End Function



    ' BitOut = BitIn
  'Note: Now deprecated. Improvements to compiler should allow correct handling of bit values
    Function FnEQUBit(in BitIn) as bit
      If BitIn then
         FnEQUBit = 1
      Else
         FnEQUBit = 0
      End If
    end Function

    ' BitOut != BitIn
  'Note: Now deprecated. Compiler built-in Not operator now supports bit values.
    Function FnNOTBit(in BitIn) as bit
      If BitIn then
         FnNOTBit = 0
      Else
         FnNOTBit = 1
      End If

    end Function

  //~ added for Single Support
  Function RoundSingle ( in _Round_Single As Single ) as Long
          _Round_Single = ([Single]_Round_Single + [single]0.5)
          RoundSingle = _Round_Single
  End Function

  Function int ( SYSSINGLETEMP as Single ) as Long
    int = SYSSINGLETEMP
    End Function

Function int ( SYSSINGLETEMP as Integer ) as Integer
    //! Int() is not supported for Integer variables.  Int() is for transforming a Single variable to an Integer variable.
    //! Please close this library and remove the INT function call from your source program
    |
    //! Or, have you called INT OFF... the syntax is INTOFF ... note no space.
    //! Please close this library and correct your source program
    End Function

Function int ( SYSSINGLETEMP as Word ) as Word
    //! Int() is not supported for Word variables.  Int() is for transforming a Single variable to an Integer variable.
    //! Please close this library and remove the INT function call from your source program
    |
    //! Or, have you called INT OFF... the syntax is INTOFF ... note no space.
    //! Please close this library and correct your source program
    End Function

Function int ( SYSSINGLETEMP as Byte ) as Byte
    //! Int() is not supported for Byte variables.  Int() is for transforming a Single variable to an Integer variable.
    //! Please close this library and remove the INT function call from your source program
    |
    //! Or, have you called INT OFF... the syntax is INTOFF ... note no space.
    //! Please close this library and correct your source program
    End Function

Function int ( SYSSINGLETEMP as Long ) as Long
    //! Int() is not supported for Long variables.  Int() is for transforming a Single variable to an Integer variable.
    //! Please close this library and remove the INT function call from your source program
    |
    //! Or, have you called INT OFF... the syntax is INTOFF ... note no space.
    //! Please close this library and correct your source program
    End Function