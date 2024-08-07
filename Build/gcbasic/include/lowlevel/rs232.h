;    Serial/RS232 routines for GCBASIC
;    Copyright (C) 2006, 2014, 2015, 2017 Hugh Considine, William Roth and Evan Venn

;    This library is free software; you can redistributet and/or
;    modify it under the terms of the GNU Lesser General Public
;    License as published by the Free Software Foundation; either
;    version 2.1 of the License, or (at your option) any later version.

;    This library is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    Lesser General Public License for more details.

;    You should have received a copy of the GNU Lesser General Public
;    License along with this library; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;********************************************************************************
;    IMPORTANT:
;    THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
;    UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
;    COMMANDS UNUSABLE!
;********************************************************************************
;
;*******************************************************************************
;   October  2014  Revised SerReceive to have a new parameter to prevent Interrupt handler
;   February 2015 - William Roth
;     1. Modified SerFullDelay  for more accurate output baud rates.
;     2. Added Padding to bit delay times to balance High/Low bit times
;  26 Aug 2016. Added Serprint Long support.
;  29 Aug 2016. Added 5 ms delay to init to ensure lines are settled.
;  08 May 2020. Reduced RAM consumption using Serprint with string constants.
;  23 Aug 2021. Repleaced all #defines with Script to prevent Constants
;     from being globally defined when library is not being called/used.
;     with these script matching the development guidelines
;********************************************************************************

;    Usage of Sys232Temp
;    Bit    Use
;    0    Dummy receive source
;    1    Receive buffer

;    I/O command defines
;    Note: These all default to dummy ports and MUST BE CHANGED PRIOR TO USE
;    #define SendALow  nop
;    #define SendAHigh  nop
;    #define RecALow Sys232Temp.0 OFF
;    #define RecAHigh Sys232Temp.0 ON
;    #define SendBLow  nop
;    #define SendBHigh  nop
;    #define RecBLow Sys232Temp.0 OFF
;    #define RecBHigh Sys232Temp.0 ON
;    #define SendCLow  nop
;    #define SendCHigh  nop
;    #define RecCLow Sys232Temp.0 OFF
;    #define RecCHigh Sys232Temp.0 ON

'Assign values to key words

    #SCRIPT
       __DEFINE_RS232H_CONSTANTS = FALSE 'default
       IF SENDALOW Then __DEFINE_RS232H_CONSTANTS = TRUE
       IF SENDBLOW Then __DEFINE_RS232H_CONSTANTS = TRUE
       IF SENDCLOW Then __DEFINE_RS232H_CONSTANTS = TRUE
       IF RECAHIGH Then __DEFINE_RS232H_CONSTANTS = TRUE
       IF RECALOW  Then __DEFINE_RS232H_CONSTANTS = TRUE

    #ENDSCRIPT

    #SCRIPT
         IF __DEFINE_RS232H_CONSTANTS = TRUE then
                none = 0 : odd = 1 : even = 2 : normal = 0 : invert = 1
                WaitForStart = 128 : SerialInitDelay = 5
                SerRxData = Sys232Temp.1
         END IF
    #ENDSCRIPT

' legacy #defines  replaced by script above
''Parity
'#define none 0
'#define odd 1
'#define even 2
'
''Signal Inversion
'#define normal 0
'#define invert 1
'
''Start Bit settings
'#define WaitForStart 128
'
'#define SerialInitDelay 5

'Calculate delay lengths
#script
    If PIC Then
       '20 MHz PIC, 7 us taken off - 35 instructions
       'These Delays are for Serial RX  with PIC
       'Modified Accuracy by William Roth 18-04-2105

      IF __DEFINE_RS232H_CONSTANTS = True then

        SerThirdDelay19200 = int(18 - 4 / ChipMHz * 22)
        SerThirdDelay9600 = int(35 - 4 / ChipMHz * 27)
        SerThirdDelay4800 = int(70 - 4 / ChipMHz * 31)
        SerThirdDelay2400 = int(142 - 5 / ChipMHz * 30)
        SerThirdDelay1200 = int(283 - 5 / ChipMHz * 33)
        SerThirdDelay600 = int(565  - 5 / ChipMhz) * 33
        SerThirdDelay300 = int(1120 - 4 / ChipMHz * 35)

        '20 MHz PIC, 10 us taken off - 50 instructions
        SerFullDelay19200 = int(52 - 4 / ChipMHz * 36)
        SerFullDelay9600 = int(104 - 4 / ChipMHz * 41)
        SerFullDelay4800 = int(208 - 4 / ChipMHz * 47)
        SerFullDelay2400 = int(417 - 4 / ChipMHz * 54)
        SerFullDelay1200 = int(833 - 4 / ChipMHz * 58)
        SerFullDelay600 = int(1666 - 4 / ChipMHz * 61)
        SerFullDelay300 = int(3333 - 4 / ChipMHz * 62)
      End IF
    END IF

    If AVR Then
        IF __DEFINE_RS232H_CONSTANTS = True Then

         '1 MHz AVR, 60 us taken off - 60 instructions
         SerThirdDelay19200 = int(19 - 1 / ChipMHz * 60)
         SerThirdDelay9600 = int(36 - 1 / ChipMHz * 60)
         SerThirdDelay4800 = int(70 - 1 / ChipMHz * 60)
         SerThirdDelay2400 = int(139 - 1 / ChipMHz * 60)
         SerThirdDelay1200 = int(278 - 1 / ChipMHz * 60)
         SerThirdDelay600 = int(555 - 1 / ChipMHz * 60)
         SerThirdDelay300 = int(1111 - 1 / ChipMHz * 60)

         '1 MHz AVR, 67 us taken off - 67 instructions
         SerFullDelay19200 = int(52 - 1 / ChipMHz * 49)
         SerFullDelay9600 = int(104 - 1 / ChipMHz * 50)
         SerFullDelay4800 = int(208 - 1 / ChipMHz * 50)
         SerFullDelay2400 = int(417 - 1 / ChipMHz * 50)
         SerFullDelay1200 = int(833 - 1 / ChipMHz * 50)
         SerFullDelay600 = int(1666 - 1 / ChipMHz * 50)
         SerFullDelay300 = int(3333 - 1 / ChipMHz * 50)
       End IF

    End If
#Endscript



sub InitSer(In Ser_Select, In Ser_Rate, In Ser_Start, In Ser_Data, In Ser_Stop, In Ser_Parity, In Ser_Invert)
  'This sub sets configuration of the serial routines
  'Sample usage for communication with Lego RCX:
  'InitSer(1,r2400,1,8,1,odd,invert)

   Ser_Select_Old = Ser_Select

  #ifdef OneOf(SendAHigh, RecAHigh)
       if Ser_Select = 1 THEN
          Ser_Rate_A = Ser_Rate
          ser_Start_A = Ser_Start
          Ser_Data_A = Ser_Data
          Ser_Stop_A = Ser_Stop
          Ser_Parity_A = Ser_Parity
          Ser_Invert_A = Ser_Invert
       END IF
  #ENDIF

  #ifdef OneOf(SendBHigh, RecBHigh)
       if Ser_Select = 2 THEN
          Ser_Rate_B = Ser_Rate
          Ser_Start_B = Ser_Start
          Ser_Data_B = Ser_Data
          Ser_Stop_B = Ser_Stop
          Ser_Parity_B = Ser_Parity
          Ser_Invert_B = Ser_Invert
       END IF
  #ENDIF

  #ifdef OneOf(SendCHigh, RecCHigh)
       If Ser_Select = 3 THEN
          Ser_Rate_C = Ser_Rate
          Ser_Start_C = Ser_Start
          Ser_Data_C = Ser_Data
          Ser_Stop_C = Ser_Stop
          Ser_Parity_C = Ser_Parity
          Ser_Invert_C = Ser_Invert
       END IF
  #ENDIF

  'ensure lines are settled
  wait SerialInitDelay ms

end sub

Sub SerSend(In Ser_Select, In Ser_Byte)

   'Load configuration data
   if Ser_Select <> Ser_Select_Old then SerCfgLoad(Ser_Select)

   'Start bit
   SerTemp = Ser_Start and (not WaitForStart)
   for SerBit = 1 to SerTemp
      if Ser_Invert = normal then SerTxLow: NOP: NOP: NOP  'fst
      if Ser_Invert = invert then SerTxHigh: NOP: NOP  'fst
   next

   'Data bits
   TempParity = SerTemp
   for SerBit = 1 to Ser_Data
      if Ser_Invert = normal then
         if Ser_Byte.0 ON then SerTxHigh: TempParity = TempParity + 1
         if Ser_Byte.0 OFF then SerTxLow: nop: nop: nop: nop: nop: nop: nop
      end if

      if Ser_Invert = invert then
         if Ser_Byte.0 ON then SerTxLow: TempParity = TempParity + 1
         if Ser_Byte.0 OFF then SerTxHigh: nop: nop: nop: nop: nop: nop: nop
      end if
      rotate Ser_Byte right
  next

   'Parity Bit
   if Ser_Parity <> 0 THEN
      if Ser_Parity = odd then TempParity = TempParity + 1
      if Ser_Invert = invert then TempParity = TempParity + 1
      if TempParity.0 = ON then SerTxLow
      if TempParity.0 = OFF then SerTxHigh
   end if

   'Stop Bit(s)
   for SerBit = 1 to Ser_Stop
      if Ser_Invert = normal then SerTxHigh   'fst
      if Ser_Invert = invert then SerTxLow    'fst
  next

  'Extra delay at end (adds extra stop bit)
  SerBitDelay
end sub


Sub SerReceive(In Ser_Select, Out Ser_Byte, Optional  Ser_DoNotChangeInterruptState = false )

  'Load configuration data
  if Ser_Select <> Ser_Select_Old then SerCfgLoad(Ser_Select)

  'Receive start bit/s
  'If Ser_Start.7 is on then wait, otherwise exit if there's no signal
  If Ser_Start.7 = On Then
    Wait Until SerQuickSample = TRUE
  Else
    If SerQuickSample = False Then Exit Sub
  End If

  if Ser_DoNotChangeInterruptState = false then
            'Disable interrupts
            IntOff
  end if

  'Get start bits
  SerTemp = Ser_Start and (not WaitForStart)
  for SerBit = 1 to SerTemp
    SerRxBit
  next

  'Receive data byte
  Ser_Byte = 0
  for SerBit = 1 to Ser_Data
    SerRxBit
    Rotate Ser_Byte Right
  next
  'Add any missing bits
  if Ser_Data < 8 then
    'SerTemp = 8 - Ser_Data
    for SerBit = Ser_Data to 8
      Set C Off
      ROTATE Ser_Byte RIGHT
    next
  end if
  if Ser_Invert = invert then Ser_Byte = !Ser_Byte
  'Ser_Byte = !Ser_Byte

  'Receive parity
  if Ser_Parity <> 0 THEN
    TempParity = 0
    SerRxBit
  end if

  'Receive stop bit/s
  'if Ser_Start.7 on then wait until SerQuickSample = FALSE
  'if Ser_Start.7 off then
  ' for Bit = 1 to Ser_Stop
  '   SerRxBit
  ' next
  'end if
  Wait Until SerQuickSample = FALSE

  if Ser_DoNotChangeInterruptState = false then
            'Re-enable interrupt
            IntOn
  end if
end sub

sub SerPrint (In Ser_Select, In PrintData As String)
   'PrintLen = LEN(PrintData$)
   PrintLen = PrintData(0)

   if PrintLen = 0 then Goto SerPrintStrEnd

   'Write Data
   for SysPrintTemp = 1 to PrintLen
      SerSend(Ser_Select, PrintData(SysPrintTemp))
   next

   'CR
   SerPrintStrEnd:
   #IFDEF SerPrintCR
     SerSend(Ser_Select, 13)
  #ENDIF
  #IFDEF SerPrintLF
     SerSend(Ser_Select, 10)
  #ENDIF
end sub

sub SerPrint (In Ser_Select, In SerPrintVal)

  OutValueTemp = 0

  IF SerPrintVal >= 100 Then
    OutValueTemp = SerPrintVal / 100
    SerPrintVal = SysCalcTempX
    SerSend(Ser_Select, OutValueTemp + 48)
  End If
  If OutValueTemp > 0 Or SerPrintVal >= 10 Then
    OutValueTemp = SerPrintVal / 10
    SerPrintVal = SysCalcTempX
    SerSend(Ser_Select, OutValueTemp + 48)
  End If
  SerSend(Ser_Select, SerPrintVal + 48)

  'CR
  #IFDEF SerPrintCR
    SerSend(Ser_Select, 13)
  #ENDIF
  #IFDEF SerPrintLF
    SerSend(Ser_Select, 10)
  #ENDIF

end sub

Sub SerPrint (In Ser_Select, In SerPrintVal As Word)
  Dim SysCalcTempX As Word

  OutValueTemp = 0

  If SerPrintVal >= 10000 then
    OutValueTemp = SerPrintVal / 10000 [word]
    SerPrintVal = SysCalcTempX
    SerSend(Ser_Select, OutValueTemp + 48)
    Goto SerPrintWord1000
  End If

  If SerPrintVal >= 1000 then
  SerPrintWord1000:
    OutValueTemp = SerPrintVal / 1000 [word]
    SerPrintVal = SysCalcTempX
    SerSend(Ser_Select, OutValueTemp + 48)
    Goto SerPrintWord100
  End If

  If SerPrintVal >= 100 then
  SerPrintWord100:
    OutValueTemp = SerPrintVal / 100 [word]
    SerPrintVal = SysCalcTempX
    SerSend(Ser_Select, OutValueTemp + 48)
    Goto SerPrintWord10:
  End If

  If SerPrintVal >= 10 then
  SerPrintWord10:
    OutValueTemp = SerPrintVal / 10 [word]
    SerPrintVal = SysCalcTempX
    SerSend(Ser_Select, OutValueTemp + 48)
  End If

  SerSend(Ser_Select, SerPrintVal + 48)

  'CR
  #IFDEF SerPrintCR
    SerSend(Ser_Select, 13)
  #ENDIF
  #IFDEF SerPrintLF
    SerSend(Ser_Select, 10)
  #ENDIF

End Sub


Sub SerPrint (In Ser_Select, In SerPrintVal As Long)
     Dim SysCalcTempA As Long
      Dim SysPrintBuffer(10)
      SysPrintBuffLen = 0

      Do
                'Divide number by 10, remainder into buffer
                SysPrintBuffLen += 1
                SysPrintBuffer(SysPrintBuffLen) = SerPrintVal % 10
                SerPrintVal = SysCalcTempA
      Loop While SerPrintVal <> 0

      'Display
      For SysPrintTemp = SysPrintBuffLen To 1 Step -1
                SerSend (Ser_Select,SysPrintBuffer(SysPrintTemp) + 48)
      Next

      'CR
      #IFDEF SerPrintCR
                SerSend (Ser_Select, 13)
      #ENDIF
      #IFDEF SerPrintLF
                SerSend ( Ser_Select, 10)
      #ENDIF

End Sub

Sub SerPrint (In Ser_Select, In SerPrintValInt As Integer)
  Dim SerPrintVal As Word

  'If sign bit is on, print - sign and then negate
  If SerPrintValInt.15 = On Then
    SerSend(Ser_Select, "-")
    SerPrintVal = -SerPrintValInt

  'Sign bit off, so just copy value
  Else
    SerPrintVal = SerPrintValInt
  End If

  'Use Print(word) to display value
  SerPrint Ser_Select, SerPrintVal
End Sub

'Note: When calling this sub, set Ser_Select and Ser_Rate, and read carry bit
sub SerRxBit
  'Clear bit counters
  RxHighCount = 0

  'Read Port
  #ifdef OneOf(SendAHigh, RecAHigh)
    if Ser_Select = 1 THEN
      for SerBitSample = 1 to 3
        if RecAHigh then RxHighCount = RxHighCount + 1
        SerThirdBitDelay
      next
    end if
  #endif
  #ifdef OneOf(SendBHigh, RecBHigh)
    if Ser_Select = 2 THEN
      for SerBitSample = 1 to 3
        if RecBHigh then RxHighCount = RxHighCount + 1
        SerThirdBitDelay
      next
    end if
  #endif
  #ifdef OneOf(SendCHigh, RecCHigh)
    if Ser_Select = 3 THEN
      for SerBitSample = 1 to 3
        if RecCHigh then RxHighCount = RxHighCount + 1
        SerThirdBitDelay
      next
    end if
  #ENDIF

  'Decide whether received bit is 0 or 1, based on bit counters
  SET C OFF
  If RxHighCount.1 = ON Then SET C ON
end sub

Macro SerTxHigh
  #ifdef SendAHigh
    if Ser_Select = 1 THEN SendAHigh
  #endif
  #ifdef SendBHigh
    if Ser_Select = 2 THEN SendBHigh
  #ENDIF
  #ifdef SendCHigh
    if Ser_Select = 3 THEN SendCHigh
  #ENDIF
  SerBitDelay
End Macro

Macro SerTxLow
  #ifdef SendALow
    if Ser_Select = 1 THEN SendALow
  #endif
  #ifdef SendBLow
    if Ser_Select = 2 THEN SendBLow
  #ENDIF
  #ifdef SendCLow
    if Ser_Select = 3 THEN SendCLow
  #ENDIF
  SerBitDelay
End Macro

function SerQuickSample
  SerQuickSample = FALSE

  'Read Port
  #ifdef OneOf(RecAHigh)
    if Ser_Select = 1 THEN
      if RecAHigh then SerQuickSample = TRUE
    end if
  #endif
  #ifdef OneOf(RecBHigh)
    if Ser_Select = 2 THEN
      if RecBHigh then SerQuickSample = TRUE
    end if
  #ENDIF
  #ifdef OneOf(RecCHigh)
    if Ser_Select = 3 THEN
      if RecCHigh then SerQuickSample = TRUE
    end if
  #ENDIF
  if Ser_Invert = normal then SerQuickSample = !SerQuickSample  'fst

end function

sub SerCfgLoad(Ser_Select) #NR
Ser_Select_Old = Ser_Select

#ifdef OneOf(SendAHigh, RecAHigh)
if Ser_Select = 1 THEN
 Ser_Rate = Ser_Rate_A
 Ser_Start = Ser_Start_A
 Ser_Data = Ser_Data_A
 Ser_Stop = Ser_Stop_A
 Ser_Parity = Ser_Parity_A
 Ser_Invert = Ser_Invert_A
end if
#endif
#ifdef OneOf(SendBHigh, RecBHigh)
if Ser_Select = 2 THEN
 Ser_Rate = Ser_Rate_B
 Ser_Start = Ser_Start_B
 Ser_Data = Ser_Data_B
 Ser_Stop = Ser_Stop_B
 Ser_Parity = Ser_Parity_B
 Ser_Invert = Ser_Invert_B
end if
#ENDIF
#ifdef OneOf(SendCHigh, RecCHigh)
if Ser_Select = 3 THEN
 Ser_Rate = Ser_Rate_C
 Ser_Start = Ser_Start_C
 Ser_Data = Ser_Data_C
 Ser_Stop = Ser_Stop_C
 Ser_Parity = Ser_Parity_C
 Ser_Invert = Ser_Invert_C
end if
#ENDIF
end sub

sub SerBitDelay
  'Predefined rates (more accurate)
  'All delays have 10 us taken off to allow processing time
  if Ser_Rate = 1 then Wait SerFullDelay19200 us: exit sub  '19200 (52 us)
  if Ser_Rate = 2 then Wait SerFullDelay9600 us: exit sub   '9600 (104 us)
  if Ser_Rate = 4 then Wait SerFullDelay4800 us: exit sub   '4800 (208 us)
  if Ser_Rate = 8 then Wait SerFullDelay2400 us: exit sub   '2400 (417 us)
  if Ser_Rate = 16 then Wait SerFullDelay1200 us: exit sub  '1200 (833 us)
  if Ser_Rate = 32 then Wait SerFullDelay600 us: exit sub   '600 (1666 us)
  if Ser_Rate = 64 then Wait SerFullDelay300 us: exit sub '300 (3333 us)

  'Other rates
  for SerDelayLoop = 1 to Ser_Rate
    Wait 42 us
  next
end sub

sub SerThirdBitDelay
  'Predefined rates (more accurate)
  if Ser_Rate = 1 then Wait SerThirdDelay19200 us: exit sub '19200 (17 us)
  if Ser_Rate = 2 then Wait SerThirdDelay9600 us: exit sub  '9600 (35 us)
  if Ser_Rate = 4 then Wait SerThirdDelay4800 us: exit sub  '4800 (69 us)
  if Ser_Rate = 8 then Wait SerThirdDelay2400 us: exit sub  '2400 (139 us)
  if Ser_Rate = 16 then Wait SerThirdDelay1200 us: exit sub   '1200 (278 us)
  if Ser_Rate = 32 then Wait SerThirdDelay600 us: exit sub  '600 (555 us)
  if Ser_Rate = 64 then Wait SerThirdDelay300 us: exit sub  '300 (1111 us)

  'Other rates
  for SerDelayLoop = 1 to Ser_Rate
    Wait 13 us
  next
end sub
