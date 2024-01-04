'    Copyright 2017-2024 Mike Otte
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


'    7 Segment LED display routines using the TM1637  for GCBASIC
'     The TM1637 module provide a cheap bright LED Display
'     The ebay modules can be had for $1 to $4, somtimes less.
'     They only need 2 pins to control : clk and dio
'     4 digit 7 seg Display
'     They do have a  colon but no decimal points
'     Brightness can be set: 8 is display on minimum bright , 15 is display on max bright
'     Less than 8  is display off
'     The TM1637 chip has the ability to read key matrix BUT that is not included.
'    Copyright (C)  2018  Mike Otte


'Changes:
'  initial h file build  8/7/2017  by Mike Otte
'Starting point was the viscomjim - 2016-11-23 post,  Thankyou
'
'Public Commands:
'
' Write4Dig (In TMdig1, In TMdig2, In TMdig3, In TMdig4, Optional In bright = 15, Optional In ColonON = 0)
' Program defines each digit as 0 to 9  OR 0x00 to 0x0F  (15)
' additionally  0x10  (16) is a blank  and 0x11 (17) is a minus sign
' optional parameters can set the brightness and the turn the colon on (only on digit 2)
'
' WriteChar (In TMaddr, In TMchar)
' TMchar is an ASCII char
' TMaddr is 0 , 1 , 2 , 3
'
'
' TMHex  ( In  TMValue as word)
' values up to 0xFFFF can be displayed
'
'
' TMDec  ( In  TMValue as word)
' Values up to 9999 can be displayed
'
'
' Remember you must define the two  ports in the main program
'#define TM1637_DIO PORTa.z
'#define TM1637_Clk PORTa.x

'commands
#define COM1 0x40   'Automatic address adding,  0x44 Fix address
#define COM2 0xC0   'display register address, 0xC0,0xC1,0xC2,0xC3
#define COM3 0x80   'Display control,  0x80 + 0x08 + 0x07
                    'Bit3 is display on =1  or off =0
                    'bits0-2 are bightness  0 to 7

#startup InitTM1637

Sub InitTM1637
  dim DispTemp as Byte
  dim Bright as Byte
      Bright = 15  'full brightness and turn on display
'#define TM1637_DIO PORTa.z
'#define TM1637_CLK PORTa.x
  #ifdef TM1637_DIO
    Dir TM1637_DIO Out
  #endif
  #ifdef TM1637_CLK
    Dir TM1637_CLK Out

end sub


'Numbers for display - support 0x00 to 0x0F
Table SevenSegDispDigit
  63 '0
  6
  91
  79
  102
  109
  125
  7
  127
  111 '9
          0x77
          0x7c
          0x39
          0x5e
          0x79
          0x71
          0x00  'blank
          0x40  ' -  dash
End Table

'Letters for display
'Letter is at ASC - 64, so A at 1, B at 2, etc
Table SevenSegDispLetter
  119 'A
  124 'B
  57 'C
  94 'D
  121 'E
  113 'F
  61 'G
  118 'H
  6 'I
  14 'J
  118 'K
  56 'L
  55 'M
  55 'N
  63 'O
  115 'P
  103 'Q
  80 'R
  109 'S
  7 'T
  62 'U
  62 'V
  62 'W
  118 'X
  110 'Y
  27 'Z
End Table

'#define TM1637_DIO PORTa.z
'#define TM1637_Clk PORTa.x




sub Write4Dig (In TMdig1, In TMdig2, In TMdig3, In TMdig4, Optional In bright = 15, Optional In ColonON = 0)
    'TMdigx is number 0 -15 , 16 =sp , 17 = -
    'convert digit number to 7segments
    ReadTable SevenSegDispDigit, TMdig1 + 1, TMseg1
    ReadTable SevenSegDispDigit, TMdig2 + 1, TMseg2
    ReadTable SevenSegDispDigit, TMdig3 + 1, TMseg3
    ReadTable SevenSegDispDigit, TMdig4 + 1, TMseg4
    If ColonON = 1 then
     TMseg2 = TMseg2 | 0B10000000   'turns on the colon
    End if
    Start
    WriteValue (COM1)
    Stop
    Start
    WriteValue (COM2)
    WriteValue (TMseg1)
    WriteValue (TMseg2)
    WriteValue (TMseg3)
    WriteValue (TMseg4)
    Stop
    Start
    WriteValue ((COM3) + (bright & 0x0f))
    Stop
end sub

sub WriteChar (In TMaddr, In TMchar)
    ' TMaddr  0 ,1, 2 ,3    digit postion to display
    'TMchar   ascii character  to display
    bright = 15
    'write ascii letter or number or sp or -
    If TMchar = 32 Then     ' space?
      TMseg1 = 0
      Goto ShowChar
  End if

  ' -
  If TMchar = 45 Then      'minus -
      TMseg1 = 64
      Goto ShowChar
  End if

  'Numbers
  If TMchar >= 48 And TMchar <= 57 Then
    ReadTable SevenSegDispDigit,  TMchar-47, TMseg1
    Goto ShowChar
  End If

  If TMchar < 65 Then Exit Sub    'should have been covered as number and space and -
  'Convert to upper case

  If TMchar > 96 Then           ' small case letter make it big
      TMchar = TMchar - 32
  End if

  'Exit if not a letter
  If TMchar > 90 Then Exit Sub

  'Convert to code for output

  ReadTable SevenSegDispLetter, TMchar -64, TMseg1
ShowChar:

    Start
    WriteValue (0x40)   '0x44 Fix address
    Stop
    Start
    WriteValue (0xC0 + TMaddr)
    WriteValue (TMseg1)
    Stop
    Start
    WriteValue ((COM3) + (bright & 0x0f))
    Stop
end sub

sub TMDec  ( In  TMValue as word)

    'TMValue must be in the range of 0 to 65535 (Dec)
    'TMValue can be entered as dec, binary or hex
    TMsVal = 16
    TMeVal = 0
    TMhVal = 0
    TMlVal = 0

          IF TMValue >= 1000 Then
                    TMsVal = TMValue / 1000 [word]
                    TMValue =SysCalcTempX
                    Goto TMWord100
          End If
          IF TMValue >= 100 Then
          TMWord100:
                    TMeVal = TMValue / 100  [word]
                    TMValue = SysCalcTempX
                    Goto TMWord10
          End If
          IF TMValue >= 10 Then
          TMWord10:
                    TMhVal = TMValue / 10 [word]
                    TMlVal = SysCalcTempX
          Else
            TMhVal = 0
            TMlVal = TMValue
          End If

    Write4Dig ( TMsVal, TMeVal, TMhVal, TMlVal)

end sub
sub TMHex  ( In  TMValue as word)

    'TMValue must be in the range of 0 to 65535 (Dec)
    'TMValue can be entered as dec, binary or hex
    TMsVal = 0
    TMeVal = 0
    TMhVal = 0
    TMlVal = 0

          IF TMValue >= 4096 Then
                    TMsVal = TMValue / 4096
                    TMValue = TMValue % 4096
          End If
          IF TMValue >= 256 Then
                    TMeVal = TMValue / 256
                    TMValue = TMValue % 256
          End If
          IF TMValue >= 16 Then
                    TMhVal = TMValue / 16
                    TMlVal = TMValue % 16
          End If

    Write4Dig ( TMsVal, TMeVal, TMhVal, TMlVal)

end sub

sub WriteValue(Value)
    for yy = 1 to 8
        set TM1637_CLK off
        'wait 5 us
        if Value & 1 = 1 then
            set TM1637_DIO on
        else
            set TM1637_DIO off
        end if
        'wait 5 us
        set TM1637_CLK on
        'wait 5 us
        Value = FnLSR(Value, 1)
    next yy

    'wait for ACK
    set TM1637_CLK off
    'wait 5 us
    dir TM1637_DIO in
    set TM1637_CLK on
    'wait 5 us
    if TM1637_DIO = 0 then
    dir TM1637_DIO out
    end if
    set TM1637_CLK off
end sub

sub Start
    set TM1637_CLK on
    set TM1637_DIO on
    'wait 5 us
    set TM1637_DIO off
    set TM1637_CLK off
    'wait 5 us
end sub

sub Stop
    set TM1637_CLK off
    set TM1637_DIO off
    'wait 5 us
    set TM1637_CLK on
    set TM1637_DIO on
    'wait 5 us
end sub
