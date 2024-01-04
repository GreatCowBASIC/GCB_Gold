'This library is free software; you can redistribute it and/or
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
'2020-09-09
' - Bugfixes in the startup and TMdec routine
'
' - Support for Siekoo 7 digit alphabet created by Alexander Fako�,
'
'   more info at:  http://en.fakoo.de/siekoo.html
'
' - 3 new special characters:
'   � Degree sign as special character 18
'   ) Bracket sign as special character 19
'   ? Question mark as special character 20
'
' - TM_char_Bright command for set brightness with TMvhar, TMdec and TMhex command
'
' - TMdec options for enable/disable leading zeroes and colon
'
'  Code review. Renaming methods and move documentation to the Help

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
  Dim TM_bright as byte
  'ERV recommend to add.
  TM_bright  = 15

  'ERV why have char_Bright as variable? Use TM_bright ?  unless I am mistaked TM_Bright can be used everywhere


  #ifdef TM1637_DIO
    Dir TM1637_DIO Out
  #endif
  #ifdef TM1637_CLK
    Dir TM1637_CLK Out
  #endif
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
          0x63  'degree
          0x0f  'bracket
          83    'queastion mark
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
  95  'a Siekoo alphabet char map
  124 'b by Alexander Fakoo 2012
  88  'c
  94  'd
  121 'E
  113 'F
  61  'G
  116 'h
  4   'i
  13  'j
  117 'k
  56  'L
  85  'M
  84  'n
  92  'o
  115 'P
  103 'Q
  80  'r
  45  'S
  120 't
  28  'u
  42  'v
  106 'W
  20  'x
  110 'y
  27  'Z
End Table





'erv renamed for consitency
sub TMWrite4Dig (In TMdig1, In TMdig2, In TMdig3, In TMdig4, Optional In TM_Bright=15, Optional In ColonON = 0)
    'TMdigx is number 0 -15 , 16 =sp , 17 = -
    'convert digit number to 7segments
    ReadTable SevenSegDispDigit, TMdig1 + 1, TMseg1
    ReadTable SevenSegDispDigit, TMdig2 + 1, TMseg2
    ReadTable SevenSegDispDigit, TMdig3 + 1, TMseg3
    ReadTable SevenSegDispDigit, TMdig4 + 1, TMseg4
    If ColonON = 1 then
     TMseg2 = TMseg2 | 0B10000000   'turns on the colon
    End if
    TM1637_Start
    TMWriteValue (COM1)
    TM1637_Stop
    TM1637_Start
    TMWriteValue (COM2)
    TMWriteValue (TMseg1)
    TMWriteValue (TMseg2)
    TMWriteValue (TMseg3)
    TMWriteValue (TMseg4)
    TM1637_Stop
    TM1637_Start
    TMWriteValue ((COM3) + (TM_Bright & 0x0f))
    TM1637_Stop
end sub

'erv renamed for consitency
sub TMWriteChar (In TMaddr, In TMchar)
    ' TMaddr  0 ,1, 2 ,3    digit postion to display
    'TMchar   ascii character  to display
    if TM_Bright > 15 then TM_Bright = 15
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

  If TMchar = 41 Then     ' )
      TMseg1 = 15
      Goto ShowChar
  End if

  If TMchar = 176 Then     '
      TMseg1 = 99
      Goto ShowChar
  End if


  If TMchar = 63 Then     ' ?
      TMseg1 = 83
      Goto ShowChar
  End if


  'Numbers
  If TMchar >= 48 And TMchar <= 57 Then
    ReadTable SevenSegDispDigit,  TMchar-47, TMseg1
    Goto ShowChar
  End If



  If TMchar < 65 Then Exit Sub    'should have been covered as number and space and -
  'Convert to upper case

  If TMchar > 96 Then           ' small case letter use Siekoo alphabet
      TMchar = TMchar - 6
  End if

  'Exit if not a letter
  If TMchar > 116 Then Exit Sub

  'Convert to code for output

  ReadTable SevenSegDispLetter, TMchar -64, TMseg1
ShowChar:

    TM1637_Start
    TMWriteValue (0x40)   '0x44 Fix address
    TM1637_Stop
    TM1637_Start
    TMWriteValue (0xC0 + TMaddr)
    TMWriteValue (TMseg1)
    TM1637_Stop
    TM1637_Start
    TMWriteValue ((COM3) + (TM_Bright & 0x0f))
    TM1637_Stop
end sub

sub TMDec  ( In  TMValue as word, Optional TM1637_Zero as byte = 0)', , Optional TM1637_Colon as byte = 0)

    'TMValue must be in the range of 0 to 65535 (Dec)
    'TMValue can be entered as dec, binary or hex
    TMsVal = 0
    TMeVal = 0
    TMhVal = 0
    TMlVal = 0
    TM1637_Colon = 0


    IF TMValue >= 10000 Then
    TMsVal = 17
    TMeVal = 17
    TMhVal = 17
    TMlVal = 17
    Goto TMWord1
    End if


          TMWord1000:
          IF TMValue >= 1000 Then
                    TMsVal = TMsVal + 1
                    TMValue = TMValue -1000
                    Goto TMWord1000
          End If
          TMWord100:
         IF TMValue >= 100 Then
                    TMeVal = TMeVal + 1
                    TMValue = TMValue - 100
                    Goto TMWord100
          End If
          TMWord10:
          IF TMValue >= 10 Then
                    TMhVal = TMhVal + 1
                    TMValue = TMValue - 10
                    Goto TMWord10
          End if
            TMlVal = TMValue
 if TM1637_Zero > 3 Then TM1637_Zero = 0
 if TM1637_Zero = 0 Or TM1637_Zero = 2 Then
    if TMsVal = 0 Then TMsVal = 16
    if TMsVal = 16 And TMeVal = 0 Then TMeVal = 16
    if TMeVal = 16 And TMhVal = 0 Then TMhVal = 16
 End if

if TM1637_Zero > 1 Then TM1637_Colon = 1
    TMWord1:
    TMWrite4Dig ( TMsVal, TMeVal, TMhVal, TMlVal,TM_Bright, TM1637_Colon)

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

    TMWrite4Dig ( TMsVal, TMeVal, TMhVal, TMlVal,TM_Bright,0)

end sub

'erv renamed for consitency
sub TMWriteValue( TMValue )
    for TM_CLOCKOUT = 1 to 8
        set TM1637_CLK off
        wait 50 us
        if TMValue & 1 = 1 then
            set TM1637_DIO on
        else
            set TM1637_DIO off
        end if
        wait 50 us
        set TM1637_CLK on
        wait 50 us
        TMValue = FnLSR(TMValue, 1)
    next TM_CLOCKOUT

    'wait for ACK
    set TM1637_CLK off
    wait 50 us
    dir TM1637_DIO in
    set TM1637_CLK on
    wait 50 us
    if TM1637_DIO = 0 then
    dir TM1637_DIO out
    end if
    set TM1637_CLK off
end sub

'erv renamed for consitency
sub TM1637_Start
    set TM1637_CLK on
    set TM1637_DIO on
    wait 50 us
    set TM1637_DIO off
    set TM1637_CLK off
    wait 50 us
end sub

'erv renamed for consitency
sub TM1637_Stop
    set TM1637_CLK off
    set TM1637_DIO off
    wait 50 us
    set TM1637_CLK on
    set TM1637_DIO on
    wait 50 us
end sub
