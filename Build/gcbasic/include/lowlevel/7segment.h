'    7 Segment LED/LCD display routines for GCBASIC
'    Copyright (C) 2006-2024 Hugh Considine, Evan R. Venn, Kent Shafer and Mike Otte

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
' 16/6/2009: New connection modes, and now uses data tables to store codes
' 29/3/2014: Updated by Evan R. Venn.  There is an incorrect calculation in the method DisplayChar. The method of displaying a numeric character was incorrect.
'            Corrected and updated the header file.
'            Also extended the DisplayValue to support 0x0 to 0xF. It was 0x0 to 0x9. Also added an example to an update to the help file.
; 1/4/2014:  Added two new defines, #define 7Seg_CommonAnode, and #define 7Seg_HighSide to support common anode devices and high side drivers
' 9/10/2016: Revised to support turning off the selects and added Deselect(S)
'26/10/2016 changed selects, added Dot parameter to DisplayValue to display decimal point
'16/11/2016 revised to correct commonanode support
'16/11/2016 revised to add decimal point and - to DisplayChar
'1/1/2018   revised to support variable naming with new compiler

'When using DisplayPortx constants, the following setup is assumed:
' PIC Pin   Display Segment
' PORTname.0  A
' PORTname.1  B
' PORTname.2  C
'  ...
' PORTname.5  F
' PORTname.6  G
' PORTname.7  Decimal
'The constants will need to be set up as follows:
'#define DisplayPortA PORTx
'#define DisplayPortD PORTx
'If several displays are connected to the same port, then the corresponding
'DisplayPortx constants should be set to the same value.
'
'If any other setup has been used, then DISP_SEG_A, etc constants must be set:
'#define DISP_SEG_A PORTa.z
'#define DISP_SEG_B PORTa.x
'...
'#define DISP_SEG_G PORTa.x
'#define DISP_SEG_DOT PORTa.y
'Using DisplayPortx constants will give more efficient code, but is harder to
'arrange for. When using DISP_SEG_x constants, it is assumed that if multiple
'displays are used, they will all be connected to the same port (ie, seg A on
'display 1 connects to the same pin as seg A on display 2. DisplayPortx and
'DISP_SEG_x constants should not both be set, as this will cause strange
'display results.

'Commands to enable individual displays
'Only used when several displays are multiplexed on the same port
'Can specify the commands, like so:
'#define DispSelectA nop
'#define DispSelectB nop
'#define DispSelectC nop
'#define DispSelectD nop
'Or the enable pins:
'#define DISP_SEL_1 PORTa.b
'...
'#define DISP_SEL_4 PORTa.b

#startup InitSevenSeg

Sub InitSevenSeg

  dim DispTemp as Byte

  'Whole port mode
  #ifdef DisplayPortA
    Dir DisplayPortA Out
  #endif
  #ifdef DisplayPortB
    Dir DisplayPortB Out
  #endif
  #ifdef DisplayPortC
    Dir DisplayPortC Out
  #endif
  #ifdef DisplayPortD
    Dir DisplayPortD Out
  #endif

  'Individual bits
  #ifdef DISP_SEG_A
    Dir DISP_SEG_A Out
  #endif
  #ifdef DISP_SEG_B
    Dir DISP_SEG_B Out
  #endif
  #ifdef DISP_SEG_C
    Dir DISP_SEG_C Out
  #endif
  #ifdef DISP_SEG_D
    Dir DISP_SEG_D Out
  #endif
  #ifdef DISP_SEG_E
    Dir DISP_SEG_E Out
  #endif
  #ifdef DISP_SEG_F
    Dir DISP_SEG_F Out
  #endif
  #ifdef DISP_SEG_G
    Dir DISP_SEG_G Out
  #endif
  #ifdef DISP_SEG_DOT
    Dir DISP_SEG_DOT Out
  #endif

  'Selection pins
  #ifdef DISP_SEL_1
    Dir DISP_SEL_1 Out
  #endif
  #ifdef DISP_SEL_2
    Dir DISP_SEL_2 Out
  #endif
  #ifdef DISP_SEL_3
    Dir DISP_SEL_3 Out
  #endif
  #ifdef DISP_SEL_4
    Dir DISP_SEL_4 Out
  #endif

End Sub

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

'Write raw values to the device
Sub DisplaySegment(In DispPort, In DispTemp)

  'Select display and show integer
  DisplaySevenSeg

End Sub

'Write integer between 0 and 9 inclusive
Sub DisplayValue(In DispPort, In DispChar, Optional In _DispDot = 0 )

  'Convert to code for output
  ReadTable SevenSegDispDigit, DispChar + 1, DispTemp
  'Add decimal point if _DispDot = 1
  If _DispDot.0 = 1 Then DispTemp= DispTemp OR 128
  'Select display and show integer
  DisplaySevenSeg

End Sub

'Write ASCII character
Sub DisplayChar(In DispPort, In DispChar, Optional In _DispDot = 0 )

  'Only accept letters A-Z
  'Space
  If DispChar = 32 Then
      DispTemp = 0
      Goto ShowChar
  End if

  ' -
  If DispChar = 45 Then
      DispTemp = 64
      Goto ShowChar
  End if

  'Numbers
  If DispChar >= 48 And DispChar <= 57 Then
    ReadTable SevenSegDispDigit, DispChar - 47, DispTemp
    Goto ShowChar
  End If

  If DispChar < 65 Then Exit Sub
  'Convert to upper case

  If DispChar > 96 Then
      DispChar = DispChar - 32
  End if

  'Exit if not a letter
  If DispChar > 90 Then Exit Sub

  'Convert to code for output
  ReadTable SevenSegDispLetter, DispChar - 64, DispTemp

ShowChar:
  'Add decimal point if _DispDot = 1
  If _DispDot.0 = 1 Then DispTemp= DispTemp OR 128
  DisplaySevenSeg
End Sub

'Display the value on the seven segment display
Macro DisplaySevenSeg

  'Individual segment code
  #ifdef DISP_SEG_A
    #ifdef 7Seg_CommonAnode
      'Clear current display
      Set DISP_SEG_A On
      Set DISP_SEG_B On
      Set DISP_SEG_C On
      Set DISP_SEG_D On
      Set DISP_SEG_E On
      Set DISP_SEG_F On
      Set DISP_SEG_G On
      #ifdef DISP_SEG_DOT
        Set DISP_SEG_DOT On
      #endif
    #endif
    #ifndef 7Seg_CommonAnode
      'Clear current display
      Set DISP_SEG_A Off
      Set DISP_SEG_B Off
      Set DISP_SEG_C Off
      Set DISP_SEG_D Off
      Set DISP_SEG_E Off
      Set DISP_SEG_F Off
      Set DISP_SEG_G Off
      #ifdef DISP_SEG_DOT
        Set DISP_SEG_DOT Off
      #endif
    #endif

    'Select new display
    'Commands to select
    #ifdef DispSelectA
    If DispPort = 1 Then DispSelectA
    #endif
    #ifdef DispSelectB
    If DispPort = 2 Then DispSelectB
    #endif
    #ifdef DispSelectC
    If DispPort = 3 Then DispSelectC
    #endif
    #ifdef DispSelectD
    If DispPort = 4 Then DispSelectD
    #endif

    'Select new display
    'Commands to Deselect
    #ifdef DispDeSelectA
    If DispPort = 1 Then DispDeSelectA
    #endif
    #ifdef DispDeSelectB
    If DispPort = 2 Then DispDeSelectB
    #endif
    #ifdef DispDeSelectC
    If DispPort = 3 Then DispDeSelectC
    #endif
    #ifdef DispDeSelectD
    If DispPort = 4 Then DispDeSelectD
    #endif

    #ifdef 7Seg_HighSide  'pnp/pfet highside driver
      'Pins to select
      'Turn all off
      #ifdef DISP_SEL_1
        Set DISP_SEL_1 On
      #endif
      #ifdef DISP_SEL_2
        Set DISP_SEL_2 On
      #endif
      #ifdef DISP_SEL_3
        Set DISP_SEL_3 On
      #endif
      #ifdef DISP_SEL_4
        Set DISP_SEL_4 On
      #endif
      'Turn appropriate pin back on
      #ifdef DISP_SEL_1
        If DispPort = 1 Then Set DISP_SEL_1 Off
      #endif
      #ifdef DISP_SEL_2
        If DispPort = 2 Then Set DISP_SEL_2 Off
      #endif
      #ifdef DISP_SEL_3
        If DispPort = 3 Then Set DISP_SEL_3 Off
      #endif
      #ifdef DISP_SEL_4
        If DispPort = 4 Then Set DISP_SEL_4 Off
      #endif
    #endif

    #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
      'Pins to select
      'Turn all off
      #ifdef DISP_SEL_1
        Set DISP_SEL_1 Off
      #endif
      #ifdef DISP_SEL_2
        Set DISP_SEL_2 Off
      #endif
      #ifdef DISP_SEL_3
        Set DISP_SEL_3 Off
      #endif
      #ifdef DISP_SEL_4
        Set DISP_SEL_4 Off
      #endif
      'Turn appropriate pin back on
      #ifdef DISP_SEL_1
        If DispPort = 1 Then Set DISP_SEL_1 On
      #endif
      #ifdef DISP_SEL_2
        If DispPort = 2 Then Set DISP_SEL_2 On
      #endif
      #ifdef DISP_SEL_3
        If DispPort = 3 Then Set DISP_SEL_3 On
      #endif
      #ifdef DISP_SEL_4
        If DispPort = 4 Then Set DISP_SEL_4 On
      #endif
    #endif
    #ifdef 7Seg_CommonAnode
    'If CommonAnode = 255 Then
      'Show number on new display
      If DispTemp.0 = On Then DISP_SEG_A = Off
      If DispTemp.1 = On Then DISP_SEG_B = Off
      If DispTemp.2 = On Then DISP_SEG_C = Off
      If DispTemp.3 = On Then DISP_SEG_D = Off
      If DispTemp.4 = On Then DISP_SEG_E = Off
      If DispTemp.5 = On Then DISP_SEG_F = Off
      If DispTemp.6 = On Then DISP_SEG_G = Off
      #ifdef DISP_SEG_DOT
        If DispTemp.7 = On Then DISP_SEG_DOT = Off
      #endif
    #endif
    #ifndef 7Seg_CommonAnode
      'Show number on new display
      If DispTemp.0 = On Then DISP_SEG_A = On
      If DispTemp.1 = On Then DISP_SEG_B = On
      If DispTemp.2 = On Then DISP_SEG_C = On
      If DispTemp.3 = On Then DISP_SEG_D = On
      If DispTemp.4 = On Then DISP_SEG_E = On
      If DispTemp.5 = On Then DISP_SEG_F = On
      If DispTemp.6 = On Then DISP_SEG_G = On
      #ifdef DISP_SEG_DOT
        If DispTemp.7 = On Then DISP_SEG_DOT = On
      #endif
    #endif

    #ifdef DispDeSelectA
    If DispPort = 1 Then DispDeSelectA
    #endif
    #ifdef DispDeSelectB
    If DispPort = 2 Then DispDeSelectB
    #endif
    #ifdef DispDeSelectC
    If DispPort = 3 Then DispDeSelectC
    #endif
    #ifdef DispDeSelectD
    If DispPort = 4 Then DispDeSelectD
    #endif



  #endif
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  'Whole port code using DisplayPortx
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  #ifndef DISP_SEG_A
     'Select new display
    'Commands to Deselect
    #ifdef DispDeSelectA
     DispDeSelectA
    #endif
    #ifdef DispDeSelectB
     DispDeSelectB
    #endif
    #ifdef DispDeSelectC
     DispDeSelectC
    #endif
    #ifdef DispDeSelectD
     DispDeSelectD
    #endif
    #ifdef 7Seg_CommonAnode
      DispTemp = NOT DispTemp
    #endif
    #ifdef 7Seg_HighSide  'pnp/pfet highside driver
      'Pins to select
      'Turn all off
      #ifdef DISP_SEL_1
        Set DISP_SEL_1 Off
      #endif
      #ifdef DISP_SEL_2
        Set DISP_SEL_2 Off
      #endif
      #ifdef DISP_SEL_3
        Set DISP_SEL_3 Off
      #endif
      #ifdef DISP_SEL_4
        Set DISP_SEL_4 Off
      #endif
    #endif
    #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
      'Pins to select
      'Turn all off
      #ifdef DISP_SEL_1
        Set DISP_SEL_1 On
      #endif
      #ifdef DISP_SEL_2
        Set DISP_SEL_2 On
      #endif
      #ifdef DISP_SEL_3
        Set DISP_SEL_3 On
      #endif
      #ifdef DISP_SEL_4
        Set DISP_SEL_4 On
      #endif
    #endif
    #ifdef DisplayPortA
      #ifdef OneOf(DisplayPortB, DisplayPortC, DisplayPortD)
        If DispPort = 1 Then
      #endif
        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_1
            Set DISP_SEL_1 Off
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_1
            Set DISP_SEL_1 On
          #endif
        #endif

        DisplayPortA = DispTemp

        ' Set select on | off
        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_1
            Set DISP_SEL_1 On
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_1
            Set DISP_SEL_1 Off
          #endif
        #endif

        #ifdef DispSelectA
          DispSelectA
        #endif

      #ifdef OneOf(DisplayPortB, DisplayPortC, DisplayPortD)
        End If
      #endif
    #endif
    #ifdef DisplayPortB
      If DispPort = 2 Then

        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_2
            Set DISP_SEL_2 Off
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_2
            Set DISP_SEL_2 On
          #endif
        #endif

        DisplayPortB = DispTemp

        ' Set select on | off
        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_2
            Set DISP_SEL_2 On
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_2
            Set DISP_SEL_2 Off
          #endif
        #endif

        #ifdef DispSelectB
          DispSelectB
        #endif

      End If
    #endif
    #ifdef DisplayPortC
      If DispPort = 3 Then

        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_3
            Set DISP_SEL_3 Off
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_3
            Set DISP_SEL_3 On
          #endif
        #endif

        DisplayPortC = DispTemp

        ' Set select on | off
        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_3
            Set DISP_SEL_3 On
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_3
            Set DISP_SEL_3 Off
          #endif
        #endif

        #ifdef DispSelectC
          DispSelectC
        #endif

      End If
    #endif
    #ifdef DisplayPortD
      If DispPort = 4 Then

        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_4
            Set DISP_SEL_4 Off
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_4
            Set DISP_SEL_4 On
          #endif
        #endif

        DisplayPortD = DispTemp

        ' Set select on | off
        #ifdef 7Seg_HighSide  'pnp/pfet highside driver
          #ifdef DISP_SEL_4
            Set DISP_SEL_4 On
          #endif
        #endif
        #ifndef 7Seg_HighSide 'CommonCathode or npn/nfet high side CommonAnode
          #ifdef DISP_SEL_4
            Set DISP_SEL_4 Off
          #endif
        #endif

        #ifdef DispSelectD
          DispSelectD
        #endif

      End If
    #endif
  #endif
End Macro
