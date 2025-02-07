'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2017-2025 Evan Venn

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

'Notes:
' Supports SSD1331 controller only.

'Changes
'  03/04/2019  Revised to support DEFAULT_GLCDBACKGROUND constant
'  27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn
#define SSD1331_GLCDCLS_Delay 500


' Hardware settings
' Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_SSD1331"

'Serial lines (SSD1331 only)
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1331; Data/Command; GLCD_DC; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1331; Chip Select; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1331; Data In (LCD Out) GLCD_DI; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1331; Data Out (LCD In) GLCD_DO; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1331; Clock; GLCD_SCK; IO_Pin

'''@hardware GLCD_TYPE GLCD_TYPE_SSD1331; Reset; GLCD_RESET; IO_Pin

'Pin mappings for SSD1331
#define SSD1331_DC GLCD_DC
#define SSD1331_CS GLCD_CS
#define SSD1331_RST GLCD_RESET

#define SSD1331_DI GLCD_DI ;DI IS NOT USED within the library - presence for completeness
#define SSD1331_DO GLCD_DO
#define SSD1331_SCK GLCD_SCK



' Screen Settings
#define SSD1331_Set_Column_Address  0x15
#define SSD1331_Set_Row_Address     0x75
#define SSD1331_contrastA           0x81
#define SSD1331_contrastB           0x82
#define SSD1331_contrastC           0x83
#define SSD1331_display_on          0xAF
#define SSD1331_display_off         0xAE
#define SSD1331_MasterCurrent       0x87
#define SSD1331_CONTRASTA           0x81
#define SSD1331_CONTRASTB           0x82
#define SSD1331_CONTRASTC           0x83
#define SSD1331_POWERMODE           0xB0


'GAC hardware acceleration commands
#define SSD1331_GAC_DRAW_LINE           0x21    ' Draw Line
#define SSD1331_GAC_DRAW_RECTANGLE      0x22    ' Rectangle
#define SSD1331_GAC_COPY_AREA           0x23    ' Copy Area
#define SSD1331_GAC_DIM_WINDOW          0x24    ' Dim Window
#define SSD1331_GAC_CLEAR_WINDOW        0x25    ' Clear Window
#define SSD1331_GAC_FILL_ENABLE_DISABLE 0x26    ' Enable Fill
#define SSD1331_SCROLL_SETUP            0x27    ' Setup scroll
#define SSD1331_SCROLL_STOP             0x2E    ' Scroll Stop
#define SSD1331_SCROLL_START            0x2F    ' Scroll Start

'Basic RGB color definitions         RED GREEN BLUE values

'#define Black           0x0000      '   0,   0,   0
'#define LightGrey       0xC618      ' 192, 192, 192
'#define DarkGrey        0x7BEF      ' 128, 128, 128
'#define Red             0xF800      ' 255,   0,   0
'#define Green           0x07E0      '   0, 255,   0
'#define Cyan            0x07FF      '   0, 255, 255
'#define Blue            0x001F      '   0,   0, 255
'#define Magenta         0xF81F      ' 255,   0, 255
'#define Yellow          0xFFE0      ' 255, 255,   0
'#define White           0xFFFF      ' 255, 255, 255
'

' Color definitions
#define SSD1331_BLACK   0x0000
#define SSD1331_RED     0xF800
#define SSD1331_GREEN   0x07E0
#define SSD1331_BLUE    0x001F
#define SSD1331_WHITE   0xFFFF
#define SSD1331_PURPLE  0xF11F
#define SSD1331_YELLOW  0xFFE0
#define SSD1331_CYAN    0x07FF
#define SSD1331_D_GRAY  0x528A
#define SSD1331_L_GRAY  0x7997
#define SSD1331_SILVER  0xC618
#define SSD1331_MAROON  0x8000
#define SSD1331_OLIVE   0x8400
#define SSD1331_LIME    0x07E0
#define SSD1331_AQUA    0x07FF
#define SSD1331_TEAL    0x0410
#define SSD1331_NAVY    0x0010
#define SSD1331_FUCHSIA 0xF81F


#startup InitGLCD_SSD1331

dim SSD1331_Command(12)
'Foreground and background colours
Dim GLCDBackground As Word
Dim GLCDForeground As Word


'''Initialise the GLCD device
Sub InitGLCD_SSD1331

  #if GLCD_TYPE = GLCD_TYPE_SSD1331

    '  Mapped to global variables to same RAM
    ' dim SSD1331_GLCD_HEIGHT, SSD1331_GLCD_WIDTH as word


    'Setup code for SSD1331 controllers


    'Pin directions
    Dir SSD1331_CS Out
    Dir SSD1331_DC Out
    Dir SSD1331_RST Out

    #if bit(SSD1331_DI)
      Dir SSD1331_DI In
    #endif
    Dir SSD1331_DO Out
    Dir SSD1331_SCK Out

    #ifdef SSD1331_HardwareSPI
          ' harware SPI mode
          asm showdebug SPI constant used equates to HWSPIMODESCRIPT, HWSPIClockModeSCRIPT
          SPIMode HWSPIMODESCRIPT, HWSPIClockModeSCRIPT
    #endif

   Set SSD1331_CS On
   Set SSD1331_DC Off


    'Reset display
    Wait 200 ms
    Set SSD1331_RST Off
    Wait 200 us
    Set SSD1331_RST On

    ' initialize sequence
    SendCommand_SSD1331(0xAE)    'OLED display OFF
    SendCommand_SSD1331(0xAE)    'OLED display OFF
    SendCommand_SSD1331(0xAE)    'OLED display OFF
    SendCommand_SSD1331(0x75)    ' Set Row Address */
    SendCommand_SSD1331(0x00)    ' Start = 0 */
    SendCommand_SSD1331(0x3F)    ' End = 63 */
    SendCommand_SSD1331(0x15)    ' Set Column Address */
    SendCommand_SSD1331(0x00)    ' Start = 0 */
    SendCommand_SSD1331(0x5F)    ' End = 95 */
    SendCommand_SSD1331(0xA0)    'Set remap & data format 0111 0000
    SendCommand_SSD1331(0x72)    ' RGB colour
    SendCommand_SSD1331(0xA1)    'set display start row RAM
    SendCommand_SSD1331(0x00)
    SendCommand_SSD1331(0xA2)    'set display offset
    SendCommand_SSD1331(0x00)
    SendCommand_SSD1331(0xA4)    'Set Display Mode
    SendCommand_SSD1331(0xA8)    'Set Multiplex Ratio
    SendCommand_SSD1331(0x3F)
    SendCommand_SSD1331(0xAD)    'Set Master Configuration
    SendCommand_SSD1331(0x8F)    '(External VCC Supply Selected)
    SendCommand_SSD1331(0xB0)    'Set Power Saving Mode
    SendCommand_SSD1331(0x1A)
    SendCommand_SSD1331(0xB1)    'Set Phase 1 & 2 Period Adjustment
    SendCommand_SSD1331(0x74)
    SendCommand_SSD1331(0xB3)    'Set Display Clock Divide Ratio / Oscillator Frequency
    SendCommand_SSD1331(0xD0)
    SendCommand_SSD1331(0x8A)    'Set Second Pre-charge Speed of Color A
    SendCommand_SSD1331(0x81)
    SendCommand_SSD1331(0x8B)    'Set Second Pre-charge Speed of Color B
    SendCommand_SSD1331(0x82)
    SendCommand_SSD1331(0x8C)    'Set Second Pre-charge Speed of Color C
    SendCommand_SSD1331(0x83)
    SendCommand_SSD1331(0xBB)    'Set Pre-charge Level
    SendCommand_SSD1331(0x3E)
    SendCommand_SSD1331(0xBE)    'Set VCOMH
    SendCommand_SSD1331(0x3E)
    SendCommand_SSD1331(0x87)    'Set Master Current Control
    SendCommand_SSD1331(0x0F)
    SendCommand_SSD1331(0x81)    'Set Contrast Control for Color &#129;gA&#129;h
    SendCommand_SSD1331(0x80)
    SendCommand_SSD1331(0x82)    'Set Contrast Control for Color &#129;gB&#129;h
    SendCommand_SSD1331(0x80)
    SendCommand_SSD1331(0x83)    'Set Contrast Control for Color &#129;gC&#129;h
    SendCommand_SSD1331(0x80)
    SendCommand_SSD1331(0xAF)    'display ON

    'Default Colours
    GLCDForeground = SSD1331_WHITE
    #ifdef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
    #endif

    #ifndef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = SSD1331_BLACK
    #endif

    'Variables required for device
    SSD1331_GLCD_WIDTH = GLCD_WIDTH
    SSD1331_GLCD_HEIGHT = GLCD_HEIGHT

    #ifndef GLCD_OLED_FONT
      GLCDFontWidth = 6
    #endif

    #ifdef GLCD_OLED_FONT
      GLCDFontWidth = 5
    #endif

    GLCDfntDefault = 0
    GLCDfntDefaultsize = 1
    GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn

  #endif

  'Prepare screen - this sets SSD1331Rotation
  GLCDRotate_SSD1331 ( LANDSCAPE )
  GLCDCLS

End Sub



'''Clears the GLCD screen
Sub GLCDCLS_SSD1331 ( Optional In  GLCDBackground as word = GLCDBackground )
    dim PrintLocX, PrintLocY as word
    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0
    PrintLocX = 0
    PrintLocY = 0

    SSD1331_Command(0) = SSD1331_GAC_CLEAR_WINDOW
    SSD1331_Command(1) = 0
    SSD1331_Command(2) = 0
    SSD1331_Command(3) = SSD1331_GLCD_WIDTH-1
    SSD1331_Command(4) = SSD1331_GLCD_HEIGHT-1
    SendMultipleCommand_SSD1331( 5 )
    wait SSD1331_GLCDCLS_Delay us

    SSD1331_Command(0) = SSD1331_GAC_FILL_ENABLE_DISABLE
    SSD1331_Command(1) = 1     'fill 1, empty 0
    SendMultipleCommand_SSD1331( 2 )

    SSD1331_Command(0) = SSD1331_GAC_DRAW_RECTANGLE
    SSD1331_Command(1) = 0
    SSD1331_Command(2) = 0
    if SSD1331Rotation.0 = 1 then
        SSD1331_Command(3) = SSD1331_GLCD_WIDTH-1
        SSD1331_Command(4) = SSD1331_GLCD_HEIGHT-1
    Else
        SSD1331_Command(4) = SSD1331_GLCD_WIDTH-1
        SSD1331_Command(3) = SSD1331_GLCD_HEIGHT-1

    end if
    SSD1331_Command(5) = (GLCDBackground / 0x800 ) * 2    '((GLCDBackground>> 11) << 1)       ' Outline Blue
    SSD1331_Command(6) = (GLCDBackground / 0x20 ) & 0x3f  '((GLCDBackground>> 5 ) & 0x3F)     ' Outline Green
    SSD1331_Command(7) = (GLCDBackground * 2 ) & 0x3f     '((GLCDBackground<< 1 ) & 0x3F)     ' Outline Red

    SSD1331_Command(8) = (GLCDBackground / 0x800 ) * 2    '((GLCDBackground>> 11) << 1)       ' fill Blue
    SSD1331_Command(9) = (GLCDBackground / 0x20 ) & 0x3f  '((GLCDBackground>> 5 ) & 0x3F)     ' fill Green
    SSD1331_Command(10)= (GLCDBackground * 2 ) & 0x3f     '((GLCDBackground<< 1 ) & 0x3F)     ' fill Red
    SendMultipleCommand_SSD1331( 11 )
    wait SSD1331_GLCDCLS_Delay us


End Sub

'''Draws a string at the specified location on the GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_SSD1331( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as word = GLCDForeground )

  dim GLCDPrintLoc as word

  GLCDPrintLoc = StringLocX

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  for xchar = 1 to Chars(0)

      GLCDDrawChar GLCDPrintLoc , CharLocY , Chars(xchar), LineColour
      GLCDPrintIncrementPixelPositionMacro

  next

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif


end sub

'''Draws a character at the specified location on the GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_SSD1331(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This has got a tad complex
  'We have three major pieces
  '1 The preamble - this just adjusted color and the input character
  '2 The code that deals with GCB fontset
  '3 The code that deals with OLED fontset
  '
  'You can make independent change to section 2 and 3 but they are mutual exclusive with many common pieces

   dim CharCol, CharRow, GLCDTemp as word
   CharCode -= 15

   CharCol=0

   #ifndef GLCD_OLED_FONT

        if CharCode>=178 and CharCode<=202 then
           CharLocY=CharLocY-1
        end if

        For CurrCharCol = 1 to 5
          Select Case CurrCharCol
            Case 1: ReadTable GLCDCharCol3, CharCode, CurrCharVal
            Case 2: ReadTable GLCDCharCol4, CharCode, CurrCharVal
            Case 3: ReadTable GLCDCharCol5, CharCode, CurrCharVal
            Case 4: ReadTable GLCDCharCol6, CharCode, CurrCharVal
            Case 5: ReadTable GLCDCharCol7, CharCode, CurrCharVal
          End Select
          CharRow=0
          For CurrCharRow = 1 to 8
              CharColS=0
              For Col=1 to GLCDfntDefaultsize
                    CharColS +=1
                    CharRowS=0
                    For Row=1 to GLCDfntDefaultsize
                        CharRowS +=1
                        if CurrCharVal.0=1 then
                           PSet [word]CharLocX + CharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, LineColour
                        Else
                           PSet [word]CharLocX + CharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, GLCDBackground
                        End if
                    Next Row
              Next Col
            Rotate CurrCharVal Right
            CharRow +=GLCDfntDefaultsize
          Next
          CharCol +=GLCDfntDefaultsize
        Next

    #endif

    #ifdef GLCD_OLED_FONT

        'Calculate the pointer to the OLED fonts.
        'These fonts are not multiple tables one is a straight list the other is a lookup table with data.
        Dim LocalCharCode as word

        'Get key information and set up the fonts parameters
        Select case GLCDfntDefaultSize
            case 1 'This font is two font tables of an index and data
              CharCode = CharCode - 16
              ReadTable OLEDFont1Index, CharCode, LocalCharCode
              ReadTable OLEDFont1Data, LocalCharCode , COLSperfont
              'If the char is the ASC(32) a SPACE set the fontwidth =1 (not 2)
              if LocalCharCode = 1 then
                  GLCDFontWidth = 1
              else
                  GLCDFontWidth = COLSperfont+1
              end if
              ROWSperfont = 7  'which is really 8 as we start at 0

            case 2 'This is one font table
              CharCode = CharCode - 17
              'Pointer to table of font elements
              LocalCharCode = (CharCode * 20)
              COLSperfont = 9  'which is really 10 as we start at 0

              ROWSperfont=15  'which is really 16 as we start at 0

        End Select


        'The main loop - loop throught the number of columns
        For CurrCharCol = 0 to COLSperfont  'number of columns in the font , with two row of data

          'Index the pointer to the code that we are looking for as we need to do this lookup many times getting more font data
          LocalCharCode++
          Select case GLCDfntDefaultSize
              case 1
                ReadTable OLEDFont1Data, LocalCharCode, CurrCharVal

              case 2
                #ifndef GLCD_Disable_OLED_FONT2
                  'Read this 20 times... (0..COLSperfont) [ * 2 ]
                  ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                #endif
                #ifdef GLCD_Disable_OLED_FONT2
                  CurrCharVal = GLCDBackground
                #endif
          End Select

            'we handle 8 or 16 pixels of height
            For CurrCharRow = 0 to ROWSperfont
                'Set the pixel
                If CurrCharVal.0 = 0 Then
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
                Else
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, LineColour
                End If

                Rotate CurrCharVal Right

                'Set to next row of date, a second row
                if GLCDfntDefaultSize = 2 and CurrCharRow = 7 then
                  LocalCharCode++
                  #ifndef GLCD_Disable_OLED_FONT2
                    ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                  #endif
                  #ifdef GLCD_Disable_OLED_FONT2
                    CurrCharVal = GLCDBackground
                  #endif
                end if

                'It is the intercharacter space, put out one pixel row
                if CurrCharCol = COLSperfont then
                    'Put out a white intercharacter pixel/space
                     GLCDTemp = CharLocX + CurrCharCol
                     if GLCDfntDefaultSize = 2 then
                        GLCDTemp++
                     end if
                     PSet GLCDTemp , CharLocY + CurrCharRow, GLCDBackground
                end if

            Next



        Next


    #endif

End Sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_SSD1331(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)

  dim GLCDTemp, LineColour as word
  'Make sure that starting point (1) is always less than end point (2)
  If LineX1 > LineX2 Then
    GLCDTemp = LineX1
    LineX1 = LineX2
    LineX2 = GLCDTemp
  End If
  If LineY1 > LineY2 Then
    GLCDTemp = LineY1
    LineY1 = LineY2
    LineY2 = GLCDTemp
  End If

  'Draw lines going across
  For DrawLine = [byte]LineX1 To [byte]LineX2
    For GLCDTemp = LineY1 To LineY2
      PSet DrawLine, GLCDTemp, [word]LineColour
    Next
  Next

End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
Sub PSet_SSD1331(In GLCDX as word, In GLCDY as word, In GLCDColour As Word)

    dim GLCDTemp as word
    'swap values to support rotation as this device DOES NOT SUPPORT Portrait! arcghhhh
    if SSD1331Rotation.0 = 0 then
        GLCDTemp = GLCDX
        GLCDX = GLCDY
        GLCDY = GLCDTemp
        GLCDY = SSD1331_GLCD_WIDTH - GLCDY - 1
    End if

    #IFDEF GLCD_PROTECTOVERRUN
        'anything off screen with be rejected
        if GLCDX => SSD1331_GLCD_WIDTH OR GLCDY => SSD1331_GLCD_HEIGHT Then
            exit sub
        end if

    #ENDIF

    SendCommand_SSD1331(SSD1331_Set_Column_Address) ' Column addr set
    SendCommand_SSD1331 GLCDX
    SendCommand_SSD1331 GLCDX

    SendCommand_SSD1331(SSD1331_Set_Row_Address) ' Row addr set
    SendCommand_SSD1331 GLCDY
    SendCommand_SSD1331 GLCDY

    SendData_SSD1331( GLCDColour_H )
    SendData_SSD1331( GLCDColour )

End Sub


sub  SendMultipleCommand_SSD1331( in SSD1331BytestoSend as byte )

  Dim SSD1331BytestoSendCounter, SSD1331SendByte as byte

  set SSD1331_CS OFF;
  set SSD1331_DC OFF;

  #ifdef SSD1331_HardwareSPI
    for SSD1331BytestoSendCounter = 0 to SSD1331BytestoSend - 1
         FastHWSPITransfer  SSD1331_Command( SSD1331BytestoSendCounter )
         wait 10 us
    next
    set SSD1331_CS ON
    exit sub
  #endif

  #ifndef SSD1331_HardwareSPI
    for SSD1331BytestoSendCounter = 0 to SSD1331BytestoSend - 1
      SSD1331SendByte = SSD1331_Command( SSD1331BytestoSendCounter )
      repeat 8

        if SSD1331SendByte.7 = ON  then
          set SSD1331_DO ON;
        else
          set SSD1331_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate SSD1331SendByte left
        set GLCD_SCK Off;

      end repeat
    next
    set SSD1331_CS ON
  #endif


end sub

'''Send a command to the SSD1331 GLCD
'''@param SSD1331SendByte Command to send
'''@hide
sub  SendCommand_SSD1331( IN SSD1331SendByte as byte )

  set SSD1331_CS OFF;
  set SSD1331_DC OFF;

  #ifdef SSD1331_HardwareSPI
     FastHWSPITransfer  SSD1331SendByte
     set SSD1331_CS ON;
     exit sub
  #endif

  #ifndef SSD1331_HardwareSPI
  repeat 8

    if SSD1331SendByte.7 = ON  then
      set SSD1331_DO ON;
    else
      set SSD1331_DO OFF;
    end if
    SET GLCD_SCK On;
    rotate SSD1331SendByte left
    set GLCD_SCK Off;

  end repeat
  set SSD1331_CS ON;
  #endif

end Sub




sub  SendMultipleData_SSD1331( in SSD1331BytestoSend as byte )

  Dim SSD1331BytestoSendCounter, SSD1331SendByte as byte
'  Dim SSD1331_Command(12)

  set SSD1331_CS OFF;
  set SSD1331_DC On;

  #ifdef SSD1331_HardwareSPI
    for SSD1331BytestoSendCounter = 0 to SSD1331BytestoSend - 1
         FastHWSPITransfer  SSD1331_Command( SSD1331BytestoSendCounter )
    next
    set SSD1331_CS ON
    exit sub
  #endif

  #ifndef SSD1331_HardwareSPI
    for SSD1331BytestoSendCounter = 0 to SSD1331BytestoSend - 1
      SSD1331SendByte = SSD1331_Command( SSD1331BytestoSendCounter )
      repeat 8

        if SSD1331SendByte.7 = ON  then
          set SSD1331_DO ON;
        else
          set SSD1331_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate SSD1331SendByte left
        set GLCD_SCK Off;

      end repeat
    next
    set SSD1331_CS ON
  #endif


end sub



'''Send a data byte to the SSD1331 GLCD
'''@param SSD1331SendByte Byte to send
'''@hide
sub  SendData_SSD1331( IN SSD1331SendByte as byte )

  set SSD1331_CS OFF;
  set SSD1331_DC ON;

  #ifdef SSD1331_HardwareSPI
     FastHWSPITransfer  SSD1331SendByte
     set SSD1331_CS ON;
     exit sub
  #endif

  #ifndef SSD1331_HardwareSPI
  repeat 8

    if SSD1331SendByte.7 = ON then
      set SSD1331_DO ON;
    else
      set SSD1331_DO OFF;
    end if
    SET GLCD_SCK On;
    rotate SSD1331SendByte left
    set GLCD_SCK Off;

  end Repeat
  set SSD1331_CS ON;
  #endif

end Sub



'''@hide
sub   GLCDRotate_SSD1331 ( in SSD1331Rotation )

  SendCommand_SSD1331(SSD1331_Set_Column_Address) ' Column addr set
  SendCommand_SSD1331 0
  SendCommand_SSD1331 SSD1331_GLCD_WIDTH-1

  SendCommand_SSD1331(SSD1331_Set_Row_Address) ' Row addr set
  SendCommand_SSD1331 0
  SendCommand_SSD1331 SSD1331_GLCD_HEIGHT-1

  repeat 4608
    SendData_SSD1331( GLCDBackground_H )
    SendData_SSD1331( GLCDBackground )
  end Repeat

  select case SSD1331Rotation
        case PORTRAIT
             SSD1331_GLCD_WIDTH = GLCD_HEIGHT
             SSD1331_GLCD_HEIGHT = GLCD_WIDTH

              'Not requrired
'             SSD1331_Command(0) = SSD1331_GAC_CLEAR_WINDOW
'             SSD1331_Command(1) = 0
'             SSD1331_Command(2) = 0
'             SSD1331_Command(3) = SSD1331_GLCD_WIDTH-1
'             SSD1331_Command(4) = SSD1331_GLCD_HEIGHT-1
'             SendMultipleCommand_SSD1331( 5 )

             SendCommand_SSD1331(0xA0)
             SendCommand_SSD1331(0x60)

        case LANDSCAPE
             SSD1331_GLCD_WIDTH = GLCD_WIDTH
             SSD1331_GLCD_HEIGHT = GLCD_HEIGHT

              'Not requrired
'             SSD1331_Command(0) = SSD1331_GAC_CLEAR_WINDOW
'             SSD1331_Command(1) = 0
'             SSD1331_Command(2) = 0
'             SSD1331_Command(3) = SSD1331_GLCD_WIDTH-1
'             SSD1331_Command(4) = SSD1331_GLCD_HEIGHT-1
'             SendMultipleCommand_SSD1331( 5 )

             SendCommand_SSD1331(0xA0)    'Set remap & data format 0111 0000
             SendCommand_SSD1331(0x72)    ' RGB colour

        case PORTRAIT_REV
             SSD1331_GLCD_WIDTH = GLCD_HEIGHT
             SSD1331_GLCD_HEIGHT = GLCD_WIDTH
              'Not requrired
'             SSD1331_Command(0) = SSD1331_GAC_CLEAR_WINDOW
'             SSD1331_Command(1) = 0
'             SSD1331_Command(2) = 0
'             SSD1331_Command(3) = SSD1331_GLCD_WIDTH-1
'             SSD1331_Command(4) = SSD1331_GLCD_HEIGHT-1
'             SendMultipleCommand_SSD1331( 5 )

             SendCommand_SSD1331(0xA0)
             SendCommand_SSD1331(0x72)

        case LANDSCAPE_REV
             SSD1331_GLCD_WIDTH = GLCD_WIDTH
             SSD1331_GLCD_HEIGHT = GLCD_HEIGHT
              'Not requrired
'             SSD1331_Command(0) = SSD1331_GAC_CLEAR_WINDOW
'             SSD1331_Command(1) = 0
'             SSD1331_Command(2) = 0
'             SSD1331_Command(3) = SSD1331_GLCD_WIDTH-1
'             SSD1331_Command(4) = SSD1331_GLCD_HEIGHT-1
'             SendMultipleCommand_SSD1331( 5 )

             SendCommand_SSD1331(0xA0)
             SendCommand_SSD1331(0x60)

        case else
             SSD1331_GLCD_WIDTH = GLCD_WIDTH
             SSD1331_GLCD_HEIGHT = GLCD_HEIGHT
              'Not requrired
'             SSD1331_Command(0) = SSD1331_GAC_CLEAR_WINDOW
'             SSD1331_Command(1) = 0
'             SSD1331_Command(2) = 0
'             SSD1331_Command(3) = SSD1331_GLCD_WIDTH-1
'             SSD1331_Command(4) = SSD1331_GLCD_HEIGHT-1
'             SendMultipleCommand_SSD1331( 5 )

             SendCommand_SSD1331(0xA0)    'Set remap & data format 0111 0000
             SendCommand_SSD1331(0x72)    ' RGB colour
  end select




end sub



'''Displays a string in a larger fonti.e.  BigPrint_SSD1331 ( 30, SSD1331_GLCD_HEIGHT - 20 , "BigPrint_SSD1331" )
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData String to display
'''@param Color Optional color
Sub BigPrint_SSD1331(In PrintLocX as Word, In PrintLocY as Word,  PrintData As String, Optional In  Color as word = GLCDForeground)
  Dim GLCDPrintLoc as word
  PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = 1 To PrintLen
    DrawBigChar_SSD1331 GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  Next
End Sub

'''Draws a Vertical Line on the GLCD with a
'''@param GLCDY1 Y coordinate of one end of the line
'''@param GLCDY2 Y coordinate of the other end of the line
'''@param GLCDX1 X coordinate of the line
'''@param LineColour color
Sub DrawBigChar_SSD1331 (In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  Color as word = GLCDForeground )

    dim Locx, Locy, CurrCol as Word


    if CharCode <=126 Then
       CharCode -=32
       Goto GCBBigTables
    end if
    if CharCode <=210 Then
       CharCode -=33
       Goto GCBBigTables
    end if
    if CharCode <= 250 Then
       CharCode -=34
       Goto GCBBigTables
    end if
    GCBBigTables:
    For CurrCharCol = 1 to 24
      CurrCol=CurrCharCol+CharCode*24-(CharCode/10)*240
      if CharCode>=0 and CharCode<=9 then ReadTable BigFont32_41 , CurrCol, CurrCharVal
      if CharCode>=10 and CharCode<=19 then ReadTable BigFont42_51 , CurrCol, CurrCharVal
      if CharCode>=20 and CharCode<=29 then ReadTable BigFont52_61 , CurrCol, CurrCharVal
      if CharCode>=30 and CharCode<=39 then ReadTable BigFont62_71 , CurrCol, CurrCharVal
      if CharCode>=40 and CharCode<=49 then ReadTable BigFont72_81 , CurrCol, CurrCharVal
      if CharCode>=50 and CharCode<=59 then ReadTable BigFont82_91 , CurrCol, CurrCharVal
      if CharCode>=60 and CharCode<=69 then ReadTable BigFont92_101 , CurrCol, CurrCharVal
      if CharCode>=70 and CharCode<=79 then ReadTable BigFont102_111 , CurrCol, CurrCharVal
      if CharCode>=80 and CharCode<=89 then ReadTable BigFont112_121 , CurrCol, CurrCharVal
      if CharCode>=90 and CharCode<=99 then ReadTable BigFont122_126 , CurrCol, CurrCharVal
      if CharCode>=160 and CharCode<=169 then ReadTable BigFont193_202 , CurrCol, CurrCharVal
      if CharCode>=170 and CharCode<=179 then ReadTable BigFont203_212 , CurrCol, CurrCharVal
      if CharCode>=180 and CharCode<=183 then ReadTable BigFont213_216 , CurrCol, CurrCharVal

        if CurrCharVal=36 then CurrCharVal=33
        For CurrCharRow = 1 to 8
          LocX=[word]CharLocX+CurrCharCol
          LocY=[Word]CharLocY+CurrCharRow
          if CurrCharCol>12 then
             LocX= LocX - 12
             LocY= LocY + 8
          end if
          if CurrCharVal.0=1 then
             PSet_SSD1331 LocX , LocY , Color
          end if
          Rotate CurrCharVal Right
        Next
    Next
End Sub

'''Sets the constrast to select 0 out of 255 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
sub SetContrast_SSD1331 ( in dim_state )

  dim_state = dim_state / 16
  SendCommand_SSD1331(SSD1331_CONTRASTA)
  SendCommand_SSD1331(dim_state)
  SendCommand_SSD1331(SSD1331_CONTRASTB)
  SendCommand_SSD1331(dim_state)
  SendCommand_SSD1331(SSD1331_CONTRASTC)
  SendCommand_SSD1331(dim_state)


end sub


'''Sets the value to select 0 out of 255 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
sub SetBrightness_SSD1331 ( in dim_state )

  dim_state = dim_state / 16
  SendCommand_SSD1331(SSD1331_MasterCurrent)
  SendCommand_SSD1331(dim_state)

end sub


'''Sets the value to select 0 out of 255 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
sub SetPowerMode_SSD1331 ( in dim_state )

  dim_state = dim_state / 16
  SendCommand_SSD1331(SSD1331_POWERMODE)
  SendCommand_SSD1331(dim_state)

end sub

Sub SetScroll_SSD1331

end sub

Sub SetScrollParameters_SSD1331

end sub

Sub CutPixels_SSD1331

end sub

Sub PastePixels_SSD1331

end sub

Sub MovePixels_SSD1331

end sub
