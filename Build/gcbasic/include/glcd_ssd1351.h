'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2018-2024 Joseph Realmuto

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
' Supports SSD1351 controller only.

'Changes
'     13/10/2019   Initial code release with code review comments.



'erv what is this?
#define SSD1351_GLCDCLS_Delay 500


' Hardware settings
' Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_SSD1351"

'Serial lines (SSD1351 only)
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1351; Data/Command; GLCD_DC; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1351; Chip Select; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1351; Data In (LCD Out) GLCD_DI; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1351; Data Out (LCD In) GLCD_DO; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1351; Clock; GLCD_SCK; IO_Pin

'''@hardware GLCD_TYPE GLCD_TYPE_SSD1351; Reset; GLCD_RESET; IO_Pin

'Pin mappings for SSD1351
#define SSD1351_DC GLCD_DC
#define SSD1351_CS GLCD_CS
#define SSD1351_RST GLCD_RESET

#define SSD1351_DI GLCD_DI ;DI IS NOT USED within the library - presence for completeness
#define SSD1351_DO GLCD_DO
#define SSD1351_SCK GLCD_SCK


' Screen Settings
#define SSD1351_Set_Column_Address      0x15
#define SSD1351_Set_Row_Address         0x75
#define SSD1351_Write_RAM               0x5C
#define SSD1351_Read_RAM                0x5D
#define SSD1351_Set_Display_Start_Line  0xA1
#define SSD1351_Set_Display_Offset      0xA2
#define SSD1351_Set_Display_All_Off     0xA4
#define SSD1351_Set_Display_All_On      0xA5
#define SSD1351_Reset_To_Normal_Display 0xA6
#define SSD1351_Inverse_Display         0xA7
#define SSD1351_NOP                     0xAD
#define SSD1351_Display_Off             0xAE
#define SSD1351_Display_On              0xAF
#define SSD1351_Set_Reset_Precharge     0xB1
#define SSD1351_Display_Enhancement     0xB2
#define SSD1351_Clock_Divider_Osc_Freq  0xB3
#define SSD1351_Set_Segment_Low_Voltage 0xB4
#define SSD1351_Set_Second_Precharge    0xB6
#define SSD1351_Set_Precharge_Voltage   0xBB
#define SSD1351_Set_V_COMH_Voltage      0xBE
#define SSD1351_Set_Contrast_A_B_C      0xC1
#define SSD1351_Master_Contrast_Current 0xC7
#define SSD1351_Set_Mux_Ratio           0xCA

'GAC hardware acceleration commands
#define SSD1351_SCROLL_SETUP            0x96    ' Setup scroll
#define SSD1351_SCROLL_STOP             0x9E    ' Scroll Stop
#define SSD1351_SCROLL_START            0x9F    ' Scroll Start

'erv remove?
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
#define SSD1351_BLACK   0x0000
#define SSD1351_RED     0xF800
#define SSD1351_GREEN   0x07E0
#define SSD1351_L_GREEN 0x3FE7
#define SSD1351_BLUE    0x001F
#define SSD1351_L_BLUE  0x6D9F
#define SSD1351_WHITE   0xFFFF
#define SSD1351_PURPLE  0xF11F
#define SSD1351_YELLOW  0xFFE0
#define SSD1351_ORANGE  0xF481
#define SSD1351_CYAN    0x07FF
#define SSD1351_D_GRAY  0x528A
#define SSD1351_L_GRAY  0x7997
#define SSD1351_SILVER  0xC618
#define SSD1351_MAROON  0x8000
#define SSD1351_OLIVE   0x8400
#define SSD1351_LIME    0x07E0
#define SSD1351_AQUA    0x07FF
#define SSD1351_TEAL    0x0410
#define SSD1351_NAVY    0x0010
#define SSD1351_FUCHSIA 0xF81F


#startup InitGLCD_SSD1351

'erv delete?
Dim SSD1351_Command(12)

'Foreground and background colours
Dim GLCDBackground As Word
Dim GLCDForeground As Word



'''Initialise the GLCD device
Sub InitGLCD_SSD1351

  'Setup code for SSD1351 controllers
  #if GLCD_TYPE = GLCD_TYPE_SSD1351

    'Pin directions
    Dir SSD1351_CS Out
    Dir SSD1351_DC Out
    Dir SSD1351_RST Out

    #if bit(SSD1351_DI)
      Dir SSD1351_DI In
    #endif
    Dir SSD1351_DO Out
    Dir SSD1351_SCK Out

    #ifdef SSD1351_HardwareSPI
     ' harware SPI mode
     asm showdebug SPI constant used equates to HWSPIMODESCRIPT
     SPIMode HWSPIMODESCRIPT, 0
    #endif

    Set SSD1351_CS On
    Set SSD1351_DC Off

    'Reset display
    wait 255 ms
    Set SSD1351_RST Off
    wait 200 us
    Set SSD1351_RST On

    ' initialize sequence
    SendCommand_SSD1351(0xAE)    'OLED display OFF
    SendCommand_SSD1351(0xAE)    'OLED display OFF
    SendCommand_SSD1351(0xAE)    'OLED display OFF
    SendCommand_SSD1351(0xFD)    'Unlock A2, B1, B3, BB, BE, C1 commands
    SendData_SSD1351(0xB1)
    SendCommand_SSD1351(0xA2)    'Set display offset from 60h (default) to 0
    SendData_SSD1351(0x00)
    SendCommand_SSD1351(0xB2)   'Enhance display
    SendData_SSD1351(0xA4)
    SendData_SSD1351(0x00)
    SendData_SSD1351(0x00)
    SendCommand_SSD1351(0xB3)    'Set display for maximum refresh rate
    SendData_SSD1351(0xF1)
    SendCommand_SSD1351(0xB1)    'Set Phase 1 & 2 Period Adjustment
    SendData_SSD1351(0x3F)
    SendCommand_SSD1351(0xB6)    'Set second precharge period
    SendData_SSD1351(0x01)
    SendCommand_SSD1351(0xAF)    'Display ON
    SendCommand_SSD1351(0xA6)    'Normal display

    'Default Colours
    GLCDBackground = SSD1351_BLACK
    GLCDForeground = SSD1351_WHITE

    'Variables required for device
    SSD1351_GLCD_WIDTH = GLCD_WIDTH
    SSD1351_GLCD_HEIGHT = GLCD_HEIGHT

    #ifndef GLCD_OLED_FONT
      GLCDFontWidth = 6
    #endif

    #ifdef GLCD_OLED_FONT
      GLCDFontWidth = 5
    #endif

    GLCDfntDefault = 0
    GLCDfntDefaultsize = 1
    GLCDfntDefaultHeight = 7

  #endif

  'Prepare screen - this sets SSD1351Rotation
  GLCDRotate_SSD1351 ( LANDSCAPE )
  GLCDCLS

End Sub



'''Clears the GLCD screen
Sub GLCDCLS_SSD1351 ( Optional In  GLCDBackground as word = GLCDBackground )

    Dim PrintLocX, PrintLocY as byte
    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0
    PrintLocX = 0
    PrintLocY = 0

    SendCommand_SSD1351(SSD1351_Set_Column_Address) ' Column addr set
    SendData_SSD1351 0
    SendData_SSD1351 127

    SendCommand_SSD1351(SSD1351_Set_Row_Address) ' Row addr set
    SendData_SSD1351 0
    SendData_SSD1351 127

    SendCommand_SSD1351(SSD1351_Write_RAM) ' Enable MCU to write to RAM

    REPEAT 16384
     SendData_SSD1351( GLCDBackground_H )
     SendData_SSD1351( GLCDBackground )
    END REPEAT

    SendCommand_SSD1351(SSD1351_Reset_To_Normal_Display)

End Sub



'''Draws a string at the specified location on the GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_SSD1351( In StringLocX as byte, In CharLocY as byte, In Chars as string, Optional In LineColour as word = GLCDForeground )

  Dim GLCDPrintLoc as byte

  GLCDPrintLoc = StringLocX

  #ifdef GLCD_OLED_FONT
      Dim OldGLCDFontWidth as byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  FOR xchar = 1 TO Chars(0)

      GLCDDrawChar GLCDPrintLoc , CharLocY , Chars(xchar), LineColour
      GLCDPrintIncrementPixelPositionMacro

  NEXT

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif

End Sub



'''Draws a character at the specified location on the GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_SSD1351(In CharLocX as byte, In CharLocY as byte, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This has got a tad complex
  'We have three major pieces
  '1 The preamble - this just adjusted color and the input character
  '2 The code that deals with GCB fontset
  '3 The code that deals with OLED fontset
  '
  'You can make independent change to section 2 and 3 but they are mutual exclusive with many common pieces

   Dim CharCol, CharRow, GLCDTemp as byte
   CharCode -= 15

   CharCol=0

   #ifndef GLCD_OLED_FONT

        #IFDEF GLCD_PROTECTOVERRUN
        'anything off screen with be rejected
          GLCDTemp = 5*GLCDfntDefaultsize
          IF CharLocX + GLCDTemp => SSD1351_GLCD_WIDTH THEN
            Exit Sub
          END IF
          GLCDTemp = 8*GLCDfntDefaultsize
          IF CharLocY + GLCDTemp => SSD1351_GLCD_HEIGHT THEN
            Exit Sub
          END IF

        #ENDIF

        IF CharCode>=178 and CharCode<=202 THEN
           CharLocY=CharLocY-1
        END IF

        number_pixels = GLCDfntDefaultsize * GLCDfntDefaultsize
        CharCol = CharLocX

        FOR CurrCharCol = 1 TO 5
          Select Case CurrCharCol
            Case 1: ReadTable GLCDCharCol3, CharCode, CurrCharVal
            Case 2: ReadTable GLCDCharCol4, CharCode, CurrCharVal
            Case 3: ReadTable GLCDCharCol5, CharCode, CurrCharVal
            Case 4: ReadTable GLCDCharCol6, CharCode, CurrCharVal
            Case 5: ReadTable GLCDCharCol7, CharCode, CurrCharVal
          End Select

          CharRow = CharLocY

          FOR CurrCharRow = 1 TO 8

           GLCDTemp = GLCDfntDefaultsize - 1

           IF SSD1351Rotation.0 = 0 THEN
            SendCommand_SSD1351(SSD1351_Set_Column_Address)
            SendData_SSD1351(CharRow)
            SendData_SSD1351(CharRow + GLCDTemp)

            SendCommand_SSD1351(SSD1351_Set_Row_Address)
            SendData_SSD1351(CharCol)
            SendData_SSD1351(CharCol + GLCDTemp)
           ELSE
            SendCommand_SSD1351(SSD1351_Set_Column_Address)
            SendData_SSD1351(CharCol)
            SendData_SSD1351(CharCol + GLCDTemp)

            SendCommand_SSD1351(SSD1351_Set_Row_Address)
            SendData_SSD1351(CharRow)
            SendData_SSD1351(CharRow + GLCDTemp)
           END IF

           SendCommand_SSD1351(SSD1351_Write_RAM)

           REPEAT number_pixels
            IF CurrCharVal.0 = ON THEN
             SendData_SSD1351( LineColour_H )
             SendData_SSD1351( LineColour )
            ELSE
             SendData_SSD1351( GLCDBackground_H )
             SendData_SSD1351( GLCDBackground )
            END IF
           END REPEAT

           ROTATE CurrCharVal Right
           CharRow += GLCDfntDefaultsize
          NEXT
          CharCol += GLCDfntDefaultsize
        NEXT

        SendCommand_SSD1351(SSD1351_Reset_To_Normal_Display)

    #endif

    #ifdef GLCD_OLED_FONT

        'Calculate the pointer to the OLED fonts.
        'These fonts are not multiple tables one is a straight list the other is a lookup table with data.
        Dim LocalCharCode as word

        'Get key information and set up the fonts parameters
        Select Case GLCDfntDefaultSize
            Case 1 'This font is two font tables of an index and data
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

            Case 2 'This is one font table
              CharCode = CharCode - 17
              'Pointer to table of font elements
              LocalCharCode = (CharCode * 20)
              COLSperfont = 9  'which is really 10 as we start at 0

              ROWSperfont=15  'which is really 16 as we start at 0

        End Select

        #IFDEF GLCD_PROTECTOVERRUN
        'anything off screen with be rejected
          IF CharLocX + COLSperfont + 1 => SSD1351_GLCD_WIDTH THEN
            Exit Sub
          END IF
          IF CharLocY + ROWSperfont + 1 => SSD1351_GLCD_HEIGHT THEN
            Exit Sub
          END IF
        #ENDIF

        'The main loop - loop throught the number of columns

        IF SSD1351Rotation.0 = 0 THEN

         SendCommand_SSD1351(SSD1351_Set_Column_Address)
         SendData_SSD1351 CharLocY
         SendData_SSD1351 CharLocY + ROWSperfont

         SendCommand_SSD1351(SSD1351_Set_Row_Address)
         SendData_SSD1351 CharLocX
         SendData_SSD1351 CharLocX + COLSperfont

         SendCommand_SSD1351(SSD1351_Write_RAM)

        ELSE

         SendCommand_SSD1351(0xA0)    'vertical address increment
         SendData_SSD1351(0x75)

         SendCommand_SSD1351(SSD1351_Set_Column_Address)
         SendData_SSD1351 CharLocX
         SendData_SSD1351 CharLocX + COLSperfont

         SendCommand_SSD1351(SSD1351_Set_Row_Address)
         SendData_SSD1351 CharLocY
         SendData_SSD1351 CharLocY + ROWSperfont

         SendCommand_SSD1351(SSD1351_Write_RAM)

        END IF

        FOR CurrCharCol = 0 TO COLSperfont

          'Index the pointer to the code that we are looking for as we need to do this lookup many times getting more font data

          LocalCharCode++

          Select Case GLCDfntDefaultSize
              Case 1
                ReadTable OLEDFont1Data, LocalCharCode, CurrCharVal

              Case 2
                #ifndef GLCD_Disable_OLED_FONT2
                  'Read this 20 times... (0..COLSperfont) [ * 2 ]
                  ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                #endif
                #ifdef GLCD_Disable_OLED_FONT2
                  CurrCharVal = GLCDBackground
                #endif
          End Select

            'we handle 8 or 16 pixels of height
            FOR CurrCharRow = 0 TO ROWSperfont
                'Set the pixel
                IF CurrCharCol = COLSperfont THEN
                     IF GLCDfntDefaultSize = 1 THEN
                          SendData_SSD1351( GLCDBackground_H )
                          SendData_SSD1351( GLCDBackground )
                     END IF
                          GOTO Intercharacter
                END IF
                IF CurrCharVal.0 = 0 THEN
                          'PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
                          SendData_SSD1351( GLCDBackground_H )
                          SendData_SSD1351( GLCDBackground )
                ELSE
                          'PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDForeground
                          SendData_SSD1351( LineColour_H )
                          SendData_SSD1351( LineColour )
                END IF

                Rotate CurrCharVal Right
         Intercharacter:
                'Set to next row of date, a second row
                IF GLCDfntDefaultSize = 2 and CurrCharRow = 7 THEN
                  LocalCharCode++
                  #ifndef GLCD_Disable_OLED_FONT2
                    ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                  #endif
                  #ifdef GLCD_Disable_OLED_FONT2
                    CurrCharVal = GLCDBackground
                  #endif
                END IF

                'It is the intercharacter space, put out one pixel row
                IF CurrCharCol = COLSperfont THEN
                   'Put out a white intercharacter pixel/space
                     GLCDTemp = CharLocX + CurrCharCol
                     IF GLCDfntDefaultSize = 2 THEN
                        GLCDTemp++
                        'PSet GLCDTemp , CharLocY + CurrCharRow, GLCDBackground
                        SendData_SSD1351( GLCDBackground_H )
                        SendData_SSD1351( GLCDBackground )
                     END IF
                     'PSet GLCDTemp , CharLocY + CurrCharRow, GLCDBackground
                END IF

            NEXT
        NEXT

        IF SSD1351Rotation.0 <> 0 THEN
         SendCommand_SSD1351(0xA0)    'horizontal address increment
         SendData_SSD1351(0x74)
        END IF

        SendCommand_SSD1351(SSD1351_Reset_To_Normal_Display)

    #endif

End Sub



'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_SSD1351(In LineX1 as byte, In LineY1 as byte, In LineX2 as byte, In LineY2 as byte, Optional In LineColour As Word = GLCDForeground)


'erv check the number_pixels does not need to be a long.  Test by filledbox 1,1,127,127
'erv we found an issues this week than meant we had to change other libraries to long

  Dim LineColour, number_pixels as word
  Dim GLCDTemp as byte
  'Make sure that starting point (1) is always less than end point (2)
  IF LineX1 > LineX2 THEN
    GLCDTemp = LineX1
    LineX1 = LineX2
    LineX2 = GLCDTemp
  END IF
  IF LineY1 > LineY2 THEN
    GLCDTemp = LineY1
    LineY1 = LineY2
    LineY2 = GLCDTemp
  END IF

  'swap values to support rotation as this device DOES NOT SUPPORT Portrait
  IF SSD1351Rotation.0 = 0 THEN
    GLCDTemp = LineX1
    LineX1 = LineY1
    LineY1 = GLCDTemp

    GLCDTemp = LineX2
    LineX2 = LineY2
    LineY2 = GLCDTemp
  END IF

  SendCommand_SSD1351(SSD1351_Set_Column_Address)
  SendData_SSD1351 LineX1
  SendData_SSD1351 LineX2

  SendCommand_SSD1351(SSD1351_Set_Row_Address)
  SendData_SSD1351 LineY1
  SendData_SSD1351 LineY2

  SendCommand_SSD1351(SSD1351_Write_RAM)

  number_pixels = ( LineX2 - LineX1 )
  number_pixels = number_pixels + 1
  GLCDTemp = ( LineY2 - LineY1 )
  GLCDTemp =  GLCDTemp + 1
  number_pixels = [word]number_pixels * GLCDTemp

  REPEAT number_pixels

   SendData_SSD1351( LineColour_H )
   SendData_SSD1351( LineColour )

  END REPEAT

  SendCommand_SSD1351(SSD1351_Reset_To_Normal_Display)

End Sub



'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
Sub PSet_SSD1351(In GLCDX as byte, In GLCDY as byte, In GLCDColour As Word)

    Dim GLCDTemp as byte
'erv is this correct? If correct then is GLCDRotate correct?
    'swap values to support rotation as this device DOES NOT SUPPORT Portrait! arcghhhh
    IF SSD1351Rotation.0 = 0 THEN
        GLCDTemp = GLCDX
        GLCDX = GLCDY
        GLCDY = GLCDTemp
    END IF

    #IFDEF GLCD_PROTECTOVERRUN
        'anything off screen with be rejected
        IF GLCDX => SSD1351_GLCD_WIDTH OR GLCDY => SSD1351_GLCD_HEIGHT THEN
            Exit Sub
        END IF

    #ENDIF

    SendCommand_SSD1351(SSD1351_Set_Column_Address) ' Column addr set
    SendData_SSD1351 GLCDX
    SendData_SSD1351 GLCDX

    SendCommand_SSD1351(SSD1351_Set_Row_Address) ' Row addr set
    SendData_SSD1351 GLCDY
    SendData_SSD1351 GLCDY

    SendCommand_SSD1351(SSD1351_Write_RAM) ' Enable MCU to write to RAM

    SendData_SSD1351( GLCDColour_H )
    SendData_SSD1351( GLCDColour )

End Sub


'erv delete?
Sub  SendMultipleCommand_SSD1351( in SSD1351BytestoSend as byte )

  Dim SSD1351BytestoSendCounter, SSD1351SendByte as byte

  Set SSD1351_CS OFF;
  Set SSD1351_DC OFF;

  #ifdef SSD1351_HardwareSPI
    FOR SSD1351BytestoSendCounter = 0 TO SSD1351BytestoSend - 1
         FastHWSPITransfer  SSD1351_Command( SSD1351BytestoSendCounter )
         wait 10 us
    NEXT
    Set SSD1351_CS ON
    Exit Sub
  #endif

  #ifndef SSD1351_HardwareSPI
    FOR SSD1351BytestoSendCounter = 0 TO SSD1351BytestoSend - 1
      SSD1351SendByte = SSD1351_Command( SSD1351BytestoSendCounter )
      REPEAT 8

        IF SSD1351SendByte.7 = ON  THEN
          Set SSD1351_DO ON;
        ELSE
          Set SSD1351_DO OFF;
        END IF
        SET GLCD_SCK On;
        Rotate SSD1351SendByte left
        Set GLCD_SCK Off;

      END REPEAT
    NEXT
    Set SSD1351_CS ON
  #endif

End Sub



'''Send a command to the SSD1351 GLCD
'''@param SSD1351SendByte Command to send
'''@hide
Sub  SendCommand_SSD1351( IN SSD1351SendByte as byte )

  Set SSD1351_CS OFF;
  Set SSD1351_DC OFF;

  #ifdef SSD1351_HardwareSPI
     FastHWSPITransfer  SSD1351SendByte
     Set SSD1351_CS ON;
     Exit Sub
  #endif

  #ifndef SSD1351_HardwareSPI
  REPEAT 8

    IF SSD1351SendByte.7 = ON  THEN
      Set SSD1351_DO ON;
    ELSE
      Set SSD1351_DO OFF;
    END IF
    SET GLCD_SCK On;
    Rotate SSD1351SendByte left
    Set GLCD_SCK Off;

  END REPEAT
  Set SSD1351_CS ON;
  #endif

End Sub



'erv delete?
Sub  SendMultipleData_SSD1351( in SSD1351BytestoSend as byte )

  Dim SSD1351BytestoSendCounter, SSD1351SendByte as byte
'  Dim SSD1351_Command(12)

  Set SSD1351_CS OFF;
  Set SSD1351_DC On;

  #ifdef SSD1351_HardwareSPI
    FOR SSD1351BytestoSendCounter = 0 TO SSD1351BytestoSend - 1
         FastHWSPITransfer  SSD1351_Command( SSD1351BytestoSendCounter )
    NEXT
    Set SSD1351_CS ON
    Exit Sub
  #endif

  #ifndef SSD1351_HardwareSPI
    FOR SSD1351BytestoSendCounter = 0 TO SSD1351BytestoSend - 1
      SSD1351SendByte = SSD1351_Command( SSD1351BytestoSendCounter )
      REPEAT 8

        IF SSD1351SendByte.7 = ON  THEN
          Set SSD1351_DO ON;
        ELSE
          Set SSD1351_DO OFF;
        END IF
        SET GLCD_SCK On;
        Rotate SSD1351SendByte left
        Set GLCD_SCK Off;

      END REPEAT
    NEXT
    Set SSD1351_CS ON
  #endif

End Sub



'''Send a data byte to the SSD1351 GLCD
'''@param SSD1351SendByte Byte to send
'''@hide
Sub  SendData_SSD1351( IN SSD1351SendByte as byte )

  Set SSD1351_CS OFF;
  Set SSD1351_DC ON;

  #ifdef SSD1351_HardwareSPI
     FastHWSPITransfer  SSD1351SendByte
     Set SSD1351_CS ON;
     Exit Sub
  #endif

  #ifndef SSD1351_HardwareSPI
  REPEAT 8

    IF SSD1351SendByte.7 = ON THEN
      Set SSD1351_DO ON;
    ELSE
      Set SSD1351_DO OFF;
    END IF
    SET GLCD_SCK On;
    Rotate SSD1351SendByte left
    Set GLCD_SCK Off;

  END REPEAT
  Set SSD1351_CS ON;
  #endif

End Sub


'erv confirm this has been tested and it works
'''@hide
Sub GLCDRotate_SSD1351 ( in SSD1351Rotation )

'erv do not clear screen here. All the other libraries DO NOT clear the screen, remove?

  SendCommand_SSD1351(SSD1351_Set_Column_Address) ' Column addr set
  SendData_SSD1351 0
  SendData_SSD1351 SSD1351_GLCD_WIDTH-1

  SendCommand_SSD1351(SSD1351_Set_Row_Address) ' Row addr set
  SendData_SSD1351 0
  SendData_SSD1351 SSD1351_GLCD_HEIGHT-1

  SendCommand_SSD1351(SSD1351_Write_RAM)

  REPEAT 16384
    SendData_SSD1351( GLCDBackground_H )
    SendData_SSD1351( GLCDBackground )
  END REPEAT

  Select Case SSD1351Rotation
        Case PORTRAIT
             SSD1351_GLCD_WIDTH = GLCD_HEIGHT
             SSD1351_GLCD_HEIGHT = GLCD_WIDTH

             SendCommand_SSD1351(0xA0)
             SendData_SSD1351(0x76)

        Case LANDSCAPE
             SSD1351_GLCD_WIDTH = GLCD_WIDTH
             SSD1351_GLCD_HEIGHT = GLCD_HEIGHT

             SendCommand_SSD1351(0xA0)    'Set remap & data format 0111 0000
             SendData_SSD1351(0x74)    ' RGB colour

        Case PORTRAIT_REV
             SSD1351_GLCD_WIDTH = GLCD_HEIGHT
             SSD1351_GLCD_HEIGHT = GLCD_WIDTH

             SendCommand_SSD1351(0xA0)
             SendData_SSD1351(0x74)

        Case LANDSCAPE_REV
             SSD1351_GLCD_WIDTH = GLCD_WIDTH
             SSD1351_GLCD_HEIGHT = GLCD_HEIGHT

             SendCommand_SSD1351(0xA0)
             SendData_SSD1351(0x76)

        Case else
             SSD1351_GLCD_WIDTH = GLCD_WIDTH
             SSD1351_GLCD_HEIGHT = GLCD_HEIGHT

             SendCommand_SSD1351(0xA0)    'Set remap & data format 0111 0000
             SendData_SSD1351(0x74)    ' RGB colour
  End Select

End Sub



'''Displays a string in a larger font i.e.  BigPrint_SSD1351 ( 30, SSD1351_GLCD_HEIGHT - 20 , "BigPrint_SSD1351" )
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData String to display
'''@param Color Optional color
Sub BigPrint_SSD1351(In PrintLocX as byte, In PrintLocY as byte,  PrintData As String, Optional In  Color as word = GLCDForeground)
  Dim GLCDPrintLoc as byte
  PrintLen = PrintData(0)
  IF PrintLen = 0 THEN Exit Sub
  GLCDPrintLoc = PrintLocX
  FOR SysPrintTemp = 1 TO PrintLen
    DrawBigChar_SSD1351 GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  NEXT
End Sub



'''Draws a a character in a larger font
Sub DrawBigChar_SSD1351 (In CharLocX as byte, In CharLocY as byte, In CharCode, Optional In  Color as word = GLCDForeground )

    Dim Locx, Locy, CurrCol as Word

    IF CharCode <=126 THEN
       CharCode -=32
       GOTO GCBBigTables
    END IF
    IF CharCode <=210 THEN
       CharCode -=33
       GOTO GCBBigTables
    END IF
    IF CharCode <= 250 THEN
       CharCode -=34
       GOTO GCBBigTables
    END IF
    GCBBigTables:
    FOR CurrCharCol = 1 TO 24
      CurrCol=CurrCharCol+CharCode*24-(CharCode/10)*240
      IF CharCode>=0 and CharCode<=9 THEN ReadTable BigFont32_41 , CurrCol, CurrCharVal
      IF CharCode>=10 and CharCode<=19 THEN ReadTable BigFont42_51 , CurrCol, CurrCharVal
      IF CharCode>=20 and CharCode<=29 THEN ReadTable BigFont52_61 , CurrCol, CurrCharVal
      IF CharCode>=30 and CharCode<=39 THEN ReadTable BigFont62_71 , CurrCol, CurrCharVal
      IF CharCode>=40 and CharCode<=49 THEN ReadTable BigFont72_81 , CurrCol, CurrCharVal
      IF CharCode>=50 and CharCode<=59 THEN ReadTable BigFont82_91 , CurrCol, CurrCharVal
      IF CharCode>=60 and CharCode<=69 THEN ReadTable BigFont92_101 , CurrCol, CurrCharVal
      IF CharCode>=70 and CharCode<=79 THEN ReadTable BigFont102_111 , CurrCol, CurrCharVal
      IF CharCode>=80 and CharCode<=89 THEN ReadTable BigFont112_121 , CurrCol, CurrCharVal
      IF CharCode>=90 and CharCode<=99 THEN ReadTable BigFont122_126 , CurrCol, CurrCharVal
      IF CharCode>=160 and CharCode<=169 THEN ReadTable BigFont193_202 , CurrCol, CurrCharVal
      IF CharCode>=170 and CharCode<=179 THEN ReadTable BigFont203_212 , CurrCol, CurrCharVal
      IF CharCode>=180 and CharCode<=183 THEN ReadTable BigFont213_216 , CurrCol, CurrCharVal

        IF CurrCharVal=36 THEN CurrCharVal=33
        FOR CurrCharRow = 1 TO 8
          LocX=CharLocX+CurrCharCol
          LocY=CharLocY+CurrCharRow
          IF CurrCharCol>12 THEN
             LocX= LocX - 12
             LocY= LocY + 8
          END IF
          IF CurrCharVal.0=1 THEN
             PSet_SSD1351 LocX , LocY , Color
          ELSE
             PSet_SSD1351 LocX , LocY ,GLCDBackground
          END IF
          Rotate CurrCharVal Right
        NEXT
    NEXT

End Sub


'erv tested?
'''Sets the constrast to select 0 out of 255 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
Sub SetContrast_SSD1351 ( in color_A, in color_B, in color_C )

  SendCommand_SSD1351(SSD1351_Set_Contrast_A_B_C)
  SendData_SSD1351(color_A)
  SendData_SSD1351(color_B)
  SendData_SSD1351(color_C)

End Sub


'erv tested?
'''Sets the value to select 0 out of 255 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
Sub SetBrightness_SSD1351 ( in dim_state )

  dim_state = dim_state / 16
  SendCommand_SSD1351(SSD1351_MasterCurrent)
  SendCommand_SSD1351(dim_state)

End Sub


'erv tested?
Sub StartScroll_SSD1351

  SendCommand_SSD1351(SSD1351_SCROLL_START )

End Sub


'erv tested?
Sub StopScroll_SSD1351

  SendCommand_SSD1351(SSD1351_SCROLL_STOP )

End Sub


'erv tested?
'''sets scroll parameters
'''horizontal_scroll_offset = number of columns to scroll horizontally during each time interval ( 0 = no horizontal scroll, 1 to 127 = scroll right, 128 to 255 = scroll left)
'''start_row_address = row number where vertical scroll starts (this row and all subsequent rows scroll) (0 to 127)
'''num_rows_scrolled = total number of rows to be horizontally scrolled ( start_row_address + num_rows_scrolled <= 128 )
'''scroll_time_interval = time interval between each scroll step in frames ( 0 = test mode, 1 = normal, 2 = slow, 3 = slowest )
Sub SetScrollParameters_SSD1351(In horizontal_scroll_offset as byte, In start_row_address as byte, In num_rows_scrolled as byte, In scroll_time_interval as byte)

  SendCommand_SSD1351(SSD1351_SCROLL_SETUP)
  SendData_SSD1351(horizontal_scroll_offset)
  SendData_SSD1351(start_row_address)
  SendData_SSD1351(num_rows_scrolled)
  SendData_SSD1351(0)
  SendData_SSD1351(scroll_time_interval)

End Sub
