'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2017-2020 Evan Venn and Kent Schafer

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
' Supports ILI9481 controller only.

'Changes
  '6/10/2017 added 16bit data bus interface for Mega2560 board KentS
  '17/9/2017 revised send command and data to resolve 8bit addressing EvanRVenn.
  '17/9/2017 revised to handle port output EvanRVenn.
  '03/04/2019 Revised to support DEFAULT_GLCDBACKGROUND constant
  '11/04/2019 Revised to clean up position and therefore the bleeding of constants into ASM
  '27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn
  '11/11/19  Removal of commented code

'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ILI9481"
'Parallel lines (ILI9481 only)
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 0; GLCD_DB0; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 1; GLCD_DB1; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 2; GLCD_DB2; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 3; GLCD_DB3; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 4; GLCD_DB4; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 5; GLCD_DB5; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 6; GLCD_DB6; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Data Bus 7; GLCD_DB7; IO_Pin

'''16bit DBI Parallel Ports (ILI9481 only)
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; DataPortH; IO_Port
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; DataPort; IO_Port

'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Reset; GLCD_RST; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Chip Select; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Register Select; GLCD_RS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Read/Write; GLCD_RW; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9481; Enable; GLCD_RD; IO_Pin

/*
      '8bit
      #define ILI9481_GLCD_RST  GLCD_RST
      #define ILI9481_GLCD_CS   GLCD_CS
      #define ILI9481_GLCD_RS   GLCD_RS
      #define ILI9481_GLCD_WR   GLCD_WR
      #define ILI9481_GLCD_RD   GLCD_RD
      #define ILI9481_GLCD_DB0  GLCD_DB0
      #define ILI9481_GLCD_DB1  GLCD_DB1
      #define ILI9481_GLCD_DB2  GLCD_DB2
      #define ILI9481_GLCD_DB3  GLCD_DB3
      #define ILI9481_GLCD_DB4  GLCD_DB4
      #define ILI9481_GLCD_DB5  GLCD_DB5
      #define ILI9481_GLCD_DB6  GLCD_DB6
      #define ILI9481_GLCD_DB7  GLCD_DB7
      #define tinyDelay nop:nop

      '16bit
      #define ILI9481_GLCD_RST GLCD_RST
      #define ILI9481_GLCD_CS  GLCD_CS
      #define ILI9481_GLCD_RS  GLCD_RS
      #define ILI9481_GLCD_WR  GLCD_WR
      '#define ILI9481_GLCD_RD GLCD_RD  'not used
*/

#define ILI9481_WHITE   TFT_WHITE
#define ILI9481_BLACK   TFT_BLACK
#define ILI9481_RED     TFT_RED
#define ILI9481_GREEN   TFT_GREEN
#define ILI9481_BLUE    TFT_BLUE
#define ILI9481_YELLOW  TFT_YELLOW
#define ILI9481_CYAN    TFT_CYAN
#define ILI9481_MAGENTA TFT_MAGENTA
#define ILI9481_NAVY          TFT_NAVY
#define ILI9481_DARKGREEN     TFT_DARKGREEN
#define ILI9481_DARKCYAN      TFT_DARKCYAN
#define ILI9481_MAROON        TFT_MAROON
#define ILI9481_PURPLE        TFT_PURPLE
#define ILI9481_OLIVE         TFT_OLIVE
#define ILI9481_LIGHTGREY     TFT_LIGHTGREY
#define ILI9481_DARKGREY      TFT_DARKGREY
#define ILI9481_ORANGE        TFT_ORANGE
#define ILI9481_GREENYELLOW   TFT_GREENYELLOW
#define ILI9481_PINK          TFT_PINK


#startup InitGLCD_ILI9481

dim GLCDDeviceWidth, GLCDDeviceHeight as word

Sub InitGLCD_ILI9481


  #if GLCD_TYPE = GLCD_TYPE_ILI9481

  'Setup code for ILI9481 controllers

    #ifndef GLCD_ILI9481_16bit

      dir  ILI9481_GLCD_DB7 OUT
      dir  ILI9481_GLCD_DB6 OUT
      dir  ILI9481_GLCD_DB5 OUT
      dir  ILI9481_GLCD_DB4 OUT
      dir  ILI9481_GLCD_DB3 OUT
      dir  ILI9481_GLCD_DB2 OUT
      dir  ILI9481_GLCD_DB1 OUT
      dir  ILI9481_GLCD_DB0 OUT

      'Set pin directions
      Dir ILI9481_GLCD_RD  Out
      Dir ILI9481_GLCD_WR  Out
      Dir ILI9481_GLCD_RS  Out
      Dir ILI9481_GLCD_CS  Out
      Dir ILI9481_GLCD_RST Out

      Set ILI9481_GLCD_RD On
      Set ILI9481_GLCD_WR On
      Set ILI9481_GLCD_RS On
      set ILI9481_GLCD_CS ON
      Set ILI9481_GLCD_RST On


      'Reset
      Set ILI9481_GLCD_RST On
      Wait 5 ms
      Set ILI9481_GLCD_RST Off
      Wait 15 ms
      Set ILI9481_GLCD_RST On
      Wait 15 ms

      set ILI9481_GLCD_CS ON
      set ILI9481_GLCD_WR ON
      set ILI9481_GLCD_CS OFF


      SendCommand_ILI9481(0x11)
      wait 20 ms

      SendCommand_ILI9481(0xD0)
      SendData_ILI9481(0x07)
      SendData_ILI9481(0x42)
      SendData_ILI9481(0x18)

      SendCommand_ILI9481(0xD1)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x07)
      SendData_ILI9481(0x10)

      SendCommand_ILI9481(0xD2)
      SendData_ILI9481(0x01)
      SendData_ILI9481(0x02)

      SendCommand_ILI9481(0xC0)
      SendData_ILI9481(0x10)
      SendData_ILI9481(0x3B)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x02)
      SendData_ILI9481(0x11)

      SendCommand_ILI9481(0xC5)
      SendData_ILI9481(0x03)
      SendData_ILI9481(0x36)
      SendData_ILI9481(0x0A)

      SendCommand_ILI9481(0x36)
      SendData_ILI9481(0x0E)

      SendCommand_ILI9481(0x3A)
      SendData_ILI9481(0x55)

      SendCommand_ILI9481(0x2a)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x01)
      SendData_ILI9481(0x3F)

      SendCommand_ILI9481(0x2B)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x01)
      SendData_ILI9481(0xE0)

      SendCommand_ILI9481(0x2a)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x01)
      SendData_ILI9481(0x3F)

      SendCommand_ILI9481(0x2B)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x01)
      SendData_ILI9481(0xE0)

      wait 120 ms

  fin1:
      SendCommand_ILI9481(0x29)
      SendCommand_ILI9481(0x2C)

      wait 120 ms
      'Colours
      GLCDBackground = ILI9481_BLACK
      GLCDForeground = ILI9481_WHITE
      GLCDFontWidth = 6
      GLCDfntDefault = 0
      GLCDfntDefaultsize = 1
      GLCDDeviceHeight = GLCD_HEIGHT - 1
      GLCDDeviceWidth  = GLCD_WIDTH - 1
    #endif


    #ifDef GLCD_ILI9481_16bit

      'Pin directions
      Dir ILI9481_GLCD_CS Out
      Dir ILI9481_GLCD_RS Out
      Dir ILI9481_GLCD_WR Out
      Dir ILI9481_GLCD_RST Out

      Dir ILI9481_DataPortH Out
      Dir ILI9481_DataPortL Out

      Set ILI9481_GLCD_CS On
      Set ILI9481_GLCD_RS On
      Set ILI9481_GLCD_WR On
      Set ILI9481_GLCD_RST On
      'reset LCD before init.

      wait 50 ms
      Set ILI9481_GLCD_RST Off
      Wait 50 ms
      Set ILI9481_GLCD_RST On
      Wait 100 ms

      Set ILI9481_GLCD_CS Off
      'initlcd.h from QDTech for QDM320B (QDM320DB16NT9481RA)
      'init. the GPIOs for LCD
      SendCommand_ILI9481(0x11)   'exit sleep mode
      wait 20 ms

      SendCommand_ILI9481(0xD0)   'power setting
      SendData_ILI9481(0x07)
      SendData_ILI9481(0x42)
      SendData_ILI9481(0x18)

      SendCommand_ILI9481(0xD1)   'vcom setting
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x07)
      SendData_ILI9481(0x10)

      SendCommand_ILI9481(0xD2)   'power setting normal mode
      SendData_ILI9481(0x01)
      SendData_ILI9481(0x02)

      SendCommand_ILI9481(0xC0)   'panel driver setting
      SendData_ILI9481(0x10)
      SendData_ILI9481(0x3B)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x02)
      SendData_ILI9481(0x11)

      SendCommand_ILI9481(0xC5)   'frame rate inversion control
      SendData_ILI9481(0x03)

      SendCommand_ILI9481(0xC8)   'gamma settings
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x32)
      SendData_ILI9481(0x36)
      SendData_ILI9481(0x45)
      SendData_ILI9481(0x06)
      SendData_ILI9481(0x16)
      SendData_ILI9481(0x37)
      SendData_ILI9481(0x75)
      SendData_ILI9481(0x77)
      SendData_ILI9481(0x54)
      SendData_ILI9481(0x0C)
      SendData_ILI9481(0x00)

      SendCommand_ILI9481(0x36)   'set address mode
      SendData_ILI9481(0x0A)      'portrait (normal) mode

      SendCommand_ILI9481(0x3A)   'set pixel format
      SendData_ILI9481(0x55)      '16bpp, 65k color

      SendCommand_ILI9481(0x21)   'enter invert mode

      SendCommand_ILI9481(0x2A)   'set column address
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x01)
      SendData_ILI9481(0xDF)
      'SendData_ILI9481(0xE0)

      SendCommand_ILI9481(0x2B)   'set page address
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x00)
      SendData_ILI9481(0x01)
      SendData_ILI9481(0x3F)
      'SendData_ILI9481(0x40)
      wait 120 ms

      SendCommand_ILI9481(0x29)   'set display on
      SendCommand_ILI9481(0x2C) 'write memory start
      wait 25 ms
      Set ILI9481_GLCD_CS On

      'Default Colours
      GLCDForeground = ILI9481_WHITE
      #ifdef DEFAULT_GLCDBACKGROUND
        GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
      #endif

      #ifndef DEFAULT_GLCDBACKGROUND
        GLCDBACKGROUND = ILI9481_BLACK
      #endif


      'Variables required for device
      ILI9481_GLCD_WIDTH = GLCD_WIDTH
      ILI9481_GLCD_HEIGHT =  GLCD_HEIGHT
      GLCDFontWidth = 6
      GLCDfntDefault = 0
      GLCDfntDefaultsize = 1
      GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn

    #endif

  #endif

  'Clear screen
  GLCDCLS_ILI9481

End Sub



'Subs
'''Clears the GLCD screen
Sub GLCDCLS_ILI9481 ( Optional In  GLCDBackground as word = GLCDBackground )

  ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
  Dim ILI9481SendWord as word
  GLCD_yordinate = 0

  #if GLCD_TYPE = GLCD_TYPE_ILI9481

    #ifndef GLCD_ILI9481_16bit
      set ILI9481_GLCD_CS OFF

      SetAddressWindow_ILI9481 ( 0,0, GLCDDeviceWidth, GLCDDeviceHeight  )
      ILI9481SendWord = GLCDBackground

      Repeat 480
        Repeat 320

          '        SendData_ILI9481 ( ILI9481SendWord_H )
          '        SendData_ILI9481 ( ILI9481SendWord )

          #ifdef PIC
            ILI9481_GLCD_DB7 = ILI9481SendWord.15
            ILI9481_GLCD_DB6 = ILI9481SendWord.14
            ILI9481_GLCD_DB5 = ILI9481SendWord.13
            ILI9481_GLCD_DB4 = ILI9481SendWord.12
            ILI9481_GLCD_DB3 = ILI9481SendWord.11
            ILI9481_GLCD_DB2 = ILI9481SendWord.10
            ILI9481_GLCD_DB1 = ILI9481SendWord.9
            ILI9481_GLCD_DB0 = ILI9481SendWord.8
          #endif
          #ifdef AVR
          PORTD = (PORTD & 0B00000011) | ((ILI9481SendWord_H) & 0B11111100);
          PORTB = (PORTB & 0B11111100) | ((ILI9481SendWord_H) & 0B00000011);
          #endif

            set ILI9481_GLCD_WR OFF
            set ILI9481_GLCD_WR ON

          #ifdef PIC
            ILI9481_GLCD_DB7 = ILI9481SendWord.7
            ILI9481_GLCD_DB6 = ILI9481SendWord.6
            ILI9481_GLCD_DB5 = ILI9481SendWord.5
            ILI9481_GLCD_DB4 = ILI9481SendWord.4
            ILI9481_GLCD_DB3 = ILI9481SendWord.3
            ILI9481_GLCD_DB2 = ILI9481SendWord.2
            ILI9481_GLCD_DB1 = ILI9481SendWord.1
            ILI9481_GLCD_DB0 = ILI9481SendWord.0
          #endif
          #ifdef AVR
          PORTD = (PORTD & 0B00000011) | ((ILI9481SendWord) & 0B11111100);
          PORTB = (PORTB & 0B11111100) | ((ILI9481SendWord) & 0B00000011);
          #endif
            set ILI9481_GLCD_WR OFF
            set ILI9481_GLCD_WR ON
        End Repeat

      End Repeat
      set ILI9481_GLCD_CS ON

    #endif

    #ifdef GLCD_ILI9481_16bit
      Dim ILI9481_pixelcount as Long
      SetAddressWindow_ILI9481 ( 0, 0, ILI9481_GLCD_WIDTH -1, ILI9481_GLCD_HEIGHT-1 )

      Set ILI9481_GLCD_CS OFF
      ILI9481_DataPortH = GLCDBackGround_H
      ILI9481_DataPortL = GLCDBackGround
        ILI9481_pixelcount = 320 * 480    'ILI9481_glcd_Width * ILI9481_glcd_Height    '320 * 480
        repeat ILI9481_pixelcount
            ILI9481_GLCD_WR = 0
            ILI9481_GLCD_WR = 1
        end repeat
      set ILI9481_GLCD_CS ON
    #endif

  #endif

End Sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_ILI9481(In LineX1 as Word, In LineY1 as Word, In LineX2 as Word, In LineY2 as Word, Optional In LineColour As Word = GLCDForeground)
  dim GLCDTemp, DrawLine as word

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

  #If GLCD_TYPE = GLCD_TYPE_ILI9481
    'Draw lines going across
    For DrawLine = LineX1 To LineX2
      For GLCDTemp = LineY1 To LineY2
        PSet DrawLine, GLCDTemp, LineColour
      Next
    Next
  #endif

End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel (0 = erase, 1 = display)
Sub PSet_ILI9481(In GLCDX as word , In GLCDY as word, In GLCDColour As Word)

  ' if GLCDY => GLCDDeviceHeight or GLCDX => GLCDDeviceWidth then exit sub
  set ILI9481_GLCD_CS OFF
  SetAddressWindow_ILI9481 ( GLCDX, GLCDY, GLCDX, GLCDY )

  #If GLCD_TYPE = GLCD_TYPE_ILI9481

    #ifndef GLCD_ILI9481_16bit
      SendData_ILI9481 GLCDColour_H
      SendData_ILI9481 GLCDColour
    #endif

    #ifdef GLCD_ILI9481_16bit
      sendWord_ILI9481 GLCDColour
    #endif

  #endif
  set ILI9481_GLCD_CS ON


End Sub

'
'          '''Send a command to the ILI9481 GLCD
'          '''@param ILI9481SendByte Command to send
'          '''@hide
sub  SendCommand_ILI9481( IN ILI9481SendByte as byte )


  #if GLCD_TYPE = GLCD_TYPE_ILI9481

    #ifndef GLCD_ILI9481_16bit
        '  'Set output command
        #ifdef PIC
          ILI9481_GLCD_DB7 = ILI9481SendByte.7
          ILI9481_GLCD_DB6 = ILI9481SendByte.6
          ILI9481_GLCD_DB5 = ILI9481SendByte.5
          ILI9481_GLCD_DB4 = ILI9481SendByte.4
          ILI9481_GLCD_DB3 = ILI9481SendByte.3
          ILI9481_GLCD_DB2 = ILI9481SendByte.2
          ILI9481_GLCD_DB1 = ILI9481SendByte.1
          ILI9481_GLCD_DB0 = ILI9481SendByte.0
        #endif
        #ifdef AVR
        PORTD = (PORTD & 0B00000011) | ((ILI9481SendByte) & 0B11111100);
        PORTB = (PORTB & 0B11111100) | ((ILI9481SendByte) & 0B00000011);
        #endif

          set ILI9481_GLCD_RS OFF
          set ILI9481_GLCD_WR OFF
          set ILI9481_GLCD_WR ON
          set ILI9481_GLCD_RS ON

    #endif

    #ifdef GLCD_ILI9481_16bit
      Set ILI9481_GLCD_CS Off
      set ILI9481_GLCD_RS OFF

      ILI9481_DataPortH = 0
      ILI9481_DataPortL = ILI9481SendByte

      'set ILI9481_GLCD_RS OFF
      set ILI9481_GLCD_WR OFF
      set ILI9481_GLCD_WR ON
      set ILI9481_GLCD_RS ON
      Set ILI9481_GLCD_CS On
    #endif

  #endif



end Sub

'''Send a data byte to the ILI9481 GLCD
'''@param ILI9481SendByte Byte to send
'''@hide
sub  SendData_ILI9481( IN ILI9481SendByte as byte )

  #if GLCD_TYPE = GLCD_TYPE_ILI9481

    #ifndef GLCD_ILI9481_16bit
        #ifdef PIC
          ILI9481_GLCD_DB7 = ILI9481SendByte.7
          ILI9481_GLCD_DB6 = ILI9481SendByte.6
          ILI9481_GLCD_DB5 = ILI9481SendByte.5
          ILI9481_GLCD_DB4 = ILI9481SendByte.4
          ILI9481_GLCD_DB3 = ILI9481SendByte.3
          ILI9481_GLCD_DB2 = ILI9481SendByte.2
          ILI9481_GLCD_DB1 = ILI9481SendByte.1
          ILI9481_GLCD_DB0 = ILI9481SendByte.0
        #endif
        #ifdef AVR
        PORTD = (PORTD & 0B00000011) | ((ILI9481SendByte) & 0B11111100)
        PORTB = (PORTB & 0B11111100) | ((ILI9481SendByte) & 0B00000011)
        #endif

          set ILI9481_GLCD_WR OFF
          set ILI9481_GLCD_WR ON

    #endif


    #ifdef GLCD_ILI9481_16bit
        Set ILI9481_GLCD_CS Off
        ILI9481_DataPortH = 0
        ILI9481_DataPortL = ILI9481SendByte
        set ILI9481_GLCD_WR OFF
        set ILI9481_GLCD_WR ON

        Set ILI9481_GLCD_CS On
    #endif

  #endif

end Sub


'''Send a data word (16 bits) to the ILI9481 GLCD
'''@param ILI9481SendWord Word to send
'''@hide
Sub SendWord_ILI9481(In ILI9481SendWord1 As Word)

  Dim ILI9481SendWord1 as Word
  #ifdef GLCD_ILI9481_16bit
    Set ILI9481_GLCD_CS Off
        ILI9481_DataPortH = ILI9481SendWord1_H
        ILI9481_DataPortL = ILI9481SendWord1
    Set ILI9481_GLCD_WR Off
    Set ILI9481_GLCD_WR On
    Set ILI9481_GLCD_CS On
  #endif

End Sub


'''Set the row or column address range for the ILI9481 GLCD
'''@param ILI9481AddressType Address Type (ILI9481_ROW or ILI9481_COLUMN)
'''@param ILI9481Start Starting address
'''@param ILI9481End Ending address
'''@hide
Sub SetAddressWindow_ILI9481( In _x1 as word, in _y1 as word, in _x2 as word, in _y2 as word )

  SendCommand_ILI9481(0x2A)  'Column addr set
  SendData_ILI9481 _x1_H
  SendData_ILI9481 _x1
  SendData_ILI9481 _x2_H
  SendData_ILI9481 _x2

  SendCommand_ILI9481(0x2B)  'Row addr set
  SendData_ILI9481 _y1_H
  SendData_ILI9481 _y1
  SendData_ILI9481 _y2_H
  SendData_ILI9481 _y2

  SendCommand_ILI9481(0x2C)  'write to RAM

End Sub

Function PixelStatus_ILI9481(In GLCDX, In GLCDY ) as word

    #if GLCD_TYPE = GLCD_TYPE_ILI9481

        set ILI9481_GLCD_CS OFF
        'Select correct buffer element
        SetAddressWindow_ILI9481 ( GLCDX, GLCDY, GLCDX, GLCDY )

        set ILI9481_GLCD_CS On
        'Need to read three bytes
        'first byte is a dummy
        'then, two color bytes to make the woord


        set ILI9481_GLCD_RS On
        set ILI9481_GLCD_RD ON
        set ILI9481_GLCD_WR ON
        set ILI9481_GLCD_CS OFF

        SendCommand_ILI9481 ( 0x2e )

        dir  ILI9481_GLCD_DB7 IN
        dir  ILI9481_GLCD_DB6 IN
        dir  ILI9481_GLCD_DB5 IN
        dir  ILI9481_GLCD_DB4 IN
        dir  ILI9481_GLCD_DB3 IN
        dir  ILI9481_GLCD_DB2 IN
        dir  ILI9481_GLCD_DB1 IN
        dir  ILI9481_GLCD_DB0 IN

        set ILI9481_GLCD_RD OFF
        set ILI9481_GLCD_RD OFF
        'dummy read
        PixelStatus_ILI9481_h = PINb & 0B00000011
        PixelStatus_ILI9481_h = (PINd & 0B11111100) + PixelStatus_ILI9481_h
        set ILI9481_GLCD_RD ON

        'the real reads
        set ILI9481_GLCD_RD OFF
        set ILI9481_GLCD_RD OFF
        PixelStatus_ILI9481_h = PINb & 0B00000011
        PixelStatus_ILI9481_h = (PINd & 0B11111100) + PixelStatus_ILI9481_h
        set ILI9481_GLCD_RD ON

        set ILI9481_GLCD_RD OFF
        set ILI9481_GLCD_RD OFF
        [byte]PixelStatus_ILI9481 = PINb & 0B00000011
        [byte]PixelStatus_ILI9481 = (PINd & 0B11111100) + [byte]PixelStatus_ILI9481
        set ILI9481_GLCD_RD ON

        set ILI9481_GLCD_CS ON

        set ILI9481_GLCD_WR ON
        dir  ILI9481_GLCD_DB7 OUT
        dir  ILI9481_GLCD_DB6 OUT
        dir  ILI9481_GLCD_DB5 OUT
        dir  ILI9481_GLCD_DB4 OUT
        dir  ILI9481_GLCD_DB3 OUT
        dir  ILI9481_GLCD_DB2 OUT
        dir  ILI9481_GLCD_DB1 OUT
        dir  ILI9481_GLCD_DB0 OUT

      #endif

end function



'''Draws a string at the specified location on the ST7920 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_ILI9481( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as word = GLCDForeground )
    dim TargetCharCol as word
    for xchar = 1 to Chars(0)
      TargetCharCol = StringLocX + ((xchar*( GLCDFontWidth * GLCDfntDefaultsize ))-( GLCDFontWidth * GLCDfntDefaultsize ))
      GLCDDrawChar TargetCharCol , CharLocY , Chars(xchar), LineColour
    next
end sub

'''Draws a character at the specified location on the ST7920 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_ILI9481(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  dim CharCol, CharRow as word
  CharCode -= 15

  if CharCode>=178 and CharCode<=202 then
     CharLocY=CharLocY-1
  end if
  CharCol=1

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


End Sub

'''Displays a string in a larger fonti.e.  BigPrint_ILI9481 ( 30, ILI9481_GLCD_HEIGHT - 20 , "BigPrint_ILI9481" )
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData String to display
'''@param Color Optional color
Sub BigPrint_ILI9481(In PrintLocX as Word, In PrintLocY as Word,  PrintData As String, Optional In  Color as word = GLCDForeground)
  Dim GLCDPrintLoc as word
  PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = 1 To PrintLen
    DrawBigChar_ILI9481 GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  Next
End Sub

'''Draws a Vertical Line on the GLCD with a
'''@param GLCDY1 Y coordinate of one end of the line
'''@param GLCDY2 Y coordinate of the other end of the line
'''@param GLCDX1 X coordinate of the line
'''@param LineColour color
Sub DrawBigChar_ILI9481 (In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  Color as word = GLCDForeground )

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
             PSet_ILI9481 LocX , LocY , Color
          end if
          Rotate CurrCharVal Right
        Next
    Next
End Sub

Sub GLCDRotate_ili9481( in ILI9481AddressType as byte )

  set ILI9481_GLCD_CS OFF
  select case ILI9481AddressType
        case PORTRAIT  '0 degree

              SendCommand_ILI9481(0x36)
              SendData_ILI9481(0x0E)
              GLCDDeviceHeight = GLCD_HEIGHT - 1
              GLCDDeviceWidth  = GLCD_WIDTH - 1

        case LANDSCAPE

              SendCommand_ILI9481(0x36)
              SendData_ILI9481(0x2b)
              GLCDDeviceHeight = GLCD_WIDTH - 1
              GLCDDeviceWidth  = GLCD_HEIGHT - 1


        case PORTRAIT_REV

              SendCommand_ILI9481(0x36)
              SendData_ILI9481(0x0D)
              GLCDDeviceHeight = GLCD_HEIGHT - 1
              GLCDDeviceWidth  = GLCD_WIDTH - 1


        case LANDSCAPE_REV

              SendCommand_ILI9481(0x36)
              SendData_ILI9481(0x2c)
              GLCDDeviceHeight = GLCD_WIDTH - 1
              GLCDDeviceWidth  = GLCD_HEIGHT - 1

        case else
              SendCommand_ILI9481(0x36)
              SendData_ILI9481(0x0E)
              GLCDDeviceHeight = GLCD_HEIGHT - 1
              GLCDDeviceWidth  = GLCD_WIDTH - 1


  end select
  set ILI9481_GLCD_CS On

end sub
