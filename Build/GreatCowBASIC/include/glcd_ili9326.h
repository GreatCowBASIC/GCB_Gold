'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2012-2020 Hugh Considine, Evan Venn and Paolo Iocco

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
' Supports ILI9326 controller only.

'Changes
' 03/04/2019 Revised to support DEFAULT_GLCDBACKGROUND constant
' 11/04/2019 Revised to clean up position and therefore the bleeding of constants into ASM
'  27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn

'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ILI9326"

'Serial lines (ILI9326 only)
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9326; Data port; GLCD_DataPort; IO_Port
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9326; Read operations; GLCD_RD; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9326; Write info; GLCD_WR; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9326; Write operations ; GLCD_RS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9326; Chip Select; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ILI9326; Reset; GLCD_RST; IO_Pin


#startup InitGLCD_ILI9326

#define ILI9326_DataPort GLCD_DataPort

#define ILI9326_GLCD_RST GLCD_RST
#define ILI9326_GLCD_CS  GLCD_CS
#define ILI9326_GLCD_RS  GLCD_RS
#define ILI9326_GLCD_WR  GLCD_WR
#define ILI9326_GLCD_RD  GLCD_RD

'standard colors same as the TFT colors
#define ILI9326_WHITE   TFT_WHITE
#define ILI9326_BLACK   TFT_BLACK
#define ILI9326_RED     TFT_RED
#define ILI9326_GREEN   TFT_GREEN
#define ILI9326_BLUE    TFT_BLUE
#define ILI9326_YELLOW  TFT_YELLOW
#define ILI9326_CYAN    TFT_CYAN
#define ILI9326_MAGENTA TFT_MAGENTA
#define ILI9326_NAVY          TFT_NAVY
#define ILI9326_DARKGREEN     TFT_DARKGREEN
#define ILI9326_DARKCYAN      TFT_DARKCYAN
#define ILI9326_MAROON        TFT_MAROON
#define ILI9326_PURPLE        TFT_PURPLE
#define ILI9326_OLIVE         TFT_OLIVE
#define ILI9326_LIGHTGREY     TFT_LIGHTGREY
#define ILI9326_DARKGREY      TFT_DARKGREY
#define ILI9326_ORANGE        TFT_ORANGE
#define ILI9326_GREENYELLOW   TFT_GREENYELLOW
#define ILI9326_PINK          TFT_PINK


'instructions see datasheet

#define ILI9326_LCD_DRV_OUTPUT_CTRL           0x0001
#define ILI9326_LCD_DRIVING_WAVE_CTRL         0x0002
#define ILI9326_ENTRY_MODE                    0x0003

'Bits of entry mode
#define ILI9326_ENTRY_MODE_TRI                0x8000
#define ILI9326_ENTRY_MODE_DFM                0x4000
#define ILI9326_ENTRY_MODE_BGR                0x1000
#define ILI9326_ENTRY_MODE_RGB                0x0000
#define ILI9326_ENTRY_MODE_HWM                0x0200
#define ILI9326_ENTRY_MODE_ORG                0x0080
#define ILI9326_ENTRY_MODE_ID1                0x0020
#define ILI9326_ENTRY_MODE_ID0                0x0010
#define ILI9326_ENTRY_MODE_AM                 0x0008

#define ILI9326_OUTLINE_SHARPENING_CTRL       0x0006
#define ILI9326_DISP_CTRL_1                   0x0007
#define ILI9326_DISP_CTRL_2                   0x0008
#define ILI9326_DISP_CTRL_3                   0x0009
#define ILI9326_LOW_PWR_CTRL                  0x000b
#define ILI9326_RGB_DISP_INTF_CTRL_1          0x000c
#define ILI9326_RGB_DISP_INTF_CTRL_2          0x000f
#define ILI9326_PANEL_INTF_CTRL_1             0x0010
#define ILI9326_PANEL_INTF_CTRL_2             0x0011
#define ILI9326_PANEL_INTF_CTRL_3             0x0012
#define ILI9326_PANEL_INTF_CTRL_4             0x0020
#define ILI9326_PANEL_INTF_CTRL_5             0x0021
#define ILI9326_PANEL_INTF_CTRL_6             0x0022
#define ILI9326_FRAME_MARKER_POS              0x0090
#define ILI9326_PWR_CTRL_1                    0x0100
#define ILI9326_PWR_CTRL_2                    0x0101
#define ILI9326_PWR_CTRL_3                    0x0102
#define ILI9326_PWR_CTRL_4                    0x0103
#define ILI9326_PWR_CTRL_5                    0x0107
#define ILI9326_GRAM_ADDR_H_SET               0x0200
#define ILI9326_GRAM_ADDR_V_SET               0x0201
#define ILI9326_GRAM_WR_DATA                  0x0202
#define ILI9326_FRAME_RATE_COLOR_CTRL         0x020b
#define ILI9326_H_ADDR_START_POS              0x0210
#define ILI9326_H_ADDR_END_POS                0x0211
#define ILI9326_V_ADDR_START_POS              0x0212
#define ILI9326_V_ADDR_END_POS                0x0213
#define ILI9326_UID_CODE                      0x0280
#define ILI9326_VCOMH                         0x0281
#define ILI9326_MPT_VCM_PROG                  0x0290
#define ILI9326_MTP_VCM_STAT_EN               0x0291
#define ILI9326_MTP_PROG_KEY                  0x0295
#define ILI9326_GAMMA_CTRL_1                  0x0300
#define ILI9326_GAMMA_CTRL_2                  0x0301
#define ILI9326_GAMMA_CTRL_3                  0x0302
#define ILI9326_GAMMA_CTRL_4                  0x0305
#define ILI9326_GAMMA_CTRL_5                  0x0306
#define ILI9326_GAMMA_CTRL_6                  0x0307
#define ILI9326_GAMMA_CTRL_7                  0x0308
#define ILI9326_GAMMA_CTRL_8                  0x0309
#define ILI9326_GAMMA_CTRL_9                  0x030c
#define ILI9326_GAMMA_CTRL_10                 0x030d
#define ILI9326_BASE_IMG_DISP_CTRL_1          0x0400
#define ILI9326_BASE_IMG_DISP_CTRL_2          0x0401
#define ILI9326_BASE_IMG_DISP_CTRL_3          0x0404
#define ILI9326_PART_IMG_1_DISP_POS           0x0500
#define ILI9326_PART_IMG_1_AREA_START         0x0501
#define ILI9326_PART_IMG_1_AREA_END           0x0502
#define ILI9326_PART_IMG_2_DISP_POS           0x0503
#define ILI9326_PART_IMG_2_AREA_START         0x0504
#define ILI9326_PART_IMG_2_AREA_END           0x0505

' Screen rotation
#define LANDSCAPE       1
#define PORTRAIT_REV    2
#define LANDSCAPE_REV   3
#define PORTRAIT        4

'Foreground and background colours
Dim GLCDBackground As Word
Dim GLCDForeground As Word
Dim ILI9326_GLCD_WIDTH, ILI9326_GLCD_HEIGHT as word


'initialise the GLCD - see datasheet
sub InitGLCD_ILI9326

  #if GLCD_TYPE = GLCD_TYPE_ILI9326



      dir ILI9326_dataPort out
      dir ILI9326_GLCD_RST out
      dir ILI9326_GLCD_CS  out
      dir ILI9326_GLCD_RS  out
      dir ILI9326_GLCD_WR  out
      dir ILI9326_GLCD_RD  out



      Dim ILI9326_GLCD_WIDTH, ILI9326_GLCD_HEIGHT as word
      Dim ILI9326_GLCD_WIDTH_ADJ, ILI9326_GLCD_HEIGHT_ADJ as word
      Dim ILI9326_AddressType as byte

      'Reset*********************
      Set ILI9326_GLCD_RST On
      Wait 15 ms
      Set ILI9326_GLCD_RST Off
      Wait 15 ms
      Set ILI9326_GLCD_RST On
      Wait 15 ms
      '//*************Power On sequence ****************//
      'parameters below taken from...
      '*  Module for Microchip Graphics Library
      '*  LCD controller driver*  ILI9326
      'discharge*********************
      SendCommand_ILI9326(ILI9326_PWR_CTRL_1)
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(ILI9326_PWR_CTRL_3)
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(ILI9326_PWR_CTRL_4)
      SendData_ili9326(0x0000)
      wait 200 ms 'power settings*****
      SendCommand_ILI9326(ILI9326_PWR_CTRL_1)
      SendData_ili9326(0x1190)
      SendCommand_ILI9326(ILI9326_PWR_CTRL_2)
      SendData_ili9326(0x0017)
      SendCommand_ILI9326(ILI9326_PWR_CTRL_3)
      SendData_ili9326(0x01BD)
      SendCommand_ILI9326(ILI9326_PWR_CTRL_4)
      SendData_ili9326(0x0350)
      SendCommand_ILI9326(ILI9326_VCOMH)
      SendData_ili9326(0x0017)
      SendCommand_ILI9326(ILI9326_GRAM_ADDR_H_SET)
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(ILI9326_GRAM_ADDR_V_SET)
      SendData_ili9326(0x0000)

      'set gamma curve**************
      SendCommand_ILI9326(0x0300)
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(0x0301)
      SendData_ili9326(0x0707)
      SendCommand_ILI9326(0x0302)
      SendData_ili9326(0x0606)
      SendCommand_ILI9326(0x0305)
      SendData_ili9326(0x0006)
      SendCommand_ILI9326(0x0306)
      SendData_ili9326(0x0A09)
      SendCommand_ILI9326(0x0307)
      SendData_ili9326(0x0203)
      SendCommand_ILI9326(0x0308)
      SendData_ili9326(0x0005)
      SendCommand_ILI9326(0x0309)
      SendData_ili9326(0x0007)
      SendCommand_ILI9326(0x030C)
      SendData_ili9326(0x0400)
      SendCommand_ILI9326(0x030D)
      SendData_ili9326(0x000A)

      'set GRAM area**************
      SendCommand_ILI9326(ILI9326_H_ADDR_START_POS)
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(ILI9326_H_ADDR_END_POS)
      SendData_ili9326(0x00EF)' 240 pixels
      SendCommand_ILI9326(ILI9326_V_ADDR_START_POS)
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(ILI9326_V_ADDR_END_POS)
      SendData_ili9326(0x018F) '400 pixels
      SendCommand_ILI9326(ILI9326_BASE_IMG_DISP_CTRL_1)
      SendData_ili9326(0x3500)
      SendCommand_ILI9326(ILI9326_BASE_IMG_DISP_CTRL_2)
      SendData_ili9326(0x0001)
      SendCommand_ILI9326(ILI9326_BASE_IMG_DISP_CTRL_3)
      SendData_ili9326(0x0000)

      SendCommand_ILI9326(ILI9326_PANEL_INTF_CTRL_1) 'set screen defaults
      SendData_ili9326(0x0010)
      SendCommand_ILI9326(ILI9326_PANEL_INTF_CTRL_2) 'last set of commands
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(ILI9326_FRAME_RATE_COLOR_CTRL)
      SendData_ili9326(0x0000)
      SendCommand_ILI9326(ILI9326_DISP_CTRL_1)
      SendData_ili9326(0x0173) 'display on

      'Change to RGB TFT color and standard orientation
      SendCommand_ILI9326(ILI9326_ENTRY_MODE)
      SendData_ili9326( ILI9326_ENTRY_MODE_BGR )
      ILI9326_AddressType = Portrait
      ILI9326_GLCD_WIDTH_ADJ = 0
      ILI9326_GLCD_HEIGHT_ADJ = GLCD_HEIGHT - 1

      'Default Colours
      GLCDForeground = ILI9326_WHITE
      'Default Colours
      #ifdef DEFAULT_GLCDBACKGROUND
        GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
      #endif

      #ifndef DEFAULT_GLCDBACKGROUND
        GLCDBACKGROUND = ILI9326_BLACK
      #endif

      'Variables required for device
      ILI9326_GLCD_WIDTH = GLCD_WIDTH
      ILI9326_GLCD_HEIGHT = GLCD_HEIGHT

      #ifndef GLCD_OLED_FONT
        GLCDFontWidth = 6
      #endif
      #ifdef GLCD_OLED_FONT
        GLCDFontWidth = 5
      #endif

      GLCDfntDefaultsize = 1
      GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn

      'Clear screen
      GLCDCLS ( GLCDBackground )
  #ENDIF

end sub


'Subs
'''Clears the GLCD screen
sub GLCDCLS_ILI9326 ( Optional In  GLCDBackground as word = GLCDBackground )

  #if GLCD_TYPE = GLCD_TYPE_ILI9326

    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0

    sendCommand_ili9326(ILI9326_GRAM_ADDR_V_SET)
    sendData_ili9326( 0 )
    sendCommand_ili9326(ILI9326_GRAM_ADDR_H_SET)
    sendData_ili9326( 0 )

    SendCommand_ili9326(ILI9326_GRAM_WR_DATA)

    ' Use this method rather then call SendData_ili9326( as this is faster
    set ILI9326_GLCD_RS ON

    repeat GLCD_HEIGHT
      repeat GLCD_WIDTH

          ILI9326_DataPort=GLCDBackground_h

          set ILI9326_GLCD_WR OFF
          set ILI9326_GLCD_WR ON

          ILI9326_DataPort=GLCDBackground

          set ILI9326_GLCD_WR OFF
          set ILI9326_GLCD_WR ON

      end repeat
    end repeat

    sendCommand_ili9326(ILI9326_GRAM_ADDR_V_SET)
    sendData_ili9326( 0 )
    sendCommand_ili9326(ILI9326_GRAM_ADDR_H_SET)
    sendData_ili9326( 0 )

  #endif

end sub


'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
sub PSet_ILI9326( In GLCDX as word, In GLCDY as word, In GLCDColour As Word )

    Dim ILI9326_GLCD_WIDTH_ADJ, ILI9326_GLCD_HEIGHT_ADJ as word
    Dim ILI9326_AddressType as byte

    sendCommand_ili9326(ILI9326_GRAM_ADDR_H_SET)
    select case ILI9326_AddressType

      case Portrait
        sendData_ili9326( GLCDX )
      case Portrait_Rev
        sendData_ili9326( ILI9326_GLCD_WIDTH_ADJ - GLCDX )

      case Landscape
        sendData_ili9326( ILI9326_GLCD_HEIGHT_ADJ - GLCDY )
      case Landscape_Rev
        sendData_ili9326( GLCDY )

    end select

    sendCommand_ili9326(ILI9326_GRAM_ADDR_V_SET)
    select case ILI9326_AddressType

      case Portrait
        sendData_ili9326( ILI9326_GLCD_HEIGHT_ADJ - GLCDY )
      case Portrait_Rev
        sendData_ili9326( GLCDY )

      case Landscape
        sendData_ili9326( ILI9326_GLCD_WIDTH_ADJ - GLCDX )
      case Landscape_Rev
        sendData_ili9326( GLCDX )

    end select

    sendCommand_ili9326(ILI9326_GRAM_WR_DATA)
    sendData_ili9326( GLCDColour )

end sub


'''Send a command to the ILI9326 GLCD
'''@param ILI9326_SENDWORD Command to send
'''@hide
sub SendCommand_ili9326(ILI9326_SENDWORD as word)

  #if GLCD_TYPE = GLCD_TYPE_ILI9326
    set ILI9326_GLCD_CS OFF
    set ILI9326_GLCD_RS OFF
    set ILI9326_GLCD_RD ON
    set ILI9326_GLCD_WR ON

    ILI9326_DataPort = ILI9326_SENDWORD_h

    set ILI9326_GLCD_WR OFF
    set ILI9326_GLCD_WR ON

    ILI9326_DataPort = ILI9326_SENDWORD

    set ILI9326_GLCD_WR OFF
    set ILI9326_GLCD_WR ON
  #endif
end sub

'''Send a datat to the ILI9326 GLCD
'''@param ILI9326_SENDWORD Command to send
'''@hide
sub SendData_ILI9326(ILI9326_SENDWORD as word)

  #if GLCD_TYPE = GLCD_TYPE_ILI9326
    set ILI9326_GLCD_RS ON

    ILI9326_DataPort=ILI9326_SENDWORD_h

    set ILI9326_GLCD_WR OFF
    set ILI9326_GLCD_WR ON

    ILI9326_DataPort=ILI9326_SENDWORD

    set ILI9326_GLCD_WR OFF
    set ILI9326_GLCD_WR ON
  #endif
end sub


'''@hide
sub GLCDRotate_ILI9326 ( in ILI9326_AddressType )

  Dim ILI9326_GLCD_WIDTH, ILI9326_GLCD_HEIGHT as word
  Dim ILI9326_GLCD_WIDTH_ADJ, ILI9326_GLCD_HEIGHT_ADJ as word


  Select case ILI9326_AddressType
      Case PORTRAIT
          ILI9326_GLCD_WIDTH = GLCD_WIDTH
          ILI9326_GLCD_HEIGHT = GLCD_HEIGHT
          ILI9326_GLCD_WIDTH_ADJ = 0
          ILI9326_GLCD_HEIGHT_ADJ = GLCD_HEIGHT - 1

          'Change to RGB TFT color and standard orientation
          SendCommand_ILI9326(ILI9326_ENTRY_MODE)
          SendData_ili9326( ILI9326_ENTRY_MODE_BGR )

      Case Landscape
          ILI9326_GLCD_WIDTH = GLCD_HEIGHT
          ILI9326_GLCD_HEIGHT = GLCD_WIDTH
          ILI9326_GLCD_WIDTH_ADJ = GLCD_HEIGHT - 1
          ILI9326_GLCD_HEIGHT_ADJ = GLCD_WIDTH - 1

          SendCommand_ILI9326(ILI9326_ENTRY_MODE)
          SendData_ili9326( ILI9326_ENTRY_MODE_BGR | ILI9326_ENTRY_MODE_ID1 | ILI9326_ENTRY_MODE_AM )

      Case Portrait_Rev
          ILI9326_GLCD_WIDTH = GLCD_WIDTH
          ILI9326_GLCD_HEIGHT = GLCD_HEIGHT
          ILI9326_GLCD_WIDTH_ADJ = GLCD_WIDTH - 1
          ILI9326_GLCD_HEIGHT_ADJ = 0

          SendCommand_ILI9326(ILI9326_ENTRY_MODE)
          SendData_ili9326( ILI9326_ENTRY_MODE_BGR | ILI9326_ENTRY_MODE_ID1 )

      Case Landscape_Rev
          ILI9326_GLCD_WIDTH = GLCD_HEIGHT
          ILI9326_GLCD_HEIGHT = GLCD_WIDTH
          ILI9326_GLCD_WIDTH_ADJ = 0
          ILI9326_GLCD_HEIGHT_ADJ = 0

          SendCommand_ILI9326(ILI9326_ENTRY_MODE)
          SendData_ili9326( ILI9326_ENTRY_MODE_BGR | ILI9326_ENTRY_MODE_ID0 | ILI9326_ENTRY_MODE_ID1 | ILI9326_ENTRY_MODE_AM )

      Case Else
          ILI9326_GLCD_WIDTH = GLCD_WIDTH
          ILI9326_GLCD_HEIGHT = GLCD_HEIGHT
          ILI9326_GLCD_WIDTH_ADJ = 0
          ILI9326_GLCD_HEIGHT_ADJ = GLCD_HEIGHT - 1

          'Change to RGB TFT color and standard orientation
          SendCommand_ILI9326(ILI9326_ENTRY_MODE)
          SendData_ili9326( ILI9326_ENTRY_MODE_BGR )

  End Select

end sub

'''Draws a character at the specified location on the ILI9326 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
'''Draws a character at the specified location on the ILI9326 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_ILI9326(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This has got a tad complex
  'We have three major pieces
  '1 The preamble - this just adjusted color and the input character
  '2 The code that deals with GCB fontset
  '3 The code that deals with OLED fontset
  '
  'You can make independent change to section 2 and 3 but they are mutual exclusive with many common pieces

   dim CharCol, CharRow, GLCDTemp as word
   dim COLSperfont, CurrCharVal, CurrCharRow, CurrCharCol, ROWSperfont as byte
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
Sub FilledBox_ILI9326(In LineX1 as Word, In LineY1 as Word, In LineX2 as Word, In LineY2 as Word, Optional In LineColour As Word = GLCDForeground)
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


  'Draw lines going across
  For DrawLine = LineX1 To LineX2
    For GLCDTemp = LineY1 To LineY2
      PSet DrawLine, GLCDTemp, LineColour
    Next
  Next

End Sub

'''Draws a string at the specified location on the ILI9326 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_ILI9326( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as word = GLCDForeground )

  dim GLCDPrintLoc as word
  dim xchar as byte

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


'''Displays a string in a larger fonti.e.  BigPrint_ILI9326 ( 30, ILI9326_GLCD_HEIGHT - 20 , "BigPrint_ILI9326" )
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData String to display
'''@param Color Optional color
Sub BigPrint_ILI9326(In PrintLocX as Word, In PrintLocY as Word,  PrintData As String, Optional In  Color as word = GLCDForeground)
  Dim GLCDPrintLoc, SysPrintTemp as word
  Dim PrintLen as byte
  PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = 1 To PrintLen
    DrawBigChar_ILI9326 GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  Next
End Sub

'''Draws a Vertical Line on the GLCD with a
'''@param GLCDY1 Y coordinate of one end of the line
'''@param GLCDY2 Y coordinate of the other end of the line
'''@param GLCDX1 X coordinate of the line
'''@param LineColour color
Sub DrawBigChar_ILI9326 (In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  Color as word = GLCDForeground )

    dim Locx, Locy, CurrCol as Word
    dim CurrCharVal, CurrCharRow, CurrCharCol as byte

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
             PSet_ILI9326 LocX , LocY , Color
          end if
          Rotate CurrCharVal Right
        Next
    Next
End Sub
