'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2021 Evan Venn

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
' Supports ST7789 controller only.

'Changes
' 25/02/21  Initial release
' 03/03/21  Added HWSPIClockMode constant

' Hardware settings
' Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ST7789_240_240"

'Serial lines (ST7789 only)
'''@hardware GLCD_TYPE GLCD_TYPE_ST7789_240_240; Data/Command; GLCD_DC; IO_Pin

''@hardware GLCD_TYPE GLCD_TYPE_ST7789_240_240; Data In (LCD Out) GLCD_DI; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7789_240_240; Clock; GLCD_SCK; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7789_240_240; Reset; GLCD_RESET; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7789_240_240; Chip Select; GLCD_CS; IO_Pin Optional

'Pin mappings for ST7789
#define ST7789_DC GLCD_DC
#define ST7789_RST GLCD_RESET

#define ST7789_DI GLCD_DI ;DI IS NOT USED within the library - presence for completeness
#define ST7789_DO GLCD_DO
#define ST7789_SCK GLCD_SCK

#define HWSPIClockMode 3

#SCRIPT
    'A little script to fix if the constant is missing
    SCRIPT_GLCD_CS_DEFINED = 0
    If GLCD_CS then
        ST7789_CS = GLCD_CS
    End if
#ENDSCRIPT

'Functional constants
#define ST7789_NOP     0x00
#define ST7789_SWRESET 0x01

#define ST7789_SLPIN   0x10  // sleep on
#define ST7789_SLPOUT  0x11  // sleep off
#define ST7789_PTLON   0x12  // partial on
#define ST7789_NORON   0x13  // partial off
#define ST7789_INVOFF  0x20  // invert off
#define ST7789_INVON   0x21  // invert on
#define ST7789_DISPOFF 0x28  // display off
#define ST7789_DISPON  0x29  // display on
#define ST7789_IDMOFF  0x38  // idle off
#define ST7789_IDMON   0x39  // idle on

#define ST7789_CASET   0x2A
#define ST7789_RASET   0x2B
#define ST7789_RAMWR   0x2C
#define ST7789_RAMRD   0x2E

#define ST7789_COLMOD  0x3A
#define ST7789_MADCTL  0x36

#define ST7789_PTLAR    0x30   // partial start/end
#define ST7789_VSCRDEF  0x33   // SETSCROLLAREA
#define ST7789_VSCRSADD 0x37

#define ST7789_WRDISBV  0x51
#define ST7789_WRCTRLD  0x53
#define ST7789_WRCACE   0x55
#define ST7789_WRCABCMB 0x5e

#define ST7789_POWSAVE    0xbc
#define ST7789_DLPOFFSAVE 0xbd

// bits in MADCTL
#define ST7789_MADCTL_MY  0x80
#define ST7789_MADCTL_MX  0x40
#define ST7789_MADCTL_MV  0x20
#define ST7789_MADCTL_ML  0x10
#define ST7789_MADCTL_RGB 0x00

#define ST7789_240x240_XSTART 0
#define ST7789_240x240_YSTART 0



'''Initialise the GLCD device
#startup InitGLCD_ST7789, 98


'''Initialise the GLCD device
Sub InitGLCD_ST7789

  #if GLCD_TYPE = GLCD_TYPE_ST7789_240_240

    'Setup code for ST7789 controllers
    'SPI mode!!!
    'Pin directions
    #ifdef ST7789_CS
       Dir ST7789_CS Out
    #endif
    Dir ST7789_DC Out
    Dir ST7789_RST Out

    #if bit(ST7789_DI)
      Dir ST7789_DI In
    #endif
    Dir ST7789_DO Out
    Dir ST7789_SCK Out

    #ifdef ST7789_HardwareSPI
      ' harware SPI mode
      asm showdebug SPI constant used equates to HWSPIMODESCRIPT
      SPIMode HWSPIMODESCRIPT, HWSPIClockMode
    #endif

    #ifndef ST7789_HardwareSPI
      Set ST7789_SCK  on
    #endif

    #ifdef  ST7789_CS
      Set ST7789_CS On
    #endif
    Set ST7789_DC On

    'Reset display
    Wait 150 ms
    Set ST7789_RST On
    Wait 150 ms
    'Reset sequence (lower line for at least 10 us)
    Set ST7789_RST Off
    Wait 150 us
    Set ST7789_RST On
    Wait 150 ms

    SendCommand_ST7789(ST7789_SLPOUT )
    Wait 255 ms

    SendCommand_ST7789(ST7789_COLMOD)
    SendData_ST7789(0x55)
    Wait 10 ms

    SendCommand_ST7789(ST7789_MADCTL )
    SendData_ST7789(ST7789_MADCTL_RGB)


    SendCommand_ST7789(ST7789_CASET)
    SendData_ST7789(0x00)
    SendData_ST7789(ST7789_240x240_XSTART)
    SendWord_ST7789 ( ST7789_240x240_XSTART + GLCD_WIDTH )

    SendCommand_ST7789(ST7789_RASET)
    SendData_ST7789(0x00)
    SendData_ST7789(ST7789_240x240_YSTART)
    SendWord_ST7789 ( ST7789_240x240_YSTART + GLCD_HEIGHT )

    SendCommand_ST7789(ST7789_INVON)
    Wait 10 ms

    SendCommand_ST7789(ST7789_NORON)
    Wait 10 ms

    SendCommand_ST7789(ST7789_DISPON)
    Wait 20 ms

    'Default Colours
    #ifdef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
    #endif

    #ifndef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = TFT_BLACK
    #endif

    GLCDForeground = TFT_WHITE

    'Variables required for device
    ST7789_GLCD_WIDTH = GLCD_WIDTH
    ST7789_GLCD_HEIGHT = GLCD_HEIGHT
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

  GLCDRotateState = Portrait
  'Clear screen
  GLCDCLS

End Sub


'Subs
'''Clears the GLCD screen
Sub GLCDCLS_ST7789 ( Optional In  GLCDBACKGROUND as word = GLCDBACKGROUND )

    dim ST7789SendWord as word
    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0

    SetAddressWindow_ST7789 ( 0, 0, 320 , 320 )

    ST7789SendWord = GLCDBACKGROUND

    #IFDEF AVR
        'VARIABLE IS USES FOR FAST CLEAR SCREEN FOR AVR ONLY
        HWSPI_Send_Word = GLCDBACKGROUND
    #ENDIF

    #ifdef  ST7789_CS
      ST7789_CS = 0
    #endif

    'SPI mode
    set ST7789_DC ON


    repeat 320

      repeat 320

        #ifdef ST7789_HardwareSPI

          #ifdef PIC

            #ifndef bit(WCOL)
                FastHWSPITransfer  ST7789SendWord_h
                FastHWSPITransfer  ST7789SendWord
            #endif

            #ifdef bit(WCOL)

                #ifndef Var(SSPCON1)
                  #ifdef Var(SSPCON)
                    Dim SSPCON1 Alias SSPCON
                  #endif
                #endif
                'Clear WCOL
                Set SSPCON1.WCOL Off
                'Put byte to send into buffer
                'Will start transfer
                SSPBUF = ST7789SendWord_h
                Wait While SSPSTAT.BF = Off
                Set SSPSTAT.BF Off
                #if ChipFamily 16
                  ST7789TempOut = SSPBUF
                #endif


                'Clear WCOL
                Set SSPCON1.WCOL Off
                'Put byte to send into buffer
                'Will start transfer
                SSPBUF = ST7789SendWord
                Wait While SSPSTAT.BF = Off
                Set SSPSTAT.BF Off
                #if ChipFamily 16
                  ST7789TempOut = SSPBUF
                #endif

            #endif

          #endif


          #ifdef AVR
            'Fast clear screen - use a macro to keep code simple
            HWSPI_Fast_Write_Word_Macro GLCDBACKGROUND
          #endif

        #endif

        #ifndef ST7789_HardwareSPI
              SendWord_ST7789 ( GLCDBACKGROUND )
        #endif




      end repeat

    end repeat

    #ifdef  ST7789_CS
      ST7789_CS = 1
    #endif

End Sub



'''Draws a string at the specified location on the GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_ST7789( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as word = GLCDForeground )

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

'''Draws a character at the specified location on the ST7920 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_ST7789(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

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
                           PSet [word]CharLocX + CharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, GLCDBACKGROUND
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
                  CurrCharVal = GLCDBACKGROUND
                #endif
          End Select

            'we handle 8 or 16 pixels of height
            For CurrCharRow = 0 to ROWSperfont
                'Set the pixel
                If CurrCharVal.0 = 0 Then
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBACKGROUND
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
                    CurrCharVal = GLCDBACKGROUND
                  #endif
                end if

                'It is the intercharacter space, put out one pixel row
                if CurrCharCol = COLSperfont then
                    'Put out a white intercharacter pixel/space
                     GLCDTemp = CharLocX + CurrCharCol
                     if GLCDfntDefaultSize = 2 then
                        GLCDTemp++
                     end if
                     PSet GLCDTemp , CharLocY + CurrCharRow, GLCDBACKGROUND
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
Sub FilledBox_ST7789(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)
  dim GLCDTemp as word
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


    'Set address window
    SetAddressWindow_ST7789 (  LineX1, LineY1, LineX2, LineY2 )
    'Fill with colour
    Dim GLCDPixelCount As Long
    GLCDPixelCount = (LineX2 - LineX1 + 1) * (LineY2 - LineY1 + 1)
    Repeat GLCDPixelCount
      SendWord_ST7789 LineColour
    End Repeat

End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
Sub PSet_ST7789(In GLCDX as word, In GLCDY as word, In GLCDColour As Word)


   select case GLCDRotateState

   case PORTRAIT_REV

        GLCDY = GLCDY + 80

   case LANDSCAPE_REV

        GLCDX = GLCDX + 80

   end select


    SetAddressWindow_ST7789 ( GLCDX, GLCDY, GLCDX, GLCDY )
    SendWord_ST7789 GLCDColour

End Sub


'''Set the row or column address range for the ST7789 GLCD
'''@param ST7789AddressType Address Type (ST7789_ROW or ST7789_COLUMN)
'''@param ST7789Start Starting address
'''@param ST7789End Ending address
'''@hide
Sub SetAddressWindow_ST7789( In _x1 as word, in _y1 as word, in _x2 as word, in _y2 as word)

  SendCommand_ST7789(ST7789_CASET); // Column addr set
  SendData_ST7789 _x1_H
  SendData_ST7789 _x1
  SendData_ST7789 _x2_H
  SendData_ST7789 _x2

  SendCommand_ST7789(ST7789_RASET); // Row addr set
  SendData_ST7789 _y1_H
  SendData_ST7789 _y1
  SendData_ST7789 _y2_H
  SendData_ST7789 _y2

  SendCommand_ST7789(ST7789_RAMWR); // write to RAM

End Sub




'''Send a command to the ST7789 GLCD
'''@param ST7789SendByte Command to send
'''@hide
sub  SendCommand_ST7789( IN ST7789SendByte as byte )

      Dim ST7789TempOut as byte

      'SPI mode
      #ifdef  ST7789_CS
        set ST7789_CS OFF
      #endif
      set ST7789_DC OFF;

      #ifdef ST7789_HardwareSPI
         SPITransfer  ST7789SendByte,  ST7789TempOut
         #ifdef  ST7789_CS
          set ST7789_CS ON
         #endif

      #endif

      #ifndef ST7789_HardwareSPI
          'SPI MODE 2!!
          set GLCD_SCK On;
          repeat 8

            if ST7789SendByte.7 = ON  then
              set ST7789_DO ON;
            else
              set ST7789_DO OFF;
            end if
            SET GLCD_SCK Off;
            rotate ST7789SendByte left
            set GLCD_SCK On;

          end repeat
          #ifdef  ST7789_CS
            set ST7789_CS ON;
          #endif
      #endif

end Sub

'''Send a data byte to the ST7789 GLCD
'''@param ST7789SendByte Byte to send
'''@hide
sub  SendData_ST7789( IN ST7789SendByte as byte )


      'SPI mode
      #ifdef  ST7789_CS
        set ST7789_CS OFF;
      #endif
      set ST7789_DC ON;

      #ifdef ST7789_HardwareSPI
         SPITransfer  ST7789SendByte, ST7789TempOut
         #ifdef  ST7789_CS
          set ST7789_CS ON;
         #endif

      #endif

      #ifndef ST7789_HardwareSPI
          'SPI MODE 2!!
          set GLCD_SCK On;
          repeat 8

            if ST7789SendByte.7 = ON then
              set ST7789_DO ON;
            else
              set ST7789_DO OFF;
            end if
            SET GLCD_SCK Off;
            rotate ST7789SendByte left
            set GLCD_SCK On;

          end Repeat
          #ifdef  ST7789_CS
            set ST7789_CS ON;
          #endif
      #endif

end Sub



'''Send a data word (16 bits) to the ST7789 GLCD
'''@param ST7789SendByte Word to send
'''@hide
Sub SendWord_ST7789(In ST7789SendWord As Word)

    dim ST7789SendWord as word

    'SPI mode
      #ifdef  ST7789_CS
        set ST7789_CS OFF
      #endif
      set ST7789_DC ON;

      #ifdef ST7789_HardwareSPI
         SPITransfer  ST7789SendWord_H,  ST7789TempOut
         SPITransfer  ST7789SendWord,  ST7789TempOut
         #ifdef  ST7789_CS
          set ST7789_CS ON;
         #endif

      #endif

      #ifndef ST7789_HardwareSPI
          'SPI MODE 2!!
          set GLCD_SCK On;
          repeat 16

            if ST7789SendWord.15 = ON then
              set ST7789_DO ON;
            else
              set ST7789_DO OFF;
            end if
            SET GLCD_SCK Off;
            rotate ST7789SendWord left
            set GLCD_SCK On;

          end repeat
          #ifdef  ST7789_CS
            set ST7789_CS ON
          #endif
      #endif

End Sub



'''@hide
sub   GLCDRotate_ST7789 ( in GLCDRotateState )

  SendCommand_ST7789 ( ST7789_MADCTL )
  select case GLCDRotateState
        case PORTRAIT
             ST7789_GLCD_WIDTH = GLCD_WIDTH
             ST7789_GLCD_HEIGHT = GLCD_HEIGHT
             SendData_ST7789( ST7789_MADCTL_RGB)
        case LANDSCAPE
             ST7789_GLCD_WIDTH = GLCD_HEIGHT
             ST7789_GLCD_HEIGHT = GLCD_WIDTH
             SendData_ST7789( ST7789_MADCTL_MV | ST7789_MADCTL_MX | ST7789_MADCTL_RGB )

        case PORTRAIT_REV
             ST7789_GLCD_WIDTH = GLCD_WIDTH
             ST7789_GLCD_HEIGHT = GLCD_HEIGHT
             SendData_ST7789( ST7789_MADCTL_MX | ST7789_MADCTL_MY | ST7789_MADCTL_RGB)

        case LANDSCAPE_REV
             ST7789_GLCD_WIDTH = GLCD_HEIGHT
             ST7789_GLCD_HEIGHT = GLCD_WIDTH
             SendData_ST7789(  ST7789_MADCTL_MV | ST7789_MADCTL_MY | ST7789_MADCTL_RGB )

        case else
             ST7789_GLCD_WIDTH = GLCD_WIDTH
             ST7789_GLCD_HEIGHT = GLCD_HEIGHT
             SendData_ST7789( ST7789_MADCTL_RGB)
  end select

end sub
