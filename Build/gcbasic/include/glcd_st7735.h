#OPTION explicit
'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2012-2020 Hugh Considine, Giuseppe D'Elia and Evan Venn

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
' Supports ST7735 controller only.

'Changes
' 29/9/2013: Added ST7735 code (and colour support)
' 14/4/2014: Added ST7920 Support
' 22/4/2014: Revised OLD_GLCD_TYPE_ST7920_GRAPHICS_MODE variable name
' 15/10/2014: Adapted for color support for ST7735
'             Added Defines and ST7735Rotation command
' 16/10/2014: Adapted to handle screen rotation and GLCDCLS for ST7735 device.
' 27/10/2014: Changed ST7735Rotation to GLCDRotate
'
' 9/11/14 New revised version.  Requires GLCD.H.  Do not call directly.  Always load via GLCD.H
' 04/12/18    Changed to support OLED fonts and updated to 2018 controls
' 05/12/18    Revised adding GLCDCLS parameter
' 16/12/18    Revised to use TFT colors. Updated GLCDCLS to be a lot faster and updated GLCDRotate to support bits.
' 03/04/19    Revised to support DEFAULT_GLCDBACKGROUND constant
' 14/04/19    Revised Rotate to support GLCDDeviceWidth and GLCDDeviceHeight
'             Added PrintLocX and PrintLocY initialisation for character mode printing.
'             Added GLCDPrintString support
' 18/04/18    Added support for ST7735_BLACKTAB | ST7735_GREENTAB | ST7735_REDTAB.  ST7735_MINI160x80 not seen in the wide therefore not supported...yet
'             Corrected offset in SetAddress by the two constants ST7735_XSTART and ST7735_YSTART - these constants can be used to adjust (OR NOT) the X and Y position of the pixels.  Where the constant is a pixel size.
'             Added ST7735_RASET_ADJUSTMENT and ST7735_RASET_ADJUSTMENT - these constants can be used to adjust (OR NOT) the CASET and RASET registers
'             Added rotation handler to SetAddress for ST7735_XSTART and ST7735_YSTART
'             Corrected SetAddress and Rotate to use GLCDRotateState
' 03/11/2019  Added GLCD_TYPE_ST7735R_160_80 shared support
' 04/05/2022  Added Scroll support

'
'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ST7735"

'Serial lines (ST7735 only)
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735; Data/Command; GLCD_DC; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735; Chip Select; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735; Data In (LCD Out); GLCD_DI; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735; Data Out (LCD In); GLCD_DO; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735; Clock; GLCD_SCK; IO_Pin

'''@hardware GLCD_TYPE GLCD_TYPE_ST7735; Reset; GLCD_RESET; IO_Pin
'Select the correct TAB type - default to BLACKTAB

'User Changeable constants
  #IFNDEF ST7735TABCOLOR
    #DEFINE ST7735TABCOLOR ST7735_BLACKTAB     'ST7735_BLACKTAB | ST7735_GREENTAB | ST7735_REDTAB
  #ENDIF

'Pin mappings for ST7735
#define ST7735_DC GLCD_DC
#define ST7735_CS GLCD_CS
#define ST7735_RST GLCD_RESET

' lat support
#define _ST7735_DC _GLCD_DC
#define _ST7735_CS _GLCD_CS
#define _ST7735_RST _GLCD_RESET

#define ST7735_DI GLCD_DI
#define ST7735_DO GLCD_DO
#define ST7735_SCK GLCD_SCK

' lat support
#define _ST7735_DI _GLCD_DI
#define _ST7735_DO _GLCD_DO
#define _ST7735_SCK _GLCD_SCK

'Column/row select commands for ST7735
#define ST7735_COLUMN 0x2A
#define ST7735_ROW 0x2B

' Defines for ST7735
#define ST7735_GREENTAB   0x00
#define ST7735_REDTAB     0x01
#define ST7735_BLACKTAB   0x02
#define ST7735_MINI160x80 0x03


'Determine the offset from the configuration of the TAB
#script

    DEFAULTST7735_YSTARTFOUND = 0
    IF ST7735_YSTART THEN
        DEFAULTST7735_YSTARTFOUND = 1
    END IF
    IF DEFAULTST7735_YSTARTFOUND = 0 THEN
        ST7735_YSTART = 0
    END IF
    IF DEFAULTST7735_YSTARTFOUND = 0 THEN
        IF ST7735TABCOLOR = ST7735_GREENTAB THEN
            IF ST7735_YSTART = 0 THEN
              ST7735_YSTART = 2
            END IF
            IF ST7735TABCOLOR = ST7735_REDTAB THEN
              IF ST7735_YSTART = 0 THEN
                ST7735_YSTART = 2
              END IF
            END IF
        END IF
        IF GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 THEN
          'geomtery
          ST7735_YSTART = 0
        END IF
    END IF


    DEFAULTST7735_XSTARTFOUND = 0
    IF ST7735_XSTART THEN
        DEFAULTST7735_XSTARTFOUND = 1
    END IF
    IF DEFAULTST7735_XSTARTFOUND = 0 THEN
        ST7735_XSTART = 0
    END IF
    IF DEFAULTST7735_XSTARTFOUND = 0 THEN
        IF ST7735TABCOLOR = ST7735_GREENTAB THEN
            IF ST7735_XSTART = 0 THEN
              ST7735_XSTART = 2
            END IF
            IF ST7735TABCOLOR = ST7735_REDTAB THEN
              IF ST7735_XSTART = 0 THEN
                ST7735_XSTART = 2
              END IF
            END IF
        END IF
        IF GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 THEN
          'geomtery
          ST7735_XSTART = 24
        END IF

    END IF

#endscript

#DEFINE ST7735_TFTWIDTH  128
#DEFINE ST7735_TFTHEIGHT 160

' Screen rotation
#define ST7735_LANDSCAPE 1
#define ST7735_PORTRAIT_REV 2
#define ST7735_LANDSCAPE_REV 3
#define ST7735_PORTRAIT 4

#define ST7735_MADCTL_MY  0x80
#define ST7735_MADCTL_MX  0x40
#define ST7735_MADCTL_MV  0x20
#define ST7735_MADCTL_ML  0x10
#define ST7735_MADCTL_RGB 0x00
#define ST7735_MADCTL_BGR 0x08
#define ST7735_MADCTL_MH  0x04


#DEFINE ST7735_NOP     0X00
#DEFINE ST7735_SWRESET 0X01
#DEFINE ST7735_RDDID   0X04
#DEFINE ST7735_RDDST   0X09
#DEFINE ST7735_SLPIN   0X10
#DEFINE ST7735_SLPOUT  0X11
#DEFINE ST7735_PTLON   0X12
#DEFINE ST7735_NORON   0X13
#DEFINE ST7735_INVOFF  0X20
#DEFINE ST7735_INVON   0X21
#DEFINE ST7735_DISPOFF 0X28
#DEFINE ST7735_DISPON  0X29
#DEFINE ST7735_CASET   0X2A
#DEFINE ST7735_RASET   0X2B
#DEFINE ST7735_RAMWR   0X2C
#DEFINE ST7735_RAMRD   0X2E
#DEFINE ST7735_PTLAR   0X30
#DEFINE ST7735_VSCRDEF 0X33   //Vertical Scroll Mode..  Do not set TFA + VSA + BFA<162. In that case, unexpected picture will be shown.
#DEFINE ST7735_SCRLAR  0x33
#DEFINE ST7735_COLMOD  0X3A
#DEFINE ST7735_MADCTL  0X36
#DEFINE ST7735_VSCRSADD 0X37
#DEFINE ST7735_FRMCTR1 0XB1
#DEFINE ST7735_FRMCTR2 0XB2
#DEFINE ST7735_FRMCTR3 0XB3
#DEFINE ST7735_INVCTR  0XB4
#DEFINE ST7735_DISSET5 0XB6
#DEFINE ST7735_PWCTR1  0XC0
#DEFINE ST7735_PWCTR2  0XC1
#DEFINE ST7735_PWCTR3  0XC2
#DEFINE ST7735_PWCTR4  0XC3
#DEFINE ST7735_PWCTR5  0XC4
#DEFINE ST7735_VMCTR1  0XC5
#DEFINE ST7735_RDID1   0XDA
#DEFINE ST7735_RDID2   0XDB
#DEFINE ST7735_RDID3   0XDC
#DEFINE ST7735_RDID4   0XDD
#DEFINE ST7735_PWCTR6  0XFC
#DEFINE ST7735_GMCTRP1 0XE0
#DEFINE ST7735_GMCTRN1 0XE1

#DEFINE ST7735_CASET_ADJUSTMENT 0
#DEFINE ST7735_RASET_ADJUSTMENT 0

//! may not be valid for ST7735 hardware acceleration commands
#DEFINE SSD1351_SCROLL_SETUP            0X96    ' SETUP SCROLL
#DEFINE SSD1351_SCROLL_STOP             0X9E    ' SCROLL STOP
#DEFINE SSD1351_SCROLL_START            0X9F    ' SCROLL START

' Color definitions
'standard colors same as the TFT colors
#define ST7735_WHITE   TFT_WHITE
#define ST7735_BLACK   TFT_BLACK
#define ST7735_RED     TFT_RED
#define ST7735_GREEN   TFT_GREEN
#define ST7735_BLUE    TFT_BLUE
#define ST7735_YELLOW  TFT_YELLOW
#define ST7735_CYAN    TFT_CYAN
#define ST7735_MAGENTA TFT_MAGENTA
#define ST7735_NAVY          TFT_NAVY
#define ST7735_DARKGREEN     TFT_DARKGREEN
#define ST7735_DARKCYAN      TFT_DARKCYAN
#define ST7735_MAROON        TFT_MAROON
#define ST7735_PURPLE        TFT_PURPLE
#define ST7735_OLIVE         TFT_OLIVE
#define ST7735_LIGHTGREY     TFT_LIGHTGREY
#define ST7735_DARKGREY      TFT_DARKGREY
#define ST7735_ORANGE        TFT_ORANGE
#define ST7735_GREENYELLOW   TFT_GREENYELLOW
#define ST7735_PINK          TFT_PINK


#startup InitGLCD_ST7735

'''Initialise the GLCD device
Sub InitGLCD_ST7735


  #if GLCD_TYPE = GLCD_TYPE_ST7735R  or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
      'This method is called when the about GLCD types are selected.
      'Why? This is a shared library and therefore this INIT is called, but, this will be empty. Cannot avoid this.
  #endif

  #if GLCD_TYPE = GLCD_TYPE_ST7735
    'Setup code for ST7735 controllers

    'mapped to global variable
    'dim GLCDDeviceWidth, GLCDDeviceHeight as byte
    dim GLCDForeground, GLCDBackground as word

    'Pin directions
    #IFNDEF GLCD_LAT
        Dir ST7735_CS Out
        Dir ST7735_DC Out
        Dir ST7735_RST Out

        #if bit(ST7735_DI)
          Dir ST7735_DI In
        #endif
        Dir ST7735_DO Out
        Dir ST7735_SCK Out
    #endif


    #IFDEF GLCD_LAT
        Dir _ST7735_CS Out
        Dir _ST7735_DC Out
        Dir _ST7735_RST Out
        #if bit(_ST7735_DI)
          Dir _ST7735_DI In
        #endif
        Dir _ST7735_DO Out
        Dir _ST7735_SCK Out
    #endif

    #ifdef ST7735_HardwareSPI
          ' harware SPI mode
          asm showdebug SPI constant used equates to HWSPIMODESCRIPT
          SPIMode HWSPIMODESCRIPT, 0
    #endif

    'Reset display
    Set ST7735_RST On
    Wait 150 ms
    'Reset sequence (lower line for at least 10 us)
    Set ST7735_RST Off
    Wait 150 us
    Set ST7735_RST On
    Wait 150 ms
    'Software reset
    SendCommand_ST7735 ST7735_SWRESET
    Wait 150 ms

    'Software reset
    SendCommand_ST7735 ST7735_SWRESET
    Wait 150 ms

    'Out of sleep mode
    SendCommand_ST7735 ST7735_SLPOUT
    Wait 1 s

    IF ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80 then
        'ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80
        SendCommand_ST7735(ST7735_COLMOD)
        SendData_ST7735(0x05)               ; 16-bit color
        Wait 10 ms

        SendCommand_ST7735(ST7735_FRMCTR1)
        SendData_ST7735(0x00)
        SendData_ST7735(0x06)
        SendData_ST7735(0x03)
        Wait 10 ms

        SendCommand_ST7735(ST7735_MADCTL)
        SendData_ST7735(0xC0)

        SendCommand_ST7735(ST7735_DISSET5)
        SendData_ST7735(0x15)
        SendData_ST7735(0x02)
        wait 10 ms;

        SendCommand_ST7735(ST7735_INVCTR )
        SendData_ST7735(0x00);
        wait 10 ms;

        SendCommand_ST7735(ST7735_FRMCTR1)
        SendData_ST7735(0x01)
        SendData_ST7735(0x2C)
        SendData_ST7735(0x2D)
        Wait 10 ms

        SendCommand_ST7735(ST7735_PWCTR1)
        SendData_ST7735(0x02)
        SendData_ST7735(0x70)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR2)
        SendData_ST7735(0x05)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR3)
        SendData_ST7735(0x0A)
        SendData_ST7735(0x02)
        wait 10 ms;

        SendCommand_ST7735(ST7735_VMCTR1)
        SendData_ST7735(0x3c)
        SendData_ST7735(0x38)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR6)
        SendData_ST7735(0x11)
        SendData_ST7735(0x15);
        wait 10 ms;

        SendCommand_ST7735(ST7735_GMCTRP1);
          SendData_ST7735(0x09);
          SendData_ST7735(0x16);
          SendData_ST7735(0x09);
          SendData_ST7735(0x20);
          SendData_ST7735(0x21);
          SendData_ST7735(0x1B);
          SendData_ST7735(0x13);
          SendData_ST7735(0x19);
          SendData_ST7735(0x17);
          SendData_ST7735(0x15);
          SendData_ST7735(0x1E);
          SendData_ST7735(0x2B);
          SendData_ST7735(0x04);
          SendData_ST7735(0x05);
          SendData_ST7735(0x02);
          SendData_ST7735(0x0E);
          wait 10 ms;
        SendCommand_ST7735(ST7735_GMCTRN1);
          SendData_ST7735(0x0B);
          SendData_ST7735(0x14);
          SendData_ST7735(0x08);
          SendData_ST7735(0x1E);
          SendData_ST7735(0x22);
          SendData_ST7735(0x1D);
          SendData_ST7735(0x18);
          SendData_ST7735(0x1E);
          SendData_ST7735(0x1B);
          SendData_ST7735(0x1A);
          SendData_ST7735(0x24);
          SendData_ST7735(0x2B);
          SendData_ST7735(0x06);
          SendData_ST7735(0x06);
          SendData_ST7735(0x02);
          SendData_ST7735(0x0F);
          wait 10 ms;

        SendCommand_ST7735(ST7735_CASET)
          SendData_ST7735(0x00)
          SendData_ST7735(0x02)
          SendData_ST7735(0x00)
          SendData_ST7735(0x81)
          wait 20 ms

        SendCommand_ST7735(ST7735_RASET)
          SendData_ST7735(0x00)
          SendData_ST7735(0x02)
          SendData_ST7735(0x00)
          SendData_ST7735(0x81)
          wait 20 ms

    else
        'red or green tab

        SendCommand_ST7735(ST7735_FRMCTR1)
        SendData_ST7735(0x01)
        SendData_ST7735(0x2C)
        SendData_ST7735(0x2D)
        Wait 10 ms

        SendCommand_ST7735(ST7735_FRMCTR2)
        SendData_ST7735(0x01)
        SendData_ST7735(0x2C)
        SendData_ST7735(0x2D)
        Wait 10 ms

        SendCommand_ST7735(ST7735_FRMCTR3)
        SendData_ST7735(0x01)
        SendData_ST7735(0x2C)
        SendData_ST7735(0x2D)
        Wait 10 ms

        SendCommand_ST7735(ST7735_INVCTR)
        SendData_ST7735(0x07)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR1)
        SendData_ST7735(0x02)
        SendData_ST7735(0x70)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR2)
        SendData_ST7735(0x05)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR3)
        SendData_ST7735(0x0A)
        SendData_ST7735(0x02)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR4)
        SendData_ST7735(0x8A)
        SendData_ST7735(0x2A)
        wait 10 ms;

        SendCommand_ST7735(ST7735_PWCTR5)
        SendData_ST7735(0x8A)
        SendData_ST7735(0xEE)
        wait 10 ms;

        SendCommand_ST7735(ST7735_VMCTR1)
        SendData_ST7735(0x0E)
        wait 10 ms;

        SendCommand_ST7735(ST7735_INVOFF)
        wait 10 ms;


        SendCommand_ST7735(ST7735_MADCTL)
        'Assumed to be GreenTab and RedTab
        SendData_ST7735(0xC8)     'row address/col address, bottom to top refresh/GBR.
        wait 20 ms

        SendCommand_ST7735(ST7735_COLMOD)
        SendData_ST7735(0x05)

        SendCommand_ST7735(ST7735_CASET)
        IF ST7735TABCOLOR = ST7735_GREENTAB then
          'Assumed to be GreenTab
          SendData_ST7735(0x00)
          SendData_ST7735(ST7735_CASET_ADJUSTMENT)
          SendData_ST7735(0x00)
          SendData_ST7735(0x7f+ST7735_CASET_ADJUSTMENT)
        end if
        IF ST7735TABCOLOR = ST7735_REDTAB then
          'Assumed to be RedTab
          SendData_ST7735(0x00)
          SendData_ST7735(0x00)
          SendData_ST7735(0x00)
          SendData_ST7735(0x7f)
        end if

        wait 20 ms

        SendCommand_ST7735(ST7735_RASET)
        IF ST7735TABCOLOR = ST7735_GREENTAB then
          'Assumed to be GreenTab
          SendData_ST7735(0x00)
          SendData_ST7735(0x00+ST7735_RASET_ADJUSTMENT)
          SendData_ST7735(0x00)
          SendData_ST7735(0x9f+ST7735_RASET_ADJUSTMENT)
        end if
        IF ST7735TABCOLOR = ST7735_REDTAB then
          'Assumed to be RedTab
          SendData_ST7735(0x00)
          SendData_ST7735(0x00)
          SendData_ST7735(0x00)
          SendData_ST7735(0x9f)
        end if
        wait 20 ms


        SendCommand_ST7735(ST7735_GMCTRP1)
        SendData_ST7735(0x02)
        SendData_ST7735(0x1c)
        SendData_ST7735(0x07)
        SendData_ST7735(0x12)
        SendData_ST7735(0x37)
        SendData_ST7735(0x32)
        SendData_ST7735(0x29)
        SendData_ST7735(0x2d)
        SendData_ST7735(0x29)
        SendData_ST7735(0x25)
        SendData_ST7735(0x2b)
        SendData_ST7735(0x39)
        SendData_ST7735(0x00)
        SendData_ST7735(0x01)
        SendData_ST7735(0x03)
        SendData_ST7735(0x10)
        wait 10 ms
        SendCommand_ST7735(ST7735_GMCTRN1)
        SendData_ST7735(0x03)
        SendData_ST7735(0x1d)
        SendData_ST7735(0x07)
        SendData_ST7735(0x06)
        SendData_ST7735(0x2e)
        SendData_ST7735(0x2c)
        SendData_ST7735(0x29)
        SendData_ST7735(0x2d)
        SendData_ST7735(0x2e)
        SendData_ST7735(0x2e)
        SendData_ST7735(0x37)
        SendData_ST7735(0x37)
        SendData_ST7735(0x00)
        SendData_ST7735(0x00)
        SendData_ST7735(0x02)
        SendData_ST7735(0x10)
        wait 10 ms

    end if


    'Display on
    SendCommand_ST7735 ST7735_NORON
    Wait 100 ms

    'Display on
    SendCommand_ST7735 ST7735_DISPON
    Wait 100 ms


    'Colours
    GLCDForeground = TFT_WHITE
    'Default Colours
    #ifdef DEFAULT_GLCDBACKGROUND
      GLCDBackground = DEFAULT_GLCDBACKGROUND
    #endif

    #ifndef DEFAULT_GLCDBACKGROUND
      GLCDBackground = TFT_BLACK
    #endif

    'Variables required for device
    GLCDDeviceWidth = GLCD_WIDTH - 1
    GLCDDeviceHeight = GLCD_HEIGHT - 1

    #ifndef GLCD_OLED_FONT
      GLCDFontWidth = 6
    #endif

    #ifdef GLCD_OLED_FONT
      GLCDFontWidth = 5
    #endif




    GLCDfntDefault = 0
    GLCDfntDefaultsize = 1
    GLCDfntDefaultHeight = 8

    GLCDRotate ( PORTRAIT )
    'Clear screen
    GLCDCLS

  #endif



End Sub


'Subs
'''Clears the GLCD screen
Sub GLCDCLS_ST7735 ( Optional In  GLCDBackground as word = GLCDBackground)

  ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
  GLCD_yordinate = 0
  Dim PrintLocX, PrintLocY as word
  PrintLocX = 0
  PrintLocY = 0


  #if GLCD_TYPE = GLCD_TYPE_ST7735
    SetAddress_ST7735 ST7735_COLUMN, 0, GLCDDeviceWidth
    wait 2 ms
    SetAddress_ST7735 ST7735_ROW, 0, GLCDDeviceHeight
    wait 2 ms
    SendCommand_ST7735 ST7735_RAMWR
    wait 2 ms
    Repeat [word] GLCD_WIDTH * GLCD_HEIGHT
      SendWord_ST7735 GLCDBackground
    End Repeat
  #endif

  #if GLCD_TYPE = GLCD_TYPE_ST7735R or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
    SetAddress_ST7735 ST7735_COLUMN, 0, GLCDDeviceWidth
    wait 2 ms
    SetAddress_ST7735 ST7735_ROW, 0, GLCDDeviceHeight
    wait 2 ms
    SendCommand_ST7735 ST7735_RAMWR
    wait 2 ms
    Repeat [word] GLCD_WIDTH * GLCD_HEIGHT
      SendWord_ST7735 GLCDBackground
    End Repeat
  #endif

End Sub

'''Draws a string at the specified location on the GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_ST7735( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as word = GLCDForeground )

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
Sub GLCDDrawChar_ST7735(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This has got a tad complex
  'We have three major pieces
  '1 The preamble - this just adjusted color and the input character
  '2 The code that deals with GCB fontset
  '3 The code that deals with OLED fontset
  '
  'You can make independent change to section 2 and 3 but they are mutual exclusive with many common pieces

   dim CharCol, CharRow, GLCDTemp as word
   dim Currcharcol, CurrCharVal, CURRCHARROW, COL, CHARROWS, ROW, ST7735TEMPOUT, CHARCOLS as Byte
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
Sub FilledBox_ST7735(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

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

  #if GLCD_TYPE = GLCD_TYPE_ST7735
    'Set address window
    SetAddress_ST7735 ST7735_COLUMN, LineX1, LineX2
    SetAddress_ST7735 ST7735_ROW, LineY1, LineY2
    'Fill with colour
    Dim GLCDPixelCount As Word
    GLCDPixelCount = (LineX2 - LineX1 + 1) * (LineY2 - LineY1 + 1)
    SendCommand_ST7735 0x2C
    Repeat GLCDPixelCount
      SendWord_ST7735 LineColour
    End Repeat
  #endif

  #if GLCD_TYPE = GLCD_TYPE_ST7735R
    'Set address window
    SetAddress_ST7735 ST7735_COLUMN, LineX1, LineX2
    SetAddress_ST7735 ST7735_ROW, LineY1, LineY2
    'Fill with colour
    Dim GLCDPixelCount As Word
    GLCDPixelCount = (LineX2 - LineX1 + 1) * (LineY2 - LineY1 + 1)
    SendCommand_ST7735 0x2C
    Repeat GLCDPixelCount
      SendWord_ST7735 LineColour
    End Repeat
  #endif

  #if GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
    'Set address window
    SetAddress_ST7735 ST7735_COLUMN, LineX1, LineX2
    SetAddress_ST7735 ST7735_ROW, LineY1, LineY2
    'Fill with colour
    Dim GLCDPixelCount As Word
    GLCDPixelCount = (LineX2 - LineX1 + 1) * (LineY2 - LineY1 + 1)
    SendCommand_ST7735 0x2C
    Repeat GLCDPixelCount
      SendWord_ST7735 LineColour
    End Repeat
  #endif

End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
Sub PSet_ST7735(In GLCDX, In GLCDY, In GLCDColour As Word)

  #if GLCD_TYPE = GLCD_TYPE_ST7735
    SetAddress_ST7735 ST7735_COLUMN, GLCDX, GLCDX
    SetAddress_ST7735 ST7735_ROW, GLCDY, GLCDY
    SendCommand_ST7735 0x2C
    SendWord_ST7735 GLCDColour
  #endif
End Sub



'''Transfer a byte
'''@hide
Sub Transfer_ST7735(In ST7735TempIn, Out ST7735TempOut)

  'Use mode 0 - CPOL = 0, CPHA = 0
          'Set SSPSTAT.CKE On
          'Set SSPCON1.CKP Off

    SET ST7735_SCK OFF
    repeat 8                      '8 data bits
      Wait 1 us
      if ST7735TempIn.7 = ON then      'put most significant bit on SDA line
        set ST7735_DO ON
      else
        set ST7735_DO OFF
      end if

      rotate ST7735TempIn left         'shift in bit for the next time
      SET ST7735_SCK ON                'now clock bit out
      Wait 1 us
      SET ST7735_SCK OFF               'done clocking that bit

    end repeat
    Wait 1 us
    SET ST7735_SCK OFF
End Sub

'''Send a command to the ST7735 GLCD
'''@param ST7735SendByte Command to send
'''@hide
sub  SendCommand_ST7735( IN ST7735SendByte as byte )

  set ST7735_CS OFF;

  set ST7735_DC OFF;

  #ifdef ST7735_HardwareSPI
     SPITransfer  ST7735SendByte,  ST7735TempOut
     set ST7735_CS ON;
     exit sub
  #endif

  #ifndef ST7735_HardwareSPI
  repeat 8

    if ST7735SendByte.7 = ON  then
      set ST7735_DO ON;
    else
      set ST7735_DO OFF;
    end if

    SET GLCD_SCK OFF;

    rotate ST7735SendByte left
    set GLCD_SCK ON;


  end repeat
  set ST7735_CS ON;
  #endif
end Sub

'''Send a data byte to the ST7735 GLCD
'''@param ST7735SendByte Byte to send
'''@hide
sub  SendData_ST7735( IN ST7735SendByte as byte )

  set ST7735_CS OFF;

  set ST7735_DC ON;

  #ifdef ST7735_HardwareSPI
     SPITransfer  ST7735SendByte,  ST7735TempOut
     set ST7735_CS ON;
     exit sub
  #endif

  #ifndef ST7735_HardwareSPI
  repeat 8

    if ST7735SendByte.7 = ON then
      set ST7735_DO ON;
    else
      set ST7735_DO OFF;
    end if

    SET GLCD_SCK OFF;

    rotate ST7735SendByte left
    set GLCD_SCK ON;

  end Repeat
  set ST7735_CS ON;
  #endif
end Sub

'''Send a data word (16 bits) to the ST7735 GLCD
'''@param ST7735SendByte Word to send
'''@hide
Sub SendWord_ST7735(In ST7735SendWord As Word)
  set ST7735_CS OFF;

  set ST7735_DC ON;

  #ifdef ST7735_HardwareSPI
     SPITransfer  ST7735SendWord_H,  ST7735TempOut
     SPITransfer  ST7735SendWord,  ST7735TempOut
     set ST7735_CS ON;
     exit sub
  #endif

  #ifndef ST7735_HardwareSPI
  repeat 16

    if ST7735SendWord.15 = ON then
      set ST7735_DO ON;
    else
      set ST7735_DO OFF;
    end if

    SET GLCD_SCK OFF;

    rotate ST7735SendWord left
    set GLCD_SCK ON;


  end repeat
  set ST7735_CS ON;
  #endif
End Sub


'''Set the row or column address range for the ST7735 GLCD
'''@param ST7735AddressType Address Type (ST7735_ROW or ST7735_COLUMN)
'''@param ST7735Start Starting address
'''@param ST7735End Ending address
'''@hide
Sub SetAddress_ST7735(In ST7735AddressType, In ST7735Start As Word, In ST7735End As Word)

    if GLCDRotateState.0 = 0 then
      if ST7735AddressType = ST7735_COLUMN then
          ST7735Start += ST7735_XSTART
          ST7735End   += ST7735_XSTART
      end if
      if ST7735AddressType = ST7735_ROW then
          ST7735Start += ST7735_YSTART
          ST7735End   += ST7735_YSTART
      end if
    else
      if ST7735AddressType = ST7735_COLUMN then
          ST7735Start += ST7735_YSTART
          ST7735End   += ST7735_YSTART
      end if
      if ST7735AddressType = ST7735_ROW then
          ST7735Start += ST7735_XSTART
          ST7735End   += ST7735_XSTART
      end if

    end if

  SendCommand_ST7735 ST7735AddressType
  SendData_ST7735 ST7735Start_H
  SendData_ST7735 ST7735Start
  SendData_ST7735 ST7735End_H
  SendData_ST7735 ST7735End
End Sub

'''@hide
sub   GLCDRotate_ST7735 ( in GLCDRotateState )

  SendCommand_ST7735 ( ST7735_MADCTL )
  select case GLCDRotateState
        case LANDSCAPE
            IF ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80 or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 then
             SendData_ST7735( ST7735_MADCTL_MX | ST7735_MADCTL_MV |  ST7735_MADCTL_RGB )
            else
             SendData_ST7735( ST7735_MADCTL_MX | ST7735_MADCTL_MV |  ST7735_MADCTL_BGR )
            end if
            GLCDDeviceWidth = GLCD_HEIGHT - 1
            GLCDDeviceHeight = GLCD_WIDTH - 1

        case PORTRAIT_REV
            GLCDDeviceWidth = GLCD_WIDTH - 1
            GLCDDeviceHeight = GLCD_HEIGHT - 1
            IF ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80 or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 then
              SendData_ST7735( ST7735_MADCTL_RGB )
            else
              SendData_ST7735( ST7735_MADCTL_BGR )
            end if
        case LANDSCAPE_REV
            IF ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80 or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 then
              SendData_ST7735( ST7735_MADCTL_MV | ST7735_MADCTL_MY | ST7735_MADCTL_RGB )
            else
              SendData_ST7735( ST7735_MADCTL_MV | ST7735_MADCTL_MY | ST7735_MADCTL_BGR )
            end if
            GLCDDeviceWidth = GLCD_HEIGHT - 1
            GLCDDeviceHeight = GLCD_WIDTH - 1
        case PORTRAIT
            GLCDDeviceWidth = GLCD_WIDTH - 1
            GLCDDeviceHeight = GLCD_HEIGHT - 1
            IF ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80 or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 then
              SendData_ST7735( ST7735_MADCTL_MX | ST7735_MADCTL_MY | ST7735_MADCTL_RGB )
            else
              SendData_ST7735( ST7735_MADCTL_MX | ST7735_MADCTL_MY | ST7735_MADCTL_BGR )
            end if
        case else
            GLCDDeviceWidth = GLCD_WIDTH - 1
            GLCDDeviceHeight = GLCD_HEIGHT - 1
            IF ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80 or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 then
              SendData_ST7735( ST7735_MADCTL_MX | ST7735_MADCTL_MY | ST7735_MADCTL_RGB )
            else
              SendData_ST7735( ST7735_MADCTL_MX | ST7735_MADCTL_MY | ST7735_MADCTL_BGR )
            end if

  end select

end sub

'''@hide
Sub SetGammaCorrection_ST7735

  SendCommand_ST7735 0x26
  SendData_ST7735 0x04

  SendCommand_ST7735 0xF2
  SendData_ST7735 0x01

  'Send commands and values from table
  For ST7735ByteNo = 1 to 32
    ReadTable ST7735GammaCorrection, ST7735ByteNo, ST7735Byte
    If ST7735ByteNo = 1 Then
      SendCommand_ST7735 ST7735Byte
    Else
      If ST7735ByteNo = 17 Then
        Wait 50 ms
        SendCommand_ST7735 ST7735Byte
      Else
        SendData_ST7735 ST7735Byte
      End If
    End If
  Next
End Sub


Sub GLCDSetScrollDefinition_ST7735(  top_fix_height,  bottom_fix_height,    scroll_direction)

  Dim scroll_height as Word
  scroll_height = ST7735_TFTHEIGHT - top_fix_height - bottom_fix_height + 2

  SendCommand_ST7735(ST7735_SCRLAR)
  SendWord_ST7735(top_fix_height)
  SendWord_ST7735(scroll_height)
  SendWord_ST7735(bottom_fix_height)

  SendCommand_ST7735(ST7735_MADCTL)
  If( scroll_direction ) Then
      SendData_ST7735(0xD0)
  Else
      SendData_ST7735(0xC0)
  End if
End Sub

Sub GLCDVerticalScroll_ST7735(  vsp )
  SendCommand_ST7735(ST7735_VSCRSADD)
  SendWord_ST7735( vsp )
End Sub

'Numbers taken from Arduino_LCD.cpp
Table ST7735GammaCorrection
  'For CMCTRP1 (command E0)
  0xE0
  0x28
  0x24
  0x22
  0x31
  0x2b
  0x0e
  0x53
  0xa5
  0x42
  0x16
  0x18
  0x12
  0x1a
  0x14
  0x03

    'For GMCTRN1 (command E1)
    0xE1
  0x17
  0x1b
  0x1d
  0x0e
  0x14
  0x11
  0x2c
  0xa5
  0x3d
  0x09
  0x27
  0x2d
  0x25
  0x2b
  0x3c
End Table
