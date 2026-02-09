'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2018-2026 Evan Venn

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
' Supports ST7735R controller only.

'Changes
' 10/09/2019  Initial release
' 03/11/2019  Added GLCD_TYPE_ST7735R_160_80 support
' 06/11/2019  Revised to add explicit GLCD_TYPE_ST7735R_160_80 support - assumes Green Tab
' 07/11/2019  Revised to resolve interline space for GLCD_TYPE_ST7735R_160_80 support
' 13/11/2019  Corrected GLCD_TYPE_ST7735R_160_80 asm debug message error


'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ST7735R | LCD_TYPE_ST7735R_160_80"

'Serial lines (ST7735R only)
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735R; Data/Command; GLCD_DC; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735R; Chip Select; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735R; Data In (LCD Out); GLCD_DI; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735R; Data Out (LCD In); GLCD_DO; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7735R; Clock; GLCD_SCK; IO_Pin


'Pin mappings for ST7735R
#define ST7735_DC GLCD_DC
#define ST7735_CS GLCD_CS
#define ST7735_RST GLCD_RESET
#define ST7735_DI GLCD_DI
#define ST7735_DO GLCD_DO
#define ST7735_SCK GLCD_SCK

#startup InitGLCD_ST7735R


'''Initialise the GLCD device
'This is the same as the InitGLCD_ST7735 apart from the power control.
Sub InitGLCD_ST7735R

  #if GLCD_TYPE = GLCD_TYPE_ST7735R  or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
    'Setup code for GLCD_TYPE_ST7735R  or GLCD_TYPE_ST7735R_160_80

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


    #if GLCD_TYPE =  GLCD_TYPE_ST7735R_160_80
              asm showdebug _GLCD_TYPE_ST7735R_160_80

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
              SendData_ST7735(0x01)
              SendData_ST7735(0x2C)
              SendData_ST7735(0x2D)

              Wait 10 ms

              SendCommand_ST7735(ST7735_INVCTR)
              SendData_ST7735(0x07)
              wait 10 ms;

              SendCommand_ST7735(ST7735_PWCTR1)
              SendData_ST7735(0xA2)
              SendData_ST7735(0x02)
              SendData_ST7735(0x84)
              wait 10 ms;

              SendCommand_ST7735(ST7735_PWCTR2)
              SendData_ST7735(0xC5)
              wait 10 ms;

              SendCommand_ST7735(ST7735_PWCTR3)
              SendData_ST7735(0x0A)
              SendData_ST7735(0x00)
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
              SendData_ST7735(0xC8)     'row address/col address, bottom to top refresh/GBR.
              wait 20 ms

              SendCommand_ST7735(ST7735_COLMOD)
              SendData_ST7735(0x05)

              SendCommand_ST7735(ST7735_CASET)
                'GreenTab
                SendData_ST7735(0x00)
                SendData_ST7735(0x00+ST7735_CASET_ADJUSTMENT)
                SendData_ST7735(0x00)
                SendData_ST7735(GLCD_WIDTH -1 +ST7735_CASET_ADJUSTMENT)
              wait 20 ms

              SendCommand_ST7735(ST7735_RASET)
                'GreenTab
                SendData_ST7735(0x00)
                SendData_ST7735(0x00+ST7735_RASET_ADJUSTMENT)
                SendData_ST7735(0x00)
                SendData_ST7735(GLCD_HEIGHT -1 +ST7735_RASET_ADJUSTMENT)
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
      #endif

    #if GLCD_TYPE <> GLCD_TYPE_ST7735R_160_80
        #IF ST7735TABCOLOR = ST7735_BLACKTAB or ST7735TABCOLOR = ST7735_MINI160x80
            asm showdebug BLACKTAB_AND_MINI_SUPPORT

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
            SendData_ST7735(0xA2)
            SendData_ST7735(0x02)
            SendData_ST7735(0x84)
            wait 10 ms;

            SendCommand_ST7735(ST7735_PWCTR2)
            SendData_ST7735(0xC5)
            wait 10 ms;

            SendCommand_ST7735(ST7735_PWCTR3)
            SendData_ST7735(0x0A)
            SendData_ST7735(0x00)
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

              #if GLCD_TYPE = GLCD_TYPE_ST7735R
                asm showdebug _GLCD_TYPE_ST7735R
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

              #endif

              #if GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
                asm showdebug _GLCD_TYPE_ST7735R_160_80
                SendCommand_ST7735(ST7735_CASET)
                SendData_ST7735(0x00)
                SendData_ST7735(0x00)
                SendData_ST7735(0x00)
                SendData_ST7735(GLCD_WIDTH -1 )
                wait 20 ms

                SendCommand_ST7735(ST7735_RASET)
                SendData_ST7735(0x00)
                SendData_ST7735(0x00)
                SendData_ST7735(0x00)
                SendData_ST7735(GLCD_HEIGHT -1 )
                wait 20 ms

              #endif

        #endif

        #IF ST7735TABCOLOR = ST7735_REDTAB  or  ST7735TABCOLOR = ST7735_GREENTAB
            asm showdebug _GREENREDTAB_SUPPORT

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
            SendData_ST7735(0xA2)
            SendData_ST7735(0x02)
            SendData_ST7735(0x84)
            wait 10 ms;

            SendCommand_ST7735(ST7735_PWCTR2)
            SendData_ST7735(0xC5)
            wait 10 ms;

            SendCommand_ST7735(ST7735_PWCTR3)
            SendData_ST7735(0x0A)
            SendData_ST7735(0x00)
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
            SendData_ST7735(0xC8)     'row address/col address, bottom to top refresh/GBR.
            wait 20 ms

            SendCommand_ST7735(ST7735_COLMOD)
            SendData_ST7735(0x05)

            SendCommand_ST7735(ST7735_CASET)
            #if GLCD_TYPE = GLCD_TYPE_ST7735R
                #IF ST7735TABCOLOR = ST7735_GREENTAB
                  'GreenTab
                  SendData_ST7735(0x00)
                  SendData_ST7735(0x00+ST7735_CASET_ADJUSTMENT)
                  SendData_ST7735(0x00)
                  SendData_ST7735(GLCD_WIDTH -1 +ST7735_CASET_ADJUSTMENT)
                #endif
                #IF ST7735TABCOLOR = ST7735_REDTAB
                  'RedTab
                  SendData_ST7735(0x00)
                  SendData_ST7735(0x00+ST7735_CASET_ADJUSTMENT)
                  SendData_ST7735(0x00)
                  SendData_ST7735(GLCD_WIDTH -1 +ST7735_CASET_ADJUSTMENT)
                #endif
            #endif
            #if GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
              'GLCD_TYPE_ST7735R_160_80
              SendData_ST7735(0x00)
              SendData_ST7735(0x00+ST7735_CASET_ADJUSTMENT)
              SendData_ST7735(0x00)
              SendData_ST7735(GLCD_WIDTH -1 +ST7735_CASET_ADJUSTMENT)

            #endif
            wait 20 ms

            SendCommand_ST7735(ST7735_RASET)
            #if GLCD_TYPE = GLCD_TYPE_ST7735R
                #IF ST7735TABCOLOR = ST7735_GREENTAB
                  'GreenTab
                  SendData_ST7735(0x00)
                  SendData_ST7735(0x00+ST7735_RASET_ADJUSTMENT)
                  SendData_ST7735(0x00)
                  SendData_ST7735(GLCD_HEIGHT -1 +ST7735_RASET_ADJUSTMENT)
                #endif
                #IF ST7735TABCOLOR = ST7735_REDTAB
                  'RedTab
                  SendData_ST7735(0x00)
                  SendData_ST7735(0x00+ST7735_RASET_ADJUSTMENT)
                  SendData_ST7735(0x00)
                  SendData_ST7735(GLCD_HEIGHT -1 +ST7735_RASET_ADJUSTMENT)
                #endif
            #endif
            #if GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
              'GLCD_TYPE_ST7735R_160_80
              SendData_ST7735(0x00)
              SendData_ST7735(0x00+ST7735_RASET_ADJUSTMENT)
              SendData_ST7735(0x00)
              SendData_ST7735(GLCD_HEIGHT -1 +ST7735_RASET_ADJUSTMENT)

            #endif
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

        #endif
    #endif

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

    GLCDRotate ( Landscape )
    'Clear screen
    GLCDCLS

  #endif



End Sub


'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
Sub PSet_ST7735R(In GLCDX, In GLCDY, In GLCDColour As Word)

  #if GLCD_TYPE = GLCD_TYPE_ST7735R  or GLCD_TYPE = GLCD_TYPE_ST7735R_160_80
    SetAddress_ST7735 ST7735_COLUMN, GLCDX, GLCDX
    SetAddress_ST7735 ST7735_ROW, GLCDY, GLCDY
    SendCommand_ST7735 0x2C
    SendWord_ST7735 GLCDColour
  #endif
End Sub
