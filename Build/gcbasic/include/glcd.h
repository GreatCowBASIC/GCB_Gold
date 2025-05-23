/*    Graphical LCD routines for the GCBASIC compiler
    Copyright (C) 2012-2025 Hugh Considine, Joseph Realmuto, Evan Venn and Giuseppe DElia

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

    9/11/14  New revised version.  Requires GLCD.H.  Do not call hardware files directly.  Always load via GLCD.H
    26/7/16  Moved code from ILIxxxx to support GLCDfntDefaultsize = 1,2,3 etc. in Drawstring and DrawChar
    22/8/16  removal of X1 and Y1 varibles. these clashed with register addresses
    08/4/17  added GLCD_TYPE_SSD1306_32 support
    01/5/17  added GLCD_TYPE_ILI9481 support
    2/05/17  Reverted to standard line routine. Local version overan Byte values
    3/07/17  Add overloaded function to cater for GLCDPrint with color
    25/07/17 Added support for low memory SSD1306 usage by adding GLCDDrawChar_SSD1306
    17/08/17 Added support for low memory SSD1306 and SH1106 GLCD by adding GLCD_Open_PageTransaction
             and GLCD_Close_PageTransaction
    21/08/17 Added support for Ellipse methods
    22/08/17 Added stubs for Triangle
    23/08/17 Revised Circle and FilledCircle, updated Draw_Filled_Ellipse_Points
    13/09/17 Added OLED font handler
    14/09/17 Added OLED font handler - revised GLCDDrawString to support OLED
    19/09/17 Revert Circle and FilledCircle
    21/09/17 Adapted to ensure fonts in correct position and they fill the intercharacter pixels
    17/11/17 Added GLCDPrintLn
    10/12/17 Added SSD1331 driver
    22/02/18 Added Nextion driver
    13/03/18 Correct Ypixel position of putting out a white intercharacter pixel/space
    24/04/18 Added Nextion support
    26/09/18 dim GLCDTemp as word added
    27/09/18 ILI9326 driver
    19/11/18 NT7108C driver
    22/11/18 Increased scope of NT7108C methods.
    07/12/18 Remove silly script warning
    24/1/19  Added GLCD_TYPE_ILI9486  to map to GLCD_TYPE_ILI9486L
    6/3/19   Added GLCD_TYPE_UC8320/ILI9320  to map to GLCD_TYPE_UC8320/ILI9320
    3/4/19   Moved DrawBMP from SSD1289 libary to GLCD.H
    14/4/19  Added GLCDPrintWithSize and update DrawEllipse routine
    20/4/19  Added GLCDPrintLargeFont - fixed font at 13pixels - ported from sdd1289
    1/5/19   Added Hyperbole
              See the following article:  "Generating Conic Sections Using an Efficient Algorithms"
              Author: Abdul-Aziz Solyman Khalil,
              Department of Computer Science, University of Mosul, Mosul, Iraq, available on Internet.
    23/9/19  Added GLCD_TYPE_UC1601 support and added improved XPOS for GLCDPrintString
    10/9/19  Added GLCD_TYPE_ST7735R support               GLCD_WIDTH = 128  GLCD_HEIGHT = 160

    13/10/19 Added GLCD_TYPE_SSD1351 support
    3/11/19  Added GLCD_TYPE_ST7735R_160_80 support.       GLCD_WIDTH = 160 GLCD_HEIGHT = 80
    29/11/19 Corrected GLCDLocateString and removed CONSTANTS from GLCD_TYPE_EPD_EPD2in13D
    1/12/19  Addded GLCD_TYPE_EPD_EPD7in5 support
    31/01/21 revised Underscrore to permit contig lines
    25/02/21 Added GLCD_TYPE_ST7789_240_240 support
    19/12/22 Revised GLCDPrintLoc to correct Xpos value.
    25/03/23 Added ILI9488 support
    26/03/23 Added GLCD_TYPE_ST7789_320_240 support
    05/02/24 Remove include statements except KX0108.h.  Other are now controlled by glcd.dat
    08/01/25 Added GLCD_TYPE_SSD1306_64x32 support
    19/02/25 Resolved same label in BigFont methods.

*/
'Constants that might need to be set
'#define GLCD_TYPE GLCD_TYPE_KS0108 | GLCD_TYPE_ST7735 | GLCD_TYPE_ST7920 | GLCD_TYPE_PCD8544 | GLCD_TYPE_SSD1306 etc etc

' Circle edge overdraw protection
' #define GLCD_PROTECTOVERRUN

'End of user changeable constants


'GLCD types - add new types here!
  #define GLCD_TYPE_KS0108  1
  #define GLCD_TYPE_ST7735  2            'GLCD_WIDTH = 128  GLCD_HEIGHT = 160
  #define GLCD_TYPE_ST7920  3
  #define GLCD_TYPE_PCD8544 4
  #define GLCD_TYPE_SSD1306 5
  #define GLCD_TYPE_ILI9340 6
  #define GLCD_TYPE_SSD1289 7
  #define GLCD_TYPE_ILI9341 8
  #define GLCD_TYPE_SH1106  9
  #define GLCD_TYPE_SSD1306_32 10
  #define GLCD_TYPE_ILI9486L 11
  #define GLCD_TYPE_ILI9486 11
  #define GLCD_TYPE_ILI9481 12
  #define GLCD_TYPE_SSD1331 14
  #define GLCD_TYPE_HX8347  15
  #define GLCD_TYPE_Nextion 16
  #define GLCD_TYPE_ILI9326 17
  #define GLCD_TYPE_NT7108C 18
  #define GLCD_TYPE_T6963   19           'ASSIGNED TO THE _64 AND _128
  #define GLCD_TYPE_T6963_64    20
  #define GLCD_TYPE_T6963_128   21
  #define GLCD_TYPE_UC8230  22
  #define GLCD_TYPE_ILI9320 23
  #define GLCD_TYPE_UC1601  24
  #define GLCD_TYPE_ST7735R 25           'GLCD_WIDTH = 128  GLCD_HEIGHT = 160
  #define GLCD_TYPE_SSD1351 26
  #define GLCD_TYPE_EPD_EPD2in13D     27
  #define GLCD_TYPE_ST7735R_160_80 28    'GLCD_WIDTH = 160 GLCD_HEIGHT = 80
  #define GLCD_TYPE_EPD_EPD7in5  29
  #define GLCD_TYPE_SSD1306_TWIN 30
  #define GLCD_TYPE_ST7789_240_240 31
  #define GLCD_TYPE_ILI9488 32
  #define GLCD_TYPE_ST7567  33
  #define GLCD_TYPE_ST7789_320_240 34
  #define LT7686_800_480_BLUE          35
  #define LT7686_1024_600_BLUE         36
  #define LT7686_1024_600_BLACK        37
  #define GLCD_TYPE_SSD1306_64x32      38

  


dim GLCDFontWidth,GLCDfntDefault, GLCDfntDefaultsize, GLCDfntDefaultheight as byte


' Default font size
  'GLCDfntDefaultsize = 1
  'GLCDfntDefaultHeight = 8

' Screen rotation constants
  #define LANDSCAPE     1
  #define PORTRAIT_REV  2
  #define LANDSCAPE_REV 3
  #define PORTRAIT      4




' Color definitions constants
  ' Define Colors
  #define TFT_BLACK       0x0000
  #define TFT_NAVY        0x000F
  #define TFT_DARKGREEN   0x03E0
  #define TFT_DARKCYAN    0x03EF
  #define TFT_MAROON      0x7800
  #define TFT_PURPLE      0x780F
  #define TFT_OLIVE       0x7BE0
  #define TFT_LIGHTGREY   0xC618
  #define TFT_DARKGREY    0x7BEF
  #define TFT_BLUE        0x001F
  #define TFT_GREEN       0x07E0
  #define TFT_CYAN        0x77FF
  #define TFT_RED         0xF800
  #define TFT_MAGENTA     0xF81F
  #define TFT_YELLOW      0xFFE0
  #define TFT_WHITE       0xFFFF
  #define TFT_ORANGE      0xFD20
  #define TFT_GREENYELLOW 0xAFE5
  #define TFT_PINK        0xF81F

  #define EPD_WHITE       0x33
  #define EPD_BLACK       0x00

  #define color65k_black   0x0000
  #define color65k_white   0xffff
  #define color65k_red     0xf800
  #define color65k_green   0x07e0
  #define color65k_blue    0x001f
  #define color65k_yellow  color65k_red|color65k_green
  #define color65k_cyan    color65k_green|color65k_blue
  #define color65k_purple  color65k_red|color65k_blue

  #define color65k_grayscale1    2113
  #define color65k_grayscale2    2113*2
  #define color65k_grayscale3    2113*3
  #define color65k_grayscale4    2113*4
  #define color65k_grayscale5    2113*5
  #define color65k_grayscale6    2113*6
  #define color65k_grayscale7    2113*7
  #define color65k_grayscale8    2113*8
  #define color65k_grayscale9    2113*9
  #define color65k_grayscale10   2113*10
  #define color65k_grayscale11   2113*11
  #define color65k_grayscale12   2113*12
  #define color65k_grayscale13   2113*13
  #define color65k_grayscale14   2113*14
  #define color65k_grayscale15   2113*15
  #define color65k_grayscale16   2113*16
  #define color65k_grayscale17   2113*17
  #define color65k_grayscale18   2113*18
  #define color65k_grayscale19   2113*19
  #define color65k_grayscale20   2113*20
  #define color65k_grayscale21   2113*21
  #define color65k_grayscale22   2113*22
  #define color65k_grayscale23   2113*23
  #define color65k_grayscale24   2113*24
  #define color65k_grayscale25   2113*25
  #define color65k_grayscale26   2113*26
  #define color65k_grayscale27   2113*27
  #define color65k_grayscale28   2113*28
  #define color65k_grayscale29   2113*29
  #define color65k_grayscale30   2113*30

' Dimension variables for global variable required for Circles.
  dim GLCD_yordinate as integer
  dim glcd_type_string as string * 7
  dim GLCDRotateState as byte

' Dimension variables for foreground and background colours
  #if GLCD_TYPE = GLCD_TYPE_ILI9488
    //~Need to define colors as long as the color definition is 18bit when using SPI
    Dim GLCDbackground, GLCDForeground as Long
  #else
    Dim GLCDBackground, GLCDForeground As Word
  #endif
' Dimension variables to minimise memory map to the these Word variables
  Dim GLCDDeviceHeight as Word
  Dim GLCDDeviceWidth as Word


#script

  'User changeable constants
  IF NODEF(GLCD_TYPE) THEN
    GLCD_TYPE=GLCD_TYPE_KS0108
  END IF

  IF NODEF(GLCDLINEWIDTH) THEN
    GLCDLINEWIDTH=1
  END IF


  GLCDVERSION=9
  

  If GLCD_TYPE = GLCD_TYPE_ST7789_240_240 Then
     ' #include <GLCD_ST7789.h>
     InitGLCD = InitGLCD_ST7789
     GLCDCLS = GLCDCLS_ST7789
     GLCDDrawChar = GLCDDrawChar_ST7789
     GLCDDrawString = GLCDDrawString_ST7789
     FilledBox = FilledBox_ST7789
     Pset = Pset_ST7789
     GLCDRotate = GLCDRotate_ST7789
     glcd_type_string = "ST7789"
     GLCD_WIDTH = 240
     GLCD_HEIGHT = 240
     ST7789_GLCD_HEIGHT = GLCDDeviceHeight
     ST7789_GLCD_WIDTH = GLCDDeviceWidth
  End If




  If GLCD_TYPE = GLCD_TYPE_KS0108 Then

     #include <glcd_ks0108.h>
     InitGLCD = InitGLCD_KS0108
     GLCDCLS = GLCDCLS_KS0108
     FilledBox = FilledBox_KS0108
     Pset = Pset_KS0108
     glcd_type_string = "KS0108"
     GLCD_WIDTH = 128
     GLCD_HEIGHT = 64
     KS0108_GLCD_HEIGHT = GLCDDeviceHeight
     KS0108_GLCD_WIDTH = GLCDDeviceWidth

  End If

    If GLCD_TYPE = GLCD_TYPE_NT7108C Then
     ' #include <glcd_nt7108c.h>
     InitGLCD = InitGLCD_NT7108C
     GLCDCLS = GLCDCLS_NT7108C
     FilledBox = FilledBox_NT7108C
     Pset = Pset_NT7108C
     GLCDReadByte = GLCDReadByte_NT7108C
     GLCDWRITEBYTE = GLCDWRITEBYTE_NT7108C

     glcd_type_string = "NT7108C"
     GLCD_WIDTH = 128
     GLCD_HEIGHT = 64
     NT7108C_GLCD_HEIGHT = GLCDDeviceHeight
     NT7108C_GLCD_WIDTH = GLCDDeviceWidth

  End If

  If GLCD_TYPE = GLCD_TYPE_SSD1306 Then
     'Support I2C, I2C2 and 4wire SPI with low memory optimisation.
     ' #include <glcd_ssd1306.h>
     InitGLCD = InitGLCD_SSD1306
     GLCDCLS = GLCDCLS_SSD1306
     GLCDDrawChar = GLCDDrawChar_SSD1306
     FilledBox = FilledBox_SSD1306
     Pset = Pset_SSD1306
     GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_SSD1306
     GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_SSD1306
     GLCDSetContrast = SetContrast_SSD1306
     glcd_type_string = "SSD1306"
     GLCD_WIDTH = 128
     GLCD_HEIGHT = 64
     SSD1306_GLCD_HEIGHT = GLCDDeviceHeight
     SSD1306_GLCD_WIDTH = GLCDDeviceWidth

  End If

  If GLCD_TYPE = GLCD_TYPE_UC1601 Then
     'Support I2C, I2C2 and 4wire SPI with low memory optimisation.
     ' #include <glcd_uc1601.h>
     InitGLCD = InitGLCD_UC1601
     GLCDCLS = GLCDCLS_UC1601
     GLCDDrawChar = GLCDDrawChar_UC1601
     FilledBox = FilledBox_UC1601
     Pset = Pset_UC1601
     GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_UC1601
     GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_UC1601
     GLCDSetContrast = SetContrast_UC1601
     GLCDRotate = GLCDRotate_UC1601
     glcd_type_string = "UC1601"
     GLCD_WIDTH = 132
     GLCD_HEIGHT = 22
     UC1601_GLCD_HEIGHT = GLCDDeviceHeight
     UC1601_GLCD_WIDTH = GLCDDeviceWidth

  End If


  If GLCD_TYPE = GLCD_TYPE_SSD1306_32 Then
     ' #include <glcd_ssd1306.h>
     InitGLCD = InitGLCD_SSD1306
     GLCDCLS = GLCDCLS_SSD1306
     GLCDDrawChar = GLCDDrawChar_SSD1306
     FilledBox = FilledBox_SSD1306
     Pset = Pset_SSD1306
     GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_SSD1306
     GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_SSD1306
     GLCDSetContrast = SetContrast_SSD1306
     glcd_type_string = "SSD1306_32"
     GLCD_WIDTH = 128
     GLCD_HEIGHT = 32
     SSD1306_GLCD_HEIGHT = GLCDDeviceHeight
     SSD1306_GLCD_WIDTH = GLCDDeviceWidth

  End If

  If GLCD_TYPE = GLCD_TYPE_SSD1306_64x32 Then
     ' #include <glcd_ssd1306.h>
     InitGLCD = InitGLCD_SSD1306
     GLCDCLS = GLCDCLS_SSD1306
     GLCDDrawChar = GLCDDrawChar_SSD1306
     FilledBox = FilledBox_SSD1306
     Pset = Pset_SSD1306
     GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_SSD1306
     GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_SSD1306
     GLCDSetContrast = SetContrast_SSD1306
     glcd_type_string = "SSD1306_32"
     GLCD_WIDTH = 64
     GLCD_HEIGHT = 32
     SSD1306_GLCD_HEIGHT = GLCDDeviceHeight
     SSD1306_GLCD_WIDTH = GLCDDeviceWidth
     GLCD_TYPE_SSD1306_64x32_XOFFSET = 32

  End If

  If GLCD_TYPE = GLCD_TYPE_SSD1306_TWIN Then

     ' #include <glcd_ssd1306_twin.h>
     InitGLCD = InitGLCD_TwinSSD1306
     GLCDCLS = GLCDCLS_TwinSSD1306
     GLCDDrawChar = GLCDDrawChar_TwinSSD1306
  '     FilledBox = FilledBox_SSD1306_twin
  '     Pset = Pset_SSD1306_twin
  '     GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_SSD1306_twin
  '     GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_SSD1306_twin
  '     GLCDSetContrast = SetContrast_SSD1306_twin
  '     glcd_type_string = "SSD1306_TWIN"
      GLCD_WIDTH = 128
      GLCD_HEIGHT = 128
  '     SSD1306_GLCD_HEIGHT = GLCDDeviceHeight
  '     SSD1306_GLCD_WIDTH = GLCDDeviceWidth

    End If

    If GLCD_TYPE = GLCD_TYPE_ILI9341 Then

      ' #include <glcd_ili9341.h>
      InitGLCD = InitGLCD_ILI9341
      GLCDCLS = GLCDCLS_ILI9341
      GLCDDrawChar = GLCDDrawChar_ILI9341
      GLCDDrawString = GLCDDrawString_ILI9341
      FilledBox = FilledBox_ILI9341
      Pset = Pset_ILI9341
      GLCDRotate = GLCDRotate_ILI9341
      glcd_type_string = "ILI9341"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 320
      ILI9341_GLCD_HEIGHT = GLCDDeviceHeight
      ILI9341_GLCD_WIDTH = GLCDDeviceWidth
    End If


    If GLCD_TYPE = GLCD_TYPE_ILI9340 Then

      ' #include <glcd_ili9340.h>
      InitGLCD = InitGLCD_ILI9340
      GLCDCLS = GLCDCLS_ILI9340
      GLCDDrawChar = GLCDDrawChar_ILI9340
      GLCDDrawString = GLCDDrawString_ILI9340
      FilledBox = FilledBox_ILI9340
      Pset = Pset_ILI9340
      GLCDRotate = GLCDRotate_ILI9340
      glcd_type_string = "ILI9340"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 320
      ILI9340_GLCD_HEIGHT = GLCDDeviceHeight
      ILI9340_GLCD_WIDTH = GLCDDeviceWidth
    End If


    If GLCD_TYPE = GLCD_TYPE_SSD1289 Then

      ' #include <glcd_ssd1289.h>
      InitGLCD = InitGLCD_SSD1289
      GLCDCLS = GLCDCLS_SSD1289
      GLCDDrawChar = GLCDDrawChar_SSD1289
      GLCDPrint = Print_SSD1289
      FilledBox = FilledBox_SSD1289
      Box = Box_SSD1289
      GLCDDrawString = Print_SSD1289
      Circle = Circle_SSD1289
      FilledCircle  = FilledCircle_SSD1289
      Pset = PSet_SSD1289
        GLCDRotate = GLCDRotate_SSD1289
      glcd_type_string = "SSD1289"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 320
      SSD1289_GLCD_HEIGHT = GLCDDeviceHeight
      SSD1289_GLCD_WIDTH = GLCDDeviceWidth

    End If

    If GLCD_TYPE = GLCD_TYPE_ST7735 Then

      ' #include <glcd_st7735.h>
      InitGLCD = InitGLCD_ST7735
      GLCDCLS = GLCDCLS_ST7735
      GLCDDrawChar = GLCDDrawChar_ST7735
      FilledBox = FilledBox_ST7735
      Pset = Pset_ST7735
      GLCDRotate = GLCDRotate_ST7735
      GLCDSetScrollDefinition = GLCDSetScrollDefinition_ST7735
      GLCDVerticalScroll = GLCDVerticalScroll_ST7735
      glcd_type_string = "ST7735"
      GLCD_WIDTH = 128
      GLCD_HEIGHT = 160
      ST7735_GLCD_HEIGHT = GLCDDeviceHeight
      ST7735_GLCD_WIDTH = GLCDDeviceWidth

    End If

    If GLCD_TYPE = GLCD_TYPE_ST7735R Then

      ' #include <glcd_st7735r.h>
      'Device specific
      InitGLCD = InitGLCD_ST7735R
      Pset = Pset_ST7735R

      'Shared with ST7735
      GLCDCLS = GLCDCLS_ST7735
      GLCDDrawChar = GLCDDrawChar_ST7735
      FilledBox = FilledBox_ST7735
      GLCDRotate = GLCDRotate_ST7735
      GLCDSetScrollDefinition = GLCDSetScrollDefinition_ST7735
      GLCDVerticalScroll = GLCDVerticalScroll_ST7735

      glcd_type_string = "ST7735R"
      GLCD_WIDTH = 128
      GLCD_HEIGHT = 160
      ST7735R_GLCD_HEIGHT = GLCDDeviceHeight
      ST7735R_GLCD_WIDTH = GLCDDeviceWidth

    End If


    If GLCD_TYPE = GLCD_TYPE_ST7735R_160_80 Then

      ' #include <glcd_st7735r.h>
      'Device specific
      InitGLCD = InitGLCD_ST7735R
      Pset = Pset_ST7735R

      'Shared with ST7735
      GLCDCLS = GLCDCLS_ST7735
      GLCDDrawChar = GLCDDrawChar_ST7735
      FilledBox = FilledBox_ST7735
      GLCDRotate = GLCDRotate_ST7735
      GLCDSetScrollDefinition = GLCDSetScrollDefinition_ST7735
      GLCDVerticalScroll = GLCDVerticalScroll_ST7735

      glcd_type_string = "ST7735R"
      GLCD_WIDTH = 80
      GLCD_HEIGHT = 160

      ST7735R_GLCD_HEIGHT = GLCDDeviceHeight
      ST7735R_GLCD_WIDTH = GLCDDeviceWidth

    End If

    If GLCD_TYPE = GLCD_TYPE_ST7920 Then

      ' #include <glcd_st7920.h>
      InitGLCD = InitGLCD_ST7920
      GLCDCLS = GLCDCLS_ST7920
      FilledBox = FilledBox_ST7920
      Pset = Pset_ST7920
      GLCDPrint = GLCDPrint_ST7920
      glcd_type_string = "ST7920"
      GLCD_WIDTH = 128
      GLCD_HEIGHT = 64
      ST7920_GLCD_HEIGHT = GLCDDeviceHeight
      ST7920_GLCD_WIDTH = GLCDDeviceWidth
    End If

    If GLCD_TYPE = GLCD_TYPE_PCD8544 Then

      ' #include <glcd_pcd8544.h>
      InitGLCD = InitGLCD_PCD8544
      GLCDCLS = GLCDCLS_PCD8544
      GLCDDrawChar = GLCDDrawChar_PCD8544
      FilledBox = FilledBox_PCD8544
      Pset = Pset_PCD8544
      glcd_type_string = "PCD8544"
      PCD8544WriteDelay = 0
      PCD8544ClockDelay = 0
      GLCD_WIDTH = 84
      GLCD_HEIGHT = 48
      PCD8544_GLCD_HEIGHT = GLCDDeviceHeight
      PCD8544_GLCD_WIDTH = GLCDDeviceWidth

    End If

    If GLCD_TYPE = GLCD_TYPE_SH1106 Then
      ' #include <glcd_sh1106.h>
      InitGLCD = InitGLCD_SH1106
      GLCDCLS = GLCDCLS_SH1106
      GLCDDrawChar = GLCDDrawChar_SH1106
      FilledBox = FilledBox_SH1106
      Pset = Pset_SH1106
      GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_SH1106
      GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_SH1106
      GLCDSetContrast = GLCDSetContrast_SSH1106
      GLCDRotate = GLCDRotate_SSH1106
      GLCDSetDisplayNormalMode = GLCDSetDisplayNormalMode_SSH1106
      GLCDSetDisplayInvertMode = GLCDSetDisplayInvertMode_SSH1106
      glcd_type_string = "SH1106"
      GLCD_WIDTH = 128
      GLCD_HEIGHT = 64
      SH1106_GLCD_HEIGHT = GLCDDeviceHeight
      SH1106_GLCD_WIDTH = GLCDDeviceWidth

    End If


    If GLCD_TYPE = GLCD_TYPE_ILI9326 Then

      ' #include <glcd_ili9326.h>
      InitGLCD = InitGLCD_ILI9326
      GLCDCLS = GLCDCLS_ILI9326
      GLCDDrawChar = GLCDDrawChar_ILI9326
      GLCDDrawString = GLCDDrawString_ILI9326
      FilledBox = FilledBox_ILI9326
      Pset = Pset_ILI9326
      GLCDRotate = GLCDRotate_ILI9326
      glcd_type_string = "ILI9326"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 400
      ILI9326_GLCD_HEIGHT = GLCDDeviceHeight
      ILI9326_GLCD_WIDTH = GLCDDeviceWidth
    End If


    If GLCD_TYPE = GLCD_TYPE_SSD1351 Then

      ' #include <glcd_ssd1351.h>
      InitGLCD = InitGLCD_SSD1351
      GLCDCLS = GLCDCLS_SSD1351
      GLCDDrawChar = GLCDDrawChar_SSD1351
      GLCDDrawString = GLCDDrawString_SSD1351
      FilledBox = FilledBox_SSD1351
      Pset = Pset_SSD1351
      GLCDRotate = GLCDRotate_SSD1351
      GLCDSetContrast = SetContrast_SSD1351
      glcd_type_string = "SSD1351"
      GLCD_WIDTH = 128
      GLCD_HEIGHT = 128
      SSD1351_GLCD_HEIGHT = GLCDDeviceHeight
      SSD1351_GLCD_WIDTH = GLCDDeviceWidth
    End If


    'Loads extended fonts set ASCII characters 31-254
    'Requires 1358 bytes of program memory
    If GLCD_EXTENDEDFONTSET1 then

      GLCDCharCol3 = GLCDCharCol3Extended1
      GLCDCharCol4 = GLCDCharCol4Extended1
      GLCDCharCol5 = GLCDCharCol5Extended1
      GLCDCharCol6 = GLCDCharCol6Extended1
      GLCDCharCol7 = GLCDCharCol7Extended1

    End If

  ' same as GLCD_TYPE_ILI9486 below
  If GLCD_TYPE = GLCD_TYPE_ILI9486L Then

      ' #include <glcd_ili9486l.h>
      InitGLCD = InitGLCD_ili9486L
      GLCDCLS = GLCDCLS_ili9486L
      GLCDDrawChar = GLCDDrawChar_ili9486L
      GLCDDrawString = GLCDDrawString_ili9486L
      FilledBox = FilledBox_ili9486L
      Pset = Pset_ili9486L
      GLCDRotate = GLCDRotate_ili9486L
      glcd_type_string = "ili9486L"
      GLCD_WIDTH = 320
      GLCD_HEIGHT = 480
      ili9486L_GLCD_HEIGHT = GLCDDeviceHeight
      ili9486L_GLCD_WIDTH = GLCDDeviceWidth
      if GLCD_DataPort Then

          InitGLCD = InitGLCD_fullport_ili9486L
          GLCDCLS = GLCDCLS_fullport_ili9486L
          SendCommand_ILI9486L = SendCommand_fullport_ILI9486L
          SendData_ILI9486L = SendData_fullport_ILI9486L
          SendWord_ILI9486L = SendWord_fullport_ILI9486L

      end if

    End If

  ' same as GLCD_TYPE_ILI9486L above
  If GLCD_TYPE = GLCD_TYPE_ILI9486 Then

      ' #include <glcd_ili9486l.h>
      InitGLCD = InitGLCD_ili9486L
      GLCDCLS = GLCDCLS_ili9486L
      GLCDDrawChar = GLCDDrawChar_ili9486L
      GLCDDrawString = GLCDDrawString_ili9486L
      FilledBox = FilledBox_ili9486L
      Pset = Pset_ili9486L
      GLCDRotate = GLCDRotate_ili9486L
      glcd_type_string = "ili9486L"
      GLCD_WIDTH = 320
      GLCD_HEIGHT = 480
      ili9486L_GLCD_HEIGHT = GLCDDeviceHeight
      ili9486L_GLCD_WIDTH = GLCDDeviceWidth
      if GLCD_DataPort Then

          InitGLCD = InitGLCD_fullport_ili9486L
          GLCDCLS = GLCDCLS_fullport_ili9486L
          SendCommand_ILI9486L = SendCommand_fullport_ILI9486L
          SendData_ILI9486L = SendData_fullport_ILI9486L
          SendWord_ILI9486L = SendWord_fullport_ILI9486L

      end if

    End If


  If GLCD_TYPE = GLCD_TYPE_ILI9488 Then

      ' uses overloader methods with COLOR=LONG for 18bit colors = box, cirle etc
      ' #include <glcd_ili9488.h>
      InitGLCD = InitGLCD_ili9488
      GLCDCLS = GLCDCLS_ili9488
      GLCDDrawChar = GLCDDrawChar_ili9488
      GLCDDrawString = GLCDDrawString_ili9488
      Pset = Pset_ili9488
      FilledBox = FilledBox_ili9488
      GLCDRotate = GLCDRotate_ili9488
      LINE = LINE_LongColor
      glcd_type_string = "ili9488"
      GLCD_WIDTH = 320
      GLCD_HEIGHT = 480
      ili9488_GLCD_HEIGHT = GLCDDeviceHeight
      ili9488_GLCD_WIDTH = GLCDDeviceWidth
      
      if GLCD_DataPort Then
          InitGLCD = InitGLCD_fullport_ili9488
          GLCDCLS = GLCDCLS_fullport_ili9488
          SendCommand_ili9488 = SendCommand_fullport_ili9488
          SendData_ili9488 = SendData_fullport_ili9488
          SendWord_ili9488 = SendWord_fullport_ili9488
      end if


    End If

  If GLCD_TYPE = GLCD_TYPE_ILI9481 Then

      ' #include <glcd_ili9481.h>
      InitGLCD = InitGLCD_ili9481
      GLCDCLS = GLCDCLS_ili9481
      GLCDDrawChar = GLCDDrawChar_ili9481
      GLCDDrawString = GLCDDrawString_ili9481
      FilledBox = FilledBox_ili9481
      Pset = Pset_ili9481
      GLCDRotate = GLCDRotate_ili9481
      glcd_type_string = "ili9481"
      GLCD_WIDTH = 320
      GLCD_HEIGHT = 480
      ili9481_GLCD_HEIGHT = GLCDDeviceHeight
      ili9481_GLCD_WIDTH = GLCDDeviceWidth
    End If


    If GLCD_TYPE = GLCD_TYPE_HX8347 Then

      ' #include <glcd_hx8347.h>
      InitGLCD = InitGLCD_HX8347
      GLCDCLS = GLCDCLS_HX8347
      GLCDDrawChar = GLCDDrawChar_HX8347
      GLCDDrawString = GLCDDrawString_HX8347
      FilledBox = FilledBox_HX8347
      Pset = Pset_HX8347
      GLCDRotate = GLCDRotate_HX8347
      glcd_type_string = "HX8347"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 320
      HX8347_GLCD_HEIGHT = GLCDDeviceHeight
      HX8347_GLCD_WIDTH = GLCDDeviceWidth

    End If


    If GLCD_TYPE = GLCD_TYPE_SSD1331 Then

      ' #include <glcd_ssd1331.h>
      InitGLCD = InitGLCD_SSD1331
      GLCDCLS = GLCDCLS_SSD1331
      GLCDDrawChar = GLCDDrawChar_SSD1331
      GLCDDrawString = GLCDDrawString_SSD1331
      FilledBox = FilledBox_SSD1331
      Pset = Pset_SSD1331
      GLCDRotate = GLCDRotate_SSD1331
      GLCDSetContrast = SetContrast_SSD1331
      glcd_type_string = "SSD1331"
      GLCD_WIDTH = 96
      GLCD_HEIGHT = 64
      SSD1331_GLCD_HEIGHT = GLCDDeviceHeight
      SSD1331_GLCD_WIDTH = GLCDDeviceWidth
    End If



  If GLCD_TYPE = GLCD_TYPE_Nextion Then

      ' #include <glcd_nextion.h>
      InitGLCD = InitGLCD_Nextion
      GLCDCLS = GLCDCLS_Nextion
      Circle = Circle_Nextion
      FilledCircle=FilledCircle_Nextion
      Box = Box_Nextion
      FilledBox = FilledBox_Nextion
      Line =  Line_Nextion
      GLCDDrawChar = GLCDDrawChar_Nextion
      GLCDDrawString = GLCDDrawString_Nextion
      Pset = Pset_Nextion
      GLCDRotate = GLCDRotate_Nextion
      'The following methods are not needed as you can call directly
  '     GLCDPrintDefaultFont_Nextion = GLCDPrintDefaultFont_NXN320x480L
  '     GLCDPrintString_Nextion = GLCDPrintString_NXN320x480L
  '     GLCDPrintStringLn_Nextion = GLCDPrintStringLn_NXN320x480L
  '     GLCDLocateString_Nextion = GLCDLocateString_NXN320x480L
  '     GLCDPrint_Nextion = GLCDPrint_NXN320x480L 'Landscape
  '     GLCDUpdateObject_Nextion = GLCDUpdateObject_NXN320x480L
      glcd_type_string = "Nextion"
      'Not required. THESE MUST BE SPECIFIED IN THE USE PROGRAM!!
  '     GLCD_WIDTH = 480
  '     GLCD_HEIGHT = 320
      Nextion_GLCD_HEIGHT = GLCDDeviceHeight
      Nextion_GLCD_WIDTH = GLCDDeviceWidth
    End If



    If GLCD_TYPE = GLCD_TYPE_T6963_64 Then
        ' #include <glcd_t6963.h>
        GLCD_TYPE = GLCD_TYPE_T6963
        LCD_IO = T36363
        LCDCursor = LCDCursor_T6963
        PUT = PUT_T6963
        GET = GET_T6963
        LOCATE = LOCATE_T6963
        CLS = CLS_T6963
        LCDHOME = LCDHOME_T6963
        LCDcmd = LCDcmd_T6963
        LCDdata = LCDdata_T6963
        LCDHex = LCDHex_T6963
        LCDSpace = LCDSpace_T6963
        LCDCursor = LCDCursor_T6963
        LCDNormalWriteByte = LCDNormalWriteByte_T6963

        pset = PSet_T6963
        GLCDRotate = GLCDRotate_T6963
        FilledBox = FilledBox_T6963
        GLCDCLS = GLCDCLS_T6963
        CLS = CLS_T6963
        BigPrint = BigPrint_ILI9486L
        SelectGLCDPage = SelectGLCDPage_T6963
        SelectLCDPage = SelectLCDPage_T6963

      glcd_type_string = "T6963"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 64
      T6963_GLCD_HEIGHT = GLCDDeviceHeight
      T6963_GLCD_WIDTH = GLCDDeviceWidth

    End if

    If GLCD_TYPE = GLCD_TYPE_T6963_128 Then
        ' #include <glcd_t6963.h>
        GLCD_TYPE = GLCD_TYPE_T6963
        LCD_IO = T36363
        LCDCursor = LCDCursor_T6963
        PUT = PUT_T6963
        GET = GET_T6963
        LOCATE = LOCATE_T6963
        CLS = CLS_T6963
        LCDHOME = LCDHOME_T6963
        LCDcmd = LCDcmd_T6963
        LCDdata = LCDdata_T6963
        LCDHex = LCDHex_T6963
        LCDSpace = LCDSpace_T6963
        LCDCursor = LCDCursor_T6963
        LCDNormalWriteByte = LCDNormalWriteByte_T6963

        pset = PSet_T6963
        GLCDRotate = GLCDRotate_T6963
        FilledBox = FilledBox_T6963
        GLCDCLS = GLCDCLS_T6963
        CLS = CLS_T6963
        BigPrint = BigPrint_ILI9486L
        SelectGLCDPage = SelectGLCDPage_T6963
        SelectLCDPage = SelectLCDPage_T6963

      glcd_type_string = "T6963"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 128
      T6963_GLCD_HEIGHT = GLCDDeviceHeight
      T6963_GLCD_WIDTH = GLCDDeviceWidth

    End if

    If GLCD_TYPE = GLCD_TYPE_UC8230 Then

      ' #include <glcd_uc8230.h>
      InitGLCD = InitGLCD_uc8230
      GLCDCLS = GLCDCLS_uc8230
      GLCDDrawChar = GLCDDrawChar_uc8230
      GLCDDrawString = GLCDDrawString_uc8230
      FilledBox = FilledBox_uc8230
      Pset = Pset_uc8230
      GLCDRotate = GLCDRotate_uc8230
      glcd_type_string = "uc8230"
      GLCD_WIDTH = 240
      GLCD_HEIGHT = 320
      uc8230_GLCD_HEIGHT = GLCDDeviceHeight
      uc8230_GLCD_WIDTH = GLCDDeviceWidth

    End If

  ' For E-Paper Waveshare EP2.13inch HAT (D)
    If GLCD_TYPE = GLCD_TYPE_EPD_EPD2in13D Then

      ' #include <epd_epd2in13d.h>
      InitGLCD = Init_EPD2in13D
      GLCDCLS  = CLS_EPD2in13D
      GLCDDrawChar = DrawChar_EPD2in13D
      GLCDDrawString = DrawString_EPD2in13D
      FilledBox = FilledBox_EPD2in13D
      Pset = Pset_EPD2in13D
      GLCDRotate = Rotate_EPD2in13D
      GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_EPD2in13D
      GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_EPD2in13D
      GLCDSleep = Display_EPD2in13D
      GLCDDisplay = Display_EPD2in13D
      GLCD_TYPE_STRING = "EPD2in13D"
      GLCD_WIDTH =  104
      GLCD_HEIGHT = 212

      Ifdef GLCD_EXTENDEDFONTSET1 then

        GLCDCharCol3 = GLCDCharCol3Extended1
        GLCDCharCol4 = GLCDCharCol4Extended1
        GLCDCharCol5 = GLCDCharCol5Extended1
        GLCDCharCol6 = GLCDCharCol6Extended1
        GLCDCharCol7 = GLCDCharCol7Extended1

      End If

      IF GLCD_TYPE = GLCD_TYPE_EPD_EPD2in13D then
        'One buffer. No issues... just need a lot of RAM in the chip
        BUFFWIDTH= 2756
        EPD_N_PAGE=  1
        EPD_CORRECTED_HEIGHT = 208
        EPD_ROWS_PER_PAGE = 26
        EPD_PIXELS_PER_PAGE = 212

      end if
      if GLCD_TYPE_EPD2in13D_LOWMEMORY1_GLCD_MODE then
        'Smallest buffer.
        '
        'One pixel per row buffer. WIDTH = 104. Pixel Row = 1.  (104*1)/8 = 13
        'We have 212 pages
        'The corrected height is 208 to cater for page wrap in GLCDPrintString
        'Character rows per page = 1 as this is the minimum value.
        'Pixels per row = 1

        BUFFWIDTH= 13
        EPD_N_PAGE= 212
        EPD_CORRECTED_HEIGHT = 208
        EPD_ROWS_PER_PAGE = 1
        EPD_PIXELS_PER_PAGE = 1
      end if
      if GLCD_TYPE_EPD2in13D_LOWMEMORY2_GLCD_MODE then
        '
        '8 pixels per row buffer. WIDTH = 104. Pixel Row = 8.  (104*8)/8 = 104
        'We have 27 pages, one partial page to cater for the 4 row pixels
        'The corrected height is 208 to cater for page wrap in GLCDPrintString
        'Character rows per page = 1
        'Pixels per row = 8

        BUFFWIDTH= 104
        EPD_N_PAGE= 27
        EPD_CORRECTED_HEIGHT = 208
        EPD_ROWS_PER_PAGE = 1
        EPD_PIXELS_PER_PAGE = 8
      end if
      if GLCD_TYPE_EPD2in13D_LOWMEMORY3_GLCD_MODE then
        '
        '16 pixels per row buffer. WIDTH = 104. Pixel Row = 16.  (104*16)/8 = 208
        'We have 13 pages, one partial page to cater for the 4 row pixels
        'The corrected height is 208 to cater for page wrap in GLCDPrintString
        'Character rows per page = 2
        'Pixels per row = 16
        BUFFWIDTH= 208
        EPD_N_PAGE= 13
        EPD_CORRECTED_HEIGHT = 208
        EPD_ROWS_PER_PAGE = 2
        EPD_PIXELS_PER_PAGE = 16
      end if
      if GLCD_TYPE_EPD2in13D_LOWMEMORY4_GLCD_MODE then
        '
        '104 pixels per row buffer. WIDTH = 104. Pixel Row = 104.  (104*104)/8 = 1352
        'We have 3 pages, one partial page to cater for the 4 row pixels
        'The corrected height is 208 to cater for page wrap in GLCDPrintString
        'Character rows per page = 13
        'Pixels per row = 104
        BUFFWIDTH =1352
        EPD_N_PAGE= 3
        EPD_CORRECTED_HEIGHT = 208
        EPD_ROWS_PER_PAGE = 13
        EPD_PIXELS_PER_PAGE = 104
      end if
      'Last calcualtion is needed
      IF GLCD_TYPE = GLCD_TYPE_EPD_EPD2in13D then
          GLCD_WIDTH8 = GLCD_WIDTH / 8
      end if

      'Resolve BUFFER when SPIRam is available
      if SPISRAM_TYPE then
        'One SRAM buffer. No issues... just need a lot of RAM in the chip
        BUFFWIDTH= 2756
        EPD_N_PAGE=  1
        EPD_CORRECTED_HEIGHT = 208
        EPD_ROWS_PER_PAGE = 26
        EPD_PIXELS_PER_PAGE = 212
      end if

    End If



  ' For E-Paper Waveshare EP7.5inch HAT ( not the BC version - needs to use another library for that)
  If GLCD_TYPE = GLCD_TYPE_EPD_EPD7in5 Then

     ' #include <epd_epd7in5.h>
     InitGLCD = Init_EPD7in5
     GLCDCLS  = CLS_EPD7in5
     GLCDDrawChar = DrawChar_EPD7in5
     GLCDDrawString = DrawString_EPD7in5
     FilledBox = FilledBox_EPD7in5
     Pset = Pset_EPD7in5
     GLCDRotate = Rotate_EPD7in5
     GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_EPD7in5
     GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_EPD7in5
     GLCDSleep = Display_EPD7in5
     GLCDDisplay = Display_EPD7in5
     GLCD_TYPE_STRING = "EPD7in5"
     GLCD_WIDTH =  640
     GLCD_HEIGHT = 384

     if GLCD_TYPE = GLCD_TYPE_EPD_EPD7in5 then
       BUFFWIDTH= 30720
       EPD_N_PAGE=  1
     end if
     if GLCD_TYPE_EPD7in5_LOWMEMORY1_GLCD_MODE then
       BUFFWIDTH= 960
       EPD_N_PAGE=  32
     end if
     if GLCD_TYPE_EPD7in5_LOWMEMORY2_GLCD_MODE then
       BUFFWIDTH= 1920
       EPD_N_PAGE=  16
     end if
     if GLCD_TYPE_EPD7in5_LOWMEMORY3_GLCD_MODE then
       BUFFWIDTH= 3840
       EPD_N_PAGE=  8
     end if
     if GLCD_TYPE_EPD7in5_LOWMEMORY4_GLCD_MODE then
       BUFFWIDTH =7680
       EPD_N_PAGE=4

     end if


     'Resolve BUFFER when SPIRam is available
     if SPISRAM_TYPE then
       'One SRAM buffer. No issues... just need a lot of RAM in the chip
       BUFFWIDTH = 30720
       EPD_N_PAGE=  1
     end if


     if GLCD_TYPE = GLCD_TYPE_EPD_EPD7in5 then

       GLCD_WIDTH8 = GLCD_WIDTH / 8
       EPD_CORRECTED_HEIGHT = int(GLCD_HEIGHT / 8)*8
       EPD_ROWS_PER_PAGE = INT(EPD_CORRECTED_HEIGHT /  EPD_N_PAGE)
       EPD_PIXELS_PER_PAGE = INT( GLCD_HEIGHT / EPD_N_PAGE )

    end if

    Ifdef GLCD_EXTENDEDFONTSET1 then

       GLCDCharCol3 = GLCDCharCol3Extended1
       GLCDCharCol4 = GLCDCharCol4Extended1
       GLCDCharCol5 = GLCDCharCol5Extended1
       GLCDCharCol6 = GLCDCharCol6Extended1
       GLCDCharCol7 = GLCDCharCol7Extended1

    End If
  End If



  If GLCD_TYPE = GLCD_TYPE_ST7567 Then
    // Supports I2C and 4wire SPI
    // Based on SSD1306 - the only difference was the INIT 
     ' #include <glcd_st7567.h>
     InitGLCD = InitGLCD_st7567
     GLCDCLS = GLCDCLS_st7567
     GLCDDrawChar = GLCDDrawChar_st7567
     FilledBox = FilledBox_st7567
     Pset = Pset_st7567
     GLCD_Open_PageTransaction = GLCD_Open_PageTransaction_st7567
     GLCD_Close_PageTransaction = GLCD_Close_PageTransaction_st7567
     GLCDSetContrast = SetContrast_st7567
     glcd_type_string = "ST7567"
     GLCD_WIDTH = 128
     GLCD_HEIGHT = 64
     st7567_GLCD_HEIGHT = GLCDDeviceHeight
     st7567_GLCD_WIDTH = GLCDDeviceWidth
     ST7567_XOFFSET = 0

  End If

If GLCD_TYPE = GLCD_TYPE_ST7789_320_240 Then
     ' #include <GLCD_ST7789.h>
     InitGLCD = InitGLCD_ST7789
     GLCDCLS = GLCDCLS_ST7789
     GLCDDrawChar = GLCDDrawChar_ST7789
     GLCDDrawString = GLCDDrawString_ST7789
     FilledBox = FilledBox_ST7789
     Pset = Pset_ST7789
     GLCDRotate = GLCDRotate_ST7789
     glcd_type_string = "ST7789"
     GLCD_WIDTH = 320
     GLCD_HEIGHT = 240
     ST7789_GLCD_HEIGHT = GLCDDeviceHeight
     ST7789_GLCD_WIDTH = GLCDDeviceWidth
  End If

    // script code for LT7686 library
  If GLCD_TYPE = LT7686_800_480_BLUE or GLCD_TYPE = LT7686_1024_600_BLUE or GLCD_TYPE = LT7686_1024_600_BLACK Then

      ' #include <glcd_lt7686.h>
      InitGLCD = InitGLCD_LT7686
      GLCDCLS = GLCDCLS_LT7686
      FilledBox = FilledBox_LT7686
      Box = Box_LT7686
      If DEF(PICAS) Then
          _Line = LINE_LT7686
      End If
      If NODEF(PICAS) Then
          Line = LINE_LT7686
      End if
      LineWidth = Line_Width_LT7686
      Circle = Circle_LT7686
      FilledCircle = FilledCircle_LT7686
      FramedFilledbox = FramedFilledbox_LT7686
      FramedFilledCircle = FramedFilledCircle_LT7686
      Ellipse = Ellipse_LT7686
      FilledEllipse = FilledEllipse_LT7686
      RoundRect = RoundRect_LT7686
      FilledRoundRect = FilledRoundRect_LT7686
      FramedFilledRoundRect = FramedFilledRoundRect_LT7686
      Triangle = Triangle_LT7686
      FilledTriangle = FilledTriangle_LT7686
      FramedFilledTriangle = FramedFilledTriangle_LT7686
      Pset = Pset_LT7686
      Cylinder = Cylinder_LT7686
      LeftUpCurve = LeftUpCurve_LT7686
      LeftDownCurve = LeftDownCurve_LT7686
      RightUpCurve = RightUpCurve_LT7686
      RightDownCurve = RightDownCurve_LT7686
      FilledLeftUpCurve = FilledLeftUpCurve_LT7686
      FilledLeftDownCurve = FilledLeftDownCurve_LT7686
      FilledRightUpCurve = FilledRightUpCurve_LT7686
      FilledRightDownCurve = FilledRightDownCurve_LT7686
      Quadrilateral = Quadrilateral_LT7686
      FilledQuadrilateral = FilledQuadrilateral_LT7686
      Pentagon = Pentagon_LT7686
      FilledPentagon = FilledPentagon_LT7686
      FilledCube = FilledCube_LT7686


      GLCDDrawChar = GLCDDrawChar_ILI9341
      GLCDDrawString = GLCDDrawString_ILI9341
      GLCDSelect_Internal_Font_Init = LT7686_Select_Internal_Font_Init
      GLCDPrint_Internal_Font = LT7686_Print_Internal_Font

      GLCDSetBackgroundColor = LT7686_SetBackgroundColor
      GLCDMakeTable = MakeTable_LT7686
      
      GLCDRotate = GLCDRotate_LT7686
      glcd_type_string = "LT7686"
      LT7686_GLCD_HEIGHT = GLCDDeviceHeight
      LT7686_GLCD_WIDTH = GLCDDeviceWidth

      IF NODEF(LT7686_STATE1_DELAY) THEN
          LT7686_STATE1_DELAY = 20
      END IF
      IF NODEF(LT7686_STATE2_DELAY) THEN
          LT7686_STATE2_DELAY = 20
      END IF

      IF NODEF(LT7686_PWM_PRESCALER_MAX) THEN
        LT7686_PWM_PRESCALER_MAX = 200
      END IF
      IF DEF(LT7686_PWM_PRESCALER_MAX) THEN
        IF LT7686_PWM_PRESCALER_MAX > 255 THEN
          Error "LT7686_PWM_PRESCALER_MAX constant cannot exceed 255"
        END IF
      END IF      
      SCRIPT_SDRAM_ITV = INT(((64000000 / 8192) / (1000 / LT7686_DRAM_FREQ))-2)
      SCRIPT_SDRAM_ITV_H = INT(INT(((64000000 / 8192) / (1000 / LT7686_DRAM_FREQ))-2) / 256)
      SCRIPT_SDRAM_ITV_L = (INT(((64000000 / 8192) / (1000 / LT7686_DRAM_FREQ))-2) AND 255)
      IF DEF(SCRIPTDEBUG) THEN
          warning SCRIPT_SDRAM_ITV
      END IF


  End If
  
  If GLCD_TYPE = LT7686_800_480_BLUE Then
      GLCD_WIDTH = 800
      GLCD_HEIGHT = 480
      LT7686_INITTFT_DATASET = LT7686_INITTFT_DATASET_800_480
  End If

  If GLCD_TYPE = LT7686_1024_600_BLUE Then
      GLCD_WIDTH = 1024
      GLCD_HEIGHT = 600
      LT7686_INITTFT_DATASET = LT7686_INITTFT_DATASET_1024_600
  End If

  If GLCD_TYPE = LT7686_1024_600_BLACK Then
      GLCD_WIDTH = 1024
      GLCD_HEIGHT = 600
  End If

#endscript


#define GLCDInit InitGLCD
  '''Initialise the GLCD device
Sub InitGLCD
    ' Empty sub DO NOT DELETE
end sub


  'Subs
  '''Clears the GLCD screen
Sub GLCDCLS
 ' initialise global variable. Required variable for Circle - DO NOT DELETE

       GLCD_yordinate = 0

End Sub

          

  '''@hide
Sub GLCDPrint(In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string, In LineColour as Long )
  Dim GLCDPrintLoc as word
  Dim GLCDPrintLen, GLCDPrint_String_Counter as Byte
  GLCDPrintLen = LCDPrintData(0)
  If GLCDPrintLen = 0 Then Exit Sub

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  GLCDPrintLoc = PrintLocX
  'Write Data
  For GLCDPrint_String_Counter = 1 To GLCDPrintLen
    GLCDDrawChar GLCDPrintLoc, PrintLocY, LCDPrintData(GLCDPrint_String_Counter), LineColour
    GLCDPrintIncrementPixelPositionMacro
  Next
 'Update the current X position for GLCDPrintString
  PrintLocX = GLCDPrintLoc

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif

End Sub


  '''Displays a message
  '''@param PrintLocX X coordinate for message
  '''@param PrintLocY Y coordinate for message
  '''@param PrintData Message to display
Sub GLCDPrint(In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string )
  'GLCD.h Sub GLCDPrint
  Dim GLCDPrintLoc  as word
  Dim GLCDPrint_String_Counter, GLCDPrintLen as byte
  GLCDPrintLen = LCDPrintData(0)
  If GLCDPrintLen = 0 Then Exit Sub

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  GLCDPrintLoc = PrintLocX
  'Write Data
  For GLCDPrint_String_Counter = 1 To GLCDPrintLen
    GLCDDrawChar  GLCDPrintLoc, PrintLocY, LCDPrintData(GLCDPrint_String_Counter)
    GLCDPrintIncrementPixelPositionMacro
  Next
  'Update the current X position for GLCDPrintString
  PrintLocX = GLCDPrintLoc

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif


End Sub

  '''@hide
Sub GLCDPrint(In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string, In LineColour as word )
  Dim GLCDPrintLoc as word
  GLCDPrintLen = LCDPrintData(0)
  If GLCDPrintLen = 0 Then Exit Sub

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  GLCDPrintLoc = PrintLocX
  'Write Data
  For GLCDPrint_String_Counter = 1 To GLCDPrintLen
    GLCDDrawChar GLCDPrintLoc, PrintLocY, LCDPrintData(GLCDPrint_String_Counter), LineColour
    GLCDPrintIncrementPixelPositionMacro
  Next
  'Update the current X position for GLCDPrintString
  PrintLocX = GLCDPrintLoc

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif


End Sub


  '''Displays a message
  '''@param PrintLocX X coordinate for message
  '''@param PrintLocY Y coordinate for message
  '''@param PrintData Message to display
  '''@param Fontsize to use
Sub GLCDPrintWithSize(In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string, In _GLCDPrintSize as byte, Optional In _LineColour as word = GLCDForeground )
  'GLCD.h Sub GLCDPrintWithSize for strings

  Dim GLCDPrintLoc  as word
  Dim GLCDPrint_String_Counter, GLCDPrintLen as byte

  'Support for optional parameter Store font size
  Dim Old_GLCDPrintSize as byte
  Old_GLCDPrintSize = GLCDFntDefaultSize
  GLCDFntDefaultSize = _GLCDPrintSize

  Dim Old_LineColour as word
  Old_LineColour = GLCDForeground
  GLCDForeground = _LineColour

  GLCDPrintLen = LCDPrintData(0)
  If GLCDPrintLen = 0 Then Exit Sub

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  'Update the current X position for GLCDPrintString
   GLCDPrintLoc = PrintLocX

  'Write Data
  For GLCDPrint_String_Counter = 1 To GLCDPrintLen
    GLCDDrawChar  GLCDPrintLoc, PrintLocY, LCDPrintData(GLCDPrint_String_Counter)
    GLCDPrintIncrementPixelPositionMacro
  Next

  'Update the current X position for GLCDPrintString

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif

  'Restore font size
  GLCDFntDefaultSize = Old_GLCDPrintSize
  GLCDForeground = Old_LineColour

End Sub




  '''Displays a number
  '''@param PrintLocX X coordinate for message
  '''@param PrintLocY Y coordinate for message
  '''@param LCDValue Number to display
Sub GLCDPrint(In PrintLocX as word, In PrintLocY as word, In LCDValue As Long)
  Dim SysCalcTempA As Long
  Dim GLCDPrintLoc as word
  Dim SysPrintBuffer(10)
  SysPrintBuffLen = 0

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  Do
    'Divide number by 10, remainder into buffer
    SysPrintBuffLen += 1
    SysPrintBuffer(SysPrintBuffLen) = LCDValue % 10
    LCDValue = SysCalcTempA
  Loop While LCDValue <> 0

  'Display
  GLCDPrintLoc = PrintLocX
  For GLCDPrint_String_Counter = SysPrintBuffLen To 1 Step -1
    GLCDDrawChar GLCDPrintLoc, PrintLocY, SysPrintBuffer(GLCDPrint_String_Counter) + 48
    GLCDPrintIncrementPixelPositionMacro
  Next

  'Update the current X position for GLCDPrintString
  PrintLocX = GLCDPrintLoc

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif


End Sub

  '''@hide
Sub GLCDPrint(In PrintLocX as word, In PrintLocY as word, In LCDValue As Long, In LineColour as word )
  Dim SysCalcTempA As Long
  Dim GLCDPrintLoc as word
  Dim SysPrintBuffer(10)
  SysPrintBuffLen = 0

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  Do
    'Divide number by 10, remainder into buffer
    SysPrintBuffLen += 1
    SysPrintBuffer(SysPrintBuffLen) = LCDValue % 10
    LCDValue = SysCalcTempA
  Loop While LCDValue <> 0

    'Display
  GLCDPrintLoc = PrintLocX
  For GLCDPrint_String_Counter = SysPrintBuffLen To 1 Step -1
    GLCDDrawChar GLCDPrintLoc, PrintLocY, SysPrintBuffer(GLCDPrint_String_Counter) + 48, LineColour
    GLCDPrintIncrementPixelPositionMacro
  Next

  'Update the current X position for GLCDPrintString
  PrintLocX = GLCDPrintLoc

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif


End Sub


  '''Displays a number
  '''@param PrintLocX X coordinate for message
  '''@param PrintLocY Y coordinate for message
  '''@param LCDValue Number to display
  '''@param Fontsize to use
Sub GLCDPrintWithSize(In PrintLocX as word, In PrintLocY as word, In LCDValue As Long, In _GLCDPrintSize as byte, Optional In _LineColour as word = GLCDForeground )
  'GLCD.h Sub GLCDPrintWithSize for numbers

  Dim SysCalcTempA As Long
  Dim GLCDPrintLoc as word
  Dim SysPrintBuffer(10)
  SysPrintBuffLen = 0

  'Support for optional parameter Store font size
  Dim Old_GLCDPrintSize as byte
  Old_GLCDPrintSize = GLCDFntDefaultSize
  GLCDFntDefaultSize = _GLCDPrintSize

  Dim Old_LineColour as word
  Old_LineColour = GLCDForeground
  GLCDForeground = _LineColour

  #ifdef GLCD_OLED_FONT
      dim OldGLCDFontWidth as Byte
      OldGLCDFontWidth = GLCDFontWidth
  #endif

  Do
    'Divide number by 10, remainder into buffer
    SysPrintBuffLen += 1
    SysPrintBuffer(SysPrintBuffLen) = LCDValue % 10
    LCDValue = SysCalcTempA
  Loop While LCDValue <> 0

  'Display
  GLCDPrintLoc = PrintLocX
  For GLCDPrint_String_Counter = SysPrintBuffLen To 1 Step -1
    GLCDDrawChar GLCDPrintLoc, PrintLocY, SysPrintBuffer(GLCDPrint_String_Counter) + 48
    GLCDPrintIncrementPixelPositionMacro
  Next

  #ifdef GLCD_OLED_FONT
      GLCDFontWidth = OldGLCDFontWidth
  #endif

  PrintLocX = GLCDPrintLoc
  'Restore font size
  GLCDFntDefaultSize = Old_GLCDPrintSize
  GLCDForeground = Old_LineColour

End Sub


Macro GLCDPrintIncrementPixelPositionMacro

    #ifndef GLCD_OLED_FONT
      GLCDPrintLoc = GLCDPrintLoc + ( GLCDFontWidth * GLCDfntDefaultsize )+1
    #endif

    #ifdef GLCD_OLED_FONT
      GLCDPrintLoc = GLCDPrintLoc + ( GLCDFontWidth * GLCDfntDefaultsize ) + GLCDfntDefaultsize - 1
    #endif

End Macro


Sub GLCDPrintString ( in LCDPrintData as string )
    dim PrintLocX as word

    'Print at the current X and Y post
    GLCDPrint( PrintLocX , PrintLocY , LCDPrintData )
End Sub


Sub GLCDPrintStringLn( in LCDPrintData as string )
    dim PrintLocX, PrintLocY as word

    GLCDPrint( PrintLocX , PrintLocY , LCDPrintData )

    'Update the current X and Y position
    PrintLocX = 0

    'When using Transaction  PrintLocY will get incremented. This needs to be protected
    #if GLCD_TYPE =  GLCD_TYPE_EPD_EPD2in13D
        PrintLocY = ( PrintLocY + ( GLCDfntDefaultHeight * GLCDfntDefaultSize ) ) mod EPD_CORRECTED_HEIGHT
    #endif

    #if GLCD_TYPE <>  GLCD_TYPE_EPD_EPD2in13D
       PrintLocY = ( PrintLocY + ( GLCDfntDefaultHeight * GLCDfntDefaultSize ) ) mod GLCD_HEIGHT
    #endif


End Sub

Sub GLCDLocateString( in PrintLocX as word, in PrintLocY as word )
   dim PrintLocY as word

   if PrintLocY < 2 then
      PrintLocY = 0
   else

     PrintLocY--

     #if GLCD_TYPE =  GLCD_TYPE_EPD_EPD2in13D
        PrintLocY = ( PrintLocY * ( GLCDfntDefaultHeight * GLCDfntDefaultSize ) ) mod EPD_CORRECTED_HEIGHT
     #endif

     #if GLCD_TYPE <>  GLCD_TYPE_EPD_EPD2in13D
         PrintLocY = ( PrintLocY * ( GLCDfntDefaultHeight * GLCDfntDefaultSize ) ) mod GLCD_HEIGHT
     #endif

    end if
End Sub
  '
  ''''Draws a string at the specified location on the ST7920 GLCD
  ''''@param StringLocX X coordinate for message
  ''''@param CharLocY Y coordinate for message
  ''''@param Chars String to display
  ''''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString( In StringLocX, In CharLocY, In Chars as string, Optional In LineColour as word = GLCDForeground )

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
Sub GLCDDrawChar(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This has got a tad complex
  'We have three major pieces
  '1 The preamble - this just adjusted color and the input character
  '2 The code that deals with GCB fontset
  '3 The code that deals with OLED fontset
  '
  'You can make independent change to section 2 and 3 but they are mutual exclusive with many common pieces

    'invert colors if required
    if LineColour <> GLCDForeground  then
      'Inverted Colours
      GLCDBackground = 1
      GLCDForeground = 0
    end if

   dim CharCol, CharRow as word
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

                    CharRowS=0
                    For Row=1 to GLCDfntDefaultsize

                        if CurrCharVal.0=1 then
                           PSet [word]CharLocX + CharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, LineColour
                        Else
                           PSet [word]CharLocX + CharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, GLCDBackground
                        End if
                        'Put out a white intercharacter pixel/space
                         PSet [word]CharLocX + ( GLCDFontWidth * GLCDfntDefaultsize) , [word]CharLocY + CharRow + CharRowS , GLCDBackground
                        CharRowS +=1


                    Next Row


                    CharColS +=1

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
              GLCDFontWidth = COLSperfont + 1
              if LocalCharCode = 1 then
                  GLCDFontWidth = 1
              else
                  GLCDFontWidth = COLSperfont+1
              end if
              ROWSperfont = 7

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
                  CurrCharVal = 255
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
                    CurrCharVal = 255
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
    'Restore
    GLCDBackground = 0
    GLCDForeground = 1

End Sub

  '''Draws a box on the GLCD screen
  '''@param LineX1 Top left corner X location
  '''@param LineY1 Top left corner Y location
  '''@param LineX2 Bottom right corner X location
  '''@param LineY2 Bottom right corner Y location
  '''@param LineColour Colour of box border (0 = erase, 1 = draw, default is 1)
#define GLCDBox Box
Sub Box(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)
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

  dim DrawLine as word
  'Draw lines going across
  For DrawLine = LineX1 To LineX2

    PSet DrawLine, LineY1, LineColour
    PSet DrawLine, LineY2, LineColour
  Next
  'Draw lines going down
  For DrawLine = LineY1 To LineY2
    PSet LineX1, DrawLine, LineColour
    PSet LineX2, DrawLine, LineColour
  Next

End Sub
Sub Box(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Long = GLCDForeground)
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

  dim DrawLine as word
  'Draw lines going across
  For DrawLine = LineX1 To LineX2

    PSet DrawLine, LineY1, LineColour
    PSet DrawLine, LineY2, LineColour
  Next
  'Draw lines going down
  For DrawLine = LineY1 To LineY2
    PSet LineX1, DrawLine, LineColour
    PSet LineX2, DrawLine, LineColour
  Next

End Sub


  '''Draws a filled box on the GLCD screen
  '''@param LineX1 Top left corner X location
  '''@param LineY1 Top left corner Y location
  '''@param LineX2 Bottom right corner X location
  '''@param LineY2 Bottom right corner Y location
  '''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
#define GCLDFilledBox FilledBox
Sub FilledBox(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)
    ' Each Device has a specific FilledBox routine
End Sub

  '''Draws a circle on the GLCD screen
  '''@param Xoffset X point of circle
  '''@param Yoffset Y point of circle
  '''@param Inxradius radius of circle
  '''@param LineColour Colour of line (0 = blank, 1 = show, default is 1)
  '''@param yordinate (optional) rounding
#define GCLDCircle Circle
sub Circle ( in xoffset as word, in yoffset as word, in Inxradius as integer, Optional In LineColour as word = GLCDForeground , Optional In yordinate = GLCD_yordinate)


    dim  radiusErr, xradius as Integer
    xradius = Inxradius
    radiusErr = -(xradius/2)
    Do While xradius >=  yordinate
       Pset ((xoffset + xradius), (yoffset + yordinate), LineColour)
       Pset ((xoffset + yordinate), (yoffset + xradius), LineColour)
       Pset ((xoffset - xradius), (yoffset + yordinate), LineColour)
       Pset ((xoffset - yordinate), (yoffset + xradius), LineColour)
       Pset ((xoffset - xradius), (yoffset - yordinate), LineColour)
       Pset ((xoffset - yordinate), (yoffset - xradius), LineColour)
       Pset ((xoffset + xradius), (yoffset - yordinate), LineColour)
       Pset ((xoffset + yordinate), (yoffset - xradius), LineColour)
       yordinate ++
       If radiusErr < 0 Then
          radiusErr = radiusErr + 2 * yordinate + 1
       else
          xradius --
          radiusErr = radiusErr + 2 * (yordinate - xradius + 1)
       end if
    Loop

end sub

sub Circle ( in xoffset as word, in yoffset as word, in Inxradius as integer, Optional In LineColour as Long = GLCDForeground , Optional In yordinate = GLCD_yordinate)


    dim  radiusErr, xradius as Integer
    xradius = Inxradius
    radiusErr = -(xradius/2)
    Do While xradius >=  yordinate
       Pset ((xoffset + xradius), (yoffset + yordinate), LineColour)
       Pset ((xoffset + yordinate), (yoffset + xradius), LineColour)
       Pset ((xoffset - xradius), (yoffset + yordinate), LineColour)
       Pset ((xoffset - yordinate), (yoffset + xradius), LineColour)
       Pset ((xoffset - xradius), (yoffset - yordinate), LineColour)
       Pset ((xoffset - yordinate), (yoffset - xradius), LineColour)
       Pset ((xoffset + xradius), (yoffset - yordinate), LineColour)
       Pset ((xoffset + yordinate), (yoffset - xradius), LineColour)
       yordinate ++
       If radiusErr < 0 Then
          radiusErr = radiusErr + 2 * yordinate + 1
       else
          xradius --
          radiusErr = radiusErr + 2 * (yordinate - xradius + 1)
       end if
    Loop

end sub

  '''Fills a circle on the GLCD screen
  '''@param Xoffset X point of circle
  '''@param Yoffset Y point of circle
  '''@param xradius radius of circle
  '''@param LineColour Colour of line (0 = blank, 1 = show, default is 1)
#define GLCDFilledCircle FilledCircle
sub FilledCircle( in xoffset as word, in yoffset as word, in xradius as word, Optional In LineColour as word = GLCDForeground)

    'Circle fill Code is merely a modification of the midpoint
    ' circle algorithem which is an adaption of Bresenham's line algorithm
    ' http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
    ' http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm

  dim ff, ddF_x, ddF_y as integer
  dim YCalc2, YCalc1 as word
  ff = 1 - xradius
  ddF_x = 1
  ddF_y = -2 * xradius
  FillCircleXX = 0
  FillCircleYY = xradius

  ' Fill in the center between the two halves
          YCalc2 = yoffset+xradius
          YCalc1 = yoffset-xradius
  Line( xoffset, YCalc1 , xoffset, YCalc2, LineColour)

  do while (FillCircleXX < FillCircleYY)
             if ff >= 0 then
                FillCircleYY--
                ddF_y += 2
                ff += ddF_y
             end if
     FillCircleXX++
     ddF_x += 2
     ff += ddF_x
             ' Now draw vertical lines between the points on the circle rather than
             ' draw the points of the circle. This draws lines between the
             ' perimeter points on the upper and lower quadrants of the 2 halves of the circle.

     Line(xoffset+FillCircleXX, yoffset+FillCircleYY, xoffset+FillCircleXX, yoffset-FillCircleYY, LineColour);
     Line(xoffset-FillCircleXX, yoffset+FillCircleYY, xoffset-FillCircleXX, yoffset-FillCircleYY, LineColour);
     Line(xoffset+FillCircleYY, yoffset+FillCircleXX, FillCircleYY+xoffset, yoffset-FillCircleXX, LineColour);
     Line(xoffset-FillCircleYY, yoffset+FillCircleXX, xoffset-FillCircleYY, yoffset-FillCircleXX, LineColour);
    loop
end sub

sub FilledCircle( in xoffset as word, in yoffset as word, in xradius as word, Optional In LineColour as Long = GLCDForeground)

    'Circle fill Code is merely a modification of the midpoint
    ' circle algorithem which is an adaption of Bresenham's line algorithm
    ' http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
    ' http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm

  dim ff, ddF_x, ddF_y as integer
  dim YCalc2, YCalc1 as word
  ff = 1 - xradius
  ddF_x = 1
  ddF_y = -2 * xradius
  FillCircleXX = 0
  FillCircleYY = xradius

  ' Fill in the center between the two halves
          YCalc2 = yoffset+xradius
          YCalc1 = yoffset-xradius
  Line( xoffset, YCalc1 , xoffset, YCalc2, LineColour)

  do while (FillCircleXX < FillCircleYY)
             if ff >= 0 then
                FillCircleYY--
                ddF_y += 2
                ff += ddF_y
             end if
     FillCircleXX++
     ddF_x += 2
     ff += ddF_x
             ' Now draw vertical lines between the points on the circle rather than
             ' draw the points of the circle. This draws lines between the
             ' perimeter points on the upper and lower quadrants of the 2 halves of the circle.

     Line(xoffset+FillCircleXX, yoffset+FillCircleYY, xoffset+FillCircleXX, yoffset-FillCircleYY, LineColour);
     Line(xoffset-FillCircleXX, yoffset+FillCircleYY, xoffset-FillCircleXX, yoffset-FillCircleYY, LineColour);
     Line(xoffset+FillCircleYY, yoffset+FillCircleXX, FillCircleYY+xoffset, yoffset-FillCircleXX, LineColour);
     Line(xoffset-FillCircleYY, yoffset+FillCircleXX, xoffset-FillCircleYY, yoffset-FillCircleXX, LineColour);
    loop
end sub


  '''Draws a line on the GLCD screen
  '''@param LineX1 Starting X point of line
  '''@param LineY1 Starting Y point of line
  '''@param LineX2 Ending X point of line
  '''@param LineY2 Ending Y point of line
  '''@param LineColour Colour of line (0 = blank, 1 = show, default is 1)
#define GLCDLine Line
Sub LINE_LongColor(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour as Long = GLCDForeground)

      dim LineStepX as integer
      dim LineStepY as integer
      dim LineDiffX, LineDiffY as integer
      dim LineDiffX_x2, LineDiffY_x2 as integer
      dim LineErr as integer



      LineDiffX = 0
      LineDiffY = 0
      LineStepX = 0
      LineStepY = 0
      LineDiffX_x2 = 0
      LineDiffY_x2 = 0
      LineErr = 0


      LineDiffX =  LineX2 -   LineX1
      LineDiffY =  LineY2 -   LineY1

      if (LineDiffX > 0) then
              LineStepX = 1
      else
              LineStepX = -1
      end if

      if (LineDiffY > 0) then
          LineStepY = 1
       else
          LineStepY = -1
      end if

      LineDiffX = LineStepX * LineDiffX
      LineDiffY = LineStepY * LineDiffY

      LineDiffX_x2 = LineDiffX*2
      LineDiffY_x2 = LineDiffY*2

      if ( LineDiffX >= LineDiffY) then

          LineErr = LineDiffY_x2 - LineDiffX

          do while (   LineX1 <>  LineX2 )

              PSet (   LineX1,   LineY1, LineColour )
              LineX1 += LineStepX
              if ( LineErr < 0) then
                  LineErr += LineDiffY_x2
              else
                  LineErr += ( LineDiffY_x2 - LineDiffX_x2 )
                  LineY1 += LineStepY
              end if
          loop

          PSet (   LineX1,   LineY1, LineColour )
      else

          LineErr = LineDiffX_x2 - LineDiffY
          do while (   LineY1 <>  LineY2)
              PSet (   LineX1,   LineY1, LineColour )
              LineY1 += LineStepY
              if ( LineErr < 0) then
                  LineErr += LineDiffX_x2
               else
                  LineErr += ( LineDiffX_x2 - LineDiffY_x2 )
                  LineX1 += LineStepX
              end if
          loop
          PSet (   LineX1,   LineY1, LineColour )

      end if



end sub

Sub Line(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour as word = GLCDForeground)

      dim LineStepX as integer
      dim LineStepY as integer
      dim LineDiffX, LineDiffY as integer
      dim LineDiffX_x2, LineDiffY_x2 as integer
      dim LineErr as integer



      LineDiffX = 0
      LineDiffY = 0
      LineStepX = 0
      LineStepY = 0
      LineDiffX_x2 = 0
      LineDiffY_x2 = 0
      LineErr = 0


      LineDiffX =  LineX2 -   LineX1
      LineDiffY =  LineY2 -   LineY1

      if (LineDiffX > 0) then
              LineStepX = 1
      else
              LineStepX = -1
      end if

      if (LineDiffY > 0) then
          LineStepY = 1
       else
          LineStepY = -1
      end if

      LineDiffX = LineStepX * LineDiffX
      LineDiffY = LineStepY * LineDiffY

      LineDiffX_x2 = LineDiffX*2
      LineDiffY_x2 = LineDiffY*2

      if ( LineDiffX >= LineDiffY) then

          LineErr = LineDiffY_x2 - LineDiffX

          do while (   LineX1 <>  LineX2 )

              PSet (   LineX1,   LineY1, LineColour )
              LineX1 += LineStepX
              if ( LineErr < 0) then
                  LineErr += LineDiffY_x2
              else
                  LineErr += ( LineDiffY_x2 - LineDiffX_x2 )
                  LineY1 += LineStepY
              end if
          loop

          PSet (   LineX1,   LineY1, LineColour )
      else

          LineErr = LineDiffX_x2 - LineDiffY
          do while (   LineY1 <>  LineY2)
              PSet (   LineX1,   LineY1, LineColour )
              LineY1 += LineStepY
              if ( LineErr < 0) then
                  LineErr += LineDiffX_x2
               else
                  LineErr += ( LineDiffX_x2 - LineDiffY_x2 )
                  LineX1 += LineStepX
              end if
          loop
          PSet (   LineX1,   LineY1, LineColour )

      end if



end sub






  '''Draws a pixel on the GLCD
  '''@param GLCDX X coordinate of pixel
  '''@param GLCDY Y coordinate of pixel
  '''@param GLCDColour State of pixel (0 = erase, 1 = display)
Sub PSet(In GLCDX, In GLCDY, In GLCDColour As Word)
    ' Each Device has a specific PSET routine
End Sub


sub GLCDRotate ( in AddressType )
    ' Empty routine  - do not delete
end sub

sub SetGLCDTouchSize( Current_GLCD_WIDTH as word, Current_GLCD_HEIGHT as word  )
    dim Current_GLCD_WIDTH, Current_GLCD_HEIGHT as word

     'this simply set the variables
End Sub


  '''Draws a Horizontal Line on the GLCD with a
  '''@param GLCDX1 X coordinate of one end of the line
  '''@param GLCDX2 X coordinate of the other end of the line
  '''@param GLCDY1 Y coordinate of the line
  '''@param LineColour color
Sub HLine(In GLCDX1 as word, In GLCDX2 as word, In GLCDY1 as word, Optional In LineColour As Word = GLCDForeground)
 GLCDTemp=[Word]GLCDY1+GLCDLineWidth-1
 Box( GLCDX1, GLCDY1, GLCDX2, GLCDTemp, LineColour )
end Sub

  '''Draws a Vertical Line on the GLCD with a
  '''@param GLCDY1 Y coordinate of one end of the line
  '''@param GLCDY2 Y coordinate of the other end of the line
  '''@param GLCDX1 X coordinate of the line
  '''@param LineColour color
Sub VLine(In GLCDY1 as word, In GLCDY2 as word, In  GLCDX1 as word, Optional In LineColour As Word = GLCDForeground)
 GLCDTemp=[Word]GLCDX1+GLCDLineWidth-1
 Box( GLCDX1, GLCDY1, GLCDTemp, GLCDY2, LineColour )
End Sub

Sub VLine(In GLCDY1 as word, In GLCDY2 as word, In  GLCDX1 as word, Optional In LineColour As Long = GLCDForeground)
 GLCDTemp=[Word]GLCDX1+GLCDLineWidth-1
 Box( GLCDX1, GLCDY1, GLCDTemp, GLCDY2, LineColour )
End Sub



Sub RoundRect(In  RX1 as Word, In  RY1 as Word, In  RX2 as Word, In  RY2 as Word, Optional In  Color as Word=GLCDForeground)
    if RX1>RX2 then SWAP (RX1 , RX2)
    if RY1>RY2 then SWAP (RY1 , RY2)
    if (RX2-RX1>4) and (RY2-RY1>4) Then
       Pset (RX1+1, RY1+2 , Color)
       Pset (RX1+2, RY1+1 , Color)
       Pset (RX2-2, RY1+1 , Color)
       Pset (RX2-1, RY1+2 , Color)
       Pset (RX1+1, RY2-2 , Color)
       Pset (RX1+2, RY2-1 , Color)
       Pset (RX2-2, RY2-1 , Color)
       Pset (RX2-1, RY2-2 , Color)
       Line RX1+3, RY1 , RX2-3, RY1, Color
       Line RX1+3, RY2 , RX2-3, RY2, Color
       Line RX1 , RY1+3, RX1 , RY2-3, Color
       Line RX2 , RY1+3, RX2 , RY2-3, Color
    end if
End Sub

Sub RoundRect(In  RX1 as Word, In  RY1 as Word, In  RX2 as Word, In  RY2 as Word, Optional In  Color as Long=GLCDForeground)
    if RX1>RX2 then SWAP (RX1 , RX2)
    if RY1>RY2 then SWAP (RY1 , RY2)
    if (RX2-RX1>4) and (RY2-RY1>4) Then
       Pset (RX1+1, RY1+2 , Color)
       Pset (RX1+2, RY1+1 , Color)
       Pset (RX2-2, RY1+1 , Color)
       Pset (RX2-1, RY1+2 , Color)
       Pset (RX1+1, RY2-2 , Color)
       Pset (RX1+2, RY2-1 , Color)
       Pset (RX2-2, RY2-1 , Color)
       Pset (RX2-1, RY2-2 , Color)
       Line RX1+3, RY1 , RX2-3, RY1, Color
       Line RX1+3, RY2 , RX2-3, RY2, Color
       Line RX1 , RY1+3, RX1 , RY2-3, Color
       Line RX2 , RY1+3, RX2 , RY2-3, Color
    end if
End Sub

Sub FillRoundRect(In  RX1 as Word, In  RY1 as Word, In  RX2 as Word, In  RY2 as Word, Optional In  Color as Word=GLCDForeground)
    Dim FRI as Word
    if RX1>RX2 then SWAP (RX1 , RX2)
    if RY1>RY2 then SWAP (RY1 , RY2)
    if (RX2-RX1>4) and (RY2-RY1>4) Then
       for FRI=0 to (RY2-RY1)/2
           Select Case FRI
               Case 0
                  Line RX1+3 , RY1 , RX2-3, RY1 , Color
                  Line RX1+3 , RY2 , RX2-3, RY2 , Color
               Case 1
                  Line RX1+2 , RY1+1 , RX2-2, RY1+1 , Color
                  Line RX1+2 , RY2-1 , RX2-1, RY2-1 , Color
               Case 2
                  Line RX1+1 , RY1+2 , RX2-1, RY1+2 , Color
                  Line RX1+1 , RY2-2 , RX2-1, RY2-2 , Color
               Case Else
                  Line RX1 , RY1+FRI , RX2, RY1+FRI , Color
                  Line RX1 , RY2-FRI , RX2, RY2-FRI , Color
           End Select
       Next
    End if
End Sub

Sub FillRoundRect(In  RX1 as Word, In  RY1 as Word, In  RX2 as Word, In  RY2 as Word, Optional In  Color as Long=GLCDForeground)
    Dim FRI as Word
    if RX1>RX2 then SWAP (RX1 , RX2)
    if RY1>RY2 then SWAP (RY1 , RY2)
    if (RX2-RX1>4) and (RY2-RY1>4) Then
       for FRI=0 to (RY2-RY1)/2
           Select Case FRI
               Case 0
                  Line RX1+3 , RY1 , RX2-3, RY1 , Color
                  Line RX1+3 , RY2 , RX2-3, RY2 , Color
               Case 1
                  Line RX1+2 , RY1+1 , RX2-2, RY1+1 , Color
                  Line RX1+2 , RY2-1 , RX2-1, RY2-1 , Color
               Case 2
                  Line RX1+1 , RY1+2 , RX2-1, RY1+2 , Color
                  Line RX1+1 , RY2-2 , RX2-1, RY2-2 , Color
               Case Else
                  Line RX1 , RY1+FRI , RX2, RY1+FRI , Color
                  Line RX1 , RY2-FRI , RX2, RY2-FRI , Color
           End Select
       Next
    End if
End Sub

  ' CreateButton_SSD1289 method creates Botton on screen.
  'Parameters
  'BX1,BY1,BX2,BY2 coordinates of Botton
  'FillColor ,
  'BorderColor the colors of Botton
  'PrintData the text in center of Botton
  'FColor The color of text and
  'Size the size of characters
Sub CreateButton (In BX1 as Word, In BY1 as Word , In BX2 as Word , In BY2 as Word , In FillColor as Word, In BorderColor as Word, In PrintData As String, In FColor as Word, In Size = 1)
    Dim TempColor1 , TempColor2 as Word
    Dim XCalc1 , YCalc1 as Word

    FillRoundRect(BX1, BY1, BX2, BY2, FillColor)
    RoundRect(BX1, BY1, BX2, BY2, BorderColor)
    XCalc1=(BX1+BX2)/2-PrintData(0)*6*Size/2
    YCalc1=(BY1+BY2)/2-4*Size
    TempColor1=GLCDBackground
    TempColor2= GLCDForeground
    GLCDBackground=FillColor

    GLCDPrint( XCalc1, YCalc1, PrintData, FColor, Size )
    GLCDBackground=TempColor1
    GLCDForeground=TempColor2
End Sub

  '''@hide
'Character bitmaps for print routines
Table GLCDCharCol3
  0
  16
  12
  10
  136
  34
  56
  32
  8
  32
  16
  16
  128
  128
  64
  4
  0
  0
  0
  40
  72
  70
  108
  0
  0
  0
  40
  16
  0
  16
  0
  64
  124
  0
  132
  130
  48
  78
  120
  6
  108
  12
  0
  0
  16
  40
  0
  4
  100
  248
  254
  124
  254
  254
  254
  124
  254
  0
  64
  254
  254
  254
  254
  124
  254
  124
  254
  76
  2
  126
  62
  126
  198
  14
  194
  0
  4
  0
  8
  128
  0
  64
  254
  112
  112
  112
  16
  16
  254
  0
  64
  254
  0
  248
  248
  112
  248
  16
  248
  144
  16
  120
  56
  120
  136
  24
  136
  0
  0
  0
  32
  120
End Table

Table GLCDCharCol4
  254
  56
  10
  6
  204
  102
  124
  112
  4
  64
  16
  56
  136
  162
  112
  28
  0
  0
  14
  254
  84
  38
  146
  10
  56
  130
  16
  16
  160
  16
  192
  32
  162
  132
  194
  130
  40
  138
  148
  2
  146
  146
  108
  172
  40
  40
  130
  2
  146
  36
  146
  130
  130
  146
  18
  130
  16
  130
  128
  16
  128
  4
  8
  130
  18
  130
  18
  146
  2
  128
  64
  128
  40
  16
  162
  254
  8
  130
  4
  128
  2
  168
  144
  136
  136
  168
  252
  168
  16
  144
  128
  32
  130
  8
  16
  136
  40
  40
  16
  168
  124
  128
  64
  128
  80
  160
  200
  16
  0
  130
  16
  68
End Table

Table GLCDCharCol5
  124
  124
  0
  0
  238
  238
  124
  168
  254
  254
  84
  84
  148
  148
  124
  124
  0
  158
  0
  40
  254
  16
  170
  6
  68
  68
  124
  124
  96
  16
  192
  16
  146
  254
  162
  138
  36
  138
  146
  226
  146
  146
  108
  108
  68
  40
  68
  162
  242
  34
  146
  130
  130
  146
  18
  146
  16
  254
  130
  40
  128
  24
  16
  130
  18
  162
  50
  146
  254
  128
  128
  112
  16
  224
  146
  130
  16
  130
  2
  128
  4
  168
  136
  136
  136
  168
  18
  168
  8
  250
  136
  80
  254
  240
  8
  136
  40
  40
  8
  168
  144
  128
  128
  96
  32
  160
  168
  108
  254
  108
  16
  66
End Table

Table GLCDCharCol6
  56
  254
  12
  10
  204
  102
  124
  32
  4
  64
  56
  16
  162
  136
  112
  28
  0
  0
  14
  254
  84
  200
  68
  0
  130
  56
  16
  16
  0
  16
  0
  8
  138
  128
  146
  150
  254
  138
  146
  18
  146
  82
  0
  0
  130
  40
  40
  18
  130
  36
  146
  130
  68
  146
  18
  146
  16
  130
  126
  68
  128
  4
  32
  130
  18
  66
  82
  146
  2
  128
  64
  128
  40
  16
  138
  130
  32
  254
  4
  128
  8
  168
  136
  136
  144
  168
  2
  168
  8
  128
  122
  136
  128
  8
  8
  136
  40
  48
  8
  168
  128
  64
  64
  128
  80
  160
  152
  130
  0
  16
  32
  68
End Table

Table GLCDCharCol7
  16
  0
  10
  6
  136
  34
  56
  62
  8
  32
  16
  16
  128
  128
  64
  4
  0
  0
  0
  40
  36
  196
  160
  0
  0
  0
  40
  16
  0
  16
  0
  4
  124
  0
  140
  98
  32
  114
  96
  14
  108
  60
  0
  0
  0
  40
  16
  12
  124
  248
  108
  68
  56
  130
  2
  244
  254
  0
  2
  130
  128
  254
  254
  124
  12
  188
  140
  100
  2
  126
  62
  126
  198
  14
  134
  0
  64
  0
  8
  128
  0
  240
  112
  64
  254
  48
  4
  120
  240
  0
  0
  0
  0
  240
  240
  112
  16
  248
  16
  64
  64
  248
  56
  120
  136
  120
  136
  0
  0
  0
  16
  120
End Table


'Character bitmaps for print routines
Table GLCDCharCol3Extended1
  0
  16
  12
  10
  136
  34
  56
  32
  8
  32
  16
  16
  128
  128
  64
  4
  0
  0
  0
  40
  72
  70
  108
  0
  0
  0
  40
  16
  0
  16
  0
  64
  124
  0
  132
  130
  48
  78
  120
  6
  108
  12
  0
  0
  16
  40
  0
  4
  100
  248  'A
  254  'B
  124  'C
  254  'D
  254  'E
  254  'F
  124  'G
  254  'H
  0    'I
  64   'J
  254  'K
  254  'L
  254  'M
  254  'N
  124  'O
  254  'P
  124  'Q
  254  'R
  76   'S
  2    'T
  126  'U
  62   'V
  126  'W
  198  'X
  14   'Y
  194  'Z
  0
  4
  0
  8
  128
  0
  64
  254
  112
  112
  112
  16
  16
  254
  0
  64
  254
  0
  248
  248
  112
  248
  16
  248
  144
  16
  120
  56
  120
  136
  24
  136
  0
  0
  0
  32
  120 '127
  0
  0
  0
  0
  0
  0
  0     '134 ?
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  248   '193 ?
  254   '194 B
  254   '195 ?
  248   '196 ?
  254   '197 E
  194   '198 Z
  254   '199 H
  124   '200 ?
  0     '201 ?
  254   '202 ?
  248   '203 ?
  254   '204 ?
  254   '205 ?
  130   '206 ?
  124   '207 ?
  254   '208 ?
  254   '209 ?
  0
  198   '211 ?
  2     '212 ?
  14    '213 ?
  56    '214 ?
  198   '215 ?
  14    '216 ?
  188   '217 ?
  0
  0
  56    '220 ?
  40    '221 ?
  124   '222 ?
  0     '223 ?
  0
  56    '225 ?
  254   '226 ?
  4     '227 ?
  48    '228 ?
  40    '229 ?
  1     '230 ?
  124   '231 ?
  48    '232 ?
  0     '233 ?
  124   '234 ?
  64    '235 ?
  252   '236 ?
  28    '237 ?
  0     '238 ?
  56    '239 ?
  4     '240 ?
  248   '241 ?
  24    '242 ?
  56    '243 ?
  4     '244 ?
  60    '245 ?
  56    '246 ?
  68    '247 ?
  60    '248 ?
  60    '249 ?
  0     '250 ?
  60    '251 ?
  56    '252 ?
  60    '253 ?
  60    '254 ?
End Table

Table GLCDCharCol4Extended1
  254
  56
  10
  6
  204
  102
  124
  112
  4
  64
  16
  56
  136
  162
  112
  28
  0
  0
  14
  254
  84
  38
  146
  10
  56
  130
  16
  16
  160
  16
  192
  32
  162
  132
  194
  130
  40
  138
  148
  2
  146
  146
  108
  172
  40
  40
  130
  2
  146
  36    '?
  146   'B
  130   'C
  130   'D
  146   'E
  18    'F
  130   'G
  16    'H
  130   'I
  128   'J
  16    'K
  128   'L
  4     'M
  8     'N
  130   'O
  18    'P
  130   'Q
  18    'R
  146   'S
  2     'T
  128   'U
  64    'V
  128   'W
  40    'X
  16    'Y
  162   'Z
  254
  8
  130
  4
  128
  2
  168
  144
  136
  136
  168
  252
  168
  16
  144
  128
  32
  130
  8
  16
  136
  40
  40
  16
  168
  124
  128
  64
  128
  80
  160
  200
  16
  0
  130
  16
  68
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  36    '193 ?
  146   '194 B
  2     '195 ?
  132   '196 ?
  146   '197 E
  162   '198 Z
  16    '199 H
  146   '200 ?
  130   '201 ?
  16    '202 ?
  4     '203 ?
  4     '204 ?
  8     '205 ?
  146   '206 ?
  130   '207 ?
  2     '208 ?
  18    '209 ?
  0
  170   '211 ?
  2     '212 ?
  16    '213 ?
  68    '214 ?
  40    '215 ?
  16    '216 ?
  194   '217 ?
  0
  0
  68    '220 ?
  84    '221 ?
  8     '222 ?
  0     '223 ?
  0
  68    '225 ?
  73    '226 ?
  104   '227 ?
  73    '228 ?
  84    '229 ?
  25    '230 ?
  8     '231 ?
  74    '232 ?
  0     '233 ?
  16    '234 ?
  50    '235 ?
  64    '236 ?
  32    '237 ?
  1     '238 ?
  68    '239 ?
  124   '240 ?
  36    '241 ?
  36    '242 ?
  68    '243 ?
  4     '244 ?
  64    '245 ?
  68    '246 ?
  40    '247 ?
  64    '248 ?
  64    '249 ?
  1     '250 ?
  65    '251 ?
  68    '252 ?
  64    '253 ?
  64    '254 ?
End Table

Table GLCDCharCol5Extended1
  124
  124
  0
  0
  238
  238
  124
  168
  254
  254
  84
  84
  148
  148
  124
  124
  0
  158
  0
  40
  254
  16
  170
  6
  68
  68
  124
  124
  96
  16
  192
  16
  146
  254
  162
  138
  36
  138
  146
  226
  146
  146
  108
  108
  68
  40
  68
  162
  242
  34   'A
  146  'B
  130  'C
  130  'D
  146  'E
  18   'F
  146  'G
  16   'H
  254  'I
  130  'J
  40   'K
  128  'L
  24   'M
  16   'N
  130  'O
  18   'P
  162  'Q
  50   'R
  146  'S
  254  'T
  128  'U
  128  'V
  112  'W
  16   'X
  224  'Y
  146  'Z
  130
  16
  130
  2
  128
  4
  168
  136
  136
  136
  168
  18
  168
  8
  250
  136
  80
  254
  240
  8
  136
  40
  40
  8
  168
  144
  128
  128
  96
  32
  160
  168
  108
  254
  108
  16
  66
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  34   '193 ?
  146  '194 B
  2    '195 ?
  130  '196 ?
  146  '197 E
  146  '198 Z
  16   '199 H
  146  '200 ?
  254  '201 ?
  40   '202 ?
  2    '203 ?
  24   '204 ?
  16   '205 ?
  146  '206 ?
  130  '207 ?
  2    '208 ?
  18   '209 ?
  0
  146  '211 ?
  254  '212 ?
  224  '213 ?
  254  '214 ?
  16   '215 ?
  254  '216 ?
  2    '217 ?
  0
  0
  69   '220 ?
  84   '221 ?
  5    '222 ?
  61   '223 ?
  0
  68    '225 ?
  73    '226 ?
  144   '227 ?
  75    '228 ?
  84    '229 ?
  165   '230 ?
  4     '231 ?
  77    '232 ?
  60    '233 ?
  40    '234 ?
  9     '235 ?
  64    '236 ?
  64    '237 ?
  149   '238 ?
  68    '239 ?
  4     '240 ?
  36    '241 ?
  164   '242 ?
  68    '243 ?
  124   '244 ?
  64    '245 ?
  254   '246 ?
  16    '247 ?
  248   '248 ?
  56    '249 ?
  60    '250 ?
  64    '251 ?
  69    '252 ?
  66    '253 ?
  58    '254 ?
End Table

Table GLCDCharCol6Extended1
  56
  254
  12
  10
  204
  102
  124
  32
  4
  64
  56
  16
  162
  136
  112
  28
  0
  0
  14
  254
  84
  200
  68
  0
  130
  56
  16
  16
  0
  16
  0
  8
  138
  128
  146
  150
  254
  138
  146
  18
  146
  82
  0
  0
  130
  40
  40
  18
  130
  36   'A
  146  'B
  130  'C
  68   'D
  146  'E
  18   'F
  146  'G
  16   'H
  130  'I
  126  'J
  68   'K
  128  'L
  4    'M
  32   'N
  130  'O
  18   'P
  66   'Q
  82   'R
  146  'S
  2    'T
  128  'U
  64   'V
  128  'W
  40   'X
  16   'Y
  138  'Z
  130
  32
  254
  4
  128
  8
  168
  136
  136
  144
  168
  2
  168
  8
  128
  122
  136
  128
  8
  8
  136
  40
  48
  8
  168
  128
  64
  64
  128
  80
  160
  152
  130
  0
  16
  32
  68
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  36   '193 ?
  146  '194 B
  2    '195 ?
  132  '196 ?
  146  '197 E
  138  '198 Z
  16   '199 H
  146  '200 ?
  130  '201 ?
  68   '202 ?
  4    '203 ?
  4    '204 ?
  32   '205 ?
  146  '206 ?
  130  '207 ?
  2    '208 ?
  18   '209 ?
  0
  130  '211 ?
  2    '212 ?
  16   '213 ?
  68   '214 ?
  40   '215 ?
  16   '216 ?
  194  '217 ?
  0
  0
  56   '220 ?
  85   '221 ?
  4    '222 ?
  64   '223 ?
  0
  56    '225 ?
  78    '226 ?
  104   '227 ?
  77    '228 ?
  84    '229 ?
  67    '230 ?
  4     '231 ?
  73    '232 ?
  64    '233 ?
  68    '234 ?
  9     '235 ?
  32    '236 ?
  32    '237 ?
  171   '238 ?
  68    '239 ?
  124   '240 ?
  36    '241 ?
  164   '242 ?
  76    '243 ?
  4     '244 ?
  64    '245 ?
  68    '246 ?
  40    '247 ?
  64    '248 ?
  64    '249 ?
  65    '250 ?
  65    '251 ?
  68    '252 ?
  65    '253 ?
  65    '254 ?
End Table

Table GLCDCharCol7Extended1
  16
  0
  10
  6
  136
  34
  56
  62
  8
  32
  16
  16
  128
  128
  64
  4
  0
  0
  0
  40
  36
  196
  160
  0
  0
  0
  40
  16
  0
  16
  0
  4
  124
  0
  140
  98
  32
  114
  96
  14
  108
  60
  0
  0
  0
  40
  16
  12
  124
  248   'A
  108   'B
  68    'C
  56    'D
  130   'E
  2     'F
  244   'G
  254   'H
  0     'I
  2     'J
  130   'K
  128   'L
  254   'M
  254   'N
  124   'O
  12    'P
  188   'Q
  140   'R
  100   'S
  2     'T
  126   'U
  62    'V
  126   'W
  198   'X
  14    'Y
  134   'Z
  0
  64
  0
  8
  128
  0
  240
  112
  64
  254
  48
  4
  120
  240
  0
  0
  0
  0
  240
  240
  112
  16
  248
  16
  64
  64
  248
  56
  120
  136
  120
  136
  0
  0
  0
  16
  120
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  248    '193 ?
  108    '194 B
  2      '195 ?
  248    '196 ?
  130    '197 E
  134    '198 Z
  254    '199 H
  124    '200 ?
  0      '201 ?
  130    '202 ?
  248    '203 ?
  254    '204 ?
  254    '205 ?
  130    '206 ?
  124    '207 ?
  254    '208 ?
  12     '209 ?
  0
  130    '211 ?
  2      '212 ?
  14     '213 ?
  56     '214 ?
  198    '215 ?
  14     '216 ?
  188    '217 ?
  0
  0
  68     '220 ?
  68     '221 ?
  120    '222 ?
  32     '223 ?
  0
  68    '225 ?
  48    '226 ?
  4     '227 ?
  49    '228 ?
  68    '229 ?
  1     '230 ?
  120   '231 ?
  62    '232 ?
  32    '233 ?
  0     '234 ?
  126   '235 ?
  124   '236 ?
  28    '237 ?
  65    '238 ?
  56    '239 ?
  4     '240 ?
  24    '241 ?
  72    '242 ?
  52    '243 ?
  4     '244 ?
  60    '245 ?
  56    '246 ?
  68    '247 ?
  60    '248 ?
  60    '249 ?
  32    '250 ?
  60    '251 ?
  56    '252 ?
  60    '253 ?
  60    '254 ?
End Table


Sub Ellipse( in xoffset as word, in yoffset as word, in Inxradius as word,in Inyradius as word,Optional In LineColour as word = GLCDForeground )

  'Version 1.00 (08/20/2017) by Joseph Realmuto
  'draws an ellipse outline at location (xoffset, yoffset)
  'Inxradius is x radius of ellipse
  'Inyradius is y radius of ellipse

    filled_ellipse = 0
    DrawEllipseRoutine( xoffset, yoffset, Inxradius, Inyradius, LineColour )

End Sub

Sub Ellipse( in xoffset as word, in yoffset as word, in Inxradius as word,in Inyradius as word,Optional In LineColour as Long = GLCDForeground )

  'Version 1.00 (08/20/2017) by Joseph Realmuto
  'draws an ellipse outline at location (xoffset, yoffset)
  'Inxradius is x radius of ellipse
  'Inyradius is y radius of ellipse

    filled_ellipse = 0
    DrawEllipseRoutine( xoffset, yoffset, Inxradius, Inyradius, LineColour )

End Sub

Sub FilledEllipse( in xoffset as word, in yoffset as word, in Inxradius as word,in Inyradius as word,Optional In LineColour as word = GLCDForeground )

  'Version 1.00 (08/20/2017) by Joseph Realmuto
  'draws a filled ellipse at location (xoffset, yoffset)
  'Inxradius is x radius of ellipse
  'Inyradius is y radius of ellipse

  filled_ellipse = 1
  DrawEllipseRoutine( xoffset, yoffset, Inxradius, Inyradius, LineColour )

End Sub

Sub FilledEllipse( in xoffset as word, in yoffset as word, in Inxradius as word,in Inyradius as word,Optional In LineColour as Long = GLCDForeground )

  'Version 1.00 (08/20/2017) by Joseph Realmuto
  'draws a filled ellipse at location (xoffset, yoffset)
  'Inxradius is x radius of ellipse
  'Inyradius is y radius of ellipse

  filled_ellipse = 1
  DrawEllipseRoutine( xoffset, yoffset, Inxradius, Inyradius, LineColour )

End Sub

Sub DrawEllipseRoutine( in xoffset as word, in yoffset as word, in Inxradius as word,in Inyradius as word,Optional In LineColour as word = GLCDForeground )

  'Version 1.00 (08/20/2017) by Joseph Realmuto
  'Version 1.01 (14/04/2019) by Giuseppe D'Elia

  'draws an ellipse outline at location (xoffset, yoffset) if filled_ellipse = 0
  'draws a filled ellipse at location (xoffset, yoffset) if filled_ellipse = 1
  'Inxradius is x radius of ellipse
  'Inyradius is y radius of ellipse

  'IF Inxradius = Inyradius THEN
  '  IF filled_ellipse = 0 THEN
  '    Circle Xoffset, Yoffset, Inxradius, LineColour
  '  ELSE
  '    FilledCircle Xoffset, Yoffset, Inxradius, LineColour
  '  END IF
  '    GLCD_exit sub
  'END IF

  IF Inxradius<2 THEN exit sub
  IF Inyradius<2 THEN exit sub

  dim GLCD_xx, GLCD_yy, GLCD_rx2, GLCD_ry2 as Word
  dim  GLCD_fx2, GLCD_fy2, GLCD_ex2, GLCD_ey2 as Long
  dim GLCD_px, GLCD_py, GLCD_pp, GLCD_pp_temp as Long

  GLCD_rx2 = Inxradius * Inxradius
  GLCD_ry2 = Inyradius * Inyradius

  'GLCD_fx2 = 4 * GLCD_rx2
  GLCD_fx2 = GLCD_rx2 + GLCD_rx2
  GLCD_fx2 = GLCD_fx2 + GLCD_fx2

  'GLCD_fy2 = 4 * GLCD_ry2
  GLCD_fy2 = GLCD_ry2 + GLCD_ry2
  GLCD_fy2 = GLCD_fy2 + GLCD_fy2

  'GLCD_ex2 = 2 * GLCD_fx2
  GLCD_ex2 = GLCD_fx2 + GLCD_fx2

  'GLCD_ey2 = 2 * GLCD_fy2
  GLCD_ey2 = GLCD_fy2 + GLCD_fy2

  GLCD_xx = 0
  GLCD_yy = Inyradius
  GLCD_px = 0
  GLCD_py = GLCD_ex2 * GLCD_yy

  'GLCD_pp = 2 + GLCD_fy2 -  GLCD_fx2 * Inyradius + GLCD_rx2
  GLCD_pp_temp = GLCD_fx2 * Inyradius
  GLCD_pp = 2 + GLCD_fy2
  GLCD_pp = GLCD_pp + GLCD_rx2
  GLCD_pp = GLCD_pp - GLCD_pp_temp

  IF filled_ellipse = 0 THEN
   Draw_Ellipse_Points
  ELSE
   Draw_Filled_Ellipse_Points
  END IF

  DO WHILE GLCD_px < GLCD_py

    GLCD_xx++

    GLCD_px = GLCD_px + GLCD_ey2

    IF GLCD_pp_E.7 = 0 THEN
     GLCD_yy--
     GLCD_py = GLCD_py - GLCD_ex2
     GLCD_pp = GLCD_pp - GLCD_py
    END IF

    'GLCD_pp = GLCD_pp + GLCD_fy2 + GLCD_px
    GLCD_pp = GLCD_pp + GLCD_fy2
    GLCD_pp = GLCD_pp + GLCD_px

    IF filled_ellipse = 0 THEN
     Draw_Ellipse_Points
    ELSE
     Draw_Filled_Ellipse_Points
    END IF

  LOOP

  'GLCD_pp = 2 + GLCD_ry2 * (2 * GLCD_xx + 1) * (2 * GLCD_xx + 1) + GLCD_fx2 * (GLCD_yy - 1) * (GLCD_yy - 1) -  GLCD_fx2 * GLCD_ry2

  'change GLCD_xx and GLCD_yy to do calculation
  GLCD_xx = GLCD_xx + GLCD_xx
  GLCD_xx++
  GLCD_yy--
  GLCD_pp_temp = GLCD_xx * GLCD_xx
  GLCD_pp_temp = GLCD_ry2 * GLCD_pp_temp
  GLCD_pp = 2 + GLCD_pp_temp
  GLCD_pp_temp = GLCD_yy * GLCD_yy
  GLCD_pp_temp = GLCD_fx2 * GLCD_pp_temp
  GLCD_pp = GLCD_pp + GLCD_pp_temp
  GLCD_pp_temp = GLCD_fx2 * GLCD_ry2
  GLCD_pp = GLCD_pp - GLCD_pp_temp

  'restore original GLCD_xx and GLCD_yy values
  GLCD_xx--
  'GLCD_xx/2
  set C off
  rotate GLCD_xx_H right
  rotate GLCD_xx right
  GLCD_yy++

  DO WHILE GLCD_yy > 0

    GLCD_yy--

    GLCD_py = GLCD_py - GLCD_ex2

    IF GLCD_pp_E.7 = 1 THEN
     GLCD_xx++
     GLCD_px = GLCD_px + GLCD_ey2
     GLCD_pp = GLCD_pp + GLCD_px
    END IF

    'GLCD_pp = GLCD_pp + GLCD_fx2 - GLCD_py
    GLCD_pp = GLCD_pp + GLCD_fx2
    GLCD_pp = GLCD_pp - GLCD_py

    IF filled_ellipse = 0 THEN
     Draw_Ellipse_Points
    ELSE
     Draw_Filled_Ellipse_Points
    END IF

  LOOP

End Sub

Sub DrawEllipseRoutine( in xoffset as word, in yoffset as word, in Inxradius as word,in Inyradius as word,Optional In LineColour as Long = GLCDForeground )

  'Version 1.00 (08/20/2017) by Joseph Realmuto
  'Version 1.01 (14/04/2019) by Giuseppe D'Elia

  'draws an ellipse outline at location (xoffset, yoffset) if filled_ellipse = 0
  'draws a filled ellipse at location (xoffset, yoffset) if filled_ellipse = 1
  'Inxradius is x radius of ellipse
  'Inyradius is y radius of ellipse

  'IF Inxradius = Inyradius THEN
  '  IF filled_ellipse = 0 THEN
  '    Circle Xoffset, Yoffset, Inxradius, LineColour
  '  ELSE
  '    FilledCircle Xoffset, Yoffset, Inxradius, LineColour
  '  END IF
  '    GLCD_exit sub
  'END IF

  IF Inxradius<2 THEN exit sub
  IF Inyradius<2 THEN exit sub

  dim GLCD_xx, GLCD_yy, GLCD_rx2, GLCD_ry2 as Word
  dim  GLCD_fx2, GLCD_fy2, GLCD_ex2, GLCD_ey2 as Long
  dim GLCD_px, GLCD_py, GLCD_pp, GLCD_pp_temp as Long

  GLCD_rx2 = Inxradius * Inxradius
  GLCD_ry2 = Inyradius * Inyradius

  'GLCD_fx2 = 4 * GLCD_rx2
  GLCD_fx2 = GLCD_rx2 + GLCD_rx2
  GLCD_fx2 = GLCD_fx2 + GLCD_fx2

  'GLCD_fy2 = 4 * GLCD_ry2
  GLCD_fy2 = GLCD_ry2 + GLCD_ry2
  GLCD_fy2 = GLCD_fy2 + GLCD_fy2

  'GLCD_ex2 = 2 * GLCD_fx2
  GLCD_ex2 = GLCD_fx2 + GLCD_fx2

  'GLCD_ey2 = 2 * GLCD_fy2
  GLCD_ey2 = GLCD_fy2 + GLCD_fy2

  GLCD_xx = 0
  GLCD_yy = Inyradius
  GLCD_px = 0
  GLCD_py = GLCD_ex2 * GLCD_yy

  'GLCD_pp = 2 + GLCD_fy2 -  GLCD_fx2 * Inyradius + GLCD_rx2
  GLCD_pp_temp = GLCD_fx2 * Inyradius
  GLCD_pp = 2 + GLCD_fy2
  GLCD_pp = GLCD_pp + GLCD_rx2
  GLCD_pp = GLCD_pp - GLCD_pp_temp

  IF filled_ellipse = 0 THEN
   Draw_Ellipse_Points
  ELSE
   Draw_Filled_Ellipse_Points
  END IF

  DO WHILE GLCD_px < GLCD_py

    GLCD_xx++

    GLCD_px = GLCD_px + GLCD_ey2

    IF GLCD_pp_E.7 = 0 THEN
     GLCD_yy--
     GLCD_py = GLCD_py - GLCD_ex2
     GLCD_pp = GLCD_pp - GLCD_py
    END IF

    'GLCD_pp = GLCD_pp + GLCD_fy2 + GLCD_px
    GLCD_pp = GLCD_pp + GLCD_fy2
    GLCD_pp = GLCD_pp + GLCD_px

    IF filled_ellipse = 0 THEN
     Draw_Ellipse_Points
    ELSE
     Draw_Filled_Ellipse_Points
    END IF

  LOOP

  'GLCD_pp = 2 + GLCD_ry2 * (2 * GLCD_xx + 1) * (2 * GLCD_xx + 1) + GLCD_fx2 * (GLCD_yy - 1) * (GLCD_yy - 1) -  GLCD_fx2 * GLCD_ry2

  'change GLCD_xx and GLCD_yy to do calculation
  GLCD_xx = GLCD_xx + GLCD_xx
  GLCD_xx++
  GLCD_yy--
  GLCD_pp_temp = GLCD_xx * GLCD_xx
  GLCD_pp_temp = GLCD_ry2 * GLCD_pp_temp
  GLCD_pp = 2 + GLCD_pp_temp
  GLCD_pp_temp = GLCD_yy * GLCD_yy
  GLCD_pp_temp = GLCD_fx2 * GLCD_pp_temp
  GLCD_pp = GLCD_pp + GLCD_pp_temp
  GLCD_pp_temp = GLCD_fx2 * GLCD_ry2
  GLCD_pp = GLCD_pp - GLCD_pp_temp

  'restore original GLCD_xx and GLCD_yy values
  GLCD_xx--
  'GLCD_xx/2
  set C off
  rotate GLCD_xx_H right
  rotate GLCD_xx right
  GLCD_yy++

  DO WHILE GLCD_yy > 0

    GLCD_yy--

    GLCD_py = GLCD_py - GLCD_ex2

    IF GLCD_pp_E.7 = 1 THEN
     GLCD_xx++
     GLCD_px = GLCD_px + GLCD_ey2
     GLCD_pp = GLCD_pp + GLCD_px
    END IF

    'GLCD_pp = GLCD_pp + GLCD_fx2 - GLCD_py
    GLCD_pp = GLCD_pp + GLCD_fx2
    GLCD_pp = GLCD_pp - GLCD_py

    IF filled_ellipse = 0 THEN
     Draw_Ellipse_Points
    ELSE
     Draw_Filled_Ellipse_Points
    END IF

  LOOP

End Sub


Sub Draw_Ellipse_Points
  dim LineColour as Long
  Pset ((xoffset + GLCD_xx), (yoffset + GLCD_yy), LineColour)
  Pset ((xoffset - GLCD_xx), (yoffset + GLCD_yy), LineColour)
  Pset ((xoffset - GLCD_xx), (yoffset - GLCD_yy), LineColour)
  Pset ((xoffset + GLCD_xx), (yoffset - GLCD_yy), LineColour)

End Sub

Sub Draw_Filled_Ellipse_Points
  dim LineColour as Long
  FOR GLCD_yy1 = (yoffset) to (yoffset + 2 * GLCD_yy)
   Pset ((xoffset + GLCD_xx), (GLCD_yy1 - GLCD_yy), LineColour)
   Pset ((xoffset - GLCD_xx), (GLCD_yy1 - GLCD_yy), LineColour)
  NEXT

End Sub

  '******************************************************************************************
  '** Function name:        Hyperbole   equation= x^2/a^2-y^2/b^2=1
  '**
  '** Description:          Draw an Hyperbole outline using major and minor axis.
  '**
  '** Input paramters:
  '**       (x_0, y_0) = coordinates (x, y) of hyperbole center
  '**       a_axis, b_axis = a, b
  '**       Type  (Type=1 Hyperbole is aligned along x axis) (Type=2 Hyperbole is aligned along y axis)
  '**   ModeStop (ModeStop=1 drawing stops when  one reacheable side of the display border is encountered.
  '**       ModeStop=2 drawing stops when all the reacheable sides of the display border are encountered)
  '**   LineColour Color of the Hyperbole drawing
  '**
  '******************************************************************************************
sub Hyperbole(x_0, y_0, a_axis, b_axis, type, ModeStop, optional LineColour=GLCDForeground)

    'if type=1 is alined along x
    'if type=2 is alined along y
    'if ModeStop=1 drawing stops when one of the borders has been encountered
    'if ModeStop=2 drawing stops when all (possible) borders has been encountered
    'ModeStop is not relevant for hyperbola centered on the display

    Dim a_axis, b_axis, a_sq, b_sq, half_a_sq, half_b_sq as word  'axis smaller than 254
    Dim dist as Long
    Dim a_sq2, b_sq2, a_sq4, b_sq4, half_ab_sq as Long
    Dim x_slope, y_slope as Long
    Dim dist_sign as byte
    Dim x_offset, y_offset, x_0, y_0  as word
    Dim x_coord, y_coord, x_max, y_max as Word
    Dim x_plus, x_minus, y_plus, y_minus as Word
    Dim type, ModeStop as byte
    Dim LineColour as word
    Dim HyperboleCondition, intersect as byte

    if Type=1 then
      x_offset=x_0
      y_offset=y_0
      x_max=GLCD_WIDTH
      y_max=GLCD_HEIGHT
    else
      y_offset=x_0
      x_offset=y_0
      y_max=GLCD_WIDTH
      x_max=GLCD_HEIGHT
    end if

    x_coord=a_axis
    y_coord=0
    a_sq=a_axis*a_axis
    b_sq=b_axis*b_axis
    a_sq2=a_sq+a_sq
    b_sq2=b_sq+b_sq
    a_sq4=a_sq2+a_sq2
    b_sq4=b_sq2+b_sq2
    x_slope=a_sq4*(x_coord+1)
    y_slope=b_sq4             '*(y_coord+1)
    half_a_sq=FNLSR(a_sq,1)
    half_b_sq=FNLSR(b_sq,1)
    half_ab_sq=half_a_sq+half_b_sq
    dist=a_sq2+half_b_sq
    dist_sign=0
    HyperboleParabolaDiffvar(dist, b_sq*(1+2*a_axis), dist_sign)

    intersect=0
    x_plus=x_offset+x_coord
    x_minus=x_offset-x_coord
    y_plus=y_offset+y_coord
    y_minus=y_offset-y_coord

    do while (dist_sign=1 or (dist_sign=0 and dist<=x_slope))
      if HyperboleCondition(ModeStop)=0 then
        exit do
      end if

      HyperboleDisplayPixel(LineColour)

      if dist_sign=0 then
        HyperboleParabolaDiffvar (dist, x_slope, dist_sign)
        x_coord=x_coord+1
        x_slope=x_slope+b_sq4
      end if
      HyperboleParabolaSumvar(dist, a_sq2+y_slope, dist_sign)
      y_coord=y_coord+1
      y_slope=y_slope+a_sq4

      x_plus=x_offset+x_coord
      x_minus=x_offset-x_coord
      y_plus=y_offset+y_coord
      y_minus=y_offset-y_coord
    Loop

    HyperboleParabolaSumvar(dist,a_sq+b_sq, dist_sign)
    HyperboleParabolaDiffvar(dist,FNLSR(x_slope+y_slope,1)+half_ab_sq, dist_sign)
    intersect=0

    if a_axis>b_axis then
      do
        if HyperboleCondition(ModeStop)=0 then
          exit do
        end if

      HyperboleDisplayPixel(LineColour)


      if dist_sign=1 then
          HyperboleParabolaSumvar(dist,y_slope,dist_sign)
          y_coord=y_coord+1
          y_slope=y_slope+a_sq4
        end if
        HyperboleParabolaDiffvar(dist,b_sq2+x_slope,dist_sign)
        x_coord=x_coord+1
        x_slope=x_slope+b_sq4

        x_plus=x_offset+x_coord
        x_minus=x_offset-x_coord
        y_plus=y_offset+y_coord
        y_minus=y_offset-y_coord

      loop
    end if

end sub

function HyperboleCondition(ModeStop)

  Dim HyperboleCondition, ModeStop as Byte
  Dim x_coord, y_coord, x_max, y_max, x_offset, y_offset, x_plus, y_plus as Word
  Dim intersect as Byte

  if ModeStop=1 then
  '    if (y_plus<y_max and x_plus<x_max) and (x_coord<=x_offset and y_coord<=y_offset) then
    if intersect=0 then
     HyperboleCondition=1
    else
      HyperboleCondition=0
    end if
  else
  '    if (y_plus<y_max or x_plus<x_max) or (x_coord<=x_offset or y_coord<=y_offset) then
    if intersect=0b1111 then
      HyperboleCondition=0
    else
      HyperboleCondition=1
    end if
  end if
end function

sub HyperboleDisplayPixel(LineColour)

  Dim x_max, y_max, x_plus, x_minus, y_plus, y_minus as Word
  Dim LineColour as word
  Dim Type, intersect as Byte

  if Type = 1 then
    if x_plus<=x_max  then
      if y_plus<=y_max then
        Pset(x_plus, y_plus, LineColour)
      else
        intersect.0=1
      end if
      if y_minus.15=0 then
        Pset(x_plus, y_minus, LineColour)
      else
        intersect.1=1
      end if
    else
      intersect.0=1
      intersect.1=1
    end if
    if x_minus.15=0 then
      if y_plus<=y_max then
        Pset(x_minus,y_plus, LineColour)
      else
        intersect.2=1
      end if
      if y_minus.15=0 then
        Pset(x_minus, y_minus, LineColour)
      else
        intersect.3=1
      end if
    else
      intersect.2=1
      intersect.3=1
    end if
  else '---------------------------------------------------
    if x_plus<=x_max then
      if y_plus<=y_max then
        Pset(y_plus, x_plus, LineColour)
      else
        intersect.0=1
      end if
      if y_minus.15=0 then
        Pset(y_minus, x_plus, LineColour)
      else
        intersect.1=1
      end if
    else
      intersect.0=1
      intersect.1=1
    end if
    if x_minus.15=0 then
      if y_plus<=y_max then
        Pset(y_plus, x_minus, LineColour)
      else
        intersect.2=1
      end if
      if y_minus.15=0 then
        Pset(y_minus, x_minus, LineColour)
      else
        intersect.3=1
      end if
    else
      intersect.2=1
      intersect.3=1
    end if
  end if

end sub

sub HyperboleParabolaDiffvar(var1, var2, sign)

      'var2 MUST be >=0

  Dim var1, var2 as Long
  Dim sign as Byte

  if sign=0 then
    if var1 > var2 then
      var1=var1-var2
    else
      var1=var2-var1
      sign=1
    end if
  else
    var1=var1+var2
  end if

end sub

sub HyperboleParabolaSumvar(var1, var2, sign)

      'var2 MUST be >=0

  Dim var1, var2 as Long
  Dim sign as Byte

  if sign=0 then
    var1=var1+var2
  else
    if var1 < var2 then
      var1=var2-var1
      sign=0
    else
      var1=var1-var2
      sign=1
    end if
  end if

end sub

  '***************************************************************************************

sub Parabola(x_0, y_0, p_factor, type, ModeStop, optional LineColour=GLCDForeground)

    'if type=1 is alined along x
    'if type=2 is alined along y
    'if ModeStop=1 drawing stops when one of the borders has been encountered
    'if ModeStop=2 drawing stops when all (possible) borders has been encountered
    'ModeStop is not relevant for hyperbola centered on the display

    Dim p_factor, p2_factor, p4_factor as word  'axis smaller than 254
    Dim dist as Long
    Dim dist_sign as byte
    Dim x_offset, y_offset, x_0, y_0  as word
    Dim x_coord, y_coord, x_max, y_max as Word
    Dim x_plus, x_minus, y_plus, y_minus as Word
    Dim type, ModeStop as byte
    Dim LineColour as word
    Dim ParabolaCondition, intersect as byte

    if Type=1 then
      x_offset=x_0
      y_offset=y_0
      x_max=GLCD_WIDTH
      y_max=GLCD_HEIGHT
    else
      y_offset=x_0
      x_offset=y_0
      y_max=GLCD_WIDTH
      x_max=GLCD_HEIGHT
    end if

    x_coord=0
    y_coord=0
    p2_factor=2*p_factor
    p4_factor=4*p_factor
    dist_sign=0
    dist=1
    HyperboleParabolaDiffvar(dist,p_factor,dist_sign)

    intersect=0
    x_plus=x_offset+x_coord
    x_minus=x_offset-x_coord
    y_plus=y_offset+y_coord
    y_minus=y_offset-y_coord

    do while y_coord<P_factor
      if ParabolaCondition(ModeStop)=0 then
        exit do
      end if

      ParabolaDisplayPixel(LineColour)

      if dist_sign=0 then
        HyperboleParabolaDiffvar (dist, p2_factor, dist_sign)
        x_coord=x_coord+1
      end if
      y_coord=y_coord+1
      HyperboleParabolaSumvar(dist, 2*y_coord+1, dist_sign)

      x_plus=x_offset+x_coord
      x_minus=x_offset-x_coord
      y_plus=y_offset+y_coord
      y_minus=y_offset-y_coord
    Loop

    if dist=1 then
      HyperboleParabolaDiffvar(dist,p4_factor,dist_sign)
    else
      HyperboleParabolaDiffvar(dist,p2_factor,dist_sign)
    end if
    intersect=0

      do while ParabolaCondition(ModeStop)=1

      ParabolaDisplayPixel(LineColour)


      if dist_sign=1 then
          HyperboleParabolaSumvar(dist,4*y_coord,dist_sign)
          y_coord=y_coord+1
        end if
        x_coord=x_coord+1
        HyperboleparabolaDiffvar(dist,p4_factor,dist_sign)

        x_plus=x_offset+x_coord
        x_minus=x_offset-x_coord
        y_plus=y_offset+y_coord
        y_minus=y_offset-y_coord

      loop

end sub

function ParabolaCondition(ModeStop)

  Dim ParabolaCondition, ModeStop as Byte
  Dim x_coord, y_coord, x_max, y_max, x_offset, y_offset, x_plus, y_plus as Word
  Dim intersect as Byte

  if ModeStop=1 then
    if intersect=0 then
      ParabolaCondition=1
    else
      ParabolaCondition=0
    end if
  else
    if intersect=0b0011 then
      ParabolaCondition=0
    else
      ParabolaCondition=1
    end if
  end if
end function

sub ParabolaDisplayPixel(LineColour)

  Dim x_max, y_max, x_plus, x_minus, y_plus, y_minus as Word
  Dim LineColour as word
  Dim Type, intersect as Byte

  if Type = 1 then
    if x_plus<=x_max  then
      if y_plus<=y_max then
        Pset(x_plus, y_plus, LineColour)
      else
        intersect.0=1
      end if
      if y_minus.15=0 then
        Pset(x_plus, y_minus, LineColour)
      else
        intersect.1=1
      end if
    else
      intersect.0=1
      intersect.1=1
    end if
  else '---------------------------------------------------
    if x_plus<=x_max then
      if y_plus<=y_max then
        Pset(y_plus, x_plus, LineColour)
      else
        intersect.0=1
      end if
      if y_minus.15=0 then
        Pset(y_minus, x_plus, LineColour)
      else
        intersect.1=1
      end if
    else
      intersect.0=1
      intersect.1=1
    end if
  end if

end sub


  '***************************************************************************************
  '** Function name:           Triangle
  '** Description:             Draw a triangle outline using 3 arbitrary points
  '***************************************************************************************
  ' Draw a triangle
Sub Triangle(  in xoffset as word, in yoffset as word, in xoffset2 as word, in yoffset2 as word,in xoffset3 as word, in yoffset3 as word ,Optional In LineColour as word = GLCDForeground )
  Line(xoffset, yoffset,   xoffset2, yoffset2, LineColour)
  Line(xoffset2, yoffset2, xoffset3, yoffset3, LineColour)
  Line(xoffset3, yoffset3, xoffset,  yoffset,  LineColour)
End Sub

Sub Triangle(  in xoffset as word, in yoffset as word, in xoffset2 as word, in yoffset2 as word,in xoffset3 as word, in yoffset3 as word ,Optional In LineColour as Long = GLCDForeground )
  Line(xoffset, yoffset,   xoffset2, yoffset2, LineColour)
  Line(xoffset2, yoffset2, xoffset3, yoffset3, LineColour)
  Line(xoffset3, yoffset3, xoffset,  yoffset,  LineColour)
End Sub

  '***************************************************************************************
  '** Function name:           FilledTriangle
  '** Description:             Draw a filled triangle using 3 arbitrary points
  '***************************************************************************************
  ' Fill a triangle
Sub FilledTriangle (  in xoffset as word, in yoffset as word, in xoffset2 as word, in yoffset2 as word,in xoffset3 as word, in yoffset3 as word ,Optional In LineColour as word = GLCDForeground )

  dim  GLCD_aa, GLCD_bb, GLCD_last as word

  ' Sort coordinates by Y order (yoffset3 >= yoffset2 >= yoffset)
  if (yoffset > yoffset2) then
    swap(yoffset, yoffset2)
    swap(xoffset, xoffset2)
  end if
  if (yoffset2 > yoffset3) then
    swap(yoffset3, yoffset2)
    swap(xoffset3, xoffset2)
  end if
  if (yoffset > yoffset2) then
    swap(yoffset, yoffset2)
    swap(xoffset, xoffset2)
  end if


  ' Handle awkward all-on-same-line case as its own thing
  if (yoffset = yoffset3) then

          GLCD_bb = xoffset
          GLCD_aa = xoffset

          if (xoffset2 < GLCD_aa ) Then
            GLCD_aa = xoffset2
          else if (xoffset2 > GLCD_bb ) then
            GLCD_bb = xoffset2
          end if

          if (xoffset3 < GLCD_aa) Then
            GLCD_aa = xoffset3
          else if ( xoffset3 > GLCD_bb) then
            GLCD_bb = xoffset3
          end if
          Line( GLCD_aa, yoffset, GLCD_bb - GLCD_aa + 1, yoffset, LineColour)
          exit sub
  end if

  dim dxoffset01, dyoffset01, dxoffset02, dyoffset02, dxoffset12, dyoffset12, glcd_y as word
  dim glcd_sa, glcd_sb as Integer
  dxoffset01 = xoffset2 - xoffset
  dyoffset01 = yoffset2 - yoffset
  dxoffset02 = xoffset3 - xoffset
  dyoffset02 = yoffset3 - yoffset
  dxoffset12 = xoffset3 - xoffset2
  dyoffset12 = yoffset3 - yoffset2
  glcd_sa   = 0
  glcd_sb   = 0

  '
  '   For upper part of triangle, find scanline crossings for segments
  '   0-1 and 0-2.  If yoffset2=yoffset3 (flat-bottomed triangle), the scanline yoffset2
  '   is included here (and second loop will be skipped, avoiding a /0
  '   error there), otherwise scanline yoffset2 is skipped here and handled
  '   in the second loop...which also avoids a /0 error here if yoffset=yoffset2
  '   (flat-topped triangle).

  if ( yoffset2 = yoffset3) then
    GLCD_last = yoffset2 ;   Include yoffset2 scanline
  else
    GLCD_last = yoffset2 - 1;  Skip it
  end if

   for glcd_y = yoffset to glcd_last

    glcd_aa   = xoffset +  glcd_sa / dyoffset01
    glcd_bb   = xoffset +  glcd_sb / dyoffset02
    glcd_sa = glcd_sa + dxoffset01
    glcd_sb = glcd_sb + dxoffset02

    if (glcd_aa > glcd_bb)  then
      swap(glcd_aa, glcd_bb)
    end if

    Line(glcd_aa, glcd_y, glcd_bb , glcd_y, LineColour)
   next

  '   For lower part of triangle, find scanline crossings for segments
  '   0-2 and 1-2.  This loop is skipped if yoffset2=yoffset3.
   glcd_sa = dxoffset12 * (glcd_y - yoffset2);
   glcd_sb = dxoffset02 * (glcd_y - yoffset);
   for glcd_y = glcd_last to yoffset3
    glcd_aa   = xoffset2 +  glcd_sa / dyoffset12
    glcd_bb   = xoffset +  glcd_sb / dyoffset02
    glcd_sa = glcd_sa + dxoffset12
    glcd_sb = glcd_sb + dxoffset02
      if (glcd_aa > glcd_bb)  then
        swap(glcd_aa, glcd_bb)
      end if
     Line(glcd_aa, glcd_y, glcd_bb , glcd_y, LineColour)
    next
End Sub

Sub FilledTriangle (  in xoffset as word, in yoffset as word, in xoffset2 as word, in yoffset2 as word,in xoffset3 as word, in yoffset3 as word ,Optional In LineColour as Long = GLCDForeground )

  dim  GLCD_aa, GLCD_bb, GLCD_last as word

  ' Sort coordinates by Y order (yoffset3 >= yoffset2 >= yoffset)
  if (yoffset > yoffset2) then
    swap(yoffset, yoffset2)
    swap(xoffset, xoffset2)
  end if
  if (yoffset2 > yoffset3) then
    swap(yoffset3, yoffset2)
    swap(xoffset3, xoffset2)
  end if
  if (yoffset > yoffset2) then
    swap(yoffset, yoffset2)
    swap(xoffset, xoffset2)
  end if


  ' Handle awkward all-on-same-line case as its own thing
  if (yoffset = yoffset3) then

          GLCD_bb = xoffset
          GLCD_aa = xoffset

          if (xoffset2 < GLCD_aa ) Then
            GLCD_aa = xoffset2
          else if (xoffset2 > GLCD_bb ) then
            GLCD_bb = xoffset2
          end if

          if (xoffset3 < GLCD_aa) Then
            GLCD_aa = xoffset3
          else if ( xoffset3 > GLCD_bb) then
            GLCD_bb = xoffset3
          end if
          Line( GLCD_aa, yoffset, GLCD_bb - GLCD_aa + 1, yoffset, LineColour)
          exit sub
  end if

  dim dxoffset01, dyoffset01, dxoffset02, dyoffset02, dxoffset12, dyoffset12, glcd_y as word
  dim glcd_sa, glcd_sb as Integer
  dxoffset01 = xoffset2 - xoffset
  dyoffset01 = yoffset2 - yoffset
  dxoffset02 = xoffset3 - xoffset
  dyoffset02 = yoffset3 - yoffset
  dxoffset12 = xoffset3 - xoffset2
  dyoffset12 = yoffset3 - yoffset2
  glcd_sa   = 0
  glcd_sb   = 0

  '
  '   For upper part of triangle, find scanline crossings for segments
  '   0-1 and 0-2.  If yoffset2=yoffset3 (flat-bottomed triangle), the scanline yoffset2
  '   is included here (and second loop will be skipped, avoiding a /0
  '   error there), otherwise scanline yoffset2 is skipped here and handled
  '   in the second loop...which also avoids a /0 error here if yoffset=yoffset2
  '   (flat-topped triangle).

  if ( yoffset2 = yoffset3) then
    GLCD_last = yoffset2 ;   Include yoffset2 scanline
  else
    GLCD_last = yoffset2 - 1;  Skip it
  end if

   for glcd_y = yoffset to glcd_last

    glcd_aa   = xoffset +  glcd_sa / dyoffset01
    glcd_bb   = xoffset +  glcd_sb / dyoffset02
    glcd_sa = glcd_sa + dxoffset01
    glcd_sb = glcd_sb + dxoffset02

    if (glcd_aa > glcd_bb)  then
      swap(glcd_aa, glcd_bb)
    end if

    Line(glcd_aa, glcd_y, glcd_bb , glcd_y, LineColour)
   next

  '   For lower part of triangle, find scanline crossings for segments
  '   0-2 and 1-2.  This loop is skipped if yoffset2=yoffset3.
   glcd_sa = dxoffset12 * (glcd_y - yoffset2);
   glcd_sb = dxoffset02 * (glcd_y - yoffset);
   for glcd_y = glcd_last to yoffset3
    glcd_aa   = xoffset2 +  glcd_sa / dyoffset12
    glcd_bb   = xoffset +  glcd_sb / dyoffset02
    glcd_sa = glcd_sa + dxoffset12
    glcd_sb = glcd_sb + dxoffset02
      if (glcd_aa > glcd_bb)  then
        swap(glcd_aa, glcd_bb)
      end if
     Line(glcd_aa, glcd_y, glcd_bb , glcd_y, LineColour)
    next
End Sub

  '''Display BMP on the screen
Sub DrawBMP ( in TFTXPos as Word, in TFTYPos as Word, in SelectedTable as word)
    Dim TableReadPosition, TableLen, SelectedTable as word
    ' Start of code
    Dim  PixelRGB , XCount, YCount, TFTYEnd, objwidth, objHeight, TableReadPosition, TFTX , TFTY as Word
    TableReadPosition = 1
    'Read selected table
    Select Case SelectedTable
        Case @TableImage1: ReadTable TableImage1, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TableImage1, TableReadPosition, objHeight
        #IFDEF TableImage2
        Case @TableImage2: ReadTable TableImage2, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TableImage2, TableReadPosition, objHeight
        #ENDIF
        #IFDEF TableImage3
        Case @TableImage3: ReadTable TableImage3, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TableImage3, TableReadPosition, objHeight
        #ENDIF
        #IFDEF TableImage4             
        Case @TableImage4: ReadTable TableImage4, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TableImage4, TableReadPosition, objHeight
        #ENDIF
        #IFDEF TableImage5             
        Case @TableImage5: ReadTable TableImage5, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TableImage5, TableReadPosition, objHeight
        #ENDIF            
    End Select
    TableReadPosition = 3
    For YCount = 0 to (objHeight - 1)
                For XCount = 0 to (objwidth - 1)
                    'Read selected table
                    Select Case SelectedTable
                    #IFDEF TableImage1
                    Case @TableImage1: ReadTable TableImage1, TableReadPosition, PixelRGB
                    #ENDIF
                    #IFDEF TableImage2
                    Case @TableImage2: ReadTable TableImage2, TableReadPosition, PixelRGB
                    #ENDIF
                    #IFDEF TableImage3
                    Case @TableImage3: ReadTable TableImage3, TableReadPosition, PixelRGB
                    #ENDIF
                    #IFDEF TableImage4
                    Case @TableImage4: ReadTable TableImage4, TableReadPosition, PixelRGB
                    #ENDIF
                    #IFDEF TableImage5
                    Case @TableImage5: ReadTable TableImage5, TableReadPosition, PixelRGB
                    #ENDIF
                    End Select
                    TableReadPosition++
                    TFTX=[Word]TFTXPos+XCount
                    TFTY=[Word]TFTYPos+YCount
                    PSet TFTX, TFTY, PixelRGB
                Next
    Next
End Sub





Sub GLCDPrintLargeFont (In PrintLocX as Word , In PrintLocY as Word,  PrintData As String, Optional In  Color as word = GLCDForeground)
  'Ported from SSD1289 library to break the dependency on the SSD1289 library
  Dim GLCDPrintLoc as Word
  PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = 1 To PrintLen
    DrawBigChar GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  Next
End Sub

Sub GLCDPrintLargeFont (In PrintLocX as Word , In PrintLocY as Word,  PrintData As String, Optional In  Color as Long = GLCDForeground)
  'Ported from SSD1289 library to break the dependency on the SSD1289 library
  Dim GLCDPrintLoc as Word
  PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = 1 To PrintLen
    DrawBigChar GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  Next
End Sub

  '''Displays a character in a larger font
Sub DrawBigChar (In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  Color as word = GLCDForeground )
    Dim LocX , LocY as Word
    if CharCode <=126 Then
       CharCode -=32
       Goto TablesW
    end if
    if CharCode <=210 Then
       CharCode -=33
       Goto TablesW
    end if
    if CharCode <= 250 Then
       CharCode -=34
       Goto TablesW
    end if
    TablesW:
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
          LocY=[word]CharLocY+CurrCharRow
          if CurrCharCol>12 then
             LocX= LocX - 12
             LocY= LocY + 8
          end if
          if CurrCharVal.0=1 then
             PSet LocX , LocY , Color
          else
             PSet LocX , LocY , GLCDBackground
          end if
          Rotate CurrCharVal Right
        Next
    Next
End Sub

Sub DrawBigChar (In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  Color as Long = GLCDForeground )
    Dim LocX , LocY as Word
    if CharCode <=126 Then
       CharCode -=32
       Goto TablesL
    end if
    if CharCode <=210 Then
       CharCode -=33
       Goto TablesL
    end if
    if CharCode <= 250 Then
       CharCode -=34
       Goto TablesL
    end if
    TablesL:
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
          LocY=[word]CharLocY+CurrCharRow
          if CurrCharCol>12 then
             LocX= LocX - 12
             LocY= LocY + 8
          end if
          if CurrCharVal.0=1 then
             PSet LocX , LocY , Color
          else
             PSet LocX , LocY , GLCDBackground
          end if
          Rotate CurrCharVal Right
        Next
    Next
End Sub

Table OLEDFont2 as byte
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x20
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFE
  0x1B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x21
  0x00
  0x00
  0x00
  0x00
  0x1E
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x1E
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x22
  0x00
  0x01
  0x20
  0x19
  0xA0
  0x07
  0x78
  0x01
  0x26
  0x19
  0xA0
  0x07
  0x78
  0x01
  0x26
  0x01
  0x20
  0x01
  0x00
  0x00  ' 0x23
  0x00
  0x00
  0x00
  0x00
  0x1C
  0x18
  0x26
  0x10
  0x42
  0x10
  0xFF
  0x3F
  0x82
  0x11
  0x02
  0x0F
  0x00
  0x00
  0x00
  0x00  ' 0x24
  0x1C
  0x10
  0x22
  0x08
  0x22
  0x04
  0x22
  0x03
  0x9C
  0x00
  0x40
  0x0E
  0x30
  0x11
  0x08
  0x11
  0x04
  0x11
  0x02
  0x0E  ' 0x25
  0x00
  0x07
  0x80
  0x08
  0x5C
  0x10
  0x62
  0x10
  0xA2
  0x11
  0x32
  0x13
  0x1C
  0x1C
  0x00
  0x18
  0x00
  0x16
  0x80
  0x01  ' 0x26
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x1E
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x27
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xE0
  0x07
  0x18
  0x18
  0x0C
  0x30
  0x04
  0x20
  0x02
  0x40
  0x02
  0x40
  0x00
  0x00  ' 0x28
  0x00
  0x00
  0x02
  0x40
  0x02
  0x40
  0x04
  0x20
  0x0C
  0x30
  0x18
  0x18
  0xE0
  0x07
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x29
  0x00
  0x00
  0x08
  0x00
  0x18
  0x00
  0xF0
  0x00
  0x4E
  0x00
  0xF0
  0x00
  0x18
  0x00
  0x08
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x2A
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0xE0
  0x1F
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0x00  ' 0x2B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x98
  0x00
  0x78
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x2C
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00  ' 0x2D
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x18
  0x00
  0x18
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x2E
  0x00
  0x00
  0x00
  0x40
  0x00
  0x30
  0x00
  0x0C
  0x00
  0x03
  0xC0
  0x00
  0x30
  0x00
  0x0C
  0x00
  0x02
  0x00
  0x00
  0x00  ' 0x2F
  0x00
  0x00
  0xF0
  0x03
  0x0C
  0x0C
  0x02
  0x10
  0x02
  0x10
  0x02
  0x10
  0x0C
  0x0C
  0xF0
  0x03
  0x00
  0x00
  0x00
  0x00  ' 0x30
  0x00
  0x00
  0x04
  0x10
  0x04
  0x10
  0x04
  0x10
  0xFE
  0x1F
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x31
  0x00
  0x00
  0x06
  0x18
  0x02
  0x14
  0x02
  0x12
  0x02
  0x11
  0xC2
  0x10
  0x3C
  0x10
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x32
  0x00
  0x00
  0x00
  0x00
  0x02
  0x10
  0x42
  0x10
  0x42
  0x10
  0x42
  0x10
  0xBC
  0x0F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x33
  0x00
  0x03
  0xC0
  0x02
  0x20
  0x02
  0x18
  0x02
  0x04
  0x02
  0xFE
  0x1F
  0x00
  0x02
  0x00
  0x02
  0x00
  0x00
  0x00
  0x00  ' 0x34
  0x00
  0x00
  0x00
  0x00
  0x3E
  0x10
  0x22
  0x10
  0x22
  0x10
  0x42
  0x08
  0x82
  0x07
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x35
  0x00
  0x00
  0xF0
  0x07
  0x4C
  0x08
  0x22
  0x10
  0x22
  0x10
  0x22
  0x10
  0x42
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x36
  0x00
  0x00
  0x02
  0x00
  0x02
  0x18
  0x02
  0x07
  0xC2
  0x00
  0x32
  0x00
  0x0A
  0x00
  0x06
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x37
  0x00
  0x00
  0x1C
  0x0F
  0xA2
  0x10
  0x42
  0x10
  0x42
  0x10
  0xA2
  0x10
  0xA2
  0x09
  0x1C
  0x06
  0x00
  0x00
  0x00
  0x00  ' 0x38
  0x00
  0x00
  0x78
  0x00
  0x84
  0x10
  0x02
  0x11
  0x02
  0x11
  0x02
  0x11
  0x84
  0x0C
  0xF8
  0x03
  0x00
  0x00
  0x00
  0x00  ' 0x39
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x60
  0x18
  0x60
  0x18
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x3A
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x60
  0x98
  0x60
  0x78
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x3B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x03
  0x00
  0x03
  0x80
  0x04
  0x80
  0x04
  0x40
  0x08
  0x40
  0x08
  0x20
  0x10
  0x00
  0x00  ' 0x3C
  0x00
  0x00
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x00
  0x00  ' 0x3D
  0x00
  0x00
  0x20
  0x10
  0x40
  0x08
  0x40
  0x08
  0x80
  0x04
  0x80
  0x04
  0x00
  0x03
  0x00
  0x03
  0x00
  0x00
  0x00
  0x00  ' 0x3E
  0x00
  0x00
  0x0E
  0x00
  0x02
  0x00
  0x02
  0x1B
  0x82
  0x00
  0x42
  0x00
  0x26
  0x00
  0x1C
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x3F
  0xF0
  0x03
  0x18
  0x0C
  0x04
  0x18
  0xE2
  0x13
  0x12
  0x14
  0x0A
  0x16
  0x8A
  0x1B
  0xFC
  0x07
  0x00
  0x04
  0x00
  0x04  ' 0x40
  0x00
  0x10
  0x00
  0x0E
  0x80
  0x03
  0x70
  0x02
  0x18
  0x02
  0x30
  0x02
  0xC0
  0x02
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x10  ' 0x41
  0x00
  0x00
  0xF8
  0x1F
  0x88
  0x10
  0x88
  0x10
  0x88
  0x10
  0x88
  0x10
  0x48
  0x11
  0x30
  0x0E
  0x00
  0x00
  0x00
  0x00  ' 0x42
  0xC0
  0x03
  0x30
  0x0C
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x18
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x43
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x44
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x11
  0x08
  0x11
  0x08
  0x11
  0x08
  0x11
  0x08
  0x11
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x45
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x46
  0x00
  0x00
  0xC0
  0x03
  0x30
  0x0C
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x11
  0x08
  0x11
  0x18
  0x1F
  0x00
  0x00  ' 0x47
  0x00
  0x00
  0xF8
  0x1F
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0xF8
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x48
  0x00
  0x00
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0xF8
  0x1F
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x49
  0x00
  0x00
  0x00
  0x00
  0x00
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0xF8
  0x0F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x4A
  0x00
  0x00
  0xF8
  0x1F
  0x80
  0x00
  0x80
  0x01
  0x40
  0x02
  0x20
  0x04
  0x10
  0x04
  0x08
  0x08
  0x00
  0x10
  0x00
  0x00  ' 0x4B
  0x00
  0x00
  0xF8
  0x1F
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x4C
  0xF8
  0x1F
  0x38
  0x00
  0xE0
  0x01
  0x00
  0x07
  0x00
  0x06
  0xC0
  0x01
  0x38
  0x00
  0xF8
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x4D
  0x00
  0x00
  0xF8
  0x1F
  0x10
  0x00
  0x60
  0x00
  0x80
  0x01
  0x00
  0x06
  0x00
  0x08
  0xF8
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x4E
  0x00
  0x00
  0xE0
  0x07
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00  ' 0x4F
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x88
  0x00
  0x70
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x50
  0x00
  0x00
  0xE0
  0x07
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x30
  0x10
  0x48
  0xE0
  0x47
  0x00
  0x00  ' 0x51
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x01
  0x08
  0x01
  0x08
  0x03
  0x88
  0x04
  0x70
  0x08
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x52
  0x00
  0x00
  0x70
  0x18
  0x48
  0x10
  0x88
  0x10
  0x88
  0x10
  0x08
  0x11
  0x08
  0x09
  0x18
  0x0E
  0x00
  0x00
  0x00
  0x00  ' 0x53
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0xF8
  0x1F
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0x00
  0x00  ' 0x54
  0x00
  0x00
  0xF8
  0x07
  0x00
  0x18
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x08
  0xF8
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x55
  0x08
  0x00
  0x30
  0x00
  0xC0
  0x01
  0x00
  0x06
  0x00
  0x18
  0x00
  0x18
  0x00
  0x07
  0xC0
  0x00
  0x30
  0x00
  0x08
  0x00  ' 0x56
  0x18
  0x00
  0xE0
  0x03
  0x00
  0x1C
  0x00
  0x0F
  0xE0
  0x00
  0xC0
  0x01
  0x00
  0x0E
  0x00
  0x1C
  0xE0
  0x03
  0x18
  0x00  ' 0x57
  0x08
  0x10
  0x10
  0x08
  0x20
  0x04
  0x40
  0x02
  0x80
  0x01
  0x80
  0x01
  0x40
  0x02
  0x20
  0x04
  0x10
  0x08
  0x08
  0x10  ' 0x58
  0x08
  0x00
  0x10
  0x00
  0x60
  0x00
  0x80
  0x00
  0x00
  0x1F
  0x80
  0x00
  0x40
  0x00
  0x20
  0x00
  0x10
  0x00
  0x08
  0x00  ' 0x59
  0x00
  0x00
  0x08
  0x18
  0x08
  0x14
  0x08
  0x12
  0x08
  0x11
  0x88
  0x10
  0x48
  0x10
  0x28
  0x10
  0x18
  0x10
  0x00
  0x00  ' 0x5A
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFE
  0x7F
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x00
  0x00
  0x00
  0x00  ' 0x5B
  0x00
  0x00
  0x02
  0x00
  0x0C
  0x00
  0x30
  0x00
  0xC0
  0x00
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x30
  0x00
  0x40
  0x00
  0x00  ' 0x5C
  0x00
  0x00
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0xFE
  0x7F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x5D
  0x00
  0x00
  0x00
  0x04
  0x00
  0x03
  0xE0
  0x00
  0x38
  0x00
  0x0E
  0x00
  0x70
  0x00
  0x80
  0x03
  0x00
  0x04
  0x00
  0x00  ' 0x5E
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20  ' 0x5F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x02
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x60
  0x00
  0x00
  0x00
  0x0C
  0x20
  0x12
  0x20
  0x11
  0x20
  0x11
  0x20
  0x09
  0xC0
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x61
  0x00
  0x00
  0xFE
  0x1F
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x60
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x62
  0x00
  0x00
  0x80
  0x07
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x63
  0x00
  0x00
  0x80
  0x07
  0x40
  0x18
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x40
  0x08
  0xFE
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x64
  0x00
  0x00
  0x80
  0x07
  0x40
  0x09
  0x20
  0x11
  0x20
  0x11
  0x20
  0x11
  0x20
  0x11
  0xC0
  0x11
  0x00
  0x00
  0x00
  0x00  ' 0x65
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0xFC
  0x1F
  0x24
  0x00
  0x22
  0x00
  0x22
  0x00
  0x22
  0x00
  0x22
  0x00
  0x00
  0x00  ' 0x66
  0x00
  0x00
  0x80
  0x07
  0x40
  0x98
  0x20
  0x90
  0x20
  0x90
  0x20
  0x90
  0x40
  0x48
  0xE0
  0x3F
  0x00
  0x00
  0x00
  0x00  ' 0x67
  0x00
  0x00
  0xFE
  0x1F
  0x80
  0x00
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x68
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0x26
  0x00
  0xE6
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x69
  0x00
  0x00
  0x00
  0x80
  0x20
  0x80
  0x20
  0x80
  0x26
  0x80
  0xE6
  0x7F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x6A
  0x00
  0x00
  0xFE
  0x1F
  0x00
  0x01
  0x00
  0x03
  0x80
  0x04
  0x40
  0x04
  0x40
  0x08
  0x20
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x6B
  0x00
  0x00
  0x00
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0xFE
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x6C
  0xE0
  0x1F
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x00
  0x00  ' 0x6D
  0x00
  0x00
  0xE0
  0x1F
  0xC0
  0x00
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x6E
  0x00
  0x00
  0x80
  0x07
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x40
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x6F
  0x00
  0x00
  0xE0
  0xFF
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x60
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x70
  0x00
  0x00
  0x80
  0x07
  0x40
  0x18
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x40
  0x08
  0xE0
  0xFF
  0x00
  0x00
  0x00
  0x00  ' 0x71
  0x00
  0x00
  0x00
  0x00
  0xE0
  0x1F
  0x80
  0x00
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0xE0
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x72
  0x00
  0x00
  0xC0
  0x18
  0x20
  0x11
  0x20
  0x11
  0x20
  0x12
  0x20
  0x12
  0x20
  0x0C
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x73
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0xF8
  0x0F
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x74
  0x00
  0x00
  0xE0
  0x0F
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x08
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x75
  0x20
  0x00
  0xC0
  0x01
  0x00
  0x06
  0x00
  0x18
  0x00
  0x10
  0x00
  0x0C
  0x00
  0x03
  0xC0
  0x00
  0x20
  0x00
  0x00
  0x00  ' 0x76
  0x60
  0x00
  0x80
  0x07
  0x00
  0x18
  0x00
  0x0E
  0xC0
  0x01
  0x80
  0x01
  0x00
  0x0E
  0x00
  0x18
  0x80
  0x07
  0x60
  0x00  ' 0x77
  0x00
  0x00
  0x20
  0x10
  0x40
  0x08
  0x80
  0x04
  0x00
  0x03
  0x00
  0x03
  0x80
  0x04
  0x40
  0x08
  0x20
  0x10
  0x00
  0x00  ' 0x78
  0x20
  0x80
  0xC0
  0x80
  0x00
  0x83
  0x00
  0x46
  0x00
  0x38
  0x00
  0x18
  0x00
  0x06
  0x00
  0x01
  0xC0
  0x00
  0x20
  0x00  ' 0x79
  0x00
  0x00
  0x20
  0x10
  0x20
  0x18
  0x20
  0x14
  0x20
  0x12
  0x20
  0x11
  0xA0
  0x10
  0x60
  0x10
  0x20
  0x10
  0x00
  0x00  ' 0x7A
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x01
  0xFC
  0x3E
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x00
  0x00
  0x00
  0x00  ' 0x7B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFE
  0x7F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x7C
  0x00
  0x00
  0x00
  0x00
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0xFC
  0x3E
  0x00
  0x01
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00  ' 0x7D
  0x00
  0x03
  0x80
  0x00
  0x80
  0x00
  0x80
  0x00
  0x00
  0x01
  0x00
  0x01
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x80
  0x01  ' 0x7E
  0x00
  0x00
  0x80
  0x1F
  0xC0
  0x10
  0x20
  0x10
  0x10
  0x10
  0x20
  0x10
  0xC0
  0x10
  0x80
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x7F
  0xC0
  0x03
  0x30
  0x0C
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x90
  0x08
  0xB0
  0x18
  0xD0
  0x00
  0x00
  0x00
End Table



Table OLEDFont1Index AS WORD
  1  'space
  3  '!
  5  'DQUOTE
  9  '#
  15 '$
  21 '%
  25 '&
  31 ' \'
  33 ' (
  36 ')
  39 '*
  45 '+
  49 ',
  51 '-
  55 '.
  57 '/
  61 '0
  66'    3, 0x44, 0x7E, 0x40    ' 1
  70'    4, 0x44, 0x62, 0x52, 0x4C  ' 2
  75'    3, 0x4A, 0x4A, 0x34    ' 3
  79'    4, 0x1E, 0x10, 0x7C, 0x10  ' 4
  84'    3, 0x4E, 0x4A, 0x32    ' 5
  88'    4, 0x3C, 0x4A, 0x4A, 0x30  ' 6
  93'    3, 0x62, 0x12, 0x0E    ' 7
  97'    4, 0x34, 0x4A, 0x4A, 0x34  ' 8
  102'    4, 0x0C, 0x52, 0x52, 0x3C  ' 9
  107'    1, 0x48                   '
  109'    2, 0x80, 0x68              ' ;
  112'    3, 0x10, 0x28, 0x44    ' <
  116'    3, 0x28, 0x28, 0x28    ' =
  120'    3, 0x44, 0x28, 0x10    ' >
  124'    3, 0x02, 0x52, 0x0C    ' ?
  128'    5, 0x3C, 0x42, 0x52, 0x2A, 0x3C ' @
  134'    4, 0x7C, 0x12, 0x12, 0x7C  ' A
  139'    4, 0x7E, 0x4A, 0x4A, 0x34  ' B
  144'    4, 0x3C, 0x42, 0x42, 0x24  ' C
  149'    4, 0x7E, 0x42, 0x42, 0x3C  ' D
  154'    3, 0x7E, 0x4A, 0x4A    ' E
  158'    3, 0x7E, 0x0A, 0x0A    ' F
  162'    4, 0x3C, 0x42, 0x52, 0x34  ' G
  167'    4, 0x7E, 0x08, 0x08, 0x7E  ' H
  172'    3, 0x42, 0x7E, 0x42    ' I
  176'    3, 0x42, 0x42, 0x3E    ' J
  180'    4, 0x7E, 0x08, 0x14, 0x62    ' K
  185'    3, 0x7E, 0x40, 0x40    ' L
  189'    5, 0x7E, 0x04, 0x08, 0x04, 0x7E ' M
  195'    5, 0x7E, 0x04, 0x18, 0x20, 0x7E ' N
  201'    4, 0x3C, 0x42, 0x42, 0x3C  ' O
  206'    4, 0x7E, 0x12, 0x12, 0x0C  ' P
  211'    4, 0x3C, 0x42, 0x42, 0xBC  ' Q
  216'    4, 0x7E, 0x12, 0x12, 0x6C  ' R
  221'    4, 0x44, 0x4A, 0x4A, 0x30  ' S
  226'    3, 0x02, 0x7E, 0x02    ' T
  230'    4, 0x3E, 0x40, 0x40, 0x3E  ' U
  235'    5, 0x06, 0x18, 0x60, 0x18, 0x06 ' V
  241'    5, 0x3E, 0x40, 0x3E, 0x40, 0x3E ' W
  247'    5, 0x42, 0x24, 0x18, 0x24, 0x42 ' X
  253'    4, 0x9E, 0xA0, 0xA0, 0x7E  ' Y
  258'    4, 0x62, 0x52, 0x4A, 0x46  ' Z
  263'    2, 0x7E, 0x42              ' left brace
  266'    3, 0x06, 0x18, 0x60    ' Backslash
  270'    2, 0x42, 0x7E              ' right brace
  273'    3, 0x20, 0x10, 0x20    ' ^
  277'    4, 0x80, 0x80, 0x80, 0x80  ' underscore
  282'    2, 0x04, 0x08              ' `
  285'    4, 0x20, 0x54, 0x54, 0x78  ' a
  290'    4, 0x7E, 0x44, 0x44, 0x38  ' b
  295'    4, 0x38, 0x44, 0x44, 0x28  ' c
  300'    4, 0x38, 0x44, 0x44, 0x7E  ' d
  305'    4, 0x38, 0x54, 0x54, 0x58  ' e
  310'    2, 0x7C, 0x0A              ' f
  313'    4, 0x98, 0xA4, 0xA4, 0x7C  ' g
  318'    4, 0x7E, 0x04, 0x04, 0x78  ' h
  323'    1, 0x7A                   ' i
  325'    2, 0x40, 0x3A              ' j
  328'    4, 0x7E, 0x10, 0x28, 0x44  ' k
  333'    1, 0x7E                   ' l
  335'    5, 0x7C, 0x04, 0x78, 0x04, 0x78 ' m
  341'    4, 0x7C, 0x04, 0x04, 0x78  ' n
  346'    4, 0x38, 0x44, 0x44, 0x38  ' o
  351'    4, 0xFC, 0x24, 0x24, 0x18  ' p
  356'    4, 0x18, 0x24, 0x24, 0xFC  ' q
  361'    2, 0x7C, 0x04              ' r
  364'    4, 0x48, 0x54, 0x54, 0x20  ' s
  369'    3, 0x04, 0x3E, 0x44    ' t
  373'    4, 0x3C, 0x40, 0x40, 0x3C    ' u
  378'    5, 0x0C, 0x30, 0x40, 0x30, 0x0C ' v
  384'    5, 0x3C, 0x40, 0x3C, 0x40, 0x3C ' w
  390'    5, 0x44, 0x28, 0x10, 0x28, 0x44 ' x
  396'    4, 0x9C, 0xA0, 0xA0, 0x7C  ' y
  401'    3, 0x64, 0x54, 0x4C    ' z
  405'    3, 0x08, 0x36, 0x41    ' {}
  409'    1, 0xFF                   ' |
  411'    3, 0x41, 0x36, 0x08         ' {}
  415'    4, 0x20, 0x10, 0x20, 0x10 // ~
  420
End Table


Table OLEDFont1Data

    1, 0x00                   ' Space
    1, 0x5E                   ' !
    3, 0x06, 0x00, 0x06     ' DQUOTE
    5, 0x28, 0x7C, 0x28, 0x7C, 0x28 ' #
    5, 0x24, 0x2A, 0x7F, 0x2A, 0x10 ' $
    3, 0x62, 0x18, 0x46     ' %
    5, 0x30, 0x4C, 0x5A, 0x24, 0x50 ' &
    1, 0x06                   ' '
    2, 0x3C, 0x42              ' (
    2, 0x42, 0x3C              ' )
    5, 0x28, 0x10, 0x7C, 0x10, 0x28 ' *
    3, 0x10, 0x38, 0x10     ' +
    1, 0xC0                   ' ,
    3, 0x10, 0x10, 0x10     ' -
    1, 0x40                   ' .
    3, 0x60, 0x18, 0x06     ' /
    4, 0x3C, 0x42, 0x42, 0x3C   ' 0
    3, 0x44, 0x7E, 0x40     ' 1
    4, 0x44, 0x62, 0x52, 0x4C   ' 2
    3, 0x4A, 0x4A, 0x34     ' 3
    4, 0x1E, 0x10, 0x7C, 0x10   ' 4
    3, 0x4E, 0x4A, 0x32     ' 5
    4, 0x3C, 0x4A, 0x4A, 0x30   ' 6
    3, 0x62, 0x12, 0x0E     ' 7
    4, 0x34, 0x4A, 0x4A, 0x34   ' 8
    4, 0x0C, 0x52, 0x52, 0x3C   ' 9
    1, 0x48                   '
    2, 0x80, 0x68              ' ;
    3, 0x10, 0x28, 0x44     ' <
    3, 0x28, 0x28, 0x28     ' =
    3, 0x44, 0x28, 0x10     ' >
    3, 0x02, 0x52, 0x0C     ' ?
    5, 0x38, 0x44, 0x54, 0x54, 0x58 ' @
    4, 0x7C, 0x12, 0x12, 0x7C   ' A
    4, 0x7E, 0x4A, 0x4A, 0x34   ' B
    4, 0x3C, 0x42, 0x42, 0x24   ' C
    4, 0x7E, 0x42, 0x42, 0x3C   ' D
    3, 0x7E, 0x4A, 0x4A     ' E
    3, 0x7E, 0x0A, 0x0A     ' F
    4, 0x3C, 0x42, 0x52, 0x34   ' G
    4, 0x7E, 0x08, 0x08, 0x7E   ' H
    3, 0x42, 0x7E, 0x42     ' I
    3, 0x42, 0x42, 0x3E     ' J
    4, 0x7E, 0x08, 0x14, 0x62    ' K
    3, 0x7E, 0x40, 0x40     ' L
    5, 0x7E, 0x04, 0x08, 0x04, 0x7E ' M
    5, 0x7E, 0x04, 0x18, 0x20, 0x7E ' N
    4, 0x3C, 0x42, 0x42, 0x3C   ' O
    4, 0x7E, 0x12, 0x12, 0x0C   ' P
    4, 0x3C, 0x42, 0x42, 0xBC   ' Q
    4, 0x7E, 0x12, 0x12, 0x6C   ' R
    4, 0x44, 0x4A, 0x4A, 0x30   ' S
    3, 0x02, 0x7E, 0x02     ' T
    4, 0x3E, 0x40, 0x40, 0x3E   ' U
    5, 0x06, 0x18, 0x60, 0x18, 0x06 ' V
    5, 0x3E, 0x40, 0x3E, 0x40, 0x3E ' W
    5, 0x42, 0x24, 0x18, 0x24, 0x42 ' X
    4, 0x9E, 0xA0, 0xA0, 0x7E   ' Y
    4, 0x62, 0x52, 0x4A, 0x46   ' Z
    2, 0x7E, 0x42              ' [
    3, 0x06, 0x18, 0x60     ' Backslash
    2, 0x42, 0x7E              ' ]
    3, 0x20, 0x10, 0x20     ' ^
    4, 0x80, 0x80, 0x80, 0x80   ' underscore
    2, 0x04, 0x08              ' `
    4, 0x20, 0x54, 0x54, 0x78   ' a
    4, 0x7E, 0x44, 0x44, 0x38   ' b
    4, 0x38, 0x44, 0x44, 0x28   ' c
    4, 0x38, 0x44, 0x44, 0x7E   ' d
    4, 0x38, 0x54, 0x54, 0x58   ' e
    2, 0x7C, 0x0A              ' f
    4, 0x98, 0xA4, 0xA4, 0x7C   ' g
    4, 0x7E, 0x04, 0x04, 0x78   ' h
    1, 0x7A                   ' i
    2, 0x40, 0x3A              ' j
    4, 0x7E, 0x10, 0x28, 0x44   ' k
    1, 0x7E                   ' l
    5, 0x7C, 0x04, 0x78, 0x04, 0x78 ' m
    4, 0x7C, 0x04, 0x04, 0x78   ' n
    4, 0x38, 0x44, 0x44, 0x38   ' o
    4, 0xFC, 0x24, 0x24, 0x18   ' p
    4, 0x18, 0x24, 0x24, 0xFC   ' q
    2, 0x7C, 0x04              ' r
    4, 0x48, 0x54, 0x54, 0x20   ' s
    3, 0x04, 0x3E, 0x44     ' t
    4, 0x3C, 0x40, 0x40, 0x3C    ' u
    5, 0x0C, 0x30, 0x40, 0x30, 0x0C ' v
    5, 0x3C, 0x40, 0x3C, 0x40, 0x3C ' w
    5, 0x44, 0x28, 0x10, 0x28, 0x44 ' x
    4, 0x9C, 0xA0, 0xA0, 0x7C   ' y
    3, 0x64, 0x54, 0x4C     ' z
    3, 0x08, 0x36, 0x41     ' {
    1, 0xFF                   ' |
    3, 0x41, 0x36, 0x08         ' }
    4, 0x20, 0x10, 0x20, 0x10   '// ~
    1, 0x00                   ' 127th...
End Table



Table OLEDExtendedFont2 as byte
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x20
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFE
  0x1B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x21
  0x00
  0x00
  0x00
  0x00
  0x1E
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x1E
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x22
  0x00
  0x01
  0x20
  0x19
  0xA0
  0x07
  0x78
  0x01
  0x26
  0x19
  0xA0
  0x07
  0x78
  0x01
  0x26
  0x01
  0x20
  0x01
  0x00
  0x00  ' 0x23
  0x00
  0x00
  0x00
  0x00
  0x1C
  0x18
  0x26
  0x10
  0x42
  0x10
  0xFF
  0x3F
  0x82
  0x11
  0x02
  0x0F
  0x00
  0x00
  0x00
  0x00  ' 0x24
  0x1C
  0x10
  0x22
  0x08
  0x22
  0x04
  0x22
  0x03
  0x9C
  0x00
  0x40
  0x0E
  0x30
  0x11
  0x08
  0x11
  0x04
  0x11
  0x02
  0x0E  ' 0x25
  0x00
  0x07
  0x80
  0x08
  0x5C
  0x10
  0x62
  0x10
  0xA2
  0x11
  0x32
  0x13
  0x1C
  0x1C
  0x00
  0x18
  0x00
  0x16
  0x80
  0x01  ' 0x26
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x1E
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x27
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xE0
  0x07
  0x18
  0x18
  0x0C
  0x30
  0x04
  0x20
  0x02
  0x40
  0x02
  0x40
  0x00
  0x00  ' 0x28
  0x00
  0x00
  0x02
  0x40
  0x02
  0x40
  0x04
  0x20
  0x0C
  0x30
  0x18
  0x18
  0xE0
  0x07
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x29
  0x00
  0x00
  0x08
  0x00
  0x18
  0x00
  0xF0
  0x00
  0x4E
  0x00
  0xF0
  0x00
  0x18
  0x00
  0x08
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x2A
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0xE0
  0x1F
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0x00  ' 0x2B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x98
  0x00
  0x78
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x2C
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00  ' 0x2D
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x18
  0x00
  0x18
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x2E
  0x00
  0x00
  0x00
  0x40
  0x00
  0x30
  0x00
  0x0C
  0x00
  0x03
  0xC0
  0x00
  0x30
  0x00
  0x0C
  0x00
  0x02
  0x00
  0x00
  0x00  ' 0x2F
  0x00
  0x00
  0xF0
  0x03
  0x0C
  0x0C
  0x02
  0x10
  0x02
  0x10
  0x02
  0x10
  0x0C
  0x0C
  0xF0
  0x03
  0x00
  0x00
  0x00
  0x00  ' 0x30
  0x00
  0x00
  0x04
  0x10
  0x04
  0x10
  0x04
  0x10
  0xFE
  0x1F
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x31
  0x00
  0x00
  0x06
  0x18
  0x02
  0x14
  0x02
  0x12
  0x02
  0x11
  0xC2
  0x10
  0x3C
  0x10
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x32
  0x00
  0x00
  0x00
  0x00
  0x02
  0x10
  0x42
  0x10
  0x42
  0x10
  0x42
  0x10
  0xBC
  0x0F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x33
  0x00
  0x03
  0xC0
  0x02
  0x20
  0x02
  0x18
  0x02
  0x04
  0x02
  0xFE
  0x1F
  0x00
  0x02
  0x00
  0x02
  0x00
  0x00
  0x00
  0x00  ' 0x34
  0x00
  0x00
  0x00
  0x00
  0x3E
  0x10
  0x22
  0x10
  0x22
  0x10
  0x42
  0x08
  0x82
  0x07
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x35
  0x00
  0x00
  0xF0
  0x07
  0x4C
  0x08
  0x22
  0x10
  0x22
  0x10
  0x22
  0x10
  0x42
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x36
  0x00
  0x00
  0x02
  0x00
  0x02
  0x18
  0x02
  0x07
  0xC2
  0x00
  0x32
  0x00
  0x0A
  0x00
  0x06
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x37
  0x00
  0x00
  0x1C
  0x0F
  0xA2
  0x10
  0x42
  0x10
  0x42
  0x10
  0xA2
  0x10
  0xA2
  0x09
  0x1C
  0x06
  0x00
  0x00
  0x00
  0x00  ' 0x38
  0x00
  0x00
  0x78
  0x00
  0x84
  0x10
  0x02
  0x11
  0x02
  0x11
  0x02
  0x11
  0x84
  0x0C
  0xF8
  0x03
  0x00
  0x00
  0x00
  0x00  ' 0x39
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x60
  0x18
  0x60
  0x18
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x3A
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x60
  0x98
  0x60
  0x78
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x3B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x03
  0x00
  0x03
  0x80
  0x04
  0x80
  0x04
  0x40
  0x08
  0x40
  0x08
  0x20
  0x10
  0x00
  0x00  ' 0x3C
  0x00
  0x00
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x80
  0x04
  0x00
  0x00  ' 0x3D
  0x00
  0x00
  0x20
  0x10
  0x40
  0x08
  0x40
  0x08
  0x80
  0x04
  0x80
  0x04
  0x00
  0x03
  0x00
  0x03
  0x00
  0x00
  0x00
  0x00  ' 0x3E
  0x00
  0x00
  0x0E
  0x00
  0x02
  0x00
  0x02
  0x1B
  0x82
  0x00
  0x42
  0x00
  0x26
  0x00
  0x1C
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x3F
  0xF0
  0x03
  0x18
  0x0C
  0x04
  0x18
  0xE2
  0x13
  0x12
  0x14
  0x0A
  0x16
  0x8A
  0x1B
  0xFC
  0x07
  0x00
  0x04
  0x00
  0x04  ' 0x40
  0x00
  0x10
  0x00
  0x0E
  0x80
  0x03
  0x70
  0x02
  0x18
  0x02
  0x30
  0x02
  0xC0
  0x02
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x10  ' 0x41
  0x00
  0x00
  0xF8
  0x1F
  0x88
  0x10
  0x88
  0x10
  0x88
  0x10
  0x88
  0x10
  0x48
  0x11
  0x30
  0x0E
  0x00
  0x00
  0x00
  0x00  ' 0x42
  0xC0
  0x03
  0x30
  0x0C
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x18
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x43
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x44
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x11
  0x08
  0x11
  0x08
  0x11
  0x08
  0x11
  0x08
  0x11
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x45
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x46
  0x00
  0x00
  0xC0
  0x03
  0x30
  0x0C
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x11
  0x08
  0x11
  0x18
  0x1F
  0x00
  0x00  ' 0x47
  0x00
  0x00
  0xF8
  0x1F
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0xF8
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x48
  0x00
  0x00
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0xF8
  0x1F
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x49
  0x00
  0x00
  0x00
  0x00
  0x00
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0xF8
  0x0F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x4A
  0x00
  0x00
  0xF8
  0x1F
  0x80
  0x00
  0x80
  0x01
  0x40
  0x02
  0x20
  0x04
  0x10
  0x04
  0x08
  0x08
  0x00
  0x10
  0x00
  0x00  ' 0x4B
  0x00
  0x00
  0xF8
  0x1F
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x4C
  0xF8
  0x1F
  0x38
  0x00
  0xE0
  0x01
  0x00
  0x07
  0x00
  0x06
  0xC0
  0x01
  0x38
  0x00
  0xF8
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x4D
  0x00
  0x00
  0xF8
  0x1F
  0x10
  0x00
  0x60
  0x00
  0x80
  0x01
  0x00
  0x06
  0x00
  0x08
  0xF8
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x4E
  0x00
  0x00
  0xE0
  0x07
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00  ' 0x4F
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x08
  0x01
  0x88
  0x00
  0x70
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x50
  0x00
  0x00
  0xE0
  0x07
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x08
  0x30
  0x10
  0x48
  0xE0
  0x47
  0x00
  0x00  ' 0x51
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x01
  0x08
  0x01
  0x08
  0x03
  0x88
  0x04
  0x70
  0x08
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x52
  0x00
  0x00
  0x70
  0x18
  0x48
  0x10
  0x88
  0x10
  0x88
  0x10
  0x08
  0x11
  0x08
  0x09
  0x18
  0x0E
  0x00
  0x00
  0x00
  0x00  ' 0x53
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0xF8
  0x1F
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0x08
  0x00
  0x00
  0x00  ' 0x54
  0x00
  0x00
  0xF8
  0x07
  0x00
  0x18
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x08
  0xF8
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x55
  0x08
  0x00
  0x30
  0x00
  0xC0
  0x01
  0x00
  0x06
  0x00
  0x18
  0x00
  0x18
  0x00
  0x07
  0xC0
  0x00
  0x30
  0x00
  0x08
  0x00  ' 0x56
  0x18
  0x00
  0xE0
  0x03
  0x00
  0x1C
  0x00
  0x0F
  0xE0
  0x00
  0xC0
  0x01
  0x00
  0x0E
  0x00
  0x1C
  0xE0
  0x03
  0x18
  0x00  ' 0x57
  0x08
  0x10
  0x10
  0x08
  0x20
  0x04
  0x40
  0x02
  0x80
  0x01
  0x80
  0x01
  0x40
  0x02
  0x20
  0x04
  0x10
  0x08
  0x08
  0x10  ' 0x58
  0x08
  0x00
  0x10
  0x00
  0x60
  0x00
  0x80
  0x00
  0x00
  0x1F
  0x80
  0x00
  0x40
  0x00
  0x20
  0x00
  0x10
  0x00
  0x08
  0x00  ' 0x59
  0x00
  0x00
  0x08
  0x18
  0x08
  0x14
  0x08
  0x12
  0x08
  0x11
  0x88
  0x10
  0x48
  0x10
  0x28
  0x10
  0x18
  0x10
  0x00
  0x00  ' 0x5A
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFE
  0x7F
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x00
  0x00
  0x00
  0x00  ' 0x5B
  0x00
  0x00
  0x02
  0x00
  0x0C
  0x00
  0x30
  0x00
  0xC0
  0x00
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x30
  0x00
  0x40
  0x00
  0x00  ' 0x5C
  0x00
  0x00
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0xFE
  0x7F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x5D
  0x00
  0x00
  0x00
  0x04
  0x00
  0x03
  0xE0
  0x00
  0x38
  0x00
  0x0E
  0x00
  0x70
  0x00
  0x80
  0x03
  0x00
  0x04
  0x00
  0x00  ' 0x5E
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20  ' 0x5F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x02
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x60
  0x00
  0x00
  0x00
  0x0C
  0x20
  0x12
  0x20
  0x11
  0x20
  0x11
  0x20
  0x09
  0xC0
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x61
  0x00
  0x00
  0xFE
  0x1F
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x60
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x62
  0x00
  0x00
  0x80
  0x07
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x63
  0x00
  0x00
  0x80
  0x07
  0x40
  0x18
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x40
  0x08
  0xFE
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x64
  0x00
  0x00
  0x80
  0x07
  0x40
  0x09
  0x20
  0x11
  0x20
  0x11
  0x20
  0x11
  0x20
  0x11
  0xC0
  0x11
  0x00
  0x00
  0x00
  0x00  ' 0x65
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0xFC
  0x1F
  0x24
  0x00
  0x22
  0x00
  0x22
  0x00
  0x22
  0x00
  0x22
  0x00
  0x00
  0x00  ' 0x66
  0x00
  0x00
  0x80
  0x07
  0x40
  0x98
  0x20
  0x90
  0x20
  0x90
  0x20
  0x90
  0x40
  0x48
  0xE0
  0x3F
  0x00
  0x00
  0x00
  0x00  ' 0x67
  0x00
  0x00
  0xFE
  0x1F
  0x80
  0x00
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x68
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0x26
  0x00
  0xE6
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x69
  0x00
  0x00
  0x00
  0x80
  0x20
  0x80
  0x20
  0x80
  0x26
  0x80
  0xE6
  0x7F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x6A
  0x00
  0x00
  0xFE
  0x1F
  0x00
  0x01
  0x00
  0x03
  0x80
  0x04
  0x40
  0x04
  0x40
  0x08
  0x20
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x6B
  0x00
  0x00
  0x00
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0xFE
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x6C
  0xE0
  0x1F
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x00
  0x00  ' 0x6D
  0x00
  0x00
  0xE0
  0x1F
  0xC0
  0x00
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0xC0
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0x6E
  0x00
  0x00
  0x80
  0x07
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x40
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x6F
  0x00
  0x00
  0xE0
  0xFF
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x60
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x70
  0x00
  0x00
  0x80
  0x07
  0x40
  0x18
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x40
  0x08
  0xE0
  0xFF
  0x00
  0x00
  0x00
  0x00  ' 0x71
  0x00
  0x00
  0x00
  0x00
  0xE0
  0x1F
  0x80
  0x00
  0x40
  0x00
  0x20
  0x00
  0x20
  0x00
  0xE0
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x72
  0x00
  0x00
  0xC0
  0x18
  0x20
  0x11
  0x20
  0x11
  0x20
  0x12
  0x20
  0x12
  0x20
  0x0C
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x73
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0xF8
  0x0F
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x74
  0x00
  0x00
  0xE0
  0x0F
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x08
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x75
  0x20
  0x00
  0xC0
  0x01
  0x00
  0x06
  0x00
  0x18
  0x00
  0x10
  0x00
  0x0C
  0x00
  0x03
  0xC0
  0x00
  0x20
  0x00
  0x00
  0x00  ' 0x76
  0x60
  0x00
  0x80
  0x07
  0x00
  0x18
  0x00
  0x0E
  0xC0
  0x01
  0x80
  0x01
  0x00
  0x0E
  0x00
  0x18
  0x80
  0x07
  0x60
  0x00  ' 0x77
  0x00
  0x00
  0x20
  0x10
  0x40
  0x08
  0x80
  0x04
  0x00
  0x03
  0x00
  0x03
  0x80
  0x04
  0x40
  0x08
  0x20
  0x10
  0x00
  0x00  ' 0x78
  0x20
  0x80
  0xC0
  0x80
  0x00
  0x83
  0x00
  0x46
  0x00
  0x38
  0x00
  0x18
  0x00
  0x06
  0x00
  0x01
  0xC0
  0x00
  0x20
  0x00  ' 0x79
  0x00
  0x00
  0x20
  0x10
  0x20
  0x18
  0x20
  0x14
  0x20
  0x12
  0x20
  0x11
  0xA0
  0x10
  0x60
  0x10
  0x20
  0x10
  0x00
  0x00  ' 0x7A
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x01
  0xFC
  0x3E
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0x00
  0x00
  0x00
  0x00  ' 0x7B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFE
  0x7F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x7C
  0x00
  0x00
  0x00
  0x00
  0x02
  0x40
  0x02
  0x40
  0x02
  0x40
  0xFC
  0x3E
  0x00
  0x01
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00  ' 0x7D
  0x00
  0x03
  0x80
  0x00
  0x80
  0x00
  0x80
  0x00
  0x00
  0x01
  0x00
  0x01
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x80
  0x01  ' 0x7E
  0x00
  0x00
  0x80
  0x1F
  0xC0
  0x10
  0x20
  0x10
  0x10
  0x10
  0x20
  0x10
  0xC0
  0x10
  0x80
  0x1F
  0x00
  0x00
  0x00
  0x00   '0x7F
  0xC0
  0x03
  0x30
  0x0C
  0x10
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x90
  0x08
  0xB0
  0x18
  0xD0
  0x00
  0x00
  0x00
  0x00  ' 0x80
  0x00
  0x00
  0xE0
  0x0F
  0x04
  0x10
  0x00
  0x10
  0x00
  0x10
  0x04
  0x08
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x81
  0x00
  0x00
  0x80
  0x07
  0x40
  0x09
  0x20
  0x11
  0x24
  0x11
  0x22
  0x11
  0x20
  0x11
  0xC0
  0x11
  0x00
  0x00
  0x00
  0x00  ' 0x82
  0x00
  0x00
  0x00
  0x0C
  0x24
  0x12
  0x22
  0x11
  0x22
  0x11
  0x24
  0x09
  0xC0
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x83
  0x00
  0x00
  0x00
  0x0C
  0x24
  0x12
  0x20
  0x11
  0x20
  0x11
  0x24
  0x09
  0xC0
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x84
  0x00
  0x00
  0x00
  0x0C
  0x22
  0x12
  0x24
  0x11
  0x20
  0x11
  0x20
  0x09
  0xC0
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x85
  0x00
  0x00
  0x00
  0x0C
  0x20
  0x12
  0x22
  0x11
  0x25
  0x11
  0x22
  0x09
  0xC0
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x86
  0x00
  0x00
  0x80
  0x07
  0x40
  0x08
  0x20
  0x10
  0x20
  0x90
  0x20
  0xB0
  0x20
  0xD0
  0x20
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x87
  0x00
  0x00
  0x80
  0x07
  0x44
  0x09
  0x22
  0x11
  0x22
  0x11
  0x24
  0x11
  0x20
  0x11
  0xC0
  0x11
  0x00
  0x00
  0x00
  0x00  ' 0x88
  0x00
  0x00
  0x80
  0x07
  0x40
  0x09
  0x24
  0x11
  0x20
  0x11
  0x20
  0x11
  0x24
  0x11
  0xC0
  0x11
  0x00
  0x00
  0x00
  0x00  ' 0x89
  0x00
  0x00
  0x80
  0x07
  0x40
  0x09
  0x22
  0x11
  0x24
  0x11
  0x20
  0x11
  0x20
  0x11
  0xC0
  0x11
  0x00
  0x00
  0x00
  0x00  ' 0x8A
  0x00
  0x00
  0x20
  0x00
  0x24
  0x00
  0x20
  0x00
  0xE0
  0x1F
  0x04
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x8B
  0x00
  0x00
  0x20
  0x00
  0x24
  0x00
  0x22
  0x00
  0xE2
  0x1F
  0x04
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x8C
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0x22
  0x00
  0xE4
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x8D
  0x00
  0x10
  0x00
  0x0E
  0x81
  0x03
  0x70
  0x02
  0x18
  0x02
  0x30
  0x02
  0xC1
  0x02
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x10  ' 0x8E
  0x00
  0x10
  0x00
  0x0E
  0x80
  0x03
  0x72
  0x02
  0x0D
  0x02
  0x1D
  0x02
  0xF2
  0x02
  0x80
  0x03
  0x00
  0x0E
  0x00
  0x10  ' 0x8F
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x11
  0x08
  0x11
  0x0A
  0x11
  0x09
  0x11
  0x08
  0x11
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x90
  0x20
  0x0E
  0x20
  0x13
  0x20
  0x11
  0x20
  0x11
  0xC0
  0x0F
  0x20
  0x19
  0x20
  0x11
  0x20
  0x11
  0xC0
  0x11
  0x00
  0x00  ' 0x91
  0x00
  0x10
  0x00
  0x0C
  0x80
  0x07
  0x60
  0x04
  0x18
  0x04
  0xF8
  0x1F
  0x08
  0x11
  0x08
  0x11
  0x08
  0x10
  0x00
  0x00  ' 0x92
  0x00
  0x00
  0x80
  0x07
  0x44
  0x08
  0x22
  0x10
  0x22
  0x10
  0x24
  0x10
  0x40
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x93
  0x00
  0x00
  0x80
  0x07
  0x44
  0x08
  0x20
  0x10
  0x20
  0x10
  0x24
  0x10
  0x40
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x94
  0x00
  0x00
  0x80
  0x07
  0x42
  0x08
  0x24
  0x10
  0x20
  0x10
  0x20
  0x10
  0x40
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x95
  0x00
  0x00
  0xE0
  0x0F
  0x04
  0x10
  0x02
  0x10
  0x02
  0x10
  0x04
  0x08
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x96
  0x00
  0x00
  0xE0
  0x0F
  0x02
  0x10
  0x04
  0x10
  0x00
  0x10
  0x00
  0x08
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x97
  0x20
  0x80
  0xC0
  0x80
  0x04
  0x83
  0x00
  0x46
  0x00
  0x38
  0x04
  0x18
  0x00
  0x06
  0x00
  0x01
  0xC0
  0x00
  0x20
  0x00  ' 0x98
  0x00
  0x00
  0xE0
  0x07
  0x11
  0x08
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x09
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00  ' 0x99
  0x00
  0x00
  0xF8
  0x07
  0x01
  0x18
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x01
  0x08
  0xF8
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x9A
  0x00
  0x00
  0x80
  0x17
  0x40
  0x08
  0x20
  0x14
  0x20
  0x13
  0xA0
  0x10
  0x40
  0x08
  0xA0
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0x9B
  0x00
  0x00
  0x00
  0x00
  0x00
  0x10
  0x40
  0x18
  0xFC
  0x17
  0x42
  0x10
  0x42
  0x10
  0x02
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0x9C
  0x00
  0x00
  0xE0
  0x17
  0x10
  0x0C
  0x08
  0x16
  0x08
  0x11
  0x88
  0x10
  0x68
  0x10
  0x30
  0x08
  0xE8
  0x07
  0x00
  0x00  ' 0x9D
  0x00
  0x00
  0x20
  0x10
  0x40
  0x08
  0x80
  0x04
  0x00
  0x03
  0x00
  0x03
  0x80
  0x04
  0x40
  0x08
  0x20
  0x10
  0x00
  0x00  ' 0x9E
  0x00
  0x00
  0x00
  0x80
  0x20
  0x80
  0x20
  0x80
  0xFE
  0x7F
  0x21
  0x00
  0x21
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0x9F
  0x00
  0x00
  0x00
  0x0C
  0x20
  0x12
  0x20
  0x11
  0x24
  0x11
  0x22
  0x09
  0xC0
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xA0
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0xE4
  0x1F
  0x02
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xA1
  0x00
  0x00
  0x80
  0x07
  0x40
  0x08
  0x20
  0x10
  0x24
  0x10
  0x22
  0x10
  0x40
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xA2
  0x00
  0x00
  0xE0
  0x0F
  0x00
  0x10
  0x00
  0x10
  0x04
  0x10
  0x02
  0x08
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xA3
  0x00
  0x00
  0xE0
  0x1F
  0xC4
  0x00
  0x42
  0x00
  0x24
  0x00
  0x24
  0x00
  0x22
  0x00
  0xC0
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0xA4
  0x00
  0x00
  0xF8
  0x1F
  0x12
  0x00
  0x61
  0x00
  0x83
  0x01
  0x02
  0x06
  0x01
  0x08
  0xF8
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0xA5
  0x00
  0x00
  0x00
  0x00
  0x32
  0x00
  0x4A
  0x00
  0x4A
  0x00
  0x4A
  0x00
  0x7C
  0x00
  0x40
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xA6
  0x00
  0x00
  0x00
  0x00
  0x3C
  0x00
  0x42
  0x00
  0x42
  0x00
  0x42
  0x00
  0x42
  0x00
  0x3C
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xA7
  0x00
  0x00
  0x00
  0x70
  0x00
  0xC8
  0x00
  0x88
  0x00
  0x84
  0x60
  0x83
  0x00
  0x80
  0x00
  0xE0
  0x00
  0x00
  0x00
  0x00  ' 0xA8
  0x00
  0x00
  0x38
  0x00
  0x44
  0x00
  0xBA
  0x00
  0xAA
  0x00
  0xBA
  0x00
  0x44
  0x00
  0x38
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xA9
  0x00
  0x00
  0x80
  0x00
  0x80
  0x00
  0x80
  0x00
  0x80
  0x00
  0x80
  0x00
  0x80
  0x00
  0x80
  0x00
  0x80
  0x07
  0x00
  0x00  ' 0xAA
  0x02
  0x10
  0x7E
  0x0C
  0x00
  0x02
  0x80
  0x01
  0x60
  0x00
  0x10
  0x00
  0x8C
  0x10
  0x82
  0x1C
  0x80
  0x14
  0x00
  0x13  ' 0xAB
  0x02
  0x10
  0x7E
  0x0C
  0x00
  0x02
  0x80
  0x01
  0x60
  0x00
  0x10
  0x06
  0x0C
  0x05
  0x82
  0x04
  0x80
  0x1F
  0x00
  0x04  ' 0xAC
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x60
  0xFF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xAD
  0x00
  0x00
  0x00
  0x01
  0x80
  0x02
  0x40
  0x04
  0x20
  0x08
  0x00
  0x01
  0x80
  0x02
  0x40
  0x04
  0x20
  0x08
  0x00
  0x00  ' 0xAE
  0x00
  0x00
  0x20
  0x08
  0x40
  0x04
  0x80
  0x02
  0x00
  0x01
  0x20
  0x08
  0x40
  0x04
  0x80
  0x02
  0x00
  0x01
  0x00
  0x00  ' 0xAF
  0xDB
  0x6C
  0xDB
  0x6C
  0x00
  0x00
  0xDB
  0x6C
  0xDB
  0x6C
  0x00
  0x00
  0xDB
  0x6C
  0xDB
  0x6C
  0x00
  0x00
  0x00
  0x00  ' 0xB0
  0x6C
  0xDB
  0x6C
  0xDB
  0xDB
  0x6C
  0xFF
  0xFF
  0x6C
  0xDB
  0xDB
  0x6C
  0xFF
  0xFF
  0x6C
  0xDB
  0xDB
  0x6C
  0xDB
  0x6C  ' 0xB1
  0xFF
  0xFF
  0xFF
  0xFF
  0xDB
  0x6C
  0xFF
  0xFF
  0xFF
  0xFF
  0xDB
  0x6C
  0xFF
  0xFF
  0xFF
  0xFF
  0xDB
  0x6C
  0xDB
  0x6C  ' 0xB2
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFF
  0xFF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xB3
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0xFF
  0xFF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xB4
  0x00
  0x10
  0x00
  0x0E
  0x80
  0x03
  0x70
  0x02
  0x1A
  0x02
  0x31
  0x02
  0xC0
  0x02
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x10  ' 0xB5
  0x00
  0x10
  0x00
  0x0E
  0x80
  0x03
  0x72
  0x02
  0x19
  0x02
  0x31
  0x02
  0xC2
  0x02
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x10  ' 0xB6
  0x00
  0x10
  0x00
  0x0E
  0x80
  0x03
  0x71
  0x02
  0x1A
  0x02
  0x30
  0x02
  0xC0
  0x02
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x10  ' 0xB7
  0xF0
  0x03
  0x0C
  0x0C
  0xE6
  0x19
  0x32
  0x13
  0x12
  0x12
  0x12
  0x12
  0x16
  0x1A
  0x0C
  0x0C
  0xF0
  0x03
  0x00
  0x00  ' 0xB8
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0xFF
  0xFE
  0x00
  0x00
  0xFF
  0xFF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xB9
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFF
  0xFF
  0x00
  0x00
  0xFF
  0xFF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xBA
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0xFE
  0x80
  0x00
  0x80
  0xFF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xBB
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0xFF
  0x02
  0x00
  0x02
  0xFF
  0x03
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xBC
  0x00
  0x00
  0xF0
  0x03
  0x08
  0x04
  0x04
  0x0C
  0x04
  0x08
  0xFE
  0x1F
  0x04
  0x08
  0x04
  0x08
  0x00
  0x00
  0x00
  0x00  ' 0xBD
  0x02
  0x00
  0x04
  0x00
  0x98
  0x04
  0xA0
  0x04
  0xC0
  0x1F
  0xA0
  0x04
  0x90
  0x04
  0x08
  0x00
  0x04
  0x00
  0x02
  0x00  ' 0xBE
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0xFF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xBF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFF
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01  ' 0xC0
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0xFF
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01  ' 0xC1
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0xFF
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01  ' 0xC2
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFF
  0xFF
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01  ' 0xC3
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01  ' 0xC4
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0xFF
  0xFF
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01  ' 0xC5
  0x00
  0x00
  0x00
  0x0C
  0x24
  0x12
  0x22
  0x11
  0x24
  0x11
  0x24
  0x09
  0xC2
  0x1F
  0x00
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xC6
  0x00
  0x10
  0x00
  0x0E
  0x82
  0x03
  0x71
  0x02
  0x1B
  0x02
  0x32
  0x02
  0xC1
  0x02
  0x00
  0x03
  0x00
  0x0C
  0x00
  0x10  ' 0xC7
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFF
  0x03
  0x00
  0x02
  0xFF
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02  ' 0xC8
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x80
  0xFF
  0x80
  0x00
  0x80
  0xFE
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02  ' 0xC9
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0xFF
  0x02
  0x00
  0x02
  0xFF
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02  ' 0xCA
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0xFE
  0x80
  0x00
  0x80
  0xFE
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02  ' 0xCB
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFF
  0xFF
  0x00
  0x00
  0xFF
  0xFE
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02  ' 0xCC
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02  ' 0xCD
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0xFF
  0xFE
  0x00
  0x00
  0xFF
  0xFE
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02
  0x80
  0x02  ' 0xCE
  0x00
  0x00
  0x08
  0x04
  0xF0
  0x03
  0x10
  0x02
  0x10
  0x02
  0x10
  0x02
  0x10
  0x02
  0xF0
  0x03
  0x08
  0x04
  0x00
  0x00  ' 0xCF
  0x00
  0x00
  0x82
  0x07
  0x4A
  0x08
  0x2E
  0x10
  0x24
  0x10
  0x2A
  0x10
  0x70
  0x08
  0xC0
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xD0
  0x80
  0x00
  0xF8
  0x1F
  0x88
  0x10
  0x88
  0x10
  0x08
  0x10
  0x08
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xD1
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x11
  0x0A
  0x11
  0x09
  0x11
  0x09
  0x11
  0x0A
  0x11
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xD2
  0x00
  0x00
  0xF8
  0x1F
  0x09
  0x11
  0x08
  0x11
  0x08
  0x11
  0x08
  0x11
  0x09
  0x11
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xD3
  0x00
  0x00
  0xF8
  0x1F
  0x08
  0x11
  0x08
  0x11
  0x09
  0x11
  0x0A
  0x11
  0x08
  0x11
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xD4
  0x00
  0x00
  0x20
  0x00
  0x20
  0x00
  0x20
  0x00
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xD5
  0x00
  0x00
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0xFA
  0x1F
  0x09
  0x10
  0x08
  0x10
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xD6
  0x00
  0x00
  0x08
  0x10
  0x08
  0x10
  0x0A
  0x10
  0xF9
  0x1F
  0x09
  0x10
  0x0A
  0x10
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xD7
  0x00
  0x00
  0x08
  0x10
  0x09
  0x10
  0x08
  0x10
  0xF8
  0x1F
  0x08
  0x10
  0x09
  0x10
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xD8
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0xFF
  0x01
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xD9
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0xFF
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01  ' 0xDA
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF
  0xFF  ' 0xDB
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF  ' 0xDC
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x7E
  0x7C
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xDD
  0x00
  0x00
  0x08
  0x10
  0x08
  0x10
  0x09
  0x10
  0xFA
  0x1F
  0x08
  0x10
  0x08
  0x10
  0x08
  0x10
  0x00
  0x00
  0x00
  0x00  ' 0xDE
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00
  0xFF
  0x00  ' 0xDF
  0x00
  0x00
  0xE0
  0x07
  0x10
  0x08
  0x08
  0x10
  0x0A
  0x10
  0x09
  0x10
  0x08
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00  ' 0xE0
  0x00
  0x00
  0xFC
  0x1F
  0x02
  0x00
  0x02
  0x00
  0xE2
  0x10
  0x1C
  0x11
  0x00
  0x12
  0x00
  0x0C
  0x00
  0x00
  0x00
  0x00  ' 0xE1
  0x00
  0x00
  0xE0
  0x07
  0x10
  0x08
  0x0A
  0x10
  0x09
  0x10
  0x09
  0x10
  0x0A
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00  ' 0xE2
  0x00
  0x00
  0xE0
  0x07
  0x10
  0x08
  0x09
  0x10
  0x0A
  0x10
  0x08
  0x10
  0x08
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00  ' 0xE3
  0x00
  0x00
  0x80
  0x07
  0x44
  0x08
  0x22
  0x10
  0x24
  0x10
  0x24
  0x10
  0x42
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xE4
  0x00
  0x00
  0xE0
  0x07
  0x12
  0x08
  0x09
  0x10
  0x0B
  0x10
  0x0A
  0x10
  0x09
  0x10
  0x10
  0x08
  0xE0
  0x07
  0x00
  0x00  ' 0xE5
  0x00
  0x00
  0xE0
  0xFF
  0x00
  0x08
  0x00
  0x10
  0x00
  0x10
  0x00
  0x10
  0x00
  0x08
  0xE0
  0x1F
  0x00
  0x00
  0x00
  0x00  ' 0xE6
  0x00
  0x00
  0xFE
  0xFF
  0x40
  0x08
  0x20
  0x10
  0x20
  0x10
  0x20
  0x10
  0x60
  0x08
  0x80
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xE7
  0x00
  0x00
  0xF8
  0x1F
  0x20
  0x04
  0x20
  0x04
  0x20
  0x04
  0x20
  0x04
  0x20
  0x02
  0xC0
  0x01
  0x00
  0x00
  0x00
  0x00  ' 0xE8
  0x00
  0x00
  0xF8
  0x07
  0x00
  0x18
  0x00
  0x10
  0x02
  0x10
  0x01
  0x10
  0x00
  0x08
  0xF8
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xE9
  0x00
  0x00
  0xF8
  0x07
  0x00
  0x18
  0x02
  0x10
  0x01
  0x10
  0x01
  0x10
  0x02
  0x08
  0xF8
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xEA
  0x00
  0x00
  0xF8
  0x07
  0x00
  0x18
  0x01
  0x10
  0x02
  0x10
  0x00
  0x10
  0x00
  0x08
  0xF8
  0x07
  0x00
  0x00
  0x00
  0x00  ' 0xEB
  0x20
  0x80
  0xC0
  0x80
  0x00
  0x83
  0x00
  0x46
  0x04
  0x38
  0x02
  0x18
  0x00
  0x06
  0x00
  0x01
  0xC0
  0x00
  0x20
  0x00  ' 0xEC
  0x08
  0x00
  0x10
  0x00
  0x60
  0x00
  0x80
  0x00
  0x02
  0x1F
  0x81
  0x00
  0x40
  0x00
  0x20
  0x00
  0x10
  0x00
  0x08
  0x00  ' 0xED
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00  ' 0xEE
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x02
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xEF
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00  ' 0xF0
  0x00
  0x00
  0x80
  0x10
  0x80
  0x10
  0x80
  0x10
  0x80
  0x10
  0xE0
  0x13
  0x80
  0x10
  0x80
  0x10
  0x80
  0x10
  0x00
  0x00  ' 0xF1
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0
  0x00
  0xA0  ' 0xF2
  0x42
  0x10
  0x4A
  0x08
  0x5A
  0x04
  0x36
  0x03
  0x80
  0x00
  0x40
  0x06
  0x30
  0x05
  0x88
  0x04
  0x84
  0x1F
  0x02
  0x04  ' 0xF3
  0x00
  0x00
  0x1C
  0x00
  0x3E
  0x00
  0x7E
  0x00
  0xFE
  0x7F
  0x02
  0x00
  0x02
  0x00
  0xFE
  0x7F
  0x00
  0x00
  0x00
  0x00  ' 0xF4
  0x00
  0x00
  0x00
  0x00
  0xDC
  0x61
  0x32
  0x43
  0x22
  0x42
  0x62
  0x46
  0x42
  0x4C
  0x82
  0x3B
  0x00
  0x00
  0x00
  0x00  ' 0xF5
  0x00
  0x00
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x60
  0x1A
  0x60
  0x1A
  0x00
  0x02
  0x00
  0x02
  0x00
  0x02
  0x00
  0x00  ' 0xF6
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x80
  0x00
  0xA0
  0x00
  0xC0
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xF7
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x0C
  0x00
  0x12
  0x00
  0x12
  0x00
  0x0C
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xF8
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x01
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xF9
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x03
  0x00
  0x03
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xFA
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x02
  0x00
  0x7E
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xFB
  0x00
  0x00
  0x00
  0x00
  0x42
  0x00
  0x4A
  0x00
  0x4A
  0x00
  0x4A
  0x00
  0x36
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xFC
  0x00
  0x00
  0x00
  0x00
  0x42
  0x00
  0x62
  0x00
  0x52
  0x00
  0x52
  0x00
  0x4C
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xFD
  0x00
  0x00
  0xE0
  0x1F
  0xE0
  0x1F
  0xE0
  0x1F
  0xE0
  0x1F
  0xE0
  0x1F
  0xE0
  0x1F
  0xE0
  0x1F
  0xE0
  0x1F
  0x00
  0x00  ' 0xFE
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00
  0x00  ' 0xFF
End Table

Table BigFont32_41
  0  '32
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0  '33
  0
  248
  252
  252
  252
  248
  0
  0
  0
  0
  0
  0
  0
  0
  115
  115
  115
  0
  0
  0
  0
  0
  0
  0  '34
  0
  30
  62
  62
  0
  0
  0
  62
  62
  30
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  48  '35
  48
  254
  254
  48
  48
  48
  48
  254
  254
  48
  48
  12
  12
  127
  127
  12
  12
  12
  12
  127
  127
  12
  12
  0  '36
  240
  248
  152
  254
  152
  152
  254
  152
  152
  24
  0
  0
  24
  25
  25
  127
  25
  25
  127
  25
  31
  15
  0
  0  '37
  0
  56
  56
  56
  128
  192
  224
  112
  56
  0
  0
  0
  0
  28
  14
  7
  3
  1
  28
  28
  28
  0
  0
  0  '38
  56
  252
  196
  196
  252
  56
  0
  0
  0
  128
  0
  0
  30
  63
  33
  33
  51
  63
  30
  30
  55
  35
  0
  0  '39
  0
  32
  60
  60
  28
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0  '40
  0
  192
  224
  240
  56
  28
  12
  4
  4
  0
  0
  0
  0
  3
  7
  15
  28
  56
  48
  32
  32
  0
  0
  0  '41
  0
  4
  4
  12
  28
  56
  240
  224
  192
  0
  0
  0
  0
  32
  32
  48
  56
  28
  15
  7
  3
  0
  0
  end Table

Table BigFont42_51
  128  '42
  136
  144
  224
  224
  252
  252
  224
  224
  144
  136
  128
  1
  17
  9
  7
  7
  63
  63
  7
  7
  9
  17
  1
  0  '43
  0
  128
  128
  128
  240
  240
  128
  128
  128
  0
  0
  0
  0
  1
  1
  1
  15
  15
  1
  1
  1
  0
  0
  0  '44
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  64
  120
  120
  56
  0
  0
  0
  0
  0
  0
  0  '45
  128
  128
  128
  128
  128
  128
  128
  128
  128
  128
  0
  0
  1
  1
  1
  1
  1
  1
  1
  1
  1
  1
  0
  0  '46
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  56
  56
  56
  0
  0
  0
  0
  0
  0
  0  '47
  0
  0
  0
  0
  0
  0
  128
  192
  224
  112
  56
  0
  32
  48
  56
  28
  14
  7
  3
  1
  0
  0
  0
  0  '48
  248
  252
  252
  4
  4
  4
  4
  252
  252
  248
  0
  0
  31
  63
  63
  32
  32
  32
  32
  63
  63
  31
  0
  0  '49
  96
  96
  96
  240
  252
  252
  0
  0
  0
  0
  0
  0
  32
  32
  32
  63
  63
  63
  32
  32
  32
  0
  0
  0  '50
  24
  28
  28
  4
  4
  132
  204
  252
  120
  48
  0
  0
  48
  56
  60
  46
  39
  35
  33
  56
  56
  56
  0
  0  '51
  24
  28
  28
  132
  132
  132
  204
  124
  120
  48
  0
  0
  24
  56
  56
  36 '33
  33
  33
  51
  62
  30
  12
  0
  End Table

Table BigFont52_61
  0  ' 4
  128
  192
  96
  48
  24
  252
  252
  252
  0
  0
  0
  0
  3
  3
  3
  35
  35
  63
  63
  63
  35
  35
  0
  0  ' 5
  252
  252
  252
  132
  132
  132
  132
  132
  4
  4
  0
  0
  25
  57
  57
  36 '33
  33
  33
  51
  63
  31
  14
  0
  0  ' 6
  224
  240
  248
  156
  140
  132
  132
  132
  128
  0
  0
  0
  31
  63
  63
  36 '33
  36 '33
  33
  33
  63
  63
  31
  0
  0  ' 7
  60
  60
  60
  4
  4
  4
  4
  132
  252
  252
  124
  0
  0
  0
  0
  56
  60
  62
  7
  3
  1
  0
  0
  0  ' 8
  120
  124
  252
  196
  196
  132
  132
  252
  124
  120
  0
  0
  30
  62
  63
  33
  33
  35
  35
  63
  62
  30
  0
  0  ' 9
  248
  252
  252
  132
  132
  132
  132
  252
  252
  248
  0
  0
  0
  1
  36 '33
  33
  33
  49
  57
  31
  15
  7
  0
  0  ' :
  0
  0
  0
  112
  112
  112
  0
  0
  0
  0
  0
  0
  0
  0
  0
  14
  14
  14
  0
  0
  0
  0
  0
  0  ' ;
  0
  0
  0
  112
  112
  112
  0
  0
  0
  0
  0
  0
  0
  0
  16
  30
  30
  14
  0
  0
  0
  0
  0
  0  ' <
  128
  192
  224
  112
  56
  28
  14
  6
  2
  0
  0
  0
  1
  3
  7
  14
  28
  56
  112
  96
  64
  0
  0
  96  ' =
  96
  96
  96
  96
  96
  96
  96
  96
  96
  96
  96
  6
  6
  6
  6
  6
  6
  6
  6
  6
  6
  6
  6
  End Table

Table BigFont62_71
  0  ' >
  2
  6
  14
  28
  56
  112
  224
  192
  128
  0
  0
  0
  64
  96
  112
  56
  28
  14
  7
  3
  1
  0
  0
  0  ' ?
  24
  28
  12
  14
  6
  134
  206
  252
  124
  56
  0
  0
  0
  0
  0
  0
  115
  115
  115
  0
  0
  0
  0
  0  ' @
  252
  254
  254
  2
  2
  194
  194
  194
  254
  254
  252
  0
  63
  63
  127
  96
  96
  99
  99
  99
  99
  67
  3
  0  ' A
  224
  240
  248
  28
  12
  12
  28
  248
  240
  224
  0
  0
  63
  63
  63
  2
  2
  2
  2
  63
  63
  63
  0
  0  ' B
  4
  252
  252
  252
  132
  132
  132
  252
  252
  120
  0
  0
  32
  63
  63
  63
  36 '33
  33
  33
  63
  63
  30
  0
  0  ' C
  240
  248
  252
  12
  4
  4
  4
  28
  28
  24
  0
  0
  15
  31
  63
  48
  32
  32
  32
  56
  56
  24
  0
  0  ' D
  4
  252
  252
  252
  4
  4
  12
  252
  248
  240
  0
  0
  32
  63
  63
  63
  32
  32
  48
  63
  31
  15
  0
  0  ' E
  4
  252
  252
  252
  132
  132
  132
  196
  204
  28
  0
  0
  32
  63
  63
  63
  36 '33
  33
  33
  35
  51
  56
  0
  0  ' F
  4
  252
  252
  252
  132
  132
  132
  196
  204
  28
  0
  0
  32
  63
  63
  63
  33
  1
  1
  3
  3
  0
  0
  0  ' G
  240
  248
  252
  12
  4
  4
  4
  60
  60
  56
  0
  0
  15
  31
  63
  48
  32
  34
  34
  62
  62
  62
  0
  end Table

Table BigFont72_81
  0  'H
  252
  252
  252
  128
  128
  128
  252
  252
  252
  0
  0
  0
  63
  63
  63
  1
  1
  1
  63
  63
  63
  0
  0
  0  ' I
  0
  4
  4
  252
  252
  252
  4
  4
  0
  0
  0
  0
  0
  32
  32
  63
  63
  63
  32
  32
  0
  0
  0
  0  ' J
  0
  0
  0
  0
  4
  4
  252
  252
  252
  4
  4
  30
  30
  62
  32
  32
  32
  32
  63
  63
  31
  0
  0
  0  'K
  4
  252
  252
  252
  192
  224
  112
  60
  28
  12
  0
  0
  32
  63
  63
  63
  3
  7
  14
  60
  56
  48
  0
  0  ' L
  4
  252
  252
  252
  4
  0
  0
  0
  0
  0
  0
  0
  32
  63
  63
  63
  32
  32
  32
  48
  56
  60
  0
  0  'M
  252
  252
  252
  120
  240
  224
  240
  120
  252
  252
  252
  0
  63
  63
  63
  0
  0
  1
  0
  0
  63
  63
  63
  0  'N
  252
  252
  252
  112
  224
  192
  128
  0
  252
  252
  252
  0
  63
  63
  63
  0
  0
  1
  3
  7
  63
  63
  63
  0  'O
  224
  240
  248
  28
  12
  12
  12
  28
  248
  240
  224
  0
  7
  15
  31
  56
  48
  48
  48
  56
  31
  15
  7
  0  'P
  4
  252
  252
  252
  132
  132
  132
  252
  252
  120
  0
  0
  32
  63
  63
  63
  33
  1
  1
  1
  1
  0
  0
  0  'Q
  224
  248
  248
  28
  12
  4
  12
  28
  248
  248
  224
  0
  7
  31
  31
  24
  24
  92
  94
  126
  127
  127
  71
  end Table

Table BigFont82_91
  0  'R
  4
  252
  252
  252
  132
  132
  132
  252
  252
  120
  0
  0
  32
  63
  63
  63
  1
  1
  3
  63
  63
  60
  0
  0  'S
  120
  252
  252
  132
  132
  132
  132
  188
  60
  56
  0
  0
  28
  60
  61
  36 '33
  36 '33
  33
  33
  63
  63
  30
  0
  0  'T
  28
  12
  4
  4
  252
  252
  252
  4
  4
  12
  28
  0
  0
  0
  32
  32
  63
  63
  63
  32
  32
  0
  0
  0  'U
  252
  252
  252
  0
  0
  0
  252
  252
  252
  0
  0
  0
  31
  63
  63
  32
  32
  32
  63
  63
  31
  0
  0
  0  'V
  252
  252
  252
  0
  0
  0
  252
  252
  252
  0
  0
  0
  7
  15
  31
  56
  48
  56
  31
  15
  7
  0
  0
  0  'W
  252
  252
  252
  0
  0
  128
  0
  0
  252
  252
  252
  0
  3
  15
  63
  60
  60
  15
  60
  60
  63
  15
  3
  0  'X
  28
  60
  124
  224
  192
  224
  124
  60
  28
  0
  0
  0
  56
  60
  62
  7
  3
  7
  62
  60
  56
  0
  0
  0  'Y
  124
  252
  252
  128
  0
  128
  252
  252
  124
  0
  0
  0
  0
  32
  33
  63
  63
  63
  33
  32
  0
  0
  0
  0 'Z
  60
  28
  12
  4
  132
  196
  228
  124
  60
  28
  0
  0
  56
  60
  62
  39
  35
  33
  32
  48
  56
  60
  0
  0  '[
  0
  0
  252
  252
  252
  4
  4
  4
  4
  0
  0
  0
  0
  0
  63
  63
  63
  32
  32
  32
  32
  0
  0
  end Table

Table BigFont92_101
  0  '/
  28
  56
  112
  224
  192
  128
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  1
  3
  7
  14
  28
  24
  48
  0  ']
  0
  0
  4
  4
  4
  4
  252
  252
  252
  0
  0
  0
  0
  0
  32
  32
  32
  32
  63
  63
  63
  0
  0
  0  '^
  32
  48
  56
  28
  14
  14
  28
  56
  48
  32
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0  '_
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  192
  192
  192
  192
  192
  192
  192
  192
  192
  192
  192
  192
  0  '`
  12
  12
  60
  48
  48
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0  'a
  0
  64
  64
  64
  64
  64
  192
  192
  128
  0
  0
  0
  28
  62
  62
  34
  34
  34
  63
  31
  63
  32
  0
  0  'b
  4
  252
  252
  252
  64
  64
  64
  192
  192
  128
  0
  0
  32
  63
  31
  63
  32
  32
  32
  63
  63
  31
  0
  0  'c
  128
  192
  192
  64
  64
  64
  192
  192
  128
  0
  0
  0
  31
  63
  63
  32
  32
  32
  57
  57
  25
  0
  0
  0  'd
  128
  192
  192
  64
  64
  68
  252
  252
  252
  4
  0
  0
  31
  63
  63
  32
  32
  32
  63
  31
  63
  32
  0
  0  'e
  128
  192
  192
  64
  64
  64
  192
  192
  128
  0
  0
  0
  31
  63
  63
  34
  34
  34
  59
  59
  27
  0
  0
  End Table

Table BigFont102_111
  0  'f
  128
  128
  248
  252
  252
  132
  156
  156
  24
  0
  0
  0
  33
  33
  63
  63
  63
  33
  33
  1
  0
  0
  0
  0  'g
  128
  192
  192
  64
  64
  64
  192
  128
  192
  64
  0
  0
  71
  207
  223
  152
  152
  152
  255
  255
  127
  0
  0
  0  'h
  4
  252
  252
  252
  128
  64
  64
  192
  192
  128
  0
  0
  32
  63
  63
  63
  1
  0
  0
  63
  63
  63
  0
  0  'i
  0
  64
  64
  64
  220
  220
  220
  0
  0
  0
  0
  0
  0
  32
  32
  32
  63
  63
  63
  32
  32
  32
  0
  0  'j
  0
  0
  0
  64
  64
  64
  220
  220
  220
  0
  0
  0
  32
  96
  224
  128
  128
  192
  255
  255
  127
  0
  0
  0  'k
  4
  252
  252
  252
  0
  0
  128
  192
  192
  64
  0
  0
  32
  63
  63
  63
  2
  7
  15
  61
  56
  48
  0
  0  'l
  0
  4
  4
  4
  252
  252
  252
  0
  0
  0
  0
  0
  0
  32
  32
  32
  63
  63
  63
  32
  32
  32
  0
  0  'm
  192
  192
  192
  64
  64
  192
  64
  64
  192
  192
  128
  0
  63
  63
  63
  0
  0
  63
  0
  0
  63
  63
  63
  0  'n
  192
  192
  192
  64
  64
  64
  192
  192
  128
  0
  0
  0
  63
  63
  63
  0
  0
  0
  63
  63
  63
  0
  0
  0  'o
  128
  192
  192
  64
  64
  64
  192
  192
  128
  0
  0
  0
  31
  63
  63
  32
  32
  32
  63
  63
  31
  0
  0
  end Table

Table BigFont112_121
  0  'p
  64
  192
  128
  192
  64
  64
  64
  192
  192
  128
  0
  0
  128
  255
  255
  255
  144
  16
  16
  31
  31
  15
  0
  128  'q
  192
  192
  64
  64
  64
  192
  128
  192
  64
  0
  0
  15
  31
  31
  16
  16
  144
  255
  255
  255
  128
  0
  0
  0  'r
  64
  192
  192
  192
  128
  192
  192
  192
  192
  128
  0
  0
  32
  63
  63
  63
  33
  0
  0
  1
  1
  1
  0
  0  's
  128
  192
  192
  64
  64
  64
  64
  192
  128
  0
  0
  0
  25
  59
  35
  38
  38
  38
  60
  61
  25
  0
  0
  0  't
  64
  64
  224
  240
  248
  64
  64
  64
  64
  0
  0
  0
  0
  0
  31
  63
  63
  32
  56
  56
  24
  0
  0
  0  'u
  192
  192
  192
  0
  0
  0
  192
  192
  192
  0
  0
  0
  31
  63
  63
  32
  32
  32
  63
  31
  63
  32
  0
  0  'v
  192
  192
  192
  0
  0
  0
  192
  192
  192
  0
  0
  0
  7
  15
  31
  56
  48
  56
  31
  15
  7
  0
  0
  0  'w
  192
  192
  192
  0
  0
  0
  0
  0
  192
  192
  192
  0
  7
  15
  63
  56
  56
  14
  56
  56
  63
  15
  7
  0  'x
  192
  192
  192
  0
  0
  192
  192
  192
  0
  0
  0
  0
  48
  57
  63
  15
  15
  63
  57
  48
  0
  0
  0
  0  'y
  0
  192
  192
  192
  0
  0
  0
  192
  192
  192
  0
  0
  128
  135
  143
  159
  216
  248
  120
  63
  15
  7
  0
  End Table

Table BigFont122_126
  0  'z
  192
  192
  64
  64
  64
  192
  192
  192
  0
  0
  0
  0
  49
  56
  60
  46
  39
  35
  49
  56
  0
  0
  0
  0  '{
  128
  128
  192
  120
  124
  60
  4
  4
  4
  4
  0
  0
  1
  1
  3
  30
  62
  60
  32
  32
  32
  32
  0
  0  '|
  0
  0
  0
  0
  254
  254
  254
  0
  0
  0
  0
  0
  0
  0
  0
  0
  127
  127
  127
  0
  0
  0
  0
  0  '}
  4
  4
  4
  4
  60
  124
  120
  192
  128
  128
  0
  0
  32
  32
  32
  32
  60
  62
  30
  3
  1
  1
  0
  56  '~
  60
  60
  4
  12
  28
  56
  48
  32
  60
  60
  28
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  end Table

Table BigFont193_202
  0   '�
  224
  240
  248
  28
  12
  12
  28
  248
  240
  224
  0
  0
  63
  63
  63
  2
  2
  2
  2
  63
  63
  63
  0
  0  '�
  4
  252
  252
  252
  132
  132
  132
  252
  252
  120
  0
  0
  32
  63
  63
  63
  36  '33
  36  '33
  36  '33
  63
  63
  30
  0
  0  '�
  0
  252
  252
  252
  12
  12
  12
  12
  12
  0
  0
  0
  0
  63
  63
  63
  0
  0
  0
  0
  0
  0
  0
  0  '�
  224
  240
  248
  28
  12
  12
  28
  248
  240
  224
  0
  0
  63
  63
  63
  48
  48
  48
  48
  63
  63
  63
  0
  0  '�
  4
  252
  252
  252
  132
  132
  132
  196
  204
  28
  0
  0
  32
  63
  63
  63
  36  '33
  36  '33
  36  '33
  35
  51
  56
  0
  0  '�
  60
  28
  12
  4
  132
  196
  228
  124
  60
  28
  0
  0
  56
  60
  62
  39
  35
  36  '33
  32
  48
  56
  60
  0
  0  '�
  252
  252
  252
  128
  128
  128
  252
  252
  252
  0
  0
  0
  63
  63
  63
  1
  1
  1
  63
  63
  63
  0
  0
  0  '�
  224
  240
  248
  156
  140
  140
  140
  156
  248
  240
  224
  0
  7
  15
  31
  57
  49
  49
  49
  57
  31
  15
  7
  0  '�
  0
  4
  4
  252
  252
  252
  4
  4
  0
  0
  0
  0
  0
  32
  32
  63
  63
  63
  32
  32
  0
  0
  0
  0  '�
  4
  252
  252
  252
  192
  224
  112
  60
  28
  12
  0
  0
  32
  63
  63
  63
  3
  7
  14
  60
  56
  48
  0
  end Table

Table BigFont203_212
  0  '�
  224
  240
  248
  28
  12
  12
  28
  248
  240
  224
  0
  0
  63
  63
  63
  0
  0
  0
  0
  63
  63
  63
  0
  0  '�
  252
  252
  252
  120
  240
  224
  240
  120
  252
  252
  252
  0
  63
  63
  63
  0
  0
  1
  0
  0
  63
  63
  63
  0  '�
  252
  252
  252
  112
  224
  192
  128
  0
  252
  252
  252
  0
  63
  63
  63
  0
  0
  1
  3
  7
  63
  63
  63
  0  '�
  0
  28
  28
  156
  156
  156
  156
  156
  28
  28
  0
  0
  0
  56
  56
  57
  57
  57
  57
  57
  56
  56
  0
  0  '�
  224
  240
  248
  28
  12
  12
  12
  28
  248
  240
  224
  0
  7
  15
  31
  56
  48
  48
  48
  56
  31
  15
  7
  0   '�
  252
  252
  252
  28
  28
  28
  28
  252
  252
  252
  0
  0
  63
  63
  63
  0
  0
  0
  0
  63
  63
  63
  0
  0   '�
  4
  252
  252
  252
  132
  132
  132
  252
  252
  120
  0
  0
  32
  63
  63
  63
  36  '33
  1
  1
  1
  1
  0
  0
  0   '�
  0
  60
  124
  252
  220
  156
  156
  28
  28
  28
  0
  0
  0
  60
  62
  63
  59
  57
  57
  56
  56
  56
  0
  0   '�
  28
  12
  4
  4
  252
  252
  252
  4
  4
  12
  28
  0
  0
  0
  32
  32
  63
  63
  63
  32
  32
  0
  0
  0   '�
  124
  252
  252
  128
  0
  128
  252
  252
  124
  0
  0
  0
  0
  32
  36   '33
  63
  63
  63
  36   '33
  32
  0
  0
  0
  end Table

Table BigFont213_216
  224  '�
  240
  248
  56
  24
  252
  252
  24
  56
  248
  240
  224
  7
  15
  31
  28
  24
  63
  63
  24
  28
  31
  15
  7
  0   '�
  28
  60
  124
  224
  192
  224
  124
  60
  28
  0
  0
  0
  56
  60
  62
  7
  3
  7
  62
  60
  56
  0
  0
  0   '�
  252
  252
  252
  128
  248
  248
  248
  128
  252
  252
  252
  0
  0
  1
  3
  35
  63
  63
  63
  35
  3
  1
  0
  0   '�
  0
  112
  252
  252
  140
  140
  140
  252
  252
  112
  0
  0
  0
  0
  49
  51
  51
  51
  51
  51
  49
  0
  0
  end Table
