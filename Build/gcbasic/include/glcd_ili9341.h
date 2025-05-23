'    Graphical LCD routines for the GCBASIC compiler
  '    Copyright (C) 2015-2025 Paolo Iocco, Stan Cartwright, Evan Venn and Uwe Seifert

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
  ' Supports ILI9341 controller only.

'Changes
  ' 04/10/2015:      Revised to add ReadID.           Evan R Venn
  ' 14/07/2016:      Revised to resolve Linux build.  Paolo Iocco edited by Evan R Venn
  ' 08/11/2016:      Revised to resolve 18f init issues by Evan R Venn
  ' 27/03/2017:      Revised to fix initialisation issue from PIC when using Priority Startup
  ' 30/5/2017:       Revised to remove ILI9341_DI GLCD_DI. SDO (MISO) is not used.
  ' 17/07/2017:      Reverted ILI9341_DI GLCD_DI
  ' 30/09/2017:      Added support for OLED Fonts
  ' 11/11/2017       Added support for ReadPixel_ILI9341
  ' 16/11/2017       Revised to support faster CLS for AVR
  ' 15/01/2019       Revised to support SPI without WCOL in CLS
  ' 10/02/2019:      Revised to add constant and script to resolve 64mhz at MasterFast.  #define HWSPIMode masterfast where #define HWSPIMode is masterslow|master|masterfast
  ' 03/04/2019:      Revised to support DEFAULT_GLCDBACKGROUND constant
  ' 27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn
  ' 11/10/19  Corrected Dim GLCDPixelCount As Long in FilledBox method, was a Word.  A word can overflow.
  ' 03/11/20  Added support for HWSPI_Fast_Write_Word_Macro where data needs to passed in the HWSPI_Send_word word variable
  ' 13/02/25  Test with revised FastHWSPITransfer in GLCDCLS
  ' 22/03/25  Add GLCD_DATA_PORT support for 8 bit bus, and, add ReadPixel_ILI9341 to support 8bit bus mode, adapted SendWord_ILI9341 to support 8bit bus mode.
  ' 26/03/25  Changed order of signals in write routines @ ILI9341_8BIT_MODE_SCRIPT=2 (data is valid at rising edge, not at falling edge). (Uwe Seifert) and ( Code merged by Evan Venn)
  ' 26/03/25  Cleaned up the code a bit (removed leftovers and commented out parts).
  ' 28/03/25  Add 8bit support to ReadPixel_ILI9341 - return 24bit color.

  ' 29/03/25  Change GLCDCLS IFDEF BIT(WCOL) to Var(SSPBUF) or Var(SPI1CON0) to ensure HW SPI routines are called when they can be used
  ' 31/03/25  Changed RGB888toRGB565 to prevent overflow.
  ' 31/03/25  - Added 2.4 times faster but b/w only "SUB GLCDCLS_FastBW_ILI9341()". (Uwe Seifert)
  '             Due to compatibility only for new "full 8bit data port" (GLCD_DATA_PORT is defined, ILI9341_8BIT_MODE_SCRIPT=2).
  '             Only available if ILI9341_8BIT_CLSFASTBW_ENABLE is defined.
  '           - Adapted Sub InitGLCD_ILI9341 to use this new CLS (due to compatibility only for new "full 8bit data port"). (Uwe Seifert)
  '           - Moved "display on" down after CLS for "full 8bit data port".
  '           - Revised Function ReadPixel_ILI9341() to work for parallel modes. (Uwe Seifert)
  '               Only for hardware version with 74xx245 level shifter whose DIR pin ist connected to RD! Due to lack of other tested only with this special hardware.
  ' 07/02/25 Changed ReadPixel_ILI9341 to support 8bit UNO Shield operations.
  


#ignore
#script

    'examine what is operational SPI or 8Bit
    'we could ask the user to define a constant  but we can do it automatically

    ILI9341_SPI_MODE_SCRIPT = 0
    ILI9341_8BIT_MODE_SCRIPT = 0
    'DC is used to prove SPI
    ILI9341_GLCD_DC_DEFINED = 0
    'RD is used to prove 8bit
    ILI9341_GLCD_RD_DEFINED = 0

    if GLCD_DC then
        ILI9341_SPI_MODE_SCRIPT = 1
    end if

    if GLCD_RD then
        ILI9341_8BIT_MODE_SCRIPT = 1
    end if

    if GLCD_DATA_PORT then
      ILI9341_8BIT_MODE_SCRIPT = 2
      ILI9341_GLCD_DATA_PORT = GLCD_DATA_PORT
    end if

#endscript

#samevar SSPCON1 SSPCON

' Hardware settings
  ' Type
  '''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ILI9341"

  'Serial lines (ILI9341 only)
  '''@hardware GLCD_TYPE GLCD_TYPE_ILI9341; Data/Command; GLCD_DC; IO_Pin
  '''@hardware GLCD_TYPE GLCD_TYPE_ILI9341; Chip Select; GLCD_CS; IO_Pin
  ''@hardware GLCD_TYPE GLCD_TYPE_ILI9341; Data In (LCD Out) GLCD_DI; IO_Pin
  '''@hardware GLCD_TYPE GLCD_TYPE_ILI9341; Data Out (LCD In) GLCD_DO; IO_Pin
  '''@hardware GLCD_TYPE GLCD_TYPE_ILI9341; Clock; GLCD_SCK; IO_Pin

  '''@hardware GLCD_TYPE GLCD_TYPE_ILI9341; Reset; GLCD_RESET; IO_Pin

'Pin mappings for ILI9341
  #define ILI9341_DC GLCD_DC
  #define ILI9341_CS GLCD_CS
  #define ILI9341_RST GLCD_RESET

  #define ILI9341_DI GLCD_DI ;DI IS NOT USED within the library - presence for completeness
  #define ILI9341_DO GLCD_DO
  #define ILI9341_SCK GLCD_SCK


  'User Hardware settings
  'Type
  '''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ILI9341"
  '    #define GLCD_TYPE GLCD_TYPE_ILI9341
  '
  '    'Pin mappings for device
  '    #define GLCD_RD       ANALOG_0          ' read command line
  '    #define GLCD_WR       ANALOG_1          ' write command line
  '    #define GLCD_RS       ANALOG_2          ' Command/Data line
  '    #define GLCD_CS       ANALOG_3          ' Chip select line
  '    #define GLCD_RESET      ANALOG_4          ' Reset line
  '
  '
  '    8bit bus lines
  '    #define GLCD_DB0       DIGITAL_8
  '    #define GLCD_DB1       DIGITAL_9
  '    #define GLCD_DB2       DIGITAL_2
  '    #define GLCD_DB3       DIGITAL_3
  '    #define GLCD_DB4       DIGITAL_4
  '    #define GLCD_DB5       DIGITAL_5
  '    #define GLCD_DB6       DIGITAL_6

' Map the 8-bit ports of the ILI9341 specific ports
      #define ILI9341_RST GLCD_RESET
      #define ILI9341_CS GLCD_CS
      #define ILI9341_RS GLCD_RS
      #define ILI9341_WR GLCD_WR
      #define ILI9341_RD GLCD_RD


      #define ILI9341_DB0 GLCD_DB0
      #define ILI9341_DB1 GLCD_DB1
      #define ILI9341_DB2 GLCD_DB2
      #define ILI9341_DB3 GLCD_DB3
      #define ILI9341_DB4 GLCD_DB4
      #define ILI9341_DB5 GLCD_DB5
      #define ILI9341_DB6 GLCD_DB6
      #define ILI9341_DB7 GLCD_DB7

  #define ILI9341_NOP     0x00
  #define ILI9341_SWRESET 0x01
  #define ILI9341_RDDID   0x04
  #define ILI9341_RDDST   0x09

  #define ILI9341_SLPIN   0x10
  #define ILI9341_SLPOUT  0x11
  #define ILI9341_PTLON   0x12
  #define ILI9341_NORON   0x13

  #define ILI9341_RDMODE  0x0A
  #define ILI9341_RDMADCTL  0x0B
  #define ILI9341_RDPIXFMT  0x0C
  #define ILI9341_RDIMGFMT  0x0D
  #define ILI9341_RDSELFDIAG  0x0F

  #define ILI9341_INVOFF  0x20
  #define ILI9341_INVON   0x21
  #define ILI9341_GAMMASET 0x26
  #define ILI9341_DISPOFF 0x28
  #define ILI9341_DISPON  0x29

  #define ILI9341_CASET   0x2A
  #define ILI9341_PASET   0x2B
  #define ILI9341_RAMWR   0x2C
  #define ILI9341_RAMRD   0x2E

  #define ILI9341_PTLAR   0x30
  #define ILI9341_VSCRDEF 0x33
  #define ILI9341_MADCTL  0x36

  #define ILI9341_MADCTL_MY  0x80
  #define ILI9341_MADCTL_MX  0x40
  #define ILI9341_MADCTL_MV  0x20
  #define ILI9341_MADCTL_ML  0x10
  #define ILI9341_MADCTL_RGB 0x00
  #define ILI9341_MADCTL_BGR 0x08
  #define ILI9341_MADCTL_MH  0x04

  #define ILI9341_PIXFMT  0x3A

  #define ILI9341_FRMCTR1 0xB1
  #define ILI9341_FRMCTR2 0xB2
  #define ILI9341_FRMCTR3 0xB3
  #define ILI9341_INVCTR  0xB4
  #define ILI9341_DFUNCTR 0xB6

  #define ILI9341_PWCTR1  0xC0
  #define ILI9341_PWCTR2  0xC1
  #define ILI9341_PWCTR3  0xC2
  #define ILI9341_PWCTR4  0xC3
  #define ILI9341_PWCTR5  0xC4
  #define ILI9341_VMCTR1  0xC5
  #define ILI9341_VMCTR2  0xC7

  #define ILI9341_RDID1   0xDA
  #define ILI9341_RDID2   0xDB
  #define ILI9341_RDID3   0xDC
  #define ILI9341_RDID4   0xDD

  #define ILI9341_GMCTRP1 0xE0
  #define ILI9341_GMCTRN1 0xE1

  #define ILI9341_PWCTR6  0xFC



' Color definitions
' Define Colors
  #define ILI9341_BLACK   0x0000
  #define ILI9341_RED     0xF800
  #define ILI9341_GREEN   0x07E0
  #define ILI9341_BLUE    0x001F
  #define ILI9341_WHITE   0xFFFF
  #define ILI9341_PURPLE  0xF11F
  #define ILI9341_YELLOW  0xFFE0
  #define ILI9341_CYAN    0x07FF
  #define ILI9341_D_GRAY  0x528A
  #define ILI9341_L_GRAY  0x7997
  #define ILI9341_SILVER  0xC618
  #define ILI9341_MAROON  0x8000
  #define ILI9341_OLIVE   0x8400
  #define ILI9341_LIME    0x07E0
  #define ILI9341_AQUA    0x07FF
  #define ILI9341_TEAL    0x0410
  #define ILI9341_NAVY    0x0010
  #define ILI9341_FUCHSIA 0xF81F

#startup InitGLCD_ILI9341, 98


'''Initialise the GLCD device
Sub InitGLCD_ILI9341

  #if GLCD_TYPE = GLCD_TYPE_ILI9341

    '  Mapped to global variables to same RAM
    ' dim ILI9341_GLCD_HEIGHT, ILI9341_GLCD_WIDTH as word


    'Setup code for ILI9341 controllers

    asm showdebug  SPI
    asm showdebug  ILI9341_SPI_MODE_SCRIPT
    asm showdebug  8BIT mode
    asm showdebug  ILI9341_8BIT_MODE_SCRIPT


    #ifdef ILI9341_SPI_MODE_SCRIPT 1
        'SPI mode!!!
    'Pin directions
    Dir ILI9341_CS Out
    Dir ILI9341_DC Out
    Dir ILI9341_RST Out

    #if bit(ILI9341_DI)
      Dir ILI9341_DI In
    #endif
    Dir ILI9341_DO Out
    Dir ILI9341_SCK Out

    #ifdef ILI9341_HardwareSPI
      ' harware SPI mode
      asm showdebug SPI constant used equates to HWSPIMODESCRIPT
      SPIMode HWSPIMODESCRIPT, HWSPIClockModeSCRIPT
    #endif

   Set ILI9341_CS On
   Set ILI9341_DC On
    #endif


    #ifdef ILI9341_8BIT_MODE_SCRIPT 1
    '8bit bus mode !!
      dir  ILI9341_DB7 OUT
      dir  ILI9341_DB6 OUT
      dir  ILI9341_DB5 OUT
      dir  ILI9341_DB4 OUT
      dir  ILI9341_DB3 OUT
      dir  ILI9341_DB2 OUT
      dir  ILI9341_DB1 OUT
      dir  ILI9341_DB0 OUT

      'Set pin directions
      Dir ILI9341_RD  Out
      Dir ILI9341_WR  Out
      Dir ILI9341_RS  Out
      Dir ILI9341_CS  Out
      Dir ILI9341_RST Out

      Set ILI9341_RD On
      Set ILI9341_WR On
      Set ILI9341_RS On
      set ILI9341_CS ON
      Set ILI9341_RST On
    #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 2
    'Full 8bit Data Port
      dir ILI9341_GLCD_DATA_PORT Out

      'Set pin directions
      Dir ILI9341_RD  Out
      Dir ILI9341_WR  Out
      Dir ILI9341_RS  Out
      Dir ILI9341_CS  Out
      Dir ILI9341_RST Out

      Set ILI9341_RD On
      Set ILI9341_WR On
      Set ILI9341_RS On
      set ILI9341_CS ON
      Set ILI9341_RST On
    #endif


    'Reset display
    Wait 50 ms
    Set ILI9341_RST On
    Wait 15 ms
    'Reset sequence (lower line for at least 10 us)
    Set ILI9341_RST Off
    Wait 15 us
    Set ILI9341_RST On
    Wait 15 ms

    SendCommand_ILI9341(0xEF)
    SendData_ILI9341(0x03)
    SendData_ILI9341(0x80)
    SendData_ILI9341(0x02)

    SendCommand_ILI9341(0xCF)
    SendData_ILI9341(0x00)
    SendData_ILI9341(0XC1)
    SendData_ILI9341(0X30)

    SendCommand_ILI9341(0xED)
    SendData_ILI9341(0x64)
    SendData_ILI9341(0x03)
    SendData_ILI9341(0X12)
    SendData_ILI9341(0X81)

    SendCommand_ILI9341(0xE8)
    SendData_ILI9341(0x85)
    SendData_ILI9341(0x00)
    SendData_ILI9341(0x78)

    SendCommand_ILI9341(0xCB)
    SendData_ILI9341(0x39)
    SendData_ILI9341(0x2C)
    SendData_ILI9341(0x00)
    SendData_ILI9341(0x34)
    SendData_ILI9341(0x02)

    SendCommand_ILI9341(0xF7)
    SendData_ILI9341(0x20)

    SendCommand_ILI9341(0xEA)
    SendData_ILI9341(0x00)
    SendData_ILI9341(0x00)

    SendCommand_ILI9341(ILI9341_PWCTR1)    'Power control
    SendData_ILI9341(0x2B)   'VRH[5:0]

    SendCommand_ILI9341(ILI9341_PWCTR2)    'Power control
    SendData_ILI9341(0x10)   'SAP[2:0];BT[3:0]

    SendCommand_ILI9341(ILI9341_VMCTR1)    'VCM control
    SendData_ILI9341(0x3e) '???????
    SendData_ILI9341(0x28)

    SendCommand_ILI9341(ILI9341_VMCTR2)    'VCM control2
    SendData_ILI9341(0x86)  '--

    SendCommand_ILI9341(ILI9341_MADCTL)    ' Memory Access Control
    SendData_ILI9341(ILI9341_MADCTL_MX | ILI9341_MADCTL_BGR)

    SendCommand_ILI9341(ILI9341_PIXFMT)
    SendData_ILI9341(0x55)

    SendCommand_ILI9341(ILI9341_FRMCTR1)
    SendData_ILI9341(0x00)
    SendData_ILI9341(0x18)

    SendCommand_ILI9341(ILI9341_DFUNCTR)    ' Display Function Control
    SendData_ILI9341(0x08)
    SendData_ILI9341(0x82)
    SendData_ILI9341(0x27)

    SendCommand_ILI9341(0xF2)    ' 3Gamma Function Disable
    SendData_ILI9341(0x00)

    SendCommand_ILI9341(ILI9341_GAMMASET)    'Gamma curve selected
    SendData_ILI9341(0x01)

    SendCommand_ILI9341(ILI9341_GMCTRP1)    'Set Gamma
    SendData_ILI9341(0x0F)
    SendData_ILI9341(0x31)
    SendData_ILI9341(0x2B)
    SendData_ILI9341(0x0C)
    SendData_ILI9341(0x0E)
    SendData_ILI9341(0x08)
    SendData_ILI9341(0x4E)
    SendData_ILI9341(0xF1)
    SendData_ILI9341(0x37)
    SendData_ILI9341(0x07)
    SendData_ILI9341(0x10)
    SendData_ILI9341(0x03)
    SendData_ILI9341(0x0E)
    SendData_ILI9341(0x09)
    SendData_ILI9341(0x00)

    SendCommand_ILI9341(ILI9341_GMCTRN1)    'Set Gamma
    SendData_ILI9341(0x00)
    SendData_ILI9341(0x0E)
    SendData_ILI9341(0x14)
    SendData_ILI9341(0x03)
    SendData_ILI9341(0x11)
    SendData_ILI9341(0x07)
    SendData_ILI9341(0x31)
    SendData_ILI9341(0xC1)
    SendData_ILI9341(0x48)
    SendData_ILI9341(0x08)
    SendData_ILI9341(0x0F)
    SendData_ILI9341(0x0C)
    SendData_ILI9341(0x31)
    SendData_ILI9341(0x36)
    SendData_ILI9341(0x0F)

    SendCommand_ILI9341(ILI9341_SLPOUT)    'Exit Sleep
    wait 150 ms
    SendCommand_ILI9341(ILI9341_DISPON)    'Display on

    'Default Colours
    #ifdef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
    #endif

    #ifndef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = ILI9341_BLACK
    #endif


    GLCDForeground = ILI9341_WHITE

      'Variables required for device
      ILI9341_GLCD_WIDTH = GLCD_WIDTH
      ILI9341_GLCD_HEIGHT = GLCD_HEIGHT
      #ifndef GLCD_OLED_FONT
        GLCDFontWidth = 6
      #endif

      #ifdef GLCD_OLED_FONT
        GLCDFontWidth = 5
      #endif

    GLCDfntDefault = 0
    GLCDfntDefaultsize = 1
    GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn

    GLCDRotateState = Portrait

    'Clear screen
    #IFDEF ILI9341_8BIT_MODE_SCRIPT 2  // Use faster CLS if "full 8bit data port" and faster CLS activated, else use standard CLS.
      #IFDEF ILI9341_8BIT_CLSFASTBW_ENABLE
        GLCDCLS_FastBW_ILI9341  // use faster CLS
      #ELSE
        GLCDCLS  // use standard CLS
      #ENDIF
    #ELSE
      GLCDCLS  // use standard CLS
    #ENDIF

    SendCommand_ILI9341(ILI9341_DISPON)    'Display on

  #endif

End Sub


'Read IC device code.
'The 1st parameter means the IC version.
'The 2rd and 3rd parameter mean the IC model name, should be 9341

Function ILI9341_ReadID as long

  #ifdef ILI9341_SPI_MODE_SCRIPT 1

    SendCommand_ILI9341( 0xd9 )
    'write data
    SendData_ILI9341( 0x11 )
    set ILI9341_CS OFF
    set ILI9341_DC OFF
    SPITransfer  0xd3,  ILI9341TempOut
    set ILI9341_DC ON
    SPITransfer  0x00,  SysCalcTempA
    set ILI9341_CS ON

    SendCommand_ILI9341( 0xd9 )
    'write data
    SendData_ILI9341( 0x12 )
    set ILI9341_CS OFF
    set ILI9341_DC OFF
    SPITransfer  0xd3,  ILI9341TempOut
    set ILI9341_DC ON
    SPITransfer  0x00,  SysCalcTempB
    set ILI9341_CS ON

    SendCommand_ILI9341( 0xd9 )
    'write data
    SendData_ILI9341( 0x13 )
    set ILI9341_CS OFF
    set ILI9341_DC OFF
    SPITransfer  0xd3,  ILI9341TempOut
    set ILI9341_DC ON
    SPITransfer  0x00,  SysCalcTempX
    set ILI9341_CS ON
    epwrite 2, ILI9341TempOut

    'Rebuild result to the long variable and return
    ILI9341_ReadID = SysCalcTempX
    ILI9341_ReadID_H = SysCalcTempB
    ILI9341_ReadID_U = SysCalcTempA
  #else
    !//
    //! Not supported, you need to implement.  This is a private function and therefore you need to implement for your configuration 
  #endif

End Function

Sub DSTB_ILI9341( Optional In PowerMode = On  )

    if PowerMode = Off Then

      SendCommand_ILI9341( 0xb7 )
      SendData_ILI9341(0x08)

    Else

      SendCommand_ILI9341( 0xb7 )
      SendData_ILI9341(0b10110111)

      repeat 2
              set ILI9341_CS OFF
              wait 100 us
              set ILI9341_CS ON
              wait 100 us
      End Repeat
      wait 1 ms
      repeat 4
              set ILI9341_CS OFF
              wait 100 us
              set ILI9341_CS ON
              wait 100 us
      End Repeat

      Set ILI9341_CS On
      Set ILI9341_DC On


      'Reset display
      Wait 50 ms
      Set ILI9341_RST On
      Wait 5 ms
      'Reset sequence (lower line for at least 10 us)
      Set ILI9341_RST Off
      Wait 20 us
      Set ILI9341_RST On
      Wait 150 ms

      SendCommand_ILI9341(0xEF)
      SendData_ILI9341(0x03)
      SendData_ILI9341(0x80)
      SendData_ILI9341(0x02)

      SendCommand_ILI9341(0xCF)
      SendData_ILI9341(0x00)
      SendData_ILI9341(0XC1)
      SendData_ILI9341(0X30)

      SendCommand_ILI9341(0xED)
      SendData_ILI9341(0x64)
      SendData_ILI9341(0x03)
      SendData_ILI9341(0X12)
      SendData_ILI9341(0X81)

      SendCommand_ILI9341(0xE8)
      SendData_ILI9341(0x85)
      SendData_ILI9341(0x00)
      SendData_ILI9341(0x78)

      SendCommand_ILI9341(0xCB)
      SendData_ILI9341(0x39)
      SendData_ILI9341(0x2C)
      SendData_ILI9341(0x00)
      SendData_ILI9341(0x34)
      SendData_ILI9341(0x02)

      SendCommand_ILI9341(0xF7)
      SendData_ILI9341(0x20)

      SendCommand_ILI9341(0xEA)
      SendData_ILI9341(0x00)
      SendData_ILI9341(0x00)

      SendCommand_ILI9341(ILI9341_PWCTR1)    'Power control
      SendData_ILI9341(0x23)   'VRH[5:0]

      SendCommand_ILI9341(ILI9341_PWCTR2)    'Power control
      SendData_ILI9341(0x10)   'SAP[2:0];BT[3:0]

      SendCommand_ILI9341(ILI9341_VMCTR1)    'VCM control
      SendData_ILI9341(0x3e) '???????
      SendData_ILI9341(0x28)

      SendCommand_ILI9341(ILI9341_VMCTR2)    'VCM control2
      SendData_ILI9341(0x86)  '--

      SendCommand_ILI9341(ILI9341_MADCTL)    ' Memory Access Control
      SendData_ILI9341(ILI9341_MADCTL_MX | ILI9341_MADCTL_BGR)

      SendCommand_ILI9341(ILI9341_PIXFMT)
      SendData_ILI9341(0x55)

      SendCommand_ILI9341(ILI9341_FRMCTR1)
      SendData_ILI9341(0x00)
      SendData_ILI9341(0x18)

      SendCommand_ILI9341(ILI9341_DFUNCTR)    ' Display Function Control
      SendData_ILI9341(0x08)
      SendData_ILI9341(0x82)
      SendData_ILI9341(0x27)

      SendCommand_ILI9341(0xF2)    ' 3Gamma Function Disable
      SendData_ILI9341(0x00)

      SendCommand_ILI9341(ILI9341_GAMMASET)    'Gamma curve selected
      SendData_ILI9341(0x01)

      SendCommand_ILI9341(ILI9341_GMCTRP1)    'Set Gamma
      SendData_ILI9341(0x0F)
      SendData_ILI9341(0x31)
      SendData_ILI9341(0x2B)
      SendData_ILI9341(0x0C)
      SendData_ILI9341(0x0E)
      SendData_ILI9341(0x08)
      SendData_ILI9341(0x4E)
      SendData_ILI9341(0xF1)
      SendData_ILI9341(0x37)
      SendData_ILI9341(0x07)
      SendData_ILI9341(0x10)
      SendData_ILI9341(0x03)
      SendData_ILI9341(0x0E)
      SendData_ILI9341(0x09)
      SendData_ILI9341(0x00)

      SendCommand_ILI9341(ILI9341_GMCTRN1)    'Set Gamma
      SendData_ILI9341(0x00)
      SendData_ILI9341(0x0E)
      SendData_ILI9341(0x14)
      SendData_ILI9341(0x03)
      SendData_ILI9341(0x11)
      SendData_ILI9341(0x07)
      SendData_ILI9341(0x31)
      SendData_ILI9341(0xC1)
      SendData_ILI9341(0x48)
      SendData_ILI9341(0x08)
      SendData_ILI9341(0x0F)
      SendData_ILI9341(0x0C)
      SendData_ILI9341(0x31)
      SendData_ILI9341(0x36)
      SendData_ILI9341(0x0F)

      SendCommand_ILI9341(ILI9341_SLPOUT)    'Exit Sleep
      wait 120 ms
      SendCommand_ILI9341(ILI9341_DISPON)    'Display on

    End if

End Sub

'Subs
'''Clears the GLCD screen
Sub GLCDCLS_ILI9341 ( Optional In  GLCDBACKGROUND as word = GLCDBACKGROUND )

    dim ILI9341SendWord as word
    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0

    SetAddressWindow_ILI9341 ( 0, 0, ILI9341_GLCD_WIDTH -1 , ILI9341_GLCD_HEIGHT-1 )
    ILI9341SendWord = GLCDBACKGROUND

    #IFDEF AVR
        'VARIABLE IS USES FOR FAST CLEAR SCREEN
        HWSPI_Send_Word = GLCDBACKGROUND
    #ENDIF

      ILI9341_CS=0

      #ifdef ILI9341_SPI_MODE_SCRIPT 1
        'SPI mode
      set ILI9341_DC ON
      #endif

    #if ILI9341_8BIT_MODE_SCRIPT = 1 or  ILI9341_8BIT_MODE_SCRIPT = 2
        '8bit mode or Full 8bit Data Port
        ILI9341_RS = 1
        ILI9341_RD = 1
      #endif

      'repeat 320*240 times... this is faster!
      repeat 2 ' ILI9341_GLCD_WIDTH

        repeat 38400  'ILI9341_GLCD_HEIGHT

          #ifdef ILI9341_SPI_MODE_SCRIPT 1
              'SPI handler

            #ifdef ILI9341_HardwareSPI

              #ifdef PIC

                #if Var(SSPBUF) or Var(SPI1CON0)
                    FastHWSPITransfer  ILI9341SendWord_h
                    FastHWSPITransfer  ILI9341SendWord
                #else
                    !//! Unhandled SPI condition - report to Forum for support
                #endif

              #endif


              #ifdef AVR
                'Fast clear screen - use a macro to keep code simple
                HWSPI_Fast_Write_Word_Macro GLCDBACKGROUND
              #endif

            #endif

            #ifndef ILI9341_HardwareSPI
                  SendWord_ILI9341 ( GLCDBACKGROUND )
            #endif

            'end of SPI
          #endif

          #ifdef ILI9341_8BIT_MODE_SCRIPT 1

            ILI9341_WR = 1
            PORTD = (PORTD & 0B00000011) | ((ILI9341SendWord_h) & 0B11111100)
            PORTB = (PORTB & 0B11111100) | ((ILI9341SendWord_h) & 0B00000011)
            ILI9341_WR = 0

            ILI9341_WR = 1
            PORTD = (PORTD & 0B00000011) | ((ILI9341SendWord) & 0B11111100)
            PORTB = (PORTB & 0B11111100) | ((ILI9341SendWord) & 0B00000011)
            ILI9341_WR = 0

          #endif

        #ifdef ILI9341_8BIT_MODE_SCRIPT 2
          // Full 8bit Data Port

          ILI9341_WR = 0
          ILI9341_GLCD_DATA_PORT =  ILI9341SendWord_h
          ILI9341_WR = 1

          ILI9341_WR = 0
          ILI9341_GLCD_DATA_PORT =  ILI9341SendWord
          ILI9341_WR = 1
        #endif


        end repeat

      end repeat

      ILI9341_CS = 1
    
End Sub

'''Clears the GLCD screen about 2.4 times faster than standard CLS, but only in black or white.
'''It needs more and additional memory, so you have to activate with a "#DEFINE ILI9341_8BIT_CLSFASTBW_ENABLE" in your program.
'''Works only with full 8bit data port (ILI9341_8BIT_MODE_SCRIPT=2).
'''@param GLCDCLS_FastBWColor_ILI9341 defines background color. Only 0 (black) and 255 (white) meaningful. Defaults to 0.
SUB GLCDCLS_FastBW_ILI9341(OPTIONAL IN GLCDCLS_FastBWColor_ILI9341 AS BYTE = 0)
  #IFDEF ILI9341_8BIT_MODE_SCRIPT 2
    #IFDEF ILI9341_8BIT_CLSFASTBW_ENABLE

      GLCD_yordinate=0  // Initialise global variable. Required variable for Circle in all DEVICE DRIVERS - DO NOT DELETE.
      DIM GLCDCLS_FastBWColor_ILI9341 AS BYTE

      SetAddressWindow_ILI9341 (0,0,ILI9341_GLCD_WIDTH-1,ILI9341_GLCD_HEIGHT-1)
      // Write same value to low and high byte several times.
      ILI9341_CS=0
      ILI9341_RS=1
      ILI9341_RD=1
      ILI9341_GLCD_DATA_PORT=GLCDCLS_FastBWColor_ILI9341
      // Repeat 153600 (=48*100*32=320x240*2) times. Works about 2.4 times faster than standard CLS.
      REPEAT 48
        REPEAT 100
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
          ILI9341_WR=0
          ILI9341_WR=1
        END REPEAT
      END REPEAT
      ILI9341_CS=1
    #ENDIF
  #ENDIF
END SUB


'''Draws a string at the specified location on the GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_ILI9341( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as word = GLCDForeground )

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
Sub GLCDDrawChar_ILI9341(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

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
Sub FilledBox_ILI9341(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)
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
'   SetAddress_ILI9341 ILI9341_COLUMN, LineX1, LineX2
'   SetAddress_ILI9341 ILI9341_ROW, LineY1, LineY2
    SetAddressWindow_ILI9341 (  LineX1, LineY1, LineX2, LineY2 )
    'Fill with colour
    Dim GLCDPixelCount As Long
    GLCDPixelCount = (LineX2 - LineX1 + 1) * (LineY2 - LineY1 + 1)
    Repeat GLCDPixelCount
      SendWord_ILI9341 LineColour
    End Repeat

End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
Sub PSet_ILI9341(In GLCDX as word, In GLCDY as word, In GLCDColour As Word)

    SetAddressWindow_ILI9341 ( GLCDX, GLCDY, GLCDX, GLCDY )
    SendWord_ILI9341 GLCDColour

End Sub



'''Send a command to the ILI9341 GLCD
'''@param ILI9341SendByte Command to send
'''@hide
sub  SendCommand_ILI9341( IN ILI9341SendByte as byte )

    #ifdef ILI9341_SPI_MODE_SCRIPT 1
      'SPI mode
      set ILI9341_CS OFF;
      set ILI9341_DC OFF;

      #ifdef ILI9341_HardwareSPI
         SPITransfer  ILI9341SendByte,  ILI9341TempOut
         set ILI9341_CS ON;
         exit sub
      #endif

      #ifndef ILI9341_HardwareSPI
      repeat 8

        if ILI9341SendByte.7 = ON  then
          set ILI9341_DO ON;
        else
          set ILI9341_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate ILI9341SendByte left
        set GLCD_SCK Off;

      end repeat
      set ILI9341_CS ON;
      #endif

    #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 1
        '8bit mode

        ILI9341_CS = 0
        ILI9341_RS = 0
        ILI9341_RD = 1

        ILI9341_WR = 1
        PORTD = (PORTD & 0B00000011) | ((ILI9341SendByte) & 0B11111100);
        PORTB = (PORTB & 0B11111100) | ((ILI9341SendByte) & 0B00000011);
        ILI9341_WR  = 0
        ILI9341_WR = 1
        ILI9341_CS = 1

    'endif 8bit
    #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 2
        //Full 8bit Data Port
        ILI9341_CS = 0
        ILI9341_RS = 0
        ILI9341_RD = 1

        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341SendByte
        ILI9341_WR = 1
        ILI9341_CS = 1
    #endif


end Sub

'''Send a data byte to the ILI9341 GLCD
'''@param ILI9341SendByte Byte to send
'''@hide
sub  SendData_ILI9341( IN ILI9341SendByte as byte )

    #ifdef ILI9341_SPI_MODE_SCRIPT 1
      'SPI mode
      set ILI9341_CS OFF;
      set ILI9341_DC ON;

      #ifdef ILI9341_HardwareSPI
         SPITransfer  ILI9341SendByte,  ILI9341TempOut
         set ILI9341_CS ON;
         exit sub
      #endif

      #ifndef ILI9341_HardwareSPI
      repeat 8

        if ILI9341SendByte.7 = ON then
          set ILI9341_DO ON;
        else
          set ILI9341_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate ILI9341SendByte left
        set GLCD_SCK Off;

      end Repeat
      set ILI9341_CS ON;
      #endif

    #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 1
    '8bit mode

        ILI9341_CS = 0
        ILI9341_RS = 1
        ILI9341_RD = 1

        ILI9341_WR = 1
        PORTD = (PORTD & 0B00000011) | ((ILI9341SendByte) & 0B11111100);
        PORTB = (PORTB & 0B11111100) | ((ILI9341SendByte) & 0B00000011);
        ILI9341_WR = 0
        ILI9341_WR = 1
        ILI9341_CS = 1

    'endif 8bit
    #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 2
        //Full 8bit Data Port
        ILI9341_CS = 0
        ILI9341_RS = 1
        ILI9341_RD = 1

        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341SendByte
        ILI9341_WR = 1
        ILI9341_CS = 1

    #endif

end Sub

'''Send a data word (16 bits) to the ILI9341 GLCD
'''@param ILI9341SendByte Word to send
'''@hide
Sub SendWord_ILI9341(In ILI9341SendWord As Word)

    dim ILI9341SendWord as word

    #ifdef ILI9341_SPI_MODE_SCRIPT 1
      'SPI mode
      set ILI9341_CS OFF;
      set ILI9341_DC ON;

      #ifdef ILI9341_HardwareSPI
         SPITransfer  ILI9341SendWord_H,  ILI9341TempOut
         SPITransfer  ILI9341SendWord,  ILI9341TempOut
         set ILI9341_CS ON;
         exit sub
      #endif

      #ifndef ILI9341_HardwareSPI
      repeat 16

        if ILI9341SendWord.15 = ON then
          set ILI9341_DO ON;
        else
          set ILI9341_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate ILI9341SendWord left
        set GLCD_SCK Off;

      end repeat
      set ILI9341_CS ON;
      #endif

    #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 1
        '8bit mode

        ILI9341_CS = 0
        ILI9341_RS = 1
        ILI9341_RD = 1

        ILI9341_WR = 1
        PORTD = (PORTD & 0B00000011) | ((ILI9341SendWord_h) & 0B11111100);
        PORTB = (PORTB & 0B11111100) | ((ILI9341SendWord_h) & 0B00000011);
        ILI9341_WR = 0

        ILI9341_WR = 1
        PORTD = (PORTD & 0B00000011) | ((ILI9341SendWord) & 0B11111100);
        PORTB = (PORTB & 0B11111100) | ((ILI9341SendWord) & 0B00000011);

        ILI9341_WR = 0
        ILI9341_WR = 1
        ILI9341_CS = 1

    'endif 8bit
    #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 2
        //Full 8bit Data Port
        ILI9341_CS = 0
        ILI9341_RS = 1
        ILI9341_RD = 1

        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341SendWord_h
        ILI9341_WR = 1

        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341SendWord
        ILI9341_WR = 1
        ILI9341_CS = 1

    #endif

End Sub



'''Set the row or column address range for the ILI9341 GLCD
'''@param ILI9341AddressType Address Type (ILI9341_ROW or ILI9341_COLUMN)
'''@param ILI9341Start Starting address
'''@param ILI9341End Ending address
'''@hide
Sub SetAddressWindow_ILI9341( In _x1 as word, in _y1 as word, in _x2 as word, in _y2 as word)

  SendCommand_ILI9341(ILI9341_CASET); // Column addr set
  SendData_ILI9341 _x1_H
  SendData_ILI9341 _x1
  SendData_ILI9341 _x2_H
  SendData_ILI9341 _x2

  SendCommand_ILI9341(ILI9341_PASET); // Row addr set
  SendData_ILI9341 _y1_H
  SendData_ILI9341 _y1
  SendData_ILI9341 _y2_H
  SendData_ILI9341 _y2

  SendCommand_ILI9341(ILI9341_RAMWR); // write to RAM

End Sub



Sub SetCursorPosition_ILI9341( In _x1 as word, in _y1 as word, in _x2 as word, in _y2 as word)
  SendCommand_ILI9341(ILI9341_CASET); // Column addr set
  SendData_ILI9341 _x1_H
  SendData_ILI9341 _x1
  SendData_ILI9341 _x2_H
  SendData_ILI9341 _x2

  SendCommand_ILI9341(ILI9341_PASET); // Row addr set
  SendData_ILI9341 _y1_H
  SendData_ILI9341 _y1
  SendData_ILI9341 _y2_H
  SendData_ILI9341 _y2

End Sub


'''@hide
sub   GLCDRotate_ILI9341 ( in GLCDRotateState )

  SendCommand_ILI9341 ( ILI9341_MADCTL )
  select case GLCDRotateState
        case PORTRAIT
             ILI9341_GLCD_WIDTH = GLCD_WIDTH
             ILI9341_GLCD_HEIGHT = GLCD_HEIGHT
             SendData_ILI9341( ILI9341_MADCTL_MX | ILI9341_MADCTL_BGR)
        case LANDSCAPE
             ILI9341_GLCD_WIDTH = GLCD_HEIGHT
             ILI9341_GLCD_HEIGHT = GLCD_WIDTH
             SendData_ILI9341( ILI9341_MADCTL_MV | ILI9341_MADCTL_BGR )

        case PORTRAIT_REV
             ILI9341_GLCD_WIDTH = GLCD_WIDTH
             ILI9341_GLCD_HEIGHT = GLCD_HEIGHT
             SendData_ILI9341( ILI9341_MADCTL_MY | ILI9341_MADCTL_BGR)
        case LANDSCAPE_REV
             ILI9341_GLCD_WIDTH = GLCD_HEIGHT
             ILI9341_GLCD_HEIGHT = GLCD_WIDTH
             SendData_ILI9341(  ILI9341_MADCTL_MV | ILI9341_MADCTL_MY | ILI9341_MADCTL_MX | ILI9341_MADCTL_BGR )

        case else
             ILI9341_GLCD_WIDTH = GLCD_WIDTH
             ILI9341_GLCD_HEIGHT = GLCD_HEIGHT
             SendData_ILI9341( ILI9341_MADCTL_MX | ILI9341_MADCTL_BGR )
  end select

end sub



'''Displays a string in a larger fonti.e.  BigPrint_ILI9341 ( 30, ILI9341_GLCD_HEIGHT - 20 , "BigPrint_ILI9341" )
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData String to display
'''@param Color Optional color
Sub BigPrint_ILI9341(In PrintLocX as Word, In PrintLocY as Word,  PrintData As String, Optional In  Color as word = GLCDForeground)
  Dim GLCDPrintLoc as word
  PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = 1 To PrintLen
    DrawBigChar_ILI9341 GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  Next
End Sub

'''Draws a Vertical Line on the GLCD with a
'''@param GLCDY1 Y coordinate of one end of the line
'''@param GLCDY2 Y coordinate of the other end of the line
'''@param GLCDX1 X coordinate of the line
'''@param LineColour color
Sub DrawBigChar_ILI9341 (In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  Color as word = GLCDForeground )

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
             PSet_ILI9341 LocX , LocY , Color
          end if
          Rotate CurrCharVal Right
        Next
    Next
End Sub


'returns a 24bit pixel color value... as GCB uses a 16bit color value the user will need to complete conversion
Function  ReadPixel_ILI9341(In ILI9341_GLCDX as word, In ILI9341_GLCDY as word ) as long

    DIM ILI9341TempOut as byte

    #ifdef ILI9341_SPI_MODE_SCRIPT 1

      set ILI9341_CS OFF
      set ILI9341_DC OFF
      SPITransfer  ILI9341_CASET,  ILI9341TempOut
      set ILI9341_DC ON;
      SPITransfer  ILI9341_GLCDX_h,  ILI9341TempOut
      SPITransfer  ILI9341_GLCDX,  ILI9341TempOut
      SPITransfer  ILI9341_GLCDX_h,  ILI9341TempOut
      SPITransfer  ILI9341_GLCDX,  ILI9341TempOut

      set ILI9341_DC OFF
      SPITransfer  ILI9341_PASET,  ILI9341TempOut

      set ILI9341_DC ON
      SPITransfer  ILI9341_GLCDY_h,  ILI9341TempOut
      SPITransfer  ILI9341_GLCDY,  ILI9341TempOut
      SPITransfer  ILI9341_GLCDY_h,  ILI9341TempOut
      SPITransfer  ILI9341_GLCDY,  ILI9341TempOut

      set ILI9341_DC OFF
      SPITransfer  ILI9341_RAMRD, ILI9341TempOut

      set ILI9341_DC ON
      SPITransfer  1,  ReadPixel_ILI9341_e  'dummy read - use the highest byte
      SPITransfer  2,  ReadPixel_ILI9341_u
      SPITransfer  3,  ReadPixel_ILI9341_h
      SPITransfer  4,  [byte]ReadPixel_ILI9341
      set ILI9341_CS ON
    #endif

    #if ILI9341_8BIT_MODE_SCRIPT= 1 OR ILI9341_8BIT_MODE_SCRIPT = 2



      #ifdef ILI9341_8BIT_MODE_SCRIPT 1
        // 8bit mode for UNO shield

        //~ Activate chip select (CS) to begin communication with display
        ILI9341_CS = 0

        //Send Command
        //~ Set Register Select (RS) low to indicate command mode
        ILI9341_RS = 0
        //~ Keep Read (RD) high as we're not reading
        ILI9341_RD = 1

        //~ Set Write (WR) low to begin write cycle
        ILI9341_WR = 0
        //~ Send upper 6 bits of CASET command to PORTD
        PORTD = (PORTD & 0B00000011) | ((ILI9341_CASET) & 0B11111100);
        //~ Send lower 2 bits of CASET command to PORTB
        PORTB = (PORTB & 0B11111100) | ((ILI9341_CASET) & 0B00000011);
        //~ Set Write high to complete write cycle
        ILI9341_WR  = 1

        //Send Data
        //~ Set Register Select high to indicate data mode
        ILI9341_RS = 1
        //~ Keep Read high as we're not reading
        ILI9341_RD = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send high byte of X coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDX_H) & 0B11111100);
        //~ Send high byte of X coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDX_H) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send low byte of X coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDX) & 0B11111100);
        //~ Send low byte of X coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDX) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send high byte of X end coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDX_H) & 0B11111100);
        //~ Send high byte of X end coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDX_H) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send low byte of X end coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDX) & 0B11111100);
        //~ Send low byte of X end coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDX) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //Send Command
        //~ Set back to command mode
        ILI9341_RS = 0
        //~ Keep Read high as we're not reading
        ILI9341_RD = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send PASET command (Page Address Set) (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_PASET) & 0B11111100);
        //~ Send PASET command (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_PASET) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //Send Data
        //~ Set to data mode
        ILI9341_RS = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send high byte of Y coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDY_H) & 0B11111100);
        //~ Send high byte of Y coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDY_H) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send low byte of Y coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDY) & 0B11111100);
        //~ Send low byte of Y coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDY) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send high byte of Y end coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDY_H) & 0B11111100);
        //~ Send high byte of Y end coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDY_H) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send low byte of Y end coordinate (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_GLCDY) & 0B11111100);
        //~ Send low byte of Y end coordinate (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_GLCDY) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Prepare to read pixel data

        //~ Send Read Memory Command (RAMRD)
        //Send Command
        //~ Set to command mode
        ILI9341_RS = 0
        //~ Keep Read high as we're not reading yet
        ILI9341_RD = 1

        //~ Begin write cycle
        ILI9341_WR = 0
        //~ Send RAMRD command (Read Memory) (upper 6 bits)
        PORTD = (PORTD & 0B00000011) | ((ILI9341_RAMRD) & 0B11111100);
        //~ Send RAMRD command (lower 2 bits)
        PORTB = (PORTB & 0B11111100) | ((ILI9341_RAMRD) & 0B00000011);
        //~ Complete write cycle
        ILI9341_WR  = 1

        //~ Switch to data mode for reading pixel      

        //~ Set data lines to input mode
        dir  ILI9341_DB7 IN
        dir  ILI9341_DB6 IN
        dir  ILI9341_DB5 IN
        dir  ILI9341_DB4 IN
        dir  ILI9341_DB3 IN
        dir  ILI9341_DB2 IN
        dir  ILI9341_DB1 IN
        dir  ILI9341_DB0 IN

        //~ Set to data mode for reading
        ILI9341_RS = 1
        
        //~ Dummy read (required by some ILI9341 implementations)
        //~ Clear error flag
        ReadPixel_ILI9341_e = 0
        //~ Begin read cycle
        ILI9341_RD = 0
        //~ End read cycle (dummy read)
        ILI9341_RD = 1

        //~ Begin read cycle
        ILI9341_RD = 0
        //~ End read cycle
        ILI9341_RD = 1
        //~ Read first byte of pixel data (typically the red component in RGB565)
        ReadPixel_ILI9341_u = ( PIND & 0B11111100 ) | ( PINB & 0B00000011 )

        //~ Begin read cycle
        ILI9341_RD = 0
        //~ End read cycle
        ILI9341_RD = 1
        //~ Read second byte of pixel data (typically the green component in RGB565)
        ReadPixel_ILI9341_h = ( PIND & 0B11111100 ) | ( PINB & 0B00000011 )

        //~ Begin read cycle
        ILI9341_RD = 0
        //~ End read cycle
        ILI9341_RD = 1
        //~ Read third byte of pixel data (typically the blue component in RGB565)
        [BYTE]ReadPixel_ILI9341   = ( PIND & 0B11111100 ) | ( PINB & 0B00000011 )

        //~ Deactivate chip select to end communication
        ILI9341_CS = 1

        //~ Return data pins to output mode for future operations
        dir  ILI9341_DB7 OUT
        dir  ILI9341_DB6 OUT
        dir  ILI9341_DB5 OUT
        dir  ILI9341_DB4 OUT
        dir  ILI9341_DB3 OUT
        dir  ILI9341_DB2 OUT
        dir  ILI9341_DB1 OUT
        dir  ILI9341_DB0 OUT

        // end of 8bit mode for UNO shield
      #endif

    #ifdef ILI9341_8BIT_MODE_SCRIPT 2

        //~ Begin pixel read sequence by selecting the display chip
        ILI9341_CS = 0

        //~ Set up to send Column Address Set command (CASET)
        //~ Switch to command mode
        ILI9341_RS = 0
        //~ Ensure read line is disabled
        ILI9341_RD = 1

        //~ Send Column Address Set command (0x2A)
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_CASET
        ILI9341_WR = 1

        //~ Switch to data mode for column address transmission
        ILI9341_RS = 1
        ILI9341_RD = 1

        //~ Send column start and end address (4 bytes total)
        //~ High byte of start column
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDX_h
        ILI9341_WR = 1

        //~ Low byte of start column
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDX
        ILI9341_WR = 1

        //~ High byte of end column
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDX_h
        ILI9341_WR = 1                        

        //~ Low byte of end column
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDX
        ILI9341_WR = 1

        //~ Set up to send Page Address Set command (PASET)
        //~ Switch back to command mode
        ILI9341_RS = 0
        ILI9341_RD = 1

        //~ Send Page Address Set command (0x2B)
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_PASET
        ILI9341_WR = 1

        //~ Switch to data mode for page address transmission
        ILI9341_RS = 1
        ILI9341_RD = 1

        //~ Send row start and end address (4 bytes total)
        //~ High byte of start row
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDY_h
        ILI9341_WR = 1

        //~ Low byte of start row
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDY
        ILI9341_WR = 1

        //~ High byte of end row
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDY_h
        ILI9341_WR = 1                        

        //~ Low byte of end row
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_GLCDY
        ILI9341_WR = 1

        //~ Prepare to read pixel data
        //~ Switch to command mode
        ILI9341_RS = 0
        ILI9341_RD = 1

        //~ Send Read Memory Command (RAMRD)
        ILI9341_WR = 0
        ILI9341_GLCD_DATA_PORT = ILI9341_RAMRD
        ILI9341_WR = 1

        //~ Switch to data mode for reading pixel
        ILI9341_RS = 1
        
        //~ Change data port to input mode for reading
        DIR ILI9341_GLCD_DATA_PORT In

        //~ Dummy read (required by some ILI9341 implementations)
        ReadPixel_ILI9341_e = 0
        ILI9341_RD = 0
        
        ILI9341_RD = 1

        //~ Read pixel data bytes
        //~ First byte
        ILI9341_RD = 0
        
        ILI9341_RD = 1
        ReadPixel_ILI9341_u = ILI9341_GLCD_DATA_PORT

        //~ Second byte (high byte of color)
        ILI9341_RD = 0
        
        ILI9341_RD = 1
        ReadPixel_ILI9341_h = ILI9341_GLCD_DATA_PORT

        //~ Third byte (low byte of color)
        ILI9341_RD = 0
        
        ILI9341_RD = 1
        [BYTE]ReadPixel_ILI9341 = ILI9341_GLCD_DATA_PORT

        //~ Deselect the display chip
        ILI9341_CS = 1
        
        //~ Restore data port to output mode
        Dir ILI9341_GLCD_DATA_PORT Out

      #endif

    #endif

End Function
