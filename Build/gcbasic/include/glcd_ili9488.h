'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2025 Evan Venn, Chris Savage

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
' Supports ILI9488 controller only in SPI mode. This uses 18bit colors.

'Changes
'  25/03/2023:      Initial release
'  25/03/2023:      Edited  release for testing
'  06/04/2023:      Revised with hardware tested - Only 18bit Color, SPI, PIC tested. AVR or PIC with UNO 8 bit shield not tested. 
'  14/04/2023:      Revised color scheme
'  22/04/2025:      Revised to additional SPI Support, improve performance.

'
'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE_ILI9488; "ILI9488"

'Serial lines (ILI9488 only)
'''@hardware GLCD_TYPE ILI9488; Data/Command; GLCD_DC; IO_Pin
'''@hardware GLCD_TYPE ILI9488; Chip Select; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE ILI9488; Data In (LCD Out) GLCD_DI; IO_Pin
'''@hardware GLCD_TYPE ILI9488; Data Out (LCD In) GLCD_DO; IO_Pin
'''@hardware GLCD_TYPE ILI9488; Clock; GLCD_SCK; IO_Pin

'''@hardware GLCD_TYPE ILI9488; Reset; GLCD_RST; IO_Pin

'Pin mappings for ILI9488
#define ILI9488_DC GLCD_DC
#define ILI9488_CS GLCD_CS
#define ILI9488_RST GLCD_RST

#define ILI9488_DI GLCD_DI
#define ILI9488_DO GLCD_DO
#define ILI9488_SCK GLCD_SCK




  '8 bit data bus constants
  #define ILI9488_RST GLCD_RST
  #define ILI9488_CS GLCD_CS
  #define ILI9488_DC GLCD_DC
  #define ILI9488_WR GLCD_WR
  #define ILI9488_RD GLCD_RD


  #define ILI9488_DB0 GLCD_DB0
  #define ILI9488_DB1 GLCD_DB1
  #define ILI9488_DB2 GLCD_DB2
  #define ILI9488_DB3 GLCD_DB3
  #define ILI9488_DB4 GLCD_DB4
  #define ILI9488_DB5 GLCD_DB5
  #define ILI9488_DB6 GLCD_DB6
  #define ILI9488_DB7 GLCD_DB7

  '8 bit whole port data bus directions
  'assumes GLCD_DataPort is defined also
  #define ILI9488_RST GLCD_RST
  #define ILI9488_CS GLCD_CS
  #define ILI9488_DC GLCD_DC
  #define ILI9488_WR GLCD_WR
  #define ILI9488_RD GLCD_RD


/* Level 1 Commands (from the display Datasheet) */
    #define ILI9488_CMD_NOP                             0x00
    #define ILI9488_CMD_SOFTWARE_RESET                  0x01
    #define ILI9488_CMD_READ_DISP_ID                    0x04
    #define ILI9488_CMD_READ_ERROR_DSI                  0x05
    #define ILI9488_CMD_READ_DISP_STATUS                0x09
    #define ILI9488_CMD_READ_DISP_POWER_MODE            0x0A
    #define ILI9488_CMD_READ_DISP_MADCTRL               0x0B
    #define ILI9488_CMD_READ_DISP_PIXEL_FORMAT          0x0C
    #define ILI9488_CMD_READ_DISP_IMAGE_MODE            0x0D
    #define ILI9488_CMD_READ_DISP_SIGNAL_MODE           0x0E
    #define ILI9488_CMD_READ_DISP_SELF_DIAGNOSTIC       0x0F
    #define ILI9488_CMD_ENTER_SLEEP_MODE                0x10
    #define ILI9488_CMD_SLEEP_OUT                       0x11
    #define ILI9488_CMD_PARTIAL_MODE_ON                 0x12
    #define ILI9488_CMD_NORMAL_DISP_MODE_ON             0x13
    #define ILI9488_CMD_DISP_INVERSION_OFF              0x20
    #define ILI9488_CMD_DISP_INVERSION_ON               0x21
    #define ILI9488_CMD_PIXEL_OFF                       0x22
    #define ILI9488_CMD_PIXEL_ON                        0x23
    #define ILI9488_CMD_DISPLAY_OFF                     0x28
    #define ILI9488_CMD_DISPLAY_ON                      0x29
    #define ILI9488_CMD_COLUMN_ADDRESS_SET              0x2A
    #define ILI9488_CASET                               0x2A
    #define ILI9488_CMD_PAGE_ADDRESS_SET                0x2B
    #define ILI9488_PASET                               0x2B
    #define ILI9488_CMD_MEMORY_WRITE                    0x2C
    #define ILI9488_RAMWR                               0x2C
    #define ILI9488_CMD_MEMORY_READ                     0x2E
    #define ILI9488_CMD_PARTIAL_AREA                    0x30
    #define ILI9488_CMD_VERT_SCROLL_DEFINITION          0x33
    #define ILI9488_CMD_TEARING_EFFECT_LINE_OFF         0x34
    #define ILI9488_CMD_TEARING_EFFECT_LINE_ON          0x35
    #define ILI9488_CMD_MEMORY_ACCESS_CONTROL           0x36
    #define ILI9488_CMD_VERT_SCROLL_START_ADDRESS       0x37
    #define ILI9488_CMD_IDLE_MODE_OFF                   0x38
    #define ILI9488_CMD_IDLE_MODE_ON                    0x39
    #define ILI9488_CMD_COLMOD_PIXEL_FORMAT_SET         0x3A
    #define ILI9488_CMD_WRITE_MEMORY_CONTINUE           0x3C
    #define ILI9488_CMD_READ_MEMORY_CONTINUE            0x3E
    #define ILI9488_CMD_SET_TEAR_SCANLINE               0x44
    #define ILI9488_CMD_GET_SCANLINE                    0x45
    #define ILI9488_CMD_WRITE_DISPLAY_BRIGHTNESS        0x51
    #define ILI9488_CMD_READ_DISPLAY_BRIGHTNESS         0x52
    #define ILI9488_CMD_WRITE_CTRL_DISPLAY              0x53
    #define ILI9488_CMD_READ_CTRL_DISPLAY               0x54
    #define ILI9488_CMD_WRITE_CONTENT_ADAPT_BRIGHTNESS  0x55
    #define ILI9488_CMD_READ_CONTENT_ADAPT_BRIGHTNESS   0x56
    #define ILI9488_CMD_WRITE_MIN_CAB_LEVEL             0x5E
    #define ILI9488_CMD_READ_MIN_CAB_LEVEL              0x5F
    #define ILI9488_CMD_READ_ABC_SELF_DIAG_RES          0x68
    #define ILI9488_CMD_READ_ID1                        0xDA
    #define ILI9488_CMD_READ_ID2                        0xDB
    #define ILI9488_CMD_READ_ID3                        0xDC
    /* Level 2 Commands (from the display Datasheet) */
    #define ILI9488_CMD_INTERFACE_MODE_CONTROL          0xB0
    #define ILI9488_CMD_FRAME_RATE_CONTROL_NORMAL       0xB1
    #define ILI9488_CMD_FRAME_RATE_CONTROL_IDLE_8COLOR  0xB2
    #define ILI9488_CMD_FRAME_RATE_CONTROL_PARTIAL      0xB3
    #define ILI9488_CMD_DISPLAY_INVERSION_CONTROL       0xB4
    #define ILI9488_CMD_BLANKING_PORCH_CONTROL          0xB5
    #define ILI9488_CMD_DISPLAY_FUNCTION_CONTROL        0xB6
    #define ILI9488_CMD_ENTRY_MODE_SET                  0xB7
    #define ILI9488_CMD_BACKLIGHT_CONTROL_1             0xB9
    #define ILI9488_CMD_BACKLIGHT_CONTROL_2             0xBA
    #define ILI9488_CMD_HS_LANES_CONTROL                0xBE
    #define ILI9488_CMD_POWER_CONTROL_1                 0xC0
    #define ILI9488_CMD_POWER_CONTROL_2                 0xC1
    #define ILI9488_CMD_POWER_CONTROL_NORMAL_3          0xC2
    #define ILI9488_CMD_POWER_CONTROL_IDEL_4            0xC3
    #define ILI9488_CMD_POWER_CONTROL_PARTIAL_5         0xC4
    #define ILI9488_CMD_VCOM_CONTROL_1                  0xC5
    #define ILI9488_CMD_CABC_CONTROL_1                  0xC6
    #define ILI9488_CMD_CABC_CONTROL_2                  0xC8
    #define ILI9488_CMD_CABC_CONTROL_3                  0xC9
    #define ILI9488_CMD_CABC_CONTROL_4                  0xCA
    #define ILI9488_CMD_CABC_CONTROL_5                  0xCB
    #define ILI9488_CMD_CABC_CONTROL_6                  0xCC
    #define ILI9488_CMD_CABC_CONTROL_7                  0xCD
    #define ILI9488_CMD_CABC_CONTROL_8                  0xCE
    #define ILI9488_CMD_CABC_CONTROL_9                  0xCF
    #define ILI9488_CMD_NVMEM_WRITE                     0xD0
    #define ILI9488_CMD_NVMEM_PROTECTION_KEY            0xD1
    #define ILI9488_CMD_NVMEM_STATUS_READ               0xD2
    #define ILI9488_CMD_READ_ID4                        0xD3
    #define ILI9488_CMD_ADJUST_CONTROL_1                0xD7
    #define ILI9488_CMD_READ_ID_VERSION                 0xD8
    #define ILI9488_CMD_POSITIVE_GAMMA_CORRECTION       0xE0
    #define ILI9488_CMD_NEGATIVE_GAMMA_CORRECTION       0xE1
    #define ILI9488_CMD_DIGITAL_GAMMA_CONTROL_1         0xE2
    #define ILI9488_CMD_DIGITAL_GAMMA_CONTROL_2         0xE3
    #define ILI9488_CMD_SET_IMAGE_FUNCTION              0xE9
    #define ILI9488_CMD_ADJUST_CONTROL_2                0xF2
    #define ILI9488_CMD_ADJUST_CONTROL_3                0xF7
    #define ILI9488_CMD_ADJUST_CONTROL_4                0xF8
    #define ILI9488_CMD_ADJUST_CONTROL_5                0xF9
    #define ILI9488_CMD_SPI_READ_SETTINGS               0xFB
    #define ILI9488_CMD_ADJUST_CONTROL_6                0xFC
    #define ILI9488_CMD_ADJUST_CONTROL_7                0xFF

#script
  ' For the UNO_8bit_Shield you can use GLCD_RS or GLCD_DC.  They are mapped automatically
  GLCD_DC_Defined = 0

  if GLCD_TYPE = GLCD_TYPE_ILI9488 then

    //~ See https://johndecember.com/html/spec/colorper.html
    //~ Take the color percentages, then apply these percents to base of 255 ( 0xFF ) then tranpose all three values to BRG
    //~ Using PINK as the example. 
    //~ See PINK1 on webpage
    //~ 100	71	71		Percents            RGB
    //~ 255	181	181		Percent of 255
    //~ FF	B5	B5		Hex value
    //~ 				
    //~ B5	B5	FF		Transposed value    BRG
    //~ 				
    //~ Use this script to set the TFT_colors as this script will execute late in the pre-processing process and will only change the colors for this GLCD
    //~ 				

      ILI9488_TFT_BLACK       = [LONG]0x000000
      ILI9488_TFT_RED         = [LONG]0xFC0000
      ILI9488_TFT_GREEN       = [LONG]0x00FC00 
      ILI9488_TFT_BLUE        = [LONG]0x0000FC
      ILI9488_TFT_WHITE       = [LONG]0xFFFFFF    

      ILI9488_TFT_CYAN        = [LONG]0x003F3F
      ILI9488_TFT_DARKCYAN    = [LONG]0x00AFAF
      ILI9488_TFT_DARKGREEN   = [LONG]0x002100
      ILI9488_TFT_DARKGREY    = [LONG]0xAAAAAA
      ILI9488_TFT_GREENYELLOW = [LONG]0x93FC33  
      ILI9488_TFT_LIGHTGREY   = [LONG]0xC9C9C9
      ILI9488_TFT_MAGENTA     = [LONG]0xCC00CC
      ILI9488_TFT_MAROON      = [LONG]0x7E007E
      ILI9488_TFT_NAVY        = [LONG]0x00003E
      ILI9488_TFT_OLIVE       = [LONG]0x783E00
      ILI9488_TFT_ORANGE      = [LONG]0xFC2900
      ILI9488_TFT_PINK        = [LONG]0xFC000F
      ILI9488_TFT_PURPLE      = [LONG]0xF01F9E
      ILI9488_TFT_YELLOW      = [LONG]0xFFFF00

      TFT_BLACK       = [LONG]0x000000
      TFT_RED         = [LONG]0xFC0000
      TFT_GREEN       = [LONG]0x00FC00 
      TFT_BLUE        = [LONG]0x0000FC
      TFT_WHITE       = [LONG]0xFFFFFF    

      TFT_CYAN        = [LONG]0x003F3F
      TFT_DARKCYAN    = [LONG]0x00AFAF
      TFT_DARKGREEN   = [LONG]0x002100
      TFT_DARKGREY    = [LONG]0xAAAAAA
      TFT_GREENYELLOW = [LONG]0x93FC33  
      TFT_LIGHTGREY   = [LONG]0xC9C9C9
      TFT_MAGENTA     = [LONG]0xCC00CC
      TFT_MAROON      = [LONG]0x7E007E
      TFT_NAVY        = [LONG]0x00003E
      TFT_OLIVE       = [LONG]0x783E00
      TFT_ORANGE      = [LONG]0xFC2900
      TFT_PINK        = [LONG]0xFC000F
      TFT_PURPLE      = [LONG]0xF01F9E
      TFT_YELLOW      = [LONG]0xFFFF00  


    if def(UNO_8bit_Shield) then

      'is there a GLCD_DC defined?
      if def(GLCD_DC) then
          GLCD_DC_Defined = 1
      end if

      'if no GLCD_DC then is there is an GLCD_RS
      if GLCD_DC_Defined = 0 Then
          if GLCD_RS then
            'map
            GLCD_DC = GLCD_RS
            GLCD_DC_Defined = 1
          end if
          'so, if there one defined?
          if GLCD_DC_Defined = 0 Then
              warning "No GLCD_DC defined"
          end if
      end if

    end if

    GLCD_RST_Defined = 0
    'is there a GLCD_DC defined?
    if def(GLCD_RST) then
        GLCD_RST_Defined = 1
    end if


    'if no GLCD_RST then is there is an GLCD_RESET
    if GLCD_RST_Defined = 0 Then
        if def(GLCD_RESET) then
            'map
            GLCD_RST = GLCD_RESET
            GLCD_RST_Defined = 1
        end if
        'so, if there one defined?
        if GLCD_RST_Defined = 0 Then
            warning "No GLCD_RST defined"
        end if
    end if
    
    if UNO_8bit_Shield then
      error "UNO_8bit_Shield not supported"
    end if

  end if

#endscript

#startup InitGLCD_ILI9488

'''Initialise the GLCD device
Sub InitGLCD_ILI9488

  #if GLCD_TYPE = GLCD_TYPE_ILI9488

    '  Mapped to global variables to same RAM
    dim GLCDDeviceHeight, GLCDDeviceWidth as word


    #ifdef GLCD_DataPort
        !Not tested
        InitGLCD_fullport_ILI9488
    #endif

    #ifndef GLCD_DataPort
        'Setup code for ILI9488 controllers
        #ifndef UNO_8bit_Shield
            //! This was tested and validated 
            'Set SPI Pin directions
            Dir ILI9488_CS Out
            Dir ILI9488_DC Out
            Dir ILI9488_RST Out

            Dir ILI9488_DI In
            Dir ILI9488_DO Out
            Dir ILI9488_SCK Out

        #endif

        #ifdef UNO_8bit_Shield
          ! No tested therefore not supported
          'constants moved to top

          dir  ILI9488_DB7 OUT
          dir  ILI9488_DB6 OUT
          dir  ILI9488_DB5 OUT
          dir  ILI9488_DB4 OUT
          dir  ILI9488_DB3 OUT
          dir  ILI9488_DB2 OUT
          dir  ILI9488_DB1 OUT
          dir  ILI9488_DB0 OUT

          'Set pin directions
          Dir ILI9488_RD  Out
          Dir ILI9488_WR  Out
          Dir ILI9488_DC  Out
          Dir ILI9488_CS  Out
          Dir ILI9488_RST Out

          Set ILI9488_RD On
          Set ILI9488_WR On
          Set ILI9488_DC On
          set ILI9488_CS ON
          Set ILI9488_RST On
        #endif

        #ifdef ILI9488_HardwareSPI
          ' harware SPI mode
          asm showdebug SPI constant used equates to HWSPIMODESCRIPT
          SPIMode HWSPIMODESCRIPT, HWSPIClockModeSCRIPT          
        #endif

        Set ILI9488_CS On
        Set ILI9488_DC On


        'Reset display
          Wait 50 ms
          Set ILI9488_RST On
          Wait 50 ms
          'Reset sequence (lower line for at least 10 us)
          Set ILI9488_RST Off
          Wait 150 us
          Set ILI9488_RST On
          Wait 150 ms


        SendCommand_ILI9488(ILI9488_CMD_POSITIVE_GAMMA_CORRECTION)
        SendData_ILI9488(0x00)
        SendData_ILI9488(0x03)
        SendData_ILI9488(0x09)
        SendData_ILI9488(0x08)
        SendData_ILI9488(0x16)
        SendData_ILI9488(0x0A)
        SendData_ILI9488(0x3F)
        SendData_ILI9488(0x78)
        SendData_ILI9488(0x4C)
        SendData_ILI9488(0x09)
        SendData_ILI9488(0x0A)
        SendData_ILI9488(0x08)
        SendData_ILI9488(0x16)
        SendData_ILI9488(0x1A)
        SendData_ILI9488(0x0F)

        SendCommand_ILI9488(ILI9488_CMD_NEGATIVE_GAMMA_CORRECTION)
        SendData_ILI9488(0x00)
        SendData_ILI9488(0x16)
        SendData_ILI9488(0x19)
        SendData_ILI9488(0x03)
        SendData_ILI9488(0x0F)
        SendData_ILI9488(0x05)
        SendData_ILI9488(0x32)
        SendData_ILI9488(0x45)
        SendData_ILI9488(0x46)
        SendData_ILI9488(0x04)
        SendData_ILI9488(0x0E)
        SendData_ILI9488(0x0D)
        SendData_ILI9488(0x35)
        SendData_ILI9488(0x37)
        SendData_ILI9488(0x0F)



        SendCommand_ILI9488(ILI9488_CMD_POWER_CONTROL_1)      //Power Control
        SendData_ILI9488(0x17)    //Vreg1out
        SendData_ILI9488(0x15)    //Verg2out

        SendCommand_ILI9488(ILI9488_CMD_POWER_CONTROL_2)      //Power Control
        SendData_ILI9488(0x41)    //VGH,VGL

        SendCommand_ILI9488(ILI9488_CMD_VCOM_CONTROL_1)      //Power Control
        SendData_ILI9488(0x00)
        SendData_ILI9488(0x12)    //Vcom
        SendData_ILI9488(0x80)

        SendCommand_ILI9488(ILI9488_CMD_MEMORY_ACCESS_CONTROL)      //Memory Access
        SendData_ILI9488(0x40)

        SendCommand_ILI9488(ILI9488_CMD_COLMOD_PIXEL_FORMAT_SET)      // Interface Pixel Format
        SendData_ILI9488(0x66) 	  //18 bit

        SendCommand_ILI9488(ILI9488_CMD_INTERFACE_MODE_CONTROL)      // Interface Mode Control
        SendData_ILI9488(0x80)     			 //SDO NOT USE

        SendCommand_ILI9488(ILI9488_CMD_FRAME_RATE_CONTROL_NORMAL)      //Frame rate
        SendData_ILI9488(0xA0)   

        SendCommand_ILI9488(ILI9488_CMD_DISPLAY_INVERSION_CONTROL)      //Display Inversion Control
        SendData_ILI9488(0x02)   

        SendCommand_ILI9488(ILI9488_CMD_DISPLAY_FUNCTION_CONTROL)      //Display Function Control  RGB/MCU Interface Control
          SendData_ILI9488(0x02)    
          SendData_ILI9488(0x02)    

        SendCommand_ILI9488(ILI9488_CMD_SET_IMAGE_FUNCTION)      // Set Image Functio
          SendData_ILI9488(0x00)    

        SendCommand_ILI9488(ILI9488_CMD_ADJUST_CONTROL_3)      // Adjust Control
          SendData_ILI9488(0xA9)
          SendData_ILI9488(0x51)
          SendData_ILI9488(0x2C)
          SendData_ILI9488(0x82) 


        SendCommand_ILI9488(ILI9488_CMD_SLEEP_OUT)    //Exit Sleep
        wait 120 ms

        SendCommand_ILI9488(ILI9488_CMD_DISPLAY_ON)
        wait 25 ms

        Dim GLCDbackground, GLCDForeground as Long
        'Default Colours
        GLCDForeground = ILI9488_TFT_WHITE
        'Default Colours
        #ifdef DEFAULT_GLCDBACKGROUND
          GLCDbackground = DEFAULT_GLCDBACKGROUND
        #endif

        #ifndef DEFAULT_GLCDBACKGROUND
          GLCDbackground = ILI9488_TFT_BLACK
        #endif

        'Variables required for device
        GLCDDeviceWidth = GLCD_WIDTH
        GLCDDeviceHeight = GLCD_HEIGHT
        GLCDFontWidth = 6
        GLCDfntDefault = 0
        GLCDfntDefaultsize = 2
        GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn
        GLCDRotate Portrait

        GLCDCLS

    #endif

  #endif

End Sub


'''Initialise the GLCD device
Sub InitGLCD_fullport_ILI9488

  '  Mapped to global variables to same RAM
  dim GLCDDeviceHeight, GLCDDeviceWidth as word

  'Setup code for ILI9488 controllers
    #if GLCD_TYPE = GLCD_TYPE_ILI9488

      'constants moved to top

      'Set pin directions
      Dir ILI9488_RD  Out
      Dir ILI9488_WR  Out
      Dir ILI9488_DC  Out
      Dir ILI9488_CS  Out
      Dir ILI9488_RST Out

      Set ILI9488_RD On
      Set ILI9488_WR On
      Set ILI9488_DC On
      set ILI9488_CS ON
      Set ILI9488_RST On

      Dir GLCD_DataPort out

      'Reset display
      Wait 50 ms
      Set ILI9488_RST On
      Wait 5 ms
      'Reset sequence (lower line for at least 10 us)
      Set ILI9488_RST Off
      Wait 20 us
      Set ILI9488_RST On
      Wait 150 ms

      SendCommand_ILI9488(ILI9488_CMD_POSITIVE_GAMMA_CORRECTION)
      SendData_ILI9488(0x00)
      SendData_ILI9488(0x03)
      SendData_ILI9488(0x09)
      SendData_ILI9488(0x08)
      SendData_ILI9488(0x16)
      SendData_ILI9488(0x0A)
      SendData_ILI9488(0x3F)
      SendData_ILI9488(0x78)
      SendData_ILI9488(0x4C)
      SendData_ILI9488(0x09)
      SendData_ILI9488(0x0A)
      SendData_ILI9488(0x08)
      SendData_ILI9488(0x16)
      SendData_ILI9488(0x1A)
      SendData_ILI9488(0x0F)


      SendCommand_ILI9488(ILI9488_CMD_NEGATIVE_GAMMA_CORRECTION)
      SendData_ILI9488(0x00)
      SendData_ILI9488(0x16)
      SendData_ILI9488(0x19)
      SendData_ILI9488(0x03)
      SendData_ILI9488(0x0F)
      SendData_ILI9488(0x05)
      SendData_ILI9488(0x32)
      SendData_ILI9488(0x45)
      SendData_ILI9488(0x46)
      SendData_ILI9488(0x04)
      SendData_ILI9488(0x0E)
      SendData_ILI9488(0x0D)
      SendData_ILI9488(0x35)
      SendData_ILI9488(0x37)
      SendData_ILI9488(0x0F)



      SendCommand_ILI9488(ILI9488_CMD_POWER_CONTROL_1)      //Power Control
      SendData_ILI9488(0x17)    //Vreg1out
      SendData_ILI9488(0x15)    //Verg2out

      SendCommand_ILI9488(ILI9488_CMD_POWER_CONTROL_2)      //Power Control
      SendData_ILI9488(0x41)    //VGH,VGL

      SendCommand_ILI9488(ILI9488_CMD_VCOM_CONTROL_1)      //Power Control
      SendData_ILI9488(0x00)
      SendData_ILI9488(0x12)    //Vcom
      SendData_ILI9488(0x80)

      SendCommand_ILI9488(ILI9488_CMD_MEMORY_ACCESS_CONTROL)      //Memory Access
      SendData_ILI9488(0x40)

      SendCommand_ILI9488(ILI9488_CMD_COLMOD_PIXEL_FORMAT_SET)      // Interface Pixel Format
      SendData_ILI9488(0x66) 	  //18 bit

      SendCommand_ILI9488(ILI9488_CMD_INTERFACE_MODE_CONTROL)      // Interface Mode Control
      SendData_ILI9488(0x80)     			 //SDO NOT USE

      SendCommand_ILI9488(ILI9488_CMD_FRAME_RATE_CONTROL_NORMAL)      //Frame rate
      SendData_ILI9488(0xA0)    //60Hz

      SendCommand_ILI9488(ILI9488_CMD_DISPLAY_INVERSION_CONTROL)      //Display Inversion Control
      SendData_ILI9488(0x02)    //2-dot

      SendCommand_ILI9488(ILI9488_CMD_DISPLAY_FUNCTION_CONTROL)      //Display Function Control  RGB/MCU Interface Control
        SendData_ILI9488(0x02)    //MCU
        SendData_ILI9488(0x02)    //Source,Gate scan dieection

      SendCommand_ILI9488(ILI9488_CMD_SET_IMAGE_FUNCTION)      // Set Image Functio
        SendData_ILI9488(0x00)    // Disable 24 bit data

      SendCommand_ILI9488(ILI9488_CMD_ADJUST_CONTROL_3)      // Adjust Control
        SendData_ILI9488(0xA9)
        SendData_ILI9488(0x51)
        SendData_ILI9488(0x2C)
        SendData_ILI9488(0x82)    // D7 stream, loose


      SendCommand_ILI9488(ILI9488_CMD_SLEEP_OUT)    //Exit Sleep
      wait 120 ms

      SendCommand_ILI9488(ILI9488_CMD_DISPLAY_ON)
      wait 25 ms

      'Default Colours
      GLCDForeground = ILI9488_WHITE
      'Default Colours
      #ifdef DEFAULT_GLCDBACKGROUND
        GLCDbackground = DEFAULT_GLCDBACKGROUND
      #endif

      #ifndef DEFAULT_GLCDBACKGROUND
        GLCDbackground = ILI9488_BLACK
      #endif


      'Variables required for device
      GLCDDeviceWidth = GLCD_WIDTH
      GLCDDeviceHeight = GLCD_HEIGHT
      GLCDFontWidth = 6
      GLCDfntDefault = 0
      GLCDfntDefaultsize = 2
      GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn

      GLCDRotate Portrait

      GLCDCLS

  #endif

End Sub


'Subs
'''Clears the GLCD screen
Sub GLCDCLS_ILI9488 ( Optional In  GLCDbackground as Long = GLCDbackground )

    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0

    Dim ILI9488SendLong as Long

    SetAddressWindow_ILI9488 ( 0, 0, GLCD_WIDTH - 1 , GLCD_HEIGHT -1 )
    ILI9488SendLong = [LONG]GLCDbackground

    #if Var(SPI1TCNTL) or Var(SPCR0)
      HWSPI_Send_18bits =  ILI9488SendLong
    #endif

    set ILI9488_CS OFF
    set ILI9488_DC ON
    Repeat 153600

        #ifndef UNO_8bit_Shield
            #ifdef ILI9488_HardwareSPI
              #ifdef PIC
                #ifndef Var(SSPCON1)
                  #ifdef Var(SSPCON)
                    Dim SSPCON1 Alias SSPCON
                  #endif
                #endif
                #ifdef Var(SPI1TCNTL)
                  //~ Tested and validated
                  // FastHWSPITransfer  ILI9488SendLong
                  // FastHWSPITransfer  ILI9488SendLong_H
                  // FastHWSPITransfer  ILI9488SendLong_U
                  HWSPI_Fast_Write_18bits_Macro

                #else
                  'Clear WCOL
                  Set SSPCON1.WCOL Off
                  'Put byte to send into buffer
                  'Will start transfer
                  SSPBUF = ILI9488SendLong_u
                  Wait While SSPSTAT.BF = Off
                  Set SSPSTAT.BF Off
                  #if ChipFamily 16
                    ILI9488TempOut = SSPBUF
                  #endif

                  'Clear WCOL
                  Set SSPCON1.WCOL Off
                  'Put byte to send into buffer
                  'Will start transfer
                  SSPBUF = ILI9488SendLong_h
                  Wait While SSPSTAT.BF = Off
                  Set SSPSTAT.BF Off
                  #if ChipFamily 16
                    ILI9488TempOut = SSPBUF
                  #endif

                  'Clear WCOL
                  Set SSPCON1.WCOL Off
                  'Put byte to send into buffer
                  'Will start transfer
                  SSPBUF = ILI9488SendLong
                  Wait While SSPSTAT.BF = Off
                  Set SSPSTAT.BF Off
                  #if ChipFamily 16
                    ILI9488TempOut = SSPBUF
                  #endif

                #endif
              #endif
              #ifdef AVR
                #if NoVar(SPCR0)
                  'Master mode only
                  SPDR = ILI9488SendLong_u
                  Do

                  Loop While SPSR.WCOL
                  'Read buffer
                  'Same for master and slave
                  Wait While SPSR.SPIF = Off

                  SPDR = ILI9488SendLong_h
                  Do

                  Loop While SPSR.WCOL
                  'Read buffer
                  'Same for master and slave
                  Wait While SPSR.SPIF = Off

                  'Master mode only
                  SPDR = [BYTE]ILI9488SendLong
                  Do

                  Loop While SPSR.WCOL
                  'Read buffer
                  'Same for master and slave
                  Wait While SPSR.SPIF = Off
                #endif
                #if Var(SPCR0)
                  
                    HWSPI_Fast_Write_18bits_Macro
                  
                #endif              
              #endif

            #endif

            #ifndef ILI9488_HardwareSPI
              Send_18bits_ILI9488 ( GLCDbackground )
            #endif

        #endif

        #ifdef UNO_8bit_Shield
              ! No tested therefore not supported
              GLCDCLS_HiBytePortion1 = (PORTD & 0B00000011) | ((ILI9488SendWord_H) & 0B11111100)
              GLCDCLS_HiBytePortion2 = (PORTB & 0B11111100) | ((ILI9488SendWord_H) & 0B00000011)
              GLCDCLS_LoBytePortion1 =  (PORTD & 0B00000011) | ((ILI9488SendWord) & 0B11111100)
              GLCDCLS_LoBytePortion2 =  (PORTB & 0B11111100) | ((ILI9488SendWord) & 0B00000011)
              
              #ifdef AVR
                'Write 8 bit bus for AVR
                PORTD = GLCDCLS_HiBytePortion1
                PORTB = GLCDCLS_HiBytePortion2
                set ILI9488_WR OFF
                set ILI9488_WR ON
                PORTD = GLCDCLS_LoBytePortion1
                PORTB = GLCDCLS_LoBytePortion2
                set ILI9488_WR OFF
                set ILI9488_WR ON
              #endif
        #endif

    end repeat
    set ILI9488_CS ON;

End Sub


Sub GLCDCLS_fullport_ILI9488 ( Optional In  GLCDbackground as word = GLCDbackground )

    dim ILI9488SendWord as word
    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0

    SetAddressWindow_ILI9488 ( 0, 0, GLCD_WIDTH  , GLCD_HEIGHT )
    ILI9488SendWord = GLCDbackground

    set ILI9488_CS OFF
    set ILI9488_DC ON
    Repeat 100
        Repeat 48
          Repeat 32
              'Write 8 bit bus
              GLCD_DataPort = ILI9488SendWord_h
              set ILI9488_WR OFF
              set ILI9488_WR ON
              GLCD_DataPort = ILI9488SendWord
              set ILI9488_WR OFF
              set ILI9488_WR ON
          end repeat
       end repeat
    end repeat
    set ILI9488_CS ON;

End Sub

'''Draws a string at the specified location on the ST7920 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawString_ILI9488( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as Long = GLCDForeground )

  dim GLCDPrintLoc, xchar as word

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

Sub GLCDDrawChar_ILI9488(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as Long = GLCDForeground )

  'This has got a tad complex
  'We have three major pieces
  '1 The preamble - this just adjusted color and the input character
  '2 The code that deals with GCB fontset
  '3 The code that deals with OLED fontset
  '
  'You can make independent change to section 2 and 3 but they are mutual exclusive with many common pieces

   dim CharCol, CharRow, CurrCharVal, CurrCharCol, CurrCharRow, Col, Row, CharRowS, CharColS, GLCDTemp as word
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
                           PSet [word]CharLocX + CharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, GLCDbackground
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
                  CurrCharVal = GLCDbackground
                #endif
          End Select

            'we handle 8 or 16 pixels of height
            For CurrCharRow = 0 to ROWSperfont
                'Set the pixel
                If CurrCharVal.0 = 0 Then
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDbackground
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
                    CurrCharVal = GLCDbackground
                  #endif
                end if

                'It is the intercharacter space, put out one pixel row
                if CurrCharCol = COLSperfont then
                    'Put out a white intercharacter pixel/space
                     GLCDTemp = CharLocX + CurrCharCol
                     if GLCDfntDefaultSize = 2 then
                        GLCDTemp++
                     end if
                     PSet GLCDTemp , CharLocY + CurrCharRow, GLCDbackground
                end if

            Next



        Next


    #endif

End Sub


'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel
Sub PSet_ILI9488(In GLCDX as word, In GLCDY as word, In GLCDColour As Long)

  select case GLCDRotateState
        case PORTRAIT  '0 degree
              SetAddressWindow_ILI9488 ( GLCDX, GLCDY, GLCDX, GLCDY )
              Send_18bits_ILI9488 GLCDColour
              GLCDDeviceWidth = GLCD_WIDTH
              GLCDDeviceHeight = GLCD_HEIGHT

        case LANDSCAPE
              SetAddressWindow_ILI9488 ( GLCDy, GLCDDeviceWidth -GLCDx -1, GLCDy, GLCDDeviceWidth -GLCDx -1 )
              Send_18bits_ILI9488 GLCDColour
              GLCDDeviceWidth = GLCD_HEIGHT
              GLCDDeviceHeight = GLCD_WIDTH            

        case PORTRAIT_REV
              SetAddressWindow_ILI9488 ( GLCDDeviceWidth - GLCDX-1, GLCDDeviceHeight - GLCDY-1, GLCDDeviceWidth - GLCDX-1, GLCDDeviceHeight - GLCDY-1 )
              Send_18bits_ILI9488 GLCDColour
              GLCDDeviceWidth = GLCD_WIDTH
              GLCDDeviceHeight = GLCD_HEIGHT

        case LANDSCAPE_REV
              SetAddressWindow_ILI9488 ( GLCDDeviceHeight - GLCDy-1, GLCDx, GLCDDeviceHeight - GLCDy-1, GLCDx )
              Send_18bits_ILI9488 GLCDColour
              GLCDDeviceWidth = GLCD_HEIGHT
              GLCDDeviceHeight = GLCD_WIDTH            

        case else
              SetAddressWindow_ILI9488 ( GLCDX, GLCDY, GLCDX, GLCDY )
              Send_18bits_ILI9488 GLCDColour
              GLCDDeviceWidth = GLCD_WIDTH
              GLCDDeviceHeight = GLCD_HEIGHT

  end select

End Sub




'''Send a command to the ILI9488 GLCD
'''@param ILI9488SendByte Command to send
'''@hide
sub  SendCommand_ILI9488( IN SPITxData as byte )

    dim ILI9488TempOut as Byte

    #ifndef UNO_8bit_Shield
      set ILI9488_CS OFF;
      set ILI9488_DC OFF;

      #ifdef ILI9488_HardwareSPI
        'Hardware SPI ****************************************
         SPITransfer  SPITxData,  SPIRxData
         set ILI9488_CS ON;
         exit sub
      #endif

      #ifndef ILI9488_HardwareSPI
      'Software SPI ****************************************
      repeat 8

        if SPITxData.7 = ON  then
          set ILI9488_DO ON;
        else
          set ILI9488_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate SPITxData left
        set GLCD_SCK Off;

      end repeat
      set ILI9488_CS ON;
      #endif
    #endif


    #ifdef UNO_8bit_Shield
      ! No tested therefore not supported
      'UNO_8bit_Shield for UNO Shield ****************************************
      set ILI9488_CS OFF;
      set ILI9488_DC OFF;

      #ifdef AVR
      PORTD = (PORTD & 0B00000011) | ((SPITxData) & 0B11111100)
      PORTB = (PORTB & 0B11111100) | ((SPITxData) & 0B00000011)
      #endif


      set ILI9488_WR OFF
      set ILI9488_WR ON


      set ILI9488_CS ON;
    #endif

end Sub

'''Send a data byte to the ILI9488 GLCD
'''@param ILI9488SendByte Byte to send
'''@hide
sub  SendData_ILI9488( IN SPITxData as byte )

    #ifndef UNO_8bit_Shield
      set ILI9488_CS OFF;
      set ILI9488_DC ON;

      #ifdef ILI9488_HardwareSPI
        'Hardware SPI ****************************************
         SPITransfer  SPITxData,  SPIRxData
         set ILI9488_CS ON;
         exit sub
      #endif

      #ifndef ILI9488_HardwareSPI
      'Software SPI ****************************************
      repeat 8

        if SPITxData.7 = ON then
          set ILI9488_DO ON;
        else
          set ILI9488_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate SPITxData left
        set GLCD_SCK Off;

      end Repeat
      set ILI9488_CS ON;
      #endif
    #endif

    #ifdef UNO_8bit_Shield
      ! No tested therefore not supported
      'UNO_8bit_Shield for UNO Shield ****************************************
      set ILI9488_CS OFF;
      set ILI9488_DC ON;

      #ifdef AVR
      PORTD = (PORTD & 0B00000011) | ((SPITxData) & 0B11111100)
      PORTB = (PORTB & 0B11111100) | ((SPITxData) & 0B00000011)
      #endif

      set ILI9488_WR OFF
      set ILI9488_WR ON

      set ILI9488_CS ON;
    #endif

end Sub

'''Send a data word (16 bits) to the ILI9488 GLCD
'''@param ILI9488SendWord  Word to send
'''@hide
Sub SendWord_ILI9488(In ILI9488SendWord As Word)

  #ifndef UNO_8bit_Shield
      set ILI9488_CS OFF;
      set ILI9488_DC ON;

      #ifdef ILI9488_HardwareSPI
        'Hardware SPI ****************************************
         SPITransfer  ILI9488SendWord_H,  ILI9488TempOut
         SPITransfer  ILI9488SendWord,  ILI9488TempOut
         set ILI9488_CS ON;
         exit sub
      #endif

      #ifndef ILI9488_HardwareSPI
      'Software SPI ****************************************
      repeat 16

        if ILI9488SendWord.15 = ON then
          set ILI9488_DO ON;
        else
          set ILI9488_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate ILI9488SendWord left
        set GLCD_SCK Off;

      end repeat
      set ILI9488_CS ON;
      #endif
  #endif

  #ifdef UNO_8bit_Shield
    ! No tested therefore not supported
    'UNO_8bit_Shield for UNO Shield ****************************************
    set ILI9488_CS OFF;
    set ILI9488_DC ON;

    #ifdef AVR
    PORTD = (PORTD & 0B00000011) | ((ILI9488SendWord_H) & 0B11111100)
    PORTB = (PORTB & 0B11111100) | ((ILI9488SendWord_H) & 0B00000011)
    #endif

    set ILI9488_WR OFF
    set ILI9488_WR ON

    #ifdef AVR
    PORTD = (PORTD & 0B00000011) | ((ILI9488SendWord) & 0B11111100)
    PORTB = (PORTB & 0B11111100) | ((ILI9488SendWord) & 0B00000011)
    #endif

    set ILI9488_WR OFF
    set ILI9488_WR ON

    set ILI9488_CS ON;
  #endif
End Sub


'''Send data (18 color bits) in 3 bytes to the ILI9488 GLCD
'''@param ILI9488SendLong to send
'''@hide
Sub Send_18bits_ILI9488(ILI9488SendLong As Long)

  #ifndef UNO_8bit_Shield
      set ILI9488_CS OFF;
      set ILI9488_DC ON;

      #ifdef ILI9488_HardwareSPI
        'Hardware SPI ****************************************
         SPITransfer  ILI9488SendLong,  ILI9488TempOut
         SPITransfer  ILI9488SendLong_H,  ILI9488TempOut
         SPITransfer  ILI9488SendLong_U,  ILI9488TempOut
         set ILI9488_CS ON;
         exit sub
      #endif

      #ifndef ILI9488_HardwareSPI
      'Software SPI ****************************************
      repeat 24

        if ILI9488SendLong.23 = ON then
          set ILI9488_DO ON;
        else
          set ILI9488_DO OFF;
        end if
        SET GLCD_SCK On;
        rotate ILI9488SendLong left
        set GLCD_SCK Off;

      end repeat
      set ILI9488_CS ON;
      #endif
  #endif

  #ifdef UNO_8bit_Shield
      !Not supported, actaully not tested
  #endif
End Sub


'''Send a command to the ILI9488 GLCD
'''@param ILI9488SendByte Command to send
'''@hide
sub  SendCommand_fullport_ILI9488( IN ILI9488SendByte as byte )

      '8Bit_Data_Port ****************************************
      set ILI9488_CS OFF;
      set ILI9488_DC OFF;

      GLCD_DataPort = ILI9488SendByte

      set ILI9488_WR OFF
      set ILI9488_WR ON

      set ILI9488_CS ON;

end Sub

'''Send a data byte to the ILI9488 GLCD
'''@param ILI9488SendByte Byte to send
'''@hide
sub  SendData_fullport_ILI9488( IN ILI9488SendByte as byte )

      '8Bit_Data_Port ****************************************
      set ILI9488_CS OFF;
      set ILI9488_DC ON;

      GLCD_DataPort = ILI9488SendByte

      set ILI9488_WR OFF
      set ILI9488_WR ON

      set ILI9488_CS ON;

end Sub

'''Send a data word (16 bits) to the ILI9488 GLCD
'''@param ILI9488SendWord  Word to send
'''@hide
Sub SendWord_fullport_ILI9488(In ILI9488SendWord As Word)

    '8Bit_Data_Port ****************************************
    set ILI9488_CS OFF;
    set ILI9488_DC ON;

    GLCD_DataPort = ILI9488SendWord_h

    set ILI9488_WR OFF
    set ILI9488_WR ON

    GLCD_DataPort = ILI9488SendWord

    set ILI9488_WR OFF
    set ILI9488_WR ON

    set ILI9488_CS ON;

End Sub


'''Set the row or column address range for the ILI9488 GLCD
'''@param ILI9488AddressType Address Type (ILI9488_ROW or ILI9488_COLUMN)
'''@param ILI9488Start Starting address
'''@param ILI9488End Ending address
'''@hide
Sub SetAddressWindow_ILI9488( In _x1 as word, in _y1 as word, in _x2 as word, in _y2 as word)

  SendCommand_ILI9488(ILI9488_CASET)  'Column addr set
  SendData_ILI9488 _x1_H
  SendData_ILI9488 _x1
  SendData_ILI9488 _x2_H
  SendData_ILI9488 _x2

  SendCommand_ILI9488(ILI9488_PASET)  'Row addr set
  SendData_ILI9488 _y1_H
  SendData_ILI9488 _y1
  SendData_ILI9488 _y2_H
  SendData_ILI9488 _y2

  SendCommand_ILI9488(ILI9488_RAMWR)  'write to RAM

End Sub



Sub SetCursorPosition_ILI9488( In _x1 as word, in _y1 as word, in _x2 as word, in _y2 as word)
  SendCommand_ILI9488(ILI9488_CASET)  Column addr set
  SendData_ILI9488 _x1_H
  SendData_ILI9488 _x1
  SendData_ILI9488 _x2_H
  SendData_ILI9488 _x2

  SendCommand_ILI9488(ILI9488_PASET)  Row addr set
  SendData_ILI9488 _y1_H
  SendData_ILI9488 _y1
  SendData_ILI9488 _y2_H
  SendData_ILI9488 _y2

End Sub


''@hide
sub   GLCDRotate_ILI9488 ( in GLCDRotateState as byte )

  select case GLCDRotateState
        case PORTRAIT  '0 degree
             GLCDDeviceWidth = GLCD_WIDTH
             GLCDDeviceHeight = GLCD_HEIGHT
        case LANDSCAPE
             GLCDDeviceWidth = GLCD_HEIGHT
             GLCDDeviceHeight = GLCD_WIDTH
        case PORTRAIT_REV
             GLCDDeviceWidth = GLCD_WIDTH
             GLCDDeviceHeight = GLCD_HEIGHT
        case LANDSCAPE_REV
             GLCDDeviceWidth = GLCD_HEIGHT
             GLCDDeviceHeight = GLCD_WIDTH
        case else
             GLCDDeviceWidth = GLCD_WIDTH
             GLCDDeviceHeight = GLCD_HEIGHT
  end select

end sub


'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_ILI9488(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Long = GLCDForeground)
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


  'Fill with colour
  'Draw lines going across
  For DrawLine = LineX1 To LineX2
    For GLCDTemp = LineY1 To LineY2
      PSet DrawLine, GLCDTemp, LineColour
    Next
  Next

End Sub

