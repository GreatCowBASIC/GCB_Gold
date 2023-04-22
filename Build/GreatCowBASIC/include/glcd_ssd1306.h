'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2015- 2021 Kent Schafer, Evan Venn and Joseph Realmuto

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
' Supports SSD1306 controller only.

'Hardware settings using I2C
'  #define GLCD_TYPE GLCD_TYPE_SSD1306
'  #define GLCD_I2C_Address 0x78
'Hardware settings using SPI (4Wire,MOSI(D1),SCK(D0),DC,CS)
'  #define GLCD_TYPE GLCD_TYPE_SSD1306
'  #define S4Wire_Data

' 1.01 Added scrollspeed
' 1.02 Add soft SPI (Kent Schafer)
' 1.03 Revised S4Wire_Data to cater for i2c or hwi2c being used at same time as SPI
' 1.04 Revised for performance only. No functional changes
' 1,05 Revised to fix initialisation issue from PIC when using Priority Startup
' 1.06 Revised to support 128 * 32 device
'       Added new init commands and GLCD_SSD1306_HEIGHT define
'       use #define GLCD_TYPE_SSD1306_32 to setup for 128*32 device
' 1.07 Added PixelStatus_SSD1306 to read the status of a pixel
'      and extremes within Pset
' 1.08 change 32 line buffer to 512 bytes from 1024
' 1.09 correct buffer assignment and resolved buffer overright issue in CLS
' 1.10 Modified InitGLCD_SSD1306 for cleaner intalization - WMR
' 1.11 Added low RAM character mode only (Joseph Realmuto) - July 26, 2017
'      Adding GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY to set configuration
'      Only TEST commands are supported. No graphics supported.
'      Y screen positions are similar to LCD. This is an 8 line device.
'      8 lines of text, any GLCDPrint (xpos, YPOS, text) and YPOS will be translated to Address Line #n.
'      Address Line #0 = YPOS 0 to 7
'      Address Line #1 = YPOS 8 to 15
'      Address Line #2 = YPOS 16 to 23
'      Address Line #3 = YPOS 14 to 31
'      Address Line #4 = YPOS 32 to 39
'      Address Line #5 = YPOS 40 to 47
'      Address Line #6 = YPOS 48 to 55
'      Address Line #7 = YPOS 56 to 63
'
'   Example of character mode usage
'    #define GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY    'This defined will be required on high RAM devices to activate low memory capability
'
'    GLCDPrint 0,10, "Great Cow BASIC"
'    GLCDPrint 0,52, "Page 2"
'
' 1.12 Corrected script to go into character only mode and assign buffer size (Evan Venn) - July 26, 2017
' 1.13 Low memory adaptation and documentation
'      Low memory mode
'           8 lines of text, any GLCDPrint (xpos, YPOS, text) and YPOS will be translated to Address Line #n.
'           Address Line #1 = YPOS 0 to 7
'           Address Line #2 = YPOS 8 to 15
'           Address Line #3 = YPOS 16 to 23
'           Address Line #4 = YPOS 14 to 31
'           Address Line #5 = YPOS 32 to 39
'           Address Line #6 = YPOS 40 to 47
'           Address Line #7 = YPOS 48 to 55
'           Address Line #8 = YPOS 56 to 63
'
'
'  1.14 Added GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE configuration
'   GLCD_Open_PageTransaction and GLCD_Close_PageTransaction added to support Low memory configuration
'   Any commands between GLCD_Open_PageTransaction and GLCD_Close_PageTransaction should be GLCD focused only.
'   Any commands within the transaction WILL BE called 8 (EIGHT) times. So, if you increment variables... they will be incremented 8 times
'   Any commands between GLCD_Open_PageTransaction and GLCD_Close_PageTransaction should be GLCD focused only.
'   Methods such as scroll, pixelstatus WILL NOT work in low memory mode.
'
'   Example of low memory usage
'    #define GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY    'This defined will be required on high RAM devices to activate low memory capability
'    #define GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
'
'      GLCD_Open_PageTransaction 0,7
'        GLCDPrint 0,10, "Great Cow BASIC"
'        GLCDPrint 0,52, "Page 2"
'        Box 0,0,127,63
'      GLCD_Close_PageTransaction
'
'
'  1.15 Fixed shifting of characters by one pixel down and one pixel right when in GLCD mode (Joseph Realmuto)
'  1.16 improved Pset for text mode
'  1.17 Improved speed of GLCDDrawChar and Pset in low RAM GLCD mode (Joseph Realmuto)
'  1.18 Corrected CLS to handle buffer correctly
'  1.19 Further speed improvements of GLCDDrawChar when in low RAM GLCD mode (Joseph Realmuto)
'  1.20 Revised 28.08.2017 to resolve non-ANSI characters
'       Changed vccstate to constant SSD1306_vccstate with a default of 0
'       Added compulsory wait of 255 ms
'  1.21 Added update to handle fonts size in LOWMEMORY_GLCD_MODE
'  1.22 Added OLED fonts
'  1.23 Adapted to ensure fonts in correct position and they fill the intercharacter pixels
'  1.24 Added support for GLCDPrintStringLN etc. Setting variables to zero on print screen
'  1.25 Added HWI2C2 support
'  1.26 Added IGNORE_GLCD_TYPE_SSD1306_LOW_MEMORY_WARNINGS to support low memory MCU warnings
'  27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn
'  21/04/20  Updated InitGLCD to correctly handle GLCD_TYPE
'  26/03/21  Revised Write_Transaction_Data_SSD1306 to correct SPI command/data settings
'  12/04/23  Revised to ERROR() when family14 and less than 225 byes of RAM. The array needs 128 bytes and these chips only support 95 bytes of contiguous RAM 

#define SSD1306_vccstate 0

#define SSD1306_SETCONTRAST 0x81
#define SSD1306_DISPLAYALLON_RESUME 0xA4
#define SSD1306_DISPLAYALLON 0xA5
#define SSD1306_NORMALDISPLAY 0xA6
#define SSD1306_INVERTDISPLAY 0xA7
#define SSD1306_DISPLAYOFF 0xAE
#define SSD1306_DISPLAYON 0xAF

#define SSD1306_SETDISPLAYOFFSET 0xD3
#define SSD1306_SETCOMPINS 0xDA

#define SSD1306_SETVCOMDETECT 0xDB

#define SSD1306_SETDISPLAYCLOCKDIV 0xD5
#define SSD1306_SETPRECHARGE 0xD9

#define SSD1306_SETMULTIPLEX 0xA8

#define SSD1306_SETLOWCOLUMN 0x00
#define SSD1306_SETHIGHCOLUMN 0x10

#define SSD1306_SETSTARTLINE 0x40

#define SSD1306_MEMORYMODE 0x20
#define SSD1306_COLUMNADDR 0x21
#define SSD1306_PAGEADDR   0x22

#define SSD1306_COMSCANINC 0xC0
#define SSD1306_COMSCANDEC 0xC8

#define SSD1306_SEGREMAP 0xA0

#define SSD1306_CHARGEPUMP 0x8D

#define SSD1306_EXTERNALVCC 0x01
#define SSD1306_SWITCHCAPVCC 0x02

' Scrolling #defines
#define SSD1306_ACTIVATE_SCROLL 0x2F
#define SSD1306_DEACTIVATE_SCROLL 0x2E
#define SSD1306_SET_VERTICAL_SCROLL_AREA 0xA3
#define SSD1306_RIGHT_HORIZONTAL_SCROLL 0x26
#define SSD1306_LEFT_HORIZONTAL_SCROLL 0x27
#define SSD1306_VERTICAL_AND_RIGHT_HORIZONTAL_SCROLL 0x29
#define SSD1306_VERTICAL_AND_LEFT_HORIZONTAL_SCROLL 0x2A

#startup InitGLCD_SSD1306, 100

'Setup code for SSD1306 controllers
  #script     ' This script set the capabilities based upon the amount of RAM

     IGNORE_SPECIFIED_GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY = 0
     if IGNORE_GLCD_TYPE_SSD1306_LOW_MEMORY_WARNINGS then
        IGNORE_SPECIFIED_GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY = 1
     end if

     if GLCD_TYPE = GLCD_TYPE_SSD1306 then
       If ChipRAM < 1024  Then
           GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY = TRUE
           GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE = TRUE

          If ChipRAM < 225  Then
            If ChipFamily = 14 Then
              Error "Selected MCU has insufficient contiguous RAM to support GLCD operations"
            End if
          End if
          If ChipRAM > 225  Then
            if NODEF(IGNORE_SPECIFIED_GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY) then
              Warning "Memory < 1024 bytes."
              Warning "Selected MCU requires use of GLCD Open&Close Page Transaction."
              Warning "See Help for usage."
              Warning "Define a constant IGNORE_GLCD_TYPE_SSD1306_LOW_MEMORY_WARNINGS to remove this message."
            end if
          End if
       End If
     end if

     if GLCD_TYPE = GLCD_TYPE_SSD1306_32 then
       If ChipRAM < 512  Then
           GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY = TRUE
           GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE = TRUE
           If ChipRAM < 256  Then
              Error "Chip has insufficient RAM to support this specific GLCD"
           End if
       End If
     end if





   #endscript

   dim SSD1306_BufferLocationCalc as Word               ' mandated in main program for SSD1306
   'dim GLCDY_Temp as Integer alias SSD1306_BufferLocationCalc_h, SSD1306_BufferLocationCalc
   'SSD1306_BufferLocationCalc = 1
   'GLCDY_Temp = -1
   'dim GLCDY_Temp as Integer alias SSD1306_BufferLocationCalc_h, SSD1306_BufferLocationCalc

   #ifndef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
     #if GLCD_TYPE = GLCD_TYPE_SSD1306
       asm showdebug  GGLCD SSD1306 buffer is 1024bytes
       Dim SSD1306_BufferAlias(1024)
     #endif
     #if GLCD_TYPE = GLCD_TYPE_SSD1306_32
       asm showdebug  GLCD SSD1306 buffer is 512bytes
       Dim SSD1306_BufferAlias(512)
     #endif
   #endif

   #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
     #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
       asm showdebug  GLCD SSD1306 buffer is 128bytes
       Dim SSD1306_BufferAlias(128)
     #endif
   #endif


'''@hide
Sub Write_Command_SSD1306 ( in SSD1306SendByte as byte )

    #ifdef S4Wire_DATA

      CS_SSD1306 = 0
      DC_SSD1306 = 0
      S4Wire_SSD1306 SSD1306SendByte
      DC_SSD1306 = 1
      CS_SSD1306 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address
      I2CSend 0x00
      I2CSend SSD1306SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA

      HI2CStart
      HI2CSend GLCD_I2C_Address
      HI2CSend 0x00
      HI2CSend SSD1306SendByte
      HI2CStop

    #endif

    #ifdef HI2C2_DATA

      HI2C2Start
      HI2C2Send GLCD_I2C_Address
      HI2C2Send 0x00
      HI2C2Send SSD1306SendByte
      HI2C2Stop

    #endif

End Sub

'''@hide
Sub Write_Data_SSD1306 ( in SSD1306SendByte as byte )

    #ifdef S4Wire_DATA

      CS_SSD1306 = 0
      DC_SSD1306 = 1
      S4Wire_SSD1306 SSD1306SendByte
      DC_SSD1306 = 0
      CS_SSD1306 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address
      I2CSend 0x40
      I2CSend SSD1306SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA

      HI2CStart
      HI2CSend GLCD_I2C_Address
      HI2CSend 0x40
      HI2CSend SSD1306SendByte
      HI2CStop

    #endif

    #ifdef HI2C2_DATA

      HI2C2Start
      HI2C2Send GLCD_I2C_Address
      HI2C2Send 0x40
      HI2C2Send SSD1306SendByte
      HI2C2Stop

    #endif

End Sub


'''@hide
Sub InitGLCD_SSD1306

  #IF ( GLCD_TYPE  = GLCD_TYPE_SSD1306_32 ) or ( GLCD_TYPE  = GLCD_TYPE_SSD1306 )then
        'Colours //Set these first
        GLCDBackground = 0
        GLCDForeground = 1
        GLCDFontWidth = 5
        GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn
        dim PrintLocX, PrintLocY as word

        GLCDfntDefault = 0
        GLCDfntDefaultsize = 1
        wait 255 ms             'added to ensure the charge pump and power is operational.
         #IFDEF HI2C_DATA
                 HI2CMode Master
                 Wait 15 ms  'wait for power-up and reset
         #ENDIF

         #IFDEF HI2C2_DATA
                 HI2C2Mode Master
                 Wait 15 ms  'wait for power-up and reset
         #ENDIF

         #ifdef S4Wire_DATA
              dir MOSI_SSD1306 Out
              dir SCK_SSD1306 Out
              dir DC_SSD1306 Out
              dir CS_SSD1306 Out
              dir RES_SSD1306 Out
              RES_SSD1306 = 0
              wait 10 us
              RES_SSD1306 = 1
            #endif

       'Setup code for SSD1306 controllers
        'Init sequence for 128x64 OLED module
        Write_Command_SSD1306(SSD1306_DISPLAYOFF)                    ' 0xAE

        Write_Command_SSD1306(SSD1306_DEACTIVATE_SCROLL)
        Write_Command_SSD1306(SSD1306_SETDISPLAYCLOCKDIV)            ' 0xD5
        Write_Command_SSD1306(0x80)                                  ' the suggested ratio 0x80

        Write_Command_SSD1306(SSD1306_SETMULTIPLEX)                  ' 0xA8
        #if GLCD_HEIGHT = 64
          Write_Command_SSD1306(0x3f)                                 '64 pixels
        #endif

        #if GLCD_HEIGHT = 32
          Write_Command_SSD1306(0x1f)                                 '32 pixels
        #endif

        Write_Command_SSD1306(SSD1306_SETDISPLAYOFFSET)              ' 0xD3
        Write_Command_SSD1306(0x00)                                   ' no offset

        Write_Command_SSD1306(SSD1306_SETSTARTLINE | 0x00)            ' line #0
        Write_Command_SSD1306(SSD1306_CHARGEPUMP)                    ' 0x8D

        if (SSD1306_vccstate = SSD1306_EXTERNALVCC) then
          Write_Command_SSD1306(0x10)
        else
          Write_Command_SSD1306(0x14)
        end if

        Write_Command_SSD1306(SSD1306_MEMORYMODE)                    ' 0x20
        'Write_Command_SSD1306(0x00)                                  ' 0x00 act like ks0108 - DO NOT SELECT!!
        Write_Command_SSD1306(0x10)                                  ' 0x01 act like PCD8544

        Write_Command_SSD1306(SSD1306_SEGREMAP | 0x1)
        Write_Command_SSD1306(SSD1306_COMSCANDEC)
        Write_Command_SSD1306(SSD1306_SETCOMPINS)                    ' 0xDA

        #if GLCD_HEIGHT = 64
          Write_Command_SSD1306(0x12)                                 '64 pixels
        #endif

        #if GLCD_HEIGHT = 32
          Write_Command_SSD1306(0x02)                                 '32 pixels
        #endif

        Write_Command_SSD1306(SSD1306_SETCONTRAST)                   ' 0x81
        if SSD1306_vccstate = SSD1306_EXTERNALVCC then
          Write_Command_SSD1306(0x9F)
        else
          Write_Command_SSD1306(0xCF)
        end if
        Write_Command_SSD1306(SSD1306_SETPRECHARGE)                  ' 0xd9
        if SSD1306_vccstate = SSD1306_EXTERNALVCC then
          Write_Command_SSD1306(0x22)
        else
          Write_Command_SSD1306(0xF1)
        end if

        Write_Command_SSD1306(SSD1306_SETVCOMDETECT)                 ' 0xDB
        Write_Command_SSD1306(0x40)

        Write_Command_SSD1306(SSD1306_DISPLAYALLON_RESUME)           ' 0xA4
        Write_Command_SSD1306(SSD1306_NORMALDISPLAY)                 ' 0xA6

       'Clear screen Here
        GLCDCLS_SSD1306

        Write_Command_SSD1306(SSD1306_DISPLAYON)                     '--turn on oled panel

  #ENDIF

End Sub



'''Clears the GLCD screen
Sub GLCDCLS_SSD1306 ( Optional In  GLCDBackground as word = GLCDBackground )
 ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
  GLCD_yordinate = 0

    #ifndef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
      #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
        For SSD1306_BufferLocationCalc = 1 to GLCD_HEIGHT * GLCD_WIDTH / 8
            SSD1306_BufferAlias(SSD1306_BufferLocationCalc) = 0
        Next
      #endif
    #endif
    #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
      #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
        For SSD1306_BufferLocationCalc = 0 to 128
            SSD1306_BufferAlias(SSD1306_BufferLocationCalc) = 0
        Next
      #endif
    #endif

  '1.14 changed to transaction
  For SSD1306_BufferLocationCalc = 0 to GLCD_HEIGHT-1 step 8
    Cursor_Position_SSD1306 ( 0 , SSD1306_BufferLocationCalc )
    Open_Transaction_SSD1306
      For GLCDTemp = 0 to 127
        Write_Transaction_Data_SSD1306(GLCDBackground)
      Next
    Close_Transaction_SSD1306
  Next

  'Removed at 1.14. Retained for documentation only
    '  Cursor_Position_SSD1306 ( 0 , 0 )
    '  for SSD1306_BufferLocationCalc = 0 to GLCD_HEIGHT-1 step 8
    '    for GLCDTemp = 0 to 127
    '        Write_Data_SSD1306(GLCDBackground)
    '    Next
    '  next

  Cursor_Position_SSD1306 ( 0 , 0 )
  PrintLocX =0
  PrintLocY =0
End Sub

'''Draws a character at the specified location on the SSD1306 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0

Sub GLCDDrawChar_SSD1306(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This is now in four parts
  '1. Handler for GLCD LM mode
  '2. Preamble
  '3. GCB Font set handler
  '4. OLED Font set handler

'***** Handler for GLCD LM mode

   #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
      if GLCDfntDefaultSize = 1 then
        'test if character lies within current page
         GLCDY_Temp = CharLocY + 7
         Repeat 3
           Set C Off
           Rotate GLCDY_Temp Right
         End Repeat
         IF GLCDY_Temp <> _GLCDPage THEN
          GLCDY_Temp = GLCDY_Temp - 1
          IF GLCDY_Temp <> _GLCDPage THEN
            EXIT SUB
          END IF
         END IF
      end if
   #endif

'****** Preamble
    'invert colors if required
    if LineColour <> GLCDForeground  then
      'Inverted Colours
      GLCDBackground = 1
      GLCDForeground = 0
    end if

   dim CharCol, CharRow as word
   CharCode -= 15

    CharCol=0

    Cursor_Position_SSD1306 ( CharLocX , CharLocY )

  '1.14 Added transaction
   #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
    Open_Transaction_SSD1306
   #endif

'****** GCB Font set handler

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

          'Full Memory GLCD mode
          #ifndef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY         ' Same as code below. Repeated as the Define is the limitation
            CharRow=0
            For CurrCharRow = 1 to 8
                CharColS=0
                For Col=1 to GLCDfntDefaultsize
                      CharRowS=0
                      For Row=1 to GLCDfntDefaultsize
                          if CurrCharVal.0=1 then
                             PSet [word]CharLocX + CharCol + CharColS, [word]CharLocY + CharRow + CharRowS, LineColour
                          Else
                             PSet [word]CharLocX + CharCol + CharColS, [word]CharLocY + CharRow + CharRowS, GLCDBackground
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
          #endif

          '1.14 Low Memory GLCD mode
          #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE         ' Same as code above. Repeated as the Define is the limitation
            CharRow=0
            For CurrCharRow = 1 to 8
                CharColS=0
                For Col=1 to GLCDfntDefaultsize
                      CharRowS=0
                      For Row=1 to GLCDfntDefaultsize
                          GLCDY = [word]CharLocY + CharRow + CharRowS

                           if CurrCharVal.0=1 then
                              PSet [word]CharLocX + CharCol + CharColS, GLCDY, LineColour
                           Else
                              PSet [word]CharLocX + CharCol + CharColS, GLCDY, GLCDBackground
                           End if
                          'End if
                          CharRowS +=1
                          'Put out a white intercharacter pixel/space
                          if ( CharCol + CharColS ) = ( GLCDFontWidth * GLCDfntDefaultsize) - GLCDfntDefaultsize - 1 then
                          PSet [word]CharLocX + CharCol + CharColS + 1, [word]CharLocY + CharRow+CharRowS -1, GLCDBackground
                          end if
                      Next Row
                      CharColS +=1
                Next Col
              Rotate CurrCharVal Right
              CharRow +=GLCDfntDefaultsize
            Next
            CharCol +=GLCDfntDefaultsize
          #endif

          '1.12 Character GLCD mode
          ' Handles specific draw sequence. This caters for write only of a bit value. No read operation.
          #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY

            'Ensure this is not called with in Low memory mode
            #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE

                GLCDX = ( CharLocX + CurrCharCol -1 )

                 #IFDEF GLCD_PROTECTOVERRUN
                    'anything off screen with be rejected

                    if GLCDX => GLCD_WIDTH OR CharLocY => GLCD_HEIGHT Then
                        exit for
                    end if

                #ENDIF

                '1.14 Added transaction
                If LineColour = 1 Then
                 Write_Transaction_Data_SSD1306( CurrCharVal )
                else
                 Write_Transaction_Data_SSD1306( 255 - CurrCharVal )
                end if

            #endif

          #endif

        Next

    #endif


'****** OLED Font set handler
   #ifdef GLCD_OLED_FONT

        'Calc pointer to the OLED fonts

        Dim LocalCharCode as word

        'Set up the font information
        Select case GLCDfntDefaultSize
            case 1 'this is two font tables of an index and data
              CharCode = CharCode - 16
              ReadTable OLEDFont1Index, CharCode, LocalCharCode
              ReadTable OLEDFont1Data, LocalCharCode , COLSperfont
              GLCDFontWidth = COLSperfont + 1
              #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
                  'Only use the correct bits/columns
                  COLSperfont--
                #endif
              #endif
              ROWSperfont = 7


              #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
                  ROWSperfont = 1
                #endif
              #endif
            case 2 'this is one font table
              CharCode = CharCode - 17
              'Pointer to table of font elements
              LocalCharCode = (CharCode * 20)
              COLSperfont = 9  'which is really 10 as we start at 0

              ROWSperfont=15  'which is really 16 as we start at 0


              #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
                  ROWSperfont = 2
                #endif
              #endif

        End Select

        For CurrCharCol = 0 to COLSperfont 'number of columns in the font , with two row of data
          'Increment the table pointer
          LocalCharCode++
          Select case GLCDfntDefaultSize
              case 1
                ReadTable OLEDFont1Data, LocalCharCode, CurrCharVal

              #ifndef GLCD_Disable_OLED_FONT2
                  case 2
                    'Read this 20 times... (0..COLSperfont) [ * 2 ]
                    ReadTable OLEDFont2, LocalCharCode, CurrCharVal
              #endif
              #ifdef GLCD_Disable_OLED_FONT2
                  case 2
                    CurrCharVal = 255
              #endif

          End Select

          '1.21 Full GLCD mode
          #ifndef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY         ' Same as code below. Repeated as the Define is the limitation

            'Handle 16 pixels of height
            For CurrCharRow = 0 to ROWSperfont
                If CurrCharVal.0 = 0 Then
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
                Else
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDForeground
                End If

                Rotate CurrCharVal Right

                'Set to next row of date, a second row pixels
                if GLCDfntDefaultSize = 2 and CurrCharRow = 7 then
                  LocalCharCode++
                  #ifndef GLCD_Disable_OLED_FONT2
                      ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                  #endif
                  #ifdef GLCD_Disable_OLED_FONT2
                      CurrCharVal = 255
                  #endif

                end if

                if CurrCharCol = COLSperfont then
                    'It is the intercharacter space, put out one pixel row
                    'Put out a white intercharacter pixel/space
                     GLCDTemp = CharLocX + CurrCharCol
                     if GLCDfntDefaultSize = 2 then
                        GLCDTemp++
                     end if
                     PSet GLCDTemp , CharLocY + CurrCharRow, GLCDBackground
                end if

            Next


          #endif

          '1.21 Low Memory GLCD mode
          #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE         ' Same as code above. Repeated as the Define is the limitation

            'Handle 16 pixels of height
            For CurrCharRow = 0 to ROWSperfont
                If CurrCharVal.0 = 0 Then
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
                Else
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDForeground
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

                if CurrCharCol = COLSperfont then
                    'It is the intercharacter space, put out one pixel row
                    'Put out a white intercharacter pixel/space
                     GLCDTemp = CharLocX + CurrCharCol
                     if GLCDfntDefaultSize = 2 then
                        GLCDTemp++
                     end if
                     PSet GLCDTemp , CharLocY + CurrCharRow , GLCDBackground
                end if


            Next


          #endif

          '1.21 Character GLCD mode
          ' Handles specific draw sequence. This caters for write only of a bit value. No read operation.
          #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY

            'Ensure this is not called with in Low memory mode
            #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE

                GLCDX = ( CharLocX + CurrCharCol )

                 #IFDEF GLCD_PROTECTOVERRUN
                    'anything off screen with be rejected

                    if GLCDX => GLCD_WIDTH OR CharLocY => GLCD_HEIGHT Then
                        exit for
                    end if

                #ENDIF

                Close_Transaction_SSD1306
                Cursor_Position_SSD1306 ( GLCDX , CharLocY )

                Open_Transaction_SSD1306
                'Support for transaction ... write data out
                If LineColour = 1 Then
                 Write_Transaction_Data_SSD1306( CurrCharVal )
                else
                 Write_Transaction_Data_SSD1306( 255 - CurrCharVal )
                end if
                Close_Transaction_SSD1306
                'Read the next part of the char data
                if GLCDfntDefaultSize = 2 then
                    LocalCharCode++

                    #ifndef GLCD_Disable_OLED_FONT2
                      ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                    #endif
                    #ifdef GLCD_Disable_OLED_FONT2
                      CurrCharVal = 255
                    #endif

                    Cursor_Position_SSD1306 ( GLCDX , CharLocY + 8 )

                    Open_Transaction_SSD1306
                    'Support for transaction ... write data out
                    If LineColour = 1 Then
                     Write_Transaction_Data_SSD1306( CurrCharVal )
                    else
                     Write_Transaction_Data_SSD1306( 255 - CurrCharVal )
                    end if
                end if

            #endif

          #endif

        Next

    #endif



   '1.14 Added transaction
   #ifdef GLCD_TYPE_SSD1306_CHARACTER_MODE_ONLY
    Close_Transaction_SSD1306
   #endif


    'Restore
    GLCDBackground = 0
    GLCDForeground = 1

End Sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_SSD1306(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

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

  #if GLCD_TYPE = GLCD_TYPE_SSD1306 or GLCD_TYPE = GLCD_TYPE_SSD1306_32
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
'''@param GLCDColour State of pixel ( GLCDBackground | GLCDForeground )
Sub PSet_SSD1306(In GLCDX, In GLCDY, In GLCDColour As Word)

    'Set pixel at X, Y on LCD to State
    'X is 0 to 127
    'Y is 0 to 63
    'Origin in top left


    #if GLCD_TYPE = GLCD_TYPE_SSD1306 or GLCD_TYPE = GLCD_TYPE_SSD1306_32

        #IFDEF GLCD_PROTECTOVERRUN
            'anything off screen with be rejected
            if GLCDX => GLCD_WIDTH OR GLCDY => GLCD_HEIGHT Then
                exit sub
            end if

        #ENDIF

        '1.14 Addresses correct device horizonal page
        #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE

            'Is YPOS addressing the page we need?
            'SSD1306_BufferLocationCalc = GLCDY
            'Repeat 3
            '  Set C Off
            '  Rotate SSD1306_BufferLocationCalc Right
            'End Repeat

            'if SSD1306_BufferLocationCalc = _GLCDPage then
            'GLCDY_Temp = ( GLCDY / 8 )* GLCD_WIDTH
            'faster than /8
            GLCDY_Temp = GLCDY
            Repeat 3
              Set C Off
              Rotate GLCDY_Temp Right
            End Repeat

            if GLCDY_Temp = _GLCDPage then
              'Mod the YPOS to get the correct pixel with the page
              GLCDY = GLCDY mod 8
            Else
              'Exit if not the page we are looking for
              exit sub
            end if
            'buffer location in LOWMEMORY_GLCD_MODE always equals GLCDX + 1
            SSD1306_BufferLocationCalc = GLCDX + 1

        #endif

        'don't need to do these calculations for in LOWMEMORY_GLCD_MODE
        #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE

           'SSD1306_BufferLocationCalc = ( GLCDY / 8 )* GLCD_WIDTH
           'faster than /8
           SSD1306_BufferLocationCalc = GLCDY
           Repeat 3
             Set C Off
             Rotate SSD1306_BufferLocationCalc Right
           End Repeat

           'faster than * 128
           #if GLCD_WIDTH=128
             Set C Off
             Repeat 7
               Rotate SSD1306_BufferLocationCalc Left
             End Repeat
           #endif
           #if GLCD_WIDTH <> 128
             SSD1306_BufferLocationCalc = SSD1306_BufferLocationCalc * GLCD_WIDTH
           #endif
           SSD1306_BufferLocationCalc = GLCDX + SSD1306_BufferLocationCalc + 1

        #endif

          #IFDEF GLCD_PROTECTOVERRUN
              'anything beyond buffer boundary?
              'why? X = 127 and Y = 64 (Y is over 63!) will have passed first check....
              if SSD1306_BufferLocationCalc > GLCD_HEIGHT * GLCD_WIDTH Then
                  exit sub
              end if

          #ENDIF

          GLCDDataTemp = SSD1306_BufferAlias(SSD1306_BufferLocationCalc)

          'Change data to set/clear pixel
          GLCDBitNo = GLCDY And 7
          If GLCDColour.0 = 0 Then
            GLCDChange = 254
            Set C On
          Else
            GLCDChange = 1
            Set C Off
          End If
          Repeat GLCDBitNo
            Rotate GLCDChange Left
          End Repeat

          If GLCDColour.0 = 0 Then
             GLCDDataTemp = GLCDDataTemp And GLCDChange
          Else
             GLCDDataTemp = GLCDDataTemp Or GLCDChange
          End If

          'added 1.14 to isolate from full glcd mode
          #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
              'restore address the correct page by adjustng the Y
              GLCDY = GLCDY + ( 8 * _GLCDPage )
              if SSD1306_BufferAlias(SSD1306_BufferLocationCalc) <> GLCDDataTemp then
                SSD1306_BufferAlias(SSD1306_BufferLocationCalc) = GLCDDataTemp
              end if

          #endif

          'revised 1.14 to isolate from low memory mode
          #ifndef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE
            if SSD1306_BufferAlias(SSD1306_BufferLocationCalc) <> GLCDDataTemp then
                SSD1306_BufferAlias(SSD1306_BufferLocationCalc) = GLCDDataTemp
                Cursor_Position_SSD1306 ( GLCDX, GLCDY )
                Write_Data_SSD1306 ( GLCDDataTemp )
            end if
          #endif

    #endif



End Sub



Function PixelStatus_SSD1306(In GLCDX, In GLCDY )

    #if GLCD_TYPE = GLCD_TYPE_SSD1306
            'Select correct buffer element
            SSD1306_BufferLocationCalc = GLCDY
            'Divide by 8
            Repeat 3
              Set C Off
              Rotate SSD1306_BufferLocationCalc Right
            End Repeat
            SSD1306_BufferLocationCalc = SSD1306_BufferLocationCalc * GLCD_WIDTH
            SSD1306_BufferLocationCalc = GLCDX + SSD1306_BufferLocationCalc+1  'add 1 as we dont use element (0)
            GLCDDataTemp = SSD1306_BufferAlias(SSD1306_BufferLocationCalc)
            'Change data to examine pixel by rotating our bit to bit zero
            GLCDBitNo = GLCDY And 7
            Repeat GLCDBitNo
              Rotate GLCDDataTemp right
            End Repeat
            PixelStatus_SSD1306 = GLCDDataTemp.0
    #endif
  end function


'''Takes raw pixel positions and translates to XY char positions
'''@param X coordinate of pixel
'''@param Y coordinate of pixel
sub Cursor_Position_SSD1306( in LocX as byte, in LocY as byte )
  dim  PosCharX, PosCharX as Word


  ' PosCharY = LocY / 8
  ' faster than /8
  PosCharY = LocY
  Repeat 3
    Set C Off
    Rotate PosCharY Right
  End Repeat


  Write_Command_SSD1306( 0xB0 + PosCharY )   ' set page address
  PosCharX = ( LocX  & 0x0f )  ' lower nibble
  Write_Command_SSD1306( PosCharX )

  PosCharX = LocX
  Repeat 4
         Set C off
         Rotate PosCharX Right
  End Repeat
  PosCharX = ( PosCharX & 0x0F ) + 0x10
  Write_Command_SSD1306 ( PosCharX )

end sub


'''Stops all scrolling
sub stopscroll_SSD1306
    Write_Command_SSD1306(SSD1306_DEACTIVATE_SCROLL)
end sub


'''Activate a right handed scroll for rows start through stop
''' Hint, the display is 16 rows tall. To scroll the whole display, run:
''' startscrollright_SSD1306(0x00, 0x0F)
'''@param Start row
'''@param End row
'''@param Set time interval between each scroll step in terms of frame frequency
SUB startscrollright_SSD1306 ( IN SSD1306_start , IN SSD1306_stop, Optional In SSD1306_scrollspeed As byte = 0 )
  Write_Command_SSD1306(SSD1306_RIGHT_HORIZONTAL_SCROLL)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(SSD1306_start)
  Write_Command_SSD1306(SSD1306_scrollspeed)
  Write_Command_SSD1306(SSD1306_stop)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(0XFF)
  Write_Command_SSD1306(SSD1306_ACTIVATE_SCROLL)
end sub



'''Activate a left handed scroll for rows start through stop
'''Hint, the display is 16 rows tall. To scroll the whole display, run:
'''startscrollleft_SSD1306(0x00, 0x0F)
'''@param Start row
'''@param End row
'''@param Set time interval between each scroll step in terms of frame frequency
SUB startscrollleft_SSD1306 ( IN SSD1306_start , IN SSD1306_stop, Optional In SSD1306_scrollspeed As byte = 0 )
  Write_Command_SSD1306(SSD1306_LEFT_HORIZONTAL_SCROLL)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(SSD1306_start)
  Write_Command_SSD1306(SSD1306_scrollspeed)
  Write_Command_SSD1306(SSD1306_stop)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(0XFF)
  Write_Command_SSD1306(SSD1306_ACTIVATE_SCROLL)
end sub



'''Activate a diagright handed scroll for rows start through stop
'''Hint, the display is 16 rows tall. To scroll the whole display, run:
'''startscrolldiagright_SSD1306(0x00, 0x0F)
'''@param Start row
'''@param End row
'''@param Set time interval between each scroll step in terms of frame frequency
SUB startscrolldiagright_SSD1306 ( IN SSD1306_start , IN SSD1306_stop, Optional In SSD1306_scrollspeed As byte = 0 )
  Write_Command_SSD1306(SSD1306_SET_VERTICAL_SCROLL_AREA)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(GLCD_HEIGHT)
  Write_Command_SSD1306(SSD1306_VERTICAL_AND_RIGHT_HORIZONTAL_SCROLL)
  Write_Command_SSD1306(SSD1306_scrollspeed)
  Write_Command_SSD1306(SSD1306_start)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(SSD1306_stop)
  Write_Command_SSD1306(0X01)
  Write_Command_SSD1306(SSD1306_ACTIVATE_SCROLL)
end sub


'''Activate a diagleft handed scroll for rows start through stop
'''Hint, the display is 16 rows tall. To scroll the whole display, run:
'''startscrolldiagleft_SSD1306(0x00, 0x0F)
'''@param Start row
'''@param End row
'''@param Set time interval between each scroll step in terms of frame frequency
SUB startscrolldiagleft_SSD1306 ( IN SSD1306_start , IN SSD1306_stop, Optional In SSD1306_scrollspeed As byte = 0 )
  Write_Command_SSD1306(SSD1306_SET_VERTICAL_SCROLL_AREA)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(GLCD_HEIGHT)
  Write_Command_SSD1306(SSD1306_VERTICAL_AND_LEFT_HORIZONTAL_SCROLL)
  Write_Command_SSD1306(SSD1306_scrollspeed)
  Write_Command_SSD1306(SSD1306_start)
  Write_Command_SSD1306(0X00)
  Write_Command_SSD1306(SSD1306_stop)
  Write_Command_SSD1306(0X01)
  Write_Command_SSD1306(SSD1306_ACTIVATE_SCROLL)
end sub

'''Sets the constrast to select 1 out of 256 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
sub SetContrast_SSD1306 ( in dim_state )

  Write_Command_SSD1306(SSD1306_SETCONTRAST)
  Write_Command_SSD1306(dim_state)

end sub

'*********************Software SPI**********************

sub S4Wire_SSD1306(in SSD1306SendByte as byte)

  For clocks = 1 to 8
    If SSD1306SendByte.7 = 1 Then
      MOSI_SSD1306 = 1
    Else
      MOSI_SSD1306 = 0
    End if
    Rotate SSD1306SendByte Left Simple
    SCK_SSD1306 = 0
    SCK_SSD1306 = 1
  Next

end sub

'added 1.14 to support low memory mode

Macro  GLCD_Open_PageTransaction_SSD1306 ( Optional In _GLCDPagesL As byte = 0 , Optional In _GLCDPagesH As byte = 7 )
  #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE

      dim _GLCDPage as byte
      'Clear buffer
      for _GLCDPage = _GLCDPagesL to _GLCDPagesH    '_GLCDPage is a global variable - DO NOT CHANGE!!!

  #endif

end Macro


Macro  GLCD_Close_PageTransaction_SSD1306

  #ifdef GLCD_TYPE_SSD1306_LOWMEMORY_GLCD_MODE

          'Set cursor position
          Cursor_Position_SSD1306 ( 0, 8 * _GLCDPage )

          'Send the buffer to the device using transaction
          Open_Transaction_SSD1306

          for SSD1306_BufferLocationCalc = 1 to 128
             Write_Transaction_Data_SSD1306 SSD1306_BufferAlias(SSD1306_BufferLocationCalc)
             'Clear the buffer byte. We need it to be empty for the next page operation
             SSD1306_BufferAlias(SSD1306_BufferLocationCalc) = 0
          next

          Close_Transaction_SSD1306

      next

    #endif

end Macro


'added 1.14 to improved performance
Macro Open_Transaction_SSD1306

    '4wire not supported, see Write_Transaction_Data_SSD1306

     #ifdef I2C_DATA

       I2CStart
       I2CSend GLCD_I2C_Address
       I2CSend 0x40

     #endif

     #ifdef HI2C_DATA

       HI2CStart
       HI2CSend GLCD_I2C_Address
       HI2CSend 0x40

     #endif

     #ifdef HI2C2_DATA

       HI2C2Start
       HI2C2Send GLCD_I2C_Address
       HI2C2Send 0x40

     #endif

End Macro

'added 1.14 to improved performance
Macro Write_Transaction_Data_SSD1306 ( in SSD1306SendByte as byte )

        #ifdef S4Wire_DATA

          CS_SSD1306 = 0
          DC_SSD1306 = 1
          S4Wire_SSD1306 SSD1306SendByte
          DC_SSD1306 = 0
          CS_SSD1306 = 1

        #endif

        #ifdef I2C_DATA

         I2CSend SSD1306SendByte

        #endif

        #ifdef HI2C_DATA

         HI2CSend SSD1306SendByte

        #endif

        #ifdef HI2C2_DATA

         HI2C2Send SSD1306SendByte

        #endif

End Macro

'added 1.14 to improved performance
Macro Close_Transaction_SSD1306

    '4wire not supported, see Write_Transaction_Data_SSD1306

     #ifdef I2C_DATA

       I2CStop

     #endif

     #ifdef HI2C_DATA

       HI2CStop

     #endif

     #ifdef HI2C2_DATA

       HI2C2Stop

     #endif


End Macro
