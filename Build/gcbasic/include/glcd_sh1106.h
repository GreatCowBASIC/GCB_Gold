'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2017-2025 Marco Cariboni, Joseph Realmuto and Evan Venn

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
'
' 27/03/2017: Revised to fix initialisation issue from PIC when using Priority Startup
' 14/08/2017: Added low-RAM character only support (Joseph Realmuto)
' 15/08/2017: Revised to support I2C transaction control for low memory usage
' 17/08/2017: Added support for low-RAM GLCD mode (ported from SSD1306 code) - Joseph Realmuto
' 17/08/2017: Improved Pset for text mode
' 18/08/2017: Improved speed of GLCDDrawChar and Pset in low RAM GLCD mode (Joseph Realmuto)
' 21/07/2020: Added SPI support - software only
' 07/08/2020: Added #ifdef S4Wire_DATA isolation
' 26/03/2025: Added GLCD_OLED_FONT and improved inter character control to support Foreground & Background controls

'Notes:
' Supports SH1106 controller only.

'Hardware settings uses I2C
'#define GLCD_I2C_Address 0x78 '8 bit addresses
'#define GLCD_I2C_Address 0x7A
'#define GLCD_I2C_Address 0x7B

'Hardware settings uses SPI
'#DEFINE GLCD_DO  PORTC.5  'SPI
'#DEFINE GLCD_SCK PORTC.4  'SPI
'#DEFINE GLCD_DC PORTB.1 ' PIN AO
'#DEFINE GLCD_CS PORTC.3 ' PIN CS
'#DEFINE GLCD_RESET PORTC.2 ' PIN RST




'****************************************************************************************************************
#define SH1106_OLEDADJUSTMENT 0x02  ' required as the 132 bits are not all available.

#define SH1106_DISPLAYOFF 0xAE
#define SH1106_SETLOWCOLUMN 0x00
#define SH1106_SETHIGHCOLUMN 0x10
#define SH1106_SETSTARTLINE 0x40
#define SH1106_PAGEADDR   0xB0
#define SH1106_SETCONTRASTCRTL 0x81
#define SH1106_SEGREMAP 0xA0
#define SH1106_NORMALDISPLAY 0xA6
#define SH1106_INVERTDISPLAY 0xA7
#define SH1106_SETMULTIPLEX 0xA8
#define SH1106_CHARGEPUMP 0xAD
#define SH1106_EXTERNALVCC 0x8B
#define SH1106_PUMPVOLTAGE 0x30
#define SH1106_COMSCANINC 0xC0
#define SH1106_COMSCANDEC 0xC8
#define SH1106_SETDISPLAYOFFSET 0xD3
#define SH1106_SETDISPLAYCLOCKDIV 0xD5
#define SH1106_SETPRECHARGE 0xD9
#define SH1106_SETCOMPINS 0xDA
#define SH1106_SETVCOMDETECT 0xDB
#define SH1106_DISPLAYON 0xAF
#define SH1106_DISPLAYALLON_RESUME 0xA4
#define SH1106_DISPLAYALLON 0xA5


#startup InitGLCD_SH1106


'Setup code for SH1106 controllers

#script

    ' This script set the capabilities based upon the amount of RAM

    IF GLCD_TYPE = GLCD_TYPE_SH1106 THEN
      IF ChipRAM < 1024  THEN
         GLCD_TYPE_SH1106_CHARACTER_MODE_ONLY = TRUE
      END IF
    END IF

#endscript


#if GLCD_TYPE = GLCD_TYPE_SH1106

    dim SH1106_BufferLocationCalc as Word               ' mandated in main program for SH1106

    #ifndef GLCD_TYPE_SH1106_CHARACTER_MODE_ONLY
        Dim SH1106_BufferAlias(1024)
    #endif

    #ifdef GLCD_TYPE_SH1106_CHARACTER_MODE_ONLY
      #ifdef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE
        Dim SH1106_BufferAlias(128)
      #endif
    #endif

#endif

'''@hide
Sub Write_Command_SH1106 ( in SH1106SendByte as byte )

    #ifdef S4Wire_DATA

      CS_SH1106 = 1
      DC_SH1106 = 0
      CS_SH1106 = 0
      S4Wire_SSD1106 SH1106SendByte
      CS_SH1106 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address
      I2CSend 0x00
      I2CSend SH1106SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA

      HI2CStart
      HI2CSend GLCD_I2C_Address
      HI2CSend 0x00
      HI2CSend SH1106SendByte
      HI2CStop

    #endif

End Sub




'''@hide
Sub Write_Data_SH1106 ( in SH1106SendByte as byte )

    #ifdef S4Wire_DATA

      CS_SH1106 = 1
      DC_SH1106 = 1
      CS_SH1106 = 0
      S4Wire_SSD1106 SH1106SendByte
      CS_SH1106 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address
      I2CSend 0x40
      I2CSend SH1106SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA

      HI2CStart
      HI2CSend GLCD_I2C_Address
      HI2CSend 0x40
      HI2CSend SH1106SendByte
      HI2CStop

    #endif

End Sub


sub S4Wire_SSD1106(in SH1106SendByte as byte)

  SCK_SH1106 = 1
  For clocks = 1 to 8
    If SH1106SendByte.7 = 1 Then
      DO_SH1106 = 1
    Else
      DO_SH1106 = 0
    End if
    SCK_SH1106 = 0
    Rotate SH1106SendByte Left Simple
    SCK_SH1106 = 1
  Next

end sub

Sub InitGLCD_SH1106

      dim PrintLocX, PrintLocY as word

      #DEFINE DO_SH1106         GLCD_DO
      #DEFINE SCK_SH1106        GLCD_SCK
      #DEFINE DC_SH1106         GLCD_DC
      #DEFINE CS_SH1106         GLCD_CS
      #DEFINE RESET_SH1106      GLCD_RESET

      #IFDEF S4Wire_DATA   'therefore SOFTWARESPI
          DIR DO_SH1106 out
          DIR SCK_SH1106  out
          DIR DC_SH1106 out
          DIR CS_SH1106 out
          DIR RESET_SH1106 out
      #ENDIF


      ' added wait time to allow power supplies to stabilize
      wait 255 ms

      #IFDEF S4Wire_DATA   'therefore SOFTWARESPI
          'Reset display
          Set RESET_SH1106 On
          Wait 150 ms
          'Reset sequence (lower line for at least 10 us)
          Set RESET_SH1106 Off
          Wait 150 us
          Set RESET_SH1106 On
          Wait 150 ms
      #ENDIF

      #IFDEF HI2C_DATA
             HI2CMode Master
             Wait 10 ms
      #ENDIF

   'Setup code for SH1106 controllers
   'Init sequence for 132x64 OLED module

    Write_Command_SH1106(SH1106_DISPLAYOFF)                    ' 0xAE
    Write_Command_SH1106(SH1106_SETLOWCOLUMN)                  ' 0x02
    Write_Command_SH1106(SH1106_SETHIGHCOLUMN)                 ' 0x10
    Write_Command_SH1106(SH1106_SETSTARTLINE)                  ' 0x40
    Write_Command_SH1106(SH1106_PAGEADDR)                      ' 0xB0

    Write_Command_SH1106(SH1106_SETCONTRASTCRTL)               ' 0x81
    Write_Command_SH1106(0x80)                                 ' Contrast = 128

    Write_Command_SH1106(SH1106_SEGREMAP | 0x1)                ' 0xA1
    Write_Command_SH1106(SH1106_COMSCANINC)
    Write_Command_SH1106(SH1106_NORMALDISPLAY)                 ' 0xA6

    Write_Command_SH1106(SH1106_SETMULTIPLEX)                  ' 0xA8
    Write_Command_SH1106(0x3F)                   ' Duty = 1/32

    Write_Command_SH1106(SH1106_CHARGEPUMP)                    ' 0xAD
    Write_Command_SH1106(SH1106_EXTERNALVCC)                   ' 0x8B
    Write_Command_SH1106(SH1106_PUMPVOLTAGE)                   ' 0x30
    Write_Command_SH1106(SH1106_COMSCANDEC)                    ' 0xC8

    Write_Command_SH1106(SH1106_SETDISPLAYOFFSET)              ' 0xD3
    Write_Command_SH1106(0x00)                                 ' No offset(Y position)
    Write_Command_SH1106(0x00)                                 ' No offset(Y position)

    Write_Command_SH1106(SH1106_SETDISPLAYCLOCKDIV)            ' 0xD5
    Write_Command_SH1106(0x50)                                 ' Suggested ratio 0x50

    Write_Command_SH1106(SH1106_SETPRECHARGE)                  ' 0xD9
    Write_Command_SH1106(0x22)

    Write_Command_SH1106(SH1106_SETCOMPINS)                    ' 0xDA
    Write_Command_SH1106(0x12)

    Write_Command_SH1106(SH1106_SETVCOMDETECT)                 ' 0xDB
    Write_Command_SH1106(0x40)


    Write_Command_SH1106(SH1106_DISPLAYALLON_RESUME)           ' 0xA4
    Write_Command_SH1106(SH1106_DISPLAYON)                     ' 0xAF Turn ON oled panel

    'Default Colours
    #ifdef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
    #endif

    #ifndef DEFAULT_GLCDBACKGROUND
      GLCDBACKGROUND = TFT_BLACK
    #endif  

    GLCDForeground = TFT_WHITE

    #ifndef GLCD_OLED_FONT
      GLCDFontWidth = 6
    #endif

    #ifdef GLCD_OLED_FONT
      GLCDFontWidth = 5
    #endif

    GLCDfntDefault = 0
    GLCDfntDefaultsize = 1
    GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn

    'Clear screen
    GLCDCLS_SH1106

End Sub


'''Clears the GLCD screen
Sub GLCDCLS_SH1106 ( Optional In  GLCDBackground as word = GLCDBackground )
  ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
  GLCD_yordinate = 0

    #ifndef GLCD_TYPE_SH1106_CHARACTER_MODE_ONLY
      #ifndef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE
        For SH1106_BufferLocationCalc = 1 to GLCD_HEIGHT * GLCD_WIDTH / 8
            SH1106_BufferAlias(SH1106_BufferLocationCalc) = 0
        Next
      #endif
    #endif
    #ifdef GLCD_TYPE_SH1106_CHARACTER_MODE_ONLY
      #ifdef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE
        For SH1106_BufferLocationCalc = 0 to 128
            SH1106_BufferAlias(SH1106_BufferLocationCalc) = 0
        Next
      #endif
    #endif

    #IFNDEF S4Wire_DATA
        For SH1106_BufferLocationCalc = 0 to GLCD_HEIGHT-1 step 8
          Cursor_Position_SH1106 ( 0 , SH1106_BufferLocationCalc )
          Open_Transaction_SH1106
            For GLCDTemp = 0 to 127
              Write_Transaction_Data_SH1106(GLCDBackground)
            Next
          Close_Transaction_SH1106
        Next
    #ELSE
        Cursor_Position_SH1106 ( 0 , 0 )
      for SH1106_BufferLocationCalc = 0 to GLCD_HEIGHT-1 step 8
        for GLCDTemp = 0 to 127
            Write_Data_SH1106(GLCDBackground)
        Next
      next
    #ENDIF

  Cursor_Position_SH1106 ( 0 , 0 )
  PrintLocX =0
  PrintLocY =0

End Sub

'''Draws a character at the specified location on the SH1106 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_SH1106(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

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

        //~ major change - this was 1 to 5 which is the character width,
        //~ but, this means when the GLCDFontWidth is padded out by the increment to the next char
        //~ there is no PSET/COLOR control.  So, use GLCDFontWidth + 1 to control the color in the inter-char pixels
        For CurrCharCol = 1 to GLCDFontWidth + 1
          Select Case CurrCharCol
            Case 1: ReadTable GLCDCharCol3, CharCode, CurrCharVal
            Case 2: ReadTable GLCDCharCol4, CharCode, CurrCharVal
            Case 3: ReadTable GLCDCharCol5, CharCode, CurrCharVal
            Case 4: ReadTable GLCDCharCol6, CharCode, CurrCharVal
            Case 5: ReadTable GLCDCharCol7, CharCode, CurrCharVal
            Case Else: CurrCharVal = 0  //~ The interchar PSET value
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
Sub FilledBox_SH1106(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

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

  #if GLCD_TYPE = GLCD_TYPE_SH1106
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
Sub PSet_SH1106(In GLCDX, In GLCDY, In GLCDColour As Word)

    'Set pixel at X, Y on LCD to State
    'X is 0 to 127
    'Y is 0 to 63
    'Origin in top left

  #if GLCD_TYPE = GLCD_TYPE_SH1106

          #IFDEF GLCD_PROTECTOVERRUN
            'anything off screen with be rejected
            if GLCDX => GLCD_WIDTH OR GLCDY => GLCD_HEIGHT Then
                exit sub
            end if

          #ENDIF

          #ifdef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE

              'Is YPOS addressing the page we need?
              'SH1106_BufferLocationCalc = GLCDY
              'Repeat 3
              '  Set C Off
              '  Rotate SH1106_BufferLocationCalc Right
              'End Repeat

              'if SH1106_BufferLocationCalc = _GLCDPage then
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
              SH1106_BufferLocationCalc = GLCDX + 1

          #endif

          'don't need to do these calculations in LOWMEMORY_GLCD_MODE
          #ifndef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE

          'SH1106_BufferLocationCalc = ( GLCDY / 8 )* GLCD_WIDTH
          'faster than /8
          SH1106_BufferLocationCalc = GLCDY
          Repeat 3
            Set C Off
            Rotate SH1106_BufferLocationCalc Right
          End Repeat

          'faster than * 128
          #if GLCD_WIDTH=128
            Set C Off
            Repeat 7
              Rotate SH1106_BufferLocationCalc Left
            End Repeat
          #endif
          #if GLCD_WIDTH <> 128
            SH1106_BufferLocationCalc = SH1106_BufferLocationCalc * GLCD_WIDTH
          #endif
          SH1106_BufferLocationCalc = GLCDX + SH1106_BufferLocationCalc + 1
          'unoptimised is shown on line below.  All the code above... just makes this faster!
          ' SH1106_BufferLocationCalc = ( GLCDY / 8 )* GLCD_WIDTH

          #endif

          #IFDEF GLCD_PROTECTOVERRUN
              'anything beyond buffer boundary?
              'why? X = 127 and Y = 64 (Y is over 63!) will have passed first check....
              if SH1106_BufferLocationCalc > GLCD_HEIGHT * GLCD_WIDTH Then
                  exit sub
              end if

          #ENDIF

          GLCDDataTemp = SH1106_BufferAlias(SH1106_BufferLocationCalc)

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

          #ifdef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE
              'restore address the correct page by adjustng the Y
              GLCDY = GLCDY + ( 8 * _GLCDPage )
              if SH1106_BufferAlias(SH1106_BufferLocationCalc) <> GLCDDataTemp then
                SH1106_BufferAlias(SH1106_BufferLocationCalc) = GLCDDataTemp
              end if

          #endif

          #ifndef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE
            if SH1106_BufferAlias(SH1106_BufferLocationCalc) <> GLCDDataTemp then
                SH1106_BufferAlias(SH1106_BufferLocationCalc) = GLCDDataTemp
                Cursor_Position_SH1106 ( GLCDX, GLCDY )
                Write_Data_SH1106  ( GLCDDataTemp )
            end if
          #endif

    #endif

End Sub


'''Takes raw pixel positions and translates to XY char positions
'''@param X coordinate of pixel
'''@param Y coordinate of pixel
sub Cursor_Position_SH1106( in LocX as byte, in LocY as byte )

  dim  PosCharX, PosCharY as Word

  'PosCharY = LocY / 8 - optimised by rotate
  PosCharY = LocY
  Repeat 3
    Set C Off
    Rotate PosCharY Right
  end Repeat

  Write_Command_SH1106( 0xB0 + PosCharY )   ' set page address
  PosCharX = ( ( LocX + SH1106_OLEDADJUSTMENT ) & 0x0f )  ' lower nibble
  Write_Command_SH1106( PosCharX )

  PosCharX = LocX + SH1106_OLEDADJUSTMENT
  Repeat 4
         Set C off
         Rotate PosCharX Right
  End Repeat
  PosCharX = ( PosCharX & 0x0F ) + 0x10
  Write_Command_SH1106 ( PosCharX )

end sub

sub GLCDRotate_SSH1106 ( in GLCDRotateState )

  select case GLCDRotateState
        case LANDSCAPE
            Write_Command_SH1106(SH1106_SEGREMAP | 0x01)
            Write_Command_SH1106(SH1106_COMSCANINC | 0x08)
            GLCDDeviceWidth = GLCD_HEIGHT - 1
            GLCDDeviceHeight = GLCD_WIDTH - 1

        case LANDSCAPE_REV
            Write_Command_SH1106(SH1106_SEGREMAP )
            Write_Command_SH1106(SH1106_COMSCANINC )
            GLCDDeviceWidth = GLCD_HEIGHT - 1
            GLCDDeviceHeight = GLCD_WIDTH - 1

        case else
            Write_Command_SH1106(SH1106_SEGREMAP | 0x01)
            Write_Command_SH1106(SH1106_COMSCANINC| 0x08)
            GLCDDeviceWidth = GLCD_WIDTH - 1
            GLCDDeviceHeight = GLCD_HEIGHT - 1
  end select

end Sub

'''Sets the constrast to select 1 out of 256 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value

sub GLCDSetContrast_SSH1106 ( in dim_state )

  'Write a word value
  Write_Command_SH1106 ( SH1106_SETCONTRASTCRTL )
  Write_Command_SH1106(dim_state)

end sub


'''Set Display in normal mode
sub GLCDSetDisplayNormalMode_SSH1106
    Write_Command_SH1106(SH1106_SEGREMAP)            ' 0xA1
    Write_Command_SH1106(SH1106_NORMALDISPLAY)       ' 0xA6
end sub

'''Set Display in invert mode
sub GLCDSetDisplayInvertMode_SSH1106
    Write_Command_SH1106(SH1106_SEGREMAP)            ' 0xA1
    Write_Command_SH1106(SH1106_INVERTDISPLAY)       ' 0xA7
end sub

Macro  GLCD_Open_PageTransaction_SH1106 ( Optional In _GLCDPagesL As byte = 0 , Optional In _GLCDPagesH As byte = 7 )
  #ifdef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE

      dim _GLCDPage as byte
      'Clear buffer
      for _GLCDPage = _GLCDPagesL to _GLCDPagesH    '_GLCDPage is a global variable - DO NOT CHANGE!!!

  #endif

end Macro

Macro  GLCD_Close_PageTransaction_SH1106

  #ifdef GLCD_TYPE_SH1106_LOWMEMORY_GLCD_MODE

          'Set cursor position
          Cursor_Position_SH1106 ( 0, 8 * _GLCDPage )

          'Send the buffer to the device using transaction
          Open_Transaction_SH1106

          for SH1106_BufferLocationCalc = 1 to 128
             Write_Transaction_Data_SH1106 SH1106_BufferAlias(SH1106_BufferLocationCalc)
             'Clear the buffer byte. We need it to be empty for the next page operation
             SH1106_BufferAlias(SH1106_BufferLocationCalc) = 0
          next

          Close_Transaction_SH1106

      next

    #endif

end Macro

Macro Open_Transaction_SH1106

    '4wire not supported, see Write_Transaction_Data_SH1106

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

End Macro

Macro Write_Transaction_Data_SH1106 ( in SH1106SendByte as byte )

        #ifdef S4Wire_DATA

          CS_SH1106 = 0
          DC_SH1106 = 1
          S4Wire_SSD1106 SH1106SendByte
          DC_SH1106 = 0
          CS_SH1106 = 1

        #endif

        #ifdef I2C_DATA

         I2CSend SH1106SendByte

        #endif

        #ifdef HI2C_DATA

         HI2CSend SH1106SendByte

        #endif

End Macro

Macro Close_Transaction_SH1106

    '4wire not supported, see Write_Transaction_Data_SH1106

     #ifdef I2C_DATA

       I2CStop

     #endif

     #ifdef HI2C_DATA

       HI2CStop

     #endif

End Macro
