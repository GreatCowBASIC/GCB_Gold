'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2024 Evan Venn

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
' Supports ST7567 controller only.

'Hardware settings using I2C
'  #define GLCD_TYPE GLCD_TYPE_ST7567
'  #define GLCD_I2C_Address 0x7E
'Hardware settings using SPI (4Wire,MOSI(D1),SCK(D0),DC,CS)
'  #define GLCD_TYPE GLCD_TYPE_ST7567
'  #define S4Wire_Data

' 1.00 Initial release - based on ST7567 library
' 1.01 Changed HWI2C error message in script.  HW IC2 does work... just not on the K-Mode I2C modules.
'      Add ST7567_BIAS controls

// 
#DEFINE ST7567_DISPLAY_ON          0xAF
#DEFINE ST7567_DISPLAY_OFF         0xAE
#DEFINE ST7567_SET_START_LINE      0x40
#DEFINE ST7567_SET_PAGE            0xB0
#DEFINE ST7567_COLUMN_UPPER        0x10
#DEFINE ST7567_COLUMN_LOWER        0x00
#DEFINE ST7567_SET_ADC_NORMAL      0xA0
#DEFINE ST7567_SET_ADC_REVERSE     0xA1
#DEFINE ST7567_SET_COL_NORMAL      0xC0
#DEFINE ST7567_SET_COL_REVERSE     0xC8
#DEFINE ST7567_SET_DISPLAY_NORMAL  0xA6
#DEFINE ST7567_SET_DISPLAY_REVERSE 0xA7
#DEFINE ST7567_SET_ALLPX_ON        0xA5
#DEFINE ST7567_SET_ALLPX_NORMAL    0xA4
#DEFINE ST7567_SET_BIAS_9          0xA2
#DEFINE ST7567_SET_BIAS_7          0xA3
#DEFINE ST7567_DISPLAY_RESET       0xE2
#DEFINE ST7567_NOP                 0xE3
#DEFINE ST7567_TEST                0xF0   'Exit this mode with ST7567_NOP
#DEFINE ST7567_SET_POWER           0x28
#DEFINE ST7567_SET_RESISTOR_RATIO  0x20
#DEFINE ST7567_SET_CONTRAST        0x81





#startup InitGLCD_ST7567, 100

'Setup code for ST7567 controllers
  #script     ' This script set the capabilities based upon the amount of RAM

    IGNORE_SPECIFIED_GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY = 0
    if IGNORE_GLCD_TYPE_ST7567_LOW_MEMORY_WARNINGS then
      IGNORE_SPECIFIED_GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY = 1
    end if

     if GLCD_TYPE = GLCD_TYPE_ST7567 then
       If ChipRAM < 1024  Then
           GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY = TRUE
           GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE = TRUE

          If ChipRAM < 225  Then
            If ChipFamily = 14 Then
              Error "Selected MCU has insufficient contiguous RAM to support GLCD operations"
            End if
          End if
          If ChipRAM > 225  Then
            if NODEF(IGNORE_SPECIFIED_GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY) then
              Warning "Memory < 1024 bytes."
              Warning "Selected MCU requires use of GLCD Open&Close Page Transaction."
              Warning "See Help for usage."
              Warning "Define a constant IGNORE_GLCD_TYPE_ST7567_LOW_MEMORY_WARNINGS to remove this message."
            end if
          End if
       End If
     end if

     if GLCD_TYPE = GLCD_TYPE_ST7567_32 then
       If ChipRAM < 512  Then
           GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY = TRUE
           GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE = TRUE
           If ChipRAM < 256  Then
              Error "Chip has insufficient RAM to support this specific GLCD"
           End if
       End If
     end if

    If NODEF( GLCD_I2C_ADDRESS) Then
        GLCD_I2C_Address = 0x7E
    End If

    if NODEF(ST7567_BIAS) Then
      ST7567_BIAS = ST7567_SET_BIAS_7
    end if

    If GLCD_TYPE = GLCD_TYPE_ST7567 Then
      IF DEF(HI2C_DATA) Then
          If BIT(I2C1CON0_EN) Then
            Error "Hardware I2C not supported for GLCD_TYPE_ST7567 when using this specific chip"
            Error "  Only software I2C supported"
          End If
      End If
    End if


   #endscript

   dim ST7567_BufferLocationCalc as Word               ' mandated in main program for ST7567
   'dim GLCDY_Temp as Integer alias ST7567_BufferLocationCalc_h, ST7567_BufferLocationCalc
   'ST7567_BufferLocationCalc = 1
   'GLCDY_Temp = -1
   'dim GLCDY_Temp as Integer alias ST7567_BufferLocationCalc_h, ST7567_BufferLocationCalc

   #ifndef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
     #if GLCD_TYPE = GLCD_TYPE_ST7567
       asm showdebug  GGLCD ST7567 buffer is 1024bytes
       Dim ST7567_BufferAlias(1024)
     #endif
     #if GLCD_TYPE = GLCD_TYPE_ST7567_32
       asm showdebug  GLCD ST7567 buffer is 512bytes
       Dim ST7567_BufferAlias(512)
     #endif
   #endif

   #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
     #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
       asm showdebug  GLCD ST7567 buffer is 128bytes
       Dim ST7567_BufferAlias(128)
     #endif
   #endif


'''@hide
Sub Write_Command_ST7567 ( in ST7567SendByte as byte )

    #ifdef S4Wire_DATA

      CS_ST7567 = 0
      DC_ST7567 = 0
      S4Wire_ST7567 ST7567SendByte
      DC_ST7567 = 1
      CS_ST7567 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address
      I2CSend 0x00
      I2CSend ST7567SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA

      HI2CStart
      HI2CSend GLCD_I2C_Address
      HI2CSend 0x00
      HI2CSend ST7567SendByte
      HI2CStop

    #endif

    #ifdef HI2C2_DATA

      HI2C2Start
      HI2C2Send GLCD_I2C_Address
      HI2C2Send 0x00
      HI2C2Send ST7567SendByte
      HI2C2Stop

    #endif

End Sub

'''@hide
Sub Write_Data_ST7567 ( in ST7567SendByte as byte )

    #ifdef S4Wire_DATA

      CS_ST7567 = 0
      DC_ST7567 = 1
      S4Wire_ST7567 ST7567SendByte
      DC_ST7567 = 0
      CS_ST7567 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address
      I2CSend 0x40
      I2CSend ST7567SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA

      HI2CStart
      HI2CSend GLCD_I2C_Address
      HI2CSend 0x40
      HI2CSend ST7567SendByte
      HI2CStop

    #endif

    #ifdef HI2C2_DATA

      HI2C2Start
      HI2C2Send GLCD_I2C_Address
      HI2C2Send 0x40
      HI2C2Send ST7567SendByte
      HI2C2Stop

    #endif

End Sub


'''@hide
Sub InitGLCD_ST7567

    #IF ( GLCD_TYPE  = GLCD_TYPE_ST7567_32 ) or ( GLCD_TYPE  = GLCD_TYPE_ST7567 )then
        'Colours set these first
        GLCDBackground = 0
        GLCDForeground = 1
        GLCDFontWidth = 5
        GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn
        dim PrintLocX, PrintLocY as word

        GLCDfntDefault = 0
        GLCDfntDefaultsize = 1
        wait 100 ms                    'added to ensure the charge pump and power is operational.
        #IFDEF HI2C_DATA
            HI2CMode Master
            Wait 15 ms               'wait for power-up and reset
        #ENDIF

        #IFDEF HI2C2_DATA
            HI2C2Mode Master
            Wait 15 ms                'wait for power-up and reset
        #ENDIF

        #ifdef S4Wire_DATA
            dir MOSI_ST7567 Out
            dir SCK_ST7567 Out
            dir DC_ST7567 Out
            dir CS_ST7567 Out
            dir RES_ST7567 Out
            RES_ST7567 = 0
            wait 10 us
            RES_ST7567 = 1
        #endif

        'Setup code for ST7567 controllers
        'Init sequence for 128x64 GLCD
        Write_Command_ST7567(0xe2)            	        // soft reset
        wait 1 s
        Write_Command_ST7567(0xae)		                // display off
        Write_Command_ST7567(0x40)		                // set display start line to 0
        
        Write_Command_ST7567(0xa0)		                // ADC 
        Write_Command_ST7567(0xc0)		                // common output mode
        

        Write_Command_ST7567(0xa6)		                // display normal, bit val 0: LCD pixel off.
        Write_Command_ST7567(ST7567_BIAS)		                
        Write_Command_ST7567(0x28|4)		                // all power  control circuits on
        Wait 50 ms
        Write_Command_ST7567(0x28|6)		                // all power  control circuits on
        Wait 50 ms
        Write_Command_ST7567(0x28|7)		                // all power  control circuits on
        Wait 50 ms
        
        Write_Command_ST7567(0x24)		                // v0 voltage resistor ratio, taken from issue 657

        // Other commands        
            // Write_Command_ST7567(0xae)		                // display off
            // Write_Command_ST7567(0xa5)		                // enter powersafe: all pixel on, issue 142    

        Write_Command_ST7567(ST7567_DISPLAY_ON)              // turn on oled panel


        'Clear screen Here
        GLCDCLS_ST7567

    #ENDIF

End Sub



'''Clears the GLCD screen
Sub GLCDCLS_ST7567 ( Optional In  GLCDBackground as word = GLCDBackground )
 ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
  GLCD_yordinate = 0

    #ifndef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
      #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
        For ST7567_BufferLocationCalc = 1 to GLCD_HEIGHT * GLCD_WIDTH / 8
            ST7567_BufferAlias(ST7567_BufferLocationCalc) = 0
        Next
      #endif
    #endif
    #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
      #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
        For ST7567_BufferLocationCalc = 0 to 131
            ST7567_BufferAlias(ST7567_BufferLocationCalc) = 0
        Next
      #endif
    #endif


  For ST7567_BufferLocationCalc = 0 to GLCD_HEIGHT-1 step 8
    Cursor_Position_ST7567 ( 0 , ST7567_BufferLocationCalc )
    Open_Transaction_ST7567
      For GLCDTemp = 0 to 131
        Write_Transaction_Data_ST7567(GLCDBackground)
      Next
    Close_Transaction_ST7567
  Next

  Cursor_Position_ST7567 ( 0 , 0 )
  PrintLocX =0
  PrintLocY =0
End Sub

'''Draws a character at the specified location on the ST7567 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0

Sub GLCDDrawChar_ST7567(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This is now in four parts
  '1. Handler for GLCD LM mode
  '2. Preamble
  '3. GCB Font set handler
  '4. OLED Font set handler

'***** Handler for GLCD LM mode

   #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
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

    Cursor_Position_ST7567 ( CharLocX , CharLocY )

  '1.14 Added transaction
   #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
    Open_Transaction_ST7567
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
          #ifndef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY         ' Same as code below. Repeated as the Define is the limitation
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
          #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE         ' Same as code above. Repeated as the Define is the limitation
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
          #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY

            'Ensure this is not called with in Low memory mode
            #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE

                GLCDX = ( CharLocX + CurrCharCol -1 )

                 #IFDEF GLCD_PROTECTOVERRUN
                    'anything off screen with be rejected

                    if GLCDX => GLCD_WIDTH OR CharLocY => GLCD_HEIGHT Then
                        exit for
                    end if

                #ENDIF

                '1.14 Added transaction
                If LineColour = 1 Then
                 Write_Transaction_Data_ST7567( CurrCharVal )
                else
                 Write_Transaction_Data_ST7567( 255 - CurrCharVal )
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
              #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
                  'Only use the correct bits/columns
                  COLSperfont--
                #endif
              #endif
              ROWSperfont = 7


              #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
                  ROWSperfont = 1
                #endif
              #endif
            case 2 'this is one font table
              CharCode = CharCode - 17
              'Pointer to table of font elements
              LocalCharCode = (CharCode * 20)
              COLSperfont = 9  'which is really 10 as we start at 0

              ROWSperfont=15  'which is really 16 as we start at 0


              #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
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
          #ifndef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY         ' Same as code below. Repeated as the Define is the limitation

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
          #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE         ' Same as code above. Repeated as the Define is the limitation

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
          #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY

            'Ensure this is not called with in Low memory mode
            #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE

                GLCDX = ( CharLocX + CurrCharCol )

                 #IFDEF GLCD_PROTECTOVERRUN
                    'anything off screen with be rejected

                    if GLCDX => GLCD_WIDTH OR CharLocY => GLCD_HEIGHT Then
                        exit for
                    end if

                #ENDIF

                Close_Transaction_ST7567
                Cursor_Position_ST7567 ( GLCDX , CharLocY )

                Open_Transaction_ST7567
                'Support for transaction ... write data out
                If LineColour = 1 Then
                 Write_Transaction_Data_ST7567( CurrCharVal )
                else
                 Write_Transaction_Data_ST7567( 255 - CurrCharVal )
                end if
                Close_Transaction_ST7567
                'Read the next part of the char data
                if GLCDfntDefaultSize = 2 then
                    LocalCharCode++

                    #ifndef GLCD_Disable_OLED_FONT2
                      ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                    #endif
                    #ifdef GLCD_Disable_OLED_FONT2
                      CurrCharVal = 255
                    #endif

                    Cursor_Position_ST7567 ( GLCDX , CharLocY + 8 )

                    Open_Transaction_ST7567
                    'Support for transaction ... write data out
                    If LineColour = 1 Then
                     Write_Transaction_Data_ST7567( CurrCharVal )
                    else
                     Write_Transaction_Data_ST7567( 255 - CurrCharVal )
                    end if
                end if

            #endif

          #endif

        Next

    #endif



   '1.14 Added transaction
   #ifdef GLCD_TYPE_ST7567_CHARACTER_MODE_ONLY
    Close_Transaction_ST7567
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
Sub FilledBox_ST7567(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

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

  #if GLCD_TYPE = GLCD_TYPE_ST7567 or GLCD_TYPE = GLCD_TYPE_ST7567_32
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
Sub PSet_ST7567(In GLCDX, In GLCDY, In GLCDColour As Word)

    'Set pixel at X, Y on LCD to State
    'X is 0 to 127
    'Y is 0 to 63
    'Origin in top left

    #ifndef  GLCDDIRECTION
    'GLCDDIRECTION
        GLCDX=127-GLCDX
    #else
    'GLCDDIRECTION
        GLCDY=63-GLCDY
    #endif


    Dim GLCDBitNo, GLCDChange, GLCDDataTemp as Byte

    #if GLCD_TYPE = GLCD_TYPE_ST7567 or GLCD_TYPE = GLCD_TYPE_ST7567_32

        #IFDEF GLCD_PROTECTOVERRUN
            'anything off screen with be rejected
            if GLCDX => GLCD_WIDTH OR GLCDY => GLCD_HEIGHT Then
                exit sub
            end if
        #ENDIF

        '1.14 Addresses correct device horizonal page
        #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE

            'Is YPOS addressing the page we need?
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
            ST7567_BufferLocationCalc = GLCDX + 1

        #endif

        'don't need to do these calculations for in LOWMEMORY_GLCD_MODE
        #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE

           'ST7567_BufferLocationCalc = ( GLCDY / 8 )* GLCD_WIDTH
           'faster than /8
           ST7567_BufferLocationCalc = GLCDY
           Repeat 3
             Set C Off
             Rotate ST7567_BufferLocationCalc Right
           End Repeat

           'faster than * 128
           #if GLCD_WIDTH=128
             Set C Off
             Repeat 7
               Rotate ST7567_BufferLocationCalc Left
             End Repeat
           #endif
           #if GLCD_WIDTH <> 128
             ST7567_BufferLocationCalc = ST7567_BufferLocationCalc * GLCD_WIDTH
           #endif
           ST7567_BufferLocationCalc = GLCDX + ST7567_BufferLocationCalc + 1

        #endif

          #IFDEF GLCD_PROTECTOVERRUN
              'anything beyond buffer boundary?
              'why? X = 127 and Y = 64 (Y is over 63!) will have passed first check....
              if ST7567_BufferLocationCalc > GLCD_HEIGHT * GLCD_WIDTH Then
                  exit sub
              end if

          #ENDIF

          GLCDDataTemp = ST7567_BufferAlias(ST7567_BufferLocationCalc)

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
          #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
              'restore address the correct page by adjustng the Y
              GLCDY = GLCDY + ( 8 * _GLCDPage )
              if ST7567_BufferAlias(ST7567_BufferLocationCalc) <> GLCDDataTemp then
                ST7567_BufferAlias(ST7567_BufferLocationCalc) = GLCDDataTemp
              end if

          #endif

          'revised 1.14 to isolate from low memory mode
          #ifndef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE
            if ST7567_BufferAlias(ST7567_BufferLocationCalc) <> GLCDDataTemp then
                ST7567_BufferAlias(ST7567_BufferLocationCalc) = GLCDDataTemp
                Cursor_Position_ST7567 ( GLCDX + ST7567_XOFFSET, GLCDY )
                Write_Data_ST7567 ( GLCDDataTemp )
            end if
          #endif

    #endif



End Sub



Function PixelStatus_ST7567(In GLCDX, In GLCDY )

    #if GLCD_TYPE = GLCD_TYPE_ST7567
            'Select correct buffer element
            ST7567_BufferLocationCalc = GLCDY
            'Divide by 8
            Repeat 3
              Set C Off
              Rotate ST7567_BufferLocationCalc Right
            End Repeat
            ST7567_BufferLocationCalc = ST7567_BufferLocationCalc * GLCD_WIDTH
            ST7567_BufferLocationCalc = GLCDX + ST7567_BufferLocationCalc+1  'add 1 as we dont use element (0)
            GLCDDataTemp = ST7567_BufferAlias(ST7567_BufferLocationCalc)
            'Change data to examine pixel by rotating our bit to bit zero
            GLCDBitNo = GLCDY And 7
            Repeat GLCDBitNo
              Rotate GLCDDataTemp right
            End Repeat
            PixelStatus_ST7567 = GLCDDataTemp.0
    #endif
  end function


'''Takes raw pixel positions and translates to XY char positions
'''@param X coordinate of pixel
'''@param Y coordinate of pixel
sub Cursor_Position_ST7567( in LocX as byte, in LocY as byte )
  dim  PosCharX, PosCharY as Byte

  ' PosCharY = LocY / 8
  ' faster than /8
  PosCharY = LocY
  Repeat 3
    Set C Off
    Rotate PosCharY Right
  End Repeat


  Write_Command_ST7567( 0xB0 + PosCharY )   ' set page address
  PosCharX = ( LocX  & 0x0f )  ' lower nibble
  Write_Command_ST7567( PosCharX )

  PosCharX = LocX
  Repeat 4
         Set C off
         Rotate PosCharX Right
  End Repeat
  PosCharX = ( PosCharX & 0x0F ) + 0x10
  Write_Command_ST7567 ( PosCharX )

end sub


'''Sets the constrast to select 1 out of 256 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
sub SetContrast_ST7567 ( in dim_state  )

    // Force range between 0 and 63
    dim_state = dim_state / 4


        Write_Command_ST7567(ST7567_SET_CONTRAST)
        Write_Command_ST7567(dim_state)

end sub

'*********************Software SPI**********************

sub S4Wire_ST7567(in ST7567SendByte as byte)

  For clocks = 1 to 8
    If ST7567SendByte.7 = 1 Then
      MOSI_ST7567 = 1
    Else
      MOSI_ST7567 = 0
    End if
    Rotate ST7567SendByte Left Simple
    SCK_ST7567 = 0
    SCK_ST7567 = 1
  Next

end sub

'added 1.14 to support low memory mode

Macro  GLCD_Open_PageTransaction_ST7567 ( Optional In _GLCDPagesL As byte = 0 , Optional In _GLCDPagesH As byte = 7 )
  #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE

      dim _GLCDPage as byte
      'Clear buffer
      for _GLCDPage = _GLCDPagesL to _GLCDPagesH    '_GLCDPage is a global variable - DO NOT CHANGE!!!

  #endif

end Macro


Macro  GLCD_Close_PageTransaction_ST7567

  #ifdef GLCD_TYPE_ST7567_LOWMEMORY_GLCD_MODE

          'Set cursor position
          Cursor_Position_ST7567 ( 0, 8 * _GLCDPage )

          'Send the buffer to the device using transaction
          Open_Transaction_ST7567

          for ST7567_BufferLocationCalc = 1 to 128
             Write_Transaction_Data_ST7567 ST7567_BufferAlias(ST7567_BufferLocationCalc)
             'Clear the buffer byte. We need it to be empty for the next page operation
             ST7567_BufferAlias(ST7567_BufferLocationCalc) = 0
          next

          Close_Transaction_ST7567

      next

    #endif

end Macro


'added 1.14 to improved performance
Macro Open_Transaction_ST7567

    '4wire not supported, see Write_Transaction_Data_ST7567

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
Macro Write_Transaction_Data_ST7567 ( in ST7567SendByte as byte )

        #ifdef S4Wire_DATA

          CS_ST7567 = 0
          DC_ST7567 = 1
          S4Wire_ST7567 ST7567SendByte
          DC_ST7567 = 0
          CS_ST7567 = 1

        #endif

        #ifdef I2C_DATA

         I2CSend ST7567SendByte

        #endif

        #ifdef HI2C_DATA

         HI2CSend ST7567SendByte

        #endif

        #ifdef HI2C2_DATA

         HI2C2Send ST7567SendByte

        #endif

End Macro

'added 1.14 to improved performance
Macro Close_Transaction_ST7567

    '4wire not supported, see Write_Transaction_Data_ST7567

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
