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
' Supports UC1601 controller only.

'Hardware settings using I2C
'  #define GLCD_TYPE GLCD_TYPE_UC1601
'  #define GLCD_I2C_Address 0x70   - Write commands - see the datasheet for 0x70..0x73
'Hardware settings using SPI (4Wire,MOSI(D1),SCK(D0),DC,CS)
'  #define GLCD_TYPE GLCD_TYPE_UC1601
'  #define S4Wire_Data

' 0.09a Initial release


'
'   Example of character mode usage
'    #define GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY    'This defined will be required on high RAM devices to activate low memory capability
'
'    GLCDPrint 0,1, "GCBASIC"
'    GLCDPrint 0,12, "Page 2"
'
'   Low memory adaptation and documentation
'      Low memory mode
'           8 lines of text, any GLCDPrint (xpos, YPOS, text) and YPOS will be translated to Address Line #n.
'           Address Line #1 = YPOS 0 to 7
'           Address Line #2 = YPOS 8 to 15
'           Address Line #3 = YPOS 16 to 23

'
'
'   GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE configuration
'   GLCD_Open_PageTransaction and GLCD_Close_PageTransaction added to support Low memory configuration
'   Any commands between GLCD_Open_PageTransaction and GLCD_Close_PageTransaction should be GLCD focused only.
'   Any commands within the transaction WILL BE called 8 (EIGHT) times. So, if you increment variables... they will be incremented 8 times
'   Any commands between GLCD_Open_PageTransaction and GLCD_Close_PageTransaction should be GLCD focused only.
'   Methods such as scroll, pixelstatus WILL NOT work in low memory mode.
'
'   Example of low memory usage
'    #define GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY    'This defined will be required on high RAM devices to activate low memory capability
'    #define GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
'
'      GLCD_Open_PageTransaction 0,7
'        GLCDPrint 0,1, "GCBASIC"
'        GLCDPrint 0,8, "Page 2"
'        Box 0,0,131,21
'      GLCD_Close_PageTransaction
'
'
'  IGNORE_GLCD_TYPE_UC1601_LOW_MEMORY_WARNINGS to support low memory MCU warnings

'Parameters to setup GLCD
#define UC1601_CONTRAST 63

'System COMMAND constants - do not change below here
#define UC1601_COL_LOW     0
#define UC1601_COL_HI      0X10
#define UC1601_TEMP_COMP   0X24
#define UC1601_PWR_CTRL    0X28
#define UC1601_ADV_PCTL    0X30   ' DANACH WER
#define UC1601_SCROLLINE   0X40
#define UC1601_SET_PAGE    0XB0
#define UC1601_SETCONTRAST 0X81   ' DANACH WERT, DEFAULT 0XC0
#define UC1601_PARTIAL     0X84
#define UC1601_RAMADCTL    0X88
#define UC1601_FRAMERATE   0XA0
#define UC1601_ALLPIXEL    0XA4
#define UC1601_INVERS      0XA6
#define UC1601_DISP_ENABLE 0XAE
#define UC1601_MAP_CTRL    0XC0
#define UC1601_MAP_CTRL_MX    0X01
#define UC1601_MAP_CTRL_MY    0X02
#define UC1601_SOFTRESET   0XE2
#define UC1601_NOP         0XE3
#define UC1601_BIAS        0XE8
#define UC1601_COM_END     0XF1    ' DANACH WERT 0..127, DEFAULT 63
#define UC1601_PART_START  0XF2    ' DANACH WERT 0..127, DEFAULT 0
#define UC1601_PART_END    0XF3    ' DANACH WERT 0..127, DEFAULT 63

#startup InitGLCD_UC1601, 100

'Setup code for UC1601 controllers
#script     ' This script set the capabilities based upon the amount of RAM

     IGNORE_SPECIFIED_GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY = 0
     if IGNORE_GLCD_TYPE_UC1601_LOW_MEMORY_WARNINGS then
        IGNORE_SPECIFIED_GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY = 1
     end if

     if GLCD_TYPE = GLCD_TYPE_UC1601 then
       If ChipRAM < 1024  Then
           GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY = TRUE
           GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE = TRUE
           if IGNORE_SPECIFIED_GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY = 0 then
              Warning "Memory < 396 bytes."
              Warning "Selected MCU requires use of GLCD Open&Close Page Transaction."
              Warning "See Help for usage."
              Warning "Define a constant IGNORE_GLCD_TYPE_UC1601_LOW_MEMORY_WARNINGS to remove this message."
           end if
       End If
     end if

     if GLCD_TYPE = GLCD_TYPE_UC1601_32 then
       If ChipRAM < 512  Then
           GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY = TRUE
           GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE = TRUE
       End If
     end if

#endscript

   dim UC1601_BufferLocationCalc  as Word               ' mandated in main program for UC1601
   'dim GLCDY_Temp as Integer alias UC1601_BufferLocationCalc_h, UC1601_BufferLocationCalc
   'UC1601_BufferLocationCalc = 1
   'GLCDY_Temp = -1
   'dim GLCDY_Temp as Integer alias UC1601_BufferLocationCalc_h, UC1601_BufferLocationCalc

   #ifndef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
     #if GLCD_TYPE = GLCD_TYPE_UC1601
       asm showdebug  GGLCD UC1601 buffer is 396bytes
       Dim UC1601_BufferAlias(396)
     #endif
   #endif

   #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
     #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
       asm showdebug  GLCD UC1601 buffer is 132bytes
       Dim UC1601_BufferAlias(132)
     #endif
   #endif



'''@hide
Sub Write_Command_UC1601 ( in UC1601SendByte as byte )

    #ifdef S4Wire_DATA

      CS_UC1601 = 0
      DC_UC1601 = 0
      S4Wire_UC1601 UC1601SendByte
      DC_UC1601 = 1
      CS_UC1601 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address
      I2CSend UC1601SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA
      Do
        HI2CStart
        HI2CSend GLCD_I2C_Address
      loop While HI2CAckPollState
      HI2CSend UC1601SendByte
      HI2CStop

    #endif

    #ifdef HI2C2_DATA

      HI2C2Start
      HI2C2Send GLCD_I2C_Address
      HI2C2Send UC1601SendByte
      HI2C2Stop

    #endif

End Sub



'''@hide
Sub Write_Data_UC1601 ( in UC1601SendByte as byte )

    #ifdef S4Wire_DATA

      CS_UC1601 = 0
      DC_UC1601 = 1
      S4Wire_UC1601 UC1601SendByte
      DC_UC1601 = 0
      CS_UC1601 = 1
      Exit Sub

    #endif

    #ifdef I2C_DATA

      I2CStart
      I2CSend GLCD_I2C_Address | 0b10
      I2CSend UC1601SendByte
      I2CStop

    #endif

    #ifdef HI2C_DATA

      do
        HI2CStart
        HI2CSend GLCD_I2C_Address | 0b10   'ERV
      loop While HI2CAckPollState
      HI2CSend UC1601SendByte
      HI2CStop

    #endif

    #ifdef HI2C2_DATA

      HI2C2Start
      HI2C2Send GLCD_I2C_Address | 0b10
      HI2C2Send UC1601SendByte
      HI2C2Stop

    #endif

End Sub


'''@hide
Sub InitGLCD_UC1601
    'key debug information for developers.
    asm showdebug GLCD_TYPE
    asm showdebug GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
    asm showdebug GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
    asm showdebug IGNORE_GLCD_TYPE_UC1601_LOW_MEMORY_WARNINGS
    asm showdebug HI2C_DATA
    asm showdebug HI2C2_DATA
    asm showdebug S4Wire_DATA

    asm showdebug

    'Colours 'Set these first
    GLCDBackground = 0
    GLCDForeground = 1
    GLCDFontWidth = 5
    GLCDfntDefaultHeight = 7
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
          dir MOSI_UC1601 Out
          dir SCK_UC1601 Out
          dir DC_UC1601 Out
          dir CS_UC1601 Out
          dir RES_UC1601 Out
          RES_UC1601 = 0
          wait 10 us
          RES_UC1601 = 1
        #endif

    'Setup code for UC1601 controllers
    'Init sequence for module

    #ifdef GLCD_RESET
      dir GLCD_RESET out
      GLCD_RESET = 0
      wait 2 ms
      GLCD_RESET = 1
      wait 10 ms
    #endif

    'Using transaction to make the init simple for the many connectivity types - comms are all handled in transactions
    Open_Transaction_UC1601
    Write_Transaction_Data_UC1601(UC1601_SOFTRESET)
    Close_Transaction_UC1601
    Wait 2 ms

    'Transactions to setcontrast
    Open_Transaction_UC1601
    Write_Transaction_Data_UC1601(UC1601_SETCONTRAST)
    Write_Transaction_Data_UC1601(UC1601_CONTRAST)
    Close_Transaction_UC1601
    Wait 2 ms

    'Transactions to set height of device
    Open_Transaction_UC1601
    Write_Transaction_Data_UC1601(UC1601_COM_END)
    Write_Transaction_Data_UC1601(GLCD_HEIGHT - 1)
    Close_Transaction_UC1601
    Wait 2 ms

    'Clear out the GLCD RAM
    Cursor_Position_UC1601 ( 0, 0 )
    Open_Transaction_UC1601
    Repeat 1056
        Write_Data_UC1601 ( 0 )
    End repeat

    Close_Transaction_UC1601
    Wait 2 ms

    'Transactions to enable the GLCD
    Open_Transaction_UC1601
    Write_Transaction_Data_UC1601(UC1601_DISP_ENABLE | ON )
    Close_Transaction_UC1601
    Wait 2 ms

    GLCDRotate_UC1601 ( Landscape )
   'Clear screen
    GLCDCLS_UC1601

End Sub



'''Clears the GLCD screen
Sub GLCDCLS_UC1601 ( Optional In  GLCDBackground as word = GLCDBackground )
 ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
  GLCD_yordinate = 0

  dim UC1601_BufferLocationCalc as word
  #ifndef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
    #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
      'NOT GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY and NOT GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
      For UC1601_BufferLocationCalc = 1 to 396
          UC1601_BufferAlias(UC1601_BufferLocationCalc) = 0
      Next
    #endif
  #endif
  #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
    #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
      ' GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY and GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
      For UC1601_BufferLocationCalc = 0 to 132
          UC1601_BufferAlias(UC1601_BufferLocationCalc) = 0
      Next
    #endif
  #endif

  'changed to transaction
  For UC1601_BufferLocationCalc = 0 to GLCD_HEIGHT step 8
    Cursor_Position_UC1601 ( 0 , UC1601_BufferLocationCalc )
    Open_Transaction_Data_UC1601
    For GLCDTemp = 0 to GLCD_WIDTH-1
      Write_Transaction_Data_UC1601(GLCDBackground)
    Next
    Close_Transaction_UC1601
  Next

  'Retained purely  for documentation only - this is slower than transactions
    '  Cursor_Position_UC1601 ( 0 , 0 )
    '  for UC1601_BufferLocationCalc = 0 to GLCD_HEIGHT-1 step 8
    '    for GLCDTemp = 0 to 127
    '        Write_Data_UC1601(GLCDBackground)
    '    Next
    '  next

  'Reset cursor
  Cursor_Position_UC1601 ( 0 , 0 )
  PrintLocX =0
  PrintLocY =0

End Sub

'''Draws a character at the specified location on the UC1601 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0

Sub GLCDDrawChar_UC1601(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  'This is now in four parts
  '1. Handler for GLCD LM mode
  '2. Preamble
  '3. GCB Font set handler
  '4. OLED Font set handler

'***** Handler for GLCD LM mode

   #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
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

    Cursor_Position_UC1601 ( CharLocX , CharLocY )

  'Added transaction
   #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
    Open_Transaction_UC1601
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
          #ifndef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY         ' Same as code below. Repeated as the Define is the limitation
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

          'Low Memory GLCD mode
          #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE         ' Same as code above. Repeated as the Define is the limitation
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
          #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY

            'Ensure this is not called with in Low memory mode
            #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE

                GLCDX = ( CharLocX + CurrCharCol -1 )

                 #IFDEF GLCD_PROTECTOVERRUN
                    'anything off screen with be rejected

                    if GLCDX => GLCD_WIDTH OR CharLocY => GLCD_HEIGHT Then
                        exit for
                    end if

                #ENDIF

                'Added transaction
                If LineColour = 1 Then
                 Write_Transaction_Data_UC1601( CurrCharVal )
                else
                 Write_Transaction_Data_UC1601( 255 - CurrCharVal )
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
              'If the char is the ASC(32) a SPACE set the fontwidth =1 (not 2)
              if LocalCharCode = 1 then
                  GLCDFontWidth = 1
              else
                  GLCDFontWidth = COLSperfont+1
              end if
              #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
                  'Only use the correct bits/columns
                  COLSperfont--
                #endif
              #endif
              ROWSperfont = 7


              #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
                  ROWSperfont = 1
                #endif
              #endif
            case 2 'this is one font table
              CharCode = CharCode - 17
              'Pointer to table of font elements
              LocalCharCode = (CharCode * 20)
              COLSperfont = 9  'which is really 10 as we start at 0

              ROWSperfont=15  'which is really 16 as we start at 0


              #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
                #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
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

          'Full GLCD mode
          #ifndef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY         ' Same as code below. Repeated as the Define is the limitation

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

          'Low Memory GLCD mode
          #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE         ' Same as code above. Repeated as the Define is the limitation

            'Handle 16 pixels of height
            For CurrCharRow = 0 to ROWSperfont
                If CurrCharVal.0 = 1 Then
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

          'Character GLCD mode
          ' Handles specific draw sequence. This caters for write only of a bit value. No read operation.
          #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY

            'Ensure this is not called with in Low memory mode
            #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE

                GLCDX = ( CharLocX + CurrCharCol )

                 #IFDEF GLCD_PROTECTOVERRUN
                    'anything off screen with be rejected

                    if GLCDX => GLCD_WIDTH OR CharLocY => GLCD_HEIGHT Then
                        exit for
                    end if

                #ENDIF

                Close_Transaction_UC1601
                Cursor_Position_UC1601 ( GLCDX , CharLocY )

                Open_Transaction_UC1601
                'Support for transaction ... write data out
                If LineColour = 1 Then
                 Write_Transaction_Data_UC1601( CurrCharVal )
                else
                 Write_Transaction_Data_UC1601( 255 - CurrCharVal )
                end if
                Close_Transaction_UC1601
                'Read the next part of the char data
                if GLCDfntDefaultSize = 2 then
                    LocalCharCode++

                    #ifndef GLCD_Disable_OLED_FONT2
                      ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                    #endif
                    #ifdef GLCD_Disable_OLED_FONT2
                      CurrCharVal = 255
                    #endif

                    Cursor_Position_UC1601 ( GLCDX , CharLocY + 8 )

                    Open_Transaction_UC1601
                    'Support for transaction ... write data out
                    If LineColour = 1 Then
                     Write_Transaction_Data_UC1601( CurrCharVal )
                    else
                     Write_Transaction_Data_UC1601( 255 - CurrCharVal )
                    end if
                end if

            #endif

          #endif

        Next

    #endif


   'Added transaction
   #ifdef GLCD_TYPE_UC1601_CHARACTER_MODE_ONLY
    Close_Transaction_UC1601
   #endif

    'Restore
    if LineColour <> GLCDForeground  then
      'Inverted Colours
      GLCDBackground = 0
      GLCDForeground = 1
    end if

End Sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_UC1601(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

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

  #if GLCD_TYPE = GLCD_TYPE_UC1601 or GLCD_TYPE = GLCD_TYPE_UC1601_32
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
Sub PSet_UC1601(In GLCDX, In GLCDY, In GLCDColour As Word)

    Select Case UC1601AddressType

      case Landscape_Rev
          GLCDY = GLCD_HEIGHT - GLCDY - 1

      case Portrait
          GLCDDataTemp = GLCDX
          GLCDX = GLCDY
          GLCDY = GLCDDataTemp

      case Portrait_Rev
          GLCDDataTemp = GLCDX
          GLCDX = GLCD_WIDTH - GLCDY - 1
          GLCDY = GLCD_HEIGHT - GLCDDataTemp - 1

    end select

    #if GLCD_TYPE = GLCD_TYPE_UC1601

        #IFDEF GLCD_PROTECTOVERRUN
            'anything off screen with be rejected
            if GLCDX => GLCD_WIDTH OR GLCDY => GLCD_HEIGHT Then
                exit sub
            end if

        #ENDIF


        #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
            'GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
            'Addresses correct device horizonal page

            'if UC1601_BufferLocationCalc = _GLCDPage then
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
            UC1601_BufferLocationCalc = GLCDX + 1

        #endif


        #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
           '#ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
           'don't need to do these calculations for in LOWMEMORY_GLCD_MODE
           'essentially calculating UC1601_BufferLocationCalc = ( GLCDY / 8 )* GLCD_WIDTH
           'this is a faster opertation than /8
           UC1601_BufferLocationCalc = GLCDY
           Repeat 3
             Set C Off
             Rotate UC1601_BufferLocationCalc Right
           End Repeat

           'this is a faster opertation  faster than * 128
           #if GLCD_WIDTH=128
             Set C Off
             Repeat 7
               Rotate UC1601_BufferLocationCalc Left
             End Repeat
           #endif
           #if GLCD_WIDTH <> 128
             '#if GLCD_WIDTH <> 128
             UC1601_BufferLocationCalc = UC1601_BufferLocationCalc * GLCD_WIDTH
           #endif
           UC1601_BufferLocationCalc = GLCDX + UC1601_BufferLocationCalc + 1
        #endif

        #IFDEF GLCD_PROTECTOVERRUN
            'anything beyond buffer boundary?
            'why? X = 127 and Y = 64 (Y is over 63!) will have passed first check....
            if UC1601_BufferLocationCalc > GLCD_HEIGHT * GLCD_WIDTH Then
                exit sub
            end if

        #ENDIF

        GLCDDataTemp = UC1601_BufferAlias(UC1601_BufferLocationCalc)
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

        'Isolate from full glcd mode
        #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
            'restore address the correct page by adjustng the Y
            GLCDY = GLCDY + ( 8 * _GLCDPage )
            if UC1601_BufferAlias(UC1601_BufferLocationCalc) <> GLCDDataTemp then
              UC1601_BufferAlias(UC1601_BufferLocationCalc) = GLCDDataTemp
            end if

        #endif
        'revised to isolate from low memory mode
        #ifndef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE
          if UC1601_BufferAlias(UC1601_BufferLocationCalc) <> GLCDDataTemp then
              UC1601_BufferAlias(UC1601_BufferLocationCalc) = GLCDDataTemp
              Cursor_Position_UC1601 ( GLCDX, GLCDY )
              Write_Data_UC1601 ( GLCDDataTemp )
          end if
        #endif

    #endif

End Sub



Function PixelStatus_UC1601(In GLCDX, In GLCDY )

    #if GLCD_TYPE = GLCD_TYPE_UC1601
            'Select correct buffer element
            UC1601_BufferLocationCalc = GLCDY
            'Divide by 8
            Repeat 3
              Set C Off
              Rotate UC1601_BufferLocationCalc Right
            End Repeat
            UC1601_BufferLocationCalc = UC1601_BufferLocationCalc * GLCD_WIDTH
            UC1601_BufferLocationCalc = GLCDX + UC1601_BufferLocationCalc+1  'add 1 as we dont use element (0)
            GLCDDataTemp = UC1601_BufferAlias(UC1601_BufferLocationCalc)
            'Change data to examine pixel by rotating our bit to bit zero
            GLCDBitNo = GLCDY And 7
            Repeat GLCDBitNo
              Rotate GLCDDataTemp right
            End Repeat
            PixelStatus_UC1601 = GLCDDataTemp.0
    #endif

  end function


'''Takes raw pixel positions and translates to XY char positions
'''@param X coordinate of pixel
'''@param Y coordinate of pixel
sub Cursor_Position_UC1601( in LocX as byte, in LocY as byte )
  dim  PosCharX, PosCharX as Word


  ' PosCharY = LocY / 8
  ' faster than /8
  PosCharY = LocY
  Repeat 3
    Set C Off
    Rotate PosCharY Right
  End Repeat


  Write_Command_UC1601( 0xB0 + PosCharY )   ' set page address
  PosCharX = ( LocX  & 0x0f )  ' lower nibble
  Write_Command_UC1601( PosCharX )

  PosCharX = LocX
  Repeat 4
         Set C off
         Rotate PosCharX Right
  End Repeat
  PosCharX = ( PosCharX & 0x0F ) + 0x10
  Write_Command_UC1601 ( PosCharX )

end sub



Sub GLCDRotate_UC1601( in UC1601AddressType as byte )

  Open_Transaction_UC1601

  select case UC1601AddressType
        case PORTRAIT
              Write_Transaction_Data_UC1601(UC1601_MAP_CTRL )

              GLCDDeviceHeight = GLCD_HEIGHT - 1
              GLCDDeviceWidth  = GLCD_WIDTH - 1

        case LANDSCAPE

              Write_Transaction_Data_UC1601(UC1601_MAP_CTRL | UC1601_MAP_CTRL_MY )

              GLCDDeviceHeight = GLCD_WIDTH - 1
              GLCDDeviceWidth  = GLCD_HEIGHT - 1

        case PORTRAIT_REV

              Write_Transaction_Data_UC1601(UC1601_MAP_CTRL )

              GLCDDeviceHeight = GLCD_HEIGHT - 1
              GLCDDeviceWidth  = GLCD_WIDTH - 1

        case LANDSCAPE_REV

              Write_Transaction_Data_UC1601(UC1601_MAP_CTRL )

              GLCDDeviceHeight = GLCD_WIDTH - 1
              GLCDDeviceWidth  = GLCD_HEIGHT - 1

        case else

              Write_Transaction_Data_UC1601(UC1601_MAP_CTRL | UC1601_MAP_CTRL_MY )

              GLCDDeviceHeight = GLCD_HEIGHT - 1
              GLCDDeviceWidth  = GLCD_WIDTH - 1

  end select

  Close_Transaction_UC1601

end sub



'''Reverts scroll
sub stopscroll_UC1601
        Open_Transaction_UC1601
        Write_Transaction_Data_UC1601( 0x40 )
        Close_Transaction_UC1601
end sub


'''Activate a  scroll for rows start
'''@param Start row
SUB scroll_UC1601 ( IN UC1601_start )
        Open_Transaction_UC1601
        Write_Transaction_Data_UC1601( 0x40 | UC1601_start )
        Close_Transaction_UC1601
end sub


'''Sets the constrast to select 1 out of 256 contrast steps.
'''Contrast increases as the value increases.
'''@param Dim byte value
sub SetContrast_UC1601 ( in dim_state )

  Open_Transaction_UC1601
  Write_Transaction_Data_UC1601(UC1601_SETCONTRAST)
  Write_Transaction_Data_UC1601(dim_state)
  Close_Transaction_UC1601


end sub

'*********************Software SPI**********************

sub S4Wire_UC1601(in UC1601SendByte as byte)

  For clocks = 1 to 8
    If UC1601SendByte.7 = 1 Then
      MOSI_UC1601 = 1
    Else
      MOSI_UC1601 = 0
    End if
    Rotate UC1601SendByte Left Simple
    SCK_UC1601 = 0
    SCK_UC1601 = 1
  Next

end sub

'Support low memory mode

Macro  GLCD_Open_PageTransaction_UC1601 ( Optional In _GLCDPagesL As byte = 0 , Optional In _GLCDPagesH As byte = 7 )
  #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE

      dim _GLCDPage as byte
      'Clear buffer
      for _GLCDPage = _GLCDPagesL to _GLCDPagesH    '_GLCDPage is a global variable - DO NOT CHANGE!!!

  #endif

end Macro


Macro  GLCD_Close_PageTransaction_UC1601

  #ifdef GLCD_TYPE_UC1601_LOWMEMORY_GLCD_MODE

          'Set cursor position
          Cursor_Position_UC1601 ( 0, 8 * _GLCDPage )

          'Send the buffer to the device using transaction
          Open_Transaction_Data_UC1601

          for UC1601_BufferLocationCalc = 1 to 132
             Write_Transaction_Data_UC1601 UC1601_BufferAlias(UC1601_BufferLocationCalc)
             'Clear the buffer byte. We need it to be empty for the next page operation
             UC1601_BufferAlias(UC1601_BufferLocationCalc) = 0
          next

          Close_Transaction_UC1601

      next

    #endif

end Macro


'Improved performance
Macro Open_Transaction_UC1601

    #ifdef S4Wire_DATA
        '4wire does not a operation here, see Write_Transaction_Data_UC1601
    #endif

     #ifdef I2C_DATA

       I2CStart
       I2CSend GLCD_I2C_Address

     #endif

     #ifdef HI2C_DATA

       do
          HI2CStart
          HI2CSend GLCD_I2C_Address
       loop While HI2CAckPollState

     #endif

     #ifdef HI2C2_DATA

       HI2C2Start
       HI2C2Send GLCD_I2C_Address

     #endif

End Macro


'Improved performance
Macro Open_Transaction_Data_UC1601

    #ifdef S4Wire_DATA
        '4wire does not a operation here, see Write_Transaction_Data_UC1601
    #endif

     #ifdef I2C_DATA

       I2CStart
       I2CSend GLCD_I2C_Address | 0b00000010

     #endif

     #ifdef HI2C_DATA

      do
         HI2CStart
         HI2CSend GLCD_I2C_Address | 0b00000010
      loop While HI2CAckPollState

     #endif

     #ifdef HI2C2_DATA

       HI2C2Start
       HI2C2Send GLCD_I2C_Address | 0b00000010

     #endif

End Macro

'Improved performance
Macro Write_Transaction_Data_UC1601 ( in UC1601SendByte as byte )

        #ifdef S4Wire_DATA

          CS_UC1601 = 0
          DC_UC1601 = 0
          S4Wire_UC1601 UC1601SendByte
          DC_UC1601 = 1
          CS_UC1601 = 1

        #endif

        #ifdef I2C_DATA

         I2CSend UC1601SendByte

        #endif

        #ifdef HI2C_DATA

         HI2CSend UC1601SendByte

        #endif

        #ifdef HI2C2_DATA

         HI2C2Send UC1601SendByte

        #endif

End Macro

'Improved performance
Macro Close_Transaction_UC1601


    #ifdef S4Wire_DATA
      '4wire does not a operation here, see Write_Transaction_Data_UC1601
    #endif


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
