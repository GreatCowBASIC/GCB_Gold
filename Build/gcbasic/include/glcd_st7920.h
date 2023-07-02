'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2012-2020 Hugh Considine and Evan Venn

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
' Supports ST7920 controller only.

'Changes
' 14/4/2014: Added ST7920 Support
' 22/4/2014: Fixed calc method in ST7920 routine
' 22/4/2014: Revised OLD_GLCD_TYPE_ST7920_GRAPHICS_MODE variable name
' 05/5/2014: Revised Line, and added Circle and FilledCircle
' 05/5/2014: Revised GLCD to ensure the screen is cleared properly
' 08/5/2014: Revised to comply with documentation requirements
' 09/5/2014: Remove more silly variable names. No functional changes
' 09/5/2014: Fixed circle and line to ensure cross device compatibility
' 11/5/2014: Revided to expose old line drawing routines
'
' 9/11/14 New revised version.  Requires GLCD.H.  Do not call directly.  Always load via GLCD.H
' 19/2/16 Revised to ensure compatibility with old code and the help file!
' 22/4/17 Revised to support () and binary
' 11/4/19 Revised to clean up position and therefore the bleeding of constants into ASM
' 27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn
' 1/4/22  Revised GLCDFontWidth to 5 from 6
' 14/08/22 Updated user changeable constants only - no functional change


'
'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_ST7920"
'Parallel lines (ST7920 only)
'''@hardware GLCD_TYPE GLCD_TYPE_ST7920; Data Port; GLCD_DATA_PORT; IO_Port
'Control lines (ST7920 only)
'''@hardware GLCD_TYPE GLCD_TYPE_ST7920; Register Select; GLCD_RS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7920; Read/Write; GLCD_RW; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7920; Enable; GLCD_ENABLE; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_ST7920; Reset; GLCD_RESET; IO_Pin


' User changeable constants
  ' A FEW CONSTANTS THAT MIGHT NEED TO BE SET TO SUPPORT THE ST7920
  ' 4.4V = 25, 5.0V 20 - YOU MAY NEED TO ADJUST


' End of user changeable constants

' Constants that might need to be set to support the ST7920
#define GLCD_ROW  (GLCD_HEIGHT/1)
#define GLCD_COLS   (GLCD_WIDTH/16)

#Script

  IF NODEF(ST7920READDELAY) THEN
    ST7920READDELAY=20
      ' IF CHIPMHZ = 4 THEN 10
      ' IF CHIPMHZ = 8 THEN 2
      ' IF CHIPMHZ = 16 THEN 2
      ' IF CHIPMHZ = 32 THEN 2
  END IF

  IF NODEF(ST7920WRITEDELAY) THEN
    ST7920WRITEDELAY=2
  END IF

  IF NODEF(GLCDENABLECHARACTERMODE_ST7920) THEN
    GLCDENABLECHARACTERMODE_ST7920=GLCDDISABLEGRAPHICS_ST7920
  END IF

#EndScript

#startup InitGLCD_ST7920


Sub InitGLCD_ST7920

          #if GLCD_TYPE = GLCD_TYPE_ST7920

              '''Initialise the GLCD device

              #IFDEF GLCD_IO 4,8

                        #IFNDEF GLCD_LAT
                               DIR GLCD_RS OUT
                               Dir GLCD_RESET Out
                        #ENDIF
                        #IFDEF GLCD_LAT
                               DIR _GLCD_RS OUT
                               Dir _GLCD_RESET Out
                        #ENDIF

                        #IFNDEF GLCD_NO_RW
                                  #IFNDEF GLCD_LAT
                                         DIR GLCD_RW OUT
                                  #ENDIF
                                  #IFDEF GLCD_LAT
                                         DIR _GLCD_RW OUT
                                  #ENDIF
                        #ENDIF


                        #IFNDEF GLCD_LAT
                               DIR GLCD_Enable OUT
                              'Reset
                              Set GLCD_RESET Off
                              set GLCD_RESET off
                              Wait 1 s
                              Set GLCD_RESET On
                              Wait 50 ms
                              Set GLCD_RESET On
                        #ENDIF
                        #IFDEF GLCD_LAT
                               DIR _GLCD_Enable OUT
                               'Reset
                               Set GLCD_RESET Off
                               Wait 1 ms
                               Set GLCD_RESET On
                               Wait 50 ms
                        #ENDIF


                        wait 10 ms
                        Wait until GLCDReady_ST7920

                        'Set data bus width
                        SET GLCD_RS OFF
              #ENDIF ' GLCD_IO 4,8

              #IFDEF GLCD_IO 8
                        SET GLCD_RS OFF
                        SET GLCD_RW OFF
                        SET GLCD_ENABLE OFF

                        wait 200 ms
                        WriteCommand_ST7920(0x56)
                        wait 1 ms
                        WriteCommand_ST7920(0x56)
                        wait 1 ms
                        WriteByte_ST7920(0x36)
                        wait 5 10us
                        WriteByte_ST7920(0x0c)
                        wait 5 10us
                        WriteByte_ST7920(0x06)
                        wait 5 10us
              #ENDIF

              #IFDEF GLCD_IO 4
                        'Set pins to output
                        #IFNDEF GLCD_LAT
                            DIR GLCD_DB4 OUT
                            DIR GLCD_DB5 OUT
                            DIR GLCD_DB6 OUT
                            DIR GLCD_DB7 OUT
                        #ENDIF
                        #IFDEF GLCD_LAT
                            DIR _GLCD_DB4 OUT
                            DIR _GLCD_DB5 OUT
                            DIR _GLCD_DB6 OUT
                            DIR _GLCD_DB7 OUT
                        #ENDIF

                        set GLCD_RS OFF
                        #IFNDEF GLCD_NO_RW
                                  set GLCD_RW OFF
                        #ENDIF

                        wait 15 ms
                        '0011
                        set GLCD_DB4 ON
                        set GLCD_DB5 ON
                        set GLCD_DB6 OFF
                        set GLCD_DB7 OFF
                        Repeat 3
                          PULSEOUT GLCD_Enable, 5 10us
                          Wait 5 ms
                          #IFNDEF GLCD_NO_RW
                                  set GLCD_RW OFF
                          #ENDIF
                        end repeat
                        Wait 5 ms


                        'function set
                        set GLCD_DB4 OFF
                        set GLCD_DB5 ON
                        set GLCD_DB6 OFF
                        set GLCD_DB7 OFF
                        PULSEOUT GLCD_Enable, 5 10us
                        Wait 5 ms

                        'function set
                        set GLCD_DB4 OFF
                        set GLCD_DB5 OFF
                        set GLCD_DB6 OFF
                        set GLCD_DB7 OFF
                        PULSEOUT GLCD_Enable, 5 10us
                        Wait 5 ms

                        set GLCD_DB4 OFF
                        set GLCD_DB5 OFF
                        set GLCD_DB6 OFF
                        set GLCD_DB7 ON

                        PULSEOUT GLCD_Enable, 5 10us
                        Wait 5 ms

                        WriteByte_ST7920(0b10000000)
                        wait 5 10us

                        WriteByte_ST7920(0b00010000)
                        wait 5 ms

                        WriteByte_ST7920(0b011000000)
                        wait 5 10us
              #ENDIF    'GLCD_TYPE = GLCD_TYPE_ST7920


              #IFDEF GLCD_IO 4,8

                        'Set Cursor movement
                        SET GLCD_RS OFF
                        WriteCommand_ST7920(0b00000110)
                        wait 5 10us
                        'Turn off cursor
                        WriteCommand_ST7920(0b00001100)
                        wait 5 10us

                        'Clear screen
                        GLCD_TYPE_ST7920_GRAPHICS_MODE = false
                        GLCDCLS
              #ENDIF
              'Colours
              GLCDBackground = 0
              GLCDForeground = 1
              GLCDFontWidth = 5
              GLCDfntDefault = 0
              GLCDfntDefaultsize = 1
              GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn
          #ENDIF

  'Clear screen
  GLCDCLS

End Sub


'Subs
'''Clears the GLCD screen
Sub GLCDCLS_ST7920
          ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
          GLCD_yordinate = 0

          #if GLCD_TYPE = GLCD_TYPE_ST7920
              if GLCD_TYPE_ST7920_GRAPHICS_MODE  = true then
                 GLCDClearGraphics_ST7920
                 Old_ST7920_GRAPHICS_MODE = true
                 GLCDDisableGraphics_ST7920
              end if
              'Clear screen
              SET GLCD_RS OFF
      WriteByte_ST7920(0x01)
              wait 80 ms

              'Move to start of visible DDRAM
               SET GLCD_RS OFF
      WriteByte_ST7920(0x80)
              wait 80 us
              if Old_ST7920_GRAPHICS_MODE  = true then
                 GLCD_TYPE_ST7920_GRAPHICS_MODE = true
                  WriteCommand_ST7920(0x36)'
                  GLCD_TYPE_ST7920_GRAPHICS_MODE  = true
              end if

          #endif

End Sub

'''Displays a number
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param LCDValue Number to display
Sub GLCDPrint_ST7920(In PrintLocX, In PrintLocY, In LCDValue As Long)
  Dim SysCalcTempA As Long
  Dim SysPrintBuffer(10)
  SysPrintBuffLen = 0

  Do
    'Divide number by 10, remainder into buffer
    SysPrintBuffLen += 1
    SysPrintBuffer(SysPrintBuffLen) = LCDValue % 10
    LCDValue = SysCalcTempA
  Loop While LCDValue <> 0

  'Display
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = SysPrintBuffLen To 1 Step -1
    GLCDDrawChar GLCDPrintLoc, PrintLocY, SysPrintBuffer(SysPrintTemp) + 48
    GLCDPrintLoc += GLCDFontWidth
  Next

End Sub


'''Displays a message
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData Message to display
Sub GLCDPrint_ST7920(In PrintLocX, In PrintLocY, in LCDPrintData as string )

          PrintLen = LCDPrintData(0)
  If PrintLen = 0 Then Exit Sub

          #IF GLCD_TYPE = GLCD_TYPE_ST7920
                ' Is this device in GRAPHICS mode?
                if GLCD_TYPE_ST7920_GRAPHICS_MODE = false then
                    select case  PrintLocY
                         case 0
                              col = PrintLocX OR 0x80

                         case 1
                              col = PrintLocX OR 0x90

                         case 2
                              col = PrintLocX OR 0x88

                         case 3
                              col = PrintLocX OR 0x98

                         case else
                              col = PrintLocX OR 0x80
                      end select
                       SET GLCD_RS OFF
               WriteByte_ST7920( col )
                else
                   GLCDDrawString PrintLocX, PrintLocY, LCDPrintData
                   exit sub
                end if

                Set GLCD_RS On
                'Write Data
                For SysLCDPrintTemp = 1 To PrintLen
                     WriteByte_ST7920 ( LCDPrintData(SysLCDPrintTemp) )
                Next

          #endif

end sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_ST7920(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

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

          #if GLCD_TYPE = GLCD_TYPE_ST7920
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
'''@param GLCDColour State of pixel (0 = erase, 1 = display)
Sub PSet_ST7920(In GLCDX, In GLCDY, In GLCDColour As Word)

  #if GLCD_TYPE = GLCD_TYPE_ST7920

                    #ifdef GLCD_PROTECTOVERRUN
                           if GLCDY => GLCD_HEIGHT then exit sub
                           if GLCDX => GLCD_WIDTH then exit sub
                    #endif

    'Set pixel at X, Y on LCD to State
    'X is 0 to 127
    'Y is 0 to 63
    'Origin in top left
    dim GLCDDataTempWord as word
                    dim GLCDChangeWord as word
    'Select horizontal address, we address in unit of 16 pixels
                    ' One xUnit equals 16 pixels
                    ' addresses are b15, b14, b13, ..... to b0.  With B15 left justified.

                    ' calculate the xUnit as a word, therefore 16 bits.
                    xUnit = GLCDX/16

                    ' get the word data of the xUnit and yposition
                    dim SysCalcPositionY as byte
                    dim SysCalcPositionX as byte
                    if GLCDY < 32 then
                        ' Y Axis
                SysCalcPositionY = 0x80 OR GLCDY
                        ' X Axis
                        SysCalcPositionX = 0x80 OR xUnit
                    else
                        ' Y Axis
                        SysCalcPositionY = 0x80 OR (GLCDY-32)
                        ' X Axis
                        SysCalcPositionX = 0x88 OR xUnit
                    end if

                    SET GLCD_RS OFF
                    WriteByte_ST7920( SysCalcPositionY )
                    WriteByte_ST7920( SysCalcPositionX )

                    ' read data
                    GLCDDataTempWord = GLCDReadByte_ST7920
                    GLCDDataTempWord = GLCDReadByte_ST7920
                    GLCDDataTempWord  = (GLCDDataTempWord*256) + GLCDReadByte_ST7920

    'Select which bit we are to set/clear per pixel
                    GLCDBitNo = 15 - (GLCDX And 15)

                    If GLCDColour.0 = 0 Then
                              GLCDChangeWord = 65534
                              Set C On
                    Else
                              GLCDChangeWord = 1
                              Set C Off
                    End If

    Repeat GLCDBitNo
      Rotate GLCDChangeWord LEFT
    End Repeat
    If GLCDColour.0 = 0 Then
      GLCDDataTempWord =  GLCDDataTempWord  And GLCDChangeWord
    Else
      GLCDDataTempWord =  GLCDDataTempWord  Or  GLCDChangeWord
    End If

                    ' Locate again for the write operation
                     SET GLCD_RS OFF
                     WriteByte_ST7920( SysCalcPositionY )
                     WriteByte_ST7920( SysCalcPositionX )

                     hbyte= GLCDDataTempWord / 256
                     SET GLCD_RS ON
                     WriteByte_ST7920(hbyte)
                     WriteByte_ST7920( GLCDDataTempWord & 0x00FF )
  #endif

End Sub


'''Write a command then the byte
'''@hide
Sub WriteCommand_ST7920 ( In GLCDByte )
  SET GLCD_RS OFF
  WriteByte_ST7920(GLCDByte)

end sub

'''Write some date then the byte
'''@hide
Sub WriteData_ST7920( In GLCDByte as byte )
  SET GLCD_RS ON
  WriteByte_ST7920(GLCDByte)

end sub

'  DESCRIPTIONS:
'  Enables Graphics Mode on the GLCD.
'  Make sure the screen is void of text before using this function.
'*******************************************************************************/
'''Enables Graphics Mode on the ST7920 GLCD.
'''  Make sure the screen is void of text before using this function.
#define ST7920GLCDEnableGraphics GLCDEnableGraphics_ST7920
sub GLCDEnableGraphics_ST7920
    #if GLCD_TYPE = GLCD_TYPE_ST7920
' WriteCommand_ST7920(0x30)'
' WriteCommand_ST7920(0x34)'      // Switch to extended instruction mode.
  WriteCommand_ST7920(0x36)'
          '// Enable graphics mode.
          GLCD_TYPE_ST7920_GRAPHICS_MODE  = true
     #endif
end sub


'''  Disable Graphics Mode on the ST9720 GLCD and return to text mode.
#define ST7920GLCDDisableGraphics GLCDDisableGraphics_ST7920
sub GLCDDisableGraphics_ST7920
    #if GLCD_TYPE = GLCD_TYPE_ST7920
' WriteByte_ST7920(0x30)'
'         WriteByte_ST7920(0x34)'     // Switch to extended instruction mode.
  WriteCommand_ST7920(0x30)'      // Graphics and extended instruction mode turned off.
          GLCD_TYPE_ST7920_GRAPHICS_MODE  = false
     #endif
end sub

'''Locates the cursor on the GLCD screen
'''@param PrintLocX X column
'''@param PrintLocY Y row
Sub Locate_ST7920( In PrintLocX , In PrintLocY )

' DESCRIPTIONS:
' Place a character to the GLCD controller on the specified row and column.
' Due to the design of the ST7920 controller (to accomodate Mandarin and Cyrillic), you must place the text on the column
' according to the numbers above the diagram below:
'
' |--0--|--1--|--2--|...    ...|--7--|
' +--+--+--+--+--+---------------------+
' |H |e |l |l |o |  ...                | <- row 0 (address 0x80)
' +--+--+--+--+--+---------------------+
' |T |h |i |s |  |i ...          | <- row 1 (address 0x90)
' +--+--+--+--+--+---------------------+
' |' |' |' |' |' |' ...          | <- row 2 (address 0x88)
' +--+--+--+--+--+---------------------+
' |- |- |- |- |- |- ...          | <- row 3 (address 0x98)
' +--+--+--+--+--+---------------------+
'
' Example:
' Writing 'a' onto the 1st column, and 1st row:
' |--0--|--1--|--2--|...    ...|--7--|
' +--+--+--+--+--+---------------------+
' |  |  |  |  |  |  ...                | <- row 0 (address 0x80)
' +--+--+--+--+--+---------------------+
' |  |  |a |  |  |  ...          | <- row 1 (address 0x90)
' +--+--+--+--+--+---------------------+
' |  |  |  |  |  |  ...          | <- row 2 (address 0x88)
' +--+--+--+--+--+---------------------+
' |  |  |  |  |  |  ...          | <- row 3 (address 0x98)
' +--+--+--+--+--+---------------------+



  select case  PrintLocY
               case 0
                    col = PrintLocX OR 0x80

               case 1
                    col = PrintLocX OR 0x90

               case 2
                    col = PrintLocX OR 0x88

               case 3
                    col = PrintLocX OR 0x98

               case else
                    col = PrintLocX OR 0x80
            end select

            WriteCommand_ST7920 ( col)



end sub

'''Write the byte
'''@hide
Sub WriteByte_ST7920( In GLCDByte )

  #IFNDEF GLCD_NO_RW
    #IFDEF GLCD_IO 4,8
      wait until GLCDReady_ST7920
      set GLCD_RW OFF 'Write mode
    #ENDIF
  #ENDIF

  #IFDEF GLCD_IO 8
    'Set data port to output, and write a value to it
                    #IFNDEF GLCD_LAT
                        DIR GLCD_DATA_PORT OUT
                        GLCD_DATA_PORT = GLCDByte
                        ' retry on failure
                        if    GLCD_DATA_PORT <> GLCDByte then GLCD_DATA_PORT = GLCDByte
                        set GLCD_Enable on
                        wait ST7920WriteDelay us
                        set GLCD_Enable off
                        wait ST7920WriteDelay us
    #ENDIF

                    #IFDEF GLCD_LAT
                        DIR _GLCD_DATA_PORT OUT
                        latd = GLCDByte
                        ' retry on failure
                        if latd <> GLCDByte then latd = GLCDByte
                        set GLCD_Enable on
                        wait ST7920WriteDelay us
                        set GLCD_Enable off
                        wait ST7920WriteDelay us
    #ENDIF
  #ENDIF

end sub


'''Tile LCD screen with two datas in Graphic Mode
'''@hide
sub gTile_ST7920( fst, snd)


         for GLCD_Count = 0 to ( GLCD_HEIGHT - 1 )
             gLocate_ST7920(0, GLCD_Count)
             for GLCD_Count_2 = 0 to ( GLCD_COLS -1 )
                 WriteData_ST7920( fst )'SendByte(iDat, fst)
                 WriteData_ST7920( snd )'SendByte(iDat, snd)
             next

         next
end sub

'''Tile screen with one data accepts a byte!
'''@hide
sub Tile_ST7920( tData as word)

    GLCDEnableCharacterMode_ST7920
    for GLCD_Count  = 0 to GLCD_ROW
      Locate_ST7920(0 , GLCD_Count)
      for GLCD_Count_2  = 0 to GLCD_COLS
          WriteData_ST7920 ( tData )
          WriteData_ST7920 ( tData )
      next
    next
end sub

''' Tile screen with a Chinese Symbol
''' Writes 2 bytes of data into DDRAM to display one 16x16 font.
''' A140H~D75FH are BIG5 code, A1A0H~F7FFH are GB code.
''' accepts a WORD!
''' Tile screen with one data accepts a byte!
'''@hide
sub cTile_ST7920( Hanzi as word )

    GLCDEnableCharacterMode_ST7920
    for GLCD_Count  = 0 to GLCD_ROW
      Locate_ST7920(0 , GLCD_Count)
      for GLCD_Count_2  = 0 to GLCD_COLS
              WriteData_ST7920( Hanzi_H )
              WriteData_ST7920( Hanzi & 0x00FF )
      next
    next

end sub

'''Set Icon ON/OFF
'''@hide
sub SetIcon_ST7920( Addr, dByte)

    GLCDEnableGraphics_ST7920
    WriteCommand_ST7920 ( 0x40 + Addr ) 'Set ICON Address
    WriteData_ST7920 ( dByte ) ' Set ICON Data

end sub


''' Display Standard Testing Graphics
'''@hide
sub GraphicTest_ST7920

    for GLCD_Count_2  = 0 to 3
        ReadTable graphicstestdata, (GLCD_Count_2*2)+1, char1
        ReadTable graphicstestdata, (GLCD_Count_2*2)+2, char2

        gTile_ST7920( char1  , char2 )
        wait 200  ms
        GLCDClearGraphics_ST7920

    next

    for GLCD_Count_2  = 4 to 7
        for GLCD_Count = 0 to 64 step 2
            ReadTable graphicstestdata, (GLCD_Count_2*2)+1, char1
            ReadTable graphicstestdata, (GLCD_Count_2*2)+2, char2
            LineHs_ST7920(0, GLCD_Count, GLCD_COLS - 1 , char1 )
            LineHs_ST7920(0, GLCD_Count + 1,  GLCD_COLS - 1, char2 )
        next
        wait 1  s
    next

    GLCDClearGraphics_ST7920
end sub



''' Draw a Horizontal Line with special style
'''@hide
sub LineHs_ST7920( X0, Y0, xUnits, Style)
    gLocate_ST7920(X0, Y0)
    for GLCD_Count_2  = 0 to xUnits
        WriteData_ST7920 ( Style )
        WriteData_ST7920 ( Style )
   next
end Sub

table graphicstestdata Store Data
0xFF 'All
0xFF
0x00 'None
0x00
0xAA 'Vertical 1
0xAA
0x55 'Vertical 2
0x55
0xFF 'Horizontal 1
0x00
0x00 'Horizontal 2
0xFF
0xAA 'Stars 1
0x55
0x55 'Stars 2
0xAA
end table

'''Locate the cursor on the display
'''@param xUnitLocate GLCDY
sub gLocate_ST7920( xUnitLocate, GLCDY )

   ' 1xUnit = 16 dots, 1yBit = 1 dot

    if GLCDY < 32 then
              ' Y Axis
              WriteCommand_ST7920(0x80 OR GLCDY)
              ' X Axis
              WriteCommand_ST7920(0x80 OR xUnitLocate )
    else
              ' Y Axis
              WriteCommand_ST7920(0x80 OR (GLCDY-32))
              ' X Axis
              WriteCommand_ST7920(0x88 OR xUnitLocate )
    end if

end sub


'''Clear the screen by filling the screen with zero bytes.
'''Enable Graphics Mode
'''before calling this function.
'''@param No parameters
sub GLCDClearGraphics_ST7920
  ' This function performs similar to the FillScreenGraphic but
  ' only zeros are sent into the screen instead of data from an array.

          #if GLCD_TYPE <> GLCD_TYPE_ST7920
              GLCDCLS
              exit sub
          #endif

  for PrintLocY = 0 to 63
    if PrintLocY < 32 then
      WriteCommand_ST7920(0x80 OR PrintLocY)
                  WriteCommand_ST7920(0x80)
    else
      WriteCommand_ST7920(0x80 OR (PrintLocY-32))
                  WriteCommand_ST7920(0x88)
    end if
    for PrintLocX = 0 to 7
      WriteData_ST7920(0x00)
      WriteData_ST7920(0x00)
            next
  next

end sub

'''@hide
function gReaddata_ST7920 ( in xUnit, in yBit ) as word

    gLocate_ST7920(xUnit, yBit)
    ' Read a word from the display device, read is the Dummy read.
    gReaddata_ST7920 = GLCDReadByte_ST7920

    ' Now read the two bytes to make up the word we are looking for
    gReaddata_ST7920 = GLCDReadByte_ST7920

    gReaddata_ST7920  = (gReaddata_ST7920*256)+GLCDReadByte_ST7920

end Function


'''Draws a horizontal line on the GLCD screen
'''@param X0 Starting X point of line
'''@param Y0 Starting Y point of line
'''@param xDots number of pixels
'''@param LineColour Colour of line (0 = blank, 1 = show, default is 1)
sub Lineh_ST7920( in X0, in Y0, in xDots, Optional In GLCDColour As Word = GLCDForeground )

    if xDots = 0 then exit sub

    dim OldData as word
    dim NewData as word
    dim GLCDChangeWord as word
    OldData = 0
    NewData = 0

    for GLCD_Count = 0 to xDots
        xUnit = (GLCD_Count + X0)/16

                 ' set location
                  dim SysCalcPositionY as byte
                  dim SysCalcPositionX as byte
                  if Y0 < 32 then
                      ' Y Axis
                      SysCalcPositionY = 0x80 OR Y0
                      ' X Axis
                      SysCalcPositionX = 0x80 OR xUnit
                  else
                      ' Y Axis
                      SysCalcPositionY = 0x80 OR (Y0-32)
                      ' X Axis
                      SysCalcPositionX = 0x88 OR xUnit
                  end if

                  SET GLCD_RS OFF
                  WriteByte_ST7920( SysCalcPositionY )
                  WriteByte_ST7920( SysCalcPositionX )

                  ' read data dummy byte, then read the two bytes
                  OldData = GLCDReadByte_ST7920
                  OldData = GLCDReadByte_ST7920

                  OldData  = (OldData*256) + GLCDReadByte_ST7920

    'Select which bit we are to set/clear per pixel
                    GLCDBitNo = 15 - ( (X0 + GLCD_Count ) And 15)

                    If GLCDColour.0 = 0 Then
                              GLCDChangeWord = 65534
                              Set C On
                    Else
                              GLCDChangeWord = 1
                              Set C Off
                    End If

    Repeat GLCDBitNo
      Rotate GLCDChangeWord LEFT
    End Repeat
    If GLCDColour.0 = 0 Then
      NewData =  OldData  And GLCDChangeWord
    Else
      NewData =  OldData  Or  GLCDChangeWord
    End If

                 ' set location
                    SET GLCD_RS OFF
                    WriteByte_ST7920( SysCalcPositionY )
                    WriteByte_ST7920( SysCalcPositionX )

                    hbyte= NewData / 256
                    SET GLCD_RS ON
                    WriteByte_ST7920(hbyte)
                    WriteByte_ST7920( NewData & 0x00FF )

   next
end Sub

'''Draws a vertical line on the GLCD screen
'''@param X0 Starting X point of line
'''@param Y0 Starting Y point of line
'''@param xBits number of pixels
'''@param LineColour Colour of line (0 = blank, 1 = show, default is 1)
sub LineV_ST7920( in X0, in Y0, in yBits, Optional In GLCDColour As Word = GLCDForeground )

    if yBits = 0 then exit sub

    xUnit = X0/16
    dim OldData as word
    dim NewData as word
    dim GLCDChangeWord as word
    OldData = 0
    NewData = 0

    for GLCD_Count = 0 to  yBits

        yPixel = Y0 + GLCD_Count
       ' set location
        dim SysCalcPositionY as byte
        dim SysCalcPositionX as byte
        if yPixel < 32 then
            ' Y Axis
            SysCalcPositionY = 0x80 OR yPixel
            ' X Axis
            SysCalcPositionX = 0x80 OR xUnit
        else
            ' Y Axis
            SysCalcPositionY = 0x80 OR (yPixel-32)
            ' X Axis
            SysCalcPositionX = 0x88 OR xUnit
        end if

        SET GLCD_RS OFF
        WriteByte_ST7920( SysCalcPositionY )
        WriteByte_ST7920( SysCalcPositionX )

        ' read data dummy byte, then read the two bytes
        OldData = GLCDReadByte_ST7920
        OldData = GLCDReadByte_ST7920
        OldData  = (OldData*256) + GLCDReadByte_ST7920

        'Select which bit we are to set/clear per pixel
        GLCDBitNo = 15 - ( (X0 ) And 15)

        If GLCDColour.0 = 0 Then
                  GLCDChangeWord = 65534
                  Set C On
        Else
                  GLCDChangeWord = 1
                  Set C Off
        End If

        Repeat GLCDBitNo
                  Rotate GLCDChangeWord LEFT
        End Repeat
        If GLCDColour.0 = 0 Then
                  NewData =  OldData  And GLCDChangeWord
        Else
                  NewData =  OldData  Or  GLCDChangeWord
        End If

     ' set location
        SET GLCD_RS OFF
        WriteByte_ST7920( SysCalcPositionY )
        WriteByte_ST7920( SysCalcPositionX )

        hbyte= NewData / 256
        SET GLCD_RS ON
        WriteByte_ST7920(hbyte)
        WriteByte_ST7920( NewData & 0x00FF )
    next
end sub


'''Read byte from LCD
'''@hide
Function GLCDReadByte_ST7920

  #IFNDEF GLCD_NO_RW
    #IFDEF GLCD_IO 4,8
      wait until GLCDReady_ST7920
      set GLCD_RW OFF 'Write mode
    #ENDIF
  #ENDIF

  'Set data pin directions
          #IFNDEF GLCD_LAT
            Dir GLCD_DATA_PORT In
          #endif

          #IFDEF GLCD_LAT
            Dir _GLCD_DATA_PORT In
          #endif

          set GLCD_RW ON
          Set GLCD_RS ON
  Set GLCD_ENABLE On
          Wait ST7920ReadDelay us

          'Get input data
          #IFNDEF GLCD_LAT
          GLCDReadByte_ST7920 = GLCD_DATA_PORT
          #ENDIF
          #IFDEF GLCD_LAT
         GLCDReadByte_ST7920 = _GLCD_DATA_PORT
          #ENDIF
          Set GLCD_ENABLE Off
End Function


'''@hide
function GLCDReady_ST7920

         #IF GLCD_TYPE = GLCD_TYPE_ST7920

              #IFDEF GLCD_NO_RW
                        ' Wait 10 ms

                        GLCDReady_ST7920 = TRUE
                        set GLCD_RW OFF 'Write mode
                        exit function
              #ENDIF

              #IFNDEF GLCD_NO_RW

                        #IFDEF GLCD_IO 4,8

                                  GLCDReady_ST7920 = FALSE
                                  GLCD_RSTemp = GLCD_RS

                                  SET GLCD_RW ON
                                  SET GLCD_RS OFF
                                  set GLCD_DATA_PORT.7 off

                                  SET GLCD_Enable ON

                                  #IFDEF GLCD_IO 8

                                            #IFNDEF GLCD_LAT
                                                   dir GLCD_DATA_PORT.7 IN

                                                   wait ST7920WriteDelay us
                                                   if GLCD_DATA_PORT.7 =  OFF THEN
                                                     GLCDReady_ST7920 = TRUE
                                                     dir GLCD_DATA_PORT.7 OUT
                                                     SET GLCD_Enable OFF
                                                     GLCD_RS = GLCD_RSTemp
                                                     exit sub
                                                   end if
                                            #ENDIF
                                            #IFDEF GLCD_LAT
                                                   dir _GLCD_DATA_PORT.7 IN
                                                   wait ST7920WriteDelay us
                                                   if _GLCD_DATA_PORT.7 =  OFF THEN
                                                     GLCDReady_ST7920 = TRUE
                                                     dir _GLCD_DATA_PORT.7 OUT
                                                     SET GLCD_Enable OFF
                                                     GLCD_RS = GLCD_RSTemp
                                                     exit sub
                                                   end if
                                            #ENDIF

                                  #ENDIF
                                  SET GLCD_Enable OFF
                                  wait ST7920WriteDelay us
                                  wait ST7920WriteDelay us
                                  GLCD_RS = GLCD_RSTemp
                        #ENDIF
              #ENDIF
         #endif

end function
