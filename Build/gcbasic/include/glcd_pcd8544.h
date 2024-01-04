'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2012-2024 Hugh Considine and Evan Venn

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
' Supports PCD8544 controller only.

'Changes
' 14/10/2012: First version (KS0108 only)
' 20/10/2014: Adapted to support PCD9844 devices.
' 21/10/2014: PCD9844 device improvements to remove a method and reduce configuration
' 22/10/2014: PCD9844 device improvements to handling of RAM limitations
'
' 9/11/14 New revised version.  Requires GLCD.H.  Do not call directly.  Always load via GLCD.H
' 13/12/14: Revised to support Linear array memory addressing.  Now requires the compiler after the date of 13/12/2014
' 17/11/17: Added GLCDPrintLn methods
' 26/04/18: Revised fontwidth now 5
' 31/05/22: Revised ExtendedCommand_PCD8544 to isolate parameter naming


'
'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_PCD8544"
'Serial lines (ST7735 only)
'''@hardware GLCD_TYPE GLCD_TYPE_PCD8544; Data/Command;         GLCD_DC; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_PCD8544; Chip Select;          GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_PCD8544; Data Out (LCD In);    GLCD_DO; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_PCD8544; Clock;                GLCD_SCK; IO_Pin
'''@hardware GLCD_TYPE_PCD8544; Reset;                          GLCD_RESET; IO_Pin

'Pin mappings for  PCD8544
#define PCD8544_DC              GLCD_DC
#define PCD8544_CS              GLCD_CS
#define PCD8544_RST             GLCD_RESET
#define PCD8544_DO              GLCD_DO
#define PCD8544_SCK             GLCD_SCK

#define _PCD8544_DC              _GLCD_DC
#define _PCD8544_CS              _GLCD_CS
#define _PCD8544_RST             _GLCD_RESET
#define _PCD8544_DO              _GLCD_DO
#define _PCD8544_SCK             _GLCD_SCK


'PCD8544 Details
'This program was written using the GCBASIC IDE for use with the PIC
'Microchio development board and a Nokia 3310 LCD. It is a nice small graphical LCD, suitable for a lot of various projects.
'The display is 38*35 mm, with an active display surface of 30*22 mm, and a 84*48 pixel resolution.
'The display is easy to interface, using an SPI type of communication.
'A 1-10 uF electrolytic capacitor from VOUT to GND, is the only external component needed.
' typically the device will need the following two defines
'    #define GLCD_WIDTH 84
'    #define GLCD_HEIGHT 48

    ' On the 16F1 chips, the linear data space from location 0x2000 maps back to the normal data space.
    ' So, each bank is 80 bytes long, so bank 0 starts at 0x2000 in the linear space, or 32 in the normal space.
    ' Bank 1 starts at 0x2050 in the linear space, or at 160 in the normal space and #bootloader 2 starts at 0x2100, or 288, and so on...
    ' So we can adddress the arrays via an alias of 0x2050 equates to address 160.
    ' For the 16f devices you will get a compiler warning with respect to memory addressing - please ignore.

    ' For 18f or AVR simply define the PCD8544_BufferAlias with 504 elements.
'          Dim PCD8544_BufferLocationCalc as Word
'
'          Dim PCD8544_Buffer1(79) at 160
'          Dim PCD8544_Buffer2(79) at 288
'          Dim PCD8544_Buffer3(79) at 416
'          Dim PCD8544_Buffer4(79) at 544
'          Dim PCD8544_Buffer5(79) at 672
'          Dim PCD8544_Buffer6(79) at 800
'          Dim PCD8544_Buffer7(79) at 928
'          Dim PCD8544_BufferAlias(2) at 0x2050

    ' For devices with limited RAM you can use Character and Picture mode (no lines, circles etc.
    ' Remove the arrays above and remove the remarks from the two defines below

'        #define GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY


'Setup code for PCD8544 controllers
#if GLCD_TYPE = GLCD_TYPE_PCD8544

    #script     ' This script set the capabilities based upon the amount of RAM

        If ChipRAM < 512  Then
'            if novar( GLCD_TYPE_PCD8544_IGNORE_MEMORY_WARNING  ) then
'                   warning "Not enough RAM for GLCD buffer on this chip model = Picture/text mode only "
'                   warning "."
'                   warning "Please use #Define GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY to use PCD8544 in text mode only"
'                   warning ""
'                   warning "Use #define GLCD_TYPE_PCD8544_IGNORE_MEMORY_WARNING to remove this warning"
'            end if

         GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY = TRUE
         end if





        End If

    #endscript

    dim PCD8544_BufferLocationCalc as Word               ' mandated in main program for PCD8544

    #ifndef GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY

       ' ChipFamily 12 for 10F/12F5/16F5, 14 for most 12F/16F, 15 for 12F1/16F1, 16 for 18F
       ' and, numbers 100, 110, 120, 130 for AVR,
       #ifdef ChipFamily 15
          '        Required for older compilers
          '        Dim PCD8544_Buffer1(79) at 160
          '        Dim PCD8544_Buffer2(79) at 288
          '        Dim PCD8544_Buffer3(79) at 416
          '        Dim PCD8544_Buffer4(79) at 544
          '        Dim PCD8544_Buffer5(79) at 672
          '        Dim PCD8544_Buffer6(79) at 800
          '        Dim PCD8544_Buffer7(79) at 928
          '        Dim PCD8544_BufferAlias(2) at 0x2050
         Dim PCD8544_BufferAlias(504)
       #endif

       #if ChipFamily <> 15
        Dim PCD8544_BufferAlias(504)
       #endif
    #endif

#endif

#startup InitGLCD_PCD8544

'''Initialise the GLCD device
Sub InitGLCD_PCD8544

          #if GLCD_TYPE = GLCD_TYPE_PCD8544
              ' required variables

                        #ifdef PCD8544_HardwareSPI
                         '  harware SPI mode
                          asm showdebug SPI constant used equates to HWSPIMODESCRIPT
                          SPIMode HWSPIMODESCRIPT, 0
                          wait 10 ms
                        #endif


                        #IFNDEF GLCD_LAT
                              'Pin directions
                              Dir PCD8544_CS Out
                              Dir PCD8544_DC Out
                              Dir PCD8544_RST Out

                              Dir PCD8544_DO Out
                              Dir PCD8544_SCK Out
                        #ENDIF
                        #IFDEF GLCD_LAT
                              'Pin directions
                              Dir _PCD8544_CS Out
                              Dir _PCD8544_DC Out
                              Dir _PCD8544_RST Out

                              Dir _PCD8544_DO Out
                              Dir _PCD8544_SCK Out
                        #ENDIF


                        'Reset ports
                        Set PCD8544_CS off
                        Set PCD8544_DC off
                        Set PCD8544_RST off
                        Set PCD8544_DO off
                        Set PCD8544_SCK off

                        ' Deselect Chip
                        SET PCD8544_CS OFF
                        Wait 10 ms
                        'Reset display
                        Set PCD8544_RST On
                        Wait 10 ms
                        'Reset sequence (lower line for at least 10 us)
                        Set PCD8544_RST Off
                        Wait 25 us
                        Set PCD8544_RST On
                        Wait 10 ms
                        PCD8544_CS = 1
                        wait 10 us
                        PCD8544_CS = 0
                        wait 10 ms

    ' /*********SEE DATASHEET FOR ALL THIS********/

                        Write_Command_PCD8544(0x21);     Activate Chip and H=1.
                        Write_Command_PCD8544(0x21);     Activate Chip and H=1.
                        Write_Command_PCD8544(0xC2);     Set LCD Voltage to about 7V.
                        Write_Command_PCD8544(0x13);     Adjust voltage bias.
                        Write_Command_PCD8544(0x20);     Horizontal addressing and H=0.
                        Write_Command_PCD8544(0x09);     Activate all segments.
                        Clear_RAM_PCD8544;               Erase all pixel on the DDRAM.
                        Write_Command_PCD8544(0x08);     Blank the Display.
                        Write_Command_PCD8544(0x0C);     Display Normal.
                        GOTO_Pixel_PCD8544(0,0);         Cursor Home.


                        'Colours
                        GLCDBackground = 0
                        GLCDForeground = 1
                        GLCDFontWidth = 5
                        GLCDfntDefault = 0
                        GLCDfntDefaultsize = 1
                        GLCDfntDefaultHeight = 7

          #endif

  'Clear screen
  GLCDCLS

End Sub


'Subs
'''Clears the GLCD screen
Sub GLCDCLS_PCD8544
          ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
          GLCD_yordinate = 0
          PrintLocY = 0
          #if GLCD_TYPE = GLCD_TYPE_PCD8544
              #ifndef GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY
                For PCD8544_BufferLocationCalc = 1 to 504
                    PCD8544_BufferAlias(PCD8544_BufferLocationCalc) = 0
                Next
              #endif

              GOTO_Pixel_PCD8544(0,0);  'Goto the pixel specified by the Co-ordinate
              for PCD8544_BufferLocationCalc = 504 to 1 step - 1
                  Write_Data_PCD8544(0x00);
              next
              GOTO_Pixel_PCD8544(0,0);  'Goto the pixel specified by the Co-ordinate

          #endif
          PrintLocX = 0
          PrintLocY = 0
End Sub

'''Draws a character at the specified location on the ST7920 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_PCD8544(In CharLocX, In CharLocY, In CharCode, Optional In LineColour as word = GLCDForeground )

  'CharCode needs to have 16 subtracted, table starts at char 16 not char 0
  CharCode -= 15

  'invert colors if required
          if LineColour <> GLCDForeground then
            'Inverted Colours
            GLCDBackground = 1
            GLCDForeground = 0
          end if

          'Need to read characters from CharColn (n = 0:7) tables
  '(First 3, ie 0:2 are blank, so can ignore)
  For CurrCharCol = 1 to 5
    Select Case CurrCharCol
      Case 1: ReadTable GLCDCharCol3, CharCode, CurrCharVal
      Case 2: ReadTable GLCDCharCol4, CharCode, CurrCharVal
      Case 3: ReadTable GLCDCharCol5, CharCode, CurrCharVal
      Case 4: ReadTable GLCDCharCol6, CharCode, CurrCharVal
      Case 5: ReadTable GLCDCharCol7, CharCode, CurrCharVal
    End Select
                    ' Handles general draw sequence. This caters for read display status then update display RAM
                    #ifndef GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY

                        'code replaced as part of font size support 7/2016
                        'For CurrCharRow = 1 to 8
                        '          If CurrCharVal.0 = 0 Then
                        '                    PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
                        '          Else
                        '                    PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDForeground
                        '          End If
                        '          Rotate CurrCharVal Right
                        'Next

                        'this code replaced the code above 7/2016
                        CharRow=0
                        For CurrCharRow = 1 to 8
                            CharColS=0
                            For Col=1 to GLCDfntDefaultsize
                                  CharColS +=1
                                  CharRowS=0
                                  For Row=1 to GLCDfntDefaultsize
                                      CharRowS +=1
                                      if CurrCharVal.0=1 then
                                         PSet [word]CharLocX + CurrCharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, GLCDForeground
                                      Else
                                         PSet [word]CharLocX + CurrCharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, GLCDBackground
                                      End if
                                  Next Row
                            Next Col
                          Rotate CurrCharVal Right
                          CharRow +=GLCDfntDefaultsize
                        Next
                        CharCol +=GLCDfntDefaultsize

                    #endif

                    ' Handles specific draw sequence. This caters for write only of a bit value. No read operation.
                    #ifdef GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY
                           Cursor_Position_PCD8544 ( ( CharLocX + CurrCharCol -1 ) , CharLocY )
                           If LineColour = 1 Then
                              Write_Data_PCD8544( CurrCharVal )
                           else
                              Write_Data_PCD8544( 255 - CurrCharVal )
                           end if
                    #endif
  Next


          GLCDBackground = 0
          GLCDForeground = 1

End Sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_PCD8544(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

  dim GLCDTemp  as byte
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

    'Draw lines going across
    For DrawLine = LineX1 To LineX2
      For GLCDTemp = LineY1 To LineY2
        PSet DrawLine, GLCDTemp, LineColour
      Next
    Next

End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel (0 = erase, 1 = display)
Sub PSet_PCD8544(In GLCDX, In GLCDY, In GLCDColour As Word)

  #if GLCD_TYPE = GLCD_TYPE_PCD8544
              if GLCDX => GLCD_WIDTH then exit sub
              if GLCDY => GLCD_HEIGHT then exit sub

              #ifndef GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY

                    'PCD8544_BufferLocationCalc = ( GLCDY / 8 )* GLCD_WIDTH

                    'faster than /8
                    PCD8544_BufferLocationCalc = GLCDY
                    Repeat 3
                      Set C Off
                      Rotate PCD8544_BufferLocationCalc Right
                    End Repeat
                    PCD8544_BufferLocationCalc = PCD8544_BufferLocationCalc * GLCD_WIDTH


                    PCD8544_BufferLocationCalc = GLCDX + PCD8544_BufferLocationCalc+1
                    GLCDDataTemp = PCD8544_BufferAlias(PCD8544_BufferLocationCalc)

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

                    if PCD8544_BufferAlias(PCD8544_BufferLocationCalc) <> GLCDDataTemp then
                      PCD8544_BufferAlias(PCD8544_BufferLocationCalc) = GLCDDataTemp
                      Cursor_Position_PCD8544 ( GLCDX, GLCDY )
                      Write_Data_PCD8544 ( GLCDDataTemp )
                    end if
              #endif

  #endif

End Sub

sub Write_Command_PCD8544(in  PCD8544SendByte)

      SET PCD8544_DC OFF;   'Data/Command is set to zero to give Command of PCD8544 Controller
      WAIT PCD8544WriteDelay us
      set PCD8544_CS off ;  'Select the Chip, Chip Enable is an Active Low Signal
      WAIT PCD8544WriteDelay us
      SPI_Send_Data_PCD8544(PCD8544SendByte);
      set PCD8544_CS on;  'Disable the Chip again by providing active high Signal
      WAIT PCD8544WriteDelay us
end sub

sub Write_Data_PCD8544(in PCD8544SendByte)

      SET PCD8544_DC ON;        'Data/Command is set to One to give Data of PCD8544 Controller
      WAIT PCD8544WriteDelay us
      set PCD8544_CS off; 'Select the Chip, Chip Enable is an Active Low Signal
      WAIT PCD8544WriteDelay us
      SPI_Send_Data_PCD8544(PCD8544SendByte);
      set PCD8544_CS on;  'Disable the Chip again by providing active high Signal
      WAIT PCD8544WriteDelay us
end sub


sub SPI_Send_Data_PCD8544(in PCD8544SendByte)

  #ifdef PCD8544_HardwareSPI
     SPITransfer  PCD8544SendByte,  PCD8544InByte
     exit sub
    #endif



  #ifndef PCD8544_HardwareSPI
        repeat 8                      '8 data bits
          wait PCD8544WriteDelay us
          if PCD8544SendByte.7 = ON then      'put most significant bit on SDA line
            set PCD8544_DO ON
          else
            set PCD8544_DO OFF
          end if

          rotate PCD8544SendByte left         'shift in bit for the next time
          wait PCD8544WriteDelay us
          SET PCD8544_SCK ON              'now clock it in
          wait PCD8544ClockDelay us
          SET PCD8544_SCK OFF               'done clocking that bit
        end repeat
        wait PCD8544WriteDelay us
    #endif

end sub

sub Clear_RAM_PCD8544

          #ifndef GLCD_TYPE_PCD8544_CHARACTER_MODE_ONLY
            For PCD8544_BufferLocationCalc = 1 to 505
                PCD8544_BufferAlias(PCD8544_BufferLocationCalc) = 0
            Next
          #endif
          GOTO_Pixel_PCD8544(0,0);  'Goto the pixel specified by the Co-ordinate
          for PCD8544_BufferLocationCalc = 503 to 0 step - 1
                    Write_Data_PCD8544(0x00);
          next

end sub

sub  GOTO_Pixel_PCD8544( In LocX, In  LocY)

  'Refer Instruction Set Page of datasheet
  'How to set Y-RAM Address and
  'How to set X-RAM Address
  Write_Command_PCD8544( 0x80 | (0x7F & LocX ));  'Set X-Address of RAM 0 <= x <= 83
  Write_Command_PCD8544( 0x40 | (0x07 & LocY ));  'Set Y-Address of RAM 0 <= y <= 5


end sub

'   Takes raw pixel positions and translates to XY char positions
sub Cursor_Position_PCD8544( in LocX, in LocY)

          PosCharY = LocY / 8

  Write_Command_PCD8544( 0x80 | (0x7F & LocX ));  'Set X-Address of RAM 0 <= x <= 83
          Write_Command_PCD8544( 0x40 | (0x07 & PosCharY ));  'Set Y-Address of RAM 0 <= y <= 5

end sub

Sub SetPixel_PCD8544( SetPixelLocX , SetPixelLocY, Optional In LineColour as word = GLCDForeground )

        row = SetPixelLocY / 8
        PCD8544_BufferLocationCalc = row * GLCD_WIDTH
        PCD8544_BufferLocationCalc = SetPixelLocX + PCD8544_BufferLocationCalc+1 ' Adjusted to the buffer start point

        value = BufferAlias(PCD8544_BufferLocationCalc)

                  ' create mask
                  systemp = SetPixelLocY % 8
                  mask = 1
                  repeat systemp
                         rotate mask
                  end Repeat
                  if ( LineColour = GLCDForeground ) then
                            'value = value | (1 << (LocY % 8))
                            value = value | mask
                  else
                            'value = value ^ (1 << (LocY % 8));
                            value = value ^ mask
                  end if

        BufferAlias(PCD8544_BufferLocationCalc) = value

end sub

sub Show_Buffer_PCD8544
PCD8544_BufferLocationCalc = 1

GOTO_Pixel_PCD8544 ( 0, 0 )
      'For AVR compatibility, status.c should also be replaced with C
      '(The compiler will convert that to STATUS.C on PIC or SREG.C on AVR as appropriate)
      for GLCDY = 0 to 5

          for GLCDX = 0 to 83
              GLCDYY = GLCDY * 8

              GLCDBitNo = 1
                repeat 8
                    GLCDBitNo =   GLCDBitNo % 9
                    mask = 0
                    SET C on

                      repeat GLCDBitNo
                             rotate mask left
                      end Repeat
                      ANSI ( (GLCDX + 2), GLCDYY + 5)
                      GLCDYY++
                      GLCDBitNo++
                end Repeat
                Write_Data_PCD8544 BufferAlias(PCD8544_BufferLocationCalc)
                PCD8544_BufferLocationCalc++

          next

      next

end sub

sub Buffer2Glcd_PCD8544
    PCD8544_BufferLocationCalc = 1
    GOTO_Pixel_PCD8544 ( 0, 0 )
    repeat 504
           Write_Data_PCD8544 BufferAlias(PCD8544_BufferLocationCalc)
           PCD8544_BufferLocationCalc++
    end Repeat

end sub


'Bits
'Temperature control  0  0  0  0  0  0  1  TC1  TC0  Set Temperature Coefficient (TCx)
'Bias system          0  0  0  0  1  0  BS2  BS1  BS0  set Bias System (BSx)
'Set Vop              0  1  Vop6  Vop5  Vop4  Vop3  Vop2  Vop1  Vop0  Write Vop to register

sub ExtendedCommand_PCD8544 ( in PCD8544SendExtendedByte as Byte )
                    Write_Command_PCD8544(0x21);                            LCD Extended Commands..
                    Write_Command_PCD8544(PCD8544SendExtendedByte);         Set PCD8544SendByte
                    Write_Command_PCD8544(0x20);                            LCD Standard Commands..
end sub
