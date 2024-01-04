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
' Supports XXXXXX controller only.

'Changes
' 9/11/14 New revised version.  Requires GLCD.H.  Do not call directly.  Always load via GLCD.H
'

'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_XXXXXX"
'Parallel lines (XXXXXX only)
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 0; GLCD_DB0; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 1; GLCD_DB1; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 2; GLCD_DB2; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 3; GLCD_DB3; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 4; GLCD_DB4; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 5; GLCD_DB5; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 6; GLCD_DB6; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Data Bus 7; GLCD_DB7; IO_Pin

'Control lines (XXXXXX only)
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Chip Select 1; GLCD_CS1; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Chip Select 2; GLCD_CS2; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Register Select; GLCD_RS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Read/Write; GLCD_RW; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Enable; GLCD_ENABLE; IO_Pin

'''@hardware GLCD_TYPE GLCD_TYPE_XXXXXX; Reset; GLCD_RESET; IO_Pin

#startup InitGLCD_XXXXXX

Sub InitGLCD_XXXXXX

  'Setup code for XXXXXX controllers
  #if GLCD_TYPE = GLCD_TYPE_XXXXXX
    'Set pin directions

    'Reset

    'Colours
    GLCDBackground = 0
    GLCDForeground = 1
                GLCDFontWidth = 6
    GLCDfntDefaultsize = 1

  #endif

  'Clear screen
  GLCDCLS_XXXXXX

End Sub



'Subs
'''Clears the GLCD screen
Sub GLCDCLS_XXXXXX

          ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
          GLCD_yordinate = 0
  #if GLCD_TYPE = GLCD_TYPE_XXXXXX

  #endif

End Sub

'''Draws a character at the specified location on the ST7920 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0
Sub GLCDDrawChar_XXXXXX(In CharLocX, In CharLocY, In CharCode, Optional In LineColour as word = GLCDForeground )

End Sub



'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_XXXXXX(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)


End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel (0 = erase, 1 = display)
Sub PSet_XXXXXX(In GLCDX, In GLCDY, In GLCDColour As Word)

  #if GLCD_TYPE = GLCD_TYPE_XXXXXX

  #endif

End Sub

' This is an Published API do not change
#define GLCDWriteByte GLCDWriteByte_XXXXXX

'''Write byte to LCD
'''@hide
Sub GLCDWriteByte_XXXXXX (In LCDByte)


End Sub

' This is an Published API do not change
#define GLCDReadByte GLCDReadByte_XXXXXX

'''Read byte from LCD
'''@hide
Function GLCDReadByte_XXXXXX


End Function
