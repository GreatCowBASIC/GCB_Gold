'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2012-2025 Hugh Considine and Evan Venn

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
' Supports KS0108 controller only.

'Changes
' 14/4/2014: Fixed KS0108 GLCDCLS CS1/CS2 line set correctly.
' 05/5/2014: Revised Line, and added Circle and FilledCircle
' 05/5/2014: Revised KS0108 read sub... timing was incorrect
' 05/5/2014: Revised GLCD to ensure the screen is cleared properly
' 08/5/2014: Revised to comply with documentation requirements
' 09/5/2014: Fixed circle and line to ensure cross device compatibility
' 11/5/2014: Revided to expose old line drawing routines
' 17/6/2014: Revised to correct error in KS0108 PSET routine.
' 28/6/2014: Revised GLCDDrawString.  Xpos was always 1 extra pixel to right.
' 14/8/2014: Removed GLCDTimeDelay to improve timing and a tidy up.
' 1/11/2014: Revised to support single controller.
'
' 9/11/14 New revised version.  Requires GLCD.H.  Do not call directly.  Always load via GLCD.H
' 31/7/15 Added GLCDDirection test to invert display
' 17/10/15 Corrected KS0108ClockDelay was KS0108_Clock_Delay and the other waits were not implemented - most odd. Erv
' 11/02/19  Removed GLCDDirection constant from script as this was impacted KS0108 library
' 27/12/24  Add support for Inverted CS1 and CS2 control lines via the constnat GLCD_KS0108_CS_LOW
' 31/12/24  Add optimisation in PSET

'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_KS0108"
'Parallel lines (KS0108 only)
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 0; GLCD_DB0; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 1; GLCD_DB1; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 2; GLCD_DB2; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 3; GLCD_DB3; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 4; GLCD_DB4; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 5; GLCD_DB5; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 6; GLCD_DB6; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Data Bus 7; GLCD_DB7; IO_Pin
'Control lines (KS0108 only)
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Chip Select 1; GLCD_CS1; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Chip Select 2; GLCD_CS2; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Register Select; GLCD_RS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Read/Write; GLCD_RW; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Enable; GLCD_ENABLE; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Enable; GLCD_KS0108_CS_LOW; IO_Pin

'''@hardware GLCD_TYPE GLCD_TYPE_KS0108; Reset; GLCD_RESET; IO_Pin

#script
    KS0108WriteDelayExists = 0
    if KS0108WriteDelay then
        KS0108WriteDelay  = KS0108WriteDelay
        KS0108WriteDelayExists = 1
    end if
    if KS0108WriteDelayExists = 0 then
        KS0108WriteDelay  = 2     ; 2 normal usage, 3 for 32 mhz!
    end if

    KS0108WriteDelayExists = 0
    if KS0108WriteDelay then
        KS0108WriteDelay  = KS0108WriteDelay
        KS0108WriteDelayExists = 1
    end if
    if KS0108WriteDelayExists = 0 then
        KS0108WriteDelay  = 2     ; 2 normal usage, 3 for 32 mhz!
    end if

    KS0108ClockDelay = 1     ; 1 normal usage, 0 works

#endscript

#startup InitGLCD_KS0108

Sub InitGLCD_KS0108
  #if GLCD_TYPE = GLCD_TYPE_KS0108
    'Set pin directions
    Dir GLCD_RS Out
    Dir GLCD_RW Out
    Dir GLCD_ENABLE Out
    Dir GLCD_CS1 Out
    Dir GLCD_CS2 Out
    Dir GLCD_RESET Out

    'Reset
    Set GLCD_RESET Off
    Wait 1 ms
    Set GLCD_RESET On
    Wait 1 ms

    'Select both chips
    #ifdef GLCD_KS0108_CS_LOW
        Set GLCD_CS1 Off
        Set GLCD_CS2 Off
    #else
        Set GLCD_CS1 On
        Set GLCD_CS2 On
    #endif

    'Set on
    Set GLCD_RS Off
    GLCDWriteByte 63

    'Set Z to 0
    GLCDWriteByte 192

    'Deselect chips
    #ifdef GLCD_KS0108_CS_LOW
        Set GLCD_CS1 On
        Set GLCD_CS2 On
    #else
        Set GLCD_CS1 Off
        Set GLCD_CS2 Off
    #endif

    'Colours
    GLCDBackground = 0
    GLCDForeground = 1
    GLCDFontWidth = 5
    GLCDfntDefault = 0
    GLCDfntDefaultsize = 1
    GLCDfntDefaultHeight = 7
  #endif

  'Clear screen
  GLCDCLS_KS0108
End Sub

'Subs
'''Clears the GLCD screen
Sub GLCDCLS_KS0108
    GLCD_yordinate = 0

    #if GLCD_TYPE = GLCD_TYPE_KS0108
        #ifdef GLCD_KS0108_CS_LOW
            Set GLCD_CS1 Off
            Set GLCD_CS2 On
        #else
            Set GLCD_CS1 On
            Set GLCD_CS2 Off
        #endif

        for GLCD_Count = 1 to 2
            For CurrPage = 0 to 7
                'Set page
                Set GLCD_RS Off
                GLCDWriteByte b'10111000' Or CurrPage

                'Clear columns
                For CurrCol = 0 to 63
                    'Select column
                    Set GLCD_RS Off
                    GLCDWriteByte 64 Or CurrCol
                    'Clear
                    Set GLCD_RS On
                    GLCDWriteByte 0
                Next
            Next

            #ifdef GLCD_KS0108_CS_LOW
                Set GLCD_CS1 On
                Set GLCD_CS2 Off
            #else
                Set GLCD_CS1 Off
                Set GLCD_CS2 On
            #endif
        next

        #ifdef GLCD_KS0108_CS_LOW
            Set GLCD_CS1 On
            Set GLCD_CS2 On
        #else
            Set GLCD_CS1 Off
            Set GLCD_CS2 Off
        #endif
    #endif
End Sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox_KS0108(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour As Word = GLCDForeground)

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

  #if GLCD_TYPE = GLCD_TYPE_KS0108
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
Sub PSet_KS0108(In GLCDX, In GLCDY, In GLCDColour As Word)
    #if GLCD_TYPE = GLCD_TYPE_KS0108
        
        Dim GLCDDataTempCache as Byte
        
        #ifdef GLCDDirection
                GLCDX=127-GLCDX
                GLCDY=63-GLCDY
        #endif

        'Select screen half
        #ifdef GLCD_KS0108_CS_LOW
            If GLCDX.6 = Off Then Set GLCD_CS2 Off:Set GLCD_CS1 On
            If GLCDX.6 = On Then Set GLCD_CS1 Off: GLCDX -= 64: Set GLCD_CS2 On
        #else
            If GLCDX.6 = Off Then Set GLCD_CS2 On:Set GLCD_CS1 Off
            If GLCDX.6 = On Then Set GLCD_CS1 On: GLCDX -= 64: Set GLCD_CS2 Off
        #endif

        'Select page
        CurrPage = GLCDY / 8
        Set GLCD_RS Off+
        GLCDWriteByte b'10111000' Or CurrPage

        'Select column
        Set GLCD_RS Off
        GLCDWriteByte 64 Or GLCDX
        'Dummy read first
        Set GLCD_RS On
        GLCDDataTempCache = GLCDReadByte
        'Read current data
        Set GLCD_RS On
        GLCDDataTempCache = GLCDReadByte

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
            GLCDDataTemp = GLCDDataTempCache And GLCDChange
        Else
            GLCDDataTemp = GLCDDataTempCache Or GLCDChange
        End If

        // Optimised to only send data when it changes
        If GLCDDataTempCache <> GLCDDataTemp Then
            'Select correct column again
            Set GLCD_RS Off
            GLCDWriteByte 64 Or GLCDX
            'Write data back
            Set GLCD_RS On
            GLCDWriteByte GLCDDataTemp
        End If
        
        #ifdef GLCD_KS0108_CS_LOW
            Set GLCD_CS1 On
            Set GLCD_CS2 On
        #else
            Set GLCD_CS1 Off
            Set GLCD_CS2 Off
        #endif
    #endif
End Sub

#define GLCDWriteByte GLCDWriteByte_KS0108
'''Write byte to LCD
'''@hide
Sub GLCDWriteByte_KS0108 (In LCDByte)
    Dim GLCDSaveRS As Bit
    Dim GLCDSaveCS2 As Bit

    'Wait until LCD is available
    GLCDSaveRS = GLCD_RS
    GLCDSaveCS2 = GLCD_CS2
    #ifdef GLCD_KS0108_CS_LOW
        If GLCD_CS1 = 0 Then
            GLCD_CS2 = 1
        End If
    #else
        If GLCD_CS1 = 1 Then
            GLCD_CS2 = 0
        End If
    #endif
    Set GLCD_RS Off
    Wait Until GLCDReadByte.7 = Off
    GLCD_RS = GLCDSaveRS
    GLCD_CS2 = GLCDSaveCS2
    
  'Set LCD data direction
  Set GLCD_RW Off

    #IFNDEF GLCD_DATAPORT 
        'Set data pin directions
        #IFNDEF GLCD_LAT
            DIR GLCD_DB0 OUT
            DIR GLCD_DB1 OUT
            DIR GLCD_DB2 OUT
            DIR GLCD_DB3 OUT
            DIR GLCD_DB4 OUT
            DIR GLCD_DB5 OUT
            DIR GLCD_DB6 OUT
            DIR GLCD_DB7 OUT
        #ENDIF
        #IFDEF GLCD_LAT
            DIR _GLCD_DB0 OUT
            DIR _GLCD_DB1 OUT
            DIR _GLCD_DB2 OUT
            DIR _GLCD_DB3 OUT
            DIR _GLCD_DB4 OUT
            DIR _GLCD_DB5 OUT
            DIR _GLCD_DB6 OUT
            DIR _GLCD_DB7 OUT
        #ENDIF

        'Set output data
        GLCD_DB7 = LCDByte.7
        GLCD_DB6 = LCDByte.6
        GLCD_DB5 = LCDByte.5
        GLCD_DB4 = LCDByte.4
        GLCD_DB3 = LCDByte.3
        GLCD_DB2 = LCDByte.2
        GLCD_DB1 = LCDByte.1
        GLCD_DB0 = LCDByte.0
    #ELSE
        DIR GLCD_DATAPORT OUT
        GLCD_DATAPORT = LCDByte
    #ENDIF
  'Write
  Wait KS0108WriteDelay us
  Set GLCD_ENABLE On
  Wait KS0108ClockDelay us
  Set GLCD_ENABLE Off
  Wait KS0108WriteDelay us
End Sub

#define GLCDReadByte GLCDReadByte_KS0108
'''Read byte from LCD
'''@hide
Function GLCDReadByte_KS0108

  'Set data pin directions
  Dir GLCD_DB7 In
  Dir GLCD_DB6 In
  Dir GLCD_DB5 In
  Dir GLCD_DB4 In
  Dir GLCD_DB3 In
  Dir GLCD_DB2 In
  Dir GLCD_DB1 In
  Dir GLCD_DB0 In

  'Set LCD data direction
  Set GLCD_RW On

  'Read
  Set GLCD_ENABLE On
  Wait KS0108WriteDelay us
  'Get input data
          ' corrected 7/05/2014
  GLCDReadByte.7 = GLCD_DB7
  GLCDReadByte.6 = GLCD_DB6
  GLCDReadByte.5 = GLCD_DB5
  GLCDReadByte.4 = GLCD_DB4
  GLCDReadByte.3 = GLCD_DB3
  GLCDReadByte.2 = GLCD_DB2
  GLCDReadByte.1 = GLCD_DB1
  GLCDReadByte.0 = GLCD_DB0
  Set GLCD_ENABLE Off
  Wait KS0108WriteDelay us

End Function
