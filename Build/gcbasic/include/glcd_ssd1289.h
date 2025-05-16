'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2018-2024 Dimitris  Katsaounis based on works of Hugh Considine and  Evan Venn

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
'    31st Aug 2015 Initial release
'    1st Sept 2015 Minor correction to color and other parameters.
'    25h Oct 2015 Corrected INIT
'    02.05.2017   Reverted to standard line routine. Local version overan Byte values
'                 Add PSET limits to prevent memory overwriting
'    23.11.2018   Removed SuperCededLine method.
'    30.12.2018   Updated FilledBox to standard method and OLED fonts
'    28.01.2019   Reverted GLCDPrint as this version has SIZE.
'    03.04.2019   Mapped DrawBMP_SSD1289 to DrawBMP in GLCD.H
'                 Revised to support DEFAULT_GLCDBACKGROUND constant
'  27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn

'Notes:
' Supports SSD1289 controller only.

'Hardware settings
'Type
'''@hardware All; Controller Type; GLCD_TYPE; "GLCD_TYPE_SSD1289"
'Serial lines (SSD1289 only)
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;     ????; GLCD_WR; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;     ????; GLCD_CS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;     ????; GLCD_RS; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;            Reset; GLCD_RST; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.0; GLCD_DB0; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.1; GLCD_DB1; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.2; GLCD_DB2; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.3; GLCD_DB3; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.4; GLCD_DB4; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.5; GLCD_DB5; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.6; GLCD_DB6; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.7; GLCD_DB7; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.8; GLCD_DB8; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.9; GLCD_DB9; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;           Port.10; GLCD_DB10; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;          Port.11; GLCD_DB11; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;          Port.12; GLCD_DB12; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;          Port.13; GLCD_DB13; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;          Port.14; GLCD_DB14; IO_Pin
'''@hardware GLCD_TYPE GLCD_TYPE_SSD1289;          Port.15; GLCD_DB15; IO_Pin
'''@font GLCD_EXTENDEDFONTSET1


'Define Orientation

#define LANDSCAPE   1
#define PORTRAIT_REV  2
#define LANDSCAPE_REV 3
#define PORTRAIT    4

'Extended font set
'#define GLCD_EXTENDEDFONTSET1

' Define Colors
#define SSD1289_BLACK   0x0000
#define SSD1289_RED     0xF800
#define SSD1289_GREEN   0x07E0
#define SSD1289_BLUE    0x001F
#define SSD1289_WHITE   0xFFFF
#define SSD1289_PURPLE  0xF11F
#define SSD1289_YELLOW  0xFFE0
#define SSD1289_CYAN    0x07FF
#define SSD1289_D_GRAY  0x528A
#define SSD1289_L_GRAY  0x7997
#define SSD1289_SILVER  0xC618
#define SSD1289_MAROON  0x8000
#define SSD1289_OLIVE   0x8400
#define SSD1289_LIME    0x07E0
#define SSD1289_AQUA    0x07FF
#define SSD1289_TEAL    0x0410
#define SSD1289_NAVY    0x0010
#define SSD1289_FUCHSIA 0xF81F


'Define ports ' map from generic user program to device specific format
#define SSD1289_WR   GLCD_WR
#define SSD1289_CS   GLCD_CS
#define SSD1289_RS   GLCD_RS
#define SSD1289_RST  GLCD_RST
#define SSD1289_DB0  GLCD_DB0
#define SSD1289_DB1  GLCD_DB1
#define SSD1289_DB2  GLCD_DB2
#define SSD1289_DB3  GLCD_DB3
#define SSD1289_DB4  GLCD_DB4
#define SSD1289_DB5  GLCD_DB5
#define SSD1289_DB6  GLCD_DB6
#define SSD1289_DB7  GLCD_DB7
#define SSD1289_DB8  GLCD_DB8
#define SSD1289_DB9  GLCD_DB9
#define SSD1289_DB10 GLCD_DB10
#define SSD1289_DB11 GLCD_DB11
#define SSD1289_DB12 GLCD_DB12
#define SSD1289_DB13 GLCD_DB13
#define SSD1289_DB14 GLCD_DB14
#define SSD1289_DB15 GLCD_DB15

#define DrawBMP_SSD1289 DrawBMP


#startup InitGLCD_SSD1289

'''Sets the bus to the Word value
'''@param DataVal the Word value to set the bus
Sub SetBus_SSD1289 (In DataVal as Word)
    SSD1289_DB0=DataVal.0
    SSD1289_DB1=DataVal.1
    SSD1289_DB2=DataVal.2
    SSD1289_DB3=DataVal.3
    SSD1289_DB4=DataVal.4
    SSD1289_DB5=DataVal.5
    SSD1289_DB6=DataVal.6
    SSD1289_DB7=DataVal.7
    SSD1289_DB8=DataVal_H.0
    SSD1289_DB9=DataVal_H.1
    SSD1289_DB10=DataVal_H.2
    SSD1289_DB11=DataVal_H.3
    SSD1289_DB12=DataVal_H.4
    SSD1289_DB13=DataVal_H.5
    SSD1289_DB14=DataVal_H.6
    SSD1289_DB15=DataVal_H.7
End Sub

'''Send Command to device.
'''@param Wcommand
Sub Write_Command_SSD1289( in Wcommand as Word)
  SSD1289_RS = 0
  SetBus_SSD1289 (Wcommand)
  SSD1289_WR = 0
  SSD1289_WR = 1
End Sub

'''Send data to the device.
'''@param Wdata
Sub Write_Data_SSD1289(In Wdata as Word)
  SSD1289_RS = 1
  SetBus_SSD1289 (Wdata)
  SSD1289_WR = 0
  SSD1289_WR = 1
End Sub

'''Send a command and data values to the display
'''@param Wcommand
'''@param Wdata Y
Sub Write_Command_Data_SSD1289(in Wcommand as Word, in Wdata as Word)
   Write_Command_SSD1289 (Wcommand)
   Write_Data_SSD1289 (Wdata)
End Sub

'''Sets the memory address of device.
'''@param PX1 X
'''@param PY1 Y
'''@param PX2 X
'''@param PY2 Y
Sub Set_Address_SSD1289(In PX1 as word, In PY1 as word, In PX2 as word, In PY2 as word)
  if Orientation_SSD1289=LANDSCAPE or Orientation_SSD1289=LANDSCAPE_REV then
     SWAP (PX1,PY1)
     SWAP (PX2,PY2)
     PX1=SSD1289_GLCD_HEIGHT-PX1
     PX2=SSD1289_GLCD_HEIGHT-PX2
     SWAP (PX1 , PX2)
  end if
  GLCDTempXY = PX2
  Repeat 8
    Rotate PX2 Left
  End Repeat
  PX2=[Word]PX2+PX1
  Write_Command_Data_SSD1289 (0x0044,PX2)  'Column address start2
  Write_Command_Data_SSD1289 (0x0045,PY1)  'Column address start1
  Write_Command_Data_SSD1289 (0x0046,PY2)  'Column address end2
  Write_Command_Data_SSD1289 (0x004E,PX1)  'Column address end1
  Write_Command_Data_SSD1289 (0x004F,PY1)  'Row address start2
  Write_Command_SSD1289 (0x0022)
  PX2 = GLCDTempXY
End Sub




'''Initialises the device
Sub InitGLCD_SSD1289

    #if GLCD_TYPE = GLCD_TYPE_SSD1289

        Dim Orientation_SSD1289 as Byte

        Dim GLCDBackground as Byte
        Orientation_SSD1289 = 4 'PORTRAIT

        Dim SSD1289_GLCD_WIDTH, SSD1289_GLCD_HEIGHT as Word


            dir GLCD_WR   out
            dir GLCD_CS   out
            dir GLCD_RS   out
            dir GLCD_RST  out
            dir GLCD_DB0  out
            dir GLCD_DB1  out
            dir GLCD_DB2  out
            dir GLCD_DB3  out
            dir GLCD_DB4  out
            dir GLCD_DB5  out
            dir GLCD_DB6  out
            dir GLCD_DB7  out
            dir GLCD_DB8  out
            dir GLCD_DB9  out
            dir GLCD_DB10 out
            dir GLCD_DB11 out
            dir GLCD_DB12 out
            dir GLCD_DB13 out
            dir GLCD_DB14 out
            dir GLCD_DB15 out

            SSD1289_CS =1
            SSD1289_RST=1
            Wait 5 ms
            SSD1289_RST=0
            Wait 15 ms
            SSD1289_RST=1
            Wait 15 ms
            SSD1289_CS =0

            Write_Command_Data_SSD1289 (0x0000,0x0001)
            Write_Command_Data_SSD1289 (0x0003,0xA8A4)
            Write_Command_Data_SSD1289 (0x000C,0x0000)
            Write_Command_Data_SSD1289 (0x000D,0x800C)
            Write_Command_Data_SSD1289 (0x000E,0x2B00)
            Write_Command_Data_SSD1289 (0x001E,0x00B7)
            Write_Command_Data_SSD1289 (0x0002,0x0600)
            Write_Command_Data_SSD1289 (0x0010,0x0000)
            Select Case Orientation_SSD1289
               case LANDSCAPE_REV
                    SSD1289_GLCD_WIDTH = GLCD_HEIGHT-1
                    SSD1289_GLCD_HEIGHT = GLCD_WIDTH-1
                    Write_Command_Data_SSD1289 (0x0001,0x2B3F)
                    Write_Command_Data_SSD1289 (0x0011,0x6070)
               case LANDSCAPE
                    SSD1289_GLCD_WIDTH = GLCD_HEIGHT-1
                    SSD1289_GLCD_HEIGHT = GLCD_WIDTH-1
                    Write_Command_Data_SSD1289 (0x0001,0x693F)
                    Write_Command_Data_SSD1289 (0x0011,0x6070)
               case PORTRAIT_REV
                    SSD1289_GLCD_WIDTH = GLCD_WIDTH-1
                    SSD1289_GLCD_HEIGHT = GLCD_HEIGHT-1
                    Write_Command_Data_SSD1289 (0x0001,0x693F)
                    Write_Command_Data_SSD1289 (0x0011,0x6070)
               case Else
                    SSD1289_GLCD_WIDTH = GLCD_WIDTH-1
                    SSD1289_GLCD_HEIGHT = GLCD_HEIGHT-1
                    Write_Command_Data_SSD1289 (0x0001,0x2B3F)
                    Write_Command_Data_SSD1289 (0x0011,0x6070)
            end Select
            Write_Command_Data_SSD1289 (0x0005,0x0000)
            Write_Command_Data_SSD1289 (0x0006,0x0000)
            Write_Command_Data_SSD1289 (0x0016,0xEF1C)
            Write_Command_Data_SSD1289 (0x0017,0x0003)
            Write_Command_Data_SSD1289 (0x0007,0x0233)
            Write_Command_Data_SSD1289 (0x000B,0x0000)
            Write_Command_Data_SSD1289 (0x000F,0x0000)
            Write_Command_Data_SSD1289 (0x0041,0x0000)
            Write_Command_Data_SSD1289 (0x0042,0x0000)
            Write_Command_Data_SSD1289 (0x0048,0x0000)
            Write_Command_Data_SSD1289 (0x0049,0x013F)
            Write_Command_Data_SSD1289 (0x004A,0x0000)
            Write_Command_Data_SSD1289 (0x004B,0x0000)
            Write_Command_Data_SSD1289 (0x0044,0xEF95)
            Write_Command_Data_SSD1289 (0x0045,0x0000)
            Write_Command_Data_SSD1289 (0x0046,0x013F)
            Write_Command_Data_SSD1289 (0x0030,0x0707)
            Write_Command_Data_SSD1289 (0x0031,0x0204)
            Write_Command_Data_SSD1289 (0x0032,0x0204)
            Write_Command_Data_SSD1289 (0x0033,0x0502)
            Write_Command_Data_SSD1289 (0x0034,0x0507)
            Write_Command_Data_SSD1289 (0x0035,0x0204)
            Write_Command_Data_SSD1289 (0x0036,0x0204)
            Write_Command_Data_SSD1289 (0x0037,0x0502)
            Write_Command_Data_SSD1289 (0x003A,0x0302)
            Write_Command_Data_SSD1289 (0x003B,0x0302)
            Write_Command_Data_SSD1289 (0x0023,0x0000)
            Write_Command_Data_SSD1289 (0x0024,0x0000)
            Write_Command_Data_SSD1289 (0x0025,0x8000)
            Write_Command_Data_SSD1289 (0x004f,0x0000)
            Write_Command_Data_SSD1289 (0x004e,0x0000)
            Write_Command_SSD1289(0x0022)
            SSD1289_CS =1


            GLCDFontWidth = 6
            'Default size
            GLCDfntDefaultSize = 2
            GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn


            GLCDForeground = 0xffff

            'Default Colours
            #ifdef DEFAULT_GLCDBACKGROUND
              GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
            #endif

            #ifndef DEFAULT_GLCDBACKGROUND
              GLCDBACKGROUND = SSD1289_BLACK
            #endif

            GLCDCLS_SSD1289 GLCDBackground

    #endif

End Sub

sub   GLCDRotate_SSD1289 ( In Orientation_SSD1289, Optional in GLCDBackground=GLCDBackground)
  Dim SSD1289_GLCD_WIDTH, SSD1289_GLCD_HEIGHT as Word
  select case Orientation_SSD1289
        case LANDSCAPE
             SSD1289_GLCD_WIDTH = GLCD_HEIGHT-1
             SSD1289_GLCD_HEIGHT = GLCD_WIDTH-1
             SSD1289_CS =0
             Write_Command_Data_SSD1289(0x0001,0x693F)
             Write_Command_Data_SSD1289(0x0011,0x6070)
             SSD1289_CS =1
             wait 1 ms
        case PORTRAIT_REV
             SSD1289_GLCD_WIDTH = GLCD_WIDTH-1
             SSD1289_GLCD_HEIGHT = GLCD_HEIGHT-1
             SSD1289_CS =0
             Write_Command_Data_SSD1289(0x0001,0x693F)
             Write_Command_Data_SSD1289(0x0011,0x6070)
             SSD1289_CS =1
             wait 1 ms
        case LANDSCAPE_REV
             SSD1289_GLCD_WIDTH = GLCD_HEIGHT-1
             SSD1289_GLCD_HEIGHT = GLCD_WIDTH-1
             SSD1289_CS =0
             Write_Command_Data_SSD1289(0x0001,0x2B3F)
             Write_Command_Data_SSD1289(0x0011,0x6070)
             SSD1289_CS =1
             wait 1 ms
        case else
             SSD1289_GLCD_WIDTH = GLCD_WIDTH-1
             SSD1289_GLCD_HEIGHT = GLCD_HEIGHT-1
             SSD1289_CS =0
             Write_Command_Data_SSD1289(0x0001,0x2B3F)
             Write_Command_Data_SSD1289(0x0011,0x6070)
             SSD1289_CS =1
             wait 1 ms
  end select

end sub



'''Compute SSD1289_COLOR from R-G-B Values
Function Set_color_SSD1289(In rC as Byte, In gC as Byte, In bC)as Word
  Set_color_H=(rC and 0xF8) or ([Word]gC/16)
  gC=(gC and 0x1C)
  Set_color=([Word]gC*8) or ([Word]bC/8)
End Function

'''Clears the devices
'''@param GLCDBackground Optional background color
Sub GLCDCLS_SSD1289( Optional In  GLCDBackground as word = GLCDBackground)
  Dim CountI,CountJ as Word
  SSD1289_CS  = 0
  Write_Command_Data_SSD1289(0x0044,0xEF00)
  Write_Command_Data_SSD1289(0x0045,0x0000)
  Write_Command_Data_SSD1289(0x0046,0x013F)
  Write_Command_Data_SSD1289(0x004E,0x0000)
  Write_Command_Data_SSD1289(0x004F,0x0000)
  Write_Command_SSD1289(0x0022)
  Write_Data_SSD1289 (GLCDBackground)
  for CountI = 0 to 319
    for CountJ = 0 to 239
        SSD1289_WR = 0
        SSD1289_WR = 1
    next CountJ
  next CountI
  SSD1289_CS  = 1
End Sub

'''Draws a box
'''@param BX1 X1 Position
'''@param BY1 Y1 Position
'''@param BX2 X2 Position
'''@param BY2 Y2 Position
'''@param color Optional
Sub Box_SSD1289(In BX1 as word, In BY1 as word, In  BX2 as word, In  BY2 as word, Optional In  FColor as word = GLCDForeground )

         'Make sure that starting point (1) is always less than end point (2)
  If BX1 > BX2 Then
    GLCDTempXY = BX1
    BX1 = BX2
    BX2 = GLCDTempXY
  End If
  If BY1 > BY2 Then
    GLCDTempXY = BY1
    BY1 = BY2
    BY2 = GLCDTempXY
  End If
          'Draw lines going across
          Line BX1 , BY1 , BX2 , BY1 , FColor
          Line BX1 , BY2 , BX2 , BY2 , FColor

  'Draw lines going down
          Line BX1 , BY1 , BX1 , BY2 , FColor
          Line BX2 , BY1 , BX2 , BY2 , FColor

End Sub

'''Draws a filled box
'''@param PX1 X1 Position
'''@param PY1 Y1 Position
'''@param PX2 X2 Position
'''@param PY2 Y2 Position
'''@param color Optional
Sub FilledBox_SSD1289(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)
  dim GLCDTemp, DrawLine as word
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

'''@hide
Sub Pset_SSD1289(In  PX1 as word, In  PY1 as word, Optional In  Color as Word = GLCDForeground)

  if PX1 > SSD1289_GLCD_WIDTH or PY1 > SSD1289_GLCD_HEIGHT then exit sub
  SSD1289_CS  = 0
  Set_Address_SSD1289 (PX1,PY1,PX1,PY1)
  Write_Data_SSD1289 (Color)
  SSD1289_CS  = 1
End Sub




' Try this with SetScrollPosition_SSD1289
  ' for pos =0 to 319
   ' SetScrollPosition_SSD1289 (pos)
   ' wait 30 ms
   'next
   'for pos = 319 to 0 step -1
   '  SetScrollPosition_SSD1289 (pos)
    ' wait 30 ms
   'next

Sub SetScrollPosition_SSD1289 (In scrollPosition as Integer)
    if scrollPosition<0 then scrollPosition+=320
    if scrollPosition>319 then scrollPosition-=320
    SSD1289_CS =0
    Write_Command_Data_SSD1289(0x000F,scrollPosition)
    SSD1289_CS =1
End Sub

'''Draws circle
'''@param xoffset X  position at the centre of circle
'''@param yoffset Y  position at the centre of circle
'''@param xradius Radius of circle
'''@param Color Optional color
'''@param yordinate Optional corner handling
Sub Circle_SSD1289 ( In xoffset as Word, In yoffset as Word, In xradius as integer, Optional In  LineColour as word = GLCDForeground , Optional In yordinate = 0)
    Dim  radiusErr as Integer
    radiusErr = -(xradius/2)
    Do While xradius >=  yordinate
       PSet_SSD1289 ((xoffset + xradius), (yoffset + yordinate), LineColour)
       PSet_SSD1289 ((xoffset + yordinate), (yoffset + xradius), LineColour)
       PSet_SSD1289 ((xoffset - xradius), (yoffset + yordinate), LineColour)
       PSet_SSD1289 ((xoffset - yordinate), (yoffset + xradius), LineColour)
       PSet_SSD1289 ((xoffset - xradius), (yoffset - yordinate), LineColour)
       PSet_SSD1289 ((xoffset - yordinate), (yoffset - xradius), LineColour)
       PSet_SSD1289 ((xoffset + xradius), (yoffset - yordinate), LineColour)
       PSet_SSD1289 ((xoffset + yordinate), (yoffset - xradius), LineColour)
       yordinate ++
       If radiusErr < 0 Then
          radiusErr = radiusErr + 2 * yordinate + 1
       else
          xradius --
          radiusErr = radiusErr + 2 * (yordinate - xradius + 1)
       end if
    Loop
End Sub

'''Draws a filled circle
'''@param xoffset X  position at the centre of circle
'''@param yoffset Y  position at the centre of circle
'''@param xradius Radius of circle
'''@param Color Optional color
Sub FilledCircle_SSD1289( In xoffset as Word, In yoffset as Word, In xradius as Word , Optional In  LineColour as word = GLCDForeground  )

  Dim ff, ddF_x, ddF_y as integer
  ff = 1 - xradius
  ddF_x = 1
  ddF_y = -2 * xradius
  FillCircleXX = 0
  FillCircleYY = xradius

  ' Fill in the center between the two halves
  Line( xoffset, yoffset-xradius , xoffset, yoffset+xradius, LineColour)

  Do while (FillCircleXX < FillCircleYY)
             if ff >= 0 then
                FillCircleYY--
                ddF_y += 2
                ff += ddF_y
             end if
     FillCircleXX++
     ddF_x += 2
     ff += ddF_x
     Line (xoffset+FillCircleXX, yoffset+FillCircleYY, xoffset+FillCircleXX, yoffset-FillCircleYY, LineColour)
     Line (xoffset-FillCircleXX, yoffset+FillCircleYY, xoffset-FillCircleXX, yoffset-FillCircleYY, LineColour)
     Line (xoffset+FillCircleYY, yoffset+FillCircleXX, FillCircleYY+xoffset, yoffset-FillCircleXX, LineColour)
     Line (xoffset-FillCircleYY, yoffset+FillCircleXX, xoffset-FillCircleYY, yoffset-FillCircleXX, LineColour)
    Loop
End Sub



'Text Routines

'''Prints small on device
'''@param PrintLocX X position
'''@param PrintLocY Y position
'''@param PrintData String to be printed
'''@param Color Optional color
Sub Print_SSD1289(In PrintLocX as Word, In PrintLocY as Word, In PrintData As String, Optional In  GLCDForeground as word = GLCDForeground, Optional In Size=GLCDfntDefaultsize )
          Dim GLCDPrintLoc as Word
          PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  'Write Data
  For SysPrintTemp = 1 To PrintLen
    GLCDDrawChar_SSD1289 GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), GLCDForeground , Size
    GLCDPrintLoc += GLCDFontWidth*Size
  Next
End Sub

'''Displays a number
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param GLCDValue Number to display
'''@param Color Optional color
Sub Print_SSD1289(In PrintLocX as Word, In PrintLocY as Word, In GLCDValue As Long, Optional In  GLCDForeground as word = GLCDForeground, Optional In Size=GLCDfntDefaultsize)
  Dim GLCDPrintLoc as Word
  Dim SysCalcTempA As Long
  Dim SysPrintBuffer(10)
  SysPrintBuffLen = 0
  Do
    'Divide number by 10, remainder into buffer
    SysPrintBuffLen += 1
    SysPrintBuffer(SysPrintBuffLen) = GLCDValue %10
    GLCDValue = SysCalcTempA
  Loop While GLCDValue <> 0

  'Display
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = SysPrintBuffLen To 1 Step -1
       GLCDDrawChar_SSD1289 GLCDPrintLoc, PrintLocY, SysPrintBuffer(SysPrintTemp) + 48, GLCDForeground , Size
       GLCDPrintLoc += GLCDFontWidth*Size
  Next

End Sub


Sub GLCDDrawChar_SSD1289(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

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
                           PSet [word]CharLocX + CharCol+ CharColS, [word]CharLocY + CharRow+CharRowS, GLCDBackground
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
                  CurrCharVal = GLCDBackground
                #endif
          End Select

            'we handle 8 or 16 pixels of height
            For CurrCharRow = 0 to ROWSperfont
                'Set the pixel
                If CurrCharVal.0 = 0 Then
                          PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
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
                    CurrCharVal = GLCDBackground
                  #endif
                end if

                'It is the intercharacter space, put out one pixel row
                if CurrCharCol = COLSperfont then
                    'Put out a white intercharacter pixel/space
                     GLCDTemp = CharLocX + CurrCharCol
                     if GLCDfntDefaultSize = 2 then
                        GLCDTemp++
                     end if
                     PSet GLCDTemp , CharLocY + CurrCharRow, GLCDBackground
                end if

            Next



        Next


    #endif

End Sub

'''Prints single character
'''@param CharLocX X position
'''@param CharLocY Y position
'''@param CharLocY Character to be printed
'''@param Color Optional color
Sub OldDrawChar_SSD1289(In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  GLCDForeground as word = GLCDForeground , Optional In Size = GLCDfntDefaultsize)
  Dim CharCol , CharRow , CharColS ,  CharRowS as Word
  CharCode -= 15
  if CharCode>=178 and CharCode<=202 then
     CharLocY=CharLocY-1
  end if
  CharCol=1
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
        For Col=1 to Size
              CharColS +=1
              CharRowS=0
              For Row=1 to Size
                  CharRowS +=1
                  if CurrCharVal.0=1 then
                     PSet_SSD1289 CharLocX + CharCol+ CharColS, CharLocY + CharRow+CharRowS, GLCDForeground
                  Else
                     PSet_SSD1289 CharLocX + CharCol+ CharColS, CharLocY + CharRow+CharRowS, GLCDBackground
                  End if
              Next Row
        Next Col
        Rotate CurrCharVal Right
       CharRow +=Size
    Next
    CharCol +=Size
  Next
End Sub

'''Displays a string in a larger font
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData String to display
'''@param Color Optional color
Sub BigPrint_SSD1289(In PrintLocX as Word , In PrintLocY as Word,  PrintData As String, Optional In  Color as word = GLCDForeground)
  Dim GLCDPrintLoc as Word
          PrintLen = PrintData(0)
  If PrintLen = 0 Then Exit Sub
  GLCDPrintLoc = PrintLocX
  For SysPrintTemp = 1 To PrintLen
    DrawBigChar_SSD1289 GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp), Color
    GLCDPrintLoc += 13
  Next
End Sub

'''Displays a character in a larger font
Sub DrawBigChar_SSD1289 (In CharLocX as Word, In CharLocY as Word, In CharCode, Optional In  Color as word = GLCDForeground )
    Dim LocX , LocY as Word
    if CharCode <=126 Then
       CharCode -=32
       Goto Tables
    end if
    if CharCode <=210 Then
       CharCode -=33
       Goto Tables
    end if
    if CharCode <= 250 Then
       CharCode -=34
       Goto Tables
    end if
    Tables:
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

        if CurrCharVal=36 then CurrCharVal=33 'For Arduino only
        For CurrCharRow = 1 to 8
          LocX=[word]CharLocX+CurrCharCol
          LocY=[word]CharLocY+CurrCharRow
          if CurrCharCol>12 then
             LocX= LocX - 12
             LocY= LocY + 8
          end if
          if CurrCharVal.0=1 then
             PSet_SSD1289 LocX , LocY , Color
          end if
  Rotate CurrCharVal Right
        Next
    Next
End Sub

'''Draws a symbol on the GLCD
Sub DrawBigSymbol_SSD1289 (In CharLocX, In CharLocY as Word, In CharCode, Optional In  GLCDForeground as word = GLCDForeground )
    CharCode -=1
    For CurrCharCol = 1 to 24
        CurrCol=CurrCharCol+CharCode*24-(CharCode/10)*240
        if CharCode>=0 and CharCode<=9 then
           ReadTable Symbols1 , CurrCol, CurrCharVal
           Goto EndSymbol
        end if
        if CharCode>=10 and CharCode<=19 then
           ReadTable Symbols2 , CurrCol, CurrCharVal
           Goto EndSymbol
        end if
        if CharCode>=20 and CharCode<=29 then
           ReadTable Symbols3 , CurrCol, CurrCharVal
           Goto EndSymbol
        end if
        if CharCode>=30 and CharCode<=39 then
           ReadTable Symbols4 , CurrCol, CurrCharVal
        end if
        EndSymbol:
        if CurrCharVal=37 then CurrCharVal=33 'only for Arduino
        For CurrCharRow = 1 to 8
            LocX=CharLocX+CurrCharCol
            LocY=CharLocY+CurrCharRow
            if CurrCharCol>12 then
               LocX= LocX - 12
               LocY= LocY + 8
            end if
            if CurrCharVal.0=1 then
               PSet_SSD1289 LocX , LocY , GLCDForeground
            Else
               PSet_SSD1289 LocX , LocY , GLCDBackground
            end if
            Rotate CurrCharVal Right
        Next
    Next
End Sub

'Image Routines
'''Display Image on the screen
Sub Image_SSD1289 ( in TFTXPos as Word, in TFTYPos as Word, in SelectedTable as word , in Color as Word)
    Dim TableReadPosition, TableLen, SelectedTable as word
    ' Start of code
    TableReadPosition = 1
    'Read selected table
    Select Case SelectedTable
        Case @TFTTable1: ReadTable TFTTable1, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TFTTable1, TableReadPosition, objHeight
        Case @TFTTable2: ReadTable TFTTable2, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TFTTable2, TableReadPosition, objHeight
        Case @TFTTable3: ReadTable TFTTable3, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TFTTable3, TableReadPosition, objHeight
        Case @TFTTable4: ReadTable TFTTable4, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TFTTable4, TableReadPosition, objHeight
        Case @TFTTable5: ReadTable TFTTable5, TableReadPosition, objwidth
             TableReadPosition++
             ReadTable TFTTable5, TableReadPosition, objHeight
    End Select
    oldTFTXPos = TFTXPos
    TableReadPosition = 3
    WholeYBytes = objHeight /  8               ; Number of whole bytes within Y Axis.  This is integer maths!
    if WholeYBytes <> 0 then
            for hCount = 0 to (WholeYBytes - 1)  ; counter to number of whole bytes
                TFTY = TFTYPos+hCount
                for widthCount = 0 to (objwidth - 1)  ; increment thru bytes horizontally
                    'Read selected table
                    Select Case SelectedTable
                    Case @TFTTable1: ReadTable TFTTable1, TableReadPosition, wByte

                    Case @TFTTable2: ReadTable TFTTable2, TableReadPosition, wByte

                    Case @TFTTable3: ReadTable TFTTable3, TableReadPosition, wByte

                    Case @TFTTable4: ReadTable TFTTable4, TableReadPosition, wByte

                    Case @TFTTable5: ReadTable TFTTable5, TableReadPosition, wByte

                    End Select
                    TableReadPosition++
                    if wByte <> 0 then             ; if the incoming byte is the same as the background, dont draw
                          For CurrCharRow = 0 to 7
                                    If wByte.0 = 1 Then
                                              PSet TFTXPos+widthCount, CurrCharRow + TFTYPos, Color
                                    End If
                                    Rotate wByte Right
                          Next
                    end if
                next widthCount
                TFTXPos =  oldTFTXPos
                TFTYPos = TFTYPos + 8
            next hCount
    end if
    maxHeight = objHeight % 8       ; calculate the remaining bits
    'writes and remaining bits, if any
    if maxHeight <> 0 then
        for widthCount = 0 to objwidth - 1
           'Read selected table
            Select Case SelectedTable
            Case @TFTTable1: ReadTable TFTTable1, TableReadPosition, wByte

            Case @TFTTable2: ReadTable TFTTable2, TableReadPosition, wByte

            Case @TFTTable3: ReadTable TFTTable3, TableReadPosition, wByte

            Case @TFTTable4: ReadTable TFTTable4, TableReadPosition, wByte

            Case @TFTTable5: ReadTable TFTTable5, TableReadPosition, wByte

            End Select
            TableReadPosition++
            For CurrCharRow = 0 to maxHeight-1
                If wByte.0 = 1 Then
                   PSet TFTXPos+widthCount, ( CurrCharRow + TFTYPos ), Color
                End If
                Rotate wByte Right
            Next
        Next
    end if
End Sub



'Character bitmaps for print routines
Table GLCDCharCol3
0
16
12
10
136
34
56
32
8
32
16
16
128
128
64
4
0
0
0
40
72
70
108
0
0
0
40
16
0
16
0
64
124
0
132
130
48
78
120
6
108
12
0
0
16
40
0
4
100
248  'A
254  'B
124  'C
254  'D
254  'E
254  'F
124  'G
254  'H
0    'I
64   'J
254  'K
254  'L
254  'M
254  'N
124  'O
254  'P
124  'Q
254  'R
76   'S
2    'T
126  'U
62   'V
126  'W
198  'X
14   'Y
194  'Z
0
4
0
8
128
0
64
254
112
112
112
16
16
254
0
64
254
0
248
248
112
248
16
248
144
16
120
56
120
136
24
136
0
0
0
32
120 '127
0
0
0
0
0
0
0     '134 �
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
248   '193 �
254   '194 B
254   '195 �
248   '196 �
254   '197 E
194   '198 Z
254   '199 H
124   '200 �
0     '201 �
254   '202 �
248   '203 �
254   '204 �
254   '205 �
130   '206 �
124   '207 �
254   '208 �
254   '209 �
0
198   '211 �
2     '212 �
14    '213 �
56    '214 �
198   '215 �
14    '216 �
188   '217 �
0
0
56    '220 �
40    '221 �
124   '222 �
0     '223 �
0
56    '225 �
254   '226 �
4     '227 �
48    '228 �
40    '229 �
1     '230 �
124   '231 �
48    '232 �
0     '233 �
124   '234 �
64    '235 �
252   '236 �
28    '237 �
0     '238 �
56    '239 �
4     '240 �
248   '241 �
24    '242 �
56    '243 �
4     '244 �
60    '245 �
56    '246 �
68    '247 �
60    '248 �
60    '249 �
0     '250 �
60    '251 �
56    '252 �
60    '253 �
60    '254 �
End Table

Table GLCDCharCol4
254
56
10
6
204
102
124
112
4
64
16
56
136
162
112
28
0
0
14
254
84
38
146
10
56
130
16
16
160
16
192
32
162
132
194
130
40
138
148
2
146
146
108
172
40
40
130
2
146
36    '�
146   'B
130   'C
130   'D
146   'E
18    'F
130   'G
16    'H
130   'I
128   'J
16    'K
128   'L
4     'M
8     'N
130   'O
18    'P
130   'Q
18    'R
146   'S
2     'T
128   'U
64    'V
128   'W
40    'X
16    'Y
162   'Z
254
8
130
4
128
2
168
144
136
136
168
252
168
16
144
128
32
130
8
16
136
40
40
16
168
124
128
64
128
80
160
200
16
0
130
16
68
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
36    '193 �
146   '194 B
2     '195 �
132   '196 �
146   '197 E
162   '198 Z
16    '199 H
146   '200 �
130   '201 �
16    '202 �
4     '203 �
4     '204 �
8     '205 �
146   '206 �
130   '207 �
2     '208 �
18    '209 �
0
170   '211 �
2     '212 �
16    '213 �
68    '214 �
40    '215 �
16    '216 �
194   '217 �
0
0
68    '220 �
84    '221 �
8     '222 �
0     '223 �
0
68    '225 �
73    '226 �
104   '227 �
73    '228 �
84    '229 �
25    '230 �
8     '231 �
74    '232 �
0     '233 �
16    '234 �
50    '235 �
64    '236 �
32    '237 �
1     '238 �
68    '239 �
124   '240 �
36    '241 �
36    '242 �
68    '243 �
4     '244 �
64    '245 �
68    '246 �
40    '247 �
64    '248 �
64    '249 �
1     '250 �
65    '251 �
68    '252 �
64    '253 �
64    '254 �
End Table

Table GLCDCharCol5
124
124
0
0
238
238
124
168
254
254
84
84
148
148
124
124
0
158
0
40
254
16
170
6
68
68
124
124
96
16
192
16
146
254
162
138
36
138
146
226
146
146
108
108
68
40
68
162
242
34   'A
146  'B
130  'C
130  'D
146  'E
18   'F
146  'G
16   'H
254  'I
130  'J
40   'K
128  'L
24   'M
16   'N
130  'O
18   'P
162  'Q
50   'R
146  'S
254  'T
128  'U
128  'V
112  'W
16   'X
224  'Y
146  'Z
130
16
130
2
128
4
168
136
136
136
168
18
168
8
250
136
80
254
240
8
136
40
40
8
168
144
128
128
96
32
160
168
108
254
108
16
66
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
34   '193 �
146  '194 B
2    '195 �
130  '196 �
146  '197 E
146  '198 Z
16   '199 H
146  '200 �
254  '201 �
40   '202 �
2    '203 �
24   '204 �
16   '205 �
146  '206 �
130  '207 �
2    '208 �
18   '209 �
0
146  '211 �
254  '212 �
224  '213 �
254  '214 �
16   '215 �
254  '216 �
2    '217 �
0
0
69   '220 �
84   '221 �
5    '222 �
61   '223 �
0
68    '225 �
73    '226 �
144   '227 �
75    '228 �
84    '229 �
165   '230 �
4     '231 �
77    '232 �
60    '233 �
40    '234 �
9     '235 �
64    '236 �
64    '237 �
149   '238 �
68    '239 �
4     '240 �
36    '241 �
164   '242 �
68    '243 �
124   '244 �
64    '245 �
254   '246 �
16    '247 �
248   '248 �
56    '249 �
60    '250 �
64    '251 �
69    '252 �
66    '253 �
58    '254 �
End Table

Table GLCDCharCol6
56
254
12
10
204
102
124
32
4
64
56
16
162
136
112
28
0
0
14
254
84
200
68
0
130
56
16
16
0
16
0
8
138
128
146
150
254
138
146
18
146
82
0
0
130
40
40
18
130
36   'A
146  'B
130  'C
68   'D
146  'E
18   'F
146  'G
16   'H
130  'I
126  'J
68   'K
128  'L
4    'M
32   'N
130  'O
18   'P
66   'Q
82   'R
146  'S
2    'T
128  'U
64   'V
128  'W
40   'X
16   'Y
138  'Z
130
32
254
4
128
8
168
136
136
144
168
2
168
8
128
122
136
128
8
8
136
40
48
8
168
128
64
64
128
80
160
152
130
0
16
32
68
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
36   '193 �
146  '194 B
2    '195 �
132  '196 �
146  '197 E
138  '198 Z
16   '199 H
146  '200 �
130  '201 �
68   '202 �
4    '203 �
4    '204 �
32   '205 �
146  '206 �
130  '207 �
2    '208 �
18   '209 �
0
130  '211 �
2    '212 �
16   '213 �
68   '214 �
40   '215 �
16   '216 �
194  '217 �
0
0
56   '220 �
85   '221 �
4    '222 �
64   '223 �
0
56    '225 �
78    '226 �
104   '227 �
77    '228 �
84    '229 �
67    '230 �
4     '231 �
73    '232 �
64    '233 �
68    '234 �
9     '235 �
32    '236 �
32    '237 �
171   '238 �
68    '239 �
124   '240 �
36    '241 �
164   '242 �
76    '243 �
4     '244 �
64    '245 �
68    '246 �
40    '247 �
64    '248 �
64    '249 �
65    '250 �
65    '251 �
68    '252 �
65    '253 �
65    '254 �
End Table

Table GLCDCharCol7
16
0
10
6
136
34
56
62
8
32
16
16
128
128
64
4
0
0
0
40
36
196
160
0
0
0
40
16
0
16
0
4
124
0
140
98
32
114
96
14
108
60
0
0
0
40
16
12
124
248   'A
108   'B
68    'C
56    'D
130   'E
2     'F
244   'G
254   'H
0     'I
2     'J
130   'K
128   'L
254   'M
254   'N
124   'O
12    'P
188   'Q
140   'R
100   'S
2     'T
126   'U
62    'V
126   'W
198   'X
14    'Y
134   'Z
0
64
0
8
128
0
240
112
64
254
48
4
120
240
0
0
0
0
240
240
112
16
248
16
64
64
248
56
120
136
120
136
0
0
0
16
120
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
248    '193 �
108    '194 B
2      '195 �
248    '196 �
130    '197 E
134    '198 Z
254    '199 H
124    '200 �
0      '201 �
130    '202 �
248    '203 �
254    '204 �
254    '205 �
130    '206 �
124    '207 �
254    '208 �
12     '209 �
0
130    '211 �
2      '212 �
14     '213 �
56     '214 �
198    '215 �
14     '216 �
188    '217 �
0
0
68     '220 �
68     '221 �
120    '222 �
32     '223 �
0
68    '225 �
48    '226 �
4     '227 �
49    '228 �
68    '229 �
1     '230 �
120   '231 �
62    '232 �
32    '233 �
0     '234 �
126   '235 �
124   '236 �
28    '237 �
65    '238 �
56    '239 �
4     '240 �
24    '241 �
72    '242 �
52    '243 �
4     '244 �
60    '245 �
56    '246 �
68    '247 �
60    '248 �
60    '249 �
32    '250 �
60    '251 �
56    '252 �
60    '253 �
60    '254 �
End Table


'**********************************************************************************

Table Symbols1
  0 ' Space
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0
  0 ' plus minus
  0
  128
  128
  128
  240
  128
  128
  128
  0
  0
  0
  0
  0
  32
  32
  32
  39
  32
  32
  32
  0
  0
  0
  192 ' null symbol
  240
  248
  120
  28
  28
  140
  76
  44
  24
  248
  246
  71
  111
  31
  24
  52
  51
  49
  56
  60
  30
  31
  15
  240 'percent symbol
  24
  8
  136
  248
  112
  128
  224
  48
  24
  8
  0
  1
  33
  49
  25
  14
  3
  29
  62
  35
  37  '33
  49
  31
  152  'mil symbol
  200
  112
  128
  224
  48
  8
  0
  0
  0
  0
  0
  32
  24
  6
  3
  28
  38
  50
  28
  0
  28
  38
  50
  64 ' EURO sign
  64
  224
  80
  72
  68
  68
  70
  70
  68
  68
  12
  0
  1
  7
  13
  17
  49
  37 '33
  37 '33
  37 '33
  37 '33
  37 '33
  16
  8   ' Yen  sign
  184
  248
  240
  192
  0
  0
  128
  208
  248
  184
  24
  4
  4
  4
  52
  63
  63
  7
  7
  4
  4
  4
  0
  0  'cent sign
  0
  0
  0
  192
  96
  16
  8
  216
  60
  24
  24
  0
  0
  0
  0
  1
  0
  14
  60
  15
  12
  4
  0
  0  ' Section sign
  0
  128
  192
  120
  120
  236
  204
  204
  204
  140
  0
  0
  0
  51
  99
  103
  102
  110
  108
  60
  59
  3
  0
  252  'floppy
  4
  252
  4
  4
  4
  4
  4
  4
  252
  4
  252
  31
  32
  37  '33
  63
  35
  35
  63
  63
  63
  37  '33
  32
  63
  end Table


Table Symbols2
  224   ' mail
  32
  96
  160
  160
  32
  32
  32
  32
  160
  160
  96
  31
  28
  20
  18
  19
  17
  18
  17
  19
  18
  20
  28
  128   ' arrow up
  192
  224
  240
  120
  252
  254
  252
  120
  240
  224
  192
  3
  1
  0
  0
  0
  255
  255
  255
  0
  0
  0
  1
  128  ' arrow down
  0
  0
  0
  0
  254
  254
  254
  0
  0
  0
  0
  3
  7
  14
  30
  60
  127
  255
  127
  60
  30
  14
  7
  224  ' arrow left
  240
  248
  252
  222
  198
  194
  192
  192
  192
  192
  192
  3
  7
  15
  31
  61
  49
  37  '33
  1
  1
  1
  1
  1
  192 ' arrow right
  192
  192
  192
  192
  194
  198
  222
  252
  248
  240
  224
  1
  1
  1
  1
  1
  37  '33
  49
  61
  31
  15
  7
  3
  128  ' check sign
  128
  0
  0
  128
  192
  96
  16
  8
  4
  2
  0
  0
  1
  15
  6
  3
  0
  0
  0
  0
  0
  0
  0
  248  'heart
  12
  4
  4
  8
  48
  8
  4
  4
  12
  248
  0
  0
  3
  4
  8
  48
  96
  16
  8
  4
  3
  0
  0
  224  ' copyright
  16
  8
  228
  20
  20
  20
  20
  20
  36
  8
  16
  7
  8
  16
  35
  36
  36
  36
  36
  36
  34
  16
  8
  224  ' Registered trademark symbol
  16
  8
  244
  148
  148
  148
  148
  100
  8
  16
  224
  7
  8
  16
  39
  32
  32
  37   '33
  34
  36
  16
  8
  7
  0  ' Backwards
  0
  128
  192
  224
  240
  248
  0
  0
  0
  0
  0
  0
  0
  1
  3
  7
  15
  31
  0
  0
  0
  0
  0
  end Table

Table Symbols3
  0  'forwards
  0
  0
  0
  248
  240
  224
  192
  128
  0
  0
  0
  0
  0
  0
  0
  31
  15
  7
  3
  1
  0
  0
  0
  0  'upwards
  128
  192
  224
  240
  240
  224
  192
  128
  0
  0
  0
  1
  1
  1
  1
  1
  1
  1
  1
  1
  1
  0
  0
  32  'downwards
  96
  224
  224
  224
  224
  224
  224
  96
  32
  0
  0
  0
  0
  0
  1
  3
  3
  1
  0
  0
  0
  0
  0
  0  'fast backwards
  128
  192
  224
  240
  0
  128
  192
  224
  240
  0
  0
  3
  7
  15
  31
  63
  3
  7
  15
  31
  63
  0
  0
  0  'fast forwards
  0
  248
  240
  224
  192
  128
  248
  240
  224
  192
  128
  0
  0
  31
  15
  7
  3
  1
  31
  15
  7
  3
  1
  240  'precedent sign
  240
  0
  128
  192
  224
  240
  0
  128
  192
  224
  240
  63
  63
  3
  7
  15
  31
  63
  3
  7
  15
  31
  63
  240  'next sign
  224
  192
  128
  0
  240
  224
  192
  128
  0
  240
  240
  63
  31
  15
  7
  3
  63
  31
  15
  7
  3
  63
  63
  120  ' tool sign
  28
  44
  118
  226
  192
  128
  224
  248
  116
  52
  32
  16
  72
  88
  60
  14
  7
  3
  7
  14
  28
  56
  112
  48  'face
  24
  8
  100
  100
  4
  100
  100
  8
  24
  48
  192
  12
  24
  18
  34
  34
  34
  34
  34
  18
  24
  12
  3
  24  'smile face
  12
  52
  50
  2
  2
  2
  50
  52
  12
  24
  224
  12
  24
  19
  36
  40
  40
  40
  36
  19
  24
  12
  3
  end Table

Table Symbols4
  224  'angry fase
  24
  12
  52
  50
  130
  130
  130
  50
  52
  12
  24
  3
  12
  24
  22
  37  '33
  32
  32
  32
  37  '33
  22
  24
  12
  0  'skull sign
  0
  0
  0
  56
  92
  188
  188
  92
  56
  0
  0
  0
  0
  38
  38
  52
  20
  9
  9
  20
  52
  38
  0
  40  'scissors sign
  56
  32
  64
  192
  192
  192
  96
  96
  32
  48
  48
  10
  14
  2
  1
  1
  1
  1
  3
  3
  2
  6
  6
  0  'note sign
  0
  0
  0
  224
  112
  48
  56
  28
  12
  254
  0
  48
  56
  56
  56
  31
  0
  6
  7
  7
  7
  3
  0
  254  'information sign
  2
  2
  146
  190
  190
  190
  154
  2
  2
  2
  254
  127
  64
  64
  64
  95
  95
  95
  95
  64
  64
  64
  127
  0  'infinity sign
  0
  128
  64
  64
  128
  128
  64
  64
  64
  128
  0
  0
  0
  3
  4
  4
  2
  3
  4
  4
  4
  3
  0
  End Table
