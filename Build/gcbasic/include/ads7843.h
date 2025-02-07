'    ADS7843 routines for the GCBASIC compiler
'    Copyright (C) 2017-2025 Dimitris  Katsaounis

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

'   30th Aug 2105
'Notes:
' module ADS7843.h only supports GLCD_SSD1289 only

'Dim Variables
Dim prec, ADS7843_Count as Byte
Dim T_x, T_y, TP_X , TP_Y as Word


'Define Precision
#define PREC_LOW  1
#define PREC_MEDIUM 2
#define PREC_HI     3
#define PREC_EXTREME    4

#startup ADS7843_Init

Sub ADS7843_Init (Optional In precision = PREC_EXTREME)
    Dir ADS7843_DOUT In
    Dir ADS7843_IRQ  In
    Dir ADS7843_CS   Out
    Dir ADS7843_CLK  Out
    Dir ADS7843_DIN  Out
    Set ADS7843_CS On
    Set ADS7843_CLK On
    Set ADS7843_DIN On
    TP_X=0
    TP_Y=0
    T_x=0
    T_y=0
    ADS7843_SetPrecision(precision)
End Sub

Sub ADS7843_Write(ControlByte)
    ADS7843_Count=0
    Set ADS7843_CLK Off
    Repeat 8
        if ControlByte.7 = 1 then
           Set ADS7843_DIN On
        else
           Set ADS7843_DIN Off
        end if
        Rotate ControlByte left
        Set ADS7843_CLK Off
        ADS7843_Count++
        Set ADS7843_CLK On
        ADS7843_Count++
    End Repeat
End Sub

Function ADS7843_Read  as  Word
   Dim ADS7843_Data as Word
   ADS7843_Count=0
   ADS7843_Data=0
   Repeat 12
       rotate ADS7843_Data Left
       Set ADS7843_CLK On
       ADS7843_Count++
       Set ADS7843_CLK Off
       ADS7843_Count++
       if ADS7843_DOUT=1 then ADS7843_Data++
   End Repeat
   ADS7843_Read=ADS7843_Data
End Function

Sub ADS7843_GetXY (out TP_X, out TP_Y )
  Dim Sx, Sy as Long
  Dim XOffSet , YOffSet as Integer
  Sx=0
  Sy=0
  Repeat prec
     ADS7843_Count=0
     T_x=0
     T_y=0
     Set ADS7843_CS Off
     ADS7843_Write(0x90)
     Set ADS7843_CLK On
     ADS7843_Count++
     Set ADS7843_CLK Off
     ADS7843_Count++
     T_y = ADS7843_Read()
     ADS7843_Write(0xD0)
     Set ADS7843_CLK On
     ADS7843_Count++
     Set ADS7843_CLK Off
     ADS7843_Count++
     T_x = ADS7843_Read()
     Set ADS7843_CS On
     Sx +=T_x
     Sy +=T_y
  End Repeat
  TP_X=(Sx/prec)/17
  XOffSet=(120-TP_X)/9
  TP_X -=XOffSet
  if TP_X > 239 then TP_X=239
  TP_Y=(Sy/prec)/12
  if TP_Y >319 then TP_Y=319
  Select Case Orientation_SSD1289
    Case PORTRAIT
      TP_Y=319-TP_Y
      YOffSet=TP_Y/14
      TP_Y +=YOffSet
    Case PORTRAIT_REV
      TP_X=239-TP_X
      TP_Y =[word]TP_Y-(30-30*TP_Y/319)
    Case LANDSCAPE
      Swap TP_X , TP_Y
      TP_X =[word]TP_x-(30-30*TP_X/319)
    Case LANDSCAPE_REV
      Swap TP_X , TP_Y
      TP_X=319-TP_X
      TP_Y=239-TP_Y
      TP_X =[word]TP_x+(32*TP_X/319)
  End Select
End Sub



Sub ADS7843_SetPrecision(In precision as Byte)
  Select Case precision
         Case PREC_LOW: prec=1
         Case PREC_MEDIUM: prec=12
         Case PREC_HI: prec=27
         Case PREC_EXTREME: prec=102
                 Case else : prec=12
  End Select
End Sub
