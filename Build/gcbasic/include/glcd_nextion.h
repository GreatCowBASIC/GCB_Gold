'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2018-2025 Evan Venn

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

'    https://nextion.itead.cc/resources/documents/instruction-set/#s1

'Notes:
' Supports Nextion controllers only.

'Changes
'01/03/2018 Initial Release
'24/03/2018 Revised to send 255 rather than the character
'25/03/2018 Removed typo.
'08/04/2018 Change type from Integer to Word in CIR and CIRS methods
'24/04/2018 Changed to support generic Nextion displays
'04/06/2018 Added GLCDGetTouch_Nextion method
'04/06/2018 Added software serial suppor to GLCDGetTouch_Nextion method
'04/06/2018 Revised GLCDGetTouch_Nextion method to support only Functions to improve support
'06/06/2018 Revised GLCDGetTouch_Nextion method to support consumption of last three bytes.
'07/06/2018 Revised GLCDGetTouch_Nextion method to support long not string.
'03/04/2018 Revised to support DEFAULT_GLCDBACKGROUND constant
'  27/08/19  Add GLCDfntDefaultHeight = 7  used by GLCDPrintString and GLCDPrintStringLn



#define GLCD_NextionSerialPrint    HSerPrint
#define GLCD_NextionSerialSend     HSerSend
#define GLCD_NextionSerialReceive  HSerReceive

'Recommended standard fonts to installed in the Nextion
#define NextionFont0      0, 8, 16    'Arial 8x16
#define NextionFont1      1, 12, 24   '24point 12x24 charset
#define NextionFont2      2, 16, 32   '32point 16x32 charset

#startup InitGLCD_Nextion

dim GLCDDeviceWidth, GLCDDeviceHeight as word

' If no width or height specified that issue an error
#script

  if GLCD_WIDTH=128 Then
    if GLCD_TYPE=GLCD_TYPE_Nextion then
      error "Nextion Device: You need to set GLCD_WIDTH and GLCD_GLCD_HEIGHT"
      error "Nextion Device: #define GLCD_WIDTH 480  'An example. Check specification."
      error "Nextion Device: #define GLCD_HEIGHT 320 'An example. Check specification."
    end if
  end if

#endscript


Sub InitGLCD_Nextion

  'Setup code for Nextion controllers

   dim PrintLocX, PrintLocY as word
   ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
   GLCD_yordinate = 0
   PrintLocX = 0
   PrintLocY = 0


  wait 1 s
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  SendParam_Nextion ( "rest", true )  '  reset the device
  Wait 1 s
  'Default Colours
  #ifdef DEFAULT_GLCDBACKGROUND
    GLCDBACKGROUND = DEFAULT_GLCDBACKGROUND
  #endif

  #ifndef DEFAULT_GLCDBACKGROUND
    GLCDBACKGROUND = TFT_BLACK
  #endif


  #ifndef GLCD_OLED_FONT
    GLCDFontWidth = 6
  #endif

  #ifdef GLCD_OLED_FONT
    GLCDFontWidth = 5
  #endif

  GLCDfntDefault = 0
  GLCDfntDefaultsize = 1
  GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn

End Sub



'Subs
'''Clears the GLCD screen
Sub GLCDCLS_Nextion ( Optional In  GLCDBackground as word = GLCDBackground )

  ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
  GLCD_yordinate = 0

  #if GLCD_TYPE = GLCD_TYPE_Nextion

    GLCD_NextionSerialPrint "cls "+str(GLCDBackground)
    GLCD_NextionSerialSend 255
    GLCD_NextionSerialSend 255
    GLCD_NextionSerialSend 255

  #endif

End Sub

Sub SendParam_Nextion ( in outstring as string, Optional In outstate  = False  )

  GLCD_NextionSerialPrint outstring
  if outstate = false then
      GLCD_NextionSerialSend 44
  else
      GLCD_NextionSerialSend 255
      GLCD_NextionSerialSend 255
      GLCD_NextionSerialSend 255
  end if

end sub

Sub SendParam_Nextion ( in outval as word, Optional In outstate  = False  )

  GLCD_NextionSerialPrint outval
  if outstate = false then
      GLCD_NextionSerialSend 44
  else
      GLCD_NextionSerialSend 255
      GLCD_NextionSerialSend 255
      GLCD_NextionSerialSend 255
  end if

end sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDColour State of pixel (0 = erase, 1 = display)
Sub PSet_Nextion(In GLCDX as word , In GLCDY as word, In GLCDColour As Word)

  GLCD_NextionSerialPrint "draw "
  SendParam_Nextion GLCDX
  SendParam_Nextion GLCDY
  SendParam_Nextion GLCDX
  SendParam_Nextion GLCDY
  SendParam_Nextion GLCDColour, true

End Sub


Sub GLCDDrawString_Nextion( In StringLocX as word, In CharLocY as word, In Chars as string, Optional In LineColour as word = GLCDForeground )
    dim TargetCharCol as word
    for xchar = 1 to Chars(0)
      TargetCharCol = StringLocX + ((xchar*( GLCDFontWidth * GLCDfntDefaultsize ))-( GLCDFontWidth * GLCDfntDefaultsize ))
      GLCDDrawChar TargetCharCol , CharLocY , Chars(xchar), LineColour
    next
end sub

Sub GLCDDrawChar_Nextion(In CharLocX as word, In CharLocY as word, In CharCode, Optional In LineColour as word = GLCDForeground )

  dim CharCol, CharRow as word
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


End Sub


'Not supported function
Sub GLCDRotate_Nextion( in ILI9481AddressType as byte )

end sub


Sub Circle_Nextion ( in xoffset as word, in yoffset as word, in Inxradius as word, Optional In LineColour as word = GLCDForeground )

  GLCD_NextionSerialPrint "cir "
  SendParam_Nextion xoffset
  SendParam_Nextion  yoffset
  SendParam_Nextion Inxradius
  SendParam_Nextion LineColour, True

end Sub

Sub FilledCircle_Nextion ( in xoffset as word, in yoffset as word, in Inxradius as word, Optional In LineColour as word = GLCDForeground )

  GLCD_NextionSerialPrint "cirs "
  SendParam_Nextion xoffset
  SendParam_Nextion  yoffset
  SendParam_Nextion Inxradius
  SendParam_Nextion LineColour, True

end Sub

Sub Box_Nextion(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)

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

  GLCD_NextionSerialPrint "draw "
  SendParam_Nextion LineX1
  SendParam_Nextion LineY1
  SendParam_Nextion LineX2
  SendParam_Nextion LineY2
  SendParam_Nextion LineColour, true

End Sub

Sub FilledBox_Nextion(In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour As Word = GLCDForeground)

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

  GLCD_NextionSerialPrint "fill "
  SendParam_Nextion LineX1
  SendParam_Nextion LineY1
  SendParam_Nextion LineX2 - LineX1
  SendParam_Nextion LineY2 - LineY1
  SendParam_Nextion LineColour, true

End Sub

sub Line_Nextion (In LineX1 as word, In LineY1 as word, In LineX2 as word, In LineY2 as word, Optional In LineColour as word = GLCDForeground)

  GLCD_NextionSerialPrint "line "
  SendParam_Nextion LineX1
  SendParam_Nextion LineY1
  SendParam_Nextion LineX2
  SendParam_Nextion LineY2
  SendParam_Nextion LineColour, true

end sub

Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string,  In GLCDForeground as word , in GLCDfntDefault,  in GLCDFontWidth , In  GLCDfntDefaultsize )

  Dim GLCDPrintLoc  as word
  Dim GLCDPrint_String_Counter, GLCDPrintLen as byte
  GLCDPrintLen = LCDPrintData(0)
  If GLCDPrintLen = 0 Then Exit Sub

  GLCD_NextionSerialPrint "xstr "
  SendParam_Nextion PrintLocX
  SendParam_Nextion PrintLocY

  SendParam_Nextion ( GLCDPrintLen+1)*GLCDFontWidth
  SendParam_Nextion GLCDfntDefaultsize
  SendParam_Nextion GLCDfntDefault
  SendParam_Nextion GLCDForeground
  SendParam_Nextion GLCDBackground
  SendParam_Nextion 0
  SendParam_Nextion 0
  SendParam_Nextion 1
  GLCD_NextionSerialSend 34
  GLCD_NextionSerialPrint LCDPrintData
  GLCD_NextionSerialSend 34
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255

End Sub

'stub to handle the various methods of calling - this one handles the Colour being omitted, so, the color is assumed
Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string, In GLCDfntDefault, In GLCDFontWidth, In  GLCDfntDefaultsize  )

    GLCDPrint_Nextion ( PrintLocX, PrintLocY , LCDPrintData , GLCDForeground, GLCDfntDefault,  GLCDFontWidth , GLCDfntDefaultsize )

End Sub


'stub to handle the various methods of calling - this one handles just the string
Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string  )

    GLCDPrint_Nextion ( PrintLocX, PrintLocY , LCDPrintData , GLCDForeground, GLCDfntDefault,  GLCDFontWidth , GLCDfntDefaultsize )

End Sub

'stub to handle the various methods of calling - this one handles the Colour
Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, in LCDPrintData as string , In GLCDForeground as word  )

    GLCDPrint_Nextion ( PrintLocX, PrintLocY , LCDPrintData , GLCDForeground, GLCDfntDefault,  GLCDFontWidth , GLCDfntDefaultsize )

End Sub


Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, In LCDValue As Long,  In GLCDForeground as word , in GLCDfntDefault,  in GLCDFontWidth , In  GLCDfntDefaultsize )

  Dim GLCDPrintLen as byte
  GLCDPrintLen = str(LCDValue)
  If GLCDPrintLen = 0 Then Exit Sub

  GLCD_NextionSerialPrint "xstr "
  SendParam_Nextion PrintLocX
  SendParam_Nextion PrintLocY

  SendParam_Nextion ( GLCDPrintLen+1)*GLCDFontWidth
  SendParam_Nextion GLCDfntDefaultsize
  SendParam_Nextion GLCDfntDefault
  SendParam_Nextion GLCDForeground
  SendParam_Nextion GLCDBackground
  SendParam_Nextion 0
  SendParam_Nextion 0
  SendParam_Nextion 1
  GLCD_NextionSerialSend 34
  GLCD_NextionSerialPrint str(LCDValue)
  GLCD_NextionSerialSend 34
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255

End Sub

'stub to handle the various methods of calling - this one handles the Colour being omitted, so, the color is assumed
Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, In LCDValue As Long, In GLCDfntDefault, In GLCDFontWidth, In  GLCDfntDefaultsize  )

    GLCDPrint_Nextion ( PrintLocX, PrintLocY , LCDValue , GLCDForeground, GLCDfntDefault,  GLCDFontWidth , GLCDfntDefaultsize )

End Sub


'stub to handle the various methods of calling - this one handles just the string
Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, in In LCDValue As Long  )

    GLCDPrint_Nextion ( PrintLocX, PrintLocY , LCDValue , GLCDForeground, GLCDfntDefault,  GLCDFontWidth , GLCDfntDefaultsize )

End Sub

'stub to handle the various methods of calling - this one handles the Colour
Sub GLCDPrint_Nextion (In PrintLocX as word, In PrintLocY as word, in In LCDValue As Long , In GLCDForeground as word  )

    GLCDPrint_Nextion ( PrintLocX, PrintLocY , LCDValue , GLCDForeground, GLCDfntDefault,  GLCDFontWidth , GLCDfntDefaultsize )

End Sub

Sub GLCDPrintString_Nextion ( in LCDPrintData as string )
    dim PrintLocX as word
    GLCDPrint_Nextion( PrintLocX , PrintLocY , LCDPrintData )
    PrintLocX = PrintLocX + ( GLCDFontWidth * LCDPrintData (0) )+2  '2 extra pixels
End Sub


Sub GLCDPrintStringLn_Nextion( in LCDPrintData as string )
    dim PrintLocX, PrintLocY as word

    GLCDPrint_Nextion( PrintLocX , PrintLocY , LCDPrintData )
    PrintLocX = 0
    PrintLocY = ( PrintLocY +  GLCDfntDefaultSize ) mod GLCD_HEIGHT

End Sub

Sub GLCDLocateString_Nextion( in PrintLocX as word, in PrintLocY as word )

dim PrintLocY as word
    PrintLocY = GLCDfntDefaultSize * ( PrintLocY - 1 )

End Sub

Sub GLCDPrintDefaultFont_Nextion ( in GLCDfntDefault,  in GLCDFontWidth , In  GLCDfntDefaultsize )
  'this will the set the defaults by purely calling this method
End Sub

sub GLCDUpdateObject_Nextion ( in nextionobject as string , in nextionstringData as string )

  GLCD_NextionSerialPrint nextionobject
  GLCD_NextionSerialPrint"="
  GLCD_NextionSerialSend 34
  GLCD_NextionSerialPrint nextionstringData
  GLCD_NextionSerialSend 34
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255


end sub

sub GLCDUpdateObject_Nextion ( in nextionobject as string , in nextionnumberData as long )

  GLCD_NextionSerialPrint nextionobject
  GLCD_NextionSerialPrint"="
  GLCD_NextionSerialPrint nextionnumberData
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255

end sub

sub GLCDUpdateObject_Nextion ( in nextionobject as string , in nextionnumberData as integer )

  GLCD_NextionSerialPrint nextionobject
  GLCD_NextionSerialPrint"="
  GLCD_NextionSerialPrint nextionnumberData
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255

end sub

sub GLCDSendOpInstruction_Nextion ( in nextionobject as string , in nextionstringData as string )

  GLCD_NextionSerialPrint nextionobject
  GLCD_NextionSerialPrint " "
  GLCD_NextionSerialPrint nextionstringData
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255
  GLCD_NextionSerialSend 255

end sub



'The data packet is handled as non-blocking.
'1. Checks for three bytes of 0xFF. If Four 0xff are received then exit = non-block.
'2. If at any time a 0x71 is recieved then we have data for the event.
'3. If seven bytes arrive, but the method did not receive a 0x71  then exit = non-block.
'4. The method supports software and hardware serial. As does all the other methods.  This may account for the extra few program words.
'5. The method uses a function to receive the data not a sub-routine. Again,   this would account for the extra few program words but this is required to provide full support.
'
'Method returns a string. Support USART1 and Softare serial
Function GLCDGetTouch_Nextion ( in nextionstringData  as string ) as long

          'variables used in the X and Y read operation
          dim myLongNextionInCount as byte alias GLCDGetTouch_Nextion_u       'aliased to save memory
          dim mySerialErrorCountNextion as byte alias GLCDGetTouch_Nextion_e  'aliased to save memory

          dim newByteInNextion as byte

          'Commence X or Y Axis Read
          'Iniitialise the variable to something that should not be returned
          GLCDGetTouch_Nextion = 0xDEADBEEF
          GLCDSendOpInstruction_Nextion( "get",  nextionstringData  )
          'header data

          myLongNextionInCount = 0
          mySerialErrorCountNextion = 0
          do
                newByteInNextion = GLCD_NextionSerialReceive

                'if we have recieved three 0xff then we are out of sequence
                if ( newByteInNextion = 0xff ) then
                    mySerialErrorCountNextion = mySerialErrorCountNextion + 1
                 else
                     mySerialErrorCountNextion = 0
                 end if

                'we located the correct data to sync with
                if ( newByteInNextion = 0x71 ) then
                    exit do
                end if

                myLongNextionInCount = myLongNextionInCount + 1

                'we are totally ouf of sequence if either of these test are valid
          loop until (myLongNextionInCount = 7  ) or ( mySerialErrorCountNextion = 4 )

          if ( newByteInNextion = 0x71 ) then

              'Receive the real data
              [byte]GLCDGetTouch_Nextion = GLCD_NextionSerialReceive
              GLCDGetTouch_Nextion_H = GLCD_NextionSerialReceive
              GLCDGetTouch_Nextion_U = GLCD_NextionSerialReceive
              GLCDGetTouch_Nextion_E = GLCD_NextionSerialReceive

              'footer data - consume the bytes to ensure the buffer is emptied
              repeat 3
                newByteInNextion = GLCD_NextionSerialReceive
              end Repeat

              'We have the axis data!!
              'Function name is now set to the value

          end if

end function
