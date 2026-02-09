'    Graphical LCD Image and addtional Fonts routines for the GCBASIC compiler
'    Copyright (C) 2014-2026  Evan Venn

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
' Supports KS0108 not tested for other GLCD devices

'Initialisation routine
#startup GLCDIFonts_Init

#define glcd_ImagesandFonts_addinversion 2
' Required contants and variables
    'Setup the Gpaphic Constants - Do not change - NEVER!!
           #define objFont  1
           #define objImage 2
           #define objMacro 3
           #define fntGCB   0

           #define fntOffset            4
           #define imgOffset            3


     ' Constants etc required for the serial Buffer Ring
     #define BUFFER_SIZE 32
     #define bkbhit (next_in <> next_out)


    ' Default spacing between font characters
    GLCDfntDefaultSpacing = 6

    'set your default font as the internal GCB font
    GLCDfntDefault = fntGCB

    'set default adjustment for character placement
    Dim GLCDfntWidthAdjustment as integer
    GLCDfntWidthAdjustment = 0


    ' Variables and Arrays required for GLCD Fonts and Images support  - Do not change
     #define eeprom_page_size 64
     Dim datarray( eeprom_page_size ) 'used to read the EEPROM
     Dim objPointer as word
     Dim syscounter, syssteps, EepromAddress, fntArray  as word
     Dim dotY as Integer

    ' Required Variables for the serial Buffer Ring
     Dim buffer(BUFFER_SIZE)
     Dim next_in as byte: next_in = 1
     Dim next_out as byte: next_out = 1
     Dim syncbyte as Byte


sub GLCDIFonts_Init

    ' Default spacing between font characters
    GLCDfntDefaultSpacing = 6

    'set your default font as the internal GCB font
    GLCDfntDefault = fntGCB

    'set default adjustment for character placement
    GLCDfntWidthAdjustment = 0


    ' Variables and Arrays required for GLCD Fonts and Images support  - Do not change
     Dim datarray(eeprom_page_size) 'used to read the EEPROM
     Dim objPointer as word
     Dim syscounter, syssteps, EepromAddress, fntArray  as word
     Dim dotY as Integer

    ' Required Variables for the serial Buffer Ring
     Dim buffer(BUFFER_SIZE)
     Dim next_in as byte: next_in = 1
     Dim next_out as byte: next_out = 1
     Dim syncbyte as Byte

end sub


sub GLCDImageandFonts_ManageEEPROM
    HSerPrintCRLF
    HSerPrint "GLCD Toolkit 'glcd_ImagesandFonts_addin.h' @ v"
    HSerPrint glcd_ImagesandFonts_addinversion
    HSerPrintCRLF 2

     GLCDDrawString 0, 0,  "Initialising"
     repeat 2000
       if bkbhit then
          ' REMOVE CASE
          syschar = ( bgetc OR 0x20)

          select case syschar

            case 0x61
                 ' load data
                 loadEEPROM
            case 0x66
                 ' load data
                 eraseEEPROM
            case 0x64
                 ' dump data to terminal
                 GLCDDrawString 0, 24,  "EEProm examination "
                 GLCDDrawString 0, 32,  "selected"
                 HSerPrint "Release the 'D' key to commence EEPROM dump.":HSerPrintCRLF
                 HSerPrint "Press any key to commence EEPROM examination":HSerPrintCRLF
                 HSerPrint "then, press any key to exit EEPROM examination":HSerPrintCRLF
                 HSerPrintCRLF
                 HSerPrint"> "
                 do while !bkbhit
                    ' consume the input buffer
                    wait 10 ms
                 loop

                 do while ( bgetc OR 0x20) = 0x64
                    ' consume the input buffer
                    wait 10 ms
                 loop
                 do while bkbhit
                    ' consume the input buffer
                    wait 10 ms
                    syschar = bgetc
                 loop
                 validateEEPROM
          end select
          wait 2 s
          goto GLCDImageandFonts_exit1
       end if
       wait 1 ms
     end Repeat
GLCDImageandFonts_exit1:
 GLCDCLS


end Sub

sub validateEEPROM
    ' validation EEPROOM code
              EepromAddress = 0
              HSerPrintCRLF
              HSerPrint "Hx"
              HSerPrint hex(EepromAddress_h)
              HSerPrint hex(EepromAddress)
              HSerPrint " "

              for EepromAddress = 0 to 0xffff
                  eeprom_rd_byte EEPromDevice, EepromAddress, objType

                  HSerPrint hex(objType)+" "
                  if ((EepromAddress+1) % 8 ) = 0 then
                     HSerPrintCRLF
                      HSerPrint "Hx"
                      syscounter = EepromAddress + 1
                      HSerPrint hex(syscounter_h)
                      HSerPrint hex(syscounter)
                      HSerPrint " "

                  end if
                  if bkbhit then
                     syschar = bgetc
                     select case syschar
                            case 32
                                 do while !bkbhit
                                 loop
                                 syschar = bgetc
                                 if syschar <> 32 then
                                    exit sub
                                 end if
                            case else
                                 exit sub
                     end select
                  end if
              next

end Sub

sub eraseEEPROM

    HSerPrint "Size of EEProm ?":HSerPrintCRLF
    HSerPrint "1 = 16kb":HSerPrintCRLF
    HSerPrint "2 = 32kb":HSerPrintCRLF
    HSerPrint "3 = 64kb":HSerPrintCRLF
    HSerPrint "4 = 128kb":HSerPrintCRLF
    HSerPrint "5 = 256kb":HSerPrintCRLF
    HSerPrint "6 = 512kb":HSerPrintCRLF
    HSerPrint "7 = 1024kb":HSerPrintCRLF
    HSerPrint"> "

    syschar = 255
    do while syschar = 255
        syschar = bgetc - 0x30
        if syschar > 0 and syschar < 8 then
           HSerPrintByteCRLF syschar
        else
           syschar = 255
        end if
    loop
    HSerPrintCRLF

    if syschar > 0 and syschar < 8 then
        HSerPrint "Select 'F' key again to commence EEPROM format,"
        HSerPrintCRLF
        HSerPrint "or any another key to exit"

        if bgetc = 0x66 then
            HSerPrintCRLF
            HSerPrint"Formatting.... "
            for sysptr = 1 to 64
                datarray(sysptr) = 0xff
            next

            select case syschar
                   case 1
                        syscounter = 32
                   case 2
                        syscounter = 64
                   case 3
                        syscounter = 128
                   case 4
                        syscounter = 256
                   case 5
                        syscounter = 512
                   case 6
                        syscounter = 1024
                   case 7
                        syscounter = 2028
            end select
            EepromAddress = 0
            repeat syscounter
                eeprom_wr_array( EEPromDevice, EEpromPageSize, EepromAddress , datarray, eeprom_page_size )
                EepromAddress = EepromAddress + eeprom_page_size
            end Repeat
        end if
    end if
    HSerPrintCRLF
    HSerPrint"Exiting...."
    HSerPrintCRLF

end sub

sub loadEEPROM
    ' variables for GLCD Fonts and Images support.

    ' timing is critical - adding debug to this will mean the eeManager transfer will fail!

     syscounter = 0
     syssteps = 0
     syswaiting = true
     GLCDDrawString 0,8,"Waiting for eeManager"

    xLoc = 0
    yLoc = 16

    do while syswaiting
       syschar = bgetc
       ' echo chars to GLCD to show the connectivity is OK.
       GLCDDrawChar xLoc,(yLoc+1), syschar

       xLoc = (xLoc + 6) % 126

       if xLoc = 0 then
          yLoc = (yLoc + 8) % 56
       end if
       if xLoc = 0 and yLoc = 0 then
          GLCDCLS
       end if
       if syschar = 0x41 then
          syswaiting = false
       end if
    loop

    GLCDDrawString 0,16,"Loading"
    wait 500 ms
    Hserprint  "Ready"
    HSerSend  10

    do while syschar = 0x41
       syschar = bgetc
    loop


    syssteps = syschar
    syssteps_h = bgetc
    GLCDDrawString 0,24,HEX(syssteps_h)
    GLCDDrawString 16,24,HEX(syssteps+1)+" Pages"

    do while syscounter <= syssteps
            'receives 64 bytes (one page)
            EepromAddress = bgetc
            EepromAddress_h = bgetc
            for sysptr = 1 to eeprom_page_size
                   datarray(sysptr) = bgetc
            next

            eeprom_wr_array(EEPromDevice, EEpromPageSize, EepromAddress , datarray, 64 )

            ' clean out array to check we get the data back
            for sysptr = 1 to eeprom_page_size
                   datarray(sysptr) = 0
            next

            eeprom_rd_array( EEPromDevice, EepromAddress , datarray , 64 )

            for sysptr = 1 to eeprom_page_size
              HserSend  datarray(sysptr)
            next sysptr
            syscounter++
'            GLCDDrawString 0,32,HEX(syscounter_h)
'            GLCDDrawString 16,32,HEX(syscounter)
    loop
    GLCDDrawString 0,40,"Data Transferred "

 end sub



 ' Start of Serial Support functions
 ' Required to read the serial port
 ' Assumes serial port has been initialised
 Sub readUSART

    buffer(next_in) = HSerReceive
    temppnt = next_in
    next_in = ( next_in + 1 ) % BUFFER_SIZE
    if ( next_in = next_out ) then  ' buffer is full!!
       next_in = temppnt
    end if


 End Sub

 ' Serial Support functions
 ' Get characters from the serial port
 function bgetc
   wait while !(bkbhit)
   bgetc = buffer(next_out)
   next_out=(next_out+1) % BUFFER_SIZE
 end Function



 sub GLCDImage  ( in dotX, in dotY, in objId, Optional in OptmiseGLCDDraw = 2 )
  'paints an image stored in eeprom, identified by objId
  'the upper left corner is set with dotX, dotY

  _x1 = dotX
  getObjHeader ( objId )
  hBytes = objHeight /  8
  onPageBoundary = ( dotY % 8 ) = 0
  if hBytes <> 0 then
      for hCount = 0 to (hBytes - 1)
          ' it is safe to use GLCDY and X here as PSET is not called wheen this is used as a variable
            GLCDY = dotY+hCount
            for wCount = 0 to (objwidth - 1)
                eeprom_rd_byte EEPromDevice,  EepromAddress,  wByte
                EepromAddress++

                if wByte <> OptmiseGLCDDraw then
                   ' we have to draw this byte

                   if onPageBoundary = true then

                      ' use fast draw byte
                      GLCDX = dotX+wCount
                      If GLCDX.6 = Off Then
                         Set GLCD_CS2 On
                         Set GLCD_CS1 off
                      end if
                      If GLCDX.6 = On Then
                         Set GLCD_CS1 On
                         GLCDX -= 64
                         Set GLCD_CS2 off
                      end if

                      'Select page
                      CurrPage = GLCDY / 8
                      Set GLCD_RS Off
                      GLCDWriteByte b'10111000' Or CurrPage

                      'Select column
                      GLCDWriteByte 64 Or GLCDX
                      'Write data back
                      Set GLCD_RS On
                      GLCDWriteByte wByte

                   else

                      For CurrCharRow = 0 to 7

                                If wByte.0 = 0 Then
                                          PSet dotX+wCount, CurrCharRow + dotY, GLCDBackground
                                Else
                                          PSet dotX+wCount, CurrCharRow + dotY, GLCDForeground
                                End If
                                Rotate wByte Right
                      Next
                   end if

                 end if

            next wCount
            dotX =  _x1
            dotY = dotY + 8

      next hCount

  end if

  maxHeight = objHeight % 8
  'writes the last nibble if any
  if maxHeight <> 0 then

    for wCount = 0 to objwidth - 1
      eeprom_rd_byte EEPromDevice,  EepromAddress,  wByte
      EepromAddress++
      if wByte <> OptmiseGLCDDraw  then
          For CurrCharRow = 0 to maxHeight-1
                    If wByte.0 = 0 Then
                              PSet dotX+wCount, CurrCharRow + dotY, GLCDBackground
                    Else
                              PSet dotX+wCount, CurrCharRow + dotY, GLCDForeground
                    End If
                    Rotate wByte Right
          Next
      end if

    next

  end if
  Set GLCD_CS1 Off
  Set GLCD_CS2 Off
 end sub


 '==================================================
 'This part deals with objects
 '==================================================
 sub getObjHeader ( in objId, optional in objType = 0  )

     'calculates image pointer address
     EepromAddress = (objId - 1) * 2

     'reads object Pointer
     eeprom_rd_byte EEPromDevice,  EepromAddress, objPointer
     EepromAddress++
     eeprom_rd_byte EEPromDevice,  EepromAddress, EepromAddress

     EepromAddress = ( objPointer * 256 ) + EepromAddress
     objPointer = EepromAddress

     eeprom_rd_byte EEPromDevice, EepromAddress, objType
     EepromAddress++
     eeprom_rd_byte EEPromDevice, EepromAddress, objWidth
     EepromAddress++
     eeprom_rd_byte EEPromDevice, EepromAddress, objHeight
     EepromAddress++

     if objType = objFont then

         eeprom_rd_byte EEPromDevice, EepromAddress, fntArray
         fntArray = fntArray + 32
         EepromAddress++

     end if
 end sub

 Sub FastSetByte(In GLCDX, In GLCDY, In GLCDByte, In GLCDColour As Word )

    ' This routine is only tested on the KS0108 GLCDs
    #if GLCD_TYPE = GLCD_TYPE_KS0108
    'Set pixel at X, Y on LCD to State
    'X is 0 to 127
    'Y is 0 to 63
    'Origin in top left

    'Select screen half
    If GLCDX.6 = Off Then Set GLCD_CS2 On:Set GLCD_CS1 off
    If GLCDX.6 = On Then Set GLCD_CS1 On: GLCDX -= 64: Set GLCD_CS2 off

    'Select page
    CurrPage = GLCDY / 8
    Set GLCD_RS Off
    GLCDWriteByte b'10111000' Or CurrPage

    'Select column
    GLCDWriteByte 64 Or GLCDX
    'Write data back
    Set GLCD_RS On
    GLCDWriteByte GLCDByte
    #endif

    #if GLCD_TYPE <> GLCD_TYPE_KS0108
      PSet(In GLCDX, In GLCDY, In GLCDColour As Word)
    #endif

end Sub

#define GLCDDrawChar myGLCDDrawChar
Sub myGLCDDrawChar(In CharLocX, In CharLocY, In CharCode, Optional In LineColour = GLCDForeground )


  'CharCode needs to have 16 subtracted, table starts at char 16 not char 0
  CharCode -= 15

  'invert colors if required

          if LineColour <> GLCDForeground and GLCD_TYPE <> GLCD_TYPE_ST7735 then
            'Inverted Colours
            GLCDBackground = 1
            GLCDForeground = 0
          end if

  select case GLCDfntDefault

                 case 0
                      GLCDfntDefaultSpacing = 6

                      ' Original GCB Code
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
                                For CurrCharRow = 1 to 8
                                          If CurrCharVal.0 = 0 Then
                                                    PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
                                          Else
                                                    PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDForeground
                                          End If
                                          Rotate CurrCharVal Right
                                Next
                      Next
                 case else

                 ' new font code
                   CharCode += 15
                   getObjHeader ( GLCDfntDefault , objFont )    'returns objPointer, objWidth, objHeight, fntArray
                   dotX = 0
'                   HSerPrintCRLF
'                   HSerPrint "objPointer: "
'                   HSerPrint hex(objPointer_h)
'                   HSerPrint hex(objPointer)
'                   HSerPrintCRLF
'                   HSerPrint "objWidth:   "
'                   HSerPrint hex(0)
'                   HSerPrint hex(objWidth)
'                   HSerPrintCRLF
'                   HSerPrint "objHeight:  "
'                   HSerPrint hex(0)
'                   HSerPrint hex(objHeight)
'                   HSerPrintCRLF
'                   HSerPrint "fntArray:   "
'                   HSerPrint hex(fntArray_h)
'                   HSerPrint hex(fntArray)
'                   HSerPrintCRLF
'                   HSerPrint "CharCode:   "
'                   HSerPrint hex(0)
'                   HSerPrint hex(CharCode)
'                   HSerSend 9
'                   HSerPrint chr(CharCode)

                    if objHeight < 9 then
                      ' Adjust font width spacing based on database and the adjustment
                      GLCDfntDefaultSpacing = objWidth+GLCDfntWidthAdjustment

                      if CharCode = 32 then
                         ' this space is culmative per string
                         GLCDspacewidth = ( objWidth / 2 ) + GLCDspacewidth
                         For CurrCharCol = 0 to objWidth-1
                                ' FastSetByte  dotX + CharLocX, CharLocY, !LineColour
                                For CurrCharRow = 0 to objHeight-1
                                    PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, !LineColour
                                next
                                dotX++
                         next
                      end if

                      if CharCode > 32 and CharCode <= fntArray then

                               EepromAddress = ( CharCode - 33 ) * objWidth
                               EepromAddress = objPointer + EepromAddress + fntOffset
                           For CurrCharCol = 0 to OBJWidth-1
                                  eeprom_rd_byte EEPromDevice,  EepromAddress, CurrCharVal

                                  EepromAddress++
                                  For CurrCharRow = 0 to objHeight-1

                                            If CurrCharVal.0 = 0 Then
                                                      PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDBackground
                                            Else
                                                      PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, GLCDForeground
                                            End If
                                            Rotate CurrCharVal Right

                                  Next
                           Next
                           dotX = OBJWidth + CharLocX

                            repeat GLCDfntWidthAdjustment
                                For CurrCharRow = 0 to objHeight-1
                                    PSet dotX, CharLocY + CurrCharRow, !LineColour
                                next
                            end repeat
                      end if

                   else
                       ' Adjust font width spacing based on database
                       GLCDfntDefaultSpacing = objWidth - GLCDfntWidthAdjustment
'                      HSerPrint "charcode: " + str(charcode)

                       ' Used to calculate the offset of the remaining bytes to be handled
                       Offset = 17 - objHeight

                        if CharCode = 32 then
                           GLCDspacewidth =  (objWidth / 2 ) + GLCDspacewidth
                           For CurrCharCol = 0 to GLCDfntDefaultSpacing - 1
                             For CurrCharRow = 0 to objHeight-1
                                 Pset  CharLocX+CurrCharCol, CharLocY + CurrCharRow, !LineColour
                             next
                             dotX++
                           Next
                        end if
                         if CharCode > 32 and CharCode <= fntArray then
                         EepromAddress = ( CharCode - 33 ) * ( objWidth * 2 )
                         EepromAddress = objPointer + EepromAddress + fntOffset

                         For CurrCharCol = 0 to OBJWidth-1
                                  dotY = 8
                                  eeprom_rd_byte EEPromDevice,  EepromAddress, CurrCharVal
                                  EepromAddress++

                                  For CurrCharRow = 1 to 8
                                            If CurrCharVal.0 = 0 Then
                                                      PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow + dotY - offset, GLCDBackground
                                            Else
                                                      PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow + dotY - offset, GLCDForeground
                                            End If
                                            Rotate CurrCharVal Right
                                  Next

                                  dotY = 0
                                  eeprom_rd_byte EEPromDevice,  EepromAddress, CurrCharVal
                                  EepromAddress++
                                  maxHeight = (9 - offset)
                                  'rotate to the correct bits - this is for the nibbles
                                  if maxHeight <> 8 then
                                      for  CurrCharRow = 1 to maxHeight
                                            Rotate CurrCharVal Right
                                      next

                                  end if
                                  secondoffset = (8 - maxHeight)
                                  For CurrCharRow = 1 to maxHeight
                                            If CurrCharVal.0 = 0 Then
                                                      PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow + dotY - offset+secondoffset, GLCDBackground
                                            Else
                                                      PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow + dotY - offset+secondoffset, GLCDForeground
                                            End If
                                            Rotate CurrCharVal Right
                                  Next
                         Next

                          dotX = OBJWidth + CharLocX
                          repeat GLCDfntWidthAdjustment
                              For CurrCharRow = 0 to objHeight-1
                                  PSet dotX, CharLocY + CurrCharRow, !LineColour
                              next
                          end repeat

                      end if
                   end if
          end select
          'Colours
          GLCDBackground = 0
          GLCDForeground = 1
End Sub
