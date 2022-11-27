'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2020 Evan Venn

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

' Notes:
' Supports SSD1306 twin controller only with Text MODE ONLY
' 24022020  Improved inter character handling

  #IFNDEF SSD1306_TOP
    #DEFINE SSD1306_TOP 0x78
  #ENDIF
  #IFNDEF SSD1306_BOT
    #DEFINE SSD1306_BOT 0x7A
  #ENDIF

#DEFINE SSD1306_vccstate 0

#DEFINE SSD1306_SETCONTRAST 0x81
#DEFINE SSD1306_DISPLAYALLON_RESUME 0xA4
#DEFINE SSD1306_DISPLAYALLON 0xA5
#DEFINE SSD1306_NORMALDISPLAY 0xA6
#DEFINE SSD1306_INVERTDISPLAY 0xA7
#DEFINE SSD1306_DISPLAYOFF 0xAE
#DEFINE SSD1306_DISPLAYON 0xAF

#DEFINE SSD1306_SETDISPLAYOFFSET 0xD3
#DEFINE SSD1306_SETCOMPINS 0xDA

#DEFINE SSD1306_SETVCOMDETECT 0xDB

#DEFINE SSD1306_SETDISPLAYCLOCKDIV 0xD5
#DEFINE SSD1306_SETPRECHARGE 0xD9

#DEFINE SSD1306_SETMULTIPLEX 0xA8

#DEFINE SSD1306_SETLOWCOLUMN 0x00
#DEFINE SSD1306_SETHIGHCOLUMN 0x10

#DEFINE SSD1306_SETSTARTLINE 0x40

#DEFINE SSD1306_MEMORYMODE 0x20
#DEFINE SSD1306_COLUMNADDR 0x21
#DEFINE SSD1306_PAGEADDR   0x22

#DEFINE SSD1306_COMSCANINC 0xC0
#DEFINE SSD1306_COMSCANDEC 0xC8

#DEFINE SSD1306_SEGREMAP 0xA0

#DEFINE SSD1306_CHARGEPUMP 0x8D

#DEFINE SSD1306_EXTERNALVCC 0x01
#DEFINE SSD1306_SWITCHCAPVCC 0x02

' Scrolling #defines
#DEFINE SSD1306_ACTIVATE_SCROLL 0x2F
#DEFINE SSD1306_DEACTIVATE_SCROLL 0x2E
#DEFINE SSD1306_SET_VERTICAL_SCROLL_AREA 0xA3
#DEFINE SSD1306_RIGHT_HORIZONTAL_SCROLL 0x26
#DEFINE SSD1306_LEFT_HORIZONTAL_SCROLL 0x27
#DEFINE SSD1306_VERTICAL_AND_RIGHT_HORIZONTAL_SCROLL 0x29
#DEFINE SSD1306_VERTICAL_AND_LEFT_HORIZONTAL_SCROLL 0x2A

#STARTUP InitGLCD_TwinSSD1306, 95

Sub InitGLCD_TwinSSD1306
    'Colours //Set these first
    GLCDBackground = TFT_BLACK
    GLCDForeground = TFT_WHITE
    GLCDFontWidth = 5
    'used by GLCDPrintString and GLCDPrintStringLn
    GLCDfntDefaultHeight = 7
    Dim PrintLocX, PrintLocY As Word
    GLCDfntDefault = 0
    GLCDfntDefaultSize = 1
    'added to ensure the charge pump and power is operational.
    Wait 255 ms
    #IFDEF HI2C_DATA
        HI2CMode Master
        'wait for power-up and reset
        Wait 15 ms
    #ENDIF
    #IFDEF HI2C2_DATA
        HI2C2Mode Master
        'wait for power-up and reset
        Wait 15 ms
    #ENDIF

    Dim GLCD_I2C_Device_Address   As Byte

    For GLCD_I2C_Device_Address  = 0x78 To 0x7A Step 2

        'Init sequence for 128x64 OLED module
        ' 0xAE
        Write_Command_TwinSSD1306(SSD1306_DISPLAYOFF)
        Write_Command_TwinSSD1306(SSD1306_DEACTIVATE_SCROLL)
        ' 0xD5
        Write_Command_TwinSSD1306(SSD1306_SETDISPLAYCLOCKDIV)
        ' the suggested ratio 0x80
        Write_Command_TwinSSD1306(0x80)
        ' 0xA8
        Write_Command_TwinSSD1306(SSD1306_SETMULTIPLEX)
        '64 pixels
        Write_Command_TwinSSD1306(0x3f)
        ' 0xD3
        Write_Command_TwinSSD1306(SSD1306_SETDISPLAYOFFSET)
        ' no offset
        Write_Command_TwinSSD1306(0x00)
        ' line #0
        Write_Command_TwinSSD1306(SSD1306_SETSTARTLINE | 0x00)
        ' 0x8D
        Write_Command_TwinSSD1306(SSD1306_CHARGEPUMP)
        If (SSD1306_vccstate = SSD1306_EXTERNALVCC) Then
            Write_Command_TwinSSD1306(0x10)
        Else
            Write_Command_TwinSSD1306(0x14)
        End If
        ' 0x20
        Write_Command_TwinSSD1306(SSD1306_MEMORYMODE)
        'Write_Command_TwinSSD1306(0x00)                                 ' 0x00 act like ks0108 - DO NOT SELECT!!
        ' 0x01 act like PCD8544
        Write_Command_TwinSSD1306(0x10)
        Write_Command_TwinSSD1306(SSD1306_SEGREMAP | 0x1)
        Write_Command_TwinSSD1306(SSD1306_COMSCANDEC)
        ' 0xDA
        Write_Command_TwinSSD1306(SSD1306_SETCOMPINS)
        ' 64 pixels
        Write_Command_TwinSSD1306(0x12)
        ' 0x81
        Write_Command_TwinSSD1306(SSD1306_SETCONTRAST)
        If SSD1306_vccstate = SSD1306_EXTERNALVCC Then
            Write_Command_TwinSSD1306(0x9F)
        Else
            Write_Command_TwinSSD1306(0xCF)
        End If
        ' 0xd9
        Write_Command_TwinSSD1306(SSD1306_SETPRECHARGE)
        If SSD1306_vccstate = SSD1306_EXTERNALVCC Then
            Write_Command_TwinSSD1306(0x22)
        Else
            Write_Command_TwinSSD1306(0xF1)
        End If
        ' 0xDB
        Write_Command_TwinSSD1306(SSD1306_SETVCOMDETECT)
        Write_Command_TwinSSD1306(0x40)
        ' 0xA4
        Write_Command_TwinSSD1306(SSD1306_DISPLAYALLON_RESUME)
        ' 0xA6
        Write_Command_TwinSSD1306(SSD1306_NORMALDISPLAY)
        ' --turn on oled panel
        Write_Command_TwinSSD1306(SSD1306_DISPLAYON)

    Next

    'Colours
    GLCDForeground = TFT_WHITE
    'Default Colours
    #IFDEF DEFAULT_GLCDBACKGROUND
        GLCDBackground = DEFAULT_GLCDBACKGROUND
    #ENDIF

    #IFNDEF DEFAULT_GLCDBACKGROUND
        GLCDBackground = TFT_BLACK
    #ENDIF

    For GLCD_I2C_Device_Address  = 0x78 To 0x7A Step 2
        GLCDCLS_TwinSSD1306 ( GLCDBackground )
    Next
End Sub



'''@hide
Sub Write_Command_TwinSSD1306 ( In SSD1306SendByte As Byte )

    #IFDEF I2C_DATA
        I2CStart
        I2CSend GLCD_I2C_Device_Address
        I2CSend 0x00
        I2CSend SSD1306SendByte
        I2CStop
    #ENDIF
    #IFDEF HI2C_DATA
        HI2CStart
        HI2CSend GLCD_I2C_Device_Address
        HI2CSend 0x00
        HI2CSend SSD1306SendByte
        HI2CStop
    #ENDIF
    #IFDEF HI2C2_DATA
        HI2C2Start
        HI2C2Send GLCD_I2C_Device_Address
        HI2C2Send 0x00
        HI2C2Send SSD1306SendByte
        HI2C2Stop
    #ENDIF
End Sub


Sub Write_Data_TwinSSD1306 ( In SSD1306SendByte As Byte )

    #IFDEF I2C_DATA
        ICStart
        I2CSend GLCD_I2C_Device_Address
        I2CSend 0x40
        I2CSend SSD1306SendByte
        I2CStop
    #ENDIF
    #IFDEF HI2C_DATA
        HI2CStart
        HI2CSend GLCD_I2C_Device_Address
        HI2CSend 0x40
        HI2CSend SSD1306SendByte
        HI2CStop
    #ENDIF
    #IFDEF HI2C2_DATA
        HI2C2Start
        HI2C2Send GLCD_I2C_Device_Address
        HI2C2Send 0x40
        HI2C2Send SSD1306SendByte
        HI2C2Stop
    #ENDIF

End Sub





'''Clears the GLCD screen
Sub GLCDCLS_TwinSSD1306 ( Optional In  GLCDBackground As Word = GLCDBackground )

    ' initialise global variable. Required variable for Circle in all DEVICE DRIVERS- DO NOT DELETE
    GLCD_yordinate = 0

    For SSD1306_BufferLocationCalc = 0 To ( GLCD_HEIGHT / 2 ) - 1 Step 8
        Literal_Cursor_Position_TwinSSD1306 ( 0 , SSD1306_BufferLocationCalc )

        #IFDEF I2C_DATA
            I2CStart
            I2CSend GLCD_I2C_Device_Address
            I2CSend 0x40
        #ENDIF
        #IFDEF HI2C_DATA
            HI2CStart
            HI2CSend GLCD_I2C_Device_Address
            HI2CSend 0x40
        #ENDIF
        #IFDEF HI2C2_DATA
            HI2C2Start
            HI2C2Send GLCD_I2C_Device_Address
            HI2C2Send 0x40
        #ENDIF
        For GLCDTemp = 0 To 127
            #IFDEF I2C_DATA
                I2CSend GLCDBackground
            #ENDIF
            #IFDEF HI2C_DATA
                HI2CSend GLCDBackground
            #ENDIF
            #IFDEF HI2C2_DATA
                HI2C2Send GLCDBackground
            #ENDIF
        Next
        #IFDEF I2C_DATA
            I2CStop
        #ENDIF
        #IFDEF HI2C_DATA
            HI2CStop
        #ENDIF
        #IFDEF HI2C2_DATA
            HI2C2Stop
        #ENDIF

    Next
    Literal_Cursor_Position_TwinSSD1306 ( 0 , 0 )
    PrintLocX =0
    PrintLocY =0


End Sub

Sub PSet_TwinSSD1306 (In GLCDX, In GLCDY, In GLCDColour As Word)

    #IF GLCD_TYPE = GLCD_TYPE_SSD1306_TWIN

        #IFDEF GLCD_PROTECTOVERRUN
            'anything off screen with be rejected
            If GLCDX => GLCD_WIDTH OR GLCDY => GLCD_HEIGHT Then
                Exit Sub
            End If

        #ENDIF

        GLCDDataTemp = _GLCDOutByte

        'Change data to set/clear pixel
        GLCDBitNo = GLCDY AND 7
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
            _GLCDOutByte = GLCDDataTemp AND GLCDChange
        Else
            _GLCDOutByte = GLCDDataTemp OR GLCDChange
        End If

    #ENDIF

End Sub

'''Takes raw pixel positions and translates to XY char positions
'''@param X coordinate of pixel
'''@param Y coordinate of pixel
Sub Cursor_Position_TwinSSD1306( In LocX As Byte, In LocY As Byte )
    Dim  PosCharX, PosCharX As Word

    If LocY > ( GLCD_HEIGHT /2 ) -1 Then
        LocY = LocY - (GLCD_HEIGHT /2)

        GLCD_I2C_Device_Address = SSD1306_BOT
    Else
        GLCD_I2C_Device_Address = SSD1306_TOP
    End If

    ' PosCharY = LocY / 8
    ' faster than /8
    PosCharY = LocY
    Repeat 3
        Set C Off
        Rotate PosCharY Right
    End Repeat


    ' set page address
    Write_Command_TwinSSD1306( 0xB0 + PosCharY )
    ' lower nibble
    PosCharX = ( LocX  & 0x0f )
    Write_Command_TwinSSD1306( PosCharX )

    PosCharX = LocX
    Repeat 4
        Set C Off
        Rotate PosCharX Right
    End Repeat
    PosCharX = ( PosCharX & 0x0F ) + 0x10
    Write_Command_TwinSSD1306 ( PosCharX )

End Sub


Sub Literal_Cursor_Position_TwinSSD1306( In LocX As Byte, In LocY As Byte )
    Dim  PosCharX, PosCharX As Word


    ' PosCharY = LocY / 8
    ' faster than /8
    PosCharY = LocY
    Repeat 3
        Set C Off
        Rotate PosCharY Right
    End Repeat


    ' set page address
    Write_Command_TwinSSD1306( 0xB0 + PosCharY )
    ' lower nibble
    PosCharX = ( LocX  & 0x0f )
    Write_Command_TwinSSD1306( PosCharX )

    PosCharX = LocX
    Repeat 4
        Set C Off
        Rotate PosCharX Right
    End Repeat
    PosCharX = ( PosCharX & 0x0F ) + 0x10
    Write_Command_TwinSSD1306 ( PosCharX )

End Sub



'''Draws a character at the specified location on the SSD1306 GLCD
'''@param StringLocX X coordinate for message
'''@param CharLocY Y coordinate for message
'''@param Chars String to display
'''@param LineColour Line Color, either 1 or 0

Sub GLCDDrawChar_TwinSSD1306(In CharLocX As Word, In CharLocY As Word, In CharCode, Optional In LineColour As Word = GLCDForeground )


    'Set this for the TWIN
    Dim AdjustedGLCDy

    Dim CharCol, CharRow As Word
    CharCode -= 15
    CharCol=0

'****** OLED Font set handler
    #IFDEF GLCD_OLED_FONT

        'Calc pointer to the OLED fonts

        Dim LocalCharCode As Word

        'Set up the font information
        Select Case GLCDfntDefaultSize
        Case 1
            'this is two font tables of an index and data
            CharCode = CharCode - 16
            ReadTable OLEDFont1Index, CharCode, LocalCharCode
            ReadTable OLEDFont1Data, LocalCharCode , COLSperfont
            GLCDFontWidth = COLSperfont + 1
            ROWSperfont = 7
            AdjustedGLCDy = ( (ROWSperfont+1) * ( CharLocY/ (ROWSperfont+1) ) )
            'Push this char width the the ring buffer
            PushChar ( GLCDFontWidth )

        Case 2
            'this is one font table
            CharCode = CharCode - 17
            'Pointer to table of font elements
            LocalCharCode = (CharCode * 20)
            'which is really 10 as we start at 0
            COLSperfont = 9
            'which is really 16 as we start at 0
            ROWSperfont=15
            AdjustedGLCDy = ( (ROWSperfont+1) * ( CharLocY/ (ROWSperfont+1) ) )
            'Push this char width the the ring buffer
            PushChar ( 128+9 )

        End Select

        'number of columns in the font , with two row of data
        For CurrCharCol = 0 To COLSperfont
            'Increment the table pointer
            LocalCharCode++
            Select Case GLCDfntDefaultSize
            Case 1
                ReadTable OLEDFont1Data, LocalCharCode, CurrCharVal

            Case 2
                'Read this 20 times... (0..COLSperfont) [ * 2 ]
                ReadTable OLEDFont2, LocalCharCode, CurrCharVal
            End Select


            'Output the pixels
            Cursor_Position_TwinSSD1306 ( CharLocX + CurrCharCol , AdjustedGLCDy )
            If GLCDForeground = TFT_WHITE Then
                Write_Data_TwinSSD1306 ( CurrCharVal )
            Else
                CurrCharVal = !CurrCharVal
                Write_Data_TwinSSD1306 ( CurrCharVal )
            End If

            'Set to next row of date, a second row pixels
            If GLCDfntDefaultSize = 2 Then
                LocalCharCode++
                ReadTable OLEDFont2, LocalCharCode, CurrCharVal
                '8 = 1/2 of the font rows
                Cursor_Position_TwinSSD1306 ( CharLocX + CurrCharCol , AdjustedGLCDy + 8 )
                If GLCDForeground = TFT_WHITE Then
                    Write_Data_TwinSSD1306 ( CurrCharVal )
                Else
                    CurrCharVal = !CurrCharVal
                    Write_Data_TwinSSD1306 ( CurrCharVal )
                End If
            End If

            If CurrCharCol = COLSperfont and GLCDfntDefaultSize = 1 Then
                'It is the intercharacter space, put out one pixel row
                'Put out a white intercharacter pixel/space
                Cursor_Position_TwinSSD1306 ( CharLocX + CurrCharCol , AdjustedGLCDy )
                Write_Data_TwinSSD1306 ( GLCDBackground )

            End If

        Next

    #ENDIF


End Sub
