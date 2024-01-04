#option Explicit
#ignorecompilation
//! check RESETCHECKONLY functionality
    // #define RESETCHECKONLY                  // only exec a RESET check, requires HW USART
    #define LT7686_RESETMAXATTEMPTS  5             // number of resets before giving up
    #define LT7686_CHECKATTEMPTS  20              // number of checks to make before giving up

//! remove or script to check no clash with SPI library
// SPI clock options
    #define SPI_CPOL_0 0
    #define SPI_CPOL_1 2
    #define SPI_CPHA_0 0
    #define SPI_CPHA_1 1


    #script
        LT7686_SPICLOCKMODE = INT( SPI_CPOL_1+SPI_CPHA_1 )
        if LT7686_SPICLOCKMODE = 0 Then
            STATE1 = ON
            STATE2 = OFF
        end if
        if LT7686_SPICLOCKMODE = 3 Then
            STATE1 = OFF
            STATE2 = ON
        end if        
    #endscript


#startup InitGLCD_LT7686, 100

Sub InitGLCD_LT7686

        #if GLCD_TYPE = LT7686_800_480_BLUE or GLCD_TYPE = LT7686_1024_600_BLUE

            Dim LT7686status as Bit 

            Dir LT7686_DI   In
            Dir LT7686_DO   Out
            Dir LT7686_SCK  Out
            Dir LT7686_CS   Out
            Dir LT7686_RST  Out

            LT7686_DO  = 0
            LT7686_DI  = 1
            LT7686_CS  = 1

            #ifdef LT7686_HARDWARESPI
                SPIMode HWSPIMODESCRIPT, LT7686_SPICLOCKMODE

            #else
                // Software SPI
                // Force SPI SCK to known state for SPI MODE 
                #if LT7686_SPICLOCKMODE = 0 Then
                    LT7686_SCK = 0 
                #else 
                    LT7686_SCK = 1  
                #endif
            #endif

            // Physical reset before any GLCD comms
            Repeat 3
                LT7686_RST = 1
                Wait 50 ms
                LT7686_RST = 0
                Wait 100 ms
                LT7686_RST = 1
                Wait 50 ms
            End Repeat

            Dim GLCDbackground, GLCDForeground as Long

            LT7686_LoadDefaultRegisters

            //! This could be a dead lock as the GLCD needs to respond
            Do While LT7686_Check_Init = False               
            Loop

            'Default Colours
            GLCDForeground = TFT_WHITE
            'Default Colours
            #ifdef DEFAULT_GLCDBACKGROUND
            GLCDbackground = DEFAULT_GLCDBACKGROUND
            #endif

            #ifndef DEFAULT_GLCDBACKGROUND
            GLCDbackground = TFT_BLACK
            #endif

            'Variables required for device
            GLCDDeviceWidth = GLCD_WIDTH
            GLCDDeviceHeight = GLCD_HEIGHT
            GLCDfntDefault = 0
            GLCDfntDefaultsize = 2
            GLCDfntDefaultHeight = 7  'used by GLCDPrintString and GLCDPrintStringLn
            GLCDRotate LANDSCAPE

            #ifndef GLCD_OLED_FONT
            GLCDFontWidth = 6
            #endif

            #ifdef GLCD_OLED_FONT
            GLCDFontWidth = 5
            #endif

            GLCDCLS

        #endif

    end sub

Sub LT7686_LoadDefaultRegisters

            // Reuse variables to reduce RAM
            Dim LT7686_pointer as Word
            Dim tableloop  as Word alias GLCDForeground_E, GLCDForeground_U
            Dim LT7686_initlength as Word alias GLCDForeground_H, GLCDForeground
            Dim LT7686_initdata as Byte alias GLCDDeviceHeight
            Dim oldLT7686_initdata as Byte alias GLCDDeviceHeight_H
            Dim lastLT7686_cmd as Byte alias GLCDDeviceWidth_H
            Dim LT7686_initverifycounter as Word

            // Get init dataset, readtable to init LC
            oldLT7686_initdata = 0
            LT7686_pointer = 0

            // table name is set in GLCD.H
            readtable LT7686_INITTFT_DATASET, LT7686_pointer, LT7686_initlength
            LT7686_pointer++

            Do Forever

                wait 1 ms    
                readtable LT7686_INITTFT_DATASET, LT7686_pointer, LT7686_initdata
            
                LT7686_pointer++

                Select Case LT7686_initdata

                    Case 0x11
                        // Wait 50ms to ensure no overrun at this initial stage of the process
                        wait 50 ms
                        readtable LT7686_INITTFT_DATASET, LT7686_pointer, LT7686_initdata
                        oldLT7686_initdata = LT7686_initdata
                        LT7686_pointer++
                        LT7686_WriteCMD( LT7686_initdata)
                        lastLT7686_cmd = LT7686_initdata
                    Case 0x12
                            /*
                            Example:
                                The command, 0x12, is to read back the content of register [01], and if the result is not [0x82], 
                                then it will keep reading the register until it reads [0x82].
                                This code is to check if PLL is ready or not. 

                            0x11, 0x01, 0x13, 0x82 // REG_WR('h01, 'h82)
                            0x11, 0x01, 0x12, 0x82 // REG_WR('h01, 'h82)  
                            */
                        
                        readtable LT7686_INITTFT_DATASET, LT7686_pointer, LT7686_initdata                 
                        
                        LT7686_pointer++            
                        //! Danger.. this could be deadlock function!! if the GLCD does not reply... then, this is a lockup
                        //! To be safe we should use a Watchdog
                        //! add Watchdog when operational
                        // Read register and get the result
                        wait 1 ms
                        do while LT7686_DataRead() <> LT7686_initdata AND LT7686_initverifycounter < 500
                            LT7686_initverifycounter++
                            wait 1 ms
                        loop
                        
                    Case 0x13
                        readtable LT7686_INITTFT_DATASET, LT7686_pointer, LT7686_initdata
                        oldLT7686_initdata = LT7686_initdata
                        LT7686_initverifycounter = 0
                        LT7686_pointer++
                        LT7686_WriteData( LT7686_initdata)
                    Case 0xAA
                        readtable LT7686_INITTFT_DATASET, LT7686_pointer, LT7686_initdata
                        LT7686_pointer++ 

                        If oldLT7686_initdata <>  LT7686_initdata Then   
                            // Only do this once
                            LT7686_Wait10ms
                            oldLT7686_initdata = LT7686_initdata
                        End if
                End Select
                If LT7686_pointer > LT7686_initlength then Exit Do

            Loop

    End Sub

Sub LT7686_Display_ColorBar ( In LT7686_Display_ColorBar_State as Byte )

        // This method destroys the state of LT7686_Display_ColorBar_State to save a byte of RAM.

        LT7686_WriteCMD( LT7686_REG_DPCR ) 
        
        If LT7686_Display_ColorBar_State = True Then
            LT7686_Display_ColorBar_State = LT7686_DataRead()
            LT7686_Display_ColorBar_State = LT7686_Display_ColorBar_State OR LT7686_SETB5
        Else
            LT7686_Display_ColorBar_State = LT7686_DataRead()
            LT7686_Display_ColorBar_State = LT7686_Display_ColorBar_State AND LT7686_CLRB5
        End If
        LT7686_WriteCMD( LT7686_REG_DPCR )
        LT7686_WriteData( LT7686_Display_ColorBar_State )
        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw       

    End Sub

Sub LT7686_Display_OnOff ( In LT7686_Display_ColorBar_State as Byte )

        // This method destroys the state of LT7686_Display_ColorBar_State to save a byte of RAM.

        LT7686_WriteCMD( LT7686_REG_DPCR ) 
        
        If LT7686_Display_ColorBar_State = True Then
            LT7686_Display_ColorBar_State = LT7686_DataRead()
            LT7686_Display_ColorBar_State.6 = 1
        Else
            LT7686_Display_ColorBar_State = LT7686_DataRead()
            LT7686_Display_ColorBar_State.6 = 0
        End If
        LT7686_WriteCMD( LT7686_REG_DPCR )
        LT7686_WriteData( LT7686_Display_ColorBar_State )      
        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw  

    End Sub

Sub LT7686_PWM_DUTY ( In LT7686_Display_PWM )


        If LT7686_Display_PWM > LT7686_PWM_PRESCALER_MAX then LT7686_Display_PWM = (LT7686_PWM_PRESCALER_MAX / 2 ) -1

        Dim temp_LT7686 as Byte

        // Stop PWM
        LT7686_WriteCMD( LT7686_REG_PCFGR )
        temp_LT7686 = LT7686_DataRead()
        temp_LT7686.4 = 0
        LT7686_WriteData ( temp_LT7686 )

        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw        

        // Select MUX input for PWM Timer 1.
        LT7686_WriteCMD( LT7686_REG_PMUXR )
        temp_LT7686 = LT7686_DataRead()
        temp_LT7686.7 = 0
        temp_LT7686.6 = 0
        LT7686_WriteData ( temp_LT7686 )

        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw        

        /* PWM Prescaler Register
        These 8 bits determine prescaler value for Timer 0 and 1.
        Time base is Core_Freq / (Prescaler + 1) */
        LT7686_WriteCMD(LT7686_REG_PSCLR)
        LT7686_WriteData( LT7686_PWM_PRESCALER_MAX )

        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw                

        /*
        Timer count buffer register
        Count buffer register total has 16 bits.
        When timer counter equal to 0 will cause PWM timer reload Count buffer register if the reload_en bit set as enable.
        It may be read back timer counter real time value when PWM timer start.
        */

        LT7686_WriteCMD(LT7686_REG_TCNTB1L)
        LT7686_WriteData( LT7686_PWM_PRESCALER_MAX /2 )
        LT7686_WriteCMD(LT7686_REG_TCNTB1H)
        LT7686_WriteData(0)

        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw

        /*
        Timer 0 compare buffer register
        Compare buffer register total has 16 bits.
        When timer counter equal or less than compare buffer register will cause PWM out
        high level if inv_on bit is off.
        */
        LT7686_WriteCMD(LT7686_REG_TCMPB1L)
        LT7686_WriteData(LT7686_Display_PWM)
        LT7686_WriteCMD(LT7686_REG_TCMPB1H)
        LT7686_WriteData(0)

        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw

        // Start PWM
        LT7686_WriteCMD( LT7686_REG_PCFGR )
        temp_LT7686 = LT7686_DataRead()
        temp_LT7686.5 = 1
        temp_LT7686.4 = 1
        LT7686_WriteData ( temp_LT7686 )

        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw

    End Sub
    
Sub GLCDCLS_LT7686 ( Optional In  GLCDbackground as Long = GLCDbackground )

        Dim LT7686_LineColour as Long

        ' initialise global variable. Required variable for Circle In all DEVICE DRIVERS- DO NOT DELETE
        GLCD_yordinate = 0

        LT7686_Check_Busy_Draw

        LT7686_WriteCMD(0x68)
        LT7686_WriteData(0x00)

        LT7686_WriteCMD(0x69)
        LT7686_WriteData(0x00)

        LT7686_WriteCMD(0x6A)
        LT7686_WriteData(0x00)

        LT7686_WriteCMD(0x6B)
        LT7686_WriteData(0x00)

        LT7686_WriteCMD(0x6C)
        LT7686_WriteData(GLCDDeviceWidth)

        LT7686_WriteCMD(0x6D)
        LT7686_WriteData(GLCDDeviceWidth_H)

        LT7686_WriteCMD(0x6E)
        LT7686_WriteData(GLCDDeviceHeight)

        LT7686_WriteCMD(0x6F)
        LT7686_WriteData(GLCDDeviceHeight_H)

        LT7686_LineColour = GLCDbackground 
        LT7686_SendColor 

        LT7686_WriteCMD( LT7686_REG_DCR1 )
        LT7686_WriteData(0xE0) 

        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw

    end sub

Sub Pset_LT7686 (In xoffset1 as word, In yoffset1 as word, _ 
    In LT7686_LineColour as Long )

    Dim LT7686_LineColour as Long
    Box( xoffset1 , yoffset1 , xoffset1 , yoffset1, LT7686_LineColour )
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Function ReadPixel_LT7686 (In xoffset1 as word, In yoffset1 as word ) As Word

    /* [Write]: Set Graphic Read/Write position
    [Read]: Current Graphic Read/Write position
    Read back is Read position or Write position depends on
    REG[5Eh] bit3, Select to read back Graphic Read/Write position.
    When DPRAM Linear mode:Graphic Read/Write Position [31:24][23:16][15:8][7:0]
    When DPRAM Active window mode:Graphic Read/Write 
    Horizontal Position [12:8][7:0], 
    Vertical Position [12:8][7:0].
    Reference Canvas image coordination. Unit: Pixel */

    LT7686_WriteCMD(0x5F)
    LT7686_WriteData(xoffset1)

    LT7686_WriteCMD(0x60)
    LT7686_WriteData(xoffset1_H)

    LT7686_WriteCMD(0x61)
    LT7686_WriteData(yoffset1)

    LT7686_WriteCMD(0x62)
    LT7686_WriteData(yoffset1_H)

    LT7686_WriteCMD( LT7686_REG_MRWDP )
    ReadPixel_LT7686 = LT7686_DataRead()  // dummy read
    Wait while ( LT7686_StatusRead() & 0x20 ) = 0x00   // check memory read FIFO is ready
    ReadPixel_LT7686 = LT7686_DataRead()
    ReadPixel_LT7686_H LT7686_DataRead()

    End Function

Sub Box_LT7686 (In xoffset1 as word, In yoffset1 as word, _
    In xoffset2 as word, In yoffset2 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateBox
    LT7686_SendColor
    LT7686_WriteCMD( LT7686_REG_DCR1 )
    LT7686_WriteData(0xA0) 
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

    // Mandates xoffset1 as word, In yoffset1 as word, In xoffset2 as word, In yoffset2 as word
Sub Filledbox_LT7686 (In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _
    Optional In LT7686_LineColour as Long = GLCDForeground )
    
    LT7686_CreateBox
    LT7686_SendColor
    LT7686_WriteCMD( LT7686_REG_DCR1 )
    LT7686_WriteData(0xE0) 
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub
    // Mandates xoffset1 as word, In yoffset1 as word, In xoffset2 as word, In yoffset2 as word

Sub FramedFilledbox_LT7686 (In _xoffset1 as word, In _yoffset1 as word, _ 
    In _xoffset2 as word, In _yoffset2 as word, _
    In _woffset as word, _ 
    In LT7686_LineColour as Long , _
    In LT7686_FillColour as Long )

	Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

    FilledBox ( _xoffset1, _yoffset1, _xoffset2, _yoffset2, cache_LT7686_LineColour )
    FilledBox ( _xoffset1 + _woffset, _yoffset1 + _woffset, _xoffset2 - _woffset, _yoffset2 - _woffset, cache_LT7686_FillColour )
    
    // Restore colors
    LT7686_LineColour = cache_LT7686_LineColour
    LT7686_FillColour = cache_LT7686_FillColour 

    End Sub

Macro LT7686_CreateBox

    LT7686_WriteCMD(0x68)
    LT7686_WriteData(xoffset1)

    LT7686_WriteCMD(0x69)
    LT7686_WriteData(xoffset1_H)

    LT7686_WriteCMD(0x6A)
    LT7686_WriteData(yoffset1)

    LT7686_WriteCMD(0x6B)
    LT7686_WriteData(yoffset1_H)

    LT7686_WriteCMD(0x6C)
    LT7686_WriteData(xoffset2)

    LT7686_WriteCMD(0x6D)
    LT7686_WriteData(xoffset2_H)

    LT7686_WriteCMD(0x6E)
    LT7686_WriteData(yoffset2)

    LT7686_WriteCMD(0x6F)
    LT7686_WriteData(yoffset2_H)

    End Macro

Sub Line_LT7686 (In xoffset1 as word, In yoffset1 as word, _
    In xoffset2 as word, In yoffset2 as word, _
    Optional In LT7686_LineColour as Long = GLCDForeground )
	
	LT7686_WriteCMD(0x68)
	LT7686_WriteData(xoffset1)

	LT7686_WriteCMD(0x69)
	LT7686_WriteData(xoffset1_H)

	LT7686_WriteCMD(0x6A)
	LT7686_WriteData(yoffset1)

	LT7686_WriteCMD(0x6B)
	LT7686_WriteData(yoffset1_H)

	LT7686_WriteCMD(0x6C)
	LT7686_WriteData(xoffset2)

	LT7686_WriteCMD(0x6D)
	LT7686_WriteData(xoffset2_H)

	LT7686_WriteCMD(0x6E)
	LT7686_WriteData(yoffset2)

	LT7686_WriteCMD(0x6F)
	LT7686_WriteData(yoffset2_H)

    LT7686_SendColor

	LT7686_WriteCMD(0x67)
	LT7686_WriteData(0x80)
    
    //Core Task is Busy, Fontwr_Busy
	LT7686_Check_Busy_Draw

    End Sub

    // Use _varaibles to ensure that the values are not destroyed.
Sub Line_Width_LT7686 ( In _xoffset1 as word, In _yoffset1 as word, _ 
    In _xoffset2 as word, In _yoffset2 as word, _ 
    In _woffset as Word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

        // Uses factorisation for integer support
        Dim xtemp, ytemp, wtemp as Integer
        Dim temp_LT7686 as Integer
        
        xtemp = ( _xoffset2 - _xoffset1 ) 
        ytemp = ( _yoffset2 - _yoffset1 ) * 10  // Y factorised by 10
        wtemp = 0

        if ( xtemp = 0 ) then 
            temp_LT7686 = 2
        else 
            // this calc is Y factorised by 10
            temp_LT7686 = ytemp / xtemp
        end if

        if  ( ABS( temp_LT7686 ) <= 10 ) Then   // factorised by 10
            do while ( _woffset > 0)
                _woffset--
                Line( _xoffset1, _yoffset1+wtemp, _xoffset2, _yoffset2+wtemp, LT7686_LineColour)
                wtemp++
            Loop	
        else 
            Do while ( _woffset > 0)
                _woffset--
                Line( _xoffset1+wtemp, _yoffset1, _xoffset2+wtemp, _yoffset2, LT7686_LineColour)
                wtemp++
            Loop	
        End if

    End Sub


Sub Circle_LT7686 ( In xoffset1 as word, In yoffset1 as word, _
    In Inxradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateCircle
    LT7686_SendColor
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0x80)//B1000_XXXX

    //Core Task is Busy, Fontwr_Busy
	LT7686_Check_Busy_Draw	

    end sub

Sub FilledCircle_LT7686 ( In xoffset1 as word, In yoffset1 as word, _
    In Inxradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateCircle
    LT7686_SendColor
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0xC0)//B1100_XXXX
	
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    end sub

Sub FramedFilledCircle_LT7686 ( In xoffset1 as word, In yoffset1 as word, _
    In _Inxradius as word, _
    In _woffset as word, _ 
    In LT7686_LineColour as Long , _
    In LT7686_FillColour as Long )

	Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

    FilledCircle ( xoffset1, yoffset1, ( _Inxradius  ), cache_LT7686_LineColour )
    FilledCircle ( xoffset1, yoffset1, _Inxradius - _woffset, cache_LT7686_FillColour )
    
    // Restore colors
    LT7686_LineColour = cache_LT7686_LineColour
    LT7686_FillColour = cache_LT7686_FillColour 

    end sub

Macro LT7686_CreateCircle
	
    LT7686_WriteCMD(0x7B)
	LT7686_WriteData(xoffset1)

	LT7686_WriteCMD(0x7C)
	LT7686_WriteData(xoffset1_H)

	LT7686_WriteCMD(0x7D)
	LT7686_WriteData(yoffset1)

	LT7686_WriteCMD(0x7E)
	LT7686_WriteData(yoffset1_H)

	LT7686_WriteCMD(0x77)
	LT7686_WriteData(Inxradius)

	LT7686_WriteCMD(0x78)
	LT7686_WriteData(Inxradius_H)

	LT7686_WriteCMD(0x79)
	LT7686_WriteData(Inxradius)

	LT7686_WriteCMD(0x7A)
	LT7686_WriteData(Inxradius_H)

    End Macro

Sub Ellipse_LT7686 ( In xoffset1 as word, In yoffset1 as word, _
    In Inxradius as word, In Inyradius as word, _
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    LT7686_WriteCMD(LT7686_REG_DCR1)
	LT7686_WriteData(0x80)//B1000_XXXX

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledEllipse_LT7686 ( In xoffset1 as word, In yoffset1 as word, _
    In Inxradius as word, In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    LT7686_WriteCMD(LT7686_REG_DCR1)
	LT7686_WriteData(0xC0)//B1100_XXXX
    
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

    // Mandated xoffset1 as word, In yoffset1 as word, In Inxradius as integer, In Inyradius as integer
Sub FramedFilledEllipse_LT7686 ( In xoffset1 as word, In yoffset1 as word, _
    In _Inxradius as word, In _Inyradius as word, _ 
    In _woffset as word, _ 
    In LT7686_LineColour as Long, _
    In LT7686_FillColour as Long )

	Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

    FilledEllipse_LT7686 ( xoffset1, yoffset1,  _Inxradius ,  _Inyradius, cache_LT7686_LineColour )
    FilledEllipse_LT7686 ( xoffset1, yoffset1, _Inxradius - _woffset, _Inyradius - _woffset, cache_LT7686_FillColour )
    
    // Restore colors
    LT7686_LineColour = cache_LT7686_LineColour
    LT7686_FillColour = cache_LT7686_FillColour 

    end sub
Macro LT7686_CreateEllipse

	LT7686_WriteCMD(0x7B)
	LT7686_WriteData(xoffset1)

	LT7686_WriteCMD(0x7C)
	LT7686_WriteData(xoffset1_H)

	LT7686_WriteCMD(0x7D)
	LT7686_WriteData(yoffset1)

	LT7686_WriteCMD(0x7E)
	LT7686_WriteData(yoffset1_H)

	LT7686_WriteCMD(0x77)
	LT7686_WriteData(Inxradius)

	LT7686_WriteCMD(0x78)
	LT7686_WriteData(Inxradius_H)

	LT7686_WriteCMD(0x79)
	LT7686_WriteData(Inyradius)

	LT7686_WriteCMD(0x7A)
	LT7686_WriteData(Inyradius_H)

    End Macro

Sub RoundRect_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _
    In Inxradius as word, In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateRoundRect
    LT7686_SendColor
    LT7686_WriteCMD(LT7686_REG_DCR1)
	LT7686_WriteData(0xB0)//B1011_XXXX
    
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw
    
    End Sub

Sub FilledRoundRect_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _ 
    In Inxradius as word, In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateRoundRect
    LT7686_SendColor
    LT7686_WriteCMD(LT7686_REG_DCR1)
	LT7686_WriteData(0xF0)//B1111_XXXX

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FramedFilledRoundRect_LT7686 ( In _xoffset1 as word, In _yoffset1 as word, _ 
    In _xoffset2 as word, In _yoffset2 as word, _ 
    In Inxradius as word, In Inyradius as word, _
    In _woffset as word, _ 
    In LT7686_LineColour as Long, _
    In LT7686_FillColour as Long )

    Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

    FilledRoundRect ( _xoffset1, _yoffset1, _xoffset2, _yoffset2, Inxradius,  Inyradius , cache_LT7686_LineColour )
    FilledRoundRect ( _xoffset1 + _woffset, _yoffset1 + _woffset, _xoffset2 - _woffset, _yoffset2 - _woffset, Inxradius,  Inyradius , cache_LT7686_FillColour )
    
    // Restore colors
    LT7686_LineColour = cache_LT7686_LineColour
    LT7686_FillColour = cache_LT7686_FillColour 

    End Sub

Macro LT7686_CreateRoundRect

    LT7686_CreateBox

	LT7686_WriteCMD(0x77)
	LT7686_WriteData(Inxradius)

	LT7686_WriteCMD(0x78)
	LT7686_WriteData(Inxradius_H)

	LT7686_WriteCMD(0x79)
	LT7686_WriteData(Inyradius)

	LT7686_WriteCMD(0x7A)
	LT7686_WriteData(Inyradius_H)    

    End Macro

Sub Triangle_LT7686  (In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _ 
    In xoffset3 as word, In yoffset3 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )
    
    LT7686_CreateTriangle
    LT7686_SendColor
    LT7686_WriteCMD(0x67)
	LT7686_WriteData(0x82)//B1000_0010

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledTriangle_LT7686  (In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _ 
    In xoffset3 as word, In yoffset3 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )
	
    LT7686_CreateTriangle
    LT7686_SendColor
    LT7686_WriteCMD(0x67)
	LT7686_WriteData(0xA2)//B1010_0010

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FramedFilledTriangle_LT7686  (In _xoffset1 as word, In _yoffset1 as word, _ 
    In _xoffset2 as word, In _yoffset2 as word, _ 
    In _xoffset3 as word, In _yoffset3 as word, _ 
    In LT7686_LineColour as Long, _
    In LT7686_FillColour as Long )

    Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

    FilledTriangle ( _xoffset1, _yoffset1, _xoffset2, _yoffset2, _xoffset3, _yoffset3, cache_LT7686_FillColour )
    Triangle ( _xoffset1, _yoffset1, _xoffset2, _yoffset2, _xoffset3, _yoffset3, cache_LT7686_LineColour )
    
    // Restore colors
    LT7686_LineColour = cache_LT7686_LineColour
    LT7686_FillColour = cache_LT7686_FillColour 


    End Sub

Macro LT7686_CreateTriangle

    LT7686_CreateBox

	LT7686_WriteCMD(0x70)
	LT7686_WriteData(xoffset3)

	LT7686_WriteCMD(0x71)
	LT7686_WriteData(xoffset3_H)

	LT7686_WriteCMD(0x72)
	LT7686_WriteData(yoffset3)

	LT7686_WriteCMD(0x73)
	LT7686_WriteData(yoffset3_H)    

    End Macro

Sub Cylinder_LT7686 ( In _xoffset1 as word, In _yoffset1 as word, _ 
    In _Inxradius as word, In _Inyradius as word, _ 
    In _zoffset as word, _ 
    Optional In LT7686_LineColour as Long = GLCDBackground, _ 
    Optional In LT7686_FillColour as Long = GLCDForeground )

    if ( _yoffset1 < _zoffset ) then Exit Sub

    Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

    FilledEllipse ( _xoffset1, _yoffset1, _Inxradius, _Inyradius, cache_LT7686_FillColour )
    Ellipse (       _xoffset1, _yoffset1, _Inxradius, _Inyradius, cache_LT7686_LineColour )
    FilledBox ( _xoffset1 - inxradius, _yoffset1 - _zoffset, _xoffset1 + _inxradius, _yoffset1, cache_LT7686_FillColour )
    FilledEllipse ( _xoffset1, _yoffset1 - _zoffset, _Inxradius, _Inyradius, cache_LT7686_FillColour )
    Ellipse ( _xoffset1, _yoffset1 - _zoffset, _Inxradius, _Inyradius, cache_LT7686_LineColour )
    Line ( _xoffset1 - _Inxradius, _yoffset1, _xoffset1 - _Inxradius, _yoffset1 - _zoffset, cache_LT7686_LineColour )
    Line ( _xoffset1 + _Inxradius, _yoffset1, _xoffset1 + _Inxradius, _yoffset1 - _zoffset, cache_LT7686_LineColour )
    
    // Restore colors
    LT7686_LineColour = cache_LT7686_LineColour
    LT7686_FillColour = cache_LT7686_FillColour 

    End Sub

    // Mandated xoffset1 as word, In yoffset1 as word, In Inxradius as integer

Sub LeftUpCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0x91)//B1001_XX01

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub LeftDownCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0x90)//B1001_XX00

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub RightUpCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0x92)//B1001_XX10

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub RightDownCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0x93)//B1001_XX11

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledLeftUpCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0xD1)//B1101_XX01

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledLeftDownCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0xD0)//B1101_XX00

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledRightUpCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _ 
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0xD2)//B1101_XX10

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledRightDownCurve_LT7686 ( In xoffset1 as word, In yoffset1 as word, _
    In Inxradius as word,In Inyradius as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateEllipse
    LT7686_SendColor
    
	LT7686_WriteCMD(0x76)
	LT7686_WriteData(0xD3)//B1101_XX11

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub Quadrilateral_LT7686  (In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _ 
    In xoffset3 as word, In yoffset3 as word, _ 
    In xoffset4 as word, In yoffset4 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateQuadrilateral
    LT7686_SendColor
	LT7686_WriteCMD(0x67)
	LT7686_WriteData(0x8D)

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledQuadrilateral_LT7686  (In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _ 
    In xoffset3 as word, In yoffset3 as word, _ 
    In xoffset4 as word, In yoffset4 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreateQuadrilateral
    LT7686_SendColor
	LT7686_WriteCMD(0x67)
	LT7686_WriteData(0xA6)

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Macro LT7686_CreateQuadrilateral

    LT7686_WriteCMD(0x68)
	LT7686_WriteData(xoffset1)

	LT7686_WriteCMD(0x69)
	LT7686_WriteData(xoffset1_H)

	LT7686_WriteCMD(0x6A)
	LT7686_WriteData(yoffset1)

	LT7686_WriteCMD(0x6B)
	LT7686_WriteData(yoffset1_H)


    LT7686_WriteCMD(0x6C)
	LT7686_WriteData(xoffset2)

	LT7686_WriteCMD(0x6D)
	LT7686_WriteData(xoffset2_H)

	LT7686_WriteCMD(0x6E)
	LT7686_WriteData(yoffset2)

	LT7686_WriteCMD(0x6F)
	LT7686_WriteData(yoffset2_H)


    LT7686_WriteCMD(0x70)
	LT7686_WriteData(xoffset3)

	LT7686_WriteCMD(0x71)
	LT7686_WriteData(xoffset3_H)

	LT7686_WriteCMD(0x72)
	LT7686_WriteData(yoffset3)

	LT7686_WriteCMD(0x73)
	LT7686_WriteData(yoffset3_H)


	LT7686_WriteCMD(0x77)
	LT7686_WriteData(xoffset4)

	LT7686_WriteCMD(0x78)
	LT7686_WriteData(xoffset4_H)

	LT7686_WriteCMD(0x79)
	LT7686_WriteData(yoffset4)

	LT7686_WriteCMD(0x7A)
	LT7686_WriteData(yoffset4_H)

    End Macro

Sub Pentagon_LT7686(In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _ 
    In xoffset3 as word, In yoffset3 as word, _ 
    In xoffset4 as word, In yoffset4 as word, _ 
    In xoffset5 as word, In yoffset5 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreatePentagon
    LT7686_SendColor
	LT7686_WriteCMD(0x67)
	LT7686_WriteData(0x8F)

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    End Sub

Sub FilledPentagon_LT7686(In xoffset1 as word, In yoffset1 as word, _ 
    In xoffset2 as word, In yoffset2 as word, _ 
    In xoffset3 as word, In yoffset3 as word, _ 
    In xoffset4 as word, In yoffset4 as word, _ 
    In xoffset5 as word, In yoffset5 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    LT7686_CreatePentagon
    LT7686_SendColor
	LT7686_WriteCMD(0x67)
	LT7686_WriteData(0xa9)

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw


    End Sub

Macro LT7686_CreatePentagon

    LT7686_WriteCMD(0x68)
	LT7686_WriteData(xoffset1)

	LT7686_WriteCMD(0x69)
	LT7686_WriteData(xoffset1_H)

	LT7686_WriteCMD(0x6A)
	LT7686_WriteData(yoffset1)

	LT7686_WriteCMD(0x6B)
	LT7686_WriteData(yoffset1_H)


    LT7686_WriteCMD(0x6C)
	LT7686_WriteData(xoffset2)

	LT7686_WriteCMD(0x6D)
	LT7686_WriteData(xoffset2_H)

	LT7686_WriteCMD(0x6E)
	LT7686_WriteData(yoffset2)

	LT7686_WriteCMD(0x6F)
	LT7686_WriteData(yoffset2_H)


    LT7686_WriteCMD(0x70)
	LT7686_WriteData(xoffset3)

	LT7686_WriteCMD(0x71)
	LT7686_WriteData(xoffset3_H)

	LT7686_WriteCMD(0x72)
	LT7686_WriteData(yoffset3)

	LT7686_WriteCMD(0x73)
	LT7686_WriteData(yoffset3_H)


	LT7686_WriteCMD(0x77)
	LT7686_WriteData(xoffset4)

	LT7686_WriteCMD(0x78)
	LT7686_WriteData(xoffset4_H)

	LT7686_WriteCMD(0x79)
	LT7686_WriteData(yoffset4)

	LT7686_WriteCMD(0x7A)
	LT7686_WriteData(yoffset4_H)

	LT7686_WriteCMD(0x7B)
	LT7686_WriteData(xoffset5)

	LT7686_WriteCMD(0x7C)
	LT7686_WriteData(xoffset5_H)

	LT7686_WriteCMD(0x7D)
	LT7686_WriteData(yoffset5)

	LT7686_WriteCMD(0x7E)
	LT7686_WriteData(yoffset5_H)

    End Macro

Sub FilledCube_LT7686 ( _ 
    In _xoffset1 as word, In _yoffset1 as word, _ 
    In _xoffset2 as word, In _yoffset2 as word, _ 
    In _xoffset3 as word, In _yoffset3 as word, _ 
    In _xoffset4 as word, In _yoffset4 as word, _ 
    In _xoffset5 as word, In _yoffset5 as word, _ 
    In _xoffset6 as word, In _yoffset6 as word, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground, _ 
    Optional In LT7686_FillColour as Long = GLCDForeground )

    Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

    FilledBox(_xoffset1,_yoffset1,_xoffset5,_yoffset5, cache_LT7686_FillColour )
	Box(_xoffset1,_yoffset1,_xoffset5,_yoffset5, cache_LT7686_LineColour )

	FilledQuadrilateral (_xoffset1,_yoffset1, _xoffset2, _yoffset2, _xoffset3, _yoffset3 , _xoffset4, _yoffset4, cache_LT7686_FillColour)
	Quadrilateral(_xoffset1,_yoffset1, _xoffset2, _yoffset2, _xoffset3, _yoffset3 , _xoffset4, _yoffset4, cache_LT7686_LineColour)


    FilledQuadrilateral (_xoffset3,_yoffset3, _xoffset4, _yoffset4, _xoffset5, _yoffset5 , _xoffset6, _yoffset6, cache_LT7686_FillColour)
	Quadrilateral(_xoffset3,_yoffset3, _xoffset4, _yoffset4, _xoffset5, _yoffset5 , _xoffset6, _yoffset6, cache_LT7686_LineColour)

    // Restore colors
    LT7686_LineColour = cache_LT7686_LineColour
    LT7686_FillColour = cache_LT7686_FillColour 

    End Sub
    
Sub MakeTable_LT7686 ( _
    In __xoffset1 as word, In __yoffset1 as word, _ 
    In __woffset as word, In __hoffset as word, _ 
    In __tablecolumns as word, In __tablerows as word, _ 
    In __width1 as word, In __width2 as word, _
    In __mode as byte, _ 
    In LT7686_TableColour as Long, _ 
    In LT7686_ItemColour as Long, _ 
    Optional In LT7686_FillColour as Long = GLCDForeground )

    Dim cache_LT7686_LineColour, cache_LT7686_FillColour as Long
    cache_LT7686_LineColour = LT7686_LineColour
    cache_LT7686_FillColour = LT7686_FillColour

            dim __temp_LT7686 as word
            __temp_LT7686 = 0
            dim __x2,__y2  as Integer
            __x2 = __xoffset1 + ( __woffset * __tablecolumns )
            __y2 = __yoffset1 + ( __hoffset * __tablerows )
            
            // Create the outline of the table  
            FramedFilledbox_LT7686( __xoffset1, __yoffset1, _
            __x2, __y2, __width2, LT7686_TableColour,cache_LT7686_FillColour)  
            
            if ( __mode.0 = 1 ) Then  
                Filledbox_LT7686( __xoffset1, __yoffset1,__xoffset1+__woffset,__y2, LT7686_ItemColour) 
            end if
            if ( __mode.1 = 1 ) Then
                Filledbox_LT7686( __xoffset1, __yoffset1,__x2,__yoffset1+__hoffset, LT7686_ItemColour)
            end if
            
            // draw the horizontal lines
            for __temp_LT7686 = 0 to __tablerows - 1
                // LT768_DrawLine__width(X1,Y1+i*H,x2,Y1+i*H,TableColor,width1)
                LineWidth( __xoffset1, __yoffset1+(__temp_LT7686*__hoffset), __x2, __yoffset1+(__temp_LT7686*__hoffset), __width1, LT7686_TableColour)
            next
            
            // draw the vertical lines
            for __temp_LT7686 = 0 to __tablecolumns - 1
                // LT768_DrawLine__width(X1+i*W,Y1,X1+i*W,y2,TableColor,width1)
                LineWidth(__xoffset1+(__temp_LT7686*__woffset),__yoffset1,__xoffset1+(__temp_LT7686*__woffset),__y2,__width1,LT7686_TableColour)
            next

    End Sub

    /* Function:    LT768_Select_Internal_Font_Init                                          */
    /*                                                                                       */
    /* Parameters:                                                                           */
    /*                   Size: Font size  16: 16*16   24: 24*24    32: 32*32                 */
    /*                    XxN: Width magnification: 1~4                                      */
    /*                    YxN: Height magnification: 1~4                                     */
    /*              ChromaKey: 0: Background color transparent; 1: Background color          */
    /*              Alignment: 0: Non-alignment; 1: Alignment                                */
    /* Returns:     None                                                                     */
    /* Description: Initialize the internal font 
    */
Sub LT7686_Select_Internal_Font_Init ( Optional In _fontsize = 16, _ 
    Optional In _xwidthmag = 1, _ 
    Optional In _yheightmag = 1 , _ 
    Optional In _chromakey = 1 , _ 
    Optional In _alignment = 1 )
	
    Dim temp_LT7686 as Byte

    LT7686_WriteCMD(LT7686_REG_CCR0)
    temp_LT7686 = 0
    Select Case _fontsize 
        // Case 16
        //     temp_LT7686.5 = 0
        //     temp_LT7686.4 = 0
        Case 24
            // temp_LT7686.5 = 0
            temp_LT7686.4 = 1
        Case 32
            temp_LT7686.5 = 1
            // temp_LT7686.4 = 0
        // Case Else
        //     temp_LT7686.5 = 0
        //     temp_LT7686.4 = 0
    End Select
    LT7686_WriteData(temp_LT7686)
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    LT7686_WriteCMD(LT7686_REG_CCR1)
    temp_LT7686 = 0
    Select Case _xwidthmag
        // Case 1
        //     temp_LT7686.3 = 0
        //     temp_LT7686.2 = 0
        Case 2
            // temp_LT7686.3 = 0
            temp_LT7686.2 = 1
        Case 3
            temp_LT7686.3 = 1
            // temp_LT7686.2 = 0
        Case 4
            temp_LT7686.3 = 1
            temp_LT7686.2 = 1
        // Case Else
        //     temp_LT7686.3 = 0
        //     temp_LT7686.2 = 0
    End Select

    Select Case _yheightmag
        // Case 1
        //     temp_LT7686.1 = 0
        //     temp_LT7686.0 = 0
        Case 2
            // temp_LT7686.1 = 0
            temp_LT7686.0 = 1
        Case 3
            temp_LT7686.1 = 1
            // temp_LT7686.0 = 0
        Case 4
            temp_LT7686.1 = 1
            temp_LT7686.0 = 1
        // Case Else
        //     temp_LT7686.1 = 0
        //     temp_LT7686.0 = 0
    End Select

    Select Case _chromakey
        Case 1
            temp_LT7686.6 = 1
        // Case 2
            // temp_LT7686.6 = 1
        // Case Else
        //     temp_LT7686.6 = 0
    End Select

    Select Case _alignment
        Case 1
            temp_LT7686.7 = 1
        // Case 2
        //     temp_LT7686.7 = 1
        // Case Else
        //     temp_LT7686.7 = 0
    End Select
    LT7686_WriteData(temp_LT7686)

    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw
    End Sub

Sub LT7686_Print_Internal_Font ( _ 
    In _xoffset1 as word, _ 
    In _yoffset1 as word, _ 
    In LCDPrintData as string, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    Dim temp_LT7686
    Dim GLCDPrintLen, GLCDPrint_String_Counter as word
    GLCDPrintLen = LCDPrintData(0)
    If GLCDPrintLen = 0 Then Exit Sub

    // Send xx yy
    LT7686_WriteCMD(LT7686_REG_F_CURX0)
    LT7686_WriteData(_xoffset1)
	LT7686_WriteCMD(LT7686_REG_F_CURX1)
    LT7686_WriteData(_xoffset1_H)
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw
	
	LT7686_WriteCMD(LT7686_REG_F_CURY0)
    LT7686_WriteData(_yoffset1)
	LT7686_WriteCMD(LT7686_REG_F_CURY1)
    LT7686_WriteData(_yoffset1_H)
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw


    // Select CGROM_Select_Internal_CGROM
	LT7686_WriteCMD(LT7686_REG_CCR0)
	temp_LT7686 = LT7686_DataRead()
    temp_LT7686.7 = 0
    temp_LT7686.6 = 0
	LT7686_WriteData(temp_LT7686)
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    LT7686_SendColor

    // Select Text mode
	LT7686_WriteCMD(0x03)
	temp_LT7686 = LT7686_DataRead()
    temp_LT7686.2 = 1
    temp_LT7686.1 = 0
    temp_LT7686.0 = 0
    
	LT7686_WriteData(temp_LT7686)
    //Core Task is Busy, Fontwr_Busy
    LT7686_Check_Busy_Draw

    'Write string - char by char
    For GLCDPrint_String_Counter = 1 To GLCDPrintLen
        LT7686_WriteCMD(LT7686_REG_MRWDP)
        LT7686_WriteData( LCDPrintData(GLCDPrint_String_Counter) )     
        //Core Task is Busy, Fontwr_Busy
        LT7686_Check_Busy_Draw
    Next

    // set graphics mode
    LT7686_WriteCMD(0x03)
	temp_LT7686 = LT7686_DataRead()
    temp_LT7686.2 = 0
	LT7686_WriteData(temp_LT7686)

    End Sub

Sub LT7686_Print_Internal_Font ( _ 
    In _xoffset1 as word, _ 
    In _yoffset1 as word, _ 
    In LCDValue as Long, _ 
    Optional In LT7686_LineColour as Long = GLCDForeground )

    'Display
    LT7686_Print_Internal_Font ( _xoffset1, _yoffset1, Str(LCDValue) , LT7686_LineColour )

End Sub



Sub Text_cursor_Init( Optional In _xoffset1 as byte = 0, _ 
        Optional In _yoffset1 as byte = 0, _ 
        Optional In _On_Off_Blinking as byte = 1, _ 
        Optional In In _Blinking_Time as byte = 127 )

        if _On_Off_Blinking = 0 Then Disable_Text_Cursor_Blinking
        if _On_Off_Blinking = 1 Then Enable_Text_Cursor_Blinking

        Blinking_Time_Frames( _Blinking_Time ) 
        
        Text_Cursor_H_V( _xoffset1, _yoffset1 )
        
        Enable_Text_Cursor()

    End Sub

Sub Enable_Text_Cursor

        dim _temp_LT7686 as Byte

        LT7686_WriteCMD(LT7686_REG_GTCCR)
        _temp_LT7686 = LT7686_DataRead()
        _temp_LT7686.1 = 1
        LT7686_WriteData(temp_LT7686)

    End Sub

Sub Disable_Text_Cursor
    
        LT7686_WriteCMD(LT7686_REG_GTCCR)
        _temp_LT7686 = LT7686_DataRead()
        _temp_LT7686.1 = 0
        LT7686_WriteData(temp_LT7686)

    End Sub

Sub Enable_Text_Cursor_Blinking

        LT7686_WriteCMD(LT7686_REG_GTCCR)
        _temp_LT7686 = LT7686_DataRead()
        _temp_LT7686.0 = 1
        LT7686_WriteData(temp_LT7686)

    End Sub

Sub Disable_Text_Cursor_Blinking

        LT7686_WriteCMD(LT7686_REG_GTCCR)
        _temp_LT7686 = LT7686_DataRead()
        _temp_LT7686.0 = 0
        LT7686_WriteData(_temp_LT7686)

    End Sub

Sub Blinking_Time_Frames ( in _temp_LT7686)

        /*
        Text Cursor Blink Time Setting (Unit: Frame)
        00h : 1 frame time.
        01h : 2 frames time.
        02h : 3 frames time.
        :
        FFh : 256 frames time.
        */

        LT7686_WriteCMD(LT7686_REG_BTCR)
        LT7686_WriteData(_temp_LT7686)


    End Sub

Sub Text_Cursor_H_V ( In _xoffset1 as byte, _ 
    In _yoffset1 as byte  )

        /*
        [3Eh]
        Text Cursor Horizontal Size Setting[4:0]
        Unit : Pixel
        Zero-based number. Value  means 1 pixel.
        Note : When font is enlarged, the cursor setting will multiply the
        same times as the font enlargement.
        [3Fh]
        Text Cursor Vertical Size Setting[4:0]
        Unit : Pixel
        Zero-based number. Value  means 1 pixel.
        Note : When font is enlarged, the cursor setting will multiply the
        same times as the font enlargement.
        */

        LT7686_WriteCMD(LT7686_REG_BTCR)
        LT7686_WriteData(_xoffset1)
        LT7686_WriteCMD(LT7686_REG_BTCR)
        LT7686_WriteData(_yoffset1)
    
    End Sub


//*
Sub Set_Graphic_cursor_Pos
    // LT768_Graphic_cursor_Init
    End Sub

Sub Enable_Graphic_Cursor
    // LT768_Set_Graphic_cursor_Pos
    End Sub
    
Sub Disable_Graphic_Cursor
    // LT768_Disable_Graphic_Cursor
    End Sub 

//*
Macro LT7686_SendColor

        dim __Set_LT7686_LineColour as Long
        __Set_LT7686_LineColour = LT7686_LineColour
        
        //Supports RGB format only 

        // red component
        LT7686_WriteCMD(LT7686_REG_FGCR)
        LT7686_WriteData( __Set_LT7686_LineColour_H  )

        Set C off
        Rotate __Set_LT7686_LineColour Left
        Rotate __Set_LT7686_LineColour Left
        Rotate __Set_LT7686_LineColour Left
            
        // blue component
        LT7686_WriteCMD(LT7686_REG_FGCB)
        LT7686_WriteData( __Set_LT7686_LineColour )

        Set C off
        Rotate __Set_LT7686_LineColour Left
        Rotate __Set_LT7686_LineColour Left

        // green component
        LT7686_WriteCMD(LT7686_REG_FGCG)
        LT7686_WriteData( __Set_LT7686_LineColour_H )



    End Macro

Sub  LT7686_SetBackgroundColor (  )

        dim __Set_LT7686_BackgroundColour as Long
        __Set_LT7686_BackgroundColour = LT7686_BackgroundColour

        // blue component
        LT7686_WriteCMD(LT7686_REG_BGCB)
        LT7686_WriteData( __Set_LT7686_BackgroundColour & 0x1F )

        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        
        // green component
        LT7686_WriteCMD(LT7686_REG_BGCG)
        LT7686_WriteData( __Set_LT7686_BackgroundColour &  0x3F )

        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right

        // red component
        LT7686_WriteCMD(LT7686_REG_BGCR)
        LT7686_WriteData( __Set_LT7686_BackgroundColour  )

    End Sub

    // Overloaded
Sub  LT7686_SetBackgroundColor ( in __Set_LT7686_BackgroundColour as Long  )

        dim __Set_LT7686_BackgroundColour as Long
        GLCDBackground = __Set_LT7686_BackgroundColour

        // blue component
        LT7686_WriteCMD(LT7686_REG_BGCB)
        LT7686_WriteData( __Set_LT7686_BackgroundColour & 0x1F )

        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        
        // green component
        LT7686_WriteCMD(LT7686_REG_BGCG)
        LT7686_WriteData( __Set_LT7686_BackgroundColour &  0x3F )

        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right
        Rotate __Set_LT7686_BackgroundColour Right

        // red component
        LT7686_WriteCMD(LT7686_REG_BGCR)
        LT7686_WriteData( __Set_LT7686_BackgroundColour  )

    End Sub

Sub GLCDRotate_LT7686 ( in GLCDRotateState )

    /*
        The internal text does NOT support rotate 180 degree. (Also, no existed API supports such function)
        It only supports 90 degree counterclockwise rotation. Refer to the LT768x datasheet, Section 13.3 for more information.
        Please also note that internal text position will be affected by the direction setting of VDIR (REG[12h] bit3).
    */

        dim _temp_LT7686, __temp_LT7686 as byte

        LT7686_WriteCMD(LT7686_REG_MACR)
        __temp_LT7686 = LT7686_DataRead()

        LT7686_WriteCMD(LT7686_REG_DPCR)
        _temp_LT7686 = LT7686_DataRead()

        select case GLCDRotateState
            case PORTRAIT
                    GLCDDeviceWidth = GLCD_WIDTH
                    GLCDDeviceHeight = GLCD_HEIGHT

            case LANDSCAPE
                    GLCDDeviceWidth = GLCD_WIDTH
                    GLCDDeviceHeight = GLCD_HEIGHT
                    
                    _temp_LT7686.3 = 0

                    __temp_LT7686.2 = 0
                    __temp_LT7686.1 = 0                   

            case PORTRAIT_REV
                    GLCDDeviceWidth = GLCD_WIDTH
                    GLCDDeviceHeight = GLCD_HEIGHT
                    
            case LANDSCAPE_REV
                    GLCDDeviceWidth = GLCD_WIDTH
                    GLCDDeviceHeight = GLCD_HEIGHT

                    _temp_LT7686.3 = 1

                    __temp_LT7686.2 = 0
                    __temp_LT7686.1 = 1

            case else
                    GLCDDeviceWidth = GLCD_WIDTH
                    GLCDDeviceHeight = GLCD_HEIGHT
                    
                    _temp_LT7686.3 = 0

                    __temp_LT7686.2 = 0
                    __temp_LT7686.1 = 0 
                    

        end select

        // rewrite to register
        LT7686_WriteCMD(LT7686_REG_MACR)
        LT7686_WriteData(__temp_LT7686)
        LT7686_Check_Busy_Draw

        LT7686_WriteCMD(LT7686_REG_DPCR)
        LT7686_WriteData(_temp_LT7686)
        LT7686_Check_Busy_Draw

    end sub

Sub LT7686_Hardware_Reset()

        LT7686_RST = 1
        Wait 30 ms
        LT7686_RST = 0
        Wait 200 ms
        LT7686_RST = 1
        Wait 30 ms

    End Sub

Sub LT7686_Software_Reset()

        LT7686_WriteCMD( LT7686_REG_SRR )
        LT7686_WriteData( 0x01 )
        wait 1 ms

    End Sub

Function LT7686_Check_Init() As Byte

            // Reuse variables to save RAM
            Dim LT7686_Check_Init_Fail, LT7686_StatusReadResult, LT7686_Check_Init_Counter as Byte
            
            LT7686_Check_Init = False
            LT7686_Check_Init_Counter = 0

            LT7686_Check_Init_Fail = 0

            do
                // Increment the exit counters... to ensure this is NOT a deadlock
                LT7686_Check_Init_Counter++
                LT7686_Check_Init_Fail++
                LT7686_StatusReadResult = LT7686_StatusRead
                                
                if ( LT7686_StatusReadResult & 0x02 ) = 0x00 and LT7686_Check_Init_Fail < LT7686_CHECKATTEMPTS Then     

                    wait 100 us                  
            
                    LT7686_WriteCMD( 0x01 )
                    // Check if CCR register and PLL are ready
                    if ( ( LT7686_DataRead() & 0x80 ) = 0x80 ) Then       
                        LT7686_Check_Init = True
                        exit Sub
                    end if
                else
                        wait 100 us
                        LT7686_WriteCMD(0x01)                       
                        LT7686_WriteData(0x80)
                end if

                //~ Hard reset every 5 times
                if LT7686_Check_Init = 0 & LT7686_Check_Init_Fail = 5 Then

                    LT7686_Hardware_Reset
                    LT7686_LoadDefaultRegisters
                    LT7686_WriteCMD ( 0xE4 )
                    LT7686_WriteData ( 0x01 )
                    wait 100 ms
                    LT7686_Check_Init_Fail = 0
                End if
            Loop while ( LT7686_Check_Init = 0 & LT7686_Check_Init_Counter <> LT7686_CHECKATTEMPTS )

            //! But, this is failure to init
            LT7686_Check_Init = True

    End Function

    ' PARAMS ( __LT7686_fbuffer, __LT7686_outbuffer )
Macro LT7686_macro_WriteTwoBytes 

        LT7686_CS = 0
        
        #ifdef LT7686_HARDWARESPI
            // Send via Hardware SPI
            FastHWSPITransfer( __LT7686_fbuffer )
            FastHWSPITransfer( __LT7686_outbuffer  )
        #else

            Wait LT7686_STATE1_DELAY us

            SET LT7686_SCK STATE2
            SET C OFF
            repeat 8
                if __LT7686_fbuffer.7 = ON  then
                set LT7686_DO ON
                else
                set LT7686_DO OFF
                end if
                SET LT7686_SCK STATE1
                Wait LT7686_STATE1_DELAY us

                rotate __LT7686_fbuffer left
                
                SET LT7686_SCK STATE2
                Wait LT7686_STATE2_DELAY us
            end repeat

            wait LT7686_STATE2_DELAY us

            SET C OFF            
            repeat 8
                SET LT7686_SCK STATE1
                if __LT7686_outbuffer.7 = ON  then
                set LT7686_DO ON
                else
                set LT7686_DO OFF
                end if
                
                Wait LT7686_STATE1_DELAY us

                rotate __LT7686_outbuffer left
                
                SET LT7686_SCK STATE2
                Wait LT7686_STATE2_DELAY us
            end repeat
            set LT7686_DO OFF
            Wait LT7686_STATE2_DELAY us
        #endif
        LT7686_CS = 1
        
    End Macro

Sub LT7686_Wait10ms

    #ifdef LT7686_COMMAND_DEBUG
        HserPrint "0xAA, 0xAA, 0xAA, 0xAA"
        HserPrintCRLF
    #endif

    Wait 10 ms

    End Sub

Sub LT7686_WriteCMD( In __outbyte as Byte )

        #ifdef LT7686_COMMAND_DEBUG
            HserPrint "0x11, 0x"
            HserPrint hex(__outbyte)
        #endif

        // 0x00
        dim __LT7686_fbuffer as Byte
        __LT7686_fbuffer = LT7686_REGISTER_ADDRESS
        __LT7686_outbuffer = __outbyte
        LT7686_macro_WriteTwoBytes

    End Sub

Sub LT7686_WriteData( In __outbyte as Byte )

        #ifdef LT7686_COMMAND_DEBUG
            HserPrint ", 0x13, 0x"
            HserPrint hex(__outbyte)
            HserPrintCRLF
        #endif

        // 0x80
        dim __LT7686_fbuffer as Byte
        __LT7686_fbuffer = LT7686_WRITE_REGISTER_MEM
        __LT7686_outbuffer = __outbyte
        LT7686_macro_WriteTwoBytes   

    End Sub

Sub LT7686_WriteData_Continous( In __outbyte as Byte )

        #ifdef LT7686_HARDWARESPI
        !
            // Send via Hardware SPI
            FastHWSPITransfer( __outbyte)
        #else
        !
            dim __LT7686_outbuffer as Byte
            __LT7686_outbuffer = __outbyte            
            repeat 8
                if __LT7686_outbuffer.7 = ON  then
                set LT7686_DO ON
                else
                set LT7686_DO OFF
                end if
                SET LT7686_SCK STATE1
                Wait LT7686_STATE1_DELAY us
                rotate __LT7686_outbuffer left
                SET LT7686_SCK STATE2
                Wait LT7686_STATE2_DELAY us
            end repeat
        #endif

    End Sub


Function LT7686_DataRead as Byte

        #ifdef LT7686_COMMAND_DEBUG
            HserPrint ", 0x12, 0x??"
            HserPrintCRLF
        #endif

        dim __LT7686_outbuffer as Byte

        LT7686_CS = 0


        #ifdef LT7686_HARDWARESPI
            // Send via Hardware SPI
            FastHWSPITransfer( 0xC0 )
            HWSPITransfer( 0x00, LT7686_DataRead )
        #else

            Wait LT7686_STATE1_DELAY us

            __LT7686_outbuffer = 0xC0            
            repeat 8
                if __LT7686_outbuffer.7 = ON  then
                set LT7686_DO ON
                else
                set LT7686_DO OFF
                end if
                SET LT7686_SCK STATE1
                Wait LT7686_STATE1_DELAY us
                rotate __LT7686_outbuffer left
                SET LT7686_SCK STATE2
                Wait LT7686_STATE2_DELAY us
            end repeat
            
            Dir LT7686_DI In
            Wait LT7686_STATE2_DELAY us

            repeat 8

                LT7686_DO = 0
                SET LT7686_SCK STATE1
                Wait LT7686_STATE1_DELAY us

                Set C Off
                rotate LT7686_DataRead left

                if LT7686_DI = On then
                    set LT7686_DataRead.0 On
                Else
                    set LT7686_DataRead.0 Off
                end if

                SET LT7686_SCK STATE2
                Wait LT7686_STATE2_DELAY us

            end repeat

            Wait While LT7686_SCK <> STATE2
            Wait LT7686_STATE1_DELAY us
            set LT7686_DO OFF
            Wait LT7686_STATE1_DELAY us
        #endif
        
        LT7686_CS = 1
        
    End Function





Function LT7686_StatusRead as Byte

        #ifdef LT7686_COMMAND_DEBUG
            HserPrint "// Read Status Register"
            HserPrintCRLF
        #endif

        LT7686_StatusRead = 0x00

        dim __LT7686_outbuffer as Byte

        LT7686_CS = 0
        

        #ifdef LT7686_HARDWARESPI
            
            // Send via Hardware SPI
            FastHWSPITransfer( 0x40 )
            HWSPITransfer( 0x00, __LT7686_outbuffer )
            LT7686_StatusRead = __LT7686_outbuffer

        #else

            Wait LT7686_STATE1_DELAY us

            __LT7686_outbuffer = 0x40            
            repeat 8
                if __LT7686_outbuffer.7 = ON  then
                set LT7686_DO ON
                else
                set LT7686_DO OFF
                end if
                SET LT7686_SCK STATE1
                Wait LT7686_STATE1_DELAY us
                rotate __LT7686_outbuffer left
                SET LT7686_SCK STATE2
                Wait LT7686_STATE2_DELAY us
            end repeat
            
            Wait LT7686_STATE2_DELAY us

            repeat 8

                Dir LT7686_DI In

                SET LT7686_SCK STATE1
                Set C Off
                rotate LT7686_StatusRead left

                Wait LT7686_STATE1_DELAY us

                if LT7686_DI = On then
                    set LT7686_StatusRead.0 On
                Else
                    set LT7686_StatusRead.0 Off
                end if
                
                SET LT7686_SCK STATE2
                Wait LT7686_STATE2_DELAY us

            end repeat

            Wait LT7686_STATE1_DELAY us
            Dir LT7686_DO Out

        #endif

        LT7686_CS = 1

    End Function

        //Core Task is Busy, Fontwr_Busy
Sub LT7686_Check_Busy_Draw

    do while  ( ( LT7686_StatusRead() & 0x08 ) = 0x08 )
        wait 10 us
	loop 
    
    End Sub


Table LT7686_INITTFT_DATASET_800_480

    #insert <levetop800x480.h>

    End Table

Table LT7686_INITTFT_DATASET_1024_600

    #insert <levetop1024x600.h>

    End Table
// Registers and constants for LT7686  from Character/Graphic TFT LCD Controller

    #define LT7686_REG_SRR				0x00	// Software Reset Register (SRR)
    #define LT7686_REG_CCR				0x01	// Chip Configuration Register (CRR)
    #define LT7686_REG_MACR				0x02	// Memory Access Control Register (MACR)
    #define LT7686_REG_ICR				0x03	// Input Control Register (ICR)
    #define LT7686_REG_MRWDP			0x04	// Memory Data Read/Write Port (MRWDP)
    #define LT7686_REG_PPLLC1			0x05	// SCLK PLL Control Register 1 (PPLLC1)
    #define LT7686_REG_PPLLC2			0x06	// SCLK PLL Control Register 2 (PPLLC2)
    #define LT7686_REG_MPLLC1			0x07	// MCLK PLL Control Register 1 (MPLLC1)
    #define LT7686_REG_MPLLC2			0x08	// MCLK PLL Control Register 2 (MPLLC2)
    #define LT7686_REG_SPLLC1			0x09	// CCLK PLL Control Register 1 (SPLLC1)
    #define LT7686_REG_SPLLC2			0x0a	// CCLK PLL Control Register 2 (SPLLC2)
    #define LT7686_REG_INTEN			0x0b	// Interrupt Enable Register (INTEN)
    #define LT7686_REG_INTF				0x0c	// Interrupt Event Flag Register (INTF)
    #define LT7686_REG_MINTFR			0x0d	// Mask Interrupt Flag Register (MINTFR)
    #define LT7686_REG_PUENR			0x0e	// Pull-high control Register (PUENR)
    #define LT7686_REG_PSFSR			0x0f	// PDAT for PIO/key Function Select Register (PSFSR)
    #define LT7686_REG_MPWCTR			0x10	// Main/PIP Window Control Register (MPWCTR)
    #define LT7686_REG_PIPCDEP			0x11	// PIP Window Colour Depth Setting (PIPCDEP)
    #define LT7686_REG_DPCR				0x12	// Display Configuration Register (DPCR)
    #define LT7686_REG_PCSR				0x13	// Panel scan Clock and Data Setting Register (PCSR)
    #define LT7686_REG_HDWR				0x14	// Horizontal Display Width Register (HDWR)
    #define LT7686_REG_HDWFTR			0x15	// Horizontal Display Width Fine Tune Register (HDWFTR)
    #define LT7686_REG_HNDR				0x16	// Horizontal Non-Display Period Register (HNDR)
    #define LT7686_REG_HNDFTR			0x17	// Horizontal Non-Display Period Fine Tune Register (HNDFTR)
    #define LT7686_REG_HSTR				0x18	// HSYNC Start Position Register (HSTR)
    #define LT7686_REG_HPWR				0x19	// HSYNC Pulse Width Register (HPWR)
    #define LT7686_REG_VDHR0			0x1a	// Vertical Display Height Register 0 (VDHR0)
    #define LT7686_REG_VDHR1			0x1b	// Vertical Display Height Register 1 (VDHR1)
    #define LT7686_REG_VNDR0			0x1c	// Vertical Non-Display Period Register 0 (VNDR0)
    #define LT7686_REG_VNDR1			0x1d	// Vertical Non-Display Period Register 1 (VNDR1)
    #define LT7686_REG_VSTR				0x1e	// VSYNC Start Position Register (VSTR)
    #define LT7686_REG_VPWR				0x1f	// VSYNC Pulse Width Register (VPWR)
    #define LT7686_REG_MISA0			0x20	// MaIn Image Start Address 0 (MISA0)
    #define LT7686_REG_MISA1			0x21	// MaIn Image Start Address 1 (MISA1)
    #define LT7686_REG_MISA2			0x22	// MaIn Image Start Address 2 (MISA2)
    #define LT7686_REG_MISA3			0x23	// MaIn Image Start Address 3 (MISA3)
    #define LT7686_REG_MIW0				0x24	// MaIn Image Width 0 (MIW0)
    #define LT7686_REG_MIW1				0x25	// MaIn Image Width 1 (MIW1)
    #define LT7686_REG_MWULX0			0x26	// MaIn Window Upper-Left corner X-coordinates 0 (MWULX0)
    #define LT7686_REG_MWULX1			0x27	// MaIn Window Upper-Left corner X-coordinates 1 (MWULX1)
    #define LT7686_REG_MWULY0			0x28	// MaIn Window Upper-Left corner Y-coordinates 0 (MWULY0)
    #define LT7686_REG_MWULY1			0x29	// MaIn Window Upper-Left corner Y-coordinates 1 (MWULY1)
    #define LT7686_REG_PWDULX0			0x2a	// PIP 1 or 2 Window Display Upper-Left corner X-coordinates 0 (PWDULX0)
    #define LT7686_REG_PWDULX1			0x2b	// PIP 1 or 2 Window Display Upper-Left corner X-coordinates 1 (PWDULX1)
    #define LT7686_REG_PWDULY0			0x2c	// PIP 1 or 2 Window Display Upper-Left corner Y-coordinates 0 (PWDULY0)
    #define LT7686_REG_PWDULY1			0x2d	// PIP 1 or 2 Window Display Upper-Left corner Y-coordinates 1 (PWDULY1)
    #define LT7686_REG_PISA0			0x2e	// PIP 1 or 2 Image Start Address 0 (PISA0)
    #define LT7686_REG_PISA1			0x2f	// PIP 1 or 2 Image Start Address 1 (PISA1)
    #define LT7686_REG_PISA2			0x30	// PIP 1 or 2 Image Start Address 2 (PISA2)
    #define LT7686_REG_PISA3			0x31	// PIP 1 or 2 Image Start Address 3 (PISA3)
    #define LT7686_REG_PIW0				0x32	// PIP 1 or 2 Image Width 0 (PIW0)
    #define LT7686_REG_PIW1				0x33	// PIP 1 or 2 Image Width 1 (PIW1)
    #define LT7686_REG_PWIULX0			0x34	// PIP 1 or 2 Window Image Upper-Left corner X-coordinates 0 (PWIULX0)
    #define LT7686_REG_PWIULX1			0x35	// PIP 1 or 2 Window Image Upper-Left corner X-coordinates 1 (PWIULX1)
    #define LT7686_REG_PWIULY0			0x36	// PIP 1 or 2 Window Image Upper-Left corner Y-coordinates 0 (PWIULY0)
    #define LT7686_REG_PWIULY1			0x37	// PIP 1 or 2 Window Image Upper-Left corner Y-coordinates 1 (PWIULY1)
    #define LT7686_REG_PWW0				0x38	// PIP 1 or 2 Window Width 0 (PWW0)
    #define LT7686_REG_PWW1				0x39	// PIP 1 or 2 Window Width 1 (PWW1)
    #define LT7686_REG_PWH0				0x3a	// PIP 1 or 2 Window Height 0 (PWH0)
    #define LT7686_REG_PWH1				0x3b	// PIP 1 or 2 Window Height 1 (PWH1)
    #define LT7686_REG_GTCCR			0x3c	// Graphic / Text Cursor Control Register (GTCCR)
    #define LT7686_REG_BTCR				0x3d	// Blink Time Control Register (BTCR)
    #define LT7686_REG_CURHS			0x3e	// Text Cursor Horizontal Size Register (CURHS)
    #define LT7686_REG_CURVS			0x3f	// Text Cursor Vertical Size Register (CURVS)
    #define LT7686_REG_GCHP0			0x40	// Graphic Cursor Horizontal Position Register 0 (GCHP0)
    #define LT7686_REG_GCHP1			0x41	// Graphic Cursor Horizontal Position Register 1 (GCHP1)
    #define LT7686_REG_GCVP0			0x42	// Graphic Cursor Vertical Position Register 0 (GCVP0)
    #define LT7686_REG_GCVP1			0x43	// Graphic Cursor Vertical Position Register 1 (GCVP1)
    #define	LT7686_REG_GCC0				0x44	// Graphic Cursor Colour 0 (GCC0)
    #define LT7686_REG_GCC1				0x45	// Graphic Cursor Colour 1 (GCC1)
    // 0x46 - 0x4f reserved
    #define LT7686_REG_CVSSA0			0x50	// Canvas Start address 0 (CVSSA0)
    #define LT7686_REG_CVSSA1			0x51	// Canvas Start address 1 (CVSSA1)
    #define LT7686_REG_CVSSA2			0x52	// Canvas Start address 2 (CVSSA2)
    #define LT7686_REG_CVSSA3			0x53	// Canvas Start address 3 (CVSSA3)
    #define LT7686_REG_CVS_IMWTH0		0x54	// Canvas image width 0 (CVS_IMWTH0)
    #define LT7686_REG_CVS_IMWTH1		0x55	// Canvas image width 1 (CVS_IMWTH1)
    #define LT7686_REG_AWUL_X0			0x56	// Active Window Upper-Left corner X-coordinates 0 (AWUL_X0)
    #define LT7686_REG_AWUL_X1			0x57	// Active Window Upper-Left corner X-coordinates 1 (AWUL_X1)
    #define LT7686_REG_AWUL_Y0			0x58	// Active Window Upper-Left corner Y-coordinates 0 (AWUL_Y0)
    #define LT7686_REG_AWUL_Y1			0x59	// Active Window Upper-Left corner Y-coordinates 1 (AWUL_Y1)
    #define LT7686_REG_AW_WTH0			0x5a	// Active Window Width 0 (AW_WTH0)
    #define LT7686_REG_AW_WTH1			0x5b	// Active Window Width 1 (AW_WTH1)
    #define LT7686_REG_AW_HT0			0x5c	// Active Window Height 0 (AW_HT0)
    #define LT7686_REG_AW_HT1			0x5d	// Active Window Height 1 (AW_HT1)
    #define LT7686_REG_AW_COLOR			0x5e	// Color Depth of Canvas & Active Window (AW_COLOR)
    #define LT7686_REG_CURH0			0x5f	// Graphic Read/Write position Horizontal Position Register 0 (CURH0)
    #define LT7686_REG_CURH1			0x60	// Graphic Read/Write position Horizontal Position Register 1 (CURH1)
    #define LT7686_REG_CURV0			0x61	// Graphic Read/Write position Vertical Position Register 0 (CURV0)
    #define LT7686_REG_CURV1			0x62	// Graphic Read/Write position Vertical Position Register 1 (CURV1)
    #define LT7686_REG_F_CURX0			0x63	// Text Write X-coordinates Register 0 (F_CURX0)
    #define LT7686_REG_F_CURX1			0x64	// Text Write X-coordinates Register 1 (F_CURX1)
    #define LT7686_REG_F_CURY0			0x65	// Text Write Y-coordinates Register 0 (F_CURY0)
    #define LT7686_REG_F_CURY1			0x66	// Text Write Y-coordinates Register 1 (F_CURY1)
    #define LT7686_REG_DCR0				0x67	// Draw Line / Triangle Control Register 0 (DCR0)
    #define LT7686_REG_DLHSR0			0x68	// Draw Line/Square/Triangle Point 1 X-coordinates Register 0 (DLHSR0)
    #define LT7686_REG_DLHSR1			0x69	// Draw Line/Square/Triangle Point 1 X-coordinates Register 1 (DLHSR1)
    #define LT7686_REG_DLVSR0			0x6a	// Draw Line/Square/Triangle Point 1 Y-coordinates Register 0 (DLVSR0)
    #define LT7686_REG_DLVSR1			0x6b	// Draw Line/Square/Triangle Point 1 Y-coordinates Register 1 (DLVSR1)
    #define LT7686_REG_DLHER0			0x6c	// Draw Line/Square/Triangle Point 2 X-coordinates Register 0 (DLHER0)
    #define LT7686_REG_DLHER1			0x6d	// Draw Line/Square/Triangle Point 2 X-coordinates Register 1 (DLHER1)
    #define LT7686_REG_DLVER0			0x6e	// Draw Line/Square/Triangle Point 2 Y-coordinates Register 0 (DLVER0)
    #define LT7686_REG_DLVER1			0x6f	// Draw Line/Square/Triangle Point 2 Y-coordinates REgister 1 (DLVER1)
    #define LT7686_REG_DTPH0			0x70	// Draw Triangle Point 3 X-coordinates Register 0 (DTPH0)
    #define LT7686_REG_DTPH1			0x71	// Draw Triangle Point 3 X-coordinates Register 1 (DTPH1)
    #define LT7686_REG_DTPV0			0x72	// Draw Triangle Point 3 Y-coordinates Register 0 (DTPV0)
    #define LT7686_REG_DTPV1			0x73	// Draw Triangle Point 3 Y-coordinates Register 1 (DTPV1)
    // 0x74 - 0x75 reserved
    #define LT7686_REG_DCR1				0x76	// Draw Circle/Ellipse/Ellipse Curve/Circle Square Control Register 1 (DCR1)
    #define LT7686_REG_ELL_A0			0x77	// Draw Circle/Ellipse/Circle Square Major radius Setting Register (ELL_A0)
    #define LT7686_REG_ELL_A1			0x78	// Draw Circle/Ellipse/Circle Square Major radius Setting Register (ELL_A1)
    #define LT7686_REG_ELL_B0			0x79	// Draw Circle/Ellipse/Circle Square Minor radius Setting Register (ELL_B0)
    #define LT7686_REG_ELL_B1			0x7a	// Draw Circle/Ellipse/Circle Square Minor radius Setting Register (ELL_B1)
    #define LT7686_REG_DEHR0			0x7b	// Draw Circle/Ellipse/Circle Square Center X-coordinates Register 0 (DEHR0)
    #define LT7686_REG_DEHR1			0x7c	// Draw Circle/Ellipse/Circle Square Center X-coordinates Register 1 (DEHR1)
    #define LT7686_REG_DEVR0			0x7d	// Draw Circle/Ellipse/Circle Square Center Y-coordinates Register 0 (DEVR0)
    #define LT7686_REG_DEVR1			0x7e	// Draw Circle/Ellipse/Circle Square Center Y-coordinates Register 1 (DEVR1)
    // 0x7f reserved
    #define LT7686_REG_PSCLR			0x84	// PWM Prescaler Register (PSCLR)
    #define LT7686_REG_PMUXR			0x85	// PWM clock Mux Register (PMUXR)
    #define LT7686_REG_PCFGR			0x86	// PWM Configuration Register (PCFGR)
    #define LT7686_REG_DZ_LENGTH		0x87	// Timer 0 Dead zone length register (DZ_LENGTH)
    #define LT7686_REG_TCMPB0L			0x88	// Timer 0 compare buffer register (TCMPB0L)
    #define LT7686_REG_TCMPB0H			0x89	// Timer 0 compare buffer register (TCMPB0H)
    #define LT7686_REG_TCNTB0L			0x8a	// Timer 0 count buffer register (TCNTB0L)
    #define LT7686_REG_TCNTB0H			0x8b	// Timer 0 count buffer register (TCNTB0H)
    #define LT7686_REG_TCMPB1L			0x8c	// Timer 1 compare buffer register (TCMPB1L)
    #define LT7686_REG_TCMPB1H			0x8d	// Timer 1 compare buffer register (TCMPB1H)
    #define LT7686_REG_TCNTB1L			0x8e	// Timer 1 count buffer register (TCNTB1L)
    #define LT7686_REG_TCNTB1H			0x8f	// Timer 1 count buffer register (TCNTB1H)
    #define LT7686_REG_BTE_CTRL0		0x90	// BTE Function Control Register 0 (BTE_CTRL0)
    #define LT7686_REG_BTE_CTRL1		0x91	// BTE Function Control Register 1 (BTE_CTRL1)
    #define LT7686_REG_BTE_COLR			0x92	// Source 0/1 & Destination Color Depth (BTE_COLR)
    #define LT7686_REG_S0_STR0			0x93	// Source 0 memory start address 0 (S0_STR0)
    #define LT7686_REG_S0_STR1			0x94	// Source 0 memory start address 1 (S0_STR1)
    #define LT7686_REG_S0_STR2			0x95	// Source 0 memory start address 2 (S0_STR2)
    #define LT7686_REG_S0_STR3			0x96	// Source 0 memory start address 3 (S0_STR3)
    #define LT7686_REG_S0_WTH0			0x97	// Source 0 image width 0 (S0_WTH0)
    #define LT7686_REG_S0_WTH1			0x98	// Source 0 image width 1 (S0_WTH1)
    #define LT7686_REG_S0_X0			0x99	// Source 0 Window Upper-Left corner X-coordinates 0 (S0_X0)
    #define LT7686_REG_S0_X1			0x9a	// Source 0 Window Upper-Left corner X-coordinates 1 (S0_X1)
    #define LT7686_REG_S0_Y0			0x9b	// Source 0 Window Upper-Left corner Y-coordinates 0 (S0_Y0)
    #define LT7686_REG_S0_Y1			0x9c	// Source 0 Window Upper-Left corner Y-coordinates 1 (S0_Y1)
    #define LT7686_REG_S1_STR0			0x9d	// Source 1 memory start address 0 (S1_STR0)
    #define LT7686_REG_S1_STR1			0x9e	// Source 1 memory start address 1 (S1_STR1)
    #define LT7686_REG_S1_STR2			0x9f	// Source 1 memory start address 2 (S1_STR2)
    #define LT7686_REG_S1_STR3			0xa0	// Source 1 memory start address 3 (S1_STR3)
    #define LT7686_REG_S1_WTH0			0xa1	// Source 1 image width 0 (S1_WTH0)
    #define LT7686_REG_S1_WTH1			0xa2	// Source 1 image width 1 (S1_WTH1)
    #define LT7686_REG_S1_X0			0xa3	// Source 1 Window Upper-Left corner X-coordinates 0 (S1_X0)
    #define LT7686_REG_S1_X1			0xa4	// Source 1 Window Upper-Left corner X-coordinates 1 (S1_X1)
    #define	LT7686_REG_S1_Y0			0xa5	// Source 1 Window Upper-Left corner Y-coordinates 0 (S1_Y0)
    #define LT7686_REG_S1_Y1			0xa6	// Source 1 Window Upper-Left corner Y-coordinates 1 (S1_Y1)
    #define LT7686_REG_DT_STR0			0xa7	// Destination memory start address 0 (DT_STR0)
    #define LT7686_REG_DT_STR1			0xa8	// Destination memory start address 1 (DT_STR1)
    #define LT7686_REG_DT_STR2			0xa9	// Destination memory start address 2 (DT_STR2)
    #define LT7686_REG_DT_STR3			0xaa	// Destination memory start address 3 (DT_STR3)
    #define LT7686_REG_DT_WTH0			0xab	// Destination image width 0 (DT_WTH0)
    #define LT7686_REG_DT_WTH1			0xac	// Destination image width 1 (DT_WTH1)
    #define LT7686_REG_DT_X0			0xad	// Destination Window Upper-Left corner X-coordinates 0 (DT_X0)
    #define LT7686_REG_DT_X1			0xae	// Destination Window Upper-Left corner X-coordinates 1 (DT_X1)
    #define LT7686_REG_DT_Y0			0xaf	// Destination Window Upper-Left corner Y-coordinates 0 (DT_Y0)
    #define LT7686_REG_DT_Y1			0xb0	// Destination Window Upper-Left corner Y-coordinates 1 (DT_Y1)
    #define LT7686_REG_BTE_WTH0			0xb1	// BTE Window Width 0 (BTE_WTH0)
    #define LT7686_REG_BTE_WTH1			0xb2	// BTE Window Width 1 (BTE_WTH1)
    #define LT7686_REG_BTE_HIG0			0xb3	// BTE Window Height 0 (BTE_HIG0)
    #define LT7686_REG_BTE_HIG1			0xb4	// BTE Window Height 1 (BTE_HIG1)
    #define LT7686_REG_APB_CTRL			0xb5	// Alpha Blending (APB_CTRL)
    #define LT7686_REG_DMA_CTRL			0xb6	// Serial flash DMA Controller REG (DMA_CTRL)
    #define LT7686_REG_SFL_CTRL			0xb7	// Serial Flash/ROM Controller Register (SFL_CTRL)
    #define LT7686_REG_SPIDR			0xb8	// SPI master Tx/Rx FIFO Data Register (SPIDR)
    #define LT7686_REG_SPIMCR2			0xb9	// SPI master Control Register (SPIMCR2)
    #define LT7686_REG_SPIMSR			0xba	// SPI master Status Register (SPIMSR)
    #define LT7686_REG_SPI_DIVSOR		0xbb	// SPI Clock period (SPI_DIVSOR)
    #define LT7686_REG_DMA_SSTR0		0xbc	// Serial flash DMA Source Starting Address 0 (DMA_SSTR0)
    #define LT7686_REG_DMA_SSTR1		0xbd	// Serial flash DMA Source Starting Address 1 (DMA_SSTR1)
    #define LT7686_REG_DMA_SSTR2		0xbe	// Serial flash DMA Source Starting Address 2 (DMA_SSTR2)
    #define LT7686_REG_DMA_SSTR3		0xbf	// Serial flash DMA Source Starting Address 3 (DMA_SSTR3)
    #define LT7686_REG_DMA_DX0			0xc0	// DMA Destination Window Upper-Left corner X-coordinates 0 (DMA_DX0)
    #define LT7686_REG_DMA_DX1			0xc1	// DMA Destination Window Upper-Left corner X-coordinates 1 (DMA_DX1)
    #define LT7686_REG_DMA_DY0			0xc2	// DMA Destination Window Upper-Left corner Y-coordinates 0 (DMA_DY0)
    #define	LT7686_REG_DMA_DY1			0xc3	// DMA Destination Window Upper-Left corner Y-coordinates 1 (DMA_DY1)
    // 0xc4 - 0xc5 reserved
    #define LT7686_REG_DMAW_WTH0		0xc6	// DMA Block Width 0 (DMAW_WTH0)
    #define LT7686_REG_DMAW_WTH1		0xc7	// DMA Block Width 1 (DMAW_WTH1)
    #define LT7686_REG_DMAW_HIGH0		0xc8	// DMA Block Height 0 (DMAW_HIGH0)
    #define LT7686_REG_DMAW_HIGH1		0xc9	// DMA Block Height 1 (DMAW_HIGH1)
    #define LT7686_REG_DMA_SWTH0		0xca	// DMA Source Picture Width 0 (DMA_SWTH0)
    #define LT7686_REG_DMA_SWTH1		0xcb	// DMA Source Picture Width 1 (DMA_SWTH1)
    #define LT7686_REG_CCR0				0xcc	// Character Control Register 0 (CCR0)
    #define LT7686_REG_CCR1				0xcd	// Character Control Register 1 (CCR1)
    #define LT7686_REG_GTFNT_SEL		0xce	// GT Character ROM Select (GTFNT_SEL)
    #define LT7686_REG_GTFNT_CR			0xcf	// GT Character ROM Control register (GTFNT_CR)
    #define LT7686_REG_FLDR				0xd0	// Character Line gap Setting Register (FLDR)
    #define LT7686_REG_F2FSSR			0xd1	// Character to Character Space Setting Register (F2FSSR)
    #define LT7686_REG_FGCR				0xd2	// Foreground Color Register - Red (FGCR)
    #define LT7686_REG_FGCG				0xd3	// Foreground Color Register - Green (FGCG)
    #define LT7686_REG_FGCB				0xd4	// Foreground Color Register - Blue (FGCB)
    #define	LT7686_REG_BGCR				0xd5	// Background Color Register - Red (GBCR)
    #define LT7686_REG_BGCG				0xd6	// Background Color Register - Green (BGCG)
    #define LT7686_REG_BGCB				0xd7	// Background Color Register - Blue (BGCB)
    // 0xd8 - 0xda reserved
    #define LT7686_REG_CGRAM_STR0		0xdb	// CGRAM Start Address 0 (CGRAM_STR0)
    #define LT7686_REG_CGRAM_STR1		0xdc	// CGRAM Start Address 1 (CGRAM_STR1)
    #define LT7686_REG_CGRAM_STR2		0xdd	// CGRAM Start Address 2 (CGRAM_STR2)
    #define LT7686_REG_CGRAM_STR3		0xde	// CGRAM Start Address 3 (CGRAM_STR3)
    #define LT7686_REG_PMU				0xdf	// Power Management register (PMU)
    #define LT7686_REG_SDRAR			0xe0	// SDRAM attribute register (SDRAR)
    #define LT7686_REG_SDRMD			0xe1	// SDRAM mode register & extended mode register (SDRMD)
    #define LT7686_REG_SDR_REF_ITVL0	0xe2	// SDRAM auto refresh interval (SDR_REG_ITVL0)
    #define LT7686_REG_SDR_REF_ITVL1	0xe3	// SDRAM auto refresh interval (SDR_REG_ITVL1)
    #define LT7686_REG_SDRCR			0xe4	// SDRAM Control register (SDRCR)
    #define	LT7686_REG_I2CMCPR0			0xe5	// I2C Master Clock Pre-scale Register 0 (I2CMCPR0)
    #define LT7686_REG_I2CMCPR1			0xe6	// I2C Master Clock Pre-scale Register 1 (I2CMCPR1)
    #define LT7686_REG_I2CMTXR			0xe7	// I2C Master Transmit Register (I2CMTXR)
    #define LT7686_REG_I2CMRXR			0xe8	// I2C Master Receive Register (I2CMRXR)
    #define LT7686_REG_I2CMCMDR			0xe9	// I2C Master Command Register (I2CMCMDR)
    #define	LT7686_REG_I2CMSTUR			0xea	// I2C Master Status Register (I2CMSTUR)
    #define	LT7686_REG_GPIOAD			0xf0	// GPIO-A direction (GPIOAD)
    #define LT7686_REG_GPIOA			0xf1	// GPIO-A
    #define LT7686_REG_GPIOB			0xf2	// GPIO-B
    #define LT7686_REG_GPIOCD			0xf3	// GPIO-C direction (GPIOCD)
    #define LT7686_REG_GPIOC			0xf4	// GPIO-C
    #define LT7686_REG_GPIODD			0xf5	// GPIO-D direction (GPIODD)
    #define LT7686_REG_GPIOD			0xf6	// GPIO-D
    #define LT7686_REG_GPIOED			0xf7	// GPIO-E direction (GPIOED)
    #define LT7686_REG_GPIOE			0xf8	// GPIO-E
    #define LT7686_REG_GPIOFD			0xf9	// GPIO-F direction (GPIOFD)
    #define LT7686_REG_GPIOF			0xfa	// GPIO-F
    #define LT7686_REG_KSCR1			0xfb	// Key-Scan Control Register 1 (KSCR1)
    #define LT7686_REG_KSCR2			0xfc	// Key-Scan Controller Register 2 (KSCR2)
    #define	LT7686_REG_KSDR0			0xfd	// Key-Scan Data Register (KSDR0)
    #define LT7686_REG_KSDR1			0xfe	// Key-Scan Data Register (KSDR1)
    #define LT7686_REG_KSDR2			0xff	// Key-Scan Data Register (KSDR2)

    // Constants
    #define	LT7686_OSC_FREQ				10							//  crystal clock (MHz)
    #define	LT7686_DRAM_FREQ			100							//  SDRAM clock frequency (MHz)
    #define LT7686_CORE_FREQ			100							//  core (system) clock frequency (MHz)
    #define LT7686_SCAN_FREQ			50							//  pixel scan clock frequency (MHz)

    #define LT7686_PANEL_HNDR			160							//  horizontal non-display period or back porch
    #define LT7686_PANEL_HSTR			160							//  horizontal start position or front porch
    #define LT7686_PANEL_HPWR			70							//  HSYNC pulse width
    #define LT7686_PANEL_VNDR			23							//  vertical non-display period
    #define LT7686_PANEL_VSTR			12							//  vertical start position
    #define LT7686_PANEL_VPWR			10							//  VSYNC pulse width

    #define LT7686_PLL_DIV_2			0x02						//  PLL divided by 2
    #define LT7686_PLL_DIV_4			0x04						//  PLL divided by 4
    #define LT7686_PLL_DIV_8			0x06						//  PLL divided by 8
    #define LT7686_PLL_DIV_16			0x16						//  PLL divided by 16
    #define LT7686_PLL_DIV_32			0x26						//  PLL divided by 32
    #define LT7686_PLL_DIV_64			0x36						//  PLL divided by 64

    #define LT7686_STATUS_READ_REGISTER     0x40
    #define LT7686_REGISTER_ADDRESS         0x00
    #define LT7686_WRITE_REGISTER_MEM       0x80
    #define LT7686_STATUS_WRITE_REGISTER    0xC0
    
    
    #define LT7686_SETB4              0b00010000
    #define LT7686_CLRB4              0b11101111

    #define LT7686_SETB5              0b00100000
    #define LT7686_CLRB5              0b11011111




