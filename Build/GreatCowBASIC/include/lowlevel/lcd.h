'     Liquid Crystal Display routines for Great Cow BASIC
'     Copyright (C) 2006-xxxx Hugh Considine, Stefano Bonomi, Ruud de Vreugd, Evan Venn, Theo Loermans and Wiliam Roth
'
'     This library is free software; you can redistribute it and/or
'     modify it under the terms of the GNU Lesser General Public
'     License as published by the Free Software Foundation; either
'     version 2.1 of the License, or (at your option) any later version.
'
'     This library is distributed in the hope that it will be useful,
'     but WITHOUT ANY WARRANTY; without even the implied warranty of
'     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'     Lesser General Public License for more details.
'
'     You should have received a copy of the GNU Lesser General Public
'     License along with this library; if not, write to the Free Software
'     Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
'
'   ******************************************************************************
'     IMPORTANT:
'     THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
'     UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
'     COMMANDS UNUSABLE!
'   ******************************************************************************
'     Credits:
'     Hugh Considine:   4 and 8 bit routines
'     Stefano Bonomi:   2 bit routines
'     Evan Venn:        Revised 4 bit routines, K107  and 404 Support
'     William Roth:     Revised to improve performance and improved functionality
'     Theo Loermans:    LCD_IO 1, LCD_IO 2m LCD_IO 2_74XX174 and LCD_IO 2_74XX164 for 1 and 2-wire modes
'   ***********************************************************************
'
' Supports
' Set LCD_10 to 10 for the YwRobot LCD1602 IIC V1 or the Sainsmart LCD_PIC I2C adapter
' Set LCD_10 to 12 for the Ywmjkdz I2C adapter with pot bent over top of chip

'   #define LCD_I2C_Address_1 0x4e
'   #define LCD_I2C_Address_2 0x4c
'   #define LCD_I2C_Address_3 0x4a
'   #define LCD_I2C_Address_4 0x49
'
'   Use LCD_I2C_Address_Current = LCD_I2C_Address to change the LCD output routines to a specific device.
'    LCD_I2C_Address_Current = LCD_I2C_Address
'    Print "LCD Address is now": LCDHex(  LCD_I2C_Address_Current, 2)

'  #define LCD_IO 3
'
'     'Change as necessary
'      #define LCD_DB     PORTb.3            ; databit
'      #define LCD_CB     PORTb.4            ; clockbit
'      #define LCD_EB     PORTa.0            ; enable bit
'
'
'***********************************************************************
' 06/04/2020   Added K107 Capabilities
' 04/05/2020   Added Support for HWI2C2
' 08/05/2020   Increase init delays to support VFDs, added LCD_VFD_DELAY
' 09/05/2020   Added LCD_VARIANT = 1601a
' 26/09/2020   Revised InitLCD(LCD_IO 4 and LCD_IO 8 only)
'***********************************************************************
' 14/08/22 Updated user changeable constants only - no functional change


#startup InitLCD

'Version
#define LCD_Version 26092020

'Compatibility with older programs
#define LCDInt Print
#define LCDWord Print

#define K107  107

'User changeable constants to control LCD


  'LCD CONFIGURATIO 1,2, 4, 8 OR 10, 12
  #IFNDEF LCD_IO
        #DEFINE LCD_IO 4
  #ENDIF

  'REDIRECT METHODS USED TO WRITE TO LCD. CAN BE ALTERED TO ALLOW CUSTOM LCD INTERFACES.
  #IFNDEF LCDWRITEBYTE
        #DEFINE LCDWRITEBYTE LCDNORMALWRITEBYTE
  #ENDIF
  #IFNDEF LCDREADBYTE
        #DEFINE LCDREADBYTE LCDNORMALREADBYTE
  #ENDIF

  'DEFAULT OPTIONS FOR LCDHEX
  #IFNDEF LEADINGZEROACTIVE
        #DEFINE LEADINGZEROACTIVE 2
  #ENDIF

  'REQUIRED FOR LCDBACKLIGHT, LED-ELECTRONIC NEEDS A ONE OR A ZERO TO BE ACTIVE
  'ADAPT, BY SWAPPING 1 AND 0, IF NEEDED.
  #IFNDEF LCD_BACKLIGHT_ON_STATE
        #DEFINE LCD_BACKLIGHT_ON_STATE  1
  #ENDIF
  #IFNDEF LCD_BACKLIGHT_OFF_STATE
        #DEFINE LCD_BACKLIGHT_OFF_STATE 0
  #ENDIF

  'DEFAULT ADDRESS FOR LCD_IO 10,12
  #IFNDEF LCD_I2C_ADDRESS_1
        #DEFINE LCD_I2C_ADDRESS_1 0X4E
  #ENDIF

  'OUTPUT BIT TO INTERFACE WITH LCD DATA BUS. USED IN 4-BIT MODE
  #IFNDEF LCD_DB4
        #DEFINE LCD_DB4 SYSLCDTEMP.0
  #ENDIF

  'OUTPUT BIT TO INTERFACE WITH LCD DATA BUS. USED IN 4-BIT MODE
  #IFNDEF LCD_DB5
        #DEFINE LCD_DB5 SYSLCDTEMP.0
  #ENDIF

  'OUTPUT BIT TO INTERFACE WITH LCD DATA BUS. USED IN 4-BIT MODE
  #IFNDEF LCD_DB6
        #DEFINE LCD_DB6 SYSLCDTEMP.0
  #ENDIF

  'OUTPUT BIT TO INTERFACE WITH LCD DATA BUS. USED IN 4-BIT MODE
  #IFNDEF LCD_DB7
        #DEFINE LCD_DB7 SYSLCDTEMP.0
  #ENDIF

  'OUTPUT BIT TO CONTROL LCD REGISTER SELECT. USED AS VARIABLE BY 2-BIT MODE, AND AS A PIN IN 4 AND 8 BIT MODE. DO NOT CHANGE FOR 2-BIT MODE.
  #IFNDEF LCD_RS
        #DEFINE LCD_RS SYSLCDTEMP.1
  #ENDIF

  'OUTPUT BIT TO SELECT READ/WRITE
  #IFNDEF LCD_RW
        #DEFINE LCD_RW SYSLCDTEMP.0
  #ENDIF

  'OUTPUT BIT TO ENABLE/DISABLE LCD
  #IFNDEF LCD_ENABLE
        #DEFINE LCD_ENABLE SYSLCDTEMP.0
  #ENDIF

  'COMBINED CLOCK & DATA BIT. USED IN 1-WIRE MODE
  #IFNDEF LCD_CD
        #DEFINE LCD_CD SYSLCDTEMP.0
  #ENDIF

  'DATA BIT. USED IN 2-BIT MODE
  #IFNDEF LCD_DB
        #DEFINE LCD_DB SYSLCDTEMP.0
  #ENDIF

  'CLOCK BIT. USED IN 2-BIT MODE
  #IFNDEF LCD_CB
        #DEFINE LCD_CB SYSLCDTEMP.0
  #ENDIF

  'PORT TO CONNECT TO LCD DATA BUS. USED IN 8-BIT MODE
  #IFNDEF LCD_DATA_PORT
        #DEFINE LCD_DATA_PORT SYSLCDTEMP
  #ENDIF

  'TEMPORARY BIT TO HOLD LCD_RS STATE
  #IFNDEF LCD_RSTEMP
        #DEFINE LCD_RSTEMP SYSLCDTEMP.2
  #ENDIF

  'DEFAULT LCD WIDTH SETTINGS
  #IFNDEF LCD_WIDTH
        #DEFINE LCD_WIDTH 20
  #ENDIF

  'DEFAULT DELAY
  #IFNDEF LCD_WRITE_DELAY
        #DEFINE LCD_WRITE_DELAY 2 US
  #ENDIF



Dim SysLCDTemp as Byte

'Defaults for LCDCURSOR Sub
#define DisplayON 12
#define LCDON 12

#define FLASHON 9
#define FLASH 9
#define CursorON 10

#define FLASHOFF 14
#define CursorOFF 13

#define DisplayOFF 11
#define LCDOFF 11

'Default lines for bar graph
#define LCD_BAR_EMPTY b'00010001'
#define LCD_BAR_FULL 255





'All scripts consolidatd here
#SCRIPT

    'LCD configuratio 1,2, 4, 8 OR 10, 12
    If NoDef(LCD_IO) Then
        LCD_IO = 4
    End if

    'Output bit to interface with LCD data bus. Used in 4-bit mode
    If NoDef(LCD_DB4) Then
        LCD_DB4 = SysLCDTemp.0
    End If
    If NoDef(LCD_DB5) Then
        LCD_DB5 = SysLCDTemp.0
    End If
    If NoDef(LCD_DB6) Then
        LCD_DB6 = SysLCDTemp.0
    End If
    If NoDef(LCD_DB7) Then
        LCD_DB7 = SysLCDTemp.0
    End If

    'Output bit to control LCD Register Select. Used as variable by 2-bit mode, and as a pin in 4 and 8 bit mode. DO NOT CHANGE FOR 2-BIT MODE.
    If NoDef(LCD_RS) Then
        LCD_RS = SysLCDTemp.1
    End If
    If NoDef(LCD_RW) Then
        LCD_RW = SysLCDTemp.0
    End If
    If NoDef(LCD_Enable) Then
        LCD_Enable = SysLCDTemp.0
    End If
    'Default Speed
    If NoDef(LCD_SPEED) Then
        LCD_SPEED = SLOW
    End If



    OPTIMAL = 0     ;use with LCD_IO 4,8 with LCD_NO_RW *NOT* defined  for optimal Speed.
    FAST = 50       'equates to 50us period or 20,000 CPS
    MEDIUM = 66     'equates to 60us period or 15,000 CPS
    SLOW = 100      'equates to 100 us period or 10,000 CPS


    IF LCD_IO = 1 THEN
            'Delay in LCD2 Nibble OUT
    END IF

    IF LCD_IO = 2 THEN
          ' No Post write delay
    END IF

    IF LCD_IO = 2_74XX174 THEN
         ' No Post  write delay
    END IF

    IF LCD_IO = 2_74XX164 THEN
         ' No post write delay
    END IF

    IF LCD_IO = 3 THEN
        ' no post write delay
        LCD_RS = LCD_DB
    END IF

    IF LCD_IO = 4 THEN

        __LCD_PERIOD = LCD_SPEED
        __LCD_DELAYS = 5
        __LCD_INSTRUCTIONS = 74

        IF LCD_NO_RW THEN  'LCD_NO_RW Defined by User
            OPTIMAL = FAST
            __LCD_PERIOD = LCD_SPEED
            __LCD_DELAYS = 4
           __LCD_INSTRUCTIONS = 56
        END IF

        IF PIC THEN
            __LCD_CALC  = INT(__LCD_PERIOD - __LCD_DELAYS - (4/ChipMHz * __LCD_INSTRUCTIONS))
        END IF

        IF AVR THEN
            __LCD_CALC  = INT(__LCD_PERIOD - __LCD_DELAYS - (2/ChipMHz * __LCD_INSTRUCTIONS))
        END IF

        SCRIPT_LCD_POSTWRITEDELAY = __LCD_CALC us

        SCRIPT_LCD_BF = LCD_DB7

    END IF

    IF LCD_IO = 8 THEN

        __LCD_PERIOD = LCD_SPEED
        __LCD_DELAYS = 3
        __LCD_INSTRUCTIONS = 44

        IF LCD_NO_RW THEN
            OPTIMAL = FAST
            __LCD_PERIOD = LCD_SPEED
            __LCD_DELAYS = 2
            __LCD_INSTRUCTIONS = 28
        END IF

        IF PIC THEN
            __LCD_CALC  = INT(__LCD_PERIOD - __LCD_DELAYS - (4/ChipMHz * __LCD_INSTRUCTIONS))
        END IF

        IF AVR THEN
             __LCD_CALC  = INT(__LCD_PERIOD - __LCD_DELAYS - (2/ChipMHz * __LCD_INSTRUCTIONS))
          END IF

        SCRIPT_LCD_POSTWRITEDELAY = __LCD_CALC us
        SCRIPT_LCD_BF = LCD_DATA_PORT.7  'Define Busy Flag Pin for LCD_IO 8

     END IF


    'YwRobot LCD1602 IIC V1
    IF LCD_IO = 10 THEN

        SLOW = 40
        MEDIUM = 20
        FAST = 10
        OPTIMAL = 10

        SCRIPT_LCD_POSTWRITEDELAY = LCD_SPEED us

        i2c_lcd_e  = i2c_lcd_byte.2
        i2c_lcd_rw = i2c_lcd_byte.1
        i2c_lcd_rs = i2c_lcd_byte.0
        i2c_lcd_bl = i2c_lcd_byte.3

        i2c_lcd_d4 = i2c_lcd_byte.4
        i2c_lcd_d5 = i2c_lcd_byte.5
        i2c_lcd_d6 = i2c_lcd_byte.6
        i2c_lcd_d7 = i2c_lcd_byte.7
    END IF

    'Definition for mjkdz I2C adapter with pot bent over top of chip
    IF LCD_IO = 12 then

        SLOW = 40
        MEDIUM = 20
        FAST = 10
        OPTIMAL = 10

        SCRIPT_LCD_POSTWRITEDELAY = LCD_SPEED us

        i2c_lcd_e  = i2c_lcd_byte.4
        i2c_lcd_rw = i2c_lcd_byte.5
        i2c_lcd_rs = i2c_lcd_byte.6
        i2c_lcd_bl = i2c_lcd_byte.7

        i2c_lcd_d4 = i2c_lcd_byte.0
        i2c_lcd_d5 = i2c_lcd_byte.1
        i2c_lcd_d6 = i2c_lcd_byte.2
        i2c_lcd_d7 = i2c_lcd_byte.3
    END IF

    IF LCD_IO = 404 THEN

        OPTIMAL = 39
        FAST = 39
        MEDIUM = 55
        SLOW = 89

        SCRIPT_LCD_POSTWRITEDELAY = LCD_SPEED

        PUT = PUT404
        LOCATE = LOCATE404
        CLS = CLS404
        LCDHOME = LCDHOME404
        LCDcmd = LCDcmd404
        Print = Print404
        LCDHex = LCDHex404
        LCDCursor = LCDCursor404
        LCDCreateChar = LCDCreateChar404
        LCDCreateGraph = LCDCreateGraph404
        LCDSpace = LCDSpace404
        LCDDisplaysOff = LCDDisplaysOff404
        LCDDisplaysOn = LCDDisplaysOn404

    END IF

    IF LCD_IO = K107 THEN

'        PUT = PUT404
        LOCATE = K107LOCATE
        CLS = K107CLS
        LCDHOME = K107LCDHOME
        LCDcmd = K107LCDcmd
        Print = K107Print
        LCDHex = K107LCDHex
        LCDCursor = K107LCDCursor
        LCDCreateChar = K107LCDCreateChar
        LCDCreateGraph = K107LCDCreateGraph
        LCDSpace = K107LCDSpace
        LCDDisplaysOff = K107LCDDisplaysOff
        LCDDisplaysOn = K107LCDDisplaysOn
        LCDBACKLIGHT =  K107LCDBACKLIGHT
        LCDSpace = K107LCDSpace

    END IF

    IF LCD_VARIANT = 1601a THEN

        PRINT  =  V1601aPRINT
        LOCATE =  V1601aLOCATE
        CLS =     V1601aCLS
        LCDHOME = V1601aLCDHOME

    END IF


#ENDSCRIPT

Sub PUT (In LCDPutLine, In LCDPutColumn, In LCDChar)
'Sub to put a character at the specified location
    LOCATE LCDPutLine, LCDPutColumn
    Set LCD_RS on
    LCDWriteByte(LCDChar)
End Sub

Function GET (LCDPutLine, LCDPutColumn)
'Sub to get the value of the current LCD GCGRAM
'GET only supported in 8 and 4 bit modes

    Locate LCDPutLine, LCDPutColumn
    Set LCD_RS on
    GET = LCDReadByte
End Function

Sub LOCATE (In LCDLine, In LCDColumn)
'Sub to locate the cursor
'Where LCDColumn is 0 to screen width-1, LCDLine is 0 to screen height-1

    Set LCD_RS Off
    If LCDLine > 1 Then
        LCDLine = LCDLine - 2
        LCDColumn = LCDColumn + LCD_WIDTH
    End If

    LCDWriteByte(0x80 or 0x40 * LCDLine + LCDColumn)
    wait 5 10us
End Sub

Sub CLS
'Sub to clear the LCD
    SET LCD_RS OFF

    'Clear screen
    LCDWriteByte (0b00000001)
    Wait 4 ms

    'Move to start of visible DDRAM
    LCDWriteByte(0x80)
    Wait 50 us

End Sub

Sub LCDHOME
'Sub to set the cursor to the home position
    SET LCD_RS OFF
    'Return CURSOR to home
    LCDWriteByte (0b00000010)
    Wait 2 ms 'Must be > 1.52 ms
End Sub

Sub LCDcmd ( In LCDValue )
'Sub to send specified command direct to the LCD
    SET LCD_RS OFF

    LCDWriteByte ( LCDValue)

    IF LCDValue = 1 OR LCDValue = 2 then  ' HOME or CLEAR
       Wait 2 ms ' Must be > 1.52 ms
    Else
       Wait 50 us
    END IF

End sub

Sub LCD3_CMD(In LCDValue as Byte)
    LCD_DB = 0  'really maps to the LCD_RS
    LCDWriteByte(LCDValue)
end sub

sub LCD3_DATA(In LCDValue as byte)
    LCD3_DB = 1  'really maps to the LCD_RS
    LCDWriteByte(LCDValue)
end sub

' Used in 1-wire mode; a "zero" bit is 10us low and minimal 20 us High
Sub Zerobit
    SET LCD_CD OFF
    wait 10 us ' bit time
    SET LCD_CD ON
    wait 20 us ' recovery RC time
    wait 1 ms
end sub

' Used in 1-wire mode; a "one" bit is 1us low and minimal 5 us High
Sub Onebit
    SET LCD_CD OFF
    wait 1 us ' bit time
    SET LCD_CD ON
    wait 5 us ' recovery RC time
    wait 1 ms
end sub

' Used in 1-wire mode; reset is 350 us low and minimal 300 us high
Sub ResetShiftReg
    SET LCD_CD OFF
    wait 350 us
    SET LCD_CD ON
    wait 300 us
    wait 1 ms
end sub

sub InitLCD

    asm showdebug  `LCD_IO selected is ` LCD_IO

    #IFDEF LCD_Speed FAST
        asm showdebug  `LCD_Speed is FAST`
    #ENDIF
    #IFDEF LCD_Speed MEDIUM
        asm showdebug  `LCD_Speed is MEDIUM`
    #ENDIF
    #IFDEF LCD_Speed SLOW
        asm showdebug  `LCD_Speed is SLOW`
    #ENDIF

    asm showdebug  `OPTIMAL is set to ` OPTIMAL
    asm showdebug  `LCD_Speed is set to ` LCD_Speed


    #IFDEF LCD_IO 4,8,10,12
        #IFDEF LCD_backlight
            Dir LCD_Backlight OUT
            Set LCD_Backlight OFF
        #ENDIF
    #ENDIF

    #IFDEF LCD_IO 1
        ' 1-wire mode with shiftreg 74HC595
        Set LCD_CD ON
        Dir LCD_CD OUT
        LCDBacklight Off  ' Prevents compiler error if LCDbacklight is not used user code
        wait 10 MS
        Set LCD_RS OFF
        ResetShiftReg
        LCD2_NIBBLEOUT 0X03
        Wait 5 ms
        LCD2_NIBBLEOUT 0X03
        WAIT 1 MS
        LCD2_NIBBLEOUT 0X03
        WAIT 1 MS
        LCD2_NIBBLEOUT 0X02
        WAIT 1 MS
        LCDWriteByte 0x28
        WAIT 1 MS
        LCDWriteByte 0x08
        WAIT 1 MS
        LCDWriteByte 0x01
        WAIT 1 MS
        LCDWriteByte 0x06
        WAIT 1 MS
        LCDWriteByte 0x0C
        WAIT 1 MS
    #ENDIF

    #IFDEF LCD_IO 2, 2_74XX174, 2_74XX164
        'All 2-wire modes
        Set LCD_DB OFF
        Set LCD_CB OFF
        Dir LCD_DB OUT
        Dir LCD_CB OUT

        LCDBacklight Off  'Prevents compiler error if LCDbacklight is not used user code

        WAIT 35 MS
        Set LCD_RS OFF
        LCD2_NIBBLEOUT 0X03
        Wait 5 ms
        LCD2_NIBBLEOUT 0X03
        WAIT 1 MS
        LCD2_NIBBLEOUT 0X03
        WAIT 1 MS
        LCD2_NIBBLEOUT 0X02
        WAIT 1 MS
        LCDWriteByte 0x28
        WAIT 1 MS
        LCDWriteByte 0x08
        WAIT 1 MS
        LCDWriteByte 0x01
        WAIT 5 MS
        LCDWriteByte 0x06
        WAIT 1 MS
        LCDWriteByte 0x0C
        WAIT 1 MS

    #ENDIF

    #IFDEF LCD_IO 3
        'LCD_IO_3
        LCDBacklight Off  'Prevents compiler error if LCDbacklight is not used user code

        Dir LCD_EB out
        Dir LCD_RS out
        Dir LCD_CB out

        Set LCD_EB OFF
        Set LCD_RS OFF
        Set LCD_CB OFF

        wait 20 ms

        Set LCD_EB ON

        LCD3_CMD(0x30)
        wait 5 ms

        LCD3_CMD(0x30)
        wait 1 ms

        LCD3_CMD(0x38)
        LCD3_CMD(0x08)
        LCD3_CMD(0x0F)
        LCD3_CMD(0x01)
        LCD3_CMD(0x38)
        LCD3_CMD(0x80)

    #ENDIF


    #IFDEF LCD_IO 4

        Wait 50 ms

        #IFNDEF LCD_NO_RW
          Dir LCD_RW OUT
          Set LCD_RW OFF
        #ENDIF
        Dir LCD_DB4 OUT
        Dir LCD_DB5 OUT
        Dir LCD_DB6 OUT
        Dir LCD_DB7 OUT
        Dir LCD_RS OUT
        Dir LCD_Enable OUT
        Set LCD_RS OFF
        Set LCD_Enable OFF

        'Wakeup (0x30 - b'0011xxxx' )
        Set LCD_DB7 OFF
        Set LCD_DB6 OFF
        Set LCD_DB5 ON
        Set LCD_DB4 ON
        Wait 2 us
        PulseOut LCD_Enable, 2 us
        Wait 10 ms
        Repeat 3
            PulseOut LCD_Enable, 2 us
            Wait 1 ms
        End Repeat

        'Set 4 bit mode (0x20 - b'0010xxxx')
        Set LCD_DB7 OFF
        Set LCD_DB6 OFF
        Set LCD_DB5 ON
        Set LCD_DB4 OFF
        Wait 2 us
        PulseOut LCD_Enable, 2 us
        Wait 100 us
       '===== now in 4 bit mode =====

        LCDWriteByte 0x28    '(b'00101000')  '0x28 set 2 line mode
        LCDWriteByte 0x06    '(b'00000110')  'Set cursor movement
        LCDWriteByte 0x0C    '(b'00001100')  'Turn off cursor
        Cls  'Clear the display

    #ENDIF

    #IFDEF LCD_IO 404

        wait 20 ms

        'Configure RS,Enable & RW pin directions for 404
        Dir LCD_RS OUT
        Dir LCD_Enable1 OUT
        Dir LCD_Enable2 OUT
        'Set datapins to output
        Dir LCD_DB4 OUT
        Dir LCD_DB5 OUT
        Dir LCD_DB6 OUT
        Dir LCD_DB7 OUT

        Set LCD_RS OFF

        #IFNDEF LCD_NO_RW
             Dir LCD_RW OUT
             Set LCD_RW OFF
        #ENDIF

        Wait 20 ms

        'Wakeup 0x30
        Set LCD_DB4 ON
        Set LCD_DB5 ON
        Set LCD_DB6 OFF
        Set LCD_DB7 OFF

        'Device 1
        Wait 2 us
        PulseOut LCD_Enable1, 2 us
        Wait 5 ms
        Repeat 3   'three more times
            PulseOut LCD_Enable1, 2 us
            Wait 5 ms
        End Repeat
        Wait 5 ms

       'Set 4 bit mode    (0x20)
        Set LCD_DB4 OFF
        Set LCD_DB5 ON
        Set LCD_DB6 OFF
        Set LCD_DB7 OFF
        Wait 2 us
        PulseOut LCD_Enable1, 2 us
        Wait 5 ms
        '===== now in 4 bit mode =====

        gLCDEnableAddress = 1
        gLCDXYPosition = 0

        LCDWriteByte 0x28    '(b'00101000')  '0x28 set 2 line mode
        LCDWriteByte 0x06    '(b'00000110')  'Set cursor movement
        LCDWriteByte 0x0C    '(b'00001100')  'Turn off cursor
        Cls  'Clear the display

        'Device 2
        Wait 5 ms
        PulseOut LCD_Enable2, 2 us
        Wait 5 ms
        Repeat 3   'three more times
            PulseOut LCD_Enable2, 2 us
            Wait 5 ms
        End Repeat
        Wait 5 ms

       'Set 4 bit mode  (0x20)
        Set LCD_DB4 OFF
        Set LCD_DB5 ON
        Set LCD_DB6 OFF
        Set LCD_DB7 OFF

        Wait 2 us
        PulseOut LCD_Enable2, 2 us
        Wait 5 ms
        '===== now in 4 bit mode =====

        LCDWriteByte 0x28    '(b'00101000')  '0x28 set 2 line mode
        LCDWriteByte 0x06    '(b'00000110')  'Set cursor movement
        LCDWriteByte 0x0C    '(b'00001100')  'Turn off cursor
        Cls  'Clear the display

    #ENDIF

    #IFDEF LCD_IO 8

        Wait 50 ms

        #IFNDEF LCD_NO_RW
          Dir LCD_RW OUT
          Set LCD_RW OFF
        #ENDIF
        Dir LCD_RS OUT
        Dir LCD_Enable OUT
        Dir LCD_DATA_PORT OUT
        Set LCD_RS OFF

        'Wakeup (0x30)
        LCD_DATA_PORT = 0x30
        Wait 2 us
        PulseOut LCD_Enable, 2 us
        Wait 10 ms
        Repeat 3
          PulseOut LCD_Enable, 2 us
          Wait 1 ms
        End Repeat

        LCDWriteByte 0x38    '(b'00111000')  '0x38  set 2 line mode
        LCDWriteByte 0x06    '(b'00000110')  'Set cursor movement
        LCDWriteByte 0x0C    '(b'00001100')  'Turn off cursor
        Cls  'Clear the display

    #ENDIF


    #IFDEF LCD_IO 10, 12

        #ifdef I2C_DATA
          InitI2C       ;call to init i2c is required here!
        #endif

        #ifdef HI2C_DATA
          HI2CMode Master    ;call to Master required to init I2C Baud Rate here!
        #endif

        #ifdef HI2C2_DATA
          HI2C2Mode Master    ;call to Master required to init I2C Baud Rate here!
        #endif


        LCD_Backlight = LCD_Backlight_On_State
        wait 2 ms

        repeat 2  ; called to ensure reset is complete.  Needed for cheap LCDs!!

            #ifdef LCD_I2C_Address_1
                   LCD_I2C_Address_Current = LCD_I2C_Address_1
                   initI2CLCD
            #endif

            #ifdef LCD_I2C_Address_2
                   LCD_I2C_Address_Current = LCD_I2C_Address_2
                   initI2CLCD
            #endif

            #ifdef LCD_I2C_Address_3
                   LCD_I2C_Address_Current = LCD_I2C_Address_3
                   initI2CLCD
            #endif

            #ifdef LCD_I2C_Address_4
                   LCD_I2C_Address_Current = LCD_I2C_Address_4
                   initI2CLCD
            #endif
        end repeat
    #ENDIF

    #IFDEF LCD_IO 107

        CLS

    #ENDIF

    LCD_State = 12

end sub

sub InitI2CLCD
    wait 15 ms
    Dim i2c_lcd_byte
    i2c_lcd_byte = 0
    LCDWriteByte 0x03: wait 5 ms
    LCDWriteByte 0x03: wait 1 ms
    LCDWriteByte 0x03: wait 1 ms
    LCDWriteByte 0x03: wait 1 ms
    LCDWriteByte 0x02: wait 1 ms
    LCDWriteByte 0x28: wait 1 ms
    LCDWriteByte 0x0c: wait 1 ms
    LCDWriteByte 0x01: wait 15 ms
    LCDWriteByte 0x06: wait 1 ms
    CLS
end sub

sub Print (In PrintData As String)
'Sub to print a string variable on the LCD

    PrintLen = PrintData(0)

    If PrintLen = 0 Then Exit Sub
    Set LCD_RS On

    'Write Data
    For SysPrintTemp = 1 To PrintLen
        LCDWriteByte PrintData(SysPrintTemp)
    Next

End Sub

Sub Print (In LCDValue)
'Sub to print a byte variable on the LCD

    LCDValueTemp = 0
    Set LCD_RS On

    IF LCDValue >= 100 Then
        LCDValueTemp = LCDValue / 100
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
    End If

    If LCDValueTemp > 0 Or LCDValue >= 10 Then
        LCDValueTemp = LCDValue / 10
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
    End If
    LCDWriteByte (LCDValue + 48)
End Sub

Sub Print (In LCDValue As Word)
'Sub to print a word variable on the LCD

    Dim SysCalcTempX As Word

    Set LCD_RS On
    LCDValueTemp = 0

    If LCDValue >= 10000 then
        LCDValueTemp = LCDValue / 10000 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        Goto LCDPrintWord1000
    End If

    If LCDValue >= 1000 then
        LCDPrintWord1000:
        LCDValueTemp = LCDValue / 1000 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        Goto LCDPrintWord100
    End If

    If LCDValue >= 100 then
        LCDPrintWord100:
        LCDValueTemp = LCDValue / 100 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        Goto LCDPrintWord10
    End If

    If LCDValue >= 10 then
        LCDPrintWord10:
        LCDValueTemp = LCDValue / 10 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
    End If

    LCDWriteByte (LCDValue + 48)
End Sub

Sub Print (In LCDValueInt As Integer)
'Sub to print an integer variable on the LCD

    Dim LCDValue As Word

    'If sign bit is on, print - sign and then negate
    If LCDValueInt.15 = On Then
              LCDWriteChar("-")
              LCDValue = -LCDValueInt

    'Sign bit off, so just copy value
    Else
              LCDValue = LCDValueInt
    End If

    'Use Print(word) to display value
    Print LCDValue
End Sub

Sub Print (In LCDValue As Long)
'Sub to print a long variable on the LCD

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
    Set LCD_RS On
    For SysPrintTemp = SysPrintBuffLen To 1 Step -1
              LCDWriteByte(SysPrintBuffer(SysPrintTemp) + 48)
    Next

End Sub


sub LCDHex  (In LCDValue, optional in LCDChar = 1)
'Sub to print a hex string from the specified byte variable on the LCD

    'Extract nibbles and convert to ascii values
    HighChar = (LCDValue / 16)  + 48
    LowChar  = (LCDValue and 15) + 48

    'Check for alpha and convert.
    If HighChar > 57 Then HighChar = HighChar + 7
    If LowChar > 57 Then  LowChar = LowChar + 7

    Set LCD_RS OFF

    'Write chars to LCD - if user specifies LeadingZeroActive then print the leading char
    IF LCDChar = LeadingZeroActive then
        if LCDValue < 16 then
           LCDWriteChar 48
        end if
    END IF

    'Write high char if LCDValue is > 15 (DEC)
    IF LCDValue > 15 then LCDWriteChar HighChar

    LCDWriteChar LowChar

end sub

sub LCDWriteChar(In LCDChar)
'Sub to print character on the LCD

     set LCD_RS on
     LCDWriteByte(LCDChar)
     wait 5 10us
end sub

function LCDReady

    #IFDEF LCD_NO_RW
        Wait 10 ms
        LCDReady = TRUE
        exit function
    #ENDIF

    #IFNDEF LCD_NO_RW
        #IFDEF LCD_IO 1, 2, 2_74XX174, 2_74XX164  ' All 1 and 2-wire modes
            LCDReady = TRUE
            exit function
        #ENDIF
    #ENDIF

end function

Sub CheckBusyFlag
'Sub that waits until LCD controller busy flag goes low (ready)
'Only used by LCD_IO 4,8 and only when LCD_NO_RW is NOT Defined
'Called by sub LCDNOrmalWriteByte
    #IFDEF LCD_IO 4,8

        #IFNDEF LCD_NO_RW

             LCD_RSTemp = LCD_RS
             DIR SCRIPT_LCD_BF  IN
             SET LCD_RS OFF
             SET LCD_RW ON

            #IFDEF LCD_IO 4

                Do
                    Set LCD_Enable ON
                    wait 1 us
                    SysLCDTemp.7 = SCRIPT_LCD_BF
                    Set LCD_Enable OFF
                    Wait 1 us
                    PulseOut LCD_Enable, 1 us
                    Wait 1 us

                Loop While SysLCDTemp.7 <> 0

            #ENDIF

            #IFDEF LCD_IO 8

                Do
                   Wait 1 us
                   Set LCD_Enable ON
                   Wait 1 us
                   SysLCDTemp.7 = SCRIPT_LCD_BF
                   Set LCD_Enable OFF


                Loop While SysLCDTemp.7 <> 0

            #ENDIF

            LCD_RS = LCD_RSTemp

        #ENDIF

    #ENDIF

End Sub


sub LCDNormalWriteByte(In LCDByte )
'Sub to write a byte to the LCD

 #IFNDEF LCD_NO_RW
        #IFDEF LCD_IO 4,8
             CheckBusyFlag         'WaitForReady
             set LCD_RW OFF
        #ENDIF
    #ENDIF

    #IFDEF LCD_IO 3

        LCD3_RSState = LCD_RS
        LCD_EB = 1

        LCD_CB = 0
        REPEAT 8
          LCD_RS = LCDByte.7
          ROTATE LCDByte LEFT
          'Clock it Out
          LCD_CB = 1
          LCD_CB = 0
        END REPEAT
        wait 3 ms
        LCD_RS = LCD3_RSState  'passed from call.
        wait 3 ms
        LCD_EB = 0
        wait 3 ms
        LCD_EB = 1

    #ENDIF


    #IFDEF LCD_IO 1, 2, 2_74XX174, 2_74XX164
        '1 and 2-wire modes

        ' Swap byte; Most significant NIBBLE sent first
        LCD2_NIBBLEOUT Swap4(LCDByte)

        ' Less Significant NIBBLE output
        LCD2_NIBBLEOUT LCDByte

    #ENDIF

    #IFDEF LCD_IO 4

       'Dim Temp as Byte
        'Pins must be outputs if returning from WaitForReady, or after LCDReadByte or GET subs
        DIR LCD_DB4 OUT
        DIR LCD_DB5 OUT
        DIR LCD_DB6 OUT
        DIR LCD_DB7 OUT

       'Write upper nibble to output pins
'        set LCD_DB4 OFF
'        set LCD_DB5 OFF
'        set LCD_DB6 OFF
'        set LCD_DB7 OFF
'        if LCDByte.7 ON THEN SET LCD_DB7 ON
'        if LCDByte.6 ON THEN SET LCD_DB6 ON
'        if LCDByte.5 ON THEN SET LCD_DB5 ON
'        if LCDByte.4 ON THEN SET LCD_DB4 ON
        LCD_DB7 = LCDByte.7
        LCD_DB6 = LCDByte.6
        LCD_DB5 = LCDByte.5
        LCD_DB4 = LCDByte.4


        #IFDEF LCD_VFD_DELAY
            Wait LCD_VFD_DELAY
        #ENDIF

        Wait 1 us
        PulseOut LCD_enable, 1 us

        'All data pins low
'        set LCD_DB4 OFF
'        set LCD_DB5 OFF
'        set LCD_DB6 OFF
        set LCD_DB7 OFF
'
'       'Write lower nibble to output pins
        if LCDByte.3 ON THEN SET LCD_DB7 ON
'        if LCDByte.2 ON THEN SET LCD_DB6 ON
'        if LCDByte.1 ON THEN SET LCD_DB5 ON
'        if LCDByte.0 ON THEN SET LCD_DB4 ON
'        LCD_DB7 = LCDByte.3
        LCD_DB6 = LCDByte.2
        LCD_DB5 = LCDByte.1
        LCD_DB4 = LCDByte.0


        Wait 1 us
        PulseOut LCD_enable, 1 us

       'Set data pins low again
        'SET LCD_DB7 OFF
        'SET LCD_DB6 OFF
        'SET LCD_DB5 OFF
        'SET LCD_DB4 OFF

        Wait SCRIPT_LCD_POSTWRITEDELAY

    #ENDIF

    #IFDEF LCD_IO 8

       'Set data port to output
        DIR LCD_DATA_PORT out

       'write the data
        LCD_DATA_PORT = LCDByte
        Wait 1 us

        PulseOut LCD_enable, 1 us

        LCD_DATA_PORT = 0

        Wait SCRIPT_LCD_POSTWRITEDELAY

    #ENDIF

    #IFDEF LCD_IO 10, 12

        Dim i2c_lcd_byte as Byte

        #ifdef I2C_DATA
            IF LCD_RS = 1 then
               i2c_lcd_rs=1;
            ELSE
               i2c_lcd_rs=0;
            end if

            i2c_lcd_rw  = 0
            i2c_lcd_bl  = LCD_Backlight.0
            I2CReStart
            I2CSend LCD_I2C_Address_Current

            ''' Send upper nibble
            i2c_lcd_d7 = LCDByte.7
            i2c_lcd_d6 = LCDByte.6
            i2c_lcd_d5 = LCDByte.5
            i2c_lcd_d4 = LCDByte.4
            i2c_lcd_e = 1;
            I2CSend i2c_lcd_byte
            i2c_lcd_e = 0;
            I2CSend i2c_lcd_byte

            ''' Send lower nibble
            i2c_lcd_d7 = LCDByte.3
            i2c_lcd_d6 = LCDByte.2
            i2c_lcd_d5 = LCDByte.1
            i2c_lcd_d4 = LCDByte.0
            i2c_lcd_e = 1;
            I2CSend i2c_lcd_byte
            i2c_lcd_e = 0;
            I2CSend i2c_lcd_byte
            I2CStop
            LCD_State = 12
         #ENDIF

        #ifdef HI2C_DATA
            IF LCD_RS = 1 then
               i2c_lcd_rs=1;
            ELSE
               i2c_lcd_rs=0;
            end if

            i2c_lcd_rw  = 0;
            i2c_lcd_bl  = LCD_Backlight.0;

            HI2CStart                        ;generate a start signal
            HI2CSend LCD_I2C_Address_Current   ;indicate a write

            ''' Send upper nibble
            i2c_lcd_d7 = LCDByte.7
            i2c_lcd_d6 = LCDByte.6
            i2c_lcd_d5 = LCDByte.5
            i2c_lcd_d4 = LCDByte.4
            i2c_lcd_e = 1;
            HI2CSend i2c_lcd_byte
            i2c_lcd_e = 0;
            HI2CSend i2c_lcd_byte

            ''' Send lower nibble
            i2c_lcd_d7 = LCDByte.3
            i2c_lcd_d6 = LCDByte.2
            i2c_lcd_d5 = LCDByte.1
            i2c_lcd_d4 = LCDByte.0
            i2c_lcd_e = 1;
            HI2CSend i2c_lcd_byte
            i2c_lcd_e = 0;
            HI2CSend i2c_lcd_byte
            HI2CStop
            LCD_State = 12

         #ENDIF

         #ifdef HI2C2_DATA
            IF LCD_RS = 1 then
               i2c_lcd_rs=1;
            ELSE
               i2c_lcd_rs=0;
            end if

            i2c_lcd_rw  = 0;
            i2c_lcd_bl  = LCD_Backlight.0;

            HI2C2Start                        ;generate a start signal
            HI2C2Send LCD_I2C_Address_Current   ;indicate a write

            ''' Send upper nibble
            i2c_lcd_d7 = LCDByte.7
            i2c_lcd_d6 = LCDByte.6
            i2c_lcd_d5 = LCDByte.5
            i2c_lcd_d4 = LCDByte.4
            i2c_lcd_e = 1;
            HI2C2Send i2c_lcd_byte
            i2c_lcd_e = 0;
            HI2C2Send i2c_lcd_byte

            ''' Send lower nibble
            i2c_lcd_d7 = LCDByte.3
            i2c_lcd_d6 = LCDByte.2
            i2c_lcd_d5 = LCDByte.1
            i2c_lcd_d4 = LCDByte.0
            i2c_lcd_e = 1;
            HI2C2Send i2c_lcd_byte
            i2c_lcd_e = 0;
            HI2C2Send i2c_lcd_byte
            HI2C2Stop
            LCD_State = 12

         #ENDIF


        WAIT LCD_SPEED us

    #ENDIF

    #IFDEF LCD_IO 404

        'Set pins to output
        DIR LCD_DB4 OUT
        DIR LCD_DB5 OUT
        DIR LCD_DB6 OUT
        DIR LCD_DB7 OUT

        #IFDEF LCD_LAT
            DIR _LCD_DB4 OUT
            DIR _LCD_DB5 OUT
            DIR _LCD_DB6 OUT
            DIR _LCD_DB7 OUT
        #ENDIF

       'Write upper nibble to output pins
        set LCD_DB4 OFF
        set LCD_DB5 OFF
        set LCD_DB6 OFF
        set LCD_DB7 OFF
        if LCDByte.7 ON THEN SET LCD_DB7 ON
        if LCDByte.6 ON THEN SET LCD_DB6 ON
        if LCDByte.5 ON THEN SET LCD_DB5 ON
        if LCDByte.4 ON THEN SET LCD_DB4 ON

        wait 1 us
        if gLCDEnableAddress = 1 then PulseOut LCD_enable1, 2 us
        if gLCDEnableAddress = 2 then PulseOut LCD_enable2, 2 us
        Wait 2 us

        'All data pins low
        set LCD_DB4 OFF
        set LCD_DB5 OFF
        set LCD_DB6 OFF
        set LCD_DB7 OFF

       'Write lower nibble to output pins
        if LCDByte.3 ON THEN SET LCD_DB7 ON
        if LCDByte.2 ON THEN SET LCD_DB6 ON
        if LCDByte.1 ON THEN SET LCD_DB5 ON
        if LCDByte.0 ON THEN SET LCD_DB4 ON
        wait 1 us
        if gLCDEnableAddress = 1 then PulseOut LCD_enable1, 2 us
        if gLCDEnableAddress = 2 then PulseOut LCD_enable2, 2 us
        wait 2 us

       'Set data pins low again
        SET LCD_DB7 OFF
        SET LCD_DB6 OFF
        SET LCD_DB5 OFF
        SET LCD_DB4 OFF

        WAIT LCD_SPEED us

    #ENDIF

    'If Register Select is low
    IF LCD_RS = 0 then
        IF LCDByte < 16 then
            if LCDByte > 7 then
               LCD_State = LCDByte
            end if
        END IF
    END IF

end sub

SUB LCD2_NIBBLEOUT (In LCD2BYTE)
'Sub to send byte in two nibbles to LCD
    #IFDEF LCD_IO 1
        '1-wire mode with 74HC595

        'Idle high
        SET LCD_CD ON

        'Send QH/QH' as a "One"; mandatory for resetting the shiftreg and a working monoflop
        Onebit

        IF LCD_RS ON THEN
            'Send QG; LCD RS signal
            Onebit
        ELSE
            Zerobit
        END IF

        'Send QF; Spare pin
        Zerobit

        If LCD_Backlight Then ' Background LED is used
            'Send QE (pin 4 74HC595); used for Backlight
            Onebit
        Else
            Zerobit
        End if

        REPEAT 4
            'Send QD - QA to Shift register; starting from Nibble most significant bit
            IF LCD2Byte.3 ON THEN
                Onebit
            Else
                Zerobit
            END IF
            ROTATE LCD2Byte LEFT
        END REPEAT

      'Generate reset signal for shiftreg and activate monoflop
      ResetShiftReg

        Wait LCD_SPEED  us

    #ENDIF

    #IFDEF LCD_IO 2, 2_74XX174, 2_74XX164
        '2-wire modes with Shiftreg

        ' Set Data and Clock bits off
        SET LCD_DB OFF
        SET LCD_CB OFF

        #IFDEF LCD_IO 2, 2_74XX174
            'This is mode with 74LS174 or  74HC164 with diode connected to pin 11
            'Backilight not supported

            'Clear Shift Register With six zero's
            REPEAT 6
                'STROBE
                SET LCD_CB ON
                SET LCD_CB OFF
            END REPEAT
        #ENDIF

         #IFDEF LCD_IO 2_74XX164
            'This is mode with 74HC164 and Backlight

            'Clear Shift Register with eight zero's
            REPEAT 8
                ' STROBE
                SET LCD_CB ON
                SET LCD_CB OFF
            END REPEAT
        #ENDIF

        'First bit out to Shift register, always 1 for E gate LCD
        SET LCD_DB ON
        'STROBE
        SET LCD_CB ON
        SET LCD_CB OFF


        #IFDEF LCD_IO 2_74XX164
            'Set Spare pin and background pin 74HC164

            'Spare bit (QG)
            SET LCD_DB OFF
            'STROBE
            SET LCD_CB ON
            SET LCD_CB OFF

            ' Background LED is used
            If LCD_Backlight Then
                'send a one bit (QF, pin 11 74HC164)
                SET LCD_DB ON
                'STROBE
                SET LCD_CB ON
                SET LCD_CB OFF
            Else
                'send a zero bit (QF, pin 11 74HC164)
                SET LCD_DB OFF
                'STROBE
                SET LCD_CB ON
                SET LCD_CB OFF
            End if
        #ENDIF

      SET LCD_DB ON
      'Shift register (QE), R/S gate LCD
      IF LCD_RS OFF THEN SET LCD_DB OFF
      'Strobe
      SET LCD_CB ON
      SET LCD_CB OFF

        '4 bits Data (QD - QA) out to Shift register, starting from Nibble most significant bit
        'FOR LCDTemp = 1 to 4
        REPEAT 4
            SET LCD_DB OFF
            IF LCD2Byte.3 ON THEN SET LCD_DB ON
            'STROBE
            SET LCD_CB ON
            SET LCD_CB OFF
            ROTATE LCD2Byte LEFT
        END REPEAT

        ' Last pulse for Nibble output. Not for Shift register
        SET LCD_DB ON
        ' Put E pin on LCD to one through an AND operation
        WAIT 1 MS
        ' with the first bit outputted (QH)
        SET LCD_DB OFF
    #ENDIF
END SUB

'LCDNormalReadByte Only supported in LCD_IO 4,8
function LCDNormalReadByte

    #IFDEF LCD_NO_RW
        LCDReadByte = 0
        Exit Function
    #ENDIF

    #IFNDEF LCD_NO_RW

        set LCD_RW ON 'Read mode
        LCDReadByte = 0

        #IFDEF LCD_IO 4
            'Set pins to input
            DIR LCD_DB4 IN
            DIR LCD_DB5 IN
            DIR LCD_DB6 IN
            DIR LCD_DB7 IN

            'Read upper nibble from input pins
            SET LCD_Enable ON
            Wait LCD_Write_Delay     '2 us
            if LCD_DB7 ON then SET LCDReadByte.7 ON
            if LCD_DB6 ON THEN SET LCDReadByte.6 ON
            if LCD_DB5 ON then SET LCDReadByte.5 ON
            if LCD_DB4 ON THEN SET LCDReadByte.4 ON
            SET LCD_Enable OFF
            Wait 5 us

            'Read lower nibble from input pins
            SET LCD_Enable ON
            Wait LCD_Write_Delay
            if LCD_DB7 ON then SET LCDReadByte.3 ON
            if LCD_DB6 ON THEN SET LCDReadByte.2 ON
            if LCD_DB5 ON then SET LCDReadByte.1 ON
            if LCD_DB4 ON THEN SET LCDReadByte.0 ON
            SET LCD_Enable OFF
            Wait 5 us
        #ENDIF

        #IFDEF LCD_IO 8
            DIR LCD_DATA_PORT 255
            SET LCD_Enable ON
            Wait LCD_Write_Delay
            LCDReadByte = LCD_DATA_PORT
            SET LCD_Enable OFF
            Wait 5 us
         #ENDIF
    #ENDIF
end function

' Method uses parameters LCDON, LCDOFF, CURSORON, CURSOROFF, FLASHON, Or FLASHOFF
sub LCDCursor(In LCDCRSR)
'Sub  to set cursor style
    Set LCD_RS OFF

    If LCDCRSR = ON  Then LCDTemp = LCD_State OR LCDON
    IF LCDCRSR = LCDON Then LCDTemp = LCD_State OR LCDON

    If LCDCRSR = OFF Then LCDTemp = LCD_State AND LCDOFF
    If LCDCRSR = LCDOFF Then LCDTemp = LCD_State AND LCDOFF

    If LCDCRSR = CursorOn Then LCDTemp = LCD_State OR CursorON
    If LCDCRSR = CursorOFF then LCDTemp = LCD_State and CursorOFF

    If LCDCRSR = FLASH  Then LCDTemp = LCD_State OR FLASHON
    If LCDCRSR = FLASHON  Then LCDTemp = LCD_State OR FLASHON
    If LCDCRSR = FLASHOFF then LCDTemp = LCD_State and FLASHOFF

    LCDWriteByte(LCDTemp)
    'save last state
    LCD_State = LCDtemp

end sub

sub LCDCreateChar (In LCDCharLoc, LCDCharData())
'Sub a create a custom character in CGRAM

    'Store old location
      #IFDEF LCD_IO 4,8,10,12
        #ifndef LCD_NO_RW
          Set LCD_RS Off
          LCDLoc = LCDReadByte
          Set LCDLoc.7 On
        #endif
    #ENDIF

    'Select location
    Set LCD_RS Off
    LCDWriteByte (64 + LCDCharLoc * 8)
    wait 5 10us

    'Write char
    Set LCD_RS On
    For LCDTemp = 1 to 8
        LCDWriteByte LCDCharData(LCDTemp)
        wait 5 10us
    Next

    'Restore location
    #IFDEF LCD_IO 4,8,10,12
        #ifndef LCD_NO_RW
            Set LCD_RS Off
            LCDWriteByte (LCDLoc)
            wait 5 10us
        #endif

        #ifdef LCD_NO_RW
            set LCD_RS off
            LCDWriteByte(0x80)
            wait 5 10us
        #endif
    #endif

    #ifndef LCD_IO 4,8,10,12
       set LCD_RS off
       LCDWriteByte(0x80)
       wait 5 10us
     #endif
end sub

Sub LCDCreateGraph(In LCDCharLoc, In LCDValue)
'Sub to create a graph character in CGRAM

    'Store old location
    #IFDEF LCD_IO 4,8
        #ifndef LCD_NO_RW
          Set LCD_RS Off
          LCDLoc = LCDReadByte
          Set LCDLoc.7 On
        #endif
    #ENDIF

    'Select location
    Set LCD_RS Off
    LCDWriteByte (64 + LCDCharLoc * 8)
    wait 5 10us

    'Write char for graph
    Set LCD_RS On
    For LCDTemp = 8 to 1
        If LCDTemp > LCDValue Then
            LCDWriteByte LCD_BAR_EMPTY
        Else
            LCDWriteByte LCD_BAR_FULL
        End If
        wait 5 10us
    Next

    'Restore location
      #IFDEF LCD_IO 4,8
        #ifndef LCD_NO_RW
            Set LCD_RS Off
            LCDWriteByte (LCDLoc)
            wait 5 10us
        #endif

        #ifdef LCD_NO_RW
            set LCD_RS off
            LCDWriteByte(0x80)
            wait 5 10us
        #endif
    #endif

    #ifndef LCD_IO 4,8
        set LCD_RS off
        LCDWriteByte(0x80)
        Wait 5 10us
    #endif
End Sub

sub LCDSpace(in LCDValue)
'Sub to print a number of spaces - upto 40

    set LCD_RS on
    if LCDValue > 40 then LCDValue = 40
    do until LCDValue = 0
        LCDWriteByte(32)
        LCDValue --
    loop
end sub

sub LCDDisplayOff
'Sub to turn off display and turn off cursor and turn off flash

    set LCD_RS off
    LCDWriteByte(0b00001000)
    LCD_State = 0
    wait 10 ms
end sub

sub LCDDisplayOn
'Sub to turn ON display, turn off curor and turn off flash

 set LCD_RS off
    LCDWriteByte(0b00001100)
    LCD_State = 8
    wait 10 ms
End Sub

sub LCDBacklight(IN LCDTemp)
'Sub set to backlight state
      #IFDEF LCD_IO 1, 2_74XX164      '1 and 2-wire mode with Backlight
        IF LCDTemp = ON then
          LCD_Backlight = LCD_Backlight_On_State
        Else
          LCD_Backlight = LCD_Backlight_Off_State
        END IF
        Set LCD_RS OFF
        LCDWriteByte(0)
    #ENDIF

    #IFDEF LCD_IO 0,4,8, 404
        'Set the port for this mode
        IF LCDTemp = OFF then set LCD_Backlight LCD_Backlight_Off_State
        IF LCDTemp = ON then  set LCD_Backlight LCD_Backlight_On_State
    #ENDIF

    #IFDEF LCD_IO 10,12
        'Assign the variable for this mode
        IF LCDTemp = OFF then LCD_Backlight = LCD_Backlight_Off_State
        IF LCDTemp = ON then  LCD_Backlight = LCD_Backlight_On_State
        ' write a zero and the method will set display backlite
        Set LCD_RS OFF
        LCDWriteByte(0)
    #ENDIF
end Sub

Sub PUT404 (In LCDPutLine, In LCDPutColumn, In LCDChar)
'Sub to put a character at the location of the cursor
    LOCATE LCDPutLine, LCDPutColumn
    Set LCD_RS on
    LCDWriteByte(LCDChar)
End Sub

Sub LOCATE404 (In LCDLine, In LCDColumn)
'sub to set the position of the cursor
    gLCDXYPosition = ( LCDLine * 40 ) + ( LCDColumn  )

    Set LCD_RS Off

    if LCDColumn < 20 Then
        gLCDEnableAddress = 1
    Else
        gLCDEnableAddress = 2
        LCDColumn = LCDColumn - 20
    end if

    If LCDLine > 1 Then
        LCDLine = LCDLine - 2
        LCDColumn = LCDColumn + LCD_WIDTH
    End If

    LCDWriteByte(0x80 or 0x40 * LCDLine + LCDColumn)
    wait 5 10us

End Sub

Sub CLS404( Optional in lLCDEnableAddress = 3 )
'Sub to clear the LCD
    gLCDEnableAddress = 0
    if lLCDEnableAddress.1 = 1 then gLCDEnableAddress = 2
    Repeat 2

        SET LCD_RS OFF

        'Clear screen
        LCDWriteByte (0b00000001)
        Wait 4 ms

        'Move to start of visible DDRAM
        LCDWriteByte(0x80)
        Wait 12 10us

        if lLCDEnableAddress.0 = 1 then gLCDEnableAddress = 1
      end repeat

    select case lLCDEnableAddress
        case 1
          gLCDXYPosition = 0
        case 2
          gLCDXYPosition = 20
        Case Else
          gLCDXYPosition = 0
    end Select

End Sub

Sub LCDHOME404( Optional in lLCDEnableAddress = 3 )
'Sub to set the cursor to the home position
    if lLCDEnableAddress.1 = 1 then gLCDEnableAddress = 2
    Repeat 2
        SET LCD_RS OFF

        'Return CURSOR to home
        LCDWriteByte (0b00000010)
        Wait 2 ms
        if lLCDEnableAddress.0 = 1 then gLCDEnableAddress = 1
    end repeat

    select case lLCDEnableAddress
        case 1
          gLCDXYPosition = 0
        case 2
          gLCDXYPosition = 20
        Case Else
          gLCDXYPosition = 0
    end Select

End Sub

Sub LCDcmd404 ( In LCDValue,  Optional in lLCDEnableAddress = 3 )
'Sub to set a specific command to the LCD
    if lLCDEnableAddress.1 = 1 then gLCDEnableAddress = 2
    Repeat 2

        SET LCD_RS OFF
        'Send Command.... this is unvalidated.. send whatever is passed!
        LCDWriteByte ( LCDValue)
        Wait 2 ms
        if lLCDEnableAddress.0 = 1 then gLCDEnableAddress = 1

    end repeat
end sub

sub Print404 (In PrintData As String)
'Sub to print a string variable on the LCD

    PrintLen = PrintData(0)

    If PrintLen = 0 Then Exit Sub

    'Write Data
    For SysPrintTemp = 1 To PrintLen
        Set LCD_RS On
        locate ( ( gLCDXYPosition / 40 ) , (gLCDXYPosition mod 40) )
        Set LCD_RS On
        LCDWriteByte PrintData(SysPrintTemp)
        'increment XY position
        gLCDXYPosition++
    Next

End Sub

Sub Print404 (In LCDValue)
'Sub to print a byte variable on the LCD
    LCDValueTemp = 0

    'calculate XY position
    locate ( ( gLCDXYPosition / 40 ) , (gLCDXYPosition mod 40) )
    Set LCD_RS On

    IF LCDValue >= 100 Then
        LCDValueTemp = LCDValue / 100
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        gLCDXYPosition++
    End If

    If LCDValueTemp > 0 Or LCDValue >= 10 Then
        LCDValueTemp = LCDValue / 10
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        gLCDXYPosition++
    End If

    LCDWriteByte (LCDValue + 48)
    'increment XY position
    gLCDXYPosition++

End Sub

Sub Print404 (In LCDValue As Word)
'Sub to print a word variable on the LCD
    Dim SysCalcTempX As Word


    'calculate XY position
    locate ( ( gLCDXYPosition / 40 ) , (gLCDXYPosition mod 40) )

    Set LCD_RS On
    LCDValueTemp = 0

    If LCDValue >= 10000 then
        LCDValueTemp = LCDValue / 10000 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        gLCDXYPosition++
        Goto LCDPrintWord1000
    End If

    If LCDValue >= 1000 then
        LCDPrintWord1000:
        LCDValueTemp = LCDValue / 1000 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        gLCDXYPosition++
        Goto LCDPrintWord100
    End If

    If LCDValue >= 100 then
        LCDPrintWord100:
        LCDValueTemp = LCDValue / 100 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        gLCDXYPosition++
        Goto LCDPrintWord10
    End If

    If LCDValue >= 10 then
        LCDPrintWord10:
        LCDValueTemp = LCDValue / 10 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        gLCDXYPosition++
    End If

    LCDWriteByte (LCDValue + 48)
    gLCDXYPosition++

End Sub

Sub Print404 (In LCDValueInt As Integer)
'Sub to print an integer variable on the LCD
    Dim LCDValue As Word

    'calculate XY position
    locate ( ( gLCDXYPosition / 40 ) , (gLCDXYPosition mod 40) )

    'If sign bit is on, print - sign and then negate
    If LCDValueInt.15 = On Then
        LCDWriteChar("-")
        gLCDXYPosition++
        LCDValue = -LCDValueInt

    'Sign bit off, so just copy value
    Else
        LCDValue = LCDValueInt
    End If

    'Use Print(word) to display value
    Print LCDValue
    gLCDXYPosition++
End Sub

Sub Print404 (In LCDValue As Long)
'Sub to print a long variable on the LCD
    Dim SysCalcTempA As Long
    Dim SysPrintBuffer(10)
    SysPrintBuffLen = 0

    'calculate XY position
    locate ( ( gLCDXYPosition / 40 ) , (gLCDXYPosition mod 40) )

    Do
         'Divide number by 10, remainder into buffer
         SysPrintBuffLen += 1
         SysPrintBuffer(SysPrintBuffLen) = LCDValue % 10
         LCDValue = SysCalcTempA
    Loop While LCDValue <> 0

    'Display
    Set LCD_RS On
    For SysPrintTemp = SysPrintBuffLen To 1 Step -1
        LCDWriteByte(SysPrintBuffer(SysPrintTemp) + 48)
        gLCDXYPosition++
    Next
End Sub

sub LCDHex404  (In LCDValue, optional in LCDChar = 1)
'Sub to print a hex string from the give byte value on the LCD
'Variable must be in the range of 0 to 255 (Dec)
'Variable can be entered as dec, binary or hex

    'calculate XY position
    locate ( ( gLCDXYPosition / 40 ) , (gLCDXYPosition mod 40) )


    'Extract nibbles and convert to ascii values
    HighChar = (LCDValue / 16)  + 48
    LowChar  = (LCDValue and 15) + 48

    'Check for alpha and convert.
    If HighChar > 57 Then HighChar = HighChar + 7
    If LowChar > 57 Then  LowChar = LowChar + 7

    Set LCD_RS OFF

    'Write chars to LCD - if user specifies LeadingZeroActive then print the leading char
     IF LCDChar = LeadingZeroActive then
        if LCDValue < 16 then
           LCDWriteChar 48
           gLCDXYPosition++
        end if
    END IF

    'Write high char if LCDValue is > 15 (DEC)
    IF LCDValue > 15 then
        LCDWriteChar HighChar
        gLCDXYPosition++
    end if

    LCDWriteChar LowChar
    gLCDXYPosition++

end sub

sub LCDCursor404( In LCDCRSR )
'Sub to set the cursor state

    Set LCD_RS OFF

    If LCDCRSR = ON  Then LCDTemp = LCD_State OR LCDON
    IF LCDCRSR = LCDON Then LCDTemp = LCD_State OR LCDON

    If LCDCRSR = OFF Then LCDTemp = LCD_State AND LCDOFF
    If LCDCRSR = LCDOFF Then LCDTemp = LCD_State AND LCDOFF

    If LCDCRSR = CursorOn Then LCDTemp = LCD_State OR CursorON
    If LCDCRSR = CursorOFF then LCDTemp = LCD_State and CursorOFF

    If LCDCRSR = FLASH  Then LCDTemp = LCD_State OR FLASHON
    If LCDCRSR = FLASHON  Then LCDTemp = LCD_State OR FLASHON
    If LCDCRSR = FLASHOFF then LCDTemp = LCD_State and FLASHOFF

    LCDWriteByte(LCDTemp)
    LCD_State = LCDtemp  'save last state

end sub

sub LCDCreateChar404 ( In LCDCharLoc, LCDCharData() )
'Sub to create a custom character in CGRAM
'This sub DOES NOT support restore location

    'Select location
    Set LCD_RS Off
    LCDWriteByte (64 + LCDCharLoc * 8)
    wait 5 10us

    'Write char
    Set LCD_RS On
    For LCDTemp = 1 to 8
      LCDWriteByte LCDCharData(LCDTemp)
      wait 5 10us
    Next

    set LCD_RS off
    LCDWriteByte(0x80)
    wait 5 10us

end sub

Sub LCDCreateGraph404(In LCDCharLoc, In LCDValue)
'Create a graph character in CGRAM

'Empty sub

End Sub

sub LCDSpace404(in LCDValue)
' Sub to print a number of spaces - upto 40

    set LCD_RS on
    if LCDValue > 40 then LCDValue = 40
    do until LCDValue = 0
        Print " "
        LCDValue --
        gLCDXYPosition++
    loop
end sub

sub LCDDisplaysOff404
'Sub to turn off display and turn off cursor and turn off flash
    LCDValue = 0
    setwith ( lcdvalue.0, LCD_enable1 )
    setwith ( lcdvalue.1, LCD_enable2 )

    Set LCD_enable1 on
    Set LCD_enable2 off
    LCDCURSOR LCDOFF

    Set LCD_enable1 off
    Set LCD_enable2 on
    LCDCURSOR LCDOFF

    wait 10 ms
    setwith ( LCD_enable2, lcdvalue.1 )
    setwith ( LCD_enable1, lcdvalue.0 )

end sub

sub LCDDisplaysOn404
'Sub to turn ON display, turn off curor and turn off flash

    lcdvalue.1 = LCD_enable2
    lcdvalue.0 = LCD_enable1

    Set LCD_enable1 on
    Set LCD_enable2 off
    LCDCURSOR LCDON

    Set LCD_enable1 off
    Set LCD_enable2 on
    LCDCURSOR LCDON

    setwith ( LCD_enable2, lcdvalue.1 )
    setwith ( LCD_enable1, lcdvalue.0 )

End Sub


'K107 Section





sub K107SendData ( in SendString as string)

    'Send data
    #if USART_BAUD_RATE
        HSerPrint SendString
    #endif
    wait 10 ms

end sub

sub K107SendRaw ( in SendValue as byte)

    'Send data
    #if USART_BAUD_RATE
        HSerSend SendValue
    #endif


end sub

'#define CLS K107CLS
sub K107CLS
    K107SendData ( "?f" )
end sub


'#define LOCATE K107LOCATE
sub K107LOCATE (In LCDLine, In LCDColumn)
    if LCDColumn < 10 then
        K107SendData ( "?x0"+str(LCDColumn) )
    else
        K107SendData ( "?x"+str(LCDColumn) )
    end if
    K107SendData ( "?y"+str(LCDLine  ) )
end sub

'#define LCDHOME K107LCDHOME
Sub K107LCDHOME
'Sub to set the cursor to the home position
    K107SendData ( "?h" )
End Sub

'#define LCDcmd K107LCDcmd
Sub K107LCDcmd ( In PrintData )
'Sub to send specified command direct to the LCD
    K107SendData ( PrintData  )
End Sub

'#DEFINE Print K107Print
sub K107Print (In PrintData As String)
'Sub to print a string variable on the LCD
    K107SendData ( PrintData  )
End Sub


Sub K107Print (In LCDValue)
'Sub to print a byte variable on the LCD
    Dim LCDValueTemp as Byte
    LCDValueTemp = 0

    IF LCDValue >= 100 Then
        LCDValueTemp = LCDValue / 100
        LCDValue = SysCalcTempX
        K107SendData chr((LCDValueTemp + 48))
    End If

    If LCDValueTemp > 0 Or LCDValue >= 10 Then
        LCDValueTemp = LCDValue / 10
        LCDValue = SysCalcTempX
        K107SendData chr(LCDValueTemp + 48)
    End If
    K107SendData chr(LCDValue + 48)
End Sub

Sub K107Print (In LCDValue As Word)
'Sub to print a word variable on the LCD
    Dim LCDValueTemp as Byte
    Dim SysCalcTempX As Word

    LCDValueTemp = 0

    If LCDValue >= 10000 then
        LCDValueTemp = LCDValue / 10000 [word]
        LCDValue = SysCalcTempX
        K107SendData( chr(LCDValueTemp + 48))
        Goto LCDPrintWord1000
    End If

    If LCDValue >= 1000 then
        LCDPrintWord1000:
        LCDValueTemp = LCDValue / 1000 [word]
        LCDValue = SysCalcTempX
        K107SendData(chr(LCDValueTemp + 48))
        Goto LCDPrintWord100
    End If

    If LCDValue >= 100 then
        LCDPrintWord100:
        LCDValueTemp = LCDValue / 100 [word]
        LCDValue = SysCalcTempX
        K107SendData(chr(LCDValueTemp + 48))
        Goto LCDPrintWord10
    End If

    If LCDValue >= 10 then
        LCDPrintWord10:
        LCDValueTemp = LCDValue / 10 [word]
        LCDValue = SysCalcTempX
        K107SendData(chr(LCDValueTemp + 48))
    End If

    K107SendData (chr(LCDValue + 48))
End Sub

Sub K107Print (In LCDValueInt As Integer)
'Sub to print an integer variable on the LCD

    Dim LCDValue As Word

    'If sign bit is on, print - sign and then negate
    If LCDValueInt.15 = On Then
              LCDWriteChar("-")
              LCDValue = -LCDValueInt

    'Sign bit off, so just copy value
    Else
              LCDValue = LCDValueInt
    End If

    'Use Print(word) to display value
    Print LCDValue
End Sub

Sub K107Print (In LCDValue As Long)
'Sub to print a long variable on the LCD
    Dim SysPrintBuffLen, SysPrintTemp as byte
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
    For SysPrintTemp = SysPrintBuffLen To 1 Step -1
      LCDValue = SysPrintBuffer(SysPrintTemp) + 48
      K107SendRaw LCDValue
    Next

End Sub

'#define LCDBacklight K107LCDBacklight
sub K107LCDBacklight(IN LCDValue)
    K107SendData( "?B" )

    K107LCDHEX ( LCDValue ,2 )

End Sub

'#define LCDCursor K107LCDCursor
sub K107LCDCursor(In LCDCRSR)
    K107SendData( "?c"+str(LCDCRSR) )
End Sub

'#define LCDHex K107LCDHex
sub K107LCDHex  (In LCDValue, optional in LCDChar = 1)
'Sub to print a hex string from the specified byte variable on the LCD

    dim myK107LCDHexString as string * 3
    myK107LCDHexString = Hex ( LCDValue )
    K107SendData myK107LCDHexString

end sub

sub K107LCDSpace(in LCDValue)
'Sub to print a number of spaces - upto 40

    if LCDValue > 40 then LCDValue = 40
    do until LCDValue = 0
        K107SendData " "
        LCDValue --
    loop
end sub

;LCD_VARIANT 1601a
;
;The variant assummes 8chars by 1row.
;Memory maps as follows - columne to memory
;0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
;0  1  2  3  4  5  6  7 40 41  42 43 44 45 46 47
;
;So, by locating to the first row on a twenty row display, the default LCD width, the additional locatates address the memory correctly.
;






Sub V1601aPrint (In PrintData As String)
'Sub to print a string variable on the LCD
    Dim CurrentCol, CurrentLine as Byte
    Dim PrintLen, SysPrintTemp  as byte

    PrintLen = PrintData(0)

    If PrintLen = 0 Then Exit Sub
    Set LCD_RS On

    'Write Data
    For SysPrintTemp = 1 To PrintLen
        LCDWriteByte PrintData(SysPrintTemp)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
    Next

End Sub

Sub V1601aPrint (In LCDValue)
'Sub to print a byte variable on the LCD

    Dim CurrentCol, CurrentLine as Byte

    LCDValueTemp = 0
    Set LCD_RS On

    IF LCDValue >= 100 Then
        LCDValueTemp = LCDValue / 100
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
    End If

    If LCDValueTemp > 0 Or LCDValue >= 10 Then
        LCDValueTemp = LCDValue / 10
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
    End If
    LCDWriteByte (LCDValue + 48)
    CurrentCol++
    Locate ( CurrentLine, CurrentCol )
    Set LCD_RS On

End Sub

Sub V1601aPrint (In LCDValue As Word)
'Sub to print a word variable on the LCD
    dim LCDValueTemp as Byte
    Dim CurrentCol, CurrentLine as Byte

    Dim SysCalcTempX As Word
    Set LCD_RS On
    LCDValueTemp = 0

    If LCDValue >= 10000 then
        LCDValueTemp = LCDValue / 10000 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
        Goto LCDPrintWord1000
    End If

    If LCDValue >= 1000 then
        LCDPrintWord1000:
        LCDValueTemp = LCDValue / 1000 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
        Goto LCDPrintWord100
    End If

    If LCDValue >= 100 then
        LCDPrintWord100:
        LCDValueTemp = LCDValue / 100 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
        Goto LCDPrintWord10
    End If

    If LCDValue >= 10 then
        LCDPrintWord10:
        LCDValueTemp = LCDValue / 10 [word]
        LCDValue = SysCalcTempX
        LCDWriteByte(LCDValueTemp + 48)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
    End If

    LCDWriteByte (LCDValue + 48)
    CurrentCol++
    Locate ( CurrentLine, CurrentCol )
    Set LCD_RS On
End Sub

Sub V1601aPrint (In LCDValueInt As Integer)
'Sub to print an integer variable on the LCD

    Dim CurrentCol, CurrentLine as Byte
    Dim LCDValue As Word

    'If sign bit is on, print - sign and then negate
    If LCDValueInt.15 = On Then
              LCDWriteChar("-")
              CurrentCol++
              Locate ( CurrentLine, CurrentCol )
              Set LCD_RS On
              LCDValue = -LCDValueInt

    'Sign bit off, so just copy value
    Else
              LCDValue = LCDValueInt
    End If

    'Use Print(word) to display value
    Print LCDValue
End Sub

Sub V1601aPrint (In LCDValue As Long)
'Sub to print a long variable on the LCD

    Dim CurrentCol, CurrentLine as Byte

    Dim SysPrintBuffLen as byte
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
    Set LCD_RS On
    For SysPrintTemp = SysPrintBuffLen To 1 Step -1
        LCDWriteByte(SysPrintBuffer(SysPrintTemp) + 48)
        CurrentCol++
        Locate ( CurrentLine, CurrentCol )
        Set LCD_RS On
    Next

End Sub

Sub V1601aLOCATE (In LCDLine, In LCDColumn)
'Sub to locate the cursor
'Where LCDColumn is 0 to screen width-1, LCDLine is 0 to screen height-1
   CurrentCol = LCDColumn
   CurrentLine = LCDLine

   Set LCD_RS Off
   If LCDColumn > 7 Then
       LCDLine = 1
       LCDColumn = LCDColumn - 8
   else
       LCDLine = 0
   End If

   LCDWriteByte(0x80 or 0x40 * LCDLine + LCDColumn)
   wait 5 10us

End Sub

Sub V1601aCLS
'Sub to clear the LCD

    Dim CurrentCol, CurrentLine as Byte


    SET LCD_RS OFF

    'Clear screen
    LCDWriteByte (0b00000001)
    Wait 4 ms

    'Move to start of visible DDRAM
    LCDWriteByte(0x80)
    Wait 50 us

    CurrentCol = 0
    CurrentLine = 0


End Sub

Sub V1601aLCDHOME
'Sub to set the cursor to the home position

    Dim CurrentCol, CurrentLine as Byte

    SET LCD_RS OFF
    'Return CURSOR to home
    LCDWriteByte (0b00000010)
    Wait 2 ms 'Must be > 1.52 ms

    CurrentCol = 0
    CurrentLine = 0

End Sub






;    Credits:
;    4 and 8 bit routines        Hugh Considine
;    2 bit routines    Stefano Bonomi
;    Testing           Stefano Adami
;    Revised 4 bit routines    Evan Venn
;    and adapted to support LAT port support for fast devices
;    Revised to improve performance and improved functionality William Roth
;*************************************************************************
;*************************************************************************
;    08-17-2014
;    Modified for speed improvement in 4 and 8 bit modes.
;
;    1. MOdified sub LCDNormalWriteByte.
;     A. Changed enable pulse duration duration to 2 us
;     B. Added DEFINE  LCD_SPEED
;         1. LCD_SPEED FAST
;         2. LCD_SPEED MEDIUM
;         3. LCD_SPEED SLOW
;     C. The speed is improved from about 5,000 chars per second to
;         apppoximately 20,000 CPS (FAST), 15,000 CPS (MEDUIM) and
;         10,000 CPS (SLOW).
;     D.  IF LCD_SPEED is not defined, the speed defaults to SLOW
;
;    2. Created separate code sections for 4-Bit & 8-bit initalization
;
;    23-1-2015 by William Roth
;
;    3. Added comments to some code sections
;
;    4. Added sub routines for LCD_OFF and LCD_ON
;
;    26-1-2015 by William and Evan following Hughs code review;
;
;    5. Added new Sub for LCDHex with optional parameter
;
;    6. Deprecated LCD_On replaced with LCDDisplayOn
;
;    7. Deprecated LCD_Off replaced with LCDDisplayOff
;
;    8. Added new method LCDBackLight
;
'**********************************************************************
;    28-1-2015 by Evan R Venn
;
;    Removed errant CLS from outside of methods.
;
;    14-2-15 by Evan R Vemm
;
;    Added I2C support.  Added/revixed functions and added scripts
;    Revised speed from constant to defines
;    Fixed Cursor to remove IF AND THEN as a fix for AVR
;    Changed init to support AVR
;    Changed backlight to support IC2 and Transistor support.
;    Revised to support multiple I2C.
;
;    Revised 31/07/2-15 to removed defines being defined when not needed.
;

;    Uses
'''Set LCD_10 to 10 for the YwRobot LCD1602 IIC V1 or the Sainsmart LCD_PIC I2C adapter
'''Set LCD_10 to 12 for the Ywmjkdz I2C adapter with pot bent over top of chip

'''   #define LCD_I2C_Address_1 0x4e
'''   #define LCD_I2C_Address_2 0x4c
'''   #define LCD_I2C_Address_3 0x4a
'''   #define LCD_I2C_Address_4 0x49
'''
'''   Use LCD_I2C_Address_Current = LCD_I2C_Address to change the LCD output routines to a specific device.
'''       LCD_I2C_Address_Current = LCD_I2C_Address
'''       Print "LCD Address is now": LCDHex(  LCD_I2C_Address_Current, 2)

'**************************************************************************
'''   14-08-2015 by Theo Loermans
'''
'''   Added LCD_IO 1: 1-wire support with shift-register 74HC595
'''   Use LCD_CD as port for combined data and clock
'''
'''   Added LCDBacklight On/Off for LCD_IO 1,2 mode
'''
'''   30-08-2015
'''   Added LCD_IO 2_74XX174 and LCD_IO 2_74XX164: 2-wire modes for different shiftregisters
'''
'''   02/01/2015  added 404 support
'*************************************************************************
'''   14/02/2019  revised LCDNormalWriteByte to correct LCDstate error


'''
'''   30/01/2017  edit the binary notation to prevent silly error message

'''   18-02-2017 by Ruud de Vreugd
'''   Added a forced write for instant backlightcontrol as suggested by Theo Loermans
'''   Changed binary number format to not use quotationmarks (to avoid compiler errors)
'''
'''   Changed Restart to Start in i2c section
'''   29-03-2018 by Evan Added LCD_WIDTH to support variable LCD widths.
'''   use #define LCD_WIDTH 16 to change the standard of 20 character width to 16.
'''   Revised 8-bit init from 200us to 20ms and revised LCDReady for 8bit schmitt trigger ports added 1 us delay

'''   28/10/2015  added LCD_3  support Evan R. Venn for the Picsimlab board K16F for the  LS74574 connectivity see the Help in the application for connectivity
'''              ;Setup LCD Parameters
'''              #define LCD_IO 3
'''
'''              'Change as necessary
'''              #define LCD_DB     PORTb.3            ; databit
'''              #define LCD_CB     PORTb.4            ; clockbit
'''              #define LCD_EB     PORTa.0            ; enable bit
'''
'''
'''  12/04/2019    Commentry tidy-up only

'*************************************************************************
'''   26/09/2020 Frank SteinBerg / William Roth

'''   Fixed bug in 8-bit init where NormalWriteByte was not being called
'''   Improved memory optimization in 4-Bit init
'''   Eliminated redundant code in Init
'''   Tidyed order of init routines to be sequential
'''   Updated version info

'*************************************************************************
