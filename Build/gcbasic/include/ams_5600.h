/*
--------------------------------------------------------------------------------
AMS 5600 class for GCBASIC

datasheet: https://ams.com/documents/20143/36005/AS5600_DS000365_5-00.pdf

@license   GPL
********************************************************************************

24/03/2022  Initial release  0.9a

*/


  // I2C ADDRESS
#DEFINE AMS5600_ADDRESS = 0X6C

  // SINGLE BYTE REGISTERS
#DEFINE AMS5600_ADDR_STATUS = 0X0B // MAGNET STATUS
#DEFINE AMS5600_ADDR_AGC    = 0X1A // AUTOMATIC GAIN CONTROL
#DEFINE AMS5600_ADDR_BURN   = 0XFF // PERMANENT BURNING OF CONFIGS (ZPOS, MPOS, MANG, CONF)
#DEFINE AMS5600_ADDR_ZMCO   = 0X00 // NUMBER OF TIMES ZPOS/MPOS HAS BEEN PERMANENTLY BURNED
                                            // ZPOS/MPOS CAN BE PERMANENTLY BURNED 3X
                                            // MANG/CONF CAN BE BURNED ONLY ONCE
  
  // DOUBLE BYTE REGISTERS, SPECIFY STARTING ADDRESS (LOWER ADDR, BUT HIGHER BYTE DATA)
  // ADDR   = UPPER BYTE OF DATA (MSB), ONLY BITS 0:3 ARE USED
  // ADDR+1 = LOWER BYTE OF DATA (LSB)
#DEFINE AMS5600_ADDR_ZPOS      = 0X01 // ZERO POSITION (START)
                                               // 0X02 - LOWER BYTE
#DEFINE AMS5600_ADDR_MPOS      = 0X03 // MAXIMUM POSITION (STOP)
                                               // 0X04 - LOWER BYTE
#DEFINE AMS5600_ADDR_MANG      = 0X05 // MAXIMUM ANGLE
                                               // 0X06 - LOWER BYTE
#DEFINE AMS5600_ADDR_CONF      = 0X07 // CONFIGURATION
                                               // 0X08 - LOWER BYTE
#DEFINE AMS5600_ADDR_RAW_ANGLE = 0X0C // RAW ANGLE
                                               // 0X0D - LOWER BYTE
#DEFINE AMS5600_ADDR_ANGLE     = 0X0E // MAPPED ANGLE
                                               // 0X0F - LOWER BYTE
#DEFINE AMS5600_ADDR_MAGNITUDE = 0X1B // MAGNITUDE OF INTERNAL CORDIC
                                               // 0X1C - LOWER BYTE


/* mode = 0, output PWM, mode = 1 output analog (full range from 0% to 100% between GND and VDD */
Sub AMS_5600_setOutPut( AMS_5600_setOutPut_mode )

    '   int _conf_lo = _addr_conf+1 // lower byte address
    '   uint8_t config_status;
    '   config_status = readOneByte(_conf_lo);
    '   if (mode == 1) {
    '     config_status = config_status & 0xcf;
    '   } else {
    '     uint8_t config_status;
    '     config_status = readOneByte(_conf_lo);
    '     if (mode == 1)
    '       config_status = config_status & 0xcf;
    '     else
    '       config_status = config_status & 0xef;
    '     writeOneByte(_conf_lo, lowByte(config_status));
    '   }

    Dim __AMS_5600_conf_status As Word
    __AMS_5600_conf_status =  AMS_5600_getConf()
    

    If AMS_5600_setOutPut_mode = 1 Then
        __AMS_5600_conf_status = __AMS_5600_conf_status & 0xFFCF
    Else
        __AMS_5600_conf_status = __AMS_5600_conf_status & 0xFFEF
    End If
    AMS_5600_setConf ( __AMS_5600_conf_status )


    End Sub

' ****************************************************
'   Method: AMS_5600
'   In: none
'   Out: i2c address of AMS 5600
'   Description: returns i2c address of AMS 5600
' ****************************************************
Function AMS_5600_getAddress
    AMS_5600_getAddress = AMS5600_ADDRESS
    End Function

' ******************************************************
'   Method: setMaxAngle
'   In: new maximum angle to set OR none
'   Out: value of max angle register
'   Description: sets a value in maximum angle register.
'   If no value is provided, method will read position of
'   magnet.  Setting this register zeros out max position
'   register.
' ******************************************************
Function AMS_5600_setMaxAngle( AMS_5600_newMaxAngle as Word ) as Word
    ' {
    '   word _maxAngle;
    '   if (newMaxAngle == -1)
    '     _maxAngle = getRawAngle();
    '   else
    '     _maxAngle = newMaxAngle;

    '   writeOneByte(_addr_mang, highByte(_maxAngle));
    '   delay(2);
    '   writeOneByte(_addr_mang+1, lowByte(_maxAngle));
    '   delay(2);

    '   word retVal = readTwoBytesSeparately(_addr_mang);
    '   return retVal;
    ' }


    Dim __AMS_5600_maxAngle AS Word

    If AMS_5600_endAngle = -1 Then
        __AMS_5600_maxAngle = AMS_5600_getRawAngle()
    Else
        __AMS_5600_maxAngle = AMS_5600_newMaxAngle
    End If
    
    HI2CStart
    HI2CSend( AMS5600_ADDRESS ) 
    HI2CSend( AMS5600_ADDR_MANG )
    HI2CSend( [Byte]__AMS_5600_maxAngle_h )
    HI2CStop
    wait 2 ms
    HI2CStart
    HI2CSend( AMS5600_ADDRESS ) 
    HI2CSend( AMS5600_ADDR_MANG+1 )
    HI2CSend( [Byte]__AMS_5600_maxAngle)
    HI2CStop
    wait 2 ms 


    HI2CStart
    HI2CSend( AMS5600_ADDRESS ) 
    HI2CSend( AMS5600_ADDR_MANG )
    HI2CReStart

    HI2CSend( AMS5600_ADDRESS+1 )
    HI2CReceive( [byte]AMS_5600_setMaxAngle_h )   
    HI2CReceive( [byte]AMS_5600_setMaxAngle, NACK ) 
    HI2CStop


    End Function

' ******************************************************
'   Method: getMaxAngle
'   In: none
'   Out: value of max angle register
'   Description: gets value of maximum angle register.
' ******************************************************
Function AMS_5600_getMaxAngle() as Word 
    ' {
    '   return readTwoBytesSeparately(_addr_mang);
    ' }

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_MANG )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getMaxAngle_h )   
        HI2CReceive( [byte]AMS_5600_getMaxAngle, NACK ) 
        HI2CStop

    End Function

' ******************************************************
'   Method: setStartPosition
'   In: new start angle position
'   Out: value of start position register
'   Description: sets a value in start position register.
'   If no value is provided, method will read position of
'   magnet.  
' ******************************************************
Function AMS_5600_setStartPosition( AMS_5600_startAngle as Word ) as Word
    ' {
    '   word _rawStartAngle;
    '   if (startAngle == -1)
    '     _rawStartAngle = getRawAngle();
    '   else
    '     _rawStartAngle = startAngle;

    '   writeOneByte(_addr_zpos, highByte(_rawStartAngle));
    '   delay(2);
    '   writeOneByte(_addr_zpos+1, lowByte(_rawStartAngle));
    '   delay(2);
    '   word _zPosition = readTwoBytesSeparately(_addr_zpos);

    '   return (_zPosition);
    ' }



        Dim __AMS_5600_rawStartAngle AS Word

        If AMS_5600_endAngle = -1 Then
            __AMS_5600_rawStartAngle = AMS_5600_getRawAngle()
        Else
            __AMS_5600_rawStartAngle = AMS_5600_startAngle
        End If
        
        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_ZPOS )
        HI2CSend( [Byte]__AMS_5600_rawStartAngle_h )
        HI2CStop
        wait 2 ms
        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_ZPOS+1 )
        HI2CSend( [Byte]__AMS_5600_rawStartAngle)
        HI2CStop
        wait 2 ms 


        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_ZPOS )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_setStartPosition_h )   
        HI2CReceive( [byte]AMS_5600_setStartPosition, NACK ) 
        HI2CStop

    End Function

    ' ******************************************************
    '   Method: getStartPosition
    '   In: none
    '   Out: value of start position register
    '   Description: gets value of start position register.
    ' ******************************************************
    Function AMS_5600_getStartPosition() AS Word

            HI2CStart
            HI2CSend( AMS5600_ADDRESS ) 
            HI2CSend( AMS5600_ADDR_ZPOS )
            HI2CReStart

            HI2CSend( AMS5600_ADDRESS+1 )
            HI2CReceive( [byte]AMS_5600_getStartPosition_h )   
            HI2CReceive( [byte]AMS_5600_getStartPosition, NACK ) 
            HI2CStop

        End Function

    ' ******************************************************
    '   Method: setEndtPosition
    '   In: new end angle position
    '   Out: value of end position register
    '   Description: sets a value in end position register.
    '   If no value is provided, method will read position of
    '   magnet.  
    ' ******************************************************
    Function AMS_5600_setEndPosition( AMS_5600_endAngle As Integer ) as Word
    ' {
    '   word _rawEndAngle;
    '   if (endAngle == -1)
    '     _rawEndAngle = getRawAngle();
    '   else
    '     _rawEndAngle = endAngle;

    '   writeOneByte(_addr_mpos, highByte(_rawEndAngle));
    '   delay(2);
    '   writeOneByte(_addr_mpos+1, lowByte(_rawEndAngle));
    '   delay(2);
    '   word _mPosition = readTwoBytesSeparately(_addr_mpos);

    '   return (_mPosition);
    ' }

        Dim __AMS_5600_rawEndAngle AS Word

        If AMS_5600_endAngle = -1 Then
            __AMS_5600_rawEndAngle = AMS_5600_getRawAngle()
        Else
            __AMS_5600_rawEndAngle = AMS_5600_endAngle
        End If
        
        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_MPOS )
        HI2CSend( [Byte]__AMS_5600_rawEndAngle_h )
        HI2CStop
        wait 2 ms
        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_MPOS+1 )
        HI2CSend( [Byte]__AMS_5600_rawEndAngle )
        HI2CStop
        wait 2 ms 

        AMS_5600_setEndPosition = AMS_5600_getEndPosition()

    End Function

' ******************************************************
'   Method: getEndPosition
'   In: none
'   Out: value of end position register
'   Description: gets value of end position register.
' ******************************************************
Function AMS_5600_getEndPosition() As Word

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_MPOS )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getEndPosition_h )   
        HI2CReceive( [byte]AMS_5600_getEndPosition, NACK ) 
        HI2CStop

    End Function
' *******************************************************
'   Method: getRawAngle
'   In: none
'   Out: value of raw angle register
'   Description: gets raw value of magnet position.
'   start, end, and max angle settings do not apply
' *******************************************************
Function AMS_5600_getRawAngle() As Word

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_RAW_ANGLE )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getRawAngle_h )   
        HI2CReceive( [byte]AMS_5600_getRawAngle, NACK ) 
        HI2CStop

    End Function
' ******************************************************
'   Method: getScaledAngle
'   In: none
'   Out: value of scaled angle register
'   Description: gets scaled value of magnet position.
'   start, end, or max angle settings are used to 
'   determine value
' ******************************************************
Function AMS_5600_getScaledAngle() As Word

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( _ADDR_ANGLE )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getScaledAngle_h )   
        HI2CReceive( [byte]AMS_5600_getScaledAngle, NACK ) 
        HI2CStop

    End Function

' ******************************************************
'   Method: detectMagnet
'   In: none
'   Out: 1 if magnet is detected, 0 if not
'   Description: reads status register and examines the 
'   MH bit
' ******************************************************
Function AMS_5600_detectMagnet() as Byte

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_STATUS )  
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_detectMagnet, NACK ) 
        HI2CStop

        If AMS_5600_detectMagnet & 0x20 = 0x20 then
            AMS_5600_detectMagnet = 1
        Else
            AMS_5600_detectMagnet = 0
        End if

    End Function
' ******************************************************
'   Method: getMagnetStrength
'   In: none
'   Out: 0 if no magnet is detected
'        1 if magnet is to weak
'        2 if magnet is just right
'        3 if magnet is to strong
'   Description: reads status register andexamins the MH,ML,MD bits
' ******************************************************
Function AMS_5600_getMagnetStrength()

        Dim __AMS_5600_magStatus as Integer

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_STATUS )  
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]__AMS_5600_magStatus, NACK ) 
        HI2CStop

        If __AMS_5600_magStatus & 0x20 = 0x20 then
            AMS_5600_getMagnetStrength = 2 'just right
        Else If __AMS_5600_magStatus & 0x10 = 0x10 then
            AMS_5600_getMagnetStrength = 1 'too weak
        Else If __AMS_5600_magStatus & 0x08 = 0x08 then
            AMS_5600_getMagnetStrength = 1 'too strong
        Else
            AMS_5600_getMagnetStrength = 0  'error
        End if


    End Function
' ******************************************************
'   Method: get Agc
'   In: none
'   Out: value of AGC register
'   Description: gets value of AGC register.
' ******************************************************
Function AMS_5600_getAgc() 

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_AGC )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getAgc, NACK ) 
        HI2CStop

    End Function
' ******************************************************
'   Method: getMagnitude
'   In: none
'   Out: value of magnitude register
'   Description: gets value of magnitude register.
' ******************************************************
Function AMS_5600_getMagnitude() as Word

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( _ADDR_MAGNITUDE )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getMagnitude_h )   
        HI2CReceive( [byte]AMS_5600_getMagnitude, NACK ) 
        HI2CStop

        AMS_5600_getMagnitude = AMS_5600_getMagnitude & 0x0FFF 'MASK OFF THE UPPER BITS

    End Function
' ******************************************************
'   Method: getConf
'   In: none
'   Out: value of CONF register 
'   Description: gets value of CONF register.
' ******************************************************
Function AMS_5600_getConf() As Word

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_CONF )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getConf_h )   
        HI2CReceive( [byte]AMS_5600_getConf, NACK ) 
        HI2CStop

    End Function
' ******************************************************
'   Method: setConf
'   In: value of CONF register
'   Out: none
'   Description: sets value of CONF register.
' ******************************************************
Sub AMS_5600_setConf( in AMS_5600_conf as Word )

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_CONF )
        HI2CSend( [BYTE]AMS_5600_conf_h )
        HI2CStop
        Wait 2 ms 

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_CONF + 1 )
        HI2CSend( [BYTE]AMS_5600_conf )
        HI2CStop
        Wait 2 ms 

    End Sub
' *******************************************************
'   Method: getBurnCount
'   In: none
'   Out: value of zmco register
'   Description: determines how many times chip has been
'   permanently written to. 
' *******************************************************
Function AMS_5600_getBurnCount()

        HI2CStart
        HI2CSend( AMS5600_ADDRESS ) 
        HI2CSend( AMS5600_ADDR_ZMCO )
        HI2CReStart

        HI2CSend( AMS5600_ADDRESS+1 )
        HI2CReceive( [byte]AMS_5600_getBurnCount, NACK ) 
        HI2CStop

    End Function
' ******************************************************
'   Method: burnAngle
'   In: none
'   Out: 1 success
'       -1 no magnet
'       -2 burn limit exceeded
'       -3 start and end positions not set (useless burn)
'   Description: burns start and end positions to chip.
'   THIS CAN ONLY BE DONE 3 TIMES
' ******************************************************
Function AMS_5600_burnAngle()
    ' {
    '   word _zPosition = getStartPosition();
    '   word _mPosition = getEndPosition();
    '   word _maxAngle = getMaxAngle();

    '   int retVal = 1;
    '   if (detectMagnet() == 1) {
    '     if (getBurnCount() < 3) {
    '       if ((_zPosition == 0) && (_mPosition == 0))
    '         retVal = -3;
    '       else
    '         writeOneByte(_addr_burn, 0x80);
    '     }
    '     else
    '       retVal = -2;
    '   } else
    '     retVal = -1;

    '   return retVal;
    ' }
    End Function
' ******************************************************
'   Method: burnMaxAngleAndConfig
'   In: none
'   Out: 1 success
'       -1 burn limit exceeded
'       -2 max angle is to small, must be at or above 18 degrees
'   Description: burns max angle and config data to chip.
'   THIS CAN ONLY BE DONE 1 TIME
' ******************************************************
Function AMS_5600_burnMaxAngleAndConfig()
    ' {
    '   word _maxAngle = getMaxAngle();

    '   int retVal = 1;
    '   if (getBurnCount() == 0) {
    '     if (_maxAngle * 0.087 < 18)
    '       retVal = -2;
    '     else
    '       writeOneByte(_addr_burn, 0x40);
    '   }
    '   else
    '     retVal = -1;

    '   return retVal;
    ' }
    End Function

