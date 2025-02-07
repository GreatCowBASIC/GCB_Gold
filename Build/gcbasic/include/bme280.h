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

'********************************************************************************
'    Bosch BME280/BMP280  routines for GCBASIC
'   This is a library for the BME280 humidity, temperature & pressure sensor. It
'   only supports the I2C bus. It does not support the SPI bus connection.
'    Copyright (C) 2006 -2025 Hugh Considine,  Evan R. Venn, Mike Otte,
'   David Thomson, David Stephenson, Theo, Thomas Henry
'
'   24/09/2019   Change the library to I2CReceive BME280_DataBuffer( BME280_DataBuffer_Pointer) , ACK

'
'There are the public methods that the calling program will use:
'BME280_GetID               'BME280 = 0x60 ,BMP280 0x58
'BME280_SoftReset
'BME280_GetStatus           ' flags for when it is sampling
'BME280_SetConfig           ' sets time between samples and IIR filter,and spimode
'BME280_GetConfig
'BME280_SetCtrlMeasurement  'Sets temp,Press oversampling and sensor mode
'BME280_GetCtrlMeasurement
'BME280_SetCtrlHumidity     'sets Humidity oversampling
'BME280_GetCtrlHumidity
'BME280_GetTemperature  'Public method - reads all adc's ,applies calibration parms in calculations, returns Temperature
'BME280_GetPressure 'Public method - reads all adc's ,applies calibration parms in calculations, returns Pressure
'BME280_GetHumidity 'Public method - reads all adc's ,applies calibration parms in calculations, returns Humidity
'
'Be sure to define your altitude using the BME280_AltitudeAdjustment
' Altitude Adjustment = 1.2 hPa * altitude in Meters  *10
'#define BME280_AltitudeAdjustment  3240 'for my 270m altitude
'
'
'Private methods
'BME280__Init       'Private -  Runs automatically at startup
'BME280_Read_TPH    'Private method used by GetTemp,GetPress,GetHum to read adc values

'The calling program may also set the following constants.
'See the descriptions below for their meaning and defaults.
'

'  #define BME280_Debug 1   'To check the calibration constants

'----- Constants

#define BME280_I2C_ADDRESS  0xEC  '0xED for read BME280 device address
' could also be 0x76    0x77 for read

' ****************************************************************************
'                              COMMON USED CONSTANTS
' ****************************************************************************
'      BME280_CHIP_ID                                       0x60
'      BMP280_CHIP_ID      samples= 0x56 , 0x57,production =0x58
'
' ****************************************************************************
'                             POWER MODE DEFINITIONS
' ****************************************************************************
'  Sensor Specific constants
#define BME280_SLEEP_MODE                                           0x00
#define BME280_FORCED_MODE                                          0x01
#define BME280_NORMAL_MODE                                          0x03
#define BME280_SOFT_RESET                                           0xB6
' ****************************************************************************
'                              STANDBY DEFINITIONS
' ****************************************************************************
#define BME280_STANDBY_TIME_1_MS                                    0x00
#define BME280_STANDBY_TIME_63_MS                                   0x01
#define BME280_STANDBY_TIME_125_MS                                  0x02
#define BME280_STANDBY_TIME_250_MS                                  0x03
#define BME280_STANDBY_TIME_500_MS                                  0x04
#define BME280_STANDBY_TIME_1000_MS                                 0x05
#define BME280_STANDBY_TIME_10_MS                                   0x06
#define BME280_STANDBY_TIME_20_MS                                   0x07
' ****************************************************************************
'                            OVER SAMPLING DEFINITIONS
' ****************************************************************************
#define BME280_OVERSAMP_SKIPPED                                     0x00
#define BME280_OVERSAMP_1X                                          0x01
#define BME280_OVERSAMP_2X                                          0x02
#define BME280_OVERSAMP_4X                                          0x03
#define BME280_OVERSAMP_8X                                          0x04
#define BME280_OVERSAMP_16X                                         0x05
' ****************************************************************************
'                            WORK MODE DEFINITIONS
' ****************************************************************************

#define BME280_ULTRALOWPOWER_MODE                                   0x00
#define BME280_LOWPOWER_MODE                                        0x01
#define BME280_STANDARDRESOLUTION_MODE                              0x02
#define BME280_HIGHRESOLUTION_MODE                                  0x03
#define BME280_ULTRAHIGHRESOLUTION_MODE                             0x04

#define BME280_ULTRALOWPOWER_OSRS_P                                 BME280_OVERSAMP_1X
#define BME280_ULTRALOWPOWER_OSRS_T                                 BME280_OVERSAMP_1X

#define BME280_LOWPOWER_OSRS_P                                      BME280_OVERSAMP_2X
#define BME280_LOWPOWER_OSRS_T                                      BME280_OVERSAMP_1X

#define BME280_STANDARDRESOLUTION_OSRS_P                            BME280_OVERSAMP_4X
#define BME280_STANDARDRESOLUTION_OSRS_T                            BME280_OVERSAMP_1X

#define BME280_HIGHRESOLUTION_OSRS_P                                BME280_OVERSAMP_8X
#define BME280_HIGHRESOLUTION_OSRS_T                                BME280_OVERSAMP_1X

#define BME280_ULTRAHIGHRESOLUTION_OSRS_P                           BME280_OVERSAMP_16X
#define BME280_ULTRAHIGHRESOLUTION_OSRS_T                           BME280_OVERSAMP_2X

#define BME280_STANDARD_OVERSAMP_HUMIDITY                           BME280_OVERSAMP_1X
' ****************************************************************************
'                            FILTER DEFINITIONS
' ****************************************************************************
#define BME280_FILTER_COEFF_OFF                                     0x00
#define BME280_FILTER_COEFF_2                                       0x01
#define BME280_FILTER_COEFF_4                                       0x02
#define BME280_FILTER_COEFF_8                                       0x03
#define BME280_FILTER_COEFF_16                                      0x04
' ****************************************************************************
'                           DELAY DEFINITIONS
' ****************************************************************************
#define T_INIT_MAX                                                  20 '  20/16 = 1.25 ms
#define T_MEASURE_PER_OSRS_MAX                                      37 '  37/16 = 2.3125 ms
#define T_SETUP_PRESSURE_MAX                                        10 '  10/16 = 0.625 ms
#define T_SETUP_HUMIDITY_MAX                                        10 '  10/16 = 0.625 ms



' ****************************************************************************
'                CALIBRATION REGISTER ADDRESS DEFINITIONS
' ****************************************************************************
' calibration parameters
#define BME280_TEMPERATURE_CALIB_DIG_T1_LSB_REG                     0x88
#define BME280_PRESSURE_CALIB_DIG_P1_LSB_REG                        0x8E
#define BME280_HUMIDITY_CALIB_DIG_H1_REG                            0xA1
#define BME280_HUMIDITY_CALIB_DIG_H2_LSB_REG                        0xE1

' ****************************************************************************
'                        REGISTER ADDRESS DEFINITIONS
' ****************************************************************************
#define BME280_CHIP_ID_REG                                          0xD0  ' Chip ID Register
#define BME280_RST_REG                                              0xE0  ' Softreset Register
#define BME280_STAT_REG                                             0xF3  ' Status Register
#define BME280_CTRL_MEAS_REG                                        0xF4  ' Ctrl Measure Register
#define BME280_CTRL_HUMIDITY_REG                                    0xF2  ' Ctrl Humidity Register
#define BME280_CONFIG_REG                                           0xF5  ' Configuration Register
#define BME280_PRESSURE_MSB_REG                                     0xF7  ' Pressure MSB Register
#define BME280_PRESSURE_LSB_REG                                     0xF8  ' Pressure LSB Register
#define BME280_PRESSURE_XLSB_REG                                    0xF9  ' Pressure XLSB Register
#define BME280_TEMPERATURE_MSB_REG                                  0xFA  ' Temperature MSB Reg
#define BME280_TEMPERATURE_LSB_REG                                  0xFB  ' Temperature LSB Reg
#define BME280_TEMPERATURE_XLSB_REG                                 0xFC  ' Temperature XLSB Reg
#define BME280_HUMIDITY_MSB_REG                                     0xFD  ' Humidity MSB Reg
#define BME280_HUMIDITY_LSB_REG                                     0xFE  ' Humidity LSB Reg
' ****************************************************************************

;The following constant is used to convert from station pressure
;to mean sea level pressure. It depends upon your altitude in
;meters. Compute the rounded value like this:
;(1/((1-altitude/44330)^5.255))*10000
; Altitude Adjustment = 1.2 hPa * altitude in Meters  *10
#define BME280_AltitudeAdjustment  3240      ' 3240 for my 270 meters altitude


#define BMP_OSS 3                 ;default to mode 3



;----- Variables


#startup BME280_Init

sub BME280_Init

  dim BME280_temp as byte
  dim BME280_DataBuffer ( 18 )
  dim BME280_LongTemp,bmp_copy1,bmp_copy2 as long
  'dim dig_T1 as word
  'dim dig_T2 ,dig_T3 as Integer
  dim dig_T1,dig_T2 ,dig_T3 as Long
  'dim dig_P1  as word
  'dim  dig_P2, dig_P3, dig_P4, dig_P5, dig_P6, dig_P7, dig_P8, dig_P9   as integer
  dim dig_P1, dig_P2, dig_P3, dig_P4, dig_P5, dig_P6, dig_P7, dig_P8, dig_P9   as Long
  'dim dig_H1,dig_H3,dig_H6 as byte
  'dim dig_H2, dig_H4, dig_H5 as Integer
  dim dig_H1,dig_H2, dig_H3,dig_H4, dig_H5,dig_H6 as Long

  #ifdef HI2C_DATA
      HI2CMode Master
  #endif
  wait 1 s

  BME280_WriteData ( BME280_CTRL_HUMIDITY_REG, BME280_OVERSAMP_1X  )
  BME280_WriteData ( BME280_CTRL_MEAS_REG , 0x27 )  'press x1, temp x1  ,normal
  BME280_WriteData ( BME280_CONFIG_REG, 0XA0 )     'Standby 1000ms, Filter off

  BME280_ReadData  ( BME280_TEMPERATURE_CALIB_DIG_T1_LSB_REG, 6, BME280_DataBuffer() ) 'read 6 bytes into the buffer
  'dig_T1   = BME280_DataBuffer(0)
  'dig_T1_H = BME280_DataBuffer(1)
  dig_T1 = BMPcastUnsigned(BME280_DataBuffer(1), BME280_DataBuffer(0))
  'dig_T2   = BME280_DataBuffer(2)
  'dig_T2_H = BME280_DataBuffer(3)
  dig_T2 = BMPcastSigned(BME280_DataBuffer(3), BME280_DataBuffer(2))
  'dig_T3   = BME280_DataBuffer(4)
  'dig_T3_H = BME280_DataBuffer(5)
  dig_T3 = BMPcastSigned(BME280_DataBuffer(5), BME280_DataBuffer(4))

  BME280_ReadData  ( BME280_PRESSURE_CALIB_DIG_P1_LSB_REG, 18, BME280_DataBuffer() ) 'read 18 bytes into the buffer
  'dig_P1   = BME280_DataBuffer(0)
  'dig_P1_H = BME280_DataBuffer(1)
  dig_P1 = BMPcastUnsigned(BME280_DataBuffer(1), BME280_DataBuffer(0))
  'dig_P2   = BME280_DataBuffer(2)
  'dig_P2_H = BME280_DataBuffer(3)
  dig_P2 = BMPcastSigned(BME280_DataBuffer(3), BME280_DataBuffer(2))
  'dig_P3   = BME280_DataBuffer(4)
  'dig_P3_H = BME280_DataBuffer(5)
  dig_P3 = BMPcastSigned(BME280_DataBuffer(5), BME280_DataBuffer(4))
  'dig_P4   = BME280_DataBuffer(6)
  'dig_P4_H = BME280_DataBuffer(7)
  dig_P4 = BMPcastSigned(BME280_DataBuffer(7), BME280_DataBuffer(6))
  'dig_P5   = BME280_DataBuffer(8)
  'dig_P5_H = BME280_DataBuffer(9)
  dig_P5 = BMPcastSigned(BME280_DataBuffer(9), BME280_DataBuffer(8))
  'dig_P6   = BME280_DataBuffer(10)
  'dig_P6_H = BME280_DataBuffer(11)
  dig_P6 = BMPcastSigned(BME280_DataBuffer(11), BME280_DataBuffer(10))
  'dig_P7   = BME280_DataBuffer(12)
  'dig_P7_H = BME280_DataBuffer(13)
  dig_P7 = BMPcastSigned(BME280_DataBuffer(13), BME280_DataBuffer(12))
  'dig_P8   = BME280_DataBuffer(14)
  'dig_P8_H = BME280_DataBuffer(15)
  dig_P8 = BMPcastSigned(BME280_DataBuffer(15), BME280_DataBuffer(14))
  'dig_P9   = BME280_DataBuffer(16)
  'dig_P9_H = BME280_DataBuffer(17)
  dig_P9 = BMPcastSigned(BME280_DataBuffer(17), BME280_DataBuffer(16))

  BME280_ReadByte ( BME280_HUMIDITY_CALIB_DIG_H1_REG,  BME280_DataBuffer(0)) 'dig_H1 )
  dig_H1 = BMPcastUnsigned(0x00, BME280_DataBuffer(0))

  BME280_ReadData  ( BME280_HUMIDITY_CALIB_DIG_H2_LSB_REG, 7, BME280_DataBuffer() ) 'read 8 bytes into the buffer
  'dig_H2   = BME280_DataBuffer(0)
  'dig_H2_H = BME280_DataBuffer(1)
  dig_H2 = BMPcastSigned(BME280_DataBuffer(1), BME280_DataBuffer(0))
  'dig_H3   = BME280_DataBuffer(2)
  dig_H3 = BMPcastUnsigned(0x00, BME280_DataBuffer(2))
  dig_H4  = BME280_DataBuffer(3)
  dig_H4 = dig_H4 *16             'shift left 4 places
  dig_H4   = dig_H4 + (BME280_DataBuffer(4) AND 0x0F)
  'dig_H4 = BMPcastSigned( 0x00 , BME280_DataBuffer(3))  ' could not get these to work
  'BMPshiftL( dig_H4 , 4)
  'dig_H4 = dig_H4 + (BME280_DataBuffer(4) AND 0x0F)
  dig_H5 =  BME280_DataBuffer(5)
  dig_H5 = dig_H5 * 16
  dig_H5   = dig_H5 + ((BME280_DataBuffer(4) AND 0xF0)/16)
  'dig_H5 = BMPcastSigned( 0x00 , BME280_DataBuffer(5))  ' could not get these to work
  'BMPshiftL( dig_H54 , 4)
  'dig_H5 = dig_H5 + (BME280_DataBuffer(4) AND 0xF0)/16
  'dig_H6   = BME280_DataBuffer(6)
  dig_H6 = BMPcastUnsigned(0x00, BME280_DataBuffer(6))

  #ifdef BME280_Debug
    'PrintTempParms
    'PrintPressParms
    PrintHumParms
  #endif
end Sub



function BME280_GetID as byte

    dim BME280_temp as byte
    BME280_ReadByte ( BME280_CHIP_ID_REG, BME280_temp )
    BME280_GetID = BME280_temp

end function

sub BME280_SoftReset

  BME280_WriteData ( BME280_RST_REG, BME280_SOFT_RESET )

end sub
'
' Register 0xF4 �ctrl_meas�
'Bit 7, 6, 5 osrs_t[2:0]Controls oversampling of temperature data.
'Bit 4, 3, 2 osrs_p[2:0]Controls oversampling of pressure data.
'Bit 1, 0 mode[1:0] Controls the sensor mode of the device.
' is the sampling rate  which can smooth the data
'000 is no sample and outputs 0x80000000
'001 is x1   no averaging, 010 x2, 011 x4...
'mode[1:0] Mode
'00 Sleep mode
'01 and 10 Forced mode
'11 Normal mode
'ctrl_meas must be written after ctrl hum
sub BME280_SetCtrlMeasurement( in BME280_temp as byte)

  BME280_WriteData ( BME280_CTRL_MEAS_REG, BME280_temp )

end sub

function BME280_GetCtrlMeasurement as byte

    dim BME280_temp as byte
    BME280_ReadByte ( BME280_CTRL_MEAS_REG, BME280_temp )
    BME280_GetCtrlMeasurement = BME280_temp

end function
'
' CTRL_Hum reg  0xF2  bits 0-3
' is the sampling rate  which can smooth the data
'000 is no sample and outputs 0x80000000
'001 is x1   no averaging, 010 x2, 011 x4...
'ctrl_meas must be written last
sub BME280_SetCtrlHumidity( in BME280_temp as byte)

  BME280_WriteData ( BME280_CTRL_HUMIDITY_REG, BME280_temp )

end sub

function BME280_GetCtrlHumidity as byte

    dim BME280_temp as byte
    BME280_ReadByte ( BME280_CTRL_HUMIDITY_REG, BME280_temp )
    BME280_GetCtrlHumidity = BME280_temp

end function
'
'Register 0xF5 �config�
'Bit 7, 6, 5 t_sb[2:0]Controls inactive duration tstandby in normal mode.
'Bit 4, 3, 2 filter[2:0] Controls the time constant of the IIR filter.
'Bit 0 spi3w_en[0]Enables 3-wire SPI interface when set to �1�.
'
sub BME280_SetConfig( in BME280_temp as byte)

  BME280_WriteData ( BME280_CONFIG_REG, BME280_temp )

end sub

function BME280_GetConfig as byte

    dim BME280_temp as byte
    BME280_ReadByte ( BME280_CONFIG_REG, BME280_temp )
    BME280_GetConfig = BME280_temp

end function
'
'Status register only has two active bits
'bit 3 is conversion in process
'bit 0 is im_update
function BME280_GetStatus as byte

    dim BME280_temp as byte
    BME280_ReadByte ( BME280_STAT_REG, BME280_temp )
    BME280_GetStatus = BME280_temp

end function

'sub BME280_GetAll (out BME280_Temperature as integer, out BME280_Pressure as long, out BME280_Humidity as long)
'    BME280_Read_TPH                         ' burst read  all adc registers
'    BME280_Temperature = BME280_CalcTemp
'    BME280_Pressure = BME280_CalcPress
'    BME280_Humidity = BME280_CalcHumidity

'end sub

function BME280_GetTemperature as integer
    BME280_Read_TPH                         ' burst read  all adc registers
    BME280_GetTemperature = BME280_CalcTemp
end Function

function BME280_GetPressure as long
    Dim BME280_TempW as long
    BME280_Read_TPH                           ' burst read  all adc registers
    BME280_TempW = BME280_CalcTemp            ' Need temperature calculated to calc press
    BME280_GetPressure = BME280_CalcPress
end Function

function BME280_GetHumidity as long
    Dim BME280_TempW as long
    BME280_Read_TPH                         ' burst read  all adc registers
    BME280_TempW = BME280_CalcTemp           ' Need temperature calculated to calc humidity
    BME280_GetHumidity = BME280_CalcHumidity
end Function


sub BME280_Read_TPH

  dim p_msb, p_lsb,p_xlsb,t_msb ,t_lsb ,t_xlsb, h_msb, h_lsb as Byte
  dim adc_P, adc_T as long
  dim adc_H as word
  dim BME280_DataBuffer ( 18 )

  BME280_ReadData  ( 0xF7, 8, BME280_DataBuffer() ) 'read 8 bytes into the buffer
  p_msb   = BME280_DataBuffer(0)
  p_lsb = BME280_DataBuffer(1)
  p_xlsb   = BME280_DataBuffer(2)
  t_msb  = BME280_DataBuffer(3)
  t_lsb   = BME280_DataBuffer(4)
  t_xlsb = BME280_DataBuffer(5)
  h_msb = BME280_DataBuffer(6)
  h_lsb = BME280_DataBuffer(7)


        ' convert to one long from 3 bytes
        ' lowb = workvariable
        ' highb = workvariable_H
        'upperb = workvariable_U
        'lastb = workvariable_E
         adc_P   = P_xlsb
         adc_P_H = P_lsb
         adc_P_U = P_msb
         adc_P = adc_P /16    ' shift to the right 4 places

         adc_T   = t_xlsb
         adc_T_H = t_lsb
         adc_T_U = t_msb
         adc_T = adc_T/16     ' shift to the right 4 places

         adc_H    = h_lsb
         adc_H_H  = h_msb
end sub


function  BME280_CalcTemp as long
        dim dig_T1 as long 'word
        dim dig_T2 ,dig_T3 as long 'Integer
        dim var1, var2, t_fine, BME280_LongTemp as long
        dim adc_P, adc_T as long
        dim adc_H as long

        'var1 = ((adc_T/8)-(dig_T1 *2)) '* dig_T2) /2048
        var1 = BMPshiftL(dig_T1,1)
        BME280_LongTemp = BMPshiftR(adc_T, 3) - var1
        var1 = BMPsignedLongMult(BME280_LongTemp, dig_T2)
        'var1 = var1 * dig_T2
        var1 = BMPshiftR(var1,11)

        'var2 = (((((adc_T/16)-dig_T1) * ((adc_T/16)-dig_T1))/4096)*dig_T3)/16384
        BME280_LongTemp =BMPshiftR(adc_T, 4) - dig_T1
         BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, BME280_LongTemp)
         BME280_LongTemp = BMPshiftR(BME280_LongTemp, 12)
        var2 = BMPsignedLongMult(BME280_LongTemp, dig_T3)
        var2 = BMPshiftR( var2, 14)

        t_fine = var1 + var2
        'HSerPrint "  t_fine = "
        'HSerPrint t_fine
        'HSerPrintCRLF
        BME280_CalcTemp  = (5 * t_fine +128)/256

end function



function BME280_CalcPress as long
      dim dig_P1  as long
      dim  dig_P2, dig_P3, dig_P4, dig_P5, dig_P6, dig_P7, dig_P8, dig_P9   as long
      dim Press as long
      dim var1, var2, t_fine, BME280_LongTemp, BME280_LongTemp2 as long
      dim adc_P, adc_T as long
      dim adc_H as long

      'BME280_temp = BME280_CalcTemp   ' need the temperature so call it to make sure

      var1 = BMPshiftR(t_fine,1) - 64000

      BME280_LongTemp = BMPshiftR(var1,2)
      BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, BME280_LongTemp)
      BME280_LongTemp = BMPshiftR(BME280_LongTemp,11)
      var2 = BMPsignedLongMult(BME280_LongTemp, dig_P6)

      BME280_LongTemp = BMPsignedLongMult(var1, dig_P5)
      BME280_LongTemp = BMPshiftL(BME280_LongTemp,1)

      var2 = var2 + BME280_LongTemp

      BME280_LongTemp2 = BMPshiftR(var2,2)
      BME280_LongTemp = BMPshiftL(dig_P4,16)
      var2 = BME280_LongTemp + BME280_LongTemp2
      ' var1 = ((( dig_p3
      BME280_LongTemp = BMPshiftR(var1,2)
      BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, BME280_LongTemp)
      BME280_LongTemp = BMPshiftR(BME280_LongTemp,13)
      BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, dig_P3)
      BME280_LongTemp = BMPshiftR(BME280_LongTemp,3)
      ' + ...dig_P2
      BME280_LongTemp2 = BMPsignedLongMult(var1, dig_P2)
      BME280_LongTemp2 = BMPshiftR(BME280_LongTemp2,1)
      var1 = BME280_LongTemp + BME280_LongTemp2
      var1 = BMPshiftR(var1,18)
      ' var1 =      dig_P1
      BME280_LongTemp = var1 + 32768
      BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, dig_P1)
      var1 =  BMPshiftR(BME280_LongTemp,15)

      'Press = ((1048576 - adc_P)-(var2/4096) )*3125
        BME280_LongTemp = BMPshiftR(var2,12)
        Press = 1048576 - adc_P - BME280_LongTemp
        Press = BMPsignedLongMult(Press, 3125)

      'If Press < 0x80000000 Then
         Press = (Press*2)/var1
      'Else
        'Press = (Press/var1) *2
      'end if
      'var1 = (dig_P9 * (((Press/8) *(Press/8))/8192))/4096
      BME280_LongTemp = BMPshiftR(Press,3)
      BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, BME280_LongTemp)
      BME280_LongTemp = BMPshiftR(BME280_LongTemp,13)
      BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, dig_P9)
      var1 = BME280_LongTemp = BMPshiftR(BME280_LongTemp,12)

      'var2 =((Press/4) * dig_P8)/8192
      BME280_LongTemp = BMPshiftR(Press,2)
      BME280_LongTemp = BMPsignedLongMult(BME280_LongTemp, dig_P8)
      var2 = BMPshiftR(BME280_LongTemp,13)
      'Press = Press + ((var2 + var1 + dig_P7)/16)
      BME280_LongTemp = var1 + var2 + dig_P7
       Press = BMPshiftR(Press,4) + Press
      BME280_CalcPress =  Press - BME280_AltitudeAdjustment   ' 3240  ' altitude adj
end function

function BME280_CalcHumidity as long
    dim  t_fine as long
    dim dig_H1,dig_H3,dig_H6 as long
    dim dig_H2, dig_H4, dig_H5 as long
    dim v_x1_u32r, v_x1_u32s, v_x1_u32t, v_x1_u32u as long
    dim adc_P, adc_T as long
    dim adc_H as long
    'BME280_temp = BME280_CalcTemp
    v_x1_u32r = t_fine - 76800

    'v_x1_u32s = (adc_H*16384)'-(dig_H4*1048576)'-(dig_H5 * v_x1_u32r))+16384)/32768
    v_x1_u32s = BMPshiftL(adc_H,14)
    'v_x1_u32t = (dig_H4*1048576)
    v_x1_u32t = BMPsignedLongMult(1048576, dig_H4)
    v_x1_u32u = v_x1_u32s - v_x1_u32t
    v_x1_u32t = BMPsignedLongMult(v_x1_u32r, dig_H5)
    v_x1_u32s = (v_x1_u32u - v_x1_u32t +16384) '/32768
    v_x1_u32s = BMPshiftR(v_x1_u32s,15)

    v_x1_u32t = BMPsignedLongMult(v_x1_u32r, dig_H6)
    v_x1_u32t = BMPshiftR(v_x1_u32t,10)
    v_x1_u32u = BMPsignedLongMult(v_x1_u32r, dig_H3)
    v_x1_u32u = BMPshiftR(v_x1_u32u,11)
    v_x1_u32t = BMPsignedLongMult(v_x1_u32t, v_x1_u32u) +32768
    v_x1_u32t = BMPshiftR(v_x1_u32t,10) + 2097152
    v_x1_u32t = BMPsignedLongMult(v_x1_u32t, dig_H2) +8192
    v_x1_u32t = BMPshiftR(v_x1_u32t,14)
    'v_x1_u32t = ((((((v_x1_u32r*dig_H6)/1024)*(((v_x1_u32r *dig_H3)/2048)+32768))/1024)+2097152)*dig_H2+8192)/32768
    'v_x1_u32r = v_x1_u32s * v_x1_u32t
    v_x1_u32r = BMPsignedLongMult(v_x1_u32s, v_x1_u32t)
    v_x1_u32t = BMPshiftR(v_x1_u32r,15)
    v_x1_u32t = BMPsignedLongMult(v_x1_u32t, v_x1_u32t)
    v_x1_u32t = BMPshiftR(v_x1_u32t,7)
    v_x1_u32t = BMPsignedLongMult(dig_H1, v_x1_u32t)
    v_x1_u32t = BMPshiftR(v_x1_u32t,4)

    v_x1_u32r = v_x1_u32r - v_x1_u32t

    'v_x1_u32r = (v_x1_u32r-((((v_x1_u32r/32768)*(v_x1_u32r/32768))/128)*dig_H1/16))
    If v_x1_u32r <0 then v_x1_u32r = 0
    'If v_x1_u32r >419430400 then v_x1_u32r = 419430400
    v_x1_u32r = v_x1_u32r/4096
    'v_x1_u32r = v_x1_u32r/1024
    BME280_CalcHumidity =  v_x1_u32r
end function


;----- Low Level Subroutines


sub BME280_WriteData ( in BME280Address,  in BME280SendByte as byte )

      #ifdef I2C_DATA
          I2CStart
          I2CSend(BME280_I2C_ADDRESS)                 ;Send address
          I2CSend(BME280Address)
          I2CSend(BME280SendByte)           ;Send data
          I2CStop
      #endif

      #ifdef HI2C_DATA
          HI2CStart
          HI2CSend(BME280_I2C_ADDRESS)
          HI2CSend(BME280Address)
          HI2CSend(BME280SendByte)
          HI2CStop
      #endif

end sub

sub BME280_ReadData ( in BME280Address,  in BME280GetByteData as byte, out BME280_DataBuffer() )
      BME280GetByteData--
      #ifdef I2C_DATA
          I2CStart
          I2CSend(BME280_I2C_ADDRESS)                 ;Send address
          I2CSend(BME280Address)
          I2CReStart
          I2CSend(BME280_I2C_ADDRESS+1)               ;indicate a read operation
          For BME280_DataBuffer_Pointer = 0 to (BME280GetByteData -1)
              I2CReceive BME280_DataBuffer( BME280_DataBuffer_Pointer), ACK
          next
          I2CReceive BME280_DataBuffer( BME280GetByteData ) , NACK
          I2CStop
      #endif

      #ifdef HI2C_DATA
          HI2CStart
          HI2CSend(BME280_I2C_ADDRESS)
          HI2CSend(BME280Address)
          HI2CReStart
          HI2CSend(BME280_I2C_ADDRESS+1)               ;indicate a read operation

          For BME280_DataBuffer_Pointer = 0 to (BME280GetByteData -1)
              HI2CReceive BME280_DataBuffer( BME280_DataBuffer_Pointer), ACK
          next
          HI2CReceive BME280_DataBuffer( BME280GetByteData  ), NACK
          HI2CStop
      #endif

end sub

sub BME280_ReadByte ( in BME280Address,  out BME280OutByteData )

      #ifdef I2C_DATA
          I2CStart
          I2CSend(BME280_I2C_ADDRESS)                 ;Send address
          I2CSend(BME280Address)
          I2CReStart
          I2CSend(BME280_I2C_ADDRESS+1)               ;indicate a read operation
          I2CReceive BME280OutByteData, NACK
          I2CStop
      #endif

      #ifdef HI2C_DATA
          HI2CStart
          HI2CSend(BME280_I2C_ADDRESS)
          HI2CSend(BME280Address)
          HI2CReStart
          HI2CSend(BME280_I2C_ADDRESS+1)               ;indicate a read operation
          HI2CReceive BME280OutByteData, NACK
          HI2CStop
      #endif

end sub

sub PrintTempParms
      dim dig_T1,dig_T2 ,dig_T3 as Long
      HSerSend 9
      HSerPrint "Dig_T = "
      HSerPrint dig_T1: HSerSend ","
      HSerPrint dig_T2: HSerSend ","
      HSerPrint dig_T3: HSerPrintCRLF
end sub
sub PrintPressParms
      dim dig_P1, dig_P2, dig_P3, dig_P4, dig_P5 as Long
      dim dig_P6, dig_P7, dig_P8, dig_P9   as Long
      HSerSend 9
      HSerPrint "Dig_P = "
      HSerPrint dig_P1: HSerSend ","
      HSerPrint dig_P2: HSerSend ","
      HSerPrint dig_P3: HSerSend ","
      HSerPrint dig_P4: HSerSend ","
      HSerPrint dig_P5: HSerSend ","
      HSerPrint dig_P6: HSerSend ","
      HSerPrint dig_P7: HSerSend ","
      HSerPrint dig_P8: HSerSend ","
      HSerPrint dig_P9: HSerPrintCRLF
end Sub
sub PrintHumParms
      dim dig_H1,dig_H3,dig_H6, dig_H2, dig_H4, dig_H5 as Long
      HSerSend 9
      HSerPrint "Dig_H = "
      HSerPrint dig_H1: HSerSend ","
      HSerPrint dig_H2: HSerSend ","
      HSerPrint dig_H3: HSerSend ","
      HSerPrint dig_H4: HSerSend ","
      HSerPrint dig_H5: HSerSend ","
      HSerPrint dig_H6
      HSerPrintCRLF

end sub

  ;----- Low Level Subroutines


  function BMPcastSigned(msb as byte, lsb as byte) as long
    ;Convert a signed 16-bit number to signed 32-bit number.
    BMPcastSigned = lsb
    BMPcastSigned_h = msb
    if msb.7 then
      BMPcastSigned_u = 0xFF            ;negative number
      BMPcastSigned_e = 0xFF
    else
      BMPcastSigned_u = 0x00            ;non-negative number
      BMPcastSigned_e = 0x00
    end if
  end function

  function BMPcastUnsigned(msb as byte, lsb as byte) as long
    ;Convert an unsigned 16-bit number to signed 32-bit number.
    BMPcastUnsigned = lsb
    BMPcastUnsigned_h = msb
    BMPcastUnsigned_u = 0x00            ;always non-negative
    BMPcastUnsigned_e = 0x00
  end function



  function BMPshiftL(BMPparam as long, BMPplaces as byte) as long
    ;Left-shift a long number.
    BMPshiftL = FnLSL(BMPparam,BMPplaces)
  end function

  function BMPshiftR(BMPparam as long, BMPplaces as byte) as long
    ;Right-shift a long number.
    if BMPparam.31 then
       Repeat BMPplaces
        Set C On
        Rotate BMPparam Right
      End Repeat
    Else
       Repeat BMPplaces
        Set C Off
        Rotate BMPparam Right
       End Repeat
    end if
     BMPshiftR = BMPparam
   end function

  function BMPsignedLongMult(BMPop1 as long, BMPop2 as long) as long
    ;Multiply two signed long numbers.
    dim bmp_copy1 , bmp_copy2 as long
    bmp_copy1 = BMPop1
    bmp_copy2 = BMPop2

    if BMPop1.31 then
      bmp_copy1 = (not BMPop1) + 1      ;take absolute value
    end if

    if BMPop2.31 then
      bmp_copy2 = (not BMPop2) + 1      ;likewise here
    end if

    BMPsignedLongMult = bmp_copy1 * bmp_copy2
    bmp_MSB = BMPop1_e XOR BMPop2_e

    if bmp_MSB.7 then                   ;mixed signs mean negative
      BMPsignedLongMult = (not BMPsignedLongMult)+1
    end if

  end function
