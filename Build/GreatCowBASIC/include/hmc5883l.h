'    HMC5883 routines for the GCBASIC compiler'
'    Copyright (C) 2015-2020 Evan Venn.

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
'    Created Evan R Venn - Jan 2014 revised July 2015
'    Revised 11th Oct 2015

'    This support the address of the generic device 0011110b, therefore
'    I2C 8bit address of HMC5883L is 0x3C.
'    Change in the main program to alternative addresses, if required


' 11/08/2022   Library correct to remove duplicate or errant constants
' 14/08/22 Updated user changeable constants - no functional change

  #define HMC5883L_address 0x3C
  #define HMC5883L_CONFIG_REG_A_INIT_CODE_CRA 0x70
  #define HMC5883L_CONFIG_REG_A_INIT_CODE_CRB 0x0A

  'Optional
  '#define HMC5883L_getDataReady_override


  'Methods
'  HMC5883L_Init
'  HMC5883L_StartContinousRead
'  HMC5883L_ContinousRead ( device_x, device_y, device_z )
'  HMC5883L_StopContinousRead
'  HMC5883L_ReadXYZ ( device_x, device_y, device_z )
'  HMC5883L_getDataReady as byte
'
'  and,
'  HMC5883L_setConfigurationA ( HMC5883L_byte )
'  HMC5883L_setConfigurationB ( HMC5883L_byte )
'  HMC5883L_getConfigurationA  as byte
'  HMC5883L_getConfigurationA as byte
'  HMC5883L_getStatus as byte


  #define HMC5883L_CONFIG_REG_A                    0x00
  #define HMC5883L_CONFIG_REG_B                    0x01
  #define HMC5883L_MODE_REG                        0x02
  #define HMC5883L_DATA_OUT_X_MSB_REG              0x03
  #define HMC5883L_DATA_OUT_X_LSB_REG              0x04
  
  #define HMC5883L_DATA_OUT_Z_MSB_REG              0x05
  #define HMC5883L_DATA_OUT_Z_LSB_REG              0x06
  #define HMC5883L_DATA_OUT_Y_MSB_REG              0x07
  #define HMC5883L_DATA_OUT_Y_LSB_REG              0x08

  #define HMC5883L_STATUS_REG                      0x09

  ' Range gain settings in (+-)Ga
  ' Note: HMC5883L and HMC5883 have different recommended sensor field
  ' ranges; default corresponds to +-1.0 Ga and +-1.3 Ga, respectively
  '
  #define HMC5883L_RANGE_GAIN_OFFSET               0x05
  #define HMC5883L_RANGE_GAIN_DEFAULT              0x01
  #define HMC5883L_RANGE_GAIN_MAX                  0x07

  ' Device status
  #define HMC5883L_DATA_READY                      0x01
  #define HMC5883L_DATA_OUTPUT_LOCK                0x02
  ' Does not exist on HMC5883, not used
   #define HMC5883L_VOLTAGE_REGULATOR_ENABLED       0x04


  ' Mode register configuration
  #define HMC5883L_MODE_CONVERSION_CONTINUOUS      0x00
  #define HMC5883L_MODE_CONVERSION_SINGLE          0x01
  #define HMC5883L_MODE_IDLE                       0x02
  #define HMC5883L_MODE_SLEEP                      0x03
  #define HMC5883L_MODE_MASK                       0x03


  ' HMC5883L: Minimum data output rate
  ' HMC5883: Typical data output rate
  #define HMC5883L_RATE_OFFSET                     0x02
  #define HMC5883L_RATE_BITMASK                    0x1C
  #define HMC5883L_RATE_NOT_USED                   0x07

  ' Device measurement configuration
  #define HMC5883L_MEAS_CONF_NORMAL                0x00
  #define HMC5883L_MEAS_CONF_POSITIVE_BIAS         0x01
  #define HMC5883L_MEAS_CONF_NEGATIVE_BIAS         0x02
  #define HMC5883L_MEAS_CONF_NOT_USED              0x03
  #define HMC5883L_MEAS_CONF_MASK                  0x03



  ;Variables
  Dim HMC5883L_x As word
  Dim HMC5883L_y As word
  Dim HMC5883L_z As word



Sub HMC5883L_Init

  #ifdef I2C_DATA
     InitI2C
     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_A , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_A_INIT_CODE_CRA, ACK )
     I2CSTOP

     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_B , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_A_INIT_CODE_CRB, ACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA
     HI2CMode Master
     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_CONFIG_REG_A , ACK)
     HI2CSEND ( 0x70, ACK )
     HI2CSTOP

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_CONFIG_REG_B , ACK)
     HI2CSEND ( 0xA0, ACK )
     HI2CSTOP

  #endif

end sub





Sub HMC5883L_StartContinousRead( Optional DeviceOpMode As Byte = HMC5883L_MODE_CONVERSION_CONTINUOUS )

     Dim XHiByte alias HMC5883L_x_h
     Dim XLoByte alias HMC5883L_x
     Dim YHiByte alias HMC5883L_y_h
     Dim YLoByte alias HMC5883L_y
     Dim ZHiByte alias HMC5883L_z_h
     Dim ZLoByte alias HMC5883L_z


  #ifdef I2C_DATA

     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_MODE_REG , ACK)
     I2CSEND ( DeviceOpMode , ACK )
     I2CSTOP
     wait 6 MS

     I2CSTART
     I2CSEND ( HMC5883L_address )
     I2CSEND ( HMC5883L_DATA_OUT_X_MSB_REG )


  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_MODE_REG , ACK)
     HI2CSEND ( DeviceOpMode , ACK )
     HI2CSTOP
     wait 6 MS

     HI2CSTART
     HI2CSEND ( HMC5883L_address )
     HI2CSEND ( HMC5883L_DATA_OUT_X_MSB_REG )

  #endif

  end sub


Sub HMC5883L_ContinousRead ( out HMC5883L_x AS WORD , Out HMC5883L_y AS WORD , Out HMC5883L_z AS WORD )

     Dim XHiByte alias HMC5883L_x_h
     Dim XLoByte alias HMC5883L_x
     Dim YHiByte alias HMC5883L_y_h
     Dim YLoByte alias HMC5883L_y
     Dim ZHiByte alias HMC5883L_z_h
     Dim ZLoByte alias HMC5883L_z


  #ifdef I2C_DATA

     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_DATA_OUT_X_MSB_REG , ACK)
     I2CSTART
     I2CSEND ( HMC5883L_address | 1 )
     I2CReceive ( XHiByte , ACK )
     I2CReceive ( XLoByte , ACK )
     I2CReceive ( ZHiByte , ACK )
     I2CReceive ( ZLoByte , ACK )
     I2CReceive ( YHiByte , ACK )
     I2CReceive ( YLoByte , NACK)
     I2CStop

     Wait while HMC5883L_getDataReady = 0



  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_DATA_OUT_X_MSB_REG , ACK)
     HI2CSTART
     HI2CSEND ( HMC5883L_address | 1 )
     HI2CReceive ( XHiByte , ACK )
     HI2CReceive ( XLoByte , ACK )
     HI2CReceive ( ZHiByte , ACK )
     HI2CReceive ( ZLoByte , ACK )
     HI2CReceive ( YHiByte , ACK )
     HI2CReceive ( YLoByte , NACK)
     HI2CStop

     Wait while HMC5883L_getDataReady = 0

  #endif

  end sub

sub HMC5883L_StopContinousRead
  #ifdef I2C_DATA

     I2CSTOP

  #endif

  #ifdef HI2C_DATA

     HI2CSTOP

  #endif

end sub

sub HMC5883L_setConfigurationA ( in HMC5883L_byte )

  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_A , ACK)
     I2CSEND ( HMC5883L_byte, ACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_CONFIG_REG_A , ACK)
     HI2CSEND ( HMC5883L_byte, ACK )
     HI2CSTOP

  #endif

end sub

sub HMC5883L_setConfigurationB ( in HMC5883L_byte )

  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_B , ACK)
     I2CSEND ( HMC5883L_byte, ACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_CONFIG_REG_B , ACK)
     HI2CSEND ( HMC5883L_byte, ACK )
     HI2CSTOP

  #endif
end sub


function HMC5883L_getConfigurationA as byte

  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_A , ACK)
     I2CSTART
     I2CSEND ( HMC5883L_address | 1, ACK)
     I2CReceive ( HMC5883L_getConfigurationA, NACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_CONFIG_REG_A , ACK)
     HI2CSTART
     HI2CSEND ( HMC5883L_address | 1, ACK)
     HI2CReceive ( HMC5883L_getConfigurationA, NACK )
     HI2CSTOP

  #endif

end function


function HMC5883L_getConfigurationB as byte

  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_CONFIG_REG_B , ACK)
     I2CSTART
     I2CSEND ( HMC5883L_address | 1, ACK)
     I2CReceive ( HMC5883L_getConfigurationB, NACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_CONFIG_REG_B , ACK)
     HI2CSTART
     HI2CSEND ( HMC5883L_address | 1, ACK)
     HI2CReceive ( HMC5883L_getConfigurationB, NACK )
     HI2CSTOP

  #endif

end function


function HMC5883L_getStatus as byte

  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_STATUS_REG , ACK)
     I2CSTART
     I2CSEND ( HMC5883L_address | 1, ACK)
     I2CReceive ( HMC5883L_getStatus, NACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_STATUS_REG , ACK)
     HI2CSTART
     HI2CSEND ( HMC5883L_address | 1, ACK)
     HI2CReceive ( HMC5883L_getStatus, NACK )
     HI2CSTOP

  #endif

end function


sub HMC5883L_ReadXYZ( out HMC5883L_x AS WORD , Out HMC5883L_y AS WORD , Out HMC5883L_z AS WORD )


     Dim XHiByte alias HMC5883L_x_h
     Dim XLoByte alias HMC5883L_x
     Dim YHiByte alias HMC5883L_y_h
     Dim YLoByte alias HMC5883L_y
     Dim ZHiByte alias HMC5883L_z_h
     Dim ZLoByte alias HMC5883L_z

  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( HMC5883L_address , ACK)
     I2CSEND ( HMC5883L_MODE_REG , ACK)
     I2CSEND ( HMC5883L_MODE_CONVERSION_SINGLE , ACK )
     I2CSTOP

     wait while HMC5883L_getDataReady = 0


      I2CSTART
      I2CSEND ( HMC5883L_address , ACK)
      I2CSEND ( HMC5883L_DATA_OUT_X_MSB_REG , ACK)
      I2CSTART
      I2CSEND ( HMC5883L_address | 1 )
      I2CReceive ( XHiByte , ACK )
      I2CReceive ( XLoByte , ACK )
      I2CReceive ( ZHiByte , ACK )
      I2CReceive ( ZLoByte , ACK )
      I2CReceive ( YHiByte , ACK )
      I2CReceive ( YLoByte , NACK)
      I2CStop

      Wait while HMC5883L_getDataReady = 0

  #endif

  #ifdef HI2C_DATA
     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_MODE_REG , ACK)
     HI2CSEND ( HMC5883L_MODE_CONVERSION_SINGLE , ACK )
     HI2CSTOP

     wait while HMC5883L_getDataReady = 0


      HI2CSTART
      HI2CSEND ( HMC5883L_address , ACK)
      HI2CSEND ( HMC5883L_DATA_OUT_X_MSB_REG , ACK)
      HI2CSTART
      HI2CSEND ( HMC5883L_address | 1 )
      HI2CReceive ( XHiByte , ACK )
      HI2CReceive ( XLoByte , ACK )
      HI2CReceive ( ZHiByte , ACK )
      HI2CReceive ( ZLoByte , ACK )
      HI2CReceive ( YHiByte , ACK )
      HI2CReceive ( YLoByte , NACK)
      HI2CStop

      Wait while HMC5883L_getDataReady = 0

  #endif




end sub


function HMC5883L_getTiltCompensatedHeading

end function


function HMC5883L_getHeadingXY

end function


function HMC5883L_getDataReady as byte

  HMC5883L_getDataReady = 0

   'HMC5883L_getDataReady_override can be used to force a 6 ms wait
   #ifdef HMC5883L_getDataReady_override
       wait HMC5883L_getDataReady_override ms
       HMC5883L_getDataReady = 1
       Exit Function
   #endif

  #ifdef I2C_DATA

      I2CSTART
      I2CSEND ( HMC5883L_address , ACK)
      I2CSEND ( HMC5883L_STATUS_REG )
      I2CRESTART
      I2CSEND ( HMC5883L_address | 1 )
      I2CReceive ( I2CByte, Nack  )
      I2CStop
      HMC5883L_getDataReady = I2CByte

  #endif

  #ifdef HI2C_DATA

     HI2CSTART
     HI2CSEND ( HMC5883L_address , ACK)
     HI2CSEND ( HMC5883L_STATUS_REG )
     HI2CSTART
     HI2CSEND ( HMC5883L_address | 1 )
     HI2CReceive (  I2CByte, Nack  )
     HI2CStop
     HMC5883L_getDataReady = I2CByte

  #endif

  HMC5883L_getDataReady = HMC5883L_getDataReady & 0b00000001

end function
