'       Ultrasonic distance sensor routines for the GCBASIC compiler
'      Copyright (C) 2014-2020 Peter Everett

' LIBRARY FOR SRF02 ULTRASONIC SENSOR MODULE IN I2C MODE
' V0.2 Peter Everett 30/11/2014

' The SRF02 is a small ultrasonic transeiver with built in calibration and measurement processing
' It can be accessed using I2C or SPI (This library is for the I2C mode)
' The module has a typical range from 16 cm to 6 m
' Information on the module can be found here http://www.robot-electronics.co.uk/htm/srf02tech.htm

' Revision history
' 0.1 07/11/2014  Initial version
' 0.2 30/11/2014  Change address function fixed and tested including error handling

' FUNCTION LIST AND STATUS

' SRF_Measure       Tested in cm and inches Oct 2014.  uS gives a result so assumed correct.
' SRF_FakeMeasure   Tested Nov 2014.  2 sensors on same I2C bus but seems to detect ping and measure correctly
' SRF_Ping          Tested Nov 2014.  2 sensors on same I2C bus but ping seems to be detected by other sensor
' SRF_GetResult     Tested Oct 2014
' SRF_GetCalib      Tested Nov 2014
' SRF_SWVer         Tested Nov 2014
' SRF_ChangeAddr    Tested Nov 2014
'


'variables
dim SRF_ver as byte     'Software version
dim SRF_units as byte   'Units of the measurement
dim SRF_cmd as byte     'Command to be sent to the module
dim SRF_rngH as byte    'High byte of the mesurement result
dim SRF_rngL as byte    'Low byte of the measurement result
dim SRF_Result as word  '16 bit word for the measurement result
dim SRF_Calib as word   '16 bit word for the calibration value (minimum distance that can be measured)

Sub SRF_Measure (in SRF_units, optional SRF_Base as byte = 0xE0)
  'Send a pulse and take a measurement
  'SRF_units determines the units of the measurement
  '0 = Inches
  '1 = Centimetres
  '2 = Microseconds
  'The unit will be stored after the reading and can be read by the main program e.g. for adding to the display
  'Taking a reading will blink the LED on the module

  Select case SRF_units
    case 0
      SRF_cmd = 0x50
    case 1
      SRF_cmd = 0x51
    case 2
      SRF_cmd = 0x52
    case else
      'this is an 'error' condition if an invalid unit is specified
      SRF_units = 255
    end select

  'send the command
  'the result is stored on the chip and must be read back by the main program using the SRF_GetResult function
  I2CStart
  I2CSend SRF_Base      'device in write mode
  I2CSend 0             'register 0 ('command' register)
  I2CSend SRF_cmd       'send command
  I2CStop
End Sub

Sub SRF_FakeMeasure (in SRF_units, optional SRF_Base as byte = 0xE0)
  'Take a measurement but don't send a pulse
  'This is used if another transmitter sends the pulse and the module is to listen for it.  The send/receive need to be synchronised.
  'SRF_units determines the units of the measurement
  '0 = Inches
  '1 = Centimetres
  '2 = Microseconds
  'The unit will be stored after the reading and can be read by the main program e.g. for adding to the display
  'Taking a reading will blink the LED on the module
  'The code for this function is the same as for SRF_Measure apart from the values of the command that is issued

  Select case SRF_units
    case 0
      SRF_cmd = 0x56
    case 1
      SRF_cmd = 0x57
    case 2
      SRF_cmd = 0x58
    case else
      'this is an 'error' condition if an invalid unit is specified
      SRF_units = 255
    end select

  'send the command
  'the result is stored on the chip and must be read back by the main program using the SRF_GetResult function
  I2CStart
  I2CSend SRF_Base    'device in write mode
  I2CSend 0           'register 0 ('command' register)
  I2CSend SRF_cmd     'send command
  I2CStop
End Sub

Sub SRF_Ping (optional SRF_Base as byte = 0xE0)
  'Send a pulse but doesn't take a measurement
  'Sending a pulse will blink the LED on the module

  'send the command
  I2CStart
  I2CSend SRF_Base    'device in write mode
  I2CSend 0           'register 0 ('command' register)
  I2CSend 0x5C        'send command
  I2CStop
End Sub

Sub SRF_GetResult (optional SRF_Base as byte = 0xE0)
  'Reads the result of the last measurement and stores in the SRF_Result variable
  'The units of the measurement are whatever was set in the SRF_Measure command
  I2CStart
  I2CSend SRF_Base          'device in write mode
  I2CSend 2                 'register 2 (start of the measurement registers)

  I2CRestart
  I2CSend (SRF_Base + 1)    'device in read mode
  I2CReceive SRF_rngH       'read high byte
  I2CReceive SRF_rngL, nack 'read low byte
  I2CStop

  'Combine the high and low bytes to give a 16 bit word as a result
  SRF_Result = (SRF_rngH * 255) + SRF_rngL
End Sub

Sub SRF_GetCalib (optional SRF_Base as byte = 0xE0)
  'Reads the auto tune result
  'The units of this measurement are the same as the last ranging measurement
  'Therefore this only works if a measurement has been taken
  'The datasheet says that 5-6 measurements need to be taken for the tuning to be completed
  'The code for this function is the same as SRF_GetResult apart from the register location

  I2CStart
  I2CSend SRF_Base          'device in write mode
  I2CSend 4                 'register 4 (start of the autotune registers)

  I2CRestart
  I2CSend (SRF_Base + 1)      'device in read mode
  I2CReceive SRF_rngH         'read high byte
  I2CReceive SRF_rngL, nack   'read low byte
  I2CStop

  'Combine the high and low bytes to give a 16 bit word as a result
  SRF_Calib = (SRF_rngH * 255) + SRF_rngL

End Sub

Sub SRF_SWVer (optional SRF_Base as byte = 0xE0)
  'Reads the software version from the module

  I2CStart
  I2CSend SRF_Base          'device in write mode
  I2CSend 1                 'register 1 (start of the autotune registers)

  I2CRestart
  I2CSend (SRF_Base + 1)    'device in read mode
  I2CReceive SRF_ver        'read software version
  I2CStop

End Sub

Sub SRF_ChangeAddr (SRF_OldBase, SRF_NewBase)
  'Changes the bus address of the sensor from SRF_OldBase to SRF_NewBase
  'Old address is compulsory in this function to ensure the correct sensor is reprogrammed
  'The address is changed by writing values 0xA0 0xAA and 0xA5 to the command register, followed by the new address.
  'The address for the module is 'flashed' on the LED when it is powered up
  'Only the following addresses can be used with the module: E0 (default), E2, E4, E6, E8, EA, EC, EE, F0, F2, F4, F6, F8, FA, FC, FE

  'check the new address is valid
  If SRF_NewBase > 0xFE then
    Exit Sub
  End If
  If SRF_NewBase < 0xE0 then
    Exit Sub
  End If
  If (SRF_NewBase & 1) = 1 then
    Exit Sub
  End If

  'Address is valid, continue
  I2CStart
  I2CSend SRF_OldBase     'device in write mode
  I2CSend 0               'register 0 ('command' register)
  I2CSend 0xA0            'send first command
  I2CStop
  I2CStart
  I2CSend SRF_OldBase     'device in write mode
  I2CSend 0               'register 0 ('command' register)
  I2CSend 0xAA            'send second command
  I2CStop
  I2CStart
  I2CSend SRF_OldBase     'device in write mode
  I2CSend 0               'register 0 ('command' register)
  I2CSend 0xA5            'send third command
  I2CStop
  I2CStart
  I2CSend SRF_OldBase     'device in write mode
  I2CSend 0               'register 0 ('command' register)
  I2CSend SRF_NewBase     'send new address
  I2CStop

End Sub

