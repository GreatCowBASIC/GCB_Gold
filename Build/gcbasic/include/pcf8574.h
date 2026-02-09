''' Copyright 2015-2026 Evan R. Venn
'''A suppport program  for GCGB and GCB.
'''--------------------------------------------------------------------------------------------------------------------------------
'''This driver support the 8-bit quasi-bidirectional port via an I2C-bus interface.
'''The PCF8574 has a low current consumption and includes latched outputs with high current drive capability for directly driving LEDs.
'''It also possesses an interrupt line (INT) which can be connected to the interrupt logic of the microcontroller.
'''By sending an interrupt signal on this line, the remote I/O can inform the microcontroller if there is incoming data on its ports without having to communicate via the I2C-bus.
'''This means that the PCF8574 can remain a simple slave device.
''':
''':The driver support two commands via I2C - this supports hardware and software I2C
''':      PCF8574_sendbyte( PCF8574_device, [out_byte] )    ' where PCF8574_device is the 8bit I2C address and out_byte is the byte variable data to be sent
''':      PCF8574_readbyte( PCF8574_device, [in_byte] )   ' where PCF8574_device is the 8bit I2C address and in_byte is the byte variable data to be received
''':
''':
''':I2C must be defined prior to calling these methods, as follows:
''':
''':
''': ----- Define Hardware settings
''': Define I2C settings
''': #define HI2C_BAUD_RATE 400
''': HI2CMode Master
''': #define HI2C_DATA
''':
''':
''': ----- Define I2C settings - CHANGE PORTS
''': #define I2C_MODE Master
''': #define I2C_DATA PORTC.4
''': #define I2C_CLOCK PORTC.5
''': #define I2C_DISABLE_INTERRUPTS ON
''':  Optionally, you can reduce the I2C timings.
''': #define I2C_BIT_DELAY 0 us
''': #define I2C_CLOCK_DELAY 1 us
''': #define I2C_END_DELAY 0 us
''':
''':
'''@author    EvanV
'''@licence   GPL
'''@version   1.01
'''@date      June 2015
'''********************************************************************************
' 14/08/22 Updated user changeable constants only - no functional change



'User changeable constants
'Whilst not required use defines to address multiple devices.

#IFNDEF PCF8574_DEVICE_1
  #DEFINE PCF8574_DEVICE_1 0X4E
#ENDIF

'End of user changeable constants

'''Send a byte value to the target device
'''@param PCF8574_device.  A byte value of the target device, an 8 bit address.
'''@param PCF8574_data.  A byte value to written to device.
sub PCF8574_sendbyte( in PCF8574_device, in PCF8574_data )

    #ifdef I2C_DATA

      I2CSTART
      I2CSEND( PCF8574_device )
      I2CSEND( PCF8574_data )
      'initiate Stop Condition
      I2CSTOP

    #endif

    #ifdef HI2C_DATA

      HI2CSTART
      HI2CSEND( PCF8574_device )
      HI2CSEND( PCF8574_data )
      'initiate Stop Condition
      HI2CSTOP

    #endif

end sub


'''Receive a byte value from the target device
'''@param PCF8574_device.  A byte value of the target device, an 8 bit address.
'''@param PCF8574_data.  A byte value to received from the device.
sub PCF8574_readbyte( in PCF8574_device, Out PCF8574_data )

    #ifdef I2C_DATA

        ' see the Data sheet for this!!! FIGURE 1-1: PCF8574 I2C DEVICE PROTOCOL
        I2CSTART
        I2CSEND( PCF8574_device OR 1 )
        I2CReceive( PCF8574_data , NACK )
        'initiate Stop Condition
        I2CSTOP


    #endif

    #ifdef HI2C_DATA

        ' see the Data sheet for this!!! FIGURE 1-1: PCF8574 I2C DEVICE PROTOCOL
        HI2CSTART
        HI2CSEND( PCF8574_device OR 1 )
        HI2CReceive( PCF8574_data , NACK )
        'initiate Stop Condition
        HI2CSTOP


    #endif



end sub
