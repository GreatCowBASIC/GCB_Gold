''' Copyright 2015--2026 Evan R. Venn
'''A driver for GCGB and GCB.
'''--------------------------------------------------------------------------------------------------------------------------------
'''This driver support the Nunchuck joystick
''':
'''Supports the following:
''':
'''Nunchuck_init alternatively you may have to use Nunchuck_Init_Alternate
'''Nunchuck_Send_Zero which simply set the joystick for the next read
'''Nunchuck_Read_Sensors which set the variables are follows:
'''   JoyX              a byte
'''   JoyY              a byte
'''   Nunchuck_AccelX   a word
'''   Nunchuck_AccelY   a word
'''   Nunchuck_AccelZ   a word
'''   Nunchuck_SwitchState where this is 0,1,2 or 3 dependent on switches C and Z
'''   NunChuck_Decode   a function to decode the data. Not normally reqired.
''':
''':
'''@author  EvanV
'''@licence GPL
'''@version 1.00
'''@date    01.11.2015
'''*******

Sub nunchuck_init

  #ifdef I2C_DATA
     InitI2C
     wait 1 ms
     I2CSTART
     I2CSEND ( NunChuck_address , ACK)
     I2CSEND ( 0xF0 , ACK)
     I2CSEND ( 0x55 , NACK)
     I2CSTOP

     Wait 1 ms
     I2CSTART
     I2CSEND ( NunChuck_address , ACK)
     I2CSEND ( 0xFB , ACK)
     I2CSEND ( 0x00 , NACK)
     I2CSTOP
  #endif

  #ifdef HI2C_DATA
     wait 10 ms
     HI2CMode Master
     HI2CSTART
     HI2CSEND ( NunChuck_address , ACK)
     HI2CSEND ( 0xF0 , ACK)
     HI2CSEND ( 0x55 , NACK)
     HI2CSTOP

     Wait 10 ms
     HI2CSTART
     HI2CSEND ( NunChuck_address , ACK)
     HI2CSEND ( 0xFB , ACK)
     HI2CSEND ( 0x00 , NACK)
     HI2CSTOP

  #endif

  wait 10 ms
  Nunchuck_Send_Zero

  wait 10 ms
  Nunchuck_Read_Sensors

End Sub


Sub Nunchuck_Init_Alternate

  #define NunChuck_Memory_Address 0x40

  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( NunChuck_address , ACK)
     I2CSEND ( NunChuck_Memory_Address , ACK)
     I2CSEND ( 0 , ACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA
     HI2CMode Master
     HI2CSTART
     HI2CSEND ( NunChuck_address , ACK)
     HI2CSEND ( NunChuck_Memory_Address , ACK)
     HI2CSend ( 0 , ACK)
     HI2CSTOP

  #endif

  wait 10 ms
  Nunchuck_Send_Zero

  wait 10 ms
  Nunchuck_Read_Sensors

End Sub

Sub Nunchuck_Send_Zero


  #ifdef I2C_DATA
     I2CSTART
     I2CSEND ( NunChuck_address , ACK)
     I2CSEND ( 0 , ACK )
     I2CSTOP

  #endif

  #ifdef HI2C_DATA
     HI2CSTART
     HI2CSEND ( NunChuck_address , ACK)
     HI2CSEND ( 0 , ACK)
     HI2CSTOP
  #endif

  WAIT Nunchuck_Read_DelayMs  ms

End Sub


Sub Nunchuck_Read_Sensors

  #ifdef I2C_DATA
     I2CRESTART
     I2CSEND ( NunChuck_address | 1 , ACK)
     I2CReceive ( JoyX , ACK)
     I2CReceive ( JoyY , ACK)
     I2CReceive ( Nunchuck_AccelX , ACK)
     I2CReceive ( Nunchuck_AccelY , ACK)
     I2CReceive ( Nunchuck_AccelZ , ACK)
     I2CReceive ( NunChuck_Status , NACK)
     I2CSTOP

  #endif

  #ifdef HI2C_DATA
     HI2CSTART
     HI2CSEND ( NunChuck_address | 1 , ACK)
     HI2CReceive ( JoyX , ACK)
     HI2CReceive ( JoyY , ACK)
     HI2CReceive ( Nunchuck_AccelX , ACK)
     HI2CReceive ( Nunchuck_AccelY , ACK)
     HI2CReceive ( Nunchuck_AccelZ , ACK)
     HI2CReceive ( NunChuck_Status , NACK)
     HI2CSTOP
  #endif

  #ifdef decode
     Nunchuck_AccelX  = ((Nunchuck_AccelX XOR 0x17)+0x17)
     Nunchuck_AccelY  = ((Nunchuck_AccelY XOR 0x17)+0x17)
     Nunchuck_AccelZ  = ((Nunchuck_AccelZ XOR 0x17)+0x17)
     JoyX  = ((JoyX XOR 0x17)+0x17)
     JoyY  = ((JoyY XOR 0x17)+0x17)
     NunChuck_Status  = ((NunChuck_Status XOR 0x17)+0x17)
  #endif



     Nunchuck_SwitchState = NunChuck_Status & 0x03
     Nunchuck_AccelX = FnLSL ( Nunchuck_AccelX, 2 )
     Nunchuck_AccelX = Nunchuck_AccelX   + FnLSR ( ( NunChuck_Status & 0B00001100 ), 2 )



     Nunchuck_AccelY = FnLSL ( Nunchuck_AccelY, 2 )
     Nunchuck_AccelY = Nunchuck_AccelY   + FnLSR ( ( NunChuck_Status & 0B00110000 ), 4 )


     Nunchuck_AccelZ = FnLSL ( Nunchuck_AccelZ, 2 )
     Nunchuck_AccelZ = Nunchuck_AccelZ   + FnLSR ( ( NunChuck_Status & 0B11000000 ), 6 )

     'Complete I2C transaction
     Nunchuck_Send_Zero



End Sub

Function NunChuck_Decode ( Nunchuck_Value )

     NunChuck_Scale  = ((Nunchuck_Value XOR 0x17)+0x17)

End Function
