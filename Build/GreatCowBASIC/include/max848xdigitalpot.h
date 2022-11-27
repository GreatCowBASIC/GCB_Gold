''' Copyright 2020 Evan R. Venn
'''A program  for GCGB and GCB.
'''--------------------------------------------------------------------------------------------------------------------------------
'''  The MAX5481–MAX5484 10-bit (1024-tap) nonvolatile, linear-taper, programmable voltage-dividers and variable resistors perform the function of a mechanical
'''  potentiometer, but replace the mechanics with a pinconfigurable 3-wire serial SPI™-compatible interface or up/down digital interface. The MAX5481/MAX5482 are
'''  3-terminal voltage-dividers and the MAX5483/MAX5484 are 2-terminal variable resistors.
'''
'''  The MAX5481–MAX5484 feature an internal, nonvolatile, electrically erasable programmable read-only memory (EEPROM) that stores the wiper position for initialization during power-up. The 3-wire SPI-compatible
'''  serial interface allows communication at data rates up to 7MHz. A pin-selectable up/down digital interface is also available.
'''
'''  The MAX5481–MAX5484 are ideal for applications requiring digitally controlled potentiometers. Two end-toend resistance values are available (10kO and 50kO) in a
'''  voltage-divider or a variable-resistor configuration.
'''  These devices ideal for applications requiring low-temperature-coefficient voltage-dividers, such as low-drift, programmable gain-amplifiers.
'''
'''  The MAX848xDigitalPotSetWiperPosition and MAX848xDigitalPotDecrement methods ONLY support software SPI.
'''
'''
'''  Methods
'''
'''
'''   MAX848xDigitalPotSetWiperPosition       ( word value where range is 0 to 1023 )  Sets the pot value
'''   MAX848xCopyWiperRegistertoNVRegister    Sets the NVRAM to the current pot value
'''   MAX848xCopyNVRegistertoWiperRegister    Sets the current pot value to MVRAM
'''   MAX848xDigitalPotIncrement              Increment the current pot value
'''   MAX848xDigitalPotDecrement              Decrement the current pot value
'''
''' Configuration
'''     Three ports and one optional port is required as shown below.
'    #DEFINE MAX848XDIGITALPOT_CS       PORTC.0
'    #DEFINE MAX848XDIGITALPOT_SCK      PORTC.1
'    #DEFINE MAX848XDIGITALPOT_DO       PORTC.2
'    #DEFINE MAX848XDIGITALPOT_IOMODE   PORTC.3   - Optional
'    MAX848XDIGITALPOT_IOMODE to control SPI or Up/Down modes automatically, or if you do not define MAX848XDIGITALPOT_IOMODE then set the SPI/UD in the Max848x as follows:
'            SPI/UD = HIGH (connected permanently to high voltage ) then the you can only use the SPI controls as follows:
'                     MAX848xDigitalPotSetWiperPosition
'                     MAX848xCopyWiperRegistertoNVRegister
'                     MAX848xCopyNVRegistertoWiperRegister
'            SPI/UD = LOW (connected permanently to 0v ) then the you can only use the SPI controls as follows:
'                     MAX848xDigitalPotIncrement
'                     MAX848xDigitalPotDecrement

'    MAX848xDigitalPotIncrement and MAX848xDigitalPotDecrement are ONLY support when using Software SPI

/* write wiper constants */
#define MAX5481_WRITE_WIPER 0
/* copy wiper reg to NV reg */
#define MAX5481_COPY_AB_TO_NV 32
/* copy NV reg to wiper reg */
#define MAX5481_COPY_NV_TO_AB 48
/* usage */
#define MAX848XSPIMODE    1
#define MAX848XUPDOWNMODE 0




'Start of library

#STARTUP MAX848xInit, 91

sub MAX848xInit

  Dim MAX848XDIGITALPOT_IOMODEState

  ' Ports are required for SPI
  Dir MAX848XDIGITALPOT_DO Out
  Dir MAX848XDIGITALPOT_SCK Out
  Dir MAX848XDIGITALPOT_CS out

  ' Initial state of ports required for SW SPI
  set MAX848XDIGITALPOT_DO off
  set MAX848XDIGITALPOT_SCK off
  set MAX848XDIGITALPOT_CS off

  'Assume SPI mode
  #ifdef MAX848XDIGITALPOT_IOMODE
      MAX848XDIGITALPOT_IOMODEState = MAX848XSPIMODE
      MAX848XDIGITALPOT_IOMODE = MAX848XDIGITALPOT_IOMODEState
  #endif

  ' Required if you are using HARDWARE SPI.
  #ifdef MAX848xHardwareSPI
      ' harware SPI mode
      asm showdebug SPI constant used equates to HWSPIMODESCRIPT
      SPIMode HWSPIMODESCRIPT, 0
  #endif



end sub


'Define a simple user name to this method.
#define MAX848xDigitalPotSetWiperPosition MAX848xDigitalWriteMemory

'''Write a word value to the device
'''@param _MAX848xValue.  The value to written to device
sub  MAX848xDigitalWriteMemory ( in _MAX848xValue as Word )

    #ifdef MAX848XDIGITALPOT_IOMODE
        MAX848XDIGITALPOT_IOMODE =  MAX848XSPIMODE
    #endif

    Repeat 6
      Set C Off
      Rotate _MAX848xValue Left
    End Repeat

    Set MAX848XDIGITALPOT_CS Off
    MAX848xSPITransfer( MAX5481_WRITE_WIPER )
    MAX848xSPITransfer( _MAX848xValue_h )
    MAX848xSPITransfer( _MAX848xValue )
    Set MAX848XDIGITALPOT_CS On

end sub


'''Decrement Pot Wiper
sub  MAX848xDigitalPotIncrement


    #ifdef MAX848XDIGITALPOT_IOMODE
        MAX848XDIGITALPOT_IOMODE =  MAX848XUPDOWNMODE
    #endif

    Set MAX848XDIGITALPOT_CS Off
    MAX848xDUTransfer( 1  )
    Set MAX848XDIGITALPOT_CS On
end sub

'''Increment Pot Wiper
sub  MAX848xDigitalPotDecrement

    #ifdef MAX848XDIGITALPOT_IOMODE
        MAX848XDIGITALPOT_IOMODE =  MAX848XUPDOWNMODE
    #endif

    Set MAX848XDIGITALPOT_CS Off
    MAX848xDUTransfer( 0  )
    Set MAX848XDIGITALPOT_CS On

end sub

sub MAX848xCopyWiperRegistertoNVRegister

    #ifdef MAX848XDIGITALPOT_IOMODE
        MAX848XDIGITALPOT_IOMODE =  MAX848XSPIMODE
    #endif

    Set MAX848XDIGITALPOT_CS Off
    MAX848xSPITransfer( MAX5481_COPY_AB_TO_NV )
    MAX848xSPITransfer( 0 )
    MAX848xSPITransfer( 0 )
    Set MAX848XDIGITALPOT_CS On

end sub


Sub MAX848xCopyNVRegistertoWiperRegister

    #ifdef MAX848XDIGITALPOT_IOMODE
        MAX848XDIGITALPOT_IOMODE =  MAX848XSPIMODE
    #endif

    Set MAX848XDIGITALPOT_CS Off
    MAX848xSPITransfer( MAX5481_COPY_NV_TO_AB )
    MAX848xSPITransfer( 0 )
    MAX848xSPITransfer( 0 )
    Set MAX848XDIGITALPOT_CS On

end sub


'''@hide
sub  MAX848xSPITransfer ( IN MAX848XDigitalPotSendByte as byte  )

  #ifdef MAX848xHardwareSPI
     FastHWSPITransfer  MAX848XDigitalPotSendByte
  #endif

  #ifndef MAX848xHardwareSPI

      Dir MAX848XDIGITALPOT_DO Out
      Dir MAX848XDIGITALPOT_SCK Out

      set MAX848XDIGITALPOT_SCK Off

      repeat 8

        if MAX848XDigitalPotSendByte.7 = ON  then
          set MAX848XDIGITALPOT_DO ON
        else
          set MAX848XDIGITALPOT_DO OFF
        end if

        'Device is cpol = 0. Invert SCK if cpol = 1
        SET MAX848XDIGITALPOT_SCK ON
        rotate MAX848XDigitalPotSendByte left
        SET MAX848XDIGITALPOT_SCK OFF

      end repeat
    #endif
end Sub


'''@hide
sub  MAX848xDUTransfer ( IN MAX848XDigitalPotSendByte as byte )

      Dir MAX848XDIGITALPOT_DO Out
      Dir MAX848XDIGITALPOT_SCK Out

      If MAX848XDigitalPotSendByte = 1 then
          MAX848XDIGITALPOT_DO = 0
      else
          MAX848XDIGITALPOT_DO = 1
      end if

      set MAX848XDIGITALPOT_SCK Off

      SET MAX848XDIGITALPOT_SCK ON

      MAX848XDIGITALPOT_DO = MAX848XDigitalPotSendByte

      SET MAX848XDIGITALPOT_SCK OFF


end Sub
