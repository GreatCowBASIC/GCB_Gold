'    DS2482 routines for the GCBASIC compiler
'    Copyright (C) 2017-2026 Evan Venn

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

'    Release notes
'    The DS2482-100 and the DS2482-800 understands eight function commands that fall into four categories: device control, I2C communication,
'    1-Wire setup, and 1-Wire communication.  The feedback path to the host is controlled by a read pointer,
'    which is set automatically by each function command for the host to efficiently access relevant  information.
'    The host processor sends these commands and applicable parameters as strings of one or two bytes using the I2C interface.
'    The I2C protocol  requires that each byte be acknowledged by the receiving party to confirm acceptance or not be acknowledged
'    to indicate an error condition (invalid code or parameter) or to end the communication. See the I2C Interface section for
'    details of the I2C protocol including acknowledge.
'    See http://www.maximintegrated.com/app-notes/index.mvp/id/3684
'
'    Revised 28.08.2017 to resolve non-ANSI characters
dim ds2482_status  as Byte
dim ds2482_short_detected as Byte
#define        ds2482_temp         SysCalcTempA   ' save memory - oNe byte1!

' constants/macros/typdefs
#define DS2482_I2C_ADDR   0x30  ' Base I2C address of DS2482 devices
#define POLL_LIMIT      0x30  ' 0x30 is the minimum poll limit


' Everything below here should not be changed.
#define CONFIG_APU                      0x01      ' Active pullup (cAPU) = on (CONFIG_APU = 0x01)

' 1-wire eeprom and silicon serial number commands
#define READ_DEVICE_ROM   0x33
#define SKIP_ROM      0xCC
#define WRITE_SCRATCHPAD            0x0F
#define READ_MEMORY     0xF0
#define COPY_SCRATCHPAD   0x55

' DS2482 command defines
#define DS2482_CMD_DRST   0xF0  '< DS2482 Device Reset
#define DS2482_CMD_SRP    0xE1  '< DS2482 Set Read Pointer
#define DS2482_CMD_WCFG   0xD2  '< DS2482 Write Configuration
#define DS2482_CMD_CHSL   0xC3  '< DS2482 Channel Select
#define DS2482_CMD_1WRS   0xB4  '< DS2482 1-Wire Reset
#define DS2482_CMD_1WWB   0xA5  '< DS2482 1-Wire Write Byte
#define DS2482_CMD_1WRB   0x96  '< DS2482 1-Wire Read Byte
#define DS2482_CMD_1WSB   0x87  '< DS2482 1-Wire Single Bit
#define DS2482_CMD_1WT    0x78  '< DS2482 1-Wire Triplet

' DS2482 status register bit defines
#define DS2482_STATUS_1WB 0x01  '< DS2482 Status 1-Wire Busy
#define DS2482_STATUS_PPD 0x02  '< DS2482 Status Presence Pulse Detect
#define DS2482_STATUS_SD  0x04  '< DS2482 Status Short Detected
#define DS2482_STATUS_LL  0x08  '< DS2482 Status 1-Wire Logic Level
#define DS2482_STATUS_RST 0x10  '< DS2482 Status Device Reset
#define DS2482_STATUS_SBR 0x20  '< DS2482 Status Single Bit Result
#define DS2482_STATUS_TSB 0x40  '< DS2482 Status Triplet Second Bit
#define DS2482_STATUS_DIR 0x80  '< DS2482 Status Branch Direction Taken

' DS2482 configuration register bit defines
#define DS2482_CFG_APU    0x01  '< DS2482 Config Active Pull-Up
#define DS2482_CFG_PPM    0x02  '< DS2482 Config Presence Pulse Masking
#define DS2482_CFG_SPU    0x04  '< DS2482 Config Strong Pull-Up
#define DS2482_CFG_1WS    0x08  '< DS2482 Config 1-Wire Speed

' DS2482 channel selection code for defines
#define DS2482_CH_IO0   0xF0  '< DS2482 Select Channel IO0
#define DS2482_CH_IO1   0xE1  '< DS2482 Select Channel IO1
#define DS2482_CH_IO2   0xD2  '< DS2482 Select Channel IO2
#define DS2482_CH_IO3   0xC3  '< DS2482 Select Channel IO3
#define DS2482_CH_IO4   0xB4  '< DS2482 Select Channel IO4
#define DS2482_CH_IO5   0xA5  '< DS2482 Select Channel IO5
#define DS2482_CH_IO6   0x96  '< DS2482 Select Channel IO6
#define DS2482_CH_IO7   0x87  '< DS2482 Select Channel IO7

' DS2482 channel selection read back code for defines
#define DS2482_RCH_IO0    0xB8  '< DS2482 Select Channel IO0
#define DS2482_RCH_IO1    0xB1  '< DS2482 Select Channel IO1
#define DS2482_RCH_IO2    0xAA  '< DS2482 Select Channel IO2
#define DS2482_RCH_IO3    0xA3  '< DS2482 Select Channel IO3
#define DS2482_RCH_IO4    0x9C  '< DS2482 Select Channel IO4
#define DS2482_RCH_IO5    0x95  '< DS2482 Select Channel IO5
#define DS2482_RCH_IO6    0x8E  '< DS2482 Select Channel IO6
#define DS2482_RCH_IO7    0x87  '< DS2482 Select Channel IO7

' DS2482 read pointer code defines
#define DS2482_READPTR_SR 0xF0  '< DS2482 Status Register
#define DS2482_READPTR_RDR  0xE1  '< DS2482 Read Data Register
#define DS2482_READPTR_CSR  0xD2  '< DS2482 Channel Selection Register
#define DS2482_READPTR_CR 0xC3  '< DS2482 Configuration Register




'        Returns: TRUE if device was reset
'                 FALSE device not detected or failure to perform reset
'
function ds2482_reset

  i2cstart
  i2csend ( DS2482_I2C_ADDR )
  i2csend ( DS2482_CMD_DRST )
          i2crestart
  i2csend ( DS2482_I2C_ADDR | 1 )
  i2creceive( ds2482_status, NACK )
  i2cstop
  ' check for failure due to incorrect read back of ds2482_status
          ' these values are determined from the device datasheet table 'status register bit assignment'
  if (( ds2482_status & 0xf7) = 0x10) then
             ds2482_reset = true
          else
             ds2482_reset = false
          end if

end function

'  DS2428 Detect routine that sets the I2C address and then performs a
'  device reset followed by writing the configuration byte to default values:
'  Returns: TRUE if device was detected and written
'           FALSE device not detected or failure to write configuration byte
'
function ds2482_detect

  if ( ds2482_reset = false ) then
    ds2482_detect = false
                    exit function
          end if

  if ( ds2482_write_config(DS2482_CFG_APU) = false ) then
    ds2482_detect = false
                    exit function
          end if

  ds2482_detect = true
end function


' Write the configuration register in the DS2482. The configuration
' options are provided in the lower nibble of the provided config byte.
' The uppper nibble in bitwise inverted when written to the DS2482.
'
'         When writing to the Configuration Register
'         the new data is accepted only if the upper nibble (bits 7 to 4) is the ones complement of the lower nibble (bits 3 to 0).
'         When read, the upper nibble is always 0h.
' Returns:  TRUE: config written and response correct
'           FALSE: response incorrect
'
function ds2482_write_config( ds2482_config )

  i2cstart
  i2csend ( DS2482_I2C_ADDR | 0 )
  I2CSend DS2482_CMD_WCFG



  ds2482_status = not ds2482_config
  ds2482_status = Fnlsl ( ds2482_status, 4 )
  ds2482_status = ds2482_config | ds2482_status

  I2csend ds2482_status
  i2cstart
  i2csend ( DS2482_I2C_ADDR | 1 )
  i2creceive( ds2482_status, NACK )
  i2cstop

  ' check for failure by reading back the value
  if ds2482_config <> ds2482_status then
  DS2482_reset
          ds2482_write_config = false
          exit function
  end if
  ds2482_write_config = true

end function



'   Select the 1-Wire channel on a DS2482-800 only!
'
'   Returns: TRUE if channel selected
'            FALSE device not detected or failure to perform select
'
function ds2482_channel_select( Optional  channel as byte = 0 )


   select case channel
  case 0
               ch = DS2482_CH_IO0: ch_read = DS2482_RCH_IO0
  case 1
               ch = DS2482_CH_IO1: ch_read = DS2482_RCH_IO1
  case 2
               ch = DS2482_CH_IO2: ch_read = DS2482_RCH_IO2
  case 3
               ch = DS2482_CH_IO3: ch_read = DS2482_RCH_IO3
  case 4
               ch = DS2482_CH_IO4: ch_read = DS2482_RCH_IO4
  case 5
               ch = DS2482_CH_IO5: ch_read = DS2482_RCH_IO5
  case 6
               ch = DS2482_CH_IO6: ch_read = DS2482_RCH_IO6
  case 7
               ch = DS2482_CH_IO7: ch_read = DS2482_RCH_IO7
   end select



    I2CStart
    i2csend ( DS2482_I2C_ADDR | 0 )
    i2csend ( DS2482_CMD_CHSL )
    I2CSend ( ch, NACK )
    I2CStart
    I2CSend ( DS2482_I2C_ADDR | 1 )          'I2C_FLAG_READ
    i2creceive( ds2482_status, NACK )        ' read_channel
    I2CStop
    ds2482_channel_select =  ( ds2482_status = ch_read )

end function


'   Reset all of the devices on the 1-Wire Net and return the result.
'
'   Returns: TRUE(1):  presence pulse(s) detected, device(s) reset
'            FALSE(0): no presence pulses detected
function ds2482_OWReset

         I2CStart
         i2csend ( DS2482_I2C_ADDR )
         i2csend ( DS2482_CMD_1WRS )
         i2cstart
         i2csend ( DS2482_I2C_ADDR | 1 )                                   ' I2C_FLAG_READ

         poll_count = 0                                                    ' Set up the poll count
         ds2482_status = 0b11111111                                        ' set status to 0xff
          do while ( poll_count < POLL_LIMIT )
              if ( ds2482_status & DS2482_STATUS_1WB  ) =  DS2482_STATUS_1WB then       ' read whilst OneWire is busy
                 i2creceive( ds2482_status, ACK )
              else
                 i2creceive( ds2482_temp, NACK )        ' write final NACK i2c command for confirmation, this specified in protocol
                 exit do
              end if
              poll_count++
          loop

          i2cstop

          if ( poll_count >= POLL_LIMIT ) then
                    DS2482_reset
                    ds2482_OWReset =  false
                    exit function
          end if

          if ( ds2482_status & DS2482_STATUS_SD) = DS2482_STATUS_SD then   ' check for presence detect
                    ds2482_short_detected =  true
          else
                    ds2482_short_detected =  false
          end if

          if ( ds2482_status & DS2482_STATUS_PPD) = DS2482_STATUS_PPD then ' check for presence detect
                    ds2482_OWReset =  true
          else
                    ds2482_OWReset =  false
          end if

end function

'   Send 1 bit of communication to the 1-Wire Net.
'   The parameter 'sendbit' least significant bit is used.
'
'   'sendbit' - 1 bit to send (least significant byte)
sub oneWire_WriteBit( In sendbit as bit )
  oneWire_TouchBit( sendbit )
end sub

'   Reads 1 bit of communication from the 1-Wire Net and returns the result
'
'   Returns:  1 bit read from 1-Wire Net
function oneWire_ReadBit
  oneWire_ReadBit = oneWire_TouchBit( 0x01 )
end function




'   Send 1 bit of communication to the 1-Wire Net and return the
'   result 1 bit read from the 1-Wire Net. The parameter 'sendbit'
'   least significant bit is used and the least significant bit
'   of the result is the return bit.
'
'   sendbit - the least significant bit is the bit to send
'
'   Returns: 0  0 bit read from sendbit
'          1   1 bit read from sendbit
'
function oneWire_TouchBit( in touchbit as bit )

      I2CStart
      i2csend ( DS2482_I2C_ADDR | 0 )
      i2csend ( DS2482_CMD_1WSB, true )
      if touchbit = 1 then
        i2csend ( 0x80 )
      else
        i2csend ( 0x00 )
      end if
      I2CStart

      I2CSend ( DS2482_I2C_ADDR | 1 )   ' I2C_FLAG_READ

      poll_count = 0                                                    ' Set up the poll count
      ds2482_status = 0b11111111                                        ' set status to 0xff
      do while ( poll_count < POLL_LIMIT )
         if ( ds2482_status & DS2482_STATUS_1WB  ) =  DS2482_STATUS_1WB then       ' read whilst OneWire is busy
            i2creceive( ds2482_status, ACK )
         else
            i2creceive( ds2482_temp, NACK )        ' write final NACK i2c command for confirmation, this specified in protocol
            exit do
         end if
         poll_count++
      loop

      i2cstop

      if ( poll_count >= POLL_LIMIT ) then
              DS2482_reset
              oneWire_TouchBit =  false
              exit function
      end if

      if ( ds2482_status & DS2482_STATUS_SBR) = DS2482_STATUS_SBR then
            oneWire_TouchBit =  true
      else
            oneWire_TouchBit =  false
      end if

end function

' Send 8 bits of communication to the 1-Wire Net and verify that the
' 8 bits read from the 1-Wire Net are the same (write operation).
' The parameter 'ds2482byte' least significant 8 bits are used.
'
' 'ds2482byte' - 8 bits to send (least significant byte)
'
' Returns:  TRUE: bytes written and echo was the same
'           FALSE: echo was not the same
'
sub oneWire_writeByte ( in ds2482byte )

    i2cstart
    i2csend ( DS2482_I2C_ADDR | 0 )
    i2csend ( DS2482_CMD_1WWB )
    i2csend ( ds2482byte )
    I2CStart
    i2csend ( DS2482_I2C_ADDR | 1)                                    ' I2C_FLAG_READ

    poll_count = 0                                                    ' Set up the poll count
    ds2482_status = 0b11111111                                        ' set status to 0xff
     do while ( poll_count < POLL_LIMIT )
         if ( ds2482_status & DS2482_STATUS_1WB  ) =  DS2482_STATUS_1WB then       ' read whilst OneWire is busy
            i2creceive( ds2482_status, ACK )
         else
            i2creceive( ds2482_temp, NACK )        ' write final NACK i2c command for confirmation, this specified in protocol
            exit do
         end if
         poll_count++
     loop

     i2cstop


     if ( poll_count >= POLL_LIMIT ) then
        ds2482_status = DS2482_reset
        ds2482_status = false
     else
        ds2482_status = true
     end if

end sub


' Send 8 bits of read communication to the 1-Wire Net and return the
' result 8 bits read from the 1-Wire Net.
'
' Returns:  8 bits read from 1-Wire Net
'
sub oneWire_readByte( out ds2482byte as byte )

    i2cstart
    i2csend ( DS2482_I2C_ADDR | 0 )
    i2csend ( DS2482_CMD_1WRB )
    I2CStart
    i2csend ( DS2482_I2C_ADDR | 1)     ' I2C_FLAG READ

    poll_count = 0                                                    ' Set up the poll count
    ds2482_status = 0b11111111                                        ' set status to 0xff
     do while ( poll_count < POLL_LIMIT )
         if ( ds2482_status & DS2482_STATUS_1WB  ) =  DS2482_STATUS_1WB then       ' read whilst OneWire is busy
            i2creceive( ds2482_status, ACK )
         else
            i2creceive( ds2482_temp, NACK )        ' write final NACK i2c command for confirmation, this specified in protocol
            exit do
         end if
         poll_count++
     loop

     i2cstop

     if ( poll_count >= POLL_LIMIT ) then
        ds2482_status = DS2482_reset
        ds2482_status = false
        ds2482byte = 0        ' exit
        exit sub
     else
        ds2482_status = true
     end if

    I2CStart
    i2csend ( DS2482_I2C_ADDR | 0 )
    i2csend ( DS2482_CMD_SRP )
    i2csend ( DS2482_READPTR_RDR )
    I2CStart
    i2csend ( DS2482_I2C_ADDR | 1 )
    i2creceive( ds2482byte, NACK )
    i2cstop

end sub

' The 'OWBlock' transfers a block of data to and from the
' 1-Wire Net. The result is returned in the same buffer.
'
' 'tran_buf' - pointer to a block of unsigned
'              chars of length 'tran_len' that will be sent
'              to the 1-Wire Net
' 'tran_len' - length in bytes to transfer
'
sub oneWire_BlockTransfer( transfer_buffer() as byte, arraylength as word )

  for ds2482_temp = 1 to arraylength
    transfer_buffer(ds2482_temp) = oneWire_TouchByte(transfer_buffer(ds2482_temp))
  next
end sub


' Send 8 bits of communication to the 1-Wire Net and return the
' result 8 bits read from the 1-Wire Net. The parameter 'sendbyte'
' least significant 8 bits are used and the least significant 8 bits
' of the result are the return byte.
'
' 'sendbyte' - 8 bits to send (least significant byte)
'
' Returns:  8 bits read from sendbyte
'
sub oneWire_TouchByte( sendbyte )
  if (sendbyte = 0xff) then
    oneWire_ReadByte ( oneWire_TouchByte )
          else
    oneWire_WriteByte( sendbyte )
    oneWire_TouchByte = sendbyte
          end if

end sub
