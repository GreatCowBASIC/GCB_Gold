'    Software SPI SRAM routines for the GCBASIC compiler
'    Copyright (C) 2019-2024  Evan R. Venn

'    This library is free software' you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation' either
'    version 2.1 of the License, or (at your option) any later version.

'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY' without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.

'    You should have received a copy of the GNU Lesser General Public
'    License along with this library' if not, write to the Free Software
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

'    12.12.2019       Initial release
'    20.11.2024       Rename dummy variables to _dummy


'SPISRAM_wr_byte( Long_address, byte_value )
'SPISRAM_rd_byte( Long_address, byte_value )
'
'There is no error detection. It is the duty of the
'calling program to specify valid addresses within
'the SRAM and not let array/strings run off the end.



#define SRAM_23LCV1024 1
#define SRAM_23LC1024  1
#define SRAM_23A1024   1
#define SRAM_23LCV512  2
#define SRAM_23LC512   2
#define SRAM_23A512    2
#define SRAM_23K256    3
#define SRAM_23A256    3
#define SRAM_23A640    4
#define SRAM_23K640    4


#define SRAM_23LC_ADDRESS_16BIT     0
#define SRAM_23LC_ADDRESS_24BIT     1

#define SRAM_23LC_DUMMY_BYTE      0xFF

'Commands supported by all chips
#define SRAM_23LC_COMMAND_READ      0x03
#define SRAM_23LC_COMMAND_WRITE     0x02
#define SRAM_23LC_COMMAND_RDMR      0x05
#define SRAM_23LC_COMMAND_WRMR      0x01

'Additional commands supported by some chips
#define SRAM_23LC_COMMAND_EDIO      0x3B
#define SRAM_23LC_COMMAND_EQIO      0x38
#define SRAM_23LC_COMMAND_RSTIO     0xFF

#define SRAM_23LC_BYTE_MODE   0x00
#define SRAM_23LC_PAGE_MODE   0x80
#define SRAM_23LC_STREAM_MODE 0x40

#script


  if SPISRAM_TYPE = SRAM_23LCV1024 then
      SPISRAM_CAPACITY = 0x20000
  end if

  if SPISRAM_TYPE = SRAM_23LC1024 then
      SPISRAM_CAPACITY = 0x20000
  end if

  if SPISRAM_TYPE = SRAM_23A1024 then
      SPISRAM_CAPACITY = 0x20000
  end if


  if SPISRAM_TYPE = SRAM_23LCV512 then
      SPISRAM_CAPACITY = 0x10000
  end if

  if SPISRAM_TYPE = SRAM_23LC512 then
      SPISRAM_CAPACITY = 0x10000
  end if

  if SPISRAM_TYPE = SRAM_23A512 then
      SPISRAM_CAPACITY = 0x10000
  end if


  if SPISRAM_TYPE = SRAM_23K256 then
      SPISRAM_CAPACITY = 0x8000
  end if

  if SPISRAM_TYPE = SRAM_23A256 then
      SPISRAM_CAPACITY = 0x8000
  end if


  if SPISRAM_TYPE = SRAM_23A640 then
      SPISRAM_CAPACITY = 0x2000
  end if

  if SPISRAM_TYPE = SRAM_23K640 then
      SPISRAM_CAPACITY = 0x2000
  end if

#endscript

#startup SPISRAMInit, 86


''----- Methods

sub SPISRAMInit

    dir SPISRAM_CS out
    dir SPISRAM_SCK  out
    dir SPISRAM_DO   out
    dir SPISRAM_DI   in

    set SPISRAM_CS ON


    #ifdef SPISRAM_HARDWARESPI
          ' harware SPI mode
          asm showdebug SPI constant used equates to HWSPIMODESCRIPT
          SPIMode HWSPIMODESCRIPT, 0
    #endif


    repeat 2
      set SPISRAM_CS OFF
      SendData_SPISRAM   ( SRAM_23LC_COMMAND_WRMR )
      SendData_SPISRAM   ( SRAM_23LC_BYTE_MODE    )
      set SPISRAM_CS ON
      wait 1 ms
    end repeat

End Sub






'------ eepAddr support for larger memory devices that require three bytes to address memory location

sub SRAMWrite ( in eepAddr as long, eepromVal as byte)

  dim eepromValtoSend as byte
  eepromValtoSend = eepromVal

  set SPISRAM_CS OFF

  SendData_SPISRAM ( SRAM_23LC_COMMAND_WRITE )

  #if SPISRAM_CAPACITY > 0x10000
      'Large device therefore need to send third byte address
      SendData_SPISRAM ( eepAddr_u )
  #endif
  SendData_SPISRAM ( eepAddr_h )
  SendData_SPISRAM ( eepAddr )
  SendData_SPISRAM  ( eepromValtoSend )

  set SPISRAM_CS ON


end sub

'-----

function SRAMRead ( in eepAddr as long )

    SRAMRead ( eepAddr, eepromVal )
    SRAMRead = eepromVal

end function

'------ eepAddr support for larger devices that requires two  byte to address memory location
sub SRAMRead( in eepAddr as long, out eepromVal as byte )


  set SPISRAM_CS OFF

  SendData_SPISRAM ( SRAM_23LC_COMMAND_READ )

  #if SPISRAM_CAPACITY > 0x10000
      'Large device therefore need to send third byte address
      SendData_SPISRAM ( eepAddr_u )
  #endif
  SendData_SPISRAM ( eepAddr_h )
  SendData_SPISRAM ( eepAddr )
  GetData_SPISRAM ( eepromVal )

  set SPISRAM_CS ON

end sub



'''Send a data byte to the SPISRAM
'''@param SPISRAMSendByte Byte to send
'''@hide
sub  SendData_SPISRAM( in SPISRAMSendByte   )

  dim eepromValCounter as byte
  dim _dummyVal as byte alias eepromValCounter
  eepromValCounter = 0

  #ifdef SPISRAM_HARDWARESPI
     SPITransfer  SPISRAMSendByte, _dummyVal
     exit sub
  #endif

  #ifndef SPISRAM_HARDWARESPI
    do

      if SPISRAMSendByte.7 = ON then
        set SPISRAM_DO ON
      else
        set SPISRAM_DO OFF
      end if
      SET SPISRAM_SCK On
      rotate SPISRAMSendByte left
      set SPISRAM_SCK Off
      eepromValCounter++

    loop while eepromValCounter < 8

  #endif

end Sub

sub  GetData_SPISRAM( out eepromVal )

  Dim _dummyByte, eepromVal, eepromValCounter as byte
  _dummyByte = 0x55
  eepromVal  = 0x55
  eepromValCounter = 0

  #ifdef SPISRAM_HARDWARESPI
     SPITransfer  _dummyByte,  eepromVal
  #endif

  #ifndef SPISRAM_HARDWARESPI

      dir SPISRAM_DI   in

      do
          'Clock something out - this is not important
          if SPISRAM_DI = ON  then
            set SPISRAM_DO OFF
          else
            set SPISRAM_DO ON
          end if

          'Device is cpol = 0. Invert SCK if cpol = 1
          SET SPISRAM_SCK ON

          rotate eepromVal left
          if SPISRAM_DI = On then
            set eepromVal.0 On
          Else
            set eepromVal.0 Off
          end if

          SET SPISRAM_SCK OFF

        eepromValCounter++

      loop while eepromValCounter < 8

  #endif

end Sub
