'    EMC1001 routines for the GCBASIC compiler
'    Copyright (C) 2017-2024 Evan Venn

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

'Default address
#define EMC1001_ADDRESS     0x70   '  slave device address

'  EMC1001 registers
#define EMC1001_TEMP_HI     0       '  temperature value high byte
#define EMC1001_TEMP_STATUS 1       '  Status
#define EMC1001_TEMP_LO     2       '  low byte containing 1/4 deg fraction
#define EMC1001_TEMP_CONFIG 3       '  configuration
#define EMC1001_CONV_RATE   4       '  conversation rate
#define EMC1001_TEMP_LIMIT_HI_H     5       '  temp high limit high byte
#define EMC1001_TEMP_LIMIT_HI_L     6       '  temp high limit low byte
#define EMC1001_TEMP_LIMIT_LO_H     7       '  temp low limit high byte
#define EMC1001_TEMP_LIMIT_LO_L     8       '  temp low limit low byte
#define EMC1001_TEMP_ONE_SHOT       0x0F       '  temp high limit high byte
#define EMC1001_TEMP_THERM_LIMIT    0x20       '  THERM limit
#define EMC1001_TEMP_THERM_HYSTERIS 0x21       '  THERM hysteris
#define EMC1001_TEMP_SMBus_Timeout  0x22       '  SMBus timeout

#define EMC1001_TEMP_PROD_ID        0xFD       '  prod ID
#define EMC1001_TEMP_MANUFACTURE_ID 0xFE       '  manufacturing ID
#define EMC1001_TEMP_REV_NUMBER     0xFF       '  revision number




'Return the read data as the second parameter.
'The first being... the register address

  sub  EMC1001_Read( in _emc_reg, out _emc_Data )

    HI2C2Start
    Hi2C2Send ( EMC1001_ADDRESS )
    HI2C2Send ( _emc_reg )
    HI2C2ReStart
    HI2C2Send( EMC1001_ADDRESS + 1 )         ;set the read flag
    HI2C2Receive( _emc_Data, NACK )           ;read one byte and conclude
    HI2C2Stop

  end sub

'Write the byte data as the second parameter.
'The first being... the register address

  sub  EMC1001_Write( in _emc_reg, in _emc_Data )

    HI2C2Start
    Hi2C2Send ( EMC1001_ADDRESS )
    HI2C2Send ( _emc_reg )
    HI2C2Send( _emc_Data )         ; Send the data
    HI2C2Stop

  end sub
