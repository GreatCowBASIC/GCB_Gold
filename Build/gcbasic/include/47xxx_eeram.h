'   Great Cow BASIC Library for Microchip EERAM - (74XXX)
'   Copyright (C) 2017-2020 Chris Roper
'
'   This library is free software for the  GNU Lesser General Public License please see the bottom of this library.
'
'
' Log of version changes:
'   15/05/2017 - 1.0 Original release - C.A.Roper
'   17/05/2017 - 1.1 New I2C address scheme with fixed offsets &
'       Added Delay costants for Tstore and Trecall &
'       Command EERAM_HWStore() added - C.A.Roper
'   17/05/2017 - 1.1b GPL license added - C.A.Roper
'   20/05/2017 - 1.1c Formatting changes only - no functional changes - E.R.Venn
'   01/06/2017 - Renamed constant for consistency

''' -----------------------------------------------------------
'''
''' EERAM is a 4 or 16 Kbit SRAM with EEPROM backup.
'''
''' Part     Density  VCC       Max. I2C      Tstore  Trecall
''' Number   (bits)   Range     Frequency     Delay   Delay
''' -----------------------------------------------------------
''' 47L04     4K    2.7-3.6V     1 MHz         8ms      2ms
''' 47C04     4K    4.5-5.5V     1 MHz         8ms      2ms
''' 47L16     16K   2.7-3.6V     1 MHz        25ms      5ms
''' 47C16     16K   4.5-5.5V     1 MHz        25ms      5ms
'''
''' The device is organized as 512 x 8 bits or 2,048 x 8 bits of memory
''' and utilizes the I2C serial interface. The 47XXX provides infinite
''' read and write cycles to the SRAM while EEPROM cells provide
''' high-endurance nonvolatile storage of data with more than one
''' million store cycles to EEPROM & a Data retention of > 200 years.
'''
''' With an external capacitor (~10uF), SRAM data is automatically
''' transferred to the EEPROM upon loss of power, giving the advantages
''' of NVRAM whilst eliminating the need for backup batteries.
'''
''' Data can also be backed up manually by using either the Hardware
''' Store pin (HS) or software control.
'''
''' On power-up, the EEPROM data is automatically recalled to the SRAM.
''' EEPROM data Recall can also be initiated through software control.
'''             __ __
'''     Vcap-->|  U  |<-- Vcc
'''      A1 -->|     |<-- HS
'''      A2 -->|     |<-> SCL
'''     Vss -->|_____|<-> SDA
'''
''' These are the user oriented functions/subroutines:
'''
''' EERAM_Status()          ; Read the Status Register.
''' EERAM_AutoStore(ON)     ; Enable Automatic Storage on power loss
''' EERAM_AutoStore(OFF)    ; Disable Automatic Storage on power loss
''' EERAM_Backup()          ; Backup / Store Now
''' EERAM_Recall()          ; Restore Now
''' EERAM_HWStore()         ; Force Backup with HS Pin
''' EERAM_Write(Adr, Data)  ; Write a Byte of Data to address
''' EERAM_Read(Address)     ; Read a Byte of Data from address
'''
''' To use the Library the 47XXX Device Address must be defined
''' With ONE of  the following Constants acording to the A1/A2 pins:
'''
'''   A1 Vss, A2 Vss - #define EERAM_I2C_Adr 0x30
'''   A1 Vdd, A2 Vss - #define EERAM_I2C_Adr 0x34
'''   A1 Vss, A2 Vdd - #define EERAM_I2C_Adr 0x38
'''   A1 Vdd, A2 Vdd - #define EERAM_I2C_Adr 0x3C
'''
''' I2C Discovery will show TWO addresses for the device, the Primary
''' address i.e. 0x30 and a Secondary address offset by 0x70 i.e. 0xA0
'''
''' The Hardware Store function is optional but if needed:
'''   #define EERAM_HS PortB.1 ; RST - Optional hardware Store Pin
'''
''' The library transparently supports both software and hardware I2C
''' but the appropriate I2C service should be initialised by the user.
'''

'''********************************************************************

' Default I2C Address
#define EERAM_I2C_Adr 0x30

#define EERAM_Tstore  25  ' Refer to device table above for options
#define EERAM_Trecall  5  ' Refer to device table above for options

' Optional hardware Store Pin
#define EERAM_HS   PortB.1

Dir EERAM_HS Out : EERAM_HS = 0

Function EERAM_Status() as Byte
  ;Read from the Control Register.
  dim EERAM_Data as Byte

  #ifdef HI2C_DATA

    HI2CStart
    HI2CSend(EERAM_I2C_Adr + 0x01)  ; initiate a Read
    HI2CReceive(EERAM_Data)         ; Read Addressed Data Byte

    HI2CStop                        ; generate a stop signal

  #endif

  #ifdef I2C_DATA
    I2CStart
    I2CSend(EERAM_I2C_Adr + 0x01)   ; initiate read mode
    I2CReceive(EERAM_Data)          ; Read Addressed Data Byte
    I2CStop
  #endif

  EERAM_Status = EERAM_Data         ; Return Result

end function

Sub EERAM_AutoStore(in EERAM_Mode)
  if EERAM_Mode = ON then
    EERAM_Write_Command(0x00, 0x82)
  else
    EERAM_Write_Command(0x00, 0x80)
  end if
; the 47XXX cannot be accessed for Twc time after the Stop condition
  wait 1 ms ; Twc = STATUS Register Write Cycle Time
End Sub

Sub EERAM_Backup()
   EERAM_Write_Command(0x55, 0x33)
   wait EERAM_Tstore ms ;  Tstore = Store Operation Duration
End Sub

Sub EERAM_Recall()
   EERAM_Write_Command(0x55, 0xDD)
   wait EERAM_Trecall ms ;  Trecall = Recall Operation Duration
End Sub

Macro EERAM_HWStore()
  PulseOut EERAM_HS, 1 us
  wait EERAM_Tstore ms ;  Tstore = Store Operation Duration
  wait 1 ms ; Twc = STATUS Register Write Cycle Time
end Macro

sub EERAM_Write(in EERAM_Adr as word, in EERAM_Data as byte)
  ;Write to the internal RAM.

  #ifdef HI2C_DATA

    do

      HI2CStart                      ; generate a start signal
      HI2CSend(EERAM_I2C_Adr + 0x70) ; initiate a write

    loop While HI2CAckPollState

    HI2CSend(EERAM_Adr_H)           ; send Memory Address MSB
    HI2CSend(EERAM_Adr)             ; send Memory Address LSB
    HI2CSend(EERAM_Data)            ; send Data Byte

    HI2CStop

  #endif

  #ifdef I2C_DATA

    I2CStart

    I2CSend(EERAM_I2C_Adr + 0x70)   ; initiate write mode
    I2CSend(EERAM_Adr_H)            ; send Memory Address MSB
    I2CSend(EERAM_Adr)              ; send Memory Address LSB
    I2CSend(EERAM_Data)             ; send Data Byte

    I2CStop

  #endif


end sub

Function EERAM_Read(in EERAM_Adr as word) as Byte
  ;Read from the internal RAM.
  dim EERAM_Data as Byte

  #ifdef HI2C_DATA

    do

      HI2CReStart                    ; generate a start signal
      HI2CSend(EERAM_I2C_Adr + 0x70) ; initiate a write

    loop While HI2CAckPollState

    HI2CSend(EERAM_Adr_H)            ; send Memory Address MSB
    HI2CSend(EERAM_Adr)              ; send Memory Address LSB

    HI2CReStart

    HI2CSend(EERAM_I2C_Adr + 0x71)   ; initiate read mode
    HI2CReceive(EERAM_Data, NACK)    ; Read Addressed Data Byte

    HI2CStop                         ; generate a stop signal

  #endif

  #ifdef I2C_DATA

    I2CStart

    I2CSend(EERAM_I2C_Adr + 0x70)    ; initiate a write
    I2CSend(EERAM_Adr_H)             ; send Memory Address MSB
    I2CSend(EERAM_Adr)               ; send Memory Address LSB

    I2CStart
    I2CSend(EERAM_I2C_Adr + 0x71)    ; initiate read mode
    I2CReceive(EERAM_Data, NACK)     ; Read Addressed Data Byte

    I2CStop

  #endif

  EERAM_Read = EERAM_Data            ; Return Result

end function

sub EERAM_Write_Command(in EERAM_Reg as byte, in EERAM_Data as byte)
  ;Write to the Command Register.

  #ifdef HI2C_DATA

    do

      HI2CStart                      ; generate a start signal
      HI2CSend(EERAM_I2C_Adr + 0x00) ; initiate a write

    loop While HI2CAckPollState

    HI2CSend(EERAM_Reg)              ; send Register Address
    HI2CSend(EERAM_Data)             ; send Register Mask

    HI2CStop

  #endif

  #ifdef I2C_DATA

    I2CStart

    I2CSend(EERAM_I2C_Adr + 0x00)    ; initiate write mode
    I2CSend(EERAM_Reg)               ; send Register Address
    I2CSend(EERAM_Data)              ; send Register Mask

    I2CStop

  #endif

end sub

'''--------------GNU Lesser General Public License--------------------
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to:
'
'    the Free Software Foundation, Inc.,
'       51 Franklin St,
'       Fifth Floor,
'       Boston,
'       MA  02110-1301  USA
'
'''--------------------------------------------------------------------
