;    Library for reading/writing to Microchip DS1672 RTC for the GCBASIC compiler
;    Copyright (C) 2013-2020 Thomas Henry and Evan Venn
;
;    Version 1.1a  29/1/2015
;
;    This code is free software; you can redistribute it and/or
;    modify it under the terms of the GNU Lesser General Public
;    License as published by the Free Software Foundation; either
;    version 2.1 of the License, or (at your option) any later version.

;    This code is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    Lesser General Public License for more details.

;    If you require a copy of the GNU Lesser General Public
;    License along with this code; please write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;    Created Evan R Venn - Oct 2013 for DS1307
;    adapted further by Thomas Henry, May 26, 2014 for DS1307
;    adapted further and further for DS1672 to add new functionality to comply with DEVICE datasheet by Evan R Venn - Jan 27 2015
;
;    The DS1672 incorporates a 32-bit counter and power-monitoring functions. The 32-bit counter
;    is designed to count seconds and can be used to derive time-of-day, week, month, month, and
;    year by using a software algorithm. A precision, temperature-compensated reference and
;    comparator circuit monitors the status of VCC. When an out-of-tolerance condition occurs, an
;    internal power-fail signal is generated that forces the reset to the active state. When VCC returns to
;    an in-tolerance condition, the reset signal is kept in the active state for a period of time to allow
;    the power supply and processor to stabilize.


;Quick Command Reference:

;DS1672_EnableOscillator(flag [true|false])

;DS1672_ResetClock
;DS1672_SetClock( value as long )
;DS1672_ReadClock( value as long )

;DS1672_SetControl(value)
;DS1672_ReadControl. A function return currrent value
;DS1672_SetControlStatus(value)
;DS1672_ReadControlStatus. A function return currrent value

;DS1672_SetTrickleCharger
;DS1672_ReadTrickleCharger

;DS1672_ClearOscillatorStopFlag
;DS1672_OscillatorStopFlagStatus. A function returning a boolean

;DS1672_ReadRegister. A function
;DS1672_WriteRegister


;See the subroutine definitions below for full details on each.
 dim DS_Value as Long

;----- Constants

 ' These are the addresses of the generic DS1672 Real-Time Clock
 #define DS_AddrWrite 0xD0               'DS1672 uses 0xD0
 #define DS_AddrRead  0xD1               'DS1672 uses 0xD1

 'Addresses for DS1672
 #define DS1672_CounterByte1    0x00    'Start of the time registers - 0x00h
 #define DS1672_CounterByte2    0x01              'TIMEKEEPING REGISTER - 0x01h
 #define DS1672_CounterByte3    0x02              'TIMEKEEPING REGISTER - 0x02h
 #define DS1672_CounterByte4    0x03    'TIMEKEEPING REGISTER - 0x03h

 #define DS1672_ControlRegister   0x04    'Control register
 #define DS1672_TrickleChargeRegister   0x05  'Control Status register

;-----
#define DS1672_Enable DS1672_EnableOscillator
;
;When set to logic 0, the oscillator is started. When set to logic 1, the oscillator is stopped when the DS1672 switches to VBAT.
;This bit is clear (logic 0) when power is first applied. When the DS1672 is powered by VCC, the oscillator is always
;on regardless of the status of the EOSC bit. When EOSC is disabled, all register data is static.
;
sub DS1672_EnableOscillator(in DS_Value)
  ;Enables clock if DS_Value = FALSE, disables if DS_Value = TRUE

  #ifdef HI2C_DATA

     do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
     loop While HI2CAckPollState
     HI2CSend( DS1672_ControlRegister )       ;indicate register
     HI2CReStart
     HI2CSend(DS_AddrRead)
     HI2CReceive(DS_Temp, NACK)               ;get the current value

     if DS_Value.0 = 1 then
        set DS_Temp.7 off            ;CH bit = 0 enables OSC
     else
        set DS_Temp.7 on             ;CH bit = 1 disables  OSC
     end if

     HI2CReStart
     HI2CSend(DS_AddrWrite)
     HI2CSend( DS1672_ControlRegister )                     ;indicate register
     HI2CSend(DS_Temp)                ;now send updated value
     HI2CStop

  #endif

  #ifdef I2C_DATA

     I2CStart
     I2CSend(DS_AddrWrite)
     I2CSend( DS1672_ControlRegister )                     ;indicate register
     I2CStart
     I2CSend(DS_AddrRead)
     I2CReceive(DS_Temp, NACK)                      ;get the current value

     if DS_Value.0 = 1 then
        set DS_Temp.7 off            ;CH bit = 0 enables OSC
     else
        set DS_Temp.7 on             ;CH bit = 1 disables  OSC
     end if

     I2CStart
     I2CSend(DS_AddrWrite)
     I2CSend( DS1672_ControlRegister )                     ;indicate register
     I2CSend(DS_Temp)                ;now send updated value
     I2CStop

  #endif

end sub

;-----

sub DS1672_ResetClock
  ;Reset clock to 0

  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(0)                      ;begin with address 0
      HI2CSend(0)                      ;then set the five
      HI2CSend(0)                      ;consecutive locations
      HI2CSend(0)
      HI2CSend(0)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(0)                      ;begin with address 0
      I2CSend(0)                      ;then set the four
      I2CSend(0)                      ;consecutive locations
      I2CSend(0)
      I2CSend(0)
      I2CStop

  #endif

end sub

;-----

sub DS1672_SetClock(in DS_Value as Long )
  ;Set entire clock

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1672_CounterByte1)               ;begin with address 0
      HI2CSend(DS_Value)
      HI2CSend(DS_Value_H)
      HI2CSend(DS_Value_U)
      HI2CSend(DS_Value_E)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1672_CounterByte1)               ;begin with address 0
      I2CSend(DS_Value)
      I2CSend(DS_Value_H)
      I2CSend(DS_Value_U)
      I2CSend(DS_Value_E)
      I2CStop

   #endif
end sub

;-----

sub DS1672_ReadClock( out DS_Value )

  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1672_CounterByte1)                      ;begin with address 0
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive
      HI2CReceive(DS_Value, ACK)
      HI2CReceive(DS_Value_H, ACK)
      HI2CReceive(DS_Value_U, ACK)
      HI2CReceive(DS_Value_E,   NACK)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1672_CounterByte1)                      ;begin with address 0
      I2CReStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Value, ACK)
      I2CReceive(DS_Value_H, ACK)
      I2CReceive(DS_Value_U, ACK)
      I2CReceive(DS_Value_E,   NACK)
      I2CStop

   #endif

 end sub



;
sub DS1672_SetControl(in MFP_Value)
  ; set the current value of the register

  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1672_ControlRegister)              ;go to register
      HI2CSend(MFP_Value)
      HI2CStop


  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1672_ControlRegister)              ;go to register
      I2CSend(MFP_Value)
      I2CStop

  #endif

end sub

;------
function DS1672_ReadControl
  'read the current value of the register


  #ifdef HI2C_DATA

      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1672_ControlRegister)              ;go to register
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_status , NACK)      ;and get value
      HI2CStop
      DS1672_ReadControl = DS_status

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1672_ControlRegister)              ;go to register
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop
      DS1672_ReadControl = DS_status

  #endif

end function


sub DS1672_SetTrickleCharger(in MFP_Value)
  'New function for DS1672. Sets Multifunction Pin status
  'set the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1672_TrickleChargeRegister)              ;go to register
      HI2CSend(MFP_Value)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1672_TrickleChargeRegister)              ;go to register
      I2CSend(MFP_Value)
      I2CStop

  #endif

end sub

;------
function DS1672_ReadTrickleCharger
  'read the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1672_TrickleChargeRegister)              ;go to register
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_status , NACK)      ;and get value
      HI2CStop
      DS1672_ReadControlStatus = DS_status

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1672_TrickleChargeRegister)              ;go to register
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop
      DS1672_ReadControlStatus = DS_status

  #endif

end function

;-----
sub DS1672_ClearOscillatorStopFlag
  ; Clearing this bit resets Reading this bit shows the Oscillator Control


  #ifdef HI2C_DATA

        do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)               ;inidcate a write
        loop While HI2CAckPollState
        HI2CSend( DS1672_ControlStatus )                     ;indicate register
        HI2CReStart
        HI2CSend(DS_AddrRead)
        HI2CReceive(DS_State, NACK)                      ;get the current value
        set DS_State.7 0                               ;CH bit = 0 disables
        HI2CReStart
        HI2CSend(DS_AddrWrite)
        HI2CSend( DS1672_ControlStatus )                 ;indicate register
        HI2CSend(DS_State)                               ;now send updated value
        HI2CStop

  #endif

  #ifdef I2C_DATA

       I2CStart
       I2CSend(DS_AddrWrite)
       I2CSend( DS1672_ControlStatus )                     ;indicate register
       I2CStart
       I2CSend(DS_AddrRead)
       I2CReceive(DS_State, NACK)                      ;get the current value
       set DS_State.7 0                               ;CH bit = 0 disables
       I2CStart
       I2CSend(DS_AddrWrite)
       I2CSend( DS1672_ControlStatus )                 ;indicate register
       I2CSend(DS_State)                               ;now send updated value
       I2CStop

  #endif

end sub
;-----
function DS1672_OscillatorStopFlagStatus
   ; Reading this bit shows the Oscillator Control


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1672_ControlStatus )                     ;indicate register
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_State, NACK)                        ;get the current status
      HI2CStop
      if DS_State.7 = 0 then
          DS1672_OscillatorStopFlagStatus = false
      else
          DS1672_OscillatorStopFlagStatus = true
      end if

  #endif

  #ifdef I2C_DATA

       I2CStart
       I2CSend(DS_AddrWrite)
       I2CSend( DS1672_ControlStatus )                     ;indicate register
       I2CStart
       I2CSend(DS_AddrRead)
       I2CReceive(DS_State, NACK)                        ;get the current status
       I2CStop
       if DS_State.7 = 0 then
          DS1672_OscillatorStopFlagStatus = false
       else
          DS1672_OscillatorStopFlagStatus = true
       end if

  #endif

end function


;------
function DS1672_ReadRegister ( in DS_Value )
  'read the current value of the register

  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS_Value)              ;go to register
      HI2CStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_status , NACK)      ;and get value
      HI2CStop
      DS1672_ReadRegister = DS_status

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS_Value)              ;go to register
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop
      DS1672_ReadRegister = DS_status

  #endif

end function


;------
sub DS1672_WriteRegister ( in DS_Value, in DS_Temp )
  'read the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
            HI2CSend(DS_Value)              ;go to register
      HI2CSend(DS_Temp)               ;write the value
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS_Value)              ;go to register
      I2CSend(DS_Temp)               ;write the value
      I2CStop

  #endif

end sub
