'    Library for reading/writing to Microchip MCP7940N RTC for the GCBASIC compiler
'    Copyright (C) 2012-2026 Thomas Henry, Pete Reverett and Evan Venn
'
'    Version 1.1a  12/1/2015 - 2017
'
'    This code is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.

'    This code is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.

'    If you require a copy of the GNU Lesser General Public
'    License along with this code; please write to the Free Software
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

'    Created Evan R Venn - Oct 2013 for DS1307
'    adapted further by Thomas Henry, May 26, 2014 for DS1307
'    adapted further for MCP7940N Special Christmas 2014 release by Pete Everett, Dec 14, 2014
'    Revised and added new functionality to comply with MCP7940N datasheet by Evan R Venn - Jan 1 2015
'    Revised and added MCP7940_ReadFailureClock functionality to comply with MCP7940N datasheet by Evan R Venn - Jan 2 2015
'    Revised by Evan R Venn - Jan 4 2015. As follows:
'            Correct SetAlarm and ReadAlarm usage of the Month parameter and Date and Month respectively.
'            Improve documentation for SetControl and SetAlarmMask
'    Revised by Evan R Venn - Jan 11 2015 to add hardware i2c suppor
'    Revised 28.08.2017 to resolve non-ANSI characters


;12 bytes are used as input and output parameters. They are:
;DS_Value, DS_Addr, DS_Hour, DS_Min, DS_Sec, DS_A_P, DS_Date
;DS_Month, DS_Year and DS_DOW.
;DS_Status, DS_temp

;Quick Command Reference:

;MCP7940_EnableOscillator(flag [true|false])
;MCP7940_BatteryBackup(flag [true|false])
;MCP7940_ClearPowerFail
;MCP7940_PowerFailStatus. This function return true or false, You can then read the date information for failures.
;MCP7940_ResetClock
;MCP7940_SetClock(hour, minute, second, DOW, date, month, year)
;MCP7940_SetTime(hour, minute, second)
;MCP7940_SetDate(DOW, date, month, year)
;MCP7940_ReadClock(hour, minute, second, a.m.|p.m., DOW, date, month, year)
;MCP7940_ReadTime(hour, minute, second, a.m.|p.m.)
;MCP7940_ReadDate(DOW, date, month, year)
;MCP7940_SetHourMode(12|24)
;MCP7940_ReadHourMode(12|24)
;MCP7940_SetSQW(rate)
;MCP7940_SetMFP(True | False OR On | Off)
;MCP7940_SetControl(value)
;MCP7940_ReadControl. A function return currrent value
;MCP7940_Write(address, value)
;MCP7940_Read(address, value)
;MCP7940_SetAlarm ( [0 |  1] , hour, minute, second, DOW, date, month )
;MCP7940_SetAlarmMask ( [0 |  1] , alarmAssertionMatch )
;MCP7940_ReadAlarm ( [0 |  1] , hour, minute, second, DOW, date, month ). Return values
;MCP7940_ClearAlarm ( [0 |  1] )
;MCP7940_AlarmStatus ( [0 |  1] )
;MCP7940_SetAlarmPolarity ( On | Off )
;MCP7940_EnableAlarm ( [0 |  1] )
;MCP7940_DisableAlarm ( [0 |  1] )
;MCP7940_ReadFailureClock ( [0 |  1] , hour, minute, DOW, date, month ). Return values.  The MCP7940N includes a power-fail time-stamp module that stores the minutes, hours, date, and month when primary power is lost and when it is restored


;See the subroutine definitions below for full details on each.

;----- Constants

 ' These are the addresses of the generic MCP7940N Real-Time Clock
 #define DS_AddrWrite 0xDE    'MCP7940 uses 0xDE
 #define DS_AddrRead  0xDF    'MCP7940 uses 0xDF

 'Addresses for MCP7940N
 #define MCP7940_RTCSEC    0x00    'Start of the time registers - 0x00h
 #define MCP7940_RTCMIN    0x01                    'TIMEKEEPING MINUTES VALUE REGISTER - 0x01h
 #define MCP7940_RTCHOUR   0x02                    'TIMEKEEPING HOURS VALUE REGISTER - 0x02h
 #define MCP7940_RTCWKDAY  0x03    'TIMEKEEPING WEEKDAY VALUE REGISTER - 0x03h
 #define MCP7940_RTCDATE   0x04    'TIMEKEEPING DATE VALUE REGISTER  - 0x04h
 #define MCP7940_RTCMTH    0x05                    'TIMEKEEPING MONTH VALUE REGISTER - 0x05h
 #define MCP7940_RTCYEAR   0x06                    'TIMEKEEPING YEAR VALUE REGISTER - 0x06h

 #define MCP7940_Control   0x07    'Control register
 #define MCP7940_OSCTRIM   0x08                    'OSCILLATOR DIGITAL TRIM REGISTER

 #define MCP7940_ALM0SEC   0x0A            ' ALARM 0 SECONDS VALUE REGISTER
 #define MCP7940_ALM1SEC   0x11            ' ALARM 1 SECONDS VALUE REGISTER

 #define MCP7940_ALM0MIN   0x0B            ' ALARM 0 MINUTES VALUE REGISTER
 #define MCP7940_ALM1MIN   0x12            ' ALARM 1 MINUTES VALUE REGISTER

 #define MCP7940_ALM0HOUR  0x0C            ' ALARM 0 HOURS VALUE REGISTER
 #define MCP7940_ALM1HOUR  0x13            ' ALARM 1 HOURS VALUE REGISTER

 #define MCP7940_ALM0WKDAY  0x0D             ' ALARM 0 WEEKDAY VALUE REGISTER
 #define MCP7940_ALM1WKDAY  0x14             ' ALARM 1 WEEKDAY VALUE REGISTER

 #define MCP7940_ALM0DATE  0x0E            ' ALARM 0 DATE VALUE REGISTER
 #define MCP7940_ALM1DATE  0x15            ' ALARM 1 DATE VALUE REGISTER

 #define MCP7940_ALM0MONTH  0x0F             ' ALARM 0 MONTH VALUE REGISTER
 #define MCP7940_ALM1MONTH  0x16             ' ALARM 1 MONTH VALUE REGISTER


 #define MCP7940_PWR00MIN   0x18             ' POWER-DOWN/POWER-UP TIME-STAMP MINUTES VALUE REGISTER (ADDRESSES 0x18/0x1C)
 #define MCP7940_PWR01MIN   0x1C

 #define MCP7940_PWR00HOUR   0x19            ' POWER-DOWN/POWER-UP TIME-STAMP HOURS VALUE REGISTER (ADDRESSES 0x19/0x1D)
 #define MCP7940_PWR01HOUR   0x1D

 #define MCP7940_PWR00DATE   0x1A            ' POWER-DOWN/POWER-UP TIME-STAMP DATE VALUE REGISTER (ADDRESSES 0x1A/0x1E)
 #define MCP7940_PWR01DATE   0x1E

 #define MCP7940_PWR00MTH   0x1B             ' POWER-DOWN/POWER-UP TIME-STAMP MONTH VALUE REGISTER (ADDRESSES 0x1B/0x1F)
 #define MCP7940_PWR01MTH   0x1F

 ;The alarm can be set to go off if any of the following conditions are met
 #define MCP7940_AlarmAssertion_Seconds            0x00                 ; a match of the seconds
 #define MCP7940_AlarmAssertion_Minutes            0x01                 ; a match of the minutes
 #define MCP7940_AlarmAssertion_Hours              0x02                 ; a match of the hours
 #define MCP7940_AlarmAssertion_DayofWeek          0x03                 ; a match of the day of the week
 #define MCP7940_AlarmAssertion_Date               0x04                 ; a match of the date
 #define MCP7940_AlarmAssertion_All                0x07                 ; a match of all paraemters equals all seconds, minutes, hours, day of week, day and month match.



;-----

function DecToBcd(in va ) as byte
  ;Convert pure decimal number to binary coded decimal
  DecToBcd=( va /10)*16+ va %10
end function

;-----

function BcdToDec(in va ) as byte
  ;Convert binary coded decimal to pure decimal
  BcdToDec=( va /16)*10+ va %16
end function

;-----
#define MCP7940_Enable MCP7940_EnableOscillator
sub MCP7940_EnableOscillator(in DS_Value)
  ;Enables clock if DS_Value = TRUE, disables if DS_Value = FALSE
  'MCP7940 requires bit 7 of the 0x00 register to be set to 1 to enable the oscillator.  DS1370 required this bit to be set to 0.

  #ifdef HI2C_DATA
   do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
   loop While HI2CAckPollState
   HI2CSend( MCP7940_RTCSEC )                     ;indicate register 0.
   HI2CReStart
   HI2CSend(DS_AddrRead)
   HI2CReceive(DS_Sec, NACK)                      ;get the current seconds
   if DS_Value.0 = 1 then
      set DS_Sec.7 on           ;CH bit = 1 enables
   else
      set DS_Sec.7 off           ;CH bit = 0 disables
   end if
   HI2CReStart
   HI2CSend(DS_AddrWrite)
   HI2CSend( MCP7940_RTCSEC )                     ;indicate register 0
   HI2CSend(DS_Sec)                ;now send updated value
   HI2CStop
  #endif

  #ifdef I2C_DATA
   I2CStart
   I2CSend(DS_AddrWrite)
   I2CSend( MCP7940_RTCSEC )                     ;indicate register 0.
   I2CStart
   I2CSend(DS_AddrRead)
   I2CReceive(DS_Sec, NACK)                      ;get the current seconds
   if DS_Value.0 = 1 then
      set DS_Sec.7 on           ;CH bit = 1 enables
   else
      set DS_Sec.7 off           ;CH bit = 0 disables
   end if
   I2CStart
   I2CSend(DS_AddrWrite)
   I2CSend( MCP7940_RTCSEC )                     ;indicate register 0
   I2CSend(DS_Sec)                ;now send updated value
   I2CStop
  #endif

end sub


sub MCP7940_BatteryBackup(in DS_Value)
  ;Enables battery if DS_Value = TRUE, disables if DS_Value = FALSE
  'MCP7940 requires bit 3 of the RTCWKDAY register to be set to 1 to enable the battery support.

  #ifdef HI2C_DATA
   do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
   loop While HI2CAckPollState
   HI2CSend( MCP7940_RTCWKDAY )                     ;indicate register
   HI2CReStart
   HI2CSend(DS_AddrRead)
   HI2CReceive(DS_State, NACK)                      ;get the current seconds
   if DS_Value.0 = 1 then
      set DS_State.3 on                            ;CH bit = 1 enables
   else
      set DS_State.3 off                           ;CH bit = 0 disables
   end if
   HI2CReStart
   HI2CSend(DS_AddrWrite)
   HI2CSend( MCP7940_RTCWKDAY )                     ;indicate register
   HI2CSend(DS_State)                               ;now send updated value
   HI2CStop
  #endif


  #ifdef I2C_DATA
   I2CStart
   I2CSend(DS_AddrWrite)
   I2CSend( MCP7940_RTCWKDAY )                     ;indicate register
   I2CStart
   I2CSend(DS_AddrRead)
   I2CReceive(DS_State, NACK)                      ;get the current seconds
   if DS_Value.0 = 1 then
      set DS_State.3 on                            ;CH bit = 1 enables
   else
      set DS_State.3 off                           ;CH bit = 0 disables
   end if
   I2CStart
   I2CSend(DS_AddrWrite)
   I2CSend( MCP7940_RTCWKDAY )                     ;indicate register
   I2CSend(DS_State)                               ;now send updated value
   I2CStop
  #endif

end sub

sub MCP7940_ResetClock
  ;Reset clock to 00:00:00 01 01/01/00.
  ;Also sets 24-hour mode and enables the clock.

  #ifdef HI2C_DATA
   do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
   loop While HI2CAckPollState
   HI2CSend( MCP7940_RTCSEC )       ;begin with address 0
   HI2CSend(0x80)                   ;then set the seven, setting bit 7 of address 0
   HI2CSend(0)                      ;consecutive locations
   HI2CSend(0x40)                   ;24-hour mode
   HI2CSend(1)
   HI2CSend(1)
   HI2CSend(1)
   HI2CSend(0)
   HI2CStop
  #endif

  #ifdef I2C_DATA
    I2CStart
    I2CSend(DS_AddrWrite)
    I2CSend( MCP7940_RTCSEC )       ;begin with address 0
    I2CSend(0x80)                   ;then set the seven, setting bit 7 of address 0
    I2CSend(0)                      ;consecutive locations
    I2CSend(0x40)                   ;24-hour mode
    I2CSend(1)
    I2CSend(1)
    I2CSend(1)
    I2CSend(0)
    I2CStop
  #endif

end sub

;-----

sub MCP7940_SetClock(in DS_Hour, in DS_Min, in DS_Sec, in DS_DOW, in DS_Date, in DS_Month, in DS_Year)
  ;Set entire clock: hours, minutes, seconds, day of week, date, month, year
  ;Also sets 24-hour mode and enables the clock.

  #ifdef HI2C_DATA
    do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
    loop While HI2CAckPollState
    HI2CSend(MCP7940_RTCSEC)               ;begin with address 0
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_Value, NACK)            ;get the seconds to obtain ST bit value
    DS_Value = BcdToDec(DS_Value & 128)   ;get ST value from bit 7

    HI2CReStart
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCSEC)               ;begin with address 0
    DS_Sec = DS_Sec or DS_Value
    HI2CSend(DecToBcd(DS_Sec))             ;then set the next two
    HI2CSend(DecToBcd(DS_Min))             ;consecutive values

    HI2CReStart                               ;restart
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCHour)               ;begin with address 0
    HI2CSend( DecToBcd(DS_Hour) )

    HI2CReStart                               ;set DOW by extracting the bits
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCWKDAY)
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_Value, NACK)                   ;get the seconds to obtain bit 7 an 6 values
    DS_Value = DS_Value & 56     ;get bit values from bit 5,4 and 3
    DS_DOW = DecToBcd(DS_DOW) or DS_Value             ;set bit values of bit 5,4 and 3

    HI2CReStart                               ;restart
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCWKDAY)
    HI2CSend(DS_DOW)

    HI2CSend(DecToBcd(DS_Date))
    HI2CSend(DecToBcd(DS_Month))          ; bit 5 is READ only, so, just overwrite
    HI2CSend(DecToBcd(DS_Year))
    HI2CStop
  #endif

  #ifdef I2C_DATA
    I2CStart
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCSEC)               ;begin with address 0
    I2CStart
    I2CSend(DS_AddrRead)
    I2CReceive(DS_Value, NACK)            ;get the seconds to obtain ST bit value
    DS_Value = BcdToDec(DS_Value & 128)   ;get ST value from bit 7

    I2CStart
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCSEC)               ;begin with address 0
    DS_Sec = DS_Sec or DS_Value
    I2CSend(DecToBcd(DS_Sec))             ;then set the next two
    I2CSend(DecToBcd(DS_Min))             ;consecutive values

    I2CStart                               ;restart
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCHour)               ;begin with address 0
    I2CSend( DecToBcd(DS_Hour) )

    I2CStart                               ;set DOW by extracting the bits
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCWKDAY)
    I2CStart
    I2CSend(DS_AddrRead)
    I2CReceive(DS_Value, NACK)                   ;get the seconds to obtain bit 7 an 6 values
    DS_Value = DS_Value & 56     ;get bit values from bit 5,4 and 3
    DS_DOW = DecToBcd(DS_DOW) or DS_Value             ;set bit values of bit 5,4 and 3

    I2CStart                               ;restart
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCWKDAY)
    I2CSend(DS_DOW)

    I2CSend(DecToBcd(DS_Date))
    I2CSend(DecToBcd(DS_Month))          ; bit 5 is READ only, so, just overwrite
    I2CSend(DecToBcd(DS_Year))
    I2CStop
  #endif

end sub

;-----

sub MCP7940_SetTime(in DS_Hour, in DS_Min, in DS_Sec)
  ;Set time only: hours, minutes, seconds.
  ;Also sets 24-hour mode and enables the clock.
  '

  #ifdef HI2C_DATA
    do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
    loop While HI2CAckPollState
    HI2CSend(MCP7940_RTCSEC)               ;begin with address 0
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_Value, ACK)                  ;get the seconds to obtain ST bit value
    DS_Value = BcdToDec(DS_Value & 128)   ;get ST value from bit 7

    HI2CReStart
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCSEC)               ;begin with address
    DS_Sec = DS_Sec or DS_Value
    HI2CSend(DecToBcd(DS_Sec))             ;then set the next two
    HI2CSend(DecToBcd(DS_Min))             ;consecutive values

    HI2CReStart
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCHour)               ;begin with address
    HI2CSend( DecToBcd(DS_Hour) )
    HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_RTCSEC)               ;begin with address 0
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Value, ACK)                  ;get the seconds to obtain ST bit value
      DS_Value = BcdToDec(DS_Value & 128)   ;get ST value from bit 7

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_RTCSEC)               ;begin with address
      DS_Sec = DS_Sec or DS_Value
      I2CSend(DecToBcd(DS_Sec))             ;then set the next two
      I2CSend(DecToBcd(DS_Min))             ;consecutive values

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_RTCHour)               ;begin with address
      I2CSend( DecToBcd(DS_Hour) )
      I2CStop
  #endif

end sub

;-----

sub MCP7940_SetDate(in DS_DOW, DS_Date, in DS_Month, in DS_Year)
  ;Set date only: day of week, date, month, year
  ;There is no error detection for out-of-range dates.

  #ifdef HI2C_DATA
    do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
    loop While HI2CAckPollState
    HI2CSend(MCP7940_RTCWKDAY)
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_Value, NACK)                   ;get the seconds to obtain bit 7 an 6 values
    DS_Value = DS_Value & 56     ;get bit values from bit 5,4 and 3
    DS_DOW = DecToBcd(DS_DOW) or DS_Value             ;set bit values of bit 5,4 and 3

    HI2CReStart                               ;restart
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCWKDAY)
    HI2CSend(DS_DOW)

    HI2CSend(DecToBcd(DS_Date))
    HI2CSend(DecToBcd(DS_Month))          ; bit 5 is READ only, so, just overwrite
    HI2CSend(DecToBcd(DS_Year))
    HI2CStop
  #endif

  #ifdef I2C_DATA
    I2CStart                               ;set DOW by extracting the bits
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCWKDAY)
    I2CStart
    I2CSend(DS_AddrRead)
    I2CReceive(DS_Value, NACK)                   ;get the seconds to obtain bit 7 an 6 values
    DS_Value = DS_Value & 56     ;get bit values from bit 5,4 and 3
    DS_DOW = DecToBcd(DS_DOW) or DS_Value             ;set bit values of bit 5,4 and 3

    I2CStart                               ;restart
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCWKDAY)
    I2CSend(DS_DOW)

    I2CSend(DecToBcd(DS_Date))
    I2CSend(DecToBcd(DS_Month))          ; bit 5 is READ only, so, just overwrite
    I2CSend(DecToBcd(DS_Year))
    I2CStop
  #endif


 end sub

;-----

sub MCP7940_ReadClock(out DS_Hour, out DS_Min, out DS_Sec, out DS_A_P, out DS_DOW, out DS_Date, out DS_Month, out DS_Year)
;Read entire clock: hours, minutes, seconds, a.m. or p.m., day of week, date, month, year
;DS_A_P = 0 means a.m. DS_A_P = 1 means p.m.

  #ifdef HI2C_DATA
    do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
    loop While HI2CAckPollState
    HI2CSend(MCP7940_RTCSEC)                      ;begin with address 0
    HI2CStop
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_Sec, ACK)              ;get seconds
    DS_Sec = BcdToDec(DS_Sec & 127) ;strip off CH bit

    HI2CReceive(DS_Min, ACK)              ;get minutes
    DS_Min = BcdToDec(DS_Min)       ;bit 7 is always 0

    HI2CReceive(DS_Hour, ACK)             ;get hours
    if DS_Hour.6 then               ;12-hour mode
      DS_A_P = DS_Hour.5            ;a.m. or p.m.
      DS_Hour = BcdToDec(DS_Hour & 31)
    else
      DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
      DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
    end if

    HI2CReceive(DS_DOW, ACK)              ;get day of week
    DS_DOW = BcdToDec(DS_DOW & 7 )

    HI2CReceive(DS_Date, ACK)             ;get date
    DS_Date = BcdToDec(DS_Date)

    HI2CReceive(DS_Month, ACK)            ;get month
    DS_Month = BcdToDec(DS_Month & 31)

    HI2CReceive(DS_Year, NACK)       ;get year
    DS_Year = BcdToDec(DS_Year)
    HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_RTCSEC)                      ;begin with address 0
      I2CStop
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Sec, ACK)              ;get seconds
      DS_Sec = BcdToDec(DS_Sec & 127) ;strip off CH bit

      I2CReceive(DS_Min, ACK)              ;get minutes
      DS_Min = BcdToDec(DS_Min)       ;bit 7 is always 0

      I2CReceive(DS_Hour, ACK)             ;get hours
      if DS_Hour.6 then               ;12-hour mode
        DS_A_P = DS_Hour.5            ;a.m. or p.m.
        DS_Hour = BcdToDec(DS_Hour & 31)
      else
        DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if

      I2CReceive(DS_DOW, ACK)              ;get day of week
      DS_DOW = BcdToDec(DS_DOW & 7 )

      I2CReceive(DS_Date, ACK)             ;get date
      DS_Date = BcdToDec(DS_Date)

      I2CReceive(DS_Month, ACK)            ;get month
      DS_Month = BcdToDec(DS_Month & 31)

      I2CReceive(DS_Year, NACK)       ;get year
      DS_Year = BcdToDec(DS_Year)
      I2CStop
  #endif

 end sub

;-----

sub MCP7940_ReadTime(out DS_Hour, out DS_Min, out DS_Sec, out DS_A_P)
  ;Read time only: hours, minutes, seconds, a.m. or p.m.

  #ifdef HI2C_DATA
    do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
    loop While HI2CAckPollState
    HI2CSend(MCP7940_RTCSEC)                      ;begin with address 0
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_Sec, ACK)              ;get the seconds
    DS_Sec = BcdToDec(DS_Sec & 127) ;strip off CH bit

    HI2CReceive(DS_Min, ACK)              ;get the minutes
    DS_Min = BcdToDec(DS_Min)

    HI2CReceive(DS_Hour, NACK)       ;get the hours
    if DS_Hour.6 then               ;12-hour mode
      DS_A_P = DS_Hour.5            ;a.m. or p.m.
      DS_Hour = BcdToDec(DS_Hour & 31)
    else
      DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
      DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
    end if
    HI2CStop
  #endif

  #ifdef I2C_DATA
    I2CStart
    I2CSend(DS_AddrWrite)
    I2CSend(MCP7940_RTCSEC)                      ;begin with address 0
    I2CStart
    I2CSend(DS_AddrRead)
    I2CReceive(DS_Sec, ACK)              ;get the seconds
    DS_Sec = BcdToDec(DS_Sec & 127) ;strip off CH bit

    I2CReceive(DS_Min, ACK)              ;get the minutes
    DS_Min = BcdToDec(DS_Min)

    I2CReceive(DS_Hour, NACK)       ;get the hours
    if DS_Hour.6 then               ;12-hour mode
      DS_A_P = DS_Hour.5            ;a.m. or p.m.
      DS_Hour = BcdToDec(DS_Hour & 31)
    else
      DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
      DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
    end if
    I2CStop
  #endif

end sub

;-----

sub MCP7940_ReadDate(out DS_DOW, out DS_Date, out DS_Month, out DS_Year)
;Get date only: day of week, date, month, year

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(MCP7940_RTCWKDAY)
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_DOW, ACK)              ;get day of week

      DS_DOW = DS_DOW & 7             'get the bits we are interested in
      DS_Date = BcdToDec(DS_DOW)
      HI2CReceive(DS_Date, ACK)             ;get date
      DS_Date = BcdToDec(DS_Date)
      HI2CReceive(DS_Month, ACK)            ;get month
      DS_Month = BcdToDec(DS_Month & 31 )
      HI2CReceive(DS_Year, NACK)       ;get year
      DS_Year = BcdToDec(DS_Year)
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_RTCWKDAY)
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_DOW, ACK)              ;get day of week

      DS_DOW = DS_DOW & 7             'get the bits we are interested in
      DS_Date = BcdToDec(DS_DOW)
      I2CReceive(DS_Date, ACK)             ;get date
      DS_Date = BcdToDec(DS_Date)
      I2CReceive(DS_Month, ACK)            ;get month
      DS_Month = BcdToDec(DS_Month & 31 )
      I2CReceive(DS_Year, NACK)       ;get year
      DS_Year = BcdToDec(DS_Year)
      I2CStop
  #endif

end sub

;-----

sub MCP7940_SetHourMode(in DS_A_P)
  ;Set hour mode. 12 = 12-hour, all else = 24-hour

  #ifdef HI2C_DATA
    do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
    loop While HI2CAckPollState
    HI2CSend(MCP7940_RTCHOUR)        ;located in address 2
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_Hour, NACK)       ;get current hours and mode
    if DS_A_P = 12 then             ;change to 12-hour mode
      if !DS_Hour.6 then            ;only if in 24-hour mode
        DS_Hour = BcdToDec(DS_Hour) ;read current hour
        if DS_Hour > 12 then        ;it's a p.m.
          DS_Hour -= 12             ;reduce to 12-hour mode
          DS_Hour = DecToBcd(DS_Hour) ;convert to BCD
          set DS_Hour.5 on          ;indicate p.m.
        else
          DS_Hour = DecToBcd(DS_Hour)
        end if
        set DS_Hour.6 on            ;indicate 12-hour mode now
      end if
    else                            ;change to 24-hour mode
      if DS_Hour.6 then             ;only if in 12-hour mode
        DS_A_P = DS_Hour & 32       ;save am/pm flag
        DS_Hour = BcdToDec(DS_Hour & 31)  ;strip it out
        if DS_A_P then              ;p.m. time
          DS_Hour += 12             ;convert to 24-hour format
        end if
        DS_Hour = DecToBcd(DS_Hour) ;top 2 bits are 0
      end if
    end if
    HI2CReStart
    HI2CSend(DS_AddrWrite)
    HI2CSend(MCP7940_RTCHOUR)
    HI2CSend(DS_Hour)                ;send hours and flag back again
    HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_RTCHOUR)        ;located in address 2
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Hour, NACK)       ;get current hours and mode
      if DS_A_P = 12 then             ;change to 12-hour mode
        if !DS_Hour.6 then            ;only if in 24-hour mode
          DS_Hour = BcdToDec(DS_Hour) ;read current hour
          if DS_Hour > 12 then        ;it's a p.m.
            DS_Hour -= 12             ;reduce to 12-hour mode
            DS_Hour = DecToBcd(DS_Hour) ;convert to BCD
            set DS_Hour.5 on          ;indicate p.m.
          else
            DS_Hour = DecToBcd(DS_Hour)
          end if
          set DS_Hour.6 on            ;indicate 12-hour mode now
        end if
      else                            ;change to 24-hour mode
        if DS_Hour.6 then             ;only if in 12-hour mode
          DS_A_P = DS_Hour & 32       ;save am/pm flag
          DS_Hour = BcdToDec(DS_Hour & 31)  ;strip it out
          if DS_A_P then              ;p.m. time
            DS_Hour += 12             ;convert to 24-hour format
          end if
          DS_Hour = DecToBcd(DS_Hour) ;top 2 bits are 0
        end if
      end if
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_RTCHOUR)
      I2CSend(DS_Hour)                ;send hours and flag back again
      I2CStop
  #endif

end sub

;-----

sub MCP7940_ReadHourMode(out DS_A_P)
  ;Return the current hour mode, 12 or 24

  #ifdef HI2C_DATA
    do
      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)                     ;inidcate a write
    loop While HI2CAckPollState
    HI2CSend(2)                      ;go to address 2
    HI2CReStart
    HI2CSend(DS_AddrRead)
    HI2CReceive(DS_A_P, NACK)        ;get entire hour byte
    HI2CStop

    if DS_A_P.6 then                ;read the mode bit
      DS_A_P = 12                   ;and translate
    else
      DS_A_P = 24
    end if
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(2)                      ;go to address 2
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_A_P, NACK)        ;get entire hour byte
      I2CStop

      if DS_A_P.6 then                ;read the mode bit
        DS_A_P = 12                   ;and translate
      else
        DS_A_P = 24
      end if
  #endif

end sub

;-----
function MCP7940_IsLeapYear

  #ifdef HI2C_DATA
     do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
     loop While HI2CAckPollState
     HI2CSend( MCP7940_RTCMTH )                     ;indicate register
     HI2CReStart
     HI2CSend(DS_AddrRead)
     HI2CReceive(DS_State, NACK)                      ;get the current seconds
     HI2CStop

     if  DS_State.6 = 1 then
         MCP7940_IsLeapYear = true
     else
         MCP7940_IsLeapYear = false
     end if
  #endif

  #ifdef I2C_DATA
     I2CStart
     I2CSend(DS_AddrWrite)
     I2CSend( MCP7940_RTCMTH )                     ;indicate register
     I2CStart
     I2CSend(DS_AddrRead)
     I2CReceive(DS_State, NACK)                      ;get the current seconds
     I2CStop

     if  DS_State.6 = 1 then
         MCP7940_IsLeapYear = true
     else
         MCP7940_IsLeapYear = false
     end if
  #endif

end function


;-----

sub MCP7940_SetSQW(in DS_Value)
  ;Set square wave output pin mode

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
     'read the current value of the register
      HI2CSend(MCP7940_Control)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Status, NACK)        ;get entire byte
      HI2CStop


      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(MCP7940_Control)                      ;location is address
      select case DS_Value
         case 0                       ;0 = disable completely
              'turn off the enable bit
              set DS_Status.6 off

         case 4                       ;4 = 4.096kHz
              set DS_Status.0 on
              set DS_Status.1 off
              set DS_Status.6 on

         case 8                       ;8 = 8.192kHz
              set DS_Status.0 off
              set DS_Status.1 on
              set DS_Status.6 on

         case 32                      ;32 = 32.768kHz
              set DS_Status.0 on
              set DS_Status.1 on
              set DS_Status.6 on

         case else                    ;anything else defaults to 1 Hz
              set DS_Status.0 off
              set DS_Status.1 off
              set DS_Status.6 on

      end select
      HI2CSend(DS_Status)
      HI2CStop
  #endif

  #ifdef I2C_DATA
      'read the current value of the register
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_Control)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Status, NACK)        ;get entire byte
      I2CStop


      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_Control)                      ;location is address
      select case DS_Value
         case 0                       ;0 = disable completely
              'turn off the enable bit
              set DS_Status.6 off

         case 4                       ;4 = 4.096kHz
              set DS_Status.0 on
              set DS_Status.1 off
              set DS_Status.6 on

         case 8                       ;8 = 8.192kHz
              set DS_Status.0 off
              set DS_Status.1 on
              set DS_Status.6 on

         case 32                      ;32 = 32.768kHz
              set DS_Status.0 on
              set DS_Status.1 on
              set DS_Status.6 on

         case else                    ;anything else defaults to 1 Hz
              set DS_Status.0 off
              set DS_Status.1 off
              set DS_Status.6 on

      end select
      I2CSend(DS_Status)
      I2CStop
  #endif

end sub
;-----

sub MCP7940_SetMFP(in MFP_Value)
  'New function for MCP7940. Sets Multifunction Pin status
  'set the current value of the register

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(MCP7940_Control)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_status , NACK)      ;and get value
      HI2CStop

      if MFP_Value.0 = 1 then
         DS_status = DS_status AND 0b00001111 ;mask the lower four bits, General Purpose Output mode (SQWEN = 0, ALM0EN = 0, and ALM1EN = 0):
         DS_status = DS_status OR  0b10000000 ;set the upper bit
      else
         DS_status = DS_status AND 0b00001111 ;mask the lower four bits, General Purpose Output mode (SQWEN = 0, ALM0EN = 0, and ALM1EN = 0):
         DS_status = DS_status OR  0b00000000 ;mask the upper bit
      end if

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(MCP7940_Control)              ;go to address
      HI2CSend(DS_status)
      HI2CStop
  #endif

  #ifdef I2C_DATA
      'read the current value of the register
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_Control)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop

      if MFP_Value.0 = 1 then
         DS_status = DS_status AND 0b00001111 ;mask the lower four bits, General Purpose Output mode (SQWEN = 0, ALM0EN = 0, and ALM1EN = 0):
         DS_status = DS_status OR  0b10000000 ;set the upper bit
      else
         DS_status = DS_status AND 0b00001111 ;mask the lower four bits, General Purpose Output mode (SQWEN = 0, ALM0EN = 0, and ALM1EN = 0):
         DS_status = DS_status OR  0b00000000 ;mask the upper bit
      end if

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_Control)              ;go to address
      I2CSend(DS_status)
      I2CStop
  #endif

end sub


;-----
;The CONTROL - RTCC CONTROL REGISTER is at ADDRESS 0x07.
;Direct access to the control register permits reading and writing of the controls.
; Bit 7           6           5           4           3           2           1           0
;     OUT         SQWEN       ALM1EN      ALM0EN      EXTOSC      CRSTRIM     SQWFS1      SQWFS0
;
;Bits usage as  below:
;7 OUT: Logic Level for General Purpose Output bit
;6 SQWEN: Square Wave Output Enable bit
;5 ALM1EN: Alarm 1 Module Enable bit
;4 ALM0EN: Alarm 0 Module Enable bit
;3 EXTOSC: External Oscillator Input bit
;2 CRSTRIM: Coarse Trim Mode Enable bit
;1 SQWFS<1:0>: Square Wave Clock Output Frequency Select bits
;0 See bit 1
;See the datasheet for more information
;
sub MCP7940_SetControl(in MFP_Value)
      'New function for MCP7940. Sets Multifunction Pin status
      'set the current value of the register

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(MCP7940_Control)              ;go to address
      HI2CSend(MFP_Value)
      HI2CStop
  #endif


  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_Control)              ;go to address
      I2CSend(MFP_Value)
      I2CStop
  #endif

end sub

;------
function MCP7940_ReadControl
  'read the current value of the register

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(MCP7940_Control)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_status , NACK)      ;and get value
      HI2CStop
      MCP7940_ReadControl = DS_status
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(MCP7940_Control)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop
      MCP7940_ReadControl = DS_status
  #endif

end function



;-----

sub MCP7940_Write(in DS_Addr, in DS_Value)
  ;Write to the internal RAM. Use addresses 0x20 to 0x5F only,
  ; There is no limit to the number of bytes that can be written
  ;in a single command. However, because the RTCC
  ;registers and SRAM are separate blocks, writing past
  ;the end of each block will cause the Address Pointer to
  ;roll over to the beginning of the same block. Specifically,
  ;the Address Pointer will roll over from 0x1F to
  ;0x00, and from 0x5F to 0x20.
  ;Note that this subroutine could
  ;also be used to write to clock registers 0 though 7 if
  ;that's really what you want, for low level access.

  #ifdef HI2C_DATA
       do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
       loop While HI2CAckPollState
       HI2CSend(DS_Addr)                ;send address
       HI2CSend(DS_Value)               ;send value
       HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend(DS_Addr)                ;send address
      I2CSend(DS_Value)               ;send value
      I2CStop
  #endif

end sub

;-----

sub MCP7940_Read(in DS_Addr, out DS_Value)
  ;Read from the internal RAM. See the notes, above.

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS_Addr)                ;send the address
      HI2CReStart
      HI2CSend(DS_AddrRead)            ;then read mode
      HI2CReceive(DS_Value, NACK)      ;and get value
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend(DS_Addr)                ;send the address
      I2CStart
      I2CSend(DS_AddrRead)            ;then read mode
      I2CReceive(DS_Value, NACK)      ;and get value
      I2CStop
  #endif

end sub



'In order to configure the alarm modules, the following steps need to be performed:
'1. Load the timekeeping registers and enable the oscillator.  This is assummed to be completed elsewhere.
'2. Configure the ALMxMSK<2:0> bits to select the desired alarm mask
'3. Set or clear the ALMPOL bit according to the desired output polarity
'4. Ensure the ALMxIF flag is cleared
'5. Based on the selected alarm mask, load the alarm match value into the appropriate register(s)
'6. Enable the alarm module by setting the ALMxEN bit

;------
;MCP7940_SetAlarm ( [0 |  1] , hour, minute, second, DOW, date )
sub MCP7940_SetAlarm ( in DS_Value, in DS_Hour, in DS_Min, in DS_Sec, in DS_DOW, in DS_Date, in DS_Month )

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      DS_Temp = MCP7940_ALM0SEC + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CSend(DecToBcd(DS_Sec))                  ;send seconds
      HI2CSend(DecToBcd(DS_Min))                  ;send mins
      HI2CSend(DecToBcd(DS_Hour))               ;send hours

      HI2CReStart                                   ;set DOW by extracting the bits
      HI2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                          ;send address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)                   ;get the weekday value

      DS_Temp = DS_Temp & 248             ;get bit values from bits 7 thru 3
      DS_DOW = DecToBcd(DS_DOW) or DS_Temp        ;set bit values of bits 2 and 0

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend(DS_Temp)
      HI2CSend(DS_DOW)

      HI2CSend(DecToBcd(DS_Date))        ;send the date
      HI2CSend(DecToBcd(DS_Month))         ;send the month

      HI2CStop
  #endif


  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0SEC + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CSend(DecToBcd(DS_Sec))                  ;send seconds
      I2CSend(DecToBcd(DS_Min))                  ;send mins
      I2CSend(DecToBcd(DS_Hour))               ;send hours

      I2CStart                                   ;set DOW by extracting the bits
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend( DS_Temp )                          ;send address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)                   ;get the weekday value

      DS_Temp = DS_Temp & 248             ;get bit values from bits 7 thru 3
      DS_DOW = DecToBcd(DS_DOW) or DS_Temp        ;set bit values of bits 2 and 0

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend(DS_Temp)
      I2CSend(DS_DOW)

      I2CSend(DecToBcd(DS_Date))         ;send the date
      I2CSend(DecToBcd(DS_Month))        ;send the month

      I2CStop
  #endif

end sub

;------

;The alarm can be set to go off if any of the following conditions are met
;MCP7940_AlarmAssertion_Seconds            0x00                 ; a match of the seconds
;MCP7940_AlarmAssertion_Minutes            0x01                 ; a match of the minutes
;MCP7940_AlarmAssertion_Hours              0x02                 ; a match of the hours
;MCP7940_AlarmAssertion_DayofWeek          0x03                 ; a match of the day of the week
;MCP7940_AlarmAssertion_Date               0x04                 ; a match of the date
;MCP7940_AlarmAssertion_All                0x07                 ; a match of all paraemters equals all seconds, minutes, hours, day of week, day and month match.
;MCP7940_SetAlarmMask ( [0 |  1] , alarmAssertionMatch )
sub MCP7940_SetAlarmMask ( in DS_Value, in DS_Status )

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                              ;send calculated address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the weekday value
      DS_NewValue = DS_NewValue and 0b10001111        ;get all bits except bit 6-4 ALMxMSK<2:0>
      DS_NewValue = DS_NewValue or ( DS_Status *  16 ) ;set bit values from bits 6 thru 4
      HI2CStop

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend(DS_Temp)                            ;send calculated address
      HI2CSend(DS_NewValue)
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend( DS_Temp )                              ;send calculated address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the weekday value
      DS_NewValue = DS_NewValue and 0b10001111        ;get all bits except bit 6-4 ALMxMSK<2:0>
      DS_NewValue = DS_NewValue or ( DS_Status *  16 ) ;set bit values from bits 6 thru 4
      I2CStop

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend(DS_Temp)                            ;send calculated address
      I2CSend(DS_NewValue)
      I2CStop
  #endif

end sub
;------
;MCP7940_ReadAlarm ( [0 |  1] , hour, minute, second, DOW, date, month ). Return values
sub MCP7940_ReadAlarm ( in DS_Value, out DS_Hour, out DS_Min, out DS_Sec, out DS_DOW, out DS_Date, out DS_Month )

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      DS_Temp = MCP7940_ALM0SEC + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Sec, ACK)              ;get the seconds
      DS_Sec = BcdToDec(DS_Sec & 127) ;strip off CH bit
      HI2CReceive(DS_Min, ACK)              ;get the minutes
      DS_Min = BcdToDec(DS_Min)
      HI2CReceive(DS_Hour, ACK)       ;get the hours
      if DS_Hour.6 then               ;12-hour mode
        DS_A_P = DS_Hour.5            ;a.m. or p.m.
        DS_Hour = BcdToDec(DS_Hour & 31)
      else
        DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if
      HI2CReceive(DS_DOW, ACK)        ;get the DOW
      DS_DOW = BcdToDec(DS_DOW & 7)  ;strip off bits

      HI2CReceive(DS_Date, ACK)        ;get the Date
      DS_Date = BcdToDec(DS_Date & 63)  ;strip off bits

      HI2CReceive(DS_Month, NACK)        ;get the Month
      DS_Month = BcdToDec(DS_Month & 31)  ;strip off bits

      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0SEC + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Sec, ACK)              ;get the seconds
      DS_Sec = BcdToDec(DS_Sec & 127) ;strip off CH bit
      I2CReceive(DS_Min, ACK)              ;get the minutes
      DS_Min = BcdToDec(DS_Min)
      I2CReceive(DS_Hour, ACK)       ;get the hours
      if DS_Hour.6 then               ;12-hour mode
        DS_A_P = DS_Hour.5            ;a.m. or p.m.
        DS_Hour = BcdToDec(DS_Hour & 31)
      else
        DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if
      I2CReceive(DS_DOW, ACK)        ;get the DOW
      DS_DOW = BcdToDec(DS_DOW & 7)  ;strip off bits

      I2CReceive(DS_Date, ACK)        ;get the Date
      DS_Date = BcdToDec(DS_Date & 63)  ;strip off bits

      I2CReceive(DS_Month, NACK)        ;get the Month
      DS_Month = BcdToDec(DS_Month & 31)  ;strip off bits

      I2CStop
  #endif

end sub
;------
;MCP7940_StartAlarm ( [0 |  1] )
sub MCP7940_ClearAlarm ( in DS_Value)

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_DOW, NACK)
      HI2CStop

      DS_DOW.3 = 0                            ;clear bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CSend(DS_DOW)               ;send value
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_DOW, NACK)
      I2CStop

      DS_DOW.3 = 0                            ;clear bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CSend(DS_DOW)               ;send value
      I2CStop
  #endif

end sub
;------
;MCP7940_AlarmStatus ( [0 |  1] )
function MCP7940_AlarmStatus ( in DS_Value)

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_DOW, NACK)
      HI2CStop

      MCP7940_AlarmStatus = DS_DOW.3          ;extract bit value
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_DOW, NACK)
      I2CStop

      MCP7940_AlarmStatus = DS_DOW.3          ;extract bit value
  #endif

end function


;MCP7940_SetAlarmPolarity ( [0 |  1], On | Off )
sub MCP7940_SetAlarmPolarity ( in DS_Value, in DS_Status)

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_DOW, NACK)
      HI2CStop

      DS_DOW.7 =  DS_Status                           ;set bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CSend(DS_DOW)               ;send value
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_DOW, NACK)
      I2CStop

      DS_DOW.7 =  DS_Status                           ;set bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      DS_Temp = MCP7940_ALM0WKDAY + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CSend(DS_DOW)               ;send value
      I2CStop
  #endif

end sub



;MCP7940_EnableAlarm ( [0 |  1] )
sub MCP7940_EnableAlarm ( in DS_Value )

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( MCP7940_Control )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_status, NACK)
      HI2CStop

      if DS_Value = 0 then
         DS_status.4 =  1                           ;set bit
      else
         DS_status.5 =  1                           ;set bit
      end if

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( MCP7940_Control )
      HI2CSend(DS_status)                 ;send value
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( MCP7940_Control )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_status, NACK)
      I2CStop

      if DS_Value = 0 then
         DS_status.4 =  1                           ;set bit
      else
         DS_status.5 =  1                           ;set bit
      end if

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( MCP7940_Control )
      I2CSend(DS_status)                 ;send value
      I2CStop
  #endif

end sub


;MCP7940_DisableAlarm ( [0 |  1] )
sub MCP7940_DisableAlarm ( in DS_Value )

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( MCP7940_Control )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_status, NACK)
      HI2CStop

      if DS_Value = 0 then
         DS_status.4 =  0                           ;set bit
      else
         DS_status.5 =  0                           ;set bit
      end if

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( MCP7940_Control )
      HI2CSend(DS_status)                 ;send value
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( MCP7940_Control )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_status, NACK)
      I2CStop

      if DS_Value = 0 then
         DS_status.4 =  0                           ;set bit
      else
         DS_status.5 =  0                           ;set bit
      end if

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( MCP7940_Control )
      I2CSend(DS_status)                 ;send value
      I2CStop
  #endif

end sub

;-----
sub MCP7940_ClearPowerFail
  ; Clearing this bit resets the power-fail time-stamp registers to 0.

  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( MCP7940_RTCWKDAY )                     ;indicate register
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_State, NACK)                      ;get the current seconds
      set DS_State.4 off                              ;CH bit = 0 disables
      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( MCP7940_RTCWKDAY )                     ;indicate register
      HI2CSend(DS_State)                               ;now send updated value
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( MCP7940_RTCWKDAY )                     ;indicate register
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_State, NACK)                      ;get the current seconds
      set DS_State.4 off                              ;CH bit = 0 disables
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( MCP7940_RTCWKDAY )                     ;indicate register
      I2CSend(DS_State)                               ;now send updated value
      I2CStop
  #endif

end sub
;-----
function MCP7940_PowerFailStatus
   ; Reading this bit shows the power-fail status

  #ifdef HI2C_DATA
       do
            HI2CReStart                          ;generate a start signal
            HI2CSend(DS_AddrWrite)                     ;inidcate a write
       loop While HI2CAckPollState
       HI2CSend( MCP7940_RTCWKDAY )                     ;indicate register
       HI2CReStart
       HI2CSend(DS_AddrRead)
       HI2CReceive(DS_State, NACK)                      ;get the current seconds
       HI2CStop
       if DS_State.4 = 0 then                              ;CH bit = 0 disables
          MCP7940_PowerFailStatus = false
       else
          MCP7940_PowerFailStatus = true
       end if
  #endif

  #ifdef I2C_DATA
       I2CStart
       I2CSend(DS_AddrWrite)
       I2CSend( MCP7940_RTCWKDAY )                     ;indicate register
       I2CStart
       I2CSend(DS_AddrRead)
       I2CReceive(DS_State, NACK)                      ;get the current seconds
       I2CStop
       if DS_State.4 = 0 then                              ;CH bit = 0 disables
          MCP7940_PowerFailStatus = false
       else
          MCP7940_PowerFailStatus = true
       end if
  #endif

end function
;-----

;--------
;MCP7940_ReadFailureClock ( [0 |  1] , hour, minute, DoW, date, month ). Return values.
;
;The MCP7940N includes a power-fail time-stamp module that stores the minutes, hours, date, and month when primary power is lost and when it is restored
;
sub MCP7940_ReadFailureClock(in DS_Value, out DS_Hour, out DS_Min, out DS_DOW, out DS_Date, out DS_Month)

;Read entire clock: hours, minutes, seconds, a.m. or p.m., day of week, date, month, year
;DS_A_P = 0 means a.m. DS_A_P = 1 means p.m.


  #ifdef HI2C_DATA
      do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      DS_Temp = MCP7940_PWR00MIN + ( 7 * DS_Value )
      HI2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      HI2CReStart
      HI2CSend(DS_AddrRead)

      HI2CReceive(DS_Min, ACK)              ;get minutes
      DS_Min = BcdToDec(DS_Min)       ;bit 7 is always 0

      HI2CReceive(DS_Hour, ACK)             ;get hours
      if DS_Hour.6 then               ;12-hour mode
        DS_A_P = DS_Hour.5            ;a.m. or p.m.
        DS_Hour = BcdToDec(DS_Hour & 31)
      else
        DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if

      HI2CReceive(DS_Date, ACK)             ;get date
      DS_Date = BcdToDec(DS_Date)

      HI2CReceive(DS_Temp, NACK)            ;get month and DOW
      DS_Month = BcdToDec(DS_Temp & 31)

      DS_DOW = DS_Temp & 224              ;get day of week
      DS_DOW = DS_DOW / 32
      DS_DOW = BcdToDec(DS_DOW & 7 )
      HI2CStop
  #endif

  #ifdef I2C_DATA
      I2CStart
      I2CSend(DS_AddrWrite)
      DS_Temp = MCP7940_PWR00MIN + ( 7 * DS_Value )
      I2CSend( DS_Temp )                         ;begin with address for alarm0 and then add the 7 for alarm1
      I2CStart
      I2CSend(DS_AddrRead)

      I2CReceive(DS_Min, ACK)              ;get minutes
      DS_Min = BcdToDec(DS_Min)       ;bit 7 is always 0

      I2CReceive(DS_Hour, ACK)             ;get hours
      if DS_Hour.6 then               ;12-hour mode
        DS_A_P = DS_Hour.5            ;a.m. or p.m.
        DS_Hour = BcdToDec(DS_Hour & 31)
      else
        DS_Hour = BcdToDec(DS_Hour)   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if

      I2CReceive(DS_Date, ACK)             ;get date
      DS_Date = BcdToDec(DS_Date)

      I2CReceive(DS_Temp, NACK)            ;get month and DOW
      DS_Month = BcdToDec(DS_Temp & 31)

      DS_DOW = DS_Temp & 224              ;get day of week
      DS_DOW = DS_DOW / 32
      DS_DOW = BcdToDec(DS_DOW & 7 )
      I2CStop
  #endif

end sub
