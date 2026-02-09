;    Library for reading/writing to Microchip DS1337 RTC for the GCBASIC compiler
;    Copyright (C) 2013-2026 Thomas Henry and Evan Venn
;
;    Version 1.1a  27/1/2015
;    Version 1.2a  17/3/2015
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
;    adapted further and further for DS1337 to add new functionality to comply with DEVICE datasheet by Evan R Venn - Jan 27 2015
;    revised to support AVR bit handling
'    Evan R Venn/Anobium  ---  24/4/2017 - Revised to remove NOT error


;13 bytes are used as input and output parameters. They are:
;DS_Value, DS_Addr, DS_Hour, DS_Min, DS_Sec, DS_A_P, DS_Date
;DS_Month, DS_Year, DS_Century and DS_DOW.
;DS_Status, DS_temp

;Quick Command Reference:

;DS1337_EnableOscillator(flag [true|false])
;DS1337_OscillatorStopFlagStatus
;DS1337_ClearOscillatorStopFlag
;DS1337_ResetClock
;DS1337_SetClock(hour, minute, second, DOW, date, month, year)
;DS1337_SetTime(hour, minute, second)
;DS1337_SetDate(DOW, date, month, year)
;DS1337_ReadClock(hour, minute, second, a.m.|p.m., DOW, date, month, year)
;DS1337_ReadTime(hour, minute, second, a.m.|p.m.)
;DS1337_ReadDate(DOW, date, month, year)
;DS1337_SetHourMode(12|24)
;DS1337_ReadHourMode(12|24)
;DS1337_SetSQW(rate)
;DS1337_EnableSQW
;DS1337_DisableSQW

;DS1337_SetControl(value)
;DS1337_ReadControl. A function return currrent value
;DS1337_SetControlStatus(value)
;DS1337_ReadControlStatus. A function return currrent value

;DS1337_SetAlarm1 ( hour, minute, second, DOW, date )
;DS1337_SetAlarmMask1 ( alarmAssertionMatch )
;DS1337_ReadAlarm1 ( hour, minute, second, DOW, date ). Return values
;DS1337_ClearAlarm1
;DS1337_EnableAlarm1Interrupt
;DS1337_DisableAlarm1Interrupt
;DS1337_Alarm1Status
;DS1337_DisableAlarm1

;DS1337_SetAlarm2 ( hour, minute, DOW, date )
;DS1337_SetAlarmMask2 ( alarmAssertionMatch )
;DS1337_ReadAlarm2 ( hour, minute, DOW, date ). Return values
;DS1337_ClearAlarm2
;DS1337_EnableAlarm2Interrupt
;DS1337_DisableAlarm2Interrupt
;DS1337_Alarm2Status
;DS1337_DisableAlarm2

;DS1337_ReadRegister. A function
;DS1337_WriteRegister


;See the subroutine definitions below for full details on each.

;----- Constants

 ' These are the addresses of the generic DS1337 Real-Time Clock
 #define DS_AddrWrite 0xD0               'DS1337 uses 0xD0
 #define DS_AddrRead  0xD1               'DS1337 uses 0xD1

 'Addresses for DS1337
 #define DS1337_RTCSEC    0x00    'Start of the time registers - 0x00h
 #define DS1337_RTCMIN    0x01                    'TIMEKEEPING MINUTES VALUE REGISTER - 0x01h
 #define DS1337_RTCHOUR   0x02                    'TIMEKEEPING HOURS VALUE REGISTER - 0x02h
 #define DS1337_RTCWKDAY  0x03    'TIMEKEEPING WEEKDAY VALUE REGISTER - 0x03h
 #define DS1337_RTCDATE   0x04    'TIMEKEEPING DATE VALUE REGISTER  - 0x04h
 #define DS1337_RTCMTH    0x05                    'TIMEKEEPING MONTH and CENTURY VALUE REGISTER - 0x05h
 #define DS1337_RTCYEAR   0x06                    'TIMEKEEPING YEAR VALUE REGISTER - 0x06h

 #define DS1337_ALM1SEC   0x07            ' ALARM 1 SECONDS VALUE REGISTER
 #define DS1337_ALM1MIN   0x08            ' ALARM 1 MINUTES VALUE REGISTER
 #define DS1337_ALM1HOUR  0x09            ' ALARM 1 HOURS VALUE REGISTER
 #define DS1337_ALM1DAY_DATE  0x0A            ' ALARM 1 DAY/DATE VALUE REGISTER

 #define DS1337_ALM2MIN   0x0B            ' ALARM 2 MINUTES VALUE REGISTER
 #define DS1337_ALM2HOUR  0x0C            ' ALARM 2 HOURS VALUE REGISTER
 #define DS1337_ALM2DAY_DATE  0x0D            ' ALARM 2 DAY/DATE VALUE REGISTER

 #define DS1337_ControlRegister   0x0E    'Control register
 #define DS1337_ControlStatus   0x0F    'Control Status register

 #define DS1337_AgingOffset  0x10           ' Aging VALUE REGISTER
 #define DS1337_MSB_Temp     0x11           ' MSB Temp VALUE REGISTER
 #define DS1337_LSB_Temp     0x12           ' LSB Temp VALUE REGISTER

 #define DS1337_Alarm1Assertion_EverySecond                     0x0F
 #define DS1337_Alarm1Assertion_Seconds                         0x0E
 #define DS1337_Alarm1Assertion_MinutesSeconds                  0x0C
 #define DS1337_Alarm1Assertion_HoursMinutesSeconds             0x08
 #define DS1337_Alarm1Assertion_DateHoursMinutesSeconds         0x00               ;Set by passing correct data
 #define DS1337_Alarm1Assertion_DayHoursMinutesSeconds          0x00               ;Set by passing correct data

 #define DS1337_Alarm2Assertion_EveryMinute                     0x07
 #define DS1337_Alarm2Assertion_Minutes                         0x06
 #define DS1337_Alarm2Assertion_HoursMinutes                    0x04
 #define DS1337_Alarm2Assertion_DateHoursMinutes                0x00               ;Set by passing correct data
 #define DS1337_Alarm2Assertion_DayHoursMinutes                 0x00               ;Set by passing correct data






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
#define DS1337_Enable DS1337_EnableOscillator
;
;When set to logic 0, the oscillator is started. When set to logic 1, the oscillator is stopped when the DS1337 switches to VBAT.
;This bit is clear (logic 0) when power is first applied. When the DS1337 is powered by VCC, the oscillator is always
;on regardless of the status of the EOSC bit. When EOSC is disabled, all register data is static.
;
sub DS1337_EnableOscillator(in DS_Value)
  ;Enables clock if DS_Value = FALSE, disables if DS_Value = TRUE

  #ifdef HI2C_DATA

     do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
     loop While HI2CAckPollState
     HI2CSend( DS1337_ControlRegister )       ;indicate register
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
     HI2CSend( DS1337_ControlRegister )                     ;indicate register
     HI2CSend(DS_Temp)                ;now send updated value
     HI2CStop

  #endif

  #ifdef I2C_DATA

     I2CStart
     I2CSend(DS_AddrWrite)
     I2CSend( DS1337_ControlRegister )                     ;indicate register
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
     I2CSend( DS1337_ControlRegister )                     ;indicate register
     I2CSend(DS_Temp)                ;now send updated value
     I2CStop

  #endif

end sub

;-----

sub DS1337_ResetClock
  ;Reset clock to 00:00:00 01 01/01/00.
  ;Also sets 24-hour mode and enables the clock.

  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(0)                      ;begin with address 0
      HI2CSend(0)                      ;then set the seven
      HI2CSend(0)                      ;consecutive locations
      HI2CSend(0)
      HI2CSend(0)
      HI2CSend(0)
      HI2CSend(0)
      HI2CSend(0)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(0)                      ;begin with address 0
      I2CSend(0)                      ;then set the seven
      I2CSend(0)                      ;consecutive locations
      I2CSend(0)
      I2CSend(0)
      I2CSend(0)
      I2CSend(0)
      I2CSend(0)
      I2CStop

  #endif

end sub

;-----

sub DS1337_SetClock(in DS_Hour, in DS_Min, in DS_Sec, in DS_DOW, in DS_Date, in DS_Month, in DS_Year)
  ;Set entire clock: hours, minutes, seconds, day of week, date, month, year
  ;Also sets 24-hour mode and enables the clock.

  'Get current state of the Oscillator

  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)                     ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCSEC)               ;begin with address 0
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Value, NACK)            ;get the seconds to obtain ST bit value
      DS_Value = BcdToDec(DS_Value & 128)   ;get ST value from bit 7

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_RTCSEC)               ;begin with address 0
      DS_Sec = DS_Sec or DS_Value
      HI2CSend(DecToBcd(DS_Sec))             ;then set the next two
      HI2CSend(DecToBcd(DS_Min))             ;consecutive values

      HI2CReStart                               ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_RTCHour)               ;begin with address 0
      HI2CSend( DecToBcd(DS_Hour) )

      HI2CReStart                                     ;set DOW by extracting the bits
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_RTCWKDAY)
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Value, NACK)                   ;get the seconds to obtain bit 7 an 6 values
      DS_Value = DS_Value & 56                     ;get bit values from bit 5,4 and 3
      DS_DOW = DecToBcd(DS_DOW) or DS_Value        ;set bit values

      HI2CReStart                                     ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_RTCWKDAY)
      HI2CSend(DS_DOW)

      HI2CSend(DecToBcd(DS_Date))
      HI2CSend(DecToBcd(DS_Month))
      HI2CSend(DecToBcd(DS_Year))
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCSEC)               ;begin with address 0
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Value, NACK)            ;get the seconds to obtain ST bit value
      DS_Value = BcdToDec(DS_Value & 128)   ;get ST value from bit 7

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCSEC)               ;begin with address 0
      DS_Sec = DS_Sec or DS_Value
      I2CSend(DecToBcd(DS_Sec))             ;then set the next two
      I2CSend(DecToBcd(DS_Min))             ;consecutive values

      I2CStart                               ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCHour)               ;begin with address 0
      I2CSend( DecToBcd(DS_Hour) )

      I2CStart                                     ;set DOW by extracting the bits
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCWKDAY)
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Value, NACK)                   ;get the seconds to obtain bit 7 an 6 values
      DS_Value = DS_Value & 56                     ;get bit values from bit 5,4 and 3
      DS_DOW = DecToBcd(DS_DOW) or DS_Value        ;set bit values

      I2CStart                                     ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCWKDAY)
      I2CSend(DS_DOW)

      I2CSend(DecToBcd(DS_Date))
      I2CSend(DecToBcd(DS_Month))
      I2CSend(DecToBcd(DS_Year))
      I2CStop

   #endif
end sub

;-----

sub DS1337_SetTime(in DS_Hour, in DS_Min, in DS_Sec)
  ;Set time only: hours, minutes, seconds.
  ;Also sets 24-hour mode and enables the clock.
  '
  'Get current state of the Oscillator

  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCSEC)                  ;begin with address 0
      HI2CSend(DecToBcd(DS_Sec))                  ;then send the next two
      HI2CSend(DecToBcd(DS_Min))                  ;consecutive values
      HI2CSend( DecToBcd(DS_Hour) )
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCSEC)                  ;begin with address 0
      I2CSend(DecToBcd(DS_Sec))                  ;then send the next two
      I2CSend(DecToBcd(DS_Min))                  ;consecutive values
      I2CSend( DecToBcd(DS_Hour) )
      I2CStop

  #endif
end sub

;-----

sub DS1337_SetDate(in DS_DOW, DS_Date, in DS_Month, in DS_Year)
  ;Set date only: day of week, date, month, year
  ;There is no error detection for out-of-range dates.


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCWKDAY)
      HI2CSend(DecToBcd(DS_DOW))
      HI2CSend(DecToBcd(DS_Date))
      HI2CSend(DecToBcd(DS_Month))
      HI2CSend(DecToBcd(DS_Year))
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart                               ;set DOW by extracting the bits
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCWKDAY)
      I2CSend(DecToBcd(DS_DOW))
      I2CSend(DecToBcd(DS_Date))
      I2CSend(DecToBcd(DS_Month))
      I2CSend(DecToBcd(DS_Year))
      I2CStop

  #endif

 end sub

;-----

sub DS1337_ReadClock(out DS_Hour, out DS_Min, out DS_Sec, out DS_A_P, out DS_DOW, out DS_Date, out DS_Month, out DS_Year)
;Read entire clock: hours, minutes, seconds, a.m. or p.m., day of week, date, month, year
;DS_A_P = 0 means a.m. DS_A_P = 1 means p.m.


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCSEC)                      ;begin with address 0
      HI2CStop
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Sec, ACK)              ;get seconds
      DS_Sec = BcdToDec(DS_Sec)

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
      I2CSend(DS1337_RTCSEC)                      ;begin with address 0
      I2CStop
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Sec, ACK)              ;get seconds
      DS_Sec = BcdToDec(DS_Sec)

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

sub DS1337_ReadTime(out DS_Hour, out DS_Min, out DS_Sec, out DS_A_P)
  ;Read time only: hours, minutes, seconds, a.m. or p.m.


  #ifdef HI2C_DATA

      do
        HI2CReStart                              ;generate a start signal
        HI2CSend(DS_AddrWrite)                   ;indicate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCSEC)
      HI2CReStart
      HI2CSend(DS_AddrRead)                       ;set the read flag
      HI2CReceive(DS_Sec, ACK)              ;get the seconds
      DS_Sec = BcdToDec(DS_Sec )

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
    I2CSend(DS1337_RTCSEC)                      ;begin with address 0
    I2CStart
    I2CSend(DS_AddrRead)
    I2CReceive(DS_Sec, ACK)              ;get the seconds
    DS_Sec = BcdToDec(DS_Sec )

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

sub DS1337_ReadDate(out DS_DOW, out DS_Date, out DS_Month, out DS_Year, out DS_Century)
;Get date only: day of week, date, month, year


  #ifdef HI2C_DATA

      do
        HI2CReStart                              ;generate a start signal
        HI2CSend(DS_AddrWrite)                   ;indicate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCWKDAY)
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_DOW, ACK)              ;get day of week

      DS_DOW = DS_DOW & 7             'get the bits we are interested in
      DS_Date = BcdToDec(DS_DOW)
      HI2CReceive(DS_Date, ACK)             ;get date
      DS_Date = BcdToDec(DS_Date)

      HI2CReceive(DS_Month, ACK)            ;get month register
      DS_Century = DS_Month & 128          ;get century bit
      DS_Century = DS_Century / 128        ;set century to 0 or 1.
      DS_Month = BcdToDec(DS_Month & 31 )

      HI2CReceive(DS_Year, NACK)       ;get year
      DS_Year = BcdToDec(DS_Year)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCWKDAY)
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_DOW, ACK)              ;get day of week

      DS_DOW = DS_DOW & 7             'get the bits we are interested in
      DS_Date = BcdToDec(DS_DOW)
      I2CReceive(DS_Date, ACK)             ;get date
      DS_Date = BcdToDec(DS_Date)

      I2CReceive(DS_Month, ACK)            ;get month register
      DS_Century = DS_Month & 128          ;get century bit
      DS_Century = DS_Century / 128        ;set century to 0 or 1.
      DS_Month = BcdToDec(DS_Month & 31 )

      I2CReceive(DS_Year, NACK)       ;get year
      DS_Year = BcdToDec(DS_Year)
      I2CStop

  #endif
end sub

;-----

sub DS1337_SetHourMode(in DS_A_P)
  ;Set hour mode. 12 = 12-hour, all else = 24-hour


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCHOUR)        ;address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Hour, NACK)       ;get current hours and mode
      if DS_A_P = 12 then             ;change to 12-hour mode
        if DS_Hour.6 = 0 then            ;only if in 24-hour mode
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
      HI2CSend(DS1337_RTCHOUR)
      HI2CSend(DS_Hour)                ;send hours and flag back again
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_RTCHOUR)        ;address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Hour, NACK)       ;get current hours and mode
      if DS_A_P = 12 then             ;change to 12-hour mode
        if DS_Hour.6 = 0 then            ;only if in 24-hour mode
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
      I2CSend(DS1337_RTCHOUR)
      I2CSend(DS_Hour)                ;send hours and flag back again
      I2CStop

  #endif

end sub

;-----

sub DS1337_ReadHourMode(out DS_A_P)
  ;Return the current hour mode, 12 or 24


  #ifdef HI2C_DATA
      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_RTCHOUR)                      ;go to address 2
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
      I2CSend(DS1337_RTCHOUR)                      ;go to address 2
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
;Bits 4 and 3: Rate Select (RS2 and RS1).
;These bits control the frequency of the square-wave output when
;the square wave has been enabled. The following table shows the square-wave frequencies that can be selected
;with the RS bits. These bits are both set to logic 1 (8.192kHz) when power is first applied.
;SQUARE-WAVE OUTPUT
;            RS2   RS1     FREQUENCY        DS_Value
;            0     0       1Hz              0
;            0     1       4.096kHz         1
;            1     0       8.192kHz         2
;            1     1       32.768kHz        3
sub DS1337_SetSQW(in DS_Value)
  ;Set square wave output pin mode
  'read the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Status, NACK)        ;get entire byte
      HI2CStop


      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlRegister)                      ;location is address
      select case DS_Value
         case 0                       ;0 = 1hz
              set DS_Status.3 off
              set DS_Status.4 off

         case 1                       ;1 = 4.096kHz
              set DS_Status.3 on
              set DS_Status.4 off

         case 2                       ;2 = 8.192kHz
              set DS_Status.3 off
              set DS_Status.4 on

         case 3                      ;3 = 32.768kHz
              set DS_Status.3 on
              set DS_Status.4 on

         case else                    ;anything else defaults to 32.768kHz
              set DS_Status.3 on
              set DS_Status.4 on

      end select
      HI2CSend(DS_Status)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Status, NACK)        ;get entire byte
      I2CStop


      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)                      ;location is address
      select case DS_Value
         case 0                       ;0 = 1hz
              set DS_Status.3 off
              set DS_Status.4 off

         case 1                       ;1 = 1.024kHz
              set DS_Status.3 on
              set DS_Status.4 off

         case 2                       ;2 = 4.096kHz
              set DS_Status.3 off
              set DS_Status.4 on

         case 3                      ;3 = 32.768kHz
              set DS_Status.3 on
              set DS_Status.4 on

         case else                    ;anything else defaults to 32.768kHz
              set DS_Status.3 on
              set DS_Status.4 on

      end select
      I2CSend(DS_Status)
      I2CStop

  #endif

end sub

;-------
sub DS1337_EnableSQW
  ;Set square wave active
  'read the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Status, NACK)        ;get entire byte
      HI2CStop

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlRegister)                      ;location is address
      DS_Status.2 = 0                              ;disable interrupt. When the INTCN bit is set to logic 0, a square wave is output on the INT/SQW pin

      HI2CSend(DS_Status)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Status, NACK)        ;get entire byte
      I2CStop

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)                      ;location is address
      DS_Status.2 = 0                              ;disable interrupt. When the INTCN bit is set to logic 0, a square wave is output on the INT/SQW pin

      I2CSend(DS_Status)
      I2CStop

  #endif

end sub

;------
;-------
sub DS1337_DisableSQW
  ;Set square wave active
  'read the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Status, NACK)        ;get entire byte
      HI2CStop

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlRegister)                      ;location is address
      DS_Status.2 = 1                                       ; intcon = 1 enables intb
      HI2CSend(DS_Status)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Status, NACK)        ;get entire byte
      I2CStop

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)                      ;location is address
      DS_Status.2 = 1
      I2CSend(DS_Status)
      I2CStop

  #endif

end sub

;
sub DS1337_SetControl(in MFP_Value)
  'New function for DS1337. Sets Multifunction Pin status
  'set the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CSend(MFP_Value)
      HI2CStop


  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CSend(MFP_Value)
      I2CStop

  #endif

end sub

;------
function DS1337_ReadControl
  'read the current value of the register


  #ifdef HI2C_DATA

      HI2CReStart                          ;generate a start signal
      HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_status , NACK)      ;and get value
      HI2CStop
      DS1337_ReadControl = DS_status

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop
      DS1337_ReadControl = DS_status

  #endif

end function


sub DS1337_SetControlStatus(in MFP_Value)
  'New function for DS1337. Sets Multifunction Pin status
  'set the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_ControlStatus)              ;go to address
      HI2CSend(MFP_Value)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlStatus)              ;go to address
      I2CSend(MFP_Value)
      I2CStop

  #endif

end sub

;------
function DS1337_ReadControlStatus
  'read the current value of the register


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend(DS1337_ControlStatus)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_status , NACK)      ;and get value
      HI2CStop
      DS1337_ReadControlStatus = DS_status

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlStatus)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop
      DS1337_ReadControlStatus = DS_status

  #endif

end function


;------
;DS1337_SetAlarm1 ( hour, minute, second, DOW, date ) wher DOW or DATE can be 0
;The DY/DT bits (bit 6 of the alarm day/date registers)
;control whether the alarm value stored in bits 0 to 5 of
;that register reflects the day of the week or the date of
;the month. If DY/DT is written to logic 0, the alarm will
;be the result of a match with date of the month. If
;DY/DT is written to logic 1, the alarm will be the result of
;a match with day of the week.
sub DS1337_SetAlarm1 ( in DS_Hour, in DS_Min, in DS_Sec, in DS_DOW, in DS_Date )


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM1SEC )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Sec)

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1SEC )
      HI2CSend(DS_Value)
      HI2CStop

      HI2CReStart                            ;get and set minutes
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1MIN )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Min)

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1MIN )
      HI2CSend(DS_Value)
      HI2CStop

      HI2CReStart                            ;get and set hours
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1HOUR )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Hour)

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1HOUR )
      HI2CSend(DS_Value)
      HI2CStop

      HI2CReStart                            ;get and set date
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1DAY_DATE )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 192 )

      if DS_Date <>  0 then    ;set alarm to the DATE, see notes above
         DS_Value = DS_Temp OR DecToBcd(DS_Date)
         DS_Value.6 = 0
      else
         DS_Value = DS_Temp OR DecToBcd(DS_DOW)
         DS_Value.6 = 1
      end if
      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1DAY_DATE )
      HI2CSend(DS_Value)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart                            ;get and set seconds
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1SEC )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Sec)

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1SEC )
      I2CSend(DS_Value)
      I2CStop

      I2CStart                            ;get and set minutes
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1MIN )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Min)

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1MIN )
      I2CSend(DS_Value)
      I2CStop

      I2CStart                            ;get and set hours
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1HOUR )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Hour)

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1HOUR )
      I2CSend(DS_Value)
      I2CStop

      I2CStart                            ;get and set date
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1DAY_DATE )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 192 )

      if DS_Date <>  0 then    ;set alarm to the DATE, see notes above
         DS_Value = DS_Temp OR DecToBcd(DS_Date)
         DS_Value.6 = 0
      else
         DS_Value = DS_Temp OR DecToBcd(DS_DOW)
         DS_Value.6 = 1
      end if
      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1DAY_DATE )
      I2CSend(DS_Value)
      I2CStop

  #endif

end sub

;------
; DS1337_Alarm1Assertion_EverySecond                     0x0F
; DS1337_Alarm1Assertion_Seconds                         0x0E
; DS1337_Alarm1Assertion_MinutesSeconds                  0x0C
; DS1337_Alarm1Assertion_HoursMinutesSeconds             0x08
; DS1337_Alarm1Assertion_DateHoursMinutesSeconds         0x00
; DS1337_Alarm1Assertion_DayHoursMinutesSeconds          0x00
;DS1337_SetAlarmMask1 (   alarmAssertionMatch )
sub DS1337_SetAlarmMask1 ( in DS_Status )
  ;set the four bits for alarm 1. Bit by bit


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM1SEC )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the current value
      HI2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.0
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ALM1SEC)
      HI2CSend(DS_NewValue)
      HI2CStop                                     ;First bit


      HI2CReStart                                    ;Second bit
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1MIN )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the current value
      HI2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.1
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ALM1MIN)
      HI2CSend(DS_NewValue)
      HI2CStop                                     ;Second bit

      HI2CReStart                                    ;Third bit
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1HOUR )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the current value
      HI2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.2
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ALM1HOUR)
      HI2CSend(DS_NewValue)
      HI2CStop                                     ;Third bit

      HI2CReStart                                    ;Forth bit
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM1DAY_DATE )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the current value
      HI2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.3
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ALM1DAY_DATE)
      HI2CSend(DS_NewValue)
      HI2CStop                                     ;Forth bit

  #endif

  #ifdef I2C_DATA
      I2CStart                                    ;First bit
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1SEC )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the current value
      I2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.0
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ALM1SEC)
      I2CSend(DS_NewValue)
      I2CStop                                     ;First bit


      I2CStart                                    ;Second bit
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1MIN )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the current value
      I2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.1
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ALM1MIN)
      I2CSend(DS_NewValue)
      I2CStop                                     ;Second bit

      I2CStart                                    ;Third bit
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1HOUR )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the current value
      I2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.2
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ALM1HOUR)
      I2CSend(DS_NewValue)
      I2CStop                                     ;Third bit

      I2CStart                                    ;Forth bit
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1DAY_DATE )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the current value
      I2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.3
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ALM1DAY_DATE)
      I2CSend(DS_NewValue)
      I2CStop                                     ;Forth bit

  #endif

end sub
;------
;DS1337_ReadAlarm1 ( hour, minute, second, DOW, date ). Return values
sub DS1337_ReadAlarm1 ( out DS_Hour, out DS_Min, out DS_Sec, out DS_DOW, out DS_Date )


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM1SEC )                         ;begin with address for alarm1
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
        DS_Hour = BcdToDec(DS_Hour  and 127 )   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if

      HI2CReceive(DS_Temp, NACK)       ;get the day OR Date
      if DS_Temp.6 then               ;Day mode
        DS_DOW = BcdToDec(DS_Temp & 31)
        DS_Date = 0
      else                            ;Date mode
        DS_Date = BcdToDec(DS_Temp & 31)
        DS_DOW = 0
      end if

      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1SEC )                         ;begin with address for alarm1
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
        DS_Hour = BcdToDec(DS_Hour  and 127 )   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if

      I2CReceive(DS_Temp, NACK)       ;get the day OR Date
      if DS_Temp.6 then               ;Day mode
        DS_DOW = BcdToDec(DS_Temp & 31)
        DS_Date = 0
      else                            ;Date mode
        DS_Date = BcdToDec(DS_Temp & 31)
        DS_DOW = 0
      end if

      I2CStop

  #endif

end sub
;------
;DS1337_ClearAlarm1
sub DS1337_ClearAlarm1


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlStatus )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS_Temp.0 = 0                            ;clear bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( DS1337_ControlStatus )
      HI2CSend(DS_Temp)               ;send value
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlStatus )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS_Temp.0 = 0                            ;clear bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( DS1337_ControlStatus )
      I2CSend(DS_Temp)               ;send value
      I2CStop

  #endif

end sub
;------
;DS1337_EnableAlarm1Interrupt
sub DS1337_EnableAlarm1Interrupt


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlRegister )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS_Temp.0 = 1                            ;set bit
      DS_Temp.2 = 1                            ;set bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( DS1337_ControlRegister )
      HI2CSend(DS_Temp)               ;send value
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlRegister )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS_Temp.0 = 1                            ;set bit
      DS_Temp.2 = 1                            ;set bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( DS1337_ControlRegister )
      I2CSend(DS_Temp)               ;send value
      I2CStop

  #endif

end sub
;------
;DS1337_DisableAlarm1Interrupt
sub DS1337_DisableAlarm1Interrupt


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlRegister )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS_Temp.0 = 0                            ;clear bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( DS1337_ControlRegister )
      HI2CSend(DS_Temp)               ;send value
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlRegister )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS_Temp.0 = 0                            ;clear bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( DS1337_ControlRegister )
      I2CSend(DS_Temp)               ;send value
      I2CStop

  #endif

end sub
;------
;DS1337_Alarm1Status
function DS1337_Alarm1Status


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlStatus )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS1337_Alarm1Status = DS_Temp.0              ;extract bit value

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlStatus )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS1337_Alarm1Status = DS_Temp.0              ;extract bit value

  #endif

end function


;DS1337_DisableAlarm1
sub DS1337_DisableAlarm1


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM1SEC )
      HI2CSend( 0 )
      HI2CSend( 0 )
      HI2CSend( 0 )
      HI2CSend( 0 )
      HI2CStop

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_Value , NACK)      ;and get value
      HI2CStop
      DS_Value.0 = 0                    ;clear the bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CSend(DS_Value)
      HI2CStop

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlStatus)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_Value , NACK)      ;and get value
      HI2CStop
      DS_Value.0 = 0

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlStatus)              ;go to address
      HI2CSend(DS_Value)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM1SEC )
      I2CSend( 0 )
      I2CSend( 0 )
      I2CSend( 0 )
      I2CSend( 0 )
      I2CStop

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_Value , NACK)      ;and get value
      I2CStop
      DS_Value.0 = 0                    ;clear the bit

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CSend(DS_Value)
      I2CStop

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlStatus)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_Value , NACK)      ;and get value
      I2CStop
      DS_Value.0 = 0

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlStatus)              ;go to address
      I2CSend(DS_Value)
      I2CStop

  #endif

end sub

;-----
sub DS1337_ClearOscillatorStopFlag
  ; Clearing this bit resets Reading this bit shows the Oscillator Stop Flag Status (OSF)


  #ifdef HI2C_DATA

        do
          HI2CReStart                          ;generate a start signal
          HI2CSend(DS_AddrWrite)               ;inidcate a write
        loop While HI2CAckPollState
        HI2CSend( DS1337_ControlStatus )                     ;indicate register
        HI2CReStart
        HI2CSend(DS_AddrRead)
        HI2CReceive(DS_State, NACK)                      ;get the current value
        set DS_State.7 0                               ;CH bit = 0 disables
        HI2CReStart
        HI2CSend(DS_AddrWrite)
        HI2CSend( DS1337_ControlStatus )                 ;indicate register
        HI2CSend(DS_State)                               ;now send updated value
        HI2CStop

  #endif

  #ifdef I2C_DATA

       I2CStart
       I2CSend(DS_AddrWrite)
       I2CSend( DS1337_ControlStatus )                     ;indicate register
       I2CStart
       I2CSend(DS_AddrRead)
       I2CReceive(DS_State, NACK)                      ;get the current value
       set DS_State.7 0                               ;CH bit = 0 disables
       I2CStart
       I2CSend(DS_AddrWrite)
       I2CSend( DS1337_ControlStatus )                 ;indicate register
       I2CSend(DS_State)                               ;now send updated value
       I2CStop

  #endif

end sub
;-----
function DS1337_OscillatorStopFlagStatus
   ; Reading this bit shows the Oscillator Stop Flag Status (OSF)


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlStatus )                     ;indicate register
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_State, NACK)                        ;get the current status
      HI2CStop
      if DS_State.7 = 0 then
          DS1337_OscillatorStopFlagStatus = false
      else
          DS1337_OscillatorStopFlagStatus = true
      end if

  #endif

  #ifdef I2C_DATA

       I2CStart
       I2CSend(DS_AddrWrite)
       I2CSend( DS1337_ControlStatus )                     ;indicate register
       I2CStart
       I2CSend(DS_AddrRead)
       I2CReceive(DS_State, NACK)                        ;get the current status
       I2CStop
       if DS_State.7 = 0 then
          DS1337_OscillatorStopFlagStatus = false
       else
          DS1337_OscillatorStopFlagStatus = true
       end if

  #endif

end function

;--------

sub DS1337_HandleCentury ( in century_flag, DS_Century )


  #ifdef HI2C_DATA

      if century_flag = 1 then
           DS_Century++
           do
              HI2CReStart                          ;generate a start signal
              HI2CSend(DS_AddrWrite)               ;inidcate a write
           loop While HI2CAckPollState
           HI2CSend(DS1337_RTCMTH)
           HI2CReStart
           HI2CSend(DS_AddrRead)
           HI2CReceive(DS_Month, NACK)            ;get month register
           DS_Month.7 = 0
           HI2CStop

           HI2CReStart                              ;send back the flag cleared
           HI2CSend(DS_AddrWrite)
           HI2CSend(DS1337_RTCMTH)
           HI2CSend(DS_Month)
           HI2CStop

      end if


  #endif

  #ifdef I2C_DATA

      if century_flag = 1 then
           DS_Century++
           I2CStart
           I2CSend(DS_AddrWrite)
           I2CSend(DS1337_RTCMTH)
           I2CStart
           I2CSend(DS_AddrRead)
           I2CReceive(DS_Month, NACK)            ;get month register
           DS_Month.7 = 0
           I2CStop

           I2CStart                              ;send back the flag cleared
           I2CSend(DS_AddrWrite)
           I2CSend(DS1337_RTCMTH)
           I2CSend(DS_Month)
           I2CStop

      end if

  #endif

end sub


;------
;DS1337_SetAlarm2 ( hour, minute, DOW, date ) wher DOW or DATE can be 0
;The DY/DT bits (bit 6 of the alarm day/date registers)
;control whether the alarm value stored in bits 0 to 5 of
;that register reflects the day of the week or the date of
;the month. If DY/DT is written to logic 0, the alarm will
;be the result of a match with date of the month. If
;DY/DT is written to logic 1, the alarm will be the result of
;a match with day of the week.
sub DS1337_SetAlarm2 ( in DS_Hour, in DS_Min, in DS_DOW, in DS_Date )


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM2MIN )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Min)

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM2MIN )
      HI2CSend(DS_Value, NACK)
      HI2CStop

      HI2CReStart                            ;get and set hours
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM2HOUR )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Hour)

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM2HOUR )
      HI2CSend(DS_Value, NACK)
      HI2CStop

      HI2CReStart                            ;get and set date
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM2DAY_DATE )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop
      DS_Temp = ( DS_Temp AND 192 )

      if DS_Date <>  0 then    ;set alarm to the DATE, see notes above
         DS_Value = DS_Temp OR DecToBcd(DS_Date)
         DS_Value.6 = 0
      else
         DS_Value = DS_Temp OR DecToBcd(DS_DOW)
         DS_Value.6 = 1
      end if

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM2DAY_DATE )
      HI2CSend(DS_Value, NACK)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart                            ;get and set minutes
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2MIN )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Min)

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2MIN )
      I2CSend(DS_Value, NACK)
      I2CStop

      I2CStart                            ;get and set hours
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2HOUR )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)

      DS_Temp = ( DS_Temp AND 128 )
      DS_Value = DS_Temp OR DecToBcd(DS_Hour)

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2HOUR )
      I2CSend(DS_Value, NACK)
      I2CStop

      I2CStart                            ;get and set date
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2DAY_DATE )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop
      DS_Temp = ( DS_Temp AND 192 )

      if DS_Date <>  0 then    ;set alarm to the DATE, see notes above
         DS_Value = DS_Temp OR DecToBcd(DS_Date)
         DS_Value.6 = 0
      else
         DS_Value = DS_Temp OR DecToBcd(DS_DOW)
         DS_Value.6 = 1
      end if

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2DAY_DATE )
      I2CSend(DS_Value, NACK)
      I2CStop

  #endif

end sub

;------
; DS1337_Alarm2Assertion_EveryMinute                     0x07
; DS1337_Alarm2Assertion_Minutes                         0x06
; DS1337_Alarm2Assertion_HoursMinutes                    0x04
; DS1337_Alarm2Assertion_DateHoursMinutes                0x00
; DS1337_Alarm2Assertion_DayHoursMinutesSeconds          0x00

;DS1337_SetAlarmMask2 (   alarmAssertionMatch )
sub DS1337_SetAlarmMask2 ( in DS_Status )
  ;set the four bits for alarm 1. Bit by bit


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM2MIN )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the current value
      HI2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.0
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ALM2MIN)
      HI2CSend(DS_NewValue)
      HI2CStop                                     ;First bit

      HI2CReStart                                    ;Second bit
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM2HOUR )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the current value
      HI2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.1
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ALM2HOUR)
      HI2CSend(DS_NewValue)
      HI2CStop                                     ;Second bit

      HI2CReStart                                    ;Third bit
      HI2CSend(DS_AddrWrite)
      HI2CSend( DS1337_ALM2DAY_DATE )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_NewValue, NACK)                   ;get the current value
      HI2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.2
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      HI2CReStart                                    ;restart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ALM2DAY_DATE)
      HI2CSend(DS_NewValue)
      HI2CStop                                     ;Third bit

  #endif

#ifdef I2C_DATA

      I2CStart                                    ;First bit
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2MIN )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the current value
      I2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.0
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ALM2MIN)
      I2CSend(DS_NewValue)
      I2CStop                                     ;First bit

      I2CStart                                    ;Second bit
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2HOUR )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the current value
      I2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.1
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ALM2HOUR)
      I2CSend(DS_NewValue)
      I2CStop                                     ;Second bit

      I2CStart                                    ;Third bit
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2DAY_DATE )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_NewValue, NACK)                   ;get the current value
      I2CStop
      DS_NewValue = DS_NewValue and 0b01111111        ;get all valid bits
      DS_Temp = DS_Status.2
      DS_NewValue = DS_NewValue or ( DS_Temp *  128 );set bit values

      I2CStart                                    ;restart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ALM2DAY_DATE)
      I2CSend(DS_NewValue)
      I2CStop                                     ;Third bit

  #endif

end sub
;------
;DS1337_ReadAlarm2 ( hour, minute, DOW, date ). Return values
sub DS1337_ReadAlarm2 ( out DS_Hour, out DS_Min, out DS_DOW, out DS_Date )


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM2MIN )                         ;begin with address for Alarm2
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Min, ACK)              ;get the minutes
      DS_Min = BcdToDec(DS_Min and 127)
      HI2CReceive(DS_Hour, ACK)       ;get the hours
      if DS_Hour.6 then               ;12-hour mode
        DS_A_P = DS_Hour.5            ;a.m. or p.m.
        DS_Hour = BcdToDec(DS_Hour & 31)
      else
        DS_Hour = BcdToDec(DS_Hour  and 127)   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if

      HI2CReceive(DS_Temp, NACK)       ;get the day OR Date
      if DS_Temp.6 then               ;Day mode
        DS_DOW = BcdToDec(DS_Temp & 31)
        DS_Date = 0
      else                            ;Date mode
        DS_Date = BcdToDec(DS_Temp & 31)
        DS_DOW = 0
      end if

      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2MIN )                         ;begin with address for Alarm2
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Min, ACK)              ;get the minutes
      DS_Min = BcdToDec(DS_Min and 127)
      I2CReceive(DS_Hour, ACK)       ;get the hours
      if DS_Hour.6 then               ;12-hour mode
        DS_A_P = DS_Hour.5            ;a.m. or p.m.
        DS_Hour = BcdToDec(DS_Hour & 31)
      else
        DS_Hour = BcdToDec(DS_Hour  and 127)   ;24-hour mode
        DS_A_P = (DS_Hour > 11)       ;a.m. or p.m.
      end if

      I2CReceive(DS_Temp, NACK)       ;get the day OR Date
      if DS_Temp.6 then               ;Day mode
        DS_DOW = BcdToDec(DS_Temp & 31)
        DS_Date = 0
      else                            ;Date mode
        DS_Date = BcdToDec(DS_Temp & 31)
        DS_DOW = 0
      end if

      I2CStop

  #endif

end sub
;------
;DS1337_ClearAlarm2
sub DS1337_ClearAlarm2


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlStatus )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS_Temp.1 = 0                    ;clear bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( DS1337_ControlStatus )
      HI2CSend(DS_Temp)               ;send value
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlStatus )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS_Temp.1 = 0                    ;clear bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( DS1337_ControlStatus )
      I2CSend(DS_Temp)               ;send value
      I2CStop

  #endif

end sub
;------
;DS1337_EnableAlarm2Interrupt
sub DS1337_EnableAlarm2Interrupt


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlRegister )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS_Temp.1 = 1                            ;set bit
      DS_Temp.2 = 1                            ;set bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( DS1337_ControlRegister )
      HI2CSend(DS_Temp)               ;send value
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlRegister )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS_Temp.1 = 1                            ;set bit
      DS_Temp.2 = 1                            ;set bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( DS1337_ControlRegister )
      I2CSend(DS_Temp)               ;send value
      I2CStop

  #endif

end sub
;------
;DS1337_DisableAlarm2Interrupt
sub DS1337_DisableAlarm2Interrupt


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlRegister )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS_Temp.1 = 0                            ;clear bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)           ;indicate write mode
      HI2CSend( DS1337_ControlRegister )
      HI2CSend(DS_Temp)               ;send value
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlRegister )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS_Temp.1 = 0                            ;clear bit

      I2CStart
      I2CSend(DS_AddrWrite)           ;indicate write mode
      I2CSend( DS1337_ControlRegister )
      I2CSend(DS_Temp)               ;send value
      I2CStop

  #endif

end sub
;------
;DS1337_Alarm2Status
function DS1337_Alarm2Status


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ControlStatus )
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive(DS_Temp, NACK)
      HI2CStop

      DS1337_Alarm2Status = DS_Temp.1              ;extract bit value

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ControlStatus )
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive(DS_Temp, NACK)
      I2CStop

      DS1337_Alarm2Status = DS_Temp.1              ;extract bit value

  #endif

end function


;DS1337_DisableAlarm2
sub DS1337_DisableAlarm2


  #ifdef HI2C_DATA

      do
        HI2CReStart                          ;generate a start signal
        HI2CSend(DS_AddrWrite)               ;inidcate a write
      loop While HI2CAckPollState
      HI2CSend( DS1337_ALM2MIN)
      HI2CSend( 0 )
      HI2CSend( 0 )
      HI2CSend( 0 )
      HI2CStop

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_Value , NACK)      ;and get value
      HI2CStop
      DS_Value.1 = 0                    ;clear the bit

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlRegister)              ;go to address
      HI2CSend(DS_Value)
      HI2CStop

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlStatus)              ;go to address
      HI2CReStart
      HI2CSend(DS_AddrRead)
      HI2CReceive( DS_Value , NACK)      ;and get value
      HI2CStop
      DS_Value.1 = 0

      HI2CReStart
      HI2CSend(DS_AddrWrite)
      HI2CSend(DS1337_ControlStatus)              ;go to address
      HI2CSend(DS_Value)
      HI2CStop

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend( DS1337_ALM2MIN)
      I2CSend( 0 )
      I2CSend( 0 )
      I2CSend( 0 )
      I2CStop

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_Value , NACK)      ;and get value
      I2CStop
      DS_Value.1 = 0                    ;clear the bit

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlRegister)              ;go to address
      I2CSend(DS_Value)
      I2CStop

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlStatus)              ;go to address
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_Value , NACK)      ;and get value
      I2CStop
      DS_Value.1 = 0

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS1337_ControlStatus)              ;go to address
      I2CSend(DS_Value)
      I2CStop

  #endif

end sub


;------
function DS1337_ReadRegister ( in DS_Value )
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
      DS1337_ReadRegister = DS_status

  #endif

  #ifdef I2C_DATA

      I2CStart
      I2CSend(DS_AddrWrite)
      I2CSend(DS_Value)              ;go to register
      I2CStart
      I2CSend(DS_AddrRead)
      I2CReceive( DS_status , NACK)      ;and get value
      I2CStop
      DS1337_ReadRegister = DS_status

  #endif

end function


;------
sub DS1337_WriteRegister ( in DS_Value, in DS_Temp )
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
