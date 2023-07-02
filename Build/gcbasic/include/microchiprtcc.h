;    Library for reading/writing to Microchip DS1337 RTC for the GCBASIC compiler
;    Copyright (C) 2017-2020 Evan Venn
;;
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

;    initial works by Thomas Henry, May 26, 2014 for DS1307

;    values in this library are BCD users should use Decimal

'''
'''******************************************************************
''' LCD demonstration for the board. We have added the LCD section to specific the ports.
''' All the LCD commands are standard Great Cow BASIC LCD commands.
'''
''' The methods used in this demonstration are shown below.
'''
''' rtcc_Initialize - required set intial values
''' rtcc_TimeGet    - MPLAB equivilent method. Return values in global variables as follows:
'''    tm_year, tm_mon, tm_wday, tm_mday, tm_hour, tm_min, tm_sec
''' rtcc_GetFormatted_Value converts byte data to tens and digites ( rtcc_data as byte , out digits , out tens)
''' rtcc_SetClock(hour, minute, second, DOW, date, month, year)
''' rtcc_SetTime(hour, minute, second)
''' rtcc_SetDate(DOW, date, month, year)
''' rtcc_ReadClock(hour, minute, second, a.m.|p.m., DOW, date, month, year)
''' rtcc_ReadTime(hour, minute, second, a.m.|p.m.)
''' rtcc_ReadDate(DOW, date, month, year)
''' rtcc_Enable ( true or false )
''' rtcc_ReadAlarm ( hour, minute, second, DOW, date ). Return values
''' rtcc_SetAlarm ( hour, minute, second, DOW, date )
''' rtcc_SetAlarmMask ( alarmAssertionMatch )
''' rtcc_ClearAlarm  unset the alarm )
''' rtcc_ReStartAlarm  reset an existing alarm
''' rtcc_GetDecimalValue ( value ) return number value in tens and digits
''' rtcc_DecToBcd  Convert pure decimal number to binary coded decimal
''' rtcc_BcdToDec  Convert binary coded decimal to pure decimal
'''
'''************************************************************************
'' 26/9/2018   Revised to support VBAT


    '*****************************************************************************************************
    'Main library commences here.. everything before this is setup for the board.


'Example RTCC specific setup

'    #define  rttc_AlarmPulse porta.1
'    #define  rttc_AlarmPort porta.0
'
'    #define rtcc_Calibration_Offset 0
'
'    'SETUP THE RTCC
'    rtcc_Initialize
'    rtcc_SetAlarm( 0 , 0 , 4 , 0, 0, 0 )   ' every 4th second
'    rtcc_SetAlarmMark ( rtcc_AlarmAssertion_EverySecond )
'    on Interrupt RTCAlarmTrigger call rttc_Interrupt



 #define rtcc_Calibration_Offset 0

'************************* RTCC Library Functions ****************************************************
' You should not change below here.


 #define rtcc_AlarmAssertion_EveryHalfSecond                     0x04
 #define rtcc_AlarmAssertion_EverySecond                         0x05
 #define rtcc_AlarmAssertion_EveryTenSeconds                     0x06
 #define rtcc_AlarmAssertion_EveryMinute                         0x07
 #define rtcc_AlarmAssertion_EveryTenMinutes                     0x08
 #define rtcc_AlarmAssertion_EveryHour                           0x09
 #define rtcc_AlarmAssertion_OnceADay                            0x0A
 #define rtcc_AlarmAssertion_OnceAWeek                           0x0B
 #define rtcc_AlarmAssertion_OnceAMonth                          0x0C
 #define rtcc_AlarmAssertion_OnceAYear                           0x0D


#startup rtcc_Initialize, 1000

sub rtcc_Initialize
    dim  rtccTimeInitialized  as Bit
    rtccTimeInitialized  = false
    rttc_AlarmPort = 0


    #ifdef rttc_AlarmPulse
      dir rttc_AlarmPulse out
    #endif

    #ifdef rttc_AlarmPort
      dir rttc_AlarmPort  out
    #endif

    'CSWHOLD may proceed; SOSCPWR High power;
    CSWHOLD = 0
    SOSCPWR = 1

'    ---------     IMPORTANT ! -----------------
    '//  If RTCEN bit is set at same time or before
    '//  RTCWREN bit, then rtcc will not start


    'In order to be able to write the Write Enable(WREN) bit for rtcc you neet to enable EEPROM writing
    'it is strongly recommended to disable interrupts during this code segment
     INTOFF
     Wait 1 s               'Wait stabilisation
     RTCCON.rtccLKSEL0 = 0  'POR  Default
     RTCCON.rtccLKSEL1 = 0  'POR Default
     RTCCON.RTCWREN = 1     'MUST Write this bit before RTCEN
     Wait 100 ms            'Wait for SOSC to stabilise

      'If you set this here the you will always reset the clock to the same value... generally not a good idea.
      if ( rtccTimeInitialized = false ) then
          'set rtcc time 2017-10-28 12-13-22
'          YEAR     = 0x17      ' year
'          MONTH    = 0x12      ' month
'          WEEKDAY  = 0x04      ' weekday
'          DAY      = 0x14      ' day
'          HOURS    = 0x23      ' hours
'          MINUTES  = 0x59      ' minutes
'          SECONDS  = 0x00      ' seconds
          rtccTimeInitialized = true
      end if
      'calibration register
      rtccAL = rtcc_Calibration_Offset
      'Enable rtcc, clear RTCWREN
      RTCCON.RTCEN = 1
      wait while RTCCON.RTCSYNC = 0
      RTCCON.RTCWREN = 0
      INTON

end sub



' This function implements rtcc_TimeGet. It access the
' registers of  rtcc and writes to them the values provided
' in a time structure.

    'public varirables
    dim tm_year as word
    dim tm_mon, tm_wday, tm_mday, tm_hour, tm_min, tm_sec as byte
function rtcc_TimeGet


    if ( RTCCON.RTCSYNC = 0 ) then
        rtcc_TimeGet =  false
        exit function
    end if

    'Set the RTCWREN bit
    RTCCON.RTCWREN = 1

    'get year
    tm_year    = ConcatInt( 20, rtcc_BcdToDec(YEAR) )
    'get month
    tm_mon     = rtcc_GetDecimalValue(MONTH)
    ' get weekday
    tm_wday    = rtcc_GetDecimalValue(WEEKDAY)
    'get day
    tm_mday    = rtcc_GetDecimalValue(DAY)
    'get hour
    tm_hour    = rtcc_GetDecimalValue(HOURS)
    'get minutes
    tm_min     = rtcc_GetDecimalValue(MINUTES)
    'get seconds
    tm_sec     = rtcc_GetDecimalValue(SECONDS)

    RTCCON.RTCWREN = 0
    rtcc_TimeGet = true

end function

sub rtcc_getFormatted_Value ( in rtcc_data as byte , out digits as byte , out tens  as byte )

          Digits = rtcc_data AND 0b00001111
          Tens.0 = rtcc_data.4
          Tens.1 = rtcc_data.5
          Tens.2 = rtcc_data.6

end sub

' Set the clock
sub rtcc_setClock(in tm_Hour, in tm_Min, in tm_Sec, in tm_wday, in tm_mday, in tm_Mon, in tm_Year)

      '    ---------     IMPORTANT ! -----------------
      'If RTCEN bit is set at same time or before
      'RTCWREN bit, then rtcc will not start

      'In order to be able to write the Write Enable(WREN) bit for rtcc you neet to enable EEPROM writing
      'it is strongly recommended to disable interrupts during this code segment
      INTOFF
      RTCCON.RTCEN = 0
      RTCCON.RTCWREN = 1     'MUST Write this bit before RTCEN

      YEAR     = rtcc_DecToBcd (tm_Year)      ' year
      MONTH    = rtcc_DecToBcd (tm_Mon)       ' month
      WEEKDAY  = rtcc_DecToBcd (tm_wday)      ' weekday
      DAY      = rtcc_DecToBcd (tm_mday)      ' day
      HOURS    = rtcc_DecToBcd (tm_Hour)      ' hours
      MINUTES  = rtcc_DecToBcd (tm_Min)       ' minutes
      SECONDS  = rtcc_DecToBcd (tm_Sec)       ' seconds
      rtccTimeInitialized = true

      'Enable rtcc, clear RTCWREN
      RTCCON.RTCEN = 1
      wait while RTCCON.RTCSYNC = 0
      RTCCON.RTCWREN = 0
      INTON
end sub

' Set the time
sub rtcc_SetTime(in tm_Hour, in tm_Min, in tm_Sec)

      '    ---------     IMPORTANT ! -----------------
      'If RTCEN bit is set at same time or before
      'RTCWREN bit, then rtcc will not start

      'In order to be able to write the Write Enable(WREN) bit for rtcc you neet to enable EEPROM writing
      'it is strongly recommended to disable interrupts during this code segment
      INTOFF
      RTCCON.RTCEN = 0
      RTCCON.RTCWREN = 1     'MUST Write this bit before RTCEN

      HOURS    = rtcc_DecToBcd (tm_Hour)      ' hours
      MINUTES  = rtcc_DecToBcd (tm_Min)       ' minutes
      SECONDS  = rtcc_DecToBcd (tm_Sec)       ' seconds
      rtccTimeInitialized = true

      'Enable rtcc, clear RTCWREN
      RTCCON.RTCEN = 1
      wait while RTCCON.RTCSYNC = 0
      RTCCON.RTCWREN = 0
      INTON
end sub

' Set the date
sub rtcc_setDate(in tm_wday, tm_mday, in tm_Mon, in tm_Year)

      '    ---------     IMPORTANT ! -----------------
      'If RTCEN bit is set at same time or before
      'RTCWREN bit, then rtcc will not start

      'In order to be able to write the Write Enable(WREN) bit for rtcc you neet to enable EEPROM writing
      'it is strongly recommended to disable interrupts during this code segment
      INTOFF
      RTCCON.RTCEN = 0
      RTCCON.RTCWREN = 1     'MUST Write this bit before RTCEN

      YEAR     = rtcc_DecToBcd( tm_Year)      ' year
      MONTH    = rtcc_DecToBcd( tm_Mon )    ' month
      WEEKDAY  = rtcc_DecToBcd( tm_wday )      ' weekday
      DAY      = rtcc_DecToBcd( tm_mday )      ' day
      rtccTimeInitialized = true

      'Enable rtcc, clear RTCWREN
      RTCCON.RTCEN = 1
      wait while RTCCON.RTCSYNC = 0
      RTCCON.RTCWREN = 0
      INTON
end sub

' Read the clock
sub rtcc_ReadClock(out tm_hour, out tm_min, out tm_sec, out tm_wday, out tm_mday, out tm_mon, out tm_year)
    ;Read entire clock: hours, minutes, seconds, day of week, date, month, year

    if ( RTCCON.RTCSYNC = 0 ) then
        rtcc_TimeGet =  false
    end if

    'get year
    tm_year    = ConcatInt( 20, rtcc_BcdToDec(YEAR) )
    'get month
    tm_mon     = rtcc_BcdToDec(MONTH)
    ' get weekday
    tm_wday    = rtcc_BcdToDec(WEEKDAY)
    'get day
    tm_mday    = rtcc_BcdToDec(DAY)
    'get hour
    tm_hour    = rtcc_BcdToDec(HOURS)
    'get minutes
    tm_min     = rtcc_BcdToDec(MINUTES)
    'get seconds
    tm_sec     = rtcc_BcdToDec(SECONDS)


    rtcc_TimeGet = true

End Sub

' Read the time
sub rtcc_Readtime(out tm_hour, out tm_min, out tm_sec)
    ;Read entire clock: hours, minutes, seconds, day of week, date, month, year

    if ( RTCCON.RTCSYNC = 0 ) then
        rtcc_TimeGet =  false
    end if

    'Set the RTCWREN bit
    RTCCON.RTCWREN = 1
    tm_hour    = rtcc_BcdToDec(HOURS)
    'get minutes
    tm_min     = rtcc_BcdToDec(MINUTES)
    'get seconds
    tm_sec     = rtcc_BcdToDec(SECONDS)

    RTCCON.RTCWREN = 0
    rtcc_TimeGet = true

End Sub


' Read the date
sub rtcc_ReadDate(out tm_wday, out tm_mday, out tm_mon, out tm_year)
    ;Read entire clock: hours, minutes, seconds, day of week, date, month, year

    if ( RTCCON.RTCSYNC = 0 ) then
        rtcc_TimeGet =  false
    end if

    'Set the RTCWREN bit
    RTCCON.RTCWREN = 1

    'get year
    tm_year    = ConcatInt( 20, rtcc_BcdToDec(YEAR) )
    'get month
    tm_mon     = rtcc_BcdToDec(MONTH)
    ' get weekday
    tm_wday    = rtcc_BcdToDec(WEEKDAY)
    'get day
    tm_mday    = rtcc_BcdToDec(DAY)
    RTCCON.RTCWREN = 0
    rtcc_TimeGet = true

End Sub

' Enable the RTCC
Sub rtcc_Enable ( in rtcc_data )

     RTCCON.RTCWREN = 1     'MUST Write this bit before RTCEN
     'Enable /Disable rtcc, clear RTCWREN
     RTCCON.RTCEN = rtcc_data.0
     wait while RTCCON.RTCSYNC = 0
     RTCCON.RTCWREN = 0

End Sub

' Read the Alarm settings
sub rtcc_ReadAlarm( out tm_Hour, out tm_Min, out tm_Sec, out tm_wday, out tm_mday, out tm_mon )


    if ( RTCCON.RTCSYNC = 0 ) then
        rtcc_TimeGet =  false
    end if

    'Set the RTCWREN bit
    RTCCON.RTCWREN = 1

    'get month
    tm_mon     = rtcc_BcdToDec(ALRMMTH)
    ' get weekday
    tm_wday    = rtcc_BcdToDec(ALRMWD)
    'get day
    tm_mday    = rtcc_BcdToDec(ALRMDAY)
    'get hour
    tm_hour    = rtcc_BcdToDec(ALRMHR)
    'get minutes
    tm_min     = rtcc_BcdToDec(ALRMMIN)
    'get seconds
    tm_sec     = rtcc_BcdToDec(ALRMSEC)

    RTCCON.RTCWREN = 0
    rtcc_TimeGet = true

end sub

'Set the alarm
sub rtcc_SetAlarm( in tm_Hour, in tm_Min, in tm_Sec, in tm_DOW, in tm_mday, in tm_month )

      ALRMCON.ALRMEN = 0
      ALRMMTH     = tm_Month     ' month
      ALRMWD      = tm_DOW       ' weekday
      ALRMDAY     = tm_mday      ' day
      ALRMHR      = tm_Hour      ' hours
      ALRMMIN     = tm_Min       ' minutes
      ALRMSEC     = tm_Sec       ' seconds
      ALRMCON.ALRMEN = 1

end sub

'Set the alarm mask
Sub rtcc_SetAlarmMark ( in rtcc_data )

    AMASK0 = rtcc_data.0
    AMASK1 = rtcc_data.1
    AMASK2 = rtcc_data.2
    AMASK3 = rtcc_data.3

End Sub

'Clear the alarm
sub rtcc_ClearAlarm
      ALRMCON.ALRMEN = 0
End sub

'Restart the alarm
sub rtcc_ReStartAlarm
      ALRMCON.ALRMEN = 1
End sub

'Get the decimal value of register
function rtcc_GetDecimalValue ( in rtcc_data ) as word

  dim rtcc_Digits, rtcc_Tens as byte

  rtcc_Digits = rtcc_data AND 0b00001111
  rtcc_Tens.0 = rtcc_data.4
  rtcc_Tens.1 = rtcc_data.5
  rtcc_Tens.2 = rtcc_data.6
  rtcc_GetDecimalValue =  ConcatInt( rtcc_Tens, rtcc_Digits )

end function

'Concat to numbers
function ConcatInt( uint1 as word, uint2) as word

    dim ConcatInt_temp as Word
    ConcatInt_temp = 1

    if (uint2 = 0) then

        if ( uint1 =  20)  then 'if the result is about the year

            ConcatInt_temp = 100

        else

            ConcatInt_temp = 10

        end if

    end if


    while ( ConcatInt_temp <= uint2 )

        ConcatInt_temp = ConcatInt_temp * 10

    loop

    ConcatInt = [word]( [word]uint1 * [word]ConcatInt_temp ) + uint2

end function

'Convert pure decimal number to binary coded decimal
function rtcc_DecToBcd(in rtcc_va) as byte
  rtcc_DecToBcd=( rtcc_va/10)*16+ rtcc_va%10
end function

'Convert binary coded decimal to pure decimal
function rtcc_BcdToDec(in rtcc_va) as byte
  rtcc_BcdToDec=( rtcc_va/16)*10+ rtcc_va%16
end function

'Example handler for the interrupt
'sub rttc_Interrupt
'    'Clear the Interrupt
'    RTCCIF = 0
'    rttc_AlarmPort = !rttc_AlarmPort
'    rtcc_ReStartAlarm
'end sub
