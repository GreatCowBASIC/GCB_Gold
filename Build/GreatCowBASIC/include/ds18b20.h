'    DS18b20 temperature sensor routines for the GCBASIC compiler
'    Copyright (C) 2007-2020 Kent Schafer, R. White and Evan Venn

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
'    Supports a single DS18B20 only.
'    Device serial port/pin needs to be defined in main program
     ' Port/pin settings, an example is shown below:
     ' #define DQ PortA.0
'    These routines use the 12 bit sensor functionality.
'    These routines will NOT work with DS18S20. This a 9 bit sensor and these routines will not work.

'     11/2013 updated to support with the 12 bit DS18b20 with temperature range from negative to positive as per device datasheet
      ' Uses Subroutine "ReadDigitalTemp" instead of function as it's complicated with decimal places
      ' modified to read negative number using XOR to convert 2's complement to signed integer
      ' Reads two bytes into 16 bit signed integer variable.
      ' A string varible "lookup table" stores the decimal digits.
      ' Timings lengthened slightly to handle long wires. Tested reliably with 3 meter cable.
      ' Oscilloscope shows 0 -> 5V (10% -90%) rise time of 14 uS with 3 Meter cable length
      ' Logic high stable time is 14uS to 28uS after pulse start

       'To read a value, call the ReadDigitalTemp subroutine.
         'DSint holds the decimal temperature
         'DSdec string holds the decimal places

'     8/2014  updated to return integer and raw data values. Improved documentation
'     updated to support GCGB
      ' New Functions "Readtemp" and "Readtemp12"

      ' Readtemp
        ' Reads temperature from a DS18B20 digital temperature sensor and store in
        ' the output variable.  The conversion takes up to 750ms.  Readtemp carries out a full 12 bit
        ' conversion and then rounds the result to the nearest full degree Celsius (byte
        ' value). For the full 12 bit value use the readtemp12 command.

        ' The temperature is read back in whole degree steps, and the sensor operates from
        ' -55 to + 125 degrees Celsius. Note that bit 7 is 0 for positive temperature values
        ' and 1 for negative values (ie negative values will appear as 128 + numeric value).
        ' Note the readtemp command does not work with the older DS1820 or DS18S20
        ' as they have a different internal resolution. This command is not designed to be
        ' used with parasitically powered DS18B20 sensors, the 5V pin of the sensor must
        ' always be connected.

      ' Readtemp12
        ' Reads 12 bit temperature data from a DS18B20 digital temperature sensor and
        ' store in variable. The conversion takes up to 750ms. Both readtemp and
        ' readtemp12 take the same time to convert.
        ' This command is for advanced users only. For standard whole degree data use the readtemp command.
        ' The temperature is read back as the raw 12 bit data into a word variable (0.0625
        ' degree resolution). The user must interpret the data through mathematical
        ' manipulation. See the DS18B20 datasheet for more information on the 12 bit
        ' Note the readtemp12 command does not work with the older DS1820 or DS18S20 as they have a different internal resolution.
        ' This command is not designed to be used with parasitically powered DS18B20 sensors, the 5V pin of the sensor must be connected.

' 16/8/2014 - Revised following feedback Kent.
  ' The original DS18S20 header file had an initial wait period of 1 s which was unnecessary, and likely an artifact of debugging.  So the initial wait of 250 ms could be dropped.
  ' After OWout ConvertT, instead of waiting the 750 ms, use polling.  This makes for shorter wait times:
  '
  '  Do
'                dir DQ out
'                Set DQ Off
'                wait 4 us
'                Dir DQ In
'                wait 7 us
'                If DQ = 1 Then Exit Do
'                wait 60 us    'minimum 60 us time slots between reads
'            Loop
  ' The wait of 1ms between one wire commands is unnecessary.
' 15/9/2014 - Correct silly errors in the code.  Timings were incorrect.
'    Revised 28.08.2017 to resolve non-ANSI characters
'    Added   14.04.2020  D18B20SetResolution  ( DS18B20_TEMP_9_BIT | DS18B20_TEMP_10_BIT, DS18B20_TEMP_11_BIT, DS18B20_TEMP_12_BIT )





' Define varaibles
  Dim RxData As Word   'used to support reading data stream
  Dim DSdata As Word   'need this for negative numbers. (Leave RXdata as a working byte variable)
  Dim DSdec as  String 'need this for holding decimal place from pseudo "lookup table"
  Dim DSint as integer 'need this to return minus sign (set bit 15 = 1 (= negative))

' Define device ROM Commands for 1-Wire DS18B20
#define SearchRom 240   '0xF0 (240)
      'Command for identifying Slave Rom codes, use as many
      'times as needed
#define ReadRom 51            '0x33 (051)
      'Command for single slave
#define MatchRom 85   '0x55 (085)
      'Command for master to identify a specific slave code
#define SkipRom 204   '0xCC (204)
      'Command for addressing all devices simultaneously
#define AlarmSearch 236     '0xEC (236)
      'Command is same as Search Rom to identify any alarm flags

' Define function Commands for for 1-Wire DS18B20
#define ConvertT 68   '0x44 (68)
      'Protocol for Single Temp Conversion
#define WriteScratch 78     '0x4E (78)
      'Protocol for Write Scratchpad
#define ReadScratch 190     '0xBE (190)
      'Protocol for Read Scratchpad
#define CopyScratch  72       '0x48 (72)
      'Protocol for copying Scratchpad TH and TL
      'registers to EEPROM
#define RecallE2 184  '0xB8 (184)
      'Protocol for recalling alarm trigger values from EEPROM
 #define ReadPowerSupply 180  '0xB4 (180)
      'Protocol for identifying Power Supply Mode

      'Protocol resolution
#define DS18B20_TEMP_9_BIT  0x1F
#define DS18B20_TEMP_10_BIT 0x3F
#define DS18B20_TEMP_11_BIT 0x5F
#define DS18B20_TEMP_12_BIT 0x7F


' Public Routines


'''Returns the value of the sensor
'''@returns whole part of the sensor value as DSint
'''@returns decimal part of the sensor value as DSdec
sub ReadDigitalTemp


    DS18B20DeviceRead
    ' Is this a negative value?
    ' XOR with 65535 and add oneto get 2's compliment
    If DSdata.15 = 1 then DSdata = (DSdata XOR 65535) + 1       'convert twos complement to decimal by xor'ing with 65535 and adding 1

    DSint = DSdata /[word] 16  ' shift right four times to lose lowest nibble (decimal data nibble). the variable now contains a round temperature number
    fraction=0
    fraction.0 = DSdata.0      'lowest nibble contains fraction of degree measured by sensor
    fraction.1 = DSdata.1
    fraction.2 = DSdata.2
    fraction.3 = DSdata.3

    ' Pseudo lookup table for decimal digits
    if fraction = 0 then  DSdec = "0   "
    if fraction = 1 then  DSdec = "0625"
    if fraction = 2 then  DSdec = "125 "
    if fraction = 3 then  DSdec = "1875"
    if fraction = 4 then  DSdec = "25  "
    if fraction = 5 then  DSdec = "3125"
    if fraction = 6 then  DSdec = "375"
    if fraction = 7 then  DSdec = "4375"
    if fraction = 8 then  DSdec = "5   "
    if fraction = 9 then  DSdec = "5625"
    if fraction = 10 then DSdec = "625 "
    if fraction = 11 then DSdec = "6875"
    if fraction = 12 then DSdec = "75  "
    if fraction = 13 then DSdec = "8125"
    if fraction = 14 then DSdec = "875 "
    if fraction = 15 then DSdec = "9375"

    ' Set value to negative value
    If RxData.7 = 1 then     ' rxData bit 7 will be =1 if negative temperature.
       DSint = DSint * -1       ' multiply by -1 . DSint integer needs to be converted to negative number after calculations (which always return a positive number after xor) .
    end if
end sub





'''Returns the value of the sensor as an integer rounded up to the nearest integer
'''@return Sensor value as an integer
function readtemp as integer
  ' Reads temperature from a DS18B20 digital temperature sensor and store in
  ' the output variable.  The conversion takes up to 750ms.  Readtemp carries out a full 12 bit
  ' conversion and then rounds the result to the nearest full degree Celsius (byte
  ' value). For the full 12 bit value use the readtemp12 command.

  ' The temperature is read back in whole degree steps, and the sensor operates from
  ' -55 to + 125 degrees Celsius. Note that bit 7 is 0 for positive temperature values
  ' and 1 for negative values (ie negative values will appear as 128 + numeric value).
  ' Note the readtemp command does not work with the older DS1820 or DS18S20
  ' as they have a different internal resolution. This command is not designed to be
  ' used with parasitically powered DS18B20 sensors, the 5V pin of the sensor must
  ' always be connected.

   DS18B20DeviceRead
   If DSdata.15 = 1 then
     ' its negative
     DSdata = ( DSdata # 0xffff ) + 1 ' take twos comp
     SignBit = true
   else
     SignBit = false
   end if

   readtemp =  DSdata * 6   ' convert raw data 12 bit value of 0.0625
   DSdata = ( DSdata * 25 ) / 100
   readtemp = readtemp + DSdata

   Whole = readtemp / 100
   fraction = TempC_100 % 100
   if fraction > 50 then
      Whole++
   end if

   if SignBit = true then
      readtemp = Whole + 128
   else
      readtemp =  Whole
   end if


end function




'''Returns the RAW value of the sensor
'''@return Raw sensor value as an integer
function readtemp12 as integer
  ' Reads 12 bit temperature data from a DS18B20 digital temperature sensor and
  ' store in variable. The conversion takes up to 750ms. Both readtemp and
  ' readtemp12 take the same time to convert.
  ' This command is for advanced users only. For standard whole degree data use the readtemp command.
  ' The temperature is read back as the raw 12 bit data into a word variable (0.0625
  ' degree resolution). The user must interpret the data through mathematical
  ' manipulation. See the DS18B20 datasheet for more information on the 12 bit
  ' Note the readtemp12 command does not work with the older DS1820 or DS18S20 as they have a different internal resolution.
  ' This command is not designed to be used with parasitically powered DS18B20 sensors, the 5V pin of the sensor must be connected.

   DS18B20DeviceRead
   readtemp12 = DSdata

end function

' Private Routines


'''@hide
Sub DS18B20DeviceRead
  ' This routine makes available a variable called DSdata
  ' DSdata is the raw data value from the sensor

   wait 250 ms ' Not really needed but left in just in case ***
   MasterRST   ' Reset ds18b20
   PPulse      ' Request a presence pulse
   wait 1 ms
   OWout SkipRom  ' Bypass ds18b20 serial number read
   wait 1 ms
   OWout ConvertT  ' Instruct ds18b20 to begin temperature conversion to digital
   Do
                dir DQ out
                Set DQ Off
                wait 4 us
                Dir DQ In
                wait 7 us
                If DQ = 1 Then Exit Do
                wait 60 us    'minimum 60 us time slots between reads
   Loop

   MasterRST
   PPulse
'   wait 1 ms
   OWout SkipRom         ' Bypass ds18b20 serial number read
'   wait 1 ms
   OWout ReadScratch     ' Read temperature from db18b20
'   wait 1 ms
   Owin                  ' Receive temperature low byte from db18b20
   DSdata = RxData       ' First byte  is low byte
'   wait 1 ms
   Owin                  ' Receive temperature high byte from db18b20
   DSdata_H = RxData     ' Second byte is high byte  (auto combined into 16 bit word)
end sub


' Utility routines for the device

'''@hide
Sub MasterRST  ' Master reset low for minimum 480 us
 Dir DQ In
 Dir DQ Out
 Set DQ Off
 wait 60 10us   ' Set to 60 10us for longer wires
 Dir DQ In      ' HiZ
end sub

'''@hide
Sub PPulse                ' Master receive DS18B20 presence pulse
  wait 100 us             ' Set to 100us for long bus wires         'test code
  wait 43 10us
  Dir DQ In
end sub

'''@hide
Sub OWout (Command) #NR

 For ClocksA = 1 to 8
  Dir DQ Out
  Set DQ Off
  wait 3 us    ' Need to release bus within 15 us
  If Command.0 On then
  Dir DQ In
  End if
  wait 100 us  ' Set to 60 us per AN2420 datasheet indicates MAX delay is 120 us . changed to 100uS for longer wires. Only relevant for writing zeros
  Dir DQ In    ' HiZ the bus if still pulled low
  wait 3 us
  ROTATE Command Right  'The DS18B20 wants data LSB first
 Next ClocksA
end sub

'''@hide
Sub OWin
  ' Sets RxData
   For ClocksB = 1 to 8
      Rotate RxData Right
      Dir DQ Out
      Set DQ Off                   ' Read time slot
      wait 6 us                    ' Minimum is 1uS
      Dir DQ In                    ' Release bus for one wire Reception
      wait 4 us
      If DQ On  Then RxData.7 = 1
      If DQ Off Then RxData.7 = 0
      wait 60 us                   ' Set to 60 uS  for use with longer wires
   Next ClocksB
end sub



Sub DS18B20SetResolution ( in DS18B20SetResolutionValue  )

   wait 250 ms ' Not really needed but left in just in case ***
   MasterRST
   PPulse
   wait 1 ms
   OWout SkipRom         ' Bypass ds18b20 serial number read
   wait 1 ms
   OWout WriteScratch
   wait 1 ms
   OWout 0x00             'User byte 0 - Unused
   OWout 0x00             'User byte 1 - Unused


   Select Case DS18B20SetResolutionValue
        Case DS18B20_TEMP_9_BIT
              OWout ( DS18B20_TEMP_9_BIT )  'set up 9bits
        Case DS18B20_TEMP_10_BIT
              OWout ( DS18B20_TEMP_10_BIT )  'set up 10bits
        Case DS18B20_TEMP_11_BIT
              OWout ( DS18B20_TEMP_11_BIT )  'set up 11bits
        Case DS18B20_TEMP_12_BIT
              OWout ( DS18B20_TEMP_12_BIT )  'set up 12bits
        Case Else
              OWout ( DS18B20_TEMP_12_BIT )  'set up en 12 bits (0x7F)
   End Select

      wait 250 ms ' wait for write to complete


End Sub