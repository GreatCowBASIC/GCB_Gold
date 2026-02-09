  '
  '    Copyright Dimitris Katsaounis <katsjim@users.sf.net> - 2014-2026
  '    Initial release
  '    Support MAX6675 - tested by Dimitris Katsaounis
  '    Max6675.h       - created by Evan R Venn






  '.............................................................................
  '          reads data from the device
  '.............................................................................


  sub MAX6675(Out SysMaxDeviceTemp , Out D2bit)
    SysMaxDeviceTemp = 0

    'Force CS low to output the first bit on the SO pin
      Set CS off
    'Cycle the clock for dummy bit 15
    Set SCK on
    Set SCK off

    'Read bits 14-3 from MAX6675
    For j=1 to 12
      Set SCK on
      wait 1 ms
      if SO=on Then
         SysMaxDeviceTemp = [word]SysMaxDeviceTemp+1
      else
      SysMaxDeviceTemp = [word]SysMaxDeviceTemp+0
    End if
    Rotate SysMaxDeviceTemp Left Simple
    Set SCK off
    Next j
    'Read D2 Bit.
    'Bit D2 is normally low and
    'goes high when the thermocouple input is open
    Set SCK on
    wait 1 ms
    if SO=on then
       D2Bit=1
    else
       D2Bit=0
    end if
    Set SCK off
    'Force CS High to stop transmition
    Set CS on

    SysMaxDeviceTemp=[word]SysMaxDeviceTemp/8

  end sub
