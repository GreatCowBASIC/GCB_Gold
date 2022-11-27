'    TEA5767 routines for the GCBASIC compiler
'    Copyright (C) 2014 -2020 Evan Venn

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


#define TEA5767_AddrWrite 192
#define TEA5767_AddrRead  193

'Variables
dim pll as Long
dim RFreqI as Byte
dim RFreqD as Byte
dim RadioFreq as Word
dim RadioTemp as Byte alias pll
dim SeekTimes as Byte
dim RadioStatusReg ( 12)
dim RadioConfigReg ( 12)
RFreqI = 0


Sub SetRadioFreq (In RFreqI, In RFreqD)
    pll=RFreqI*1000000+((RFreqD*100000)/10)+225000
    RadioFreq=pll/8192
    if SysCalcTempX >= 50 then
       RadioFreq=[word]RadioFreq +1
    end if
    RadioConfigReg(2)=RadioFreq%256
    RadioConfigReg(1)=RadioFreq/256
    RadioWrite
End Sub

Sub ReadRadioFreq (Out RadioFreq, Out RFreqI, Out RFreqD)
    RadioRead
    RadioTemp=RadioStatusReg(1) AND 63
    RadioFreq=256*RadioTemp+RadioStatusReg(2)
    pll=RadioFreq*8192
    RFreqI=pll/1000000
    RFreqD=(pll-RFreqI*1000000-255000)/10000
    if RFreqD>=100 then
       RFreqD=[word]RFreqD/10
       if SysCalcTempX >= 5 then
          RFreqD=[word]RFreqD+1
       end if
    end if
End Sub

Sub RadioInit
    SeekTimes=0
    RadioFreq=10709 '87.5MHZ
    RadioConfigReg(1)=0 'No MUTE, Not in search mode
    RadioConfigReg(1)=RadioFreq/256
    RadioConfigReg(2)=RadioFreq%256
    RadioConfigReg(3)=16 'No mute channels , Stereo , High side LO injection
    RadioConfigReg(4)=18 'Stereo Noise Canceling, XTAL=32.768KHz, US_Europe Band
    RadioConfigReg(5)=0
    RadioWrite
End sub


Sub SeekUp
    SetRadioFreq (RFreqI, RFreqD)
    RadioConfigReg(1).7=1
    RadioConfigReg(1).6=1
    RadioConfigReg(3).7=1
    RadioConfigReg(3).6=0
    RadioConfigReg(3).5=1
    RadioWrite

    ContinueSeek:
    wait 50 ms
    RadioRead
    if RadioStatusReg(1).7=0 then Goto ContinueSeek
    ReadRadioFreq (RadioFreq, RFreqI, RFreqD)
    if RadioStatusReg(1).7=1 then
       if RadioStatusReg(1).6=1 then
          SetRadioFreq (87, 50)
          RadioConfigReg(1).7=1
          RadioConfigReg(1).6=1
          RadioConfigReg(3).7=1
          RadioConfigReg(3).6=0
          RadioConfigReg(3).5=1
          RadioWrite
          wait 100 ms
          SeekTimes=SeekTimes+1
          if SeekTimes>=2 then
             SeekTimes=0
             Exit Sub
          end if
          Goto ContinueSeek
       end if
       RadioConfigReg(1).7=0
       RadioConfigReg(1).6=0
       RadioWrite
       SeekTimes=0
       Exit Sub
    end if
End sub

Sub SeekDown
    RadioConfigReg(1).6=1
    RadioConfigReg(3).7=0
    RadioWrite
End sub

Sub SeekStop
    RadioConfigReg(1).6=0
    RadioWrite
End sub

Sub RadioMono
    RadioConfigReg(3).3=1
    RadioWrite
End sub

Sub RadioStereo
    RadioConfigReg(3).3=0
    RadioWrite
End sub

Sub Mute
    RadioConfigReg(1).7=1
    RadioWrite
End sub

Sub UnMute
    RadioConfigReg(1).7=0
    RadioWrite
End sub

Sub RadioStandby
    RadioConfigReg(4).6=1
    RadioWrite
End sub

Sub RadioPlay
    RadioConfigReg(4).6=0
    RadioWrite
End sub

Sub JapaneseBand  ' 76-90MHz
    RadioConfigReg(4).5=1
    RadioWrite
End sub

Sub US_EuropeBand  ' 87.5-108MHz
    RadioConfigReg(4).5=0
    RadioWrite
End sub

Function StationFound
   RadioRead
   if RadioStatusReg(1).7=1 and RadioStatusReg(1).6=0 then
      StationFound=True
   else
      StationFound=False
   end if
End Function

Function RadioBandLimit
   RadioRead
   if RadioStatusReg(1).6=1 then
      RadioBandLimit=True
   else
      RadioBandLimit=False
   end if
End Function

Function StereoReception
   RadioRead
   if RadioStatusReg(3).7=1 then
      StereoReception=True
   else
      StereoReception=False
   end if
End Function


Sub RadioWrite

  #ifdef HI2C_DATA
    HI2CSTART
    HI2CSEND ( TEA5767_AddrWrite, True )
    HI2CSEND (RadioConfigReg(1), ACK)
    HI2CSEND (RadioConfigReg(2), ACK)
    HI2CSEND (RadioConfigReg(3), ACK)
    HI2CSEND (RadioConfigReg(4), ACK)
    HI2CSEND (RadioConfigReg(5), ACK)
    HI2CStop
  #endif

  #ifdef I2C_DATA
    I2CSTART
    I2CSEND ( TEA5767_AddrWrite, True )
    I2CSEND (RadioConfigReg(1), ACK)
    I2CSEND (RadioConfigReg(2), ACK)
    I2CSEND (RadioConfigReg(3), ACK)
    I2CSEND (RadioConfigReg(4), ACK)
    I2CSEND (RadioConfigReg(5), ACK)
    I2CStop
  #endif


End sub


Sub RadioRead
  #ifdef HI2C_DATA
    HI2CSTART
    HI2CSEND ( TEA5767_AddrRead , True)
    HI2CReceive (RadioStatusReg(1) , ACK )
    HI2CReceive (RadioStatusReg(2) , ACK )
    HI2CReceive (RadioStatusReg(3) , ACK )
    HI2CReceive (RadioStatusReg(4) , ACK )
    HI2CReceive (RadioStatusReg(5) , NACK )
    HI2CStop
  #endif

  #ifdef I2C_DATA
    I2CSTART
    I2CSEND ( TEA5767_AddrRead , True)
    I2CReceive (RadioStatusReg(1) , ACK )
    I2CReceive (RadioStatusReg(2) , ACK )
    I2CReceive (RadioStatusReg(3) , ACK )
    I2CReceive (RadioStatusReg(4) , ACK )
    I2CReceive (RadioStatusReg(5) , NACK )
    I2CStop
  #endif


End sub
