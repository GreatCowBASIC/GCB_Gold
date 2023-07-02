'    Sound/Tone generation routines for GCBASIC
'    Copyright (C) 2006-2020 Hugh Considine

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
'
'    30/09/2020 Fixed a VERY LONG STANDING BUG!! variable not initialised!
'    24/07/2021 Added DIM for Variable "TonePerid" / Removed commented out lines
' 14/08/22 Updated user changeable constants only - no functional change

'********************************************************************************
'IMPORTANT:
'THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
'UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
'COMMANDS UNUSABLE!
'********************************************************************************

'Defaults
#IFNDEF SOUNDOUT
  #DEFINE SOUNDOUT SYSTEMP.0 'SET TO PORT FOR SOUND OUTPUT
#ENDIF

#IFNDEF SOUNDOUT2
  #DEFINE SOUNDOUT2 SYSTEMP.0 'SECOND PORT, USED BY MULTITONE ONLY
#ENDIF

#startup InitSound

'Frequency is Hz, Duration is in 10ms units
sub Tone (In ToneFrequency as word, In ToneDuration as word)

  dim ToneLoop as word
  dim ToneLoop2 as word
  dim TonePeriod as Word

  If ToneFrequency = 0 Then Exit Sub


  TonePeriod = 50000 / ToneFrequency
  ToneFrequency = ToneFrequency / 100

  For ToneLoop = 1 to ToneDuration
    For ToneLoop2 = 1 to ToneFrequency
      Set SoundOut ON
      SysToneDelay
      Set SoundOut OFF
      SysToneDelay
    Next
  Next
End Sub

'#define SysToneDelay Wait TonePeriod 10us: Wait TonePeriod 10us
#define SysToneDelay Wait TonePeriod 10us


sub InitSound
 dir SoundOut out
end sub

sub ShortToneDelay
 for ToneDelayLoop = 1 to TonePeriod
  Wait 9 10us
 next
end sub


'Frequency is Hz/10, Duration is in 1 ms units
sub ShortTone (ToneFrequency, ToneDuration) #NR

 TonePeriod = 250/ToneFrequency
 ToneFrequency = ToneFrequency / 10
 if ToneDuration =< 4 then ToneFrequency = ToneFrequency/4
 if ToneDuration > 4 then ToneDuration = ToneDuration/4

 for ToneLoop = 1 to ToneDuration
  for ToneLoop2 = 1 to ToneFrequency
   SET SoundOut ON
   ShortToneDelay
   SET SoundOut OFF
   ShortToneDelay
  next
 next
end sub
