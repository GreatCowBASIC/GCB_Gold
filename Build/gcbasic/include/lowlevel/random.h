'    Pseudo random number generator routines for the GCBASIC compiler
'    Copyright (C) 2006-2025 Hugh Considine

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
' 14/08/22 Updated user changeable constants only - no functional change
' 23/08/22 Correct typo in Script
' 28/08/22 Expand calc to reduce maths complexity

'Initialisation routine
#startup InitRandom

#SCRIPT
'A TIMER TO USE AS A RANDOM SEED
  IF NODEF(RANDOMIZE_SEED) THEN
    RANDOMIZE_SEED=TIMER0
  END IF
#ENDSCRIPT

'Get initial random seeds
#define RandStart = INT(RND(1) * 65536) + 1

'Initialisation routine
Sub InitRandom
  Dim RandomSeed As Word
  RandomSeed = RandStart
End Sub

'Function to get a random number
function Random
  Repeat 7
    Dim RandomTemp As Word
    Dim RandomSeed As Word
    RandomTemp = RandomSeed
    Rotate RandomTemp Left Simple
    'Taps at 16, 15, 13, 11, And with b'1101 0100 0000 0000' = D800

    //Expanded below calculation to reduce complexity of calc
      RandomSeed = RandomTemp XOR (Not (RandomSeed And 1) + 1 And 0xB400)
  
    //Expanded below calculation to reduce complexity of calc
      // RandomSeed = Not (RandomSeed And 1) + 1 
      // RandomSeed = RandomSeed And 0xB400
      // RandomSeed = RandomTemp XOR RandomSeed

    Random = Random Xor RandomSeed_H
  End Repeat

end function

'Sub to set the random seed
sub Randomize(Optional In RandomTemp As Word = RANDOMIZE_SEED)
  Dim RandomSeed As Word
  RandomSeed += RandomTemp
  'If random seed gets set to 0, Random will return nothing but 0
  If RandomSeed = 0 Then RandomSeed = 1
end sub
