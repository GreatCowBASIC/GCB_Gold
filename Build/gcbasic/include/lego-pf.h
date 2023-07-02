'    Lego Power Functions IR interface routines for the GCBASIC compiler
'    Copyright (C) 2011-2020 Hugh Considine

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

'    This file is included for the convenience of the user, and does not represent any
'    form of endorsement of or association with GCBASIC by Lego.

'Notes:
' Requires that IR_In is defined as a constant

'Changes:
' 18/12/2011: First version
' 23/12/2011: VLL?

#define PWM_Freq 38

'''Control speed of outputs
'''@param PFChannel Channel of receiver (1 to 4)
'''@param PFPowerA Power of output A (0: float, 1-7:forward, 8: brake, 15-9: reverse)
'''@param PFPowerB Power of output B (0: float, 1-7:forward, 8: brake, 15-9: reverse)
Sub PFComboPWM(In PFChannel, In PFPowerA, In PFPowerB)
  PFSendCommand PFChannel, 16 + PFPowerB, PFPowerA
End Sub

'''Send a low level command to the Power Functions receiver
'''@param PFChannel Channel of receiver (1 to 4)
'''@param PFMode Mode command to send
'''@param PFData Data for mode
Sub PFSendCommand(In PFChannel, In PFMode, In PFData)

  'Format output data
  Dim PFOutData As Word
  Dim PFSendBuffer As Word

  'Clear all bits that aren't set yet
  PFOutData = 0

  'Escape bit (0 for Mode, 1 for Combo PWM)
  'Get from fifth bit of PFMode
  If PFMode.4 = 1 Then
    'Mode bits (controls motor B)
    PFOutData_H = PFMode And 0x0F
    'Escape bit
    PFOutData.14 = 1
  Else
    'Address bit (0)
    'Mode bits (3 bits)
    PFOutData_H = PFMode And 0x07
  End If

  'Channel bits (2 bits, 1-4)
  PFChannel -= 1
  If PFChannel.1 = 1 Then PFOutData.13 = 1
  If PFChannel.0 = 1 Then PFOutData.12 = 1

  'Data bits (4 bits)
  If PFData.3 = 1 Then PFOutData.7 = 1
  If PFData.2 = 1 Then PFOutData.6 = 1
  If PFData.1 = 1 Then PFOutData.5 = 1
  If PFData.0 = 1 Then PFOutData.4 = 1

  'Need to resend message at correct rate
  For PFResendNo = 1 to 5

    'Set Toggle bit
    PFToggle += 1
    PFOutData.15 = PFToggle.0

    'Calc Checksum bits (4 bits)
    PFChecksum = 0x0F XOR Swap4(PFOutData_H)
    PFChecksum = PFChecksum XOR PFOutData_H
    PFChecksum = PFChecksum XOR PFData
    PFChecksum = PFChecksum And 0x0F
    PFOutData = PFOutData And 0xFFF0
    PFOutData = PFOutData Or PFChecksum

    'Send command
    'Start bit
    PWMOn
    Wait 158 us
    PWMOff
    Wait 1026 us

    'Command
    PFSendBuffer = PFOutData
    Repeat 16
      PWMOn
      Wait 158 us
      PWMOff
      If PFSendBuffer.15 = 1 Then
        Wait 552 us
      Else
        Wait 263 us
      End If
      Rotate PFSendBuffer Left
    End Repeat

    'Stop bit
    PWMOn
    Wait 158 us
    PWMOff
    Wait 1026 us

    'Calculate appropriate delay
    '(Depends on channel and resend count)
    If PFResendNumber < 3 Then
      Wait 80 ms
    Else
      PFChecksum = 8 + 2 * PFChannel
      Repeat PFChecksum
        Wait 16 ms
      End Repeat
    End If

  Next
End Sub

Sub VLLBegin
  Dir VLL_OUT Out
  PulseOut VLL_OUT, 400 ms
End Sub

Sub VLLSend (In VLLData)

  'Dim VLLDataOut As Word Alias VLLChecksum, VLLData
  Dim VLLDataOut As Word

  'Calc checksum
  VLLTemp = VLLData
  VLLChecksum = 0
  Repeat 3
    VLLChecksum = VLLChecksum + VLLTemp
    Rotate VLLTemp Right
    Rotate VLLTemp Right
    VLLTemp = VLLTemp And b'00111111'
  End Repeat
  VLLChecksum = VLLChecksum And 7
  VLLChecksum = 7 - VLLChecksum

  'Initial pulse/start bit
  Wait 20 ms

  'Send checksum and 7 bit VLL code
  Rotate VLLData Left
  [byte]VLLDataOut = VLLData
  VLLDataOut_H = VLLChecksum
  Repeat 10
    If VLLDataOut.10 = 1 Then
      PulseOut VLL_OUT, 20 ms
      Wait 40 ms
    Else
      PulseOut VLL_OUT, 40 ms
      Wait 20 ms
    End If
    Rotate VLLDataOut Left
  End Repeat

  'Stop bit
  PulseOut VLL_OUT, 20 ms
  Wait 60 ms
  PulseOut VLL_OUT, 120 ms
  'On again for 120 ms?

End Sub
