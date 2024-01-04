'    Lego Mindstorms interface routines for the GCBASIC compiler
'    Copyright (C) 2006-2024 Hugh Considine

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

'    For more info on Lego Mindstorms, please see http://www.legomindstorms.com.
'    This file is included for the convenience of the user, and does not represent any
'    form of endorsement of or association with GCBASIC by Lego.

'Notes:
' Requires that IR_In is defined as a constant

'Changes:
' 18/12/2011: Some tidying of code, GCGB help added

'Subroutines
'RCX Standard firmware compatible
' SendMessage (message)     Send a message using standard Lego Mindstorms protocol
' SendOpcode (data(), size)   Send the array data() using the standard Lego protocol
' ReceiveOpcode (data(), size)    Read a standard-protocol opcode into the array data()
'RCX BrickOS/LeJOS compatible
' SendLNP (data(), size)    Send the array data() using the LNP protocol
' SendLNPAdd (address, data(), size)  Send the array data() to the given address, using LNP
' ReceiveLNP (data(), size)   Read from the LNP protocol, storing the received data
' SetLNPAddress (port, address)   Set own LNP address to selected value between 0 and 15. Port is usually 1

'Functions
' Message       Receive a message using standard Lego Mindstorms protocol

'Specify the initialisation routine for this file
#startup InitLego

'Serial Port Settings
#define RecALow IR_In = On
#define RecAHigh IR_In = Off
#define SendAHigh PWMOn
#define SendALow PWMOff

'Misc Settings
#define PWM_Freq 38
#define PWM_Duty 50

'''@hide
Sub InitLego
  InitSer(1,r2400,WaitForStart+1,8,1,odd,invert)
  OwnLNPPort = 1
End Sub

'''Sends an RCX message
'''@param LegoTemp Message number to send (0 to 255)
Sub SendMessage(In LegoTemp)
  'Send Header
  SerSend(1, 0x55)
  SerSend(1, 0xff)
  SerSend(1, 0x00)
  'Message Command
  SerSend(1, 0xf7)
  SerSend(1, 0x08)
  'Message Value
  SerSend(1, LegoTemp)
  SerSend(1, !LegoTemp)
  'Checksum
  SerSend(1, 0xf7+LegoTemp)
  SerSend(1, 0x08+(!LegoTemp))
End Sub

''' Read an RCX message. Will return 0 if no message is being sent.
function Message
  Message = 0
  if RecALow then exit function
  'Header
  'Can be 55 ff 00 or ff 00.
  SerReceive(1, LegoTemp)
  if LegoTemp = 0x55 then SerReceive(1, LegoTemp)
  SerReceive(1, LegoTemp)
  'Command
  SerReceive(1, LegoTemp)
  if LegoTemp <> 0xf7 then exit function
  SerReceive(1, LegoTemp)
  'Message
  SerReceive(1, Message)
  SerReceive(1, LegoTemp)
  'Checksum
  SerReceive(1, LegoTemp)
  SerReceive(1, LegoTemp)
end function

'''Send an opcode using the standard RCX IR protocol
'''@param LegoArray Array of data to send. Complements and checksum are calculated.
'''@param LegoByteCount Number of data bytes in LegoArray
sub SendOpcode(In LegoArray(), In LegoByteCount)
  LegoChecksum = 0

  'Header
  SerSend(1, 0x55)
  SerSend(1, 0xff)
  SerSend(1, 0x00)

  'Opcodes
  for LegoByte = 1 to LegoByteCount
    LegoTemp = LegoArray(LegoByte)
    SerSend(1, LegoTemp)
    SerSend(1, !LegoTemp)
    LegoChecksum = LegoChecksum + LegoTemp
  next

  'Checksum
  SerSend(1, LegoChecksum)
  SerSend(1, !LegoChecksum)
end sub

'''Receive an opcode using the standard RCX IR protocol
'''@param LegoArray Array to store received bytes in
'''@param LegoByteCount Variable to store number of bytes received
sub ReceiveOpcode(LegoArray(), Out LegoByteCount)
  LegoByteCount = 0
  LegoChecksum = 0
  if RecALow then exit function

  'Header
  SerReceive(1, LegoTemp)
  if LegoTemp = 0x55 then SerReceive(1, LegoTemp)
  SerReceive(1, LegoTemp)

  'Command
  SerReceive(1, LegoCommand)
  SerReceive(1, LegoTemp)
  LegoArray(1) = LegoCommand
  LegoParams = LegoCommand AND 7
  LegoByteCount = LegoParams + 1

  'Parameters
  for LegoByte = 1 to LegoParams
    SerReceive(1, LegoTemp)
    LegoArray(LegoByte+1) = LegoTemp
    LegoChecksum = LegoChecksum + LegoTemp
    SerReceive(1, LegoTemp)
  next

  'Checksum
  SerReceive(1, LegoTemp)
  LegoChecksum = LegoChecksum - LegoTemp
  SerReceive(1, LegoTemp)
end sub

'''Send LNP (BrickOS/LeJOS) integrity layer packet
'''@param LNPArray Bytes to send
'''@param LNPByteCount Number of bytes in LNPArray
sub SendLNP(In LNPArray(), In LNPByteCount)
  LNPChecksum = 0xef + LNPByteCount '0xf0 + byte count - 1

  'Send type
  SerSend(1, 0xf0)

  'Byte Count
  SerSend(1, LNPByteCount)

  'Data
  for LNPByte = 1 to LNPByteCount
    LNPTemp = LNPArray(LNPByte)
    LNPChecksum = LNPTemp + LNPChecksum
    SerSend(1, LNPTemp)
  next

  'Checksum
  SerSend(1, LNPChecksum)
end sub

'''Send LNP (BrickOS/LeJOS) addressing layer packet
'''@param LNPPort Destination port for data
'''@param LNPAddress Destination address for data
'''@param LNPArray Bytes to send
'''@param LNPByteCount Number of bytes in LNPArray
sub SendLNPAdd(In LNPPort, In LNPAddress, In LNPArray(), In LNPByteCount)
  LNPChecksum = 0xf2 + LNPByteCount '0xf1 + byte count + 2 - 1

  'Send type
  SerSend(1, 0xf1)

  'Byte Count
  SerSend(1, LNPByteCount+2)

  'Destination
  LNPTemp = LNPPort
  LNPTemp = (Swap4(LNPTemp) and 0xf0)+(LNPAddress and 0x0f)
  SerSend(1, LNPTemp)
  LNPChecksum = LNPTemp + LNPChecksum

  'Source
  LNPTemp = OwnLNPPort
  LNPTemp = (Swap4(LNPTemp) and 0xf0)+(OwnLNPAddress and 0x0f)
  SerSend(1, LNPTemp)
  LNPChecksum = LNPTemp + LNP_Checksum

  'Data
  for LNPByte = 1 to LNPByteCount
    LNPTemp = LNPArray(LNPByte)
    LNPChecksum = LNPTemp + LNPChecksum
    SerSend(1, LNPTemp)
  next

  'Checksum
  SerSend(1, LNPChecksum)
end sub

'''Receive LNP (BrickOS/LeJOS) packet. Will detect if addressing or integrity
'''layer packet has been received.
'''@param LNPArray Array to store received bytes in
'''@param LNPByteCount Number of bytes in LNPArray
sub ReceiveLNP(LNPArray(), Out LNPByteCount)
  LNPByteCount = 0
  LNPChecksum = 0
  if RecALow then exit sub

  SerReceive(1, LNPType) 'Header
  SerReceive(1, LNPByteCount) 'Byte Count
  LNPChecksum = LNPType + LNPByteCount - 1
  if LNPType = 0xf0 then goto LNPIntReceive
  if LNPType = 0xf1 then goto LNPAddReceive
  LNPChecksum = FALSE
  exit function

  LNPAddReceive:
  SerReceive(1, LNP_Receiver)
  SerReceive(1, LNP_Sender)
  LNPByteCount = LNPByteCount - 2
  LNPChecksum = LNP_Checksum + LNP_Receiver + LNP_Sender

  LNPIntReceive:
  for LNPByte = 1 to LNPByteCount
    SerReceive(1, LNPTemp)
    LNPArray(LNPByte) = LNPTemp
    LNPChecksum = LNPChecksum + LNPTemp
  next

  SerReceive(1, LegoTemp)
  if LegoTemp = LNPChecksum then LNPChecksum = TRUE: exit function
  LNPChecksum = FALSE
end sub

'''Set LNP Address. The ReceiveLNP routine will only receive packets sent to
'''the address set here.
'''@param OwnLNPPort LNP port to listen on
'''@param OwnLNPAddress LNP address to listen for
sub SetLNPAddress(In OwnLNPPort, In OwnLNPAddress)
end sub
