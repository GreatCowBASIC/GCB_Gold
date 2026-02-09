'    USB routines for the GCBASIC compiler
'    Copyright (C) 2014-2026 Hugh Considine and Evan R. Venn

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

'Notes:
'

'Changes:
' 25/7/2014: Started file
' 27/8/2014: Enumeration working. Still more to do.
' 28/8/2014: More handlers added for control transfers
' 11/6/2018: Major revision for first formal release.
' 13/6/2018: Updated to include USBBufferWrite()
' 30/10/201: Added USB_INTERFACE_PROTOCOL
' 14/08/22 Updated user changeable constants only - no functional change
' 08/18/2024: Fixed allocation of ram for low memory parts (18F13K50, 18F14K50, 18F2450 and 18F4450) Angel Mier
' 08/18/2024: Changed DIM to ALLOC to clarify usage

' Example User code for SetupHandler_CallBack showing usage of USBBufferWrite
'
' Notes
'
' CallBack methods now have _Callback appended
' USBRequest is a public varialbe of the incoming request number
' All sending of acknowlegement and data is handled by the calling methods - users only need to address
'   1. Consuming incoming requests
'   2. Providing data via USBBufferWrite()
'
'        Sub SetupHandler_CallBack
'
'              Select Case USBRequest
'
'                'Response to control request: Transfer USBDeviceReadPortb5LEDStatus, send status of port
'                Case USBDeviceReadPortb5LEDStatus
'                  USBBufferWrite( LED1 )
'
'                'Response to control request: Transfer USBDeviceSetPortb5StatusOn, turn on LED
'                Case USBDeviceSetPortb5StatusOn
'                  Set LED1 On
'
'                'Response to control request: Transfer USBDeviceSetPortb5StatusOff, turn off LED
'                Case USBDeviceSetPortb5StatusOff
'                  Set LED1 Off
'
'                'Response to control request: Transfer USBDeviceReadADCValues, send back ADC readings
'                Case USBDeviceReadADCValues
'
'                  USBBufferWrite( ReadAD10(AN0) )
'                  USBBufferWrite( ReadAD10(AN1) )
'                  USBBufferWrite( ReadAD10(AN2) )
'                  USBBufferWrite( ReadAD10(AN3) )
'
'                'Response to control request: Transfer 255, keep alive
'                Case USBDeviceKeepAlive
'                  USBBufferWrite( USBDeviceKeepAlive )
'                  HeartBeatLED = !HeartBeatLED
'
'                'Handle the others via the serial port as we do not know what they are
'                Case Else
'                  HSerPrint "Req "
'                  HSerPrint USBTempBuffer(1)
'                  HSerPrintCRLF
'              End Select
'
'        End Sub



'Note: On 18F4550, make sure to set VREGEN on in config

' User changeable constants
  #IFNDEF USB_VENDOR_NAME
    #DEFINE USB_VENDOR_NAME "GCBASIC"
  #ENDIF
  #IFNDEF USB_PRODUCT_NAME
    #DEFINE USB_PRODUCT_NAME "GCBASIC USB TEST"
  #ENDIF
' End of user changeable constants

'Initialisation routine
#startup InitUSB

'Hardware settings
'''@hardware All; Vendor ID; USB_VID; const_word
'''@hardware All; Product ID; USB_PID; const_word
'''@hardware All; Product ID; USB_REV; const_word
'''@hardware All; Vendor Name; USB_VENDOR_NAME; string
'''@hardware All; Product Name; USB_PRODUCT_NAME; string

'Constants


#define USB_ENDPOINTS 1
#define USB_CURRENT_MA 50


'Set these to call appropriate subroutines and deal with USB input
'#define USB_SETUP_HANDLER
'#define USB_DESCRIPTOR_HANDLER
'#define USB_IN_HANDLER
'#define USB_OUT_HANDLER
'#define USB_ERROR_HANDLER

'Set these to replace descriptors
#define USB_DEVICE_DESCRIPTOR USBDeviceDescriptor
#define USB_CONFIG_DESCRIPTOR USBConfigDescriptor

#define USB_DEVICE_CLASS 0            'Use class code info from Interface Descriptors
#define USB_DEVICE_SUB_CLASS 0
#define USB_DEVICE_PROTOCOL 0

'Interface class (set to 3 for HID, 0xFF for user defined class)
#define USB_INTERFACE_CLASS 0xFF
#define USB_INTERFACE_SUBCLASS 0
#define USB_INTERFACE_PROTOCOL 0xFF


#define USB_EP_0_IN 0
#define USB_EP_0_OUT 80
#define USB_EP_1_IN 1
#define USB_EP_1_OUT 81
'#define USB_EP_1_IN 2
'#define USB_EP_1_OUT 82



#define USB_RAM_START USB_RAM_START_MEMORY_ADDRESS
#define USB_RAM_SIZE USB_RAM_SIZE_BYTES
#define USB_MAX_PACKET 128

'PID values
'Multiply by 4 because of bit shifting when reading BDSTAT
#define USB_PID_OUT = 4 * 1
#define USB_PID_IN = 4 * 9
#define USB_PID_SOF = 4 * 5
#define USB_PID_SETUP = 4 * 13
#define USB_PID_DATA0 = 4 * 3
#define USB_PID_DATA1 = 4 * 11
#define USB_PID_DATA2 = 4 * 7
#define USB_PID_MDATA = 4 * 15
#define USB_PID_ACK = 4 * 2
#define USB_PID_NACK = 4 * 10
#define USB_PID_STALL = 4 * 14
#define USB_PID_NYET = 4 * 6

'States
#define USB_STATE_POWERED 0
#define USB_STATE_DEFAULT 1
#define USB_STATE_ADDRESS 2
#define USB_STATE_CONFIGURED 3

'Control transfers
#define USB_GET_STATUS 0
#define USB_CLEAR_FEATURE 1
#define USB_SET_FEATURE 3
#define USB_SET_ADDRESS 5
#define USB_GET_DESCRIPTOR 6
#define USB_SET_DESCRIPTOR 7
#define USB_GET_CONFIGURATION 8
#define USB_SET_CONFIGURATION 9
#define USB_GET_INTERFACE 10
#define USB_SET_INTERFACE 11
#define USB_SYNCH_FRAME 12
#define USB_NONE 255


#define UOWN 7

'Endpoint buffers
Dim USBTempBuffer(16)
Dim USBTempString As String
// Dim USBRam(USB_RAM_SIZE - 1 ) At USB_RAM_START
Dim USBRam as Alloc * USB_RAM_SIZE At USB_RAM_START // change to ALLOC to clarify usage

''Buffer descriptors
Dim USB_OUT0_STAT Alias USBRAM(0)
Dim USB_OUT0_CNT Alias USBRAM(1)
Dim USB_OUT0_ADDR As Word Alias USBRAM(3), USBRAM(2)
Dim USB_IN0_STAT Alias USBRAM(4)
Dim USB_IN0_CNT Alias USBRAM(5)
Dim USB_IN0_ADDR As Word Alias USBRAM(7), USBRAM(6)

Dim USBRequest as Byte Alias USBTempBuffer(1)

'Global variables for user space
Dim USBDescType as byte
Dim USB_CNT_POINTER as byte
'Global constants for user space
#define USBDeviceKeepAlive 255


'Script to check clock speed
#script

  // Set the defaults in the case the user forgets!
  IF NODEF(USB_VID) THEN
    USB_VID = 0x1209
  END IF
  IF NODEF(USB_PID) THEN
    USB_PID = 0x2006
  END IF
  IF NODEF(USB_VID) THEN
    'The default USB_REV ID.  DO NOT USE THIS ID! See https://sourceforge.net/p/gcbasic/discussion/579125/thread/1e7daf8a/#0034/d2d3
    USB_REV = 0x0000
  END IF

  If ChipMHz = 6 Then
    USB_LOWSPEED = True
  End If
  If ChipMhz = 48 Then
    USB_HIGHSPEED = True
  End If
  If ChipMhz <> 6 And ChipMHz <> 48 Then
    Error "Chip speed must be 6 MHz or 48 MHz for USB library"
  End If
  If NoVar(UCFG) Then
    Error "Current chip is not compatible with the USB library"
  End If

  if ChipFamily = 15 then
      USB_RAM_START_MEMORY_ADDRESS = 0x2000
      USB_RAM_SIZE_BYTES = 512
  end if

  if ChipFamily = 16 then
      USB_RAM_START_MEMORY_ADDRESS = 0x400
      USB_RAM_SIZE_BYTES = 512
  end if

  if ChipRam < 1024 and ChipEEprom <> 0 then
    'Setting for current low ram parts (ie 18F14k50, 18F13K50)
      USB_RAM_START_MEMORY_ADDRESS = 0x200
      USB_RAM_SIZE_BYTES = 256
  end if

  if ChipRam < 1024 and ChipEEprom = 0  then
    'Settings for old low ram parts (ie 18F2450 18F4450)
      USB_RAM_START_MEMORY_ADDRESS = 0x400
      USB_RAM_SIZE_BYTES = 256
  end if


#endscript

'Subs
Sub InitUSB

  'Time for PLL to stabilise
  Wait 2 ms

  'Add interrupt handlers
  UIE = 127
  UEIE = b'10011111'
  On Interrupt USB Call USBInterruptHandler

  'Set up USB module
  #ifdef USB_HIGHSPEED
    Set UCFG.FSEN On
  #endif
  #ifdef Bit(UTRDIS)
    Set UCFG.UTRDIS Off
  #endif
  Set UCFG.UPUEN On

  USBResetEndpoints

  'Clear interrupt flags
  UIR = 0
  UEIR = 0

  UADDR = 0
  USBState = USB_STATE_POWERED
  USBCurrConfiguration = 0

  'Enable
  Set UCON.USBEN On

End Sub

'''Resets all USB endpoints to default state. Used on hard reset and USB reset.
Macro USBResetEndpoints

  'Take ownership of all endpoints
  USB_OUT0_STAT = 8
  USB_IN0_STAT = 8

  'Clear transmission complete flag
  Do While TRNIF
    TRNIF = 0
  Loop

  'Disable endpoints
  UEP0 = 0

  USB_IN0_ADDR = USB_RAM_START + 64
  USB_OUT0_ADDR = USB_RAM_START + 64 + USB_MAX_PACKET
  'Set buffer data counts to 0
  USB_IN0_CNT = 0
  USB_OUT0_CNT = USB_MAX_PACKET
  USB_IN0_STAT = 8
  USB_OUT0_STAT = 0x88

  'Set up endpoint 0 for handshaking, setup, in and out, but not stalled
  PKTDIS = 0
  UEP0 = b'00010110'

End Macro

'''Handler for USB interrupts
'''@hide
Sub USBInterruptHandler
  'Event that should be handled by library?
  'Reset
  If URSTIF Then

    USBResetEndpoints
    UADDR = 0

    'Clear interrupt flags
    UEIR = 0
    UIR = 0
    SUSPND = 0

    'Wait for single ended zero to clear
    Wait While UCON.SE0

    USBLastControl = USB_NONE
    USBState = USB_STATE_DEFAULT
    USBCurrConfiguration = 0

  End If

  'Error
  If UERRIF Then

    #ifdef USB_ERROR_HANDLER
      USB_ERROR_HANDLER
    #endif

    UEIR = 0
    UERRIF = 0
  End If

  'Activity on D+/D-
  If ACTVIF Then
    'If activity detected, switch from suspend to normal
    SUSPND = 0

    'Clear ACTVIF
    'Do While ACTVIF
      Set ACTVIF Off
    'Loop

  End If

  'Transfer complete
  If TRNIF Then
    'Determine transfer that has occurred
    USBCurrEndpoint = (USTAT And 0x78) / 8
    If USTAT.DIR Then
      'In
      USBBufferStat = USB_IN0_STAT
    Else
      'Setup/Out
      USBBufferStat = USB_OUT0_STAT
      USB_OUT0_STAT.UOWN = 0
    End If
    'Get PID (is multiplied by 4)
    USBPID = USBBufferStat And 0x3C

    Select Case USBPID
      Case USB_PID_OUT
        'Out transfer
        #IF SCRIPT_USART_USAGE_CHECK > 0
        'HSerPrint "OUT"
        'HSerPrintCRLF
        #ENDIF 

        USB_OUT0_CNT = USB_MAX_PACKET
        USB_OUT0_STAT.UOWN = 1

      Case USB_PID_IN
        'In transfer completed
        #IF SCRIPT_USART_USAGE_CHECK > 0
        'HSerPrint "IN"
        'HSerPrintCRLF
        #ENDIF
        Select Case USBLastControl
          'Set address completed
          Case USB_SET_ADDRESS
            UADDR = USBNewAddress
            USBState = USB_STATE_ADDRESS
            USBLastControl = USB_NONE

          'Continue responding to Get Descriptor
          Case USB_GET_DESCRIPTOR
            USBSendDescriptor

          Case Else
            'Prepare incoming buffer again
            USB_OUT0_CNT = USB_MAX_PACKET
            USB_OUT0_STAT = 0x88
        End Select

      Case USB_PID_SETUP
        'Setup transfer
        'Copy data to buffer
        For USBCurrByte = 0 to 7
          USBTempBuffer(USBCurrByte) = Peek(USB_OUT0_ADDR + USBCurrByte)
        Next
        'Stop any outgoing transmissions
        USB_IN0_STAT = 8
        'Prepare incoming buffer again
        USB_OUT0_CNT = USB_MAX_PACKET
        USB_OUT0_STAT = 0x88

        USBProcessSetup
        'USBDumpControlIn
        #IF SCRIPT_USART_USAGE_CHECK > 0
        'HSerPrint "S"
        'HSerPrintCRLF
        #ENDIF

      Case Else
        #IF SCRIPT_USART_USAGE_CHECK > 0
        'HSerPrint "PID Err "
        'HSerPrint USBPID
        'HSerPrintCRLF
        #ENDIF

    End Select

    USBHasData = True
    TRNIF = 0

  End If

  'Idle
  If IDLEIF Then
    IDLEIF = 0
  End If

  'Stalled
  If STALLIF Then
    STALLIF = 0
  End If

  'Start of frame
  If SOFIF Then
    SOFIF = 0
  End If

End Sub

'''Process incoming SETUP packet
'''@hide
Sub USBProcessSetup
  'Buffer format:
  '0:bmRequestType 1:bRequest 3-2:wValue 5-4:wIndex 7-6:wLength
  USBbmRequestType = USBTempBuffer(0) And 0x7F

  'Re-enable input processing
  PKTDIS = 0

  'Device requests
  If USBbmRequestType = 0 Then
    USBLastControl = USB_NONE
    Select Case USBTempBuffer(1)
      'Case USB_GET_STATUS

      'Case USB_CLEAR_FEATURE

      'Case USB_SET_FEATURE

      Case USB_SET_ADDRESS
        USBLastControl = USB_SET_ADDRESS
        USBNewAddress = USBTempBuffer(2)
        'Send empty DATA1 packet
        USBSendDATA1Ack

      Case USB_GET_DESCRIPTOR
        USBLastControl = USB_GET_DESCRIPTOR
        USBDescType = USBTempBuffer(3)
        USBDescIndex = USBTempBuffer(2)
        USBDescSizeIn = USBTempBuffer(6)
        USBDescStart = 0
        USBSendDescriptor

      'Case USB_SET_DESCRIPTOR

      'Case USB_GET_CONFIGURATION

      Case USB_SET_CONFIGURATION
        USBCurrConfiguration = USBTempBuffer(2)
        If USBCurrConfiguration = 0 Then
          USBState = USB_STATE_ADDRESS
        Else
          USBState = USB_STATE_CONFIGURED
        End If
        USBSendData1Ack

      'Unknown case - send to the User Handler
      Case Else
        #ifdef USB_SETUP_HANDLER
          'Call the user callback
          USB_SETUP_HANDLER
        #endif

    End Select

  Else

    'USBDumpControlIn    'method to assist in debuggin
    #ifdef USB_SETUP_HANDLER

          'clear the pointer.  If the pointer has been incremented then we know what to do!
          'the is the number of buffer bytes to be sent
          USB_IN0_CNT = 0
          'point to the output buffer
          USB_CNT_POINTER = 0

          'Call the user callback
          USB_SETUP_HANDLER

          if USB_IN0_CNT = 0 then
            'No data added to the output buffer - just send an ACK
            USBSendData1Ack

          else
            'Data is thee output buffer - send the data
            USBStartSend

          end if


    #endif

  End If

End Sub

'''Send USB descriptor to host
'''@param USBType Descriptor Type
'''@param USBIndex Starting index to send
'''@param USBSize Requested size of descriptor
'''@hide
Sub USBSendDescriptor
  'Descriptor types:
  '1: Device
  '2: Configuration
  '4: Interface
  '5: Endpoint
  '3: String

  'To send, need to copy into EP0 IN buffer
  'Take ownership
  USB_IN0_STAT.UOWN = 0

  Select Case USBDescType
    'Device
    Case 1
      'Copy bytes into buffer
      ReadTable USB_DEVICE_DESCRIPTOR, 1, USBSize
      If USBSize > USBDescSizeIn Then USBSize = USBDescSizeIn
      If USBDescStart >= USBSize Then
        USBSize = 0
        USBLastControl = USB_NONE
      Else
        USBSize -= USBDescStart
      End If
      For USBCurrByte = 1 To USBSize
        ReadTable USB_DEVICE_DESCRIPTOR, USBCurrByte + USBDescStart, USBTempByte
        Poke USB_IN0_ADDR + (USBCurrByte - 1), USBTempByte
      Next
      USB_IN0_CNT = USBSize
      USBDescStart += USBSize

    'Configuration
    Case 2
      'Copy bytes into buffer
      ReadTable USB_CONFIG_DESCRIPTOR, 0, USBSize
      If USBSize > USBDescSizeIn Then USBSize = USBDescSizeIn
      If USBDescStart >= USBSize Then
        USBSize = 0
        USBLastControl = USB_NONE
      Else
        USBSize -= USBDescStart
      End If
      For USBCurrByte = 1 To USBSize
        ReadTable USB_CONFIG_DESCRIPTOR, USBCurrByte + USBDescStart, USBTempByte
        Poke USB_IN0_ADDR + (USBCurrByte - 1), USBTempByte
      Next
      USB_IN0_CNT = USBSize
      USBDescStart += USBSize

    'Device Qualifier
    Case 6
      'Copy bytes into buffer
      ReadTable USBDeviceQualDescriptor, 0, USBSize
      If USBSize > USBDescSizeIn Then USBSize = USBDescSizeIn
      If USBDescStart >= USBSize Then
        USBSize = 0
        USBLastControl = USB_NONE
      Else
        USBSize -= USBDescStart
      End If
      For USBCurrByte = 1 To USBSize
        ReadTable USBDeviceQualDescriptor, USBCurrByte + USBDescStart, USBTempByte
        Poke USB_IN0_ADDR + (USBCurrByte - 1), USBTempByte
      Next
      USB_IN0_CNT = USBSize
      USBDescStart += USBSize

    'String
    Case 3
      If USBDescIndex = 0 Then
        'Assume that these aren't continued over
        USBLastControl = USB_NONE

        'Language ID
        Poke USB_IN0_ADDR, 4
        Poke USB_IN0_ADDR + 1, 3
        Poke USB_IN0_ADDR + 2, 0x09
        Poke USB_IN0_ADDR + 3, 0x04
        USB_IN0_CNT = 4
      Else
        'Get requested string
        Select Case USBDescIndex
          'Vendor name
          Case 1: USBTempString = USB_VENDOR_NAME
          'Product name
          Case 2: USBTempString = USB_PRODUCT_NAME
          'Other
          Case Else: USBTempString = ""
        End Select

        'Format string into unicode string descriptor
        USB_IN0_CNT = 2 + (USBTempString(0) * 2)

        If USBDescStart > USB_IN0_CNT Then
          USB_IN0_CNT = 0
          USBLastControl = USB_NONE
        Else
          USB_IN0_CNT -= USBDescStart
        End If

        Poke USB_IN0_ADDR, USB_IN0_CNT
        Poke USB_IN0_ADDR + 1, 3
        For USBCurrByte = 1 to USBTempString(0)
          USBTempByte = USBTempString(USBCurrByte)
          Poke USB_IN0_ADDR + (USBCurrByte * 2), USBTempByte
          Poke USB_IN0_ADDR + (USBCurrByte * 2 + 1), 0
        Next
        If USB_IN0_CNT > USBDescSizeIn Then USB_IN0_CNT = USBDescSizeIn
        USBDescStart += USB_IN0_CNT

      End If

    'Interface
    'Case 4

    'Endpoint
    'Case 5

    'Other
    Case Else
      #ifdef USB_DESCRIPTOR_HANDLER
        USB_DESCRIPTOR_HANDLER
      #endif
      Exit Sub
  End Select

  USBStartSend

End Sub

'''Toggle DATA0/DATA1 and start transmit
'''@hide
Macro USBStartSend
  If USB_IN0_STAT.6 Then
    USB_IN0_STAT = b'10001000'
  Else
    USB_IN0_STAT = b'11001000'
  End If
End Macro

'''Send ACK for DATA1
'''@hide
Sub USBSendDATA1Ack
  USB_IN0_CNT = 0
  USB_IN0_STAT = 0xC8
End Sub

'''Send ACK for DATA0
'''@hide
Sub USBSendDATA0Ack
  USB_IN0_CNT = 0
  USB_IN0_STAT = 0x88
End Sub

'Table for device descriptor
Table USBDeviceDescriptor
  18
  1
  0, 2
  USB_DEVICE_CLASS
  USB_DEVICE_SUB_CLASS
  USB_DEVICE_PROTOCOL
  64
  USB_VID And 255
  USB_VID / 256
  USB_PID And 255
  USB_PID / 256
  USB_REV And 255
  USB_REV / 256
  1,2,0
  1
End Table

'Table for device qualifier descriptor
Table USBDeviceQualDescriptor
  10
  6
  0, 2
  USB_DEVICE_CLASS
  USB_DEVICE_SUB_CLASS
  USB_DEVICE_PROTOCOL
  64
  1
  0
End Table

'Table for configuration and interface descriptors (combined)
Table USBConfigDescriptor
  'Descriptor length and type
  9
  2
  'Length of this descriptor and subordinates
  18, 0
  1
  1
  0
  128
  USB_CURRENT_MA / 2

  'Interface descriptor start
  9
  4
  0
  0
  'Number of endpoints, other than 0. Set to 0 (only EP0), fix later
  0

  USB_INTERFACE_CLASS
  USB_INTERFACE_SUBCLASS
  USB_INTERFACE_PROTOCOL
  0
End Table

'''Dump control transfer. Debugging only
'''@hide
Sub USBDumpControlIn
  #IF SCRIPT_USART_USAGE_CHECK > 0
  HSerPrint "PID: 0x"
  HSerPrint Hex([BYTE]USBPID_H)
  HSerPrint Hex([BYTE]USBPID)
  HSerPrint " bmRequestType: "
  HSerPrint Hex(USBTempBuffer(0))
  HSerPrint " bRequest: "
  HSerPrint Hex(USBTempBuffer(1))
  HSerPrint " wValue: "
  HSerPrint Hex(USBTempBuffer(3))
  HSerPrint Hex(USBTempBuffer(2))
  HSerPrint " wIndex: "
  HSerPrint Hex(USBTempBuffer(5))
  HSerPrint Hex(USBTempBuffer(4))
  HSerPrint " wLength: "
  HSerPrint Hex(USBTempBuffer(7))
  HSerPrint Hex(USBTempBuffer(6))
  HSerPrintCRLF
  #ENDIF
End Sub

Macro USBSendAcknowledge
    USBSendData1Ack
End Macro

Sub USBBufferWrite( _USBinValue as byte )

      'Increment the buffer counter
      USB_IN0_CNT++
      Poke USB_IN0_ADDR+USB_CNT_POINTER, _USBinValue

      'Increment the data pointer
      USB_CNT_POINTER++

End sub

Sub USBBufferWrite( _USBinValue as word )

      'Increment the buffer counter
      USB_IN0_CNT++
      Poke USB_IN0_ADDR+USB_CNT_POINTER,    _USBinValue

      'Increment the buffer counter
      USB_IN0_CNT++
      Poke USB_IN0_ADDR+USB_CNT_POINTER+ 1, _USBinValue_H

      'Increment the data pointer
      USB_CNT_POINTER = USB_CNT_POINTER + 2

End sub

Sub USBBufferWrite( _USBinValue as long )

      'Increment the buffer counter
      USB_IN0_CNT++
      Poke USB_IN0_ADDR+USB_CNT_POINTER,    _USBinValue

      'Increment the buffer counter
      USB_IN0_CNT++
      Poke USB_IN0_ADDR+USB_CNT_POINTER+ 1, _USBinValue_H

      'Increment the buffer counter
      USB_IN0_CNT++
      Poke USB_IN0_ADDR+USB_CNT_POINTER+ 2, _USBinValue_U

      'Increment the buffer counter
      USB_IN0_CNT++
      Poke USB_IN0_ADDR+USB_CNT_POINTER+ 3, _USBinValue_E

      'Increment the data pointer
      USB_CNT_POINTER = USB_CNT_POINTER + 4

End sub



'the following is added to ensure the USB is Initialised - DO NOT REMOVE
#ifdef USB_VID
If USB_RAM_SIZE = 0 Then
End If
#endif
