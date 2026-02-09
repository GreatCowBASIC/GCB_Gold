'    Hardware I2C2 routines for GCBASIC
'    Copyright (C) 2015-2026  Evan R. Venn
'    Version 1.1

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

'********************************************************************************
'IMPORTANT:
'THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
'UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
'COMMANDS UNUSABLE!
'********************************************************************************
'    Updated Oct 2016  - for Option Explicit and to fix the script issue
'    Updated jan 2019  - Correct support in HI2C2Start
'    Updated Jan 2020 - Correct SSPxADD calculation for out of bound values
'    Updated May 2020 - Added support for K42 type I2C2 Module
' 14/08/22 Updated user changeable constants only - no functional change

'SPI mode constants ALSO used by hardware I2C:
' Define HI2C2 settings - CHANGE PORTS
'  #define MasterFast 13
'  #define Master 12             ; Used in this module
'  #define MasterSlow 11
'  #define SlaveSS 2
'  #define Slave 1               ; Used in this module
'

'HI2C Mode constants
'#define Slave10 3
'use Slave for 7-bit slave mode and Master for master mode


'Setup
'        ;Master
'        #define HI2C2_DATA PORTC.4
'        #define HI2C2_CLOCK PORTC.3
'        ;I2C pins need to be input for SSP module on a PIC
'        Dir HI2C2_DATA in
'        Dir HI2C2_CLOCK in
'
'        Type
'        HI2C2Mode Master


'      ' SLAVE
'      '  'Buffer for incoming HI2C messages for the slave operation
'        'Each message takes 4 bytes
'        Dim HI22CBuffer(10)
'        HI2C2BufferSize = 0
'        OldHI2C2BufferSize = 0
'        HI2C2Mode Slave
'        HI2C2SetAddress 128
'        'Set up interrupt to process I2C
'        On Interrupt SSP2Ready Call HI2C2HandlerSlave



#startup HI2C2Init, 90

#script

  IF NODEF(HI2C2_BAUD_RATE) THEN
      HI2C2_BAUD_RATE = 100
  END IF 

  'has the defined this constant?  We cannot  use HI2C2_BAUD_RATE as this is always defined
  if hi2c2_DATA then

      HI2C2_BAUD_TEMP = 0

      if int( (ChipMhz * 1000000)/(4000 * HI2C2_BAUD_RATE))-1 > 0 then
          HI2C2_BAUD_TEMP = int((ChipMhz * 1000000)/(4000 * HI2C2_BAUD_RATE)) - 1

      end if
      if int((ChipMhz * 1000000)/(4000 * HI2C2_BAUD_RATE)) < 0 then
          Warning "Clock Frequency to slow for desired I2C2 baud rate"
          HI2C2_BAUD_TEMP = 0
      end if

      if HI2C2_BAUD_TEMP > 255 then
        Warning "Clock Frequency for desired I2C2 baud rate high"
      end if


      If PIC Then


            If Bit(I2C2CON0_EN) Then
                'Redirects to I2C Module for new MSSP aka K42 family
                HI2C2Init =    SI2C2Init
                HI2C2Start =   SI2C2Start
                HI2C2Stop =    SI2C2Stop
                HI2C2ReStart = SI2C2ReStart
                HI2C2Send =    SI2C2Send
                HI2C2Receive = SI2C2Receive
            end if

      end If

  end if

#endscript


Sub HI2C2Mode (In HI2C2CurrentMode)


  #ifdef PIC

      #ifdef var(SSP2CON1)

            set SSP2STAT_SMP on
            set SSP2CON1_CKP on
            set SSP2CON1_WCOL Off

            'Select mode and clock
            If HI2C2CurrentMode = Master Then
              set SSP2CON1_SSPM3 on
              set SSP2CON1_SSPM2 off
              set SSP2CON1_SSPM1 off
              set SSP2CON1_SSPM0 off

              SSP2ADD = HI2C2_BAUD_TEMP and 127
            end if

            if HI2C2CurrentMode = Slave then
              set SSP2CON1_SSPM3 off
              set SSP2CON1_SSPM2 on
              set SSP2CON1_SSPM1 on
              set SSP2CON1_SSPM0 off
            end if

            if HI2C2CurrentMode = Slave10 then
              set SSP2CON1_SSPM3 off
              set SSP2CON1_SSPM2 on
              set SSP2CON1_SSPM1 on
              set SSP2CON1_SSPM0 on
            end if

            'Enable I2C
            set SSP2CON1_SSPEN on

      #endif


  #ENDIF


End Sub

Sub HI2C2SetAddress(In HI2C2Address)
  #ifdef PIC
    'Slave mode only
    If HI2C2CurrentMode <= 10 Then
      SSP2ADD = HI2C2Address
    End If
  #endif
End Sub

Sub HI2C2Start

  'Master mode
  If HI2C2CurrentMode > 10 Then
    #ifdef PIC
      #ifdef BIT(SSP2CON2_SEN)
        Set SSP2CON2_SEN On
        HI2C2WaitMSSP
      #endif

      #ifdef BIT(SEN_SSP2CON2)
        Set SEN_SSP2CON2 On
        HI2C2WaitMSSP
      #endif

    #endif

  'Slave mode
  Else
    #ifdef PIC
      Wait Until SSP2STAT_S = On
    #endif

  End If

End Sub

Sub HI2C2ReStart

  'Master mode
  If HI2C2CurrentMode > 10 Then
    #ifdef PIC
      #ifdef BIT(SSP2CON2_RSEN)
        Set SSP2CON2_RSEN On
        HI2C2WaitMSSP
      #endif
    #endif
  End If

End Sub


Sub HI2C2Stop

  'Master mode
  If HI2C2CurrentMode > 10 Then
    #ifdef PIC

        wait while SSP2STAT_R_NOT_W = 1   'wait for completion of activities
        Set SSP2CON2_PEN On
        HI2C2WaitMSSP

    #endif

  'Slave mode
  Else
    #ifdef PIC
      Wait Until SSP2STAT_P = On
    #endif

  End If

End Sub

Function HI2C2StartOccurred
  'Master mode
  'Always return true
  If HI2C2CurrentMode > 10 Then
    HI2C2StartOccurred = TRUE
    Exit Function

  'Slave mode, check if start condition received last
  Else
    HI2C2StartOccurred = FALSE
    #ifdef PIC

        If SSP2STAT_S = On Then HI2C2StartOccurred = TRUE

    #endif

  End If
End Function

Function HI2C2Stopped
  'Master mode
  'Always return false
  If HI2C2CurrentMode > 10 Then
    HI2C2Stopped = FALSE
    Exit Function

  'Slave mode, check if start condition received last
  Else
    HI2C2Stopped = FALSE
    #ifdef PIC

        If SSP2STAT_P = On Then HI2C2Stopped = TRUE

    #endif

  End If
End Function


Dim HI2C2ACKPOLLSTATE as byte

Sub HI2C2Send(In I2C2Byte)

  #ifdef PIC

    RetryHI2C2Send:
      'Clear WCOL
      SET SSP2CON1_WCOL OFF
      'Load data to send
      SSP2BUF = I2C2Byte
      HI2C2WaitMSSP

      if SSP2CON2_ACKSTAT =  1 then
        HI2C2AckPollState = true
      else
        HI2C2AckPollState = false
      end if

    If SSP2CON1_WCOL  = On Then
      If HI2C2CurrentMode <= 10 Then Goto RetryHI2C2Send
    End If

    'Release clock (only needed by slave)
    If HI2C2CurrentMode <= 10 Then Set SSP2CON1_CKP On

  #endif

End Sub

Sub HI2C2Receive (Out HI2C2Byte, Optional In HI2CGetAck = 1 )

  #ifdef PIC

    'Enable receive

      'Master mode
      If HI2C2CurrentMode > 10 Then
        if HI2CGetAck.0 = 1 then
           ' Acknowledge
            SSP2CON2_ACKDT = 0
        else
            ' Not Acknowledge
            SSP2CON2_ACKDT = 1
        end if
        SSP2CON2_RCEN = 1
      'Slave mode
      Else
        SET SSP2STAT_R_NOT_W ON
      End If


    'Clear Collisions
    SET SSP2CON1_WCOL OFF
    SET SSP2CON1_SSPOV Off

    'Wait for receive
    Wait Until SSP2STAT_BF = 1

    HI2C2Byte = SSP2BUF
    SSP2IF = 0
    SSP2CON2_ACKEN = 1
    ' Clear flag - this is required
    SSP2STAT_BF = 0
    HI2C2WaitMSSP

    'Disable receive (master mode)

      'Master mode
      If HI2C2CurrentMode > 10 Then
        Set SSP2CON2_RCEN Off
      'Slave mode
      Else
        SET SSP2STAT_R_NOT_W Off
      End If

  #endif

End Sub

; This routine waits for the last I2C operation to complete.
; It does this by polling the SSPIF flag in PIR1.
; Then, it clears SSPIF
Dim HI2C2WaitMSSPTimeout as byte
sub HI2C2WaitMSSP

    ' adapted for 18f - v0.94

    HI2C2WaitMSSPTimeout = 0
    HI2C2WaitMSSPWait:
    HI2C2WaitMSSPTimeout++
    if HI2C2WaitMSSPTimeout < 255 then
        #ifdef bit(SSP2IF)
            ''Support for SSP2IF
            if SSP2IF = 0 then goto HI2C2WaitMSSPWait
            SSP2IF = 0
        #endif

        #ifndef  bit(SSP2IF)
          ' no int flag so wait a while and exit
          wait 1 us
        #endif
    end if

end sub

sub HI2C2Init
    HI2C2CurrentMode = 0

     'Initialise the I2C module
    #ifdef var(I2C2CON0)
        SI2C2Init
    #endif


end sub


Sub SI2C2Init
    asm showdebug  This method sets the MSSP modules for K42s family chips

    Dir HI2C2_DATA out
    Dir HI2C2_CLOCK out

    I2C2CON1 = I2C2CON1Default
    I2C2CON2 = I2C2CON2Default
    I2C2CLK =  I2C2CLOCKSOURCE
    I2C2CON0 = I2C2CON0Default

    I2C2PIR = 0    ;Clear all the error flags
    I2C2ERR = 0
    I2C2CON0.EN=1

    'Commence I2C protocol
    I2C2CON2.ACNT = 0
    I2C2CON2.ABD=0
    I2C2CON0.MDR=1

    'Initialise correct state of I2C2 module. Not sure why this is needed but it is. Microchip failed to explain why this is required. But, it is.
    SI2C2Start
    SI2C2Send ( 0xff )
    SI2C2Stop

    HI2C2CurrentMode = 0

End sub

'New I2C2 module support


    Dim HI2C2StateMachine as byte
    Dim HI2C2ACKPOLLSTATE  as Byte
    Dim HI2C2lastError as Byte

    #DEFINE I2C2_GOOD             0
    #DEFINE I2C2_FAIL_TIMEOUT     1
    #DEFINE I2C2_TXBE_TIMEOUT     2
    #DEFINE I2C2_START_TIMEOUT    4
    #DEFINE I2C2_RESTART_TIMEOUT  8
    #DEFINE I2C2_RXBF_TIMEOUT     16
    #DEFINE I2C2_ACK_TIMEOUT      32
    #DEFINE I2C2_MDR_TIMEOUT      64
    #DEFINE I2C2_STOP_TIMEOUT     128

    #DEFINE I2C2CLOCK_SMT1           0X09
    #DEFINE I2C2CLOCK_TIMER6PSO      0X08
    #DEFINE I2C2CLOCK_TIMER4PSO      0X07
    #DEFINE I2C2CLOCK_TIMER2PSO      0X06
    #DEFINE I2C2CLOCK_TIMER0OVERFLOW 0X05
    #DEFINE I2C2CLOCK_REFERENCEOUT   0X04
    #DEFINE I2C2CLOCK_MFINTOSC       0X03
    #DEFINE I2C2CLOCK_HFINTOSC       0X02
    #DEFINE I2C2CLOCK_FOSC           0X01
    #DEFINE I2C2CLOCK_FOSC4          0X00

    #DEFINE I2C2CLOCKSOURCE  I2C2CLOCK_MFINTOSC


    #define I2C2CON0Default     0x04
    #define I2C2CON1Default     0x80
    #define I2C2CON2Default     0x21

    #define HI2C2ITSCLWaitPeriod 70    'minimum of twenty plus 5 clock ticks


Sub SI2C2Start
  asm showdebug  Redirected for K42 family probalby called HI2C2Start
  asm showdebug  This method sets the registers and register bits to generate the I2C  START signal. Master_mode only.

        HI2C2StateMachine = 1
        HI2C2WaitMSSPTimeout = false
        'Clear the error state variable
        HI2C2lastError = I2C2_GOOD

End Sub

Sub SI2C2ReStart
  asm showdebug  Redirected for K42 family probalby called HI2C2ReStart
  asm showdebug  This method sets the registers and register bits to generate the I2C  RESTART signal. Master_mode only.
        HI2C2StateMachine = 3
        HI2C2WaitMSSPTimeout = false

End Sub

Sub SI2C2Stop
  asm showdebug  Redirected for K42 family probalby called HI2C2Stop
  asm showdebug  This method sets the registers and register bits to generate the I2C  STOP signal

    'Waits up to 254us then set the error state
    HI2C2StateMachine = 0
    HI2C2WaitMSSPTimeout = 0

    do while HI2C2WaitMSSPTimeout < 255

        HI2C2WaitMSSPTimeout++

        'Wait till this event
        if I2C2PIR.PCIF = 1 then
            SI2C2wait4Stop
            'TSCL wait, part of the specificiation of the I2C Module
            wait HI2C2ITSCLWaitPeriod us
            exit sub

        else
            wait 1 us

        end if
    loop
    if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_STOP_TIMEOUT

End Sub


Sub SI2C2Send ( in I2C2Byte )
    asm showdebug  Redirected for K42 family probalby called HI2C2Send
    asm showdebug  This method sets the registers and register bits to send I2C data

    'This is now a state Machine to cater for the new approach with the I2C module
    Select Case HI2C2StateMachine


      case 2  'send data

        HI2C2WaitMSSPTimeout = 0
        'waits up to 254us then creates error message
        do while HI2C2WaitMSSPTimeout < 255

            HI2C2WaitMSSPTimeout++
            'Wait for this event
            if I2C2STAT1.TXBE = 1 then

                'Set the byte count to 1, place outbyte in register, and wait for hardware state machine
                I2C2CNT = 1
                I2C2TXB = I2C2Byte
                SI2C2Wait4Ack
                SI2C2wait4MDR

                exit Sub

            else

                wait 1 us

            end if
        loop

        if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_TXBE_TIMEOUT


      case 1  'A start

        'Clear the output buffers, set byte count to zero, clear event and place Slave Address in register
        I2C2STAT1.CLRBF = 1
        I2C2CNT = 0
        I2C2PIR.SCIF = 0
        I2C2ADB1 = I2C2Byte

        HI2C2WaitMSSPTimeout = 0
        'waits up to 254us then creates error message
        do while HI2C2WaitMSSPTimeout < 255

            HI2C2WaitMSSPTimeout++
            'Wait for this event
            if I2C2PIR.SCIF = 1 then
                HI2C2StateMachine = 2  'Set state Machine to send data
                HI2C2AckPollState = I2C2CON1.5
                exit Sub

            else
                'Set to start
                I2C2CON0.S = 1
                wait 1 us

            end if
        loop

        if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_START_TIMEOUT

      case 3  'A restart

        'wait for hardware machine to settle
        wait while I2C2STAT0.MMA = 1

        'set count to zero, place Slave address in register and clear event
        I2C2CNT = 0
        I2C2ADB1 = I2C2Byte
        I2C2PIR.SCIF = 0


        HI2C2WaitMSSPTimeout = 0
        'waits up to 254us then creates error message
        do while HI2C2WaitMSSPTimeout < 255

            HI2C2WaitMSSPTimeout++
            'Wait for this event
            if I2C2PIR.SCIF = 1 then
                HI2C2StateMachine = 2  'Set state Machine to send data
                HI2C2AckPollState = I2C2CON1.5
                exit Sub

            else
                'Set as Start and Clear Restart
                I2C2CON0.S = 1
                I2C2CON0.RSEN=0
                wait 1 us

            end if
        loop

        if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_RESTART_TIMEOUT

    end select

End Sub


Sub SI2C2Receive (Out I2C2Byte, Optional In HI2C2GetAck = 1 )
  asm showdebug  Redirected for K42 family probalby called HI2C2Receive
  asm showdebug  This method sets the registers and register bits to get I2C data
      I2C2CNT = 1
      HI2C2WaitMSSPTimeout = 0

      'waits up to 254us then creates error message
      do while HI2C2WaitMSSPTimeout < 255

          HI2C2WaitMSSPTimeout++
          'Wait for this event
          if I2C2STAT1.RXBF = 1 then
              'Read input buffer to output byte
              I2C2Byte = I2C2RXB
              exit Sub

          else

              wait 1 us

          end if
      loop

      if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_RXBF_TIMEOUT



End Sub

Sub SI2C2Wait4Ack

    HI2C2WaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2C2WaitMSSPTimeout < 255

        HI2C2WaitMSSPTimeout++
        'Wait for this event
        if I2C2CON1.ACKSTAT = 0 then
            'Set status
            HI2C2AckPollState = I2C2CON1.ACKSTAT
            exit sub

        else

            wait 1 us

        end if
    loop
    if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_ACK_TIMEOUT
    ' HI2C2WaitMSSPTimeout is now TRUE = 255
End Sub



Sub SI2C2wait4MDR

    HI2C2WaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2C2WaitMSSPTimeout < 255

        HI2C2WaitMSSPTimeout++
        'wait for this event
        if I2C2CON0.MDR = 0 then

            exit sub

        else

            wait 1 us

        end if
    loop
    if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_MDR_TIMEOUT
    ' HI2C2WaitMSSPTimeout is now TRUE = 255
End Sub


Sub SI2C2wait4Stop

    HI2C2WaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2C2WaitMSSPTimeout < 255

        HI2C2WaitMSSPTimeout++
        'Wait for this event
        if I2C2PIR.PCIF = 1 then

            exit sub

        else

            wait 1 us

        end if
    loop
    if HI2C2WaitMSSPTimeout = 255 then HI2C2lastError = HI2C2lastError or I2C2_STOP_TIMEOUT
    ' HI2C2WaitMSSPTimeout is now TRUE = 255
End Sub


sub SI2C2Discovery ( address )

    HI2C2CON1 = HI2C2CON1Default
    I2C2CON2 = I2C2CON2Default
    I2C2CLK =  I2C2CLOCKSOURCE
    I2C2CON0 = I2C2CON0Default

    I2C2PIR = 0    ;Clear all the error flags

    I2C2CON0.EN=1

    'Commence I2C protocol
    I2C2CON2.ACNT = 0
    I2C2CON2.ABD=0
    I2C2CON0.MDR=1
    I2C2CON1.7=0

    I2C2ADB1= address
    I2C2CON0.S=1

    wait while I2C2STAT1.TXBE <> 1

    'Set the byte count to 1, place outbyte in register, and wait for hardware state machine
    I2C2CNT = 1
    I2C2TXB = 0 'reg
    I2C2CNT = 1
    I2C2TXB = 0 'data
    SI2C2Stop
    HI2C2AckpollState = 0
    HI2C2AckpollState.0 = !I2C2PIR.7
    'Reset module
    I2C2CON0.EN=0
    wait 1 ms
    I2C2CON0.EN=1

end sub
