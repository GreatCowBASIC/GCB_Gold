'    Hardware I2C routines for GCBASIC
  '    Copyright (C) 2010-2024 Hugh Considine, Jacques Erdemaal and Evan R. Venn

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


  '    Updated Feb 2015 by Jacques Erdemaal to improve (to remove the guess work) from the configuration for AVR
  '            and to improve the initialisation of the AVR
  '
  '    Updated Feb 2015 to support AVR and correct HI2CReceive parameter error.
  '    Moved defines ACK/NACK to sysen.ini to
  '
  '    Updated May 2015 - enhance hi2cwaitmssp
  '    Updated Oct 2015 - enhance hi2cwaitmssp
  '    Updated Jan 2016 - enhance hi2cwaitmssp
  '    Updated May 2016 - resolved AVR TWINT lockup issues
  '    Updated Sept 2016 - resolve 16f18855 register mapping
  '    Updated Oct 2016  - for Option Explicit and to fix the script issue
  '    Updated Oct 2016  - ... Slave10 was NOT defined.
  '    Updated Dec 2016  - ... Added SSPIF = SSP1IF to correct error
  '    Updated Feb 2017  - Added AVRDisable_HI2CAckPollState for AVR performance
  '    Updated May 2017 -  Added support for PIC chips with bit "SEN_SSP1CON2"
  '    Updated Sep 2017 -  Added SAMEVAR and optimised HSerReceive
  '    Updated Oct 2017 - Added MASTER for I2C module. No slave.
  '    Updated Oct 2017 - Updated to add SI2C discovery support methods.
  '    Updated Jan 2018 - Updated to handle AVR frequency and I2C baud rate. Warning added
  '    Updated Jan 2018 - Updated to handle AVR I2C message handling
  '    Updated Aug 2019 - Updated documentation only. No functional changes
  '    Updated Jan 2020 - Correct SSPxADD calculation for out of bound values
  '    Updated Apr 2020 - Corrected Si2CReceive.  Was set to 255 bytes, not 1!
  '    Updated Apr 2020 - Updated to SI32CDisovery. Now using I2C1PIR.7 to detect part and renamed HIC2Init to HI2CInit
  '    Updated May 2020 - Removed Unused Constant
  '    Updated Dec 2020 - Added support for 18FxxQ10
  '    Updated Jan 2021 - Added support for 18FxxQ43
  '    Updated Sep 2021 - Revised SI2CDiscovery adding #IFDEF var(I2C1CNTL) and #IFDEF var(I2C1CNT) to isolate Q43 that only supports I2C1CNT
  '            14/08/22 - Updated user changeable constants only - no functional change
  '            28/02/23 - Added support fort 18FxxQ71 and resolved constant isolation
  '    Updated Jun 2023 - Revised to make I2C1I2C1CONxDefaults optional
  '    Updated 30/09/24 - Added AVRDx support
  '    Updated 10/10/24 - Isolation of the Byte variables
  '    Updated 29/10/24 - Add CHIPSUBFAMILY = ChipFamily18FxxQ20
  '    Updated 01/12/24 - Add CHIPSUBFAMILY = ChipFamily18FxxQ24
  

'User changeable constants


'End of user changeable constants

'SPI mode constants also SHARED by hardware I2C:
  '  #define MasterFast 13
  '  #define Master 12             ; Used in this module
  '  #define MasterSlow 11
  '  #define SlaveSS 2
  '  #define Slave 1               ; Used in this module
  '

'HI2C Mode constants
#define Slave10 3
'use Slave for 7-bit slave mode and Master for Master_mode


'Setup
  '        ;Master and AVR
  '        #define I2C_DATA PORTC.4
  '        #define I2C_CLOCK PORTC.3
  '        ;I2C pins need to be input for SSP module on a PIC
  '        Dir HI2C_DATA in
  '        Dir HI2C_CLOCK in
  '        ;Type
  '        HI2CMode Master


  '      ' SLAVE
  '      '  'Buffer for incoming HI2C messages for the slave operation
  '        'Each message takes 4 bytes
  '        Dim HI2CBuffer(10)
  '        HI2CBufferSize = 0
  '        OldHI2CBufferSize = 0
  '        HI2CMode Slave
  '        HI2CSetAddress 128
  '        'Set up interrupt to process I2C
  '        On Interrupt SSP1Ready Call HI2CHandlerSlave




  ' Move to HWI2C.h when completed
  #define AVR_I2C_START 0x08
  #define AVR_I2C_ReSTART 0x10

  #define MT_SLA_ACK 0x18
  #define MT_SLA_NACK_REC 0x20
  #define MT_DATA_ACK 0x28
  #define MT_DATA_NACK_REC 0x30
  #define ARB_LOST 0x38

  #define MR_SLA_ACK 0x40
  #define MR_SLA_NACK_REC 0x48
  #define MR_DATA_ACK 0x50
  #define MR_DATA_NACK_REC 0x58

  #samebit R_NOT_W,R_NW1

#startup HI2CInit, 90

#script

  IF NODEF(HI2C_BAUD_RATE) THEN
    // Set default HI2C_BAUD_RATE
    HI2C_BAUD_RATE = 100
  END IF 

  if HI2C_DATA  then

      HI2C_BAUD_TEMP = 0


      If PIC Then

            if int((ChipMhz * 1000000)/(4000 * HI2C_BAUD_RATE))-1 > 0 then
                HI2C_BAUD_TEMP = int((ChipMhz * 1000000)/(4000 * HI2C_BAUD_RATE)) - 1
            end if
            if int((ChipMhz * 1000000)/(4000 * HI2C_BAUD_RATE)) = 0 then
                Warning "Clock Frequency to slow for desired I2C baud rate"
                HI2C_BAUD_TEMP = 0
            end if

            if HI2C_BAUD_TEMP > 255 then
              Warning "Clock Frequency for desired I2C baud rate high"
            end if

            If Bit(I2C1CON0_EN) Then
                'Redirects to I2C Module for new MSSP aka K-Mode family
                HI2CInit =    SI2CInit
                HI2CStart =   SI2CStart
                HI2CStop =    SI2CStop
                HI2CReStart = SI2CReStart
                HI2CSend =    SI2CSend
                HI2CReceive = SI2CReceive
            end if

            If ChipFamily18FxxQ24 = ChipSubFamily or ChipFamily18FxxQ20 = ChipSubFamily Then
              If NODEF ( DEFAULT_HIC2Q2XBUFFERSIZE ) Then
                DEFAULT_HIC2Q2XBUFFERSIZE = 128
              End If
            End if

            If ChipFamily18FxxQ20 = ChipSubFamily Then
                HI2CStop =    HI2CQ20Stop
                HI2CSend =    HI2CQ20Send
                // Set buffer size to SCRIPT_HIC2Q2XBUFFERSIZE
                If NODEF( HIC2Q2XBUFFERSIZE ) Then
                  SCRIPT_HIC2Q2XBUFFERSIZE = DEFAULT_HIC2Q2XBUFFERSIZE
                End If
                If DEF( HIC2Q2XBUFFERSIZE ) Then
                  SCRIPT_HIC2Q2XBUFFERSIZE = HIC2Q2XBUFFERSIZE
                End If
            End If

            If ChipFamily18FxxQ24 = ChipSubFamily Then
                HI2CStart =   SI2CStart
                HI2CStop =    HI2CQ24Stop
                HI2CSend =    HI2CQ24Send
                SI2CInit =    HI2CQ24Init
                // Set buffer size to SCRIPT_HIC2Q2XBUFFERSIZE
                If NODEF( HIC2Q2XBUFFERSIZE ) Then
                  SCRIPT_HIC2Q2XBUFFERSIZE = DEFAULT_HIC2Q2XBUFFERSIZE
                End If
                If DEF( HIC2Q2XBUFFERSIZE ) Then
                  SCRIPT_HIC2Q2XBUFFERSIZE = HIC2Q2XBUFFERSIZE
                End If
            End If

            If ChipFamily18FxxQ24 = ChipSubFamily or ChipFamily18FxxQ20 = ChipSubFamily Then
              If NODEF ( MUTE_HIC2Q2XBUFFERSIZE_WARNING ) Then
                Warning "I2C Buffer Size set to " SCRIPT_HIC2Q2XBUFFERSIZE " bytes"
                Warning " Use '#DEFINE MUTE_HIC2Q2XBUFFERSIZE_WARNING' to mute this message"
              End If
            End If

            iF var( I2C1CNTL ) THEN
                'for the 18FxxQ10's
                I2C1CNT = I2C1CNTL
            end if

      end If

    end if

    IF AVR then
      IF NODEF( CHIPAVRDX ) then
        'Redirects to legacy AVR CODE
        HI2CMode = AVRHI2CMode
        HI2CStart = AVRHI2CStart
        HI2CStop = AVRHI2CStop
        HI2CReStart = AVRHI2CReStart
        HI2CSend = AVRHI2CSend
        HI2CReceive = AVRHI2CReceive


        TWIMode = AVRHI2CMode
        TWIStart = AVRHI2CStart
        TWIStop = AVRHI2CStop
        TWICReStart = AVRHI2CReStart
        TWISend = AVRHI2CSend
        TWIReceive = AVRHI2CReceive
        TWIByte = I2CByte
        TWIAckPollState = HI2CAckPollState
        TWIStartOccurred = HI2CStartOccurred
      End If

      IF DEF( CHIPAVRDX ) then

        IF NODEF(TWI_SEND_DELAY) Then
          // Set default delay
          TWI_SEND_DELAY = 0
          IF HI2C_BAUD_RATE  < 101 Then
            TWI_SEND_DELAY = 10
          End If

        END IF

        // Redirects to AVRDx CODE
        HI2CMode = AVRDxTWI0Mode
        HI2CStart = AVRDxTWI0Start
        HI2CStop = AVRDxTWI0Stop
        HI2CReStart = AVRDxTWI0ReStart
        HI2CSend = AVRDxTWI0Send
        HI2CReceive = AVRDxTWI0Receive

        TWIMode = AVRDxTWI0Mode
        TWIStart = AVRDxTWI0Start
        TWIStop = AVRDxTWI0Stop
        TWICReStart = AVRDxTWI0ReStart
        TWISend = AVRDxTWI0Send
        TWIReceive = AVRDxTWI0Receive
        TWIByte = I2CByte
        TWIAckPollState = HI2CAckPollState
        TWIStartOccurred = HI2CStartOccurred

        SCRIPT_TWI_BAUD = INT(INT((ChipMhz * 1000000 ) /  (HI2C_BAUD_RATE * 1000)-10)/2)
        SCRIPT_TWI_FAST_MODE = 0
        
        // IF SCRIPT_TWI_BAUD < 64 then 
        //   // Recalc with fast mode
        //     warning "Switch to fast mode"
        //       SCRIPT_TWI_BAUD = INT(INT((ChipMhz * 1000000 ) /  (HI2C_BAUD_RATE * 500)-10)/2)
        //       warning SCRIPT_TWI_BAUD
        //       SCRIPT_TWI_BAUD = 95
        //       SCRIPT_TWI_FAST_MODE = 28
        // End IF

        IF DEF(HI2C_DATA) THEN
          // Only issue error when used!
          IF INT(SCRIPT_TWI_BAUD) < 0 Then
            Error "TWI_BAUD calculation is invalid = " SCRIPT_TWI_BAUD
            Error "HWI2C.H AVRDx script section has negative calculation - contact us on the GCBASIC Forum.  We can resolve very quickly."
          END IF
        END IF
      END IF

      IF NODEF(CHIPAVRDX) Then

         ' SCL CLOCK
         ' ---------
         ' TWBR= (CPU_CLK - SCL_BD * 16)/2 * SCL_BD * PRESC       (328P  PDF 21.5.2)
         CPU_Clk = ChipMhz * 1e6
         BD_RATE=1000 * HI2C_BAUD_RATE

         CST_TWBR = Int((CPU_Clk - (16*BD_RATE))/(2*BD_RATE*1))
         If CST_TWBR <= 255 Then
             CST_PRESCALER = 0
             IF CST_TWBR < 0 Then
                CST_TWBR = 0
                BD_RATECALC = int((1000 * ChipMHz )/16+((CST_TWBR*2)*4) )

                'Ensure this warning only happens when HI2C is used.
                if HI2C_CLOCK <> "HI2C_CLOCK" then
                    warning "Baud Rate below specificiation at" BD_RATECALC "KHz - lower baud rate or increase clock frequency to remove this warning."
                end if
             End if
         End If

         If CST_TWBR > 255 Then
             CST_TWBR = Int((CPU_Clk - (16*BD_RATE))/(2*BD_RATE*4))
             If CST_TWBR <= 255 Then
                CST_PRESCALER = 1
             End If
             If CST_TWBR > 255 Then
                CST_TWBR = Int((CPU_Clk - (16*BD_RATE))/(2*BD_RATE*16))
                If CST_TWBE <= 255 Then
                   CST_PRESCALER = 2
                End If
                If CST_TWBR > 255 Then
                   CST_TWBR = Int((CPU_Clk - (16*BD_RATE))/(2*BD_RATE*64))
                   If CST_TWBR <=255 Then
                      CST_PRESCALER = 3
                   End If
                   If CST_TWBR > 255 Then
                       Compiler Will Show error
                      Error "Error Computing TWI-I2C BAUD RATE PARAMETERS"
                      Error "    CPU FREQ    = " ChipMhz " Mhz"
                      Error "    I2C BAUD RATE " HI2C_BAUD_RATE " kHz"
                   End If
                End If
             End If
         End If
        ' Uncommented Displays Results In GCB Output Window
         ' warning " CST_PRESCALER = "  CST_PRESCALER  "    CST_TWBR = "  CST_TWBR
        END IF

      END IF
    
    END IF  // end AVR

   if novar(SSPCON1) then

      if var(SSP1CON1) then
          SSPCON1 = SSP1CON1
          SSPSTAT = SSP1STAT
          SSPBUF  = SSP1BUF
          SSPCON2 = SSP1CON2
          SSPADD  = SSP1ADD
          SSPIF = SSP1IF
      end if

   end if


   if novar(SSPCON1) then
      if var(SSPCON) then
          SSPCON1 = SSPCON
      end if
   end if

#endscript

Sub HI2CMode (In HI2CCurrentMode)
  asm showdebug  This method sets the variable `HI2CCurrentMode`, and, if required, sets the SSPCON1.bits
  #samebit SSPIF, SSP1IF
  #samebit SSPIF, SSP1IF

  #ifdef PIC


    #ifdef var(SSPCON1)
        '#ifdef var(SSPCON1) added to separate from newer i2C module which does not have an SSPCON1
        set SSPSTAT.SMP on
        set SSPCON1.CKP on
        set SSPCON1.WCOL Off

        'Select mode and clock
        If HI2CCurrentMode = Master Then
          set SSPCON1.SSPM3 on
          set SSPCON1.SSPM2 off
          set SSPCON1.SSPM1 off
          set SSPCON1.SSPM0 off

          SSPADD = HI2C_BAUD_TEMP and 127
        end if

        if HI2CCurrentMode = Slave then
          set SSPCON1.SSPM3 off
          set SSPCON1.SSPM2 on
          set SSPCON1.SSPM1 on
          set SSPCON1.SSPM0 off
        end if

        if HI2CCurrentMode = Slave10 then
          set SSPCON1.SSPM3 off
          set SSPCON1.SSPM2 on
          set SSPCON1.SSPM1 on
          set SSPCON1.SSPM0 on
        end if

        'Enable I2C
        set SSPCON1.SSPEN on
    #endif
  #ENDIF

End Sub

Sub HI2CSetAddress(In I2CAddress)
 asm showdebug  This method sets the `SSPADD` register to the variable `I2CAddress` when slave mode only
  #ifdef PIC
    'Slave mode only
    If HI2CCurrentMode <= 10 Then
       SSPADD = I2CAddress
    End If
  #endif
End Sub

Sub HI2CStart
  asm showdebug  This method sets the registers and register bits to generate the I2C  START signal

  If HI2CCurrentMode > 10 Then
    'Master_mode operational
    #ifdef PIC

      #ifdef bit(SSP1CON2_SEN)
          Set SSP1CON2_SEN On
          HI2CWaitMSSP      
      #else
        #ifdef bit(SEN)
            Set SEN On
            HI2CWaitMSSP
        #endif

        #ifdef bit(SEN_SSP1CON2)
          ' for device with SEN_SSP1CON2 and therefore devices including the 18FxxK40 
            Set SEN_SSP1CON2 On
            HI2CWaitMSSP
        #endif

      #endif

    #endif

  Else
    'Slave mode operational
    #ifdef PIC
      Wait Until SSPSTAT.S = On
    #endif

  End If

End Sub

Sub HI2CReStart
  asm showdebug  This method sets the registers and register bits to generate the I2C  RESTART signal
  If HI2CCurrentMode > 10 Then
    'Master_mode operational
    #ifdef PIC
      #ifdef BIT(RSEN)
            Set RSEN On
            HI2CWaitMSSP
      #endif
    #endif
  End If

End Sub

Sub HI2CStop

  'Master_mode
  If HI2CCurrentMode > 10 Then
    #ifdef PIC
      #ifdef BIT(PEN)
           ' set SSPIE OFF; disable SSP interrupt, tested by Anobium but not implemented.
           wait while R_NOT_W = 1   'wait for completion of activities
           Set SSPCON2.PEN On
           Set SSPCON2.PEN On
           HI2CWaitMSSP
      #endif
    #endif

  'Slave mode
  Else
    #ifdef PIC
      Wait Until SSPSTAT.P = On
    #endif

  End If

End Sub

Function HI2CStartOccurred

  'Should return true
  If HI2CCurrentMode > 10 Then
    'Master_mode operational
    HI2CStartOccurred = TRUE
    Exit Function


  Else
  'Slave mode, check if start condition received last
    HI2CStartOccurred = FALSE
    #ifdef PIC
      #ifdef BIT(S)
        If SSPSTAT.S = On Then HI2CStartOccurred = TRUE
      #endif
    #endif

  End If
End Function

Function HI2CStopped

  'Should return false
  If HI2CCurrentMode > 10 Then
    'Master_mode operational
    HI2CStopped = FALSE
    Exit Function
  Else
  'Slave mode, check if start condition received last
    HI2CStopped = FALSE
    #ifdef PIC
      #ifdef BIT(P)
        If SSPSTAT.P = On Then HI2CStopped = TRUE
      #endif
    #endif

  End If
End Function

Dim HI2CACKPOLLSTATE  as Byte

Sub HI2CSend(In I2CByte)
  asm showdebug  This method sets the registers and register bits to send I2C data

  #ifdef PIC

    RetryHI2CSend:
      'Clear WCOL
      SET SSPCON1.WCOL OFF
      'Load data to send
      SSPBUF = I2CByte
      HI2CWaitMSSP

      if ACKSTAT =  1 then
             HI2CAckPollState = true
          else
             HI2CAckPollState = false
          end if

    If SSPCON1.WCOL = On Then
      If HI2CCurrentMode <= 10 Then Goto RetryHI2CSend
    End If

    'Release clock (only needed by slave)
    If HI2CCurrentMode <= 10 Then Set SSPCON1.CKP On

  #endif

End Sub

Sub HI2CReceive (Out I2CByte, Optional In HI2CGetAck = 1 )

 #ifdef PIC

   'Enable receive

     'Master_mode
     If HI2CCurrentMode > 10 Then
       if HI2CGetAck.0 = 1 then
          ' Acknowledge
           ACKDT = 0
       else
           ' Not Acknowledge
           ACKDT = 1
       end if
       RCEN = 1
     'Slave mode
     Else
       SET SSPSTAT.R_NOT_W ON
     End If


   'Clear Collisions
   SET SSPCON1.WCOL OFF
   SET SSPCON1.SSPOV Off



   'Wait for receive
   Wait Until SSPSTAT.BF = 1 AND SSPIF = 1

   I2CByte = SSPBUF
   SSPIF = 0''Support for SSPIF

   ACKEN = 1; Send ACK DATA now. ' bsf SSPCON2,ACKEN
   ' Clear flag - this is required
   SSPSTAT.BF = 0
   HI2CWaitMSSP

   'Disable receive (Master_mode)

   'Master_mode
   If HI2CCurrentMode > 10 Then
     Set SSPCON2.RCEN Off
   'Slave mode
   Else
     SET SSPSTAT.R_NOT_W Off
   End If

 #endif

End Sub

  ; This routine waits for the last I2C operation to complete.
  ; It does this by polling the SSPIF flag in PIR1.
  ; Then, it clears SSPIF
  ; Updated at v0.95.010 Option Explicit
  Dim HI2CWaitMSSPTimeout as byte
sub HI2CWaitMSSP


    HI2CWaitMSSPTimeout = 0
    HI2CWaitMSSPWait:
    HI2CWaitMSSPTimeout++
    if HI2CWaitMSSPTimeout < 255 then
        #ifdef bit(SSP1IF)
            ''Support for SSP1IF
            if SSP1IF = 0 then 
                #ifdef ChipFamily18FxxK40 16104
                  wait 2 us
                #endif
              goto HI2CWaitMSSPWait
            end if
            // Clear the flag and exit sub with HI2CWaitMSSPTimeout NOT set to 255
            SSP1IF = 0

        #else

            #ifdef bit(SSPIF)
                ''Support for SSPIF
                if SSPIF = 0 then goto HI2CWaitMSSPWait
                // Clear the flag and exit sub with HI2CWaitMSSPTimeout NOT set to 255
                SSPIF = 0
            #endif

            ' no int flag so wait a while and exit
                wait 1 us
                
        #endif

    end if
    #ifdef bit(BCL1IF)
      // Clear the flag
      if HI2CWaitMSSPTimeout = 255 Then BCL1IF = 0
    #endif
end sub



Sub AVRHI2CMode ( In HI2CCurrentMode )

          #ifdef AVR
                 If HI2CCurrentMode = Master Then

                    TWSR = CST_PRESCALER
                    TWBR = CST_TWBR

                 end if

                 if HI2CCurrentMode = Slave then
                 '  [tbd]
                 end if
                 ldi  SysValueCopy, 0|(1<<TWEN)
                 sts  TWCR,SysValueCopy

                 HI2CWaitMSSPTimeout = 0
            #endif


End Sub

Sub AVRHI2CStart


    if HI2CCurrentMode = 0 then
       'assume MASTER
       HI2CMode Master
    end if

    lds SysValueCopy,TWCR
    sbr   SysValueCopy, (1<<TWINT)|(1<<TWSTA)| (1<<TWEN)
    sts TWCR,SysValueCopy

    do while TWINT = 0
    loop

    I2CByte = TWSR & 0xF8

    if I2CByte = AVR_I2C_START then
       HI2CStartOccurred = true
    else
       HI2CStartOccurred = false
    end if

End sub

Sub AVRHI2CReStart

    lds SysValueCopy,TWCR
    sbr   SysValueCopy, (1<<TWINT)|(1<<TWSTA)| (1<<TWEN)
    sts TWCR,SysValueCopy

    do while TWINT = 0
    loop


    I2CByte = TWSR & 0xF8

    ' extended code to ensure compiler gets the correctly.
    if I2CByte = AVR_I2C_START then
       HI2CStartOccurred = true
       goto exitAVRHI2CReStart
    else
       HI2CStartOccurred = false
    end if


    if I2CByte = AVR_I2C_ReSTART then
       HI2CStartOccurred = true
       goto exitAVRHI2CReStart
    else
       HI2CStartOccurred = false
    end if

exitAVRHI2CReStart:
End sub


Sub AVRHI2CStop

    lds SysValueCopy,TWCR
    sbr   SysValueCopy, (1<<TWINT)|(1<<TWEN)|(1<<TWSTO)
    sts TWCR,SysValueCopy

End Sub


Sub AVRHI2CSend ( In I2CByte )

        lds       SysValueCopy,I2CBYTE
        sts       TWDR,SysValueCopy
        ldi       SysValueCopy, (1<<TWINT) | (1<<TWEN)
        sts       TWCR, SysValueCopy

        nop
        nop
        do while TWINT = 0
        loop


        'Destructive use of I2CByte to save memory
        I2CByte = TWSR & 0xF8

        HI2CAckPollState = true

        #ifndef AVRDisable_HI2CAckPollState
            Select case I2CByte          'Use AVRDisable_HI2CAckPollState to make a tad faster
                   case AVR_I2C_START
                        ' dummy
                   case MT_SLA_ACK
                      HI2CAckPollState = false
                   case MT_DATA_ACK
                      HI2CAckPollState = false
                   case MT_SLA_NACK_REC
                       HI2CAckPollState = true
                   case MT_DATA_NACK_REC
                       HI2CAckPollState = true
                   case MR_SLA_NACK_REC
                       HI2CAckPollState = true
                   case MR_SLA_ACK
                       HI2CAckPollState = false
                   case MR_DATA_NACK_REC
                       HI2CAckPollState = true
                   case else
                        ' bad event!!
            end select
        #endif
        HI2CStartOccurred = false

End Sub


Sub AVRHI2CReceive (Out I2CByte, Optional In HI2CGetAck = 1 )

        if HI2CGetAck.0 = 0 then

          lds SysValueCopy,TWCR
          sbr       SysValueCopy, (1<<TWINT)
          cbr       SysValueCopy, (1<<TWSTA) | (1<<TWSTO) | (1<<TWEA)
          sts TWCR,SysValueCopy

        else
            ' Acknowledge
          lds SysValueCopy,TWCR
          cbr       SysValueCopy, (1<<TWSTA) | (1<<TWSTO)
          sbr       SysValueCopy, (1<<TWINT) | (1<<TWEA)
          sts TWCR,SysValueCopy
        end if

        nop
        nop
        do while TWINT = 0
        loop


        'Get byte
        I2CByte = TWDR

End Sub



sub HI2CInit

    Dim HI2C1StateMachine as byte
    Dim HI2CACKPOLLSTATE  as Byte
    Dim TWI0ACKPOLLSTATE  as Byte Alias HI2CACKPOLLSTATE
    Dim HI2C1lastError as Byte

    asm showdebug  This method sets the variable `HI2CCurrentMode`, and, if required calls the method `SI2CInit` to set up new MSSP modules - aka K-Mode family chips
    HI2CCurrentMode = 0

    'Initialise the I2C module
    #ifdef var(I2C1CON0)
        SI2CInit
    #endif

end sub




'New I2C module support

    #DEFINE I2C1_GOOD             0
    #DEFINE I2C1_FAIL_TIMEOUT     1
    #DEFINE I2C1_TXBE_TIMEOUT     2
    #DEFINE I2C1_START_TIMEOUT    4
    #DEFINE I2C1_RESTART_TIMEOUT  8
    #DEFINE I2C1_RXBF_TIMEOUT     16
    #DEFINE I2C1_ACK_TIMEOUT      32
    #DEFINE I2C1_MDR_TIMEOUT      64
    #DEFINE I2C1_STOP_TIMEOUT     128

    #DEFINE I2C1CLOCK_SMT1           0X09
    #DEFINE I2C1CLOCK_TIMER6PSO      0X08
    #DEFINE I2C1CLOCK_TIMER4PSO      0X07
    #DEFINE I2C1CLOCK_TIMER2PSO      0X06
    #DEFINE I2C1CLOCK_TIMER0OVERFLOW 0X05
    #DEFINE I2C1CLOCK_REFERENCEOUT   0X04
    #DEFINE I2C1CLOCK_MFINTOSC       0X03
    #DEFINE I2C1CLOCK_HFINTOSC       0X02
    #DEFINE I2C1CLOCK_FOSC           0X01
    #DEFINE I2C1CLOCK_FOSC4          0X00

#script

  IF NODEF(HI2CITSCLWaitPeriod) THEN
      HI2CITSCLWAITPERIOD = 70 'minimum of twenty plus 5 clock ticks
  END IF 

  IF NODEF(I2C1CLOCKSOURCE) THEN
      I2C1CLOCKSOURCE = I2C1CLOCK_MFINTOSC
  END IF

  IF NODEF(I2C1I2C1CON0Default) THEN
      I2C1I2C1CON0Default = 0x84
  END IF

  IF NODEF(I2C1I2C1CON1Default) THEN
      I2C1I2C1CON1Default = 0x80
  END IF

  IF NODEF(I2C1I2C1CON2Default) THEN
      I2C1I2C1CON2Default = 0x21
  END IF


#endscript

'    Example
'
'    #Include <SMT_Timers.h>
'    #define I2C1ClockSource I2C1Clock_SMT1
'    SETSMT1PERIOD ( 39 )   400 KHZ @ 64MHZ
'    SETSMT1PERIOD ( 158 )   '100 KHZ @ 64MHZ
'    InitSMT1(SMT_FOSC,SMTPres_1)
'    StartSMT1
'    #define HI2CITSCLWaitPeriod 100


'Check the define above this method

Sub SI2CInit
    asm showdebug  This method sets the MSSP modules for K-mode family chips

    Dir HI2C_DATA out
    Dir HI2C_CLOCK out

    I2C1CON1 = I2C1I2C1CON1Default
    I2C1CON2 = I2C1I2C1CON2Default
    I2C1CLK =  I2C1CLOCKSOURCE
    I2C1CON0 = I2C1I2C1CON0Default

    I2C1PIR = 0    ;Clear all the error flags
    I2C1ERR = 0
    I2C1CON0.EN=1

    'Commence I2C protocol
    I2C1CON2.ACNT = 0
    I2C1CON2.ABD=0
    I2C1CON0.MDR=1

    'Initialise correct state of I2C module. Not sure why this is needed but it is. Microchip failed to explain why this is required. But, it is.
    // SI2CStart
    // SI2CSend ( 0xff )
    // SI2CStop

    HI2CCurrentMode = 0

End sub

Sub SI2CStart
  asm showdebug  Redirected for K-Mode family probalby called HI2CStart
  asm showdebug  This method sets the registers and register bits to generate the I2C  START signal. Master_mode only.

        HI2C1StateMachine = 1
        HI2CWaitMSSPTimeout = false
        'Clear the error state variable
        HI2C1lastError = I2C1_GOOD

End Sub

Sub SI2CReStart
  asm showdebug  Redirected for K-mode family probalby called HI2CReStart
  asm showdebug  This method sets the registers and register bits to generate the I2C  RESTART signal. Master_mode only.
        HI2C1StateMachine = 3
        HI2CWaitMSSPTimeout = false

End Sub

Sub SI2CStop
  asm showdebug  Redirected for K-mode family probalby called HI2CStop
  asm showdebug  This method sets the registers and register bits to generate the I2C  STOP signal

    'Waits up to 254us then set the error state
    HI2C1StateMachine = 0
    HI2CWaitMSSPTimeout = 0

    do while HI2CWaitMSSPTimeout < 255

        HI2CWaitMSSPTimeout++

        'Wait till this event
        if I2C1PIR.PCIF = 1 then
            SI2Cwait4Stop
            'TSCL wait, part of the specificiation of the I2C Module
            wait HI2CITSCLWaitPeriod us
            exit sub

        else
            wait 1 us

        end if
    loop
    if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_STOP_TIMEOUT

End Sub


Sub SI2CSend ( in I2Cbyte )

    asm showdebug  Redirected for K-mode family probalby called HI2CSend
    asm showdebug  This method sets the registers and register bits to send I2C data

    'This is now a state Machine to cater for the new approach with the I2C module
    Select Case HI2C1StateMachine


      case 2  'send data

        HI2CWaitMSSPTimeout = 0
        'waits up to 254us then creates error message
        do while HI2CWaitMSSPTimeout < 255

            HI2CWaitMSSPTimeout++
            'Wait for this event
            if I2C1STAT1.TXBE = 1 then

                'Set the byte count to 1, place outbyte in register, and wait for hardware state machine
                I2C1CNT = 1
                I2C1TXB = I2Cbyte
                SI2CWait4Ack
                SI2Cwait4MDR

                exit Sub

            else

                wait 1 us

            end if
        loop

        if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_TXBE_TIMEOUT


      case 1  'A start

        'Clear the output buffers, set byte count to zero, clear event and place Slave Address in register
        I2C1STAT1.CLRBF = 1
        I2C1CNT = 0
        I2C1PIR.SCIF = 0
        I2C1ADB1 = I2Cbyte

        HI2CWaitMSSPTimeout = 0
        'waits up to 254us then creates error message
        do while HI2CWaitMSSPTimeout < 255

            HI2CWaitMSSPTimeout++
            'Wait for this event
            if I2C1PIR.SCIF = 1 then
                HI2C1StateMachine = 2  'Set state Machine to send data
                HI2CAckPollState = I2C1CON1.5
                exit Sub

            else
                'Set to start
                I2C1CON0.S = 1
                wait 1 us

            end if
        loop

        if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_START_TIMEOUT

      case 3  'A restart

        'wait for hardware machine to settle
        wait while I2C1STAT0.MMA = 1

        'set count to zero, place Slave address in register and clear event
        I2C1CNT = 0
        I2C1ADB1 = I2Cbyte
        I2C1PIR.SCIF = 0

        HI2CWaitMSSPTimeout = 0
        'waits up to 254us then creates error message
        do while HI2CWaitMSSPTimeout < 255

            HI2CWaitMSSPTimeout++
            'Wait for this event
            if I2C1PIR.SCIF = 1 then
                HI2C1StateMachine = 2  'Set state Machine to send data
                HI2CAckPollState = I2C1CON1.5
                exit Sub

            else
                'Set as Start and Clear Restart
                I2C1CON0.S = 1
                I2C1CON0.RSEN=0
                wait 1 us

            end if
        loop

        if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_RESTART_TIMEOUT

    end select

End Sub


Sub SI2CReceive (Out I2CByte, Optional In HI2CGetAck = 1 )
  asm showdebug  Redirected for K-mode family probalby called HI2CReceive
  asm showdebug  This method sets the registers and register bits to get I2C data
      I2C1CNT = 1
      HI2CWaitMSSPTimeout = 0

      'waits up to 254us then creates error message
      do while HI2CWaitMSSPTimeout < 255

          HI2CWaitMSSPTimeout++
          'Wait for this event
          if I2C1STAT1.RXBF = 1 then
              'Read input buffer to output byte
              I2CByte = I2C1RXB
              exit Sub

          else

              wait 1 us

          end if
      loop

      if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_RXBF_TIMEOUT

End Sub

Sub SI2CWait4Ack

    HI2CWaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2CWaitMSSPTimeout < 255

        HI2CWaitMSSPTimeout++
        'Wait for this event
        if I2C1CON1.ACKSTAT = 0 then
            'Set status
            #IF ChipSubFamily =  ChipFamily18FxxQ41
              HI2CAckPollState = !I2C1CON1.ACKSTAT
            #ELSE
              HI2CAckPollState = I2C1CON1.ACKSTAT
            #ENDIF
            exit sub

        else

            wait 1 us

        end if
    loop
    if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_ACK_TIMEOUT
    ' HI2CWaitMSSPTimeout is now TRUE = 255
End Sub



Sub SI2Cwait4MDR

    HI2CWaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2CWaitMSSPTimeout < 255

        HI2CWaitMSSPTimeout++
        'wait for this event
        if I2C1CON0.MDR = 0 then

            exit sub

        else

            wait 1 us

        end if
    loop
    if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_MDR_TIMEOUT
    ' HI2CWaitMSSPTimeout is now TRUE = 255
End Sub


Sub SI2Cwait4Stop

    HI2CWaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2CWaitMSSPTimeout < 255

        HI2CWaitMSSPTimeout++
        'Wait for this event
        if I2C1PIR.PCIF = 1 then

            exit sub

        else

            wait 1 us

        end if
    loop
    if HI2CWaitMSSPTimeout = 255 then HI2C1lastError = HI2C1lastError or I2C1_STOP_TIMEOUT
    ' HI2CWaitMSSPTimeout is now TRUE = 255
End Sub


sub SI2CDiscovery ( address )

    I2C1CON1 = I2C1I2C1CON1Default
    I2C1CON2 = I2C1I2C1CON2Default
    I2C1CLK =  I2C1CLOCKSOURCE
    I2C1CON0 = I2C1I2C1CON0Default

    I2C1PIR = 0    ;Clear all the error flags

    I2C1CON0.EN=1

    'Commence I2C protocol
    I2C1CON2.ACNT = 0
    I2C1CON2.ABD=0
    I2C1CON0.MDR=1
    I2C1CON1.7=0

    I2C1ADB1= address
    I2C1CON0.S=1

    wait while I2C1STAT1.TXBE <> 1

    #IF ChipSubFamily =  ChipFamily18FxxQ41 or ChipSubFamily =  ChipFamily18FxxQ71

      'this chip has a proper STOP bit, so, get statis and exit SUB
      HI2CSend 0
      HI2CSend 0
      wait 2 ms  'wait for I2C1CON1!
      HI2CAckpollState.0 = I2C1CON1.ACKSTAT
      I2C1CON1.P = 1           'stop
      exit sub
    #ENDIF


    #IFNDEF bit(P)   'searching for I2C1CON1.P

      'Set the byte count to 1, place outbyte in register, and wait for hardware state machine
      #IFDEF var(I2C1CNTL)
        I2C1CNTL = 1
      #ENDIF
      #IFDEF var(I2C1CNT)
        I2C1CNT = 1
      #ENDIF

      I2C1TXB = 0 'reg

      HI2CAckpollState = 0

      SI2CStop
      'wait for I2C to fail or not..
      wait 1 ms
      HI2CAckpollState.0 = !I2C1PIR.7
      'Reset module
      I2C1CON0.EN=0
      wait 1 ms
      I2C1CON0.EN=1

    #ENDIF

end sub

// AVRdx support *******************************************************************



Sub AVRDxTWI0Mode

    Dim HI2CCurrentMode as Byte
    Dim TWI0Timeout as Byte Alias HI2CWaitMSSPTimeout

    Dim HI2C1StateMachine as byte
    Dim HI2CACKPOLLSTATE  as Byte
    Dim TWI0ACKPOLLSTATE  as Byte Alias HI2CACKPOLLSTATE
    Dim HI2C1lastError as Byte
    Dim TWI0LastError as Byte Alias HI2C1lastError

    HI2CCurrentMode = 0
    TWI0Timeout = 0

    Do

      TWI0Timeout = 0

      Dir HI2C_DATA out
      Dir HI2C_CLOCK Out
      
      Do 

        If TWI0Timeout = 255 then Exit Sub  // Users can check for TWI0Timeout = TRUE
        TWI0Timeout++

        // Reset the TWI!
        TWI0_MCTRLB = TWI_FLUSH_bm
        TWI0_MSTATUS= 0
        TWI0_CTRLA = 0
        TWI0_MCTRLA = 0
        TWI0_MCTRLB = 0

        // Bit bang a START/STOP sequence
          HI2C_DATA = 1                 // SDA and SCL idle high
          HI2C_CLOCK = 1
          Wait 1 us
          HI2C_DATA = 0                 // then, SDA low while SCL still high
          Wait 1 us                     // for this amount of time
          HI2C_CLOCK = 0                // end with SCL low, ready to clock
          HI2C_DATA = 0
          Wait 1 us                     // let ports settle
          HI2C_CLOCK = 1                // make SCL=1 first
          Wait 1 us                     // hold for normal clock width time
          HI2C_DATA = 1                 // then make SDA=1 afterwards
          wait 1 ms
    
      Loop While ( TWI0_MSTATUS and 3 ) = 3

      TWI0_CTRLA = SCRIPT_TWI_FAST_MODE  // for slow mode = 0 
      
      TWI0_DUALCTRL = 0

      //Debug Run
      TWI0_DBGCTRL = 0x00
      
      //Master Baud Rate Control
      TWI0_MBAUD = SCRIPT_TWI_BAUD //(uint8_t)TWI0_BAUD(100000, 0)
      
      TWI0_MCTRLA = 0x02
      
      TWI0_MSTATUS = 0x61
      
      //Master Address
      TWI0_MADDR = 0x00
      
      //FLUSH  ACKACT ACK MCMD NOACT 
      TWI0_MCTRLB = 0x08
      
      //Master Data
      TWI0_MDATA = 0x00

      TWI0_MCTRLA.0 = 1
      
      wait 10 ms
    
    Loop While ( TWI0_MSTATUS and 3 ) = 3

End sub

Sub AVRDxTWI0Start

    Dim TWI0StateMachine,TWI0LastError as Byte
    Dim TWI0Timeout as Byte Alias HI2CWaitMSSPTimeout
    Dim TWI0AckPollState as Byte Alias HI2CAckPollState

    TWI0StateMachine = 1
    TWI0Timeout = 0

    // Clear the error state variable
    TWI0LastError = I2C1_GOOD
    
End Sub

Sub AVRDxTWI0ReStart
    
    Dim TWI0StateMachine, TWI0Timeout,TWI0LastError as Byte

    TWI0StateMachine = 3
    TWI0Timeout = 0

    // Clear the error state variable
    TWI0LastError = I2C1_GOOD
End Sub

Sub AVRDxTWI0Stop

    Dim TWI0Timeout as Byte Alias HI2CWaitMSSPTimeout
    Dim TWI0AckPollState as Byte Alias HI2CAckPollState

    // Waits up to 254us then set the error state
    TWI0StateMachine = 0
    TWI0Timeout = 0

    // Stop Command
    TWI0_MCTRLB = TWI0_MCTRLB OR 3

    do while TWI0Timeout < 255
        TWI0Timeout++
        // Wait till bus is IDLE 
        if TWI0_MSTATUS.TWI_BUSSTATE_0_bp  = 1 and TWI0_MSTATUS.TWI_BUSSTATE_1_bp  = 0 then
            exit sub
        else
          #IF TWI_SEND_DELAY= 0 
            NOP
            NOP
          #ELSE
            wait   TWI_SEND_DELAY us
          #ENDIF
        end if
    loop
    
    if TWI0Timeout = 255 then TWI0LastError = TWI0LastError or I2C1_STOP_TIMEOUT

End Sub

Sub AVRDxTWI0Send ( in I2Cbyte )

    Dim TWI0Timeout as Byte Alias HI2CWaitMSSPTimeout
    Dim TWI0AckPollState as Byte Alias HI2CAckPollState

    // This is now a state Machine to cater for the new approach with the AVRDX TWI module
    Select Case TWI0StateMachine

      case 2  // Send data
      
        // Send Data Command
        TWI0_MCTRLB = TWI0_MCTRLB OR 6

        TWI0_MDATA = I2Cbyte
        // Ensure stability on the bus
        TWI0Timeout = 1
        dim cache_TWI0_MSTATUS
        cache_TWI0_MSTATUS = TWI0_MSTATUS
        Do while cache_TWI0_MSTATUS = TWI0_MSTATUS
            If TWI0Timeout = 0 Then 
              TWI0Timeout = 255
              Exit Do
            End If
            TWI0Timeout++
        Loop

        // At this point the TWI0Timeout may ( or may not be ) 255.. if 255 then the timeout has happended, prepare to exit sub
        // Waits up to 254us then creates error message
        do while TWI0Timeout < 255

            TWI0Timeout++
            // Wait for this event
            if TWI0_MSTATUS.TWI_RXACK_bp = 0 then
                TWI0AckPollState = False
                exit Sub
            else
              #IF TWI_SEND_DELAY= 0 
                NOP
                NOP
              #ELSE
                wait   TWI_SEND_DELAY us
              #ENDIF
            end if
        loop

        if TWI0Timeout = 255 then TWI0LastError = TWI0LastError
        TWI0AckPollState = True

      case 1  // A start

        // Send Start command
        TWI0_MCTRLB = TWI0_MCTRLB OR 1

        #IF TWI_SEND_DELAY= 0 
          NOP
          NOP
        #ELSE
          wait   TWI_SEND_DELAY us
        #ENDIF
        TWI0_MADDR = I2Cbyte
        
        // Ensure stability on the bus
        TWI0Timeout = 1
        dim cache_TWI0_MSTATUS
        cache_TWI0_MSTATUS = TWI0_MSTATUS
        Do while cache_TWI0_MSTATUS = TWI0_MSTATUS
            If TWI0Timeout = 0 Then 
              TWI0Timeout = 255
              Exit Do
            End If
            TWI0Timeout++
        Loop
        TWI0Timeout = 0

        TWI0StateMachine = 2

        // Can wait up to 254 operations then set error code
        do while TWI0Timeout < 255
            TWI0Timeout++

            // Wait for this event
            If ( TWI0_MSTATUS.4 = 0 ) or ( TWI0_MSTATUS.3 = 1 ) then
                // Move to next state upon exit
                TWI0AckPollState = False
                exit Sub
            else
              #IF TWI_SEND_DELAY= 0 
                NOP
                NOP
              #ELSE
                wait   TWI_SEND_DELAY us
              #ENDIF
            end if
        loop

        if TWI0Timeout = 255 then TWI0LastError = TWI0Timeout or I2C1_START_TIMEOUT
        TWI0AckPollState = True

      case 3  // A restart

        TWI0_MCTRLB = TWI0_MCTRLB OR 1

        #IF TWI_SEND_DELAY= 0 
          NOP
          NOP
        #ELSE
          wait   TWI_SEND_DELAY us
        #ENDIF
        TWI0_MADDR = I2Cbyte
        
        // Ensure stability on the bus
        TWI0Timeout = 1
        dim cache_TWI0_MSTATUS
        cache_TWI0_MSTATUS = TWI0_MSTATUS
        Do while cache_TWI0_MSTATUS = TWI0_MSTATUS
            If TWI0Timeout = 0 Then 
              TWI0Timeout = 255
              Exit Do
            End If
            TWI0Timeout++
        Loop
        TWI0Timeout = 0

        TWI0StateMachine = 2

        // Can wait up to 254 operations then set error code
        do while TWI0Timeout < 255
            TWI0Timeout++

            // Wait for this event
            If ( TWI0_MSTATUS.4 = 0 ) or ( TWI0_MSTATUS.3 = 1 ) then
                // Move to next state upon exit
                TWI0AckPollState = False
                exit Sub
            else
              #IF TWI_SEND_DELAY= 0 
                NOP
                NOP
              #ELSE
                wait   TWI_SEND_DELAY us
              #ENDIF
            end if
        loop

        if TWI0Timeout = 255 then TWI0LastError = TWI0Timeout or I2C1_START_TIMEOUT
        TWI0AckPollState = True

      End Select 

End Sub

Sub AVRDxTWI0Receive ( Out I2CByte, Optional In HI2CGetAck = 1 )

    Dim TWI0Timeout as Byte Alias HI2CWaitMSSPTimeout
    Dim TWI0AckPollState as Byte Alias HI2CAckPollState

    DIR HI2C_DATA IN 

    // Wait for bus to clear
    wait while TWI0_MSTATUS.TWI_BUSSTATE_0_bp  = 1 and TWI0_MSTATUS.TWI_BUSSTATE_1_bp  = 0

    TWI0_MCTRLB = TWI0_MCTRLB OR TWI_MCMD_RECVTRANS_gc
    TWI0_MCTRLB.TWI_ACKACT_bp = HI2CGetAck
    I2Cbyte = TWI0_MDATA

    TWI0Timeout = 0

    // Waits up to 254us then creates error message
    do while TWI0Timeout < 255

        TWI0Timeout++
        // Wait for this event
        if TWI0_MSTATUS.TWI_RXACK_bp = 1 then
            TWI0AckPollState = False
            exit Sub
        else
              #IF TWI_SEND_DELAY= 0 
                NOP
                NOP
              #ELSE
                wait   TWI_SEND_DELAY us
              #ENDIF
        end if
    loop

    if TWI0Timeout = 255 then TWI0LastError = TWI0LastError
    TWI0AckPollState = True

    DIR HI2C_DATA OUT

End Sub

// ****************************** Q20 Support

Dim HWI2C_Buffer( SCRIPT_HIC2Q2XBUFFERSIZE )

Sub HI2CQ24Stop

    // This routine handles the full transaction.
    // 1. Enables and loads the key registers
    // 2. Send IC2START
    // 3. Send data.. using I2CSEND
    // 4. Set  HI2CWaitMSSPTimeout and HI2CAckpollState for public use.

    // ReStart I2C this prevent lock ups
    I2C1CON0.I2CEN=1
    // Clear the test bit
    I2C1PIR.7 = 0

    // Number of bytes excluding address
    I2C1CNTH = HWI2C_BufferLocationCounter_H 
    I2C1CNT = HWI2C_BufferLocationCounter

    // Start
    I2C1CON0.S = 1

    Do while I2C1CON0.S = 1
        // Wait for start 
        NOP
    Loop

    Dim HWI2C_BufferLocationCounterIndex as Word
    
    For HWI2C_BufferLocationCounterIndex = 1 to HWI2C_BufferLocationCounter

        HI2CWaitMSSPTimeout = 0

        // Send/Clock data out
        I2C1TXB = HWI2C_Buffer ( HWI2C_BufferLocationCounterIndex )

        Do while I2C1STAT1.TXBE = 0
          // Wait for Transmit Buffer to Empty
          wait 1 us
          HI2CWaitMSSPTimeout++
          If HI2CWaitMSSPTimeout = 255 Then
              // Transmission failure, exit.
              Goto HI2CQ24StopExit
          End If
          
        Loop

    Next

    // Wait for a stop bit to complete
    Do while I2C1STAT0.MMA = 1 and HI2CWaitMSSPTimeout <> 255
      wait 1 us
    Loop

    HI2CQ24StopExit:
    If I2C1PIR.7 = 1 then
      HI2CAckpollState = False
    Else
      HI2CAckpollState = True
    End If
    I2C1CON0.I2CEN=0

End Sub

Sub HI2CQ24Send ( in I2Cbyte )
    
  //~ Redirected for Q24 family higher level programs or the user program probably called HI2CSend

    Dim HWI2C_Buffer( SCRIPT_HIC2Q2XBUFFERSIZE )
    Dim HWI2C_BufferLocationCounter as Word

    asm showdebug  Redirected for Q2x-mode family probalby called HI2CSend
    //~ This method sets the state and loads the buffer

    // This is a state Machine to cater for the new approach with the I2C module
    Select Case HI2C1StateMachine

      case 2  'send data

        //~ Load buffer
        HWI2C_BufferLocationCounter++
        HWI2C_Buffer(HWI2C_BufferLocationCounter)=I2Cbyte
        exit Sub

      case 1  'A start

        //~ Reset buffer
        HWI2C_BufferLocationCounter = 0
        I2C1ADB1 = I2Cbyte
        HI2C1StateMachine = 2  'Set state Machine to send data
        Exit Sub

      case 3  'A restart

        //~ Reset buffer
        HWI2C_BufferLocationCounter = 0
        I2C1ADB1 = I2Cbyte
        HI2C1StateMachine = 2  'Set state Machine to send data
        Exit Sub

    end select

End Sub

Sub HI2CQ24Init

      // SCL pin
      HI2C_CLOCK = 1
      DIR HI2C_CLOCK Out

      // SDA pin
      HI2C_DATA = 1
      DIR HI2C_DATA Out

      // Configure the peripheral itself
      I2C1CON0.MODE0=0; // 0b100 => I2C Host mode, 7-bit address
      I2C1CON0.MODE1=0; // 0b100 => I2C Host mode, 7-bit address
      I2C1CON0.MODE2=1; // 0b100 => I2C Host mode, 7-bit address
      
      I2C1CON1.ACKCNT=1; // 1 => I2CxCNT == 0 => Not Acknowledge (NACK) copied to SDA output
      I2C1CON1.ACKDT=0; // 0 => I2CxCNT != 0 => Acknowledge (ACK) copied to SDA output
      I2C1CON3.FME0=0b0; // 0b00 => SCL frequency (FSCL) = FI2CxCLK/5 // Q24 specific
      I2C1CON3.FME1=0b0; // 0b00 => SCL frequency (FSCL) = FI2CxCLK/5 // Q24 specific
      
      I2C1CON2.BFRET0=0b0; // 0b00 => Minimum stop time 8 I2CxCLK pulses
      I2C1CON2.BFRET1=0b0; // 0b00 => Minimum stop time 8 I2CxCLK pulses
      I2C1CON2.ABD=0; // 0 => Address buffers are enabled.
      I2C1CLK=0b0011; // 0b0011 => MFINTOSC (500kHz): gives 100kHz with FME=0. 125kHz with FME=1
      I2C1BAUD=0; // 0 => div-by-1. // **** New for Q24 devices
      I2C1CON0.I2CEN=1;
      // This is the workaround as stated in the errata
      wait 1 us
      nop
      nop
      nop
      nop
      nop
      nop

End Sub


// ****************************** Q20 Support

Dim HWI2C_Buffer( SCRIPT_HIC2Q2XBUFFERSIZE )

Sub HI2CQ20Stop

  asm showdebug  Redirected for Q20 family higher level programs or the user program probably called HI2CStop
  asm showdebug  This method set the buffer, so this actually sends START..BUFFER..END

    Dim HWI2C_Buffer( SCRIPT_HIC2Q2XBUFFERSIZE )
    Dim HWI2C_BufferLocationCounter as Word
    Dim HWI2C_BufferLocationCounterIndex as Word
    
    //~ Set clear register buffer
    I2C1STAT1.CLRBF = 1
    I2C1STAT1.I2C1TXB = 1
    I2C1CNT = HWI2C_BufferLocationCounter
    //~ Set address  
    I2C1ADB1 = I2C1ADB1_cache
    I2C1PIR.SCIF = 0
    I2C1CON0.S = 1

    If HWI2C_BufferLocationCounter > 0 then
      HI2CAckpollState = 0

      // Iterate thru the I2C buffer   
      For HWI2C_BufferLocationCounterIndex = 1 to HWI2C_BufferLocationCounter

          HI2CWaitMSSPTimeout = 0
          'waits up to 254us then creates error message the user can use
          Do while HI2CWaitMSSPTimeout < 255
              HI2CWaitMSSPTimeout++

              'HWI2C_BufferLocationCounter++
              I2C1TXB = HWI2C_Buffer(HWI2C_BufferLocationCounterIndex)
          
              Do while HI2CWaitMSSPTimeout < 255

                HI2CWaitMSSPTimeout++
                'Wait for this event
                If I2C1STAT1.TXBE = 1 then

                    Do while HI2CWaitMSSPTimeout < 255

                      If I2C1CON1.ACKSTAT = 0 Then                       
                        Goto NextI2CBufferByte
                      End If
                      HI2CWaitMSSPTimeout++
                    Loop
                Else
                    wait 1 us
                End If
              Loop
          loop
          If HI2CWaitMSSPTimeout = 255 then 
            Exit For
          End If
          NextI2CBufferByte:
      Next
    End If

    HI2C1StateMachine = 0
    HWI2C_BufferLocationCounter = 0
    HI2CAckpollState = I2C1CON1.5
    //~ forced delay to ensure the register is cleared
    wait 95 us
    I2C1CON1.P = 1
    
End Sub

Sub HI2CQ20Send ( in I2Cbyte )
    
  asm showdebug  Redirected for Q20 family higher level programs or the user program probably called HI2CSend

    Dim HWI2C_Buffer( SCRIPT_HIC2Q2XBUFFERSIZE )
    Dim HWI2C_BufferLocationCounter as Word

    asm showdebug  Redirected for K-mode family probalby called HI2CSend
    asm showdebug  This method sets the registers and register bits to send I2C data

    'This is now a state Machine to cater for the new approach with the I2C module
    Select Case HI2C1StateMachine

      case 2  'send data

        If HWI2C_BufferLocationCounter = SCRIPT_HIC2Q2XBUFFERSIZE + 1 Then
          // Send the buffer when full, HWI2C_BufferLocationCounter is reset automatically
          // Means MAX I2C packet for the Q20 is 128 bytes
          SI2CStop
        End if

        //~ Load buffer
        HWI2C_BufferLocationCounter++
        HWI2C_Buffer(HWI2C_BufferLocationCounter)=I2Cbyte
        exit Sub

      case 1  'A start

        //~ Reset buffer
        HWI2C_BufferLocationCounter = 0
        I2C1ADB1_cache = I2Cbyte
        HI2C1StateMachine = 2  'Set state Machine to send data
        Exit Sub

      case 3  'A restart

        //~ Rest buffer
        HWI2C_BufferLocationCounter = 0
        I2C1ADB1_cache = I2Cbyte
        Exit Sub

    end select

End Sub
