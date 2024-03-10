'    USART routines for GCBASIC
'    Copyright (C) 2009-2024 Hugh Considine, Mike Otte, William Roth and Evan Venn

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

'Changes:
' 12/12/2009: Corrected TXIF checking
' 8/2/2010: Added delay after each byte in print
' 7/6/2011: Added HSerPrint(integer)
' 17/7/2011: Fixes for ATmega328P
' 12/2/2012: Added HSerPrint(long)
' 2/6/2013: Added In directive to HSerPrint(string)
' 10/7/2013: Added USART_TX_BLOCKING option, fixes for chips with 2 modules
' 29/9/2013: Fixes for ATmega32u4
' 16/2/2014: Fixed HERPRINT Long Bug
' 28/6/2014: Changed HSerPrintCRLF to have a parameter... you can have lots of CRLF's
' 04/07/2015:  Improved timing
' 31/7/2015: Fix Compile Error for 16F1705 1709  and other Pics - WMR
'            See lines 311 - 320
' 02/10/2015: Fix for missing SPBRG - mapped to SPBRGL
' 11/10/2015: Fixed bug for missing SPBRG - mapped to SPBRGL in script
' 31/10/2015: Added second usart capabilities, HSerGetNum,HSerGetString -mlo
' 9/12/2015: Changed default USART_DELAY from 12 ms back to 1 ms - fix very slow USART default!
'            Corrected dates above
' 18/12/2015:Added new function.methods HSerReceive1, HSerReceive2, HSerReceiveFrom
' 18/12/2015: Repositioned functions
' 19/12/2015: Revised to support CREN2 and SPEN2
' 07/01/2016: Added AVR 4 channel USARTs
' 24/01/2016: Added default value to comport
' 26/01/2016: Corrected USART2 operations
' 08/03/2016: Correct for 16f18855 SYNC_TX1STA to SYNC mapping
' 04/01/2017: Correct for 18FxxK40 SP1RGH/L to SPBRGH/L mapping - WMR
' 21/01/2017: Revised to support standardised .dat files - ERV
' 04/02/2017: Revised HSerGetNum to support long and prevent buffer overrun - Motte
' 07/10/2017: Added U1CON1 and U2 UART support.
' 01/12/2017: Added HSerPrintStringCRLF
' 23/09/2018: Improved accuracy of USART_Delay PICs). 1ms is now 1ms - WMR
' 23/09/2018: Moved all "wait USART_DELAYs"  to a single method in hsersend sub -WMR
' 23/09/2018: Edited as part of the QA check
' 17/11/2018: Correct errant setting of SYNC on K42/83
' 02/01/2019: Added IF-DEF to protect WAIT in HSerSend
' 13/08/2019: Adapted as follows
'              Added new overloaded method for HSerSend
'              Added new handler in HSerSend for USART modules with the TXREG register to improve performance
' 17/11/2019  Added USARTHasDataSet to work around the evaluation issue in If USARTHasData Then issue
' 18/12/2019  Revised #ifdef Var(TXREG) section of HSerReceive methods to ensure that previous data has been transmitted
'             Revised init to improve documentation
' 16/02/2020  Revised to restore setting in  INIT
' 03/04/2020  Revised HSerSend to correct TRMT blocking error
' 04/05/2020  Changed to canskip to silently exit when no USART for TXEN bits
' 15/02/2020  Correct legacy chip init. #ifndef BIT(BRG16) change to  #ifdef VAR(SPBRG) to prevent creation of SPBRG on newer chips.
' 06/07/2020  Added AVRrc support to reduce memory usage
' 14/09/2020  Added script adjustments for low frequency BPS
' 29/09/2020  Added script to detect USART calc errors.  Use the following constants to control the checking and warning messages
'             #DEFINE CHECK_USART_BAUD_RATE
'             #DEFINE ISSUE_CHECK_USART_BAUD_RATE_WARNING
' 16/02/2021  Add ChipSubFamily = 15001 TX1REG redirection in script section
' 21/03/2021  Added Support for 90S Series AVR
' 10/04/2021  Removed incorrect mapping as TRMT is this impacts USART1 removed
' 01/01/2022  Added ChipSubFamily = 16106 support.  Script added to flag to correct TX interrupt bits in HSerSend
' 10/02/2022  Remove variable type from functions
' 25/06/2022  Add RaiseCompilerError handler - required compiler build 1131 or later
' 04/07/2022  Enable ISSUE_CHECK_USART_BAUD_RATE_WARNING .. it was missing from library
' 11/08/2022  Move USART_DELAY to script - was static Constant
' 19/08/2022  Add ChipSubFamily = 15002/3/4 TX1REG redirection in script section
' 22/08/2022  Revised HSerPrint and HSerPrintRC to ensure HSerSend uses Byte, Byte overloaded method.  Fixes USART1 and USART2 overwrite/corruption issue
' 14/10/2022  Major addition of usart3,4 and 5 for PIC - send() by Bill Roth and receive() by Evan Venn
'             Rationalisation of Usart selection using Case-Select
'             Change/added comments to use //~
'             Rationalisation of init to minimise ASM generated
'             Improved USART usage to create STRICT use of baud rates to ensure specific USART capability exists
'             Improved INIT for K40 type uC
'             Added RAISECOMPILERERROR where appropiate
'             Improved handling of variable comport and Defaultusartreturnvalue variable to only set when more than one USART
' 14/12/2022  Correct InitUSART for K22 ( missing register update )
' 06/08/2023  Improved handling of registers for USART2
' 27/08/2023  Added support for K40 USART3, 4 and 5
' 07/09/2023  Added ORR handling for PIC 3,4 and 5
' 26/10/2023  Correct #IF Var() where is shoul have need #IFDEF Var() in USART2Receive. 2nd revision
' 13/11/2023  Resolve 18F1220 digital port setting
' 13/02/2024  Isolation of CREN by conditional test of RCSTA. Error encountered tested 18FxxQ20 as chip with I3CCON0.EN cause I3CCON0.EN = 1 in serial handler.




/*

USART constants
#DEFINE USART_BAUD_RATE 9600
#DEFINE USART_BLOCKING          'waits for byte to be received and transmitted
#DEFINE USART_TX_BLOCKING       'waits for empty Tx input buffer/register so the previous activity has been completed.
#DEFINE USART2_BAUD_RATE 9600
#DEFINE USART2_BLOCKING         'Usart 2 waits for byte to be received
#DEFINE USART2_TX_BLOCKING      'Usart 2 waits for empty Tx input buffer/register so the previous activity has been completed.
#DEFINE USART3_BAUD_RATE 9600
#DEFINE USART3_BLOCKING         'Usart 3 waits for byte to be received
#DEFINE USART3_TX_BLOCKING      'Usart 3 waits for empty Tx input buffer/register so the previous activity has been completed.
#DEFINE USART4_BAUD_RATE 9600
#DEFINE USART4_BLOCKING         'Usart 4 waits for byte to be received
#DEFINE USART4_TX_BLOCKING      'Usart 4 waits for empty Tx input buffer/register so the previous activity has been completed.
#DEFINE USART5_BAUD_RATE 9600
#DEFINE USART5_BLOCKING         'Usart 5 waits for byte to be received
#DEFINE USART5_TX_BLOCKING      'Usart 5 waits for empty Tx input buffer/register so the previous activity has been completed.

#DEFINE DEFAULTUSARTRETURNVALUE 'The byte value returned from USART receive when USARTx_BLOCKING is NOT used.

#DEFINE SerPrintCR  'prints a CR at the end of every print Sub
#DEFINE SerPrintLF  'prints a LF at the end of every print Sub
#DEFINE SerPrintCRn  'prints a CR at the end of every print Sub for a specific USART
#DEFINE SerPrintLFn  'prints a LF at the end of every print Sub for a specific USART


Remember also to set the appropriate Comport Pins for direction In and Out - the may be implied by the uC default condition but good practice is to set the DIR for the USART port.pin

Note that the HSerReceive routine is implemented differently. In this
library, it is a subroutine, as with SerReceive in rs232.h. This is purely
a design decision, and has been made to keep included routines consistent
with each other.

To show USART1 (only USART1) calculations in terms of actual BPS and % error.  Use the following constants to control the checking and warning messages

#DEFINE CHECK_USART_BAUD_RATE                   'Show baud rate errors above 2% during compiling
#DEFINE ISSUE_CHECK_USART_BAUD_RATE_WARNING     'Show actual baud rate during compiling

*/



'Ensure that CHIPUSART is present in the DAT file.
#option REQUIRED PIC CHIPUSART "Hardware Serial operations. Remove USART commands to resolve errors."
#option REQUIRED AVR CHIPUSART "Hardware Serial operations. Remove USART commands to resolve errors."

'Some wrappers for compatibility with Contributors USART routines
#DEFINE HserPrintByte HSerPrint

; ----------------------------------------------------
; #samevar/#samebit used to determined registers/bits
; in Hsersend sub routine where different chips have
; different register or bit names with the same function
; -----------------------------------------------------
#samevar TXREG, TXREG1, U1TXB
#samebit TXIF, TX1IF, U1TXIF

#sameVar TXREG2, TX2REG, U2TXB
#samevar TXSTA2, TX2STA
#samebit TX2IF, U2TXIF
' incorrect mapping as TRMT is also USART1 removed 10.4.21 #samebit TXSTA2.TRMT, TX2STA.TRMT, U2ERRIR.TXMTIF
#samebit TXSTA2_TRMT, TX2STA_TRMT, U2ERRIR_TXMTIF '// PIC16F1946/47

#sameVar TXREG3, TX3REG, U3TXB
#samevar TXSTA3, TX3STA
#samebit TX3IF, U3TXIF
#samebit TXSTA3_TRMT, TX3STA_TRMT, U3ERRIR_TXMTIF

#sameVar TXREG4, TX4REG, U4TXB
#samevar TXSTA4, TX4STA
#samebit TX4IF, U4TXIF
#samebit TXSTA4_TRMT, TX4STA_TRMT, U4ERRIR_TXMTIF

#sameVar TXREG5, TX5REG, U5TXB
#samevar TXSTA5, TX5STA
#samebit TX5IF, U5TXIF
#samebit TXSTA5_TRMT, TX5STA_TRMT, U5ERRIR_TXMTIF
#samevar RCREG5, RC5REG

'Script to calculate baud rate generator values
'Also sets constants to check if byte received

#script

  IF NODEF(USART_DELAY) THEN
      'Set the default value of the USART_DELAY using a constant
      USART_DELAY=1 ms
  END IF

  IF NODEF(USART2_DELAY) THEN
      'Set the default value of the USART_DELAY using a constant
      USART2_DELAY=1 ms
  END IF

  IF NODEF(USART3_DELAY) THEN
      'Set the default value of the USART_DELAY using a constant
      USART3_DELAY=1 ms
  END IF

  IF NODEF(USART4_DELAY) THEN
      'Set the default value of the USART_DELAY using a constant
      USART4_DELAY=1 ms
  END IF

  IF NODEF(USART5_DELAY) THEN
      'Set the default value of the USART_DELAY using a constant
      USART5_DELAY=1 ms
  END IF

  IF NODEF(DEFAULTUSARTRETURNVALUE) THEN
    'WHEN NOT USING USART_BLOCKING THE RETURN VALUE IS DEFINED BY
    DEFINE Defaultusartreturnvalue=255
  END IF


  'Users may define USART1_BAUD_RATE as this makes logical sense, but, all the methods() use USART_BAUD_RATE for COM1.
  'So, if they use USART1_BAUD_RATE then this will automatically create the correct CONSTANTs
  If USART1_BAUD_RATE Then
    USART_BAUD_RATE   = USART1_BAUD_RATE
    USART_BLOCKING    = USART1_BLOCKING
    USART_TX_BLOCKING = USART1_TX_BLOCKING
    USART_BLOCKING    = USART1_BLOCKING
  End if

  //~There is SELECT-CASE in HSerSend() to improve performance and to only include the CASEs when more than one USART is use.
  SCRIPT_USART_USAGE_CHECK = 0
  IF USART_BAUD_RATE THEN
    SCRIPT_USART_USAGE_CHECK = SCRIPT_USART_USAGE_CHECK + 1
  END IF
  IF USART2_BAUD_RATE THEN
    SCRIPT_USART_USAGE_CHECK = SCRIPT_USART_USAGE_CHECK + 1
  END IF
  IF USART3_BAUD_RATE THEN
    SCRIPT_USART_USAGE_CHECK = SCRIPT_USART_USAGE_CHECK + 1
  END IF
  IF USART4_BAUD_RATE THEN
    SCRIPT_USART_USAGE_CHECK = SCRIPT_USART_USAGE_CHECK + 1
  END IF
  IF USART5_BAUD_RATE THEN
    SCRIPT_USART_USAGE_CHECK = SCRIPT_USART_USAGE_CHECK + 1
  END IF

  If PIC Then

    if ChipSubFamily = 15001 then
          TXREG = TX1REG
          RCREG = RC1REG
    end if

    if ChipSubFamily = 15002 then
          TXREG = TX1REG
          RCREG = RC1REG
    end if

    if ChipSubFamily = 15003 then
          TXREG = TX1REG
          RCREG = RC1REG
    end if

    if ChipSubFamily = 15004 then
          TXREG = TX1REG
          RCREG = RC1REG
    end if

    // All 161xx chips
    if INT(ChipSubFamily/100)=161 then
          TXREG = TX1REG
          RCREG = RC1REG
    end if

    'CANBUS chip use the 'old' USART Interrupt bit for CANBUS interrupt, so, need to flag this to the HSERSEND
    CANBUSCHIP_SCRIPT = FALSE
    if ChipSubFamily = 16106 then
          CANBUSCHIP_SCRIPT = TRUE
    end if
    if ChipSubFamily = 16108 then
          CANBUSCHIP_SCRIPT = TRUE
    end if



    'Added to resolve 18FxxK40 register & Bit mappings
    '-----------------------------------
    if var(SP1BRGH) then
        SPBRGH = SP1BRGH
    end if

    if var(SP1BRGL) then
        SPBRGL = SP1BRGL
    end if

    if bt(RC1STA_OERR) then
        OERR1 = RC1STA_OERR
    end if

    'for second Comport
    if Var(RC2STA) then
        SPEN2 = RC2STA_SPEN
        CREN2 = RC2STA_CREN
        OERR2 = RC2STA_OERR
    end if

    if Var(TX2STA) then
      if Nobit(TXSTA2_SYNC) then
        TXSTA2_SYNC = SYNC_TX2STA
      end if
      if Nobit(TX2STA_SYNC) then
        TX2STA_SYNC = SYNC_TX2STA
      end if
      if NoBit(TXEN2) then
        TXEN2 = TX2STA_TXEN
      end if
    end if

    if Var(RC3STA) then
        SPEN3 = RC3STA_SPEN
        CREN3 = RC3STA_CREN
        OERR3 = RC3STA_OERR
    end if

    if Var(TX3STA) then
      if Nobit(TXSTA3_SYNC) then
        TXSTA3_SYNC = SYNC_TX3STA
      end if
      if Nobit(TX3STA_SYNC) then
        TX3STA_SYNC = SYNC_TX3STA
      end if
      if NoBit(TXEN3) then
        TXEN3 = TX3STA_TXEN
      end if
    end if

    if Var(RC4STA) then
        SPEN4 = RC4STA_SPEN
        CREN4 = RC4STA_CREN
        OERR4 = RC4STA_OERR
    end if

    if Var(TX4STA) then
      if Nobit(TXSTA4_SYNC) then
        TXSTA4_SYNC = SYNC_TX4STA
      end if
      if Nobit(TX4STA_SYNC) then
        TX4STA_SYNC = SYNC_TX4STA
      end if
      if NoBit(TXEN4) then
        TXEN4 = TX4STA_TXEN
      end if
    end if

    if Var(RC5STA) then
        SPEN5 = RC5STA_SPEN
        CREN5 = RC5STA_CREN
        OERR5 = RC5STA_OERR
    end if

    if Var(TX5STA) then
      if Nobit(TXSTA5_SYNC) then
        TXSTA5_SYNC = SYNC_TX5STA
      end if
      if Nobit(TX5STA_SYNC) then
        TX5STA_SYNC = SYNC_TX5STA
      end if
      if NoBit(TXEN5) then
        TXEN5 = TX5STA_TXEN
      end if
    end if


   '---------------------------------------

    'Added to resolve new chip bit mapping
    If bit(SYNC_TX1STA) then
        SYNC = SYNC_TX1STA
    end if

    'Added USARTHasDataSet to work around the evaluation issue in If USARTHasData Then
    USARTHasDataSet = 0

    If Bit(RCIF) Then
      USARTHasData = "RCIF = On"
      USARTHasDataSet = 1
    End If

    If Bit(RC1IF) Then
      If USARTHasDataSet = 0 then
        USARTHasData = "RC1IF = On"
      End If
   End If

    If Bit(RC2IF) Then
      USART2HasData = "RC2IF = On"
    End If

    If Bit(RC3IF) Then
      USART3HasData = "RC3IF = On"
    End If

    If Bit(RC4IF) Then
      USART4HasData = "RC4IF = On"
    End If

    If Bit(RC5IF) Then
      USART5HasData = "RC5IF = On"
    End If


    If bit(U1RXIF) Then
      USARTHasData = "U1RXIF = On"
    End If

    If bit(U2RXIF) Then
      USART2HasData = "U2RXIF = On"
    End If

    If bit(U3RXIF) Then
      USART3HasData = "U3RXIF = On"
    End If

    If bit(U4RXIF) Then
      USART4HasData = "U4RXIF = On"
    End If

    If bit(U5RXIF) Then
      USART5HasData = "U5RXIF = On"
    End If

   ' USART 1 baud calculation
    If USART_BAUD_RATE Then
      'Find best baud rate
      If Bit(BRG16) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP = ChipMHz / USART_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP = 1
        BRG16_TEMP = 1

        If SPBRG_TEMP > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP = ChipMHz / USART_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP = 0
          BRG16_TEMP = 1
          If SPBRG_TEMP < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP = 1
            BRG16_TEMP = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP > 65535 Then
            Error Msg(UsartBaudTooLow)
          End If
        End If
      End If

      If NoBit(BRG16) Then

         'Try /16
        SPBRG_TEMP = ChipMHz / USART_BAUD_RATE / 16 * 1000000 - 1


        BRGH_TEMP = 1
        'If too high, try /64
        If SPBRG_TEMP > 255 Then
          SPBRG_TEMP = ChipMHz / USART_BAUD_RATE / 64 * 1000000 - 1
          BRGH_TEMP = 0
          'If still too high, error
          If SPBRG_TEMP > 255 Then
            Error Msg(UsartBaudTooLow)
          End If
        End If

      End If

      If Bit(U1BRGS) Then    'New 2017 UART module
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP = ChipMHz / USART_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP = 1
        BRGS1_SCRIPT = 1


        If SPBRG_TEMP > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP = ChipMHz / USART_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP = 0
          BRGS1_SCRIPT = 1
          If SPBRG_TEMP < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP = 1
            BRGS1_SCRIPT = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP > 65535 Then
            Error Msg(UsartBaudTooLow)
          End If
        End If


      End If

      'Get high and low bytes
        SPBRGL_TEMP = Int(SPBRG_TEMP) And 255
        SPBRGH_TEMP = Int((SPBRG_TEMP) / 256)

      'adjustments
      IF USART_BAUD_RATE = 256000 then
        SPBRGL_TEMP = Int(SPBRG_TEMP+1) And 255
        SPBRGH_TEMP = Int((SPBRG_TEMP+1) / 256)
      END IF

      'adjustments
      IF USART_BAUD_RATE = 115200 AND ChipMHz < 8 then
        SPBRGL_TEMP = Int(SPBRG_TEMP+1) And 255
        SPBRGH_TEMP = Int((SPBRG_TEMP+1) / 256)
      END IF

      'adjustments
      IF USART_BAUD_RATE = 57600 AND ChipMHz < 8 then
        SPBRGL_TEMP = Int(SPBRG_TEMP+1) And 255
        SPBRGH_TEMP = Int((SPBRG_TEMP+1) / 256)
      END IF

      If ISSUE_CHECK_USART_BAUD_RATE_WARNING then

          If BRGH_TEMP = 1 Then
            RoundResult = INT ( ( ChipMHz * 1000000 ) / ( 4 * ( int(SPBRG_TEMP) + 1 )) )
          end if
          If BRGH_TEMP = 0 Then
            RoundResult = INT ( ( ChipMHz * 1000000 ) / ( 16 * ( int(SPBRG_TEMP) + 1 )) )
          end if
          warning "Baud rate = " RoundResult

      end if


      if CHECK_USART_BAUD_RATE then

          If BRGH_TEMP = 1 Then
            RoundResult = INT ( ( ChipMHz * 1000000 ) / ( 4 * ( int(SPBRG_TEMP) + 1 )) )
          end if
          If BRGH_TEMP = 0 Then
            RoundResult = INT ( ( ChipMHz * 1000000 ) / ( 16 * ( int(SPBRG_TEMP) + 1 )) )
          end if

          'What is the error %
          if RoundResult > USART_BAUD_RATE then
              resultingUSART_BAUD_RATE = RoundResult - USART_BAUD_RATE

          end if
          if RoundResult < USART_BAUD_RATE then
              resultingUSART_BAUD_RATE = USART_BAUD_RATE - RoundResult

          end if

          resulting_error = int( ( resultingUSART_BAUD_RATE * 100 ) / USART_BAUD_RATE)
          RoundResult = int( USART_BAUD_RATE * ( 1 + ( resulting_error / 100 )))

          issuederrormessage = 0
          if resulting_error > 2 then

              Error "Big error in baudrate - percentage error =" resulting_error " @ " RoundResult
              issuederrormessage = 1
          end if

          if ISSUE_CHECK_USART_BAUD_RATE_WARNING then
            if resulting_error > 2 then
                If issuederrormessage = 0 then
                  Warning "Baudrate not exact.  Error percentage = "  resulting_error " @ " RoundResult
                end if
            end if
          end if

          'end of error rate check
      end if


      IF USART_BAUD_RATE then
          IF SPBRGL_TEMP < 0 then
              warning Chip frequency to slow for desired baud rate
          end if
      end if


      'added 02.10.2015 to resolve PIC devices without SPBRG define.  SPBRG needs to map to SPBRGL
      'fixes the list of known devices as follows
      '                16f1454.dat
      '                16f1455.dat
      '                16f1458.dat
      '                16f1459.dat
      '                16f1508.dat
      '                16f1509.dat
      if  NoVar(SPBRG) then
        SPBRG = SPBRGL
      end if

    End If

    'USART2
    If USART2_BAUD_RATE Then

      'Find best baud rate
      If Bit(BAUDCON2_BRG16) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP2 = ChipMHz / USART2_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP2 = 1
        BRG16_TEMP2 = 1

        If SPBRG_TEMP2 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP2 = ChipMHz / USART2_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP2 = 0
          BRG16_TEMP2 = 1
          If SPBRG_TEMP2 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP2 = 1
            BRG16_TEMP2 = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP2 > 65535 Then
            Error Msg(UsartBaud2_TooLow)
          End If
        End If
      End If

      If Bit(U2BRGS) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP2 = ChipMHz / USART2_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP2 = 1
        BRGS2_SCRIPT = 1

        If SPBRG_TEMP2 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP2 = ChipMHz / USART2_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP2 = 0
          BRGS2_SCRIPT = 1
          If SPBRG_TEMP2 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP2 = 1
            BRGS2_SCRIPT = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP2 > 65535 Then
            Error Msg(UsartBaud2_TooLow)
          End If
        End If


      End If
      'Get high and low bytes
      SPBRGL_TEMP2 = Int(SPBRG_TEMP2) And 255
      SPBRGH_TEMP2 = Int(SPBRG_TEMP2 / 256)

      'adjustments
      IF USART2_BAUD_RATE = 256000 then
        SPBRGL_TEMP2 = Int(SPBRG_TEMP2+1) And 255
        SPBRGH_TEMP2 = Int((SPBRG_TEMP2+1) / 256)
      END IF

    End If

    'USART3 Script support
    If USART3_BAUD_RATE Then
      'Find best baud rate
      If Bit(BAUDCON3_BRG16) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP3 = ChipMHz / USART2_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP3 = 1
        BRG16_TEMP3 = 1

        If SPBRG_TEMP3 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP3 = ChipMHz / USART3_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP3 = 0
          BRG16_TEMP3 = 1
          If SPBRG_TEMP3 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP3 = 1
            BRG16_TEMP3 = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP3 > 65535 Then
            Error Msg(UsartBaud3_TooLow)
          End If
        End If
      End If

      If Bit(U3BRGS) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP3 = ChipMHz / USART3_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP3 = 1
        BRGS3_SCRIPT = 1

        If SPBRG_TEMP3 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP3 = ChipMHz / USART3_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP3 = 0
          BRGS3_SCRIPT = 1
          If SPBRG_TEMP3 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP3 = 1
            BRGS3_SCRIPT = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP3 > 65535 Then
            Error Msg(UsartBaud3_TooLow)
          End If
        End If

      End If
      'Get high and low bytes
      SPBRGL_TEMP3 = Int(SPBRG_TEMP3) And 255
      SPBRGH_TEMP3 = Int(SPBRG_TEMP3 / 256)

      'adjustments
      IF USART3_BAUD_RATE = 256000 then
        SPBRGL_TEMP3 = Int(SPBRG_TEMP3+1) And 255
        SPBRGH_TEMP3 = Int((SPBRG_TEMP3+1) / 256)
      END IF

    End If

    'USART4 script support
    If USART4_BAUD_RATE Then
      'Find best baud rate
      If Bit(BAUDCON4_BRG16) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP4 = ChipMHz / USART4_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP4 = 1
        BRG16_TEMP4 = 1

        If SPBRG_TEMP4 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP4 = ChipMHz / USART4_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP4 = 0
          BRG16_TEMP4 = 1
          If SPBRG_TEMP4 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP4 = 1
            BRG16_TEMP4 = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP4 > 65535 Then
            Error Msg(UsartBaud4_TooLow)
          End If
        End If
      End If

      If Bit(U4BRGS) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP4 = ChipMHz / USART4_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP4 = 1
        BRGS4_SCRIPT = 1

        If SPBRG_TEMP4 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP4 = ChipMHz / USART4_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP4 = 0
          BRGS4_SCRIPT = 1
          If SPBRG_TEMP4 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP4 = 1
            BRGS4_SCRIPT = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP4 > 65535 Then
            Error Msg(UsartBaud4_TooLow)
          End If
        End If

      End If
      'Get high and low bytes
      SPBRGL_TEMP4 = Int(SPBRG_TEMP4) And 255
      SPBRGH_TEMP4 = Int(SPBRG_TEMP4 / 256)

      'adjustments
      IF USART4_BAUD_RATE = 256000 then
        SPBRGL_TEMP4 = Int(SPBRG_TEMP4+1) And 255
        SPBRGH_TEMP4 = Int((SPBRG_TEMP4+1) / 256)
      END IF


    End If

    'USART5 script support
    If USART5_BAUD_RATE Then
      'Find best baud rate
      If Bit(BAUDCON5_BRG16) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP5 = ChipMHz / USART5_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP5 = 1
        BRG16_TEMP5 = 1

        If SPBRG_TEMP5 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP5 = ChipMHz / USART5_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP5 = 0
          BRG16_TEMP5 = 1
          If SPBRG_TEMP5 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP5 = 1
            BRG16_TEMP5 = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP5 > 65535 Then
            Error Msg(UsartBaud5_TooLow)
          End If
        End If
      End If

      If Bit(U5BRGS) Then
         'Try 16 bit /4 (H = 1, 16 = 1)
        SPBRG_TEMP5 = ChipMHz / USART5_BAUD_RATE / 4 * 1000000 - 1
        BRGH_TEMP5 = 1
        BRGS5_SCRIPT = 1

        If SPBRG_TEMP5 > 65535 Then
           'If too high, try 16 bit /16 (H = 0, 16 = 1)
          SPBRG_TEMP5 = ChipMHz / USART5_BAUD_RATE / 16 * 1000000 - 1
          BRGH_TEMP5 = 0
          BRGS5_SCRIPT = 1
          If SPBRG_TEMP5 < 256 Then
            'If low enough, use 8 bit /16 (H = 1, 16 = 0)
            BRGH_TEMP5 = 1
            BRGS5_SCRIPT = 0
          End If
          'If value is too high, baud rate is too low
          If SPBRG_TEMP5 > 65535 Then
            Error Msg(UsartBaud5_TooLow)
          End If
        End If

      End If
      'Get high and low bytes
      SPBRGL_TEMP5 = Int(SPBRG_TEMP5) And 255
      SPBRGH_TEMP5 = Int(SPBRG_TEMP5 / 256)

      'adjustments
      IF USART5_BAUD_RATE = 256000 then
        SPBRGL_TEMP5 = Int(SPBRG_TEMP5+1) And 255
        SPBRGH_TEMP5 = Int((SPBRG_TEMP5+1) / 256)
      END IF

    End If

  End If

  If AVR Then

    'Remap to the AVRrc methods()
    IF ChipFamily = 121 Then
        HSerGetNum = HSerGetNumRC
        HSerGetString = HSerGetStringRC
        HSerPrint = HSerPrintRC
        HSerPrintByteCRLF =HSerPrintByteCRLFRC
        HSerPrintCRLF = HSerPrintCRLFRC
        HSerSend = HSerSendRC
        HSerReceive = HSerReceiveRC
    End If

    'AVR Usart has data flags setup
    If Bit(RXC0) Then
      USARTHasData = "RXC0 = On"
    End If
    If NoBit(RXC0) Then
      USARTHasData = "RXC = On"
    End If
    If Bit(RXC1) Then
      USART2HasData = "RXC1 = On"
    End If
    If Bit(RXC2) Then
      USART3HasData = "RXC2 = On"
    End If
    If Bit(RXC3) Then
      USART4HasData = "RXC3 = On"
    End If

    'AVR USART 1 baud calc
    If USART_BAUD_RATE Then

      UBRR_TEMP = ChipMHz * 1000000 / (16 * USART_BAUD_RATE) - 1
      U2X0_TEMP = 0

      'For 90S Series With 8-bit BRG
      IF VAR(UBRR) Then
         UBRR_TEMP = Int(UBRR_TEMP) And 255
      END IF

     'AVR Chips with 16-Bit BRG
     ''If using a high speed, use double speed mode
      IF NOVAR(UBRR) Then ' must have UBRRH or UBRRxH

          If UBRR_TEMP < 16384 Then
            UBRR_TEMP = ChipMHz * 1000000 / (8 * USART_BAUD_RATE) - 1
            U2X0_TEMP = 1
          End If

          'Check that rate will work
          If UBRR_TEMP > 65535 Then
            Error Msg(UsartBaudTooLow)
          End If

          'Get high and low bytes
          UBRRL_TEMP = Int(UBRR_TEMP) And 255
          UBRRH_TEMP = Int(UBRR_TEMP / 256)
      END IF


    End If

    'AVR USART 2 baud calc
    If USART2_BAUD_RATE Then
      UBRR2_TEMP = ChipMHz * 1000000 / (16 * USART2_BAUD_RATE) - 1
      U2X02_TEMP = 0
      'If using a high speed, use double speed mode
      If UBRR2_TEMP < 16384 Then
        UBRR2_TEMP = ChipMHz * 1000000 / (8 * USART2_BAUD_RATE) - 1
        U2X02_TEMP = 1
      End If

      'Check that rate will work
      If UBRR2_TEMP > 65535 Then
        Error Msg(UsartBaudTooLow)
      End If

      'Get high and low bytes
      UBRRL2_TEMP = Int(UBRR2_TEMP) And 255
      UBRRH2_TEMP = Int(UBRR2_TEMP / 256)
    End If

    'Script support for AVR USART 3 baud calc
    If USART3_BAUD_RATE Then
      UBRR3_TEMP = ChipMHz * 1000000 / (16 * USART3_BAUD_RATE) - 1
      U2X03_TEMP = 0
      'If using a high speed, use double speed mode
      If UBRR3_TEMP < 16384 Then
        UBRR3_TEMP = ChipMHz * 1000000 / (8 * USART3_BAUD_RATE) - 1
        U2X03_TEMP = 1
      End If

      'Check that rate will work
      If UBRR3_TEMP > 65535 Then
        Error Msg(UsartBaudTooLow)
      End If

      'Get high and low bytes
      UBRRL3_TEMP = Int(UBRR3_TEMP) And 255
      UBRRH3_TEMP = Int(UBRR3_TEMP / 256)
    End If
    'AVR USART 4 baud clac
    If USART4_BAUD_RATE Then
      UBRR4_TEMP = ChipMHz * 1000000 / (16 * USART4_BAUD_RATE) - 1
      U2X04_TEMP = 0
      'If using a high speed, use double speed mode
      If UBRR4_TEMP < 16384 Then
        UBRR4_TEMP = ChipMHz * 1000000 / (8 * USART4_BAUD_RATE) - 1
        U2X04_TEMP = 1
      End If

      'Check that rate will work
      If UBRR4_TEMP > 65535 Then
        Error Msg(UsartBaudTooLow)
      End If

      'Get high and low bytes
      UBRRL4_TEMP = Int(UBRR4_TEMP) And 255
      UBRRH4_TEMP = Int(UBRR4_TEMP / 256)
    End If
  End If

  'Figure out if SerData needs to be set by DefaultUsartReturnValue
  SCRIPT_SET_DEFAULTUSART1RETURNVALUE = 0
  SCRIPT_SET_DEFAULTUSART2RETURNVALUE = 0
  SCRIPT_SET_DEFAULTUSART3RETURNVALUE = 0
  SCRIPT_SET_DEFAULTUSART4RETURNVALUE = 0
  SCRIPT_SET_DEFAULTUSART5RETURNVALUE = 0

  If USART_BAUD_RATE Then
    If NODEF(USART_BLOCKING) Then
      SCRIPT_SET_DEFAULTUSART1RETURNVALUE = 1
    End If
  End If
  If USART2_BAUD_RATE Then
    If NODEF(USART2_BLOCKING) Then
      SCRIPT_SET_DEFAULTUSART2RETURNVALUE = 1
    End If
  End If
  If USART3_BAUD_RATE Then
    If NODEF(USART3_BLOCKING) Then
      SCRIPT_SET_DEFAULTUSART3RETURNVALUE = 1
    End If
  End If
  If USART4_BAUD_RATE Then
    If NODEF(USART4_BLOCKING) Then
      SCRIPT_SET_DEFAULTUSART4RETURNVALUE = 1
    End If
  End If
  If USART5_BAUD_RATE Then
    If NODEF(USART5_BLOCKING) Then
      SCRIPT_SET_DEFAULTUSART5RETURNVALUE = 1
    End If
  End If





#endscript

#startup InitUSART, 90

Sub InitUSART

  #IF CHIPUSART > 1
  'Set the default value for USART handler - required when more than one USART
  comport = 1
  #ENDIF

  #IFNDEF ONEOF(USART_BAUD_RATE,USART1_BAUD_RATE,USART2_BAUD_RATE,USART3_BAUD_RATE,USART4_BAUD_RATE,USART5_BAUD_RATE)
    #IFNDEF STOPCOMPILERERRORHANDLER
      //! Your program has a reference to Serial opertions however the Serial setup has not been completed.
      //!
      //! You should add the constants like those shown below. 
      //!
      //! #DEFINE USART_BAUD_RATE 9600
      //! #DEFINE USART_TX_BLOCKING
      //! #DEFINE USART_DELAY OFF
      //! 
      //! Please close this library once you have resolved or remove the Serial setup or commands respectively.
      //! 
                      RaiseCompilerError "%USART_NO_BAUD_RATE%"   'uses messages.dat
    #ENDIF
  #ENDIF

  #IFDEF USART2_BAUD_RATE
    #IFNDEF ONEOF(CHIP_MEGA8U4,CHIP_MEGA16U4,CHIP_MEGA32U4)  //~These chips only have one USART - and, that is USART2. So, they need a little help
      #IF CHIPUSART < 2
        RaiseCompilerError "%NO_USART2%"
      #ENDIF
    #ENDIF
  #ENDIF

  #IFDEF USART3_BAUD_RATE
    #IF CHIPUSART < 3
      RaiseCompilerError "%NO_USART3%"
    #ENDIF
  #ENDIF

  #IFDEF USART4_BAUD_RATE
    #IF CHIPUSART < 4
      RaiseCompilerError "%NO_USART4%"
    #ENDIF
  #ENDIF

  #IFDEF USART5_BAUD_RATE
    #IF CHIPUSART < 5
      RaiseCompilerError "%NO_USART5%"
    #ENDIF
  #ENDIF

  #ifdef PIC
      #If USART_BAUD_RATE Then
        'PIC USART 1 Init

          #ifndef Bit(TXEN1)
              #ifndef var(U1BRGH)
                  #ifndef Bit(BRG16)
                    #ifdef VAR(SPBRG)
                      'Set baud rate for legacy chips
                      //~ therefore no BRG16 bit
                      SPBRG = SPBRGL_TEMP
                    #endif
                  #else
                    'Set baud rate for chips with BRG16 bit
                    SPBRGH = SPBRGH_TEMP
                    #ifdef var(SPBRGL)
                      SPBRGL = SPBRGL_TEMP
                    #endif

                    #ifdef var(SPBRG)
                      SPBRG = SPBRGL_TEMP
                    #endif

                    //~ BRG16: 16-bit Baud Rate Generator bit
                    //~ 1 = bsf - 16-bit Baud Rate Generator is used
                    //~ 0 = bcf - 8-bit Baud Rate Generator is used
                    BRG16 = BRG16_TEMP
                  #endif
                  //~ Set High Baud Rate Select bit
                  BRGH = BRGH_TEMP
              #endif

              #ifdef var(U1BRGH)
                  //~Set baud rate for for 18fxxK42/K83 series UART
                  U1BRGH=SPBRGH_TEMP
                  U1BRGL=SPBRGL_TEMP
                  U1BRGS = BRGS1_SCRIPT
                  U1TXEN=1   'Enable TX1
                  U1RXEN=1   'Enable RX1
                  ON_U1CON1=1 'Enable USART1
              #endif

             #IFNDEF var(U1CON0)
              //~ Enable async and TX mode
              //~ Changed to canskip to silently exit when no USART
              //~ Changed to canskip to silently exit when no USART
              #IFDEF bit(SYNC)
                SYNC=0
              #ENDIF
              #IFDEF bit(TXEN)
                TXEN=1
              #ENDIF

             #ENDIF

             #ifdef bit(SPEN)
                  SPEN=1
              #endif

              #if var(RCSTA)                     //~ Isolate EN to RCSTA 
                #ifdef bit(CREN)                 'Enable Continuous Receive Enable bit
                    CREN=1
                #endif
              #endif

          #endif

          #ifdef Bit(TXEN1)
            'Set baud rate
            SPBRG1 = SPBRGL_TEMP
            #ifdef Bit(BRG16)
              SPBRGH1 = SPBRGH_TEMP
              BAUDCON1.BRG16 = BRG16_TEMP
            #endif

            #ifdef Var(TX1STA)
              //~chips with TXSTA1 instead of TX1STA
              TX1STA.BRGH = BRGH_TEMP
            #else
              #ifdef  Var(TXSTA1)
                TXSTA1.BRGH = BRGH_TEMP
              #Else
                RaiseCompilerError "Unhandled USART1 setup - report to GCBASIC Forum"
              #endif
            #endif

            'Enable async mode
            Set SYNC1 Off
            Set SPEN1 On

            'Enable Continuous Receive and Transmit Enable bit
            Set CREN1 On
            Set TXEN1 On
          #endif
      #endif

      #if USART2_BAUD_RATE Then
       'PIC USART 2 Init

          #ifdef Bit(TXSTA2_TXEN)
          'Set baud rate
            SPBRG2 = SPBRGL_TEMP2
            #ifdef Bit(BAUDCON2_BRG16)
              SPBRGH2 = SPBRGH_TEMP2
              BAUDCON2_BRG16 = BRG16_TEMP2
            #endif

            TXSTA2_BRGH = BRGH_TEMP2

          'Enable async mode
            Set TXSTA2_SYNC Off
            Set SPEN2 On  ' SPEN2

            'Enable Continuous Receive and Transmit Enable bit
            Set CREN2 On
            Set TXEN2 On
          #endif

          #ifdef Bit(TX2STA_TXEN)

               //~ Prevent Double Init if chip datfile has both TXSTA2_TXEN and TX2STA_TXEN bits defined
               //~ Affects Most chips with legacy type USART
               #ifndef Bit(TXSTA2_TXEN)

                   'Set baud rate
                    SPBRG2 = SPBRGL_TEMP2
                    #ifdef Bit(BAUD2CON_BRG16)
                      SPBRGH2 = SPBRGH_TEMP2
                      BAUD2CON_BRG16 = BRG16_TEMP2
                    #endif
                    TX2STA_BRGH = BRGH_TEMP2

                    'Enable async mode
                    Set TX2STA_SYNC Off
                    Set RC2STA_SPEN On

                    'Enable Continuous Receive and Transmit Enable bit
                    Set RC2STA_CREN On
                    Set TX2STA_TXEN On
               #endif

          #endif

          #ifdef Var(U2BRGH)
            //~ Supports 18fxxK42/K83 series UART
            U2BRGH=SPBRGH_TEMP2
            U2BRGL=SPBRGL_TEMP2
            U2BRGS = BRGS2_SCRIPT
            ON_U2CON1=1
            U2TXEN=1
            U2RXEN=1
          #endif
      #endif

      #if USART3_BAUD_RATE Then
          'PIC USART 3 InitUSART

          #ifdef Bit(TXSTA3_TXEN)
          'Set baud rate
            SPBRG3 = SPBRGL_TEMP3
            #ifdef Bit(BAUDCON3_BRG16)
              SPBRGH3 = SPBRGH_TEMP3
              BAUDCON3_BRG16 = BRG16_TEMP3
            #endif

            TXSTA3_BRGH = BRGH_TEMP3

          'Enable async mode
            Set TXSTA3_SYNC Off
            Set SPEN3 On  ' SPEN2

            'Enable Continuous Receive and Transmit Enable bit
            Set CREN3 On
            Set TXEN3 On
          #endif

          #ifdef Bit(TX3STA_TXEN)
          'Set baud rate
            SPBRG3 = SPBRGL_TEMP3
            #ifdef Bit(BAUD3CON_BRG16)
              SPBRGH3 = SPBRGH_TEMP3
              BAUD3CON_BRG16 = BRG16_TEMP3
            #endif
            TX3STA_BRGH = BRGH_TEMP3

            'Enable async mode
            Set TX3STA_SYNC Off
            Set RC3STA_SPEN On

            'Enable Continuous Receive and Transmit Enable bit
            Set RC3STA_CREN On
            Set TX3STA_TXEN On
          #endif

          #ifdef Var(U3BRGH)
            //~ Supports 18fxxK42/K83 series UART
            U3BRGH=SPBRGH_TEMP3
            U3BRGL=SPBRGL_TEMP3
            U3BRGS = BRGS3_SCRIPT
            ON_U3CON1=1
            U3TXEN=1
            U3RXEN=1
          #endif
      #endif

      #if USART4_BAUD_RATE Then
          ' PIC USART 4 InitUSART

          #ifdef Bit(TXSTA4_TXEN)
          'Set baud rate
            SPBRG4 = SPBRGL_TEMP4
            #ifdef Bit(BAUDCON4_BRG16)
              SPBRGH4 = SPBRGH_TEMP4
              BAUDCON4_BRG16 = BRG16_TEMP4
            #endif

            TXSTA4_BRGH = BRGH_TEMP4

          'Enable async mode
            Set TXSTA4_SYNC Off
            Set SPEN4 On  ' SPEN2

            'Enable Continuous Receive and Transmit Enable bit
            Set CREN4 On
            Set TXEN4 On
          #endif

          #ifdef Bit(TX4STA_TXEN)
          'Set baud rate
            SPBRG4 = SPBRGL_TEMP4
            #ifdef Bit(BAUD4CON_BRG16)
              SPBRGH4 = SPBRGH_TEMP4
              BAUD4CON_BRG16 = BRG16_TEMP4
            #endif
            TX4STA_BRGH = BRGH_TEMP4

            'Enable async mode
            Set TX4STA_SYNC Off
            Set RC4STA_SPEN On

            'Enable Continuous Receive and Transmit Enable bit
            Set RC4STA_CREN On
            Set TX4STA_TXEN On
          #endif

          #ifdef Var(U4BRGH)
            //~For 18fxxK42/K84 type of UART
            U4BRGH=SPBRGH_TEMP4
            U4BRGL=SPBRGL_TEMP4
            U4BRGS = BRGS4_SCRIPT
            ON_U4CON1=1
            U4TXEN=1
            U4RXEN=1
          #endif
      #endif

      #if USART5_BAUD_RATE Then
          'PIC USART 5 InitUSART

          #ifdef Bit(TXSTA5_TXEN)
          'Set baud rate
            SPBRG5 = SPBRGL_TEMP5
            #ifdef Bit(BAUDCON5_BRG16)
              SPBRGH5 = SPBRGH_TEMP5
              BAUDCON5_BRG16 = BRG16_TEMP5
            #endif

            TXSTA5_BRGH = BRGH_TEMP5

          'Enable async mode
            Set TXSTA5_SYNC Off
            Set SPEN5 On  ' SPEN2

            'Enable Continuous Receive and Transmit Enable bit
            Set CREN5 On
            Set TXEN5 On
          #endif

          #ifdef Bit(TX5STA_TXEN)
          'Set baud rate
            SPBRG5 = SPBRGL_TEMP5
            #ifdef Bit(BAUD5CON_BRG16)
              SPBRGH5 = SPBRGH_TEMP5
              BAUD5CON_BRG16 = BRG16_TEMP5
            #endif
            TX5STA_BRGH = BRGH_TEMP5

            'Enable async mode
            Set TX5STA_SYNC Off
            Set RC5STA_SPEN On

            'Enable Continuous Receive and Transmit Enable bit
            Set RC5STA_CREN On
            Set TX5STA_TXEN On
          #endif

          #ifdef Var(U5BRGH)
            //~For 18fxxK42/K84 type of UART
            U5BRGH=SPBRGH_TEMP5
            U5BRGL=SPBRGL_TEMP5
            U5BRGS = BRGS5_SCRIPT
            ON_U5CON1=1
            U5TXEN=1
            U5RXEN=1
          #endif
      #endif

  #endif

  #ifdef AVR
    #If USART_BAUD_RATE Then

      'Set baud rate
      #ifndef Bit(U2X0)

        #ifndef Bit(U2X1)       'Check for second usart? why

          #IFDEF Bit(U2X)
             U2X = U2X0_TEMP       'Set/Clear bit to double USART transmission speed
             UBRRL = UBRRL_TEMP
             UBRRH = UBRRH_TEMP
          #ENDIF

          //~Added to support 90S Series AVR with no UBRRL/H or UBRRxL/xH
          #IFDEF VAR(UBRR)
             UBRR = UBRR_TEMP
          #ENDIF

        #endif
      #endif


      #ifdef Bit(U2X0)
        U2X0 = U2X0_TEMP        'Set/Clear bit to double USART transmission speed
        UBRR0L = UBRRL_TEMP
        UBRR0H = UBRRH_TEMP
      #endif

      'Enable TX and RX
      #ifndef Bit(RXEN0)
        RXEN = On
        TXEN = On
      #endif
      #ifdef Bit(RXEN0)
        RXEN0 = On
        TXEN0 = On
      #endif
    #endif

    #If USART2_BAUD_RATE Then
        ' set baud reg
       #ifdef Bit(U2X1)           'multiple port chips
          U2X1 = U2X02_TEMP       'speed doubling bit ex:Mega 128,2560
          UBRR1L = UBRRL2_TEMP    'baudrate register low
          UBRR1H = UBRRH2_TEMP    'baudrate register h
       #endif
       'Enable TX and RX
       #ifdef Bit(RXEN1)
        RXEN1 = On
        TXEN1 = On
       #endif
    #endif

    #If USART3_BAUD_RATE Then
        ' set baud reg
       #ifdef Bit(U2X2)       'Multiple port chips
          U2X2 = U2X03_TEMP     'speed doubling bit ex:Mega 128,2560
          UBRR2L = UBRRL3_TEMP    'baudrate register low
          UBRR2H = UBRRH3_TEMP    'baudrate register h
       #endif
       'Enable TX and RX
       #ifdef Bit(RXEN2)
        RXEN2 = On
        TXEN2 = On
       #endif
    #endif

    #If USART4_BAUD_RATE Then
        ' set baud reg
       #ifdef Bit(U2X3)       'Multiple port chips
          U2X3 = U2X04_TEMP     'speed doubling bit ex:Mega 128,2560
          UBRR3L = UBRRL4_TEMP    'baudrate register low
          UBRR3H = UBRRH4_TEMP    'baudrate register h
       #endif
       'Enable TX and RX
       #ifdef Bit(RXEN3)
        RXEN3 = On
        TXEN3 = On
       #endif
    #endif

  #endif
End Sub

sub HSerSend (In SerData)

 #ifdef PIC
    #If USART_BAUD_RATE Then

      //~Ensure specific 18F are set to digital port prior to USART usage
      #Ifdef ChipFamily 16 
        #ifdef bit(PCFG6) 
          PCFG6 = 1
        #endif
        #ifdef bit(PCFG5) 
          PCFG5 = 1
        #endif                
      #Endif

      //~Registers/Bits determined by #samevar at top of library

          #ifdef USART_TX_BLOCKING
            'USART_TX_BLOCKING

              #IF CANBUSCHIP_SCRIPT = TRUE
                //~ family 18fxx84 chips have TXIF as CANBUS Interrupt and we need to isolate TX1F from USART and use U1TXIF. Cannot use samebit as this would break any CANBUS usage
                Wait While U1TXIF = Off
              #endif

              #IF CANBUSCHIP_SCRIPT = FALSE
                #ifdef Bit(TXIF)
                  Wait While TXIF = Off
                #endif
                #ifdef Bit(TRMT)
                  //~Ensure any previous operation has completed
                  Wait until TRMT = 1
                #endif
            #endif
          #endif

          #ifdef USART_BLOCKING

            #ifndef USART_TX_BLOCKING //~The ifndef tests ensure the only one of USART_BLOCKING or USART_TX_BLOCKING is implemented
              #IF CANBUSCHIP_SCRIPT = TRUE
                //~ family 18fxx84 chips have TXIF as CANBUS Interrupt and we need to isolate TX1F from USART and use U1TXIF. Cannot use samebit as this would break any CANBUS usage
                Wait While U1TXIF = Off
              #endif

              #IF CANBUSCHIP_SCRIPT = FALSE
                //~USART_BLOCKING and NOT USART_TX_BLOCKING
                  #ifdef Bit(TXIF)
                    Wait While TXIF = Off
                  #endif
                  #ifdef Bit(TRMT)
                    //~Ensure any previous operation has completed
                    Wait until TRMT = 1
                  #endif
              #endif
            #endif
          #endif

          #ifdef Var(TXREG)
              //~Write The Data Byte To The Usart
              //~Sets register to value of SerData - where register could be TXREG or TXREG1 or U1TXB set via the #samevar
              TXREG = SerData

              #IF USART_DELAY <> OFF
                  //~ All bits shifted out on TX Pin
                  Wait USART_DELAY
              #ENDIF

          #endif

    #endif

  #endif

  #ifdef AVR
   'AVR USART1 Send
    #If USART_BAUD_RATE Then


        #ifdef USART_BLOCKING
          #ifdef Bit(UDRE0)
            Wait While UDRE0 = Off    'Blocking Both Transmit buffer empty ,ready for data
          #endif

          #ifndef Bit(UDRE0)
            Wait While UDRE = Off
          #endif
        #endif

        #ifdef  USART_TX_BLOCKING
          #ifdef Bit(UDRE0)
            Wait While UDRE0 = Off    'Blocking Transmit buffer empty ,ready for data
          #endif

          #ifndef Bit(UDRE0)
            Wait While UDRE = Off
          #endif
        #endif

        #ifdef Var(UDR) ' ***************
          UDR = SerData
        #endif

        #ifdef Var(UDR0)
          UDR0 = SerData ' *******************
        #endif

    #endif

  #endif

end sub


sub HSerSend (In SerData, optional In comport = 1)

 #ifdef PIC

    //~There is SELECT-CASE to improve performance and to only include the CASEs when more than one USART is use.
    #if SCRIPT_USART_USAGE_CHECK > 1 Then
    Select Case comport
    #endif

    #If USART_BAUD_RATE Then
      HSerSendUSART1Handler:

      //~Ensure specific 18F are set to digital port prior to USART usage
      #Ifdef ChipFamily 16 
        #ifdef bit(PCFG6) 
          PCFG6 = 1
        #endif
        #ifdef bit(PCFG5) 
          PCFG5 = 1
        #endif                
      #Endif

      //~Registers/Bits determined by #samevar at top of library
       #if SCRIPT_USART_USAGE_CHECK > 1 Then
       Case 1
       #endif

          //~HANDLE USART1
          #ifdef USART_TX_BLOCKING
            'USART_TX_BLOCKING
            #ifdef Bit(TXIF)
              #IF CANBUSCHIP_SCRIPT = TRUE
                //~ family 18fxx84 chips have TXIF as CANBUS Interrupt and we need to isolate TX1F from USART and use U1TXIF. Cannot use samebit as this would break any CANBUS usage
                Wait While U1TXIF = Off
              #endif

              #IF CANBUSCHIP_SCRIPT = FALSE
                Wait While TXIF = Off
              #endif

            #endif
            #ifdef Bit(TRMT)
              //~Ensure any previous operation has completed
              Wait until TRMT = 1
            #endif
          #endif

          #ifdef USART_BLOCKING

            #ifndef USART_TX_BLOCKING //~The ifndef tests ensure the only one of USART_BLOCKING or USART_TX_BLOCKING is implemented
                'USART_BLOCKING and NOT USART_TX_BLOCKING

              #IF CANBUSCHIP_SCRIPT = TRUE
                //~ family 18fxx84 chips have TXIF as CANBUS Interrupt and we need to isolate TX1F from USART and use U1TXIF. Cannot use samebit as this would break any CANBUS usage
                Wait While U1TXIF = Off
              #endif

              #IF CANBUSCHIP_SCRIPT = FALSE
                #ifdef Bit(TXIF)
                  Wait While TXIF = Off
                #endif
                #ifdef Bit(TRMT)
                  //~Ensure any previous operation has completed
                  Wait until TRMT = 1
                #endif
              #endif

            #endif

          #endif

          #ifdef Var(TXREG)

              //~WRITE THE DATA BYTE TO THE USART
              //~ Sets register to value of SerData - where register could be TXREG or TXREG1 or U1TXB set via the #samevar
              TXREG = SerData

              #IF USART_DELAY <> OFF
                  //~ All bits shifted out on TX Pin
                  Wait USART_DELAY
              #ENDIF

          #endif

    #endif

    #If USART2_BAUD_RATE Then
       //~Registers/Bits determined by #samevar at top of file
       #if SCRIPT_USART_USAGE_CHECK > 1 Then
       HSerSendUSART2Handler:
       Case 2
       #endif

          //~HANDLE USART2
          #ifdef USART2_TX_BLOCKING     'Blocking TX
             #ifdef Bit(TX2IF)
                Wait While TX2IF = Off
             #endif
          #endif

          #ifdef USART2_BLOCKING
            #ifndef USART2_TX_BLOCKING   //~The ifndef tests ensure the only one of USART2_BLOCKING or USART2_TX_BLOCKING is implemented
                'USART2_BLOCKING and NOT USART2_TX_BLOCKING

                 #ifdef Bit(TX2IF)
                    Wait While TX2IF = Off
                 #endif
            #endif
          #endif

          #ifdef Var(TXREG2)
             //~WRITE THE DATA BYTE TO THE USART
             //~ Sets register to value of SerData - where register could be TX2REG, TXREG2  or U2TXB  via the #samevar
             #IFDEF VAR(TX2REG)
              TX2REG = SerData
             #ELSE
              TXREG2 = SerData
            #ENDIF

             #IF USART2_DELAY <> OFF
                //~ The USART_DELAY After all bits are shifted out
                Wait until TXSTA2_TRMT = 1
                //~ All bits shifted out on TX Pin
                Wait USART2_DELAY
             #ENDIF
          #endif

    #endif

    #If USART3_BAUD_RATE Then
       //~Registers/Bits determined by #samevar at top of file
       #if SCRIPT_USART_USAGE_CHECK > 1 Then
       HSerSendUSART3Handler:
       Case 3
       #endif

          //~HANDLE USART3
          #ifdef USART3_TX_BLOCKING     'Blocking TX
             #ifdef Bit(TX3IF)
                Wait While TX3IF = Off
             #endif
          #endif

          #ifdef USART3_BLOCKING
            #ifndef USART3_TX_BLOCKING   //~The ifndef tests ensure the only one of USART3_BLOCKING or USART3_TX_BLOCKING is implemented
                'USART3_BLOCKING and NOT USART3_TX_BLOCKING

                 #ifdef Bit(TX3IF)
                    Wait While TX3IF = Off
                 #endif
            #endif
          #endif

          #ifdef Var(TXREG3)
             //~WRITE THE DATA BYTE TO THE USART
             //~ Sets register to value of SerData - where register could be TX2REG, TXREG2  or U2TXB  via the #samevar
             TXREG3 = SerData

             #IF USART3_DELAY <> OFF
                //~ The USART_DELAY After all bits are shifted out
                Wait until TXSTA3_TRMT = 1
                //~ All bits shifted out on TX Pin
                Wait USART3_DELAY
             #ENDIF
          #endif

    #endif

    #If USART4_BAUD_RATE Then
       //~Registers/Bits determined by #samevar at top of file
       #if SCRIPT_USART_USAGE_CHECK > 1 Then
       HSerSendUSART4Handler:
       Case 4
       #endif

          //~HANDLE USART4
          #ifdef USART4_TX_BLOCKING     'Blocking TX
             #ifdef Bit(TX4IF)
                Wait While TX4IF = Off
             #endif
          #endif

          #ifdef USART4_BLOCKING
            #ifndef USART4_TX_BLOCKING   //~The ifndef tests ensure the only one of USART4_BLOCKING or USART4_TX_BLOCKING is implemented
                'USART4_BLOCKING and NOT USART4_TX_BLOCKING

                 #ifdef Bit(TX4IF)
                    Wait While TX4IF = Off
                 #endif
            #endif
          #endif

          #ifdef Var(TXREG4)
             //~WRITE THE DATA BYTE TO THE USART
             //~ Sets register to value of SerData - where register could be TX2REG, TXREG2  or U2TXB  via the #samevar
             TXREG4 = SerData

             #IF USART4_DELAY <> OFF
                //~ The USART_DELAY After all bits are shifted out
                Wait until TXSTA4_TRMT = 1
                //~ All bits shifted out on TX Pin
                Wait USART4_DELAY
             #ENDIF
          #endif

    #endif

    #If USART5_BAUD_RATE Then
       //~Registers/Bits determined by #samevar at top of file
       #if SCRIPT_USART_USAGE_CHECK > 1 Then
       HSerSendUSART5Handler:
       Case 5
       #endif

          //~HANDLE USART5
          #ifdef USART5_TX_BLOCKING     'Blocking TX
             #ifdef Bit(TX5IF)
                Wait While TX5IF = Off
             #endif
          #endif

          #ifdef USART5_BLOCKING
            #ifndef USART5_TX_BLOCKING   //~The ifndef tests ensure the only one of USART5_BLOCKING or USART5_TX_BLOCKING is implemented
                'USART5_BLOCKING and NOT USART5_TX_BLOCKING

                 #ifdef Bit(TX5IF)
                    Wait While TX5IF = Off
                 #endif
            #endif
          #endif

          #ifdef Var(TXREG5)
             // WRITE THE DATA BYTE TO THE USART
             //~ Sets register to value of SerData - where register could be TX5REG, TXREG5  or U5TXB  via the #samevar
             TXREG5 = SerData

             #IF USART5_DELAY <> OFF
                //~ The USART_DELAY After all bits are shifted out
                Wait until TXSTA5_TRMT = 1
                //~ All bits shifted out on TX Pin
                Wait USART5_DELAY
             #ENDIF
          #endif

    #endif

    #if SCRIPT_USART_USAGE_CHECK > 1 Then
    End Select
    #endif


  #endif



  #ifdef AVR
   //~ AVR USART1 Send
    #If USART_BAUD_RATE Then
      if comport = 1 Then

        #ifdef USART_BLOCKING
          #ifdef Bit(UDRE0)
            Wait While UDRE0 = Off    'Blocking Both Transmit buffer empty ,ready for data
          #endif

          #ifndef Bit(UDRE0)
            Wait While UDRE = Off
          #endif
        #endif

        #ifdef  USART_TX_BLOCKING
          #ifdef Bit(UDRE0)
            Wait While UDRE0 = Off    'Blocking Transmit buffer empty ,ready for data
          #endif

          #ifndef Bit(UDRE0)
            Wait While UDRE = Off
          #endif
        #endif

        #ifdef Var(UDR) ' ***************
          UDR = SerData
        #endif

        #ifdef Var(HIDETHESECOMMENTEDCODE)
        '#ifndef Var(UDR0)
        ' #ifdef Var(UDR1)
        '   UDR1 = SerData    '? second comport
        ' #endif
        '#endif
        #endif
        #ifdef Var(UDR0)
          UDR0 = SerData ' *******************
        #endif
      End If
    #endif

  ;----------------------------------------------------

    #If USART2_BAUD_RATE Then
   'AVR USART 2 send
      if comport = 2 Then
        #ifdef USART2_BLOCKING
          #ifdef Bit(UDRE1)       'comport 2 TX and Rxblocker
              Wait While UDRE1 = Off    'Transmit buffer empty ,ready for data
          #endif
        #endif

        #ifdef USART2_TX_BLOCKING
          #ifdef Bit(UDRE1)       'comport 2 TX blocker
              Wait While UDRE1 = Off    'Transmit buffer empty ,ready for data
          #endif
        #endif

        #ifdef Var(UDR1)
          UDR1 = SerData ' *****************
        #endif

      End If
    #endif

    #If USART3_BAUD_RATE Then
   'AVR USART 3 send
      if comport = 3 Then
        #ifdef USART3_BLOCKING
          #ifdef Bit(UDRE2)       'comport 3 TX and Rx blocker
            Wait While UDRE2 = Off    'Transmit buffer empty ,ready for data
          #endif
        #endif

        #ifdef USART3_TX_BLOCKING
          #ifdef Bit(UDRE2)       'comport 3 TX blocker
            Wait While UDRE2 = Off    'Transmit buffer empty ,ready for data
          #endif
        #endif

        #ifdef Var(UDR2)
            UDR2 = SerData
        #endif
      End If
    #endif

    #If USART4_BAUD_RATE Then
  'AVR USART 3 send
      if comport = 4 Then
        #ifdef USART4_BLOCKING
          #ifdef Bit(UDRE3)       'comport 4 TX and RX  blocker
            Wait While UDRE3 = Off    'Transmit buffer empty ,ready for data
          #endif
        #endif

        #ifdef USART4_TX_BLOCKING
          #ifdef Bit(UDRE3)       'comport 4 TX blocker
            Wait While UDRE3 = Off    'Transmit buffer empty ,ready for data
          #endif
        #endif

        #ifdef Var(UDR3)
            UDR3 = SerData
        #endif

      End If
    #endif
  #endif
end sub

Function HSerReceive
  Comport = 1
  HSerReceive( SerData )
  HSerReceive = SerData
End Function

Function HSerReceive1
  Comport = 1
  HSerReceive( SerData )
  HSerReceive1 = SerData
End Function


Function HSerReceive2
  Comport = 2
  HSerReceive( SerData )
  HSerReceive2 = SerData
End Function


Function HSerReceiveFrom ( Optional in comport = 1 )
  Comport = comport
  HSerReceive( SerData )
  HSerReceiveFrom = SerData
End Function


Sub HSerReceive(Out SerData)

  #ifdef PIC

    HSerReceive1Handler:

      //~Ensure specific 18F are set to digital port prior to USART usage
      #Ifdef ChipFamily 16 
        #ifdef bit(PCFG6) 
          PCFG6 = 1
        #endif
        #ifdef bit(PCFG5) 
          PCFG5 = 1
        #endif                
      #Endif

    //~Only set the Serdata to DefaultUsartReturnValue if needed. See the script for the examimation criteria
    //~It has to check all 5 usarts as any or all could need the value setting.
    #IF SCRIPT_SET_DEFAULTUSART1RETURNVALUE = 1
      SerData = DefaultUsartReturnValue
    #ELSE
      #IF SCRIPT_SET_DEFAULTUSART2RETURNVALUE = 1
        SerData = DefaultUsartReturnValue
      #ELSE
        #IF SCRIPT_SET_DEFAULTUSART3RETURNVALUE = 1
          SerData = DefaultUsartReturnValue
        #ELSE
          #IF SCRIPT_SET_DEFAULTUSART4RETURNVALUE = 1
            SerData = DefaultUsartReturnValue
          #ELSE
            #IF SCRIPT_SET_DEFAULTUSART5RETURNVALUE = 1
              SerData = DefaultUsartReturnValue
            #ENDIF
          #ENDIF
        #ENDIF
      #ENDIF
    #ENDIF

    //~There is SELECT-CASE to improve performance and to only include the CASEs when more than one USART is use.
    #if SCRIPT_USART_USAGE_CHECK > 1 Then
    // Requires the byte variable `comport` to be set first by calling routines, this defaults to 1
    Select Case comport
    #endif

    #If USART_BAUD_RATE Then

      //~There is SELECT-CASE to improve performance and to only include the CASEs when more than one USART is use.
      #if SCRIPT_USART_USAGE_CHECK > 1 Then
       Case 1
      #endif

        #ifdef USART_BLOCKING
           'Get a byte from register, if interrupt flag is valid
           Wait Until USARTHasData
        #endif

        #ifdef var(U1RXB)

            #ifdef USART_BLOCKING
              SerData = U1RXB
            #else
              If USARTHasData Then
                SerData = U1RXB
              end if
            #endif

            U1RXEN = 1
            U1ERRIR=0
            if ( U1FERIF = 1 ) then
                'UART1 error - restart
                ON_U1CON1 = 0
                ON_U1CON1 = 1
            end if


        #endif


        #ifndef Var(RCREG1)

            #ifndef var(U1RXB)
              'Get a byte from register, if interrupt flag is valid
              #ifdef USART_BLOCKING
                  SerData = RCREG
              #else
                If USARTHasData Then
                  SerData = RCREG
                End if
              #endif

              'Clear error to ensure next in byte can happen
              If OERR Then
                Set CREN Off
                Set CREN On
              End If
          #endif
        #endif

        #ifdef Var(RCREG1)

          'Get a byte from register, if interrupt flag is valid
          #ifdef USART_BLOCKING
            SerData = RCREG1
          #else
            If USARTHasData Then
              SerData = RCREG1
            End if
          #endif

          #ifdef bit(OERR1)
          'Clear error to ensure next in byte can happen

            If OERR1 Then
              Set CREN1 Off
              Set CREN1 On
            End If

          #endif

          #Ifndef bit(OERR1) '  For Chips with RCREG1 but no OEER1

            #IFDEF Bit(OERR)
              IF OERR then
                Set CREN off
                Set CREN On
              END IF
            #ENDIF
          #ENDIF
        #endif
    #endif


    #If USART2_BAUD_RATE Then

      //~There is SELECT-CASE to improve performance and to only include the CASEs when more than one USART is use.
      #if SCRIPT_USART_USAGE_CHECK > 1 Then
      HSerReceive2Handler:
      Case 2
      #endif

        #ifdef USART2_BLOCKING
           Wait Until USART2HasData
        #endif

        #ifdef Var(RC2REG)

          #ifdef Bit(RC2STA_OERR)
              if  RC2STA_OERR = 1 Then
                  // EUSART5 error - restart
                  RC2STA_CREN = 0
                  RC2STA_CREN = 1 
              End IF
          #endif

         'Get a byte from register, if interrupt flag is valid
          If USART2HasData Then
             SerData = RC2REG
          End if

        #endif

        #ifdef var(U2RXB)
            U2RXEN = 1
            if ( U2FERIF = 1 ) then
                'UART2 error - clear the error
                ON_U2CON1 = 0
                ON_U2CON1 = 1
            end if
            SerData = U2RXB
        #endif

        #ifdef Var(RCREG2)
          'Get a byte from register, if interrupt flag is valid
          If USART2HasData Then
              SerData = RCREG2
          End if
        #endif

        #ifdef bit(OERR2)
          'Clear error to ensure next in byte can happen
            If OERR2 Then
              Set CREN2 Off
              Set CREN2 On
            End If
        #endif
    #endif

    #If USART3_BAUD_RATE Then

      //~There is SELECT-CASE to improve performance and to only include the CASEs when more than one USART is use.
      #if SCRIPT_USART_USAGE_CHECK > 1 Then
      HSerReceive3Handler:
      Case 3
      #endif

        #ifdef USART3_BLOCKING
           Wait Until USART3HasData
        #endif
        
        #ifdef var(U3RXB)
            U3RXEN = 1
            'If UART3 has an error - clear the error
            if ( U3FERIF = 1 ) then
                ON_U3CON1 = 0
                ON_U3CON1 = 1
            end if
            SerData = U3RXB
        #endif

        #ifdef Var(RCREG3)

          #ifdef Bit(RC3STA_OERR)
              if  RC3STA_OERR = 1 Then
                  // EUSART5 error - restart
                  RC3STA_CREN = 0
                  RC3STA_CREN = 1 
              End IF
          #endif

          'Get a byte from register, if interrupt flag is valid
          If USART3HasData Then
            SerData = RCREG3
          End if
        #endif

    #endif

    #If USART4_BAUD_RATE Then

      //~There is SELECT-CASE to improve performance and to only include the CASEs when more than one USART is use.
      #if SCRIPT_USART_USAGE_CHECK > 1 Then
      HSerReceive4Handler:
      Case 4
      #endif

        #ifdef USART4_BLOCKING
           Wait Until USART4HasData
        #endif

        #ifdef var(U4RXB)
            U4RXEN = 1
            'If UART4 has an error - clear the error
            if ( U4FERIF = 1 ) then
                ON_U4CON1 = 0
                ON_U4CON1 = 1
            end if
            SerData = U4RXB
        #endif

        #ifdef Var(RC4REG)

          #ifdef Bit(RC4STA_OERR)
              if  RC4STA_OERR = 1 Then
                  // EUSART5 error - restart
                  RC4STA_CREN = 0
                  RC4STA_CREN = 1 
              End IF
          #endif

          'Get a byte from register, if interrupt flag is valid
          If USART4HasData Then
            SerData = RC4REG
          End if
        #endif

    #endif

    #If USART5_BAUD_RATE Then

      //~There is SELECT-CASE to improve performance and to only include the CASEs when more than one USART is use.
      #if SCRIPT_USART_USAGE_CHECK > 1 Then
      HSerReceive5Handler:
      Case 5
      #endif

        #ifdef USART5_BLOCKING
           Wait Until USART5HasData
        #endif

        #ifdef Var(RC5REG)

          #ifdef Bit(RC5STA_OERR)
              if  RC5STA_OERR = 1 Then
                  // EUSART5 error - restart
                  RC5STA_CREN = 0
                  RC5STA_CREN = 1 
              End IF
          #endif

          'Get a byte from register, if interrupt flag is valid
          If USART5HasData Then
              SerData = RC5REG
          End if
        #endif

        #ifdef var(U5RXB)
            U5RXEN = 1
            'If UART5 has an error - clear the error
            if ( U5FERIF = 1 ) then
                ON_U5CON1 = 0
                ON_U5CON1 = 1
            end if
            SerData = U5RXB
        #endif

    #endif

    #if SCRIPT_USART_USAGE_CHECK > 1 Then
    End Select
    #endif

  #endif


  #ifdef AVR
    'AVR USART 1 receive
    #If USART_BAUD_RATE Then
      if comport = 1 Then
        SerData = DefaultUsartReturnValue
        'If set up to block, wait for data
        #ifdef USART_BLOCKING
        Wait Until USARTHasData
        #endif
        If USARTHasData Then
          #ifndef Var(UDR0)
            #ifdef Var(UDR1)
              SerData = UDR1
            #endif
            #ifndef Var(UDR1)
              SerData = UDR
            #endif
          #endif
          #ifdef Var(UDR0)
            SerData = UDR0
          #endif
        End If
      End If
    #endif


    #If USART2_BAUD_RATE Then
       'AVR USART 2 receive
        if comport = 2 Then
          SerData = DefaultUsartReturnValue
          'If set up to block, wait for data
          #ifdef USART2_BLOCKING
          Wait Until USART2HasData
          #endif
          If USART2HasData Then
            #ifdef Var(UDR1)
              SerData = UDR1
            #endif
          End If
        End If
    #endif


    #If USART3_BAUD_RATE Then
        'AVR USART 3 receive
        if comport = 3 Then
          SerData = DefaultUsartReturnValue
          'If set up to block, wait for data
          #ifdef USART3_BLOCKING
          Wait Until USART3HasData
          #endif
          If USART3HasData Then
            #ifdef Var(UDR2)
              SerData = UDR2
            #endif
          End If
        End If
      #endif


    #If USART4_BAUD_RATE Then
        'AVR USART 4 receive
        if comport = 4 Then
          SerData = DefaultUsartReturnValue
          'If set up to block, wait for data
          #ifdef USART4_BLOCKING
          Wait Until USART4HasData
          #endif
          If USART4HasData Then
            #ifdef Var(UDR3)
              SerData = UDR3
            #endif
          End If
        End If
      #endif
  #endif

End Sub


  'Added  11/4/2015 by mlo
  'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.

  ' A number is input to a USART as a series of ASCII digits with a CR at the end
  'Output Value is in range of 0 to 65535 (Dec)
  'Input value is entered as decimal digits
sub HSerGetNum (Out HSerNum As Word, optional In comport = 1)
  Dim HSerDataIn(5)
  HSerIndex = 0
  HSerNum = 0

  Do
    HSerReceive( HSerInByte )
    'Enter key?
    If HSerInByte = 13 OR HSerIndex >= 5 Then       ' ***** look for CR  OR digits >= 5****
      For HSerTemp = 1 to HSerIndex
        HSerNum = HSerNum * 10 + HSerDataIn(HSerTemp) - 48
      Next
      Exit Sub
    End If
    'Number?
    If HSerInByte >= 48 and HSerInByte <= 57 Then
        HSerIndex++
        HSerDataIn(HSerIndex) = HSerInByte
    End If
  Loop
End Sub

  'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.

  ' A number is input to a USART as a series of ASCII digits with a CR at the end
  'Output Value is in range of 0 to 99999 (Dec)
  'Input value is entered as decimal digits
sub HSerGetNum (Out HSerNum As Long, optional In comport = 1)
  Dim HSerDataIn(5)
  HSerIndex = 0
  HSerNum = 0

  Do
    HSerReceive( HSerInByte )
    'Enter key?
    If HSerInByte = 13 OR HSerIndex >= 5 Then       ' ***** look for CR  OR digits >= 5****
      For HSerTemp = 1 to HSerIndex
        HSerNum = HSerNum * 10 + HSerDataIn(HSerTemp) - 48
      Next

      Exit Sub
    End If

   'Number?
    If HSerInByte >= 48 and HSerInByte <= 57 Then
        HSerIndex++
        HSerDataIn(HSerIndex) = HSerInByte
    End If
  Loop
End Sub



  'Added  11/4/2015 by mlo
  'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.

  ' A string is input to a USART as a series of ASCII chars with a CR at the end
  'Output Value is a string
  'Input value is entered as digits,letters and most punctuation
sub HSerGetString (Out HSerString As String, optional In comport = 1)
  HSerIndex = 0
  Do
    comport = comport 'not really required but added for clarity
    HSerReceive ( HSerInByte )
    'Enter key?
    If HSerInByte = 13 Then
       Exit Sub
    End If
    'letters,numbers,punctuation
    If HSerInByte >= 32 and HSerInByte <= 126 Then
        HSerIndex++
        HSerString(HSerIndex) = HSerInByte
        HSerString(0) = HSerIndex
    End If
  Loop
End Sub

 'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.
sub HSerPrintStringCRLF (In PrintData As String, optional In comport = 1)

  PrintLen = PrintData(0)

  If PrintLen <> 0 then
    'Write Data
    for SysPrintTemp = 1 to PrintLen
      HSerSend(PrintData(SysPrintTemp),comport )
    next
  End If

  HSerSend(13,comport)
  HSerSend(10,comport)

End Sub

 'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.
sub HSerPrint (In PrintData As String, optional In comport = 1)

  PrintLen = PrintData(0)

  If PrintLen <> 0 then
    'Write Data
    for SysPrintTemp = 1 to PrintLen
      HSerSend(PrintData(SysPrintTemp),comport )
    next
  End If

  'CR
  'All Usarts
  #IFDEF SerPrintCR
    HSerSend(13,comport)
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10,comport)
  #ENDIF
 '--------------------------

  'Specific USARTs
  #IFDEF HSerPrintCR1
    HSerSend(13,1)
  #ENDIF

  #IFDEF HSerPrintLF1
    HSerSend(10,1)
  #ENDIF

   #IFDEF HSerPrintCR2
    HSerSend(13,2)
  #ENDIF

  #IFDEF HSerPrintLF2
    HSerSend(10,2)
  #ENDIF

  #IFDEF HSerPrintCR3
    HSerSend(13,3)
  #ENDIF

  #IFDEF HSerPrintLF3
    HSerSend(10,3)
  #ENDIF

  #IFDEF HSerPrintCR4
    HSerSend(13,4)
  #ENDIF

  #IFDEF HSerPrintLF4
    HSerSend(10,4)
  #ENDIF

  #IFDEF HSerPrintCR5
    HSerSend(13,5)
  #ENDIF

  #IFDEF HSerPrintLF5
    HSerSend(10,5)
  #ENDIF
 '------------------

end sub

Sub HserSpace(IN Optional NumSpaces = 1, IN Optional Comport = 1)
   Repeat Numspaces
     Hsersend 32, Comport
   End Repeat
End Sub


 'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.
sub HSerPrint (In SerPrintVal, optional In comport = 1)

  OutValueTemp = 0

  IF SerPrintVal >= 100 Then
    OutValueTemp = SerPrintVal / 100
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48 ,comport )

  End If
  If OutValueTemp > 0 Or SerPrintVal >= 10 Then
    OutValueTemp = SerPrintVal / 10
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48 ,comport )
  End If
  HSerSend(SerPrintVal + 48 ,comport)

  'CR
  #IFDEF SerPrintCR
    HSerSend(13,comport)
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10,comport)
  #ENDIF

end sub

 'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.
Sub HSerPrint (In SerPrintVal As Word, optional In comport = 1)
  Dim SysCalcTempX As Word

  OutValueTemp = 0

  If SerPrintVal >= 10000 then
    OutValueTemp = SerPrintVal / 10000 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48 ,comport )
    Goto HSerPrintWord1000
  End If

  If SerPrintVal >= 1000 then
  HSerPrintWord1000:
    OutValueTemp = SerPrintVal / 1000 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48 ,comport  )
    Goto HSerPrintWord100
  End If

  If SerPrintVal >= 100 then
  HSerPrintWord100:
    OutValueTemp = SerPrintVal / 100 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48 ,comport )
    Goto HSerPrintWord10
  End If

  If SerPrintVal >= 10 then
  HSerPrintWord10:
    OutValueTemp = SerPrintVal / 10 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48 ,comport )
  End If

  OutValueTemp = SerPrintVal
  HSerSend(OutValueTemp + 48 ,comport  )

  'CR
  #IFDEF SerPrintCR
    HSerSend(13,comport)
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10,comport)
  #ENDIF

End Sub

 'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.
Sub HSerPrint (In SerPrintValInt As Integer, optional In comport = 1)
  Dim SerPrintVal As Word

  'If sign bit is on, print - sign and then negate
  If SerPrintValInt.15 = On Then
    HSerSend( 0x2D ,comport )

    SerPrintVal = -SerPrintValInt

  'Sign bit off, so just copy value
  Else
    SerPrintVal = SerPrintValInt
  End If

  'Use Print(word) to display value
  HSerPrint SerPrintVal,comport
End Sub

 'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.
Sub HSerPrint (In SerPrintVal As Long, optional In comport = 1)

  Dim SysCalcTempA As Long
  Dim SysPrintBuffer(10)
  SysPrintBuffLen = 0

  Do
    'Divide number by 10, remainder into buffer
    SysPrintBuffLen += 1
    SysPrintBuffer(SysPrintBuffLen) = SerPrintVal % 10
    SerPrintVal = SysCalcTempA
  Loop While SerPrintVal <> 0

  'Display
  For SysPrintTemp = SysPrintBuffLen To 1 Step -1
    HSerSend(SysPrintBuffer(SysPrintTemp) + 48, comport  )
  Next

  'CR
  #IFDEF SerPrintCR
    HSerSend(13 )
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10 )
  #ENDIF

End Sub

 'Revised 05/07/2020 to  duplicate method without the comport parameter.  Change this method and you must change the duplicate method - the one without  comport.
Sub HserPrintByteCRLF(In PrintValue,optional In comport =1)
  HSerPrint(PrintValue)
  HSerSend(13,comport)
  HSerSend(10,comport)
End Sub

Sub HserPrintCRLF  ( Optional in HSerPrintCRLFCount = 1,Optional In comport =1 )
    repeat HSerPrintCRLFCount
      HSerSend(13,comport)
      HSerSend(10,comport)
    end Repeat
End Sub


' NO COMPORT variable for these methods() below
'***********  same methods, as above methods, but duplicated to remove COMPORT

sub HSerSendRC(In SerData)

  #ifdef AVR
 'AVR USART1 Send
    #If USART_BAUD_RATE Then


        #ifdef USART_BLOCKING
          #ifdef Bit(UDRE0)
            Wait While UDRE0 = Off    'Blocking Both Transmit buffer empty ,ready for data
          #endif

          #ifndef Bit(UDRE0)
            Wait While UDRE = Off
          #endif
        #endif

        #ifdef  USART_TX_BLOCKING
          #ifdef Bit(UDRE0)
            Wait While UDRE0 = Off    'Blocking Transmit buffer empty ,ready for data
          #endif

          #ifndef Bit(UDRE0)
            Wait While UDRE = Off
          #endif
        #endif

        #ifdef Var(UDR) ' ***************
          UDR = SerData
        #endif

        #ifdef Var(UDR0)
          UDR0 = SerData ' *******************
        #endif

    #endif

  #endif
end sub


sub HSerGetNumRC (Out HSerNum As Word)
  Dim HSerDataIn(5)
  HSerIndex = 0
  HSerNum = 0

  Do
    HSerReceive( HSerInByte )
    'Enter key?
    If HSerInByte = 13 OR HSerIndex >= 5 Then       ' ***** look for CR  OR digits >= 5****
      For HSerTemp = 1 to HSerIndex
        HSerNum = HSerNum * 10 + HSerDataIn(HSerTemp) - 48
      Next
      Exit Sub
    End If
    'Number?
    If HSerInByte >= 48 and HSerInByte <= 57 Then
        HSerIndex++
        HSerDataIn(HSerIndex) = HSerInByte
    End If
  Loop
End Sub

' A number is input to a USART as a series of ASCII digits with a CR at the end
'Output Value is in range of 0 to 99999 (Dec)
'Input value is entered as decimal digits
sub HSerGetNumRC (Out HSerNum As Long)
  Dim HSerDataIn(5)
  HSerIndex = 0
  HSerNum = 0

  Do
    HSerReceive( HSerInByte )
    'Enter key?
    If HSerInByte = 13 OR HSerIndex >= 5 Then       ' ***** look for CR  OR digits >= 5****
      For HSerTemp = 1 to HSerIndex
        HSerNum = HSerNum * 10 + HSerDataIn(HSerTemp) - 48
      Next

      Exit Sub
    End If

   'Number?
    If HSerInByte >= 48 and HSerInByte <= 57 Then
        HSerIndex++
        HSerDataIn(HSerIndex) = HSerInByte
    End If
  Loop
End Sub


'Added  11/4/2015 by mlo
' A string is input to a USART as a series of ASCII chars with a CR at the end
'Output Value is a string
'Input value is entered as digits,letters and most punctuation
sub HSerGetStringRC (Out HSerString As String)
  HSerIndex = 0
  Do
    HSerReceive ( HSerInByte )
    'Enter key?
    If HSerInByte = 13 Then
       Exit Sub
    End If
    'letters,numbers,punctuation
    If HSerInByte >= 32 and HSerInByte <= 126 Then
        HSerIndex++
        HSerString(HSerIndex) = HSerInByte
        HSerString(0) = HSerIndex
    End If
  Loop
End Sub


sub HSerPrintStringCRLFRC (In PrintData As String)

  PrintLen = PrintData(0)

  If PrintLen <> 0 then
    'Write Data
    for SysPrintTemp = 1 to PrintLen
      HSerSend(PrintData(SysPrintTemp) )
    next
  End If

  HSerSend(13)
  HSerSend(10)

End Sub

sub HSerPrintRC (In PrintData As String)

  PrintLen = PrintData(0)

  If PrintLen <> 0 then
    'Write Data
    for SysPrintTemp = 1 to PrintLen
      HSerSend(PrintData(SysPrintTemp) )
    next
  End If

  'CR
  #IFDEF SerPrintCR
    HSerSend(13)
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10)
  #ENDIF
end sub



sub HSerPrintRC (In SerPrintVal)

  OutValueTemp = 0

  IF SerPrintVal >= 100 Then
    OutValueTemp = SerPrintVal / 100
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48  )

  End If
  If OutValueTemp > 0 Or SerPrintVal >= 10 Then
    OutValueTemp = SerPrintVal / 10
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48  )
  End If
  OutValueTemp = SerPrintVal
  HSerSend(OutValueTemp + 48 )

  'CR
  #IFDEF SerPrintCR
    HSerSend(13)
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10)
  #ENDIF

end sub

Sub HSerPrintRC (In SerPrintVal As Word)
  Dim SysCalcTempX As Word

  OutValueTemp = 0

  If SerPrintVal >= 10000 then
    OutValueTemp = SerPrintVal / 10000 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48  )
    Goto HSerPrintWord1000
  End If

  If SerPrintVal >= 1000 then
  HSerPrintWord1000:
    OutValueTemp = SerPrintVal / 1000 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48   )
    Goto HSerPrintWord100
  End If

  If SerPrintVal >= 100 then
  HSerPrintWord100:
    OutValueTemp = SerPrintVal / 100 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48  )
    Goto HSerPrintWord10
  End If

  If SerPrintVal >= 10 then
  HSerPrintWord10:
    OutValueTemp = SerPrintVal / 10 [word]
    SerPrintVal = SysCalcTempX
    HSerSend(OutValueTemp + 48  )
  End If
  OutValueTemp = SerPrintVal
  HSerSend(OutValueTemp + 48   )

  'CR
  #IFDEF SerPrintCR
    HSerSend(13)
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10)
  #ENDIF

End Sub

Sub HSerPrintRC (In SerPrintValInt As Integer)
  Dim SerPrintVal As Word

  'If sign bit is on, print - sign and then negate
  If SerPrintValInt.15 = On Then
    HSerSend("-" )

    SerPrintVal = -SerPrintValInt

  'Sign bit off, so just copy value
  Else
    SerPrintVal = SerPrintValInt
  End If

  'Use Print(word) to display value
  HSerPrint SerPrintVal
End Sub

Sub HSerPrintRC (In SerPrintVal As Long)

  Dim SysCalcTempA As Long
  Dim SysPrintBuffer(10)
  SysPrintBuffLen = 0

  Do
    'Divide number by 10, remainder into buffer
    SysPrintBuffLen += 1
    SysPrintBuffer(SysPrintBuffLen) = SerPrintVal % 10
    SerPrintVal = SysCalcTempA
  Loop While SerPrintVal <> 0

  'Display
  For SysPrintTemp = SysPrintBuffLen To 1 Step -1
    HSerSend(SysPrintBuffer(SysPrintTemp) + 48 )
  Next

  'CR
  #IFDEF SerPrintCR
    HSerSend(13 )
  #ENDIF

  #IFDEF SerPrintLF
    HSerSend(10 )
  #ENDIF

End Sub

Sub HserPrintByteCRLFRC(In PrintValue )
  HSerPrint(PrintValue)
  HSerSend(13)
  HSerSend(10)
End Sub

Sub HserPrintCRLFRC( Optional in HSerPrintCRLFCount = 1 )
    repeat HSerPrintCRLFCount
      HSerSend(13)
      HSerSend(10)
    end Repeat
End Sub


Function HSerReceiveRC
  HSerReceiveRC( SerData )
  HSerReceiveRC = SerData
End Function

Sub HSerReceiveRC(Out SerData)

  /* Removal of PIC section at build 1176 - to validate that this section is not required as HSerReceiveRC is for AVR family 121 chips ONLY - so, AVRs!!
    //~ Needs comport to be set first by calling routines
    #ifdef PIC

      #If USART_BAUD_RATE Then

        if comport = 1 Then

          SerData = DefaultUsartReturnValue

          #ifdef USART_BLOCKING
            'If set up to block, wait for data
            Wait Until USARTHasData
          #endif

          #ifdef var(U1RXB)

              If USARTHasData Then
                SerData = U1RXB
              end if

              U1RXEN = 1
              U1ERRIR=0
              if ( U1FERIF = 1 ) then
                  'UART1 error - restart
                  ON_U1CON1 = 0
                  ON_U1CON1 = 1
              end if


          #endif


          #ifndef Var(RCREG1)
              #ifndef var(U1RXB)
                'Get a byte from register, if interrupt flag is valid
                If USARTHasData Then
                  SerData = RCREG
                End if

                'Clear error to ensure next in byte can happen
                If OERR Then
                  Set CREN Off
                  Set CREN On
                End If
            #endif
          #endif

          #ifdef Var(RCREG1)
            'Get a byte from register, if interrupt flag is valid
            If USARTHasData Then
              SerData = RCREG1
            End if

            #ifdef bit(OERR1)
            'Clear error to ensure next in byte can happen

              If OERR1 Then
                Set CREN1 Off
                Set CREN1 On
              End If

            #endif

            #Ifndef bit(OERR1) '  For Chips with RCREG1 but no OEER1

              #IFDEF Bit(OERR)
                IF OERR then
                  Set CREN off
                  Set CREN On
                END IF
              #ENDIF

            #ENDIF
          #endif
        end if
      #endif


      #If USART2_BAUD_RATE Then
        'PIC USART 2 receive
        if comport = 2 Then
          SerData = DefaultUsartReturnValue

          #ifdef USART2_BLOCKING
            Wait Until USART2HasData
          #endif

          #ifdef Var(RC2REG)

          'Get a byte from register, if interrupt flag is valid
            If USART2HasData Then
              SerData = RC2REG
            End if

          #endif

          #ifdef var(U2RXB)
              U2RXEN = 1
              'Wait Until USARTHasData
              if ( U2FERIF = 1 ) then
                  'UART1 error - restart
                  ON_U2CON1 = 0
                  ON_U2CON1 = 1
              end if
              SerData = U2RXB
          #endif

          #ifdef Var(RCREG2)
            'Get a byte from register, if interrupt flag is valid
            If USART2HasData Then
              SerData = RCREG2
            End if
          #endif

          #ifdef bit(OERR2)
            'Clear error to ensure next in byte can happen
              If OERR2 Then
                Set CREN2 Off
                Set CREN2 On
              End If
          #endif
        end if
      #endif
    #endif
  */

    #ifdef AVR
      'AVR USART 1 receive
      #If USART_BAUD_RATE Then
          SerData = DefaultUsartReturnValue
          'If set up to block, wait for data
          #ifdef USART_BLOCKING
          Wait Until USARTHasData
          #endif
          If USARTHasData Then
            #ifndef Var(UDR0)
              #ifdef Var(UDR1)
                SerData = UDR1
              #endif
              #ifndef Var(UDR1)
                SerData = UDR
              #endif
            #endif
            #ifdef Var(UDR0)
              SerData = UDR0
            #endif
          End If
      #endif

    #endif

End Sub
