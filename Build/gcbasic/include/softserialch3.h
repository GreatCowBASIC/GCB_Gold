'    MicroChip specific to support Software Serial for GCBASIC
'    Copyright (C) 2017-2024 Frank Steinberg

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

'''Assembler-based Software Serial UART Routines for GCB
'''-----------------------------------------------------------------------------
''':
'''  This are channel 1 routines!
'''  See  SoftSerial.h  for more info!
''':
'''  Text-adjustments for channel 3:
'''  SerN -> Ser3  /  SERn_ -> SER3_
'''  STxN -> STx3  /  STXn_ -> STX3_
'''  SRxN -> SRx3  /  SRXn_ -> SRX3_
''':
'''@licence GPL
'''@version 1.21
'''@date    25.02.2017
'''******************************************************************************
'  20.02.2018     Updated license only
'  05.02.2019     Bugfix for AVR-receive
'  08.05.2020     Reduced RAM consumption using Ser3Print with string constants
'  02.02.2022     Initialization of some script variables with zero (for compiler build >= 1077)
' 14/08/22 Updated user changeable constants only - no functional change


  #script


    ''SET THE DEFAULT VALUE
    IF NODEF(SER3_INVERT) THEN
        SER3_INVERT = OFF
    END IF

    IF NODEF(SER3_TXPORT) THEN
        SER3_TXPORT = SER3_TXPORT_NOT_SET
    END IF

    IF NODEF(SER3_RXPORT) THEN
        SER3_RXPORT = SER3_RXPORT_NOT_SET
    END IF

    IF NODEF(SER3_DATABITS) THEN
         SER3_DATABITS = 8
    END IF

    IF NODEF(SER3_STOPBITS) THEN
         SER3_STOPBITS = 1
    END IF

    IF NODEF(SER3_RXNOWAIT) THEN
        SER3_RXNOWAIT  = OFF
    END IF

   If Var(SER3_TXPORT) Or Var(SER3_TXPIN) Or Var(SER3_RXPORT) Or Var(SER3_RXPIN) Or Var(SER3_BAUD) Then
   '*** Check if all necessary constants for sending or receiving are set and valid:
     If (SER3_DATABITS < 5) Or (SER3_DATABITS > 8) Then ERROR "Valid value for SER3_DATABITS: 5 - 8"
     If (SER3_STOPBITS < 1) Or (SER3_STOPBITS > 2) Then ERROR "Valid value for SER3_STOPBITS: 1 or 2"
     If (SER3_INVERT <> 0) And (SER3_INVERT <> 1) Then ERROR "Valid value for SER3_INVERT: On or Off"
     If (Int(SER3_BAUD) = 0) And (SER3_BAUD <> 0) Then ERROR "Please define a value for SER3_BAUD; i.e. '#define SER3_BAUD 19200'"
   End If

   If Var(SER3_TXPORT) Or Var(SER3_TXPIN) Then
   '*** Check if all necessary constants for sending are set and valid:
     If NoVar(SER3_TXPORT) Then
       ERROR "  Please define a valid I/O Port for Software UART   "
       ERROR "  i.e.  '#define SER3_TXPORT PortA'  for transmitting"
     End If
     If SER3_TXPORT <> SER3_TXPORT_not_set Then
       If (Int(SER3_TXPIN) = 0) And (SER3_TXPIN <> 0) Then ERROR "Please define a value for SER3_TXPIN; i.e. '#define SER3_TXPIN 4'"
       If SER3_TXPIN < 0 Or SER3_TXPIN > 7 Then ERROR "Valid value for SER3_TXPIN: 0 - 7"
     End If
    '*** Calculate number of delayloops for sending:
     STX3_DELAYH = 0  'required since build 1077
     If SER3_TXPORT <> SER3_TXPORT_not_set Then  '... for channel if a port ist selected
       If PIC Then STX3_DELAY = INT(((ChipMHz*1000000/4/SER3_BAUD)-12.5)/3)  '12.5 = -14 cycles +1.5 account for integer truncation
       If AVR Then STX3_DELAY = INT(((ChipMHz*1000000/SER3_BAUD)-8.5)/3)     ' 8.5 = -10 cycles +1.5 account for integer truncation
       'warning  send-Delay         STX3_DELAY
       If STX3_DELAY < 4 Then ERROR "'SER3_BAUD" SER3_BAUD"' too high - Speed up chip or reduce baudrate!"
       If STX3_DELAY > 255 Then
         STX3_DELAYH = Int(STX3_DELAY / 256)
         STX3_DELAY =  STX3_DELAY - STX3_DELAYH * 256
       End If
         If STX3_DELAYH > 255 Then ERROR "'SER3_BAUD" SER3_BAUD"' too low - Slow down chip or increase baudrate!"
     End If
     'warning  send-Delay1   STX3_DELAY
     'warning  send-DelayH1  STX3_DELAYH
     'If Var(SER3_TXPORT) And Var(SER3_TXPIN) Then
     '*** Process pin-direction and -polarity for sending:
     '  STx3PinSetup
     'End If
   End If

   If Var(SER3_RXPORT) Or Var(SER3_RXPIN) Then
   '*** Check if all necessary constants for receiving are set and valid:
     If NoVar(SER3_RXPORT) Then
       ERROR "  Please define a valid I/O Port for Software UART   "
       ERROR "  i.e.  '#define SER3_RXPORT PortA'  for receiving   "
     End If
     If SER3_RXPORT <> SER3_RXPORT_not_set Then
       If (Int(SER3_RXPIN) = 0) And (SER3_RXPIN <> 0) Then ERROR "Please define a value for SER3_RXPIN; i.e. '#define SER3_RXPIN 4'"
       If SER3_RXPIN < 0 Or SER3_RXPIN > 7 Then ERROR "Valid value for SER3_RXPIN: 0 - 7"
       If (SER3_RXNOWAIT <> 0) And (SER3_RXNOWAIT <> 1) Then ERROR "Valid value for SER3_RXNOWAIT: On or Off"
     End If
     '*** Calculate number of delayloops for receiving:
     SRX3_DELAYH     = 0  'required since build 1077
     SRX3_HALFDELAYH = 0  '
     If SER3_RXPORT <> SER3_RXPORT_not_set Then  '... for channel if a port ist selected
       If PIC Then SRX3_DELAY = INT(((ChipMHz*1000000/4/SER3_BAUD)-6.5)/3)  '6.5 = -8 cycles +1.5 account for integer truncation
       If AVR Then SRX3_DELAY = INT(((ChipMHz*1000000/SER3_BAUD)-4.5)/3)    '4.5 = -6 cycles +1.5 account for integer truncation
       If SRX3_DELAY < 6 Then ERROR "'SER3_BAUD" SER3_BAUD"' too high - Speed up chip or reduce baudrate!"
       SRX3_HALFDELAY = Int(SRX3_DELAY * 1.5)
       'warning  receive-Delay1      SRX3_DELAY
       'warning  receive-HalfDelay1  SRX3_HALFDELAY
       If SER3_RXNOWAIT = On Then    'reduce delay because of overhead when calling from an ISR
         If AVR Then SRX3_HALFDELAY = SRX3_HALFDELAY - 10  'tested ok -2 to -12
         If PIC Then SRX3_HALFDELAY = SRX3_HALFDELAY - 8  'tested ok -4 to -11
         If SRX3_HALFDELAY < 0 Then SRX3_HALFDELAY = 0
       End If
       If SRX3_DELAY > 255 Then
         'If AVR Then SRX3_DELAY = Int(((ChipMHz*1000000/SER3_BAUD)-4.5)/4)     '16bit delayloop is one cycle longer
         SRX3_DELAYH = Int(SRX3_DELAY / 256)
         SRX3_DELAY =  SRX3_DELAY - SRX3_DELAYH * 256
       End If
       If SRX3_HALFDELAY > 255 Then
         SRX3_HALFDELAYH = Int(SRX3_HALFDELAY / 256)
         SRX3_HALFDELAY =  SRX3_HALFDELAY - SRX3_HALFDELAYH * 256
       End If
       If SRX3_HALFDELAYH > 255 Then ERROR "'SER3_BAUD" SER3_BAUD"' too low - Slow down chip or increase baudrate!"
     End If
      'warning  receive-DelayH2     SRX3_DELAYH
      'warning  receive-Delay2      SRX3_DELAY
      'warning  receive-HalfDelayH2 SRX3_HALFDELAYH
      'warning  receive-HalfDelay2  SRX3_HALFDELAY
   End If

 #endscript


Sub STx3PinSetup
'*** Process pin-direction and -polarity for sending on programmstart:
   #ifdef AllOf(SER3_TXPORT, SER3_TXPIN)  'if a Port for channel is selected
     Dir SER3_TXPORT.SER3_TXPIN Out       '... make it output
     #if SER3_INVERT = Off                'if normal polarity
       Set SER3_TXPORT.SER3_TXPIN On      '... set HIGH to make the first startbit recognizable
     #endif
     #if SER3_INVERT = On                 'if normal polarity
       Set SER3_TXPORT.SER3_TXPIN Off     '... set HIGH to make the first startbit recognizable
     #endif
     '#ifdef PIC
     '  nop                                'otherwise the first 2 bytes are garbage !?
     '#endif
   #endif
End Sub


Sub Ser3Send (In STxDataByte)
'*** Code for sending a byte is here!

 '*** Code for PIC:
  #ifdef PIC

  '**PIC; send to channel:
    Dim SerDlyCnt, SerBitCnt As Byte

      SerBitCnt = SER3_DATABITS+2       ;number of bits to transmit (1 start + n data + 1 stop) [GCB]
      bcf STATUS,C                      ;clear CarryFlag to 0 (needed for startbit LOW) [ASM]
      STx3Loop:
        ;9 cycle loop + delay per byte
        btfsc STATUS,C                  ;skip next line if CarryFlag=0 [ASM]
       goto STx3One                     ;goto STxOne if CarryFlag=1
        #if SER3_INVERT = Off           ;normal polarity ...
         Set SER3_TXPORT.SER3_TXPIN Off ;set pin LOW if CarryFlag=0 [GCB]
        #endif
        #if SER3_INVERT = On            ;inverted polarity ...
         Set SER3_TXPORT.SER3_TXPIN On  ;set pin HIGH if CarryFlag=0 [GCB]
        #endif
      STx3One:
        btfss STATUS,C                  ;skip next line if CarryFlag=1 [ASM]
       goto  STx3Done                   ;goto STxDone if CarryFlag=0
        #if SER3_INVERT = Off
         Set SER3_TXPORT.SER3_TXPIN On  ;set pin LOW if CarryFlag=0 [GCB]
        #endif
        #if SER3_INVERT = On
         Set SER3_TXPORT.SER3_TXPIN Off ;set pin HIGH if CarryFlag=0 [GCB]
        #endif
      STx3Done:
        ;--- start short delay ---------
        #if STX3_DELAY > 0
          SerDlyCnt = STX3_DELAY        ;number of delay-loops [GCB]          cycle 8,9,10 (movlw,banksel,movwf)
        STx3Delay:
          ;delay-loop = (3  cycle * delaycounter) -1
          decfsz SerDlyCnt,F            ;decrement delaycounter, skip next line if 0 [ASM]
         goto STx3Delay                 ;loop to STxDelay until delaycounter=0
        #endif
        ;--- end short delay ----------
        ;--- start long delay ---------
        #if STX3_DELAYH > 0
         Sub_SerLongDelay(STX3_DELAYH)
        #endif
        ;--- end long delay ----------
        rrf STxDataByte,F               ;shift next bit to CarryFlag [ASM]                cycle 11
        bsf STxDataByte,SER3_DATABITS-1 ;set Bit7 to 1; needed for stopbit HIGH           cycle 12
        decfsz SerBitCnt,F              ;decrement bitcounter, skip next line if 0 [ASM]  cycle 13
       goto STx3Loop                    ;process next bit until bitcounter=0              cycle 14,15

       ;* Add additional stopbit
       #if SER3_STOPBITS = 2
        ;--- start short delay ---------
        #if STX3_DELAY > 0
          SerDlyCnt = STX3_DELAY        ;number of delay-loops [GCB]
        STx3StopDelay:
          decfsz SerDlyCnt,F            ;decrement delaycounter, skip next line if 0 [ASM]
         goto STx3StopDelay             ;loop to STxDelay until delaycounter=0
        #endif
        ;--- end short delay ----------
        ;--- start long delay ---------
        #if STX3_DELAYH > 0
         Sub_SerLongDelay(STX3_DELAYH)
        #endif
        ;--- end long delay ----------
       #endif

  #endif '... for PIC code

 '*** Code for AVR:
  #ifdef AVR

      '** AVR; send to channel:
      lds R23,STxDataByte               ;load DataByte to register
      #if SER3_DATABITS < 8
       sbr R23,1<<SER3_DATABITS         ;set MSB+1 for proper stopbit
      #endif
      ldi R26,SER3_DATABITS+2           ;load number of DataBits to register (1 start + N data + 1 stop)
      com R23                           ;invert DataBits and set Carry to 1
      STx3Loop:
        ;10 cycle loop + delay per byte
       brcc STx3One                     ;jump to STxOne if Carry=0  [ASM]             cycle 1
        #if SER3_INVERT = Off           ;normal polarity ...
         Set SER3_TXPORT,SER3_TXPIN Off ;set pin LOW if Carry=1 (translated to: cbi)  cycle 2,3
        #endif
        #if SER3_INVERT = On            ;inverted polarity ...
         Set SER3_TXPORT,SER3_TXPIN On  ;set pin HIGH if Carry=1 (translated to: cbi) cycle 2,3
        #endif
      STx3One:
       brcs STx3Done                    ;jump to STxDone if Carry=1                   cycle 4,5
        #if SER3_INVERT = Off           ;normal polarity ...
         Set SER3_TXPORT,SER3_TXPIN On  ;set pin HIGH if Carry=0 (translated to: cbi) cycle -
        #endif
        #if SER3_INVERT = On            ;inverted polarity ...
         Set SER3_TXPORT,SER3_TXPIN Off ;set pin LOW if Carry=0 (translated to: cbi)  cycle -
        #endif
      STx3Done:
      ;--- start short delay --------
       #if STX3_DELAY > 0
         ldi R24, STX3_DELAY            ;load delaycounter to register [ASM]          cycle 6
       STx3Delay:
         ;delay-loop = (3 cycle * delaycounter) -1
         dec R24                        ;decrement delaycounter
        brne STx3Delay                  ;loop to STxDelay until delaycounter=0
       #endif
      ;--- end short delay ----------
      ;--- start long delay --------
       #if STX3_DELAYH > 0
         Sub_SerLongDelay(STX3_DELAYH)  ;process long delay
       #endif
      ;--- end long delay ----------
        lsr R23                         ;shift next DataBit to Carry  [ASM]  cycle 7
        dec R26                         ;decrement bitcounter [ASM]          cycle 8
       brne STx3Loop                    ;jump to STxLoop and transmit next bit until bitcounter=0  [ASM] cycle 9,10

      ;* Add additional stopbit
      #if SER3_STOPBITS = 2
          ;--- start short delay --------
         #if STX3_DELAY > 0
           ldi R24, STX3_DELAY          ;load delaycounter to register
         STx3StopDelay:
           ;delay-loop = (3 cycle * delaycounter) -1
           dec R24                      ;decrement delaycounter
          brne STx3StopDelay            ;loop to STxStopDelay until delaycounter=0
         #endif
        ;--- end short delay ----------
        ;--- start long delay --------
         #if STX3_DELAYH > 0
           Sub_SerLongDelay(STX3_DELAYH) ;process long delay
         #endif
        ;--- end long delay ----------
      #endif

  #endif  '... for AVR code

End Sub


Sub Ser3Print (In PrintData As String)
   'PrintLen = LEN(PrintData$)
   PrintLen = PrintData(0)

   if PrintLen = 0 then Goto Ser3PrintStrEnd

   'Write Data
   for SysPrintTemp = 1 to PrintLen
      Ser3Send PrintData(SysPrintTemp)
   next

 'CR
 Ser3PrintStrEnd:
  #IFDEF SerPrintCR
    Ser3Send 13
  #ENDIF
  #IFDEF SerPrintLF
    Ser3Send 10
  #ENDIF
End Sub


Sub Ser3Print (In SerPrintVal)

  OutValueTemp = 0

  IF SerPrintVal >= 100 Then
    OutValueTemp = SerPrintVal / 100
    SerPrintVal = SysCalcTempX
    Ser3Send OutValueTemp + 48
  End If
  If OutValueTemp > 0 Or SerPrintVal >= 10 Then
    OutValueTemp = SerPrintVal / 10
    SerPrintVal = SysCalcTempX
    Ser3Send OutValueTemp + 48
  End If
  Ser3Send SerPrintVal + 48

  'CR
  #IFDEF SerPrintCR
    Ser3Send 13
  #ENDIF
  #IFDEF SerPrintLF
    Ser3Send 10
  #ENDIF

End Sub


Sub Ser3Print (In SerPrintVal As Word)

  Dim SysCalcTempX As Word

  OutValueTemp = 0

  If SerPrintVal >= 10000 then
    OutValueTemp = SerPrintVal / 10000 [word]
    SerPrintVal = SysCalcTempX
    Ser3Send OutValueTemp + 48
    Goto Ser3PrintWord1000
  End If

  If SerPrintVal >= 1000 then
  Ser3PrintWord1000:
    OutValueTemp = SerPrintVal / 1000 [word]
    SerPrintVal = SysCalcTempX
    Ser3Send OutValueTemp + 48
    Goto Ser3PrintWord100
  End If

  If SerPrintVal >= 100 then
  Ser3PrintWord100:
    OutValueTemp = SerPrintVal / 100 [word]
    SerPrintVal = SysCalcTempX
    Ser3Send OutValueTemp + 48
    Goto Ser3PrintWord10:
  End If

  If SerPrintVal >= 10 then
  Ser3PrintWord10:
    OutValueTemp = SerPrintVal / 10 [word]
    SerPrintVal = SysCalcTempX
    Ser3Send OutValueTemp + 48
  End If

  Ser3Send SerPrintVal + 48

  'CR
  #IFDEF SerPrintCR
    Ser3Send 13
  #ENDIF
  #IFDEF SerPrintLF
    Ser3Send 10
  #ENDIF

End Sub


Sub Ser3Print (In SerPrintVal As Long)

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
    Ser3Send SysPrintBuffer(SysPrintTemp) + 48
  Next

  'CR
  #IFDEF SerPrintCR
    Ser3Send 13
  #ENDIF
  #IFDEF SerPrintLF
    Ser3Send 10
  #ENDIF

End Sub


Sub Ser3Print (In SerPrintValInt As Integer)

  Dim SerPrintVal As Word

  'If sign bit is on, print - sign and then negate
  If SerPrintValInt.15 = On Then
    Ser3Send("-")
    SerPrintVal = -SerPrintValInt
   'Sign bit off, so just copy value
  Else
    SerPrintVal = SerPrintValInt
  End If

  'Use Print(word) to display value
  Ser3Print SerPrintVal
End Sub


Function Ser3Receive
'*** Code for receiving a byte is here!

 '*** Code for PIC:
  #ifdef PIC

    Dim SerDlyCnt As Byte

    '** PIC; receive from channel:
    '#ifdef Allof(SER3_RXPORT, SER3_BAUD, SRX3_HALFDELAY, SRX3_DELAY)

      bcf STATUS,C
      Ser3Receive = 0
      'Set Ser3Receive.SER3_DATABITS-1   ;set index-bit (indicates end of transmission)
      bsf Ser3Receive,SER3_DATABITS-1    ;set index-bit (indicates end of transmission)
                     ;
      ;* wait for start-bit edge if waiting is defined:
      #if SER3_RXNOWAIT = Off
       SRx3WaitStart:
        #if SER3_INVERT = Off            ;normal polarity ...
         btfsc SER3_RXPORT,SER3_RXPIN    ;skip next line if RxPin LOW
        #endif
        #if SER3_INVERT = On             ;reverse polarity ...
         btfss SER3_RXPORT,SER3_RXPIN    ;skip next line if RxPin HIGH
        #endif
        goto SRx3WaitStart               ;loop while RxPin HIGH (UART idle)
      #endif

      ;* process a delayloop of 1.5 bitlength to catch the middle of the impulses:
      ;--- start short halfbit delay ---------
      #if SRX3_HALFDELAY > 0
        SerDlyCnt = SRX3_HALFDELAY       ;load delaycounter with a half-bit delay
      SRx3LoopHalf:
        decfsz SerDlyCnt,F               ;decrement delaycounter, skip next line if 0 [ASM]
       goto SRx3LoopHalf                 ;loop to SRxHalfBit while delaycounter > 0
      #endif
      ;--- end short halfbit delay ----------
      ;--- start long halfbit delay ---------
      #if SRX3_HALFDELAYH > 0
       Sub_SerLongDelay(SRX3_HALFDELAYH)
      #endif
      ;--- end long halfbit delay -----------

      ;* read bitstatus from port:
      SRx3GetBit:
        ;SerDlyCnt = SRX3_DELAY           ;reload delaycounter with one-bit delay  cycle 1,2
        rrf Ser3Receive,F                ;rotate right and bit0 to Carry [ASM]    cycle 3
        #if SER3_INVERT = Off            ;normal polarity ...
         btfsc SER3_RXPORT,SER3_RXPIN    ;skip next line if RxPin LOW [ASM]       cycle 4 or 4,5
        #endif
        #if SER3_INVERT = On             ;reverse polarity ...
         btfss SER3_RXPORT,SER3_RXPIN    ;skip next line if RxPin HIGH [ASM]      cycle 4 or 4,5
        #endif
        bsf Ser3Receive,SER3_DATABITS-1  ;set new leftmost bit to 1 if RxPin HIGH  cycle 6
        ;* process a 1-bit bitlength delayloop to wait next bit:
        ;--- start short delay ---------
        #if SRX3_DELAY > 0
          SerDlyCnt = SRX3_DELAY         ;reload delaycounter with one-bit delay  cycle 1,2
        SRx3LoopStop:
          decfsz SerDlyCnt,F             ;decrement delaycounter, skip next line if 0 [ASM]
         goto SRx3LoopStop               ;loop to SRxStopBit while delaycounter > 0
        #endif
        ;--- end short delay -----------
        ;--- start long delay ----------
        #if SRX3_DELAYH > 0
         Sub_SerLongDelay(SRX3_DELAYH)
        #endif
        ;--- end long delay ------------

       btfss STATUS,C                   ;end if index-bit is shifted to Carry (from TxRecByte=N) [ASM] cycle 7 (no jump)
      goto SRx3GetBit                   ;get next DataBit while Carry=0 (from TxRecByte=N) [ASM]  cycle 8,9

      ;* wait second stopbit:
      #if SER3_STOPBITS = 2
        ;--- start short delay ---------
        #if SRX3_DELAY > 0
          SerDlyCnt = SRX3_DELAY         ;reload delaycounter with one-bit delay
         SRx3LoopSecondStop:
          decfsz SerDlyCnt,F             ;decrement delaycounter, skip next line if 0 [ASM]
         goto SRx3LoopSecondStop         ;loop to SRxStopBit while delaycounter > 0
        #endif
        ;--- end short delay -----------
        ;--- start long delay ----------
        #if SRX3_DELAYH > 0
         Sub_SerLongDelay(SRX3_DELAYH)
        #endif
        ;--- end long delay ------------
      #endif

    '#endif

  #endif  '... for PIC code


  '*** Code for AVR:
  #ifdef AVR

    ;** AVR; receive from channel:

    ;* Save register content to stack:
    ;push R23                          ;DataByte
    ;push R24                          ;DelayLoop LowByte
    #ifdef SRX3_HALFDELAYH
     ;push R25                         ;DelayLoop HighByte
      ldi R25,SRX3_HALFDELAYH          ;load delaycounter with a half-bit delay HighByte
    #endif
    ldi R24,SRX3_HALFDELAY             ;load delaycounter with a half-bit delay LowByte
    ldi R23,1<<(SER3_DATABITS-1)       ;set index-bit (indicates end of transmission)
    clc                                ;bugfix 05.02.2019: clear carry flag, otherwise
                                       ;                   LSB is ok only at first call

    ;* wait for start-bit edge:
    #if SER3_RXNOWAIT = Off
     SRx3WaitStart:                    ;wait for start edge
      #if SER3_INVERT = Off            ;normal polarity ...
       sbic SER3_RXPORT-2,SER3_RXPIN   ;skip next line if RxPin LOW [ASM]
      #endif
      #if SER3_INVERT = On             ;reverse polarity ...
       sbis SER3_RXPORT-2,SER3_RXPIN   ;skip next line if RxPin HIGH [ASM]
      #endif
     rjmp  SRx3WaitStart               ;loop while RxPin HIGH (UART idle)
    #endif

    ;* process a half-bit delayloop to catch the middle of the impulses:
    SRx3LoopHalf:
     ;--- start short halfbit delay --------
     #if SRX3_HALFDELAY > 0
       ;delay-loop = (3 cycle * delaycounter) -1
       dec R24                        ;decrement delaycounter
      brne SRx3LoopHalf               ;loop to SRxWaitHalf until delaycounter = 0
     #endif
     ;--- end short halfbit delay ----------
     ;--- start long halfbit delay --------
     #if SRX3_HALFDELAYH > 0
       Sub_SerLongDelay(SRX3_HALFDELAYH)
     #endif
    ;--- end long halfbit delay ----------

    ;* read bitstatus from port:
    SRx3GetBit:
      ror R23                         ;rotate Carry into DataByte MSB and LSB to Carry  cycle 2
      #if SER3_INVERT = Off           ;normal polarity ...
       sbic SER3_RXPORT-2,SER3_RXPIN  ;skip next line if RxPin LOW [ASM]                cycle 3 or 3,4
      #endif
      #if SER3_INVERT = On            ;reverse polarity ...
       sbis SER3_RXPORT-2,SER3_RXPIN  ;skip next line if RxPin HIGH [ASM]               cycle 3 or 3,4
      #endif
      sbr R23,1<<(SER3_DATABITS-1)    ;set new leftmost bit to 1 if RxPin HIGH          cycle 4,-

      ;* process a full-bit delayloop to wait next bit:
      ;--- start short delay --------
      #if SRX3_DELAY > 0
        ldi R24, SRX3_DELAY          ;reload delaycounter with a one-bit delay
       SRx3LoopStop:
        ;delay-loop = (3 cycle * delaycounter) -1
        dec R24                      ;decrement delaycounter and clear Carry to 0
       brne SRx3LoopStop             ;loop to SRxLoopStop until delaycounter = 0
      #endif
      ;--- end short delay ---------
      ;--- start long delay --------
      #if SRX3_DELAYH > 0
        Sub_SerLongDelay(SRX3_DELAYH)
      #endif
      ;--- end long delay --------

     brcc SRx3GetBit                  ;get next bit until Carry=1 (from ldi r24, 0x80)  cycle 5,6

     ;* wait second stopbit:
      #if SER3_STOPBITS = 2
       ;--- start short delay --------
        #if SRX3_DELAY > 0
         ldi R24, SRX3_DELAY         ;reload delaycounter with a one-bit delay
        SRx3LoopSecondStop:
          ;delay-loop = (3 cycle * delaycounter) -1
          subi R24,1                ;decrement delaycounter and clear Carry to 0
         brne SRx3LoopSecondStop    ;loop to SRxLoopSecondStop until delaycounter = 0
        #endif
       ;--- end short delay ---------
       ;--- start long delay --------
        #if SRX3_DELAYH > 0
          Sub_SerLongDelay(SRX3_DELAYH)
        #endif
       ;--- end long delay --------
      #endif

     sts Ser3Receive, R23             ;store data into variable

     ;#ifdef TRX_HALFDELAYH1
      ;pop R25                        ;restore register content from stack
     ;#endif
     ;pop R24                         ;
     ;pop R23                         ;

  #endif '... for AVR code

End Function
