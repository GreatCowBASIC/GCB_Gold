'    MicroChip specific to support Software Serial for GCBASIC
'    Copyright (C) 2017-2026 Frank Steinberg

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
'''This routines sends and receives serial (RS232) data with a minimal overhead.
'''
''' - 3 independent channels Ser1... , Ser2... , Ser3...
''' - I/O pins user configurable
''' - polarity can be inverted
''' - freely adjustable baud rate
''' - maximum baudrate depends on MCU speed:
'''   * PIC@ 1Mhz    9600 baud
'''   * PIC@ 4Mhz   38400 baud
'''   * PIC@ 8Mhz   64000 baud
'''   * PIC@16Mhz  128000 baud
'''   * AVR@ 1Mhz   28800 baud
'''   * AVR@ 8Mhz  115200 baud
'''   * AVR@16Mhz  460800 baud
''' - 5 - 8 data bits
''' - 1 or 2 stop bits
''' - parity bit not supported
''':
'''  For a proper configuration see sample files.
''':
'''@author  Frank Steinberg  guided by a code of Ralph Doncaster:
''' http://nerdralph.blogspot.de/2013/12/writing-avr-assembler-code-with-arduino.html
'''@licence GPL
'''@version 1.21
'''@date    25.02.2017
'''******************************************************************************
'  20.02.2018     Updated license only
'  05.02.2019     Bugfix for AVR-receive
'  08.05.2020     Reduced RAM consumption using SerNPrint with string constants
'  02.02.2022     Initialization of some script variables with zero (for compiler build >= 1077)


'*** 3 independent channels in 3 different files:
#include <SoftSerialCh1.h>
#include <SoftSerialCh2.h>
#include <SoftSerialCh3.h>

; The following does not work (all includes allways loaded):
  '#script
  '#ifdef OneOf(SER1_TXPORT, SER1_TXPIN, SER1_RXPORT, SER1_RXPIN, SER1_BAUD)
  ' #include <SoftSerialCh1.h>
  '#endif
  '#ifdef OneOf(SER2_TXPORT, SER2_TXPIN, SER2_RXPORT, SER2_RXPIN, SER2_BAUD)
  ' '#include <SoftSerialCh2.h>
  '#endif
  '#ifdef OneOf(SER3_TXPORT, SER3_TXPIN, SER3_RXPORT, SER3_RXPIN, SER3_BAUD)
  ' #include <SoftSerialCh3.h>
  '#endif
  '#endscript
  '
  '#script
  'If Var(SER2_TXPORT) Or Var(SER2_TXPIN) Or Var(SER2_RXPORT) Or Var(SER2_RXPIN) Or Var(SER2_BAUD) Then
  ' #include <SoftSerialCh2.h>
  'End If
  '#endscript


'*** Long-delay routines used by all channels:
Sub Sub_SerLongDelay(In SerDlyCnt)

 Dim  SerDlyCntI As Byte

  #ifdef PIC
    ;Calling & returning from Sub:  4 cycles
    ;loading TxCntDly            :  2 cycles
    ;--- start outer loop --------------
    STxDelayH:              ;outer loop = 253*3 cycles inner loop + 6 cycles outer loop = 255*3 total)
       SerDlyCntI = 253     ;number of inner delay-loops [GCB]            ;outer loop cycle 1,2
       nop                                                                ;outer loop cycle 3
      ;--- start inner loop --------------
      STxDelayI:            ;inner loop = 253*3 -1  +1(nop) = 253*3 cycles
       decfsz SerDlyCntI,F  ;decrement delaycounter, skip next line if 0  ;inner loop cycle 1 (no jump)
      goto STxDelayI        ;loop to STxDelayI until delaycounter=0       ;inner loop cycle 2,3
       nop                  ;last loop:  decfsz jump +1 cycles; no goto -2 cycles; nop +1 cycle
      ;--- end inner loop -----------------
       decfsz SerDlyCnt,F   ;decrement delaycounter, skip next line if 0  ;outer loop cycle 4 (no jump)
    goto STxDelayH          ;loop to STxDelay until delaycounter=0        ;outer loop cycle 5,6
    ;--- end outer loop ---------------
  #endif

  #ifdef AVR
    ;Calling & returning from Sub:  4 cycles
    ;loading TxCntDly            :  1 cycles
    ;--- start outer loop --------------
     lds R25, SerDlyCnt
      ' ldi R25, SRX1_DELAYH
    STxDelayH:               ;outer loop = 253*3 cycles inner loop + 6 cycles outer loop = 255*3 total)
      ldi R24, 253           ;number of inner delay-loops [GCB]         ;outer loop cycle 1
      ;--- start inner loop --------------
      STxDelayI:             ;inner loop = 253*3 -1  +1(nop) = 253*3 cycles
        dec R24              ;decrement inner delaycounter              ;inner loop cycle 1
       brne STxDelayI        ;loop to STxDelayI until delaycounter = 0  ;inner loop cycle 2,3
        nop                  ;last loop:  no jump -1 cycles; nop +1 cycle
      ;--- end inner loop -----------------
       dec R25               ;decrement outer delaycounter               ;outer loop cycle 2
       nop                                                               ;outer loop cycle 3
       nop                                                               ;outer loop cycle 4
     brne STxDelayH          ;loop to STxDelayH until delaycounter = 0   ;outer loop cycle 5,6
    ;--- end outer loop ---------------
  #endif

End Sub
