'    Timer control routines for GCBASIC
'    Copyright (C) 2006-2024 Hugh Considine, Evan R. Venn and  William Roth
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
' 10/07/2009: Added AVR support
' 24/08/2014: Added SetTimer subroutine
' 16/08/2014: Added support for Timer4 and Timer6
' 03/06/2015: Added support for PIC Timer 5
' 12/06/2015: Added support for PIC Timer 7/8/10/12 - WMR
' 12/06/2015: Changed "cleartimer" to not stop timers 1/3/5/7 -WMR
' 12/06/2015: Added support for PICs that use SOSCEN instead of OSCEN
' 12/06/2015: Added Starttimer/Stoptimer support for Timer 0 for 18F PICS
' 12/06/2015: Added support for Timer 0 16-Bit mode
' 12/06/2015: Corrected some typos - WMR
' 24/06/2015: Corrected "Timer 0 Notes" formatting
' 25/08/2015: Added redirection of TIMER2OVERFLOW to TIMER2MATCH for handling deprecated commands - ERV
' 25/08/2015: Added Functions for Timer8/10&12 - ERV
' 25/08/2015: Removed errors and improved handles timers and added Inittimer7- ERV
' 10/10/2015: Added ifdef to protect StopTimer 1 error
' 10/10/2015: Change Stoptimer ifdef to T1CON to  TMR1ON etc. etc. TMR12ON
' 29/12/2015: Revised to resolve TMR0 16 bit and AVR addressing.
' 31/12/2015: Emergency changes to sort timer2, 4, 6, 7 8, 10 as words not byte.

' 04/01/2016: ALL PIC and ARV Timers now Aliases (no more functions)
'             Corrected/Changed  #IFEFs for 8-Bit/16-bit TIMER0
'             Corrected multiple typos -WMR
'
' 05/01/2016: Added support for ATtiny15/25/45/85/261/461/861
'             (register, prescaler, 16bit-timer0, 10bit-timer1)
'
'
' 15/01/2016  Eliminated unnecessary PIC "Sinitimer" Subs
'             Removed dead code (Some PIC Stuff)
'
' 15/01/2016  PIC Timer prescale and source settings now
'             written to TxCON register in Inittimer sub
'             instead of Starttimer.
'
' 15/01/2016  TMRPres used as mutipurpose variable. (PIC)
'
' 15/01/2016  Replaced  ASM code routines for Timers 2/4/6/8/10/12
'             to save memory (SYSTEMP variables)
'
' 15/01/2016  Added "failsafes" for some timers settings (PIC)
' 15/01/2016  Added missing Timer 12 entry (PIC Cleartimer)
'
' 15/01/2016  Corrected TYPO for AVR timer4 Alias
'
' 15/01/2016  No unique variables for TMRxSource, TMRxPres, or TMRxPost
' 15/01/2016  All Timers share TMRSource, TMRPres and TMRPost (Save Memory)
' 15/01/2016  Added (AVR)shadow variable TMRx_TMP for each timer
' 18/01/2016  Corrected Typo AVR inittimer4
' 15/02/2016  Added InitTimer0 support for baseline Pics with
'             with non-addressable / write only option_reg - WMR
' 18/02/2016  Added correct prescales for AVR with TCCR2A register -Wmr
' 18/02/2016  Added/corrected TCCR2A setting in Stop/starttimer -Wmr
' 22/02/2016  Corrected TCCR2A error - Wmr
' 24/04/2016: Removed PSn_1/x constants - no longer valid syntax - HC
' 15/07/2016: Revised Settimer to improve support for 16f188xx and T016BIT.
'             Change is limited ChipFamily 15
' 1/12/2016:  Added set Clock Source, if required by specific chip classes. Set to FOSC/4 via T[2,4,6]CLKCON support
'             Added revised constants via script for timer 2/4 and 6
' 7/12/2016:  Revised to correctly support settimer and cleartimer for 8 bit clock on chips that are tmr0 16 bit capable
' 7/12/2016:  Added optimisation Constants
' 10/12/2016: Revised Settimer to remove typo
' 13/01/2017: Updated Timer1/3/5/7 Support for newer chips with TxCLK register
' 18/03/2017: Corrected OSC(Source)  Support for PIC Timers 3/5/7 - WMR
' 21/03/2017: Corrected Timer0 Clock Source select bits constants for 16f18855 and others
'             Reverted the missing Settimer for AVR
' 05/02/2017: Added specific Support for TMR2/4/6/8 for 93 chips with Enhanced Timer2 with TxCKPS2 Bit and TxCLKCON Register.
' 05/02/2017: Corrected Typos in Inittimer 5/7
' 21/01/2019: Added timersource timer source variants #1,2and3 - handled in the script by testing bit(TMR1CLK_T1CS4) then changing the clock source constants
' 23/01/2019: Revised Clock sources 'Added set Clock Source on1/12/2016' the script was incorrect and was not operating as expected
' 27/04/2019: Revised InitTimer1, 3 and 5 to resolve missing clock source  LFINTOSC. Chip requires TMR1CS1 and TMR1CS0 to support LFINTOSC.
' 05/04/2020: Improve InitTimer0 to better support new class of timers.
' 25/04/2020: Improve Settimer to better support new class of timers for timer 0
' 18/09/2020: Added TCNT1 handling SetTimer specific change for AVR
' 02/01/2021: Corrected TCNT1 value for PS_1_8192
' 10/08/2021: Added fix for K40 Chips that require T0CON1 ASYNC bit to be set On
' 14/08/2022: Updated user changeable constants only - no functional change
' 25/10/2023: Updated to restore FOSC4 constant
' 27/11/2023: Updated to isolate AVR and PIC constants
' 30/08/2024: Added AVRDX support
'             Moved AVR constants to script for AVR legacy and AVRDX isolation
'             Correct AVRDX masks for CLKSEL[2:0]
'***********************************************************

'Subroutines:
' InitTimer0 (Source, Prescaler)
' InitTimer1 (Source, Prescaler)
' InitTimer2 (Prescaler, Postscaler)
' InitTimer3 (Source, Prescaler)
' InitTimer4 (Prescaler,Postascaler)
' InitTimer5 (Source, Prescaler)
' InitTimer6 (Prescaler,Postscaler)
' InitTimer7 (Source, Prescaler)
' InitTimer8 (Prescaler.Postscaler)
' InitTimer10 (Prescaler,Postscaler)
' InitTimer12 (Prescaler,Postscaler)

' ClearTimer(TimerNumber)
' StartTimer(TimerNumber)
' StopTimer(TimerNumber)
' SetTimer(TimerNumber, Value)



'User changeable constants



'Optimisation constants
   // none
'end of user changeable constants


'**************  Timer 0 Notes ***********************
' PIC Timer0 is a free running timer on midrange and
' baseline PICs. On these chips timer 0 always runs
' and cannot be stopped.   However on most 18Fxxx PICs,
' timer 0 can be started or stopped via the T0CON.7 Bit.
' Therefore, on these chips The timer MUST be started with
' StartTimer 0.
'
' On most 18F PICS, Timer 0 can be either an 8-Bit or 16-Bit
' timer.  On these chips this is determined by TC0CON.6
' (T08BIT). The timer defaults to 8-bit. To set the timer
' to 16-Bit mode the GCB source code must include the
' following line.
'
'              #define TMR0_16BIT
'
' Using 16 bit mode with the 1:256 prescaler allows for very
' long timer periods ( > 8 sec @ 8Mhz)
'*********************************************************

'Some simpler names for the timers (use to read)
'
 #ifdef PIC
    '*** Timer0 Test for 18F Chips ***
    #ifdef var(TMR0L)  'must be 18F w/selectable 8/16-Bit timer
        #ifdef TMR0_16BIT   'User defined 16 bit mode
              DIM Timer0 Alias TMR0H,TMR0L as Word
        #endif

        #ifndef TMR0_16BIT  'Default 8-bit mode
            DIM Timer0 Alias TMR0L as Byte
            ' TMR0 is aliased to "TMR0L" in
            '(some?) Chip datfiles. Best use TMR0L here
        #endif
    #endif

    'Test for other PICs with Timer0
    #ifdef Var(TMR0)         'Has a Timer0
        #ifndef var(TMR0H)   'And is not 16-bit
            DIM Timer0 Alias TMR0 as Byte
        #endif
    #endif

    #ifdef Var(TMR1H)  '16-bit Timer1
       DIM Timer1 Alias TMR1H,TMR1L as WORD
    #endif

    #ifdef Var(TMR2)   '8-bit Timer2
       DIM Timer2 Alias TMR2 as Byte
    #endif

    #ifdef Var(TMR3H)  '16-bit Timer3
       Dim Timer3 Alias TMR3H,TMR3L as WORD 'added "as WORD"
    #endif

    #ifdef Var(TMR4)   '8-bit Timer4
       Dim Timer4 Alias TMR4 as Byte
    #endif

    #ifdef Var(TMR5H)  '16-bit Timer5
       Dim Timer5 Alias TMR5H,TMR5L as WORD
    #endif

    #ifdef Var(TMR6)    '8-bit Timer6
       Dim Timer6 Alias TMR6 as Byte
    #endif

    #ifdef Var(TMR7H)  '16-bit Timer7
       Dim Timer7 Alias TMR7H,TMR7L as WORD
    #endif

    #ifdef Var(TMR8)    '8-bit Timer8
       Dim Timer8 Alias TMR8 as Byte
        'Timer8=0
    #endif

    #ifdef Var(TMR10)    '8-bit Timer10
       Dim Timer10 Alias TMR10 as Byte
    #endif

    #ifdef Var(TMR12)    '8-bit Timer12
       Dim Timer12 Alias TMR12 as Byte
    #endif
 #endif
     '********* END OF PIC TimerX Aliases *******************

 '******   AVR TIMER  DIM/ALIAS ****************
 #ifdef AVR

    #Ifdef var(TCA0_SINGLE_PERH) 
      Dim TCA0_SINGLE_PERIOD as Word Alias TCA0_SINGLE_PERH, TCA0_SINGLE_PERL
    #endif
    
    #ifdef var(TCNT0) '8-bit Timer0
        DIM Timer0 Alias TCNT0 as Byte
    #endif

    '*** 05/01/2016 Timer0 Test for ATtiny 261/461/861 ***
    #ifdef var(TCNT0L)   'must be selectable 8/16-Bit timer
      #ifdef TMR0_16BIT  'user defined 16-bit Timer0
         DIM Timer0 Alias TCNT0H,TCNT0L as Word
      #endif
      #ifndef TMR0_16BIT  'Default 8-bit Timer0
         DIM Timer0 Alias TCNT0L as Byte
      #endif
    #endif

    #ifdef var(TCNT1)   '8-bit Timer1
       DIM Timer1 Alias TCNT1 as Byte
    #endif

    #ifdef var(TC1H)    '10-Bit Timer1 on ATtiny 261/461/861
      DIM Timer1 Alias TC1H,TCNT1 as WORD
    #endif

    #ifdef var(TCNT1L)  '16-Bit Timer1
      DIM Timer1 Alias TCNT1H,TCNT1L as WORD
    #endif

    #ifdef var(TCNT2)
       DIM Timer2 Alias TCNT2 as byte
    #endif

    #ifdef var(TCNT2L)
      DIM Timer2 Alias TCNT2H,TCNT2L as WORD
    #endif

    #ifdef var(TCNT3)
       DIM Timer3 Alias TCNT3 As Byte
    #endif

    #ifdef var(TCNT3L)
       DIM Timer3 Alias TCNT3H,TCNT3L as WORD
    #endif

    #ifdef var(TCNT4)
       DIM Timer4 Alias TCNT4 as Byte
    #endif

    #ifdef var(TCNT4L)
       DIM Timer4 Alias TCNT4H,TCNT4L as WORD 'Typo
    #endif

    #ifdef var(TCNT5)
       DIM Timer5 Alias TCNT5 as Word
    #endif

    #ifdef var(TCNT5L)
        DIM Timer5 Alias TCNT5H,TCNT5L as WORD
    #endif

 #endif

 'Generic Clock Sources
  #define Osc 1
  #define FOSC4 = 1
  #define Ext 2
  #define ExtOsc 3
  #define sosc ExtOsc
  #define LFINTOSC 4

 #SCRIPT

 IF NODEF(USE_TIMER0) THEN
     USE_TIMER0=TRUE
  END IF
  IF NODEF(USE_TIMER1) THEN
     USE_TIMER1=TRUE
  END IF
  IF NODEF(USE_TIMER2) THEN
     USE_TIMER2=TRUE
  END IF
  IF NODEF(USE_TIMER3) THEN
     USE_TIMER3=TRUE
  END IF
  IF NODEF(USE_TIMER4) THEN
     USE_TIMER4=TRUE
  END IF
  IF NODEF(USE_TIMER5) THEN
     USE_TIMER5=TRUE
  END IF
  IF NODEF(USE_TIMER6) THEN
     USE_TIMER6=TRUE
  END IF
  IF NODEF(USE_TIMER7) THEN
     USE_TIMER7=TRUE
  END IF
  IF NODEF(USE_TIMER8) THEN
     USE_TIMER8=TRUE
  END IF
  IF NODEF(USE_TIMER9) THEN
     USE_TIMER9=TRUE
  END IF
  IF NODEF(USE_TIMER10) THEN
     USE_TIMER10=TRUE
  END IF
  IF NODEF(USE_TIMER11) THEN
     USE_TIMER11=TRUE
  END IF
  IF NODEF(USE_TIMER12) THEN
     USE_TIMER12=TRUE
  END IF

  if PIC then

    clocksourcetypetype  = 0
    if  var(T1CLK) Then
          clocksourcetypetype  = 1
    end if
    if  var(TMR1CLK) Then
          clocksourcetypetype  = 1
    end if
    if  var(T2CLK) Then
          clocksourcetypetype  = 1
    end if
    if  var(TMR2CLKCON) Then
          clocksourcetypetype  = 1
    end if


    if clocksourcetypetype = 1 Then
       'Added to support Chips with T1CLK register
       ' Includes 16F188xx, 16F153xx and others

       'keep for compatibility
       OSC =1      'FOSC/4
       EXT =0      'T1CKIPPS
       EXTOSC =6   'SOSC

       'Some Aditional Clock Sources
       'Some others not included  - can be set manually
       SOSC =ExtOsc
       MFINTOSC =5
       LFINTOSC =4
       HFINTOSC =3
       FOSC  =2
       FOSC4 =1
       TxCKIPPS  =0

    end if

    if bit(TMR1CLK_T1CS4) then
      'These parts have a four bit CS<4:0> to set the clock source. It is assumed these sources are common across all timers.
      'Use TMR1CLK_T1CS4 as the common test.
      'examples are 16f18424, 16f18446, 18f*k42 and 18f*k83 at Jan 2019
      REF_CLK = 8
      EXTOSC = 7
      sosc = ExtOsc
      MFINTOSC_32 = 6
      MFINTOSC_500 = 5
      LFINTOSC = 4
      HFINTOSC = 3
      FOSC = 2 '16MHz regardless of FOSC
      FOSC4 =  1
      TxCKIPPS = 0
    end IF

  end if

  If AVR Then

   // Added for AVRDX support as AVRDX uses different timer constant values
   IF NODEF(CHIPAVRDX) THEN
      //~ timer for AVR - these were defines but need to be script to support AVRDX
      //~ timer 0
      PS_0     = 0         ' no clock source
      PS_1     = 1
      PS_8     = 2
      PS_64    = 3
      PS_256   = 4
      PS_1024  = 5

      PS_0_0   = 0        ' no clock source
      PS_0_1   = 1
      PS_0_8   = 2
      PS_0_64  = 3
      PS_0_256 = 4
      PS_0_1024 = 5

      ' Set the script for PS_1_*

      PS_3_0   = 0         ' no clock source
      PS_3_1   = 1
      PS_3_8   = 2
      PS_3_64  = 3
      PS_3_256 = 4
      PS_3_1024 = 5

      PS_4_0   = 0         ' no clock source
      PS_4_1   = 1
      PS_4_8   = 2
      PS_4_64  = 3
      PS_4_256 = 4
      PS_4_1024 = 5

      PS_5_0   = 0         ' no clock source
      PS_5_1   = 1
      PS_5_8   = 2
      PS_5_64  = 3
      PS_5_256 = 4
      PS_5_1024 = 5
   END IF
   IF DEF(CHIPAVRDX) THEN

      //~ timer 0 CLKSEL[2:0] which is bits [3:1]
      PS_0     =  0       ' System clock
      PS_2     =  2
      PS_4     =  4
      PS_8     =  6
      PS_16    =  8
      PS_64    =  10
      PS_256   =  12
      PS_1024  =  14

      PS_0_0     =  PS_0
      PS_0_2     =  PS_2
      PS_0_4     =  PS_4
      PS_0_8     =  PS_8
      PS_0_16    =  PS_16
      PS_0_64    =  PS_64
      PS_0_256   =  PS_256
      PS_0_1024  =  PS_1024

   END IF


  End If

#ENDSCRIPT

//  #ifdef AVR
//       //~ timer for AVR - ERV
//       //~ timer 0
//       #define PS_0 0         ' no clock source
//       #define PS_1 1
//       #define PS_8 2
//       #define PS_64 3
//       #define PS_256 4
//       #define PS_1024 5

//       #define PS_0_0 0        ' no clock source
//       #define PS_0_1 1
//       #define PS_0_8 2
//       #define PS_0_64 3
//       #define PS_0_256 4
//       #define PS_0_1024 5

//       ' Set the script for PS_1_*

//       #define PS_3_0 0         ' no clock source
//       #define PS_3_1 1
//       #define PS_3_8 2
//       #define PS_3_64 3
//       #define PS_3_256 4
//       #define PS_3_1024 5

//       #define PS_4_0 0         ' no clock source
//       #define PS_4_1 1
//       #define PS_4_8 2
//       #define PS_4_64 3
//       #define PS_4_256 4
//       #define PS_4_1024 5

//       #define PS_5_0 0         ' no clock source
//       #define PS_5_1 1
//       #define PS_5_8 2
//       #define PS_5_64 3
//       #define PS_5_256 4
//       #define PS_5_1024 5

// #endif

  'support 16f188xx series (and others) 8/16 bit timers
  '
  #define   TMR0_CLC1       0xC0
  #define   TMR0_SOSC       0xA0
  #define   TMR0_LFINTOSC   0x80

  #define   TMR0_HFINTOSC   0x60
  #define   TMR0_FOSC4      0x40
  #define   TMR0_T0CKIPPS_Inverted  0x20
  #define   TMR0_T0CKIPPS_True      0x00

  #define   TMR0_T0ASYNC    0x10

  #define TMR0_T016BIT     0x10

  #define PRE0_1 = 0
  #define PRE0_2 = 1
  #define PRE0_4 = 2
  #define PRE0_8 = 3
  #define PRE0_16 = 4
  #define PRE0_32 = 5
  #define PRE0_64 = 6
  #define PRE0_128 = 7
  #define PRE0_256 = 8
  #define PRE0_512 = 9
  #define PRE0_1024 = 10
  #define PRE0_2048 = 11
  #define PRE0_4096 = 12
  #define PRE0_8192 = 13
  #define PRE0_16384 = 14
  #define PRE0_32768 = 15

  #define POST0_1 = 0
  #define POST0_2 = 1
  #define POST0_3 = 2
  #define POST0_4 = 3
  #define POST0_5 = 4
  #define POST0_6 = 5
  #define POST0_7 = 6
  #define POST0_8 = 7
  #define POST0_9 = 8
  #define POST0_10 = 9
  #define POST0_11 = 10
  #define POST0_12 = 11
  #define POST0_13 = 12
  #define POST0_14 = 13
  #define POST0_15 = 14
  #define POST0_16 = 15

'Generic Postscaler for TMR2/4/6/8 on chips with TxCLK Register
' Includes 16F188xx, 16F177x, 16F155xx, 18FxxK40 and others
  #define POST_1 = 0
  #define POST_2 = 1
  #define POST_3 = 2
  #define POST_4 = 3
  #define POST_5 = 4
  #define POST_6 = 5
  #define POST_7 = 6
  #define POST_8 = 7
  #define POST_9 = 8
  #define POST_10 = 9
  #define POST_11 = 10
  #define POST_12 = 11
  #define POST_13 = 12
  #define POST_14 = 13
  #define POST_15 = 14
  #define POST_16 = 15


#script

  if novar(TCNT1) then  
      PS_1_0 = 0         ' no clock source
      PS_1_1 = 1
      PS_1_8 = 2
      PS_1_64 = 3
      PS_1_256 = 4
      PS_1_1024 = 5
   End if

  if var(TCNT1) then  '05/01/2016 added for ATtiny15/25/45/85/216/461/861
    PS_1_0 = 0
    PS_1_1 = 1
    PS_1_2 = 2
    PS_1_4 = 3
    PS_1_8 = 4
    PS_1_16 = 5
    PS_1_32 = 6
    PS_1_64 = 7
    PS_1_128 = 8
    PS_1_256 = 9
    PS_1_512 = 10
    PS_1_1024 = 11
    PS_1_2048 = 12
    PS_1_4096 = 13
    PS_1_8192 = 14
    PS_1_16384 = 15
  end if

  'needs work- "some" AVR with TCCR2 also support 1:32 and 1:128
  if var(TCCR2) then
    PS_2_0 = 0              ' no clock source
    PS_2_1 = 1
    PS_2_8 = 2
    PS_2_64 = 3
    PS_2_256 = 4
    PS_2_1024 = 5
  end if

  if var(TCCR2B) then
    PS_2_0 = 0              ' no clock source
    PS_2_1 = 1
    PS_2_8 = 2
    PS_2_32 = 3
    PS_2_64 = 4
    PS_2_128 = 5
    PS_2_256 = 6
    PS_2_1024 = 7
  end if

  '' "ALL" AVR with TCCR2A and no TCCR2B support these prescales - WMR
  if var(TCCR2A) then
     If noVar(TCCR2B) then
        PS_2_0 = 0              ' no clock source
        PS_2_1 = 1
        PS_2_8 = 2
        PS_2_32 = 3
        PS_2_64 = 4
        PS_2_128 = 5
        PS_2_256 = 6
        PS_2_1024 = 7
     End if
  end if

#endscript

#define AVR_EXT_TMR_0_FE 6    'external, clock on falling edge
#define AVR_EXT_TMR_0_RE 7    'external, clock on rising edge

#define AVR_EXT_TMR_1_FE 6    'external, clock on falling edge
#define AVR_EXT_TMR_1_RE 7    'external, clock on rising edge

#define AVR_EXT_TMR_2_FE 6    'external, clock on falling edge
#define AVR_EXT_TMR_2_RE 7    'external, clock on rising edge

#define AVR_EXT_TMR_3_FE 6    'external, clock on falling edge
#define AVR_EXT_TMR_3_RE 7    'external, clock on rising edge

#define AVR_EXT_TMR_4_FE 6    'external, clock on falling edge
#define AVR_EXT_TMR_4_RE 7    'external, clock on rising edge

#define AVR_EXT_TMR_5_FE 6    'external, clock on falling edge
#define AVR_EXT_TMR_5_RE 7    'external, clock on rising edge

''AVR prescaler settings
'#define PS_1 1
'#define PS_8 2
'#define PS_64 3
'#define PS_256 4
'#define PS_1024 5
''Also worth noting here,
''0: stop timer

   #ifdef PIC
      'Timer 0 prescales (PIC)
      #define PS0_2 0
      #define PS0_4 1
      #define PS0_8 2
      #define PS0_16 3
      #define PS0_32 4
      #define PS0_64 5
      #define PS0_128 6
      #define PS0_256 7

      'PIC Timer 1 prescales
      #define PS1_1 0
      #define PS1_2 16
      #define PS1_4 32
      #define PS1_8 48

      'Timer 3 prescales
      #define PS3_1 0
      #define PS3_2 16
      #define PS3_4 32
      #define PS3_8 48

      'Timer 5 prescales
      #define PS5_1 0
      #define PS5_2 16
      #define PS5_4 32
      #define PS5_8 48

      'Timer 7 prescales -wmr
      #define PS7_1 0
      #define PS7_2 16
      #define PS7_4 32
      #define PS7_8 48

      'Timer 2 prescales for PIC - added Dec 2016
   #endif

#script

  if bit(T2CKPS2) then
    PS2_1   = 0
    PS2_2   = 1
    PS2_4   = 2
    PS2_8   = 3
    PS2_16  = 4
    PS2_32  = 5
    PS2_64  = 6
    PS2_128 = 7

  End if

  if nobit(T2CKPS2) then
    'Pre Dec 2016 constants
    PS2_1  = 0
    PS2_4  = 1
    PS2_16 = 2
    PS2_64 = 3

  end if

#endscript


'Timer 4 prescales for PIC - added Dec 2016

#script

  if bit(T4CKPS2) then
    PS4_1   = 0
    PS4_2   = 1
    PS4_4   = 2
    PS4_8   = 3
    PS4_16  = 4
    PS4_32  = 5
    PS4_64  = 6
    PS4_128 = 7

  End if

 if nobit(T4CKPS2) then
    'Pre Dec 2016 constants
    PS4_1  = 0
    PS4_4  = 1
    PS4_16 = 2
    PS4_64 = 3

  end if

#endscript

'Timer 6 prescales for PIC - added Dec 2016
#script

  if bit(T6CKPS2) then
    PS6_1   = 0
    PS6_2   = 1
    PS6_4   = 2
    PS6_8   = 3
    PS6_16  = 4
    PS6_32  = 5
    PS6_64  = 6
    PS6_128 = 7

  End if

  if nobit(T6CKPS2) then
    'Pre Dec 2016 constants
    PS6_1  = 0
    PS6_4  = 1
    PS6_16 = 2
    PS6_64 = 3

  end if

#endscript


#Script

  if bit(T8CKPS2) then
    PS8_1   = 0
    PS8_2   = 1
    PS8_4   = 2
    PS8_8   = 3
    PS8_16  = 4
    PS8_32  = 5
    PS8_64  = 6
    PS8_128 = 7

  End if

  if nobit(T8CKPS2) then
    'Pre Dec 2016 constants
    PS8_1  = 0
    PS8_4  = 1
    PS8_16 = 2
    PS8_64 = 3

  end if

#Endscript


'Timer 10 prescales -wmr
#define PS10_1 0
#define PS10_4 1
#define PS10_16 2
#define PS10_64 3

'Timer 12 prescales -wmr
#define PS12_1 0
#define PS12_4 1
#define PS12_16 2
#define PS12_64 3

'Start/Clear/Stop subs
Sub StartTimer(In TMRNumber)
  #ifdef PIC

      #ifdef USE_Timer0 TRUE
         #ifdef bit(TMR0ON)
            IF TMRNumber = 0 then Set TMR0ON on
         #endif


         #ifdef bit(T0EN)
            IF TMRNumber = 0 then Set T0EN on
         #endif
      #endif

      #ifdef USE_Timer1 TRUE
         #ifdef bit(TMR1ON)
            IF TMRNumber = 1 then Set TMR1ON on
         #endif
      #endif

      #ifdef USE_Timer2 TRUE
         #ifdef bit(TMR2ON)
            IF TMRNumber = 2 then Set TMR2ON on
         #endif
      #endif

      #ifdef USE_Timer3 TRUE
         #ifdef bit(TMR3ON)
            IF TMRNumber = 3 then Set TMR3ON on
         #endif
      #endif

      #ifdef USE_Timer4 TRUE
         #ifdef bit(TMR4ON)
            IF TMRNumber = 4 then Set TMR4ON on
         #endif
      #endif

      #ifdef USE_Timer5 TRUE
         #ifdef bit(TMR5ON)
            IF TMRNumber = 5 then Set TMR5ON on
         #endif
      #endif

      #ifdef USE_Timer6 TRUE
         #ifdef bit(TMR6ON)
            IF TMRNumber = 6 then Set TMR6ON on
         #endif
      #endif

      #ifdef USE_Timer7 TRUE
         #ifdef bit(TMR7ON)
            IF TMRNumber = 7 then Set TMR7ON on
         #endif
      #endif

      #ifdef USE_Timer8 TRUE
         #ifdef bit(TMR8ON)
            IF TMRNumber = 8 then Set TMR8ON on
         #endif
      #endif

      #ifdef USE_Timer10 TRUE
         #ifdef bit(TMR10ON)
            IF TMRNumber = 10 then Set TMR10ON on
         #endif
      #endif

      #ifdef USE_Timer12 TRUE
         #ifdef bit(TMR12ON)
            IF TMRNumber = 12 then Set TMR12ON on
         #endif
      #endif

  #endif


  #ifdef AVR

     'Need to set clock select bits to 0 (Stops Timer)
      #ifndef Var(TCCR0B)
        #ifdef Var(TCCR0)
           If TMRNumber = 0 Then
              TCCR0 = TMR0_TMP
            End If
        #endif
     #endif

     #ifdef Var(TCCR0B)
        If TMRNumber = 0 Then
          TCCR0B = TCCR0B And 248 Or TMR0_TMP
        End If
     #endif

     #ifdef Var(TCCR1B)
        If TMRNumber = 1 Then
           #ifndef Var(TC1H)
             TCCR1B = TCCR1B And 248 Or TMR1_TMP
           #endif

           #ifdef var(TC1H)  '05/01/2016 added for ATtiny216/461/861
               TCCR1B = TCCR1B And 240 Or TMR1_TMP  '4bit preset
           #endif
        End If
      #endif

     #ifdef Var(TCCR1)  '05/01/2016 added for ATtiny15/25/45/85
        If TMRNumber = 1 Then
          TCCR1 = TCCR1 And 240 Or TMR1_TMP  '4bit preset
        End If
     #endif

     #ifdef Var(TCCR2B)
        If TMRNumber = 2 Then
          TCCR2B = TCCR2B And 248 Or TMR2_TMP
        End If
     #endif

     #ifdef Var(TCCR2)
        If TMRNumber = 2 Then
          TCCR2 = TCCR2 And 248 Or TMR2_TMP
        End If
     #endif

      #ifdef Var(TCCR2A)
        If TMRNumber = 2 Then
          TCCR2A = TCCR2A And 248 Or TMR2_TMP
        End If
     #endif


     #ifdef Var(TCCR3B)
        If TMRNumber = 3 Then
          TCCR3B = TCCR3B And 248 Or TMR3_TMP
        End If
     #endif

     #ifdef Var(TCCR4B)
        If TMRNumber = 4 Then
          TCCR4B = TCCR4B And 248 Or TMR4_TMP
        End If
     #endif

           'Timer5
     #ifdef Var(TCCR5B)
        If TMRNumber = 5 Then
          TCCR5B = TCCR5B And 248 Or TMR5_TMP
        End If
     #endif

    // AVRDX support   - Starttimer
    #ifdef Var(TCA0_SINGLE_CTRLA)
      If TMRNumber = 0 Then
        TCA0_SINGLE_CTRLA = TCA0_SINGLE_CTRLA OR TCA_SINGLE_ENABLE_bm OR TMR0_TMP
      End If
    #endif

  #endif

End Sub

Sub ClearTimer (In TMRNumber)

  #ifdef PIC

    #ifdef USE_Timer0 TRUE
         #ifndef Var(TMR0H) ;Timer 0  is not 16-bit capable
            if TMRNumber = 0 then  TMR0 = 0
         #endif

          #ifdef Var(TMR0H)  '18F or other with 8/16 bit timer

             #ifndef TMR0_16BIT
                if TMRNumber = 0 then TMR0L = 0
               ;don't clear the TMR0H PR0/MATCH register !
             #endif

             #ifdef TMR0_16BIT
                if TMRNumber = 0 then
                   TMR0H = 0
                   TMR0L = 0
                end if
             #endif

         #endif
    #endif

    #ifdef USE_Timer1 TRUE
         #ifdef Var(T1CON)
            If TMRNumber = 1 Then
                TMR1H = 0
                TMR1L = 0
            End If
         #endif
    #endif

    #ifdef USE_Timer2 TRUE
         #ifdef Var(T2CON)
            If TMRNumber = 2 Then TMR2 = 0
         #endif
    #endif

    #ifdef USE_Timer3 TRUE
         #ifdef Var(T3CON)
            If TMRNumber = 3 then
                TMR3H = 0
                TMR3L = 0
            End If
         #endif
    #endif

    #ifdef USE_Timer4 TRUE
         #ifdef Var(T4CON)
            If TMRNumber = 4 Then TMR4 = 0
         #endif
    #endif

    #ifdef USE_Timer5 TRUE
         #ifdef Var(T5CON)
            If TMRNumber = 5 then
                TMR5H = 0
                TMR5L = 0
            End If
         #endif
    #endif

    #ifdef USE_Timer6 TRUE
         #ifdef Var(T6CON)
            If TMRNumber = 6 Then TMR6 = 0
         #endif
    #endif

    #ifdef USE_Timer7 TRUE
         #ifdef Var(T7CON)
            If TMRNumber = 7 then
               TMR7H = 0
               TMR7L = 0
            End If
         #endif
    #endif

    #ifdef USE_Timer8 TRUE
         #ifdef Var(T8CON)
            If TMRNumber = 8 Then TMR8 = 0
         #endif
    #endif

    #ifdef USE_Timer10 TRUE
         #ifdef Var(T10CON)
            If TMRNumber = 10 Then TMR10 = 0
         #endif
    #endif

    #ifdef USE_Timer12 TRUE
         #ifdef Var(T12CON)
            If TMRNumber = 12 Then TMR12 = 0
         #endif
    #endif

  #endif


  #ifdef AVR

     If TMRNumber = 0 Then
        #ifdef Var(TCNT0)
          TCNT0 = 0
        #endif

        #ifdef Var(TCNT0L)  'added 05/01/2016 Timer0 16bit support (ATtiny 261/461/861)
           #ifdef TMR0_16BIT
             TCNT0H = 0
           #endif
           TCNT0L = 0
        #endif
     End If

     #ifdef Var(TCNT1L)
        If TMRNumber = 1 Then
          TCNT1H = 0
          TCNT1L = 0
        End If
     #endif

     #ifdef Var(TC1H)  'added 05/01/2016 for ATtiny 261/461/861
        If TMRNumber = 1 Then
          TC1H = 0      'must be written before TCNT1
        End If
     #endif

     #ifdef Var(TCNT1)  'added 05/01/2016 for ATtiny 15/25/45/85, 261/461/861
        If TMRNumber = 1 Then
          TCNT1 = 0
        End If
     #endif

     #ifdef Var(TCNT2)
        If TMRNumber = 2 Then
          TCNT2 = 0
        End If
     #endif

     #ifdef Var(TCNT3L)
        If TMRNumber = 3 Then
          TCNT3H = 0
          TCNT3L = 0
        End If
     #endif

     #ifdef Var(TCNT4L)
        If TMRNumber = 4 Then
          TCNT4H = 0
          TCNT4L = 0
        End If
     #endif

     #ifdef Var(TCNT5L)
        If TMRNumber = 5 Then
          TCNT5H = 0
          TCNT5L = 0
        End If
     #endif

  #endif

End Sub


'Added overloaded SetTimer to resolve the 8/16 bit timer0 issue on the newer classes of chips with T016BIT bit
'This simply take the Byte value in TMRValue, put in the TMRValue_H hi-byte and then clears [byte]TMRValue, then, calls the existing method.
'This means
Sub SetTimer (In TMRNumber, In TMRValue As Byte)

  Dim TMRValue as word

  #ifdef PIC
    #ifdef USE_Timer0 TRUE
      #ifdef bit(T016BIT)
          If TMRNumber = 0 Then
              TMRValue_H = [byte]TMRValue
              [byte]TMRValue = 0
          end if
      #endif
    #endif
  #endif

  SetTimer ( TMRNumber, [word]TMRValue )

End Sub

' if device has T016BIT then YOU MUST pass a 16bit number
' Revised Settimer to improve support for 16f188xx and T016BIT 15/7/16
Sub SetTimer (In TMRNumber, In TMRValue As Word)


  #ifdef PIC
    #ifdef USE_Timer0 TRUE
       If TMRNumber = 0 Then

          #ifndef Var(TMR0H)
             ' Handle chips withOUT TMR0H
             TMR0 = TMRValue
             exit sub 'just get out faster
          #endif


          #ifdef Var(TMR0H)
             ' Handle chips with TMR0H
             #ifdef TMR0_16BIT
                TMR0H = TMRValue_H
                TMR0L = TMRValue
                ' exit to prevent default setting TMR0L = TMRValue
                exit sub
             #endif


             #ifndef TMR0_16BIT
               'Use default 8-bit mode therefore _TMR0_16BIT_ not defined
                #ifdef bit(T016BIT)
                   'Added to resolve using 8bit Timer0 with enhance timer
                  'High byte is timer0 register of interest
                   TMR0H = TMRValue_H
                #endif

                #ifndef bit(T016BIT)
                    'This is therefore the default action of any chip with TMR0H and the T016BIT does not exist
                    'Low byte is timer0 register of interest
                   TMR0L = TMRValue
                #endif

             #endif

          #endif

       End If
    #endif

    #ifdef USE_Timer1 TRUE
     #ifdef Var(T1CON)
        If TMRNumber = 1 then
          TMR1H = TMRValue_H
          TMR1L = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer2 TRUE
     #ifdef Var(T2CON)
        If TMRNumber = 2 Then
           TMR2 = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer3 TRUE
     #ifdef Var(T3CON)
        If TMRNumber = 3 then
          TMR3H = TMRValue_H
          TMR3L = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer4 TRUE
     #ifdef Var(T4CON)
        If TMRNumber = 4 Then
           TMR4 = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer5 TRUE
     #ifdef Var(T5CON)
        If TMRNumber = 5 then
           TMR5H = TMRValue_H
           TMR5L = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer6 TRUE
     #ifdef Var(T6CON)
        If TMRNumber = 6 Then
            TMR6 = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer7 TRUE
     #ifdef Var(T7CON)
        If TMRNumber = 7 then
           TMR7H = TMRValue_H
           TMR7L = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer8 TRUE
     #ifdef Var(T8CON)
        If TMRNumber = 8 Then
           TMR8 = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer10 TRUE
     #ifdef Var(T10CON)
        If TMRNumber = 10 Then
           TMR10 = TMRValue
        End If
     #endif
    #endif

    #ifdef USE_Timer12 TRUE
     #ifdef Var(T12CON)
        If TMRNumber = 12 Then
           TMR12 = TMRValue
        End If
     #endif
    #endif

  #endif

  #ifdef AVR
    #ifdef Var(TCNT0)
      If TMRNumber = 0 Then
        TCNT0 = TMRValue
      End If
    #endif
    #ifdef Var(TCNT1L)
      If TMRNumber = 1 Then
        TCNT1H = TMRValue_H
        TCNT1L = TMRValue
      End If
    #endif
    #ifdef Var(TCNT1)  'added 17/09/2020 for ATtiny 15/25/45/85, 261/461/861
      If TMRNumber = 1 Then
        TCNT1 = TMRValue
      End If
    #endif
    #ifdef Var(TCNT2)
      If TMRNumber = 2 Then
        TCNT2 = TMRValue
      End If
    #endif
    #ifdef Var(TCNT3L)
      If TMRNumber = 3 Then
        TCNT3H = TMRValue_H
        TCNT3L = TMRValue
      End If
    #endif
    #ifdef Var(TCNT4L)
      If TMRNumber = 4 Then
        TCNT4H = TMRValue_H
        TCNT4L = TMRValue
      End If
    #endif
    #ifdef Var(TCNT5L)
      If TMRNumber = 5 Then
        TCNT5H = TMRValue_H
        TCNT5L = TMRValue
      End If
    #endif

    // AVRDX support  
    #ifdef Var(TCA0_SINGLE_PER)
      If TMRNumber = 0 Then
        TCA0_SINGLE_PERIOD = TMRValue
      End If
    #endif

  #endif

End Sub


Sub StopTimer (In TMRNumber)

  #ifdef PIC
    #ifdef USE_Timer0 TRUE
        'Timer 0 always runs
        'Note: Not Always!  Timer 0 on 18F Can be stopped/started

         #ifdef bit(TMR0ON)
             If TMRNumber = 0 Then Set TMR0ON off
         #endif

         #ifdef bit(T0EN)
             If TMRNumber = 0 Then Set T0EN off
         #endif
    #endif

    #ifdef USE_Timer1 TRUE
         #ifdef Bit(TMR1ON)
             If TMRNumber = 1 Then Set TMR1ON OFF
         #endif
    #endif

    #ifdef USE_Timer2 TRUE
         #ifdef Bit(TMR2ON)
             If TMRNumber = 2 Then Set TMR2ON OFF
         #endif
    #endif

    #ifdef USE_Timer3 TRUE
         #ifdef Bit(TMR3ON)
             If TMRNumber = 3 Then Set TMR3ON OFF
         #endif
    #endif

    #ifdef USE_Timer4 TRUE
         #ifdef bit(TMR4ON)
             If TMRNumber = 4 Then Set TMR4ON OFF
         #endif
    #endif

    #ifdef USE_Timer5 TRUE
         #ifdef bit(TMR5ON)
             If TMRNumber = 5 Then Set TMR5ON OFF
         #endif
    #endif

    #ifdef USE_Timer6 TRUE
         #ifdef bit(TMR6ON)
             If TMRNumber = 6 Then Set TMR6ON OFF
         #endif
    #endif

    #ifdef USE_Timer7 TRUE
         #ifdef bit(TMR7ON)
             If TMRNumber = 7 Then Set TMR7ON OFF
         #endif
    #endif

    #ifdef USE_Timer8 TRUE
         #ifdef bit(TMR8ON)
             If TMRNumber = 8 Then Set TMR8ON OFF
         #endif
    #endif

    #ifdef USE_Timer10 TRUE
         #ifdef bit(TMR10ON)
             If TMRNumber = 10 Then Set TMR10ON OFF
         #endif
    #endif

    #ifdef USE_Timer12 TRUE
         #ifdef bit(TMR12ON)
             If TMRNumber = 12 Then Set TMR12ON OFF
         #endif
    #endif
  #endif

  #ifdef AVR
     'Need to set clock select bits to 0
      #ifdef Var(TCCR0B)
        If TMRNumber = 0 Then
          TCCR0B = TCCR0B And 248
        End If
     #endif

     #ifdef Var(TCCR1B)
        If TMRNumber = 1 Then
           #ifndef Var(TC1H)
             TCCR1B = TCCR1B And 248
           #endif

           #ifdef var(TC1H)  '05/01/2016 added for ATtiny216/461/861
             TCCR1B = TCCR1B And 240  '4bit prescaler
           #endif
        End If
     #endif

     #ifdef Var(TCCR1)  '05/01/2016 added for ATtiny15/25/45/85
        If TMRNumber = 1 Then
          TCCR1 = TCCR1 And 240  '4bit prescaler
        End If
     #endif

     #ifdef Var(TCCR2B)
        If TMRNumber = 2 Then
          TCCR2B = TCCR2B And 248
        End If
     #endif

     #ifdef Var(TCCR2A)
        If TMRNumber = 2 Then
          TCCR2A = TCCR2A And 248
        End If
     #endif

     #ifdef Var(TCCR3B)
        If TMRNumber = 3 Then
          TCCR3B = TCCR3B And 248
        End If
     #endif

     #ifdef Var(TCCR4B)
        If TMRNumber = 4 Then
          TCCR4B = TCCR4B And 248
        End If
     #endif

     #ifdef Var(TCCR5B)
        If TMRNumber = 5 Then
          TCCR5B = TCCR5B And 248
        End If
     #endif

    // AVRDX support  - StopTimer
    #ifdef Var(TCA0_SINGLE_CTRLA)
      If TMRNumber = 0 Then
        TCA0_SINGLE_CTRLA = TCA0_SINGLE_CTRLA XOR TCA_SINGLE_ENABLE_bm
      End If
    #endif

  #endif
End Sub

Sub InitTimer0(In TMRSource, In TMRPres, in TMRPost )
 'Equate to      T0CON1 7:5      3:0        T0CON0 3:0

     'Assumed for code below Timer0 is 16-bit capable as we have been passed three parameters
     'Set prescaler
     #ifdef  Var(T0CON1)
         'T0CON1 Sets up  CLK Source (7:5)  Sync (4) & Prescale 3:0
         'T0CON1 POR Val = b'00000000'

         'Re-Use TMRPres as T0CON1 Temp register
         'Keep T0CON1 7:4 and write bits 3:0 to  register
         'Bits therefore will be cleared!
          TMRPres = (T0CON1 And 240 ) OR TMRPres

          'Set the Source bit
          IF TMRSource <> OSC  THEN
             SET TMRPost.5 ON
          ELSE
             SET TMRPost.5 OFF
          END IF

          'Write the TOCON register
           T0CON1 = TMRPres

           #IF ChipSUBFamily = 16104
             T0ASYNC = 1
           #ENDIF

     #endif

     'Assumed for code below Timer0 is 16-bit capable
     'Set Postscaler
     #ifdef  Var(T0CON0)
         'Re-Use TMRPost as T0CON0 Temp register
         'Keep T0CON0 7:5  and write bits 5:0 to register
         'Bits therefore will be cleared!
          TMRPost = (T0CON0 And 224) OR TMRPost



       'Set TO16BIT
        #ifdef TMR0_16BIT
            Set TMRPost.4 ON '16-bit Timer0
        #endif

        #IFNDEF TMR0_16BIT
           Set TMRPost.4 OFF '8-bit Timer0
        #endif

          'Write the TOCON register
           T0CON0 = TMRPost
     #endif


End Sub

Sub InitTimer0(In TMRSource, In TMRPres)

   Dim TMR0_TMP as Byte

  #ifdef PIC
     'Some PICS (18F+) Use T0CON for timer0 Control
     'ALL OTHER PICS USE OPTION_REG for timer0 control
     'Sub modified to set all TMR Control bits at once
     ' * TMRPres, TMRSource & TMRPost now shared *

     if TMRPRes > 7 then TMRPRes = 0 'failsafe

     'If Timer0 is 16-bit capable
     #ifdef  Var(T0CON0)
         'Re-Use TMRPres as T0CON Temp register
         'Keep T0CON0 7 and write bits 2:0 from TMRPres
         'Bits 5,4 & 3 will be cleared!
          TMRPres = (T0CON0 And 128) OR TMRPres

          'Set the Source bit
        IF TMRSource <> OSC  THEN
           SET TMRPres.5 ON
        ELSE
           SET TMRPres.5 OFF
        END IF

       'Set TO8BIT (T0CON.6)
        #ifdef TMR0_16BIT
           Set TMRPres.4 OFF '16-bit Timer0
        #endif

        #IFNDEF TMR0_16BIT
           Set TMRpres.4 ON  '8-bit Timer0
        #endif

          'Write the TOCON register
           T0CON0 = TMRPres
     #endif


     #Ifndef Var(T0CON) 'Timer0 uses OPTION_REG
       'Re-Use TMRPres as T0CON Temp_register
       'Keep T0CON 7:6 and write bits 2:0 from TMRPres
       'Bits 5,4 & 3 will be cleared!
        #IFNDEF ChipFamily 12
           TMRPres = (OPTION_REG AND 192) OR TMRPres

           IF TMRSource = EXT then
              Set TMRPres.5 ON   'EXT
           ELSE
              Set TMRPres.5 OFF  'OSC
           END IF
          'Now Write the OPTION_REG
           OPTION_REG = TMRPres
        #ENDIF

        'Added For baseline Chips with write only option_reg
        #IFDEF ChipFamily 12
           'option_reg (var) defaults to 'b11000111'

           TMRPRes = (OPTION_REG and 192) or TMRPres

           if TMRSource = EXT then
              Set TMRPres.5 ON
           Else
              Set TMRPres.5 Off
           End if

          OPTION_REG = TMRPres ' maintian option_reg (Var)

          clrwdt               'clear watchdog timer
          clrw                 'Clear "W" to zero
          addwf tmrpres,W      'Add tmrpres to "W"
          option               'write "W" to option_reg

        #EndIF

     #endif

     'If Timer0 is 16-bit capable
     #ifdef  Var(T0CON)   '18F and a few 16F Chips
         'Re-Use TMRPres as T0CON Temp register
         'Keep T0CON 7:6 and write bits 2:0 from TMRPres
         'Bits 5,4 & 3 will be cleared!
          TMRPres = (T0CON And 192) OR TMRPres

          'Set the Source bit
        IF TMRSource <> OSC  THEN
           SET TMRPres.5 ON
        ELSE
           SET TMRPres.5 OFF
        END IF

       'Set TO8BIT (T0CON.6)
        #ifdef TMR0_16BIT
            Set TMRPres.6 OFF '16-bit Timer0
        #endif

        #IFNDEF TMR0_16BIT
           Set TMRpres.6 ON  '8-bit Timer0
        #endif

          'Write the TOCON register
           T0CON = TMRPres
     #endif


  #endif

  #ifdef AVR

      #IFNDEF CHIPAVRDX
         // Legacy AVRs
         //~ Just need to buffer TMR0Pres
         //~ (And change it for external clock)
         //~ TMRPres, TMRSource now shared and local - WMR
         //~ TMR0_TMP now used as shadow register    - WMR

         If TMRSource = Ext Then
            TMRPres = AVR_EXT_TMR_0_RE
         End If

         #ifdef TMR0_16BIT    //~ 05/01/2016 added for Timer0 16-bit mode
               #ifdef bit(TCW0) 
                  // ATtiny216/461/861 and some others
                  Set TCW0 ON 
               #endif
         #endif
         TMR0_TMP = TMRPres
      #ENDIF

      #IFDEF CHIPAVRDX
         /* AVRDX time0 initialise. OSC Source is ignored. AVRDx cannot select Clock source for specific timer.
         
         In Normal ( Coounter ) mode, the TCA Overflow output is not present on a pin, 
         but the TCA can be used to generate an interrupt when the number of counting events reached the TCAn.CMPn value. 
         The GCBASIC uses the configuration of TCA to generate periodic interrupts on the overflow flag.

         */
           
         //Compare 0
         TCA0_SINGLE_CMP0 = 0x01

         //Compare 1
         TCA0_SINGLE_CMP1 = 0x01

         //Compare 2
         TCA0_SINGLE_CMP2 = 0x01

         //Count
         TCA0_SINGLE_CNT = 0x00

         //CMP2EN disabled; CMP1EN disabled; CMP0EN disabled; ALUPD disabled; WGMODE NORMAL; 
         TCA0_SINGLE_CTRLB = 0x00

         //CMP2OV disabled; CMP1OV disabled; CMP0OV disabled; 
         TCA0_SINGLE_CTRLC = 0x00

         //SPLITM disabled; 
         TCA0_SINGLE_CTRLD = 0x00

         //CMD NONE; LUPD disabled; DIR disabled; 
         TCA0_SINGLE_CTRLECLR = 0x00

         //CMD NONE; LUPD disabled; DIR UP; 
         TCA0_SINGLE_CTRLESET = 0x00

         //CMP2BV disabled; CMP1BV disabled; CMP0BV disabled; PERBV disabled; 
         TCA0_SINGLE_CTRLFCLR = 0x00

         //CMP2BV disabled; CMP1BV disabled; CMP0BV disabled; PERBV disabled; 
         TCA0_SINGLE_CTRLFSET = 0x00

         //DBGRUN disabled; 
         TCA0_SINGLE_DBGCTRL = 0x00

         //EVACT POSEDGE; CNTEI disabled; 
         TCA0_SINGLE_EVCTRL = 0x00

         //CMP2 disabled; CMP1 disabled; CMP0 disabled; OVF enabled; 
         // TCA0_SINGLE_INTCTRL = 0x01

         //CMP2 disabled; CMP1 disabled; CMP0 disabled; OVF disabled; 
         TCA0_SINGLE_INTFLAGS = 0x00

         //Period
         TCA0_SINGLE_PERIOD = 0x00

         //Temporary data for 16-bit Access
         TCA0_SINGLE_TEMP = 0x00

         //CLKSEL = TMRPres; ENABLE enabled; 
         TCA0_SINGLE_CTRLA = TMRPres OR TCA_SINGLE_ENABLE_bm
         TMR0_TMP = TMRPres

      #ENDIF

  #endif

End Sub

Sub InitTimer1(In TMRSource, In TMRPres)

  #ifdef PIC
    'Asummptions
    '- Pwr On Reset State of TxCON for 1/3/5/7 is "0"
    '- TXCON Timer REGISTER for 1/3/5/7 are NOT the Same on ALL PICS
    '- TMRON is Bit0 on all Chips
    '- OSCEN & SOSCEN are always TxCON.3 and perform the same function
    'but, of course, see Datasheet for Detailed Register Information

     #ifdef Var(T1CON)
        'Handle PICs with Timer1 module have T1CON register

        #IF NoVar(T1CLK)
           '
           'Handle NoVar(T1CLK)
           '
           'Test for valid Pres parameter
           '
           'This coding approach uses less memory than multiple boolean "AND" statements
            If TMRPres <> 0 then
               IF TMRPres <> 16 then
                  IF TMRPres <> 32 then
                     IF TMRPres <> 48 then
                        TMRPres = 0
                     END IF
                  END IF
               END IF
            END IF

            'Re-Using TMRPres as TxCON Temp Register
            IF TMR1ON = 1 then Set TMRPres.0 ON  'The timer running/ Dont Stop !

           'Examine parameter TMRSource
            IF TMRSource = OSC then
               #ifdef bit(TMR1CS)
                   'TMR1CS1 is TXCON.1 on some chips
                   Set TMRPres.1 OFF
               #endif

               #ifdef Bit(TMR1CS1)
                   'TMR1CS1 is TXcon.7 on other chips
                   Set TMRPres.7 OFF
               #endif
               'Assumes that SOSCEN and OSCEN are always Bit 3
               Set TMRPres.3 OFF
            END IF

            IF TMRSource = EXT then
               #ifdef bit(TMR1CS)
                   'See datasheet 16f690 for TMR1CS example as T1CON.bit
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR1CS1)
                  Set TMRPres.7 ON
               #endif
               Set TMRPres.3 OFF
            END IF

            If TMRSource = ExtOsc Then
               #ifdef bit(TMR1CS)
                  'See datasheet 16f690 for TMR1CS example as T1CON.bit
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR1CS1)
                   Set TMRPres.7 ON
               #endif
                Set TMRPres.3 ON
            END IF


            If TMRSource = LFINTOSC Then
               'Added to resolve missing clock source  LFINTOSC
               'Chip requires TMR1CS1 and TMR1CS0
               #ifdef bit(TMR1CS)
                   'No supportted on this type of chip
               #endif

               #ifdef Bit(TMR1CS1)
                   Set TMRPres.7 ON
                   Set TMRPres.6 ON
               #endif
               Set TMRPres.3 OFF
            END IF

           'Done building Temp Variable. Now write register
           T1CON = TMRPres
         #endif
     #ENDIF



       #IFDEF VAR(T1CLK)
        ; 33 Newer Chips have TxCLK Register ( TIMER 1/3/5/7 )
        ;
        ' 12/16F16xx   Series
        ' 16F153xx     Series
        ' 16F188xx     Series
        ' 18F1xxK40    series


           If TMRPres <> 0 then
                 IF TMRPres <> 16 then
                    IF TMRPres <> 32 then
                       IF TMRPres <> 48 then
                          TMRPres = 0
                       END IF
                    END IF
                 END IF
           END IF

          IF TMR1ON = 1 then Set TMRPres.0 ON  'The timer running/ Dont Stop !

          IF TMRSource > 15 OR TMRSource <0 then TRMSource = 0  'failsafe

          T1CLK = TMRSource
          T1CON = TMRPres
      #ENDIF

  #ENDIF




  #ifdef AVR
     TMR1_TMP = TMRPres
     If TMRSource = Ext Then
          TMR1_TMP = AVR_EXT_TMR_1_RE
     End If

     #ifdef var(TC1H) '05/01/2016 added for ATtiny216/461/861
         TC1H = 3       'needed to start timer1 in 10bit mode
         OCR1C = 255
     #endif

  #endif
End Sub

Sub InitTimer2 (In TMRSource, In TMRPres, In TMRPost)
   'Overloaded sub to include CLK source newer PIC chips with T2CLK and Multiple timer sources
   'Supported CLK Sources

       '  SOSC      (Same as EXT_OSC)
       '  EXT_OSC   (same as SOSC)
       '  MFINTOSC
       '  LFINTOSC
       '  HFINTOSC
       '  FOSC
       '  FOSC4     (Same as OSC)
       '  OSC       (same as FOSC4)
       '  TxCKIPPS  (Same as EXT)
       '  EXT       (Same as TxCKIPPS)

  #ifdef PIC

       #ifdef Var(T2CON) 'does the timer exist?

           #IFDEF bit(T2CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and TxCLKCON Register

              IF TMRPres > 7 then  TMRPres = 0 'failsafe
              IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T2CLKCON = TMRSOurce
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T2CON.7  'Dont change Enable/RUN state
              T2CON =  TMRPres

           #Endif

       #endif

  #endif
End SUB

Sub InitTimer2 (In TMRPres, In TMRPost)
  #ifdef PIC

       #ifdef Var(T2CON) 'does the timer exist?

           #IFDEF NoBit(T2CKPS2)  ' TYPE 1 Timer2

            'Valid Prescales are   0 - 3
             IF TMRPres > 3 then  TMRPres = 0 'failsafe
             'Valid PostScales are  0 - 15
             IF TMRPost > 15 then  TMRPost = 0 'failsafe

            'Start Building a Temporary Variable
            'Re-Using tmrpres as TxCON shadow register
            '
            ' Alternative Method to Previous inline ASM Method
             '(Saves 3 byte of Memory)
            TMRPres = TMRPres OR (TMRPost * 8)
            If TMR2ON = ON then SET TMRPres.2 ON

           'Write the control register
            T2CON = TMRPres
           #endif


           #IFDEF bit(T2CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and TxCLKCON Register

              IF TMRPres > 7 then  TMRPres = 0 'failsafe
              IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T2CLKCON = 1  'CLK Source  = FOSC/4
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T2CON.7  'Dont change Enable/RUN state
              T2CON =  TMRPres
           #Endif

       #endif

  #endif

  #ifdef AVR
     'Some wacky swapping of variable names here
     'Using TMRPres for Source and TMRPost for Prescale
     TMR2_TMP = TMRPost   'Internal OSC
     If TMRPres = Ext Then
         TMR2_TMP = AVR_EXT_TMR_2_RE
     End If
  #endif
End Sub

Sub InitTimer3(In TMRSource, In TMRPres)

  #ifdef PIC
    'Pwr On Reset State of TxCON for 1/3/5/7 is "0"
    'TXCON Timer REGISTER for 1/3/5/7 are NOT the Same on ALL PICS
    'TMRON is Bit0 on all Chips

    '** OSCEN & SOSCEN are always TxCON.3 and perform the same function
    'See Datasheet for Detailed Register Information

     #ifdef Var(T3CON) 'ALL Pics w/Timer3 module have T3CON reg

        #IF NoVar(T3CLK)

           'Test for valid Pres parameter
            'uses less memory than multiple boulean "AND"
            If TMRPres <> 0 then
               IF TMRPres <> 16 then
                  IF TMRPres <> 32 then
                     IF TMRPres <> 48 then
                        TMRPres = 0
                     END IF
                  END IF
               END IF
            END IF

            'Re-Using TMRPres as TxCON Temp Register
            IF TMR3ON = 1 then Set TMRPres.0 ON  'The timer running/ Dont Stop !

           'Select Case uses too much memory - changed
            IF TMRSource = OSC then
               #ifdef bit(TMR1CS)    'TXCON.1 on some chips
                   Set TMRPres.1 OFF
               #endif

               #ifdef Bit(TMR3CS1)   'TXcon.7 on other chips
                   Set TMRPres.7 OFF
               #endif
               Set TMRPres.3 OFF  'SOSCEN and OSCEN are Always Bit 3
            END IF

            IF TMRSource = EXT then
               #ifdef bit(TMR3CS)
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR3CS1)
                  Set TMRPres.7 ON
               #endif
               Set TMRPres.3 OFF
            END IF

            If TMRSource = ExtOsc Then
               #ifdef bit(TMR3CS)
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR3CS1)
                   Set TMRPres.7 ON
               #endif
                Set TMRPres.3 ON
            END IF

            If TMRSource = LFINTOSC Then
               'Added to resolve missing clock source  LFINTOSC
               'Chip requires TMR3CS1 and TMR3CS0
               #ifdef bit(TMR3CS)
                   'No supportted on this type of chip
               #endif

               #ifdef Bit(TMR3CS1)
                   Set TMRPres.7 ON
                   Set TMRPres.6 ON
               #endif
               Set TMRPres.3 OFF
            END IF

           'Done building Temp Variable. Now write register
           T3CON = TMRPres
         #endif
     #ENDIF


      ; 33 Newer Chips have TxCLK Register ( TIMER 1/3/5/7 )
      ;
      ' 12/16F16xx   Series
      ' 16F153xx     Series
      ' 16F188xx     Series
      ' 18F1xxK40    series

       #IFDEF VAR(T3CLK)  ;

           If TMRPres <> 0 then
                 IF TMRPres <> 16 then
                    IF TMRPres <> 32 then
                       IF TMRPres <> 48 then
                          TMRPres = 0
                       END IF
                    END IF
                 END IF
           END IF

          IF TMR3ON = 1 then Set TMRPres.0 ON  'The timer running/ Dont Stop !

          IF TMRSource > 15 OR TMRSource <0 then TRMSource = 0  'failsafe

          T3CLK = TMRSource
          T3CON = TMRPres
      #ENDIF

  #ENDIF

   #ifdef AVR
      TMR3_TMP = TMRPres
      If TMRSource = Ext Then
        TMR3_TMP  = AVR_EXT_TMR_3_RE
      End If
   #endif
End Sub

Sub InitTimer4 (In TMRSource, In TMRPres, In TMRPost)
   'Overloaded sub to include CLK source newer PIC chips with T2CLK and Multiple timer sources
   'Supported CLK Sources

       '  SOSC      (Same as EXT_OSC)
       '  EXTOSC   (same as SOSC)
       '  MFINTOSC
       '  LFINTOSC
       '  HFINTOSC
       '  FOSC
       '  FOSC4     (Same as OSC)
       '  OSC       (same as FOSC4)
       '  TxCKIPPS  (Same as EXT)
       '  EXT       (Same as TxCKIPPS)

  #ifdef PIC

       #ifdef Var(T4CON) 'does the timer exist?

           #IFDEF bit(T4CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and TxCLKCON Register

              IF TMRPres > 7 then  TMRPres = 0 'failsafe
              IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T4CLKCON = TMRSOurce
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T4CON.7  'Dont change Enable/RUN state
              T4CON =  TMRPres

           #Endif

       #endif

  #endif
End SUB


Sub InitTimer4 (In TMRPres, In TMRPost)
 #ifdef PIC

       #ifdef Var(T4CON) 'does the timer exist?

           #IFDEF NoBit(T4CKPS2)  ' TYPE 1 Timer2


            'Valid Prescales are   0 - 3
             IF TMRPres > 3 then  TMRPres = 0 'failsafe
             'Valid PostScales are  0 - 15
             IF TMRPost > 15 then  TMRPost = 0 'failsafe

            'Start Building a Temporary Variable
            'Re-Using tmrpres as TxCON shadow register
            '
            ' Alternative Method to Previous inline ASM Method
             '(Saves 3 byte of Memory)
            TMRPres = TMRPres OR (TMRPost * 8)
            If TMR4ON = ON then SET TMRPres.2 ON

           'Write the control register
            T4CON = TMRPres
           #endif


           #IFDEF bit(T4CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and T2CLKCON Register

             IF TMRPres > 7 then  TMRPres = 0 'failsafe
             IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T4CLKCON = 1  'CLK Source  = FOSC/4
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T4CON.7  'Dont change Enable/RUN state
              T4CON =  TMRPres
           #Endif

       #endif

  #endif

  #ifdef AVR
     TMR4_TMP = TMRPost
     If TMRSource = Ext Then
         TMR_TMP = AVR_EXT_TMR_4_RE
     End If
  #endif

End Sub

Sub InitTimer5(In TMRSource, In TMRPres)
  #ifdef PIC
    'Pwr On Reset State of TxCON for 1/3/5/7 is "0"
    'TXCON Timer REGISTER for 1/3/5/7 are NOT the Same on ALL PICS
    'TMRON is Bit0 on all Chips

    '** OSCEN & SOSCEN are always TxCON.3 and perform the same function
    'See Datasheet for Detailed Register Information

     #ifdef Var(T5CON) 'ALL Pics w/Timer5 module have T5CON reg

        #IF NoVar(T5CLK)

           'Test for valid Pres parameter
            'uses less memory than multiple boulean "AND"
            If TMRPres <> 0 then
               IF TMRPres <> 16 then
                  IF TMRPres <> 32 then
                     IF TMRPres <> 48 then
                        TMRPres = 0
                     END IF
                  END IF
               END IF
            END IF

            'Re-Using TMRPres as TxCON Temp Register
            IF TMR5ON = 1 then Set TMRPres.0 ON  'The timer running/ Dont Stop !

           'Select Case uses too much memory - changed
            IF TMRSource = OSC then
               #ifdef bit(TMR5CS)    'TXCON.1 on some chips
                   Set TMRPres.1 OFF
               #endif

               #ifdef Bit(TMR5CS1)   'TXcon.7 on other chips
                   Set TMRPres.7 OFF
               #endif
               Set TMRPres.3 OFF  'SOSCEN and OSCEN are Always Bit 3
            END IF

            IF TMRSource = EXT then
               #ifdef bit(TMR5CS)
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR5CS1)
                  Set TMRPres.7 ON
               #endif
               Set TMRPres.3 OFF
            END IF

            If TMRSource = ExtOsc Then
               #ifdef bit(TMR5CS)
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR5CS1)
                   Set TMRPres.7 ON
               #endif
                Set TMRPres.3 ON
            END IF

            If TMRSource = LFINTOSC Then
               'Added to resolve missing clock source  LFINTOSC
               'Chip requires TMR5CS1 and TMR5CS0
               #ifdef bit(TMR5CS)
                   'No supportted on this type of chip
               #endif

               #ifdef Bit(TMR5CS1)
                   Set TMRPres.7 ON
                   Set TMRPres.6 ON
               #endif
               Set TMRPres.3 OFF
            END IF

           'Done building Temp Variable. Now write register
           T5CON = TMRPres
         #endif
     #ENDIF


      ; 33 Newer Chips have TxCLK Register ( TIMER 1/3/5/7 )
      ;
      ' 12/16F16xx   Series
      ' 16F153xx     Series
      ' 16F188xx     Series
      ' 18F1xxK40    series

       #IFDEF VAR(T5CLK)  ;

           If TMRPres <> 0 then
                 IF TMRPres <> 16 then
                    IF TMRPres <> 32 then
                       IF TMRPres <> 48 then
                          TMRPres = 0
                       END IF
                    END IF
                 END IF
           END IF

          IF TMR5ON = 1 then Set TMRPres.0 ON  'The timer running/ Dont Stop !

          IF TMRSource > 15 OR TMRSource <0 then TRMSource = 0  'failsafe

          T5CLK = TMRSource
          T5CON = TMRPres
      #ENDIF

  #ENDIF

  #ifdef AVR
     TMR5_TMP = TMRPres
     If TMRSource = Ext Then
         TMR5_TMP = AVR_EXT_TMR_5_RE
     End If
  #endif

End Sub

Sub InitTimer6 (In TMRSource, In TMRPres, In TMRPost)
   'Overloaded sub to include CLK source newer PIC chips with T2CLK and Multiple timer sources
   'Supported CLK Sources

       '  SOSC      (Same as EXT_OSC)
       '  EXT_OSC   (same as SOSC)
       '  MFINTOSC
       '  LFINTOSC
       '  HFINTOSC
       '  FOSC
       '  FOSC4     (Same as OSC)
       '  OSC       (same as FOSC4)
       '  TxCKIPPS  (Same as EXT)
       '  EXT       (Same as TxCKIPPS)

  #ifdef PIC

       #ifdef Var(T6CON) 'does the timer exist?

           #IFDEF bit(T6CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and TxCLKCON Register

              IF TMRPres > 7 then  TMRPres = 0 'failsafe
              IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T6CLKCON = TMRSOurce
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T6CON.7  'Dont change Enable/RUN state
              T6CON =  TMRPres

           #Endif

       #endif

  #endif
End SUB


Sub InitTimer6 (In TMRPres, In TMRPost)
    'See Intimer2 for notes
  #ifdef PIC

       #ifdef Var(T6CON) 'does the timer exist?

           #IFDEF NoBit(T6CKPS2)  ' TYPE 1 Timer2


            'Valid Prescales are   0 - 3
             IF TMRPres > 3 then  TMRPres = 0 'failsafe
             'Valid PostScales are  0 - 15
             IF TMRPost > 15 then  TMRPost = 0 'failsafe

            'Start Building a Temporary Variable
            'Re-Using tmrpres as TxCON shadow register
            '
            ' Alternative Method to Previous inline ASM Method
             '(Saves 3 byte of Memory)
            TMRPres = TMRPres OR (TMRPost * 8)
            If TMR6ON = ON then SET TMRPres.2 ON

           'Write the control register
            T6CON = TMRPres
           #endif


           #IFDEF bit(T6CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and T2CLKCON Register

             IF TMRPres > 7 then  TMRPres = 0 'failsafe
             IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T6CLKCON = 1  'CLK Source  = FOSC/4
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T6CON.7  'Dont change Enable/RUN state
              T6CON =  TMRPres
           #Endif

       #endif

  #endif
End Sub

Sub InitTimer7(In TMRSource, In TMRPres)

 ''' Timer 7 is only supported by the following PIC CHIPS



  #ifdef PIC
    'Pwr On Reset State of TxCON for 1/3/5/7 is "0"
    'TXCON Timer REGISTER for 1/3/5/7 are NOT the Same on ALL PICS
    'TMRON is Bit0 on all Chips

    '** OSCEN & SOSCEN are always TxCON.3 and perform the same function
    'See Datasheet for Detailed Register Information

     #ifdef Var(T7CON) 'ALL Pics w/Timer7 module have T7CON reg

        #IF NoVar(T7CLK)

           'Test for valid Pres parameter
            'uses less memory than multiple boulean "AND"
            If TMRPres <> 0 then
               IF TMRPres <> 16 then
                  IF TMRPres <> 32 then
                     IF TMRPres <> 48 then
                        TMRPres = 0
                     END IF
                  END IF
               END IF
            END IF

            'Re-Using TMRPres as TxCON Temp Register
            IF TM7ON = 1 then Set TMRPres.0 ON  'The timer is running/ Dont Stop !

           'Select Case uses too much memory - changed
            IF TMRSource = OSC then
               #ifdef bit(TMR7CS)    'TXCON.1 on some chips
                   Set TMRPres.1 OFF
               #endif

               #ifdef Bit(TMR7CS1)   'TXcon.7 on other chips
                   Set TMRPres.7 OFF
               #endif
               Set TMRPres.3 OFF  'SOSCEN and OSCEN are Always Bit 3
            END IF

            IF TMRSource = EXT then
               #ifdef bit(TMR7CS)
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR7CS1)
                  Set TMRPres.7 ON
               #endif
               Set TMRPres.3 OFF
            END IF

            If TMRSource = ExtOsc Then
               #ifdef bit(TMR7CS)
                   Set TMRPres.1 ON
               #endif

               #ifdef Bit(TMR7CS1)
                   Set TMRPres.7 ON
               #endif
                Set TMRPres.3 ON
            END IF
           'Done building Temp Variable. Now write register

          T7CON = TMRPres

         #endif
     #ENDIF


      ; 33 Newer Chips have TxCLK Register ( TIMER 1/3/5/7 )
      ;
      ' 12/16F16xx   Series
      ' 16F153xx     Series
      ' 16F188xx     Series
      ' 18F1xxK40    series

       #IFDEF VAR(T3CLK)  ;

           If TMRPres <> 0 then
                 IF TMRPres <> 16 then
                    IF TMRPres <> 32 then
                       IF TMRPres <> 48 then
                          TMRPres = 0
                       END IF
                    END IF
                 END IF
           END IF

          IF TMR7ON = 1 then Set TMRPres.0 ON  'The timer running/ Dont Stop !

          IF TMRSource > 15 OR TMRSource <0 then TRMSource = 0  'failsafe

          T7CLK = TMRSource
          T7CON = TMRPres
      #ENDIF

  #ENDIF
End Sub

Sub InitTimer8 (In TMRSource, In TMRPres, In TMRPost)
   'Overloaded sub to include CLK source newer PIC chips with T2CLK and Multiple timer sources
   'Supported CLK Sources

       '  SOSC      (Same as EXT_OSC)
       '  EXT_OSC   (same as SOSC)
       '  MFINTOSC
       '  LFINTOSC
       '  HFINTOSC
       '  FOSC
       '  FOSC4     (Same as OSC)
       '  OSC       (same as FOSC4)
       '  TxCKIPPS  (Same as EXT)
       '  EXT       (Same as TxCKIPPS)

  #ifdef PIC

       #ifdef Var(T8CON) 'does the timer exist?

           #IFDEF bit(T8CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and TxCLKCON Register

              IF TMRPres > 7 then  TMRPres = 0 'failsafe
              IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T8CLKCON = TMRSOurce
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T8CON.7  'Dont change Enable/RUN state
              T8CON =  TMRPres

           #Endif

       #endif

  #endif
End SUB


Sub InitTimer8 (In TMRPres, In TMRPost)

 #ifdef PIC

       #ifdef Var(T8CON) 'does the timer exist?

           #IFDEF NoBit(T8CKPS2)  ' TYPE 1 Timer2


            'Valid Prescales are   0 - 3
             IF TMRPres > 3 then  TMRPres = 0 'failsafe
             'Valid PostScales are  0 - 15
             IF TMRPost > 15 then  TMRPost = 0 'failsafe

            'Start Building a Temporary Variable
            'Re-Using tmrpres as TxCON shadow register
            '
            ' Alternative Method to Previous inline ASM Method
             '(Saves 3 byte of Memory)
            TMRPres = TMRPres OR (TMRPost * 8)
            If TMR8ON = ON then SET TMRPres.2 ON

           'Write the control register
            T8CON = TMRPres
           #endif


           #IFDEF bit(T8CKPS2)  '// Type 2 Timer 2
              'Enhanced Chips with 1:128 prescale and T2CLKCON Register

             IF TMRPres > 7 then  TMRPres = 0 'failsafe
             IF TMRPost > 15 then  TMRPost = 0 'failsafe

              T8CLKCON = 1  'CLK Source  = FOSC/4
              TMRPres = (TMRPres * 16) + TMRPost
              TMRPres.7 = T8CON.7  'Dont change Enable/RUN state
              T8CON =  TMRPres
           #Endif

       #endif

  #endif

End SUB

Sub InitTimer10 (In TMRPres, In TMRPost)
    'See Intimer2 for notes
  #ifdef PIC
     #ifdef Var(T10CON)
        IF TMRPres > 3 then  TMRPres = 0
        IF TMRPost > 15 then  TMRPost = 0
        TMRPres = TMRPres OR (TMRPost * 8)
        If TMR10ON = ON then SET TMRPres.2 ON
        T10CON = TMRPres 'write the register
     #endif
  #endif
End Sub

Sub InitTimer12 (In TMRPres, In TMRPost)
  #ifdef PIC
    'See Intimer2 for notes
     #ifdef Var(T12CON)
         IF TMRPres > 3 then  TMRPres = 0
         IF TMRPost > 15 then  TMRPost = 0
         TMRPres = TMRPres OR (TMRPost * 8)
         If TMR12ON = ON then SET TMRPres.2 ON
         T12CON = TMRPres 'write the register
     #endif
  #endif
End Sub
