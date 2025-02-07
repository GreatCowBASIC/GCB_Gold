/*
      millis() function for GCBASIC

    Copyright (C) 2018-2025, 2025 Chris Roper, Evan Venn
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
    MA 02110-1301  USA
  */

'Changes:
    // 30/12/2018: Initial test release - 8 Bit TIMER0, 16Mhz Clock - ChrisR
    // 02/01/2019: 8 Bit TIMER0, 1Mhz - 32Mhz Clock support - ChrisR
    // 04/01/2019: Add PIC support for 16Bit Timer0 - EvanV / ChrisR
    // 06/01/2019: Add  support for 16Bit Timer0 no postscaler - EvanV
    // 04/04/2019: Add AVR support - 16mhz only
    // 09/10/2020: Add AVR support - 32..1 mhz
    // 10/08/2021: Add Fix for errata bug in DS40001816F in 18fX6K40 chips
    // 21/10/2023: Add Fix for legacy 18F chips where T0CON1 does not exist.
    // 01/09/2024: Add AVRDx support

'***********************************************************
// User adjustments
// User can used #DEFINE MILLIS_TIMER_VALUE_ADJUST n to change the delta timer value to decrease or increase latency
//    #DEFINE MILLIS_TIMER_VALUE_ADJUST 1

// The script determines the period setting for the timer. The conditional test set the prescaler. The rest is common.

// Subroutines:
  // MsCtr_Int_Hdlr
  // Init_MsCtr_Int
  // Millis(void)

  // A constant is used to  set SCRIPT_TMR0INITVAL. When folks have issues we can tell people to change this in the user program when the oscillator is not correctly setup.
    
'**************  Timer 0 Notes ***********************
    // PIC Timer0 is a free running timer on midrange and
    // baseline PICs. On these chips timer 0 always runs
    // and cannot be stopped.   However on most 18Fxxx PICs,
    // timer 0 can be started or stopped via the T0CON.7 Bit.
    // Therefore, on these chips The timer MUST be started with
    // StartTimer 0.

    // On most 18F or Enhanced Core 16F PICS, Timer 0 can be either an 8-Bit     // or 16-Bit timer.  On these chips this is determined by TC0CON.6
    // (T08BIT). The timer defaults to 8-bit.

    // On legacy AVR.  8bit timer.
    // On AVTRDX. 16bit timer.
    // *********************************************************

#script

  if nodef( MILLIS_TIMER_VALUE_ADJUST ) then
    MILLIS_TIMER_VALUE_ADJUST = 0
  end if

  SCRIPT_16BITTIMER0 = 0  
  if bit(T016BIT) then
      SCRIPT_16BITTIMER0 = 1
      // added to Fix for legacy 18F chips where T0CON1 does not exist.
      // These chips are 18Fs where there is only POSTscaler exists, and, not POST and PRE scaler for Timer0.
      if novar(T0CON1) then
        // some 18Fs have T0CON and not T0CON1. So, assume this are 8 bit initialisation of the timer0
        SCRIPT_16BITTIMER0 = 0
      end if
  end if

  ;check Frequency
  SCRIPT_MILLISERRORHANDLER = 0

  if nodef(CHIPAVRDX) then
    //~ Therefore AVR legacy and PIC
    if ChipMHz=64 then
      SCRIPT_MILLISERRORHANDLER=1
      SCRIPT_TMR0INITVAL = 6
    end if
    if ChipMHz=48 then
      SCRIPT_MILLISERRORHANDLER=1
      SCRIPT_TMR0INITVAL = 68
    end if
    if ChipMHz=32 then
      SCRIPT_MILLISERRORHANDLER=1
      if AVR then
          SCRIPT_TMR0INITVAL = 131
      end if
    end if
    if ChipMHz=16 then
      SCRIPT_MILLISERRORHANDLER=1
    end if
    if ChipMHz=8 then
      SCRIPT_MILLISERRORHANDLER=1
      if AVR then
          SCRIPT_TMR0INITVAL = 131
      end if
    end if
    if ChipMHz=4 then
      SCRIPT_MILLISERRORHANDLER=1
      if AVR then
          SCRIPT_TMR0INITVAL = 193
      end if
    end if
    if ChipMHz=2 then
      SCRIPT_MILLISERRORHANDLER=1
    end if
    if ChipMHz=1 then
      SCRIPT_MILLISERRORHANDLER=1
      if AVR then
          SCRIPT_TMR0INITVAL = 131
      end if
    end if
  end if

  if def(CHIPAVRDX) then
    //~ Therefore AVRDX Max20
    if ChipMaxMHz = 20 Then
      if ChipMHz=20 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x9C3
      end if
      if ChipMHz=10 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x4E1
      end if
      if ChipMHz=5 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x270
      end if
      if ChipMHz=3.333333 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x1A0
      end if
      if ChipMHz=2.5 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x138
      end if
      if ChipMHz=2 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0xF9
      end if          
      if ChipMHz=1.666667 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0xCF
      end if      
      if ChipMHz=1.25 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x9B
      end if
    end if

    if ChipMaxMHz = 24 Then
      if ChipMHz=24 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x5D
      end if
      if ChipMHz=20 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x4D
      end if
      if ChipMHz=16 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x3E
      end if      
      if ChipMHz=12 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x2E
      end if
      if ChipMHz=8 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x1E
      end if
      if ChipMHz=4 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x1F3
      end if
      if ChipMHz=3 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0x176
      end if
      if ChipMHz=2 then
        SCRIPT_MILLISERRORHANDLER=1
        SCRIPT_TMR0INITVAL = 0xF9
      end if    
    end if
    
  end if

  if SCRIPT_MILLISERRORHANDLER = 0 Then
      Warning ChipMHz
      Error "Millis() does not support the selected Chip Frequency"
      Warning "Contact us on the Great Cow BASIC forum for more information"
  End if

  // Check family
  // This check could be removed
  SCRIPT_MILLISERRORHANDLER = 0

  if def(CHIPAVRDX) then
    SCRIPT_MILLISERRORHANDLER=1
  end if
  if ChipFamily=100 then  'AVR core version V0E class microcontrollers
    SCRIPT_MILLISERRORHANDLER=1
  End if
  if ChipFamily=110 then    'AVR core version V1E class microcontrollers
    SCRIPT_MILLISERRORHANDLER=1
  End if
  if ChipFamily=120 then   'AVR core version V2E class microcontrollers
    SCRIPT_MILLISERRORHANDLER=1
  end if
  if ChipFamily=122 then   'AVR core version V2E class microcontrollers
    SCRIPT_MILLISERRORHANDLER=1
  end if
  if ChipFamily=130 then   'AVR core version V3E class microcontrollers but essentially the mega32u6 only
    SCRIPT_MILLISERRORHANDLER=1
  end if
  if ChipFamily=12 then
    SCRIPT_MILLISERRORHANDLER=1
  end if
  if ChipFamily=14 then
    SCRIPT_MILLISERRORHANDLER=1
  end if
  if ChipFamily=15 then
    SCRIPT_MILLISERRORHANDLER=1
  end if
  if ChipFamily=16 then
      SCRIPT_MILLISERRORHANDLER=1
  end if

  if SCRIPT_MILLISERRORHANDLER = 0 Then
      Warning ChipFamily
      Warning "Millis() does not support the selected Chip Family"
  End if

#endscript

#startup Init_MsCtr_Int   ' Initialize Hardware Counter and Interrupt

  #Ifdef var(TCA0_SINGLE_PERH) 
    Dim TCA0_SINGLE_PERIOD as Word Alias TCA0_SINGLE_PERH, TCA0_SINGLE_PERL
  #endif


'This will be called when the Timer overflows
dim MsCtr_ as long

Sub MsCtr_Int_Hdlr
  dim MsCtr_ as Long

  asm ShowDebug Call_SetTimer_Millis_macro
  #ifndef CHIPAVRDX
    SetTimer_Millis  SCRIPT_TMR0INITVAL + MILLIS_TIMER_VALUE_ADJUST   ' Reset Inital Counter value
  #endif
  #ifdef CHIPAVRDX
    SetTimer_Millis_AVRDX  SCRIPT_TMR0INITVAL + MILLIS_TIMER_VALUE_ADJUST   ' Reset Inital Counter value
  #endif

  MsCtr_ = MsCtr_ + 1
End Sub


Sub Init_MsCtr_Int
  
  // Add the handler for the interrupt
  On Interrupt Timer0Overflow Call MsCtr_Int_Hdlr
  MsCtr_ = 0
  Millis = 0

  #ifdef AVR

    #ifndef CHIPAVRDX
      #IFDEF ChipMHz 32
        ' Set prescaler to 256, Preload and then start the timer
        InitTimer0 Osc, PS_0_256
      #endif

      #IFDEF ChipMHz 16
        ' Set prescaler to 64, Preload and then start the timer
        InitTimer0 Osc, PS_0_64
      #endif

      #IFDEF ChipMHz 8
        ' Set prescaler to 64, Preload and then start the timer
        InitTimer0 Osc, PS_0_64
      #endif

      #IFDEF ChipMHz 4
        ' Set prescaler to 64, Preload and then start the timer
        InitTimer0 Osc, PS_0_64
      #endif

      #IFDEF ChipMHz 2
        ' Set prescaler to 8, Preload and then start the timer
        InitTimer0 Osc, PS_0_8
      #endif

      #IFDEF ChipMHz 1
        ' Set prescaler to 8, Preload and then start the timer
        InitTimer0 Osc, PS_0_8
      #endif     
    #endif

    #ifdef CHIPAVRDX

      #ifdef ChipMHz 24
        InitTimer0 Osc, PS_0_0
      #endif
      #ifdef ChipMHz 20
        InitTimer0 Osc, PS_0_8  //
      #endif        
      #ifdef ChipMHz 16
        InitTimer0 Osc, PS_0_0
      #endif
      #ifdef ChipMHz 12
        InitTimer0 Osc, PS_0_0
      #endif
      #ifdef ChipMHz 10
        InitTimer0 Osc, PS_0_8 //
      #endif
      #ifdef ChipMHz 8
        InitTimer0 Osc, PS_0_0
      #endif
      #ifdef ChipMHz 5
        InitTimer0 Osc, PS_0_8 //
      #endif
      #ifdef ChipMHz 4
        InitTimer0 Osc, PS_0_8 //
      #endif
      #ifdef ChipMHz 3
        InitTimer0 Osc, PS_0_8
      #endif
      #ifdef ChipMHz 3.333333
        InitTimer0 Osc, PS_0_8 //
      #endif
      #ifdef ChipMHz 2.5
        InitTimer0 Osc, PS_0_8 //
      #endif
      #ifdef ChipMHz 2
        InitTimer0 Osc, PS_0_8 //
      #endif
      #ifdef ChipMHz 1.6666667
        InitTimer0 Osc, PS_0_8 //
      #endif
      #ifdef ChipMHz 1.25
        InitTimer0 Osc, PS_0_8 //
      #endif
    #endif
  #endif

  #ifdef PIC
    #IFDEF ChipFamily 14
      ' Set prescaler
      #IFDEF ChipMHz 16
        InitTimer0 Osc, PS0_4
      #endif

      #IFDEF ChipMHz 8
        InitTimer0 Osc, PS0_8
      #endif

      #IFDEF ChipMHz 4
        InitTimer0 Osc, PS0_4
      #endif

      #IFDEF ChipMHz 2
        InitTimer0 Osc, PS0_2
      #endif

      #IFDEF ChipMHz 1
        PSA = 1 ' PSA = 1 Prescaler is assigned to WDT
        InitTimer0 Osc, PS0_1
      #endif
    #ENDIF

    #IFDEF ChipFamily 15, 16

       #IFDEF ChipMHz 64

          #if SCRIPT_16BITTIMER0 = 0
            asm ShowDebug 8bit / 16bit No Postscaler @ 48
            InitTimer0 Osc, PS0_64
          #endif

          #if SCRIPT_16BITTIMER0 = 1
            asm ShowDebug 16bit capable, but running in 8bit mode
            InitTimer0 Osc, PRE0_64 + TMR0_FOSC4 ,  POST0_1
          #endif
        #endif


       #IFDEF ChipMHz 48

          #if SCRIPT_16BITTIMER0 = 0
            asm ShowDebug 8bit / 16bit No Postscaler @ 48
            InitTimer0 Osc, PS0_64
          #endif

          #if SCRIPT_16BITTIMER0 = 1
            asm ShowDebug 16bit capable, but running in 8bit mode
            InitTimer0 Osc, PRE0_64 + TMR0_FOSC4 ,  POST0_1
          #endif
        #endif

      #IFDEF ChipMHz 32

        #if SCRIPT_16BITTIMER0 = 0
          asm ShowDebug 8bit / 16bit No Postscaler
          InitTimer0 Osc, PS0_32
        #endif

        #if SCRIPT_16BITTIMER0 = 1
          asm ShowDebug 16bit capable, but running in 8bit mode
          InitTimer0 Osc, PRE0_32 + TMR0_FOSC4 ,  POST0_1
        #endif
      #endif

      #IFDEF ChipMHz 16

        #if SCRIPT_16BITTIMER0 = 0
          asm ShowDebug 8bit / 16bit No Postscaler
          InitTimer0 Osc, PS0_16
        #endif

        #if SCRIPT_16BITTIMER0 = 1
          asm ShowDebug 16bit capable, but running in 8bit mode
          InitTimer0 Osc, PRE0_16 + TMR0_FOSC4 ,  POST0_1
        #endif
      #endif

      #IFDEF ChipMHz 8

        #if SCRIPT_16BITTIMER0 = 0
          asm ShowDebug 8bit / 16bit No Postscaler
          InitTimer0 Osc, PS0_8
        #endif

        #if SCRIPT_16BITTIMER0 = 1
          asm ShowDebug 16bit capable, but running in 8bit mode
          InitTimer0 Osc, PRE0_8 + TMR0_FOSC4 ,  POST0_1
        #endif
      #endif

       #IFDEF ChipMHz 4

        #if SCRIPT_16BITTIMER0 = 0
          asm ShowDebug 8bit / 16bit No Postscaler
          InitTimer0 Osc, PS0_4
        #endif

        #if SCRIPT_16BITTIMER0 = 1
          asm ShowDebug 16bit capable, but running in 8bit mode
          InitTimer0 Osc, PRE0_4 + TMR0_FOSC4 ,  POST0_1
        #endif
      #endif

      #IFDEF ChipMHz 2

        #if SCRIPT_16BITTIMER0 = 0
          asm ShowDebug 8bit / 16bit No Postscaler
          InitTimer0 Osc, PS0_2
        #endif

        #if SCRIPT_16BITTIMER0 = 1
          asm ShowDebug 16bit capable, but running in 8bit mode
          InitTimer0 Osc, PRE0_2 + TMR0_FOSC4 ,  POST0_1
        #endif
      #endif

      #IFDEF ChipMHz 1

        #if SCRIPT_16BITTIMER0 = 0
          asm ShowDebug 8bit / 16bit No Postscaler
          InitTimer0 Osc, PS0_1
        #endif

        #if SCRIPT_16BITTIMER0 = 1
          asm ShowDebug 16bit capable, but running in 8bit mode
          InitTimer0 Osc, PRE0_1 + TMR0_FOSC4 ,  POST0_1
        #endif

      #endif

      #IF CHIPSUBFAMILY = 16104
          T0ASYNC = 1
      #ENDIF

    #endif
  #ENDIF

  asm ShowDebug  Call_SetTimer_Millis_macro
  
  #ifndef CHIPAVRDX
    SetTimer_Millis SCRIPT_TMR0INITVAL  + MILLIS_TIMER_VALUE_ADJUST   ' Reset Inital Counter value
  #endif
  #ifdef CHIPAVRDX
    SetTimer_Millis_AVRDX SCRIPT_TMR0INITVAL  + MILLIS_TIMER_VALUE_ADJUST  ' Reset Inital Counter value
  #endif

  asm ShowDebug Call_StartTimer_Millis_macro
  StartTimer_Millis

End Sub

'Return the current ms as Millis
Function Millis as Long
  dim Millis, MsCtr_ as long
  'disable interrupts while we read millis or we might get an
  'inconsistent value (e.g. in the middle of a write to millis)
  IntOff
    Millis = MsCtr_
  IntOn
End Function

'PIC Code taken from timer.h
macro SetTimer_Millis ( In TMRValueMillis As Word)

    #ifdef PIC
        #ifndef Var(TMR0H)
         ' Handle chips withOUT TMR0H
           TMR0 = TMRValueMillis
        #endif

        #ifdef Var(TMR0H)
         ' Handle chips with TMR0H

           #ifdef TMR0_16BIT
              TMR0H = TMRValueMillis_H
              TMR0L = TMRValueMillis
           #endif


           #ifndef TMR0_16BIT
              ' USe default 8-bit mode
               TMR0L = TMRValueMillis
           #endif

        #endif
    #endif

    #ifdef AVR
        TCNT0 = TMRValueMillis
    #endif

End macro

macro SetTimer_Millis_AVRDX ( In TMRValueMillis As Word)

    #ifdef AVR
        TCA0_SINGLE_PERIOD = [WORD]TMRValueMillis
    #endif

End macro

'Code taken from timer.h
macro StartTimer_Millis

  #ifdef PIC
     #ifdef bit(TMR0ON)
        Set TMR0ON on
     #endif


     #ifdef bit(T0EN)
        Set T0EN on
     #endif
  #endif


  #ifdef AVR
    #ifndef CHIPAVRDX
     'Need to set clock select bits to 0 (Stops Timer)
      #ifndef Var(TCCR0B)
        #ifdef Var(TCCR0)
          TCCR0 = TMR0_TMP
        #endif
     #endif

     #ifdef Var(TCCR0B)
      TCCR0B = TCCR0B And 248 Or TMR0_TMP
     #endif
    #endif
    
    #ifdef CHIPAVRDX
      TCA0_SINGLE_CTRLA = TCA0_SINGLE_CTRLA OR TCA_SINGLE_ENABLE_bm
      StartTimer 0
    #endif

  #endif

End macro


Sub SetMillis ( in localCurMs   as long, out localLstMs as long )
  IntOff
  localLstMs = localCurMs
  IntOn
End Sub
