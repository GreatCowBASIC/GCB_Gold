'
'    millis() function for Great Cow BASIC
'
'  Copyright (C) 2018-2024, 2024 Chris Roper, Evan Venn
'  This library is free software; you can redistribute it and/or
'  modify it under the terms of the GNU Lesser General Public
'  License as published by the Free Software Foundation; either
'  version 2.1 of the License, or (at your option) any later version.

'  This library is distributed in the hope that it will be useful,
'  but WITHOUT ANY WARRANTY; without even the implied warranty of
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
'  See the GNU Lesser General Public License for more details.

'  You should have received a copy of the GNU Lesser General Public
'  License along with this library; if not, write to the Free Software
'  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
'  MA 02110-1301  USA

'Changes:
' 30/12/2018: Initial test release - 8 Bit TIMER0, 16Mhz Clock - ChrisR
' 02/01/2019: 8 Bit TIMER0, 1Mhz - 32Mhz Clock support - ChrisR
' 04/01/2019: Add PIC support for 16Bit Timer0 - EvanV / ChrisR
' 06/01/2019: Add  support for 16Bit Timer0 no postscaler - EvanV
' 04/04/2019: Add AVR support - 16mhz only
' 09/10/2020: Add AVR support - 32..1 mhz
' 10/08/2021: Add Fix for errata bug in DS40001816F in 18fX6K40 chips
' 21/10/2023: Add Fix for legacy 18F chips where T0CON1 does not exist.


'***********************************************************

'Subroutines:
' MsCtr_Int_Hdlr
' Init_MsCtr_Int
' Millis(void)
'
' A constant is used to  set SCRIPT_TMR0INITVAL. When folks have issues we can tell people to change this in the user program when the oscillator is not correctly setup.
#define MILLIS_DEFAULT_TIMER0_VALUE 6

'**************  Timer 0 Notes ***********************
' PIC Timer0 is a free running timer on midrange and
' baseline PICs. On these chips timer 0 always runs
' and cannot be stopped.   However on most 18Fxxx PICs,
' timer 0 can be started or stopped via the T0CON.7 Bit.
' Therefore, on these chips The timer MUST be started with
' StartTimer 0.
'
' On most 18F or Enhanced Core 16F PICS, Timer 0 can be either an 8-Bit ' or 16-Bit timer.  On these chips this is determined by TC0CON.6
' (T08BIT). The timer defaults to 8-bit.
'
' On AVR.  8bit timer.
'*********************************************************
'



#script

  SCRIPT_TMR0INITVAL = MILLIS_DEFAULT_TIMER0_VALUE
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

  if SCRIPT_MILLISERRORHANDLER = 0 Then
      Warning ChipMHz
      Warning "Millis() does not support the selected Chip Frequency"
      Warning "Contact us on the Great Cow BASIC forum for more information"
  End if

;Check family
  'This check could be removed
  SCRIPT_MILLISERRORHANDLER = 0
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

'This will be called when the Timer overflows
dim MsCtr_ as long

Sub MsCtr_Int_Hdlr
  dim MsCtr_ as Long

  asm ShowDebug Call_SetTimer_Millis_macro
  SetTimer_Millis  SCRIPT_TMR0INITVAL   ' Reset Inital Counter valueue

  MsCtr_ = MsCtr_ + 1
End Sub


Sub Init_MsCtr_Int
  ' Add the handler for the interrupt
  On Interrupt Timer0Overflow Call MsCtr_Int_Hdlr
  MsCtr_ = 0
  Millis = 0

//~ 
//~ #define PS_0_0 0        ' no clock source
//~ #define PS_0_1 1
//~ #define PS_0_8 2
//~ #define PS_0_64 3
//~ #define PS_0_256 4
//~ #define PS_0_1024 5

  #ifdef AVR

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
  SetTimer_Millis SCRIPT_TMR0INITVAL

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

'Code taken from timer.h
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

End macro



Sub SetMillis ( in localCurMs   as long, out localLstMs as long )
  IntOff
  localLstMs = localCurMs
  IntOn
End Sub
