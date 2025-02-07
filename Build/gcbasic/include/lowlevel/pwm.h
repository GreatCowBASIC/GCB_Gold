'    Pulse Width Modulation routines for GCBASIC
'    Copyright (C) 2006-2025 Hugh Considine, William Roth, Kent Schafer and Evan R. Venn
'
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
'
'********************************************************************************
'IMPORTANT:
'THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
'UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
'COMMANDS UNUSABLE!
'********************************************************************************
'  Test Devices
'  16f19156
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2 and 4
'     PWMon, PWMModule for PWM 3 and 4 but only timer 2 and 4
'     HPWM_CCPTimerN 1, 20, 127, n  for 8bit PWM for timers 2 and 4
'     HPWM 3, 20, 127, 2  for 16bit PWM for timers 2 and 4 only
' PIC16F1709
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2
'     HPWM 3, 20, 127, 2  for 10bit PWM for timers 2 and 4 only
' PIC18F27K40
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2
'     HPWM 3, 20, 127, 2  for 10bit PWM for timers 2 and 4 only
' PIC16F1825 - family 14
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2
'     HPWM_CCPTimerN 1, 20, 127, n  for 8bit PWM for timers 2,4 and 6
'     HPWM 3, 20, 127, 2  for 10bit PWM for timers 2 and 4 only
' 16F1938 - Family 14
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2
'     HPWM 1, 20, 127  for 8bit PWM for timers 2
'     HPWM_CCPTimerN 1, 20, 127, n  for 8bit PWM for timers 2,4 and 6
' 16F886 - Family 0
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2
'     HPWM 1, 20, 127  for 8bit PWM for timers 2
' 16F18855
'     Legacy PWM
'     PWMOn for CCP1 and 2,4 and 6
'     PWMon for PWMModule for PWM 6 and 7 other should work, of course
'     HPWM [6 or 7], 20, 127  for 10bit PWM for timers 2, 4 and 6
' 16F690 - family 0
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2
'     HPWM 1, 20, 127  for 8bit PWM for timers 2
' 18F26K20
'     Legacy PWM
'     PWMOn for CCP1 and 2 but only timer 2
'     HPWM 1, 20, 127 for 8 PWM for timers only

'''Changed position of 'Dim PRx_Temp as word' to remove declaration of variables when not required
''' 14/3/16   Added support for Hardware PMW and revised CCP PWM to support 16f18855 series
''' 01/08/16  Added support for 16f18326* series
''' 08/08/16  Revised to remove the silly error instroduced above when I called bit() with a leading space!
''' 09/08/16  Revised to remove fix the all case ENable option
''' 13/08/16  Added HPWM5 missing constant
''' 10/10/16  Added by EvanV CCP5 support with great help from Isay Goltman
''' 12/10/16  Revised to sort out that the work of 14/3/16 had broken all the legacy chips!  Added alternative BIT() Test
'''           Also, revised calcs to improve performance
''' 1/12/2016 Revised to enhance variable usafe when specific registers do not exist
''' 3/12/2016 Revised to remove script that is not required
''' 4/12/2016 Improved to remove creation of unneeded variables when the register  does not exist
''' 20/2/2017 Revised to add T2CLKCON to resolve T2CLK issues when using GPASM
''' 3/5/2017  Added AVR microcontrollers KentS
''' 13/3/2017 Revised to add HPWM3 10bit and HPWM4 10bit
''' 24/5/2017 Added HPWMOn to reference PWMOn and correct CCPTMRS settings for HPWM3 and HPWM4
''' 25/5/2017 Added support for PWMTimerVariant1 to address timer addressing issue. Added further optimisation to NOT include code when the PWM channel does not exist
''' 26/5/2017 Further improvements of PWMTimerVariant1 to address timer addressing issue
''' 26/5/2017 Revised to add full 10bit Frequency support to the 10bit HPWM function and to add improved HPWMOFF
''' 27/5/2017 Revised 10bit duty to be 10bit duty and rename of PWMTimerVariant1 to ChipPWMTimerVariant
''' 27/5/2017 Add byte support for a 10bit duty method
''' 28/5/2017 Added HPWMUpdate and changed calculateDuty to a sub to save memory
''' 29/5/2017 Improved support as follows:
'''           New constants for user use PRxPeriodRegister and TimerXprescaleselectValue
'''           Support for 8bit call to 10bit PWM (non-CCP)
'''           Memory improvements in terms of program size
'''           Support for HPWM_Fast within 10bit PWM (non-CCP) method
'''           Removal of script to calculate PR2Temp. Replaced with Sub-routine this reduces code size
'''           Removal of complexity by removal of FnLSL in calculateDuty method
'''           Added support for HPWMOff ( channel, PWMHardware ). Turns of 10bit PWM (non-CCP) channel
'''           Improved documentation
''' 06/6/2017 Corrected missing Dim of variable in CCP/PWM
''' 03/7/2017 Added HPWM3 and HPWM4 constants
''' 15/7/2017 Moved the default constants to prevent entry into ASM
''' 17/8/2017 Revised to protect CCP1M0-3 being set when no CCP module exists.
''' 08/9/2017 Revised to futher isolate CCP1M0-3 being set when no CCP module exists and added PWM1 and PWM2 support -
'''           PWM1 and PWM2 support is disabled by default
''' 28/9/2017 Revised to support CCP/PWM for CCP/PWM 1..5
'''           Also, supports CCP_PWM_1..5_On and CCP_PWM_1..5_Off to save even more memory.
''' 2/10/2017 Revised HPWM CCP method to support chips with T2CKPS2 as these are a different truth table.
'''           Optional DisableCCPFixedModePWM will disable the Fixed Mode CCP/PWM
'''           Added Fixed Mode PWM for 8bit and 10bit PWM
''' 30/10/2017 Revised to show overflow issue
''' 1/11/2017 Revised SetofFixedModePWM to test a VAR not a BIT see section SetPWMDutyCode - EndofFixedPWMModeCode
''' 4/11/2017 Revised to isolate AVR PWM_Duty setting in Script and rename PMW to PWM
''' 6/11/2017 Restore the cache value in HPWMUpdate
''' 7/11/2017 Added _v9081Patch=1 and _v9081Patch=2 for fix
''' 28/1/2018 AVR canskip and TCCR0B handler added for Mega8 style chips.
''' 15/8/2018 Added HPWM_CCPTimerN to support timer selection for CCP timer selection
''' 15/8/2018 Added protection from earlier parts within HPWM for CCP/PWM and test
''' 16/8/2018 Revised PWMon to better support support timer selection for CCP timer selection
''' 22/8/2018 Revised to include support for CCP with mutlple 10 bit CPP with timers 2 and 4
''' 25/8/2018 Revised to resolve clock source of CCPTimerN, this impacted the CCP/PWM method, so, added Select-Case
''' 25/8/2018 Revised to add clock source for PWM method. Datasheet was incorrect!!! In PWM Harware module.
''' 24/11/2018 Added PWM8 for K42
''' 13/11/2018 Added channels 1,2 and 3 to HPWUpdate
''' 14/12/2018 Added PWMxOE enable bits to support parts that need it. 16f1503 etc.
''' 02/05/2019 Added HPWMUpdate for CCP1..CCP5. Two sections one for the hardware PWM module(s) a nd one for the CCP module(s)
''' 14/08/2019 Added labels only. No functional changes.
''' 07/05/2020 Revised to isolate Timer4 and Timer6 in HPWM()
''' 21/06/2020 Revised to support ChipFamily 121
''' 30/12/2020 Revised sub HPWM (In PWMChannel, In PWMFreq, PWMDuty )/SetupCCPPWMRegisters section to identify the Q41 chip that only has ONE CCP1PMW! That is new... one CCP1. Just like the old days
''' 25/01/2020 Correct typo in ChipSubFamily Q41 in HMPWM
''' 10/01/2022 Revised sub HPWM (In PWMChannel, In PWMFreq, PWMDuty )/SetupCCPPWMRegisters section to identify the Q40 chip that only has ONE CCP1PMW! That is new... one CCP1. Just like the old days
''' 14/08/2022 Updated user changeable constants only - no functional change
''' 09/02/2023 Updated to isolate CCP1/PWM setup when CCP1/PWM does not exist
'''            Update to remove ASM comments but retaining the comments in this library
''' 19/03/2023 Updated to isolate CCP1/PWM setup when CCP1/PWM does not exist using #opion required not a script
''' 05/04/2024 Revised to add CHIPTIMERXCLOCKSOURCESVARIANT to support chip that FOSC/4 clock source in T2CLKCON, T4CLKCON or T6CLKCON equate to 0b0000000 - the standard is 0b00000001
''' 05/12/2023 Added 9bit and 10Bit CCP1 support, and, HPWM_CCPSetDuty() to support 9bit and 10Bit operations.
''' 06/12/2023 Added 9bit and 10Bit CCP1 support, and, HPWM_CCP_MAXDUTYVALUE() to support 9bit and 10Bit operations.
''' 23/09/2024 Added AVRDx PWM Fixed Mode support for TCA0/Channel 1
''' 11/12/2024 Added CCP1CON_EN support for Fixed Mode operations for 18FxxQ24



'user changeable constants


'end user changeable constants


#OPTION REQUIRED PIC CCP1CON "This chip does not support CCP1/PWM - use `#DEFINE DISABLECCPFIXEDMODEPWM` to disable CCP1/PWM"


  'Optionalal DisableCCPFixedModePWM will disable the Fixed Mode CCP/PWM
  '#define DisableCCPFixedModePWM

    #define HPWM1 1
    #define HPWM2 2
    #define HPWM3 3
    #define HPWM4 4
    #define HPWM5 5
    #define HPWM6 6
    #define HPWM7 7
    #define HPWM8 8

    #SAMEBIT C1TSEL0, CCP1TSEL0
    #SAMEBIT C1TSEL1, CCP1TSEL1
    #SAMEBIT C2TSEL0, CCP2TSEL0
    #SAMEBIT C2TSEL1, CCP2TSEL1
    #SAMEBIT C3TSEL0, CCP3TSEL0
    #SAMEBIT C3TSEL1, CCP3TSEL1
    #SAMEBIT C4TSEL0, CCP4TSEL0
    #SAMEBIT C4TSEL1, CCP4TSEL1


  #script

    IF NODEF(PWM_FREQ) THEN
      PWM_FREQ=38
    END IF
    IF NODEF(PWM_DUTY) THEN
      PWM_DUTY=50
    END IF
    IF NODEF(PWMMODULE) THEN
      PWMMODULE=TRUE
    END IF
    IF NODEF(USE_HPWMCCP1) THEN
      USE_HPWMCCP1=TRUE
    END IF
    IF NODEF(USE_HPWMCCP2) THEN
      USE_HPWMCCP2=TRUE
    END IF
    IF NODEF(USE_HPWMCCP3) THEN
      USE_HPWMCCP3=TRUE
    END IF
    IF NODEF(USE_HPWMCCP4) THEN
      USE_HPWMCCP4=TRUE
    END IF
    IF NODEF(USE_HPWMCCP5) THEN
      USE_HPWMCCP5=TRUE
    END IF
    IF NODEF(USE_HPWM1) THEN
      USE_HPWM1=TRUE
    END IF
    IF NODEF(USE_HPWM2) THEN
      USE_HPWM2=TRUE
    END IF
    IF NODEF(USE_HPWM3) THEN
      USE_HPWM3=TRUE
    END IF
    IF NODEF(USE_HPWM4) THEN
      USE_HPWM4=TRUE
    END IF
    IF NODEF(USE_HPWM5) THEN
      USE_HPWM5=TRUE
    END IF
    IF NODEF(USE_HPWM6) THEN
      USE_HPWM6=TRUE
    END IF
    IF NODEF(USE_HPWM7) THEN
      USE_HPWM7=TRUE
    END IF
    IF NODEF(USE_HPWM8) THEN
      USE_HPWM8=TRUE
    END IF
    IF NODEF(USE_HPWM_TIMER2) THEN
      USE_HPWM_TIMER2=TRUE
    END IF
    IF NODEF(USE_HPWM_TIMER4) THEN
      USE_HPWM_TIMER4=TRUE
    END IF
    IF NODEF(USE_HPWM_TIMER6) THEN
      USE_HPWM_TIMER6=TRUE
    END IF
    IF NODEF(CHIPAVRDX) THEN
      IF NODEF(AVRTC0) THEN
        AVRTC0=TRUE
      END IF
      IF NODEF(AVRCHAN2) THEN
        AVRCHAN2=TRUE
      END IF
    END IF

    If AVR Then
      'redirects to AVR code
      HPWM = AVRHPWM
      PWMOn = AVRPWMOn
      PWMOff = AVRPWMOff
    End If

    _v9081Patch=1

  #endscript


#startup InitPWM

  #IF DEF(CHIPAVRDX)
    Dim TCA0_SINGLE_COMPARE0 as Word Alias TCA0_SINGLE_CMP0H, TCA0_SINGLE_CMP0L
    Dim TCA0_SINGLE_COMPARE1 as Word Alias TCA0_SINGLE_CMP1H, TCA0_SINGLE_CMP1L
    Dim TCA0_SINGLE_COMPARE2 as Word Alias TCA0_SINGLE_CMP2H, TCA0_SINGLE_CMP2L
  #ENDIF


Sub InitPWM



  #ifdef PIC

      _PWMTimerSelected = 2

      Dim PRx_Temp as LONG

      '~Script to calculate constants required for given Frequency and Duty Cycle
      #script

      '~revised to support new chips like 16f18326*, 16f18555 and related microcontrollers"
       if bit(CCP1MODE0) then

            if nobit(CCP1M0) Then
                If def(DEVELOPER_PWM_DEBUG) Then
                  warning "Supporting microcontrollers with CCPxMODEx/CCP1MODE0 and nobit(CCP1M0)"
                End If              
		  	  CCP1M0 = CCP1MODE0
              CCP1M1 = CCP1MODE1
              CCP1M2 = CCP1MODE2
              CCP1M3 = CCP1MODE3

              CCP2M0 = CCP2MODE0
              CCP2M1 = CCP2MODE1
              CCP2M2 = CCP2MODE2
              CCP2M3 = CCP2MODE3

              CCP3M0 = CCP3MODE0
              CCP3M1 = CCP3MODE1
              CCP3M2 = CCP3MODE2
              CCP3M3 = CCP3MODE3

              CCP4M0 = CCP4MODE0
              CCP4M1 = CCP4MODE1
              CCP4M2 = CCP4MODE2
              CCP4M3 = CCP4MODE3

              CCP5M0 = CCP5MODE0
              CCP5M1 = CCP5MODE1
              CCP5M2 = CCP5MODE2
              CCP5M3 = CCP5MODE3

            end if

        end if

        'remapped for consistency
        if bit(CCP4EN) then
              If def(DEVELOPER_PWM_DEBUG) Then
                warning "Supporting microcontrollers with CCP4EN, CCPxEN"
              End If            
            CCP2CON_EN = CCP2EN
            CCP3CON_EN = CCP3EN
            CCP4CON_EN = CCP4EN
            CCP5CON_EN = CCP5EN
        end if

    #endscript

      #script
        Legacy_FixedCCPPWMModeDutyHandler:

        'The now supports all 5 CCP channels all use timer2, so, this is more about not using the Method to reduce memory

        'T2PR was used in these routines, but, that is now a register, so, Changed to TxPR
          PR2_CPP_PWM_Temp = int((1/PWM_Freq)/(4*(1/(ChipMHz*1000))))
        'Global constant for users to use
          PWMCCP1MAXDUTYVALUE = PR2_CPP_PWM_Temp

          PR2_CPP_PWM_Temp_10BIT_POTENTIALLY = int((1/PWM_Freq)/((1/(ChipMHz*1000))))
          If PR2_CPP_PWM_Temp_10BIT_POTENTIALLY > 255 and PR2_CPP_PWM_Temp_10BIT_POTENTIALLY < 1024 Then
            PWMCCP1MAXDUTYVALUE = PR2_CPP_PWM_Temp_10BIT_POTENTIALLY
          End If

          TxPR = 1
          if PR2_CPP_PWM_Temp > 255 then
              PR2_CPP_PWM_Temp = int((1/PWM_Freq)/(16*(1/(ChipMHz*1000))))
              TxPR = 4
              if PR2_CPP_PWM_Temp > 255 then
                PR2_CPP_PWM_Temp = int((1/PWM_Freq)/(64*(1/(ChipMHz*1000))))
                TxPR = 16
                PWMCCP1MAXDUTYVALUE = INT(PWMCCP1MAXDUTYVALUE / 4)
              if PR2_CPP_PWM_Temp > 255 then
                'error TxPR, PR2_CPP_PWM_Temp
                error msg(BadPWMFreq)
              end if
            end if
          end if

        'Global constant for users to use
        PRxPeriodRegister = PR2_CPP_PWM_Temp
        TIMERXPRESCALESELECTVALUE = TxPR

         IF DEF(DEVELOPER_PWM_DEBUG)  or DEF(SHOWPWMCCPCALCULATIONRESULTS) THEN
          If PWMCCP1MAXDUTYVALUE < 256 Then
            warning "8Bit PWMCCP"
            //~ Shift value to left to aling to CCPR1L<8>:DC1B1<1>:DC1B0<1> bits
            PWMCCP1MAXDUTYVALUE = INT(PWMCCP1MAXDUTYVALUE*4)
          END IF
          If PWMCCP1MAXDUTYVALUE > 255 Then
            warning "9Bit or 10Bit PWMCCP"
          END IF  
          warning "PWMCCP1MAXDUTYVALUE: " PWMCCP1MAXDUTYVALUE
          warning "PRxPeriodRegister  : " PRxPeriodRegister
          warning "PreScaler is       : " TIMERXPRESCALESELECTVALUE
        END IF

        If PWMCCP1MAXDUTYVALUE > 1023 Then
          Error "Duty Cycles exceeds 10bit value. Change frequency and or duty cycle"
        End If
        'Code from Early days
        'DutyCycle = (PWM_Duty*10.24)*PR2_CPP_PWM_Temp/1024
        DutyCycle = (PWM_Duty / 25) * (PR2_CPP_PWM_Temp + 1)
        DutyCycleH = int((DutyCycle AND 1020)/4)
        DutyCycleL = int(DutyCycle AND 3)
        'End of Code from Early days

      If def(DEVELOPER_PWM_DEBUG) THEN
        Warning "---------------------------------"
        Warning "DutyCycleH         : " DutyCycleH
        Warning "DutyCycleL         : " DutyCycleL
      End if
        PWMOsc1 = int(60000/(240/ChipMHz))    'This is used in the calculations
        PWMOsc4 = int(60000/(960/ChipMHz))    '*** Unused constant ***
        PWMOsc16 = int(60000/(3840/ChipMHz))  '*** Unused constant ***
        ' End of CCP/PWM script - see end of section for code

      #endscript

    Legacy_StartofFixedCCPPWMModeCode:


      #ifndef DisableCCPFixedModePWM

          '~You can disable all the legacy CCPPWM fixed mode code to reduce program size

          '~This section is Library code, so it generates ASM
          '~This section uses the constants defined the script above.
          '~Essentially, sets CCPCONCache with the bits set correctly.
          '~And, timer 2.  Remember timer 2 can be the timer for CCP/PWM but the other timers can be specified for certain parts.



          '~If CCP1CON does not exist then there is NO CCP1 so no point in setting, as all this is to set up the CCP1 using constants method
          #ifdef Var(CCP1CON)

                DIM CCPCONCache as BYTE
                CCPCONCache = 0

                'Set PWM Period
                PR2 = PR2_CPP_PWM_Temp
                #ifdef TxPR 1
                  #ifndef bit(T2CKPS2)
                    SET T2CON.T2CKPS0 OFF
                    SET T2CON.T2CKPS1 OFF
                  #endif
                  #ifdef bit(T2CKPS2)
                    SET T2CON.T2CKPS0 OFF
                    SET T2CON.T2CKPS1 OFF
                    SET T2CON.T2CKPS2 OFF
                  #endif
                #endif
                #ifdef TxPR 4
                  #ifndef bit(T2CKPS2)
                    SET T2CON.T2CKPS0 ON
                    SET T2CON.T2CKPS1 OFF
                  #endif
                  #ifdef bit(T2CKPS2)
                    SET T2CON.T2CKPS0 OFF
                    SET T2CON.T2CKPS1 ON
                    SET T2CON.T2CKPS2 OFF
                  #endif
                #endif
                #ifdef TxPR 16
                  #ifndef bit(T2CKPS2)
                    SET T2CON.T2CKPS0 OFF
                    SET T2CON.T2CKPS1 ON
                  #endif
                  #ifdef bit(T2CKPS2)
                    SET T2CON.T2CKPS0 OFF
                    SET T2CON.T2CKPS1 OFF
                    SET T2CON.T2CKPS2 ON
                  #endif
                #endif

                'Set Duty cycle
                #ifndef Bit(CCP1FMT)
                  
                  #if PWMCCP1MAXDUTYVALUE < 256
                    'This is the legacy code to support only one CCPPWM channel 8bit
                    CCPR1L =  DutyCycleH
                  #else
                      'This is the legacy code to support only one CCPPWM channel 9bit or 10bit
                      DC1B1  = DutyCycleL.7
                      DC1B0  = DutyCycleL.6
                      CCPR1L = DutyCycleH
                  #endif

                #endif

                #IFNDEF Oneof(PWM_Timer2_Freq,PWM_Timer2_Freq,PWM_Timer2_Freq)

                    #ifdef Bit(CCP1FMT)
                          CCPCONCache.CCP1FMT = 1
                          CCPR1H = DutyCycleH
                          CCPR1L = DutyCycleL*64
                          [canskip]T2CLKCON = 1
                          // set the enable bit in the Fixed Mode cache variable
                          #ifdef Bit(CCP1CON_EN)
                            CCPCONCache.CCP1CON_EN = 1
                          #else
                          [canskip]CCPCONCache.en = 1
                    #endif
                    #endif



                    #ifdef DutyCycleL 0
                        #ifdef Bit(CCP1X)               '***  NOt Defined
                          SET CCPCONCache.CCP1Y OFF
                          SET CCPCONCache.CCP1X OFF
                        #endif
                        #ifdef Bit(DC1B1)              '*** DEFINED
                          SET CCPCONCache.DC1B1 OFF
                          SET CCPCONCache.DC1B0 OFF
                        #endif

                    #endif

                    #ifdef DutyCycleL 1
                        #ifdef Bit(CCP1X)
                          SET CCPCONCache.CCP1Y ON
                          SET CCPCONCache.CCP1X OFF
                        #endif
                        #ifdef Bit(DC1B1)
                          SET CCPCONCache.DC1B1 OFF
                          SET CCPCONCache.DC1B0 ON
                        #endif

                    #endif

                    #ifdef DutyCycleL 2
                        #ifdef Bit(CCP1X)
                          SET CCPCONCache.CCP1Y OFF
                          SET CCPCONCache.CCP1X ON
                        #endif

                        #ifdef Bit(DC1B1)
                          SET CCPCONCache.DC1B1 ON
                          SET CCPCONCache.DC1B0 OFF
                        #endif

                    #endif

                    #ifdef DutyCycleL 3
                        #ifdef Bit(CCP1X)
                          SET CCPCONCache.CCP1Y ON
                          SET CCPCONCache.CCP1X ON
                        #endif
                        #ifdef Bit(DC1B1)
                          SET CCPCONCache.DC1B1 ON
                          SET CCPCONCache.DC1B0 ON
                        #endif

                    #endif

                    //~ legacy code, replaced by canskip
                        //~         Finish preparing CCP*CON
                        //~         SET CCPCONCache.CCP1M3 ON
                        //~         SET CCPCONCache.CCP1M2 ON
                        //~         SET CCPCONCache.CCP1M1 OFF
                        //~         SET CCPCONCache.CCP1M0 OFF
                     CCPCONCache.CCP1M3, CCPCONCache.CCP1M2, CCPCONCache.CCP1M1, CCPCONCache.CCP1M0 = b'1100'

                    'Enable Timer 2
                    SET T2CON.TMR2ON ON

                #ENDIF

          #endif

          #ifdef HPWM_FAST
            PWMFreqOld = 0
            PWMTimerOld = 0
          #endif
      #endif



    #script
      Rev2018FixedPWMModeHandler:

       'Fixed mode of Non CCP/PWM calculate constants required for given Frequency and Duty Cycle
       'Process:
       '        Go thru each timer 2, 4 and 6 and calculate the Duty cycle registers.
       '        This has to be done 27 times for the the PWM channels 9 to 1, as this a constants we cannot use paramters.
       '        Each section is identical apart for the timer and the PWM channel.
       '        This therefore sets the Duty registers and based upon the correct timer source as appropiate.

       'Handle Timer 2
       if PWM_Timer2_Freq Then

          If def(DEVELOPER_PWM_DEBUG) THEN
            Warning "This specific chip is using PWM_TimerX_Freq script section of PWM.h"
          End If          
          if  PWM_9_Clock_Source then

              Script_PWM9CON = 0b01000000 + ( HPWMxCon_Default * 128) 'Enable PWM Module, Module Output

              if PWM_9_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_9_Duty / 25) * (PR2Temp + 1)
                  Script_PWM9_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM9_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM9_DutyCycleH = int(Script_PWM9_DutyCycleH * 1 )
                  Script_PWM9_DutyCycleL = int(Script_PWM9_DutyCycleL * 1 )

              end if

          end if

          if  PWM_8_Clock_Source then

              Script_PWM8CON = 0b01000000 + ( HPWMxCon_Default * 128) 'Enable PWM Module, Module Output

              if PWM_8_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_8_Duty / 25) * (PR2Temp + 1)
                  Script_PWM8_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM8_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM8_DutyCycleH = int(Script_PWM8_DutyCycleH * 1 )
                  Script_PWM8_DutyCycleL = int(Script_PWM8_DutyCycleL * 1 )

              end if

          end if

          if PWM_7_Clock_Source then

              Script_PWM7CON = 0b01000000 + ( HPWMxCon_Default * 128) 'Enable PWM Module, Module Output

              if PWM_7_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_7_Duty / 25) * (PR2Temp + 1)
                  Script_PWM7_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM7_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM7_DutyCycleH = int(Script_PWM7_DutyCycleH * 1 )
                  Script_PWM7_DutyCycleL = int(Script_PWM7_DutyCycleL * 1 )

              end if

          end if

          if  PWM_6_Clock_Source then

              Script_PWM6CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_6_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_6_Duty / 25) * (PR2Temp + 1)
                  Script_PWM6_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM6_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM6_DutyCycleH = int(Script_PWM6_DutyCycleH * 1 )
                  Script_PWM6_DutyCycleL = int(Script_PWM6_DutyCycleL * 1 )

              end if

          end if

          if  PWM_5_Clock_Source then

              Script_PWM5CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_5_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_5_Duty / 25) * (PR2Temp + 1)
                  Script_PWM5_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM5_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM5_DutyCycleH = int(Script_PWM5_DutyCycleH * 1 )
                  Script_PWM5_DutyCycleL = int(Script_PWM5_DutyCycleL * 1 )

              end if

          end if

          if  PWM_4_Clock_Source then

              Script_PWM4CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_4_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_4_Duty / 25) * (PR2Temp + 1)
                  Script_PWM4_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM4_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM4_DutyCycleH = int(Script_PWM4_DutyCycleH * 1 )
                  Script_PWM4_DutyCycleL = int(Script_PWM4_DutyCycleL * 1 )

              end if

          end if

          if  PWM_3_Clock_Source then

              Script_PWM3CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_3_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_3_Duty / 25) * (PR2Temp + 1)
                  Script_PWM3_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM3_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM3_DutyCycleH = int(Script_PWM3_DutyCycleH * 1 )
                  Script_PWM3_DutyCycleL = int(Script_PWM3_DutyCycleL * 1 )

              end if

          end if

          if  PWM_2_Clock_Source then

              Script_PWM2CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_2_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_2_Duty / 25) * (PR2Temp + 1)
                  Script_PWM2_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM2_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM2_DutyCycleH = int(Script_PWM2_DutyCycleH * 1 )
                  Script_PWM2_DutyCycleL = int(Script_PWM2_DutyCycleL * 1 )

              end if

          end if

          if  PWM_1_Clock_Source then

              Script_PWM1CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_1_Clock_Source = 2 then
                  'Duty Section
                  PR2Temp = int((1/PWM_Timer2_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR2Temp > 255 then
                      PR2Temp = int((1/PWM_Timer2_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR2Temp > 255 then
                        PR2Temp = int((1/PWM_Timer2_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR2Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_1_Duty / 25) * (PR2Temp + 1)
                  Script_PWM1_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM1_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM1_DutyCycleH = int(Script_PWM1_DutyCycleH * 1 )
                  Script_PWM1_DutyCycleL = int(Script_PWM1_DutyCycleL * 1 )

              end if

          end if

          'This section, repeated for each timer source, sets constants for the prescaler and ,if required, other bits.

          'Timer section for Timer2
          PWMOsc1_Calc = 60000/(240/ChipMHz)    'This is used in the calculations

          Script_Tx_PR = 1
          Script_PR2_Temp_Calc = PWMOsc1_Calc / PWM_Timer2_Freq

          IF Int(Script_PR2_Temp_Calc / 256 ) > 0 then
            Script_Tx_PR = 4
            'Divide by 4
            Script_PR2_Temp_Calc = Script_PR2_Temp_Calc / 4

          end if

          IF Int(Script_PR2_Temp_Calc / 256 ) > 0 then
            Script_Tx_PR = 16
            'Divide by 4
            Script_PR2_Temp_Calc = Script_PR2_Temp_Calc / 4
          end if

          Script_PR2_Temp_Calc = Int ( Script_PR2_Temp_Calc * 1 )

          if bit(T2CKPS2) then

             'Set Prescaler bits1
              if Script_Tx_PR = 1 Then
                Script_OR_T2CON = b'0000000'
              end if
              if Script_Tx_PR = 4 Then
                Script_OR_T2CON = b'0100000'
              end if
              if Script_Tx_PR = 16 Then
                Script_OR_T2CON = b'1000000'
              end if
              if Script_Tx_PR = 64 Then
                Script_OR_T2CON = b'1100000'
              end if

          end if

          if nobit(T2CKPS2) then

              'Set Prescaler bits2
              if Script_Tx_PR = 1 Then
                Script_OR_T2CON = b'0000000'
              end if
              if Script_Tx_PR = 4 Then
                Script_OR_T2CON = b'0010000'
              end if
              if Script_Tx_PR = 16 Then
                Script_OR_T2CON = b'0100000'
              end if
              if Script_Tx_PR = 64 Then
                Script_OR_T2CON = b'0110000'
              end if
          end if

          if var(T2CLKCON) Then
              Script_T2CLKCON = 1  '0b00000001
          end if

        end if


      'See timer 2 for documentation
       if PWM_Timer4_Freq Then

          if  PWM_9_Clock_Source then

              Script_PWM9CON = 0b01000000 + ( HPWMxCon_Default * 128) 'Enable PWM Module, Module Output

              if PWM_9_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_9_Duty / 25) * (PR4Temp + 1)
                  Script_PWM9_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM9_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM9_DutyCycleH = int(Script_PWM9_DutyCycleH * 1 )
                  Script_PWM9_DutyCycleL = int(Script_PWM9_DutyCycleL * 1 )

              end if

          end if

          if  PWM_8_Clock_Source then

              Script_PWM8CON = 0b01000000 + ( HPWMxCon_Default * 128) 'Enable PWM Module, Module Output

              if PWM_8_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_8_Duty / 25) * (PR4Temp + 1)
                  Script_PWM8_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM8_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM8_DutyCycleH = int(Script_PWM8_DutyCycleH * 1 )
                  Script_PWM8_DutyCycleL = int(Script_PWM8_DutyCycleL * 1 )

              end if

          end if

          if  PWM_7_Clock_Source then

              Script_PWM7CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_7_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_7_Duty / 25) * (PR4Temp + 1)
                  Script_PWM7_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM7_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM7_DutyCycleH = int(Script_PWM7_DutyCycleH * 1 )
                  Script_PWM7_DutyCycleL = int(Script_PWM7_DutyCycleL * 1 )

              end if

          end if

          if  PWM_6_Clock_Source then

              Script_PWM6CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_6_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_6_Duty / 25) * (PR4Temp + 1)
                  Script_PWM6_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM6_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM6_DutyCycleH = int(Script_PWM6_DutyCycleH * 1 )
                  Script_PWM6_DutyCycleL = int(Script_PWM6_DutyCycleL * 1 )

              end if

          end if

          if  PWM_5_Clock_Source then

              Script_PWM5CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_5_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_5_Duty / 25) * (PR4Temp + 1)
                  Script_PWM5_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM5_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM5_DutyCycleH = int(Script_PWM5_DutyCycleH * 1 )
                  Script_PWM5_DutyCycleL = int(Script_PWM5_DutyCycleL * 1 )

              end if

          end if

          if  PWM_4_Clock_Source then

              Script_PWM4CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_4_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_4_Duty / 25) * (PR4Temp + 1)
                  Script_PWM4_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM4_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM4_DutyCycleH = int(Script_PWM4_DutyCycleH * 1 )
                  Script_PWM4_DutyCycleL = int(Script_PWM4_DutyCycleL * 1 )

              end if

          end if

          if  PWM_3_Clock_Source then

              Script_PWM3CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_3_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_3_Duty / 25) * (PR4Temp + 1)
                  Script_PWM3_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM3_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM3_DutyCycleH = int(Script_PWM3_DutyCycleH * 1 )
                  Script_PWM3_DutyCycleL = int(Script_PWM3_DutyCycleL * 1 )

              end if

          end if

          if  PWM_2_Clock_Source then

              Script_PWM2CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_2_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_2_Duty / 25) * (PR4Temp + 1)
                  Script_PWM2_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM2_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM2_DutyCycleH = int(Script_PWM2_DutyCycleH * 1 )
                  Script_PWM2_DutyCycleL = int(Script_PWM2_DutyCycleL * 1 )

              end if

          end if

          if  PWM_1_Clock_Source then

              Script_PWM1CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_1_Clock_Source = 4 then
                  'Duty Section
                  PR4Temp = int((1/PWM_Timer4_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR4Temp > 255 then
                      PR4Temp = int((1/PWM_Timer4_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR4Temp > 255 then
                        PR4Temp = int((1/PWM_Timer4_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR4Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_1_Duty / 25) * (PR4Temp + 1)
                  Script_PWM1_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM1_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM1_DutyCycleH = int(Script_PWM1_DutyCycleH * 1 )
                  Script_PWM1_DutyCycleL = int(Script_PWM1_DutyCycleL * 1 )

              end if

          end if

          'See timer 2 for documentation
          'Timer section for Timer4
          PWMOsc1_Calc = 60000/(240/ChipMHz)    'This is used in the calculations

          Script_Tx_PR = 1
          Script_PR4_Temp_Calc = PWMOsc1_Calc / PWM_Timer4_Freq

          IF Int(Script_PR4_Temp_Calc / 256 ) > 0 then
            Script_Tx_PR = 4
            'Divide by 4
            Script_PR4_Temp_Calc = Script_PR4_Temp_Calc / 4

          end if

          IF Int(Script_PR4_Temp_Calc / 256 ) > 0 then
            Script_Tx_PR = 16
            'Divide by 4
            Script_PR4_Temp_Calc = Script_PR4_Temp_Calc / 4
          end if

          Script_PR4_Temp_Calc = Int ( Script_PR4_Temp_Calc * 1 )

          if bit(T4CKPS2) then

             'Set Prescaler bits T4CON
              if Script_Tx_PR = 1 Then
                Script_OR_T4CON = b'0000000'
              end if
              if Script_Tx_PR = 4 Then
                Script_OR_T4CON = b'0100000'
              end if
              if Script_Tx_PR = 16 Then
                Script_OR_T4CON = b'1000000'
              end if
              if Script_Tx_PR = 64 Then
                Script_OR_T4CON = b'1100000'
              end if

          end if

          if nobit(T4CKPS2) then

              'Set Prescaler bits T4CON
              if Script_Tx_PR = 1 Then
                Script_OR_T4CON = b'0000000'
              end if
              if Script_Tx_PR = 4 Then
                Script_OR_T4CON = b'0010000'
              end if
              if Script_Tx_PR = 16 Then
                Script_OR_T4CON = b'0100000'
              end if
              if Script_Tx_PR = 64 Then
                Script_OR_T4CON = b'0110000'
              end if
          end if

          if var(T4CLKCON) Then
              Script_T4CLKCON = 1  '0b00000001
          end if

       end if

       'See timer 2 for documentation
       if PWM_Timer6_Freq Then

          if  PWM_9_Clock_Source then

              Script_PWM9CON = 0b01000000 + ( HPWMxCon_Default * 128) 'Enable PWM Module, Module Output

              if PWM_9_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_9_Duty / 25) * (PR6Temp + 1)
                  Script_PWM9_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM9_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM9_DutyCycleH = int(Script_PWM9_DutyCycleH * 1 )
                  Script_PWM9_DutyCycleL = int(Script_PWM9_DutyCycleL * 1 )

              end if

          end if

          if  PWM_8_Clock_Source then

              Script_PWM8CON = 0b01000000 + ( HPWMxCon_Default * 128) 'Enable PWM Module, Module Output

              if PWM_8_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_8_Duty / 25) * (PR6Temp + 1)
                  Script_PWM8_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM8_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM8_DutyCycleH = int(Script_PWM8_DutyCycleH * 1 )
                  Script_PWM8_DutyCycleL = int(Script_PWM8_DutyCycleL * 1 )

              end if

          end if

          if  PWM_7_Clock_Source then

              Script_PWM7CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_7_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_7_Duty / 25) * (PR6Temp + 1)
                  Script_PWM7_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM7_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM7_DutyCycleH = int(Script_PWM7_DutyCycleH * 1 )
                  Script_PWM7_DutyCycleL = int(Script_PWM7_DutyCycleL * 1 )

              end if

          end if

          if  PWM_6_Clock_Source then

              Script_PWM6CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_6_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_6_Duty / 25) * (PR6Temp + 1)
                  Script_PWM6_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM6_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM6_DutyCycleH = int(Script_PWM6_DutyCycleH * 1 )
                  Script_PWM6_DutyCycleL = int(Script_PWM6_DutyCycleL * 1 )

              end if

          end if

          if  PWM_5_Clock_Source then

              Script_PWM5CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_5_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_5_Duty / 25) * (PR6Temp + 1)
                  Script_PWM5_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM5_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM5_DutyCycleH = int(Script_PWM5_DutyCycleH * 1 )
                  Script_PWM5_DutyCycleL = int(Script_PWM5_DutyCycleL * 1 )

              end if

          end if

          if  PWM_4_Clock_Source then

              Script_PWM4CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_4_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_4_Duty / 25) * (PR6Temp + 1)
                  Script_PWM4_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM4_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM4_DutyCycleH = int(Script_PWM4_DutyCycleH * 1 )
                  Script_PWM4_DutyCycleL = int(Script_PWM4_DutyCycleL * 1 )

              end if

          end if

          if  PWM_3_Clock_Source then

              Script_PWM3CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_3_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_3_Duty / 25) * (PR6Temp + 1)
                  Script_PWM3_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM3_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM3_DutyCycleH = int(Script_PWM3_DutyCycleH * 1 )
                  Script_PWM3_DutyCycleL = int(Script_PWM3_DutyCycleL * 1 )

              end if

          end if

          if  PWM_2_Clock_Source then

              Script_PWM2CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_2_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_2_Duty / 25) * (PR6Temp + 1)
                  Script_PWM2_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM2_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM2_DutyCycleH = int(Script_PWM2_DutyCycleH * 1 )
                  Script_PWM2_DutyCycleL = int(Script_PWM2_DutyCycleL * 1 )

              end if

          end if

          if  PWM_1_Clock_Source then

              Script_PWM1CON = 0b01000000 + ( HPWMxCon_Default * 128)   'Enable PWM Module, Module Output

              if PWM_1_Clock_Source = 6 then
                  'Duty Section
                  PR6Temp = int((1/PWM_Timer6_Freq)/(4*(1/(ChipMHz*1000))))
                  TxPR = 1
                  if PR6Temp > 255 then
                      PR6Temp = int((1/PWM_Timer6_Freq)/(16*(1/(ChipMHz*1000))))
                      TxPR = 4
                      if PR6Temp > 255 then
                        PR6Temp = int((1/PWM_Timer6_Freq)/(64*(1/(ChipMHz*1000))))
                        TxPR = 16
                      if PR6Temp > 255 then
                        error msg(BadPWMFreq)
                      end if
                    end if
                  end if

                  DutyCycle = (PWM_1_Duty / 25) * (PR6Temp + 1)
                  Script_PWM1_DutyCycleH = int(DutyCycle AND 1020)/4
                  Script_PWM1_DutyCycleL = int((DutyCycle AND 3) * 64)
                  'just for display purposes
                  Script_PWM1_DutyCycleH = int(Script_PWM1_DutyCycleH * 1 )
                  Script_PWM1_DutyCycleL = int(Script_PWM1_DutyCycleL * 1 )

              end if

          end if

          'See timer 2 for documentation
          'Timer section for Timer6
          PWMOsc1_Calc = 60000/(240/ChipMHz)    'This is used in the calculations

          Script_Tx_PR = 1
          Script_PR6_Temp_Calc = PWMOsc1_Calc / PWM_Timer6_Freq

          IF Int(Script_PR6_Temp_Calc / 256 ) > 0 then
            Script_Tx_PR = 4
            'Divide by 4
            Script_PR6_Temp_Calc = Script_PR6_Temp_Calc / 4

          end if

          IF Int(Script_PR6_Temp_Calc / 256 ) > 0 then
            Script_Tx_PR = 16
            'Divide by 4
            Script_PR6_Temp_Calc = Script_PR6_Temp_Calc / 4
          end if

          Script_PR6_Temp_Calc = Int ( Script_PR6_Temp_Calc * 1 )

          if bit(T6CKPS2) then

             'Set Prescaler bits T6CON
              if Script_Tx_PR = 1 Then
                Script_OR_T6CON = b'0000000'
              end if
              if Script_Tx_PR = 4 Then
                Script_OR_T6CON = b'0100000'
              end if
              if Script_Tx_PR = 16 Then
                Script_OR_T6CON = b'1000000'
              end if
              if Script_Tx_PR = 64 Then
                Script_OR_T6CON = b'1100000'
              end if

          end if

          if nobit(T6CKPS2) then

              'Set Prescaler bits T6CON
              if Script_Tx_PR = 1 Then
                Script_OR_T6CON = b'0000000'
              end if
              if Script_Tx_PR = 4 Then
                Script_OR_T6CON = b'0010000'
              end if
              if Script_Tx_PR = 16 Then
                Script_OR_T6CON = b'0100000'
              end if
              if Script_Tx_PR = 64 Then
                Script_OR_T6CON = b'0110000'
              end if
          end if

          if var(T6CLKCON) Then
              Script_T6CLKCON = 1  '0b00000001
          end if

       end if
       EndFixedPWMModeHandler:
    #endscript
    '~This is the end of script section, now we use the constants created to updated registers.

    StartofFixedPWMModeCode:

      '~Set registers using the constants from script
      '~This is repeated for timer 2, 4 and 6 - and the two timer variants and the 9 PWM channels
      '~This uses the user defined constants to set the appropiate registers.
      #IFDEF PWM_Timer2_Freq
          T2CON = (T2CON and 143) or Script_OR_T2CON    ' Set Timer x PreScaler
          #if var(T2CLKCON)
              T2CLKCON = Script_T2CLKCON 'Select Timer source as FOSC/4
          #endif
          TMR2IF = 0
          TMR2ON = 1
          PR2 =  Script_PR2_Temp_Calc

          'Set Clock Sources
          #ifndef ChipPWMTimerVariant

              #if PWM_9_Clock_Source = 2
                  [canskip]P9TSEL1,P9TSEL0=b'01'   'PWM9 Timer 2 source
              #endif

              #if PWM_8_Clock_Source = 2
                  [canskip]P8TSEL1,P8TSEL0=b'01'   'PWM8 Timer 2 source
              #endif

              #if PWM_7_Clock_Source = 2
                  [canskip]P7TSEL1,P7TSEL0=b'01'   'PWM7 Timer 2 source
              #endif

              #if PWM_6_Clock_Source = 2
                  [canskip]P6TSEL1,P6TSEL0=b'01'   'PWM6 Timer 2 source
              #endif

              #if PWM_5_Clock_Source = 2
                  [canskip]P5TSEL1,P5TSEL0=b'01'   'PWM5 Timer 2 source
              #endif

              #if PWM_4_Clock_Source = 2
                  [canskip]P4TSEL1,P4TSEL0=b'01'   'PWM4 Timer 2 source
                  [canskip]C4TSEL1,C4TSEL0=b'00'
              #endif

              #if PWM_3_Clock_Source = 2
                  [canskip]P3TSEL1,P3TSEL0=b'01'   'PWM3 Timer 2 source
                  [canskip]C3TSEL1,C3TSEL0=b'00'
              #endif

              #if PWM_2_Clock_Source = 2
                  [canskip]P2TSEL1,P2TSEL0=b'01'   'PWM2 Timer 2 source
                  [canskip]C2TSEL1,C2TSEL0=b'00'
              #endif

              #if PWM_1_Clock_Source = 2
                  [canskip]P1TSEL1,P1TSEL0=b'01'   'PWM1 Timer 2 source
                  [canskip]C1TSEL1,C1TSEL0=b'00'
              #endif
          #endif

          'Set Clock Sources
          #if ChipPWMTimerVariant = 1

              #if PWM_9_Clock_Source = 2
                  [canskip]P9TSEL1,P9TSEL0=b'00'   'PWM9 Timer 2 source
              #endif

              #if PWM_8_Clock_Source = 2
                  [canskip]P8TSEL1,P8TSEL0=b'00'   'PWM8 Timer 2 source
              #endif

              #if PWM_7_Clock_Source = 2
                  [canskip]P7TSEL1,P7TSEL0=b'00'   'PWM7 Timer 2 source
              #endif

              #if PWM_6_Clock_Source = 2
                  [canskip]P6TSEL1,P6TSEL0=b'00'   'PWM6 Timer 2 source
              #endif

              #if PWM_5_Clock_Source = 2
                  [canskip]P5TSEL1,P5TSEL0=b'00'   'PWM5 Timer 2 source
              #endif

              #if PWM_4_Clock_Source = 2
                  [canskip]P4TSEL1,P4TSEL0=b'00'   'PWM4 Timer 2 source
              #endif

              #if PWM_3_Clock_Source = 2
                  [canskip]P3TSEL1,P3TSEL0=b'00'   'PWM3 Timer 2 source
              #endif

              #if PWM_2_Clock_Source = 2
                  [canskip]P2TSEL1,P2TSEL0=b'00'   'PWM2 Timer 2 source
              #endif

              #if PWM_1_Clock_Source = 2
                  [canskip]P1TSEL1,P1TSEL0=b'00'   'PWM1 Timer 2 source
              #endif
          #endif

         'Set Clock Sources
          #if ChipPWMTimerVariant = 2

              #if PWM_9_Clock_Source = 2
                  [canskip]C9TSEL1,C9TSEL0=b'01'   'PWM9 Timer 2 source
              #endif

              #if PWM_8_Clock_Source = 2
                  [canskip]C8TSEL1,C8TSEL0=b'01'   'PWM8 Timer 2 source
              #endif

              #if PWM_7_Clock_Source = 2
                  [canskip]C7TSEL1,C7TSEL0=b'01'   'PWM7 Timer 2 source
              #endif

              #if PWM_6_Clock_Source = 2
                  [canskip]C6TSEL1,C6TSEL0=b'01'   'PWM6 Timer 2 source
              #endif

              #if PWM_5_Clock_Source = 2
                  [canskip]C5TSEL1,C5TSEL0=b'01'   'PWM5 Timer 2 source
              #endif

              #if PWM_4_Clock_Source = 2
                  [canskip]C4TSEL1,C4TSEL0=b'01'   'PWM4 Timer 2 source
              #endif

              #if PWM_3_Clock_Source = 2
                  [canskip]C3TSEL1,C3TSEL0=b'01'   'PWM3 Timer 2 source
              #endif

              #if PWM_2_Clock_Source = 2
                  [canskip]C2TSEL1,C2TSEL0=b'01'   'PWM2 Timer 2 source
              #endif

              #if PWM_1_Clock_Source = 2
                  [canskip]C1TSEL1,C1TSEL0=b'01'   'PWM1 Timer 2 source
              #endif
          #endif

        #endif

      #IFDEF PWM_Timer4_Freq
          T4CON = (T4CON and 143) or Script_OR_T4CON    ' Set Timer x PreScaler
          #if var(T4CLKCON)
              T4CLKCON = Script_T4CLKCON 'Select Timer source as FOSC/4
          #endif
          TMR4IF = 0
          TMR4ON = 1
          PR4 =  Script_PR4_Temp_Calc

          'Set Clock Sources
          #ifndef ChipPWMTimerVariant

              #if PWM_9_Clock_Source = 4
                  [canskip]P9TSEL1,P9TSEL0=b'10'   'PWM9 Timer 4 source
              #endif

              #if PWM_8_Clock_Source = 4
                  [canskip]P8TSEL1,P8TSEL0=b'10'   'PWM8 Timer 4 source
              #endif

              #if PWM_7_Clock_Source = 4
                  [canskip]P7TSEL1,P7TSEL0=b'10'   'PWM7 Timer 4 source
              #endif

              #if PWM_6_Clock_Source = 4
                  [canskip]P6TSEL1,P6TSEL0=b'10'   'PWM6 Timer 4 source
              #endif

              #if PWM_5_Clock_Source = 4
                  [canskip]P5TSEL1,P5TSEL0=b'10'   'PWM5 Timer 4 source
              #endif

              #if PWM_4_Clock_Source = 4
                  [canskip]P4TSEL1,P4TSEL0=b'10'   'PWM4 Timer 4 source
                  [canskip]C4TSEL1,C4TSEL0=b'01'
              #endif

              #if PWM_3_Clock_Source = 4
                  [canskip]P3TSEL1,P3TSEL0=b'10'   'PWM3 Timer 4 source
                  [canskip]C3TSEL1,C3TSEL0=b'01'
              #endif

              #if PWM_2_Clock_Source = 4
                  [canskip]P2TSEL1,P2TSEL0=b'10'   'PWM2 Timer 4 source
                  [canskip]C2TSEL1,C2TSEL0=b'01'
              #endif

              #if PWM_1_Clock_Source = 4
                  [canskip]P1TSEL1,P1TSEL0=b'10'   'PWM1 Timer 4 source
                  [canskip]C1TSEL1,C1TSEL0=b'01'
              #endif
          #endif

          #if ChipPWMTimerVariant = 1

              #if PWM_9_Clock_Source = 4
                  [canskip]P9TSEL1,P9TSEL0=b'01'   'PWM9 Timer 4 source
              #endif

              #if PWM_8_Clock_Source = 4
                  [canskip]P8TSEL1,P8TSEL0=b'01'   'PWM8 Timer 4 source
              #endif

              #if PWM_7_Clock_Source = 4
                  [canskip]P7TSEL1,P7TSEL0=b'01'   'PWM7 Timer 4 source
              #endif

              #if PWM_6_Clock_Source = 4
                  [canskip]P6TSEL1,P6TSEL0=b'01'   'PWM6 Timer 4 source
              #endif

              #if PWM_5_Clock_Source = 4
                  [canskip]P5TSEL1,P5TSEL0=b'01'   'PWM5 Timer 4 source
              #endif

              #if PWM_4_Clock_Source = 4
                  [canskip]P4TSEL1,P4TSEL0=b'01'   'PWM4 Timer 4 source
              #endif

              #if PWM_3_Clock_Source = 4
                  [canskip]P3TSEL1,P3TSEL0=b'01'   'PWM3 Timer 4 source
              #endif

              #if PWM_2_Clock_Source = 4
                  [canskip]P2TSEL1,P2TSEL0=b'01'   'PWM2 Timer 4 source
              #endif

              #if PWM_1_Clock_Source = 4
                  [canskip]P1TSEL1,P1TSEL0=b'01'   'PWM1 Timer 4 source
              #endif
          #endif

         'Set Clock Sources
          #if ChipPWMTimerVariant = 2

              #if PWM_9_Clock_Source = 4
                  [canskip]C9TSEL1,C9TSEL0=b'10'   'PWM9 Timer 4 source
                  [canskip]P9TSEL1,P9TSEL0=b'10'   'PWM9 Timer 4 source
              #endif

              #if PWM_8_Clock_Source = 4
                  [canskip]C8TSEL1,C8TSEL0=b'10'   'PWM8 Timer 4 source
                  [canskip]P8TSEL1,P8TSEL0=b'10'   'PWM8 Timer 4 source
              #endif

              #if PWM_7_Clock_Source = 4
                  [canskip]C7TSEL1,C7TSEL0=b'10'   'PWM7 Timer 4 source
                  [canskip]P7TSEL1,P7TSEL0=b'10'   'PWM7 Timer 4 source
              #endif

              #if PWM_6_Clock_Source = 4
                  [canskip]C6TSEL1,C6TSEL0=b'10'   'PWM6 Timer 4 source
                  [canskip]P6TSEL1,P6TSEL0=b'10'   'PWM4 Timer 4 source
              #endif

              #if PWM_5_Clock_Source = 4
                  [canskip]C5TSEL1,C5TSEL0=b'10'   'PWM5 Timer 4 source
                  [canskip]P5TSEL1,P5TSEL0=b'10'   'PWM5 Timer 4 source
              #endif

              #if PWM_4_Clock_Source = 4
                  [canskip]C4TSEL1,C4TSEL0=b'10'   'PWM4 Timer 4 source
                  [canskip]P4TSEL1,P4TSEL0=b'10'   'PWM4 Timer 4 source
              #endif

              #if PWM_3_Clock_Source = 4
                  [canskip]C3TSEL1,C3TSEL0=b'10'   'PWM3 Timer 4 source
                  [canskip]P3TSEL1,P3TSEL0=b'10'   'PWM3 Timer 4 source
              #endif

              #if PWM_2_Clock_Source = 4
                  [canskip]C2TSEL1,C2TSEL0=b'10'   'PWM2 Timer 4 source
                  [canskip]P2TSEL1,P2TSEL0=b'10'   'PWM2 Timer 4 source
              #endif

              #if PWM_1_Clock_Source = 4
                  [canskip]C1TSEL1,C1TSEL0=b'10'   'PWM1 Timer 4 source
                  [canskip]P1TSEL1,P1TSEL0=b'10'   'PWM1 Timer 4 source
              #endif
          #endif

      #ENDIF

      #IFDEF PWM_Timer6_Freq
          T6CON = (T6CON and 143) or Script_OR_T6CON    ' Set Timer x PreScaler
          #if var(T6CLKCON)
              T6CLKCON = Script_T6CLKCON 'Select Timer source as FOSC/4
          #ENDIF
          TMR6IF = 0
          TMR6ON = 1
          PR6 =  Script_PR6_Temp_Calc

          #ifndef ChipPWMTimerVariant

              #if PWM_9_Clock_Source = 6
                  [canskip]P9TSEL1,P9TSEL0=b'11'   'PWM9 Timer 6 source
              #endif

              #if PWM_8_Clock_Source = 6
                  [canskip]P8TSEL1,P8TSEL0=b'11'   'PWM8 Timer 6 source
              #endif

              #if PWM_7_Clock_Source = 6
                  [canskip]P7TSEL1,P7TSEL0=b'11'   'PWM7 Timer 6 source
              #endif

              #if PWM_6_Clock_Source = 6
                  [canskip]P6TSEL1,P6TSEL0=b'11'   'PWM6 Timer 6 source
              #endif

              #if PWM_5_Clock_Source = 6
                  [canskip]P5TSEL1,P5TSEL0=b'11'   'PWM5 Timer 6 source
              #endif

              #if PWM_4_Clock_Source = 6
                  [canskip]P4TSEL1,P4TSEL0=b'11'   'PWM4 Timer 6 source
                  [canskip]C4TSEL1,C4TSEL0=b'10'
              #endif

              #if PWM_3_Clock_Source = 6
                  [canskip]P3TSEL1,P3TSEL0=b'11'   'PWM3 Timer 6 source
                  [canskip]C3TSEL1,C3TSEL0=b'10'
              #endif

              #if PWM_2_Clock_Source = 6
                  [canskip]P2TSEL1,P2TSEL0=b'11'   'PWM2 Timer 6 source
                  [canskip]C2TSEL1,C2TSEL0=b'10'
              #endif

              #if PWM_1_Clock_Source = 6
                  [canskip]P1TSEL1,P1TSEL0=b'11'   'PWM1 Timer 6 source
                  [canskip]C1TSEL1,C1TSEL0=b'10'
              #endif

          #endif

          #if ChipPWMTimerVariant = 1

              #if PWM_9_Clock_Source = 6
                  [canskip]P9TSEL1,P9TSEL0=b'10'   'PWM9 Timer 6 source
              #endif

              #if PWM_8_Clock_Source = 6
                  [canskip]P8TSEL1,P8TSEL0=b'10'   'PWM8 Timer 6 source
              #endif

              #if PWM_7_Clock_Source = 6
                  [canskip]P7TSEL1,P7TSEL0=b'10'   'PWM7 Timer 6 source
              #endif

              #if PWM_6_Clock_Source = 6
                  [canskip]P6TSEL1,P6TSEL0=b'10'   'PWM6 Timer 6 source
              #endif

              #if PWM_5_Clock_Source = 6
                  [canskip]P5TSEL1,P5TSEL0=b'10'   'PWM5 Timer 6 source
              #endif

              #if PWM_4_Clock_Source = 6
                  [canskip]P4TSEL1,P4TSEL0=b'10'   'PWM4 Timer 6 source
              #endif

              #if PWM_3_Clock_Source = 6
                  [canskip]P3TSEL1,P3TSEL0=b'10'   'PWM3 Timer 6 source
              #endif

              #if PWM_2_Clock_Source = 6
                  [canskip]P2TSEL1,P2TSEL0=b'10'   'PWM2 Timer 6 source
              #endif

              #if PWM_1_Clock_Source = 6
                  [canskip]P1TSEL1,P1TSEL0=b'10'   'PWM1 Timer 6 source
              #endif
          #endif

         'Set Clock Sources
          #if ChipPWMTimerVariant = 2

             #if PWM_9_Clock_Source = 6
                  [canskip]C9TSEL1,C9TSEL0=b'11'   'PWM9 Timer 6 source
                  [canskip]P9TSEL1,P9TSEL0=b'11'   'PWM9 Timer 6 source
              #endif

              #if PWM_8_Clock_Source = 6
                  [canskip]C8TSEL1,C8TSEL0=b'11'   'PWM8 Timer 6 source
                  [canskip]P8TSEL1,P8TSEL0=b'11'   'PWM8 Timer 6 source
              #endif

              #if PWM_7_Clock_Source = 6
                  [canskip]C7TSEL1,C7TSEL0=b'11'   'PWM7 Timer 6 source
                  [canskip]P7TSEL1,P7TSEL0=b'11'   'PWM7 Timer 6 source
              #endif

              #if PWM_6_Clock_Source = 6
                  [canskip]C6TSEL1,C6TSEL0=b'11'   'PWM6 Timer 6 source
                  [canskip]P6TSEL1,P6TSEL0=b'11'   'PWM6 Timer 6 source
              #endif

              #if PWM_5_Clock_Source = 6
                  [canskip]C5TSEL1,C5TSEL0=b'11'   'PWM5 Timer 6 source
                  [canskip]P5TSEL1,P5TSEL0=b'11'   'PWM5 Timer 6 source
              #endif

              #if PWM_4_Clock_Source = 6
                  [canskip]C4TSEL1,C4TSEL0=b'11'   'PWM4 Timer 6 source
                  [canskip]P4TSEL1,P4TSEL0=b'11'   'PWM4 Timer 6 source
              #endif

              #if PWM_3_Clock_Source = 6
                  [canskip]C3TSEL1,C3TSEL0=b'11'   'PWM3 Timer 6 source
                  [canskip]P3TSEL1,P3TSEL0=b'11'   'PWM3 Timer 6 source
              #endif

              #if PWM_2_Clock_Source = 6
                  [canskip]C2TSEL1,C2TSEL0=b'11'   'PWM2 Timer 6 source
                  [canskip]P2TSEL1,P2TSEL0=b'11'   'PWM2 Timer 6 source
              #endif

              #if PWM_1_Clock_Source = 6
                  [canskip]C1TSEL1,C1TSEL0=b'11'   'PWM1 Timer 6 source
                  [canskip]P1TSEL1,P1TSEL0=b'11'   'PWM1 Timer 6 source
              #endif

          #endif

      #ENDIF

    SetPWMDutyCode:
      '~This section finally, sets the Duty using the constants from the script.
      '~This uses the user defined constants to set the appropiate registers.
      #IFDEF PWM_1_Duty
        #IFDEF VAR(PWM1CON)   'Means this is a PWM Channel chip
          PWM1CON =  Script_PWM1CON
          #IFDEF VAR(PWM1DCH)
            PWM1DCH =  Script_PWM1_DutyCycleH
            PWM1DCL =  Script_PWM1_DutyCycleL
          #ENDIF

        #ENDIF

        #IFDEF VAR(CCPR1L)
          #ifndef BIT(CCP1FMT)
            'This is the legacy code to support only one CCPPWM channel
            CCPR1L =  Script_PWM1_DutyCycleH
          #endif
          #ifdef BIT(CCP1FMT)
            'This is the CCPPWM with 10 bit
            CCPR1L = Script_PWM1_DutyCycleL
            CCPR1H = Script_PWM1_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_2_Duty

        #IFDEF VAR(PWM2CON)   'Means this is a PWM Channel chip
          PWM2CON =  Script_PWM2CON
          #IFDEF VAR(PWM2DCH)
            PWM2DCH =  Script_PWM2_DutyCycleH
            PWM2DCL =  Script_PWM2_DutyCycleL
          #ENDIF
        #ENDIF

        #IFDEF VAR(CCPR2L)
          #ifndef BIT(CCP2FMT)
            CCPR2L =  Script_PWM2_DutyCycleH
          #endif
          #ifdef BIT(CCP2FMT)
            CCPR2L = Script_PWM2_DutyCycleL
            CCPR2H = Script_PWM2_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_3_Duty
        #IFDEF VAR(PWM3CON)   'Means this is a PWM Channel chip
          PWM3CON =  Script_PWM3CON
          #IFDEF VAR(PWM3DCH)
            PWM3DCH =  Script_PWM3_DutyCycleH
            PWM3DCL =  Script_PWM3_DutyCycleL
          #ENDIF
        #ENDIF

        #IFDEF VAR(CCPR3L)
          #ifndef BIT(CCP3FMT)
            CCPR3L =  Script_PWM3_DutyCycleH
          #endif
          #ifdef BIT(CCP3FMT)
            CCPR3L = Script_PWM3_DutyCycleL
            CCPR3H = Script_PWM3_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_4_Duty
        #IFDEF VAR(PWM4CON)   'Means this is a PWM Channel chip
          PWM4CON =  Script_PWM4CON
          #IFDEF VAR(PWM4DCH)
            PWM4DCH =  Script_PWM4_DutyCycleH
            PWM4DCL =  Script_PWM4_DutyCycleL
          #ENDIF
        #ENDIF

        #IFDEF VAR(CCPR4L)
          #ifndef BIT(CCP4FMT)
            CCPR4L =  Script_PWM4_DutyCycleH
          #endif
          #ifdef BIT(CCP4FMT)
            CCPR4L = Script_PWM4_DutyCycleL
            CCPR4H = Script_PWM4_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_5_Duty
        #IFDEF VAR(PWM5CON)   'Means this is a PWM Channel chip
          PWM5CON =  Script_PWM5CON
          #IFDEF VAR(PWM5DCH)
            PWM5DCH =  Script_PWM5_DutyCycleH
            PWM5DCL =  Script_PWM5_DutyCycleL
          #ENDIF

        #ENDIF

        #IFDEF VAR(CCPR5L)
          #ifndef BIT(CCP5FMT)
            CCPR5L =  Script_PWM5_DutyCycleH
          #endif
          #ifdef BIT(CCP5FMT)
            CCPR5L = Script_PWM5_DutyCycleL
            CCPR5H = Script_PWM5_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_6_Duty
        #IFDEF VAR(PWM6CON)   'Means this is a PWM Channel chip
          PWM6CON =  Script_PWM6CON
          #IFDEF VAR(PWM6DCH)
            PWM6DCH =  Script_PWM6_DutyCycleH
            PWM6DCL =  Script_PWM6_DutyCycleL
          #ENDIF
        #ENDIF

        #IFDEF VAR(CCPR6L)
          #ifndef BIT(CCP6FMT)
            CCPR6L =  Script_PWM6_DutyCycleH
          #endif
          #ifdef BIT(CCP6FMT)
            CCPR6L = Script_PWM6_DutyCycleL
            CCPR6H = Script_PWM6_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_7_Duty
        #IFDEF VAR(PWM7CON)   'Means this is a PWM Channel chip
          PWM7CON =  Script_PWM7CON
          #IFDEF VAR(PWM7DCH)
            PWM7DCH =  Script_PWM7_DutyCycleH
            PWM7DCL =  Script_PWM7_DutyCycleL
          #ENDIF

        #ENDIF

        #IFDEF VAR(CCPR7L)
          #ifndef BIT(CCP7FMT)
            CCPR7L =  Script_PWM7_DutyCycleH
          #endif
          #ifdef BIT(CCP7FMT)
            CCPR7L = Script_PWM7_DutyCycleL
            CCPR7H = Script_PWM7_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_8_Duty
        #IFDEF VAR(PWM8CON)   'Means this is a PWM Channel chip
          PWM8CON =  Script_PWM8CON
          #IFDEF VAR(PWM8DCH)
            PWM8DCH =  Script_PWM8_DutyCycleH
            PWM8DCL =  Script_PWM8_DutyCycleL
          #ENDIF

        #ENDIF

        #IFDEF VAR(CCPR8L)
          #ifndef BIT(CCP8FMT)
            CCPR8L =  Script_PWM8_DutyCycleH
          #endif
          #ifdef BIT(CCP8FMT)
            CCPR8L = Script_PWM8_DutyCycleL
            CCPR8H = Script_PWM8_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

      #IFDEF PWM_9_Duty
        #IFDEF VAR(PWM9CON)   'Means this is a PWM Channel chip
          PWM9CON =  Script_PWM9CON
          #IFDEF VAR(PWM9DCH)
            PWM9DCH =  Script_PWM9_DutyCycleH
            PWM9DCL =  Script_PWM9_DutyCycleL
          #ENDIF

        #ENDIF

        #IFDEF VAR(CCPR9L)
          #ifndef BIT(CCP9FMT)
            CCPR9L =  Script_PWM9_DutyCycleH
          #endif
          #ifdef BIT(CCP9FMT)
            CCPR9L = Script_PWM9_DutyCycleL
            CCPR9H = Script_PWM9_DutyCycleH
          #endif
        #ENDIF

      #ENDIF

  '~This is the end of the fixed PWM Mode handler
  #endif

  #script

  if AVR then
    
    // Support fixed mode PWM

    if nodef(CHIPAVRDX) then
      ICR1temp = int(((chipMHZ*1000) / PWM_Freq) - 1)
      SCRIPT_PWMPRESCALER = 1
      If ICR1temp > 255 Then
        ICR1temp = int(ICR1temp / 8)
        SCRIPT_PWMPRESCALER = 8

        If ICR1temp > 255 Then
          ICR1temp = int(ICR1temp / 8)
          SCRIPT_PWMPRESCALER = 64
        End If

          If ICR1temp > 255 Then
            error msg(BadHPWMFreq)
          End If
      End If
      PWM_Duty = int((PWM_Duty * 255)/100)
      PWMDutyTempAVR = int((PWM_Duty * ICR1temp) / 255)
    end if

    if def(CHIPAVRDX) then
      ICR1temp = int(((chipMHZ*1000000) / PWM_Freq) - 1)
      
      // Calculate the correct prescaler
      SCRIPT_PWMPRESCALER = TCA_SINGLE_CLKSEL_DIV1_GC
      If ICR1temp > 65535 Then
          SCRIPT_PWMPRESCALER = TCA_SINGLE_CLKSEL_DIV2_GC
          ICR1temp = int(ICR1temp / 2)

          If ICR1temp > 65535 Then
          SCRIPT_PWMPRESCALER = TCA_SINGLE_CLKSEL_DIV4_GC
          ICR1temp = int(ICR1temp / 2)
          End If

          If ICR1temp > 65535 Then
          SCRIPT_PWMPRESCALER = TCA_SINGLE_CLKSEL_DIV8_GC
          ICR1temp = int(ICR1temp / 2)
          End If

          If ICR1temp > 65535 Then
          SCRIPT_PWMPRESCALER = TCA_SINGLE_CLKSEL_DIV16_GC
          ICR1temp = int(ICR1temp / 2)
          End If

          If ICR1temp > 65535 Then
          SCRIPT_PWMPRESCALER =TCA_SINGLE_CLKSEL_DIV64_GC
          ICR1temp = int(ICR1temp / 2)
          End If

          If ICR1temp > 65535 Then
          SCRIPT_PWMPRESCALER = TCA_SINGLE_CLKSEL_DIV256_GC
          ICR1temp = int(ICR1temp / 2)
          End If

          If ICR1temp > 65535 Then
          SCRIPT_PWMPRESCALER = TCA_SINGLE_CLKSEL_DIV1024_GC
          ICR1temp = int(ICR1temp / 2)
          End If

          If ICR1temp > 65535 Then
              error msg(BadPWMFreq)
          End If

      end If

      SCRIPT_PWM_PERIOD = ICR1temp
      SCRIPT_PWM_DUTY = int(SCRIPT_PWM_PERIOD / ( 100 / PWM_Duty) )

    end if


  end if
  _v9081Patch=2
  #endscript

  #IFDEF AVR
    //~ Set all defined channel modes, and default channel 2 mode
    //~ Mode must be set first
    AVRSetHPWMMode
    PWMPrescale = SCRIPT_PWMPRESCALER

    #IF NODEF(CHIPAVRDX)
      'Set prescale and duty cycle on timer0 OCR0B (channel 2)
      'when using PWMON/PWMOFF
      #IFDEF AVRCHAN2
        AVRTimer = 0
        AVRSetPrescale (AVRTimer)
        'OCR2A is used as the period register
        #IFNDEF VAR(OCR0AL)
          OCR0A = ICR1temp
          OCR0B = PWMDutyTempAVR
        #ENDIF

        #IFDEF VAR(OCR0AL)
          OCR0AL = ICR1temp
          OCR0BL = PWMDutyTempAVR
        #ENDIF
      #ENDIF
    #ENDIF
    AVRPWMFreqOld = 0

  #ENDIF


End Sub


'Legacy methd - Enables the CCP1 capability - the setup was completed in the Init script
'This is an overloaded method
'PWMOn - no parameters enables Fixed Mode PWM CCPPWM 1 ONLY
'PWMOn ( PWMChannel )  with ONE parameter enables Fixed Mode PWM CCPPWM1-CCPPWM5 using the same Freq and Duty from the script engage for CCP
'PWMOn (IN Channel, IN PWMHardware) - with TWO parameters  enables the Fixed Mode PWM module using the same specfic Freq and Duty and clock for each PWM channel. These is NOT the CCPPWM.
'
'PWMOff  - no parameters disables Fixed Mode PWM CCPPWM 1 ONLY
'PWMOff ( In PWMChannel ) with ONE parameter disables Fixed Mode PWM CCPPWM1-CCPPWM5
'PWMOff (IN Channel, IN PWMHardware)  - with TWO parameters disables the Fixed Mode PWM module channel. Not the CCP!! see previous method for CCP

#Define HPWMOn PWMOn
sub PWMOn
  CCP1CON = CCPCONCache
end sub

'Enables the CCPn capability - the using the setup completed in the Init script
'Updated to support 10bit CCP/PWM
sub PWMOn ( In PWMChannel )
  'Simply set the channel to the CCPCONCache

         #ifdef USE_HPWMCCP1 TRUE               'optimisation check
            #ifdef Var(CCP1CON)                 'check the CCP1CON exits
             if PWMChannel =1 then              'is it this channel?

                #ifdef PWM_1_Clock_Source      'if the clock source is specified... just a method to isolate the code
                  #IFDEF VAR(CCPR1L)           'check the register exists

                    #ifndef bit(CCP1FMT)
                      CCP1CON = CCP1CON OR 0x0C  'Set PWM mode
                      CCPR1L = Script_PWM1_DutyCycleH
                    #endif
                    #ifdef bit(CCP1FMT)
                      CCP1CON = CCP1CON OR 0xBF  'Set PWM mode
                      CCPR1H = Script_PWM1_DutyCycleH
                      CCPR1L = Script_PWM1_DutyCycleL
                    #endif

                  #ENDIF
                  'get out quickly
                  Exit Sub
                #endif

                #ifndef PWM_1_Clock_Source    'not fixed PWM model, so, this is a legacy call
                    CCP1CON = CCPCONCache
                    #ifndef bit(CCP1FMT)
                      CCPR1L = DutyCycleH
                    #endif
                    #ifdef bit(CCP1FMT)
                      CCPR1H = DutyCycleH
                      CCPR1L = DutyCycleL * 64
                    #endif
                #endif

              end if
            #endif
         #endif

         #ifdef USE_HPWMCCP2 TRUE
            #ifdef Var(CCP2CON)
             if PWMChannel =2 then

                #ifdef PWM_2_Clock_Source
                  #IFDEF VAR(CCPR2L)
                    #ifndef bit(CCP2FMT)
                      CCP2CON = CCP2CON OR 0x0C  'Set PWM mode
                      CCPR2L = Script_PWM2_DutyCycleH
                    #endif
                    #ifdef bit(CCP2FMT)
                      CCP2CON = CCP2CON OR 0xBF  'Set PWM mode
                      CCPR2H = Script_PWM2_DutyCycleH
                      CCPR2L = Script_PWM2_DutyCycleL
                    #endif
                  #ENDIF
                  Exit Sub
                #endif

                #ifndef PWM_2_Clock_Source
                    CCP2CON = CCPCONCache
                    #ifndef bit(CCP2FMT)
                      CCPR2L = DutyCycleH
                    #endif
                    #ifdef bit(CCP2FMT)
                      CCPR2H = DutyCycleH
                      CCPR2L = DutyCycleL * 64
                    #endif
                #endif

              end if
            #endif
         #endif

         #ifdef USE_HPWMCCP3 TRUE
            #ifdef Var(CCP3CON)
             if PWMChannel =3 then

                #ifdef PWM_3_Clock_Source
                  #IFDEF VAR(CCPR3L)
                    #ifndef bit(CCP3FMT)
                      CCP3CON = CCP3CON OR 0x0C  'Set PWM mode
                      CCPR3L = Script_PWM3_DutyCycleH
                    #endif
                    #ifdef bit(CCP3FMT)
                      CCP3CON = CCP3CON OR 0xBF  'Set PWM mode
                      CCPR3H = Script_PWM3_DutyCycleH
                      CCPR3L = Script_PWM3_DutyCycleL
                    #endif
                  #ENDIF
                  Exit Sub
                #endif

                #ifndef PWM_3_Clock_Source
                    CCP3CON = CCPCONCache
                    #ifndef bit(CCP3FMT)
                      CCPR3L = DutyCycleH
                    #endif
                    #ifdef bit(CCP3FMT)
                      CCPR3H = DutyCycleH
                      CCPR3L = DutyCycleL * 64
                    #endif
                #endif

              end if
            #endif
         #endif

         #ifdef USE_HPWMCCP4 TRUE
            #ifdef Var(CCP4CON)
             if PWMChannel =4 then

                #ifdef PWM_4_Clock_Source
                  #IFDEF VAR(CCPR4L)
                    #ifndef bit(CCP4FMT)
                      CCP4CON = CCP4CON OR 0x0C  'Set PWM mode
                      CCPR4L = Script_PWM4_DutyCycleH
                    #endif
                    #ifdef bit(CCP4FMT)
                      CCP4CON = CCP4CON OR 0xBF  'Set PWM mode
                      CCPR4H = Script_PWM4_DutyCycleH
                      CCPR4L = Script_PWM4_DutyCycleL
                    #endif
                  #ENDIF
                  Exit Sub
                #endif

                #ifndef PWM_4_Clock_Source
                    CCP4CON = CCPCONCache
                    #ifndef bit(CCP4FMT)
                      CCPR4L = DutyCycleH
                    #endif
                    #ifdef bit(CCP4FMT)
                      CCPR4H = DutyCycleH
                      CCPR4L = DutyCycleL * 64
                    #endif
                #endif

              end if
            #endif
         #endif

         #ifdef USE_HPWMCCP5 TRUE
            #ifdef Var(CCP5CON)
             if PWMChannel =5 then

                #ifdef PWM_5_Clock_Source
                  #IFDEF VAR(CCPR5L)
                    #ifndef bit(CCP5FMT)
                      CCP5CON = CCP5CON OR 0x0C  'Set PWM mode
                      CCPR5L = Script_PWM5_DutyCycleH
                    #endif
                    #ifdef bit(CCP5FMT)
                      CCP5CON = CCP5CON OR 0xBF  'Set PWM mode
                      CCPR5H = Script_PWM5_DutyCycleH
                      CCPR5L = Script_PWM5_DutyCycleL
                    #endif
                  #ENDIF
                  Exit Sub
                #endif

                #ifndef PWM_5_Clock_Source
                    CCP5CON = CCPCONCache
                    #ifndef bit(CCP5FMT)
                      CCPR5L = DutyCycleH
                    #endif
                    #ifdef bit(CCP5FMT)
                      CCPR5H = DutyCycleH
                      CCPR5L = DutyCycleL * 65
                    #endif
                #endif

              end if
            #endif
         #endif

end sub


'Enable the PWM module channel. Not the CCP/PWM!! See previous method for CCP/PWM
SUB PWMOn (IN PWMChannel, IN PWMHardware )

    'PWMHardware is used to purely identify this method as the hardware OFF not the CCP OFF

      Select Case PWMChannel
          #IFDEF BIT(PWM1EN) 'this simply stops error messages when the does not exit
            Case 1
              Set PWM1EN On

              'Enable PWM output enable bit on chips like the 10f322
              #IFDEF BIT(PWM1OE)
                PWM1OE = 1
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM2EN) 'this simply stops error messages when the does not exit
            Case 2
              Set PWM2EN On

              'Enable PWM output enable bit
              #IFDEF BIT(PWM2OE)
                PWM2OE = 1
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM3EN) 'this simply stops error messages when the does not exit
            Case 3
              Set PWM3EN On

              'Enable PWM output enable bit on chips
              #IFDEF BIT(PWM3OE)
                PWM3OE = 1
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM4EN) 'this simply stops error messages when the does not exit
            Case 4
              Set PWM4EN On

              'Enable PWM output enable bit on chips
              #IFDEF BIT(PWM4OE)
                PWM4OE = 1
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM5EN) 'this simply stops error messages when the does not exit
            Case 5
              Set PWM5EN On

              'Enable PWM output enable bit on chips
              #IFDEF BIT(PWM5OE)
                PWM5OE = 1
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM6EN) 'this simply stops error messages when the does not exit
            Case 6
              Set PWM6EN On

              'Enable PWM output enable bit on chips
              #IFDEF BIT(PWM6OE)
                PWM6OE = 1
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM7EN) 'this simply stops error messages when the does not exit
            Case 7
              Set PWM7EN On

              'Enable PWM output enable bit on chips
              #IFDEF BIT(PWM7OE)
                PWM7OE = 1
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM8EN) 'this simply stops error messages when the does not exit
            Case 8
              Set PWM8EN On

              'Enable PWM output enable bit on chips
              #IFDEF BIT(PWM8OE)
                PWM8OE = 1
              #ENDIF

          #ENDIF

          #IFDEF BIT(PWM9EN) 'this simply stops error messages when the does not exit
            Case 9
              Set PWM9EN On

              'Enable PWM output enable bit on chips
              #IFDEF BIT(PWM9OE)
                PWM9OE = 1
              #ENDIF

          #ENDIF
      End Select

END SUB


#Define HPWMOff PWMOff

'Disables the CCP1 capability
sub PWMOff
  CCP1CON = 0
end sub

sub PWMOff ( In PWMChannel )
  'Simply set the channel to ZERO
         #ifdef USE_HPWMCCP1 TRUE
            #ifdef Var(CCP1CON)
             if PWMChannel =1 then
                CCP1CON = 0
              end if

            #endif
         #endif
         #ifdef USE_HPWMCCP2 TRUE
            #ifdef Var(CCP2CON)
              if PWMChannel =2 then
                  CCP2CON = 0
              end if
            #endif
         #endif
         #ifdef USE_HPWMCCP3 TRUE
            #ifdef Var(CCP3CON)
                if PWMChannel =3 then
                    CCP3CON = 0
                end if
            #endif
         #endif
         #ifdef USE_HPWMCCP4 TRUE
            #ifdef Var(CCP4CON)
                if PWMChannel =4 then
                    CCP4CON = 0
                end if
            #endif
         #endif
         #ifdef USE_HPWMCCP5 TRUE
            #ifdef Var(CCP5CON)
                if PWMChannel =5 then
                    CCP5CON = 0
                end if
            #endif
         #endif
end sub

'Disable the CCP channel

'SUB PWMOff (IN Channel)   'Added 27.04.2015
'
'    If Channel > 5 OR Channel < 1 then channel = 1
'
'    #IF bit(CCP1EN)
'      Select Case Channel
'          Case 1
'              SET CCP1EN OFF
'          Case 2
'              SET CCP2CON_EN OFF
'          Case 3
'              SET CCP3CON_EN OFF
'          Case 4
'              SET CCP4CON_EN OFF
'          Case 5
'              SET CCP5CON_EN OFF
'      End Select
'      Exit Sub
'    #ENDIF
'
'    #IFNDEF VAR(CCP2CON)  'There is only 1 CCP Module
'     if channel = 1 then
'          CCP1CON = 0
'          exit sub
'     end if
'    #ENDIF
'
'    #IFDEF VAR(CCP2CON)   '2 or more CCP modules
'       IF Channel = 1 then CCP1CON = 0
'       IF Channel = 2 then CCP2CON = 0
'    #ENDIF
'
'    #IFDEF VAR(CCP3CON)
'       IF Channel = 3 then CCP3CON = 0
'    #ENDIF
'
'    #IFDEF VAR(CCP4CON)
'       IF Channel = 4 then CCP4CON = 0
'    #ENDIF
'
'    #IFDEF VAR(CCP5CON)
'       IF Channel = 5 then CCP5CON = 0
'    #ENDIF
'
'END SUB

'Disable the PWM module channel. Not the CCP!! see previous method for CCP
SUB PWMOff (IN Channel, IN PWMHardware)

    'PWMHardware is used to purely identify this method as the hardware OFF not the CCP OFF

      Select Case Channel
          #IFDEF BIT(PWM1EN) 'this simply stops error messages when the does not exit
            Case 1
              Set PWM1EN Off

              'Disable PWM output enable bit on chips like the 10f322
              #IFDEF BIT(PWM1OE)
                PWM1OE = 0
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM2EN) 'this simply stops error messages when the does not exit
            Case 2
              Set PWM2EN Off

              'Disable PWM output enable bit
              #IFDEF BIT(PWM2OE)
                PWM2OE = 0
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM3EN) 'this simply stops error messages when the does not exit
            Case 3
              Set PWM3EN Off

              'Disable PWM output enable bit
              #IFDEF BIT(PWM3OE)
                PWM3OE = 0
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM4EN) 'this simply stops error messages when the does not exit
            Case 4
              Set PWM4EN Off

              'Disable PWM output enable bit
              #IFDEF BIT(PWM4OE)
                PWM4OE = 0
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM5EN) 'this simply stops error messages when the does not exit
            Case 5
              Set PWM5EN Off

              'Disable PWM output enable bit
              #IFDEF BIT(PWM5OE)
                PWM5OE = 0
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM6EN) 'this simply stops error messages when the does not exit
            Case 6
              Set PWM6EN Off

              'Disable PWM output enable bit
              #IFDEF BIT(PWM6OE)
                PWM6OE = 0
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM7EN) 'this simply stops error messages when the does not exit
            Case 7
              Set PWM7EN Off

              'Disable PWM output enable bit
              #IFDEF BIT(PWM7OE)
                PWM7OE = 0
              #ENDIF

          #ENDIF
          #IFDEF BIT(PWM8EN) 'this simply stops error messages when the does not exit
            Case 8
              Set PWM8EN Off

              'Disable PWM output enable bit
              #IFDEF BIT(PWM8OE)
                PWM8OE = 0
              #ENDIF

          #ENDIF

      End Select

END SUB



sub HPWM_CCPTimerN (In PWMChannel, In PWMFreq, PWMDuty, Optional in _PWMTimerSelected as byte = 2 )

    dim PWMDuty as word

    'This will call the CCP/PWM method with the _PWMTimerSelected set.... clever
    Select Case _PWMTimerSelected
      #ifdef USE_HPWM_TIMER2 TRUE
      Case 2
            HPWM (PWMChannel, PWMFreq, PWMDuty )
            TMR2ON= 1
      #endif

      #ifdef USE_HPWM_TIMER4 TRUE
        #ifdef var(T4CON)
          case 4
            HPWM (PWMChannel, PWMFreq, PWMDuty )
            TMR4ON= 1
        #endif
      #endif

        #ifdef USE_HPWM_TIMER6 TRUE
          #ifdef var(T4CON)
            case 6
              HPWM (PWMChannel, PWMFreq, PWMDuty )
              TMR6ON= 1
          #endif
      #endif

    End Select
    'Set the default timer, else if HPWM is called for CCP/PWM the timersource will be incorrect
    _PWMTimerSelected = 2

end Sub

'Supports CCPx [1-5] the clock souce is always Timer2, however, if the part can support timerselection bits this will try to set, else, it will default to timer 2
'This is an 8bit resolution
#define HPWM_CCPTimer2 HPWM
sub HPWM (In PWMChannel, In PWMFreq, PWMDuty )  '8bit resolution on timer 2

  Dim PRx_Temp as LONG
  Dim PRx_Temp_Cache as Long
  dim PWMDuty as word
  dim PWMResolution as word: PWMResolution = 255

  'If HPWM_FAST operation selected, only recalculate timer prescaler when
  'needed. Gives faster operation, but uses extra byte of RAM and may cause
  'problems if HPWM and PWMOn are used together in a program.
  '(No issues using HPWM and PWMOff in the same program with HPWM_FAST.)
  #ifdef HPWM_FAST
      If PWMFreq <> PWMFreqOld Then
  #endif

          //~ Commence calculations of PMW parameters
          //~ This figures out Prescaler required.  We can set to 1, 4 or 16 to set Tx_PR
          //~ So, start with 1 - check the remainder. If the remainder in the high byte is greater then zero then do same with a prescaler value of 4
          //~ So, using 4 - check the remainder. If the remainder in the high byte is greater then zero then do same with a prescaler value of 16
          //~ So, using 16
          //~ This simply set Tx_PR to 1,4 or 16

          Tx_PR = 1
          /*
          wait 1 s
          HserPrintCRLF 2
          HserPrint "PWMFreq " + str(PWMFreq)
          HserPrintCRLF
          HserPrint "PWMOsc1 " + str(PWMOsc1)
            HserPrintCRLF
          */
          
          PRx_Temp = PWMOsc1 / PWMFreq
          
          /*
          HserPrint "1) PRx_Temp " + str(PRx_Temp)
          HserPrintCRLF          
          */

          IF PRx_Temp_H > 0 then
            Tx_PR = 4
            'Divide by 4
            set STATUS.C off
            rotate PRx_Temp right
            set STATUS.C off
            rotate PRx_Temp right
          end if

          IF PRx_Temp_H > 0 then
            Tx_PR = 16
            'Divide by 4
            set STATUS.C off
            rotate PRx_Temp right
            set STATUS.C off
            rotate PRx_Temp right
          end if

          IF PRx_Temp_H > 0 then
            Tx_PR = 64
            'Divide by 4
            set STATUS.C off
            rotate PRx_Temp right
            set STATUS.C off
            rotate PRx_Temp right
          end if
          /*
          HserPrint "2) PRx_Temp " + str(PRx_Temp)
          HserPrintCRLF
          HserPrint "Tx_PR " + str(Tx_PR)
          HserPrintCRLF
          */
          //~added to handle different timer sources
          //~added to support HPWM_CCPTimerN. Makes the code longer but more flexible
          //~user optimisation to reduce code.
 CCPPWMSetupClockSource:
          //~ Tx_PR is either 1, 4, 16 or 64.  This is controlled by the maths section.  
          //~ So, other values are NOT ever going to be selected.
          select case _PWMTimerSelected

             #ifdef USE_HPWM_TIMER2 TRUE
             case 2:
                PR2 = PRx_Temp
                'Set the Bits for the Prescaler
                'Setup Timer2 by clearing the Prescaler bits - it is set next....
                #ifdef bit(T2CKPS2)
                    SET T2CKPS0 OFF
                    SET T2CKPS1 OFF
                    SET T2CKPS2 OFF
                    'Set Prescaler bits T2CON@1
                    if Tx_PR = 4  then SET T2CKPS1 ON
                    if Tx_PR = 16 then SET T2CKPS2 ON
                    if Tx_PR = 64 then SET T2CKPS2 ON: SET T2CKPS1 ON
                #endif

                '~Revised to show overflow issue
                #ifndef bit(T2CKPS2)
                    SET T2CKPS0 OFF
                    SET T2CKPS1 OFF
                    'Set Prescaler bits T2CON@2
                    if Tx_PR = 4  then SET T2CKPS0 ON
                    if Tx_PR = 16 then SET T2CKPS1 ON
                    'Overflowed - this chip cannot handle the desired PWMFrequency. Lower clock speed.
                    'if T2CON and 3 = 3 then an overflow has occured!
                    if Tx_PR = 64 then SET T2CKPS0 ON: SET T2CKPS1 ON
                #endif

                'Set Clock Source, if required
                #ifdef var(T2CLKCON)
                  #ifndef  CHIPTIMERXCLOCKSOURCESVARIANT
                      'Set to FOSC/4 for backward compatibility@2a where CS<3:0> = 0001 = Fosc/4
                      T2CLKCON.T2CS0 = 1
                      T2CLKCON.T2CS1 = 0
                      T2CLKCON.T2CS2 = 0
                      T2CLKCON.T2CS3 = 0
                  #else
                    #if CHIPTIMERxCLOCKSOURCESVariant = 1
                      'Set to FOSC/4 for backward compatibility@2b where CS<3:0> = 0000 = Fosc/4
                      T2CLKCON.T2CS0 = 0
                      T2CLKCON.T2CS1 = 0
                      T2CLKCON.T2CS2 = 0
                      T2CLKCON.T2CS3 = 0
                    #endif
                  #endif
                #endif
              #endif

             #ifdef USE_HPWM_TIMER4 TRUE
                #ifdef var(T4CON)
                 case 4:
                     PR4 = PRx_Temp
                    'Set the Bits for the Prescaler
                    'Setup Timerx by clearing the Prescaler bits - it is set next....
                    #ifdef bit(T4CKPS4)
                        SET T4CKPS0 OFF
                        SET T4CKPS1 OFF
                        SET T4CKPS4 OFF
                        'Set Prescaler bits T4CON@1
                        if Tx_PR = 4  then SET T4CKPS1 ON
                        if Tx_PR = 16 then SET T4CKPS4 ON
                        if Tx_PR = 64 then SET T4CKPS4 ON: SET T4CKPS1 ON
                    #endif

                    'Revised to show overflow issue
                    #ifndef bit(T4CKPS4)
                      #ifdef bit(T4CKPS0)
                        SET T4CKPS0 OFF
                        SET T4CKPS1 OFF
                        'Set Prescaler bits T4CON@2
                        if Tx_PR = 4  then SET T4CKPS0 ON
                        if Tx_PR = 16 then SET T4CKPS1 ON
                        'Overflowed - this chip cannot handle the desired PWMFrequency. Lower clock speed.
                        'if T4CON and 3 = 3 then an overflow has occured!
                        if Tx_PR = 64 then SET T4CKPS0 ON: SET T4CKPS1 ON
                      #endif
                    #endif

                    'Set Clock Source, if required
                    #ifdef var(T4CLKCON)
                      #ifndef  CHIPTIMERXCLOCKSOURCESVariant
                          'Set to FOSC/4 for backward compatibility@4a where CS<3:0> = 0001 = Fosc/4
                          T4CLKCON.T4CS0 = 1
                          T4CLKCON.T4CS1 = 0
                          T4CLKCON.T4CS2 = 0
                          T4CLKCON.T4CS3 = 0
                      #else
                        #if CHIPTIMERxCLOCKSOURCESVariant = 1
                          'Set to FOSC/4 for backward compatibility@4b where CS<3:0> = 0000 = Fosc/4
                          T4CLKCON.T4CS0 = 0
                          T4CLKCON.T4CS1 = 0
                          T4CLKCON.T4CS2 = 0
                          T4CLKCON.T4CS3 = 0
                        #endif
                      #endif
                    #endif
                #endif
             #endif

             #ifdef USE_HPWM_TIMER6 TRUE
                #ifdef var(T6CON)
                  case 6:
                    PR6 = PRx_Temp
                    'Set the Bits for the Prescaler
                    'Setup Timerx by clearing the Prescaler bits - it is set next....
                    #ifdef bit(T6CKPS6)
                        SET T6CKPS0 OFF
                        SET T6CKPS1 OFF
                        SET T6CKPS6 OFF
                        'Set Prescaler bits T6CON @1
                        if Tx_PR = 4  then SET T6CKPS1 ON
                        if Tx_PR = 16 then SET T6CKPS6 ON
                        if Tx_PR = 64 then SET T6CKPS6 ON: SET T6CKPS1 ON
                    #endif

                    'Revised to show overflow issue
                    #ifndef bit(T6CKPS6)
                      #ifdef bit(T6CKPS0)
                        SET T6CKPS0 OFF
                        SET T6CKPS1 OFF
                        'Set Prescaler bits T6CON @2
                        if Tx_PR = 4  then SET T6CKPS0 ON
                        if Tx_PR = 16 then SET T6CKPS1 ON
                        'Overflowed - this chip cannot handle the desired PWMFrequency. Lower clock speed.
                        'if T6CON and 3 = 3 then an overflow has occured!
                        if Tx_PR = 64 then SET T6CKPS0 ON: SET T6CKPS1 ON
                      #endif
                    #endif

                    'Set Clock Source, if required
                    #ifdef var(T6CLKCON)
                      #ifndef  CHIPTIMERXCLOCKSOURCESVariant
                          'Set to FOSC/4 for backward compatibility@6a where CS<3:0> = 0001 = Fosc/4
                          T6CLKCON.T6CS0 = 1
                          T6CLKCON.T6CS1 = 0
                          T6CLKCON.T6CS2 = 0
                          T6CLKCON.T6CS3 = 0
                      #else
                        #if CHIPTIMERxCLOCKSOURCESVariant = 1
                          'Set to FOSC/4 for backward compatibility@6b where CS<3:0> = 0000 = Fosc/4
                          T6CLKCON.T6CS0 = 0
                          T6CLKCON.T6CS1 = 0
                          T6CLKCON.T6CS2 = 0
                          T6CLKCON.T6CS3 = 0
                        #endif
                      #endif
                    #endif
                #endif
              #endif

          end Select

 End_of_CCPPWMSetupClockSource:

  #ifdef HPWM_FAST
          PWMFreqOld = PWMFreq
          'Cache the PRx_Temp to support HPWM_FAST.
          PRx_Temp_Cache = PRx_Temp
      Else
        'As wierd as this looks.... restore the cache to PRX_temp as we need it when in HPWM_Fast mode
        'PRx_Temp is used in channel setting section, so, if we did not restore we would not have a consistent set of information.
        PRx_Temp = PRx_Temp_Cache
      End If
  #endif

  'this code can be optimised by using defines USE_HPWMCCP1|2|3|4|5
  'and, you can define user setup and exit commands using AddHPWMCCPSetupN and  AddHPWMCCPExitN
  '   These can be used to FIX little errors!
 SetupTheCorrectTimerBits:
  'ChipPWMTimerVariant some chips have variants on CCPTMRS0
  Dim TimerSelectionBits as Byte
  #if ChipPWMTimerVariant = 2
        TimerSelectionBits =  (_PWMTimerSelected / 2 )
  #endif

  #if ChipPWMTimerVariant = 1
        TimerSelectionBits =  (_PWMTimerSelected / 2 )-1
  #endif

  #ifndef ChipPWMTimerVariant
      TimerSelectionBits =  (_PWMTimerSelected / 2 )-1
  #endif

 SetupCCPPWMRegisters:
  'If there is no ASM here then the CHIP is not recognised, look at section SETUPCCPPWMREGISTERS in PMW.H

  #ifdef USE_HPWMCCP1 TRUE

    #ifdef VAR(CCP1CON)
      //Only process this section when CCP1CON exists
      
      #ifdef AddHPWMCCPSetup1
        AddHPWMCCPSetup1
      #endif

      #ifndef BIT(CCP1FMT)

          #ifdef NoVar(CCP2CON)
            'NoVar(CCP2CON) - We assumne there is is only 1 CCP module on Chip and this assumes the legacy chip do not have CCP1FMT
            PRx_Temp = PWMDuty * (PRx_Temp + 2)
            CCPR1L = PRx_Temp_H
            If PWMDuty = 0 Then CCPR1L = 0  ' Assure OFF at Zero
            SET CCP1CON.CCP1M3 ON
            SET CCP1CON.CCP1M2 ON
            SET CCP1CON.CCP1M1 OFF
            SET CCP1CON.CCP1M0 OFF
          #endif

          #ifdef BIT(C1TSEL0)
          C1TSEL0 = TimerSelectionBits.0
          C1TSEL1 = TimerSelectionBits.1
          #endif

      #endif

      #if ChipFamily = 16
          ChipFamilyPWMCCP1CON16Handler:
      #endif

      #if ChipSubFamily = ChipFamily18FxxQ41 OR ChipSubFamily = ChipFamily18FxxQ40
          'Identify the 18FxxQxx chips that only have ONE CCP1PMW
          calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
          CCPR1H = PRx_Temp_H
          CCPR1L = PRx_Temp
          SET CCP1M3 ON
          SET CCP1M2 ON
          SET CCP1M1 ON
          SET CCP1M0 ON

          #ifdef bit(CCP1EN)
            SET CCP1EN ON
          #endif

          #ifdef bit(CCP1CON_EN)
            SET CCP1CON_EN ON
          #endif

          #ifdef bit(CCP1FMT)
            SET CCP1FMT ON
          #endif

      #endif

      #ifdef AddHPWMCCPExit1
        AddHPWMCCPExit1
      #endif

      #if ChipFamily = 16
          EndChipFamilyPWMCCP1CON16Handler:
      #endif

    #endif
  #endif



  #ifdef Var(CCP2CON)
   'Devices with more than one CCP
    #ifdef USE_HPWMCCP1 TRUE 'USE_HPWMCCP1 TRUE

      #ifdef AddHPWMCCPSetup1
        AddHPWMCCPSetup1
      #endif

      if PWMChannel = 1 then



        #ifndef BIT(CCP1FMT)
            'ifndef BIT(CCP1FMT) Testing this bit is to identify the 2016 chip that use CCPR1H and CCPR1L for PWM
            PRx_Temp = PWMDuty * (PRx_Temp + 2)  'Correction
            CCPR1L = PRx_Temp_H
            If PWMDuty = 0 Then CCPR1L = 0  ' Assure OFF at Zero
            SET CCP1M3 ON
            SET CCP1M2 ON
            SET CCP1M1 OFF
            SET CCP1M0 OFF
        #ENDIF


        #ifdef BIT(CCP1FMT)
            'ifdef BIT(CCP1FMT) Testing this bit is to identify the 2016 chip that use CCPR1H and CCPR1L for PWM
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            CCPR1H = PRx_Temp_H
            CCPR1L = PRx_Temp
            SET CCP1M3 ON
            SET CCP1M2 ON
            SET CCP1M1 ON
            SET CCP1M0 ON

            #ifdef bit(CCP1EN)
              SET CCP1EN ON
            #endif

            #ifdef bit(CCP1CON_EN)
              SET CCP1CON_EN ON
            #endif

            #ifdef bit(CCP1FMT)
              SET CCP1FMT ON
            #endif
        #ENDIF

        #ifdef BIT(C1TSEL0)
        C1TSEL0 = TimerSelectionBits.0
        C1TSEL1 = TimerSelectionBits.1
        #endif

      end if

      #ifdef AddHPWMCCPExit1
        AddHPWMCCPExit1
      #endif

    #endif

    #ifdef USE_HPWMCCP2 TRUE 'USE_HPWMCCP2 TRUE

      #ifdef AddHPWMCCPSetup2
        AddHPWMCCPSetup2
      #endif

      #IFDEF VAR(CCP2CON)

          if PWMChannel = 2 then

            #ifndef BIT(CCP2FMT)
                PRx_Temp = PWMDuty * ( PRx_Temp + 2)  'Correction
                CCPR2L = PRx_Temp_H
                If PWMDuty = 0 Then CCPR2L = 0  ' Assure OFF at Zero
                SET CCP2M3 ON
                SET CCP2M2 ON
                SET CCP2M1 OFF
                SET CCP2M0 OFF
            #ENDIF

            #ifdef BIT(CCP2FMT)
                calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
                CCPR2H = PRx_Temp_H
                CCPR2L = PRx_Temp
                SET CCP2M3 ON
                SET CCP2M2 ON
                SET CCP2M1 ON
                SET CCP2M0 ON

                #ifdef bit(CCP2EN)
                  SET CCP2EN ON
                #endif

                #ifdef bit(CCP2CON_EN)
                  SET CCP2CON_EN ON
                #endif

                #ifdef bit(CCP2FMT)
                  SET CCP2FMT ON
                #endif
            #ENDIF

            #ifdef BIT(C2TSEL0)
            C2TSEL0 = TimerSelectionBits.0
            C2TSEL1 = TimerSelectionBits.1
            #endif

          end if
      #endif

      #ifdef AddHPWMCCPExit2
        AddHPWMCCPExit2
      #endif

    #endif

  #endif

  #ifdef USE_HPWMCCP3 TRUE

    #ifdef AddHPWMCCPSetup3
      AddHPWMCCPSetup3
    #endif

    #ifdef Var(CCP3CON)
      if PWMChannel = 3 then 'USE_HPWMCCP3 TRUE

        #ifndef BIT(CCP3FMT)
            PRx_Temp = PWMDuty * (PRx_Temp + 2)  'Correction
            CCPR3L = PRx_Temp_H
            If PWMDuty = 0 Then CCPR3L = 0  ' Assure OFF at Zero
            SET CCP3M3 ON
            SET CCP3M2 ON
            SET CCP3M1 OFF
            SET CCP3M0 OFF
        #ENDIF

        #ifdef BIT(CCP3FMT)
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            CCPR3H = PRx_Temp_H
            CCPR3L = PRx_Temp
            SET CCP3M3 ON
            SET CCP3M2 ON
            SET CCP3M1 ON
            SET CCP3M0 ON

            #ifdef bit(CCP3EN)
              SET CCP3EN ON
            #endif


            #ifdef bit(CCP3CON_EN)
              SET CCP3CON_EN ON
            #endif

            #ifdef bit(CCP3FMT)
              SET CCP3FMT ON
            #endif

        #ENDIF

        #ifdef BIT(C3TSEL0)
        C3TSEL0 = TimerSelectionBits.0
        C3TSEL1 = TimerSelectionBits.1
        #endif

      end if
    #endif

    #ifdef AddHPWMCCPExit3
      AddHPWMCCPExit3
    #endif

  #endif

  #ifdef USE_HPWMCCP4 TRUE

    #ifdef AddHPWMCCPSetup4
      AddHPWMCCPSetup4
    #endif

    #ifdef Var(CCP4CON)    'Added 17.04.2105
      if PWMChannel = 4 then  'USE_HPWMCCP4 TRUE

        #ifndef BIT(CCP4FMT)
            PRx_Temp = PWMDuty * (PRx_Temp + 2)  '
            CCPR4L = PRx_Temp_H
            If PWMDuty = 0 Then CCPR4L = 0  ' Assure OFF at Zero

            SET CCP4M3 ON'These my have been remapped using a script - do check ASM and script in INITPWM @1@CCP4
            SET CCP4M2 ON'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP4M1 OFF'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP4M0 OFF'These my have been remapped using a script - do check ASM and script in INITPWM
        #ENDIF

        #ifdef BIT(CCP4FMT)
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6.  ifdef VAR(CCPR1H)
            CCPR4H = PRx_Temp_H
            CCPR4L = PRx_Temp
            SET CCP4M3 ON'These my have been remapped using a script - do check ASM and script in INITPWM @2@CCP4
            SET CCP4M2 ON'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP4M1 ON'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP4M0 ON'These my have been remapped using a script - do check ASM and script in INITPWM

            #ifdef bit(CCP4EN)
              SET CCP4EN ON
            #endif

            #ifdef bit(CCP4CON_EN)
              SET CCP4CON_EN ON
            #endif

            #ifdef bit(CCP4FMT)
              SET CCP4FMT ON
            #endif

        #ENDIF

        #ifdef BIT(C4TSEL0)
        C4TSEL0 = TimerSelectionBits.0
        C4TSEL1 = TimerSelectionBits.1
        #endif

      end if
    #endif

    #ifdef AddHPWMCCPExit4
      AddHPWMCCPExit4
    #endif

  #endif

  #ifdef USE_HPWMCCP5 TRUE

    #ifdef AddHPWMCCPSetup5
      AddHPWMCCPSetup5
    #endif

    #ifdef Var(CCP5CON)    'ADDED 10.10.2016 erv
      if PWMChannel = 5 then  'USE_HPWMCCP5 TRUE

        #ifndef BIT(CCP5FMT)
            PRx_Temp = PWMDuty * (PRx_Temp + 2)
            CCPR5L = PRx_Temp_H
            If PWMDuty = 0 Then CCPR5L = 0  ' Assure OFF at Zero

            SET CCP5M3 ON'These my have been remapped using a script - do check ASM and script in INITPWM @1@CCP5
            SET CCP5M2 ON'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP5M1 OFF'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP5M0 OFF'These my have been remapped using a script - do check ASM and script in INITPWM
        #ENDIF

        #ifdef BIT(CCP5FMT)
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6.  ifdef VAR(CCPR1H)
            CCPR5H = PRx_Temp_H
            CCPR5L = PRx_Temp
            SET CCP5M3 ON'These my have been remapped using a script - do check ASM and script in INITPWM @2@CCP5
            SET CCP5M2 ON'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP5M1 ON'These my have been remapped using a script - do check ASM and script in INITPWM
            SET CCP5M0 ON'These my have been remapped using a script - do check ASM and script in INITPWM

            #ifdef bit(CCP5EN)
              SET CCP5EN ON
            #endif

            #ifdef bit(CCP5CON_EN)
              SET CCP5CON_EN ON
            #endif

            #ifdef bit(CCP5FMT)
              SET CCP5FMT ON
            #endif

        #ENDIF

        #ifdef BIT(C5TSEL0)
        C5TSEL0 = TimerSelectionBits.0
        C5TSEL1 = TimerSelectionBits.1
        #endif

      end if
    #endif

    #ifdef AddHPWMCCPExit5
      AddHPWMCCPExit5
    #endif

  #endif

end sub


' Byte support for DUTY... folks may not know that they need to pass a 10bit number
' Takes a byte in the range of 0-255 and passes the the 10bit method with the PWMResolution set to 255
sub HPWM (In PWMChannel, In PWMFreq as WORD, in PWMDuty as byte, in _PWMTimerSelected as byte, Optional in PWMResolution as word = 255 )
    dim PWMDuty as word
    HPWM ( PWMChannel, PWMFreq, [word]PWMDuty, _PWMTimerSelected, PWMResolution  )
end Sub

'Word for DUTY also defaults the PWMResolution to 1023 (10bit support)
;Overloaded signature: BYTE:WORD:WORD:BYTE:word:
sub HPWM (In PWMChannel, In PWMFreq as WORD, in PWMDuty as WORD , in _PWMTimerSelected  as byte, Optional in PWMResolution as word = 1023)

    Dim PRx_Temp as LONG
    #ifdef ChipPWMTimerVariant
      Dim TimerSelectionBits as byte
    #endif

  'If HPWM_FAST operation selected, only recalculate duty timer prescaler when
  'needed.
  #ifdef HPWM_FAST
      If ( PWMFreq <> PWMFreqOld )  or ( _PWMTimerSelected <> PWMTimerOld ) Then
  #endif


    ' Commence calculations of PMW parameters
    ' This figures out Prescaler required.  We can set to 1, 4 or 16 to set Tx_PR
    ' So, start with 1 - check the remainder. If the remainder in the high byte is greater then zero then do same with a prescaler value of 4
    ' So, using 4 - check the remainder. If the remainder in the high byte is greater then zero then do same with a prescaler value of 16
    ' So, using 16
    'This simply sets up Tx_PR to 1,4 or 16
    PRx_Temp  = PWMOsc1 / PWMFreq
    Tx_PR = 1
    IF PRx_Temp_H > 0 then
      Tx_PR = 4
      'Divide by 4
      set STATUS.C off
      rotate PRx_Temp right
      set STATUS.C off
      rotate PRx_Temp right
    end if

    IF PRx_Temp_H > 0 then
      Tx_PR = 16
      'Divide by 4
      set STATUS.C off
      rotate PRx_Temp right
      set STATUS.C off
      rotate PRx_Temp right
    end if

    IF PRx_Temp_H > 0 then
      Tx_PR = 64
      'Divide by 4
      set STATUS.C off
      rotate PRx_Temp right
      set STATUS.C off
      rotate PRx_Temp right
    end if

'This section setups the timer

    'Set up PRx dependent on timer selected
    Select Case _PWMTimerSelected

        #ifdef USE_HPWM_TIMER2 TRUE
          case 2 'Select timer2 as the driver for PWM
            'Set PR2
            PR2 = PRx_Temp  'This is required in the next sction of code, and as will not know which timer has been selected

            'Set Clock Source, if required
            #ifdef var(T2CLKCON)
              #ifndef  CHIPTIMERXCLOCKSOURCESVariant
                  'Set to FOSC/4 for backward compatibility@2c where CS<3:0> = 0001 = Fosc/4
                  T2CLKCON.T2CS0 = 1
                  T2CLKCON.T2CS1 = 0
                  T2CLKCON.T2CS2 = 0
                  T2CLKCON.T2CS3 = 0
              #else
                #if CHIPTIMERxCLOCKSOURCESVariant = 1
                  'Set to FOSC/4 for backward compatibility@2d where CS<3:0> = 0000 = Fosc/4
                  T2CLKCON.T2CS0 = 0
                  T2CLKCON.T2CS1 = 0
                  T2CLKCON.T2CS2 = 0
                  T2CLKCON.T2CS3 = 0
                #endif
              #endif
            #endif

            'T2PSYNC Not Synchronized; T2MODE Software control; T2CKPOL Rising Edge; T2CKSYNC Not Synchronized
            'T2HLT = 0x00
            #ifdef var(T2HLT): T2HLT = 0x00: #ENDIF

            'T2RSEL T2CKIPPS pin
            'T2RST = 0x00
            #ifdef var(T2RST): T2RST = 0x00: #ENDIF

            'TMR2.  Holding Register for the 8-bit TMR2 Register
            'T2TMR = 0x00
            #ifdef var(T2TMR): T2TMR = 0x00: #ENDIF

            'Setup Timerx by clearing the Prescaler bits - it is set next....
            #ifdef bit(T2CKPS2)
                SET T2CKPS0 OFF
                SET T2CKPS1 OFF
                SET T2CKPS2 OFF
                'Set Prescaler bits T2CON @3
                if Tx_PR = 4  then SET T2CKPS1 ON
                if Tx_PR = 16 then SET T2CKPS2 ON
                if Tx_PR = 64 then SET T2CKPS2 ON: SET T2CKPS1 ON
            #endif

            #ifndef bit(T2CKPS2)
                SET T2CKPS0 OFF
                SET T2CKPS1 OFF
                'Set Prescaler bits T2CON @4
                if Tx_PR = 4  then SET T2CKPS0 ON
                if Tx_PR = 16 then SET T2CKPS1 ON
                if Tx_PR = 64 then SET T2CKPS0 ON: SET T2CKPS1 ON
            #endif

            'Clearing IF flag.
            SET TMR2IF OFF

            'Start Timer2
            SET TMR2ON ON
        #endif

        #ifdef USE_HPWM_TIMER4 TRUE
            #ifdef var(T4CON)
              case 4
                'Set PR4
                PR4 = PRx_Temp  'This is required in the next sction of code, and as will not know which timer has been selected

                'Set Clock Source, if required
                #ifdef var(T4CLKCON)
                  #ifndef  CHIPTIMERXCLOCKSOURCESVariant
                      'Set to FOSC/4 for backward compatibility@4c where CS<3:0> = 0001 = Fosc/4
                      T4CLKCON.T4CS0 = 1
                      T4CLKCON.T4CS1 = 0
                      T4CLKCON.T4CS2 = 0
                      T4CLKCON.T4CS3 = 0
                  #else
                    #if CHIPTIMERxCLOCKSOURCESVariant = 1
                      'Set to FOSC/4 for backward compatibility@4d where CS<3:0> = 0000 = Fosc/4
                      T4CLKCON.T4CS0 = 0
                      T4CLKCON.T4CS1 = 0
                      T4CLKCON.T4CS2 = 0
                      T4CLKCON.T4CS3 = 0
                    #endif
                  #endif
                #endif

                'T4PSYNC Not Synchronized; T4MODE Software control; T4CKPOL Rising Edge; T4CKSYNC Not Synchronized
                'T4HLT = 0x00
              #ifdef var(T4HLT): T4HLT = 0x00: #ENDIF

              'T4RSEL T4CKIPPS pin
              'T4RST = 0x00
              #ifdef var(T4RST): T4RST = 0x00: #ENDIF

              'TMR4. Holding Register for the 8-bit TMR4` Register
              'T4TMR = 0x00
              #ifdef var(T4TMR): T4TMR = 0x00: #ENDIF

                #ifdef bit(T4CKPS2)
                    SET T4CKPS0 OFF
                    SET T4CKPS1 OFF
                    SET T4CKPS2 OFF
                    'Set Prescaler bits T4CON @3
                    if Tx_PR = 4  then SET T4CKPS1 ON
                    if Tx_PR = 16 then SET T4CKPS2 ON
                    if Tx_PR = 64 then SET T4CKPS2 ON: SET T4CKPS1 ON
                #endif

                #ifndef bit(T4CKPS2)
                    SET T4CKPS0 OFF
                    SET T4CKPS1 OFF
                    'Set Prescaler bits T4CON@4
                    if Tx_PR = 4  then SET T4CKPS0 ON
                    if Tx_PR = 16 then SET T4CKPS1 ON
                    if Tx_PR = 64 then SET T4CKPS0 ON: SET T4CKPS1 ON
                #endif
                'Clearing IF flag.
                SET TMR4IF OFF

                'Start Timer2
                SET TMR4ON ON
            #endif
        #endif

        #ifdef USE_HPWM_TIMER6 TRUE

          #ifdef var(T6CON)
              case 6

              'Set PR6
                PR6 = PRx_Temp  'This is required in the next sction of code, and will not know which timer has been selected

                'Set Clock Source, if required
                #ifdef var(T6CLKCON)
                  #ifndef  CHIPTIMERXCLOCKSOURCESVariant
                      'Set to FOSC/4 for backward compatibility@6c where CS<3:0> = 0001 = Fosc/4
                      T6CLKCON.T6CS0 = 1
                      T6CLKCON.T6CS1 = 0
                      T6CLKCON.T6CS2 = 0
                      T6CLKCON.T6CS3 = 0
                  #else
                    #if CHIPTIMERxCLOCKSOURCESVariant = 1
                      'Set to FOSC/4 for backward compatibility@6d where CS<3:0> = 0000 = Fosc/4
                      T6CLKCON.T6CS0 = 0
                      T6CLKCON.T6CS1 = 0
                      T6CLKCON.T6CS2 = 0
                      T6CLKCON.T6CS3 = 0
                    #endif
                  #endif
                #endif

                'T6PSYNC Not Synchronized; T6MODE Software control; T6CKPOL Rising Edge; T6CKSYNC Not Synchronized
                'T6HLT = 0x00
                #ifdef var(T6HLT): T6HLT = 0x00: #ENDIF

                'T6RSEL T6CKIPPS pin
                'T6RST = 0x00
                #ifdef var(T6RST): T6RST = 0x00: #ENDIF

                'TMR6.  Holding Register for the 8-bit TMR6 Register
                'T6TMR = 0x00
                #ifdef var(T6TMR): T6TMR = 0x00: #ENDIF

                #ifdef bit(T6CKPS2)
                    SET T6CKPS0 OFF
                    SET T6CKPS1 OFF
                    SET T6CKPS2 OFF
                    'Set Prescaler bits T6CON @3
                    if Tx_PR = 4  then SET T6CKPS1 ON
                    if Tx_PR = 16 then SET T6CKPS2 ON
                    if Tx_PR = 64 then SET T6CKPS2 ON: SET T6CKPS1 ON
                #endif

                #ifndef bit(T6CKPS2)
                    SET T6CKPS0 OFF
                    SET T6CKPS1 OFF
                    'Set Prescaler bits T6CON@4
                    if Tx_PR = 4  then SET T6CKPS0 ON
                    if Tx_PR = 16 then SET T6CKPS1 ON
                    if Tx_PR = 64 then SET T6CKPS0 ON: SET T6CKPS1 ON
                #endif

                'Clearing IF flag.
                SET TMR6IF OFF

                'Start Timer6
                SET TMR6ON ON
            #endif
        #endif

    End Select

  #ifdef HPWM_FAST
          PWMFreqOld = PWMFreq
          PWMTimerOld = _PWMTimerSelected
          'Cache the PRx_Temp to support HPWM_FAST.
          PRx_Temp_Cache = PRx_Temp
      Else
        'As wierd as this looks.... restore the cache to PRX_temp as we need it when in HPWM_Fast mode
        'PRx_Temp is used in channel setting section, so, if we did not restore we would not have a consistent set of information.
        PRx_Temp = PRx_Temp_Cache
      End if
  #endif

 TenBitPMWSection:
'This section setups up the PWM Duty

 'The following code can be optimised via 'defines'

    Dim PRx_Temp_Cache as Long

    'Cache the PRx_Temp value ONLY when NOT HPWM_FAST - this can be then used by HPWMUpdate
    #ifndef HPWM_FAST
        PRx_Temp_Cache = PRx_Temp
    #endif


    #ifdef USE_HPWM1 TRUE

      #ifdef AddHPWMSetup1
        AddHPWMSetup1
      #endif

      #if var(PWM1DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 1 then  'in section USE_HPWM1
            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM1DCH = PRx_Temp_H
            PWM1DCL = PRx_Temp

            ' Select timer by updating CCPTMRS register
            #if ChipPWMTimerVariant = 1
              TimerSelectionBits =  (_PWMTimerSelected / 2 )-1 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              SetWith ( P1TSEL0, TimerSelectionBits.0 )
              SetWith ( P1TSEL1, TimerSelectionBits.1 )
            #endif

            #if ChipPWMTimerVariant = 2
              TimerSelectionBits =  (_PWMTimerSelected / 2 ) 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              #IFDEF BIT(P1TSEL0)
                SetWith ( P1TSEL0, TimerSelectionBits.0 )
                SetWith ( P1TSEL1, TimerSelectionBits.1 )
              #endif

              #IFDEF BIT(C1TSEL1)
                SetWith ( C1TSEL0, TimerSelectionBits.0 )
                SetWith ( C1TSEL1, TimerSelectionBits.1 )
              #endif

            #endif

            #ifndef ChipPWMTimerVariant

              #IFDEF BIT(P1TSEL0)
                SetWith ( P1TSEL0, _PWMTimerSelected.1 )
                SetWith ( P1TSEL1, _PWMTimerSelected.2 )
              #ENDIF

              #IFDEF BIT(PWM1OE)
                PWM1OE = 1
              #ENDIF

            #endif

            #IFDEF BIT(PWM1EN) 'this simply stops error messages when the does not exit
              'Start PMW1
              Set PWM1EN On
            #ENDIF

        End if

      #endif

      #ifdef AddHPWMExit1
        AddHPWMExit1
      #endif

    #endif

    #ifdef USE_HPWM2 TRUE

      #ifdef AddHPWMSetup2
        AddHPWMSetup2
      #endif

      #if var(PWM2DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 2 then  'in section USE_HPWM2
            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM2DCH = PRx_Temp_H
            PWM2DCL = PRx_Temp

            ' Select timer by updating CCPTMRS register
            #if ChipPWMTimerVariant = 1
              TimerSelectionBits =  (_PWMTimerSelected / 2 )-1 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              SetWith ( P2TSEL0, TimerSelectionBits.0 )
              SetWith ( P2TSEL1, TimerSelectionBits.1 )
            #endif

            #if ChipPWMTimerVariant = 2
              TimerSelectionBits =  (_PWMTimerSelected / 2 ) 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              #IFDEF BIT(P2TSEL0)
                SetWith ( P2TSEL0, TimerSelectionBits.0 )
                SetWith ( P2TSEL1, TimerSelectionBits.1 )
              #endif

              #IFDEF BIT(C2TSEL1)
                SetWith ( C2TSEL0, TimerSelectionBits.0 )
                SetWith ( C2TSEL1, TimerSelectionBits.1 )
              #endif

            #endif

            #ifndef ChipPWMTimerVariant

              #IFDEF BIT(P2TSEL0)
                SetWith ( P2TSEL0, _PWMTimerSelected.1 )
                SetWith ( P2TSEL1, _PWMTimerSelected.2 )
              #ENDIF

              #IFDEF BIT(PWM2OE)
                PWM2OE = 1
              #ENDIF

            #endif

            #IFDEF BIT(PWM2EN) 'this simply stops error messages when the does not exit
              'Start PMW2
              Set PWM2EN On
            #ENDIF

        End if

      #endif

      #ifdef AddHPWMExit2
        AddHPWMExit2
      #endif

    #endif

    #ifdef USE_HPWM3 TRUE

      #ifdef AddHPWMSetup3
        AddHPWMSetup3
      #endif

      #if var(PWM3DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 3 then  'in section USE_HPWM3
            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM3DCH = PRx_Temp_H
            PWM3DCL = PRx_Temp

            ' Select timer by updating CCPTMRS register
            #if ChipPWMTimerVariant = 1
              TimerSelectionBits =  (_PWMTimerSelected / 2 )-1 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              SetWith ( P3TSEL0, TimerSelectionBits.0 )
              SetWith ( P3TSEL1, TimerSelectionBits.1 )
            #endif


            #if ChipPWMTimerVariant = 2
              TimerSelectionBits =  (_PWMTimerSelected / 2 ) 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              #IFDEF BIT(P3TSEL0)
                SetWith ( P3TSEL0, TimerSelectionBits.0 )
                SetWith ( P3TSEL1, TimerSelectionBits.1 )
              #endif

              #IFDEF BIT(C3TSEL1)
                SetWith ( C3TSEL0, TimerSelectionBits.0 )
                SetWith ( C3TSEL1, TimerSelectionBits.1 )
              #endif

            #endif


            #ifndef ChipPWMTimerVariant

              #IFDEF BIT(P3TSEL0)
                SetWith ( P3TSEL0, _PWMTimerSelected.1 )
                SetWith ( P3TSEL1, _PWMTimerSelected.2 )
              #ENDIF

              #IFDEF BIT(PWM3OE)
                PWM3OE = 1
              #ENDIF

            #endif

            #IFDEF BIT(PWM3EN) 'this simply stops error messages when the does not exit
              'Start PMW3
              Set PWM3EN On
            #ENDIF

        End if

      #endif

      #ifdef AddHPWMExit3
        AddHPWMExit3
      #endif

    #endif


    #ifdef USE_HPWM4 TRUE

      #ifdef AddHPWMSetup4
        AddHPWMSetup4
      #endif

      #if var(PWM4DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 4 then  'in section USE_HPWM4
            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM4DCH = PRx_Temp_H
            PWM4DCL = PRx_Temp

            ' Select timer by updating CCPTMRS1 register
            #if ChipPWMTimerVariant = 1
              TimerSelectionBits =  (_PWMTimerSelected / 2 )-1 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              SetWith ( P4TSEL0, TimerSelectionBits.0 )
              SetWith ( P4TSEL1, TimerSelectionBits.1 )
            #endif

            #if ChipPWMTimerVariant = 2
              TimerSelectionBits =  (_PWMTimerSelected / 2 ) 'This is a ChipPWMTimerVariant chip.  Move the timer bits to the left by one bit to cater for different addressing

              #IFDEF BIT(P4TSEL0)
                SetWith ( P4TSEL0, TimerSelectionBits.0 )
                SetWith ( P4TSEL1, TimerSelectionBits.1 )
              #endif

              #IFDEF BIT(C4TSEL1)
                SetWith ( C4TSEL0, TimerSelectionBits.0 )
                SetWith ( C4TSEL1, TimerSelectionBits.1 )
              #endif

            #endif


            #ifndef ChipPWMTimerVariant
              #IFDEF BIT(P4TSEL0)
                SetWith ( P4TSEL0, _PWMTimerSelected.1 )
                SetWith ( P4TSEL1, _PWMTimerSelected.2 )
              #ENDIF

              #IFDEF BIT(PWM4OE)
                PWM4OE = 1
              #ENDIF
            #endif

            #IFDEF BIT(PWM4EN) 'this simply stops error messages when the does not exit
              'Start PMW4
              Set PWM4EN On
            #ENDIF

        End if

      #endif

      #ifdef AddHPWMExit4
        AddHPWMExit4
      #endif

    #endif


    #ifdef USE_HPWM5 TRUE

      #ifdef AddHPWMSetup5
        AddHPWMSetup5
      #endif

      #if var(PWM5DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 5 then  'in section USE_HPWM5
            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM5DCH = PRx_Temp_H
            PWM5DCL = PRx_Temp

            ' Select timer by updating CCPTMRS1 register
            #ifdef bit(P5TSEL0)
              SetWith ( P5TSEL0, _PWMTimerSelected.1 )
              SetWith ( P5TSEL1, _PWMTimerSelected.2 )
            #endif

            #IFDEF BIT(PWM5OE)
              PWM5OE = 1
            #ENDIF

            #IFDEF BIT(PWM5EN) 'this simply stops error messages when the does not exit
              'Start PMW5
              Set PWM5EN On
            #ENDIF

        End if

      #endif

      #ifdef AddHPWMExit5
        AddHPWMExit5
      #endif

    #endif


    #ifdef USE_HPWM6 TRUE

      #ifdef AddHPWMSetup6
        AddHPWMSetup6
      #endif

      #if var(PWM6DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 6 then  'in section USE_HPWM6
            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM6DCH = PRx_Temp_H
            PWM6DCL = PRx_Temp
            ' Select timer by updating CCPTMRS1 register
            #ifdef bit(P6TSEL0)
              SetWith ( P6TSEL0, _PWMTimerSelected.1 )
              SetWith ( P6TSEL1, _PWMTimerSelected.2 )
            #endif

            #IFDEF BIT(PWM6OE)
              PWM6OE = 1
            #ENDIF

            #IFDEF BIT(PWM6EN) 'this simply stops error messages when the does not exit
              'Start PMW6
              Set PWM6EN On
            #ENDIF


        End if

      #endif

      #ifdef AddHPWMExit6
        AddHPWMExit6
      #endif

    #endif


    #ifdef USE_HPWM7 TRUE

      #ifdef AddHPWMSetup7
        AddHPWMSetup7
      #endif

      #if var(PWM7DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 7 then   'in section USE_HPWM7

            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM7DCH = PRx_Temp_H
            PWM7DCL = PRx_Temp

            ' Select timer by updating CCPTMRS1 register
            #ifdef bit(P7TSEL0)
              SetWith ( P7TSEL0, _PWMTimerSelected.1 )
              SetWith ( P7TSEL1, _PWMTimerSelected.2 )
            #endif

            #IFDEF BIT(PWM7OE)
              PWM7OE = 1
            #ENDIF

            #IFDEF BIT(PWM7EN) 'this simply stops error messages when the does not exit
              'Start PMW7
              Set PWM7EN On
            #ENDIF

        end if

      #endif

      #ifdef AddHPWMExit7
        AddHPWMExit7
      #endif

    #endif

    #ifdef USE_HPWM8 TRUE

      #ifdef AddHPWMSetup8
        AddHPWMSetup8
      #endif

      #if var(PWM8DCH)   'If no channel.... no-point in compiling the code

        if PWMChannel = 8 then   'in section USE_HPWM8

            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM8DCH = PRx_Temp_H
            PWM8DCL = PRx_Temp

            ' Select timer by updating CCPTMRS1 register
            #ifdef bit(P8TSEL0)
              SetWith ( P8TSEL0, _PWMTimerSelected.1 )
              SetWith ( P8TSEL1, _PWMTimerSelected.2 )
            #endif

            #IFDEF BIT(PWM8OE)
              PWM8OE = 1
            #ENDIF

            #IFDEF BIT(PWM8EN) 'this simply stops error messages when the does not exit
              'Start PMW8
              Set PWM8EN On
            #ENDIF

        end if

      #endif

      #ifdef AddHPWMExit8
        AddHPWMExit8
      #endif

    #endif

end sub


' Update the Duty Only.
' Assumes that PRx_Temp_Cache  and PWMResolution are defined by previous calls to the non-CCP HPWM method
Sub HPWMUpdate (In PWMChannel, in PWMDuty as WORD  )
    'The following code can be optimised via 'defines'

    dim PRx_Temp as long
    dim PWMDuty as word
    dim PWMResolution as word

    'Restore the cache value
    PRx_Temp = PRx_Temp_Cache



    #ifdef USE_HPWM1 TRUE
PWMUPDATE_HARDWAREMODEPWM1:

      #if var(PWM1DCH)   'If no channel.... no-point in compiling the code

          if PWMChannel = 1 then  'in section USE_HPWM1

              #ifdef AddHPWMUpdate1
                AddHPWMUpdate1
              #endif

              ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
              calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
              'assumes PRx_Temp and PWMDuty are valid
              PWM1DCH = PRx_Temp_H
              PWM1DCL = PRx_Temp

              #IFDEF BIT(PWM1OE)
                PWM1OE = 1
              #ENDIF

              #IFDEF BIT(PWM1EN) 'this simply stops error messages when the does not exit
                'Start PMW1
                Set PWM1EN On
              #ENDIF

              #ifdef AddHPWMExit1
                AddHPWMExit1
              #endif

          End if

      #endif

    #endif

    #ifdef USE_HPWM2 TRUE
PWMUPDATE_HARDWAREMODEPWM2:

      #if var(PWM2DCH)   'If no channel.... no-point in compiling the code

          if PWMChannel = 2 then  'in section USE_HPWM2

              #ifdef AddHPWMUpdate2
                AddHPWMUpdate2
              #endif

              ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
              calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
              'assumes PRx_Temp and PWMDuty are valid
              PWM2DCH = PRx_Temp_H
              PWM2DCL = PRx_Temp

              #IFDEF BIT(PWM2OE)
                PWM2OE = 1
              #ENDIF

              #IFDEF BIT(PWM2EN) 'this simply stops error messages when the does not exit
                Set PWM2EN On
              #ENDIF

              #ifdef AddHPWMExit2
                AddHPWMExit2
              #endif

          end if

      #endif

    #endif

    #ifdef USE_HPWM3 TRUE
PWMUPDATE_HARDWAREMODEPWM3:

      #if var(PWM3DCH)   'If no channel.... no-point in compiling the code

          if PWMChannel = 3 then  'in section USE_HPWM3

            #ifdef AddHPWMUpdate3
            AddHPWMUpdate3
            #endif

            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            'assumes PRx_Temp and PWMDuty are valid
            PWM3DCH = PRx_Temp_H
            PWM3DCL = PRx_Temp

            #IFDEF BIT(PWM3OE)
              PWM3OE = 1
            #ENDIF

            #IFDEF BIT(PWM3EN) 'this simply stops error messages when the does not exit
              Set PWM3EN On
            #ENDIF

            #ifdef AddHPWMExit3
            AddHPWMExit3
            #endif

          End if

      #endif

    #endif

    #ifdef USE_HPWM4 TRUE
PWMUPDATE_HARDWAREMODEPWM4:

      #if var(PWM4DCH)   'If no channel.... no-point in compiling the code

          if PWMChannel = 4 then  'in section USE_HPWM4

              #ifdef AddHPWMUpdate4
                AddHPWMUpdate4
              #endif

              ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
              calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
              PWM4DCH = PRx_Temp_H
              PWM4DCL = PRx_Temp

              #IFDEF BIT(PWM4OE)
                PWM4OE = 1
              #ENDIF

              #IFDEF BIT(PWM4EN) 'this simply stops error messages when the does not exit
                Set PWM4EN On
              #ENDIF

              #ifdef AddHPWMExit4
                AddHPWMExit4
              #endif

          End if

      #endif

    #endif

    #ifdef USE_HPWM5 TRUE
PWMUPDATE_HARDWAREMODEPWM5:

      #if var(PWM5DCH)   'If no channel.... no-point in compiling the code

          if PWMChannel = 5 then  'in section USE_HPWM5

              #ifdef AddHPWMUpdate5
                AddHPWMUpdate5
              #endif

              ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
              calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
              PWM5DCH = PRx_Temp_H
              PWM5DCL = PRx_Temp

              #IFDEF BIT(PWM5OE)
                PWM5OE = 1
              #ENDIF

              #IFDEF BIT(PWM5EN) 'this simply stops error messages when the does not exit
                Set PWM5EN On
              #ENDIF

              #ifdef AddHPWMExit5
                AddHPWMExit5
              #endif

          End if

      #endif

    #endif

    #ifdef USE_HPWM6 TRUE
PWMUPDATE_HARDWAREMODEPWM6:

      #if var(PWM6DCH)   'If no channel.... no-point in compiling the code

          #ifdef AddHPWMUpdate6
            AddHPWMUpdate6
          #endif

          if PWMChannel = 6 then  'in section USE_HPWM6
              ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
              calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
              PWM6DCH = PRx_Temp_H
              PWM6DCL = PRx_Temp

              #IFDEF BIT(PWM6OE)
                PWM6OE = 1
              #ENDIF

              #IFDEF BIT(PWM6EN) 'this simply stops error messages when the does not exit
                Set PWM6EN On
              #ENDIF

          End if

          #ifdef AddHPWMExit6
            AddHPWMExit6
          #endif

      #endif

    #endif

    #ifdef USE_HPWM7 TRUE
PWMUPDATE_HARDWAREMODEPWM7:

      #if var(PWM7DCH)   'If no channel.... no-point in compiling the code

        #ifdef AddHPWMUpdate7
          AddHPWMUpdate7
        #endif

        if PWMChannel = 7 then   'in section USE_HPWM7

            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM7DCH = PRx_Temp_H
            PWM7DCL = PRx_Temp

            #IFDEF BIT(PWM7OE)
              PWM7OE = 1
            #ENDIF

            #IFDEF BIT(PWM7EN) 'this simply stops error messages when the does not exit
              Set PWM7EN On
            #ENDIF

        end if

        #ifdef AddHPWMExit7
          AddHPWMExit7
        #endif

      #endif

    #endif

    #ifdef USE_HPWM8 TRUE
PWMUPDATE_HARDWAREMODEPWM8:

      #if var(PWM8DCH)   'If no channel.... no-point in compiling the code

        #ifdef AddHPWMUpdate8
          AddHPWMUpdate8
        #endif

        if PWMChannel = 8 then   'in section USE_HPWM8

            ' calculates duty, assisgns duty to  bits 15-8 and 7-6 of PMWxDH(H&L) and links this PWM to the correct timer
            calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
            PWM8DCH = PRx_Temp_H
            PWM8DCL = PRx_Temp

            #IFDEF BIT(PWM8OE)
              PWM8OE = 1
            #ENDIF

            #IFDEF BIT(PWM8EN) 'this simply stops error messages when the does not exit
              Set PWM8EN On
            #ENDIF

        end if

        #ifdef AddHPWMExit8
          AddHPWMExit8
        #endif

      #endif

    #endif


PWMUPDATE_CCPMODE:

    #ifdef USE_HPWMCCP1 TRUE
PWMUPDATE_HARDWAREMODEPWMCCP1:
       #ifdef VAR(CCPR1L)
         'ifdef BIT(CCPR1L) Testing this bit is to identify the use of CCPR1L for PWM

          if PWMChannel = 1 then  'in section USE_HPWMCCP1

              #ifdef AddHPWMCCPSetup1
                AddHPWMCCPSetup1
              #endif

              #ifdef NoVar(CCP2CON)
                  'Assume only one CCP
                  PRx_Temp = PWMDuty * (PRx_Temp + 2)  'a correction
                  CCPR1L = PRx_Temp_H
                  If PWMDuty = 0 Then CCPR1L = 0  ' Assure OFF at Zero
              #ENDIF

              #ifdef Var(CCP2CON)
                  'Assume more that one CCP
                  #ifndef BIT(CCP1FMT)
                      'ifndef BIT(CCP1FMT) Testing this bit is to identify the use of CCPR1L for PWM
                      PRx_Temp = PWMDuty * (PRx_Temp + 2)  'Correction
                      CCPR1L = PRx_Temp_H
                      If PWMDuty = 0 Then CCPR1L = 0  ' Assure OFF at Zero
                  #ENDIF


                  #ifdef BIT(CCP1FMT)
                      'ifdef BIT(CCP1FMT) Testing this bit is to identify the use of CCPR1H and CCPR1L for PWM
                      calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
                      CCPR1H = PRx_Temp_H
                      CCPR1L = PRx_Temp
                  #ENDIF
              #ENDIF


              #ifdef AddHPWMCCPExit1
                AddHPWMCCPExit1
              #endif

          End if

        #endif

    #endif

    #ifdef USE_HPWMCCP2 TRUE
PWMUPDATE_HARDWAREMODEPWMCCP2:

      #ifdef VAR(CCPR2L)


          if PWMChannel = 2 then  'in section USE_HPWMCCP2

              #ifdef AddHPWMCCPSetup2
                AddHPWMCCPSetup2
              #endif

              #ifndef BIT(CCP2FMT)
                  'ifndef BIT(CCP2FMT) Testing this bit is to identify the use of CCPR2L for PWM
                  PRx_Temp = PWMDuty * (PRx_Temp + 2)  'Correction
                  CCPR2L = PRx_Temp_H
                  If PWMDuty = 0 Then CCPR2L = 0  ' Assure OFF at Zero
              #ENDIF


              #ifdef BIT(CCP2FMT)
                  'ifdef BIT(CCP2FMT) Testing this bit is to identify the use of CCPR2H and CCPR2L for PWM
                  calculateDuty 'Sets PRx_Temp  to the duty value for bits 15-8 and 7-6
                  CCPR2H = PRx_Temp_H
                  CCPR2L = PRx_Temp
              #ENDIF

              #ifdef AddHPWMCCPExit2
                AddHPWMCCPExit2
              #endif

          end if

      #ENDIF

    #endif

    #ifdef USE_HPWMCCP3 TRUE
PWMUPDATE_HARDWAREMODEPWMCCP3:

      #ifdef VAR(CCPR3L)

          if PWMChannel = 3 then  'in section USE_HPWMCCP3

              #ifdef AddHPWMCCPSetup3
                AddHPWMCCPSetup3
              #endif

              #ifndef BIT(CCP3FMT)
                  'ifndef BIT(CCP3FMT) Testing this bit is to identify the use of CCPR3L for PWM
                  PRx_Temp = PWMDuty * (PRx_Temp + 2)  'Correction
                  CCPR3L = PRx_Temp_H
                  If PWMDuty = 0 Then CCPR3L = 0  ' Assure OFF at Zero
              #ENDIF


              #ifdef BIT(CCP3FMT)
                  'ifdef BIT(CCP3FMT) Testing this bit is to identify the use of CCPR3H and CCPR3L for PWM
                  calculateDuty 'Sets PRx_Temp  to the duty value for bits 0x0F-8 and 7-6
                  CCPR3H = PRx_Temp_H
                  CCPR3L = PRx_Temp
              #ENDIF

              #ifdef AddHPWMCCPExit3
                AddHPWMCCPExit3
              #endif

          End if

      #ENDIF


    #endif

    #ifdef USE_HPWMCCP4 TRUE
PWMUPDATE_HARDWAREMODEPWMCCP4:

      #ifdef VAR(CCPR4L)

          if PWMChannel = 4 then  'in section USE_HPWMCCP4

              #ifdef AddHPWMCCPSetup4
                AddHPWMCCPSetup4
              #endif

              #ifndef BIT(CCP4FMT)
                  'ifndef BIT(CCP4FMT) Testing this bit is to identify the use of CCPR4L for PWM
                  PRx_Temp = PWMDuty * (PRx_Temp + 2)  'Correction
                  CCPR4L = PRx_Temp_H
                  If PWMDuty = 0 Then CCPR4L = 0  ' Assure OFF at Zero
              #ENDIF


              #ifdef BIT(CCP4FMT)
                  'ifdef BIT(CCP4FMT) Testing this bit is to identify the use of CCPR4H and CCPR4L for PWM
                  calculateDuty 'Sets PRx_Temp  to the duty value for bits 0x0F-8 and 7-6
                  CCPR4H = PRx_Temp_H
                  CCPR4L = PRx_Temp
              #ENDIF

              #ifdef AddHPWMCCPExit4
                AddHPWMCCPExit4
              #endif

          End if

      #ENDIF

    #endif

    #ifdef USE_HPWMCCP5 TRUE
PWMUPDATE_HARDWAREMODEPWMCCP5:

      #ifdef VAR(CCPR5L)

          if PWMChannel = 5 then  'in section USE_HPWMCCP5

              #ifdef AddHPWMCCPSetup5
                AddHPWMCCPSetup5
              #endif


              #ifndef BIT(CCP5FMT)
                  'ifndef BIT(CCP5FMT) Testing this bit is to identify the use of CCPR5L for PWM
                  PRx_Temp = PWMDuty * (PRx_Temp + 2)  'Correction
                  CCPR5L = PRx_Temp_H
                  If PWMDuty = 0 Then CCPR5L = 0  ' Assure OFF at Zero
              #ENDIF


              #ifdef BIT(CCP5FMT)
                  'ifdef BIT(CCP5FMT) Testing this bit is to identify the use of CCPR5H and CCPR5L for PWM
                  calculateDuty 'Sets PRx_Temp  to the duty value for bits 0x0F-8 and 7-6
                  CCPR5H = PRx_Temp_H
                  CCPR5L = PRx_Temp
              #ENDIF

              #ifdef AddHPWMCCPExit5
                AddHPWMCCPExit5
              #endif

          End if

      #ENDIF

    #endif


end sub


'''@Hidden
' Simply calculates the value of PRx_Temp from the Duty and PRx_Temp (destroying the value of PRx_Temp hence we need the variable PRx_Temp_Cache)
sub calculateDuty
    dim PRx_Temp as long
    dim PWMDuty as word
    dim PWMResolution as word

    'assumes PRx_Temp and PWMDuty are valid
    PRx_Temp = PWMDuty  * ( PRx_Temp + 1 )

    Set C off
    Repeat 2
      rotate PRx_Temp left
    End Repeat
    PRx_Temp = PRx_Temp / PWMResolution    'where PWMResolution is 255 or 1023

    Set C off
    Repeat 6
      rotate PRx_Temp left
    End Repeat

    'exits with PRx_Temp calculated
end sub

//~ Method to cater for users calling the HPWM_CCPSetDuty() with a byte when it should be word
//~ This method ensure that if a user calls HPWM_CCPSetDuty() with __CCPDutyVar as a byte they get 8bit control of the duty
Sub HPWM_CCPSetDuty ( In PWMChannel, in __CCPDutyVar as Byte )

    Select Case PWMChannel
        #ifdef USE_HPWMCCP1 TRUE and VAR(CCP1CON)
            Case 1:
                //~ Ensure duty value does not exceed maximum value
                If __CCPDutyVar > PWMCCP1MAXDUTYVALUE Then 
                  Exit Sub
                End If
                HPWM_CCPSetDuty ( PWMChannel, [WORD]Scale(__CCPDutyVar,0, 255,0,PWMCCP1MaximumDutyValue) )
        #endif
        #ifdef USE_HPWMCCP2 TRUE and VAR(CCP2CON)
            Case 2:
                //~ Ensure duty value does not exceed maximum value
                If __CCPDutyVar > PWMCCP2MAXDUTYVALUE Then 
                  Exit Sub
                End If
                HPWM_CCPSetDuty ( PWMChannel, [WORD]Scale(__CCPDutyVar,0, 255,0,PWMCCP2MaximumDutyValue) )                
        #endif
        #ifdef USE_HPWMCCP3 TRUE and VAR(CCP3CON)   
            Case 3:
                //~ Ensure duty value does not exceed maximum value
                If __CCPDutyVar > PWMCCP3MAXDUTYVALUE Then 
                  Exit Sub
                End If
                HPWM_CCPSetDuty ( PWMChannel, [WORD]Scale(__CCPDutyVar,0, 255,0,PWMCCP3MaximumDutyValue) )                
        #endif
        #ifdef USE_HPWMCCP4 TRUE and VAR(CCP4CON)   
            Case 4:
                //~ Ensure duty value does not exceed maximum value
                If __CCPDutyVar > PWMCCP4MAXDUTYVALUE Then 
                  Exit Sub
                End If
                HPWM_CCPSetDuty ( PWMChannel, [WORD]Scale(__CCPDutyVar,0, 255,0,PWMCCP4MaximumDutyValue) )
        #endif
        #ifdef USE_HPWMCCP5 TRUE and VAR(CCP5CON)                  
            Case 5:
                //~ Ensure duty value does not exceed maximum value
                If __CCPDutyVar > PWMCCP5MAXDUTYVALUE Then 
                  Exit Sub
                End If
                HPWM_CCPSetDuty ( PWMChannel, [WORD]Scale(__CCPDutyVar,0, 255,0,PWMCCP5MaximumDutyValue) )                                
        #endif
    End Select

End Sub

Sub HPWM_CCPSetDuty ( In PWMChannel, in __CCPDutyVar as Word )

    #ifdef USE_HPWMCCP1 TRUE
        #ifdef VAR(CCP1CON)
            'This is the legacy code to support only one CCCPWM channel
            DC1B1  = __CCPDutyVar.1
            DC1B0  = __CCPDutyVar.0
            //~ Shift value to right, twice, and use the byte value
            Set C off
            Rotate __CCPDutyVar Right
            Rotate __CCPDutyVar Right
            CCPR1L = [Byte]__CCPDutyVar
        #endif
    #endif

End Sub

Function HPWM_CCP_MAXDUTYVALUE (  Optional In PWMChannel as Byte = 1 ) as Word

  Select Case PWMChannel
    Case 1     
      HPWM_CCP_MAXDUTYVALUE =  PR2 + 1
      SET C OFF
      ROTATE  HPWM_CCP_MAXDUTYVALUE LEFT
      ROTATE  HPWM_CCP_MAXDUTYVALUE LEFT
                          
  End Select

end Function



//~  AVR SECTION


sub AVRHPWM (In AVRPWMChannel, In AVRPWMFreq, IN AVRPWMDuty)

Dim  ICRxTemp as Word   'ChipMHz <= 64

    'AVR fast pwm has a small spike on period register rollover
    If AVRPWMDuty = 0 Then
      AVRPWMOff (AVRPWMChannel)
      exit sub
    End If

    If AVRPWMFreq <> AVRPWMFreqOld Then

      ICRxTemp = ((chipMHZ*1000) / AVRPWMFreq) - 1
      PWMPrescale = 1
      If ICRxTemp > 255 Then
        ICRxTemp = ICRxTemp / 8
        PWMPrescale = 8
        If ICRxTemp > 255 Then
          ICRxTemp = ICRxTemp / 8
          PWMPrescale = 64
        End If
          If ICRxTemp > 255 Then
            'error msg(BadHPWMFreq)
          End If
      End If
      AVRPWMFreqOld = AVRPWMFreq

    End If

    AVRPWMDuty = (AVRPWMDuty * ICRxTemp) / 255
    'Set Timer prescale and period, Channel Duty Cycle,
    'and then enable channel
    #IFDEF AVRTC0
      AVRTimer = 0
      AVRSetPrescale (AVRTimer)
      #IFDEF AVRCHAN2   'OC0B
        IF  AVRPWMChannel = 2 Then
          #IFNDEF VAR(OCR0)
            #IFDEF VAR(OCR0AL)    'ICR0L
              'ATtiny10
              OCR0AL = ICRxTemp
              OCR0BL = AVRPWMDuty
            #ENDIF

            #IFNDEF VAR(OCR0AL)
              'OCR0A is used as the period register
              OCR0A = ICRxTemp
              OCR0B = AVRPWMDuty
            #ENDIF
          #ENDIF

          #IFDEF VAR(OCR0)   'OC2
            '8bit single channel devices
            'no period register like mega128
            'error msg(TMR2 HPWMNot Available)
          #ENDIF
          [canskip]COM0B1 = 1
          [canskip]COM0B0 = 0
        END IF
      #ENDIF
    #ENDIF

    #IFDEF AVRTC1
      AVRTimer = 1
      AVRSetPrescale (AVRTimer)
      #IFNDEF BIT(PWM1A)
        ICR1L = ICRxTemp
      #ENDIF

      #IFDEF BIT(PWM1A)
        #IFNDEF BIT(PWM11)
          OCR1C = ICRxTemp
        #ENDIF

        #IFDEF BIT(PWM11)
          ICR1L = ICRxTemp
        #ENDIF
      #ENDIF

      #IFDEF AVRCHAN3   'OC1A
        IF  AVRPWMChannel = 3 Then
          #IFDEF BIT(PWM1A)
            #IFNDEF BIT(PWM11)
              'ATtiny25 and ATtiny261A series
              OCR1A = AVRPWMDuty
            #ENDIF

            #IFDEF BIT(PWM11)
              'ATmega103 16bit register
              OCR1AL = AVRPWMDuty
            #ENDIF
          #ENDIF

          #IFNDEF BIT(PWM1A)
            '16bit register
            OCR1AL = AVRPWMDuty
          #ENDIF
          Set COM1A1 On
          Set COM1A0 Off
        END IF
      #ENDIF

      #IFDEF AVRCHAN4   'OC1B
        IF  AVRPWMChannel = 4 Then
          #IFDEF BIT(PWM1B)
            #IFNDEF BIT(PWM11)
              'ATtiny25 and ATtiny261A series
              OCR1B = AVRPWMDuty
            #ENDIF

            #IFDEF BIT(PWM11)
              'ATmega103 16bit register
              'no period register
              OCR1BL = AVRPWMDuty
            #ENDIF
          #ENDIF

          #IFNDEF BIT(PWM1B)
            #IFDEF VAR(ICR1H)
              OCR1BL = AVRPWMDuty
            #ENDIF
          #ENDIF
          Set COM1B1 ON
          Set COM1B0 OFF
        END IF
      #ENDIF

      #IFDEF AVRCHAN5   'OC1C larger megas and OC1D larger/newer tinys
        If AVRPWMChannel = 5 Then
          #IFDEF BIT(PWM1D)
            'ATtiny261A series
            OCR1D = AVRPWMDuty
            Set COM1D1 ON
            Set COM1D0 OFF
          #ENDIF

          #IFNDEF BIT(PWM1D)
            OCR1CL = AVRPWMDuty
            Set COM1C1 ON
            Set COM1C0 OFF
          #ENDIF
        End If
      #ENDIF
    #ENDIF

    #IFDEF AVRTC2
      AVRTimer = 2
      AVRSetPrescale (AVRTimer)
      #IFDEF AVRCHAN7   'OC2B
        IF  AVRPWMChannel = 7 Then
          #IFNDEF OCR2
          'OCR2A is used as the period register
            OCR2A = ICRxTemp
            OCR2B = AVRPWMDuty
            Set COM2B1 ON
            Set COM2B0 OFF
          #ENDIF

          #IFDEF OCR2   'OC2
            '8bit single channel devices
            'no period register like mega128
            'error msg(TMR2 HPWMNot Available)
          #ENDIF
        END IF
      #ENDIF
    #ENDIF

    #IFDEF AVRTC3
      AVRTimer = 3
      AVRSetPrescale (AVRTimer)
      ICR3L = ICRxTemp
      #IFDEF AVRCHAN8   'OC3A
        IF  AVRPWMChannel = 8 Then
          OCR3AL = AVRPWMDuty
          Set COM3A1 ON
          Set COM3A0 OFF
        END IF
      #ENDIF

      #IFDEF AVRCHAN9   'OC3B
        IF  AVRPWMChannel = 9 Then
          OCR3BL = AVRPWMDuty
          Set COM3B1 ON
          Set COM3B0 OFF
        END IF
      #ENDIF

      #IFDEF AVRCHAN10    'OC3C
        IF  AVRPWMChannel = 10 Then
          OCR3CL = AVRPWMDuty
          Set COM3C1 ON
          Set COM3C0 OFF
        END IF
      #ENDIF
    #ENDIF

    #IFDEF AVRTC4
      AVRTimer = 4
      AVRSetPrescale (AVRTimer)
      ICR4L = ICRxTemp
      #IFDEF AVRCHAN11   'OC4A
        IF  AVRPWMChannel = 11 Then
          OCR4AL = AVRPWMDuty
          Set COM4A1 ON
          Set COM4A0 OFF
        END IF
      #ENDIF

      #IFDEF AVRCHAN12   'OC4B
        IF  AVRPWMChannel = 12 Then
          OCR4BL = AVRPWMDuty
          Set COM4B1 ON
          Set COM4B0 OFF
        END IF
      #ENDIF

      #IFDEF AVRCHAN13    'OC4C
        IF  AVRPWMChannel = 13 Then
          OCR4CL = AVRPWMDuty
          Set COM4C1 ON
          Set COM4C0 OFF
        END IF
      #ENDIF
    #ENDIF

    #IFDEF AVRTC5
      AVRTimer = 5
      AVRSetPrescale (AVRTimer)
      ICR5L = ICRxTemp
      #IFDEF AVRCHAN14   'OC5A
        IF  AVRPWMChannel = 14 Then
          OCR5AL = AVRPWMDuty
          Set COM5A1 ON
          Set COM5A0 OFF
        END IF
      #ENDIF

      #IFDEF AVRCHAN15   'OC5B
        IF  AVRPWMChannel = 15 Then
          OCR5BL = AVRPWMDuty
          Set COM5B1 ON
          Set COM5B0 OFF
        END IF
      #ENDIF

      #IFDEF AVRCHAN16    'OC5C
        IF  AVRPWMChannel = 16 Then
          OCR5CL = AVRPWMDuty
          Set COM5C1 ON
          Set COM5C0 OFF
        END IF
      #ENDIF
    #ENDIF

end sub


sub AVRSetHPWMMode

  #IF DEF(CHIPAVRDX)
    Dim TCA0_SINGLE_COMPARE0 as Word Alias TCA0_SINGLE_CMP0H, TCA0_SINGLE_CMP0L
    Dim TCA0_SINGLE_COMPARE1 as Word Alias TCA0_SINGLE_CMP1H, TCA0_SINGLE_CMP1L
    Dim TCA0_SINGLE_COMPARE2 as Word Alias TCA0_SINGLE_CMP2H, TCA0_SINGLE_CMP2L

    //~ CMP2EN disabled; CMP1EN disabled; CMP0EN enabled; ALUPD disabled; WGMODE SINGLESLOPE;
    TCA0_SINGLE_CTRLB = TCA0_SINGLE_CTRLB | TCA_SINGLE_WGMODE_SINGLESLOPE_gc
    TCA0_SINGLE_CTRLB.TCA_SINGLE_CMP0EN_bp = 1
    TCA0_SINGLE_CTRLB.TCA_SINGLE_ALUPD_bp = 0

    //~ Period
    Dim TCA0_SINGLE_PERIOD as Word ALIAS TCA0_SINGLE_PERH, TCA0_SINGLE_PERL
    TCA0_SINGLE_PERIOD = SCRIPT_PWM_PERIOD

    //~ Compare 0
    TCA0_SINGLE_COMPARE0 = SCRIPT_PWM_Duty
    //~ TCA0 pins on PA[5:0]
    PORTMUX_TCAROUTEA = 0x00

    //~ CLKSEL by SCRIPT constant; DISABLED;
    TCA0_SINGLE_CTRLA =SCRIPT_PWMPRESCALER
    TCA0_SINGLE_CTRLA.TCA_SINGLE_ENABLE_bp = 0

  #ENDIF

  //~Legacy AVR
    #IF NODEF(CHIPAVRDX)
      //~ AVR HPWM makes use of the Fast PWM mode by setting WGMnx bits
      //~ TC1,TC3,TC4,TC5 can have OCR1C and ICRxL as TOP
      //~ TC0 and TC2 have OCR0A and OCR2A as TOP
      #IFDEF AVRTC0
        #IFDEF BIT(WGM02)
          #IFDEF VAR(OCR0)
            //~ 8bit single channel devices
            //~ no period register like mega128
            //error msg(TMR0 HPWMNot Available)
          #ENDIF

          #IFNDEF VAR(OCR0)
            #IFNDEF VAR(OCR0AL)
              '8bit 2 channel devices
              asm ShowDebug Canskip handler addded for TCCR0B and TCCR0A
              [canskip]TCCR0B = b'00001000'
              [canskip]TCCR0A = b'00000011'
            #ENDIF

            #IFDEF VAR(OCR0AL)
              'ATtiny10
              '16bit 2 channel devices
              [canskip]TCCR0B = b'00011000'
              [canskip]TCCR0A = b'00000011'
            #ENDIF
          #ENDIF
        #ENDIF

        #IFNDEF BIT(WGM02)
          //~no period register like ATtiny261A series
          //error msg(TMR0 HPWMNot Available)
        #ENDIF
      #ENDIF

      #IFDEF AVRTC1
        #IFDEF BIT(PWM1A)
          #IFDEF BIT(PWM11)
            'ATmega103 8bit mode
            SET PWM11 OFF
            SET PWM10 ON
          #ENDIF

          'ATtiny25, ATtiny261A  series
          #IFDEF AVRCHAN3
            SET PWM1A ON
          #ENDIF

          #IFDEF AVRCHAN4
            SET PWM1B ON
          #ENDIF

          'ATtiny261A  series
          #IFDEF PWM1D
            SET WGM11 ON
            SET WGM10 OFF
          #ENDIF

          #IFDEF AVRCHAN5
            SET PWM1D ON
          #ENDIF

        #ENDIF

        #IFNDEF BIT(PWM1A)
          '16 bit, 16 mode devices like ATmega328p series
          'Fast PWM
          TCCR1B = b'00011000'
          TCCR1A = b'00000010'
        #ENDIF
      #ENDIF

      #IFDEF AVRTC2
        #IFDEF BIT(WGM22)
          #IFDEF OCR2
            '8bit single channel devices
            'no period register like mega128
            'error msg(TMR2 HPWMNot Available)
          #ENDIF

          #IFNDEF OCR2
            #IFNDEF ICR2L
              '8bit 2 channel devices
              TCCR2B = b'00001000'
              TCCR2A = b'00000011'
            #ENDIF

            #IFDEF ICR2L
              'ATtiny102
              '16bit 2 channel devices
              TCCR2B = b'00011000'
              TCCR2A = b'00000010'
            #ENDIF
          #ENDIF
        #ENDIF

        #IFNDEF BIT(WGM22)
          'no period register like ATtiny261A series
          'error msg(TMR2 HPWMNot Available)
        #ENDIF
      #ENDIF

      #IFDEF AVRTC3
        TCCR3B = b'00011000'
        TCCR3A = b'00000010'
      #ENDIF

      #IFDEF AVRTC4
        TCCR4B = b'00011000'
        TCCR4A = b'00000010'
      #ENDIF

      #IFDEF AVRTC5
        TCCR5B = b'00011000'
        TCCR5A = b'00000010'
      #ENDIF
    #ENDIF
end sub

sub AVRSetPrescale (AVRTimer)

  'AVR prescale applied to timers
  If PWMPrescale = 1 Then
    #IFDEF AVRTC0
      If AVRTimer = 0 Then
        #ifdef var(TCCR0B)  'not all chips have this register
          SysValueCopy = TCCR0B
          andi SysValueCopy, 0xf8
          inc SysValueCopy
          TCCR0B = SysValueCopy
        #endif
      End If
    #ENDIF

    #IFDEF AVRTC1
      If AVRTimer = 1 Then
        #IFDEF BIT(PWM1A)
          #IFNDEF (PWM11)
            lds SysValueCopy,TCCR1
            andi SysValueCopy, 0xf0
            inc SysValueCopy
            sts TCCR1, SysValueCopy
          #ENDIF

          #IFDEF (PWM11)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf8
            inc SysValueCopy
            sts TCCR1B, SysValueCopy
          #ENDIF
        #ENDIF

        #IFNDEF BIT(PWM1A)
          #IFNDEF BIT(CS13)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf8
            inc SysValueCopy
            sts TCCR1B, SysValueCopy
          #ENDIF

          #IFDEF BIT(CS13)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf0
            inc SysValueCopy
            sts TCCR1B, SysValueCopy
          #ENDIF
        #ENDIF
      End If
    #ENDIF

    #IFDEF AVRTC2
      If AVRTimer = 2 Then
        lds SysValueCopy,TCCR2B
        andi SysValueCopy, 0xf8
        inc SysValueCopy
        sts TCCR2B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC3
      If AVRTimer = 3 Then
        lds SysValueCopy,TCCR3B
        andi SysValueCopy, 0xf8
        inc SysValueCopy
        sts TCCR3B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC4
      If AVRTimer = 4 Then
        lds SysValueCopy,TCCR4B
        andi SysValueCopy, 0xf8
        inc SysValueCopy
        sts TCCR4B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC5
      If AVRTimer = 5 Then
        lds SysValueCopy,TCCR5B
        andi SysValueCopy, 0xf8
        inc SysValueCopy
        sts TCCR5B, SysValueCopy
      End If
    #ENDIF
  End If

  If PWMPrescale = 8 Then
    #IFDEF AVRTC0
      If AVRTimer = 0 Then
        #ifdef var(TCCR0B)  'not all chips have this register
          SysValueCopy = TCCR0B
          andi SysValueCopy, 0xf8
          ori SysValueCopy, 0x02
          TCCR0B = SysValueCopy
        #endif
      End If
    #ENDIF

    #IFDEF AVRTC1
      If AVRTimer = 1 Then
        #IFDEF BIT(PWM1A)
          #IFNDEF (PWM11)
            lds SysValueCopy,TCCR1
            andi SysValueCopy, 0xf0
            ori SysValueCopy, 0x04
            sts TCCR1, SysValueCopy
          #ENDIF

          #IFDEF (PWM11)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf0
            ori SysValueCopy, 0x04
            sts TCCR1B, SysValueCopy
          #ENDIF
        #ENDIF

        #IFNDEF BIT(PWM1A)
          #IFNDEF BIT(CS13)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf8
            ori SysValueCopy, 0x02
            sts TCCR1B, SysValueCopy
          #ENDIF

          #IFDEF BIT(CS13)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf0
            ori SysValueCopy, 0x04
            sts TCCR1B, SysValueCopy
          #ENDIF
        #ENDIF
      End If
    #ENDIF

    #IFDEF AVRTC2
      If AVRTimer = 2 Then
        lds SysValueCopy,TCCR2B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x02
        sts TCCR2B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC3
      If AVRTimer = 3 Then
        lds SysValueCopy,TCCR3B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x02
        sts TCCR3B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC4
      If AVRTimer = 4 Then
        lds SysValueCopy,TCCR4B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x02
        sts TCCR4B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC5
      If AVRTimer = 5 Then
        lds SysValueCopy,TCCR5B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x02
        sts TCCR5B, SysValueCopy
      End If
    #ENDIF
  End If

  If PWMPrescale = 64 Then
    #IFDEF AVRTC0
      If AVRTimer = 0 Then
        #ifdef var(TCCR0B)  'not all chips have this register
          SysValueCopy = TCCR0B
          andi SysValueCopy, 0xf8
          ori SysValueCopy, 0x03
          TCCR0B = SysValueCopy
        #endif
      End If
    #ENDIF

    #IFDEF AVRTC1
      If AVRTimer = 1 Then
        #IFDEF BIT(PWM1A)
          #IFNDEF (PWM11)
            lds SysValueCopy,TCCR1
            andi SysValueCopy, 0xf0
            ori SysValueCopy, 0x07
            sts TCCR1, SysValueCopy
          #ENDIF

          #IFDEF (PWM11)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf0
            ori SysValueCopy, 0x07
            sts TCCR1B, SysValueCopy
          #ENDIF
        #ENDIF

        #IFNDEF BIT(PWM1A)
          #IFNDEF BIT(CS13)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf8
            ori SysValueCopy, 0x03
            sts TCCR1B, SysValueCopy
          #ENDIF

          #IFDEF BIT(CS13)
            lds SysValueCopy,TCCR1B
            andi SysValueCopy, 0xf0
            ori SysValueCopy, 0x07
            sts TCCR1B, SysValueCopy
          #ENDIF
        #ENDIF
      End If
    #ENDIF

    #IFDEF AVRTC2
      If AVRTimer = 2 Then
        lds SysValueCopy,TCCR2B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x04
        sts TCCR2B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC3
      If AVRTimer = 3 Then
        lds SysValueCopy,TCCR3B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x03
        sts TCCR3B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC4
      If AVRTimer = 4 Then
        lds SysValueCopy,TCCR4B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x03
        sts TCCR4B, SysValueCopy
      End If
    #ENDIF

    #IFDEF AVRTC5
      If AVRTimer = 5 Then
        lds SysValueCopy,TCCR5B
        andi SysValueCopy, 0xf8
        ori SysValueCopy, 0x03
        sts TCCR5B, SysValueCopy
      End If
    #ENDIF
  End If

end sub


sub AVRPWMOn

  #IF DEF(CHIPAVRDX)
    //~ Enable single channel PWM
    TCA0_SINGLE_CTRLB.TCA_SINGLE_CMP0EN_bp = 1
    TCA0_SINGLE_CTRLA.TCA_SINGLE_ENABLE_bp = 1
  #ENDIF

  #IF NODEF(CHIPAVRDX)
    [canskip]COM0B1 = 1
    [canskip]COM0B0 = 0
  #ENDIF
  
end sub


sub AVRPWMOff


  #IF DEF(CHIPAVRDX)
    //~ Disalble single channel PWM
    TCA0_SINGLE_CTRLB.TCA_SINGLE_CMP0EN_bp = 0
    TCA0_SINGLE_CTRLA.TCA_SINGLE_ENABLE_bp = 0
  #ENDIF

  #IF NODEF(CHIPAVRDX)
    [canskip]COM0B1 = 0
    [canskip]COM0B0 = 0
  #ENDIF

end sub


SUB AVRPWMOff (IN AVRPWMChannel)

  'channels 1 and 6 not available, see Help
  #IFDEF AVRCHAN2
    If AVRPWMChannel = 2 Then
      [canskip]COM0B1 = 0
      [canskip]COM0B0 = 0
    End If
  #ENDIF

  #IFDEF AVRCHAN3
    If AVRPWMChannel = 3 Then
      Set COM1A1 OFF
      Set COM1A0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN4
    If AVRPWMChannel = 4 Then
      Set COM1B1 OFF
      Set COM1B0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN5
    If AVRPWMChannel = 5 Then
      #IFDEF BIT(PWM1D)
        'ATtiny261A series
        Set COM1D1 OFF
        Set COM1D0 OFF
      #ENDIF

      #IFNDEF BIT(PWM1D)
        Set COM1C1 OFF
        Set COM1C0 OFF
      #ENDIF
    End If
  #ENDIF

  #IFDEF AVRCHAN7
    If AVRPWMChannel = 7 Then
      Set COM2B1 OFF
      Set COM2B0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN8
    If AVRPWMChannel = 8 Then
        Set COM3A1 OFF
        Set COM3A0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN9
    If AVRPWMChannel = 9 Then
        Set COM3B1 OFF
        Set COM3B0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN10
    If AVRPWMChannel = 10 Then
        Set COM3C1 OFF
        Set COM3C0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN11
    If AVRPWMChannel = 11 Then
        Set COM4A1 OFF
        Set COM4A0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN12
    If AVRPWMChannel = 12 Then
        Set COM4B1 OFF
        Set COM4B0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN13
    If AVRPWMChannel = 13 Then
        Set COM4C1 OFF
        Set COM4C0 OFF
    End If
  #ENDIF


  #IFDEF AVRCHAN14
    If AVRPWMChannel = 14 Then
        Set COM5A1 OFF
        Set COM5A0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN15
    If AVRPWMChannel = 15 Then
        Set COM5B1 OFF
        Set COM5B0 OFF
    End If
  #ENDIF

  #IFDEF AVRCHAN16
    If AVRPWMChannel = 16 Then
        Set COM5C1 OFF
        Set COM5C0 OFF
    End If
  #ENDIF

END SUB


'This is the setup section for fixed mode PWM

    'The only options are PWM_Timer2_Freq nn|PWM_Timer4_Freq nn|PWM_Timer6_Freq nn. These are the PWM timers
    'The PWM_yy_Duty xx' where yy is between 1 and 9 and is a valid PWM module, and, xx is the Duty cycle for specific channels
    'The PWM_zz_Clock_Source tt.  Where zz is channel and tt is the PWM clock source.
    'You do not need to define all the timers and channels, just define the constants you need.
    'The minimum is
    '   A timer with a frequency
    '   A PWM channel with a duty
    '   A PWM channel clock source
    '   For PWM channel 2 with a frequency of 38Khz with a duty of 50% with a clock source of timer 2, use
    '     #define PWM_Timer2_Freq 38
    '     #define PWM_7_Duty 50
    '     #define PWM_7_Clock_Source 2

'    #define PWM_Timer2_Freq 20        'Set frequency in KHz, just change the number
'    #define PWM_Timer4_Freq 40        'Set frequency in KHz, just change the number
'    #define PWM_Timer6_Freq 60        'Set frequency in KHz, just change the number

'    #define PWM_1_Duty 10            'Set duty cycle as percentage 0-100%, just change the number
'    #define PWM_1_Clock_Source 2
'
'    #define PWM_2_Duty 20
'    #define PWM_2_Clock_Source 4
'
'    #define PWM_3_Duty 30
'    #define PWM_3_Clock_Source 6
'
'    #define PWM_4_Duty 40
'    #define PWM_4_Clock_Source 2
'
'    #define PWM_5_Duty 50
'    #define PWM_5_Clock_Source 4
'
'    #define PWM_6_Duty 60
'    #define PWM_6_Clock_Source 6
'
'    #define PWM_7_Duty 70
'    #define PWM_7_Clock_Source 2
'
'    #define PWM_8_Duty 80
'    #define PWM_8_Clock_Source 4
'
'    #define PWM_9_Duty 90
'    #define PWM_9_Clock_Source 6


#define HPWMxCon_Default On   'This will start the PWM channels as on.
'#define HPWMxCon_Default off   'Disable PWM out by default
'    wait 2 s
'    HPWMOn ( 7, PWMModule )
'    wait 2 s
'    HPWMOff ( 7, PWMModule)
'    wait 2 s
'    HPWMOn ( 7, PWMModule )
