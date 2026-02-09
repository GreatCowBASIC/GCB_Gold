'    16 bit pulse Width Modulation routines for GCBASIC
'    Copyright (C) 2017-2026  Peter Everett


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

' 28/08/17   Initial version
' 09/09/17   Error in claculation of log2 fixed.
'            Potential overflow error fixed in calculation on HPWM16_PRPS_Req
'            Supports PWMChannels 1-12, with #ifdefs to allow code to be disabled
'            Prescalar is no longer calculated, but can be user specified for sub 1 kHz PWM
' 24/09/17   PWMxPHH set to zero (undefined at power on, messes up DC calculation)
'            PWMxxLDCON.LDA changed to PWMxxLD
'            Tweak to Log2 function to save some program memory
'            All variable names prefixed with HPWM16_ (except those reused from PWM functions)
' 02/10/17   Added HPWM16On and HPWM16Off commands for fixed frequency/duty PWM
' 05/11/17   Revised to handle fixed mode operations, changed constants to map to PWM.h
'            revised script hanlder, new init and code validation
' 11/11/17   Fixed problem with calculation of duty cycle
' 26/01/19   Overflow/calculation errors fixed when working out duty cycle registers
' 14/02/21   Added CHIPPWM16TYPE validation
' 15/20/21   Removed PWM16 script warning
' 14/08/22 Updated user changeable constants only - no functional change

#option REQUIRED PIC CHIPPWM16TYPE "PWM16 commands. Either chip is not supported, or, not validated with this chip"


'By default, compile code for all PWM modeules
'Set to false in user code to save on program space if module is not required


'    #define HPWM16_1_Duty 10            'Set duty cycle as percentage 0-100%, just change the number
'    #define HPWM16_1_Freq 10
'
'    #define HPWM16_2_Duty 20
'    #define HPWM16_2_Freq 20
'
'    #define HPWM16_3_Duty 30
'    #define HPWM16_3_Freq 30
'
'    #define HPWM16_4_Duty 40
'    #define HPWM16_4_Freq 40
'
'    #define HPWM16_5_Duty 50
'    #define HPWM16_5_Freq 50
'
'    #define HPWM16_6_Duty 60
'    #define HPWM16_6_Freq 60
'
'    #define HPWM16_7_Duty 70
'    #define HPWM16_7_Freq 70
'
'    #define HPWM16_8_Duty 80
'    #define HPWM16_8_Freq 80
'
'    #define HPWM16_9_Duty 90
'    #define HPWM16_9_Freq 90
'
'    #define HPWM16_10_Duty 10
'    #define HPWM16_10_Freq 100
'
'    #define HPWM16_11_Duty 20
'    #define HPWM16_11_Freq 110
'
'    #define HPWM16_12_Duty 40
'    #define HPWM16_12_Freq 120


 'Create some alias, used by Phase and Offset
      #ifdef var(PWM1DCH)
      DIM HPWM16_1_Duty_RegWord alias PWM1DCH,PWM1DCL As Word
      #endif
      #ifdef var(PWM2DCH)
      DIM HPWM16_2_Duty_RegWord alias PWM2DCH,PWM2DCL As Word
      #endif
      #ifdef var(PWM3DCH)
      DIM HPWM16_3_Duty_RegWord alias PWM3DCH,PWM3DCL As Word
      #endif
      #ifdef var(PWM4DCH)
      DIM HPWM16_4_Duty_RegWord alias PWM4DCH,PWM4DCL As Word
      #endif
      #ifdef var(PWM5DCH)
      DIM HPWM16_5_Duty_RegWord alias PWM5DCH,PWM5DCL As Word
      #endif
      #ifdef var(PWM6DCH)
      DIM HPWM16_6_Duty_RegWord alias PWM6DCH,PWM6DCL As Word
      #endif
      #ifdef var(PWM7DCH)
      DIM HPWM16_7_Duty_RegWord alias PWM7DCH,PWM7DCL As Word
      #endif
      #ifdef var(PWM8DCH)
      DIM HPWM16_8_Duty_RegWord alias PWM8DCH,PWM8DCL As Word
      #endif
      #ifdef var(PWM9DCH)
      DIM HPWM16_9_Duty_RegWord alias PWM9DCH,PWM9DCL As Word
      #endif
      #ifdef var(PWM10DCH)
      DIM HPWM16_10_Duty_RegWord alias PWM10DCH,PWM10DCL As Word
      #endif
      #ifdef var(PWM11DCH)
      DIM HPWM16_11_Duty_RegWord alias PWM11DCH,PWM11DCL As Word
      #endif
      #ifdef var(PWM12DCH)
      DIM HPWM16_12_Duty_RegWord alias PWM12DCH,PWM12DCL As Word
      #endif

      #ifdef var(PWM1PRH)
      DIM HPWM16_1_Period_RegWord alias PWM1PRH,PWM1PRL As Word
      #endif
      #ifdef var(PWM2PRH)
      DIM HPWM16_2_Period_RegWord alias PWM2PRH,PWM2PRL As Word
      #endif
      #ifdef var(PWM3PRH)
      DIM HPWM16_3_Period_RegWord alias PWM3PRH,PWM3PRL As Word
      #endif
      #ifdef var(PWM4PRH)
      DIM HPWM16_4_Period_RegWord alias PWM4PRH,PWM4PRL As Word
      #endif
      #ifdef var(PWM5PRH)
      DIM HPWM16_5_Period_RegWord alias PWM5PRH,PWM5PRL As Word
      #endif
      #ifdef var(PWM6PRH)
      DIM HPWM16_6_Period_RegWord alias PWM6PRH,PWM6PRL As Word
      #endif
      #ifdef var(PWM7PRH)
      DIM HPWM16_7_Period_RegWord alias PWM7PRH,PWM7PRL As Word
      #endif
      #ifdef var(PWM8PRH)
      DIM HPWM16_8_Period_RegWord alias PWM8PRH,PWM8PRL As Word
      #endif
      #ifdef var(PWM9PRH)
      DIM HPWM16_9_Period_RegWord alias PWM9PRH,PWM9PRL As Word
      #endif
      #ifdef var(PWM10PRH)
      DIM HPWM1610__Period_RegWord alias PWM10PRH,PWM10PRL As Word
      #endif
      #ifdef var(PWM11PRH)
      DIM HPWM16_11_Period_RegWord alias PWM11PRH,PWM11PRL As Word
      #endif
      #ifdef var(PWM12PRH)
      DIM HPWM16_12_Period_RegWord alias PWM12PRH,PWM12PRL As Word
      #endif

      #ifdef var(PWM1PHH)
      DIM HPWM16_1_Phase_RegWord alias PWM1PHH,PWM1PHL As Word
      #endif
      #ifdef var(PWM2PHH)
      DIM HPWM16_2_Phase_RegWord alias PWM2PHH,PWM2PHL As Word
      #endif
      #ifdef var(PWM2PHH)
      DIM HPWM16_3_Phase_RegWord alias PWM3PHH,PWM3PHL As Word
      #endif
      #ifdef var(PWM4PHH)
      DIM HPWM16_4_Phase_RegWord alias PWM4PHH,PWM4PHL As Word
      #endif
      #ifdef var(PWM5PHH)
      DIM HPWM16_5_Phase_RegWord alias PWM5PHH,PWM5PHL As Word
      #endif
      #ifdef var(PWM6PHH)
      DIM HPWM16_6_Phase_RegWord alias PWM6PHH,PWM6PHL As Word
      #endif
      #ifdef var(PWM7PHH)
      DIM HPWM16_7_Phase_RegWord alias PWM7PHH,PWM7PHL As Word
      #endif
      #ifdef var(PWM8PHH)
      DIM HPWM16_8_Phase_RegWord alias PWM8PHH,PWM8PHL As Word
      #endif
      #ifdef var(PWM9PHH)
      DIM HPWM16_9_Phase_RegWord alias PWM9PHH,PWM9PHL As Word
      #endif
      #ifdef var(PWM10PHH)
      DIM HPWM16_10_Phase_RegWord alias PWM10PHH,PWM10PHL As Word
      #endif
      #ifdef var(PWM11PHH)
      DIM HPWM16_11_Phase_RegWord alias PWM11PHH,PWM11PHL As Word
      #endif
      #ifdef var(PWM12PHH)
      DIM HPWM16_12_Phase_RegWord alias PWM12PHH,PWM12PHL As Word
      #endif

#SCRIPT

  IF NODEF(USE_HPWM16_1) THEN
    USE_HPWM16_1=TRUE
  END IF
  IF NODEF(USE_HPWM16_2) THEN
    USE_HPWM16_2=TRUE
  END IF
  IF NODEF(USE_HPWM16_3) THEN
    USE_HPWM16_3=TRUE
  END IF
  IF NODEF(USE_HPWM16_4) THEN
    USE_HPWM16_4=TRUE
  END IF
  IF NODEF(USE_HPWM16_5) THEN
    USE_HPWM16_5=TRUE
  END IF
  IF NODEF(USE_HPWM16_6) THEN
    USE_HPWM16_6=TRUE
  END IF
  IF NODEF(USE_HPWM16_7) THEN
    USE_HPWM16_7=TRUE
  END IF
  IF NODEF(USE_HPWM16_8) THEN
    USE_HPWM16_8=TRUE
  END IF
  IF NODEF(USE_HPWM16_9) THEN
    USE_HPWM16_9=TRUE
  END IF
  IF NODEF(USE_HPWM16_10) THEN
    USE_HPWM16_10=TRUE
  END IF
  IF NODEF(USE_HPWM16_11) THEN
    USE_HPWM16_11=TRUE
  END IF
  IF NODEF(USE_HPWM16_12) THEN
    USE_HPWM16_12=TRUE
  END IF
  IF NODEF(PWM_FREQ) THEN
    PWM_FREQ=38
  END IF
  IF NODEF(PWM_DUTY) THEN
    PWM_DUTY=50
  END IF

#ENDSCRIPT

#startup HPWM16Init

sub HPWM16Init

  #ifdef PIC

      'Calculate the constants
      #script

          if PIC then
            'Calculate the frequency the defaults
            HPWM16F_PRPS_Req = ChipMHz * 1000 / PWM_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = PWM_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16F_DCH_Script=0
            HPWM16F_DCL_Script=0
            HPWM16F_PRH_Script=0
            HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16F_DCH_Script * 256))
            HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16F_PRH_Script * 256))

'            'Channel xx
'            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_xx_Freq
'            'Calculate the duty cycle.
'            'Simpler than main code because the compiler does the calculation using FP math
'            HPWM16F_DCy = HPWM16_xx_Duty / 100 * HPWM16F_PRPS_Req
'            HPWM16_xx_HPWM16F_DCH_Script=0
'            HPWM16_xx_HPWM16F_DCL_Script=0
'            HPWM16_xx_HPWM16F_PRH_Script=0
'            HPWM16_xx_HPWM16F_PRL_Script=0
'
'            'Output the values for use in the registers
'            HPWM16_xx_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
'            HPWM16_xx_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_xx_HPWM16F_DCH_Script * 256))
'            HPWM16_xx_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
'            HPWM16_xx_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_xx_HPWM16F_PRH_Script * 256))

            'Channel 1
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_1_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_1_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_1_HPWM16F_DCH_Script=0
            HPWM16_1_HPWM16F_DCL_Script=0
            HPWM16_1_HPWM16F_PRH_Script=0
            HPWM16_1_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_1_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_1_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_1_HPWM16F_DCH_Script * 256))
            HPWM16_1_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_1_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_1_HPWM16F_PRH_Script * 256))
            if HPWM16_1_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_1_HPWM16F_PRH_Script
            end if

            'Channel 2
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_2_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_2_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_2_HPWM16F_DCH_Script=0
            HPWM16_2_HPWM16F_DCL_Script=0
            HPWM16_2_HPWM16F_PRH_Script=0
            HPWM16_2_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_2_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_2_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_2_HPWM16F_DCH_Script * 256))
            HPWM16_2_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_2_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_2_HPWM16F_PRH_Script * 256))
            if HPWM16_2_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_2_HPWM16F_PRH_Script
            end if


            'Channel 3
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_3_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_3_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_3_HPWM16F_DCH_Script=0
            HPWM16_3_HPWM16F_DCL_Script=0
            HPWM16_3_HPWM16F_PRH_Script=0
            HPWM16_3_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_3_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_3_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_3_HPWM16F_DCH_Script * 256))
            HPWM16_3_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_3_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_3_HPWM16F_PRH_Script * 256))
            if HPWM16_3_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_3_HPWM16F_PRH_Script
            end if

            'Channel 4
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_4_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_4_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_4_HPWM16F_DCH_Script=0
            HPWM16_4_HPWM16F_DCL_Script=0
            HPWM16_4_HPWM16F_PRH_Script=0
            HPWM16_4_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_4_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_4_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_4_HPWM16F_DCH_Script * 256))
            HPWM16_4_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_4_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_4_HPWM16F_PRH_Script * 256))
            if HPWM16_4_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_4_HPWM16F_PRH_Script
            end if

            'Channel 5
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_5_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_5_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_5_HPWM16F_DCH_Script=0
            HPWM16_5_HPWM16F_DCL_Script=0
            HPWM16_5_HPWM16F_PRH_Script=0
            HPWM16_5_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_5_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_5_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_5_HPWM16F_DCH_Script * 256))
            HPWM16_5_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_5_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_5_HPWM16F_PRH_Script * 256))
            if HPWM16_5_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_5_HPWM16F_PRH_Script
            end if

            'Channel 6
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_6_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_6_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_6_HPWM16F_DCH_Script=0
            HPWM16_6_HPWM16F_DCL_Script=0
            HPWM16_6_HPWM16F_PRH_Script=0
            HPWM16_6_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_6_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_6_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_6_HPWM16F_DCH_Script * 256))
            HPWM16_6_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_6_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_6_HPWM16F_PRH_Script * 256))
            if HPWM16_6_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_6_HPWM16F_PRH_Script
            end if

            'Channel 7
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_7_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_7_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_7_HPWM16F_DCH_Script=0
            HPWM16_7_HPWM16F_DCL_Script=0
            HPWM16_7_HPWM16F_PRH_Script=0
            HPWM16_7_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_7_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_7_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_7_HPWM16F_DCH_Script * 256))
            HPWM16_7_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_7_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_7_HPWM16F_PRH_Script * 256))
            if HPWM16_7_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_7_HPWM16F_PRH_Script
            end if

            'Channel 8
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_8_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_8_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_8_HPWM16F_DCH_Script=0
            HPWM16_8_HPWM16F_DCL_Script=0
            HPWM16_8_HPWM16F_PRH_Script=0
            HPWM16_8_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_8_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_8_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_8_HPWM16F_DCH_Script * 256))
            HPWM16_8_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_8_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_8_HPWM16F_PRH_Script * 256))
            if HPWM16_8_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_8_HPWM16F_PRH_Script
            end if

            'Channel 9
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_9_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_9_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_9_HPWM16F_DCH_Script=0
            HPWM16_9_HPWM16F_DCL_Script=0
            HPWM16_9_HPWM16F_PRH_Script=0
            HPWM16_9_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_9_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_9_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_9_HPWM16F_DCH_Script * 256))
            HPWM16_9_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_9_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_9_HPWM16F_PRH_Script * 256))
            if HPWM16_9_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_9_HPWM16F_PRH_Script
            end if

            'Channel 10
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_10_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_10_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_10_HPWM16F_DCH_Script=0
            HPWM16_10_HPWM16F_DCL_Script=0
            HPWM16_10_HPWM16F_PRH_Script=0
            HPWM16_10_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_10_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_10_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_10_HPWM16F_DCH_Script * 256))
            HPWM16_10_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_10_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_10_HPWM16F_PRH_Script * 256))
            if HPWM16_10_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_10_HPWM16F_PRH_Script
            end if

            'Channel 11
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_11_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_11_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_11_HPWM16F_DCH_Script=0
            HPWM16_11_HPWM16F_DCL_Script=0
            HPWM16_11_HPWM16F_PRH_Script=0
            HPWM16_11_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_11_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_11_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_11_HPWM16F_DCH_Script * 256))
            HPWM16_11_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_11_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_11_HPWM16F_PRH_Script * 256))
            if HPWM16_11_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_11_HPWM16F_PRH_Script
            end if

            'Channel 12
            HPWM16F_PRPS_Req = ChipMHz * 1000 / HPWM16_12_Freq
            'Calculate the duty cycle.
            'Simpler than main code because the compiler does the calculation using FP math
            HPWM16F_DCy = HPWM16_12_Duty / 100 * HPWM16F_PRPS_Req
            HPWM16_12_HPWM16F_DCH_Script=0
            HPWM16_12_HPWM16F_DCL_Script=0
            HPWM16_12_HPWM16F_PRH_Script=0
            HPWM16_12_HPWM16F_PRL_Script=0

            'Output the values for use in the registers
            HPWM16_12_HPWM16F_DCH_Script = INT(HPWM16F_DCy / 256)
            HPWM16_12_HPWM16F_DCL_Script = INT(HPWM16F_DCy - INT(HPWM16_12_HPWM16F_DCH_Script * 256))
            HPWM16_12_HPWM16F_PRH_Script = INT(HPWM16F_PRPS_Req / 256)
            HPWM16_12_HPWM16F_PRL_Script = INT(HPWM16F_PRPS_Req - (HPWM16_12_HPWM16F_PRH_Script * 256))
            if HPWM16_12_HPWM16F_PRH_Script > 255 then
              Warning "Chip Clock Speed to high for selected frequency - parameter must be less than 256 not" HPWM16_12_HPWM16F_PRH_Script
            end if

          end if
      #endscript

  #endif

end sub

'Method to support Offset and Phase - not operational
Sub HPWM16(in PWMChannel as Byte, in PWMFreq as Byte, in PWMDuty as Word, optional in PWMPhase as Word = 0 , optional in PWMOffset as Word = 0, optional HPWM16_prescalar as Byte = 1)
'PWMChannel is the number of the PWM module
'PWM_Freq is the frequency in kHz.  Minimum frequency is 1 kHz, Maximum is 255 kHz
'PWMDuty is the duty cycle expressed as PWMDuty/65535
'There is no HPWM_FAST option because there is no iterative calculation for the prescaler

'The maximum value of prescalar is 128
If HPWM16_prescalar > 128 Then
  HPWM16_prescalar = 128
End If

HPWM16_setPRPS    'Set the prescaler and period counter
HPWM16_setDC      'Set the duty cycle
HPWM16_setGoing   'Set the PWM running

End Sub

Sub HPWM16(in PWMChannel as Byte, in PWMFreq as Byte, in PWMDuty as Word, optional HPWM16_prescalar as Byte = 1)
'PWMChannel is the number of the PWM module
'PWM_Freq is the frequency in kHz.  Minimum frequency is 1 kHz, Maximum is 255 kHz
'PWMDuty is the duty cycle expressed as PWMDuty/65535
'There is no HPWM_FAST option because there is no iterative calculation for the prescaler

'The maximum value of prescalar is 128
If HPWM16_prescalar > 128 Then
  HPWM16_prescalar = 128
End If

HPWM16_setPRPS    'Set the prescaler and period counter
HPWM16_setDC      'Set the duty cycle
HPWM16_setGoing   'Set the PWM running

End Sub

Sub HPWM16_setGoing
  'Loads the values and turns the PWM on
  Select Case PWMChannel
    #ifdef Var(PWM1CON)       'only include if there is a PWM1 module
    #ifdef USE_HPWM16_1 TRUE  'only include if PWM1 has not been disabled
    Case 1
      PWM1LD = 1
      PWM1CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM2CON)       'only include if there is a PWM2 module
    #ifdef USE_HPWM16_2 TRUE  'only include if PWM2 has not been disabled
    Case 2
      PWM2LD = 1
      PWM2CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM3CON)       'only include if there is a PWM3 module
    #ifdef USE_HPWM16_3 TRUE  'only include if PWM3 has not been disabled
    Case 3
      PWM3LD = 1
      PWM3CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM4CON)       'only include if there is a PWM4 module
    #ifdef USE_HPWM16_4 TRUE  'only include if PWM4 has not been disabled
    Case 4
      PWM4LD = 1
      PWM4CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM5CON)       'only include if there is a PWM5 module
    #ifdef USE_HPWM16_5 TRUE  'only include if PWM5 has not been disabled
    Case 5
      PWM5LD = 1
      PWM5CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM6CON)       'only include if there is a PWM6 module
    #ifdef USE_HPWM16_6 TRUE  'only include if PWM6 has not been disabled
    Case 6
      PWM6LD = 1
      PWM6CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM7CON)       'only include if there is a PWM7 module
    #ifdef USE_HPWM16_7 TRUE  'only include if PWM7 has not been disabled
    Case 7
      PWM7LD = 1
      PWM7CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM8CON)       'only include if there is a PWM8 module
    #ifdef USE_HPWM16_8 TRUE  'only include if PWM8 has not been disabled
    Case 8
      PWM8LD = 1
      PWM8CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM9CON)       'only include if there is a PWM9 module
    #ifdef USE_HPWM16_9 TRUE  'only include if PWM9 has not been disabled
    Case 9
      PWM9LD = 1
      PWM9CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM10CON)       'only include if there is a PWM10 module
    #ifdef USE_HPWM16_10 TRUE  'only include if PWM10 has not been disabled
    Case 10
      PWM10LD = 1
      PWM10CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM11CON)       'only include if there is a PWM11 module
    #ifdef USE_HPWM16_11 TRUE  'only include if PWM11 has not been disabled
    Case 11
      PWM11LD = 1
      PWM11CON = 0b11000000
    #endif
    #endif

    #ifdef Var(PWM12CON)       'only include if there is a PWM12 module
    #ifdef USE_HPWM16_12 TRUE  'only include if PWM12 has not been disabled
    Case 12
      PWM12LD = 1
      PWM12CON = 0b11000000
    #endif
    #endif

  End Select

End Sub
Sub HPWM16_setPRPS
'Frequency is in kHz.
'Chip frequency can be anything from 31 kHz to 64 MHz with internal oscillator

Dim HPWM16_PRPS_Req as Word  'the required product of PR and PS to get the frequency

HPWM16_PRPS_Req = ChipMHz * 1000 / PWMFreq

SetRegisters:
'convert the prescalar to the register value
HPWM16_PS = HPWM16_Log2(HPWM16_prescalar)

'add ifdefs in here to save on program code space
Select Case PWMChannel
  #ifdef Var(PWM1CON)       'only include if there is a PWM1 module
  #ifdef USE_HPWM16_1 TRUE  'only include if PWM1 has not been disabled
  Case 1
    PWM1PS0 = HPWM16_PS.0
    PWM1PS1 = HPWM16_PS.1
    PWM1PS2 = HPWM16_PS.2
    PWM1PRL = HPWM16_PRPS_Req
    PWM1PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM2CON)       'only include if there is a PWM2 module
  #ifdef USE_HPWM16_2 TRUE  'only include if PWM2 has not been disabled
  Case 2
    PWM2PS0 = HPWM16_PS.0
    PWM2PS1 = HPWM16_PS.1
    PWM2PS2 = HPWM16_PS.2
    PWM2PRL = HPWM16_PRPS_Req
    PWM2PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM3CON)       'only include if there is a PWM3 module
  #ifdef USE_HPWM16_3 TRUE  'only include if PWM3 has not been disabled
  Case 3
    PWM3PS0 = HPWM16_PS.0
    PWM3PS1 = HPWM16_PS.1
    PWM3PS2 = HPWM16_PS.2
    PWM3PRL = HPWM16_PRPS_Req
    PWM3PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM4CON)       'only include if there is a PWM4 module
  #ifdef USE_HPWM16_4 TRUE  'only include if PWM4 has not been disabled
  Case 4
    PWM4PS0 = HPWM16_PS.0
    PWM4PS1 = HPWM16_PS.1
    PWM4PS2 = HPWM16_PS.2
    PWM4PRL = HPWM16_PRPS_Req
    PWM4PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM5CON)       'only include if there is a PWM5 module
  #ifdef USE_HPWM16_5 TRUE  'only include if PWM5 has not been disabled
  Case 5
    PWM5PS0 = HPWM16_PS.0
    PWM5PS1 = HPWM16_PS.1
    PWM5PS2 = HPWM16_PS.2
    PWM5PRL = HPWM16_PRPS_Req
    PWM5PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM6CON)       'only include if there is a PWM6 module
  #ifdef USE_HPWM16_6 TRUE  'only include if PWM6 has not been disabled
  Case 6
    PWM6PS0 = HPWM16_PS.0
    PWM6PS1 = HPWM16_PS.1
    PWM6PS2 = HPWM16_PS.2
    PWM6PRL = HPWM16_PRPS_Req
    PWM6PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM7CON)       'only include if there is a PWM7 module
  #ifdef USE_HPWM16_7 TRUE  'only include if PWM7 has not been disabled
  Case 7
    PWM7PS0 = HPWM16_PS.0
    PWM7PS1 = HPWM16_PS.1
    PWM7PS2 = HPWM16_PS.2
    PWM7PRL = HPWM16_PRPS_Req
    PWM7PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM8CON)       'only include if there is a PWM8 module
  #ifdef USE_HPWM16_8 TRUE  'only include if PWM8 has not been disabled
  Case 8
    PWM8PS0 = HPWM16_PS.0
    PWM8PS1 = HPWM16_PS.1
    PWM8PS2 = HPWM16_PS.2
    PWM8PRL = HPWM16_PRPS_Req
    PWM8PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM9CON)       'only include if there is a PWM9 module
  #ifdef USE_HPWM16_9 TRUE  'only include if PWM9 has not been disabled
  Case 9
    PWM9PS0 = HPWM16_PS.0
    PWM9PS1 = HPWM16_PS.1
    PWM9PS2 = HPWM16_PS.2
    PWM9PRL = HPWM16_PRPS_Req
    PWM9PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM10CON)       'only include if there is a PWM10 module
  #ifdef USE_HPWM16_10 TRUE  'only include if PWM10 has not been disabled
  Case 10
    PWM10PS0 = HPWM16_PS.0
    PWM10PS1 = HPWM16_PS.1
    PWM10PS2 = HPWM16_PS.2
    PWM10PRL = HPWM16_PRPS_Req
    PWM10PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM11CON)       'only include if there is a PWM11 module
  #ifdef USE_HPWM16_11 TRUE  'only include if PWM11 has not been disabled
  Case 11
    PWM11PS0 = HPWM16_PS.0
    PWM11PS1 = HPWM16_PS.1
    PWM11PS2 = HPWM16_PS.2
    PWM11PRL = HPWM16_PRPS_Req
    PWM11PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

  #ifdef Var(PWM12CON)       'only include if there is a PWM12 module
  #ifdef USE_HPWM16_12 TRUE  'only include if PWM12 has not been disabled
  Case 12
    PWM12PS0 = HPWM16_PS.0
    PWM12PS1 = HPWM16_PS.1
    PWM12PS2 = HPWM16_PS.2
    PWM12PRL = HPWM16_PRPS_Req
    PWM12PRH = HPWM16_PRPS_Req_H
  #endif
  #endif

End Select

End Sub

Sub HPWM16_setDC
'DutyCycle depends on the values of the prescaler
'Therefore setPRPS() must be called first to set PR
'The phase counter (PH) is not used in this driver
'
'PWMDuty / 2^16 * (PWMxPR+1) = PWMxDC

dim HPWM16_DCy as Word

Dim PWMDuty as Word
Dim HPWM16_PRPS_Req as Word

'If the DutyCycle is 1 then make sure it is on
If PWMDuty = 65535 Then
  HPWM16_DCy = HPWM16_PRPS_Req + 1
  Goto SetDC
End If

'Calculate the DC register, but use Long math to prevent overflow
HPWM16_DCy = [long]PWMDuty * (HPWM16_PRPS_Req + 1) / 0xFFFF

SetDC:

'Set the registers
'Set the PHH register because this is 'undefined' at power up

Select Case PWMChannel
  #ifdef Var(PWM1CON)       'only include if there is a PWM1 module
  #ifdef USE_HPWM16_1 TRUE  'only include if PWM1 has not been disabled
  Case 1
    PWM1PHH = 0x00
    PWM1PHL = 0x00
    PWM1DCL = HPWM16_DCy
    PWM1DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM2CON)       'only include if there is a PWM2 module
  #ifdef USE_HPWM16_2 TRUE  'only include if PWM2 has not been disabled
  Case 2
    PWM2PHH = 0x00
    PWM2PHL = 0x00
    PWM2DCL = HPWM16_DCy
    PWM2DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM3CON)       'only include if there is a PWM3 module
  #ifdef USE_HPWM16_3 TRUE  'only include if PWM3 has not been disabled
  Case 3
    PWM3PHH = 0x00
    PWM3PHL = 0x00
    PWM3DCL = HPWM16_DCy
    PWM3DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM4CON)       'only include if there is a PWM4 module
  #ifdef USE_HPWM16_4 TRUE  'only include if PWM4 has not been disabled
  Case 4
    PWM4PHH = 0x00
    PWM4PHL = 0x00
    PWM4DCL = HPWM16_DCy
    PWM4DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM5CON)       'only include if there is a PWM5 module
  #ifdef USE_HPWM16_5 TRUE  'only include if PWM5 has not been disabled
  Case 5
    PWM5PHH = 0x00
    PWM5PHL = 0x00
    PWM5DCL = HPWM16_DCy
    PWM5DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM6CON)       'only include if there is a PWM6 module
  #ifdef USE_HPWM16_6 TRUE  'only include if PWM6 has not been disabled
  Case 6
    PWM6PHH = 0x00
    PWM6PHL = 0x00
    PWM6DCL = HPWM16_DCy
    PWM6DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM7CON)       'only include if there is a PWM7 module
  #ifdef USE_HPWM16_7 TRUE  'only include if PWM7 has not been disabled
  Case 7
    PWM7PHH = 0x00
    PWM7PHL = 0x00
    PWM7DCL = HPWM16_DCy
    PWM7DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM8CON)       'only include if there is a PWM8 module
  #ifdef USE_HPWM16_8 TRUE  'only include if PWM8 has not been disabled
  Case 8
    PWM8PHH = 0x00
    PWM8PHL = 0x00
    PWM8DCL = HPWM16_DCy
    PWM8DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM9CON)       'only include if there is a PWM9 module
  #ifdef USE_HPWM16_9 TRUE  'only include if PWM9 has not been disabled
  Case 9
    PWM9PHH = 0x00
    PWM9PHL = 0x00
    PWM9DCL = HPWM16_DCy
    PWM9DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM10CON)       'only include if there is a PWM10 module
  #ifdef USE_HPWM16_10 TRUE  'only include if PWM10 has not been disabled
  Case 10
    PWM10PHH = 0x00
    PWM10PHL = 0x00
    PWM10DCL = HPWM16_DCy
    PWM10DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM11CON)       'only include if there is a PWM11 module
  #ifdef USE_HPWM16_11 TRUE  'only include if PWM11 has not been disabled
  Case 11
    PWM11PHH = 0x00
    PWM11PHL = 0x00
    PWM11DCL = HPWM16_DCy
    PWM11DCH = HPWM16_DCy_H
  #endif
  #endif
  #ifdef Var(PWM12CON)       'only include if there is a PWM12 module
  #ifdef USE_HPWM16_12 TRUE  'only include if PWM12 has not been disabled
  Case 12
    PWM12PHH = 0x00
    PWM12PHL = 0x00
    PWM12DCL = HPWM16_DCy
    PWM12DCH = HPWM16_DCy_H
  #endif
  #endif

End Select

End Sub

Sub HPWM16On(in PWMChannel as Byte)
  'Simple PWM using constant values
  'Register values are calculated by the compiler which saves on program memory and RAM

  'set the registers
  Select Case PWMChannel

      #ifdef Var(PWM1CON)         'only include if there is a PWM1 module
        #ifdef USE_HPWM16_1 TRUE  'only include if PWM1 has not been disabled
    Case 1
            #ifndef HPWM16_1_Freq
              PWM1PS0 = 0x00            'prescale is always 1 (0x00)
              PWM1PS1 = 0x00
              PWM1PS2 = 0x00
              PWM1PRL = HPWM16F_PRL_Script     'frequency
              PWM1PRH = HPWM16F_PRH_Script
              PWM1PHH = 0x00            'phase count is always zero
              PWM1PHL = 0x00
              PWM1DCL = HPWM16F_DCL_Script     'duty cycle
              PWM1DCH = HPWM16F_DCH_Script
              PWM1LD = 1
              PWM1CON = 0b11000000
            #endif

            #ifdef HPWM16_1_Freq
              PWM1PS0 = 0x00            'prescale is always 1 (0x00)
              PWM1PS1 = 0x00
              PWM1PS2 = 0x00
              PWM1PRL = HPWM16_1_HPWM16F_PRL_Script     'frequency
              PWM1PRH = HPWM16_1_HPWM16F_PRH_Script
              PWM1PHH = 0x00            'phase count is always zero
              PWM1PHL = 0x00
              PWM1DCL = HPWM16_1_HPWM16F_DCL_Script     'duty cycle
              PWM1DCH = HPWM16_1_HPWM16F_DCH_Script
              PWM1LD = 1
              PWM1CON = 0b11000000
            #endif
        #endif
      #endif

      #ifdef Var(PWM2CON)         'only include if there is a PWM2 module
        #ifdef USE_HPWM16_2 TRUE  'only include if PWM2 has not been disabled
    Case 2
        PWM2PS0 = 0x00            'prescale is always 1 (0x00)
        PWM2PS1 = 0x00
        PWM2PS2 = 0x00
        PWM2PRL = HPWM16_2_HPWM16F_PRL_Script     'frequency
        PWM2PRH = HPWM16_2_HPWM16F_PRH_Script
        PWM2PHH = 0x00            'phase count is always zero
        PWM2PHL = 0x00
        PWM2DCL = HPWM16_2_HPWM16F_DCL_Script     'duty cycle
        PWM2DCH = HPWM16_2_HPWM16F_DCH_Script
        PWM2LD = 1
        PWM2CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM3CON)         'only include if there is a PWM3 module
        #ifdef USE_HPWM16_3 TRUE  'only include if PWM3 has not been disabled
    Case 3
        PWM3PS0 = 0x00            'prescale is always 1 (0x00)
        PWM3PS1 = 0x00
        PWM3PS2 = 0x00
        PWM3PRL = HPWM16_3_HPWM16F_PRL_Script     'frequency
        PWM3PRH = HPWM16_3_HPWM16F_PRH_Script
        PWM3PHH = 0x00            'phase count is always zero
        PWM3PHL = 0x00
        PWM3DCL = HPWM16_3_HPWM16F_DCL_Script     'duty cycle
        PWM3DCH = HPWM16_3_HPWM16F_DCH_Script
        PWM3LD = 1
        PWM3CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM4CON)         'only include if there is a PWM4 module
        #ifdef USE_HPWM16_4 TRUE  'only include if PWM4 has not been disabled
    Case 4
        PWM4PS0 = 0x00            'prescale is always 1 (0x00)
        PWM4PS1 = 0x00
        PWM4PS2 = 0x00
        PWM4PRL = HPWM16_4_HPWM16F_PRL_Script     'frequency
        PWM4PRH = HPWM16_4_HPWM16F_PRH_Script
        PWM4PHH = 0x00            'phase count is always zero
        PWM4PHL = 0x00
        PWM4DCL = HPWM16_4_HPWM16F_DCL_Script     'duty cycle
        PWM4DCH = HPWM16_4_HPWM16F_DCH_Script
        PWM4LD = 1
        PWM4CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM5CON)         'only include if there is a PWM5 module
        #ifdef USE_HPWM16_5 TRUE  'only include if PWM5 has not been disabled
    Case 5
        PWM5PS0 = 0x00            'prescale is always 1 (0x00)
        PWM5PS1 = 0x00
        PWM5PS2 = 0x00
        PWM5PRL = HPWM16_5_HPWM16F_PRL_Script     'frequency
        PWM5PRH = HPWM16_5_HPWM16F_PRH_Script
        PWM5PHH = 0x00            'phase count is always zero
        PWM5PHL = 0x00
        PWM5DCL = HPWM16_5_HPWM16F_DCL_Script     'duty cycle
        PWM5DCH = HPWM16_5_HPWM16F_DCH_Script
        PWM5LD = 1
        PWM5CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM6CON)         'only include if there is a PWM6 module
        #ifdef USE_HPWM16_6 TRUE  'only include if PWM6 has not been disabled
    Case 6
        PWM6PS0 = 0x00            'prescale is always 1 (0x00)
        PWM6PS1 = 0x00
        PWM6PS2 = 0x00
        PWM6PRL = HPWM16_6_HPWM16F_PRL_Script     'frequency
        PWM6PRH = HPWM16_6_HPWM16F_PRH_Script
        PWM6PHH = 0x00            'phase count is always zero
        PWM6PHL = 0x00
        PWM6DCL = HPWM16_6_HPWM16F_DCL_Script     'duty cycle
        PWM6DCH = HPWM16_6_HPWM16F_DCH_Script
        PWM6LD = 1
        PWM6CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM7CON)         'only include if there is a PWM7 module
        #ifdef USE_HPWM16_7 TRUE  'only include if PWM7 has not been disabled
    Case 7
        PWM7PS0 = 0x00            'prescale is always 1 (0x00)
        PWM7PS1 = 0x00
        PWM7PS2 = 0x00
        PWM7PRL = HPWM16_7_HPWM16F_PRL_Script     'frequency
        PWM7PRH = HPWM16_7_HPWM16F_PRH_Script
        PWM7PHH = 0x00            'phase count is always zero
        PWM7PHL = 0x00
        PWM7DCL = HPWM16_7_HPWM16F_DCL_Script     'duty cycle
        PWM7DCH = HPWM16_7_HPWM16F_DCH_Script
        PWM7LD = 1
        PWM7CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM8CON)         'only include if there is a PWM8 module
        #ifdef USE_HPWM16_8 TRUE  'only include if PWM8 has not been disabled
    Case 8
        PWM8PS0 = 0x00            'prescale is always 1 (0x00)
        PWM8PS1 = 0x00
        PWM8PS2 = 0x00
        PWM8PRL = HPWM16_8_HPWM16F_PRL_Script     'frequency
        PWM8PRH = HPWM16_8_HPWM16F_PRH_Script
        PWM8PHH = 0x00            'phase count is always zero
        PWM8PHL = 0x00
        PWM8DCL = HPWM16_8_HPWM16F_DCL_Script     'duty cycle
        PWM8DCH = HPWM16_8_HPWM16F_DCH_Script
        PWM8LD = 1
        PWM8CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM9CON)         'only include if there is a PWM9 module
        #ifdef USE_HPWM16_9 TRUE  'only include if PWM9 has not been disabled
    Case 9
        PWM9PS0 = 0x00            'prescale is always 1 (0x00)
        PWM9PS1 = 0x00
        PWM9PS2 = 0x00
        PWM9PRL = HPWM16_9_HPWM16F_PRL_Script     'frequency
        PWM9PRH = HPWM16_9_HPWM16F_PRH_Script
        PWM9PHH = 0x00            'phase count is always zero
        PWM9PHL = 0x00
        PWM9DCL = HPWM16_9_HPWM16F_DCL_Script     'duty cycle
        PWM9DCH = HPWM16_9_HPWM16F_DCH_Script
        PWM9LD = 1
        PWM9CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM10CON)         'only include if there is a PWM10 module
        #ifdef USE_HPWM16_10 TRUE  'only include if PWM10 has not been disabled
    Case 10
        PWM10PS0 = 0x00            'prescale is always 1 (0x00)
        PWM10PS1 = 0x00
        PWM10PS2 = 0x00
        PWM10PRL = HPWM16_10_HPWM16F_PRL_Script     'frequency
        PWM10PRH = HPWM16_10_HPWM16F_PRH_Script
        PWM10PHH = 0x00            'phase count is always zero
        PWM10PHL = 0x00
        PWM10DCL = HPWM16_10_HPWM16F_DCL_Script     'duty cycle
        PWM10DCH = HPWM16_10_HPWM16F_DCH_Script
        PWM10LD = 1
        PWM10CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM11CON)         'only include if there is a PWM11 module
        #ifdef USE_HPWM16_11 TRUE  'only include if PWM11 has not been disabled
    Case 11
        PWM11PS0 = 0x00            'prescale is always 1 (0x00)
        PWM11PS1 = 0x00
        PWM11PS2 = 0x00
        PWM11PRL = HPWM16_11_HPWM16F_PRL_Script     'frequency
        PWM11PRH = HPWM16_11_HPWM16F_PRH_Script
        PWM11PHH = 0x00            'phase count is always zero
        PWM11PHL = 0x00
        PWM11DCL = HPWM16_11_HPWM16F_DCL_Script     'duty cycle
        PWM11DCH = HPWM16_11_HPWM16F_DCH_Script
        PWM11LD = 1
        PWM11CON = 0b11000000
        #endif
      #endif

      #ifdef Var(PWM12CON)         'only include if there is a PWM12 module
        #ifdef USE_HPWM16_12 TRUE  'only include if PWM12 has not been disabled
    Case 12
        PWM12PS0 = 0x00            'prescale is always 1 (0x00)
        PWM12PS1 = 0x00
        PWM12PS2 = 0x00
        PWM12PRL = HPWM16_12_HPWM16F_PRL_Script     'frequency
        PWM12PRH = HPWM16_12_HPWM16F_PRH_Script
        PWM12PHH = 0x00            'phase count is always zero
        PWM12PHL = 0x00
        PWM12DCL = HPWM16_12_HPWM16F_DCL_Script     'duty cycle
        PWM12DCH = HPWM16_12_HPWM16F_DCH_Script
        PWM12LD = 1
        PWM12CON = 0b11000000
        #endif
      #endif

  End Select
End Sub

Sub HPWM16Off (in PWMChannel as Byte)
  'Turns off the selected PWM channel

  Select Case PWMChannel

    #ifdef Var(PWM1CON)       'only include if there is a PWM1 module
    #ifdef USE_HPWM16_1 TRUE  'only include if PWM1 has not been disabled
    Case 1
      PWM1EN = 0
    #endif
    #endif

    #ifdef Var(PWM2CON)       'only include if there is a PWM2 module
    #ifdef USE_HPWM16_2 TRUE  'only include if PWM2 has not been disabled
    Case 2
      PWM2EN = 0
    #endif
    #endif

    #ifdef Var(PWM3CON)       'only include if there is a PWM3 module
    #ifdef USE_HPWM16_3 TRUE  'only include if PWM3 has not been disabled
    Case 3
      PWM3EN = 0
    #endif
    #endif

  #ifdef Var(PWM4CON)       'only include if there is a PWM4 module
    #ifdef USE_HPWM16_4 TRUE  'only include if PWM4 has not been disabled
    Case 4
      PWM4EN = 0
    #endif
    #endif

    #ifdef Var(PWM5CON)       'only include if there is a PWM5 module
    #ifdef USE_HPWM16_5 TRUE  'only include if PWM5 has not been disabled
    Case 5
      PWM5EN = 0
    #endif
    #endif

    #ifdef Var(PWM6CON)       'only include if there is a PWM6 module
    #ifdef USE_HPWM16_6 TRUE  'only include if PWM6 has not been disabled
    Case 6
      PWM6EN = 0
    #endif
    #endif

    #ifdef Var(PWM7CON)       'only include if there is a PWM7 module
    #ifdef USE_HPWM16_7 TRUE  'only include if PWM7 has not been disabled
    Case 7
      PWM7EN = 0
    #endif
    #endif

    #ifdef Var(PWM8CON)       'only include if there is a PWM8 module
    #ifdef USE_HPWM16_8 TRUE  'only include if PWM8 has not been disabled
    Case 8
      PWM8EN = 0
    #endif
    #endif

    #ifdef Var(PWM9CON)       'only include if there is a PWM9 module
    #ifdef USE_HPWM16_9 TRUE  'only include if PWM9 has not been disabled
    Case 9
      PWM9EN = 0
    #endif
    #endif

    #ifdef Var(PWM10CON)       'only include if there is a PWM10 module
    #ifdef USE_HPWM16_10 TRUE  'only include if PWM10 has not been disabled
    Case 10
      PWM10EN = 0
    #endif
    #endif

    #ifdef Var(PWM11CON)       'only include if there is a PWM11 module
    #ifdef USE_HPWM16_11 TRUE  'only include if PWM11 has not been disabled
    Case 11
      PWM11EN = 0
    #endif
    #endif

    #ifdef Var(PWM12CON)       'only include if there is a PWM12 module
    #ifdef USE_HPWM16_12 TRUE  'only include if PWM12 has not been disabled
    Case 12
      PWM12EN = 0
    #endif
    #endif

  End Select

End Sub

Function HPWM16_Log2(in exponent as Byte) as Byte
'Calculates the log base 2 of a number
'assumes the number is a whole power of 2

  HPWM16_Log2 = 0

  Do while exponent > 1
    exponent = exponent / 2
    HPWM16_Log2 = HPWM16_Log2 + 1
  Loop

End Function
