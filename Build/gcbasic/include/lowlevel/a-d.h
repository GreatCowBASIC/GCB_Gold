'    Analog to Digital conversion routines for GCBASIC
'    Copyright (C) 2006-2025 Hugh Considine, Kent Schafer, William Roth, Evan Venn

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

'This is full of debug. LEAVE IT IN!  WE NEED IT TO DEBUG THIS !

'Original code by Hugh Considine
'18Fxx31 code by Kent Schafer
'added ReadAD12 2/12/2015

'24/12/2015 - William Roth
'   1) ReadADx now returns correct values when changing between
'      ReadAD, ReadAD10, and ReadAd12 in same program.
'   2) Added optional parameter In ReadADx function setup
'      Negative AD Analog Port.
'   3) Negative ADC Values Supported for Differential ADC
'   4) 8-Bit Differential Reads supported with negative nunbers
'   5) NOTE: Negative numbers require user to declare
'       AD Read Variable as integer
'-------------------------------------------------
'27/12/2015 - William Roth
' Revised ReadAD and ReadAD10 for greater compatibility.  Revised at RC37
'6/3/2016 - Evan Venn
' Revised ReadAD, ReadAD10 and ReadAD12 for ADPCH
' Corrected typo in LLREADAD

'7/3/2016 - William Roth
'Added IFDEF's to prevent unnecessary select case code from compiling when

'AD Channel is not used. New Define "USE_ADx" where x is AD Channel
'Set to USE_AD0 FALSE to optimise the code.
' This can be changed if desired.

' Example:  #define USE_AD9
' This will save about 200 bytes
'8/3/2016 revised to USE_ADx support by default.

'14/3/2016 - Added spport for 16F168xx Chips
'See new section starting at line 637 for details

'10/10/2016 - added support for 14 and 20 ANSELx config for devices. These were missing.
'     Added ADReadAdditionalPreReadCommand for support
'
'     #ifdef ADReadAdditionalPreReadCommand
'        ADReadAdditionalPreReadCommand
'     #endif
'11/10/2016 - added support for 8 pin ANSELx config for devices.
'11/10/2016 - Reverted READAD and READAD10 commands for compatbility
'             added ReadAD, ReadAD10 and ReadAD12 for Differential reads
'18/10/2016 - Added the debug. This was driving me mad!
'14/4/2017 -  Resolved a host of errors as follows:
'             Test results and chips used shown below. All tests on ADC0 (just dont have time to test every option!)
'ReadAD
'          READAD (AN0) – 16f1789, 16f877a, 16f1939, 18f4525, 16f88, 18f45k80
'             The ADN_PORT call that I must have replicated from other methods was incorrect. As, in this case, no value would have been returned.
'             Needed to resolve  CHSN ADNREF = ADCON2 = 0x0F;  not being set.  By setting the CHSN3:0 bits.
'             Also emoved ADN_PORT <> 0, then revised the read registers to return a byte.
'
'          READAD (AN0, true) – 16f1789, 16f877a, 16f1939, 18f4525, 16f88, 18f45k80
'             This needs to return a byte.
'             Resolved CHSN ADNREF = ADCON2 = 0x0F;  not being set.  By setting the CHSN3:0 bits.
'
'          READAD (AN0, AN3) – 16f1789, 18f45k80
'             using -VREF
'             This needs to return a byte.
'             Reverted to READAD = READAD / 16 as the ROTATE command destroyed the integer values
'
'
'READAD10
'          READAD10(AN0) 16f1789, 16f877a, 16f1939, 18f4525, 16f88, 18f45k80
'             This needs to return the full range of the ADC module – see the HELP. I have updated the range to 0-4096.  You may not agree with this… but, Hugh insists.
'             Needed to resolve  CHSN ADNREF = ADCON2 = 0x0F;  not being set.  By setting the CHSN3:0 bits within the existing IF-ENDIF
'
'          READ10(AN0, AN2)  – 16f1789, 18f45k80
'            For differential ADC reading, the returned value is an integer as negative values can be returned.
'            Works ok
'
'          READAD10(AN0, TRUE)  – 16f1789, 18f45k80
'            Undocumented. Completed test to examine results
'            Works ok returns correct value
'
'READAD12
'          READAD12(AN0)  – 16f1789, 16f877a, 16f1939, 18f4525, 18f45k80
'            This needs to return the full range of the ADC module 0-4096.  .
'            Needed to resolve  CHSN ADNREF = ADCON2 = 0x0F;  not being set.  By setting the CHSN3:0 bits within the existing IF-ENDIF
'
'          READAD12(AN0, AN2)  – 16f1789, 18f45k80
'             For differential ADC reading, the returned value is an integer as negative values can be returned.
'             Works ok
'
'          READAD12(AN0, TRUE)  – 16f1789, 18f45k80
'             Undocumented. Completed test to examine results
'             Needed to resolve  CHSN ADNREF = ADCON2 = 0x0F;  not being set.  By setting the CHSN3:0 bits within the existing IF-ENDIF
'             Now works ok returns correct value
' End of 11/4/2017 -  changes
'       15/4/2017 - Adapted AD_Acquisition_Time_Select_bits to  0b100 or decimal 4
'                   Added to set AD_Acquisition_Time_Select_bits  in SetNegativeChannelSelectbits macro - needed to address timing issue
' 4/6/2017  - Removal of Testprogram label set. Put silly labels in ASM!
' 13/6/2017 - Removal of explicit register definitions

' 9/7/17 Added FVR
'       ADFVR_Initialize ( FVR_OFF | ADFVR_1x | ADFVR_2x| ADFVR_3x )
'       ADFVR_IsOutputReady returns true or false
' 10/7/17  Corrected FVR to set ADC FVR correctly
' 18/7/17  Improved Set conversion clock to handle ADCS2
' 15/9/17  Commentted out #define SINGLE 255.  Problem is that the compiler (in some places) already recognises Single as a data type, part of the initial work for supporting floating point
           ' "Dim MyVar As Single" used to create a 4-byte variable big enough to store a single precision floating point number,
           ' now it causes an error message because of the constant.
' 9/11/17  '#samebit GONDONE GO_NOT_DONE
' 18/2/18  'Adapted AVR Reference settings, see #180218.  Left old code as is but testing for MUX4.
' 19/2/18  'Revised AD_REF_SOURCE = AD_REF_AVCC section to handle TINYx chips.  The VCC reference is different from MEGA devices o the Tiny devices!
           'Reverted adaption AVR Reference settings, see #180218.  Left old code as is but now test suitable bit for MUX4.
' 31/4/18  Added ChipReadAD10BitForceVariant to ensure 12 bit ADC force a 10 bit ADC result.
' 10/11/18 Adapted FVRInitialize to suppport 18f FVR with FVRST
' 19/11/18 Revised to add  #ifdef bit(ADFM) protection on ReadAD for 10f devices
' 11/12/18 Added references to support 16f1834x ADC channels
' 14/12/18 Added references to support 16f1834x ADC channels
' 17/02/19 Corrected AN29 address. Removed a typo. Prevented PORTD.5 ADC from operating as expected.
' 21/08/19 Improved documentation only. No functional change
' 07/05/20 Isolation of ANSELx to reduce variables creation
' 20/06/20 Support for ChipFamily 121 and tidy up in LLREADAD and changed to ADLAR = ADLeftAdjust support 121 and reduce code size
' 08/12/20 Support for ChipFamily 122 as these are 12bit ADCs
' 02/02/21 Resolved typo in line 1613
' 16/02/21 Improved Read A/D @3 to use ADCON0.GO rather than G0 as GO can also reference NVM1CON.GO
' 28/08/21 Added ADREADPREREADCOMMAND to AVR read
' 06/02/22 Corrected ADREF register settings for chipfamily LGT
' 26/03/22 Added support for older 18F with ADCCON3
' 04/04/22 Corrected FVRInitialize test for BIT(ADPREF1).  Was incorrect as testing for a Register (VAR) within FVRInitialize #IF CAR(ADPREF1) now reads #IF BIT(ADPREF1)
' 12/07/22 Correct #IFDEF VAR(ADCLK) isolation test - impacted 16F18855 class
' 14/08/22 Updated user changeable constants only - no functional change
' 07/09/22 Added #SAMEBIT to ADGO for 16F180xx family
' 02/10/22 Add support for legacy chips PIC18F2331/2431/4331/4431 where ADCHS exists and AD port is set use ACMOD1/ACMOD0 (see above)
' 24/10/22 Add support for family 15002 and 15003.  Added Diffrential handler as this chipfamily has new registers for Diffrential Operations
' 25/11/22 Added isolation for family 15002, 15003 and 15004 for Diffrential handler
' 21/09/24 Add ReadADAVRDx and ReadAD10AVRDx. User can control ADC Prescaler via #define AVRX_ADC_PRESC_DIV 16 | 4 - defaults to AVRX_ADC_PRESC_DIV 16 
' 22/09/24 Add ReadADAVRDx and ReadAD10AVRDx optimisation constants.  AVRX_ADC_PORTx_PinyCTRL and AVRX_ADC_NOCACHE_PORTx_PinyCTRL
/*
AVRDX ReadAD and ReadAD10 optional control constants added 22/09/24

Change the constants can save up to 61 words but the functions are constrained to a specfic PORTx_Piny

  Set explicit port.pin - this will prevent any table lookup - would be used when either a single ADC is used, or, an error in the lookup table
      Save 22 words but makes the READAD and READAD10 locked on the specified PORTx_Piny
      ANIx MUST be correct for the specified PORTx_Piny

      #DEFINE AVRX_ADC_PORTx_PinyCTRL PORTD_Pin1CTRL //~ for AIN1 on a MEGA4809

  Do not cache the port.pin setting.Therefore, you need to manage the PORTx_PinyCTRL state.
      Saves 9 words
      
      #DEFINE AVRX_ADC_NOCACHE_PORTx_PinyCTRL

  Change the ADC frequency
      No program size impact. This just overrides the default value
      
      #DEFINE AVRX_ADC_PRESC_DIV 16    // Options are 16 or 4
*/        



#option REQUIRED PIC ChipADC %NoADC%

'Commands:
'var = ReadAD(port, optional port)  Reads port(s), and returns value.
'ADFormat(type)   Choose Left or Right justified
'ADOff      Set A/D converter off. Use if trouble is experienced when
'attempting to use ports in digital mode

'User Defaults
#SCRIPT

  If DEF(CHIPAVRDX) Then
      READAD=ReadADAVRDx
      READAD10=ReadAD10AVRDx

      // the default
      If NODEF(AVRX_ADC_PRESC_DIV) Then
        SCRIPTADC_PRESC_DIV_gc = ADC_PRESC_DIV16_gc
      End If

      // suppot user defined value using the contant AVRX_ADC_PRESC_DIV
      If DEF(AVRX_ADC_PRESC_DIV) Then
        AVRX_ADC_PRESC_DIV_Check = 0
        If AVRX_ADC_PRESC_DIV = 16 Then
          AVRX_ADC_PRESC_DIV_Check = 1
          // assign the constant value
          SCRIPTADC_PRESC_DIV_gc = ADC_PRESC_DIV16_gc
        End If
        If AVRX_ADC_PRESC_DIV = 4 Then
          AVRX_ADC_PRESC_DIV_Check = 1
          // assign the constant value
          SCRIPTADC_PRESC_DIV_gc = ADC_PRESC_DIV4_gc
        End If

        If AVRX_ADC_PRESC_DIV_Check = 0 then
          Warning "Constant AVRX_ADC_PRESC_DIV4 can only be 16 or 4"
          SCRIPTADC_PRESC_DIV_gc = ADC_PRESC_DIV4_gc
        End if 

      End If
      

  End If

  IF NODEF(AD_ACQUISITION_TIME_SELECT_BITS) THEN
      AD_ACQUISITION_TIME_SELECT_BITS = 0b00000100  'set the three bits
  END IF

  IF NODEF(AD_DELAY) THEN
     'Acquisition time. Can be reduced in some circumstances - see PIC manual for details
     AD_DELAY = 2 10US
  END IF

  IF NODEF(AD_VREF_DELAY ) THEN
    'Delay for AVR VRef capacitor charging from internal reference
    'Increase if reference voltage is too low, or decrease for faster readings
    '(ATmega328p datasheet suggests 70 us, but this isn't enough for Arduino)
    'Only applies whe AD_REF_SOURCE is AD_REF_256
     AD_VREF_DELAY  = 1 ms
  END IF

  IF NODEF(AD_REF_SOURCE) THEN
     AD_REF_SOURCE = AD_REF_AVCC
  END IF


  IF NODEF(ADSpeed) THEN
     ADSPEED = MEDIUMSPEED
  END IF

  IF ChipFamily = 15002 Then 
    SCRIPT_16FDIFFENTIALADC = TRUE
  END IF
  IF ChipFamily = 15003 Then 
    SCRIPT_16FDIFFENTIALADC = TRUE
  END IF
  IF ChipFamily = 15004 Then 
    SCRIPT_16FDIFFENTIALADC = TRUE
  END IF
  
  IF NODEF(USE_AD0) THEN
     USE_AD0 = TRUE
  END IF
  IF NODEF(USE_AD1) THEN
     USE_AD1 = TRUE
  END IF
  IF NODEF(USE_AD2) THEN
     USE_AD2 = TRUE
  END IF
  IF NODEF(USE_AD3) THEN
     USE_AD3 = TRUE
  END IF
  IF NODEF(USE_AD4) THEN
     USE_AD4 = TRUE
  END IF
  IF NODEF(USE_AD5) THEN
     USE_AD5 = TRUE
  END IF
  IF NODEF(USE_AD6) THEN
     USE_AD6 = TRUE
  END IF
  IF NODEF(USE_AD7) THEN
     USE_AD7 = TRUE
  END IF
  IF NODEF(USE_AD8) THEN
     USE_AD8 = TRUE
  END IF
  IF NODEF(USE_AD9) THEN
     USE_AD9 = TRUE
  END IF
  IF NODEF(USE_AD10) THEN
     USE_AD10 = TRUE
  END IF
  IF NODEF(USE_AD11) THEN
     USE_AD11 = TRUE
  END IF
  IF NODEF(USE_AD12) THEN
     USE_AD12 = TRUE
  END IF
  IF NODEF(USE_AD13) THEN
     USE_AD13 = TRUE
  END IF
  IF NODEF(USE_AD14) THEN
     USE_AD14 = TRUE
  END IF
  IF NODEF(USE_AD15) THEN
     USE_AD15 = TRUE
  END IF
  IF NODEF(USE_AD16) THEN
     USE_AD16 = TRUE
  END IF
  IF NODEF(USE_AD17) THEN
     USE_AD17 = TRUE
  END IF
  IF NODEF(USE_AD18) THEN
     USE_AD18 = TRUE
  END IF
  IF NODEF(USE_AD19) THEN
     USE_AD19 = TRUE
  END IF
  IF NODEF(USE_AD20) THEN
     USE_AD20 = TRUE
  END IF
  IF NODEF(USE_AD21) THEN
     USE_AD21 = TRUE
  END IF
  IF NODEF(USE_AD22) THEN
     USE_AD22 = TRUE
  END IF
  IF NODEF(USE_AD23) THEN
     USE_AD23 = TRUE
  END IF
  IF NODEF(USE_AD24) THEN
     USE_AD24 = TRUE
  END IF
  IF NODEF(USE_AD25) THEN
     USE_AD25 = TRUE
  END IF
  IF NODEF(USE_AD26) THEN
     USE_AD26 = TRUE
  END IF
  IF NODEF(USE_AD27) THEN
     USE_AD27 = TRUE
  END IF
  IF NODEF(USE_AD28) THEN
     USE_AD28 = TRUE
  END IF
  IF NODEF(USE_AD29) THEN
     USE_AD29 = TRUE
  END IF
  IF NODEF(USE_AD30) THEN
     USE_AD30 = TRUE
  END IF
  IF NODEF(USE_AD31) THEN
     USE_AD31 = TRUE
  END IF
  IF NODEF(USE_AD32) THEN
     USE_AD32 = TRUE
  END IF
  IF NODEF(USE_AD33) THEN
     USE_AD33 = TRUE
  END IF
  IF NODEF(USE_AD34) THEN
     USE_AD34 = TRUE
  END IF


'16f1688x Chip family or style of chip
  IF NODEF(USE_ADA0) THEN
     USE_ADA0 = TRUE
  END IF
  IF NODEF(USE_ADA1) THEN
     USE_ADA1 = TRUE
  END IF
  IF NODEF(USE_ADA2) THEN
     USE_ADA2 = TRUE
  END IF
  IF NODEF(USE_ADA3) THEN
     USE_ADA3 = TRUE
  END IF
  IF NODEF(USE_ADA4) THEN
     USE_ADA4 = TRUE
  END IF
  IF NODEF(USE_ADA5) THEN
     USE_ADA5 = TRUE
  END IF
  IF NODEF(USE_ADA6) THEN
     USE_ADA6 = TRUE
  END IF
  IF NODEF(USE_ADA7) THEN
     USE_ADA7 = TRUE
  END IF
  IF NODEF(USE_ADB0) THEN
     USE_ADB0 = TRUE
  END IF
  IF NODEF(USE_ADB1) THEN
     USE_ADB1 = TRUE
  END IF
  IF NODEF(USE_ADB2) THEN
     USE_ADB2 = TRUE
  END IF
  IF NODEF(USE_ADB3) THEN
     USE_ADB3 = TRUE
  END IF
  IF NODEF(USE_ADB4) THEN
     USE_ADB4 = TRUE
  END IF
  IF NODEF(USE_ADB5) THEN
     USE_ADB5 = TRUE
  END IF
  IF NODEF(USE_ADB6) THEN
     USE_ADB6 = TRUE
  END IF
  IF NODEF(USE_ADB7) THEN
     USE_ADB7 = TRUE
  END IF
  IF NODEF(USE_ADC0) THEN
     USE_ADC0 = TRUE
  END IF
  IF NODEF(USE_ADC1) THEN
     USE_ADC1 = TRUE
  END IF
  IF NODEF(USE_ADC2) THEN
     USE_ADC2 = TRUE
  END IF
  IF NODEF(USE_ADC3) THEN
     USE_ADC3 = TRUE
  END IF
  IF NODEF(USE_ADC4) THEN
     USE_ADC4 = TRUE
  END IF
  IF NODEF(USE_ADC5) THEN
     USE_ADC5 = TRUE
  END IF
  IF NODEF(USE_ADC6) THEN
     USE_ADC6 = TRUE
  END IF
  IF NODEF(USE_ADC7) THEN
     USE_ADC7 = TRUE
  END IF
  IF NODEF(USE_ADD0) THEN
     USE_ADD0 = TRUE
  END IF
  IF NODEF(USE_ADD1) THEN
     USE_ADD1 = TRUE
  END IF
  IF NODEF(USE_ADD2) THEN
     USE_ADD2 = TRUE
  END IF
  IF NODEF(USE_ADD3) THEN
     USE_ADD3 = TRUE
  END IF
  IF NODEF(USE_ADD4) THEN
     USE_ADD4 = TRUE
  END IF
  IF NODEF(USE_ADD5) THEN
     USE_ADD5 = TRUE
  END IF
  IF NODEF(USE_ADD6) THEN
     USE_ADD6 = TRUE
  END IF
  IF NODEF(USE_ADD7) THEN
     USE_ADD7 = TRUE
  END IF
  IF NODEF(USE_ADE0) THEN
     USE_ADE0 = TRUE
  END IF
  IF NODEF(USE_ADE1) THEN
     USE_ADE1 = TRUE
  END IF
  IF NODEF(USE_ADE2) THEN
     USE_ADE2 = TRUE
  END IF
  IF NODEF(USE_ADE3) THEN
     USE_ADE3 = TRUE
  END IF
  IF NODEF(USE_ADE4) THEN
     USE_ADE4 = TRUE
  END IF

  'ACQUISITION = TIME. CAN BE REDUCED IN SOME CIRCUMSTANCES - SEE PIC MANUAL FOR DETAILS
  IF NODEF(AD_DELAY) THEN
     AD_DELAY 2 = 10US
  END IF

  'DELAY FOR AVR VREF CAPACITOR CHARGING FROM INTERNAL REFERENCE
  'INCREASE IF REFERENCE VOLTAGE IS = TOO LOW, OR DECREASE FOR FASTER READINGS
  '(ATMEGA328P DATASHEET SUGGESTS 70 US, BUT = THIS ISN'T ENOUGH FOR ARDUINO)
  'ONLY APPLIES WHE AD_REF_SOURCE IS AD_REF_256
  IF NODEF(AD_VREF_DELAY) THEN
     AD_VREF_DELAY = 1 MS
  END IF

  IF NODEF(AD_REF_SOURCE) THEN
     AD_REF_SOURCE = AD_REF_AVCC
  END IF

  SCRIPTREADAD10BITFORCEVARIANTDIFFENTIAL  = INT(ChipReadAD10BitForceVariant / 2)

#ENDSCRIPT
'End of User Defaults


#samebit GONDONE,GO_NOT_DONE, ADGO   'to support 16f15313 constants, and ADGO for 16F180xx family
#samebit FVRRDY, FVRST         'to support FVR on 18f


#define Format_Left 0
#define Format_Right 255
'#define SINGLE 255




#define HighSpeed 255
#define MediumSpeed 128
#define LowSpeed 0
#define InternalClock 192

'ADC reference sources (AVR only)
#define AD_REF_AREF 0
#define AD_REF_AVCC 1
#define AD_REF_256 3
#define AD_REF_4096 4
#define AD_REF_2048 5
#define AD_REF_1024 6

'Port names
'PIC style
#define AN0 0
#define AN1 1
#define AN2 2
#define AN3 3
#define AN4 4
#define AN5 5
#define AN6 6
#define AN7 7
#define AN8 8
#define AN9 9
#define AN10 10
#define AN11 11
#define AN12 12
#define AN13 13
#define AN14 14
#define AN15 15
#define AN16 16
#define AN17 17
#define AN18 18
#define AN19 19
#define AN20 20
#define AN21 21
#define AN22 22
#define AN23 23
#define AN24 24
#define AN25 25
#define AN26 26
#define AN27 27
#define AN28 28
#define AN29 29
#define AN30 30
#define AN31 31
#define AN32 32
#define AN33 33
#define AN34 34

#define ANA0 AN0
#define ANA1 AN1
#define ANA2 AN2
#define ANA3 AN3
#define ANA4 AN4
#define ANA5 AN5
#define ANA6 AN6
#define ANA7 AN7
#define ANB0 AN8
#define ANB1 AN9
#define ANB2 AN10
#define ANB3 AN11
#define ANB4 AN12
#define ANB5 AN13
#define ANB6 AN14
#define ANB7 AN15
#define ANC0 AN16
#define ANC1 AN17
#define ANC2 AN18
#define ANC3 AN19
#define ANC4 AN20
#define ANC5 AN21
#define ANC6 AN22
#define ANC7 AN23
#define AND0 AN24
#define AND1 AN25
#define AND2 AN26
#define AND3 AN27
#define AND4 AN28
#define AND5 AN29
#define AND6 AN30
#define AND7 AN31
#define ANE0 AN32
#define ANE1 AN33
#define ANE2 AN34

'AVR style
#define ADC0 0
#define ADC1 1
#define ADC2 2
#define ADC3 3
#define ADC4 4
#define ADC5 5
#define ADC6 6
#define ADC7 7
#define ADC8 8
#define ADC9 9
#define ADC10 10
#define ADC11 11
#define ADC12 12
#define ADC13 13




#script
    FVR_OFF = 0x00
    FVR_1x  = 0x01
    FVR_2x  = 0x02
    FVR_4x  = 0x03

    if bit(PVCFG1) Then
        FVR_1x  = 0x10
        FVR_2x  = 0x20
        FVR_4x  = 0x30
    end if

#endscript

macro LLReadAD (ADLeftAdjust)

  #IFDEF PIC

    #IFDEF DebugADC_H
        NOP 'Start of LLReadAD macro @DebugADC_H
    #ENDIF

    #IFDEF NoVar(ADCON3)  ' is not 16F188xx

      #IFDEF DebugADC_H
          NOP '@NoVar(ADCON3)@DebugADC_H
      #ENDIF

      #IFDEF NoVar(ANSEL)
        #IFDEF NoVar(ANSEL0)
          'Handle devices with no ANSEL0 AND no ANSEL
          #IFDEF NoVar(ANSELA)
            #IFDEF NoVar(ANSELB)

              #IFDEF NoBit(PCFG4)
                #IFDEF NoVar(ADCON2)
                  #IFDEF NoBit(ANS0)
                    'Example: 16F877A
                    #IFDEF Bit(PCFG3)
                      SET PCFG3 OFF
                    #ENDIF
                    SET PCFG2 OFF
                    SET PCFG1 OFF
                    SET PCFG0 OFF
                  #ENDIF
                  'Example: 10F220
                  #IFDEF Bit(ANS0)
                    SET ANS0 OFF
                    SET ANS1 OFF
                  #ENDIF
                #ENDIF

                #IFDEF Var(ADCON2)
                  #IFDEF BIT(PCFG3)
                    'ADCON2 and bit PCFG3 exist. Example: 18F4620
                    SET PCFG3 OFF
                    SET PCFG2 OFF
                    SET PCFG1 OFF
                    SET PCFG0 OFF
                  #ENDIF
                #ENDIF
              #ENDIF


              #IFDEF Bit(PCFG4)
                'PICs with PCFG4 and higher use ADCON1 as an ANSEL type register
                'Example: 18F1320
                'Some 18F8xxxx chips have error in chip definition
                'They claim to have PCFG4, but actually don't, can spot them by presence of ADCON2
                Dim AllANSEL As Byte Alias ADCON1
                AllANSEL = 0
                ADTemp = ADReadPort + 1
                Set C On
                Do
                  Rotate AllANSEL Left
                  decfsz ADTemp,F
                Loop
              #ENDIF

            #ENDIF
          #ENDIF


          #IFDEF Var(ANSELA)
            'Code for devices with ANSELA/ANSELB/ANSELE registers
            Select Case ADReadPort ' #IFDEF Var(ANSELA). ANSELA exists @DebugADC_H

              #IF ChipPins = 14  Or ChipPins = 8

                #IFDEF DebugADC_H
                    NOP 'ChipPins = 14  Or ChipPins = 8 @DebugADC_H
                #ENDIF

                #ifndef Bit(CHS5)
                    '#ifNdef Bit(CHS5)
                  #if var(ANSELA)
                      #ifdef USE_AD0 TRUE
                        Case 0: Set ANSELA.0 On
                      #endif
                      #ifdef USE_AD1 TRUE
                        Case 1: Set ANSELA.1 On
                      #endif
                      #ifdef USE_AD2 TRUE
                        Case 2: Set ANSELA.2 On
                      #endif
                      #ifdef USE_AD3 TRUE
                        Case 3: Set ANSELA.4 On
                      #endif
                      #ifdef USE_AD4 TRUE
                        Case 4: Set ANSELA.5 On
                      #endif
                  #endif
                  #if var(ANSELC)
                      #Ifdef USE_AD16 TRUE
                        Case 16: Set ANSELC.0 On
                      #endif
                      #Ifdef USE_AD17 TRUE
                        Case 17: Set ANSELC.1 On
                      #endif
                      #Ifdef USE_AD18 TRUE
                        Case 18: Set ANSELC.2 On
                      #endif
                      #Ifdef USE_AD19 TRUE
                        Case 19: Set ANSELC.3 On
                      #endif
                      #Ifdef USE_AD20 TRUE
                        Case 20: Set ANSELC.4 On
                      #endif
                      #Ifdef USE_AD21 TRUE
                        Case 21: Set ANSELC.5 On
                      #endif
                  #endif
                #endif '#ifNdef Bit(CHS5)

                #ifdef Bit(CHS5)
                    '#ifdef Bit(CHS5)
                  #if var(ANSELA)
                      #ifdef USE_AD0 TRUE
                        Case 0: Set ANSELA.0 On
                      #endif
                      #ifdef USE_AD1 TRUE
                        Case 1: Set ANSELA.1 On
                      #endif
                      #ifdef USE_AD2 TRUE
                        Case 2: Set ANSELA.2 On
                      #endif
                      #ifdef USE_AD3 TRUE
                        Case 3: Set ANSELA.4 On
                      #endif
                      #ifdef USE_AD4 TRUE
                        Case 4: Set ANSELA.5 On
                      #endif
                  #endif
                  #if var(ANSELC)
                      #Ifdef USE_AD16 TRUE
                        Case 16: Set ANSELC.0 On
                      #endif
                      #Ifdef USE_AD17 TRUE
                        Case 17: Set ANSELC.1 On
                      #endif
                      #Ifdef USE_AD18 TRUE
                        Case 18: Set ANSELC.2 On
                      #endif
                      #Ifdef USE_AD19 TRUE
                        Case 19: Set ANSELC.3 On
                      #endif
                      #Ifdef USE_AD20 TRUE
                        Case 20: Set ANSELC.4 On
                      #endif
                      #Ifdef USE_AD21 TRUE
                        Case 21: Set ANSELC.5 On
                      #endif
                  #endif
                #endif '#ifdef Bit(CHS5)


              #ENDIF

              #IF ChipPins = 20
              'ChipPins = 20
                #ifndef Bit(CHS5)
                    '#ifNdef Bit(CHS5)
                    #IFDEF DebugADC_H
                        NOP 'ChipPins = 20 @DebugADC_H
                    #ENDIF
                    #if var(ANSELA)
                        #ifdef USE_AD0 TRUE
                          Case 0: Set ANSELA.0 On
                        #endif
                        #ifdef USE_AD1 TRUE
                          Case 1: Set ANSELA.1 On
                        #endif
                        #ifdef USE_AD2 TRUE
                          Case 2: Set ANSELA.2 On
                        #endif
                        #ifdef USE_AD3 TRUE
                          Case 3: Set ANSELA.4 On
                        #endif
                    #endif
                    #if var(ANSELC)
                        #ifdef USE_AD4 TRUE
                          Case 4: Set ANSELc.0 On
                        #endif
                        #ifdef USE_AD5 TRUE
                          Case 5: Set ANSELc.1 On
                        #endif
                        #ifdef USE_AD6 TRUE
                          Case 6: Set ANSELc.2 On
                        #endif
                        #ifdef USE_AD7 TRUE
                          Case 7: Set ANSELc.3 On
                        #endif
                        #ifdef USE_AD8 TRUE
                          Case 8: Set ANSELc.6 On
                        #endif
                        #ifdef USE_AD9 TRUE
                          Case 9: Set ANSELc.7 On
                        #endif
                    #endif
                    #if var(ANSELB)
                        #Ifdef USE_AD10 TRUE
                          Case 10: Set ANSELb.4 On
                        #endif
                        #Ifdef USE_AD11 TRUE
                          Case 11: Set ANSELb.5 On
                        #endif
                    #endif
                #endif '#ifNdef Bit(CHS5)
                #ifdef Bit(CHS5)
                    '#ifdef Bit(CHS5)
                    #IFDEF DebugADC_H
                        NOP 'ChipPins = 20 @DebugADC_H
                    #ENDIF
                    #if var(ANSELA)
                        #ifdef USE_AD0 TRUE
                          Case 0: Set ANSELA.0 On
                        #endif
                        #ifdef USE_AD1 TRUE
                          Case 1: Set ANSELA.1 On
                        #endif
                        #ifdef USE_AD2 TRUE
                          Case 2: Set ANSELA.2 On
                        #endif
                        #ifdef USE_AD3 TRUE
                          Case 3: Set ANSELA.4 On
                        #endif
                        #ifdef USE_AD4 TRUE
                          Case 4: Set ANSELA.5 On
                        #endif
                    #endif
                    #if var(ANSELB)
                        #ifdef USE_AD12 TRUE
                          Case 12: Set ANSELb.4 On
                        #endif
                        #ifdef USE_AD13 TRUE
                          Case 13: Set ANSELb.5 On
                        #endif
                        #ifdef USE_AD14 TRUE
                          Case 14: Set ANSELb.6 On
                        #endif
                        #ifdef USE_AD15 TRUE
                          Case 15: Set ANSELb.7 On
                        #endif
                    #endif
                    #if var(ANSELC)
                        #ifdef USE_AD16 TRUE
                          Case 16: Set ANSELc.0 On
                        #endif
                        #ifdef USE_AD17 TRUE
                          Case 17: Set ANSELc.1 On
                        #endif
                        #Ifdef USE_AD18 TRUE
                          Case 18: Set ANSELc.2 On
                        #endif
                        #Ifdef USE_AD19 TRUE
                          Case 19: Set ANSELc.3 On
                        #endif
                        #Ifdef USE_AD20 TRUE
                          Case 20: Set ANSELc.4 On
                        #endif
                        #Ifdef USE_AD21 TRUE
                          Case 21: Set ANSELc.5 On
                        #endif
                        #Ifdef USE_AD22 TRUE
                          Case 22: Set ANSELc.6 On
                        #endif
                        #Ifdef USE_AD23 TRUE
                          Case 23: Set ANSELc.7 On
                        #endif
                     #endif
                #endif '#ifdef Bit(CHS5)

              #ENDIF


              #IF ChipPins = 18

                #IFDEF DebugADC_H
                    NOP 'ChipPins = 18 @DebugADC_H
                #ENDIF
                #if var(ANSELA)
                    #ifdef USE_AD0 TRUE
                      Case 0: Set ANSELA.0 On
                    #endif
                    #ifdef USE_AD1 TRUE
                      Case 1: Set ANSELA.1 On
                    #endif
                    #ifdef USE_AD2 TRUE
                      Case 2: Set ANSELA.2 On
                    #endif
                    #ifdef USE_AD3 TRUE
                      Case 3: Set ANSELA.3 On
                    #endif
                    #ifdef USE_AD4 TRUE
                      Case 4: Set ANSELA.4 On
                    #endif
                #endif
                #if var(ANSELB)
                    #ifdef USE_AD11 TRUE
                      Case 11: Set ANSELB.1 On
                    #endif
                    #ifdef USE_AD10 TRUE
                      Case 10: Set ANSELB.2 On
                    #endif
                    #ifdef USE_AD9 TRUE
                      Case 9: Set ANSELB.3 On
                    #endif
                    #ifdef USE_AD8 TRUE
                      Case 8: Set ANSELB.4 On
                    #endif
                    #ifdef USE_AD7 TRUE
                      Case 7: Set ANSELB.5 On
                    #endif
                    #ifdef USE_AD5 TRUE
                      Case 5: Set ANSELB.6 On
                    #endif
                    #ifdef USE_AD6 TRUE
                      Case 6: Set ANSELB.7 On
                    #endif
                #endif
              #ENDIF

              #IF ChipPins = 28 Or ChipPins = 40

                #ifndef Bit(CHS5)
                    ' #ifNdef Bit(CHS5) ChipPins = 28  Or ChipPins = 40 @DebugADC_H
                    #IFDEF DebugADC_H
                        NOP 'ChipPins = 28  Or ChipPins = 40 @DebugADC_H
                    #ENDIF

                    #if var(ANSELA)
                        #Ifdef USE_AD0 TRUE
                          Case 0: Set ANSELA.0 On
                        #endif
                        #Ifdef USE_AD1 TRUE
                          Case 1: Set ANSELA.1 On
                        #endif
                        #Ifdef USE_AD2 TRUE
                          Case 2: Set ANSELA.2 On
                        #endif
                        #Ifdef USE_AD3 TRUE
                          Case 3: Set ANSELA.3 On
                        #endif
                        #Ifdef USE_AD4 TRUE
                          Case 4: Set ANSELA.5 On
                        #endif
                    #endif

                    #IFDEF Var(ANSELB)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELB). ANSELB exists @DebugADC_H
                      #ENDIF

                      #Ifdef USE_AD12 TRUE
                         Case 12: Set ANSELB.0 On
                      #endif

                      #Ifdef USE_AD10 TRUE
                        Case 10: Set ANSELB.1 On
                      #endif

                      #Ifdef USE_AD8 TRUE
                        Case 8: Set ANSELB.2 On
                      #endif

                      #Ifdef USE_AD9 TRUE
                        Case 9: Set ANSELB.3 On
                      #endif

                      #Ifdef USE_AD11 TRUE
                        Case 11: Set ANSELB.4 On
                      #endif

                      #Ifdef USE_AD13 TRUE
                        Case 13: Set ANSELB.5 On
                      #endif

                    #ENDIF

                    #IFDEF Var(ANSELC)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELC). ANSELC exists @DebugADC_H
                      #ENDIF


                      #Ifdef USE_AD14 TRUE
                        Case 14: Set ANSELC.2 On
                      #endif
                      #Ifdef USE_AD15 TRUE
                        Case 15: Set ANSELC.3 On
                      #endif
                      #Ifdef USE_AD16 TRUE
                        Case 16: Set ANSELC.4 On
                      #endif
                      #Ifdef USE_AD17 TRUE
                        Case 17: Set ANSELC.5 On
                      #endif
                      #Ifdef USE_AD18 TRUE
                        Case 18: Set ANSELC.6 On
                      #endif
                      #Ifdef USE_AD19 TRUE
                        Case 19: Set ANSELC.7 On
                      #endif
                    #ENDIF

                    #IFDEF Var(ANSELD)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELD). ANSELD exists @DebugADC_H
                      #ENDIF

                      #Ifdef USE_AD20 TRUE
                        Case 20: Set ANSELD.0 On
                      #endif
                      #Ifdef USE_AD21 TRUE
                        Case 21: Set ANSELD.1 On
                      #endif
                      #Ifdef USE_AD22 TRUE
                        Case 22: Set ANSELD.2 On
                      #endif
                      #Ifdef USE_AD23 TRUE
                        Case 23: Set ANSELD.3 On
                      #endif
                      #Ifdef USE_AD24 TRUE
                        Case 24: Set ANSELD.4 On
                      #endif
                      #Ifdef USE_AD25 TRUE
                        Case 25: Set ANSELD.5 On
                      #endif
                      #Ifdef USE_AD26 TRUE
                        Case 26: Set ANSELD.6 On
                      #endif
                      #Ifdef USE_AD27 TRUE
                        Case 27: Set ANSELD.7 On
                      #endif
                    #ENDIF

                    #IFDEF Var(ANSELE)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELE). ANSELE exists @DebugADC_H
                      #ENDIF

                      #Ifdef USE_AD5 TRUE
                        Case 5: Set ANSELE.0 On
                      #endif
                      #Ifdef USE_AD6 TRUE
                        Case 6: Set ANSELE.1 On
                      #endif
                      #Ifdef USE_AD7 TRUE
                        Case 7: Set ANSELE.2 On
                      #endif
                    #ENDIF

                #endif  'Bit(CHS5)


                #ifdef Bit(CHS5)
                    ' #ifdef Bit(CHS5) ChipPins = 28  Or ChipPins = 40 @DebugADC_H
                    #IFDEF DebugADC_H
                        NOP 'ChipPins = 28  Or ChipPins = 40 @DebugADC_H
                    #ENDIF

                    #Ifdef USE_AD0 TRUE
                      Case 0: Set ANSELA.0 On
                    #endif
                    #Ifdef USE_AD1 TRUE
                      Case 1: Set ANSELA.1 On
                    #endif
                    #Ifdef USE_AD2 TRUE
                      Case 2: Set ANSELA.2 On
                    #endif
                    #Ifdef USE_AD3 TRUE
                      Case 3: Set ANSELA.3 On
                    #endif
                    #Ifdef USE_AD4 TRUE
                      Case 4: Set ANSELA.4 On
                    #endif
                    #Ifdef USE_AD5 TRUE
                      Case 5: Set ANSELA.5 On
                    #endif
                    #Ifdef USE_AD6 TRUE
                      Case 6: Set ANSELA.6 On
                    #endif
                    #Ifdef USE_AD7 TRUE
                      Case 7: Set ANSELA.7 On
                    #endif

                    #IFDEF Var(ANSELB)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELB). ANSELB exists @DebugADC_H
                      #ENDIF

                      #Ifdef USE_AD8 TRUE
                         Case 8: Set ANSELB.0 On
                      #endif

                      #Ifdef USE_AD9 TRUE
                        Case 9: Set ANSELB.1 On
                      #endif

                      #Ifdef USE_AD10 TRUE
                        Case 10: Set ANSELB.2 On
                      #endif

                      #Ifdef USE_AD11 TRUE
                        Case 11: Set ANSELB.3 On
                      #endif

                      #Ifdef USE_AD12 TRUE
                        Case 12: Set ANSELB.4 On
                      #endif

                      #Ifdef USE_AD13 TRUE
                        Case 13: Set ANSELB.5 On
                      #endif

                      #Ifdef USE_AD14 TRUE
                        Case 14: Set ANSELB.6 On
                      #endif

                      #Ifdef USE_AD15 TRUE
                        Case 15: Set ANSELB.7 On
                      #endif


                    #ENDIF

                    #IFDEF Var(ANSELC)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELC). ANSELC exists @DebugADC_H
                      #ENDIF


                      #Ifdef USE_AD16 TRUE
                        Case 16: Set ANSELC.0 On
                      #endif
                      #Ifdef USE_AD17 TRUE
                        Case 17: Set ANSELC.1 On
                      #endif
                      #Ifdef USE_AD18 TRUE
                        Case 18: Set ANSELC.2 On
                      #endif
                      #Ifdef USE_AD19 TRUE
                        Case 19: Set ANSELC.3 On
                      #endif
                      #Ifdef USE_AD20 TRUE
                        Case 20: Set ANSELC.4 On
                      #endif
                      #Ifdef USE_AD21 TRUE
                        Case 21: Set ANSELC.5 On
                      #endif
                      #Ifdef USE_AD22 TRUE
                        Case 22: Set ANSELC.6 On
                      #endif
                      #Ifdef USE_AD23 TRUE
                        Case 23: Set ANSELC.7 On
                      #endif
                    #ENDIF

                    #IFDEF Var(ANSELD)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELD). ANSELD exists @DebugADC_H
                      #ENDIF

                      #Ifdef USE_AD24 TRUE
                        Case 24: Set ANSELD.0 On
                      #endif
                      #Ifdef USE_AD25 TRUE
                        Case 25: Set ANSELD.1 On
                      #endif
                      #Ifdef USE_AD26 TRUE
                        Case 26: Set ANSELD.2 On
                      #endif
                      #Ifdef USE_AD27 TRUE
                        Case 27: Set ANSELD.3 On
                      #endif
                      #Ifdef USE_AD28 TRUE
                        Case 28: Set ANSELD.4 On
                      #endif
                      #Ifdef USE_AD29 TRUE
                        Case 29: Set ANSELD.5 On
                      #endif
                      #Ifdef USE_AD30 TRUE
                        Case 30: Set ANSELD.6 On
                      #endif
                      #Ifdef USE_AD31 TRUE
                        Case 31: Set ANSELD.7 On
                      #endif
                    #ENDIF

                    #IFDEF Var(ANSELE)

                      #IFDEF DebugADC_H
                          NOP '#IFDEF Var(ANSELE). ANSELE exists @DebugADC_H
                      #ENDIF
                      #Ifdef USE_AD32 TRUE
                        Case 32: Set ANSELE.0 On
                      #endif
                      #Ifdef USE_AD33 TRUE
                        Case 33: Set ANSELE.1 On
                      #endif
                      #Ifdef USE_AD34 TRUE
                        Case 34: Set ANSELE.2 On
                      #endif

                    #ENDIF

                #endif  'Bit(CHS5)

              #ENDIF  'pins28 or 40

            End Select  'End Select #1

          #ENDIF


        #ENDIF
      #ENDIF


      #IFDEF Var(ANSEL)
        'Code for PICs with with ANSEL register
        #IFDEF DebugADC_H
            NOP '#IFDEF Var(ANSEL). ANSEL exists @DebugADC_H
        #ENDIF

        #IFDEF Var(ANSELH)
          Dim AllANSEL As Word Alias ANSELH, ANSEL
        #ENDIF
        #IFDEF NoVar(ANSELH)
          Dim AllANSEL As Byte Alias ANSEL
        #ENDIF
        AllANSEL = 0
        ADTemp = ADReadPort + 1
        Set C On
        Do
          Rotate AllANSEL Left
          decfsz ADTemp,F
        Loop

      #ENDIF
      #IFDEF Var(ANSEL0)
       'Code for 18F4431 family parts, uses ANSEL0 and ANSEL1
        #IFDEF DebugADC_H
            NOP '#IFDEF Var(ANSEL0). ANSEL0 exists @DebugADC_H
        #ENDIF

        #IFDEF Var(ANSEL1)
          Dim AllANSEL As Word Alias ANSEL1, ANSEL0
        #ENDIF
        #IFDEF NoVar(ANSEL1)
          Dim AllANSEL As Byte Alias ANSEL0
        #ENDIF
        AllANSEL = 0
        ADTemp = ADReadPort + 1
        Set C On
        Do
          Rotate AllANSEL Left
          decfsz ADTemp,F
        Loop

      #ENDIF



      #IFDEF Bit(ACONV)
        'Set Auto or Single Convert Mode
        #IFDEF DebugADC_H
            NOP '#IFDEF Bit(ACONV). Set Auto or Single Convert Mode @DebugADC_H
        #ENDIF

        SET ACONV OFF  'Single shot mode
        SET ACSCH OFF  'Single channel CONVERSION

        IF ADReadPort = 0 OR ADReadPort = 4 OR ADReadPort = 8 Then
          'GroupA
          SET ACMOD1 OFF
          SET ACMOD0 OFF
        END IF

        IF ADReadPort = 1 OR ADReadPort = 5 Then
          'GroupB
          SET ACMOD1 OFF
          SET ACMOD0 ON
        END IF

        IF ADReadPort = 2 OR ADReadPort = 6 Then
          'GroupC
          SET ACMOD1 ON
          SET ACMOD0 OFF
        END IF

        IF ADReadPort = 3 OR ADReadPort = 7 Then
          'GroupD
          SET ACMOD1 ON
          SET ACMOD0 ON
        END IF

      #ENDIF


      #IFDEF Bit(ADCS0)

        #IFDEF DebugADC_H
            NOP '#IFDEF Bit(ADCS0). Set conversion clock @DebugADC_H
        #ENDIF

        #IFDEF ADSpeed HighSpeed  'set to  FOSC/2
          #IFDEF DebugADC_H
              NOP 'ADSpeed HighSpeed @DebugADC_H 'set to  FOSC/2
          #ENDIF

          #IFDEF BIT(ADCS2)
            'Later microcontrollers have three bits not two bites. Set conversion clock - to handle ADCS2
            SET ADCS2 OFF
          #ENDIF
          SET ADCS1 OFF
          SET ADCS0 OFF
        #ENDIF

        #IFDEF ADSpeed MediumSpeed  'set to  FOSC/8
          #IFDEF DebugADC_H
              NOP 'ADSpeed MediumSpeed @DebugADC_H  'set to  FOSC/8
          #ENDIF
          #IFDEF BIT(ADCS2)
            SET ADCS2 OFF
          #ENDIF
          SET ADCS1 OFF
          SET ADCS0 ON
        #ENDIF

        #IFDEF ADSpeed LowSpeed   'set to  FOSC/32
          #IFDEF DebugADC_H
              NOP 'ADSpeed LowSpeed @DebugADC_H 'set to  FOSC/32
          #ENDIF
          #IFDEF BIT(ADCS2)
            SET ADCS2 OFF
          #ENDIF
          SET ADCS1 ON
          SET ADCS0 OFF
        #ENDIF
        #IFDEF ADSpeed InternalClock
          #IFDEF DebugADC_H
              NOP 'ADSpeed InternalClock @DebugADC_H
          #ENDIF
          #IFDEF BIT(ADCS2)
            SET ADCS2 OFF
          #ENDIF
          SET ADCS1 ON
          SET ADCS0 ON
        #ENDIF
      #ENDIF

      'Choose port
      #IFDEF Bit(CHS0)

        #IFDEF DebugADC_H
            NOP '#IFDEF Bit(CHS0). Clear channels bits. @DebugADC_H
        #ENDIF

        SET CHS0 OFF
        SET CHS1 OFF
        #IFDEF Bit(CHS2)
          SET CHS2 OFF
          #IFDEF Bit(CHS3)
            SET CHS3 OFF
            #IFDEF Bit(CHS4)
              SET CHS4 OFF
              #IFDEF Bit(CHS5)
                SET CHS5 OFF
              #ENDIF
            #ENDIF
          #ENDIF
        #ENDIF

        #IFDEF DebugADC_H
            NOP 'IF ADReadPort.0 thru to IF ADReadPort.4 to set ADCON0. Set channel bits. @DebugADC_H
        #ENDIF
        IF ADReadPort.0 On Then Set CHS0 On
        IF ADReadPort.1 On Then Set CHS1 On
        #IFDEF Bit(CHS2)
          IF ADReadPort.2 On Then Set CHS2 On
          #IFDEF Bit(CHS3)
            If ADReadPort.3 On Then Set CHS3 On
            #IFDEF Bit(CHS4)
              If ADReadPort.4 On Then Set CHS4 On
              #IFDEF Bit(CHS5)
                If ADReadPort.5 On Then Set CHS5 On
              #ENDIF
            #ENDIF
          #ENDIF
        #ENDIF
        #IFDEF DebugADC_H
            NOP 'End of ADReadPort.0 thru to IF ADReadPort.4 to set ADCON0. End of setting channel bits @DebugADC_H
        #ENDIF
      #ENDIF


      #IFDEF BIT(GASEL0)

        #IFDEF DebugADC_H
            NOP 'BIT(GASEL0). GROUP A SELECT BITS. @DebugADC_H
        #ENDIF

        'GROUP A SELECT BITS
        IF ADReadPort = 0 THEN
          SET GASEL1 OFF
          SET GASEL0 OFF
        END IF
        IF ADReadPort = 4 THEN
          SET GASEL1 OFF
          SET GASEL0 ON
        END IF
        IF ADReadPort = 8 THEN
          SET GASEL1 ON
          SET GASEL0 OFF
        END IF
        'GROUP C SELECT BITS
        IF ADReadPort = 2 THEN
          SET GCSEL1 OFF
          SET GCSEL0 OFF
        END IF
        IF ADReadPort = 6 THEN
          SET GCSEL1 OFF
          SET GCSEL0 ON
        END IF
        'GROUP B SELECT BITS
        IF ADReadPort = 1 THEN
          SET GBSEL1 OFF
          SET GBSEL0 OFF
        END IF
        IF ADReadPort = 5 THEN
          SET GBSEL1 OFF
          SET GBSEL0 ON
        END IF
        'GROUP D SELECT BITS
        IF ADReadPort = 3 THEN
          SET GDSEL1 OFF
          SET GDSEL0 OFF
        END IF
        IF ADReadPort = 7 THEN
          SET GDSEL1 OFF
          SET GDSEL0 ON
        END IF
      #ENDIF

     #ENDIF

     #IFDEF Var(ADCON3)
        //~For chips with ADCON3 ***
        #IFDEF DebugADC_H
            'Var(ADCON3). Configure ANSELA/B/C/D. @DebugADC_H
            NOP
        #ENDIF

       'Configure ANSELA/B/C/D @DebugADC_H
        #IFNDEF Var(ADCHS)
        Select Case ADReadPort
            #IFDEF DebugADC_H
                NOP 'Set ANSEL Bits. @DebugADC_H
            #ENDIF

            #ifdef USE_ADA0 TRUE
              Case 0: Set ANSELA.0 On
            #endif
            #ifdef USE_ADA1 TRUE
              Case 1: Set ANSELA.1 On
            #endif
            #ifdef USE_ADA2 TRUE
              Case 2: Set ANSELA.2 On
            #endif
            #ifdef USE_ADA3 TRUE
              Case 3: Set ANSELA.3 On
            #endif
            #ifdef USE_ADA4 TRUE
              Case 4: Set ANSELA.4 ON
            #endif
            #ifdef USE_ADA5 TRUE
              Case 5: Set ANSELA.5 On
            #endif
            #ifdef USE_ADA6 TRUE
              Case 6: Set ANSELA.6 On
            #endif
            #ifdef USE_ADA7 TRUE
              Case 7: Set ANSELA.7 On
            #endif

            #ifdef USE_ADB0 TRUE
              Case 8: Set ANSELB.0 On
            #endif
            #ifdef USE_ADB1 TRUE
              Case 9: Set ANSELB.1 On
            #endif
            #ifdef USE_ADB2 TRUE
              Case 10: Set ANSELB.2 On
            #endif
            #ifdef USE_ADB3 TRUE
              Case 11: Set ANSELB.3 On
            #endif
             #Ifdef USE_ADB4 TRUE
              Case 12: Set ANSELB.4 On
            #endif
            #Ifdef USE_ADB5 TRUE
              Case 13: Set ANSELB.5 On
            #endif
            #Ifdef USE_ADB6 TRUE
              Case 14: Set ANSELB.6 On
            #endif
            #Ifdef USE_ADB7 TRUE
              Case 15: Set ANSELB.7 On
            #endif

            #Ifdef USE_ADC0 TRUE
              Case 16: Set ANSELC.0 On
            #endif
            #Ifdef USE_ADC1 TRUE
              Case 17: Set ANSELC.1 On
            #endif
            #Ifdef USE_ADC2 TRUE
              Case 18: Set ANSELC.2 On
            #endif
            #Ifdef USE_ADC3 TRUE
              Case 19: Set ANSELC.3 On
            #endif
            #Ifdef USE_ADC4 TRUE
              Case 20: Set ANSELC.4 On
            #endif
            #Ifdef USE_ADC5 TRUE
              Case 21: Set ANSELC.5 On
            #endif
            #Ifdef USE_ADC6 TRUE
              Case 22: Set ANSELC.6 On
            #endif
            #Ifdef USE_ADC7 TRUE
              Case 23: Set ANSELC.7 On
            #endif

            #IFDEF Var(ANSELD)

                #Ifdef USE_ADD0 TRUE
                  Case 24: Set ANSELD.0 On
                #endif
                #Ifdef USE_ADD1 TRUE
                  Case 25: Set ANSELD.1 On
                #endif
                #Ifdef USE_ADD2 TRUE
                  Case 26: Set ANSELD.2 On
                #endif
                #Ifdef USE_ADD3 TRUE
                  Case 27: Set ANSELD.3 On
                #endif
                #Ifdef USE_ADD4 TRUE
                  Case 28: Set ANSELD.4 On
                #endif
                #Ifdef USE_ADD5 TRUE
                  Case 29: Set ANSELD.5 On
                #endif
                #Ifdef USE_ADD6 TRUE
                  Case 30: Set ANSELD.6 On
                #endif
                #Ifdef USE_ADD7 TRUE
                  Case 31: Set ANSELD.7 On
                #endif
            #ENDIF

            #IFDEF Var(ANSELE)

              #Ifdef USE_AD32 TRUE
                Case 32: Set ANSELE.0 On
              #endif
              #Ifdef USE_AD33 TRUE
                Case 33: Set ANSELE.1 On
              #endif
              #Ifdef USE_AD34 TRUE
                Case 34: Set ANSELE.2 On
              #endif
            #ENDIF
          End Select  '*** ANSEL Bits should now be set ***
        #ENDIF
        #IFDEF Var(ADCHS)
            Select Case ADReadPort
              #IFDEF DebugADC_H
                  NOP 'Set ANSEL Bits. @DebugADC_H
              #ENDIF
              ADCHS = 0
              #ifdef USE_ADA0 TRUE
                Case 0: Set ANSEL0.0 On
              #endif
              #ifdef USE_ADA1 TRUE
                Case 1: Set ANSEL0.1 On:
              #endif
              #ifdef USE_ADA2 TRUE
                Case 2: Set ANSEL0.2 On
              #endif
              #ifdef USE_ADA3 TRUE
                Case 3: Set ANSEL0.3 On
              #endif
              #ifdef USE_ADA4 TRUE
                Case 4: Set ANSEL0.4 ON:ADCHS.0 = 1
              #endif
              #ifdef USE_ADA5 TRUE
                Case 5: Set ANSEL0.5 On:ADCHS.4 = 1
              #endif
              #ifdef USE_ADA6 TRUE
                Case 6: Set ANSEL0.6 On:ADCHS.2 = 1
              #endif
              #ifdef USE_ADA7 TRUE
                Case 7: Set ANSEL0.7 On:ADCHS.6 = 1
              #endif
        
              #ifdef USE_ADB0 TRUE
                Case 8: Set ANSEL1.0 On:ADCHS.1 = 1
              #endif
          End Select  '*** ANSEL Bits should now be set ***
'erv 1591
          #IFDEF Bit(ACONV) '
          'erv 1593
            'Set Auto or Single Convert Mode
        #IFDEF DebugADC_H
            NOP 'ANSEL Bits are now set . @DebugADC_H
        #ENDIF

            SET ACONV OFF  'Single shot mode
            SET ACSCH OFF  'Single channel CONVERSION

            IF ADReadPort = 0 OR ADReadPort = 4 OR ADReadPort = 8 Then
              'GroupA
              SET ACMOD1 OFF
              SET ACMOD0 OFF
            END IF

            IF ADReadPort = 1 OR ADReadPort = 5 Then
              'GroupB
              SET ACMOD1 OFF
              SET ACMOD0 ON
            END IF
      
            IF ADReadPort = 2 OR ADReadPort = 6 Then
              'GroupC
              SET ACMOD1 ON
              SET ACMOD0 OFF
            END IF
       
            IF ADReadPort = 3 OR ADReadPort = 7 Then
              'GroupD
              SET ACMOD1 ON
              SET ACMOD0 ON
            END IF

          #ENDIF

        #ENDIF
        '*** ANSEL Bits are now set ***
       #IFDEF DebugADC_H 
            NOP 'ANSEL Bits are now set . @DebugADC_H
        #ENDIF



       'Set voltage reference
       'ADREF = 0  'Default = 0 /Vref+ = Vdd/ Vref-  = Vss

        
        //~Set voltage reference
        //~ADREF = 0  'Default = 0 /Vref+ = Vdd/ Vref-  = Vss
		#IFDEF DebugADC_H    
			NOP 'Start of conversion Clock Speed. @DebugADC_H
        #ENDIF

        #IFDEF VAR(ADCLK)

            'Configure AD clock defaults
            Set ADCS off 'Clock source = FOSC/ADCLK
            ADCLK = 1 ' default to FOSC/2

            'Conversion Clock Speed
            #IFDEF ADSpeed HighSpeed
             Set ADCS OFF   ' ADCON0.4
             ADCLK = 1      ' FOSC/2
            #ENDIF

            #IFDEF ADSpeed MediumSpeed
             SET ADCS OFF  'ADCON0.4
             ADCLK = 15    'FOSC/16

            #ENDIF

            #IFDEF ADSpeed LowSpeed
             SET ADCS OFF  ' ADCON0.4
             ADCLK = 31    ' FOSC/32
            #ENDIF
        #ENDIF

        #IFNDEF ADCLK

          #IF ChipFamily = 16
            'Configure AD clock defaults fpr older 18F that do not have ADCLK

            #IFDEF DebugADC_H
                NOP '#IFDEF Bit(ADCS0). Set conversion clock @DebugADC_H
            #ENDIF

            #IFDEF ADSpeed HighSpeed  'set to  FOSC/2
              #IFDEF DebugADC_H
                  NOP 'ADSpeed HighSpeed @DebugADC_H 'set to  FOSC/2
              #ENDIF

              #IFDEF BIT(ADCS2)
                'Later microcontrollers have three bits not two bites. Set conversion clock - to handle ADCS2
                SET ADCS2 OFF
              #ENDIF
              SET ADCS1 OFF
              SET ADCS0 OFF
            #ENDIF

            #IFDEF ADSpeed MediumSpeed  'set to  FOSC/8
              #IFDEF DebugADC_H
                  NOP 'ADSpeed MediumSpeed @DebugADC_H  'set to  FOSC/8
              #ENDIF
              #IFDEF BIT(ADCS2)
                SET ADCS2 OFF
              #ENDIF
              SET ADCS1 OFF
              SET ADCS0 ON
            #ENDIF

            #IFDEF ADSpeed LowSpeed   'set to  FOSC/32
              #IFDEF DebugADC_H
                  NOP 'ADSpeed LowSpeed @DebugADC_H 'set to  FOSC/32
              #ENDIF
              #IFDEF BIT(ADCS2)
                SET ADCS2 OFF
              #ENDIF
              SET ADCS1 ON
              SET ADCS0 OFF
            #ENDIF

            #IFDEF ADSpeed InternalClock
              #IFDEF DebugADC_H
                  NOP 'ADSpeed InternalClock @DebugADC_H
              #ENDIF
              #IFDEF BIT(ADCS2)
                SET ADCS2 OFF
              #ENDIF
              SET ADCS1 ON
              SET ADCS0 ON
            #ENDIF

          #ENDIF

        #ENDIF


        #IFDEF DebugADC_H
            NOP 'End of conversion Clock Speed. @DebugADC_H
        #ENDIF


        #IFDEF ADSpeed InternalClock
          #IFDEF DebugADC_H
              NOP '#IFDEF ADSpeed InternalClock. @DebugADC_H
          #ENDIF

          SET ADCS ON 'ADCLK has no effect
        #ENDIF

        #IFDEF DebugADC_H
            NOP 'Result formatting for 8 bit or 10 bit. @DebugADC_H
        #ENDIF

       'Result formatting
        if ADLeftadjust = 0 then  
           ' Set ADCON0.2 ON
           #ifdef bit(ADFM)
              Set ADFM ON
           #endif

           #ifdef bit(ADFM0)
              Set ADFM0 ON
           #endif
        Else
           ' Set ADCON.2 off     '8-bit

           #ifdef bit(ADFM)
              Set ADFM OFF
           #endif

           #ifdef bit(ADFM0)
              Set ADFM0 OFF
           #endif

        End if

        #IFDEF DebugADC_H
            NOP 'ADPCH = ADReadPort  'Configure AD read Channel. @DebugADC_H
        #ENDIF

        #IFDEF NoVar(ADCHS)
          'Select Channel
          ADPCH = ADReadPort  'Configure AD read Channel
        #Else
          'Chips with ADCHS use ACMOD1/ACMOD0 (see code section above)
     #ENDIF   

    #ENDIF   'End of 16F188x Section

    #IFDEF DebugADC_H
        NOP '@Start of ADReadPreReadCommand. @DebugADC_H
    #ENDIF

    #ifdef ADReadPreReadCommand
        ADReadPreReadCommand  'user commands are added here
    #endif

    #IFDEF DebugADC_H
        NOP '@End of ADReadPreReadCommand. @DebugADC_H
    #ENDIF


    #IFDEF DebugADC_H
        NOP 'Enabling A/D. @DebugADC_H
    #ENDIF

    'Enable AD Operations
    SET ADON ON

    #IFDEF DebugADC_H
        NOP 'Acquisition Delay. @DebugADC_H
    #ENDIF

    'Acquisition Delay
    Wait AD_Delay

    #IFDEF DebugADC_H
        NOP 'Read A/D. @DebugADC_H
    #ENDIF



    #ifdef bit(GO_NOT_DONE)
      'Read A/D @1
      SET GO_NOT_DONE ON
      nop
      Wait While GO_NOT_DONE ON
    #endif

    #ifndef bit(GO_NOT_DONE)
      #IFDEF Bit(GO_DONE)
        'Read A/D @2
        SET GO_DONE ON
        Wait While GO_DONE ON
      #ENDIF
      #IFNDEF Bit(GO_DONE)
        #IFDEF Bit(GO)
          'Read A/D @3
          SET ADCON0.GO ON
          Wait While ADCON0.GO ON
        #ENDIF
      #ENDIF
    #endif

    'Switch off A/D
    #IFDEF Var(ADCON0)
      SET ADCON0.ADON OFF
      #IFDEF NoVar(ANSEL)
        #IFDEF NoVar(ANSELA)

          #IFDEF NoBit(PCFG4)
            #IFDEF NoVar(ADCON2)
              #IFDEF NoBit(ANS0)
                #IFDEF Bit(PCFG3)
                  SET PCFG3 OFF
                #ENDIF
                SET PCFG2 ON
                SET PCFG1 ON
                SET PCFG0 OFF
              #ENDIF
              #IFDEF Bit(ANS0)
                SET ANS0 OFF
                SET ANS1 OFF
              #ENDIF
            #ENDIF

            #IFDEF Var(ADCON2)
              #IFDEF BIT(PCFG3)
                SET PCFG3 ON
                SET PCFG2 ON
                SET PCFG1 ON
                SET PCFG0 ON
              #ENDIF
            #ENDIF
          #ENDIF


          #IFDEF Bit(PCFG4)
            'For 18F1320, which uses ADCON1 as an ANSEL register
            ADCON1 = 0
          #ENDIF
        #ENDIF
      #ENDIF
    #ENDIF


    #IFDEF Var(ANSEL)      'Clear whatever ANSEL variants the chip has
      ANSEL = 0
    #ENDIF
    #IFDEF Var(ANSELH)
      ANSELH = 0
    #ENDIF
    #IFDEF Var(ANSEL0)
      ANSEL0 = 0
    #ENDIF
    #IFDEF Var(ANSEL1)
      ANSEL1 = 0
    #ENDIF
    #IFDEF Var(ANSELA)
      ANSELA = 0
    #ENDIF
    #IFDEF Var(ANSELB)
      ANSELB = 0
    #ENDIF
    #IFDEF Var(ANSELC)
      ANSELC = 0
    #ENDIF
    #IFDEF Var(ANSELD)
      ANSELD = 0
    #ENDIF
    #IFDEF Var(ANSELE)
      ANSELE = 0
    #ENDIF

  #ENDIF


  #IFDEF AVR

   #IF ChipADC > 0
    'Select channel by setting ADMUX
    #IFNDEF Bit(MUX5)
      ADMUX = ADReadPort
    #endif
    #ifdef Bit(MUX5)
      #ifdef NoBit(ADATE)
        ADCSRB.MUX5 = ADReadPort.5
        ADMUX = ADReadPort And 0x1F
      #endif
      #ifdef Bit(ADATE)
        ADMUX = 0
        MUX5 = ADReadPort.5
        MUX4 = ADReadPort.4
        MUX3 = ADReadPort.3
        MUX2 = ADReadPort.2
        MUX1 = ADReadPort.1
        MUX0 = ADReadPort.0
      #endif
    #endif
    #ifdef Bit(ADLAR)
        ADLAR = ADLeftAdjust
    #endif




    #ifndef Bit(REFS2)

      #if ChipFamily <> 121

        'Select reference source for chips that DO NOT HAVE Bit(REFS2)
        If AD_REF_SOURCE = AD_REF_AVCC Then
            #ifndef Bit(REFS1)
              #ifdef Bit(REFS0)
                ASM showdebug  'Bit(REFS1) does not exist, so assume 'VCC used as analog reference' REFS0=b'0'
                [canskip]REFS0=b'0'
              #endif
            #endif
            #ifdef Bit(REFS1)
             #IFDEF Oneof(CHIP_tiny13, CHIP_tiny13a, CHIP_tiny13, CHIP_tiny1634, CHIP_tiny167, CHIP_tiny20, CHIP_tiny2313, CHIP_tiny24, CHIP_tiny24a, CHIP_tiny25, CHIP_tiny25, CHIP_tiny26, CHIP_tiny261a, CHIP_tiny40, CHIP_tiny43u, CHIP_tiny44, CHIP_tiny44l, CHIP_tiny44a, CHIP_tiny45, CHIP_tiny46l, CHIP_tiny461a, CHIP_tiny48, CHIP_tiny5, CHIP_tiny828, CHIP_tiny84, CHIP_tiny84l, CHIP_tiny84a, CHIP_tiny85, CHIP_tiny861, CHIP_tiny861a, CHIP_tiny87, CHIP_tiny88, CHIP_tiny9)
                ASM showdebug  'Assume REFS0 is set to 0 for AD_REF_AVCC
                [canskip]REFS0=b'0'
             #ENDIF

             #IFNDEF Oneof(CHIP_tiny13, CHIP_tiny13a, CHIP_tiny13, CHIP_tiny1634, CHIP_tiny167, CHIP_tiny20, CHIP_tiny2313, CHIP_tiny24, CHIP_tiny24a, CHIP_tiny25, CHIP_tiny25, CHIP_tiny26, CHIP_tiny261a, CHIP_tiny40, CHIP_tiny43u, CHIP_tiny44, CHIP_tiny44l, CHIP_tiny44a, CHIP_tiny45, CHIP_tiny46l, CHIP_tiny461a, CHIP_tiny48, CHIP_tiny5, CHIP_tiny828, CHIP_tiny84, CHIP_tiny84l, CHIP_tiny84a, CHIP_tiny85, CHIP_tiny861, CHIP_tiny861a, CHIP_tiny87, CHIP_tiny88, CHIP_tiny9)
                ASM showdebug  'Bit(REFS1) does exist, so assume 'VCC used as analog reference' REFS0=b'1'
                [canskip]REFS0=b'1'
             #ENDIF
            #endif
        end If

        #IF AD_REF_SOURCE = AD_REF_256
            If AD_REF_SOURCE = AD_REF_256 Then  ' case 1
              [canskip]REFS0=b'1'
              [canskip]REFS1=b'1'
            End If
        #ENDIF

      #endif
    #endif

    #ifdef Bit(REFS2)

      'Select reference source for chips that DO HAVE Bit(REFS2)
      #if ChipFamily <> 122

          If AD_REF_SOURCE = AD_REF_AREF Then
            Set ADMUX.REFS0 On
          End If
          #IF AD_REF_SOURCE = AD_REF_256
              If AD_REF_SOURCE = AD_REF_256 Then ' case 2
                Set ADMUX.REFS0 On
                Set ADMUX.REFS1 On
                Set REFS2 On
              End If
          #ENDIF
      #endif
      #if ChipFamily = 122


          #IF AD_REF_SOURCE = AD_REF_AREF
                'Set for AD_REF_AREF
                REFS0=b'0'
                REFS1=b'0'
                REFS2=0
                VCAL = VCAL1; // calibration for 1.024V
          #ENDIF

          #IF AD_REF_SOURCE = AD_REF_AVCC
                'Set for AD_REF_AVCC
                REFS0=b'1'
                REFS1=b'0'
                REFS2=0
                VCAL = VCAL1; // calibration for 1.024V
          #ENDIF

          #IF AD_REF_SOURCE = AD_REF_4096
                'Set for AD_REF_4096
                REFS0=b'0'
                REFS1=b'0'
                REFS2=1
                VCAL = VCAL3  // calibration for 4.096V
          #ENDIF

          #IF AD_REF_SOURCE = AD_REF_2048
                'Set for AD_REF_2048
                REFS0=b'0'
                REFS1=b'1'
                REFS2=0
                VCAL = VCAL2  // calibration for 2.048V
          #ENDIF

          #IF AD_REF_SOURCE = AD_REF_1024
                'Set for AD_REF_1024
                REFS0=b'1'
                REFS1=b'1'
                REFS2=0
                VCAL = VCAL1; // calibration for 1.024V
          #ENDIF

      #endif



    #endif


    #IFDEF Bit(ADPS2)

      #IFDEF ADSpeed HighSpeed
        'Set conversion clock to HighSpeed
        SET ADPS2 Off
        SET ADPS1 Off
      #ENDIF
      #IFDEF ADSpeed MediumSpeed
        'Set conversion clock to MediumSpeed
        SET ADPS2 On
        SET ADPS1 Off
      #ENDIF
      #IFDEF ADSpeed LowSpeed
        'Set conversion clock to LowSpeed
        SET ADPS2 On
        SET ADPS1 On
        SET ADPS0 On
      #ENDIF
    #ENDIF

    Wait AD_Delay   'Execute the acquisition Delay

    Set ADEN On     'Take reading

    #IF AD_REF_SOURCE = AD_REF_256

      If AD_REF_SOURCE = AD_REF_256 Then ' case 3
        'After turning on ADC, let internal Vref stabilise
        Wait AD_VREF_DELAY
      End If

    #ENDIF

    #ifdef ADREADPREREADCOMMAND
        ADREADPREREADCOMMAND  'user commands are added here
    #endif

    Set ADSC On
    Wait While ADSC On
    Set ADEN Off

   #ENDIF  'CHIPADC>0

  #ENDIF

end macro

'Returns Byte
function ReadAD( in ADReadPort) as byte
    'ADFM should configured to ensure LEFT justified


    #IFDEF DebugADC_H
        NOP 'ReadAD( in ADReadPort) as byte. @DebugADC_H
    #ENDIF


    #IFDEF PIC
        #samebit ADFM, ADFRM0, ADFM0
        #IFDEF BIT(ADFM)
          SET ADFM OFF
        #ENDIF


        #IFDEF VAR(ADPCH)
            'for 16F1885x and possibly future others

            #IFDEF DebugADC_H
                NOP '#IFDEF VAR(ADPCH). @DebugADC_H
            #ENDIF

            ADPCH = ADReadPort
        #ENDIF


        #IFDEF Bit(CHSN0)
            'A differential ADC
            SetNegativeChannelSelectbits
        #ELSE
            #IFDEF Bit(ADIC)
                //~ Set to ADC is operating in Single-Ended mode
                //~Chipfamily 15002, 15003 & 15004 defaults to Differential mode, so, we ensure this cleared to 0 
                SetNegativeChannelSelectbits
            #ENDIF
        #ENDIF

    #ENDIF

  '***************************************

  'Perform conversion
  LLReadAD 1


  #IFDEF DebugADC_H
      NOP '@Write output Readad#1
  #ENDIF

  #IFDEF PIC
    #IFNDEF Bit(CHSN0)

         #IFDEF DebugADC_H
            NOP 'Chips with no differential ADC (MOST CHIPS). @DebugADC_H
        #ENDIF

        #IFDEF Var(ADRESH)
            ReadAD = ADRESH
        #ENDIF
        #IFDEF NoVar(ADRESH)
            ReadAD = ADRES
        #ENDIF
    #ENDIF



    #IFDEF Bit(CHSN0)
        ' Support 8-bit differential reads

        #IFDEF DebugADC_H
            NOP 'Chip has differential ADC. @DebugADC_H
        #ENDIF

        'Write output
          #IFDEF NoVar(ADRESL)
              ReadAD = ADRES
          #ENDIF

          #IFDEF Var(ADRESL)
              #IFDEF DebugADC_H
                  NOP 'Read registers, to a word var and then divide by 4 to make a representative byte. @DebugADC_H
              #ENDIF

              'As we are ALWAYS LEFT JUSTIFIED just return the ADRESH ignoring ADRESL
              #IFDEF Var(ADRESH)
                  ReadAD = ADRESH
              #ENDIF
          #ENDIF
    #ENDIF

      #IFDEF Bit(ADFM)

          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADFM). Setting ADFM. @DebugADC_H
          #ENDIF

          SET ADFM OFF
      #ENDIF

 #ENDIF


   #IFDEF AVR
      #IF ChipFamily = 121
        #IFDEF BIT(ADLAR)
          ReadAD = ADCH
        #ENDIF
        #IFNDEF BIT(ADLAR)
          ReadAD = ADCL
        #ENDIF
      #ENDIF
      #IF ChipFamily <> 121
        ReadAD = ADCH
      #ENDIF
   #ENDIF

End Function

'ReadAD - suppports Differential reads
function ReadAD( in ADReadPort, in ADN_PORT ) as integer
  'Optional ADN_PORT to support differential ADC
  'Always LEFT justified

    #IFDEF DebugADC_H
        NOP 'ReadAD( in ADReadPort, in ADN_PORT ) as integer. @DebugADC_H
    #ENDIF

    #IFDEF PIC


        #IFDEF Bit(CHSN0) 
          'A differential ADC
          'ADFM can be set to ON here as this will not impacted by backwards compatbility
          'Rational is that the orginal READAD DID not chnage ADFM, so, we need ensure we do not mess up on Differential chips
          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADFM). Setting ADFM in READAD() @DebugADC_H
          #ENDIF
          SET ADFM ON

          IF ADN_PORT <> 255 then
                configNegativeChannel
          Else
                SetNegativeChannelSelectbits
          END IF
        #ENDIF

        #IFDEF Bit(ADIC)
          'A differential ADC
          'ADFM can be set to ON here as this will not impacted by backwards compatbility
          'Rational is that the orginal READAD DID not chnage ADFM, so, we need ensure we do not mess up on Differential chips
          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADFM). Setting ADFM in READAD() @DebugADC_H
          #ENDIF
          SET ADFM ON

          IF ADN_PORT <> 255 then
                configNegativeChannel
          Else
                SetNegativeChannelSelectbits
          END IF
        #ENDIF

        #IFDEF VAR(ADPCH)
            ADPCH = ADReadPort
        #ENDIF

    #ENDIF

'***************************************

'Perform conversion
LLReadAD 1

 #IFDEF DebugADC_H
    NOP '@Write output Readad#2. @DebugADC_H
#ENDIF


'Determine output value and type
 #IFDEF PIC


    #IFDEF Bit(ADIC)
        //~ Chip has differential ADC
        //~ Support 8-bit differential reads for chipfamily 15002, 15003 & 15004 
        IF ADN_PORT = TRUE then
          'Deterine output
          ReadAD = ADRESH
        ELSE
          ReadAD = ADRESL
          ReadAD_H = ADRESH
          ReadAD = [WORD]READAD / 128
        END IF
    #ELSE

      #IFNDEF Bit(CHSN0)
          'Chips with no differential ADC
          #IFDEF Var(ADRESH)

              #IFDEF Var(ADRESL)
                  IF ADN_PORT = True then

                      #IFDEF Bit(ADRMD)  'check for 10 or 12 bit register bit
                          #IFDEF DebugADC_H
                              NOP 'ADN_PORT = True, so #IFDEF Bit(ADRMD).  @DebugADC_H
                          #ENDIF

                          'Force an 8 Bit result for a 12bit number
                          ReadAD = FnLSL(ADRESH,8) + ADRESL
                          ReadAD = FnLSR(ReadAD,2)
                      #ENDIF
                      #IFNDEF Bit(ADRMD)
                          #IFDEF DebugADC_H
                              NOP 'ADN_PORT = True, so #IFNDEF Bit(ADRMD).  @DebugADC_H
                          #ENDIF

                        'Return an 8 Bit result for a 10bit number, as we are ALWAYS LEFT JUSTIFIED just return the ADRESH ignoring ADRESL
                          #IFDEF Var(ADRESH)
                              ReadAD = ADRESH
                          #ENDIF
                          #IFDEF NoVar(ADRESH)
                              ReadAD = ADRES
                          #ENDIF

                      #ENDIF
                  End if
              #ENDIF

              #IFDEF NoVar(ADRESL)
                  'the function just returned ADRESH
                  ReadAD = ADRESH
              #ENDIF

          #ENDIF

          #IFDEF NoVar(ADRESH)
              ReadAD = ADRES
          #ENDIF
      #ENDIF
      // ~ an effective #ELSE for #IFNDEF Bit(CHSN0)
      #IFDEF Bit(CHSN0) 'Chip has differential ADC
          'Support 8-bit differential reads
          IF ADN_PORT <> 0 then
              'Deterine output
                #IFDEF NoVar(ADRESL)
                    ReadAD = ADRES
                #ENDIF

                #IFDEF Var(ADRESL)
                    ReadAD = ADRESL
                #ENDIF

                #IFDEF Var(ADRESH)
                    ReadAD_H = ADRESH
                #ENDIF

                READAD = READAD / 16

          END IF
      #ENDIF

      #IFDEF Bit(ADFM)
          SET ADFM OFF
      #ENDIF

  #ENDIF //~ End of Bit(ADIC) 'Chip has differential ADC with #else

 #ENDIF

   #IFDEF AVR
      #IF ChipFamily = 121
        #IFDEF BIT(ADLAR)
          ReadAD = ADCH
        #ENDIF
        #IFNDEF BIT(ADLAR)
          ReadAD = ADCL
        #ENDIF
      #ENDIF
      #IF ChipFamily <> 121
        ReadAD = ADCH
      #ENDIF
   #ENDIF

End Function

'Large ReadAD
function ReadAD10( ADReadPort ) As Word
  'Always RIGHT justified

  #IFDEF DebugADC_H
      NOP 'function ReadAD10( ADReadPort ) As Word. @DebugADC_H
  #ENDIF


  #IFDEF PIC
      #IFDEF Bit(ADFM)

          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADFM). Setting ADFM @DebugADC_H
          #ENDIF

          SET ADFM ON
      #ENDIF

      #IFDEF VAR(ADPCH)

          #IFDEF DebugADC_H
              NOP '#IFDEF VAR(ADPCH). Setting ADPCH @DebugADC_H
          #ENDIF

          ADPCH = ADReadPort
      #ENDIF


      #IFDEF Bit(CHSN0)
          'A differential ADC
          SetNegativeChannelSelectbits
      #ELSE
            #IFDEF Bit(ADIC)
                //~ Set to ADC is operating in Single-Ended mode
                //~Chipfamily 15002, 15003 & 15004 defaults to Differential mode, so, we ensure this cleared to 0 
                ADCON0.ADIC = 0
            #ENDIF
      #ENDIF

  #ENDIF

  #IFDEF AVR
      Dim LLADResult As Word Alias ADCH, ADCL
  #ENDIF

  'Do conversion
  LLReadAD 0

    #IFDEF PIC
        'Write output
        #IFDEF NoVar(ADRESL)
          ReadAD10 = ADRES
        #ENDIF

        #IFDEF Var(ADRESL)
            ReadAD10 = ADRESL
        #ENDIF

        #IFDEF Var(ADRESH)
            ReadAD10_H = ADRESH
        #ENDIF

        'Put A/D format back to normal
        #IFDEF Bit(ADFM)
            SET ADFM OFF
        #ENDIF

    #ENDIF

    #IFDEF AVR
        ReadAD10 = LLADResult
    #ENDIF
end function



  'Large ReadAD  - suppports Differential reads
function ReadAD10(ADReadPort, in ADN_PORT ) As integer
  

  #IFDEF DebugADC_H
      NOP 'ReadAD10(ADReadPort, in ADN_PORT ) As integer. @DebugADC_H
  #ENDIF



  #IFDEF PIC
      #IFDEF Bit(ADFM)

          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADFM). Setting ADFM @DebugADC_H
          #ENDIF
          'Always RIGHT justified
          SET ADFM ON
      #ENDIF


      #IFDEF Bit(CHSN0)
          'A differential ADC
          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(CHSN0). ???? @DebugADC_H
          #ENDIF

          ;set AD Result Mode to 10-Bit
          #IFDEF Bit(ADRMD)      ;Added for 16F178x
              #IFDEF DebugADC_H
                  NOP '#IFDEF Bit(ADRMD). set AD Result Mode to 10-Bit. @DebugADC_H
              #ENDIF

              SET ADRMD ON      ; WMR
          #ENDIF

          IF ADN_PORT <> 255 then
              configNegativeChannel
          Else
              SetNegativeChannelSelectbits
          END IF
      #ELSE
          //~Handler for Chipfamily 15002, 15003 & 15004 Differential mode
          #IFDEF SCRIPT_16FDIFFENTIALADC
            IF ADN_PORT <> 255 then
                configNegativeChannel
            Else
                SetNegativeChannelSelectbits
            END IF
          #ENDIF
      #ENDIF

      #IFDEF VAR(ADPCH)
          #IFDEF DebugADC_H
              NOP  '#IFDEF VAR(ADPCH). @DebugADC_H
          #ENDIF

          ADPCH = ADReadPort
      #ENDIF


  #ENDIF

  #IFDEF AVR
      Dim LLADResult As Word Alias ADCH, ADCL
  #ENDIF

  'Do conversion
  LLReadAD 0

    #IFDEF PIC
        'Write output
        #IFDEF NoVar(ADRESL)
          ReadAD10 = ADRES
        #ENDIF

        #IFDEF Var(ADRESL)
            ReadAD10 = ADRESL
        #ENDIF

        #IFDEF Var(ADRESH)
            ReadAD10_H = ADRESH
        #ENDIF

        'Put A/D format back to normal
        #IFDEF Bit(ADFM)
            SET ADFM OFF
        #ENDIF

        #IFNDEF ChipReadAD10BitForceVariant
            #IFDEF Bit(CHSN0)'18F PIC with 12=bit ADC does not support 10-bit result, so recalc
                IF ADN_PORT <> 0 then
                    ' Added DIV/4 to return 10 bit value
                      #IFNDEF Bit(CHSN3)

                            READAD10 = READAD10/4

                      #ENDIF
                END IF
            #ENDIF
        #ENDIF

        #IFDEF ChipReadAD10BitForceVariant
                'Shift the data to 10bits when a 12bit ADC
                IF ADN_PORT = TRUE then
                    READAD10 = READAD10/ChipReadAD10BitForceVariant
                ELSE
                    //~ This is 1/2 of the ChipReadAD10BitForceVariant as the range is -1023 to 1023
                    READAD10 = READAD10/SCRIPTREADAD10BITFORCEVARIANTDIFFENTIAL
                END IF
        #ENDIF
  #script


  #endscript

  #ENDIF

  #IFDEF AVR
      ReadAD10 = LLADResult
  #ENDIF

end function


'Larger ReadAD
function ReadAD12( ADReadPort ) As Word

  #IFDEF DebugADC_H
      NOP 'function ReadAD12( ADReadPort ) As Word. @DebugADC_H
  #ENDIF

  #IFDEF PIC

      //~ Set up A/D format
      #IFDEF Bit(ADFM)
          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADFM). Setting ADFM @DebugADC_H
          #ENDIF
          'Always RIGHT justified
          SET ADFM ON
      #ENDIF

      
      #IFDEF Bit(ADRMD)
          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADRMD). Set A/D Result Mode to 12-Bit @DebugADC_H
          #ENDIF
          'Set A/D Result Mode to 12-Bit
          SET ADRMD OFF
      #ENDIF


      #IFDEF VAR(ADPCH)
          #IFDEF DebugADC_H
              NOP '#IFDEF VAR(ADPCH). Set ???? @DebugADC_H
          #ENDIF

          ADPCH = ADReadPort
      #ENDIF


      #IFDEF Bit(CHSN0)
          'A differential ADC
          SetNegativeChannelSelectbits
        #ELSE
            #IFDEF Bit(ADIC)
                //~ Set to ADC is operating in Single-Ended mode
                //~Chipfamily 15002, 15003 & 15004 defaults to Differential mode, so, we ensure this cleared to 0 
                SetNegativeChannelSelectbits
            #ENDIF
      #ENDIF

  #ENDIF


  #IFDEF AVR
      Dim LLADResult As Word Alias ADCH, ADCL
  #ENDIF

  'Do conversion using Macro LLReadAD ( 0 )
  LLReadAD 0

  #IFDEF PIC
    'Write output
    #IFDEF NoVar(ADRESL)
      ReadAD12 = ADRES
    #ENDIF

    #IFDEF Var(ADRESL)
      ReadAD12 = ADRESL
    #ENDIF

    #IFDEF Var(ADRESH)
      ReadAD12_H = ADRESH
    #ENDIF

    'Put A/D format back to normal
    #IFDEF Bit(ADFM)
      SET ADFM OFF
    #ENDIF

  #ENDIF

  #IFDEF AVR
      ReadAD12 = LLADResult
  #ENDIF


end function


'Larger ReadAD - suppports Differential reads
function ReadAD12(ADReadPort, ADN_PORT ) As integer
  'Always RIGHT justified

  #IFDEF DebugADC_H
      NOP 'function ReadAD12(ADReadPort, ADN_PORT ) As integer. @DebugADC_H
  #ENDIF

  #IFDEF PIC

   'Set up A/D format
      #IFDEF Bit(ADFM)
          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADFM). Setting ADFM @DebugADC_H
          #ENDIF

          SET ADFM ON
      #ENDIF


      #IFDEF Bit(CHSN0)'Chip has Differential ADC Module
         'A differential ADC
         IF ADN_PORT <> 255 then
            configNegativeChannel
         Else
            SetNegativeChannelSelectbits
         END IF
      #ELSE
            #IFDEF Bit(ADIC)

              'Use differential AD
              IF ADN_PORT <> 255 then
                  configNegativeChannel
              Else
                  SetNegativeChannelSelectbits
              END IF
            #ENDIF         
      #ENDIF

      #IFDEF Bit(ADRMD)
          #IFDEF DebugADC_H
              NOP '#IFDEF Bit(ADRMD). Set A/D Result Mode to 12-Bit @DebugADC_H
          #ENDIF
                'Set A/D Result Mode to 12-Bit
          SET ADRMD OFF
      #ENDIF

      #IFDEF VAR(ADPCH)
          #IFDEF DebugADC_H
              NOP '#IFDEF VAR(ADPCH). Set ???? @DebugADC_H
          #ENDIF

          ADPCH = ADReadPort
      #ENDIF

  #ENDIF

  #IFDEF AVR
      Dim LLADResult As Word Alias ADCH, ADCL
  #ENDIF

  'Do conversion
  LLReadAD 0

  #IFDEF PIC
    'Write output
    #IFDEF NoVar(ADRESL)
      ReadAD12 = ADRES
    #ENDIF

    #IFDEF Var(ADRESL)
      ReadAD12 = ADRESL
    #ENDIF

    #IFDEF Var(ADRESH)
      ReadAD12_H = ADRESH
    #ENDIF

    'Put A/D format back to normal
    #IFDEF Bit(ADFM)
      SET ADFM OFF
    #ENDIF

  #ENDIF

  #IFDEF AVR
      ReadAD12 = LLADResult
  #ENDIF


end function

'This sub is deprecated
sub ADFormat(ADReadFormat)
 SET ADFM OFF
 IF ADReadFormat.1 ON THEN SET ADFM ON
end sub

'This sub is deprecated
sub ADOff
'Disable the A/D converter, and set all ports to digital.
'This sub is deprecated, InitSys automatically turns off A/D

 SET ADON OFF
#IFDEF NoBit(PCFG4)
 #IFDEF NoVar(ANSEL)
  #IFDEF NoVar(ADCON2)
   #IFDEF Bit(PCFG3)
    SET PCFG3 OFF
   #ENDIF
   SET PCFG2 ON
   SET PCFG1 ON
   SET PCFG0 OFF
  #ENDIF
  #IFDEF Var(ADCON2)
   SET PCFG3 ON
   SET PCFG2 ON
   SET PCFG1 ON
   SET PCFG0 ON
  #ENDIF
 #ENDIF
#ENDIF

 #IFDEF Bit(PCFG4)
  #IFDEF Bit(PCFG6)
   SET PCFG6 ON
  #ENDIF
  #IFDEF Bit(PCFG5)
   SET PCFG5 ON
  #ENDIF
  SET PCFG4 ON
  SET PCFG3 ON
  SET PCFG2 ON
  SET PCFG1 ON
  SET PCFG0 ON
 #ENDIF

 #IFDEF Var(ANSEL)
  ANSEL = 0
 #ENDIF
 #IFDEF Var(ANSELH)
  ANSELH = 0
 #ENDIF

end sub

Macro ConfigNegativeChannel
   //~ Macro added to support PICS with Differential ADC MOdule

   #IFNDEF Bit(CHSN3) '18F
      #IFDEF Bit( ADIC )
        //~ Chipfamily 15002, 15003 & 15004
        ADNCH = ADN_PORT
        ADIC = 1
      #ELSE
          'Configure Negative A/D Channel
          'Only AN0 - AN6 allowed
        IF ADN_PORT >=0 and ADN_PORT <=6 then
              N_CHAN = ADN_PORT + 1
              ADCON1 = ADCON1 AND b'11111000' OR N_CHAN
        END IF

        IF ADN_PORT > 6  then
              set CHSN2 OFF
              set CHSN1 OFF
              set CHSN0 OFF
        END IF

        'Addeed aqtime
        #IFDEF bit(ACQT0)
            ACQT0 = AD_Acquisition_Time_Select_bits.0
            ACQT1 = AD_Acquisition_Time_Select_bits.1
            ACQT2 = AD_Acquisition_Time_Select_bits.2
        #ENDIF
      #ENDIF
   #ENDIF

   #IFDEF Bit(CHSN3) '16F178x
      ' AN0 - AN13 and AN21 Allowed
      'configure Negative A/D Channel
      IF ADN_PORT >= 0 and ADN_PORT <= 13 then
           N_CHAN = ADN_PORT
           ADCON2 = ADCON2 AND b'11110000' OR N_CHAN
      END IF

      'exception for (16f1789, 16f877a, 16f1939, 18f4525) where AN21 = channel 14
      IF ADN_PORT = 21 then
           N_CHAN = 14
           ADCON2 = ADCON2 AND b'11110000' OR N_CHAN
      END IF

      IF ADN_PORT > 13 and ADN_PORT <> 21 then
          set CHSN3 ON
          set CHSN2 ON
          set CHSN1 ON
          set CHSN0 ON
      END IF
   #ENDIF

End Macro


Macro  SetNegativeChannelSelectbits

          #IFDEF Bit(CHSN3)
              #IFDEF DebugADC_H
                  NOP ' #IFDEF Bit(CHSN3) So, set all CHSN3:0 to 1, so, we get ADC Negative reference – selected by ADNREF. @DebugADC_H
              #ENDIF
              //~ADC Negative reference – selected by ADNREF
              //~Assumed to be connected to VSS
              CHSN0 = 1
              CHSN1 = 1
              CHSN2 = 1
              CHSN3 = 1
          #ENDIF

          #IFDEF Bit( ADIC )
              //~ Chipfamily 15002, 15003 & 15004
              //~Not differntial
              ADCON0.ADIC = 0
              //~Assumed to be connected to VSS
              ADNCH = 0x00
          #ELSE
            #IFNDEF Bit(CHSN3)
                #IFDEF DebugADC_H
                    NOP ' #IFDEF Bit(CHSN0) So, set all three CHSN2:0 to 0, so, we get ADC Negative reference – selected by ADNREF. @DebugADC_H
                #ENDIF
                //~Assumed to be connected to VSS
                CHSN0 = 0
                CHSN1 = 0
                CHSN2 = 0
                'Addeed aqtime
                #IFDEF bit(ACQT0)
                    ACQT0 = AD_Acquisition_Time_Select_bits.0
                    ACQT1 = AD_Acquisition_Time_Select_bits.1
                    ACQT2 = AD_Acquisition_Time_Select_bits.2
                #ENDIF
            #ENDIF
          #ENDIF
end macro



' ADFVR_Initialize ( FVR_OFF | ADFVR_1x | ADFVR_2x| ADFVR_3x )
' ADFVR_IsOutputReady returns true or false





#define FVREN_enabled = 0x80
#define FVREN_disabled = 0x80

'''Initializes the FVR
'''@param FVR_OFF, FVR_1x, FVR_2x or FVR_4x = (1.024v, 2.048v or 4.096v)
sub FVRInitialize ( Optional in FVR_Bits = FVR_OFF )
   'Strip the bits we need to retain
   FVRCON = FVRCON and 0xc0 '0x7C
    'FVREN enabled; FVR
    if FVR_Bits <> FVR_OFF then

        FVRCON = FVRCON or FVREN_enabled or FVR_Bits
       'VREF+ is connected to FVR
        #IFDEF VAR(ADREF)
        asm showdebug Found VAR ADREF
          ADREF.0 = 1     'ADREF<1:0>
          ADREF.1 = 1     'ADREF<1:0>
        #ENDIF

        #IFDEF VAR(ADPREF)
        asm showdebug Found VAR ADPREF
          ADPREF.0 = 1     'ADPREF<1:0>
          ADPREF.1 = 1     'ADPREF<1:0>
        #ENDIF

        #IFDEF BIT(ADPREF1)
        asm showdebug Found BIT ADPREF1 and therefore ADPREF0
          ADPREF1 = 1
          ADPREF0 = 1
        #ENDIF

        #IFDEF BIT(PVCFG1)
          asm showdebug Found BIT PVCFG1
          PVCFG1 = 1
          PVCFG0 = 0
        #ENDIF

    Else
        ' VREF+ is connected to VDD
        #IFDEF VAR(ADREF)
          ADREF.0 = 0     'ADPREF<1:0>
          ADREF.1 = 0     'ADPREF<1:0>
        #ENDIF

        #IFDEF VAR(ADPREF)
          ADPREF.0 = 0     'ADPREF<1:0>
          ADPREF.1 = 0     'ADPREF<1:0>
        #ENDIF

        #IFDEF BIT(ADPREF1)
          asm showdebug Found BIT ADPREF1 and therefore ADPREF0
          ADPREF1 = 0
          ADPREF0 = 0
        #ENDIF

        #IFDEF BIT(PVCFG1)
          PVCFG1 = 1
          PVCFG0 = 0
        #ENDIF

    end if

end sub

function FVRIsOutputReady as bit

    FVRIsOutputReady = FVRRDY

end function


Function ReadAD10AVRDx( ADReadPort as Byte ) as Word

    Dim SYSSTRINGA as Word at 26

    //~ Dimension is the function name, the alias return ADC result registers.  
    //~ These will be set to the result upon exit of this function
    Dim ReadAD10AVRDx as Word alias ADC0_RESH, ADC0_RESL
    Dim PORTx_PinyCTRL as Word 
    Dim ADCPort_Ctrl_Cache as Byte

      #IF NODEF( AVRX_ADC_PORTx_PinyCTRL )
        //~ User define not present so use the lookup
        // Get the register address for the specified AINx/ADReadPort from the specific table
        ReadTable CHIPADCPPORTMAP, ADReadPort + 1, PORTx_PinyCTRL
      #ELSE
        // User define PORTx_PinyCTRL, so, just use it
        PORTx_PinyCTRL = @AVRX_ADC_PORTx_PinyCTRL
      #ENDIF

    // Port Specific Operations - get the existing value and set for ADC

        //~ set X,  SYSSTRINGA is the X registers            
        [word]SYSSTRINGA = [word]PORTx_PinyCTRL
        ld SysValueCopy, x 
        #IF NODEF( AVRX_ADC_NOCACHE_PORTx_PinyCTRL )
          // Cache current register for the selected AINx/ADReadPort
          ADCPort_Ctrl_Cache = SysValueCopy 
        #ENDIF
        // For selected PORT ; Disable digital input buffer
        SysValueCopy.PORT_ISC_0_bp = 0
        SysValueCopy.PORT_ISC_1_bp = 0
        SysValueCopy.PORT_ISC_2_bp = 0
        
        // Disable pull-up resistor
        SysValueCopy.PORT_PULLUPEN_bp = 0

        // Load the selected AINx/ADReadPort register with the ADC config
        [word]SYSSTRINGA = [word]PORTx_PinyCTRL // set X
        st X+, SysValueCopy

    // Select ADC channel using GCBASIC variable
    ADC0_MUXPOS = ADReadPort

    // Non port specific operations
        //~ CLK_PER divided by 4 

        //~ use ADC_REFSEL_VDDREF_gc to reference VDD 
        //~ or ADC_REFSEL_INTREF_gc for 4v3 
        //~ ADC prescaler: The ADC requires an input clock frequency between 50 kHz and 1.5 MHz 
        //~ for maximum resolution. If a lower resolution than 10 bits is selected, the input 
        //~ clock frequency to the ADC can be higher than 1.5 MHz to get a higher sample rate.

        ADC0_CTRLC = SCRIPTADC_PRESC_DIV_gc  | ADC_REFSEL_VDDREF_gc         
                    
        // Internal reference; ADC Enable; 10-bit mode; 
        ADC0_CTRLA = ADC_ENABLE_bm  | ADC_RESSEL_10BIT_gc

        // Clear ADC interrupt flag - this is required
        ADC0_INTFLAGS.ADC_RESRDY_bp = 1

        // Start ADC conversion
        ADC0_COMMAND = ADC_STCONV_bm
        
        //~ Wait until ADC conversion done
        wait while ADC0_INTFLAGS.ADC_RESRDY_bp = 1
        
        #IF NODEF( AVRX_ADC_NOCACHE_PORTx_PinyCTRL )
          // Port Specific - revert cache to the AINx/ADReadPort register
          [word]SYSSTRINGA = [word]PORTx_PinyCTRL // set X
          SysValueCopy = ADCPort_Ctrl_Cache
          st X+, SysValueCopy
        #ENDIF

End Function

Function ReadADAVRDx( ADReadPort as Byte ) as Word

    Dim SYSSTRINGA as Word at 26 

    //~ Dimension is the function name, the alias return ADC result registers.  
    //~ These will be set to the result upon exit of this function

    Dim ReadADAVRDx as Word alias ADC0_RESH, ADC0_RESL
    Dim PORTx_PinyCTRL as Word 
    Dim ADCPort_Ctrl_Cache as Byte

      #IF NODEF( AVRX_ADC_PORTx_PinyCTRL )
        //~ User define not present so use the lookup
        // Get the register address for the specified AINx/ADReadPort from the specific table
        ReadTable CHIPADCPPORTMAP, ADReadPort + 1, PORTx_PinyCTRL
      #ELSE
        // User define PORTx_PinyCTRL, so, just use it
        PORTx_PinyCTRL = @AVRX_ADC_PORTx_PinyCTRL
      #ENDIF
    
    // Port Specific Operations - get the existing value and set for ADC

        //~ set X,  SYSSTRINGA is the X registers            
        [word]SYSSTRINGA = [word]PORTx_PinyCTRL
        ld SysValueCopy, x 
        #IF NODEF( AVRX_ADC_NOCACHE_PORTx_PinyCTRL )
          // Cache current register for the selected AINx/ADReadPort
          ADCPort_Ctrl_Cache = SysValueCopy 
        #ENDIF

        // For selected PORT ; Disable digital input buffer
        SysValueCopy.PORT_ISC_0_bp = 0
        SysValueCopy.PORT_ISC_1_bp = 0
        SysValueCopy.PORT_ISC_2_bp = 0
        
        // Disable pull-up resistor
        SysValueCopy.PORT_PULLUPEN_bp = 0

        // Load the selected AINx/ADReadPort register with the ADC config
        [word]SYSSTRINGA = [word]PORTx_PinyCTRL // set X
        st X+, SysValueCopy

    // Select ADC channel using GCBASIC variable
    ADC0_MUXPOS = ADReadPort

    // Non port specific operations
        //~ CLK_PER divided by 4 

        //~ use ADC_REFSEL_VDDREF_gc to reference VDD 
        //~ or ADC_REFSEL_INTREF_gc for 4v3 
        //~ ADC prescaler: The ADC requires an input clock frequency between 50 kHz and 1.5 MHz 
        //~ for maximum resolution. If a lower resolution than 10 bits is selected, the input 
        //~ clock frequency to the ADC can be higher than 1.5 MHz to get a higher sample rate.

        ADC0_CTRLC = SCRIPTADC_PRESC_DIV_gc  | ADC_REFSEL_VDDREF_gc         
                    
        // Internal reference; ADC Enable; 8-bit mode; 
        ADC0_CTRLA = ADC_ENABLE_bm  | ADC_RESSEL_8BIT_gc

        // Clear ADC interrupt flag - this is required
        ADC0_INTFLAGS.ADC_RESRDY_bp = 1

        // Start ADC conversion
        ADC0_COMMAND = ADC_STCONV_bm
        
        //~ Wait until ADC conversion done
        wait while ADC0_INTFLAGS.ADC_RESRDY_bp = 1
      
        #IF NODEF( AVRX_ADC_NOCACHE_PORTx_PinyCTRL )
          // Port Specific - revert cache to the AINx/ADReadPort register
          [word]SYSSTRINGA = [word]PORTx_PinyCTRL // set X
          SysValueCopy = ADCPort_Ctrl_Cache
          st X+, SysValueCopy
        #ENDIF

End Function



// for each entry in the table this is the port.in cntl for the ADC.  this are the address regisers.
// they are sequential AIN1 to AIN22
// if 0x000, then that specific AINx does not exist.
// this was lifted from the datasheets

Table AVRDxADC06Type1Lookup as Word
    0x410, 0x411, 0x412, 0x413 , 0x416, 0x417           // PORTA
End Table

Table AVRDxADC12Type1Lookup as Word
    0x410, 0x411, 0x412, 0x413 , 0x414, 0x415, 0x416    // PORTA
           0x431, 0x432                                 // PORT?
End Table

Table AVRDxADC16Type1Lookup as Word                                 // supports 4808/4809
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477    // PORTD
    0x490, 0x491, 0x492, 0x493                                // PORTE
    0x4B0, 0x4B1, 0x4B2, 0x4B3                                // PORTF
End Table

Table AVRDxADC22Type1Lookup as Word                            // supports 64 pin AVRxxxDxA64 - 22
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x490, 0x491, 0x492, 0x493, 0x494, 0x495, 0x496, 0x497     // PORTE
    0x4B0, 0x4B1, 0x4B2, 0x4B3, 0x4B4, 0x4B5                   // PORTF
End Table

Table AVRDxADC18Type1Lookup as Word                            // supports 48 pin AVRxxxDxA48 - 18
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x490, 0x491, 0x492, 0x493, 0x000, 0x000, 0x000, 0x000     // PORTE
    0x4B0, 0x4B1, 0x4B2, 0x4B3, 0x4B4, 0x4B5                   // PORTF
End Table

Table AVRDxADC14Type1Lookup as Word                            // supports 32 pin AVRxxxDxA32 - 14
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000     // PORTE
    0x4B0, 0x4B1, 0x4B2, 0x4B3, 0x4B4, 0x4B5                   // PORTF
End Table


Table AVRDxADC10Type1Lookup as Word                            // supports 28 pin AVRxxxDxA28 - 10
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000     // PORTE
    0x4B0, 0x4B1                                               // PORTF
End Table

Table AVRDxADC23Type2Lookup as Word                            // supports 32 pin AVR16/32DD28/32 - 23
    0x000, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000     // none
    0x4B0, 0x4B1, 0x4B2, 0x4B3, 0x4B4, 0x4B5                   // PORTF
    0x412, 0x413, 0x414, 0x415, 0x416, 0x417                   // PORTA
    0x450, 0x451, 0x452, 0x453                                 // PORTC
End Table


Table AVRDxADC19Type2Lookup as Word                            // supports 28 pin AVR16/32DD28/32 - 19
    0x000, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000     // none
    0x4B0, 0x4B1, 0x000, 0x000, 0x000, 0x000                   // PORTF
    0x412, 0x413, 0x414, 0x415, 0x416, 0x417                   // PORTA
    0x450, 0x451, 0x452, 0x453                                 // PORTC
End Table



Table AVRDxADC13Type2Lookup as Word                            // supports 20 pin AVR16/32DD28/32 - 13
    0x000, 0x000, 0x000, 0x000, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000     // none
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000                   // PORTF
    0x412, 0x413, 0x414, 0x415, 0x416, 0x417                   // PORTA
    0x000, 0x451, 0x452, 0x453                                 // PORTC
End Table

Table AVRDxADC7Type2Lookup as Word                            // supports 14 pin AVR16/32DD28/32 - 7
    0x000, 0x000, 0x000, 0x000, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000     // none
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000                   // PORTF
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000                   // PORTA
    0x000, 0x451, 0x452, 0x453                                 // PORTC
End Table

Table AVRDxADC12Type3Lookup as Word                           // supports 24&20 pin ATtiny804/806/807/1604/1606/1607 - 12
    0x410, 0x411, 0x412, 0x413, 0x414, 0x415, 0x416, 0x417    // PORTA
    0x435, 0x434, 0x431, 0x430                                // PORTB
End Table

Table AVRDxADC10Type3Lookup as Word                           // supports 14 pin ATtiny804/806/807/1604/1606/1607 - 10
    0x410, 0x411, 0x412, 0x413, 0x414, 0x415, 0x416, 0x417    // PORTA
    0x000, 0x000, 0x431, 0x430                                // PORTB
End Table

Table AVRDxADC6Type3Lookup as Word                            // supports 8 pin ATtiny202/204/402/404/406 - 6
    0x410, 0x411, 0x412, 0x413, 0x000, 0x000, 0x416, 0x417    // PORTA
End Table

Table AVRDxADC15Type4Lookup as Word                           // supports 24&20 pin ATtiny1624/1626/1627 - 15
    0x000, 0x411, 0x412, 0x413, 0x414, 0x415, 0x416, 0x417    // PORTA
    0x435, 0x434, 0x431, 0x430                                // PORTB
    0x450, 0x451, 0x452, 0x453
End Table


Table AVRDxADC9Type4Lookup as Word                            // supports 14 pin ATtiny1624/1626/1627 - 9
    0x000, 0x411, 0x412, 0x413, 0x414, 0x415, 0x416, 0x417    // PORTA
    0x435, 0x434    
End Table


Table AVRDxADC16Type5Lookup as Word                            // supports 48 pin ATmega808/809/1608/1609 - 16
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x490, 0x491, 0x492, 0x493                                 // PORTE
    0x4B2, 0x4B3, 0x4B4, 0x4B5                                 // PORTF
End Table

Table AVRDxADC12Type5Lookup as Word                            // supports 32 pin ATmega808/809/1608/1609 - 12
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477     // PORTD
    0x000, 0x000, 0x000, 0x000                                 // PORTE
    0x4B2, 0x4B3, 0x4B4, 0x4B5                                 // PORTF
End Table

Table AVRDxADC8Type5Lookup as Word                            // supports 28 pin ATmega808/809/1608/1609 - 8
    0x470, 0x471, 0x472, 0x473, 0x474, 0x475, 0x476, 0x477    // PORTD
End Table
'
'Testprogram:
'
'''  #chip 16F877a, 16
'''  #chip 16F1939
'''  #chip 16F1789   'DIFF CHIP
'''  #chip 18f4525
'''   #chip 16f18877
'''   #chip 16f88,4
'''   #chip 18f45k80, 16:  #define AD_Delay 4 10us
''
''    'Generated by PIC PPS Tool for GCBASIC
''    'PPS Tool version: 0.0.5.5
''    'PinManager data: 07/03/2017
''    '
''    'Template comment at the start of the config file
''    '
''    #startup InitPPS, 85
''
''    Sub InitPPS
''
''            'Module: EUSART
''            RC6PPS = 0x0010    'TX > RC6
''            TXPPS = 0x0016    'RC6 > TX (bi-directional)
''
''    End Sub
''    'Template comment at the end of the config file
''
''
''    'USART settings
''        #define USART_BAUD_RATE 9600  'Initializes USART port with 9600 baud
''        #define USART_TX_BLOCKING ' wait for tx register to be empty
''        wait 100 ms
''        HSerPrintCRLF 2
''        HSerPrint "ADC Test"
''        HSerPrintCRLF
''
'''    #define ADReadPreReadCommand  setupADC
'''    #define DebugADC_H
''
''
''    #define ADSpeed LowSpeed
''
''    dir porta.0 in
''
''    dim user_variable as Integer
''
''    do
''        wait 100 ms
'''tests
''         user_variable = ReadAD( AN0  )
'''         user_variable = ReadAD( AN0, TRUE )
'''         user_variable = ReadAD( AN0, AN2 )
'''
'''        user_variable = ReadAD10( AN0  )
'''        user_variable = ReadAD10( AN0 , TRUE )
'''        user_variable = ReadAD10( AN0 , AN2 )
'''
'''        user_variable = ReadAD12( AN0  )
'''        user_variable = ReadAD12( AN0 , TRUE )
'''        user_variable = ReadAD12( AN0 , AN2 )
''
''        HSerPrint "ADCON0: "
''        HSerPrint  ADCON0
''        HSerSend 9
''        HSerPrint "ADCON1: "
''        HSerPrint ADCON1
''        HSerSend 9
''        #IFDEF Var(ADCON2)
''          HSerPrint "ADCON2: "
''          HSerPrint ADCON2
''          HSerSend 9
''        #ENDIF
''        HSerPrint "Reg: "
''        HSerPrint hex(ADRESH)
''        HSerPrint  hex(ADRESL)
''        HSerSend 9
''        HSerPrint hex(ADRES)
''        HSerPrint ":"
''        HSerSend 9
''        HSerPrint "Returned: "
''        HSerPrint user_variable
''        HSerPrintCRLF
''    '
''    '
''    '     HSerPrint "1. Test Run"
''    '     user_variable = ReadAD( AN0 )
''    '     HSerPrint "ReadAD( AN0  ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "2. Test Run"
''    '     user_variable = ReadAD( AN0, TRUE )
''    '     HSerPrint "ReadAD( AN0, TRUE ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "3. Test Run"
''    '     user_variable = ReadAD( AN0, AN2 )
''    '     HSerPrint "ReadAD( AN0, TRUE ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '
''    '     HSerPrint "1. Test Run"
''    '     user_variable = ReadAD( AN0 )
''    '     HSerPrint "ReadAD( AN0 ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "2. Test Run"
''    '     user_variable = ReadAD( AN0, TRUE )
''    '     HSerPrint "ReadAD( AN0, TRUE ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "3. Test Run"
''    '     user_variable = ReadAD( AN0, AN2 )
''    '     HSerPrint "ReadAD( AN0, AN2 ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '
''    '     HSerPrint "4. Test Run"
''    '     user_variable = ReadAD10( AN0 )
''    '     HSerPrint "ReadAD10( AN0 ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "5. Test Run"
''    '     user_variable = ReadAD10( AN0, TRUE )
''    '     HSerPrint "ReadAD10( AN0, TRUE ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "6. Test Run"
''    '     user_variable = ReadAD10( AN0, AN2 )
''    '     HSerPrint "ReadAD( AN0, AN2 ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '
''    '     HSerPrint "7. Test Run"
''    '     user_variable = ReadAD12( AN0 )
''    '     HSerPrint "ReadAD12( AN0 ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "8. Test Run"
''    '     user_variable = ReadAD12( AN0, TRUE )
''    '     HSerPrint "ReadAD12( AN0, TRUE ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '     HSerPrint "9. Test Run"
''    '     user_variable = ReadAD12( AN0, AN2 )
''    '     HSerPrint "ReadAD12( AN0, AN2 ) :"
''    '     HSerPrint user_variable
''    '     HSerPrintCRLF
''    '
''    '
''    '     wait 1 s
''
''    loop
''
''    sub setupADC
''
''    end sub
''
''
''''' GCB Optimisation file
''
'''Optmise PWM.h
''    #define USE_HPWMCCP1 true
''    #define USE_HPWMCCP2 true
''    #define USE_HPWMCCP3 true
''    #define USE_HPWMCCP4 true
''    #define USE_HPWMCCP5 true
''
''    #define USE_HPWM5 true
''    #define USE_HPWM6 true
''    #define USE_HPWM7 true
''
''    #define USE_HPWM_TIMER2 true
''    #define USE_HPWM_TIMER4 true
''    #define USE_HPWM_TIMER6 true
''    #define USE_HPWM_TIMER7 true
''
'''Optimise A-d.h
''    'Standard family chips
''    #define USE_AD0 true
''    #define USE_AD1 true
''    #define USE_AD2 true
''    #define USE_AD2 true
''    #define USE_AD3 true
''    #define USE_AD4 true
''    #define USE_AD5 true
''    #define USE_AD6 true
''    #define USE_AD7 true
''    #define USE_AD8 true
''    #define USE_AD9 true
''    #define USE_AD10 true
''    #define USE_AD11 true
''    #define USE_AD12 true
''    #define USE_AD13 true
''    #define USE_AD14 true
''    #define USE_AD15 true
''    #define USE_AD16 true
''    #define USE_AD17 true
''    #define USE_AD18 true
''    #define USE_AD19 true
''    #define USE_AD20 true
''    #define USE_AD21 true
''    #define USE_AD22 true
''    #define USE_AD23 true
''    #define USE_AD24 true
''    #define USE_AD25 true
''    #define USE_AD26 true
''    #define USE_AD27 true
''    #define USE_AD28 true
''    #define USE_AD29 true
''    #define USE_AD30 true
''    #define USE_AD31 true
''    #define USE_AD32 true
''    #define USE_AD33 true
''    #define USE_AD34 true
''
''    'Family of chips based on 16f1688x with ADCON3 register
''    #define USE_ADA0 true
''    #define USE_ADA1 true
''    #define USE_ADA2 true
''    #define USE_ADA3 true
''    #define USE_ADA4 true
''    #define USE_ADA5 true
''    #define USE_ADA6 true
''    #define USE_ADA7 true
''    #define USE_ADB0 true
''    #define USE_ADB1 true
''    #define USE_ADB2 true
''    #define USE_ADB3 true
''    #define USE_ADB4 true
''    #define USE_ADB5 true
''    #define USE_ADB6 true
''    #define USE_ADB7 true
''    #define USE_ADC0 true
''    #define USE_ADC1 true
''    #define USE_ADC2 true
''    #define USE_ADC3 true
''    #define USE_ADC4 true
''    #define USE_ADC5 true
''    #define USE_ADC6 true
''    #define USE_ADC7 true
''    #define USE_ADD0 true
''    #define USE_ADD1 true
''    #define USE_ADD2 true
''    #define USE_ADD3 true
''    #define USE_ADD4 true
''    #define USE_ADD5 true
''    #define USE_ADD6 true
''    #define USE_ADD7 true
''    #define USE_ADE0 true
''    #define USE_ADE1 true
''    #define USE_ADE2 true
'
'EndofTestprogram:
