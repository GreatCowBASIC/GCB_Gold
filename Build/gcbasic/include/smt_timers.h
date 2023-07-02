'    PIC Signal Measurment Timer (SMT) routines for the GCBASIC compiler
'    Copyright (C) 2017-2020  Evan Venn and William Roth

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
'
'   13/06/2017 - Initial Release
'   26/03/2021 - Prepended "Chip" to constant SMTClockSourceVariant as necessary

 #IFDEF PIC

      #Ifdef Var(SMT1CON0)
         DIM Sys_Dum as Byte ' dummy byte  - holder for _E bytes
         DIM SMT1Period  Alias Sys_Dum, SMT1PRU, SMT1PRH, SMT1PRL as Long
         DIM SMT1 Alias Sys_Dum, SMT1TMRU, SMT1TMRH,SMT1TMRL as Long
         Sys_DUM = 0
         SMT1 = 0
      #Endif

     #Ifdef Var(SMT2CON0)
         DIM SMT2Period  Alias Sys_Dum, SMT2PRU, SMT2PRH, SMT2PRL as Long
         DIM SMT2 Alias Sys_Dum, SMT2TMRU, SMT2TMRH,SMT2TMRL as Long
         Sys_DUM = 0
         SMT2 = 0
     #Endif


    #Script
     'This script defines constants for SMT Clock source
     'based upon 'SMTClockSourceVariant' in the chip's datfile

     IF CHIPSMTClockSourceVariant = 1 then
        '// does not support MFINTOSC (500KHz)
        SMT_MFINTOSC_16 = 4
        SMT_LFINTOSC = 3
        SMT_HFINTOSC = 2  '16MHz regardless of FOSC
        SMT_FOSC4 = 1
        SMT_FOSC = 0
     End if

     IF CHIPSMTClockSourceVariant = 2 then
        SMT_AT1_perclk = 6
        SMT_MFINTOSC = 5
        SMT_MFINTOSC_16 = 4
        SMT_LFINTOSC = 3
        SMT_HFINTOSC = 2 '16MHz regardless of FOSC
        SMT_FOSC4 = 1
        SMT_FOSC = 0
     End IF

     IF CHIPSMTClockSourceVariant = 3 then
        SMT_REF_CLK = 7
        SMT_SOSC = 6
        SMT_MFINTOSC_16 = 5
        SMT_MFINTOSC = 4
        SMT_LFINTOSC = 3
        SMT_HFINTOSC = 2 '16MHz regardless of FOSC
        SMT_FOSC =  1
        SMT_FOSC4 = 0
     End IF
   #ENDSCRIPT

   'Prescales   (Same for all variations)
    #Define SMTPres_1  0
    #Define SMTPres_2  1
    #Define SMTPres_4  2
    #Define SMTPres_8  3
    DIM SMTTEMP as LONG  'DIM HERE

 #ENDIF

Sub InitSMT1(In SMTSource,SMTPres)

  #Ifdef var(SMT1CON0)

     'Default to FOSC4 if source is unsupported
     IF ChipSMTClockSourceVariant = 1 then
        If SMTSource > 4 then 'Unsupported Source
           SMTSource = SMT_FOSC4
        End if
     End If

     If ChipSMTClockSourceVariant = 2 then
        If SMTSource > 6 then   'Unsupported Source
          SMTSource = SMT_FOSC4
        End IF
     END IF

     If ChipSMTClockSourceVariant = 3 then
        If SMTSource > 7 then   'Unsupported Source
          SMTSource = SMT_FOSC4
        End IF
     END IF

     SMT1CLK = SMTSource
     SMT1CON0 = SMTPres
     SET SMT1CON0_EN ON    '''Does not start the timer

  #ENDIF
End Sub

Sub InitSMT2(In SMTSource,SMTPres)
  #Ifdef var(SMT1CON0)

     'Default to FOSC4 if source is unsupported
     IF ChipSMTClockSourceVariant = 1 then
        If SMTSource > 4 then 'Unsupported Source
           SMTSource = SMT_FOSC4
        End if
     End If

     If ChipSMTClockSourceVariant = 2 then
        If SMTSource > 6 then   'Unsupported Source
          SMTSource = SMT_FOSC4
        End IF
     END IF

     If ChipSMTClockSourceVariant = 3 then
        If SMTSource > 7 then   'Unsupported Source
           SMTSource = SMT_FOSC4
        End IF
     END IF

     SMT2CLK = SMTSource
     SMT2CON0 = SMTPres
     SET SMT2CON0_EN ON    '''Does not start the timer
  #ENDIF
End Sub

MACRO ClearSMT1
  #IFDEF Var(SMT1TMRL)
      Sys_DUM = 0
      SMT1TMRU = 0
      SMT1TMRH = 0
      SMT1TMRL = 0
  #ENDIF
End MACRO

Macro ClearSMT2
  #IFDEF Var(SMT2TMRL)
      Sys_DUM = 0
      SMT2TMRU = 0
      SMT2TMRH = 0
      SMT2TMRL = 0
  #ENDIF
End Macro

MACRO SetSMT1(in SMTTEMP as LONG)
  'Write LSB LAST
  #Ifdef Var(SMT1TMRL)
     SMT1_U = SMTTEMP_U
     SMT1_H = SMTTEMP_H
     SMT1 = SMTTEMP
  #endif
End MACRO

MACRO SetSMT2(in SMTTEMP as LONG)
   'Write LSB LAST
  #Ifdef Var(SMT2TMRL)
     SMT2_U = SMTTEMP_U
     SMT2_H = SMTTEMP_H
     SMT2 = SMTTEMP
  #endif
End MACRO

MACRO STOPSMT1
  #IFDEF bit(SMT1GO)
      SET SMT1GO OFF
  #ENDIF
END MACRO

MACRO STOPSMT2
  #IFDEF bit(SMT2GO)
      SET SMT2GO OFF
  #ENDIF
END MACRO

MACRO STARTSMT1
  #IFDEF bit(SMT1GO)
      SET SMT1GO ON
  #ENDIF
END MACRO

MACRO STARTSMT2
  #IFDEF bit(SMT2GO)
      SET SMT2GO ON
  #ENDIF
END MACRO

MACRO SETSMT1PERIOD(IN SMTTEMP as LONG)
  SMT1PERIOD_U = SMTTEMP_U
  SMT1PERIOD_H = SMTTEMP_H
  SMT1PERIOD = SMTTEMP
END MACRO

MACRO SETSMT2PERIOD(IN SMTTEMP as LONG)
  SMT2PERIOD_U = SMTTEMP_U
  SMT2PERIOD_H = SMTTEMP_H
  SMT2PERIOD = SMTTEMP
END MACRO
