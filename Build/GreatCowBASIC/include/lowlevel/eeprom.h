'    EEPROM routines for Great Cow BASIC
'    Copyright (C) 2006-2020 Hugh Considine, William Roth and Evan R. Venn

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
' 3/5/2006: Program EEPROM read/write added
' 16/5/2006: Write mode disabled at end of EPWrite
' 23/5/2006: EPWrite and EPRead changed to stop use of temporary vars
' 19/8/2006: Program EEPROM altered to take input as word
' 9/2/2007: Bug fix: EEPGD bit not set if not present
' 5/8/2007: Altered to use alias to access address, data vars
' 4/9/2007: Data EEPROM code altered to support AVR
' 15/10/2007: Bug fix: FREE bit not set if not present, altered to use IntOn/IntOff to control interrupt
' 4/12/2007: Added In/Out to parameters
' 17/6/2008: Altered to make EPRead a system sub, to allow use in ReadTable
' 19/4/2009: Bugfixes for AVR, allow EEPE instead of EEWE
' 4/3/2013: Corrections for PIC16F1847 (EEDAT instead of EEDATA)
' 23/2/2016: Use EEDATL instead of EEDATA (16F1825, possibly others)
' 16/3/16: Added supported for NVMADRH:NVMADRL
' 10/9/17: Removed depencies on Register.Bits - just use Bits. This prevents variables being created. And, change Script from NVMADRH to NVMADRL test.
' 21/10/18: Added EEPROM support for 18fxxq10 the   'Select DATA EE section (0x310000 - 0x3100FF) section
' 20/02/19: Revised bit check to ensure bit exists in ProgWrite - Bit(EEPGD)
' 20/04/19: Revised bit check to ensure bit exists in EPWrite - NVMEN and added cleared NVCON1
'           Set EEDATL_REF to whatever it is actually called (EEDAT, EEDATA or EEDATL)
' 23/09/19: Revised to handle K40 Per  Chip Errata Sheets to correctly support table reads on specific chips.
' 10/04/20: Revised to EEPGD within ProgramRead() and implement NVMREGS check
' 03/05/20: Moved ProgramErase, ProgramRead and ProgramWrite to system.h to ensure we isolate EEPRom code.
' 13/07/20: Revised to support Q43 chips
' 01/12/20: Revised script to support redirection of sysread()
' 02/01/20: Rewrite of NVMADR_EPWrite/EPread to better support 18fxxQxx chips/NVMADRU register moving to ChipSubFamily
' 15/08/21: Rewrite of NVMADR_EPWrite/EPread to better support 18fxxQxx chips/NVMADRU register moving to ChipSubFamily resolve RD issue and adding Q40 support
' 02/10/21: Fixed Q41 GO_NVMCON0 issue where GO is no longer valid.
' 02/01/22: Added suppoort for ChipFamily18FxxQ84
' 06/10/21: Fixed Q40 GO_NVMCON0 issue where GO is no longer valid.
' 28/02/23: Added suppoort for ChipFamily18FxxQ71

#option REQUIRED PIC CHipEEPROM %NoEEProm%
#option REQUIRED AVR CHipEEPROM %NoEEProm%


#script

  If Var(EEDATA) Then
    EEDATL_REF = EEDATA
  End If

  If Var(EEDAT) Then
    EEDATL_REF = EEDAT
  End If

  If Var(EEDATL) Then
    EEDATL_REF = EEDATL
  End If

  'Change from NVMADRH test as NVMADRH does not exist on every chip. The 18F25K42 for instance. 10/9/17
  If var(NVMADRL) Then
    EPWrite = NVMADR_EPWrite
    EPRead  = NVMADR_EPRead
    'Need to redefine SysEPRead to support READTABLE
    SysEPRead = NVMADR_EPRead
  End If


#endscript

sub EPWrite(In EEAddress, In EEDataValue)

#IFDEF PIC

  'Variable alias
  #IFNDEF Var(EEADRH)
    Dim EEAddress Alias EEADR  'erv
  #ENDIF

  #IFDEF Var(EEADRH)
    Dim EEAddress As Word Alias EEADRH, EEADR
  #ENDIF

  #ifdef EEDATL_REF
    Dim EEDataValue Alias EEDATL_REF
  #endif

  'Disable interrupt
  IntOff

  'Select data memory
  #IFDEF Bit(EEPGD)
    SET EEPGD OFF
  #ENDIF

 #IFDEF Bit(CFGS)
    Set CFGS OFF
  #ENDIF

  'Start write
  SET WREN ON
  EECON2 = 0x55
  EECON2 = 0xAA
  SET WR ON
  SET WREN OFF

  'Wait for write to complete
  WAIT WHILE WR ON
  SET WREN OFF

  'Restore interrupt
  IntOn

#ENDIF

#IFDEF AVR
  'Variable alias
  #IFDEF Var(EEARH)
    Dim EEAddress As Word Alias EEARH, EEARL
  #ENDIF
  #IFNDEF Var(EEARH)
    #IFDEF Var(EEAR)
      Dim EEAddress Alias EEAR
    #ENDIF
  #ENDIF
  Dim EEDataValue Alias EEDR

  'Enable write
  #IFDEF Bit(EEMWE)
    Set EEMWE On
  #ENDIF
  #IFNDEF Bit(EEMWE)
    #IFDEF Bit(EEMPE)
      Set EEMPE On
    #ENDIF
  #ENDIF
  'Start write, wait for it to complete
  #IFDEF Bit(EEWE)
    Set EEWE On
    Wait Until EEWE Off
  #ENDIF
  #IFNDEF Bit(EEWE)
    #IFDEF Bit(EEPE)
      Set EEPE On
      Wait Until EEPE Off
    #ENDIF
  #ENDIF

#ENDIF

end sub

'EPRead made into system subroutine
'Constant added to allow it to still be referred to as EPRead
#define EPRead SysEPRead
sub SysEPRead(In EEAddress, Out EEDataValue)

#IFDEF PIC

  'Variable alias
  #IFNDEF Var(EEADRH)
    Dim EEAddress Alias EEADR
  #ENDIF
  #IFDEF Var(EEADRH)
    Dim EEAddress As Word Alias EEADRH, EEADR
  #ENDIF
  #ifdef EEDATL_REF
    Dim EEDataValue Alias EEDATL_REF
  #endif

  'Disable interrupt
  IntOff

  'Select data memory
  #IFDEF Bit(EEPGD)
    SET EEPGD OFF
  #ENDIF
  #IFDEF Bit(CFGS)
    Set CFGS OFF
  #ENDIF

  'Read
  SET RD ON

  #IFDEF Oneof(CHIP_18F24K40,CHIP_18F25K40,CHIP_18F26K40,CHIP_18F27K40,CHIP_18F45K40,CHIP_18F46K40,CHIP_18F47K40,CHIP_18F65K40,CHIP_18F66K40,CHIP_18LF24K40, CHIP_18LF25K40, CHIP_18LF26K40, CHIP_18LF27K40, CHIP_18LF45K40, CHIP_18LF46K40, CHIP_18LF47K40, CHIP_18LF65K40, CHIP_18LF66K40)
     ' Added (Per  Chip Errata Sheets) to correctly support table reads on specific chips)
     ' Sets NVRAM pointer to Static RAM as default location.
      #Ifdef Var(NVMCON1)
         NVMCON1.7 = 1
         NVMCON1.6 = 0
      #endif
  #ENDIF


  'Restore interrupt
  IntOn
#ENDIF

#IFDEF AVR
  'Variable alias
  #IFDEF Var(EEARH)
    Dim EEAddress As Word Alias EEARH, EEARL
  #ENDIF
  #IFNDEF Var(EEARH)
    #IFDEF Var(EEAR)
      Dim EEAddress Alias EEAR
    #ENDIF
  #ENDIF
  Dim EEDataValue Alias EEDR

  'Start read
  Set EERE On



#ENDIF

end sub

function ReadEP(EEAddress)

#IFDEF PIC
  'Variable alias
  #IFNDEF Var(EEADRH)
    Dim EEAddress Alias EEADR
  #ENDIF
  #IFDEF Var(EEADRH)
    Dim EEAddress As Word Alias EEADRH, EEADR
  #ENDIF
  #ifdef EEDATL_REF
    Dim EEDataValue Alias EEDATL_REF
  #endif

  'Disable interrupt
  IntOff

  'Select data memory
  #IFDEF Bit(EEPGD)
    SET EEPGD OFF
  #ENDIF
  #IFDEF Bit(CFGS)
    Set CFGS OFF
  #ENDIF

  'Read
  SET RD ON

  'Restore interrupt
  IntOn
#ENDIF

#IFDEF AVR
  'Variable alias
  #IFDEF Var(EEARH)
    Dim EEAddress As Word Alias EEARH, EEARL
  #ENDIF
  #IFNDEF Var(EEARH)
    #IFDEF Var(EEAR)
      Dim EEAddress Alias EEAR
    #ENDIF
  #ENDIF
  Dim EEDataValue Alias EEDR

  'Start read
  Set EERE On
#ENDIF

end function


'  Section: Data EEPROM Module APIs

Sub NVMADR_EPWrite(IN SysEEAddress as WORD , in EEData)

    #samebit WR, GO

    dim IntState as bit
    IntState = GIE
    Dim SysEEPromAddress As Word

    #IFDEF Oneof(CHIP_18F24K40,CHIP_18F25K40,CHIP_18F26K40,CHIP_18F27K40,CHIP_18F45K40,CHIP_18F46K40,CHIP_18F47K40,CHIP_18F65K40,CHIP_18F66K40,CHIP_18LF24K40, CHIP_18LF25K40, CHIP_18LF26K40, CHIP_18LF27K40, CHIP_18LF45K40, CHIP_18LF46K40, CHIP_18LF47K40, CHIP_18LF65K40, CHIP_18LF66K40)
       ' Added (Per  Chip Errata Sheets) to correctly support table reads on specific chips)
       ' Sets NVRAM pointer to Static RAM as default location.
        #Ifdef Var(NVMCON1)
           Dim  NVMCON1_7_State, NVMCON1_6_State as bit
           NVMCON1_7_State = NVMCON1.7
           NVMCON1_6_State = NVMCON1.6
        #endif
    #ENDIF

   #IFDEF BIT(NVMREGS)
      'ALL 16F NVMREGS Devices Except 18FxxK40/K42
       SysEEPromAddress = SysEEAddress + 0x7000
       NVMADRH =SysEEPromAddress_h
       NVMADRL =SysEEPromAddress
       NVMDATL = EEData
       NVMREGS = 1
    #ENDIF

   #IFDEF BIT(NVMREG0)
        '' 18FXXk40/K42 and others with NVMREG0/NVMREG1
       NVMCON1 = 0
       SysEEPromAddress = SysEEAddress
       NVMADRH =SysEEPromAddress_h & 0X03  'ERV
       NVMADRL =SysEEPromAddress
        'Access EEPROM LOcations
        NVMREG0 = 0
        NVMREG1 = 0
        NVMDAT = EEData
   #ENDIF

    #IFDEF VAR(NVMADRU)

       Dim  NVMCON1_0_State, NVMCON1_1_State, NVMCON1_2_State as bit
       NVMCON1_0_State = NVMCON1.0
       NVMCON1_1_State = NVMCON1.1
       NVMCON1_2_State = NVMCON1.2

       #if ChipSubFamily = ChipFamily18FxxQ83
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ83
        NVMADRU = 0x38
        //Set the NVMCMD control bits for DFM Byte Read operation
        NVMCON1 = NVMCON1 and 0XF8 or 0x03' set bits ,1 and0
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ84
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ84
        NVMADRU = 0x38
        //Set the NVMCMD control bits for DFM Byte Read operation
        NVMCON1 = NVMCON1 and 0XF8 or 0x03' set bits ,1 and0
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ43
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ43
        NVMADRU = 0x38
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ41
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ41
        NVMADRU = 0x38
        //Set the NVMCMD control bits for DFM Byte Read operation
        NVMCON1 = NVMCON1 and 0XF8 or 0x03' set bits ,1 and0
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ40
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ40
        NVMADRU = 0x38
        //Set the NVMCMD control bits for DFM Byte Read operation
        NVMCON1 = NVMCON1 and 0XF8 or 0x03' set bits ,1 and0
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ10
       'Select DATA EE section (0x310000- 0x3103FF) for ChipFamily18FxxQ10
        NVMADRU = 0x31
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ71
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ71
        NVMADRU = 0x38
        //Set the NVMCMD control bits for DFM Byte Read operation
        NVMCON1 = NVMCON1 and 0XF8 or 0x03' set bits ,1 and0
       #ENDIF

       NVMADRH =SysEEAddress_h
       NVMADRL =SysEEAddress
       NVMDATL = EEData

    #ENDIF



    #if bit(FREE)
        FREE =0b'0'
    #endif
    #if bit(WREN)
        WREN= 0b'1'
    #endif
    #if bit(NVMEN)
      NVMEN=0b'1'
    #endif

    #if bit(NVMCMD0)
      NVMCON1 = 3
    #endif

    GIE = 0
    #IFNDEF VAR(ChipMemorylock)    'ChipMemorylock defined in .DAT
      NVMCON2 = 0x55
      NVMCON2 = 0xAA
    #ENDIF

    #IFDEF VAR(ChipMemorylock)
      ChipMemorylock = 0x55
      ChipMemorylock = 0xAA

      #If bit(GO_NVMCON0)
          GO_NVMCON0 =1
          wait while GO_NVMCON0 = 1
          NVMCON1 = NVMCON1 and 0XF8
      #Endif

    #ENDIF

    #if bit(WR)
      WR = 1
    #endif
    NOP     ' NOPs may be required for latency at high frequencies
    NOP
    NOP
    NOP
    NOP

    #if bit(WR)
        wait while WR = 1
    #endif

    #if bit(WREN)
        WREN= 0b'0'
    #endif
    #if bit(NVMEN)
      NVMEN=0b'0'
    #endif

    #IFDEF Oneof(CHIP_18F24K40,CHIP_18F25K40,CHIP_18F26K40,CHIP_18F27K40,CHIP_18F45K40,CHIP_18F46K40,CHIP_18F47K40,CHIP_18F65K40,CHIP_18F66K40,CHIP_18LF24K40, CHIP_18LF25K40, CHIP_18LF26K40, CHIP_18LF27K40, CHIP_18LF45K40, CHIP_18LF46K40, CHIP_18LF47K40, CHIP_18LF65K40, CHIP_18LF66K40)
       ' Added (Per  Chip Errata Sheets) to correctly support table reads on specific chips)
       ' Sets NVRAM pointer to Static RAM as default location.
        #Ifdef Var(NVMCON1)
           Dim  NVMCON1_7_State, NVMCON1_6_State as bit
           NVMCON1.7 = NVMCON1_7_State
           NVMCON1.6 = NVMCON1_6_State
        #endif
    #ENDIF

    #IFDEF VAR(NVMADRU)  'assumes therefore 18fxxQxx series at 1/1/2021
       NVMCON1.0 = NVMCON1_0_State
       NVMCON1.1 = NVMCON1_1_State
       NVMCON1.2 = NVMCON1_2_State
    #endif

    'Restore interrupt to initial  State
    GIE = IntState

end sub

Sub NVMADR_EPRead(IN SysEEAddress AS word  , out EEDataValue )

 #samebit WR, RD

  #IFDEF Oneof(CHIP_18F24K40,CHIP_18F25K40,CHIP_18F26K40,CHIP_18F27K40,CHIP_18F45K40,CHIP_18F46K40,CHIP_18F47K40,CHIP_18F65K40,CHIP_18F66K40,CHIP_18LF24K40, CHIP_18LF25K40, CHIP_18LF26K40, CHIP_18LF27K40, CHIP_18LF45K40, CHIP_18LF46K40, CHIP_18LF47K40, CHIP_18LF65K40, CHIP_18LF66K40)
     ' Added (Per  Chip Errata Sheets) to correctly support table reads on specific chips)
     ' Sets NVRAM pointer to Static RAM as default location.
      #Ifdef Var(NVMCON1)
         Dim  NVMCON1_7_State, NVMCON1_6_State as bit
         NVMCON1_7_State = NVMCON1.7
         NVMCON1_6_State = NVMCON1.6
      #endif
  #ENDIF

  #IFDEf bit(NVMREGS)
      Dim SysEEPromAddress As Word
      SysEEPromAddress = SysEEAddress + 0x7000
      NVMADRH = SysEEPromAddress_h
      NVMADRL = SysEEPromAddress
      NVMREGS = 1
      RD = 1
      NOP     ' NOPs may be required for latency at high frequencies
      NOP
      NOP
      NOP
      EEDataValue = NVMDATL
  #ENDIF

  #IFDEf bit(NVMREG0)
      Dim SysEEPromAddress As Word
      SysEEPromAddress = SysEEAddress
      NVMADRH =SysEEPromAddress_h
      NVMADRL =SysEEPromAddress
      NVMREG0 = 0
      NVMREG1 = 0
      RD = 1
      NOP     ' NOPs may be required for latency at high frequencies
      NOP
      NOP
      NOP
      EEDataValue = NVMDAT
  #ENDIF


  #IFDEF VAR(NVMADRU)

       #if ChipSubFamily = ChipFamily18FxxQ83
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ83
        NVMADRU = 0x38
        NVMADRH =SysEEAddress_h
        NVMADRL =SysEEAddress
       'Set the NVMCMD control bits for DFM Byte Read operation by clearing NVMCMD[2:0] NVM Command bits
        NVMCON1 = 0
        GO_NVMCON0 = 1
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ84
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ84
        NVMADRU = 0x38
        NVMADRH =SysEEAddress_h
        NVMADRL =SysEEAddress
       'Set the NVMCMD control bits for DFM Byte Read operation by clearing NVMCMD[2:0] NVM Command bits
        NVMCON1 = 0
        GO_NVMCON0 = 1
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ43
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ43
        NVMADRU = 0x38
        NVMADRH =SysEEAddress_h
        NVMADRL =SysEEAddress
       'Set the NVMCMD control bits for DFM Byte Read operation by clearing NVMCMD[2:0] NVM Command bits
        NVMCON1 = 0
        NVMCON0.GO = 1
       #ENDIF

       #if ChipSubFamily =  ChipFamily18FxxQ40
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ40
        NVMADRU = 0x38
        NVMADRH =SysEEAddress_h
        NVMADRL =SysEEAddress
       'Set the NVMCMD control bits for DFM Byte Read operation by clearing NVMCMD[2:0] NVM Command bits
        NVMCON1 = 0
        GO_NVMCON0 = 1
       #ENDIF

       #if ChipSubFamily =  ChipFamily18FxxQ41
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ41
        NVMADRU = 0x38
        NVMADRH =SysEEAddress_h
        NVMADRL =SysEEAddress
       'Set the NVMCMD control bits for DFM Byte Read operation by clearing NVMCMD[2:0] NVM Command bits
        NVMCON1 = 0
        GO_NVMCON0 = 1
       #ENDIF

       #if ChipSubFamily = ChipFamily18FxxQ10
       'Select DATA EE section (0x310000- 0x3103FF) for ChipFamily18FxxQ10
        NVMADRU = 0x31
        NVMADRH =SysEEAddress_h
        NVMADRL =SysEEAddress
        RD = 1
        NOP     ' NOPs added for latency at high frequencies
        NOP
        NOP
        NOP
       #ENDIF

       #if ChipSubFamily =  ChipFamily18FxxQ71
       'Select DATA EE section (0x380000 - 0x3803FF) for ChipFamily18FxxQ71
        NVMADRU = 0x38
        NVMADRH =SysEEAddress_h
        NVMADRL =SysEEAddress
       'Set the NVMCMD control bits for DFM Byte Read operation by clearing NVMCMD[2:0] NVM Command bits
        NVMCON1 = 0
        GO_NVMCON0 = 1
       #ENDIF

      EEDataValue = NVMDATL
  #ENDIF

  #IFDEF Oneof(CHIP_18F24K40,CHIP_18F25K40,CHIP_18F26K40,CHIP_18F27K40,CHIP_18F45K40,CHIP_18F46K40,CHIP_18F47K40,CHIP_18F65K40,CHIP_18F66K40,CHIP_18LF24K40, CHIP_18LF25K40, CHIP_18LF26K40, CHIP_18LF27K40, CHIP_18LF45K40, CHIP_18LF46K40, CHIP_18LF47K40, CHIP_18LF65K40, CHIP_18LF66K40)
     ' Added (Per  Chip Errata Sheets) to correctly support table reads on specific chips)
     ' Sets NVRAM pointer to Static RAM as default location.
      #Ifdef Var(NVMCON1)
         Dim  NVMCON1_7_State, NVMCON1_6_State as bit
         NVMCON1.7 = NVMCON1_7_State
         NVMCON1.6 = NVMCON1_6_State
      #endif
  #ENDIF


End Sub
