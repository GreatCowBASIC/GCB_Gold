'    MicroChip PIC specific to read GetUserID routines for GCBASIC
'    Copyright (C) 2016-2024 Chris Roper

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

' Changes.
' 1.0      21/08/2016: C.A.Roper - Created
' 1.01     22/08/2016: Testing version
' 1.02     24/01/2017: Released version
' 1.03     20/02/2018: Updated license only

' ____________________________________________________________________
'
''' Usage:
'
' Dim User_ID as Word
' User_ID = GetUserID( Index )
'
' Reads the memory location 0x8000 + Index and returns it as a Word value.
;
' where Index = 0x0 to 0xB as follows:
;
;    Address      Function            Read  Write
;    8000h-8003h  User IDs              Yes   Yes
;    8006h/8005h  Device ID/Revision ID         Yes   No
;    8007h-800Bh  Configuration Words 1 through 5   Yes   No
;
; Refer to your particular Device Datasheet to confirm the address table
' ____________________________________________________________________
'


' Define the mandated variable
  'create WORD variable that is defined as follows.
  '16f create a word varaible that is aliased to _MVCON
  '18f alaises the registers
  dim NVMRes as word Alias NVMResH, NVMResL


#script

  GetUserIDCapability = 0
  GetUserID = GetUserID_Not16
  If Var(PMCON1) Then
   'warning "PMCON1"
    _NVMCon  = PMCON1
    _CFG     = CFGS
    _NVMADRL = PMADRL
    _NVMADRH = PMADRH
    _ValADRH = 0
    _NVMDATL = PMDATL
    _NVMDATH = PMDATH
    GetUserIDCapability = 1
  End If

  If Var(EECON1) Then
    if Bit(CFGS) then
      'warning "EECON1"
      _NVMCon  = EECON1
      _CFG     = CFGS
      _NVMADRL = EEADRL
      _NVMADRH = EEADRH
      _ValADRH = 0
      _NVMDATL = EEDATL
      _NVMDATH = EEDATH
      GetUserIDCapability = 1
    End if
  End If

  If Var(NVMCON1) Then
   'warning "NVMCON1"
    _NVMCon  = NVMCON1
    _CFG     = NVMREGS
    _NVMADRL = NVMADRL
    _NVMADRH = NVMADRH
    _ValADRH = 0x80
    _NVMDATL = NVMDATL
    _NVMDATH = NVMDATH
    GetUserIDCapability = 1
  End If

  if ChipFamily = 16 Then
    GetUserIDCapability = 1
    GetUserID = GetUserID_16
  end if

  if GetUserIDCapability = 0 Then
      Warning "GetUserID not supported by this microcontroller"
  end if


#endscript

#IFDEF ChipFamily 16

  function GetUserID_16 ( in regnum) as word

      #if  GetUserIDCapability = 1

              TBLPTRU = 0x20
              TBLPTRH = 0
              TBLPTRL = regnum
              TBLRD*
              GetUserID = TABLAT

      #endif

  end function

#ENDIF

#IFNDEF ChipFamily 16

    function GetUserID_Not16( in regnum ) as word

      #if  GetUserIDCapability = 1
               dim NVMWord as Word Alias NVMH,NVML
              _NVMADRL =  regnum  ; Store LSB of address
              _NVMADRH = _ValADRH ; Store MSB of address

              INToff      ; Disable interrupts
              set _CFG on   ; Select Configuration Space
              set  RD  on   ; Initiate read
              nop       ; Executed
              nop       ; Ignored
              INTon     ; Restore interrupts

              NVMH  = _NVMDATH
              NVML  = _NVMDATL
              GetUserID =  NVMWord
      #endif

    end function
#ENDIF
