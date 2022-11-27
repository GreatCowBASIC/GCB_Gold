'    GCB Library for reading/writing to High Endurance Flash Memory (HEF)
'    and Storage Area Memory (SAF) on PIC Microcontrollers
'    Copyright (C) 2018-2022  William Roth, Evan Venn

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

'    29.01.2019.   Updated to prevent _HEF_ABS_ADDR creating variables when no HEFM for ChipFamily 15
'    05.02.2019.   Further revised  to prevent _HEF_ABS_ADDR creating variables when no HEFM for ChipFamily 15
'    14.07.2020    Updated to support Q43 chips
'    20.12.2020    Revised to support Q41 chips by removing the #SameVar NVMLOCK, NVMCON2 as they are NOT the samevar
'    11.04.2022    Revised to support Q43 chips by adding bit tests in write operation


#option REQUIRED PIC ChipSAFMemWords "SAF.  No SAF memory."
#option REQUIRED AVR ChipSAFMemWords "SAF.  No SAF memory."

 #Script

 'For scripts see HEF.h

 #ENDSCRIPT

  #IFDEF HAS_HEFSAF TRUE

     'SameVars & Bits for 16F Family 15
      #SameVar INTCON, INTCON0
      #SameVar NVMDATH, PMDATH
      #SameVar NVMDATL, PMDATL
      #SameVar NVMADRH, PMADRH
      #SameVar NVMADRL, PMADRL
      #SameVar NVMCON1, PMCON1
      #SameVar NVMCON2, PMCON2
      #SameBit NVMREGS, CFGS


      #DEFINE SAF_ROWSIZE_BYTES    HEF_ROWSIZE_BYTES
      #DEFINE SAF_ROWSIZE_WORDS    HEF_ROWSIZE_WORDS
      #DEFINE SAF_START_ADDR       HEF_START_ADDR
      #DEFINE SAF_WORDS            HEF_WORDS
      #DEFINE SAF_BYTES            HEF_BYTES
      #DEFINE SAF_NUM_BLOCKS       HEF_NUM_BLOCKS

      #DEFINE SAF_BLOCK0_ADDR     HEF_BLOCK0_ADDR
      #DEFINE SAF_BLOCK1_ADDR     HEF_BLOCK1_ADDR
      #DEFINE SAF_BLOCK2_ADDR     HEF_BLOCK2_ADDR
      #DEFINE SAF_BLOCK3_ADDR     HEF_BLOCK3_ADDR
  #ENDIF

     '--- Variables ---
      Dim _HEF_Index, _HEF_Offset as word
      Dim _GIE_Save
      Dim _HEF_BlockNum, _HEF_REL_ADDR, _HEF_REL_ADDR2
      Dim _HEF_Buffer(HEF_ROWSIZE_BYTES)  '16, 32 or 128  Bytes depending upon chip


      #IFDEF ChipFamily 16
          Dim  _HEF_ABS_ADDR As Long  ' *** Do Not alias to TBLPTR ! ***
          Dim _HEF_Buffer(HEF_ROWSIZE_BYTES)
      #ENDIF

      #IFDEF Chipfamily 15
          #IFDEF HAS_HEFSAF TRUE
             '@05.02.2019.   Further revised  to prevent _HEF_ABS_ADDR creating variables when no HEFM for ChipFamily 15
             '               Move dimension of the array to the methods
            'Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long
            Dim _HEF_Buffer(HEF_ROWSIZE_BYTES)
          #endif
      #Endif



'*********** Subs & Functions   *************************

Function SAFRead(in _HEF_REL_ADDR) as Byte

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16
          #IF BIT(REG0)
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
          #ENDIF

          #IF BIT(NVMCMD0)
            NVMCMD0 = 0 ; Read operations
            NVMCMD1 = 0
            NVMCMD2 = 0
          #ENDIF

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*+
          SAFRead = TABLAT

      #Endif

      #IFDEF ChipFamily 15

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

         _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          NVMCON1.NVMREGS = 0    'Select the Flash address space
          NVMCON1.RD = 1         'Read control bit on
          NOP
          NOP
          SAFRead = NVMDATL

      #ENDIF
  #ENDIF

 End Function

Function SAFReadWord(IN _HEF_REL_ADDR2) as Word

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16

          #IF BIT(REG0)
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
          #ENDIF

          #IF BIT(NVMCMD0)
            NVMCMD0 = 0 ; Read operations
            NVMCMD1 = 0
            NVMCMD2 = 0
          #ENDIF

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR2

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*+
          SAFReadWord = TABLAT
          TBLRD*
          HEFReadWord_H = TABLAT
      #ENDIF

      #IFDEF ChipFamily 15

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR2

           NVMCON1.NVMREGS = 0
           NVMCON1.RD = 1
             NOP
             NOP

             SAFReadword = NVMDATL
             _HEF_ABS_ADDR++  'Next HEF Aaddress
             NVMCON1.RD = 1
             NOP
             NOP
            HEFReadWord_H = NVMDATL

        #ENDIF

  #Endif

END Function

Sub SAFRead(in _HEF_REL_ADDR, out _HEF_DataByte as Byte)
  'overloaded sub byte only
  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16
          #IF BIT(REG0)
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
          #ENDIF

          #IF BIT(NVMCMD0)
            NVMCMD0 = 0 ; Read operations
            NVMCMD1 = 0
            NVMCMD2 = 0
          #ENDIF
          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*
          _HEF_DataByte = TABLAT

      #Endif

      #IFDEF CHIPFamily 15
          ; Returns in _HEF_HEF_DataOut the byte value stored in SAFM
          ; at relative location [_HEF_Address] with 0 being the first HEFM location
          ; _HEF_Address is generally be comprised between 0 and 127
          ; The syntax is similar to the EPRead subroutine

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          NVMCON1.NVMREGS = 0    'Select the Flash address space
          NVMCON1.RD = 1         'Read control bit on
          NOP
          NOP
          _HEF_DataByte = NVMDATL

      #ENDIF

  #ENDIF

END SUB

Sub SAFRead(in _HEF_REL_ADDR, out _HEF_DataWord as Word)
  'overloaded sub Word or byte

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16
          #IF BIT(REG0)
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
          #ENDIF

          #IF BIT(NVMCMD0)
            NVMCMD0 = 0 ; Read operations
            NVMCMD1 = 0
            NVMCMD2 = 0
          #ENDIF

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*
          _HEF_DataWord = TABLAT '14-bit word not the same as HEFreadWord


      #Endif

      #IFDEF CHIPFamily 15
          ; Returns in _HEF_HEF_DataOut the byte value stored in SAFM
          ; at relative location [_HEF_Address] with 0 being the first HEFM location
          ; _HEF_Address is generally be comprised between 0 and 127
          ; The syntax is similar to the EPRead subroutine

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          NVMCON1.NVMREGS = 0    'Select the Flash address space
          NVMCON1.RD = 1         'Read control bit on
          NOP
          NOP
          _HEF_DataWord = NVMDATL
          _HEF_DataWord_H = NVMDATH

      #ENDIF

  #ENDIF

END SUB



Sub SAFReadWord(IN _HEF_REL_ADDR, OUT _HEF_DataWord as Word)

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16

          #IF BIT(REG0)
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
          #ENDIF
          #IF BIT(NVMCMD0)
            NVMCMD0 = 0 ; Read operations
            NVMCMD1 = 0
            NVMCMD2 = 0
          #ENDIF

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*+
          _HEF_DataWord = TABLAT
          TBLRD*
          _HEF_DataWord_H = TABLAT
      #ENDIF

      #IFDEF ChipFamily 15

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

           NVMCON1.NVMREGS = 0
           NVMCON1.RD = 1
           NOP
           NOP
          _HEF_DataWord = NVMDATL

          _HEF_ABS_ADDR++  'Next HEF Aaddress
           NVMCON1.RD = 1
           NOP
           NOP
           _HEF_DataWord_H = NVMDATL
      #ENDIF

  #ENDIF

End Sub

Sub SAFReadBlock(in _HEF_BlockNum, Out _HEF_Buffer(),Optional in _HEF_Count = HEF_ROWSIZE_BYTES)

 #IFDEF HAS_HEFSAF TRUE

    Dim _HEF_Count as Word

   ; This subroutine returns in _HEF_Buffer array the [_HEF_Count] byte values
   ; stored in SAF given its row/block #

    #IFDEF ChipFamily 16
        'Reads a row of SAF Data and puts it into a array/buffer
        ' for later use

          _HEF_ABS_ADDR =  HEF_START_ADDR + (_HEF_BlockNum * HEF_ROWSIZE_BYTES)

          #IF BIT(REG0)
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
            TBLPTRU = _HEF_ABS_ADDR_U
            TBLPTRH = _HEF_ABS_ADDR_H
            TBLPTRL = _HEF_ABS_ADDR

            for  _HEF_Index = 1 to _HEF_Count -1
               TBLRD*+ ; read into TABLAT and increment
              _HEF_Buffer(_HEF_Index) = TABLAT
            next
            'Get last byte
            TBLRD*
            _HEF_Buffer(_HEF_Index+1) = TABLAT
          #ENDIF

          #IF BIT(NVMCMD0)
            NVMCON1 = 0 ; Read operations
            TBLPTRU = _HEF_ABS_ADDR_U
            TBLPTRH = _HEF_ABS_ADDR_H
            TBLPTRL = _HEF_ABS_ADDR

            for  _HEF_Index = 1 to _HEF_Count -1
               TBLRD*+ ; read into TABLAT and increment
              _HEF_Buffer(_HEF_Index) = TABLAT
            next
            'Get last byte
            TBLRD*
            _HEF_Buffer(_HEF_Index+1) = TABLAT

          #ENDIF

          #IF VAR(NVMLOCK)
            NVMCON1 = 0 ; point to Program Flash Memory Q10 series
            TBLPTRU = _HEF_ABS_ADDR_U
            TBLPTRH = _HEF_ABS_ADDR_H
            TBLPTRL = _HEF_ABS_ADDR

            for  _HEF_Index = 1 to _HEF_Count -1
               TBLRD*+ ; read into TABLAT and increment
              _HEF_Buffer(_HEF_Index) = TABLAT
            next
            'Get last byte
            TBLRD*
            _HEF_Buffer(_HEF_Index+1) = TABLAT
          #ENDIF

    #ENDIF

    #IFDEF ChipFamily 15

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

        ; Obtain absolute address of HEFlash row
        _HEF_ABS_ADDR =  HEF_START_ADDR + (_HEF_BlockNum * HEF_ROWSIZE_BYTES)

        NVMCON1.NVMREGS  = 0       'Select the Flash address space

        for _HEF_Index  = 1 to _HEF_Count -1
           NVMCON1.RD = 1         'Read control bit on
           NOP
           NOP
          _HEF_Buffer(_HEF_Index) = NVMDATL
          _HEF_ABS_ADDR++
        next
         'get last byte
         NVMCON1.RD = 1         'Read control bit on
         NOP
         NOP
        _HEF_Buffer(_HEF_Index+1) = NVMDATL
    #ENDIF

 #ENDIF

End sub

Sub SAFEraseBlock(In _HEF_BlockNum)

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16
          _HEF_ABS_ADDR = HEF_START_ADDR + (HEF_ROWSIZE_BYTES * _HEF_BlockNum)

          _GIE_SAVE = INTCON0.GIE  'Save interrupt setting
          INTCON0.GIE = 0 'Disable interrupts



          #IF BIT(REG0)
            TBLPTRU = _HEF_ABS_ADDR_U
            TBLPTRH = _HEF_ABS_ADDR_H
            TBLPTRL = _HEF_ABS_ADDR

            NVMCON1 = 0
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
          #ENDIF
          #IF BIT(FREE)
            NVMCON1.FREE = 1           'Next operation will be erase
          #ENDIF
          #IF BIT(WREN)
            NVMCON1.WREN = 1           ''Enable Flash Memory write/erase
          #ENDIF

          #IF BIT(NVMCMD0)
            NVMADRU = _HEF_ABS_ADDR_U
            NVMADRH = _HEF_ABS_ADDR_H
            NVMADRL = _HEF_ABS_ADDR
            NVMCON1 = 2 ; Page read
            NVMCON0.GO = 1            'Write takes place here for chips with NVMCON0.GO
            NVMCON1 = 6 ; Erase operations
          #ENDIF


          #if var(NVMCON2)
            NVMCON2 = 0x55
            NVMCON2 = 0xAA
          #endif

          #if var(NVMLOCK)
            'For Q41 series
            NVMADRU = _HEF_ABS_ADDR_U
            NVMADRH = _HEF_ABS_ADDR_H
            NVMADRL = _HEF_ABS_ADDR
            NVMCON1 = 6 ; Erase operations
            NVMLOCK = 0x55
            NVMLOCK = 0xAA
            #if BIT(GO_NVMCON0)
              GO_NVMCON0 = 1
              wait while GO_NVMCON0 = 1
              NVMCON1 = NVMCON1 and 0XF8
            #endif
            #if BIT(NVMCON0.GO)
              NVMCON0.GO  = 1
              wait while NVMCON0.GO = 1
              NVMCON1 = NVMCON1 and 0XF8
            #endif

          #endif



          #IF BIT(REG0)
            NVMCON1.WR = 1            'Write takes place here for legacy chips with NVMCON1.WR
          #ENDIF
          #IF BIT(NVMCMD0)
            NVMCON0.GO = 1            'Write takes place here for chips with NVMCON0.GO
          #ENDIF
          NOP
          NOP

          #IF BIT(WREN)
            NVMCON1.WREN = 0           ''Disable Flash Memory write/erase
          #ENDIF
          #IF BIT(NVMCMD0)
            NVMCMD0 = 0 ; Read operations
            NVMCMD1 = 0
            NVMCMD2 = 0
          #ENDIF

          INTCON0.GIE = _GIE_SAVE    ' Restore saved interrupt setting
      #ENDIF

      #IFDEF ChipFamily 15

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

         _HEF_ABS_ADDR = HEF_START_ADDR + (HEF_ROWSIZE_BYTES * _HEF_BlockNum)

          NVMCON1 = 0
          NVMCON1.NVMREGS = 0
          NVMCON1.FREE = 1
          NVMCON1.WREN = 1

           _GIE_Save = INTCON.GIE   'Save interrupt setting
          INTCON.GIE = 0 'Disable Interrupts

          ;unlock Sequence
          NVMCON2 = 0x55
          NVMCON2 = 0xAA
          NVMCON1.WR = 1            'Write takes place here
          NOP
          NOP

          NVMCON1.WREN = 0           'Disable Flash Memory write/erase
          INTCON.GIE = _GIE_SAVE    ' Restore saved interrupt setting
      #ENDIF

  #ENDIF

End Sub

Sub SAFWriteBlock(in _HEF_BlockNum, in _HEF_Buffer(), Optional in _HEF_Count = HEF_ROWSIZE_BYTES)
    ; This subroutine writes _HEF_Count from the _HEF_Buffer() array
    ; at a given row # of the HEM

  #IFDEF HAS_HEFSAF TRUE

      Dim _HEF_Count as Word

      #IFDEF ChipFamily 16

          ;Calculate Base Address of NVM Erase Block for Chipfamily16
         _HEF_ABS_ADDR =  HEF_START_ADDR + (_HEF_BlockNum * HEF_ROWSIZE_BYTES)   '130816

          ; Erase the entire row - Call sub
          SAFEraseBlock (_HEF_BlockNum)

          _GIE_SAVE = INTCON0.GIE    'Save interrupt
          INTCON0.GIE = 0           'disable interrupts

          #IF BIT(REG0)
            TBLPTRU = _HEF_ABS_ADDR_U
            TBLPTRH = _HEF_ABS_ADDR_H
            TBLPTRL = _HEF_ABS_ADDR
            NVMCON1.REG0 = 0 ; point to Program Flash Memory
            NVMCON1.REG1 = 1 ; access Program Flash Memory
            NVMCON1.FREE = 0           'Next operation will be a write
            NVMCON1.WREN = 1           ''Enable Flash Memory write/erase
            ; Fill the latches with data
            For _HEF_Index = 1 to _HEF_Count -1
             TABLAT = _HEF_Buffer(_HEF_Index)
             TBLWT*+
            Next
            'last byte
            TABLAT =  _HEF_Buffer(_HEF_Index +1)
            TBLWT*

          #ENDIF

          #IF BIT(NVMCMD0)
            TBLPTRU = _HEF_ABS_ADDR_U
            TBLPTRH = _HEF_ABS_ADDR_H
            TBLPTRL = _HEF_ABS_ADDR
            NVMCON1 = 5                ' Page Write operations
            ; Fill the latches with data
            For _HEF_Index = 1 to _HEF_Count -1
             TABLAT = _HEF_Buffer(_HEF_Index)
             TBLWT*+
            Next
            'last byte
            TABLAT =  _HEF_Buffer(_HEF_Index +1)
            TBLWT*

          #ENDIF


          #if var(NVMCON2)
            NVMCON2 = 0x55
            NVMCON2 = 0xAA
          #endif

          #if var(NVMLOCK)
            TBLPTRU = _HEF_ABS_ADDR_U
            TBLPTRH = _HEF_ABS_ADDR_H
            TBLPTRL = _HEF_ABS_ADDR
            NVMCON1 = 5                ' Page Write operations

            ; Fill the latches with data
            For _HEF_Index = 1 to _HEF_Count -1
             TABLAT = _HEF_Buffer(_HEF_Index)
             TBLWT*+
            Next
            'last byte
            TABLAT =  _HEF_Buffer(_HEF_Index +1)
            TBLWT*

            NVMLOCK = 0x55
            NVMLOCK = 0xAA
            
            #if BIT(GO_NVMCON0)
              GO_NVMCON0 = 1
              wait while GO_NVMCON0 = 1
              NVMCON1 = NVMCON1 and 0XF8
            #endif
            #if BIT(NVMCON0.GO)
              NVMCON0.GO  = 1
              wait while NVMCON0.GO = 1
              NVMCON1 = NVMCON1 and 0XF8
            #endif

          #endif

          #IF BIT(REG0)
            NVMCON1.WR = 1            'Write takes place here for legacy chips with NVMCON1.WR
          #ENDIF
          #IF BIT(NVMCMD0)
            NVMCON0.GO = 1            'Write takes place here for chips with NVMCON0.GO
          #ENDIF


          #IF BIT(WREN)
            NVMCON1.WREN = 0           'Disnable Flash Memory write/erase
          #ENDIF
          #IF BIT(NVMCMD0)
            NVMCMD0 = 0 ; Read operations
            NVMCMD1 = 0
            NVMCMD2 = 0
          #ENDIF
          INTCON0.GIE = _GIE_SAVE     'restore saved interrupt
      #ENDIF


      #IFDEF ChipFamily 15
          ;Calculate Base Address of NVM Erase Block for Chipfamily16
          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

          ;Calculate Base Address of NVM Erase Block
           _HEF_ABS_ADDR =  HEF_Block0_Addr + (_HEF_BlockNum * HEF_ROWSIZE_BYTES)

          ; Erase the entire row
           SAFEraseBlock (_HEF_BlockNum)

          NVMCON1.CFGS = 0           'Select the Flash address space
          NVMCON1.FREE = 0           'Next operation will be a write
          NVMCON1.WREN = 1           'Enable Flash Memory write/erase

          NVMDATH = 0x3F  'Erase State Defaults
          NVMDATL = 0xFF

         _GIE_SAVE = INTCON.GIE    'Save initial GIE state
          INTCON.GIE = 0 'Disable INterrupts

           ; Fill the latches with data no write
          NVMCON1.LWLO = 1          '1=latch 0=write row

          for _HEF_Index = 1 to _HEF_Count -1
            ; Load data in latches without writing
              NVMDATL = _HEF_Buffer(_HEF_Index)  'Only write Lower Byte!

              NVMCON2 = 0x55
              NVMCON2 = 0xAA
              NVMCON1.WR = 1
              NOP
              NOP
              _HEF_ABS_ADDR++
          Next

          'last byte
          NVMDATL =  _HEF_Buffer(_HEF_Index + 1)
          NVMCON1.LWLO = 0  'write latches

          'unlock
          NVMCON2 = 0x55
          NVMCON2 = 0xAA
          NVMCON1.WR = 1 'Write !
          NOP
          NOP

          NVMCON1.WREN = 0           'Disable Flash Memory write/erase
          INTCON.GIE = _GIE_SAVE    ' Restore initial GIE state
      #ENDIF

  #ENDiF

End Sub

Sub SAFWrite(in _HEF_REL_ADDR, in _HEF_DataByte as Byte)

  #IFDEF HAS_HEFSAF TRUE


     ; Writes a single byte of data [_HEF_HEF_DataOut] in SAFM
     ; at relative location [_HEF_Address] between 0 and 127
     ;
     ; The syntax is similar to the EPWrite subroutine
     ; The existing data in the row of [_HEF_Address] is preserved

      ; Calculate block number
      _HEF_BlockNum = _HEF_REL_ADDR / HEF_ROWSIZE_BYTES

      ; Calculate offset in block/row
      _HEF_Offset = _HEF_REL_Addr % HEF_ROWSIZE_BYTES

      ; Save data previously stored in row#
      SAFReadBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

      ; Update buffer with DataValue at offset
      _HEF_Buffer(_HEF_Offset + 1) =  _HEF_DataByte

      ; Write back the updated row buffer to  HEFM
      SAFwriteBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

  #ENDIF

End sub

Sub SAFWriteWord(in _HEF_REL_ADDR, in _HEF_DataWord as Word)

  #IFDEF HAS_HEFSAF TRUE

      ; Writes a single WORD to 18F SAF Memory
      ; at relative locations [_HEF_REL_ADDR]
      ; where each address is an even number.
      ;
      ; Existing data in the row of [_HEF_REL_ADDR] is preserved

      ; Calculate block number
      _HEF_BlockNum = _HEF_REL_ADDR / HEF_ROWSIZE_BYTES

      ; Calculate offset in block/row
      _HEF_Offset = _HEF_REL_ADDR % HEF_ROWSIZE_BYTES

      ; Save data previously stored in row#
      SAFReadBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

      ; Update buffer with DataValue at offset
      _HEF_Buffer(_HEF_Offset + 1) = _HEF_DataWord
      _HEF_Buffer(_HEF_Offset + 2) = _HEF_DataWord_H


      ; Write back the updated row buffer to  HEFM
      SAFwriteBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

  #ENDIF

End sub
