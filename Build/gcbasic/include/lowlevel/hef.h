'    GCB Library for reading/writing to High Endurance Flash Memory (HEF)
'    and Storage Area Memory (SAF) on PIC Microcontrollers
'    Copyright (C) 2018-2024  William Roth, Evan Venn

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
'    23.08.2023    Revised to ensure  NVM operations in SYSTEM.H work by adding `SAF_ROWSIZE_BYTES = CHIPWRITEFLASHBLOCKSIZE` in script

#option REQUIRED PIC ChipHEFMemWords %NoHEFRAM%
#option REQUIRED AVR ChipHEFMemWords "HEF. HEF not supported by AVR chips"

 #Script

    ChipHEFMemWords = ChipHEFMemWords + 0 'Necessary if missing from datfile
    ChipSAFMemWords = ChipSAFMemWords + 0 'Necessary if missing from datfile

     ' Need to define these to prevent
     ' compiler warnings if chip has no
     ' HEF or SAF Memmory and user attempts
     ' to use HEF/SAF commands in source
      HAS_HEFSAF = FALSE
      HEF_ROWSIZE_BYTES = 2
      HEF_ROWSIZE_WORDS = 2
      HEF_START_ADDR = 32
      HEF_NUM_BLOCKS = 0

    IF ChipHEFMemWords = 128 OR ChipSAFMemWords = 128 OR ChipSAFMemWords = 1024 then
       HAS_HEFSAF = TRUE
    End IF

    If HAS_HEFSAF = True then

        If ChipFamily = 15 then '16F with HEF
           '16F with HEF and PMCON instead of NVMCON
           HEF_ROWSIZE_WORDS = ChipEraseRowSizeWords
           HEF_ROWSIZE_BYTES = ChipEraseRowSizeWords 'same as words on 16F

           IF ChipHEFMemWords = 128  then
              HEF_WORDS = ChipHEFMemWords  'Words or Bytes
              HEF_BYTES = ChipHEFMemWords
              HEF_START_ADDR = Chipwords - ChipHEFMemWords
              HEF_NUM_BLOCKS = ChipHEFMemWords / ChipEraseRowSizeWords
           END IF

           IF ChipSAFMemWords = 128  then
              HEF_WORDS = ChipSAFMemWords  'Words or Bytes
              HEF_BYTES = ChipSAFMemWords
              HEF_START_ADDR = Chipwords - ChipSAFMemWords
              HEF_NUM_BLOCKS = ChipSAFMemWords / ChipEraseRowSizeWords

           END IF

           If HEF_NUM_BLOCKS = 4  then
              HEF_BLOCK0_ADDR = HEF_START_ADDR
              HEF_BLOCK1_ADDR = HEF_START_ADDR + 32
              HEF_BLOCK2_ADDR = HEF_START_ADDR + 64
              HEF_BLOCK3_ADDR = HEF_START_ADDR + 96
           End if

           If HEF_NUM_BLOCKS = 8  then
              HEF_BLOCK0_ADDR = HEF_START_ADDR
              HEF_BLOCK1_ADDR = HEF_START_ADDR + 16
              HEF_BLOCK2_ADDR = HEF_START_ADDR + 32
              HEF_BLOCK3_ADDR = HEF_START_ADDR + 48
              HEF_BLOCK4_ADDR = HEF_START_ADDR + 64
              HEF_BLOCK5_ADDR = HEF_START_ADDR + 80
              HEF_BLOCK6_ADDR = HEF_START_ADDR + 96
              HEF_BLOCK7_ADDR = HEF_START_ADDR + 112
           END IF
        End IF

        IF ChipFamily = 16  Then
            '18F with 256 bytes SAF - Uses Table Reads
            HEF_ROWSIZE_WORDS = ChipEraseRowSizeWords
            HEF_ROWSIZE_BYTES = ChipEraseRowSizeWords * 2

            HEF_ROWSIZE_BYTES = ChipEraseRowSizeWords * 2
            HEF_START_ADDR = (Chipwords * 2) - (ChipSAFMemWords * 2)
            HEF_WORDS = ChipSAFMemWords
            HEF_BYTES = ChipSAFMemWords * 2  'Bytes
            HEF_NUM_BLOCKS = HEF_BYTES / HEF_ROWSIZE_BYTES

            If HEF_NUM_BLOCKS = 2 then
               HEF_BLOCK0_ADDR = HEF_START_ADDR
               HEF_BLOCK1_ADDR = HEF_START_ADDR + 128
            End if

            If HEF_NUM_BLOCKS = 4 then
               HEF_BLOCK0_ADDR  = HEF_START_ADDR
               HEF_BLOCK1_ADDR  = HEF_START_ADDR + 64
               HEF_BLOCK2_ADDR  = HEF_START_ADDR + 128
               HEF_BLOCK3_ADDR  = HEF_START_ADDR + 192
            End if
        END IF
    End IF

    If HAS_HEFSAF = FALSE Then
        // Needed to support NVM operations in SYSTEM.H
        If NODEF(ChipEraseRowSizeWords) Then
            If DEF(CHIPWRITEFLASHBLOCKSIZE) Then     
                ChipEraseRowSizeWords = CHIPWRITEFLASHBLOCKSIZE
                SAF_ROWSIZE_BYTES = CHIPWRITEFLASHBLOCKSIZE
            End If
        End If
    End If

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
      Dim _HEF_Index, _GIE_Save, _HEF_Offset
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

Function HEFRead(in _HEF_REL_ADDR) as Byte

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16
          NVMCON1.REG0 = 0 ; point to Program Flash Memory
          NVMCON1.REG1 = 1 ; access Program Flash Memory

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*+
          HEFRead = TABLAT

      #Endif

      #IFDEF ChipFamily 15

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

         _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          NVMCON1.NVMREGS = 0    'Select the Flash address space
          NVMCON1.RD = 1         'Read control bit on
          NOP
          NOP
          HEFRead = NVMDATL

      #ENDIF
  #ENDIF

 End Function

Function HEFReadWord(IN _HEF_REL_ADDR2) as Word

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16

          NVMCON1.REG0 = 0 ; point to Program Flash Memory
          NVMCON1.REG1 = 1 ; access Program Flash Memory

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR2

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*+
          HEFReadWord = TABLAT
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

             HEFReadword = NVMDATL
             _HEF_ABS_ADDR++  'Next HEF Aaddress
             NVMCON1.RD = 1
             NOP
             NOP
            HEFReadWord_H = NVMDATL

        #ENDIF

  #Endif

END Function

Sub HEFRead(in _HEF_REL_ADDR, out _HEF_DataByte as Byte)
  'overloaded sub byte only
  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16
          NVMCON1.REG0 = 0 ; point to Program Flash Memory
          NVMCON1.REG1 = 1 ; access Program Flash Memory

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*
          _HEF_DataByte = TABLAT

      #Endif

      #IFDEF CHIPFamily 15
          ; Returns in _HEF_HEF_DataOut the byte value stored in HEFM
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

Sub HEFRead(in _HEF_REL_ADDR, out _HEF_DataWord as Word)
  'overloaded sub Word or byte

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16
          NVMCON1.REG0 = 0 ; point to Program Flash Memory
          NVMCON1.REG1 = 1 ; access Program Flash Memory

          _HEF_ABS_ADDR = HEF_START_ADDR + _HEF_REL_ADDR

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          TBLRD*
          _HEF_DataWord = TABLAT '14-bit word not the same as HEFreadWord


      #Endif

      #IFDEF CHIPFamily 15
          ; Returns in _HEF_HEF_DataOut the byte value stored in HEFM
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



Sub HEFReadWord(IN _HEF_REL_ADDR, OUT _HEF_DataWord as Word)

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16

          NVMCON1.REG0 = 0 ; point to Program Flash Memory
          NVMCON1.REG1 = 1 ; access Program Flash Memory

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

Sub HEFReadBlock(in _HEF_BlockNum, Out _HEF_Buffer(),Optional in _HEF_Count = HEF_ROWSIZE_BYTES)

 #IFDEF HAS_HEFSAF TRUE

   ; This subroutine returns in _HEF_Buffer array the [_HEF_Count] byte values
   ; stored in HEF given its row/block #

    #IFDEF ChipFamily 16
        'Reads a row of SAF Data and puts it into a array/buffer
        ' for later use

        NVMCON1.REG0 = 0 ; point to Program Flash Memory
        NVMCON1.REG1 = 1 ; access Program Flash Memory

        _HEF_ABS_ADDR =  HEF_START_ADDR + (_HEF_BlockNum * HEF_ROWSIZE_BYTES)

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

Sub HEFEraseBlock(In _HEF_BlockNum)

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16

          _HEF_ABS_ADDR = HEF_START_ADDR + (HEF_ROWSIZE_BYTES * _HEF_BlockNum)

          NVMCON1 = 0
          NVMCON1.REG0 = 0           'Select the Flash address space
          NVMCON1.Reg1 = 1
          NVMCON1.FREE = 1           'Next operation will be erase
          NVMCON1.WREN = 1           'Enable Flash Memory write/erase

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          _GIE_SAVE = INTCON0.GIE  'Save interrupt setting
          INTCON0.GIE = 0 'Disable interrupts

          'unlock Sequence
          NVMCON2 = 0x55
          NVMCON2 = 0xAA
          NVMCON1.WR = 1            'Write takes place here
          NOP
          NOP

          NVMCON1.WREN = 0           'Disable Flash Memory write/erase
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

Sub HEFWriteBlock(in _HEF_BlockNum, in _HEF_Buffer(), Optional in _HEF_Count = HEF_ROWSIZE_BYTES)
    ; This subroutine writes _HEF_Count bytes from the _HEF_Buffer() array
    ; at a given row # of the HEM

  #IFDEF HAS_HEFSAF TRUE

      #IFDEF ChipFamily 16

          ;Calculate Base Address of NVM Erase Block
         _HEF_ABS_ADDR =  HEF_START_ADDR + (_HEF_BlockNum * HEF_ROWSIZE_BYTES)

          TBLPTRU = _HEF_ABS_ADDR_U
          TBLPTRH = _HEF_ABS_ADDR_H
          TBLPTRL = _HEF_ABS_ADDR

          ; Erase the entire row - Call sub
          HEFEraseBlock (_HEF_BlockNum)

          NVMCON1.REG0 = 0     ; point to Program Flash Memory
          NVMCON1.REG1 = 1     ; access Program Flash Memory
          NVMCON1.FREE = 0     ; Next operation will be a write
          NVMCON1.WREN = 1     ; Enable Flash Memory write/erase

          _GIE_SAVE = INTCON0.GIE    'Save interrupt
          INTCON0.GIE = 0           'disable interrupts

          ; Fill the latches with data
          For _HEF_Index = 1 to _HEF_Count -1
           TABLAT = _HEF_Buffer(_HEF_Index)
           TBLWT*+
          Next
          'last byte
          TABLAT =  _HEF_Buffer(_HEF_Index +1)
          TBLWT*

          'unlock sequence
          NVMCON2 = 0x55
          NVMCON2 = 0xAA
          NVMCON1.WR = 1             ; Write takes place here

          NVMCON1.WREN = 0           ;Disable Flash Memory write/erase
          INTCON0.GIE = _GIE_SAVE     'restore saved interrupt
      #ENDIF


      #IFDEF ChipFamily 15

          Dim _HEF_ABS_ADDR Alias _HEF_DUMMY,_HEF_DUMMY, NVMADRH, NVMADRL as Long

          ;Calculate Base Address of NVM Erase Block
           _HEF_ABS_ADDR =  HEF_Block0_Addr + (_HEF_BlockNum * HEF_ROWSIZE_BYTES)

          ; Erase the entire row
           HEFEraseBlock (_HEF_BlockNum)

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

Sub HEFWrite(in _HEF_REL_ADDR, in _HEF_DataByte as Byte)

  #IFDEF HAS_HEFSAF TRUE


     ; Writes a single byte of data [_HEF_HEF_DataOut] in HEFM
     ; at relative location [_HEF_Address] between 0 and 127
     ;
     ; The syntax is similar to the EPWrite subroutine
     ; The existing data in the row of [_HEF_Address] is preserved

      ; Calculate block number
      _HEF_BlockNum = _HEF_REL_ADDR / HEF_ROWSIZE_BYTES

      ; Calculate offset in block/row
      _HEF_Offset = _HEF_REL_Addr % HEF_ROWSIZE_BYTES

      ; Save data previously stored in row#
      HEFReadBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

      ; Update buffer with DataValue at offset
      _HEF_Buffer(_HEF_Offset + 1) =  _HEF_DataByte

      ; Write back the updated row buffer to  HEFM
      HEFwriteBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

  #ENDIF

End sub

Sub HEFWriteWord(in _HEF_REL_ADDR, in _HEF_DataWord as Word)

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
      HEFReadBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

      ; Update buffer with DataValue at offset
      _HEF_Buffer(_HEF_Offset + 1) = _HEF_DataWord
      _HEF_Buffer(_HEF_Offset + 2) = _HEF_DataWord_H


      ; Write back the updated row buffer to  HEFM
      HEFwriteBlock(_HEF_BlockNum, _HEF_Buffer(), HEF_ROWSIZE_BYTES)

  #ENDIF

End sub
