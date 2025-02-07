'''Copyright 2015 -2025 Jaques Nilo
'''
'''GCB Library for reading/writing to High Endurance Flash Memory
'''-----------------------------------------------------------------------------
'''The high endurance flash memory (HEFM) is a block of 128 locations found
'''at the top of the Flash program memory. Each location can only be used to
'''hold a byte variable (whereas the standard Flash memory for a mid-range
'''PIC MCU will typically hold 14 bits of information).
'''
'''The main difference between EEPROM and HEFM is that the former does allow
'''byte-by-byte erase whereas the latter does not. With HEFM data must be
'''erased before a write and this can only be performed in blocks
'''(referred as rows) of a fixed size associated with the chip design.
'''
'''The heflash.h library provide the following variables for the current
'''list of chips supporting this design :
'''  HEFLASH_ROWSIZE : size of a HEFM row. 16 or 32 words (byte variable)
'''  HEFLASH_START   : starting address of HEFM (word variable)
'''  HEFLASH_END     : ending address of HEFM (word variable)
'''
'''These are the user's oriented functions/subroutines:
'''  HEFreadBlock  : sub to read a HEFM row into a n bytes buffer array
'''  HEFreadByte   : byte function to read a HEFM row at a given offset
'''  HEFRead       : sub equivalent to EPRead sub for EEPROM
'''  HEFwriteBlock : sub to write a buffer array of n bytes in a HEFM row
'''  HEFWrite      : sub equivalent to EPWrite sub for EEPROM
'''
'''WORD OF CAUTION:
'''Whenever you update the hex file of your PIC with your programmer you
'''will erase the data that are stored in HEFM.  If you want to avoid that
'''you will have to flash your PIC with a software that allow memory exclusion
'''when flashing. This is the case of Microchip MPLAB IPE :
'''(Go In Advanced Mode/Enter password/Select Memory/Tick Preserve Flash
'''on Program/Enter Start and End address of your HEFM).
'''
'''This program has been adaptated from the Microchip Application note
'''AN1673 "Using the PIC16F1XXX High-Endurance Flash (HEF) block
'''by Lucio Di Jasio, 2014
'''
'''Log of version changes:
'''1.0 Original release
'''1.1 HEFWrite sub fixed
'''1.2 Include inline comments. Hide internal functions
'''1.3 FLASH_read/write made private. FLASH_readconfig removed.
'''    Fews subs and var names changed for better consistency
'''
'''@author  Jacques Nilo
'''@licence GPL
'''@version 1.3
'''@date    05.06.2015
'''*****************************************************************************
'
'    Revised 28.08.2017 to resolve non-ANSI characters
'
;
; Computes HEF global variables from *.dat file characteristics
#script
If ChipEEPROM = 0 And Var(PMDATH) And Bit(LWLO) Then
    HEFLASH_END = ChipWords - 1
    HEFLASH_START = HEFLASH_END - 127
    HEFLASH_ROWSIZE = 16
    If ChipWords > 2048 Then
        HEFLASH_ROWSIZE = 32
    End If
End If

if NoVar(PMDATH) And NoBit(LWLO) Then
  Warning "This is probably NOT a microprocessor with HFEM capabilities"
  Warning "Subsequent error messages may be related to lack of HFEM capabilities"
end if

#endscript
'''@hide
function FLASH_read(in FlashAddress as word) as word
;
; This word function returns the memory value stored at
; absolute address [Flashaddress]
; Used by HEF subs and functions. For experienced programmers only.
;
Dim FlashAddress alias PMADRH,PMADRL
Dim FLASH_read   alias PMDATH,PMDATL
PMCON1.CFGS=0       'Select the Flash address space
PMCON1.RD=1         'Read control bit on
NOP
NOP
end function

'''@hide
sub FLASH_unlock
;
; This subroutine unlocks Flash memory to allow erase or write operation
; Used by HEF subs and functions. For experienced programmers only.
;
; Unlock sequence
PMCON2 = 0x55
PMCON2 = 0xAA
PMCON1.WR=1
NOP
NOP
end sub

'''@hide
sub FLASH_write(In FlashAddress as word,In FlashDataValue as word,in Flashlatch)
;
; This subroutine writes the word [FlashDataValue] at absolute address given
; in the word variable [Flashaddress]
; When Flashlatch is 1 no real write is performed.
; When Flashlatch is 0 the real write function is executed
; Used by HEF subs and functions. For experienced programmers only.
;
Dim FlashAddress   alias PMADRH,PMADRL
Dim FlashDataValue alias PMDATH,PMDATL
Dim old_GIE as bit
old_gie = INTCON.GIE    'Save initial GIE state
INTCON.GIE=0            'Disable interrupts
PMCON1.LWLO=Flashlatch  '1=latch 0=write row
PMCON1.CFGS=0           'Select the Flash address space
PMCON1.FREE=0           'Next operation will be a write
PMCON1.WREN=1           'Enable Flash Memory write/erase
Flash_unlock            'Unlock memory
PMCON1.WREN=0           'Enable Flash Memory write/erase
INTCON.GIE=old_gie      'Restore initial GIE state
end sub

'''@hide
sub FLASH_erase(In FlashAddress as word)
;
; This subroutine erase the full HEM row to which belongs the given
; address [Flashaddress]
; Flashaddress must always be comprised betwwen HEFLASH_START & _END
; Used by HEF subs and functions. For experienced programmers only.
;
Dim FlashAddress alias PMADRH,PMADRL
Dim old_GIE as bit
old_gie = INTCON.GIE    'Save initial GIE state
INTCON.GIE=0            'Disable interrupts
PMCON1.CFGS=0           'Select the Flash address space
PMCON1.FREE=1           'Next operation will be a write
PMCON1.WREN=1           'Enable Flash Memory write/erase
Flash_unlock            'Unlock memory
PMCON1.WREN=0           'Enable Flash Memory write/erase
INTCON.GIE=old_gie      'Restore initial GIE state
end sub

'''Write a block of data to HEFM. (Lenght should be < row size)
'''@param Flash_rel_addr HEFM block number (0,...,HEFLASH_ROWSIZE-1)
'''@param Flash_buffer HEFM block source buffer
'''@param Flashcount # of bytes to write to HEFM block (<HEFLASH_ROWSIZE)
sub HEFwriteBlock(in Flash_rel_addr, in Flash_buffer(),in FlashCount)
;
; This subroutine writes Flashcount bytes from the Flash_buffer() array
; at a given row # of the HEM
; row [Flash_rel_addr] is generally comprised between 0 and 3
; The writing starts at the begining at the first byte of the row
; Flashcount must always be lower than HEFLASH_ROWSIZE
;
Dim Flash_abs_addr as word
; Obtain absolute address if HEFlash row
Flash_abs_addr=Flash_rel_addr*HEFLASH_ROWSIZE+HEFLASH_START
; Erase the entire row
FLASH_erase(Flash_abs_addr)
index=0
; Fill the latches with data
do while FlashCount > 1
; Load data in latches without writing
  FLASH_Write(Flash_abs_addr+index,Flash_buffer(index+1),1)
  index = index + 1
  FlashCount = FlashCount - 1
loop
; Last write of data --> write
FLASH_Write(Flash_abs_addr+index,Flash_buffer(index+1),0)
end sub

'''Read a block of data from HEFM. (Lenght should be < row size)
'''@param Flash_buffer destination buffer (should be large enough)
'''@param Flash_rel_addr HEFM block number (0,...,HEFLASH_ROWSIZE-1)
'''@param Flashcount # of bytes to read from HEFM block (<HEFLASH_ROWSIZE)
sub HEFreadBlock(in Flash_buffer(),in Flash_rel_addr,in FlashCount)
;
; This subroutine returns in Flash_buffer array the [Flashcount] byte values
; stored in HEM given its row #
; row [Flash_rel_addr] is generally comprised between 0 and 3
; The reading starts at the begining at the first byte of the row
; Flashcount must therefore be always lower than HEFLASH_ROWSIZE
;
Dim Flash_abs_addr as word
; Obtain absolute address if HEFlash row
Flash_abs_addr=Flash_rel_addr*HEFLASH_ROWSIZE+HEFLASH_START
index=1
; Read content
do while FlashCount > 0
  Flash_buffer(index)=FLASH_Read(Flash_abs_addr+index-1)
  index=index+1
  FlashCount=FlashCount-1
loop
end sub

'''Read a byte of data from HEFM
'''@param Flash_rel_addr HEFM block number (0,...,HEFLASH_ROWSIZE-1)
'''@param Flash_offset offset within the HEFM block (<HEFLASH_ROWSIZE)
'''@return Byte of data retrieved
function HEFreadByte(in Flash_rel_addr, in Flash_offset) as byte
;
; Returns the byte value stored in HEM given row # and offset #
; row [Flash_rel_addr] is generally comprised between 0 and 3
; offset [Flash_offset] cannot be greater than HEFLASH_ROWSIZE
;
Dim Flash_abs_addr as word
; Add offset into HEFlash memory
Flash_abs_addr=Flash_rel_addr*HEFLASH_ROWSIZE+HEFLASH_START+Flash_offset
; Read content
HEFreadByte=FLASH_Read(Flash_abs_addr)
end function

'''Read a byte of data from HEFM
'''@param HEFaddress relative adress within global HEFM (0,...,127)
'''@param HEFDataValue Byte of data retrieved
sub HEFRead(in HEFaddress, out HEFDataValue)
;
; This subroutine returns in HEFDataValue the byte value stored in HEFM
; at relative location [HEFaddress] with 0 being the first HEFM location
; HEFaddress is generally be comprised between 0 and 127
; The syntax is therefore similar to the EPRead subroutine
;
Flash_rel_addr = HEFaddress / HEFLASH_ROWSIZE
Flash_offset = HEFaddress % HEFLASH_ROWSIZE
HEFDataValue = HEFreadByte(Flash_rel_addr,Flash_offset)
end sub

'''Write a byte of data to HEFM. Remaining block data kept unchanged
'''@param HEFaddress relative adress within global HEFM (0,...,127)
'''@param HEFDataValue Byte of data to be written
sub HEFwrite(in HEFaddress, in HEFDataValue)
;
; This subroutine write the byte data [HEFDataValue] in HEFM
; at relative location [HEFaddress] with 0 being the first HEFM location
; HEFaddress is generally be comprised between 0 and 127
; The syntax is therefore similar to the EPWrite subroutine
; The data already existing in the row of [HEFaddress] are preserved
; If you need to save several data at once in the same row the
; HEFLASH_writeBlock subroutine will be more efficient
;
dim HEFTempBuffer(HEFLASH_ROWSIZE)
; Compute row #
Flash_rel_addr = HEFaddress / HEFLASH_ROWSIZE
; Compute offset # in row
Flash_offset = HEFaddress % HEFLASH_ROWSIZE
; Save data previously stored in row #
HEFreadBlock(HEFTempBuffer(),Flash_rel_addr,HEFLASH_ROWSIZE)
; Update row buffer with HEFDataValue at offset
HEFTempBuffer(Flash_offset+1)=HEFDataValue
; Write back the updated row buffer in HEFM
HEFwriteBlock(Flash_rel_addr,HEFTempBuffer(),HEFLASH_ROWSIZE)
end sub
