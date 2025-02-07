'    SD card routines for the GCBASIC compiler
'    Copyright (C) 2013-2025 Hugh Considine

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

'Notes:
'
'Required functions:
' - Find a file
' - Find all files in directory
' - Read a file
' - Append to a file
' - Replace a file
' - Edit a file?
' - Delete a file
'
'Related QB commands:
' - Open [for input | output | append | binary]
' - Get/Put
' - Input/Print
' - Dir (Can't use this one, clashes with GCBASIC command)
' - Kill
' - ChDir/MkDir/RmDir
' - Functions: EOF, CurrDir

'Changes:
' date: change description

'Initialisation routine
'#startup InitSD

'Hardware settings
'''@hardware All; SD card CS pin; SD_CS; IO_Pin

'Constants
'Errors that may occur
#define SD_ERR_NONE 0
#define SD_ERR_BLOCK_END 10
#define SD_ERR_DIR_NOT_FOUND 100

'Variables:
'SDVersion: Version of SD card (1 or 2)
'SDPartitionType: Type of last partition found with SDFindPartition
'SDError: Error number
Dim FirstFATAddress As Long
Dim FATRootDirectoryAddress As Long
Dim FATDataRegionAddress As Long
Dim FATCurrDirAddress As Long
Dim FATCurrDirCluster As Word
Dim FATCurrReadFileAddress As Long
Dim FATCurrReadFileCluster As Word

'''Sets up SD card. Resets card, reads version.
Sub InitSD
  Dir SD_CS Out

  SPIMode MasterFast, 0
  Wait 1 ms

  'Send at least 80 clock pulses, then delay
  Set SD_CS Off
  For SDCRC = 1 to 12
    If SDCRC = 11 Then Set SD_CS On
    SPITransfer 255, SDTemp
  Next
  Wait 10 us

  'Reset card
  SDSendCommand 0, 0, True

  'Check for SD 2.0 card
  'Send CMD8, see if bit 0x04 (bad command) bit is set in response
  SDVersion = 1
  SDSendCommand 8, 0x000001AA, True
  If SDResponse.2 = Off Then
    'Bad command bit clear, so card is V2
    SDVersion = 2
    'Get remaining 4 bytes
    Repeat 4
      SPITransfer 255, SDTemp
    End Repeat
    'Last byte should be 0xAA for SD2
  End If

  'ACMD41 - check if ready
  Repeat 500
    SDSendCommand 55, 0, True
    SDSendCommand 41, 0, True
    If SDResponse = 0 Then Goto SDInitDone
    Wait 1 ms
  End Repeat

  SDInitDone:
  Set SD_CS On

  'Set read block size to 512 bytes
  SDSendCommand 16, 512

  'Clear error
  SDError = SD_ERR_NONE

End Sub

'''Sends a command to the SD card
'''@param SDCommand Command to send
'''@param SDArgument Argument for the command (32 bits)
'''@param SDLeaveEnabled Leave SD card enabled after sending command
Sub SDSendCommand(In SDCommand, In SDArgument As Long, Optional SDLeaveEnabled = False)
  'Send command to SD card

  'Clear CRC
  SDCRC = 0
  Set SD_CS Off
  Wait 10 us

  'Check if card is busy
  Repeat 500
    SPITransfer 255, SDTemp
    If SDTemp <> 255 Then Goto SDCardNotBusy
    Wait 1 ms
  End Repeat
  SDCardNotBusy:

  'First byte has start bit (0), transaction bit (1), 6 bit command
  Set SDCommand.7 Off
  Set SDCommand.6 On
  SPITransfer SDCommand, SDTemp
  SDCalcCRC SDCommand

  'Then 32 bit (4 byte) argument
  SPITransfer SDArgument_E, SDTemp
  SDCalcCRC SDArgument_E
  SPITransfer SDArgument_U, SDTemp
  SDCalcCRC SDArgument_U
  SPITransfer SDArgument_H, SDTemp
  SDCalcCRC SDArgument_H
  SPITransfer [byte]SDArgument, SDTemp
  SDCalcCRC [byte]SDArgument

  'Then 7 bit CRC and 1 end bit (1)
  SPITransfer SDCRC * 2 + 1, SDTemp
  Wait 10 us

  'Get response
  Repeat 100
    SPITransfer 0xFF, SDResponse
    If SDResponse.7 = Off Then Goto SDEndSend
  End Repeat

  SDEndSend:
  If SDLeaveEnabled = False Then Set SD_CS On

End Sub

'''Reads the response to a command from the SD card.
Function SDReadResponse
  SPITransfer 0xFF, SDResponse
  SDReadResponse = SDResponse
End Function

'''Reads a sector from the SD card
'''@param SDLocation Sector number to read
'''@param SDOffset Bytes to skip at start of sector
'''@param SDDataBuffer Array to store read bytes in
'''@param SDByteCount Number of bytes to read
Sub SDReadDiskBlock(In SDLocation As Long, In SDOffset As Word, SDDataBuffer(), In SDByteCount As Word)

  Dim SDDiskLoc, SDBytesRead As Word
  SDDiskLoc = 0
  SDBytesRead = 0

  'On non SDHC, location must be absolute address, not block number. Multiply block by 512.
  SDLocation = SDLocation * 512

  'Select block to read
  SDSendCommand 17, SDLocation, True

  'Ignore first 255s
  Repeat 1000
    SPITransfer 255, SDDataIn
    If SDDataIn <> 255 Then Goto SDDiskBytesStarted
  End Repeat
  SDDiskBytesStarted:

  'If SDByteCount = 0, exit without reading any bytes
  'These will be read one by one later
  If SDByteCount = 0 Then Exit Sub

  'Read data block
  Repeat 512
    SPITransfer 255, SDDataIn
    'Have enough bytes been skipped?
    If SDDiskLoc >= SDOffset Then
      'Have enough bytes been read?
      If SDBytesRead < SDByteCount Then
        SDDataBuffer(SDBytesRead) = SDDataIn
        SDBytesRead += 1
      End If
    End If
    SDDiskLoc += 1
  End Repeat

  Set SD_CS On

End Sub

'''Reads a sector from the SD card, and uses a callback to process
'''Callback should use SDBytesRead (word) for byte number, and SDDataIn for
'''read byte. Can change SDByteCount (word) to 0 to stop read
'''@param SDLocation Sector number to read
'''@param SDOffset Bytes to skip at start of sector
'''@param SDCallback Subroutine to call for each incoming byte
'''@param SDByteCount Number of bytes to read
Sub SDReadDiskBlockCB(In SDLocation As Long, In SDOffset As Word, SDCallback As Word, Optional In SDByteCount As Word = 512)

  Dim SDDiskLoc, SDBytesRead As Word
  SDDiskLoc = 0
  SDBytesRead = 0

  'On non SDHC, location must be absolute address, not block number. Multiply block by 512.
  SDLocation = SDLocation * 512

  'Select block to read
  SDSendCommand 17, SDLocation, True

  'Ignore first 255s
  Repeat 1000
    SPITransfer 255, SDDataIn
    If SDDataIn <> 255 Then Goto SDDiskBytesStartedCB
  End Repeat
  SDDiskBytesStartedCB:

  'Read data block
  Repeat 512
    SPITransfer 255, SDDataIn
    'Have enough bytes been skipped?
    If SDDiskLoc >= SDOffset Then
      'Have enough bytes been read?
      If SDBytesRead < SDByteCount Then
        'SDDataBuffer(SDBytesRead) = SDDataIn
        IndCall SDCallback
        SDBytesRead += 1
      End If
    End If
    SDDiskLoc += 1
  End Repeat

  Set SD_CS On

End Sub

'''Read a single byte from the SD card
'''Call after using SDReadDiskBlock with length = 0
Function SDReadDiskByte
  Dim SDDiskLoc As Word

  'Transfer and increment location
  SPITransfer 255, SDReadDiskByte
  SDDiskLoc += 1

  'End of block?
  SDError = SD_ERR_NONE
  If SDDiskLoc >= 512 Then
    SDError = SD_ERR_BLOCK_END
  End If
End Function

'''Finish a read operation
Sub SDFinishRead
  'Read remaining bytes?

  'Disable interface
  Set SD_CS On
End Sub

'''Calculate current CRC code for SD card.
'''@hide
Sub SDCalcCRC(In SDNewCRC)

  Repeat 8
    Set C Off
    Rotate SDCRC Left

    'Get new bit 6 of CRC
    'bit 6 = in XOR (old bit 0)
    'Set SDCRC.0 Off
    If SDCRC.7 = On Then
      If SDNewCRC.7 = Off Then
        'C = 1, in = 0, XOR = 1
        Set SDCRC.0 On
      End If
    Else
      If SDNewCRC.7 = On Then
        'C = 0, in = 1, XOR = 1
        Set SDCRC.0 On
      End If
    End If
    Set SDCRC.7 Off

    'Update bit 3
    'bit 3 = bit 3 XOR bit 6
    If SDCRC.0 = On Then
      If SDCRC.3 = On Then
        Set SDCRC.3 Off
      Else
        Set SDCRC.3 On
      End If
    End If

    Rotate SDNewCRC Left
  End Repeat

End Sub

'''Find the starting sector of a partition
'''@param SDPartition Number of partition to find (1-4)
Function SDFindPartition(SDPartition) As Long
  Dim SDDiskLoc As Word
  SDDiskLoc = 430 + SDPartition * 16

  'Read partition table entry
  Dim SDBuffer(12)
  SDReadDiskBlock 0, SDDiskLoc, SDBuffer(), 12
  'Get partition starting sector, type
  SDPartitionType = SDBuffer(4)
  [byte]SDFindPartition = SDBuffer(8)
  SDFindPartition_H = SDBuffer(9)
  SDFindPartition_U = SDBuffer(10)
  SDFindPartition_E = SDBuffer(11)
End Function

'''Initialise FAT16 file access routines.
'''Finds the partition and then the root directory
'''@param FATPartStart Starting sector of FAT partition
Sub InitFAT(In FATPartStart As Long)

  'Get partition information
  Dim FATBytesPerSector As Word
  Dim SectorsPerFAT As Word
  Dim FATMaxRootEntries As Word

  SDReadDiskBlockCB FATPartStart, 0, @InitFATCB, 32

  'Address of first FAT
  FirstFATAddress = FATPartStart + FATReservedSectorCount
  'Address of root directory and data region
  FATRootDirectoryAddress = FirstFATAddress + NumberOfFATs * SectorsPerFAT
  FATDataRegionAddress = FATRootDirectoryAddress + FATMaxRootEntries / 16

  FATCurrDirAddress = FATRootDirectoryAddress

End Sub

'''Callback for InitFAT routine
'''@hide
Sub InitFATCB
  Dim FATBytesPerSector As Word
  Dim SectorsPerFAT As Word
  Dim FATMaxRootEntries As Word

  Select Case SDBytesRead
    'Reserved Sector Count
    Case 14: FATReservedSectorCount = SDDataIn
    'Bytes per sector
    '(Should be 512)
    Case 11: [byte]FATBytesPerSector = SDDataIn
    Case 12: FATBytesPerSector_H = SDDataIn
    'Sectors per cluster
    Case 13: FATSectorsPerCluster = SDDataIn

    'Number of FATs
    'Should be 2
    Case 16: NumberOfFATs = SDDataIn
    'Sectors per FAT
    Case 22: [byte]SectorsPerFAT = SDDataIn
    Case 23: SectorsPerFAT_H = SDDataIn
    'Max root directory entries
    Case 17: [byte]FATMaxRootEntries = SDDataIn
    Case 18: FATMaxRootEntries_H = SDDataIn
  End Select
End Sub

'''Change the current directory to that specified
'''@param FATName Name of directory to change in to
Sub FATChangeDir(FATNameIn As String)
  Dim FATFileStart As Word
  Dim FATFileSize As Long
  Dim FATFoundStartAddress As Long
  Dim FATName As String * 12
  FATName = FATNameIn

  'Change to root directory?
  If FATName = "/" Then
    FATCurrDirAddress = FATRootDirectoryAddress
    SDError = SD_ERR_NONE
  Else
    'Assume error, clear on success
    SDError = SD_ERR_DIR_NOT_FOUND

    'Find new directory in current directory
    FATFindInfo FATName
    If FATFileStart <> 0 Then
      If FATFileSize = 0 Then
        'Set current directory address
        FATCurrDirAddress = FATFoundStartAddress
        'Clear error
        SDError = SD_ERR_NONE
      End If
    End If
  End If

  'Reset directory listing
  FATDirReset
End Sub

'''Find info for an item in the current directory
'''@param FATName item to find info on
Sub FATFindInfo(FATName As String)
  Dim FATCurrByte As Word
  Dim FATSearchName As String * 12
  FATCurrByte = 0
  FATSearchName = FATName

  'Parse current directory
  SDReadDiskBlockCB FATCurrDirAddress, 0, @FATDirParserCB

  Dim FATFileStart As Word
  Dim FATFileSize As Long
  Dim FATFoundStartAddress As Long
  Dim FATBytesPerSector As Word

  FATFoundStartAddress = FATDataRegionAddress + [long](FATFileStart - 2) * FATSectorsPerCluster
' HSerPrint "Data region address: "
' HSerPrint FATDataRegionAddress
' HSerPrintCRLF
' HSerPrint "File start cluster: "
' HSerPrint FATFileStart
' HSerPrintCRLF
' HSerPrint "Sectors per cluster: "
' HSerPrint FATSectorsPerCluster
' HSerPrintCRLF
' HSerPrint "Bytes per sector: "
' HSerPrint FATBytesPerSector
' HSerPrintCRLF
' HSerPrint "Resulting start address: "
' HSerPrint FATFoundStartAddress
' HSerPrintCRLF
End Sub

'''Callback for parsing directory records
'''@hide
Sub FATDirParserCB
  'Things to extract:
  ' - File name and extension (8.3)
  ' - File attributes (single byte)
  ' - File start cluster (word)
  ' - File size (long)
  Dim FATFileName As String * 12
  Dim FATFileStart As Word
  Dim FATFileSize As Long
  Dim FATSearchName As String * 12
  Dim SDByteCount As Word

  Select Case FATCurrByte
    'Reading filename
    Case <= 10:
      'Store byte
      If SDDataIn <> 32 Then
        'Append . before extension
        If FATCurrByte = 8 Then
          FATFileNameSize += 1
          FATFileName(FATFileNameSize) = 0x2e
        End If
        FATFileNameSize += 1
        FATFileName(FATFileNameSize) = SDDataIn
      End If
      FATFileName(0) = FATFileNameSize
    'File attributes
    Case 0x0A: FATFileAttrib = SDDataIn
    'File starting cluster
    Case 0x1A: [byte]FATFileStart = SDDataIn
    Case 0x1B: FATFileStart_H = SDDataIn
    'File size
    Case 0x1C: [byte]FATFileSize = SDDataIn
    Case 0x1D: FATFileSize_H = SDDataIn
    Case 0x1E: FATFileSize_U = SDDataIn
    Case 0x1F: FATFileSize_E = SDDataIn
  End Select

  'Increment current byte
  If FATCurrByte = 0x1F Then
    'End of record, is this the correct file?
    If FATFileName = FATSearchName Then
      'Correct file, stop further reading
      SDByteCount = 0
    End If
    'End of directory? Indicated by 0 in first byte of filename
    If FATFileName(1) = 0 Then
      FATFileSize = 0
      FATFileStart = 0
      SDByteCount = 0
    End If
    'Prepare for next record
    FATCurrByte = 0
    FATFileNameSize = 0
  Else
    FATCurrByte += 1
  End If

End Sub

'''Reset directory file list to retrieve name of first file
Sub FATDirReset

End Sub

'''Retrieve next filename in directory
Function FATDirNextFile As String * 12

End Function

'''Open a file for reading
'''@param FATName Name of file to read
Sub FATFileOpenRead(FATName As String)

End Sub

'''Seek to a particular location in the current file
Sub FATFileSeek(FATLocation As Long)

End Sub

'''Read the next byte from the current file
Sub FATFileReadByte

End Sub

''Read the next line from the current file
Sub FATFileReadLine

End Sub
