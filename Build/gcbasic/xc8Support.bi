' Function and Sub Declarations


' Usage
'  GetLatestDFPFile(chipName)
'  GetChipSpecifics(chipName, "element,attribute")

' =============================================================================
' MPLAB X DFP Header Finder
' =============================================================================
'
' Purpose:
'   This script locates the most recent Device Family Pack (DFP) header file (.inc)
'   for a specified PIC microcontroller chip within Microchip's MPLAB X or XC8
'   compiler installation directories. It scans predefined paths for versioned
'   folders, parses version numbers, and selects the highest version containing
'   the chip's header file.
'
' Usage:
'   Run with a chip name as the first command-line argument and an optional "DEBUG" flag:
'     fbc script.bas && script.exe PIC18F47Q43
'     fbc script.bas && script.exe PIC18F47Q43 DEBUG
'   Or run without arguments to prompt for chip name input.
'   - First argument: Chip name (e.g., PIC18F47Q43).
'   - Second argument (optional): If "DEBUG", enables debug output by setting DEBUG_MODE to -1.
'
' Functions:
'   - GetLatestDFPFile(chipNameRaw): Returns path to latest .inc file.
'   - GetChipSpecifics(chipNameRaw, param): Extracts attribute from latest .PIC file.
'     param format: "element,attribute" e.g., "Breakpoints,hwbpcount" returns "3"
'     or "CodeSector,endaddr" returns "16384" (hex 0x4000 converted to decimal).
'
' Output:
'   Prints the chip name followed by the path to the version folder containing
'   the header file, or "[Not found]" if no match is located.
'
' Dependencies:
'   - FreeBASIC compiler (fbc)
'   - Windows command-line environment (uses 'dir' and 'cmd' for folder listing)
'   - Access to Microchip paths (e.g., C:\Program Files\Microchip\*)
'
' Notes:
'   - Debug output is enabled by setting DEBUG_MODE to non-zero (e.g., via "DEBUG" argument).
'   - Version folders are expected in formats like "1.2" or "1.2.3" with non-negative integers.
'   - Creates and deletes a temporary file "folderlist.txt" during execution.
'   - Assumes Windows paths and permissions; limited error handling.
'
' Author: Evan R. Venn
' Version: 1.0
' Date: December 08, 2025
' License: Public Domain / Open Source (assumed)
'
' =============================================================================

' Constants for filename and folder-related strings
Const PATH_DELIMITER = "\"
Const TEMP_FILE_NAME = "folderlist.txt"
Const SHELL_DIR_PREFIX = "cmd /c dir """
Const SHELL_DIR_SUFFIX = """ /ad /b /s > "
Const XC8_INCLUDE_PATH = "xc8\pic\include\proc"
Const USER_MCHP_PACKS_PATH = "C:\Users\{username}\.mchp_packs"
Const MPLABX_PATH = "C:\Program Files\Microchip\MPLABX"
Const INC_FILE_EXTENSION = ".inc"
Const PIC_FILE_EXTENSION = ".PIC"
Const XC8_BASE_PATH = "C:\Program Files\Microchip\xc8"
Const DIR_WILDCARD = "\*"
Const XC8_PROC_PATH = "\pic\include\proc\"
Const PIC_EDC_PATH_SUFFIX = "\edc\"

Dim Shared DEBUG_MODE As Integer
Dim Shared cachedPICPath As String: cachedPICPath = ""
Dim Shared lastPICChip As String: lastPICChip = ""
DEBUG_MODE = 0  ' Set to 0 to disable debug output; set to non-zero (e.g., -1) to enable

' =============================================================================
' Sub DebugPrint(msg As String)
' =============================================================================
'
' Purpose:
'   Prints a debug message to the console if DEBUG_MODE is non-zero.
'   Provides verbose logging for debugging purposes.
'
' Parameters:
'   msg (String): The message to print.
'
' Returns:
'   None (void subroutine).
'
' Notes:
'   - Controlled by the global DEBUG_MODE variable.
'   - Used throughout the script for diagnostic output.
'
Sub DebugPrint(msg As String)

    Dim spacePos As Integer
    spacePos = Instr(msg, " ")

    If spacePos > 0 Then
        msg = Trim( Mid(msg, spacePos))
    End If

    If DEBUG_MODE or VBS = 1 Then 
        PRINT SPC(10);  msg
    End If
End Sub

' =============================================================================
' Sub SplitFolder(ByVal source As String, ByVal delimiter As String, result() As String)
' =============================================================================
'
' Purpose:
'   Splits a source string into an array of substrings based on a delimiter.
'   Populates the result array with the split segments.
'
' Parameters:
'   source (String, ByVal): The input string to split.
'   delimiter (String, ByVal): The delimiter to split on (e.g., "\").
'   result() (String array, ByRef): Array to store the split segments.
'
' Returns:
'   None (void subroutine). Modifies result array in-place.
'
' Notes:
'   - Uses InStr and Mid to locate and extract segments.
'   - Dynamically resizes an internal segments array with ReDim Preserve.
'   - Copies segments to result array to avoid scope issues.
'   - Assumes non-empty delimiter; no error checking for edge cases.
'
Sub SplitFolder(ByVal source As String, ByVal delimiter As String, result() As String)
    Dim segments() As String
    Dim startPos As Integer = 1
    Dim nextPos As Integer
    Dim count As Integer = 0

    Do
        nextPos = InStr(startPos, source, delimiter)
        If nextPos = 0 Then
            ReDim Preserve segments(count)
            segments(count) = Mid(source, startPos)
            Exit Do
        Else
            ReDim Preserve segments(count)
            segments(count) = Mid(source, startPos, nextPos - startPos)
            startPos = nextPos + Len(delimiter)
            count += 1
        End If
    Loop

    ' Copy segments into result()
    ReDim result(0 To UBound(segments))
    For i As Integer = 0 To UBound(segments)
        result(i) = segments(i)
    Next
End Sub

' =============================================================================
' Function ExtractVersionPath(path As String) As String
' =============================================================================
'
' Purpose:
'   Extracts the path up to and including the first valid version folder
'   (excluding the filename) from a given file path. Skips hidden folders
'   starting with a dot.
'
' Parameters:
'   path (String): The full file path to analyze.
'
' Returns:
'   String: The path from root to the version folder, or empty if no valid version.
'
' Notes:
'   - A valid version folder has 1 or 2 dot-separated non-negative numeric parts (e.g., "1.2" or "1.2.3").
'   - Uses Split to parse path and version segments.
'   - Skips segments starting with "." (e.g., hidden folders).
'   - Returns the path up to the first valid version folder encountered.
'   - Excludes the filename (last path component).
'
Function ExtractVersionPath(path As String) As String
    Dim parts() As String
    Dim versionPath As String
    SplitFolder(path, PATH_DELIMITER, parts())

    versionPath = ""

    For i As Integer = 0 To UBound(parts) - 1  ' exclude filename
        Dim segment As String
        segment = parts(i)

        ' Skip segments that start with a dot
        If Left(segment, 1) = "." Then Continue For

        Dim verParts() As String
        SplitFolder(segment, ".", verParts())

        If UBound(verParts) = 1 Or UBound(verParts) = 2 Then
            Dim valid As Integer
            valid = -1
            For j As Integer = 0 To UBound(verParts)
                If Val(verParts(j)) < 0 And verParts(j) <> "0" Then valid = 0
            Next

            If valid Then
                versionPath = ""
                For k As Integer = 0 To i + 1
                    If versionPath <> "" Then versionPath += PATH_DELIMITER
                    versionPath += parts(k)
                Next
                Return versionPath 
            End If
        End If
    Next

    Return ""
End Function


' =============================================================================
' Function IsVersionFolder(fullPath As String) As Integer
' =============================================================================
'
' Purpose:
'   Checks if a folder path is a valid version folder within the XC8/PIC structure.
'   Validates the path ends with "xc8\pic\include\proc" and contains a version segment.
'
' Parameters:
'   fullPath (String): The full folder path to validate.
'
' Returns:
'   Integer: -1 (true) if a valid version folder, 0 (false) otherwise.
'
' Notes:
'   - Checks for specific path suffix (case-insensitive).
'   - Scans backward for a version segment with 1 or 2 non-negative numeric parts.
'   - Used to filter folders during directory scanning.
'
Function IsVersionFolder(fullPath As String) As Integer
    ' Check path ending
    If Right(LCase(fullPath), Len(XC8_INCLUDE_PATH)) <> XC8_INCLUDE_PATH Then Return 0

    ' Split path into parts
    Dim parts() As String
    SplitFolder(fullPath, PATH_DELIMITER, parts())

    ' Scan backwards to find version folder
    For i As Integer = UBound(parts) To 0 Step -1
        Dim versionParts() As String
        SplitFolder(parts(i), ".", versionParts())
        If UBound(versionParts) = 2 Or UBound(versionParts) = 1 Then
            Dim valid As Integer = -1
            For j As Integer = 0 To UBound(versionParts)
                If Val(versionParts(j)) < 0 Then valid = 0
            Next
            If valid Then Return -1
        End If
    Next

    Return 0
End Function

' =============================================================================
' Function ParseVersion(path As String) As Long
' =============================================================================
'
' Purpose:
'   Converts a version segment in a path to a comparable Long integer score.
'   Supports 1 or 2 part versions (e.g., "1.2" or "1.2.3").
'
' Parameters:
'   path (String): The path containing the version segment.
'
' Returns:
'   Long: Version score (e.g., 1.2.3 -> 1002003), or 0 if no valid version.
'
' Notes:
'   - For 2-part versions (x.y.z), score = x*1e6 + y*1e3 + z.
'   - For 1-part versions, returns 0 (incomplete handling in code).
'   - Scans all path parts; returns first valid version score.
'   - Rejects negative numbers; allows zeros.
'
Function ParseVersion(path As String) As Long
    Dim parts() As String
    Dim element as Integer
    SplitFolder(path, PATH_DELIMITER, parts())
    For element As Integer = 0 To UBound(parts)
        Dim verParts() As String
        SplitFolder(parts(element), ".", verParts())
        If UBound(verParts) = 2 Or UBound(verParts) = 1 Then
            Dim valid As Integer = -1
            For j As Integer = 0 To UBound(verParts)
                If Val(verParts(j)) < 0 Then valid = 0
            Next
            If valid Then
                If UBound(verParts) = 2 Then
                    Return Val(verParts(0)) * 1000000 + Val(verParts(1)) * 1000 + Val(verParts(2))
                End If
            End If
        End If
    Next

    Return 0
End Function



' =============================================================================
' Function ScanDFP(basePath As String, chipName As String, ByRef bestScore As Long, ByRef bestPath As String) As Integer
' =============================================================================
'
' Purpose:
'   Scans a base directory recursively for version folders, checks for the chip's
'   .inc file, and updates the best match based on the highest version score.
'
' Parameters:
'   basePath (String): Root directory to scan (e.g., MPLAB X packs).
'   chipName (String): Lowercase chip name without .inc.
'   bestScore (Long, ByRef): Updated with the highest version score.
'   bestPath (String, ByRef): Updated with the path to the best .inc file.
'
' Returns:
'   Integer: -1 if a match is found, 0 otherwise.
'
' Notes:
'   - Uses shell 'dir' to list folders, saved to "folderlist.txt".
'   - Filters folders with IsVersionFolder.
'   - Scores versions with ParseVersion.
'   - Probes for chipName + ".inc" in valid folders.
'   - Updates best match if score exceeds current bestScore.
'   - Deletes temp file after use.
'
Function ScanDFP(basePath As String, chipName As String, ByRef bestScore As Long, ByRef bestPath As String) As Integer
    Dim tempFile As String = TEMP_FILE_NAME
    Dim foundMatch As Integer = 0
    
    ' Check if the base directory actually exists
    If Dir(basePath, 16) <> "" Then

        Shell(SHELL_DIR_PREFIX + basePath + SHELL_DIR_SUFFIX + tempFile)

        Dim folderName As String
        Dim fnum As Integer = FreeFile
        If Open(tempFile For Input As #fnum) = 0 Then
            While Not EOF(fnum)
                Line Input #fnum, folderName
                If IsVersionFolder(folderName) Then
                    Dim score As Long = ParseVersion(folderName)
                    Dim candidate As String = folderName + PATH_DELIMITER + chipName + INC_FILE_EXTENSION
                    If Dir(candidate) <> "" Then
                        foundMatch = -1
                        If score > bestScore Then
                            bestScore = score
                            bestPath = candidate
                        End If
                    End If
                End If
            Wend
            Close #fnum
            Kill tempFile
        Else
            DebugPrint("🚫 Failed to open folder list file.")
        End If
    Else
        If VBS = 1 Then Print Space(5) + "Directory does not exist: " + basePath + ". Skipping scan."
    End If

    Return foundMatch
End Function
' =============================================================================
' Function ScanPIC(basePath As String, chipName As String, ByRef bestScore As Long, ByRef bestPath As String) As Integer
' =============================================================================
'
' Purpose:
'   Scans a base directory recursively for version folders, checks for the chip's
'   .PIC file in the edc directory, and updates the best match based on the highest version score.
'
' Parameters:
'   basePath (String): Root directory to scan (e.g., MPLAB X packs).
'   chipName (String): Chip name without .PIC (e.g., "PIC16F17556").
'   bestScore (Long, ByRef): Updated with the highest version score.
'   bestPath (String, ByRef): Updated with the path to the best .PIC file.
'
' Returns:
'   Integer: -1 if a match is found, 0 otherwise.
'
' Notes:
'   - Similar to ScanDFP, but probes for .PIC in the edc subdirectory under the version folder.
'   - Uses IsVersionFolder to filter proc folders, then computes edc path.
'   - Updates best match if score exceeds current bestScore.
'
Function ScanPIC(basePath As String, chipName As String, ByRef bestScore As Long, ByRef bestPath As String) As Integer
    Dim tempFile As String = TEMP_FILE_NAME
    Dim foundMatch As Integer = 0

    ' Check if the base directory actually exists
    If Dir(basePath, 16) <> "" Then

        Shell(SHELL_DIR_PREFIX + basePath + SHELL_DIR_SUFFIX + tempFile)

        Dim folderName As String
        Dim fnum As Integer = FreeFile
        If Open(tempFile For Input As #fnum) = 0 Then
            While Not EOF(fnum)
                Line Input #fnum, folderName
                If IsVersionFolder(folderName) Then
                    Dim score As Long = ParseVersion(folderName)
                    Dim edc_candidate As String = Left(folderName, Len(folderName) - Len(XC8_INCLUDE_PATH)) + PIC_EDC_PATH_SUFFIX + chipName + PIC_FILE_EXTENSION
                    If Dir(edc_candidate) <> "" Then
                        foundMatch = -1
                        If score > bestScore Then
                            bestScore = score
                            bestPath = edc_candidate
                        End If
                    End If
                End If
            Wend
            Close #fnum
            Kill tempFile
        Else
            DebugPrint("🚫 Failed to open folder list file for PIC scan.")
        End If
    Else
        If VBS = 1 Then Print space(5) + "Directory does not exist: " + basePath + ". Skipping scan."
    End If

    Return foundMatch
End Function

' =============================================================================
' Function GetLatestDFPFile(chipNameRaw As String) As String
' =============================================================================
'
' Purpose:
'   Searches for the latest DFP header file across user packs, MPLAB X, and XC8
'   fallback directories. Normalizes chip name and returns the best match.
'
' Parameters:
'   chipNameRaw (String): Raw chip name (e.g., "PIC18F47Q43" or "pic18f47q43.inc").
'
' Returns:
'   String: Full path to the best .inc file, or empty if none found.
'
' Notes:
'   - Normalizes chip name to lowercase, removes .inc if present.
'   - Scans two primary paths with ScanDFP, then falls back to XC8 folders.
'   - XC8 fallback looks for folders starting with "v" containing a version.
'   - Selects the highest-scored match via ParseVersion.
'   - Logs progress via DebugPrint.
'
Function GetLatestDFPFile(chipNameRaw As String) As String
    Dim chipName As String = LCase(chipNameRaw)
    If Right(chipName, 4) = INC_FILE_EXTENSION Then
        chipName = Left(chipName, Len(chipName) - 4)
    End If

    Dim bestScore As Long = 0
    Dim bestPath As String = ""
    Dim dfpFound As Integer = 0

    ' Construct user packs path by combining USERPROFILE with the suffix
    Dim userPacksPath As String = Environ("USERPROFILE") + "\.mchp_packs"

    dfpFound = ScanDFP(userPacksPath, chipName, bestScore, bestPath)
    dfpFound = dfpFound Or ScanDFP(MPLABX_PATH, chipName, bestScore, bestPath)

    If dfpFound = 0 and Dir(XC8_BASE_PATH, 16) <> "" Then
        Dim fallbackBase As String = XC8_BASE_PATH
        Dim fallbackFolder As String = Dir(fallbackBase + DIR_WILDCARD, 16)
        While fallbackFolder <> ""
            If LCase(Left(fallbackFolder, 1)) = "v" And InStr(fallbackFolder, ".") > 0 Then
                Dim versionStr As String = Mid(fallbackFolder, 2)
                Dim score As Long = ParseVersion(versionStr)
                Dim candidate As String = fallbackBase + "\" + fallbackFolder + XC8_PROC_PATH + chipName + INC_FILE_EXTENSION
                DebugPrint("🔍 Fallback probing: " + candidate)
                If Dir(candidate) <> "" Then
                    DebugPrint("✅ Fallback found: " + candidate + " | Score: " + Str(score))
                    If score > bestScore Then
                        DebugPrint("   🏆 New best fallback match!")
                        bestScore = score
                        bestPath = candidate
                    End If
                End If
            End If
            fallbackFolder = Dir()
        Wend
    End If

    If bestPath <> "" Then
        IF VBS = 1 THEN Print space(5)+("DFP INC selected as " + bestPath)
        Return bestPath
    End If

    DebugPrint("🚫 GetLatestDFPFile: No match found.")
    Return ""
End Function

' =============================================================================
' Function GetLatestPICFile(chipNameRaw As String) As String
' =============================================================================
'
' Purpose:
'   Searches for the latest .PIC device description file across user packs, MPLAB X, and XC8
'   fallback directories. Returns the best match based on the highest version score.
'
' Parameters:
'   chipNameRaw (String): Raw chip name (e.g., "PIC16F17556").
'
' Returns:
'   String: Full path to the best .PIC file, or empty if none found.
'
' Notes:
'   - Scans two primary paths with ScanPIC, then falls back to XC8 folders.
'   - Probes for .PIC in the edc subdirectory under the version folder.
'   - Selects the highest-scored match via ParseVersion.
'   - Logs progress via DebugPrint.
'
Function GetLatestPICFile(chipNameRaw As String) As String
    ' Cache check: If we have a cached path for this exact chip, return it immediately
    If cachedPICPath <> "" And lastPICChip = chipNameRaw Then
        ' DebugPrint("Using cached PIC path for chip: " + cachedPICPath)
        Return cachedPICPath
    End If

    Dim bestScore As Long = 0
    Dim bestPath As String = ""
    Dim dfpFound As Integer = 0

    ' Construct user packs path by combining USERPROFILE with the suffix
    Dim userPacksPath As String = Environ("USERPROFILE") + "\.mchp_packs"
    
    dfpFound = ScanPIC(userPacksPath, chipNameRaw, bestScore, bestPath)
    
    dfpFound = dfpFound Or ScanPIC(MPLABX_PATH, chipNameRaw, bestScore, bestPath)
    
    If dfpFound = 0 Then
        Dim fallbackBase As String = XC8_BASE_PATH
        Dim fallbackFolder As String = Dir(fallbackBase + DIR_WILDCARD, 16)
        While fallbackFolder <> ""
            If LCase(Left(fallbackFolder, 1)) = "v" And InStr(fallbackFolder, ".") > 0 Then
                Dim versionStr As String = Mid(fallbackFolder, 2)
                Dim score As Long = ParseVersion(versionStr)
                Dim candidate As String = fallbackBase + "\" + fallbackFolder + PIC_EDC_PATH_SUFFIX + chipNameRaw + PIC_FILE_EXTENSION
                DebugPrint("🔍 Fallback probing PIC: " + candidate)
                If Dir(candidate) <> "" Then
                    DebugPrint("✅ Fallback PIC found: " + candidate + " | Score: " + Str(score))
                    If score > bestScore Then
                        DebugPrint("   🏆 New best fallback PIC match!")
                        bestScore = score
                        bestPath = candidate
                    End If
                End If
            End If
            fallbackFolder = Dir()
        Wend
    End If

    If bestPath <> "" Then
        IF VBS = 1 THEN Print space(5)+("DFP PIC selected as " + bestPath)
        ' Cache the result for this chip
        cachedPICPath = bestPath
        lastPICChip = chipNameRaw
        Return bestPath
    End If

    DebugPrint("🚫 No PIC match found.")
    ' Clear cache if no match (to force re-search next time)
    If lastPICChip = chipNameRaw Then
        cachedPICPath = ""
    End If
    Return ""
End Function

' =============================================================================
' Function GetChipSpecifics(chipNameRaw As String, param As String) As String
' =============================================================================
'
' Purpose:
'   Locates the latest .PIC file for the chip and extracts the specified attribute value
'   from the XML element. Converts hex values (0x...) to decimal.
'
' Parameters:
'   chipNameRaw (String): The chip name (e.g., "PIC16F17556").
'   param (String): The element and attribute to extract, format "element,attribute"
'                   e.g., "Breakpoints,hwbpcount" or "CodeSector,beginaddr".
'
' Returns:
'   String: The attribute value as decimal string, or empty if not found/file missing.
'
' Notes:
'   - Uses GetLatestPICFile to locate the .PIC file.
'   - Parses XML using string search; assumes first occurrence of element.
'   - Handles hex (0x...) conversion to decimal; other values returned as-is.
'
'   - Reads file in 64kb chunks and exits when finished, instead of reading the full file, other minor optimizations - Angel Mier
Function GetChipSpecifics(chipNameRaw As String, param As String) As String
    Dim picPath As String = GetLatestPICFile(chipNameRaw)
    If picPath = "" Then Return ""

    ' Pre-parse parameters to avoid repeated work
    Dim parts() As String
    SplitFolder(param, ",", parts())
    If UBound(parts) < 1 Then Return ""
    
    Dim element As String = Trim(parts(0))
    Dim attr As String = Trim(parts(1))
    Dim tagStart As String = "<edc:" + element + " "
    Dim attrSearch As String = attr + "="
    Dim quote As String = Chr(34)

    Dim fnum As Integer = FreeFile
    If Open(picPath For Binary Access Read As #fnum) <> 0 Then Return ""

    Dim fileSize As LongInt = Lof(fnum)
    Dim buffer As String = Space(65536)  ' Read in 64KB chunks instead of loading entire file
    Dim bytesRead As LongInt = 0
    Dim totalRead As LongInt = 0
    Dim content As String = ""
    Dim tagFound As Integer = 0

    ' Stream file and search until we find the tag
    Do While totalRead < fileSize And tagFound = 0
        Get #fnum, , buffer
        bytesRead = CInt(fileSize - totalRead)
        If bytesRead > 65536 Then bytesRead = 65536
        
        content = content + Left(buffer, bytesRead)
        totalRead = totalRead + bytesRead
        
        If InStr(content, tagStart) > 0 Then
            tagFound = -1
        End If
    Loop
    Close #fnum

    If tagFound = 0 Then Return ""

    Dim tagPos As Integer = InStr(content, tagStart)
    If tagPos = 0 Then Return ""

    Dim attrPos As Integer = InStr(tagPos, content, attrSearch)
    If attrPos = 0 Then Return ""

    Dim eqPos As Integer = attrPos + Len(attrSearch)
    ' Skip spaces after =
    Do While eqPos <= Len(content) And Mid(content, eqPos, 1) = " "
        eqPos = eqPos + 1
    Loop

    Dim quoteStart As Integer = InStr(eqPos, content, quote)
    If quoteStart = 0 Then Return ""

    Dim quoteEnd As Integer = InStr(quoteStart + 1, content, quote)
    If quoteEnd = 0 Then Return ""

    Dim value As String = Mid(content, quoteStart + 1, quoteEnd - quoteStart - 1)

    ' Handle hex conversion efficiently
    If Left(value, 2) = "0x" OrElse Left(value, 2) = "0X" Then
        value = Str(Val("&H" + Mid(value, 3)))
    End If

    Return value
End Function