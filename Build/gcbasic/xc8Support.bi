' Function and Sub Declarations
'Declare Sub DebugPrint(msg As String)
'Declare Sub SplitFolder(ByVal source As String, ByVal delimiter As String, result() As String)
'Declare Function ExtractVersionPath(path As String) As String
'Declare Function IsVersionFolder(fullPath As String) As Integer
'Declare Function ParseVersion(path As String) As Long
'Declare Function ScanDFP(basePath As String, chipName As String, ByRef bestScore As Long, ByRef bestPath As String) As Integer
'Declare Function GetLatestDFPFile(chipNameRaw As String) As String


' Usage
'  GetLatestDFPFile(chipName)

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
' Date: September 13, 2025 (current date)
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
Const XC8_BASE_PATH = "C:\Program Files\Microchip\xc8"
Const DIR_WILDCARD = "\*"
Const XC8_PROC_PATH = "\pic\include\proc\"

Dim Shared DEBUG_MODE As Integer
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
    Shell(SHELL_DIR_PREFIX + basePath + SHELL_DIR_SUFFIX + tempFile)

    Dim folderName As String
    Dim fnum As Integer = FreeFile
    If Open(tempFile For Input As #fnum) = 0 Then
        While Not EOF(fnum)
            Line Input #fnum, folderName
            If ExtendedVerboseMessages Then DebugPrint("📂 Found candidate folder: " + folderName) + "    " +  Str(IsVersionFolder(folderName) )
            If IsVersionFolder(folderName) Then
                Dim score As Long = ParseVersion(folderName)
                Dim candidate As String = folderName + PATH_DELIMITER + chipName + INC_FILE_EXTENSION
                If ExtendedVerboseMessages Then DebugPrint("🔍        Probing: " + candidate)
                If Dir(candidate) <> "" Then
                    foundMatch = -1
                    DebugPrint("✅        Found: " + candidate + " | Score: " + Str(score))
                    If score > bestScore Then
                        If ExtendedVerboseMessages Then DebugPrint("   🏆 New best match!")
                        bestScore = score
                        bestPath = candidate
                        
                    End If
                End If
            Else
                If ExtendedVerboseMessages Then DebugPrint("⏭ Skipping non-version folder: " + folderName)
            End If
        Wend
        Close #fnum
        Kill tempFile
    Else
        DebugPrint("🚫 Failed to open folder list file.")
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

    If dfpFound = 0 Then
        Dim fallbackBase As String = XC8_BASE_PATH
        Dim fallbackFolder As String = Dir(fallbackBase + DIR_WILDCARD, 16)
        While fallbackFolder <> ""
            If LCase(Left(fallbackFolder, 1)) = "v" And InStr(fallbackFolder, ".") > 0 Then
                Dim versionStr As String = Mid(fallbackFolder, 2)
                Dim score As Long = ParseVersion(versionStr)
                Dim candidate As String = fallbackBase + PATH_DELIMITER + fallbackFolder + XC8_PROC_PATH + chipName + INC_FILE_EXTENSION
                If ExtendedVerboseMessages Then DebugPrint("🔍 Fallback probing: " + candidate)
                If Dir(candidate) <> "" Then
                    DebugPrint("✅ Fallback found: " + candidate + " | Score: " + Str(score))
                    If score > bestScore Then
                        If ExtendedVerboseMessages Then DebugPrint("   🏆 New best fallback match!")
                        bestScore = score
                        bestPath = candidate
                    End If
                End If
            End If
            fallbackFolder = Dir()
        Wend
    End If

    If bestPath <> "" Then
        DebugPrint("🎯 Final selection: " + bestPath)
        Return bestPath
    End If

    DebugPrint("🚫 No match found.")
    Return ""
End Function

