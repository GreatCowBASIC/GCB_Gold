'-- IDE.bas ---------------------------------------------------- >>fst'18<< -
' - starter for SynWrite - Editor
' - since V 20151122 ist does _not_ set environment variable
'   containing program path any more
' - restores configuration after external modification (i.e. update)
' - compiled with FreeBasic 0.24 (parameter -s gui)
'-- V 20180429 ------------------------------- http://www.FrankSteinberg.de -
' - added underscore to \GreatCowBasic\Release_ List.txt @ 02122018


#include once "file.bi"
#include once "windows.bi"

Declare Sub FileBackup(ByVal As String)
Declare Sub RestoreCfg()

Dim FirstStart As String

'check, if SynWrite.exe can be found:
If Not FileExists(ExePath + "\SynWrite\syn.exe") Then
   MessageBox NULL, ExePath + "\SynWrite\syn.exe  not found!", "IDE-Starter for SynWrite and GreatCowBASIC", 16
  End
End If

'on first start = open readmes and sample-source:
If Not MkDir(ExePath + "\CfgBackup") Then   'MkDir returns -1 if dir "CfgBackup" exists and 0 if created
  If FileExists(ExePath + "\readme.txt") Then
      FirstStart = FirstStart + Chr(34) + ExePath + "\readme.txt" + Chr(34) + " "
   End If
   If FileExists(ExePath + "\history.txt") Then
      FirstStart = FirstStart + Chr(34) + ExePath + "\history.txt" + Chr(34) + " "
   End If
   If FileExists(ExePath + "\GreatCowBasic\release_list.txt") Then
      FirstStart = FirstStart + Chr(34) + ExePath + "\GreatCowBasic\release_list.txt" + Chr(34) + " "
   End If
   If FileExists(ExePath + "\GreatCowBasic\Demos\first-start-sample.gcb") Then
      FirstStart = FirstStart + Chr(34) + ExePath + "\GreatCowBasic\Demos\first-start-sample.gcb" + Chr(34) + " "
   End If
End If

'set environment variable to actual GCB+Syn - path:
'SetEnviron("G+SDir=" + ExePath)

'go to our root directory:
ChDir(ExePath)

'check, if files to restore an do if user wants to:
RestoreCfg()

'run SynWrite:
Exec (ExePath + "\SynWrite\syn.exe", FirstStart + Chr(34) + Command(-1) + Chr(34))

'backup configuration:
MkDir(ExePath + "\CfgBackup")
MkDir(ExePath + "\CfgBackup\G+Stools")
MkDir(ExePath + "\CfgBackup\SynWrite")
MkDir(ExePath + "\CfgBackup\SynWrite\Settings")
MkDir(ExePath + "\CfgBackup\GreatCowBasic")

'Copy config-files
FileBackup("SynWrite\Settings\syn.ini")
FileBackup("SynWrite\Settings\SynHide.ini")
FileBackup("SynWrite\Settings\SynHistory.ini")
FileBackup("SynWrite\Settings\SynHistoryStates.ini")
FileBackup("SynWrite\Settings\SynSpell.ini")
FileBackup("SynWrite\Settings\SynState.ini")
FileBackup("SynWrite\Settings\SynStyles.ini")
FileBackup("SynWrite\Settings\SynToolbars.ini")
FileBackup("G+Stools\flashAVR.bat")
FileBackup("G+Stools\flashPIC.bat")
FileBackup("G+Stools\makeASM.bat")
FileBackup("G+Stools\makeHEX.bat")
FileBackup("G+Stools\useINI.bat")
FileBackup("G+Stools\foINI.bat")
FileBackup("GreatCowBasic\use.ini")

End


Sub FileBackup (ByVal sFilePath As String)

  'do nothing when file is unmodified:
  If FileDateTime(ExePath + "\" + sFilePath) = FileDateTime(ExePath + "\CfgBackup\" + sFilePath) Then Exit Sub

  'backup modified file:
 FileCopy(ExePath + "\" + sFilePath, ExePath + "\CfgBackup\" + sFilePath)

End Sub


Sub RestoreCfg()

 'do nothing when no backup-files:
 If Not FileExists(ExePath + "\CfgBackup\SynWrite\Settings\syn.ini") Then Exit Sub

 'do nothing, if syn.ini not changed:
 If FileDateTime(ExePath + "\SynWrite\Settings\syn.ini") _
    = FileDateTime(ExePath + "\CfgBackup\SynWrite\Settings\syn.ini") Then Exit Sub

 'ask user for restore:
 Dim iAnswer As Integer
 iAnswer = MessageBox(NULL, "Configuration changed outside!" + Chr(10) + Chr(10) + _
                            "Restore your last used configuration?", _
                            "GCB+Syn IDE for Great Cow Basic", MB_ICONQUESTION Or MB_YESNO)
 If iAnswer = IDNO  Then Exit Sub  'do nothing, if restore not wanted

  'Restore Files from backup-folder:
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\Syn.ini", ExePath + "\SynWrite\Settings\Syn.ini")
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\SynHide.ini", ExePath + "\SynWrite\Settings\SynHide.ini")
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\SynHistory.ini", ExePath + "\SynWrite\Settings\SynHistory.ini")
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\SynHistoryStates.ini", ExePath + "\SynWrite\Settings\SynHistoryStates.ini")
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\SynSpell.ini", ExePath + "\SynWrite\Settings\SynSpell.ini")
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\SynState.ini", ExePath + "\SynWrite\Settings\SynState.ini")
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\SynStyles.ini", ExePath + "\SynWrite\Settings\SynStyles.ini")
 FileCopy(ExePath + "\CfgBackup\SynWrite\Settings\SynToolbars.ini", ExePath + "\SynWrite\Settings\SynToolbars.ini")
 FileCopy(ExePath + "\CfgBackup\G+Stools\flashAVR.bat", ExePath + "\G+Stools\flashAVR.bat")
 FileCopy(ExePath + "\CfgBackup\G+Stools\flashPIC.bat", ExePath + "\G+Stools\flashPIC.bat")
 FileCopy(ExePath + "\CfgBackup\G+Stools\makeASM.bat", ExePath + "\G+Stools\makeASM.bat")
 FileCopy(ExePath + "\CfgBackup\G+Stools\makeHEX.bat", ExePath + "\G+Stools\makeHEX.bat")
 FileCopy(ExePath + "\CfgBackup\GreatCowBasic\use.ini", ExePath + "\GreatCowBasic\use.ini")

End Sub
