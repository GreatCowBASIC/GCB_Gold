'    18F Multi tasking routines for the GCBASIC compiler
'    Copyright (C) 2007-2020 Hugh Considine

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

'WARNING: These routines are incomplete and have not been even partially tested.
'         Don't even try to use them as is!

#startup InitMultitask

'Initialisation routine
sub InitMultitask

 'TaskData format: (n = 0 * 9 to 9 * 9)
 'n + 1 : W
 'n + 2 : Status
 'n + 3, n + 4: FSR0
 'n + 5, n + 6: FSR1
 'n + 7, n + 8: FSR2
 'n + 9 : Stack size, 255 = location unused
 Dim TaskData (90)
 Dim StackData (240)

 'Mark all tasks as unused (except first)
 TaskData (9) = 0
 For MultiTemp = 2 to 10
  TaskData (MultiTemp * 9) = 255
 Next

 TaskCount = 1
 CurrentTask = 0

end sub

'Start a task
'Input: Address to start execution at
'Return: 0 if failure, otherwise an index between 1 and 10
function StartTask (NewLoc As Word)
 'Find location to store data
 For Start = 1 to 10
  If TaskData (Start * 9) = 255 Then Goto NewTaskLocFound
 Next

 'If no room, set Start to 0 and exit
 Start = 0
 Return

NewTaskLocFound:
 TaskData (Start * 9) = 1
 StackData (Start * 24 - 24) = NewLoc
 StackData (Start * 24 - 23) = NewLoc_H

end function

'Stop a task
'Input: task index between 1 and 10
sub StopTask (TaskID) #nr
 TaskData (TaskID * 9) = 255
end sub

'Switch to another task
'Input: task index between 1 and 10
sub SwitchToTask (TaskID) #nr
 Dim WTemp As Byte
 Dim StatusTemp As Byte
 Dim FSR0Temp As Word

 'Remove this sub from stack
 pop

 'Save current task
 movwf WTemp
 swapf STATUS, W
 movwf StatusTemp
 movff FSR0L, FSR0Temp
 movff FSR0H, FSR0Temp_H

 StackSize = STKPTR And 31
 'Control data
 TaskData (CurrentTask * 9 + 1) = WTemp
 PREINC0 = StatusTemp
 PREINC0 = FSR0Temp
 PREINC0 = FSR0Temp_H
 PREINC0 = FSR1L
 PREINC0 = FSR1H
 PREINC0 = FSR2L
 PREINC0 = FSR2H
 PREINC0 = StackSize
 'Stack
 StackData (CurrentTask * 24 + 1) = TOSL
 PREINC0 = TOSH
 If StackSize > 1 Then
  For MultiTemp = 2 to StackSize
   pop
   PREINC0 = TOSL
   PREINC0 = TOSH
  Next
 End If

 'Restore new task
 decf TaskID,F
 'Control data
 StackSize = TaskData (TaskID * 9 + 9)
 FSR2H = PREDEC0
 FSR2L = PREDEC0
 FSR1H = PREDEC0
 FSR1L = PREDEC0
 FSR0Temp_H = PREDEC0
 FSR0Temp = PREDEC0
 StatusTemp = PREDEC0
 WTemp = PREDEC0

 'Stack
 STKPTR = 1
 TOSH = StackData (TaskID * 24 + StackSize)
 TOSL = POSTDEC0
 If StackSize > 1 Then
  For MultiTemp = 2 to StackSize
   push
   TOSH = POSTDEC0
   TOSL = POSTDEC0
  Next
 End If

 movff FSR0Temp, FSR0L
 movff FSR0Temp_H, FSR0H

 swapf StatusTemp,W
 movwf STATUS
 swapf WTemp,F
 swapf WTemp,W

end sub

'Switch to next task
'Called by interrupt
sub NextTask
 Dim WTemp As Byte
 Dim StatusTemp As Byte
 Dim FSR0Temp As Word

 'Remove this sub from stack
 pop

 'Save current task
 movwf WTemp
 swapf STATUS, W
 movwf StatusTemp

 If EndTask = TRUE Then Goto StartNextTask

 movff FSR0L, FSR0Temp
 movff FSR0H, FSR0Temp_H

 StackSize = STKPTR And 31
 'Control data
 TaskData (CurrentTask * 9 + 1) = WTemp
 PREINC0 = StatusTemp
 PREINC0 = FSR0Temp
 PREINC0 = FSR0Temp_H
 PREINC0 = FSR1L
 PREINC0 = FSR1H
 PREINC0 = FSR2L
 PREINC0 = FSR2H
 PREINC0 = StackSize
 'Stack
 StackData (CurrentTask * 24 + 1) = TOSL
 PREINC0 = TOSH
 If StackSize > 1 Then
  For MultiTemp = 2 to StackSize
   pop
   PREINC0 = TOSL
   PREINC0 = TOSH
  Next
 End If

 'Find new task to start
 TaskID = CurrentTask
StartNextTask:
 incf TaskID,F
 If TaskID > 10 Then TaskID = 1
 If TaskData (TaskID * 9) <> 255 Then Goto NextTaskFound
 goto StartNextTask
NextTaskFound:
 decf TaskID,F
 movff TaskID,CurrentTask

 'Restore new task
 'Control data
 StackSize = TaskData (TaskID * 9 + 9)
 FSR2H = PREDEC0
 FSR2L = PREDEC0
 FSR1H = PREDEC0
 FSR1L = PREDEC0
 FSR0Temp_H = PREDEC0
 FSR0Temp = PREDEC0
 StatusTemp = PREDEC0
 WTemp = PREDEC0

 'Stack
 STKPTR = 1
 TOSH = StackData (TaskID * 24 + StackSize)
 TOSL = POSTDEC0
 If StackSize > 1 Then
  For MultiTemp = 2 to StackSize
   push
   TOSH = POSTDEC0
   TOSL = POSTDEC0
  Next
 End If

 movff FSR0Temp, FSR0L
 movff FSR0Temp_H, FSR0H

 swapf StatusTemp,W
 movwf STATUS
 swapf WTemp,F
 swapf WTemp,W

end sub

'End the current task
'Called when stack underflow occurs in current task
sub EndCurrentTask
 if TaskCount = 0 Then Exit Sub

 EndTask = TRUE
 pop
 NextTask
end sub
