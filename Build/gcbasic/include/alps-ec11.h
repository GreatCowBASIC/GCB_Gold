''' Copyright 2015-2025 Immo
'''----------------------------------------------
''' This program is a includefile for ALPS rotary Encoder EC11
''' ALPS-EC11.h
'''
''' @author   Immo, DL5KB &EvanV
''' @licence  GPL
''' @version  3.0
''' @date     Dec. 2015
'''**********************************************
''' enjoy it!
'
' encoder routines for ALPS mechanical rotary encoder
' EC11B/ EC11E/ EC11G/ EC11J with push-Button
' all these have two different stable detents: A and B are High or Low
' followig graph will show:
'
'   --:,   :,---:,    H
' A   :|   :|   :|
'     :'---:'   :'--- L
'     :    :    :
'   ,-:--, :  ,-:--,  H
' B | :  | :  | :  |
'   ' :  '-:--' :  '- L
'     :    :    :
'     det1 det2 det1
'
' Because of two possible different states at any detent,
' you have to separate the detent positions from transient,
' in order to become 1 count between 2 detents.
' First transient (i.e. A from H to L) is counted,
' second (i.e. B from L to H) is not regarded. To get the direction
' of turning makes XOR between old and new position of A and B.
'
' You have to define the encoder-bits to ports
 ; #define Alps_ENCODER_A PortB.0   'Clock
 ; #define Alps_ENCODER_B PortB.2   'cw
 ; #define Alps_ENCODER_SW PortB.3  'switch
 ; #define Alps_ENCODER_ENC_READ_DELAY 100 'delay in us
' port direction is made at ALPS_encoder_init
 ; dir Alps_ENCODER_A in
 ; dir Alps_ENCODER_B in
 ; dir Alps_ENCODER_SW in

'Variables exposed by include file are:
'Alps_ESB_Bit is a bit that is exposed to detect state of button. Detects event has occurred
'Alps_EncoderEvent is a bit that is exposed to detect state of button. Detects event has occurred
'Alps_CurrentPosition is a public Integer value that is the current rotational value
'Alps_Directn is a public bit that is the current direction of the encoder

' Before entering the main program, the starting state
' of the encoder bits (a detent position) is stored
' to the variables new and old, bit 6 and 7.


' User Changeable constants

   'DEFAULT DELAY TO PREVENT SWITCH BOUNCE
  #IFNDEF ALPS_ENCODER_ENC_READ_DELAY
   #DEFINE ALPS_ENCODER_ENC_READ_DELAY 100
  #ENDIF

'End of user changeable constants

 ; ---- variables

 'gets Alps_New_Byte_Position_Value position
 DIM Alps_New_Byte_Position_Value as Byte

 'saves old position
 DIM Alps_Old_Byte_Position_Value as Byte

 'sense of rotation: 1 = CW, 0 = CCW
 DIM Alps_DirectN as bit

 'public variable to show encoder event has occurred
 DIM Alps_EncoderEvent as bit

 'encoder switch pushed, is toggeled
 DIM Alps_ESB_Bit, Alps_ESB_Bit_Current_State as bit



 'variable to add to counter
 DIM ALPS_Count_delta as integer

 'global position variable
 DIM Alps_currentPosition as integer

 'init encoder first
 #startup Alps_Encoder_init

 ; ---- Methods
 sub Alps_Encoder_Init

  'set port direction
  dir Alps_ENCODER_A in
  dir Alps_ENCODER_B in
  dir Alps_ENCODER_SW in

  'position buffer set to zero
  Alps_Old_Byte_Position_Value = 0
  Alps_New_Byte_Position_Value = 0

  'change the connections A and B, if turning direction is opposite

  'copy encoder bits to new
  Alps_New_Byte_Position_Value.6 = Alps_encoder_B
  Alps_New_Byte_Position_Value.7 = Alps_encoder_A

  'store actual detent
  'mask bits and copy Alps_New_Byte_Position_Value into old.
  Alps_Old_Byte_Position_Value = Alps_New_Byte_Position_Value & b'11000000'

  'switch not pressed.nSet intial state of button selection
  Alps_ESB_Bit = 1

  'No clock has happened yet
  Alps_EncoderEvent = 0

  'Initialise variable
  Alps_CurrentPosition = 0

 end sub

 sub Alps_ENCODER_SW_Pressed
    'Prevent re-entry to this event routine
    SetExternalEventEnable_Switch = 0
    'Clear Interrupt. See the datasheet for more information.
    ClearExternalEvent_Switch = 0

    'toggle esb
    'set the Bit Alps_ESB_Bit to the Compliment of Alps_ESB_Bit
    SetWith( Alps_ESB_Bit, FnNotBIT( Alps_ESB_Bit ) )

    wait while Alps_ENCODER_SW = 0

    'Re-Enable Interrupt
    SetExternalEventEnable_Switch = 1

 end sub

 sub Alps_ENCODER_Clock_Event   'Prevent re-entry to this event routine

  if ClearExternalEvent_Clock = 1 and Alps_EncoderEvent = 0 then
      SetExternalEventEnable_Clock = 0

      'Clear Interrupt. See the datasheet for more information.
      ClearExternalEvent_Clock = 0

      if Alps_ENCODER_A = 1 then

          'Purely debounce
          wait Alps_ENCODER_ENC_READ_DELAY us

          'Set clock event pending
          Alps_EncoderEvent = 1

          if Alps_ENCODER_B = 1 then
              Alps_Directn = 1
              Alps_CurrentPosition++
          Else
              Alps_Directn = 0
              Alps_CurrentPosition--
          end if

      end if


      'Re-Enable Interrupt
      SetExternalEventEnable_Clock = 1

  end if


 end sub

sub Alps_Encoder_Read

 'Returns 1 or -1 depending on rotation between two detents
 'based on value of 0 for Alps_Count_delta. The return value can
 'be set with Alps_Count_delta to any value at your affords.

 ''' Gets first stable detent with A = B High or Low

 do Forever

  ; gets a detent at A = B
    Alps_New_Byte_Position_Value.7 = Alps_encoder_A
    Alps_New_Byte_Position_Value.6 = Alps_encoder_B

 loop while Alps_New_Byte_Position_Value.6 = Alps_New_Byte_Position_Value.7

  ; save detent to old and new
    Alps_Old_Byte_Position_Value = Alps_New_Byte_Position_Value & b'11000000'

  ;''' ---------------------------------------
  ;  MENTION:
  ;  stays in following loop, while no change.
  ;  Encoder_switch reading added for exiting.
  ;''' ---------------------------------------
  ;'debouncing time for quick turning
    wait 5 ms

 do Forever
   ;read encoder bits to new
    Alps_New_Byte_Position_Value.7 = Alps_encoder_A
    Alps_New_Byte_Position_Value.6 = Alps_encoder_B

   ;reads encoder-sw, leave enc-Bits unchanged
    Alps_Switch_Read
    if Alps_ESB_Bit Then exit do

 loop while Alps_New_Byte_Position_Value = Alps_Old_Byte_Position_Value

    if Alps_ESB_Bit Then return

 ''' -----------------------------------
 'to get direction of rotation
 'XOR right bit of new vs. left bit of old.
  Alps_DirectN = FnEquBit(Alps_New_Byte_Position_Value.6 XOR Alps_Old_Byte_Position_Value.7)

   Alps_Count_delta = 0

   if Alps_DirectN = 1 then
    ' encoder turned CW
     Alps_Count_delta++
   else
     ' encoder turned CCW
     Alps_Count_delta--
   End if

 'copy new into old.
  Alps_Old_Byte_Position_Value = Alps_New_Byte_Position_Value & b'11000000'
 end sub

 sub Alps_Switch_Read

 if Alps_Encoder_Sw = 0 Then
  'wait for pushbutton release
   wait until Alps_Encoder_Sw

   wait 50 ms

  'set the Bit Alps_ESB_Bit to the Compliment of Alps_ESB_Bit
   SetWith( Alps_ESB_Bit, FnNotBIT( Alps_ESB_Bit ) )

 end if

 end sub
