'''Copyright 2016 - -2026 Evan R.Venn
'''
'''An I2C Message Handler for GCGB and GCB.
'''------------------------------------------------------------------------------------------------------------------------------
''' Description: this ISR library provides an ISR to implement
''' a stateful i2c hardware slave.
''' This is a GCB implementation of Microchip Application Note AN734.
'''
''' This library provides an message interface to i2c hardware slave.
''' It relies on the GCB implementation of Microchip Application Note AN734.
'''
''' The user needs to supply a procedure to process the received message.
''' Basically, this i2c slave waits for a full message to arrive. Then it calls
'''
''' the user procedure to process the message and (optional) prepare a response.
'''
''' Subsequently, this lib will pass the response data over to the master, if
''' it wants to have them.
'''
''' Callbacks:
''' - HI2C_Process_In_Message( HI2CIndex as byte )
'''      called when i2c amaster starts a talk.  HI2CIndex is the length of the incoming packet
'''
''' - HI2C_Process_Outgoing_Message
'''      called when i2c amaster needs to a talk.
'''      HI2CBUFFER should be populated with the data to be sent.
'''
'''   All other methods are Callbacks from the ISR routine.
'''
'''   Sebastien Lelong and Joep Suijs are credited with JAL implementation.
'''
'''@author  EvanV
'''@licence GPL
'''@version 0.9j
'''@date    30.12.2016
'''********************************************************************************
#include <HWI2C_ISR_Handler.h>

Dim HI2CBUFFER ( HI2CBUFFERSIZE )
Dim HI2CDATAPRESENT, HI2CDATAREQUESTED  as byte


'REDIRECT ALL THE METHODS TO THIS HANDLER
#IFNDEF HI2CSLAVE_STATE_1
	#DEFINE HI2CSLAVE_STATE_1         HI2CMESSAGEHANDLER_STATE_1
#ENDIF
#IFNDEF HI2CSLAVE_STATE_2
   #DEFINE HI2CSLAVE_STATE_2         HI2CMESSAGEHANDLER_STATE_2
#ENDIF
#IFNDEF HI2CSLAVE_STATE_3
   #DEFINE HI2CSLAVE_STATE_3         HI2CMESSAGEHANDLER_STATE_3
#ENDIF
#IFNDEF HI2CSLAVE_STATE_4
   #DEFINE HI2CSLAVE_STATE_4         HI2CMESSAGEHANDLER_STATE_4
#ENDIF
#IFNDEF HI2CSLAVE_STATE_5
   #DEFINE HI2CSLAVE_STATE_5         HI2CMESSAGEHANDLER_STATE_5
#ENDIF
#IFNDEF HI2CSLAVE_STATE_ERROR
   #DEFINE HI2CSLAVE_STATE_ERROR     HI2CMESSAGEHANDLER_STATE_ERROR
#ENDIF
#IFNDEF HI2CSLAVE_STATE_ON_START
   #DEFINE HI2CSLAVE_STATE_ON_START  HI2CMESSAGEHANDLER_STATE_ON_START
#ENDIF
#IFNDEF HI2CSLAVE_STATE_ON_STOP
   #DEFINE HI2CSLAVE_STATE_ON_STOP   HI2CMESSAGEHANDLER_STATE_ON_STOP
#ENDIF

#startup HI2CMessageHandler_Process_Init

dim HI2CIndex as byte

sub HI2CMessageHandler_Process_Init
    HI2CIndex = 0
    HI2CDATAPRESENT = false
    HI2CDATAREQUESTED = false

    for HI2CForLoop = 0 to HI2CBUFFERSIZE
        HI2CBUFFER(HI2CForLoop) = 0
    next

end sub

sub HI2CMessageHandler_Process_Message

   ' let user process buffer
   if (HI2CDATAPRESENT = true) then

      I2CTemp = HI2CIndex
      HI2C_Process_In_Message( I2CTemp )
      HI2CDATAPRESENT = false

   end if

   if HI2CDATAREQUESTED = true then

      HI2C_Process_Out_Message
      HI2CDATAREQUESTED = false

   end if

   HI2CIndex = 0

end sub

' this callback is used when something wrong happened
' during communication between master and us
sub HI2CMessageHandler_State_Error ( in HI2CErrorCode )

    ' HI2CErrorCode = What caused the error...
    ' SSPSTAT will tell you the status
    ' Recommend to consume the Buffer

end Sub


' this callback is used when master wants to talk to us
' and our HI2C address has been recognized
sub HI2CMessageHandler_State_1 ( in I2CByte )
   ' optionally let user process buffer
    HI2CMessageHandler_Process_Message

end sub


' This callback is used when master sends a data byte
sub HI2CMessageHandler_State_2 ( in I2CByte )

   HI2CDATAPRESENT   = true      ' Indicate we received data

   ' store data
   HI2CBUFFER(HI2CIndex) = I2CByte

   if (HI2CIndex < HI2CBUFFERSIZE) then
      HI2CIndex = HI2CIndex + 1   ' point to next position
   end if

end sub


' this callback is used when master has sent us a stop after sending a byte
sub HI2CMessageHandler_State_3

   HI2CDATAREQUESTED = true
   ' let user process buffer
   HI2CMessageHandler_Process_Message

end sub


' this callback is used when master wants to read something
' from us. It should use HI2CSlave_WriteI2CByte to send something
sub HI2CMessageHandler_State_4

   HI2CDATAREQUESTED = true
   ' let user process buffer
   HI2CMessageHandler_Process_Message

   ' send first byte to master
   HI2CSlave_WriteI2CByte(HI2CBUFFER(0))   ' send first databyte
   HI2CIndex = 1                           ' point to next position

end sub


' this callback is used when master, after having read something,
' still wants to read and get data from us.
sub HI2CMessageHandler_State_5

   HI2CDATAREQUESTED = true
   HI2CSlave_WriteI2CByte(HI2CBUFFER(HI2CIndex))    ' send data

   if (HI2CIndex < HI2CBUFFERSIZE) then
      HI2CIndex = HI2CIndex + 1   ' point to next position
   end if


end sub
' this callback is used when master does not want to talk
' with us anymore... This is an appropriate place to reset
' data for instance
sub HI2CMessageHandler_State_6

end Sub

sub HI2CMessageHandler_State_On_Start

end sub

sub HI2CMessageHandler_State_On_Stop

'        let user process buffer
         HI2CMessageHandler_Process_Message

end sub
