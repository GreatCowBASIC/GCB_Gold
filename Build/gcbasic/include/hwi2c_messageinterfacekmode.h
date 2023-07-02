''' Copyright 2017-2020 Evan R. Venn
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
'''@version 1.0
'''@date    30.12.2017
'''********************************************************************************
#include <HWI2C_ISR_HandlerKMode.h>

Dim HI2CBUFFER ( HI2CBUFFERSIZE )
Dim HI2CDATAPRESENT, HI2CDATAREQUESTED  as byte
dim HI2CIndex as byte


'Redirect all the methods to this Handler
#define SI2CSlave_State_1         SI2CMessageHandler_State_1
#define SI2CSlave_State_2         SI2CMessageHandler_State_2
#define SI2CSlave_State_3         SI2CMessageHandler_State_3
#define SI2CSlave_State_4         SI2CMessageHandler_State_4
#define SI2CSlave_State_5         SI2CMessageHandler_State_5
#define SI2CSlave_State_Error     SI2CMessageHandler_State_Error
#define SI2CSlave_State_On_Start  SI2CMessageHandler_State_On_Start
#define SI2CSlave_State_On_Stop   SI2CMessageHandler_State_On_Stop

#startup SI2CMessageHandler_Process_Init


sub SI2CMessageHandler_Process_Init
    HI2CIndex = 0
    HI2CDATAPRESENT = false
    HI2CDATAREQUESTED = false

    for HI2CForLoop = 0 to HI2CBUFFERSIZE
        HI2CBUFFER(HI2CForLoop) = 0x00
    next

end sub

sub SI2CMessageHandler_Process_Message

   ' let user process buffer
   if (HI2CDATAPRESENT = true) then

      I2CTemp = HI2CIndex                'dont pass the index as the user may destroy the value!!
      HI2C_Process_In_Message( I2CTemp ) 'user callback with the size of the current packet passed as a parameter
      HI2CDATAPRESENT = false

   end if

   if HI2CDATAREQUESTED = true then

      HI2C_Process_Out_Message      'user callback - Master is requesting data
      HI2CDATAREQUESTED = false

   end if

   HI2CIndex = 0

end sub

' this callback is used when something wrong happened
' during communication between master and us
sub SI2CMessageHandler_State_Error ( in HI2CErrorCode )

    ' HI2CErrorCode = What caused the error...
    ' SSPSTAT will tell you the status
    ' Recommend to consume the Buffer

end Sub


' this callback is used when master wants to talk to us - this is called for Write operations when the Start event occurs
' and our HI2C address has been recognized
sub SI2CMessageHandler_State_1 ( in I2CByte )

   ' Let user process buffer
    SI2CMessageHandler_Process_Message

end sub


' This callback is used when master sends a data byte
sub SI2CMessageHandler_State_2 ( in I2CByte )

   HI2CDATAPRESENT   = true      ' Indicate we received data

   ' store data
   HI2CBUFFER(HI2CIndex) = I2CByte

   if (HI2CIndex < HI2CBUFFERSIZE-1) then
      HI2CIndex = HI2CIndex + 1   ' point to next position
   end if

end sub


' this callback is used when master has sent us a stop after sending a byte, the Stop event
sub SI2CMessageHandler_State_3

   HI2CDATAREQUESTED = true
   ' let user process buffer
   HI2CIndex--
   SI2CMessageHandler_Process_Message

end sub


' this callback is used when master wants to read something
' from us - the is the Start event for Read operations.  We handle the first byte differently to maintain the state engine,
sub SI2CMessageHandler_State_4

   HI2CDATAREQUESTED = true
   ' let user process buffer
   SI2CMessageHandler_Process_Message

   ' send first byte to master
    HI2CWaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2CWaitMSSPTimeout < 255

        HI2CWaitMSSPTimeout++
        'Wait for this event
        if I2C1STAT1.TXBE = 1 then

            I2C1TXB = (HI2CBUFFER(0))
            I2C1STAT1.2 = 1 ' equates I2C2STAT1_CLRBF
            exit do

        else

            wait 1 us

        end if
    loop


   HI2CIndex = 1 ' set pointer to next position in the buffer

end sub


' this callback is used when master, after having read something,
' still wants to read and get data from us.
sub SI2CMessageHandler_State_5


   HI2CDATAREQUESTED = true

    HI2CWaitMSSPTimeout = 0
    'waits up to 254us then creates error message
    do while HI2CWaitMSSPTimeout < 255

        HI2CWaitMSSPTimeout++
        'Wait for this event
        if I2C1STAT1.TXBE = 1 then

             I2C1TXB = HI2CBUFFER(HI2CIndex)    ' send data
             I2C1STAT1.2 = 1 ' equates I2C2STAT1_CLRBF
             exit do

        else

            wait 1 us

        end if
    loop

   'Protect the buffer, increment to end of buffer, or, return last value in the buffer.
   if (HI2CIndex < HI2CBUFFERSIZE-1) then
      HI2CIndex = HI2CIndex + 1   ' point to next position
   end if


end sub
' this callback is used when master does not want to talk
' with us anymore... This is an appropriate place to reset
' data for instance.  Call when the Stop event happens.
sub SI2CMessageHandler_State_6

end Sub

sub SI2CMessageHandler_State_On_Start

end sub

sub SI2CMessageHandler_State_On_Stop

end sub
