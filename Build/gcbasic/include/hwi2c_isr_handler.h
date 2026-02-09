'';Copyrght 2017-2026 Evan R. Venn
'''
'''A Hardware I2C ISR Handler for GCGB and GCB.
'''------------------------------------------------------------------------------------------------------------------------------
''' Description: this ISR library provides an ISR to implement
''' a stateful i2c hardware slave for 16f and 18f Microprocessors
''' This is a GCB implementation of Microchip Application Note AN734.
'''
''' According to AN734, there are 5 possible i2c states. During ISR, each
''' of this states are detected. This ISR provides a standard skeleton to implement
''' an i2c hardware slaves, while client code must implement several callbacks
''' the ISR is expecting to call while processing states.
'''
''' Callbacks:
''' - HI2CSlave_State_1 ( in I2Ctemp as byte )
'''      called when i2c address matches (master starts a talk)
'''
''' - HI2CSlave_State_2 ( in I2CByte as byte):
'''      called when master is writing a byte. Slave is thus
'''      receiving this byte. This callback takes this bytes as
'''      argument
'''
''' - HI2CSlave_State_3
'''      called when master wants to read a byte from slave. Thus,
'''      slave should send a byte using HI2C2Send
'''
''' - HI2CSlave_State_4
'''      called when master still wants to read a byte from slave. That is,
'''      master required to read (state 3) and now still want to read a byte
'''      slave should send a byte using HI2C2Send
'''
''' - HI2CSlave_State_5
'''      called when master does not want to talk to slave anymore
'''      usually a good place to reset data or slave's logic
'''
''' - HI2CSlave_State_Error
'''      called when something wrong happens. You can do what you want in
'''      this case, like resetting the PIC, log some information using usart, ...
'''         => called any cases other than states 1, 2, 3, 4 or 5
'''
'''  Enable the line below to enable testing of the clock stretching feature
'''  it will add an additional delay of 200us in the interrupt handler
'''  so were sure that clock stretching is required for 100 KHz I2C operation
'''    #define HI2CSlave_TEST_CLOCK_STRETCHING 1
'''
'''
'''   Sebastien Lelong and Joep Suijs are credited with JAL implementation.
'''
'''
'''
'''@author  EvanV
'''@licence GPL
'''@version 0.9k
'''@date    22.1.2017
'''********************************************************************************
'''
'''  0.9k Revised to support SSP1IF
'''


; ---'''Variables

' Required I2C settings - CHANGE PORTS if required
'      #define HI2C_BAUD_RATE 100
'      #define HI2C_DATA PORTC.4
'      #define HI2C_CLOCK PORTC.3
'      I2C pins need to be input for SSP module
'      Dir HI2C_DATA in
'      Dir HI2C_CLOCK in
'      HI2CMode slave
'      HI2CSetAddress 0x4C
'      On Interrupt  SSP1Ready call HI2CSlave_ISR_Handler

'Optional - set the LED message ports
    'Error messaging LEDs
'    #define HI2CSlaveSSPOVOverflowErrorLED    porta.0
'    #define HI2CSlaveStateNotHandledErrorLED  porta.1

'Set the start stop handlers
'#define HI2CSlaveEnableStartStopInterrupts


#script


         if novar(SSPCON1) then

            if var(SSP1CON1) then
                SSPCON1 = SSP1CON1
                SSPSTAT = SSP1STAT
                SSPBUF  = SSP1BUF
                SSPCON2 = SSP1CON2
                SSPADD  = SSP1ADD
                SSPIF =   SSP1IF
            end if

         end if


         if novar(SSPCON1) then
            if var(SSPCON) then
                SSPCON1 = SSPCON
            end if
         end if


          If nobit(SSPIF) then

              If bit(SSP1IF) then
                  SSPIF=SSP1IF
              end if

              if nobit(SSP1IF) then
                  Warning "SSPIF not mapped to SSP1IF �.library is likely to fail"
              End if

          End if


#endscript

#define HI2CSlaveSSPOVOverflowError    1
#define HI2CSlaveStateNotHandledError  2

dim HI2CSlavetmpstat, i2ctemp, i2cbyte as byte

sub HI2CSlave_ISR_Init

    #ifdef HI2CSlaveStateNotHandledErrorLED
          Dir HI2CSlaveStateNotHandledErrorLED out
    #endif
    #ifdef HI2CSlaveSSPOVOverflowErrorLED
          Dir HI2CSlaveSSPOVOverflowErrorLED out
    #endif

    #ifdef HI2CSlaveEnableStartStopInterrupts

      SSPCON1 = SSPCON1 | 0b00001000     'enable start/stop interrupts

    #endif

end sub

'  HI2CSlave_ISR_Handler. Detects states a call appropriate procedures
sub HI2CSlave_ISR_Handler

    if sspif = 0 then exit sub
    'SSPIF is now assumed to 1

    ' Test code to simulate a large delay in the I2C interrupt handling
    ' to test if the clock stretching feature is working properly
     #ifdef HI2CSlave_TEST_CLOCK_STRETCHING then
        wait 200 us
     #endif

     ' Clear the Interrupt and read the current status

     SSPIF = 0
     HI2CSlavetmpstat = SSPSTAT


overflowstate:
   ' check for overflow and then handle
    if ( SSPOV = 1 ) then
       I2Ctemp = SSPBUF
       I2Ctemp = SSPBUF
       SSPOV = 0
       HI2CSlave_State_Error ( HI2CSlaveSSPOVOverflowError )
    end if



   ' if this constant is defined and true, call start & stop handlers.
   #ifdef HI2CSlaveEnableStartStopInterrupts
         I2Ctemp = ( HI2CSlavetmpstat & 0b00001111 )
         I2CTemp = I2CTemp | 0b00000001
         if I2Ctemp = 0b00001001 then
            HI2CSlave_State_On_Start
            if BF = 0 then Exit Sub
         end if

         I2Ctemp = ( HI2CSlavetmpstat & 0b00010000 )
         I2Ctemp = I2Ctemp  | 0b00100001
         if I2Ctemp = 0b00110001 then
            HI2CSlave_State_On_Stop
            if BF = 0 then Exit Sub
         end if

   #endif

   ' Mask out unimportant bit(s)
    HI2CSlavetmpstat = HI2CSlavetmpstat & 0b00101101

state1:
    '  state 1: write operation, last byte is address, buffer full
    '  byte is an address, it we get here, we just know master
    '  wants to talk to us, in a new write operation...
    '  and we also know address is recognized (BF is set, see spec)
    '  anyway, we must read buffer to reset BF bit
    '  (buffer actually contains previously detected address, that's
    '  why it's usually ignored)

    '  => call HI2CSlave_State_1( byte )


      if ( HI2CSlavetmpstat XOR 0b00001001 ) = 0  then
'          HSerSend 0x01
          I2Ctemp = SSPBUF
          HI2CSlave_State_1( I2Ctemp )

      else

state2:

      '  state 2: write operation, last byte is data, buffer full
      '  master wants to write, it sends a byte
      '      => call HI2CSlave_State_2(byte)
      '  (note: at a write-only sequence, fast masters give a stop (clearing bit3)
      '   before we can read the register. The or statment is handles this)

        i2ctemp = HI2CSlavetmpstat OR  0b00001001
        i2ctemp = i2ctemp XOR 0b00101001
        if i2ctemp = 0 then
'          HSerSend 0x02
          HI2CSlave_State_2( SSPBUF )

          'Has Stop been received, test the Start bit.... it will have toggled

            if S = 0 then

state3:
             ' let user process existing buffer
'             HSerSend 0x03
              HI2CSlave_State_3
              CKP = 1
            end if

        else


state4:
            '  state 4: read operation, last byte is address, buffer empty
            '  master wants to get a value from us
            '      => call HI2CSlave_State_4

            'Consume the Read Address Byte
            i2ctemp = HI2CSlavetmpstat XOR 0b00001101
            if i2ctemp = 0 then
              i2ctemp = SSPBUF
            end if

            i2ctemp = HI2CSlavetmpstat OR 0b00000001
            i2ctemp = i2ctemp XOR 0b00001101
            if i2ctemp = 0 then
'              HSerSend 0x04
              HI2CSlave_State_4
            else

state5:
              '  state 5: read operation, last byte is data, buffer empty
              '  master still wants to get a value from us
              '      => call HI2CSlave_State_5
              if (HI2CSlavetmpstat XOR 0b00101100) = false then
                HI2CSlave_State_5
'                HSerSend 0x05
              else
state6:
                  '  state 6: nack
                  '  master does not want to talk with us anymore
                  '      => call HI2CSlave_State_6
                  if (HI2CSlavetmpstat XOR 0b00101000) = false  then
                     '  AN734 does not talk about setting CKP, whereas spec says
                     '  it must be set. Some people say it can be error prone.
'                    HSerSend 0x06
                    CKP = 1
                    HI2CSlave_State_6

                  '  state not recognized... Error !
                  else
stateerror:
'                    HSerSend 0x07
                    '  something went wrong, that is, XOR operations did not match
                    '  SSPSTAT bits
                    CKP = 1
                    HI2CSlave_State_Error ( HI2CSlaveStateNotHandledError )
                  end if

              end if

            end if

        end if

      end if

end sub

sub HI2CSlave_WriteI2CByte ( in I2CByte )

    Wait Until BF = 0

    dim I2CTemp as Byte
    I2CTemp = 1
    do while I2CTemp = 1
        WCOL = 0
'        try to write into buffer, checking collision
        SSPBUF = I2CByte
        if WCOL = 0 then
            'ok, done
            I2CTemp = 0
        end if
'        else continue trying
    loop
    CKP = 1

End Sub


'Example routines - all are needed in the Message handler or in the user program

sub HI2CSlave_State_1 ( in I2CByte )

end Sub

sub HI2CSlave_State_2 ( in I2CByte )

end Sub

sub HI2CSlave_State_3

end Sub

sub HI2CSlave_State_4

end Sub

sub HI2CSlave_State_5

end Sub

sub HI2CSlave_State_6

end Sub

sub HI2CSlave_State_Error ( in I2CErrorCode )

    ' I2CErrorCode = What caused the error...
    ' SSPSTAT will tell you the status

    'Recommend to consume the Buffer
'    if BF = 1 then I2Ctemp = SSPBUF
'
'        Select Case HI2CErrorCode
'          #ifdef HI2CSlaveSSPOVOverflowErrorLED
'            Case HI2CSlaveSSPOVOverflowError
'                Dir HI2CSlaveSSPOVOverflowErrorLED out
'                HI2CSlaveSSPOVOverflowErrorLED = !HI2CSlaveSSPOVOverflowErrorLED
'          #endif
'          #ifdef HI2CSlaveStateNotHandledErrorLED
'            Case HI2CSlaveStateNotHandledError
'                Dir HI2CSlaveStateNotHandledErrorLED out
'                HI2CSlaveStateNotHandledErrorLED = !HI2CSlaveStateNotHandledErrorLED
'        End Select

end Sub

sub HI2CSlave_State_On_Start

end sub

sub HI2CSlave_State_On_Stop

'         ' let user process buffer
         HI2CMessageHandler_Process_Message

end sub
