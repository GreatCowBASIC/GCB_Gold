'''Copyright 2017-2025 Evan R. Venn
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
'''      called when master has completed writing the bytes.
'''      Slave can now process the queue.
'''
''' - HI2CSlave_State_4
'''      called when master wants to read a byte from slave. Thus,
'''      slave should send a byte using method I2C1CNT set to greater than 0 and I2C1TXB = value.
'''
''' - HI2CSlave_State_5
'''      called when master still wants to read a byte from slave. That is,
'''      master required to read (state 4) and now still want to read a byte
'''      slave should send a byte using method I2C1CNT set to greater than 0 and I2C1TXB = value.
'''
''' - HI2CSlave_State_6
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
'''@version 1.0
'''@date    20.12.2017
'''********************************************************************************
'''
'''


'    #include <HWI2C_MessageInterfaceKMode.h>  'Defines a set of callbacks - you do NOT need to define HI2CSlave_State_1-5

'    'Buffer size
'    #define HI2CBUFFERSIZE 16
'
'    'Required I2C settings
'    #define HI2C_BAUD_RATE 400  'not important as we are a slave
'    #define HI2C_DATA PORTC.4
'    #define HI2C_CLOCK PORTC.3
'    'Initialise I2C
'    Dir HI2C_DATA out
'    Dir HI2C_CLOCK out
'    RC3I2C.TH0=1
'    RC4I2C.TH0=1
'    ODCONC.ODCC4 = 1
'    ODCONC.ODCC3 = 1
'
'    'Now set up the I2C slave library - four commands needed
'    HI2CSetAddress 0x70
'    HI2CMode Slave
'    SI2CSlave_ISR_Init
'    On Interrupt I2C1Interrupt call SI2CSlave_ISR_Handler

#script

    if novar(I2C1STAT1) then
        Warning "You may using the incorrect library function �.library is likely to fail"
    End if


#endscript


dim i2cbyte as byte



sub SI2CSlave_ISR_Init

    #ifdef HI2CSlaveStateNotHandledErrorLED
          Dir HI2CSlaveStateNotHandledErrorLED out
    #endif
    #ifdef HI2CSlaveSSPOVOverflowErrorLED
          Dir HI2CSlaveSSPOVOverflowErrorLED out
    #endif

    I2C1PIR = 0x00    '    Clear all the error flags
    I2C1ERR = 0x00


    I2C1CON0 = 0x00
    I2C1CON1 = 0x00
    I2C1CON2 = 0x08
    I2C1STAT1 = 0x00

    'CLEAR I2C FLAGS
    PIR3.I2C1IF = 0
    I2C1PIR = 0x00

    'Enable local interrupt on ACK Sequence and reset register
    I2C1PIE = 0x40

    'Enable interrupts
    I2C1PIE.PCIE=1     'enable I2C interrupt for stop condition
    I2C1PIE.SCIE = 1
    I2C1PIE.RSCIE = 1

    'Enable I2C module
    I2C1CON0.EN = 1

    'init the array
    HI2CIndex = 0

end sub



sub SI2CSlave_ISR_Handler

   if PIR3.I2C1IF = 1 or I2C1PIR.PCIF = 1 then

      'Clear the I2C interrupt flag
      PIR3.I2C1IF = 0

      'ACK Sequence Interrupt Detected
      if I2C1PIR.ACKTIF = 1 then

          'For Slave Read/Master Write
          if ( I2C1STAT0.R = 0 ) then
            'Data Byte Received
            if I2C1STAT0.D = 1 then
                'Read from RXB

state1:

                'start
                if I2C1PIR.SCIF = 1 or I2C1PIR.RSCIF = 1 then

                  SI2CSlave_State_1 ( I2C1ADR0 )
                  I2C1PIR.SCIF = 0
                  I2C1PIR.RSCIF = 0

state2a:

                  'data
                  if I2C1STAT1.rxbf = 1 then
                      SI2CSlave_State_2 ( I2C1RXB )
                  end if

                else

state2b:
                  'data
                  if I2C1STAT1.rxbf = 1 then
                      SI2CSlave_State_2 ( I2C1RXB )
                  end if

                end if

            end if

          else
                'For Slave Write/Master Read
                'Write to TXB

                'start
                if I2C1PIR.SCIF = 1 or I2C1PIR.RSCIF = 1 then
state4:
                    SI2CSlave_State_4
                    I2C1PIR.SCIF = 0
                    I2C1PIR.RSCIF = 0

                else

                   if I2C1PIR.PCIF = 0 then  'handle everything that is NOT a STOP
state5:
                      SI2CSlave_State_5

                   end if

                end if

          end if


      end if

     'handle stop Interrupt
      if I2C1PIR.PCIF = 1 then

        if I2C1STAT0.R = 0 and I2C1STAT0.D = 1 then

state3:
            'data
            if I2C1STAT1.rxbf = 1 then

                'Ensure all conditions are met....
                SI2CSlave_State_2 ( I2C1RXB )
            end if

            SI2CSlave_State_3

        else
state6:
            SI2CSlave_State_6
        end if

      end if

      'Clear the interrupt flag
      I2C1PIR.ACKTIF = 0

      I2C1ERR = 0
      I2C1PIR.PCIF = 0
      'Release SCL
      I2C1CON0.CSTR = 0
   end if
end sub

'Example routines - all are needed in the Message handler or in the user program

sub SI2CSlave_State_1 ( in I2CByte )

end Sub

sub SI2CSlave_State_2 ( in I2CByte )

end Sub

sub SI2CSlave_State_3

end Sub

sub SI2CSlave_State_4

end Sub

sub SI2CSlave_State_5

end Sub

sub SI2CSlave_State_6

end Sub

sub SI2CSlave_State_Error ( in I2CErrorCode )

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

sub SI2CSlave_State_On_Start

end sub

sub SI2CSlave_State_On_Stop

'         ' let user process buffer
         SI2CMessageHandler_Process_Message

end sub
