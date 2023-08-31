'    Software I2C routines for the GCBASIC compiler
'    Copyright (C) 2009-2020 Hugh Considine, Evan R. Venn, Thomas Henry, William Roth

'    This library is free software' you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation' either
'    version 2.1 of the License, or (at your option) any later version.

'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY' without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.

'    You should have received a copy of the GNU Lesser General Public
'    License along with this library' if not, write to the Free Software
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
'
'    Updated Evan R Venn - Oct 2013, thanks go to Microchip and then Application Note AN1488 @ Beta v0.90
'
'     0.81 - Initial release
'     0.82 - Improved ACKPOLL handling
'     0.83 - Improved ACKPOLL handling - added I2CReturnState to return error condition
'     0.83 - Added default sub values
'     0.84 - Added Optional I2CGETACK As byte = TRUE to maintain SLAVE compatibility
'     0.85 - Improved stability and corrections
'     0.86 - Delete v2 subs
'     0.87 - Added new sub I2CSlaveDeviceReceive to handle slave correctly.  I2CSlaveDeviceReceive thisI2CSlavedevice, readDeviceAddress, TRU
'     0.88 - Revised timings and ensured the DATA line goes low at end of process.
'     0.89 - Revised I2CAckPollState = I2CSendState
'     0.90 - Revised to remove the BUGS!!!!!
'     I2c-alt.h - Revised and documented by Thomas Henry, July 17, 2014
'     0.92 - Revised and merged 0.91 and I2C-alt.h
'     0.93 - Revised to remove use of Constants where they should have been variables and the reverse case.
'     0.94 - Revised to Move variable defintion to prevent variable defintion when not required
'    - Eliminated parameter on I2CSend as this was causing backward compatibility issues
'    - This is a megerd version of the alternate version of the software I2C routines by
'          - Thomas Henry, July 17, 2014.
'          - ACK/NAK logic for Slave multibyte reception
'          - therefore multiple Slaves are permitted on the bus
'    - eliminated need for a separate addressed slave read command,
'    - reduced the command set to make I2C more understandable
'    - made the basic commands more intuitive for beginners
'    - eliminated redundant code while speeding up the loops
'    - a single error flag (I2CAck or I2CSendState) checks many error conditions
'    - more extensive documentation
'    - eliminated redundancies and unneeded global variables
'    - accounted for parameter corruption throughout
'     0.95 revised to restore I2C_Dev_OK
'     0.96 revised to support I2C_USE_TIMEOUT for I2C Master
'     To enable failsafe in I2C Master mode, user must add #define I2C_USE_TIMEOUT to source code
'    Updated Oct 2016  - for Option Explicit
'    28/92/2020   Updated to add LEFT to rotate I2CByte left on line 412 and removed I2C_DATA_LOW in I2CSend SLAVE method (caused SDA line glitch)
' 14/08/22 Updated user changeable constants only - no functional change
' 22/08/23 Added I2CPreSendMacro and I2CPostSendMacro and I2CPostSendMacroLabel to support greater control of I2CSend
' 23/08/23 Revised INTERRUPT caching to ensure state is preserved by I2C routines

'    - With the default constants, communication can be as high as 75 kHz.

'    - Command Set (parameters I2CAck and I2C_Dev_OK are optional):

'    -   I2CStart() and I2CStop() are the usual commands used to start
'    -   and stop I2C communications. I2CAckPoll() is used to sense
'    -   whether the Slave device is alive and ready to respond. It
'    -   is also useful for waiting until an Eeprom has completed its
'    -   write cycle. The optional parameter I2C_Dev_OK reports whether
'    -   the device responded or timed out.

'    -   The two high-level commands are I2CReceive() and I2CSend().
'    -   Either of these may be used in either Master or Slave mode.
'    -   For emphasis, this means that the following combinations
'    -   are valid:

'    -   Master receives from the Slave
'    -   Slave receives from the Master
'    -   Master sends to the Slave
'    -   Slave sends to the Master

'    -   Additionally, a fair amount of error detection is worked in
'    -   which can detect certain problems, typically with the SCL
'    -   (clock) line. By using the error flag (a parameter called
'    -   I2CAck) it is possible to have the Master or Slave retry the
'    -   communication in case of errors. The parameter I2CAck is
'    -   optional, and may be ignored in many situations. Note, too,
'    -   that clock stretching is implemented which gives the Slave
'    -   a chance to get caught up when needed. This is invisible to
'    -   the programmer/user.

'    -   Interrupts are always disabled during I2C communication.
'    -   since it makes no sense to permit interruptions in a
'    -   synchronous environment.

'    -   The purpose of the parameter I2CByte in these two commands is
'    -   obvious--it is the byte which is received or transmitted.
'    -   On the other hand, the second parameter, I2CAck, can be used
'    -   in a couple different ways to monitor for error conditions,
'    -   among other things. Here are the details for the two commands.

'    -   *** I2CReceive(out I2CByte, in/out I2CAck)

'    -   MASTER FROM SLAVE:
'    -   In Master mode, if your program sets I2CAck to TRUE before
'    -   calling I2CReceive(), then the Master will send an ACK to
'    -   the Slave after receiving its byte. If FALSE then the Master
'    -   will send a NAK to the slave after receiving its byte
'    -   ACK and NAK are aliases for TRUE and FALSE, respectively.

'    -   SLAVE FROM MASTER:
'    -   In Slave mode, upon exit from I2CReceive(), the Slave will
'    -   set I2CAck to TRUE if things went well, or FALSE if a timeout
'    -   occurred (supposing you had requested timeouts be used).

'    -   *** I2CSend(in I2CByte )

'    -   MASTER TO SLAVE:
'    -   In Master mode, I2CAck is returned to indicate whether an
'    -   ACK or a NAK was received from the Slave after transmission.
'    -   ACK means all is okay, while NAK means something went wrong.

'    -   SLAVE TO MASTER:
'    -   In slave mode, I2CAck is returned to indicate whether an ACK
'    -   or a NAK was received from the Master after transmission.
'    -   ACK means all is okay, while NAK means something went wrong.

'    -   Please note: for many programs, you simply won't need the
'    -   error detection and can ignore the value of the I2CAck parameter.

'             --- Constants

'Constants with no defaults which must be set in the calling program
'#define I2C_DATA                 'port line for SDA
'#define I2C_CLOCK                'port line for SCL
'#define I2C_USE_TIMEOUT          'if defined, the Slave gives the Master
                                  '256 chances to respond before giving up


' User Changeable constants

'End of User changeable constants


'Constants whose values shouldn't be changed

#define I2C_DATA_HIGH   dir I2C_DATA in
#define I2C_DATA_LOW    dir I2C_DATA out: set I2C_DATA OFF
#define I2C_CLOCK_HIGH  dir I2C_CLOCK in
#define I2C_CLOCK_LOW   dir I2C_CLOCK out:set I2C_CLOCK OFF

#define NAK             FALSE
#define NACK            FALSE     'permit alternative spelling
#define ACK             TRUE

'             --- Variables


dim I2CAckPollState as byte

#define I2CSendState I2CAckPollState 'retained I2CSendState for backwards compatibility
#define I2C_Dev_OK I2CAck

#script

  'Constants with defaults which may be changed in the calling program
  IF NODEF(I2C_MODE) THEN
     I2C_MODE=MASTER           'DEFAULT TO MASTER MODE
  END IF

  IF NODEF(I2C_BIT_DELAY) THEN
     I2C_BIT_DELAY=2 US      'WIDTH OF DATA BIT ON SDA
  END IF


  IF NODEF(I2C_CLOCK_DELAY) THEN
     I2C_CLOCK_DELAY=1 US      'WIDTH OF CLOCK PULSE ON SCL
  END IF

  IF NODEF(I2C_END_DELAY) THEN
     I2C_END_DELAY=1 US      'INTERVAL BETWEEN CLOCK PULSES
  END IF

#endscript

'             --- Subroutines

#startup InitI2C, 90                  'automatically call in main program




sub InitI2C()
  ' Moved to prevent variable defintion when not required
  dim I2COldState, I2CState, I2CMatch, I2CTemp as byte
  dim I2CCount alias I2CState
  I2C_DATA_HIGH                   'release SDA (open drain floats high)
  I2C_CLOCK_HIGH                  'release SCL (open drain floats high)
  I2CMatch = FALSE                'address doesn't match (Slave mode)

  #if I2C_MODE = Slave            'previous State unknown to begin with
    I2COldState = 255             'so use this to detect an I2C Start
  #endif
end sub

'             ---

sub I2CStart()
  Dim I2CINTSTATESAVE as BIT
  #if I2C_MODE = Master           'send a Start in Master mode
    #IFNDEF I2C_DISABLE_INTERRUPTS
        I2CINTSTATESAVE = GIE
        GIE = 0 'don't interrupt now we've started
    #endif
    I2C_DATA_HIGH                 'SDA and SCL idle high
    I2C_CLOCK_HIGH
    wait I2C_END_DELAY            'let settle a while

    I2C_DATA_LOW                  'then, SDA low while SCL still high
    wait I2C_CLOCK_DELAY          'for this amount of time

    I2C_CLOCK_LOW                 'end with SCL low, ready to clock
  #endif

  #if I2C_MODE = Slave            'wait for a Start in Slave mode
    'State 255 is a dummy, indicating previous state not known yet
    'State 0 is SCL = 1 and SDA = 1
    'State 1 is SCL = 1 and SDA = 0
    'State 2 is SCL = 0 and SDA = don't care
    'Start occurs only on transition from State 0 to State 1

    I2CState = 255                'always begin in dummy state
    do                            'loop until state goes from 0 to 1
      I2COldState = I2CState      'save current state for next time
      if I2C_CLOCK = 1 then       'it's either State 0 or 1
        if I2C_DATA = 1 then      'State 0 is SCL=1 and SDA=1
          I2CState = 0
        else                      'otherwise update to State 1
          if I2COldState = 0 then
            exit do               'exit if State changed from 0 to 1
          end if
          I2CState = 1            'State 1 is SCL=1 and SDA=0
        end if
      else
        I2CState = 2              'State 2 is anything else
      end if
    loop

    #IFNDEF I2C_DISABLE_INTERRUPTS
        intOff                        'don't interrupt now we've started
    #endif

  #endif
end sub

'             ---

sub I2CStop()
  Dim I2CINTSTATESAVE as BIT
  #if I2C_MODE = Master           'send a Stop in Master mode
    I2C_CLOCK_LOW                 'begin with SCL=0 and SDA=0
    I2C_DATA_LOW
    wait I2C_END_DELAY            'let ports settle

    I2C_CLOCK_HIGH                'make SCL=1 first
    wait I2C_CLOCK_DELAY          'hold for normal clock width time

    I2C_DATA_HIGH                 'then make SDA=1 afterwards
    wait I2C_END_DELAY            'hold for normal between-time
    'back idling with SCL=1 and SDA=1 at this point


    #IFNDEF I2C_DISABLE_INTERRUPTS
        GIE = I2CINTSTATESAVE                         'done, so re-enable interrupts
    #endif

  #endif

  #if I2C_MODE = Slave            'in Slave mode we are
    I2CMatch = FALSE              'no longer addressing this device
    #IFNDEF I2C_DISABLE_INTERRUPTS
        GIE = I2CINTSTATESAVE                     'so re-enable interrupts
    #endif
  #endif
end sub

' Resets the I2C bus.  See http://www.analog.com/static/imported-files/application_notes/54305147357414AN686_0.pdf
Sub I2CReset
       ' added 0.84
       I2CAckPollState = false
       I2CSTART
       repeat 9
              I2CSEND ( 0 )    ' send nine zero's
              if I2C_DATA = 1 then
                 I2CAckPollState = true    ' line has gone high. The line is reset now leave
                 exit repeat
              end if
       end Repeat
I2CResetFin:
       I2CSTOP

end Sub


' Restarts the bus
Sub I2CRestart
    I2CStart
end Sub


Function I2CStartOccurred
  'Check if a start condition has occurred since the last run of this function
  'Only used in slave mode

  #if I2C_MODE = Master
    I2CStartOccurred = TRUE
  #endif

  #if I2C_MODE = Slave
    I2CStartOccurred = FALSE

    If I2C_CLOCK = 1 Then
      'State 0, CK 1, DA 1
      If I2C_DATA = 1 Then
        I2CState = 0
      'State 1, CK 1, DA 0
      Else
        'Start happens when data drops while clock high, so:
        'start occurs on transition from CK 1 DA 1 (state 0) to CK 1 DA 0 (state 1)
        If I2COldState = 0 Then
          I2CStartOccurred = TRUE
        End If
        I2CState = 1
      End If
    Else
      'State 2 when CK 0
      I2CState = 2
    End If
    I2COldState = I2CState

  #endif

End Function

'             ---
sub I2CSend(in I2CByte )

  #ifdef I2CPreSendMacro
    I2CPreSendMacro
  #endif

  #if I2C_MODE = Master           'send byte from Master to Slave
     I2C_CLOCK_LOW                 'begin with SCL=0
     wait I2C_END_DELAY            'let port settle

    repeat 8                      '8 data bits
      if I2CByte.7 = ON then      'put most significant bit on SDA line
        I2C_DATA_HIGH
      else
        I2C_DATA_LOW
      end if

      rotate I2CByte left         'shift in bit for the next time
      I2C_CLOCK_HIGH              'now clock it in

      #ifndef I2C_USE_TIMEOUT
        wait while I2C_CLOCK = OFF    'permit clock stretching here
      #endif

      #ifdef I2C_USE_TIMEOUT         'clock strething with timeout
         I2CCount = 0                'wait until clock goes high
         do while I2C_CLOCK = OFF
           IC2CCount++                'give SCL another chance to go high
           if I2CCount = 0 then       'else, used up our 256 chances
             I2CAck = FALSE
             Exit sub                 'so indicate an error
           end if                      'bail out, we're giving up
         wait 100 us
        loop
      #endif

      wait I2C_CLOCK_DELAY        'clock pulse width given here
      I2C_CLOCK_LOW               'done clocking that bit
      wait I2C_END_DELAY          'time between clock pulses
    end repeat                    'then do next bit

    wait I2C_BIT_DELAY            'pad timing just a little

    I2C_DATA_HIGH                 'idle SDA to let Slave respond
    wait I2C_END_DELAY            'let SDA port line settle
    I2C_CLOCK_HIGH                'clock for the ACK/NAK bit

    #ifndef USE_I2C_TIMEOUT
       wait while I2C_CLOCK = OFF    'permit clock stretching here
    #endif

    #Ifdef USE_I2C_TIMEOUT
     I2CCount = 0                'wait until clock goes high
       do until I2C_CLOCK = ON
          I2CCount ++            'give SCL another chance to go high
          if I2CCount = 0 then    'else, used up our 256 chances
             I2CAck = FALSE        'so indicate an error
             exit sub              'bail out, we're giving up
          end if
        loop
    #Endif
                                  'restored 'I2CSendState' variable for backwards compatibility
    if I2C_DATA then              'read 9th bit in from Slave
      I2CAck = FALSE              'return a NAK to the program
      I2CSendState =  FALSE        'state of target device with respect. Retained for backwards compatibility
    else
      I2CAck = TRUE               'else, return an ACK to the program
      I2CSendState =  TRUE        'state of target device with respect. Retained for backwards compatibility
    end if

    I2C_CLOCK_LOW                 'may be more bytes to clock out
    wait I2C_END_DELAY            'so keep idling both
    I2C_DATA_LOW                  'SCL and SDA low
    wait I2C_BIT_DELAY            'wait the usual bit length
  #endif


  #if I2C_MODE = Slave            'send a byte from Slave to Master
    #ifndef I2C_USE_TIMEOUT       'never give up on the Master
      wait until I2C_CLOCK = OFF  'and just keep waiting for response
    #endif

    #ifdef I2C_USE_TIMEOUT        'else give Master 256 chances to act
      I2CCount = 0                'prime the timing loop
      do until I2C_CLOCK = OFF    'hang around until SCL goes low
        I2CCount++                'bump while waiting
        if I2CCount = 0 then      'all the way round the horn, so
          I2CAck = FALSE          'flag as an error
          exit sub                'give up and exit entire subroutine
        end if
      loop                        'else, try again
    #endif

                                   'removed I2C_DATA_LOW as this caused glitches on I2C bus
    'I2C_DATA_LOW                  'begin with SDA=0 as a baseline

    repeat 8                      '8 data bits
      if I2CByte.7 = ON then      'put most significant bit on SDA line
        I2C_DATA_HIGH
      else
        I2C_DATA_LOW
      end if

      rotate I2CByte left
      wait until I2C_CLOCK = ON   'wait for SCL=1

      #ifndef I2C_USE_TIMEOUT     'never give up on the Master
        wait until I2C_CLOCK = OFF
      #endif

      #ifdef I2C_USE_TIMEOUT      'if using timeout option, then
        I2CCount = 0              'wait until clock goes low
        do until I2C_CLOCK = OFF
          I2CCount += 1           'give SCL another chance to go low
          if I2CCount = 0 then    'else, used up our 256 chances
            I2CAck = FALSE        'so indicate an error
            exit sub              'bail out, we're giving up
          end if
        loop
      #endif
    end repeat                    'all 8 bits sent now

    I2C_DATA_HIGH                 'idle the SDA line again

    wait until I2C_CLOCK = ON     'wait for the 9th bit

    if I2C_DATA then              'read 9th bit in from Master
      I2CAck = FALSE              'return a NAK to program
    else
      I2CAck = TRUE               'else, return ACK if all okay
    end if

    #ifndef I2C_USE_TIMEOUT       'never give up on the Master
      wait until I2C_CLOCK = OFF  'let SCL go low
    #endif

    #ifdef I2C_USE_TIMEOUT        'if using timeouts
      I2CCount = 0                'give SCL 256 chances to go low
      do until I2C_CLOCK = OFF
        I2CCount++                'give it another chance
        if I2CCount = 0 then      'after this, give up altogether
          I2CAck = FALSE          'flag it as a problem
          exit sub
        end if
      loop
    #endif
  #endif

  #ifdef I2CPreSendMacro
    I2CPostSendMacroLabel:
  #endif
  #ifdef I2CPostSendMacro
    I2CPostSendMacro
  #endif
end sub

'             ---

sub I2CReceive (out I2CByte, optional I2CAck = ACK)
  #if I2C_MODE = Master           'receive byte from Slave to Master
    I2C_CLOCK_LOW                 'SCL begins low
    I2C_DATA_HIGH                 'this makes SDA an input now

    I2CByte = 0                   'received byte built up here
    repeat 8                      'fetch the 8 bits
      wait I2C_END_DELAY          'let port lines settle down
      I2C_CLOCK_HIGH              'send a clock pulse

      #ifndef USE_I2C_TIMEOUT
         wait while I2C_CLOCK = OFF  'permit clock stretching
      #endif

      #ifdef I2C_USE_TIMEOUT      'clock stretching with timeout
        I2CCount = 0              'wait until clock goes high
         do until I2C_CLOCK = ON
           I2CCount += 1           'give SCL another chance to go high
           if I2CCount = 0 then    'else, used up our 256 chances
             I2CAck = FALSE        'so indicate an error
             exit sub              'bail out, we're giving up
           end if
          wait 100 us
        loop
      #endif

      rotate I2CByte left         'make room for next bit

      set I2CByte.0 OFF           'assume it's a zero
      if I2C_DATA = ON then       'else, it's a one
        set I2CByte.0 ON
      end if

      wait I2C_CLOCK_DELAY        'clock high this amount of time
      I2C_CLOCK_LOW               'SCL now low
      wait I2C_END_DELAY          'time before next clock pulse
    end repeat

    if I2CAck then
      I2C_DATA_LOW                'SDA=0 means ACK
    else
      I2C_DATA_HIGH               'SDA=1 means NAK
    end if

    wait I2C_END_DELAY            'either way, let it settle
    I2C_CLOCK_HIGH                'then clock it out

     #ifndef USE_I2C_TIMEOUT
       wait while I2C_CLOCK = OFF    'permit clock stretching
     #endif

     #ifdef I2C_USE_TIMEOUT       'clock stretchihg with timeout
        I2CCount = 0              'wait until clock goes high
        do until I2C_CLOCK = ON
          I2CCount += 1           'give SCL another chance to go high
          if I2CCount = 0 then    'else, used up our 256 chances
            I2CAck = FALSE        'so indicate an error
            exit sub              'bail out, we're giving up
          end if
          wait 100 us
        loop
     #endif

    wait I2C_CLOCK_DELAY          'keep high long enough
    I2C_CLOCK_LOW                 'then SCL goes low again
    wait I2C_END_DELAY            'and settles before proceeding
  #endif

  #if I2C_MODE = Slave            'receive byte from Master to Slave
    I2CAck = TRUE                 'assume success to begin with
    #ifndef I2C_USE_TIMEOUT       'never give up on the Master
      wait until I2C_CLOCK = OFF  'SCL must start low
    #endif

    #ifdef I2C_USE_TIMEOUT        'if using timeout option, then
      I2CCount = 0                'give 256 chances for SCL to go low
      do until I2C_CLOCK = OFF
        I2CCount++                'give SCL another chance to go low
        if I2CCount = 0 then
          goto I2C_Failed         'else give up after 256 tries
        end if
      loop
    #endif

    I2CByte = 0                   'received byte built up here
    repeat 8                      'fetch the 8 bits
      wait until I2C_CLOCK = ON   'wait for SCL to go high
      rotate I2CByte left         'make room for the next bit

      set I2CByte.0 OFF           'assume it's a zero
      if I2C_DATA = ON then       'else, SDA is a one
        set I2CByte.0 ON
      end if

      #ifndef I2C_USE_TIMEOUT     'never give up on the Master
        Wait Until I2C_CLOCK = OFF
      #endif

      #ifdef I2C_USE_TIMEOUT      'if using timeout option, then
        I2CCount = 0              'give 256 chances for SCL to go low
        do until I2C_CLOCK = OFF
          I2CCount++              'give SCL another chance to go low
          if I2CCount = 0 then
            goto I2C_Failed       'else give up after 256 tries
          end if
        loop
      #endif
    end repeat

    if I2CByte = I2C_ADDRESS then 'wake up the slave now
      I2CMatch = TRUE             'valid until Stop or failure
    end if

    if I2CMatch then              'only ACK if msg is for this device
      I2C_DATA_LOW                'sending SDA=0 means ACK
      wait until I2C_CLOCK = ON   'then Master clocks it in

      #ifndef I2C_USE_TIMEOUT     'never give up on the Master
        wait until I2C_CLOCK = OFF
      #endif

      #ifdef I2C_USE_TIMEOUT      'if using timeout option, then
        I2CCount = 0              'give 256 chances for SCL to go low
        do until I2C_CLOCK = OFF  'else, used up our chances
          I2CCount++
          if I2CCount = 0 then
            goto I2C_Failed       'else give up after 256 tries
          end if
        loop
      #endif
    end if
    goto I2C_Okay                 'everything worked, so exit

  I2C_Failed:
    I2CAck = FALSE                'signal communication failed
    I2CMatch = FALSE              'no longer addressing this device
  I2C_Okay:                       'I2CAck is still TRUE at this point
    I2C_DATA_HIGH                 'either way, raise SDA again
  #endif
end sub

'             ---

sub I2CAckPoll(in I2CByte, optional I2C_Dev_OK = TRUE)
  'I2CByte contains base device number to check on (R/W bit clear).
  'Check if device is ready or give up after 25 milliseconds
  'of retries. Each retry consumes about 100 microseconds

  I2CTemp = I2CByte               'use a copy to avoid interaction
  I2C_Dev_OK = FALSE              'assume no acknowledgment at outset

  for I2CCount = 0 to 255
    I2CStart                      'attempt to set up a write
    I2CSend(I2CTemp, I2C_Dev_OK)  'to this device
    I2CStop                       'release the bus
    if I2C_Dev_OK then            'device acknowledged with ACK
      exit for                    'exit means device is alive
    end if

    wait 10 10us                  'just wait a moment to stop pounding against device
  next I2CCount                   'else try again
  I2CAckPollState = I2C_Dev_OK    'set state.  Used to understand if the device responded in IC2 discover process
end sub
