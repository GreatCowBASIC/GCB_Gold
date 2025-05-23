;Copyright 2017-2025 Thomas Henry
;
;Sony IR Remote Control Library for GCBASIC

;Thomas Henry
;Version 2.0 -- 4/23/2014 - 2017

;This include file will let you easily read and use the
;infrared signals from a Sony compatible television remote
;control. In particular, the remote control transmits a
;pulse modulated signal, the sensor detects this, and the
;subroutine in this header file decodes the signal,
;returning two numbers: one representing the device
;(television, VCR, DVD, tuner, etc.), while the the other
;returns the key which has been depressed (VOL+, MUTE,
;channel numbers 0 through 9, etc.).

;This has been tested and confirmed with a fixed
;remote control purchased surplus for $2.00 from All
;Electronics, as well as an universal remote control, set
;to Sony mode.

;Moreover it has also been tested with a Panasonic IR
;sensor and a Vishay sensor, both purchased surplus for
;about fifty cents.

;Every combination performed well, and it is probably the;
;case that most any garden variety 38 kHz IR sensor will
;work. The only tricky bit is making sure you get the pinout
;for your sensor correct; search out the datasheet for
;whichever device you use. There are only three pins:

;Ground
;Vcc
;Data

;It is essential to filter the power applied to the Vcc pin.
;Do this by connecting a 100 ohm resistor from the +5V power
;supply to the Vcc pin, and bridge the pin to ground with a
;4.7uF electrolytic capacitor.

;The Data pin requires a 4.7k pullup resistor.

;There is only one constant required of the calling program.
;It indicates which port line the IR sensor is connected to.
;For example,

;#DEFINE IR_DATA_PIN PORTA.0

;There is one subroutine:

;readIR_Remote(IR_rem_dev, IR_rem_key)

;The values returned are, respectively, the device number
;mentioned earlier and the key that is currently pressed. Both
;are byte values.

;Seventeen local bytes are consumed, and two bytes are used for
;the output parameters. That's a grand total of nineteen bytes
;required when invoking this subroutine.

sub readIR_Remote(out IR_rem_dev as byte, out IR_rem_key as byte)
  dim IR_rem_count, IR_rem_i as byte
  dim IR_rem_width(12) as byte            ;pulse width array

  do
    IR_rem_count = 0                      ;wait for start bit
    do while IR_DATA_PIN = 0              ;measure width (active low)
      wait 100 uS                         ;24 X 100 uS = 2.4 mS
      IR_rem_count++
    loop
  loop while IR_rem_count < 20            ;less than this so wait

  for IR_rem_i = 1 to 12                  ;read/store the 12 pulses
    do
      IR_rem_count = 0
      do while IR_DATA_PIN = 0            ;zero = 6 units = 0.6 mS
        wait 100 uS                       ;one = 12 units = 1.2 mS
        IR_rem_count++
      loop
    loop while IR_rem_count < 4           ;too small to be legit

    IR_rem_width(IR_rem_i) = IR_rem_count ;else store pulse width
  next IR_rem_i

  IR_rem_key = 0                          ;command built up here
  for IR_rem_i = 1 to 7                   ;1st 7 bits are the key
    IR_rem_key = IR_rem_key / 2           ;shift into place
    if IR_rem_width(IR_rem_i) > 10 then   ;longer than 10 mS
       IR_rem_key = IR_rem_key + 64       ;so call it a one
    end if
  next

  IR_rem_dev = 0                          ;device number built up here
  for IR_rem_i = 8 to 12                  ;next 5 bits are device number
    IR_rem_dev = IR_rem_dev / 2
    if IR_rem_width( IR_rem_i ) > 10 then
       IR_rem_dev = IR_rem_dev + 16
    end if
  next
end sub
