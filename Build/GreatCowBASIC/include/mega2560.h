'    Library to allow the Arduino mega2560 to work with GCBASIC
'    Copyright (C) 2018 R. White


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


'    28/02/2018	    Created by R. White based on the works of Hugh Considine.  The original works was the the UNO_328p.h pin map
'    05/10/2020     Corrected pin designations in the analog section Friderik Back



'    Used to connect the correct GCBASIC port to the relevant pin as labelled on an Arduino mega 2560    
'    i.e GCBasic PORTE.0 corresponds to the pin marked DIGITAL_0 on an Arduino mega 2560 PCB
'    For HW details look:

'    https://www.arduino.cc/en/Hacking/PinMapping2560

'------ Usage example ------
'
'  #define BUZZER DIGITAL_12
'          set BUZZER out        
'
'------ END of example -----

'Analog inputs
'Analog 0 to 15 = AN0 to AN15 in GCBasic
#define ANALOG_0 PORTF.0
#define ANALOG_1 PORTF.1
#define ANALOG_2 PORTF.2
#define ANALOG_3 PORTF.3
#define ANALOG_4 PORTF.4
#define ANALOG_5 PORTF.5
#define ANALOG_6 PORTF.6
#define ANALOG_7 PORTF.7

#define ANALOG_8  PORTK.0
#define ANALOG_9  PORTK.1
#define ANALOG_10 PORTK.2
#define ANALOG_11 PORTK.3
#define ANALOG_12 PORTK.4
#define ANALOG_12 PORTK.5
#define ANALOG_14 PORTK.6
#define ANALOG_15 PORTK.7

'Digital pins
'Digital ports 2-13 and 44-46 has a PWM ability

#define DIGITAL_0 PORTE.0       'UART1 RX
#define DIGITAL_1 PORTE.1       'UART1 TX
#define DIGITAL_2 PORTE.4       
#define DIGITAL_3 PORTE.5
#define DIGITAL_4 PORTG.5       
#define DIGITAL_5 PORTE.3
#define DIGITAL_6 PORTH.3       
#define DIGITAL_7 PORTH.4       

#define DIGITAL_8 PORTH.5         
#define DIGITAL_9  PORTH.6      
#define DIGITAL_10 PORTB.4      
#define DIGITAL_11 PORTB.5      
#define DIGITAL_12 PORTB.6      
#define DIGITAL_13 PORTB.7      
#define DIGITAL_14 PORTJ.1      'TX3
#define DIGITAL_15 PORTJ.0      'RX3

#define DIGITAL_16 PORTH.1      'TX2
#define DIGITAL_17 PORTH.0      'RX2
                                
#define DIGITAL_18 PORTD.3      'TX1        '                 , EXT INT 3 
#define DIGITAL_19 PORTD.2      'RX1        '                 , EXT INT 2 

#define DIGITAL_20 PORTD.1   	'I2C SDA  , EXT INT 1
#define DIGITAL_21 PORTD.0      'I2C SCL  , EXT INT 0

#define DIGITAL_22 PORTA.0
#define DIGITAL_23 PORTA.1
#define DIGITAL_24 PORTA.2
#define DIGITAL_25 PORTA.3
#define DIGITAL_26 PORTA.4
#define DIGITAL_27 PORTA.5
#define DIGITAL_28 PORTA.6
#define DIGITAL_29 PORTA.7

#define DIGITAL_30 PORTC.7
#define DIGITAL_31 PORTC.6
#define DIGITAL_32 PORTC.5
#define DIGITAL_33 PORTC.4
#define DIGITAL_34 PORTC.3
#define DIGITAL_35 PORTC.2
#define DIGITAL_36 PORTC.1
#define DIGITAL_37 PORTC.0

#define DIGITAL_38 PORTD.7
#define DIGITAL_39 PORTG.2
#define DIGITAL_40 PORTG.1
#define DIGITAL_41 PORTG.0

#define DIGITAL_42 PORTL.7
#define DIGITAL_43 PORTL.6
#define DIGITAL_44 PORTL.5      'PWM
#define DIGITAL_45 PORTL.4      'PWM
#define DIGITAL_46 PORTL.3      'PWM
#define DIGITAL_47 PORTL.2
#define DIGITAL_48 PORTL.1
#define DIGITAL_49 PORTL.0

#define DIGITAL_50 PORTB.3     'MISO
#define DIGITAL_51 PORTB.2     'MOSI
#define DIGITAL_52 PORTB.1     'SCK
#define DIGITAL_53 PORTB.0     'SS

' Analog ports 0-15 as digital ports 54-69

#define DIGITAL_54 PORTF.0
#define DIGITAL_55 PORTF.1
#define DIGITAL_56 PORTF.2

#define DIGITAL_57 PORTF.3
#define DIGITAL_58 PORTF.4
#define DIGITAL_59 PORTF.5
#define DIGITAL_60 PORTF.6
#define DIGITAL_61 PORTF.7

#define DIGITAL_62 PORTK.0
#define DIGITAL_63 PORTK.1
#define DIGITAL_64 PORTK.2
#define DIGITAL_65 PORTK.3
#define DIGITAL_66 PORTK.4
#define DIGITAL_67 PORTK.5
#define DIGITAL_68 PORTK.6
#define DIGITAL_69 PORTK.7

