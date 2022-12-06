'    Library to allow the ATtiny88 (32 pin) breakout board to work with GCBASIC - modification based on UNO_mega328p.h by Richard White 15.08.2022
'    Copyright (C) 2010-2020 Hugh Considine

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

'Chip settings
#chip tiny88, 12

'A/D speed
'Need to slow down due to high clock speed
#define ADSpeed LowSpeed

'Analog inputs
'Analog 0 to 7 = AN0 to AN7 in GCBASIC
#define ANALOG_0 PORTC.0
#define ANALOG_1 PORTC.1
#define ANALOG_2 PORTC.2
#define ANALOG_3 PORTC.3
#define ANALOG_4 PORTC.4
#define ANALOG_5 PORTC.5
#define ANALOG_6 PORTA.0
#define ANALOG_7 PORTA.1
#define RESET portc.6

'Digital pins
#define DIGITAL_0 PORTD.0
#define DIGITAL_1 PORTD.1
#define DIGITAL_2 PORTD.2
#define DIGITAL_3 PORTD.3
#define DIGITAL_4 PORTD.4

#define DIGITAL_5 PORTD.5
#define DIGITAL_6 PORTD.6
#define DIGITAL_7 PORTD.7
#define DIGITAL_8 PORTB.0

#define DIGITAL_9 PORTB.1
#define DIGITAL_10 PORTB.2
#define DIGITAL_11 PORTB.3
#define DIGITAL_12 PORTB.4
#define DIGITAL_13 PORTB.5

#define DIGITAL_14 PORTB.6
#define DIGITAL_15 PORTB.7
#define DIGITAL_16 PORTC.7

#define DIGITAL_25 PORTA.2
#define DIGITAL_26 PORTA.3



'On-board devices
#define LED PORTD.0



