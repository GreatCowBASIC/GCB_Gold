
'    ADAFruit_mini_colorTFT_joystick  routines for the GCBASIC compiler
'    Copyright (C) 2020 Evan Venn

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


#include <glcd.h>
#OPTION UserInclude
#define GLCD_TYPE GLCD_TYPE_ST7735R_160_80

'Need to user SW SPI as we are using MSSP1 for HWI2C
'#define ST7735_HardwareSPI     ' remove comment out if you want to use software SPI.

#define ST7735TABCOLOR ST7735_GREENTAB  ' can be ST7735_BLACKTAB, ST7735_GREENTAB or ST7735_REDTAB.  Each tab is a different ST7735 configuration.


#DEFINE MINITFT_WRITEADDR 188             ;0XBC
#DEFINE MINITFT_READADDR 189              ;0XBD

'Define a set of constants to the register bits
#Define Minitft_Button_A    Readminitft_H.2
#Define Minitft_Button_B    Readminitft_H.1
#Define Minitft_Joy_Select  Readminitft_H.3
#Define Minitft_Joy_Up      Readminitft.2
#Define Minitft_Joy_Down    Readminitft.4
#Define Minitft_Joy_Left    Readminitft.3
#Define Minitft_Joy_Right   Readminitft.7

#Define MINITFT_MODULE_BASE_ADDRESS_GPIO 1          ;GPIO = 1  Pg 21 of Seesaw Tutorial
#Define MINITFT_GPIO_BULK 4                         ;GPIO = 4 Reading this register reads all pins on PORTA of the seesaw device.

'Buttons
#Define MINITFT_DIRCLR_GPIO 3                         ;0x03 Pg 22 of Seesaw Tutorial - Writing a 1 to any bit in this register sets the direction of the corresponding pin to INPUT.
#Define MINITFT_PULLENSET_GPIO 11                     ;0x0B Pg 23 of Seesaw Tutorial - Writing a 1 to any bit in this register enables the internal pullup or pulldown on the corresponding pin.
#Define MINITFT_SET_FUNCTIONREG 5                     ;Writing a 1 to any bit in this register writes the corresponding pin HIGH. pg 22


#Define MINITFT_MODULE_BASE_ADDRESS_PWM 8                      ;PWM = 8  Pg 34 of Seesaw Tutorial
#Define MINITFT_MODULE_BASE_ADDRESS_AD  9                      ;AD = 9  Pg 24 of Seesaw Tutorial
#Define MINITFT_MODULE_FUNCTION_REGISTER_ADDRESS_AD0 7        ;PA02_A0 AD  = 7
#Define MINITFT_MODULE_FUNCTION_REGISTER_ADDRESS_AD1 8        ;PA03_A1 AD  = 8

#DEFINE MINITFT_REGISTER_4_DEFAULT 211
#DEFINE MINITFT_REGISTER_3_DEFAULT 195
#DEFINE MINITFT_REGISTER_2_DEFAULT 14
#DEFINE MINITFT_REGISTER_1_DEFAULT 156



Function ReadMiniTFT as long


        HI2CStart
            HI2CSend MINITFT_WRITEADDR
            HI2CSend MINITFT_MODULE_BASE_ADDRESS_GPIO ;Writing a 1 to any bit in this register enables the internal pullup or pulldown on the corresponding pin. The pull direction
            HI2CSend MINITFT_PULLENSET_GPIO ;(up/down) is determined by the GPIO (output) value - if the corresponding GPIO register bit is low, its a pulldown. High, its a pullup.
            HI2CSend MINITFT_REGISTER_4_DEFAULT
            HI2CSend MINITFT_REGISTER_3_DEFAULT
            HI2CSend MINITFT_REGISTER_2_DEFAULT
            HI2CSend MINITFT_REGISTER_1_DEFAULT
'        HI2CStop

        HI2CStart
            HI2CSend MINITFT_WRITEADDR
            HI2CSend MINITFT_MODULE_BASE_ADDRESS_GPIO
            HI2CSend MINITFT_DIRCLR_GPIO
            HI2CSend MINITFT_REGISTER_4_DEFAULT ;Writing a 1 to any bit in this register sets the direction of the corresponding pin to INPUT.
            HI2CSend MINITFT_REGISTER_3_DEFAULT ;Writing zeros to this register has no effect.
            HI2CSend MINITFT_REGISTER_2_DEFAULT ;PA15:PA08 = 1100 1111 - Pin 12 to Pin 7
            HI2CSend MINITFT_REGISTER_1_DEFAULT ;PA07:PA02 = 1101 1100 - Pin 6 to Pin 1   LSB
'        HI2CStop

        HI2CStart
            HI2CSend MINITFT_WRITEADDR
            HI2CSend MINITFT_MODULE_BASE_ADDRESS_GPIO ;Writing a 1 to any bit in this register writes the corresponding pin HIGH.  Writing zeros to this register has no effect.
            HI2CSend MINITFT_SET_FUNCTIONREG
            HI2CSend MINITFT_REGISTER_4_DEFAULT
            HI2CSend MINITFT_REGISTER_3_DEFAULT
            HI2CSend MINITFT_REGISTER_2_DEFAULT
            HI2CSend MINITFT_REGISTER_1_DEFAULT
'        HI2CStop

        HI2CStart
        HI2CSend MINITFT_WRITEADDR
        HI2CSend MINITFT_MODULE_BASE_ADDRESS_GPIO
        HI2CSend MINITFT_GPIO_BULK ;Reading this register reads all pins on PORTA of the seesaw device.
        HI2CRestart
        HI2CSend MINITFT_READADDR
        HI2CReceive [BYTE]ReadMiniTFT_E         ;Bits [31:24] MSB      Seems like BIG_ENDIAN is being used - Data sheet says LITTLE_ENDIAN ????????????
        HI2CReceive [BYTE]ReadMiniTFT_U         ;Bits [23:16]         I show bit order as if it is BIG ENDIAN
        HI2CReceive [BYTE]ReadMiniTFT_H         ;Bits [15:8]
        HI2CReceive [BYTE]ReadMiniTFT, Nack     ;Bits [7:0] LSB
        HI2CStop


End Function
