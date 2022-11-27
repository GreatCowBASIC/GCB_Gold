This solution is focused on the 16f145x family.  

Background: An ASM development has completed a robust DFU implementation for the 16f145x family.  This DFU implementation is a bootloader use case and it is relatively simple to use.

1.	16f145x bootloader_ProgC5_ResetA3.hex.  A  specific HEX file. This has a valid VID/ID using the Microchip DFU address as a Microchip External Memory Programmer.  See the ZIP for the hex file.

	USB_VENDOR_ID		equ	0x1209
	USB_PRODUCT_ID		equ	0x2002

2.	Simply program the HEX file using PK+

3.	From within Great Cow BASIC IDE/PP Tool. Add a new programmer to support the DFU.  See  the USE.INIPART in the GreatCowBASIC folder in the ZIP.

[tool = dfu]
desc = DFU
type = programmer
useif = 
progconfig = 
command = %instdir%\..\DFU\HEX2DFU.exe
params = %filename%
workingdir = %instdir%\..\DFU

4.	Copy the required DFU utilites to the DFU folder from the ZIP file.  Typically needs to be  ..\GCB@SYN\DFU folder.   Copies other important file – copy them all.

5.	Attach the board/chip and hold down RC5 and press Reset.   The board/chip will activate the USB.  An LED attached to PortA.5 will pulse.  The board/chip may not be recognised, see next.

6.	Update the driver for the ‘not be recognised’ device.  The driver is in the  ..\GCB@SYN\DFU\Driver folder.  After few minutes this will completed and install the driver.

7.	Great Cow BASIC IDE.  Create programs as normal.  The header MUST have #option bootloader 0x200 to ensure the DFU bootloader is protected.

; ----- Configuration
  #chip 16f1459, 1
  #option bootloader 0x200   'must for usb Bootloader
  #option NoConfig                 'Tells compiler not to set config bits
  #option explicit                   'All variables must be declared!

I have included 16F1459Test.GCB in the c:\gcb@syn folder.

8.	Reset the microcontroller – and Portc.5/prog MUST be low for the bootloader to be activated.

9.	Then, program from the IDE using the DFU defined in PP Tool.


So, in summary  - program as normal. Reset the part to activate the bootloader and program.





