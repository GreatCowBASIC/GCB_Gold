#IGNORE
#option Explicit

/*
	AS726x spectral sensor routines for the GCBASIC compiler
	Copyright (C) 2025 Evan Venn

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

	//	26042024	Initial release







/*
	AS7262 Visible Light 6-channel spectral (visible) sensor
	AS7263 Near Infrared Spectral Sensor

	Detects six distinct channels of visible light with this spectral sensor. 
	Unlike normal RGB colour sensors, with the AS7262 provides six bands of colour readings that roughly translate to red, orange, yellow, green, blue, and violet. The rainbow.

	The two onboard white LEDs provides illumination to give more accurate spectral readings and also measure the reflectivity of surfaces. 
	The LEDs are typically high-quality ones with a CRI of >=90 and a colour temperature of 4,000K for consistent and accurate colour rendering.

	With this sensor, like analysing different light sources, measuring the transmission of light through materials or solutions (with an additional light source).

	The AS7262 6-Channel Spectral Sensor Breakout is an I2C interface and is 3.3V or 5V compatible. 

		I2C interface (address: 0x49)
		6 spectral channels (450, 500, 550, 570, 600, 650nm)
		2 onboard illumination LEDs
		3.3V or 5V compatible
		Reverse polarity protection

		*************************************************************************

		! Utility methods
		Sub AS726X_Init( ( optional AS726XGain as Byte = 3, optional AS726XMeasurementMode as Byte = 3 )	 )
			Sets up the sensor for constant read
			Returns the sensor version (AS7262 or AS7263)
			Returns the sensor error number
			
			Set to 12.5mA (minimum)
			Turn off to avoid heating the sensor
			Set to 8mA (maximum)
			Turn off lights to save power
			setIntegrationTime 50 * 2.8ms = 140ms. 0 to 255 is valid.
			If you use Mode 2 or 3 (all the colors) then integration time is double. 140*2 = 280ms between readings.
			Set gain to 64x
			One-shot reading of VBGYOR

		Sub AS726X_SetGain( in AS726XSetGain = 0b00..0b11 )
			Sets the measurement mode
			Mode 0: Continuous reading of VBGY (7262) / STUV (7263)
			Mode 1: Continuous reading of GYOR (7262) / RTUX (7263)
			Mode 2: Continuous reading of all channels (power-on default)
			Mode 3: One-shot reading of all channels

		Sub AS726X_TakeMeasurements()
			Tells IC to take measurements and polls for data ready flag
			Clear DATA_RDY flag when using Mode 3
			Set mode 3 for one shot measurement of all channels
			Wait for data to be ready
			Readings can now be accessed via getViolet(), getBlue(), etc

		Function AS726X_getVersion()
			Return version value

		Sub AS726X_TakeMeasurementsWithBulb()
			Turns on bulb, takes measurements, turns off bulb

		Function AS726X_GetTemperature()
			Returns the temperature in C
			Pretty inaccurate: +/-8.5C

		Sub AS726X_SetMeasurementMode( in AS726XMeasurementMode = 0b00..0b11 )
			Sets the measurement mode
			Mode 0: Continuous reading of VBGY (7262) / STUV (7263)
			Mode 1: Continuous reading of GYOR (7262) / RTUX (7263)
			Mode 2: Continuous reading of all channels (power-on default)
			Mode 3: One-shot reading of all channels

		Function AS726X_DataAvailable()
			Checks to see if DRDY flag is set in the control setup register

		Sub AS726X_ClearDataAvailable()
			Clears the DRDY flag
			Normally this should clear when data registers are read

		Sub AS726X_EnableIndicator()
			Enable the onboard indicator LED

		Sub AS726X_DisableIndicator()
			Disable the onboard indicator LED

		Sub AS726X_SetIndicatorCurrent( in AS726xCurrent = 0b00..0b11 )
			Set the current limit of onboard LED. Default is max 8mA = 0b11.

		Sub AS726X_EnableBulb()
			Enable the onboard 5700k or external incandescent bulb

		Sub AS726X_DisableBulb()
			Disable the onboard 5700k or external incandescent bulb

		Sub AS726X_SetBulbCurrent( in AS726xCurrent = 0b00..0b11 )
			Set the current limit of bulb/LED.
			Current 0: 12.5mA
			Current 1: 25mA
			Current 2: 50mA
			Current 3: 100mA

		Sub AS726X_SoftReset()
			Does a soft reset
			Delays sensor operations for 1000ms to reset

		Sub AS726X_SetIntegrationTime( in AS726XValue as Byte)
			Sets the integration value
			Give this function a from 0 to 255.
			Time will be 2.8ms * [AS726XValue]

		Sub AS726X_enableInterrupt()
		Sub AS726X_disableInterrupt()
		
		!Get the various color readings - the 16-bit value stored in a given channel register
		
		Function AS7262_getViolet()
		Function AS7262_getBlue()
		Function AS7262_getGreen()
		Function AS7262_getYellow()
		Function AS7262_getOrange()
		Function AS7262_getRed()

		!Get the various NIR readings - the 16-bit value stored in a given channel register
		Function AS7263_getR()
		Function AS7263_getS()
		Function AS7263_getT()
		Function AS7263_getU()
		Function AS7263_getV()
		Function AS7263_getW()

		!AS7263 specific - returns the various calibration data as float
		Function AS7262_getCalibratedViolet()
		Function AS7262_ggetCalibratedBlue()
		Function AS7262_ggetCalibratedGreen()
		Function AS7262_ggetCalibratedYellow()
		Function AS7262_ggetCalibratedOrange()
		Function AS7262_ggetCalibratedRed()

		! AS7263 specific - returns the various calibration data as float
		Function AS7263_getCalibratedR()
		Function AS7263_getCalibratedS()
		Function AS7263_getCalibratedT()
		Function AS7263_getCalibratedU()
		Function AS7263_getCalibratedV()
		Function AS7263_getCalibratedW()

		!Private methods
		Function AS726X_GetCalibrated ( AS726XCalAddress as Byte ) as Single
			Given an address, read four uint8_ts and return the floating point calibrated value


	*/
	
// Constants
	// Determine if user has set an alternative I2C address
	#script
		IF NODEF(AS726X_ADDRESS) THEN
			AS726X_ADDRESS = 0X92
		END IF
	#endscript

	#DEFINE SENSORTYPE_AS7262 		0X3E
	#DEFINE SENSORTYPE_AS7263 		0X3F

	//REGISTER ADDRESSES
	#DEFINE AS726X_DEVICE_TYPE 		0X00
	#DEFINE AS726X_HW_VERSION 		0X01
	#DEFINE AS726X_CONTROL_SETUP 	0X04
	#DEFINE AS726X_INT_T 			0X05
	#DEFINE AS726X_DEVICE_TEMP 		0X06
	#DEFINE AS726X_LED_CONTROL 		0X07
	#DEFINE AS72XX_SLAVE_STATUS_REG 0X00
	#DEFINE AS72XX_SLAVE_WRITE_REG 	0X01
	#DEFINE AS72XX_SLAVE_READ_REG 	0X02

	//THE SAME REGISTER LOCATIONS ARE SHARED BETWEEN THE AS7262 AND AS7263, THEY'RE JUST CALLED SOMETHING DIFFERENT

	//AS7262 REGISTERS
	#DEFINE AS7262_V 0X08
	#DEFINE AS7262_B 0X0A
	#DEFINE AS7262_G 0X0C
	#DEFINE AS7262_Y 0X0E
	#DEFINE AS7262_O 0X10
	#DEFINE AS7262_R 0X12
	#DEFINE AS7262_V_CAL 0X14
	#DEFINE AS7262_B_CAL 0X18
	#DEFINE AS7262_G_CAL 0X1C
	#DEFINE AS7262_Y_CAL 0X20
	#DEFINE AS7262_O_CAL 0X24
	#DEFINE AS7262_R_CAL 0X28

	//AS7263 REGISTERS
	#DEFINE AS7263_R 0X08
	#DEFINE AS7263_S 0X0A
	#DEFINE AS7263_T 0X0C
	#DEFINE AS7263_U 0X0E
	#DEFINE AS7263_V 0X10
	#DEFINE AS7263_W 0X12
	#DEFINE AS7263_R_CAL 0X14
	#DEFINE AS7263_S_CAL 0X18
	#DEFINE AS7263_T_CAL 0X1C
	#DEFINE AS7263_U_CAL 0X20
	#DEFINE AS7263_V_CAL 0X24
	#DEFINE AS7263_W_CAL 0X28

	#DEFINE AS72XX_SLAVE_TX_VALID 0X02
	#DEFINE AS72XX_SLAVE_RX_VALID 0X01
	#DEFINE SENSORTYPE_AS7262 0X3E
	#DEFINE SENSORTYPE_AS7263 0X3F
	#DEFINE POLLING_DELAY 5 ms //AMOUNT OF MS TO WAIT BETWEEN CHECKING FOR VIRTUAL REGISTER CHANGES

//*****************PROCESSED******************************************

#startup AS726X_Init, 95

Sub AS726X_Init ( optional AS726XGain as Byte = 3, optional AS726XMeasurementMode as Byte = 3 )	

	wait 255 ms             'added to ensure the charge pump and power is operational.
	#IFDEF HI2C_DATA

		Dir HI2C_DATA In
		Dir HI2C_CLOCK In
	
		HI2CMode Master
		Wait 15 ms  'wait for power-up and reset
	#ENDIF

	Dim AS726XSensorVersion, AS726XSensorState as Byte
	AS726XSensorVersion = 0
	AS726XSensorState = False

	AS726XSensorVersion = AS726x_VirtualReadRegister( AS726X_HW_VERSION )  

	IF ( AS726XSensorVersion <> SENSORTYPE_AS7262 & AS726XSensorVersion <> SENSORTYPE_AS7263 )  Then // HW VERSION FOR AS7262 AND AS7263	
		AS726XSensorState = False
		Exit Sub
	End If

	AS726X_SetBulbCurrent(0b00) //SET TO 12.5MA (MINIMUM)	
	AS726X_Disablebulb() //TURN OFF TO AHEATING THE SENSOR	
	AS726X_SetIndicatorCurrent(0b11) //SET TO 8MA (MAXIMUM)	
	AS726X_DisableIndicator() //TURN OFF LIGHTS TO SAVE POWER	
	AS726X_SetIntegrationTime(50) //50 * 2.8MS = 140MS. 0 TO 255 IS VALID.	
	AS726X_SetGain( AS726XGain ) //SET GAIN TO 64X	
	AS726X_SetMeasurementMode( AS726XMeasurementMode ) //ONE-SHOT READING OF VBGYOR	
	IF (AS726XSensorVersion = 0) Then	
		AS726XSensorState = False
		Exit Sub
	End If	
	AS726XSensorState = True	

	End Sub

Function AS726X_GetVersion() As Byte	
	
	AS726X_GetVersion = AS726XSensorVersion;	
	
	End Function	

Sub AS726X_SetMeasurementMode( AS726XMeasurementMode )	

	//Sets the measurement mode
	//Mode 0: Continuous reading of VBGY (7262) / STUV (7263)
	//Mode 1: Continuous reading of GYOR (7262) / RTUX (7263)
	//Mode 2: Continuous reading of all channels (power-on default)
	//Mode 3: One-shot reading of all channels

	Dim AS726XValue as Byte

	IF ( AS726XMeasurementMode > 0b11 ) Then AS726XMeasurementMode = 0b11
	AS726XValue = AS726x_VirtualReadRegister(AS726X_CONTROL_SETUP) //READ	
	AS726XValue = AS726XValue & 0b11110011 //CLEAR BANK BITS	

	AS726XValue.3 = AS726XMeasurementMode.1 //SET BANK BITS WITH USER'S CHOICE	
	AS726XValue.2 = AS726XMeasurementMode.0
	
	AS726x_VirtualWriteRegister(AS726X_CONTROL_SETUP, AS726XValue) //WRITE	

	End Sub	

Sub AS726X_SetGain( in AS726XGain)	

	// Sets the gain value
	// Gain 0: 1x (power-on default)
	// Gain 1: 3.7x
	// Gain 2: 16x
	// Gain 3: 64x

	Dim AS726XValue as Byte

	IF ( AS726XGain > 0b11) Then AS726XGain = 0b11	
	AS726XValue = AS726x_VirtualReadRegister(AS726X_CONTROL_SETUP) //READ	
	AS726XValue = AS726XValue & 0b11001111 //CLEAR GAIN BITS	
	
	AS726XValue.5 = AS726XValue.1 //SET GAIN BITS WITH USER'S CHOICE	
	AS726XValue.4 = AS726XValue.0 //SET GAIN BITS WITH USER'S CHOICE	
	
	AS726x_VirtualWriteRegister(AS726X_CONTROL_SETUP, AS726XValue) //WRITE	

	End Sub	

	// Integration time = <value> * 2.8ms
Sub AS726X_SetIntegrationTime( AS726XValue as Byte )	
	AS726x_VirtualWriteRegister(AS726X_INT_T, AS726XValue) //WRITE	
	End Sub	

Sub AS726X_EnableInterrupt()	

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_CONTROL_SETUP) //READ	
	AS726XValue.6 = 1 //SET Function BIT	
	AS726x_VirtualWriteRegister(AS726X_CONTROL_SETUP, AS726XValue) //WRITE	

	End Sub	

Sub AS726X_DisableInterrupt()	

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_CONTROL_SETUP) 
	AS726XValue.6 = 0 //CLEAR Function BIT	
	AS726x_VirtualWriteRegister(AS726X_CONTROL_SETUP, AS726XValue)

	End Sub	

Sub AS726X_TakeMeasurements()	
	
	// Tells IC to take measurements and polls for data ready flag
	// Clear DATA_RDY flag when using Mode 3
	// Set mode 3 for one shot measurement of all channels
	// Wait for data to be ready
	// Readings can now be accessed via getViolet(), getBlue(), etc

	AS726X_ClearDataAvailable() //CLEAR DATA_RDY FLAG WHEN USING MODE 3	
	AS726X_SetMeasurementMode(3)	
	Do WHILE ( AS726X_DataAvailable() = 0 ) 
		Wait POLLING_DELAY	
	Loop
	End Sub	

Sub AS726X_TakeMeasurementsWithBulb()	

	//Turns on bulb, takes measurements, turns off bulb

	AS726X_EnableBulb() //TURN ON BULB TO TAKE MEASUREMENT	
	AS726X_TakeMeasurements()	
	AS726X_DisableBulb() //TURN OFF BULB TO AHEATING SENSOR	
	End Sub	

	//Get the various color readings
Function AS726X_getViolet()
	return AS726x_GetChannel(AS7262_V)
	End Function

Function AS726X_getBlue() 
	return AS726x_GetChannel(AS7262_B)
	End Function

Function AS726X_getGreen()
	return AS726x_GetChannel(AS7262_G)
	End Function

Function AS726X_getYellow() 
	return AS726x_GetChannel(AS7262_Y)
	End Function

Function AS726X_getOrange() 
	return AS726x_GetChannel(AS7262_O)
	End Function

Function AS726X_getRed() 
	return AS726x_GetChannel(AS7262_R)
	End Function

	//Get the various NIR readings
Function AS726X_getR() 
	return AS726x_GetChannel(AS7263_R)
	End Function

Function AS726X_getS() 
	return AS726x_GetChannel(AS7263_S)
	End Function

Function AS726X_getT() 
	return AS726x_GetChannel(AS7263_T)
	End Function

Function AS726X_getU() 
	return AS726x_GetChannel(AS7263_U)
	End Function

Function AS726X_getV() 
	return AS726x_GetChannel(AS7263_V)
	End Function

Function AS726X_getW() 
	return AS726x_GetChannel(AS7263_W)
	End Function

	// Get a 16-bit value stored in a given channel register 
Function AS726X_getChannel( channelRegister ) as Word

	AS726X_getChannel = 0 
	AS726X_getChannel_H = AS726X_virtualReadRegister(channelRegister) //High uint8_t
	AS726X_getChannel = [Byte]AS726X_virtualReadRegister(channelRegister + 1) //Low uint8_t
	
	Return AS726X_getChannel

	End Function

Function AS7262_GetCalibratedViolet as Single

	Return AS726X_GetCalibrated(AS7262_V_CAL)
	
	End Function

Function AS7262_GetCalibratedBlue as Single

	Return AS726X_GetCalibrated(AS7262_B_CAL)
	
	End Function

Function AS7262_GetCalibratedGreen as Single

	Return AS726X_GetCalibrated(AS7262_G_CAL)
	
	End Function
	
Function AS7262_GetCalibratedYellow as Single

	Return AS726X_GetCalibrated(AS7262_Y_CAL)
	
	End Function

Function AS7262_GetCalibratedOrange as Single

	Return AS726X_GetCalibrated(AS7262_O_CAL)
	
	End Function

Function AS7262_GetCalibratedRed as Single

	Return AS726X_GetCalibrated(AS7262_R_CAL)
	
	End Function

Function AS7263_GetCalibratedR as Single

	Return AS726X_GetCalibrated(AS7263_R_CAL)
	
	End Function

Function AS7263_GetCalibratedS as Single

	Return AS726X_GetCalibrated(AS7263_S_CAL)
	
	End Function

Function AS7263_GetCalibratedT as Single

	Return AS726X_GetCalibrated(AS7263_T_CAL)
	
	End Function
	
Function AS7263_GetCalibratedU as Single

	Return AS726X_GetCalibrated(AS7263_U_CAL)
	
	End Function

Function AS7263_GetCalibratedV as Single

	Return AS726X_GetCalibrated(AS7263_V_CAL)
	
	End Function

Function AS7263_GetCalibratedW as Single

	Return AS726X_GetCalibrated(AS7263_W_CAL)
	
	End Function

Function AS726X_GetCalibrated ( AS726XCalAddress as Byte ) as Single	

	// Given an address, read four uint8_ts and return the floating point calibrated value

	AS726X_GetCalibrated_E = AS726x_VirtualReadRegister(AS726XCalAddress + 0)	
	AS726X_GetCalibrated_U = AS726x_VirtualReadRegister(AS726XCalAddress + 1)	
	AS726X_GetCalibrated_H = AS726x_VirtualReadRegister(AS726XCalAddress + 2)	
	[Byte]AS726X_GetCalibrated   = [Byte]AS726x_VirtualReadRegister(AS726XCalAddress + 3)	
	wait 25 ms
	End Function	


Function  AS726X_DataAvailable()	

	// Checks to see if DRDY flag is set in the control setup register

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_CONTROL_SETUP)	
	
	AS726X_DataAvailable = AS726XValue.1 //BIT 1 IS DATA_RDY	

	End Function	

Sub AS726X_ClearDataAvailable()	

	//Clears the DRDY flag
	//Normally this should clear when data registers are read

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_CONTROL_SETUP)	
	AS726XValue.1 = 1 //SET THE DATA_RDY BIT	
	AS726x_VirtualWriteRegister(AS726X_CONTROL_SETUP, AS726XValue)	

	End Sub	

Sub AS726X_EnableIndicator()	

	//Enable the onboard indicator LED

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_LED_CONTROL)
	AS726XValue.0 = 1
	AS726x_VirtualWriteRegister(AS726X_LED_CONTROL, AS726XValue)

	End Sub	


Sub AS726X_DisableIndicator()	

	// Disable the onboard indicator LED

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_LED_CONTROL)	
	AS726XValue.0 = 0 //CLEAR THE BIT	
	AS726x_VirtualWriteRegister(AS726X_LED_CONTROL, AS726XValue)	

	End Sub	

Sub AS726X_SetIndicatorCurrent( in AS726xCurrent )	

	Dim AS726XValue as Byte

	IF ( AS726xCurrent > 0b11)  Then AS726xCurrent = 0b11	

	AS726XValue = AS726x_VirtualReadRegister(AS726X_LED_CONTROL) //READ	
	//CLEAR ICL_IND BITS	
	//SET ICL_IND BITS WITH USER'S CHOICE	

	AS726XValue.1 = AS726xCurrent.1
	AS726XValue.2 = AS726xCurrent.0

	AS726x_VirtualWriteRegister(AS726X_LED_CONTROL, AS726XValue) //WRITE	

	End Sub	

Sub AS726X_EnableBulb()	

	// Enable the onboard 5700k or external incandescent bulb

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_LED_CONTROL)	
	AS726XValue.3 = 1 //SET THE BIT	
	AS726x_VirtualWriteRegister(AS726X_LED_CONTROL, AS726XValue)	

	End Sub	

Sub AS726X_DisableBulb()	
	
	// Disable the onboard 5700k or external incandescent bulb

	Dim AS726XValue as Byte

	AS726XValue = AS726x_VirtualReadRegister(AS726X_LED_CONTROL)	
	AS726XValue.3 = 0 //CLEAR THE BIT	
	AS726x_VirtualWriteRegister(AS726X_LED_CONTROL, AS726XValue)	
	
	End Sub	

Sub AS726X_SetBulbCurrent( AS726xCurrent )	
	
	// Set the current limit of bulb/LED.
	// Current 0: 12.5mA
	// Current 1: 25mA
	// Current 2: 50mA
	// Current 3: 100mA

	Dim AS726xValue as Byte

	IF ( AS726xCurrent > 0b11 ) Then AS726xCurrent = 0b11 //LIMIT TO TWO BITS	
	
	AS726xValue = AS726x_VirtualReadRegister(AS726X_LED_CONTROL) //READ	
	AS726xValue = AS726xValue & 0b11001111 //CLEAR ICL_DRV BITS	
	AS726xValue = AS726xValue | FnLSL ( AS726xCurrent, 4) //SET ICL_DRV BITS WITH USER'S CHOICE	
	AS726x_VirtualWriteRegister(AS726X_LED_CONTROL, AS726xValue) //WRITE	
	
	End Sub	

Function AS726X_GetTemperature() As Byte	

	// Returns the temperature in C
	// Pretty inaccurate: +/-8.5C

	Return (AS726x_VirtualReadRegister(AS726X_DEVICE_TEMP))	

	End Function	

Sub AS726X_SoftReset()	
	
	Dim AS726XValue as Byte
	
	AS726XValue = AS726x_VirtualReadRegister(AS726X_CONTROL_SETUP) //READ	
	AS726XValue.7 = 1  //SET RST BIT	
	AS726x_VirtualWriteRegister(AS726X_CONTROL_SETUP, AS726XValue) //WRITE	
	wait 1 s
	
	End Sub	

Function AS726x_VirtualReadRegister( In AS726xVirtualaddr As Byte) As Byte

	// Read a virtual register from the AS726x

	Dim AS726xError, AS726x_Status, AS726x_Incoming as Byte
	// There is data to be read
	AS726x_Status = AS726x_ReadRegister(AS72XX_SLAVE_STATUS_REG)

	If ((AS726x_Status & AS72XX_SLAVE_RX_VALID) <> 0) Then 				//THERE IS DATA TO BE READ	
		AS726x_Incoming = AS726x_ReadRegister(AS72XX_SLAVE_READ_REG) 	//READ BUT DO NOTHING WITH IT	
	End If

	AS726xError = 0
	Do	

		AS726x_Status = AS726x_ReadRegister(AS72XX_SLAVE_STATUS_REG)	

		IF ((AS726x_Status & AS72XX_SLAVE_TX_VALID) = 0) Then
			AS726xError = 0
			Exit Do // IF TX BIT IS CLEAR, IT IS OK TO WRITE	
		Else
			Wait POLLING_DELAY
			AS726xError++
		End If
	Loop While !AS726xError

	// Send the virtual register address (bit 7 should be 0 to indicate we are reading a register).
	AS726x_WriteRegister(AS72XX_SLAVE_WRITE_REG, AS726xVirtualaddr)

	//Wait for READ flag to be set
	AS726xError = 0
	Do
		
		AS726x_Status = AS726x_ReadRegister(AS72XX_SLAVE_STATUS_REG)
		IF ((AS726x_Status & AS72XX_SLAVE_RX_VALID) <> 0) Then
			AS726xError = 0
			Exit Do // READ DATA IS READY.	
		Else
			Wait POLLING_DELAY
			AS726xError++
		End If
	Loop While !AS726xError 

	AS726x_VirtualReadRegister = AS726x_ReadRegister(AS72XX_SLAVE_READ_REG)
	// Always returns AS726xError for examination
	End Function	

Sub AS726x_VirtualWriteRegister( AS726xVirtualaddr, DATATOWRITE)	

	// Write to a virtual register in the AS726x

	Dim AS726x_Status, AS726xError as Byte	

	AS726xError = 0
	Do	
		

		//Wait for WRITE register to be empty
		AS726x_Status = AS726x_ReadRegister(AS72XX_SLAVE_STATUS_REG)	
		IF ((AS726x_Status & AS72XX_SLAVE_TX_VALID) = 0) Then 
			Exit Do // NO INBOUND TX PENDING AT SLAVE. OKAY TO WRITE NOW.	
		Else
			AS726xError++
			Wait POLLING_DELAY	
		End If
	Loop While !AS726xError	

	AS726x_WriteRegister(AS72XX_SLAVE_WRITE_REG, (AS726xVirtualaddr | 0X80))	
	
	Do	
		
		AS726x_Status = AS726x_ReadRegister(AS72XX_SLAVE_STATUS_REG)	
		IF ((AS726x_Status & AS72XX_SLAVE_TX_VALID) = 0) Then // NO INBOUND TX PENDING AT SLAVE. OKAY TO WRITE NOW.	
			Exit Do
		Else
			AS726xError++
			Wait POLLING_DELAY	
		End If
	Loop While !AS726xError	

	AS726x_WriteRegister(AS72XX_SLAVE_WRITE_REG, DATATOWRITE)	
	// Always returns AS726xError for examination
	End Sub	

Function AS726x_ReadRegister( AS726XRegisterAddr ) As Byte	

	// Reads from a given location from the AS726x

	#ifdef HI2C_DATA

		do
			HI2CStart                            	// generate a start signal
			HI2CSend(AS726X_ADDRESS)               		// indicate a write				
		loop While HI2CAckPollState
		HI2CSend(AS726XRegisterAddr)                // send register address

		HI2CStart
		HI2CSend(AS726X_ADDRESS + 1)               	// set the read flag

		HI2CReceive(AS726x_ReadRegister, NACK)     	// read one byte and conclude
			// Hsersend "<"
			// HserPrFunction AS726x_ReadRegister
			// HserprFunction ">"
		HI2CStop
	#endif

	#ifdef I2C_DATA
		I2CStart                              		// generate a start signal
		I2CSend(AS726X_ADDRESS)                     // indicate a write
		I2CSend(AS726XRegisterAddr)					// send register address
		I2CReStart
		I2CSend(AS726X_ADDRESS + 1)                 // set the read flag
		I2CReceive(AS726x_ReadRegister, NACK)        // read one byte and conclude
		I2CStop
		I2CAckPoll(AS726X_ADDRESS)          		// wait for buffer write
	#endif
	
	End Function	

Sub AS726x_WriteRegister( in AS726XRegisterAddr, in AS726XVal)	

	// Write a value to a spot in the AS726x

	#ifdef HI2C_DATA
		do
			HI2CReStart                            	// generate a start signal
			HI2CSend(AS726X_ADDRESS)               		// indicate a write
		loop While HI2CAckPollState
		HI2CSend(AS726XRegisterAddr)                // send address as two bytes
		HI2CSend(AS726XVal)                         // then the value
		HI2CStop
	#endif

	#ifdef I2C_DATA

		I2CStart                                     // generate a start signal
		I2CSend(AS726X_ADDRESS)                      // indicate a write
		I2CSend(AS726XRegisterAddr)                  // send address as two bytes
		I2CSend(AS726XVal)                           // then the value
		I2CStop
		I2CAckPoll(AS726X_ADDR)
	#endif

	End Sub	
