'    Hardware SPI routines for GCBASIC
'    Copyright (C) 2006-2025 Hugh Considine and Evan R. Venn

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

'********************************************************************************
'IMPORTANT:
'THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
'UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
'COMMANDS UNUSABLE!
'********************************************************************************

'Changes:
' 3/2/2011: Some minor tweaks to reduce code size and make code neater
' 5/2/2011: Added AVR support, new version of SPIMode to set clock mode
' 14/6/2015: Adapted tp support software SPI by addressing SPITransfer via HWSPITransfer
' 25/3/2016: Adapted to add 16f18855 support
' 29/3/2016: Adapted to add 18f support in FastHWSPITransfer
' 19/10/2017: Added support SPI Specific Module, aka K42 chips
' 29/02/2019: Added MasterUltraFast option for AVR Meag328p
' 29/03/2019: Major update for the K42 SPI support
'              For the K42 type SPI MODULE ONLY - for hardware SPI only
'              Specify a SPI baud rate - the baud rate is dependent on the chip frequency!  If you get nothing, you have exceeded the SPI, so, slow the BAUD RATE or use the SPI_BAUD_RATE_REGISTER constant
'              #define SPI_BAUD_RATE 8000
'              or, use an Explicit setting show below
'              #define SPI_BAUD_RATE_REGISTER 3
'
'              Scripts added to calculate the baudrate
'              Update the SPIMode to using 1/4, 1/16 and 1/64 of clock speed when using SPIMode if Baud rate not set
'  13/10/2019: Added MasterSSPADDMode support.
'              #define HWSPIMode MasterSSPADDMode
'              It will set the two bits that are needed and then set SSP1ADD =1.   This way all the SPI libraries can use the feature.
'              And, if the user does not add the `HWSPIMode` constant it will work as before.
'  12/10/2020  Add basic ChipFamily 122 support
'  02/11/2020  Add further ChipFamily 122 support by adding the constant SPIDELAY_SCRIPT_MASTER.  This is adds clock cycles to permit the data (a byte to exit the buffer before the CS line is changed.
'              Added HWSPI_Fast_Write_Word_Macro where data needs to passed in the HWSPI_Send_word word variable
' 14/08/22 Updated user changeable constants only - no functional change
' 26/09/24 Added AVRDx support
' 28/09/24 Added HWSPI2 support for PIC
' 02/12/24 Tidied SPI1CON0_EN usage
' 15/02/25 Revert undocumented SPIClockMode.0 = On change.  This must be 'Off' 
' 11/02/25 Revise SPIMODE to support bit2
' 12/03/25 Revised to support MASTERULTRAFAST mode for K series.
' 14/03/25 Revised to support HWSPI2 mode for K series.
' 22/04/25 Add MEGA328PB SPI support

'To make the PIC pause until it receives an SPI message while in slave mode, set the
'constant "WaitForSPI" at the start of the program. The value does not matter.

'Subs
' SPIMode(Mode)     Set the SPI mode. See the list of constants below
' SPITransfer(Tx, Rx)   Simultaneously send and receive an SPI byte

'SPI mode constants
'Also used for hardware I2C

#define MasterSSPADDMode 16
#define MasterUltraFast 14
#define MasterFast 13
#define Master 12
#define MasterSlow 11
#define SlaveSS 1
#define Slave 0

'SPI clock options
'For SPIMode (mode, clock)
#define SPI_CPOL_0 0
#define SPI_CPOL_1 2
#define SPI_CPHA_0 0
#define SPI_CPHA_1 1
#define SPI_SS_0   0    //  to support clearing of SPI1CON1.SSP
#define SPI_SS_1   4    //  to support setting of SPI1CON1.SSP


'SPI Module specific clock options
#define SSP1_Clock_SMT_match     8
#define SSP1_TMR6_Postscaled     7
#define SSP1_TMR4_Postscaled     6
#define SSP1_TMR2_Postscaled     5
#define SSP1_TMR0_overflow       4
#define SSP1_CLKREF              3
#define SSP1_MFINTOSC            2
#define SSP1_HFINTOSC            1
#define SSP1_FOSC                0

'K42 or the SPI module specific constants
#define SPI_RX_IN_PROGRESS       0x00

#script

  // HWSPI - addressess HWSP1
    'Set the SPI Clock Mode if the user has not set it.....
    HWSPIClockModeSCRIPT = 0
    if DEF(HWSPIClockMode) then
        HWSPIClockModeSCRIPT = HWSPIClockMode
    end if

    'Set the SPI Mode if the user had not set it... used in the SPI libraries
    userspecifiedHWSPIMode = 0
    if HWSPIMode then
        HWSPIMODESCRIPT = HWSPIMode
        userspecifiedHWSPIMode = 1
    end if

    if userspecifiedHWSPIMode = 0 then
        HWSPIMODESCRIPT = MasterFast
        'If the ChipMHz > 32 then user Master NOT MasterFast
        if ChipMHz > 32 then
            HWSPIMODESCRIPT = Master
        end if
        userspecifiedHWSPIMode = 1
    end if

    'create a constant, as this IS needed, if the user have not defined
    USERHASDEFINETHESPI_BAUD_RATE = 0
    IF SPI_BAUD_RATE THEN
        USERHASDEFINETHESPI_BAUD_RATE = 1
    END IF
    IF USERHASDEFINETHESPI_BAUD_RATE = 0 THEN
        SPI_BAUD_RATE = INT(ChipMhz/4)*1000
    END IF

    'Calculate SPI Baud Rate for SPImode modules on K42 per datasheet
    SPIBAUDRATE_SCRIPT = INT( ChipMHz / INT( SPI_BAUD_RATE  ) / 2 * 1000) + 1

    'Calculate SPI Baud Rate for SPImode modules on K42 per datasheet
    SPIBAUDRATE_SCRIPT_MASTER = INT( ChipMHz / INT( SPI_BAUD_RATE /4  ) / 2 * 1000) + 1

    'Calculate SPI Baud Rate for SPImode modules on K42 per datasheet
    SPIBAUDRATE_SCRIPT_MASTERSLOW = INT(ChipMHz / INT( SPI_BAUD_RATE / 16  ) / 2 * 1000) + 1


  // HWSP2 - addressess HWSP2
    'Set the SPI Clock Mode if the user has not set it.....
    HWSPI2ClockModeSCRIPT = 0
    if DEF(HWSPI2ClockMode) then
        HWSPI2ClockModeSCRIPT = HWSPI2ClockMode
    end if

    'Set the SPI Mode if the user had not set it... used in the SPI libraries
    userspecifiedHWSPI2Mode = 0
    if HWSPI2Mode then
        HWSPI2MODESCRIPT = HWSPI2Mode
        userspecifiedHWSPI2Mode = 1
    end if

    if userspecifiedHWSPI2Mode = 0 then
        HWSPI2MODESCRIPT = MasterFast
        'If the ChipMHz > 32 then user Master NOT MasterFast
        if ChipMHz > 32 then
            HWSPI2MODESCRIPT = Master
        end if
        userspecifiedHWSPIMode = 1
    end if

    'create a constant, as this IS needed, if the user have not defined
    USERHASDEFINETHESPI2_BAUD_RATE = 0
    IF SPI2_BAUD_RATE THEN
        USERHASDEFINETHESPI2_BAUD_RATE = 1
    END IF
    IF USERHASDEFINETHESPI2_BAUD_RATE = 0 THEN
        SPI2_BAUD_RATE = INT(ChipMhz/4)*1000
    END IF

    'Calculate SPI Baud Rate for SPI2mode modules on K42 per datasheet
    SPI2BAUDRATE_SCRIPT = INT( ChipMHz / INT( SPI2_BAUD_RATE  ) / 2 * 1000) + 1

    'Calculate SPI Baud Rate for SPImode modules on K42 per datasheet
    SPI2BAUDRATE_SCRIPT_MASTER = INT( ChipMHz / INT( SPI2_BAUD_RATE /4  ) / 2 * 1000) + 1

    'Calculate SPI Baud Rate for SPImode modules on K42 per datasheet
    SPI2BAUDRATE_SCRIPT_MASTERSLOW = INT(ChipMHz / INT( SPI2_BAUD_RATE / 16  ) / 2 * 1000) + 1

    // End of SPI and SPI2 management

    'calculate a delay if needed
    SPI1DELAY_SCRIPT = 1
    if ChipMhz > 0 then
        SPI1DELAY_SCRIPT = 30
    end if

    if ChipMhz > 2 then
        SPI1DELAY_SCRIPT = 20
    end if

    if ChipMhz > 4 then
        SPI1DELAY_SCRIPT = 10
    end if

    if ChipMhz > 8 then
        SPI1DELAY_SCRIPT = 8
    end if

    if ChipMhz > 16 then
        SPI1DELAY_SCRIPT = 4
    end if

    if ChipMhz > 32 then
        SPI1DELAY_SCRIPT = 2
    end if

    if ChipMhz > 48 then
        SPI1DELAY_SCRIPT = 1
    end if

    If ChipFamily = 122 then

      'There are hard coded clock cycles to permit the data (a byte to exit the buffer before the CS line is changed.
      If HWSPIMode = MasterSlow then
          SPIDELAY_SCRIPT_MASTER = 64
      End if
      If HWSPIMode = Master  then
          SPIDELAY_SCRIPT_MASTER = 16
      End if
      If HWSPIMode = MasterFast then
          SPIDELAY_SCRIPT_MASTER = 1
      End if
      If HWSPIMode = MasterUltraFast then
          SPIDELAY_SCRIPT_MASTER = 0
      End if

    End if

#endscript

#samebit SSPM0 SSPCON_SSP1M0
#samebit SSPM1 SSPCON_SSP1M1
#samebit SSPM2 SSPCON_SSP1M2
#samebit SSPM3 SSPCON_SSP1M3

'Initialisation sub
'Used to set mode
'Overloaded: Other version also allows clock options to be set
Sub SPIMode (In SPICurrentMode)

  Dim SPI1BAUD as Byte
  
  #ifdef PIC
    #ifndef Var(SSPCON1)
      #ifdef Var(SSPCON)
        Dim SSPCON1 Alias SSPCON
      #endif
    #endif


    #ifdef Var(SSPCON1)
        'Supports Legacy SPI via MSSP module

        'Turn off SPI
        '(Prevents any weird glitches during setup)
        Set SSPCON1.SSPEN Off

        'Set clock pulse settings
        Set SSPSTAT.SMP Off
        'Data write on rising (idle > active) clock (CPHA = 1)
        Set SSPSTAT.CKE Off
        'Clock idle low (CPOL = 0)
        Set SSPCON1.CKP Off

        'Select mode and clock
        'Set some mode bits off, can set on later
        Set SSPCON1.SSPM3 Off
        Set SSPCON1.SSPM2 Off
        Set SSPCON1.SSPM1 Off
        Set SSPCON1.SSPM0 Off

        Select Case SPICurrentMode
        Case MasterSSPADDMode   '1010
          Set SSPCON1.SSPM3 On
          Set SSPCON1.SSPM1 On
          #ifdef Var(SSP1ADD)
              SSP1ADD = 1
          #endif
        Case MasterUltraFast
          'Nothing to do on PIC
        Case MasterFast   '0000
          'Nothing to do
        Case Master    '0001
          Set SSPCON1.SSPM0 On
        Case MasterSlow   '0010
          Set SSPCON1.SSPM1 On
        Case Slave
          Set SSPCON1.SSPM2 On
          Set SSPCON1.SSPM0 On
        Case SlaveSS
          Set SSPCON1.SSPM2 On
        End Select

        'Enable SPI
        Set SSPCON1.SSPEN On

    #endif



    #ifdef Var(SPI1CON0)
        
         'Supports K mode SPI using the specific SPI module

        'Turn off SPI
        '(Prevents any weird glitches during setup)
        #if BIT(SPI1CON0_EN)
        SPI1CON0_EN = 0
        #else
          SPI1CON0.EN = 0
        #endif


        'Set clock pulse settings
        SPI1CON1.SMP = 0
        'Data write on rising (idle > active) clock (CPHA = 1)
        SPI1CON1.CKE = 0
        'Clock idle low (CPOL = 0)
        SPI1CON1.CKP = 0

        SPI1CON1 = 0x40

        'Transfer
        SPI1CON2 = SPI1CON2 or 3

        #ifdef SPI1_DC
          dir SPI1_DC       out
        #endif

        #ifdef SPI1_CS
          dir SPI1_CS       out
        #endif

        #ifdef SPI1_RESET
          dir SPI1_RESET    out
        #endif

        #ifdef SPI1_DI
          dir SPI1_DI       in
        #endif

        #ifdef SPI1_DO
          dir SPI1_DO       out
        #endif

        #ifdef SPI1_SCK
          dir SPI1_SCK      out
        #endif

        'Select mode and clock
        SPI1CLK = SSP1_FOSC

        Select Case SPICurrentMode
          Case MasterFast or MasterUltraFast
            asm showdebug Script value is calculated as SPIBAUDRATE_SCRIPT
            SPI1BAUD = SPIBAUDRATE_SCRIPT
            SPI1CON0.MST = 1
          Case Master
            asm showdebug Script value is calculated as SPIBAUDRATE_SCRIPT_MASTER
            SPI1BAUD = SPIBAUDRATE_SCRIPT_MASTER
            SPI1CON0.MST = 1
          Case MasterSlow
            asm showdebug Script value is calculated as SPIBAUDRATE_SCRIPT_MASTERSLOW
            SPI1BAUD = SPIBAUDRATE_SCRIPT_MASTERSLOW
            SPI1CON0.MST = 1
          Case Slave

            SPI1CON0.MST = 0
            #ifdef SPI1_SCK
              dir SPI1_SCK      in
            #endif
          Case SlaveSS

            SPI1CON0.MST = 0
            #ifdef SPI1_SCK
              dir SPI1_SCK      in
            #endif
        End Select


        #ifdef SPI_BAUD_RATE_REGISTER
          // override the script calculation
          SPI1BAUD = SPI_BAUD_RATE_REGISTER
        #endif

        #if BIT(SPI1CON0_EN)
          SPI1CON0_EN = 0
        #else
          SPI1CON0.EN = 0
    #endif

  #endif

  #endif

  #ifdef AVR
    #if nodef(CHIPAVRDX)
      'Turn off SPI
      '(Prevents any weird glitches during setup)
      Set SPCR.SPE Off

      'Set clock pulse settings
      'Need CPHA = 1 to match default PIC settings
      '(CPOL = 0 matches PIC)
      Set SPCR.CPHA On

      'Select mode and clock
      'Set some mode bits off, can set on later
      Set SPCR.MSTR Off
      Set SPSR.SPI2X Off
      Set SPCR.SPR0 Off
      Set SPCR.SPR1 Off

      Select Case SPICurrentMode
      Case MasterUltraFast
        Set SPCR.MSTR On
        Set SPSR.SPI2X On

      Case MasterFast
        Set SPCR.MSTR On

      Case Master
        Set SPCR.MSTR On
        Set SPCR.SPR0 On

      Case MasterSlow
        Set SPCR.MSTR On
        Set SPCR.SPR1 On

      'Nothing needed for slave
      'Case Slave
      'Case SlaveSS

      End Select

      'Enable SPI
      Set SPCR.SPE On
    #endif

    #if def(CHIPAVRDX)

      'Turn off SPI
      '(Prevents any weird glitches during setup)
      // Disable module
      SPI0_CTRLA.SPI_ENABLE_bp = 0          

      // Data mode
      SPI0_CTRLB = 0 /* Data Mode */

      // LSB is transmitted first; SPI module in Master mode; System Clock divided by 16
      Select Case SPICurrentMode
        Case MasterUltraFast
            SPI0_CTRLA = SPI_CLK2X_bm | SPI_DORD_bm | SPI_MASTER_bm 
        Case MasterFast
            SPI0_CTRLA =  SPI_DORD_bm | SPI_MASTER_bm | SPI_PRESC_0_bm
        Case Master
            SPI0_CTRLA = SPI_DORD_bm | SPI_MASTER_bm | SPI_PRESC_1_bm
        Case MasterSlow
            SPI0_CTRLA = SPI_DORD_bm | SPI_MASTER_bm | SPI_PRESC_1_bm | SPI_PRESC_0_bm
        'Nothing needed for slave
        'Case Slave
        'Case SlaveSS
      End Select

      'Enable SPI
      SPI0_CTRLA.SPI_ENABLE_bp = 1          // Enable module

    #endif
  #endif


End Sub

'Initialisation sub
'Used to set mode and clock options
'Overloaded: Other version is more compact but doesn't allow clock options to be set
Sub SPIMode (In SPICurrentMode, In SPIClockMode)

  #ifdef PIC
    #ifndef Var(SSPCON1)
      #ifdef Var(SSPCON)
        Dim SSPCON1 Alias SSPCON
      #endif
    #endif


    #ifdef Var(SSPCON1)
        'Supports Legacy SPI via MSSP module

        #ifndef Var(SSPCON1)
          #ifdef Var(SSP1CON1)
            Dim SSPCON1 Alias SSP1CON1 ;added for 18f18855
          #endif
        #endif

        #ifndef Var(SSPSTAT)
          #ifdef Var(SSP1STAT)
            Dim SSPSTAT Alias SSP1STAT ;added for 18f18855
          #endif
        #endif

        #ifndef Var(SSPBUF)
          #ifdef Var(SSP1BUF)
            Dim SSPBUF Alias SSP1BUF
          #endif
        #endif

        'Turn off SPI
        '(Prevents any weird glitches during setup)
        Set SSPCON1.SSPEN Off

        'Set clock pulse settings
        Set SSPSTAT.SMP Off
        Set SSPSTAT.CKE Off

        If SPIClockMode.0 = Off Then
          Set SSPSTAT.CKE On
        End If
        Set SSPCON1.CKP Off
        If SPIClockMode.1 = On Then
          Set SSPCON1.CKP On
        End If

        'Select mode and clock
        'Set some mode bits off, can set on later
        Set SSPCON1.SSPM3 Off
        Set SSPCON1.SSPM2 Off
        Set SSPCON1.SSPM1 Off
        Set SSPCON1.SSPM0 Off

        Select Case SPICurrentMode
        Case MasterSSPADDMode   '1010
          Set SSPCON1.SSPM3 On
          Set SSPCON1.SSPM1 On
          #ifdef Var(SSP1ADD)
              SSP1ADD = 1
          #endif
        Case MasterUltraFast
          'Nothing to do on PIC
        Case MasterFast
          'Nothing to do
        Case Master
          Set SSPCON1.SSPM0 On
        Case MasterSlow
          Set SSPCON1.SSPM1 On
        Case Slave
          Set SSPCON1.SSPM2 On
          Set SSPCON1.SSPM0 On
        Case SlaveSS
          Set SSPCON1.SSPM2 On
        End Select

        'Enable SPI
        Set SSPCON1.SSPEN On
    #endif


    #ifdef Var(SPI1CON0)

        // Supports newer SPI module using the specific SPI module

        //~Turn off SPI
        //~(Prevents any weird glitches during setup)
        #if BIT(SPI1CON0_EN)
        SPI1CON0_EN = 0
        #else
          SPI1CON0.EN = 0
        #endif

        //~Clear register
        //~ Data write on rising (idle > active) clock (CPHA = 1)
        //~ Clock idle low (CPOL = 0)
        //~ Set clock pulse settings to middle
        //~ SPI1CON1.SMP = 0
        //~  SPI1CON1.CKE = 0
        //~  SPI1CON1.CKP = 0
        SPI1CON1 = 0x00

        If SPIClockMode.0 = Off Then
          SPI1CON1.CKE = 1
        End If

        If SPIClockMode.1 = On Then
          SPI1CON1.CKP = 1
        End If

        #IF BIT(SSP)
          If SPIClockMode.2 = On Then
            SPI1CON1.SSP = 1
          End If
        #ENDIF

        'Transfer
        SPI1CON2 = SPI1CON2 or 3

        //~Set the ports as required
        #ifdef SPI1_DC
          dir SPI1_DC       out
        #endif
        #ifdef SPI_DC
          dir SPI_DC       out
        #endif

        #ifdef SPI1_CS
          dir SPI1_CS       out
        #endif
        #ifdef SPI_CS
          dir SPI_CS       out
        #endif


        #ifdef SPI1_RESET
          dir SPI1_RESET    out
        #endif
        #ifdef SPI_RESET
          dir SPI_RESET    out
        #endif

        #ifdef SPI1_DI
          dir SPI1_DI       in
        #endif
        #ifdef SPI_DI
          dir SPI_DI       in
        #endif

        #ifdef SPI1_DO
          dir SPI1_DO       out
        #endif
        #ifdef SPI_DO
          dir SPI_DO       out
        #endif

        #ifdef SPI1_SCK
          dir SPI1_SCK      out
        #endif
        #ifdef SPI_SCK
          dir SPI_SCK      out
        #endif

        // Select clock source
        SPI1CLK = SSP1_FOSC

        // Supports newer SPI module using the specific SPI module
        Select Case SPICurrentMode

            Case MasterUltraFast
              // assumed that 0 is fastest
              SPI1BAUD = 0
              SPI1CON0.MST = 1
            Case MasterFast
              // see CDF file for SPIBAUDRATE_SCRIPT
              SPI1BAUD = SPIBAUDRATE_SCRIPT
              SPI1CON0.MST = 1
            Case Master
              // see CDF file for SPIBAUDRATE_SCRIPT_MASTER
              SPI1BAUD = SPIBAUDRATE_SCRIPT_MASTER
              SPI1CON0.MST = 1
            Case MasterSlow
              // see CDF file for SPIBAUDRATE_SCRIPT_MASTERSLOW
              SPI1BAUD = SPIBAUDRATE_SCRIPT_MASTERSLOW
              SPI1CON0.MST = 1
            Case Slave
              SPI1CON0.MST = 0
              #ifdef SPI1_SCK
                dir SPI1_SCK      in
              #endif

            Case SlaveSS
              SPI1CON0.MST = 0
              #ifdef SPI1_SCK
                dir SPI1_SCK      in
              #endif

        End Select

        #ifdef SPI_BAUD_RATE_REGISTER
        'override the script calculation
          SPI1BAUD = SPI_BAUD_RATE_REGISTER
        #endif


        'Enable SPI
        #if BIT(SPI1CON0_EN)
          SPI1CON0_EN = 1
        #else
        SPI1CON0.EN = 1
        #endif

    #endif
  #endif

  #ifdef AVR
    #if nodef(CHIPAVRDX)
      #if NoVar(SPCR0)
        // Legacy AVR Excluding MEGA3208PB
        'Turn off SPI
        '(Prevents any weird glitches during setup)
        Set SPCR.SPE Off

        'Set clock pulse settings
        Set SPCR.CPHA Off
        If SPIClockMode.0 = On Then
          Set SPCR.CPHA On
        End If
        Set SPCR.CPOL Off
        If SPIClockMode.1 = On Then
          Set SPCR.CPOL On
        End If

        'Select mode and clock
        'Set some mode bits off, can set on later
        Set SPCR.MSTR Off
        Set SPSR.SPI2X Off
        Set SPCR.SPR0 Off
        Set SPCR.SPR1 Off

        Select Case SPICurrentMode
        Case MasterUltraFast
          Set SPCR.MSTR On
          Set SPSR.SPI2X On

        Case MasterFast
          Set SPCR.MSTR On

        Case Master
          Set SPCR.MSTR On
          Set SPCR.SPR0 On

        Case MasterSlow
          Set SPCR.MSTR On
          Set SPCR.SPR1 On

        'Nothing needed for slave
        'Case Slave
        'Case SlaveSS

        End Select

        'Enable SPI
        Set SPCR.SPE On
      #else

        // Legacy AVR including MEGA328PB

        'Turn off SPI
        '(Prevents any weird glitches during setup)
        Set SPCR0.SPE0 Off

        'Set clock pulse settings
        Set SPCR0.CPHA0 Off
        If SPIClockMode.0 = On Then
          Set SPCR0.CPHA0 On
        End If
        Set SPCR0.CPOL0 Off
        If SPIClockMode.1 = On Then
          Set SPCR0.CPOL0 On
        End If

        'Select mode and clock
        'Set some mode bits off, can set on later
        Set SPCR0.MSTR0 Off
        Set SPSR0.SPI2X0 Off
        Set SPCR0.SPR00 Off
        Set SPCR0.SPR01 Off

        Select Case SPICurrentMode
        Case MasterUltraFast
          Set SPCR0.MSTR0 On
          Set SPSR0.SPI2X0 On

        Case MasterFast
          'No clock divider bits set, fosc/4
          Set SPCR0.MSTR0 On

        Case Master
          'fosc/16
          Set SPCR0.MSTR0 On
          Set SPCR0.SPR00 On

        Case MasterSlow
          Set SPCR0.MSTR0 On
          Set SPCR0.SPR01 On
          Set SPCR0.SPR00 On  'Added to set the slowest speed (fosc/128)

        Case Slave
          'Keep MSTR0 Off for slave mode

        Case SlaveSS
          'Keep MSTR0 Off for slave mode
          Set SPCR0.SPIE0 On  'Enable SPI interrupt for SS mode

        End Select

        'Enable SPI
        Set SPCR0.SPE0 On

      #endif

    #endif
    #if def(CHIPAVRDX)

      'Turn off SPI
      '(Prevents any weird glitches during setup)
      // Disable module
      SPI0_CTRLA.SPI_ENABLE_bp = 0          

      // Data mode
      SPI0_CTRLB = SPI0_CTRLB | SPIClockMode; /* Data Mode */

      // LSB is transmitted first; SPI module in Master mode; System Clock divided by 16
      Select Case SPICurrentMode
        Case MasterUltraFast
            SPI0_CTRLA = SPI_CLK2X_bm | SPI_DORD_bm | SPI_MASTER_bm 
        Case MasterFast
            SPI0_CTRLA =  SPI_DORD_bm | SPI_MASTER_bm | SPI_PRESC_0_bm
        Case Master
            SPI0_CTRLA = SPI_DORD_bm | SPI_MASTER_bm | SPI_PRESC_1_bm
        Case MasterSlow
            SPI0_CTRLA = SPI_DORD_bm | SPI_MASTER_bm | SPI_PRESC_1_bm | SPI_PRESC_0_bm
        'Nothing needed for slave
        'Case Slave
        'Case SlaveSS
      End Select

      'Enable SPI
      SPI0_CTRLA.SPI_ENABLE_bp = 1          // Enable module

    #endif

  #endif

End Sub

'Turn off SSP
Sub SSPOFF
  #samevar SSPEN, SPI1SPIEN
  #ifdef PIC
    Set SSPEN Off
  #endif

  #ifdef AVR
    #if nodef(CHIPAVRDX)
      Set SPE Off
    #endif
    #if def(CHIPAVRDX)
      // Disable module
      SPI0_CTRLA.SPI_ENABLE_bp = 0  
    #endif
  #endif

End Sub
#undefine SPITransfer
#define SPITransfer HWSPITransfer
'Sub to handle SPI data send/receive
Sub HWSPITransfer(In SPITxData, Out SPIRxData)

  #ifdef PIC

    #ifdef Var(SSPBUF)
        'Master mode
        If SPICurrentMode > 10 Then
          'Clear WCOL
          Set SSPCON1.WCOL Off

          'Put byte to send into buffer
          'Will start transfer
          SSPBUF = SPITxData

        'Slave mode
        Else
          'Retry until send succeeds
          Do
            SET SSPCON1.WCOL OFF
            SSPBUF = SPITxData
          Loop While SSPCON1.WCOL = On
        End If

        'Read buffer
        'Same for master and slave
        Wait While SSPSTAT.BF = Off
        SPIRxData = SSPBUF
        Set SSPSTAT.BF Off
    #endif

    #ifdef Var(SPI1CON0)

      'One byte transfer count
      SPI1TCNTL = 1
      SPI1TXB = SPITxData

      wait while SPI1RXIF = SPI_RX_IN_PROGRESS
      SPIRxData = SPI1RXB

    #endif

  #endif

  #ifdef AVR
    #if nodef(CHIPAVRDX)
      'Master mode
      If SPICurrentMode > 10 Then
        'Put byte to send into buffer, Will start transfer
        #IF ChipFamily <> 122
          #IF VAR(SPDR)
            Do
              SPDR = SPITxData
            Loop While SPSR.WCOL
          #ENDIF
          #IF VAR(SPDR0)
              SPDR0 = SPITxData
              Do 
                  // Wait for flag
              Loop While (SPSR0.SPIF0 = 0)
          #ENDIF  
        #ENDIF
        #IF ChipFamily = 122
          SPFR = SPFR & 0x44 'setup  RDEMPT and WREMPT simutanously to clear buffer
          SPDR = SPITxData
          Repeat SPIDELAY_SCRIPT_MASTER
            nop
          End Repeat
        #ENDIF

      Else
        'Slave mode
        'Retry until send succeeds
        #IF VAR(SPDR)
          Do
            SPDR = SPITxData
          Loop While SPSR.WCOL
        #ENDIF
        #IF VAR(SPDR0)
          Do While SPSR0.WCOL0
            SPDR0 = SPITxData
          Loop
        #ENDIF
      End If

      'Read buffer, same for master and slave
      #IF ChipFamily <> 122
          #IF VAR(SPDR)
            Wait While SPSR.SPIF = Off
          #ENDIF
          #IF VAR(SPDR0)
            Wait While SPSR0.SPIF0 = Off
          #ENDIF
      #ENDIF
      #IF ChipFamily = 122
        'Chipfmaily 122 has different registers.... and, is so fast...
        Wait While ( SPSR.SPIF = Off and RDEMPT = 0 )
      #ENDIF
      #IF VAR(SPDR)
        SPIRxData = SPDR
      #ENDIF
      #IF VAR(SPDR0)
        SPIRxData = SPDR0
      #ENDIF
    #endif

    #if def(CHIPAVRDX)
      SPI0_DATA = SPITxData
      // waits until data is exchanged
      wait while SPI0_INTFLAGS.SPI_IF_bp = 0
      SPIRxData = SPI0_DATA
    #endif    
  #endif

End Sub


'Sub to handle SPI data send - Master mode only
Sub FastHWSPITransfer( In SPITxData )
  'Master mode only
  #ifdef PIC

    #ifdef Var(SSPBUF)
        'Clear WCOL
        Set SSPCON1.WCOL Off

        'Put byte to send into buffer
        'Will start transfer
        SSPBUF = SPITxData
        'Same for master and slave
        Wait While SSPSTAT.BF = Off
        Set SSPSTAT.BF Off


        #if ChipFamily = 16
           'Handle 18F chips
          SPIRxData = SSPBUF
        #endif

    #endif

    #ifdef Var(SPI1CON0)

      'One byte transfer count
      SPI1TCNTL = 1
      SPI1TXB = SPITxData

      wait while SPI1RXIF = SPI_RX_IN_PROGRESS
      SPIRxData = SPI1RXB

    #endif

  #endif

  #ifdef AVR
    #if nodef(CHIPAVRDX)
      'Master mode only
      #IF ChipFamily <> 122
        Do
          SPDR = SPITxData
        Loop While SPSR.WCOL
      #ENDIF
      #IF ChipFamily = 122
        SPFR = SPFR & 0x44 'setup  RDEMPT and WREMPT simutanously to clear buffer
        SPDR = SPITxData
        Repeat SPIDELAY_SCRIPT_MASTER
          nop
        End Repeat
      #ENDIF

      'Same for master and slave
      #IF ChipFamily <> 122
        Wait While SPSR.SPIF = Off
      #ENDIF

      #IF ChipFamily = 122
        'Chipfmaily 122 has different registers.... and, is so fast...
        Wait While ( SPSR.SPIF = Off and RDEMPT = 0 )
      #ENDIF
    #endif
    #if def(CHIPAVRDX)
      SPI0_DATA = SPITxData
      // waits until data is exchanged
      wait while SPI0_INTFLAGS.SPI_IF_bp = 0
    #endif
  #endif

End Sub


'requires word data to be in HWSPI_Send_Word as word
Macro HWSPI_Fast_Write_Word_Macro

  Dim HWSPI_Send_Word as word

            #IF ChipFamily <> 122
                Do
                  SPDR = HWSPI_Send_word_h
                Loop While SPSR.WCOL
                'Read buffer
                'Same for master and slave
                Wait While SPSR.SPIF = Off

                Do
                  SPDR = HWSPI_Send_word
                Loop While SPSR.WCOL
                'Read buffer
                'Same for master and slave
                Wait While SPSR.SPIF = Off

            #ENDIF

              #IF ChipFamily = 122

                SPFR = SPFR & 0x44 'setup  RDEMPT and WREMPT simutanously to clear buffer
                SPDR = HWSPI_Send_word_h
                Repeat SPIDELAY_SCRIPT_MASTER
                nop
                End Repeat
                Wait While ( SPSR.SPIF = Off and RDEMPT = 0 )

                SPDR = HWSPI_Send_word
                Repeat SPIDELAY_SCRIPT_MASTER
                nop
                End Repeat
                Wait While ( SPSR.SPIF = Off and RDEMPT = 0 )

            #ENDIF

End Macro


Dim HWSPI_Send_18bits as Long

'requires word data to be in HWSPI_Send_18bits as Long but actually only sends _18bits
Macro HWSPI_Fast_Write_18bits_Macro

  #ifdef Var(SPI1CON0)

    'One byte transfer count
    SPI1TCNTL = 1
    SPI1TXB = HWSPI_Send_18bits
    wait while SPI1RXIF = SPI_RX_IN_PROGRESS
    SPIRxData = SPI1RXB

    SPI1TCNTL = 1
    SPI1TXB = HWSPI_Send_18bits_H
    wait while SPI1RXIF = SPI_RX_IN_PROGRESS
    SPIRxData = SPI1RXB

    SPI1TCNTL = 1
    SPI1TXB = HWSPI_Send_18bits_U
    wait while SPI1RXIF = SPI_RX_IN_PROGRESS
    SPIRxData = SPI1RXB

  #endif

  #IF VAR(SPDR0)
    SPDR0 = HWSPI_Send_18bits_U
    Do 
        // Wait for flag
    Loop While (SPSR0.SPIF0 = 0)
 
    SPIRxData = SPDR0
    SPDR0 = HWSPI_Send_18bits_H
    Do 
        // Wait for flag
    Loop While (SPSR0.SPIF0 = 0)
 
    SPIRxData = SPDR0
    SPDR0 = HWSPI_Send_18bits
    Do 
        // Wait for flag
    Loop While (SPSR0.SPIF0 = 0)
 
    SPIRxData = SPDR0
  #ENDIF

End Macro

//*********************************** HWSPI2 support

Sub SPI2Mode (In SPICurrentMode)

  #ifdef PIC
    #ifndef Var(SSPCON1)
      #ifdef Var(SSPCON)
        Dim SSPCON1 Alias SSPCON
      #endif
    #endif

    #ifdef Var(SPI2CON0)
        
         'Supports K mode SPI using the specific SPI module

        'Turn off SPI
        '(Prevents any weird glitches during setup)
        SPI2CON0_EN = 0

        'Set clock pulse settings
        SPI2CON1.SMP = 0
        'Data write on rising (idle > active) clock (CPHA = 1)
        SPI2CON1.CKE = 0
        'Clock idle low (CPOL = 0)
        SPI2CON1.CKP = 0

        SPI2CON1 = 0x40

        'Transfer
        SPI2CON2 = SPI2CON2 or 3

        #ifdef SPI2_DC
          dir SPI2_DC       out
        #endif

        #ifdef SPI2_CS
          dir SPI2_CS       out
        #endif

        #ifdef SPI2_RESET
          dir SPI2_RESET    out
        #endif

        #ifdef SPI2_DI
          dir SPI2_DI       in
        #endif

        #ifdef SPI2_DO
          dir SPI2_DO       out
        #endif

        #ifdef SPI2_SCK
          dir SPI2_SCK      out
        #endif

        'Select mode and clock
        SPI2CLK = SSP1_FOSC

        Select Case SPICurrentMode
          Case MasterFast or MasterUltraFast
            // Assumption that 0 is fastest
            SPI2BAUD = 0
            SPI2CON0.MST = 1
          Case MasterFast
            // See cdf file for SPIBAUDRATE_SCRIPT
            SPI2BAUD = SPIBAUDRATE_SCRIPT
            SPI2CON0.MST = 1
          Case Master
            // See cdf file for SPIBAUDRATE_SCRIPT_MASTER
            SPI2BAUD = SPIBAUDRATE_SCRIPT_MASTER
            SPI2CON0.MST = 1
          Case MasterSlow
            // See cdf file for SPIBAUDRATE_SCRIPT_MASTERSLOW
            SPI2BAUD = SPIBAUDRATE_SCRIPT_MASTERSLOW
            SPI2CON0.MST = 1
          Case Slave

            SPI2CON0.MST = 0
            #ifdef SPI2_SCK
              dir SPI2_SCK      in
            #endif
          Case SlaveSS

            SPI2CON0.MST = 0
            #ifdef SPI2_SCK
              dir SPI2_SCK      in
            #endif
        End Select


        #ifdef SPI_BAUD_RATE_REGISTER
         'override the script calculation
          SPI2BAUD = SPI_BAUD_RATE_REGISTER
        #endif

        'Enable SPI
        SPI2CON0.EN = 1
    #endif

  #endif

  #ifdef AVR
    #if nodef(CHIPAVRDX)
      'Turn off SPI
      '(Prevents any weird glitches during setup)
      Set SPCR.SPE Off

      'Set clock pulse settings
      'Need CPHA = 1 to match default PIC settings
      '(CPOL = 0 matches PIC)
      Set SPCR.CPHA On

      'Select mode and clock
      'Set some mode bits off, can set on later
      Set SPCR.MSTR Off
      Set SPSR.SPI2X Off
      Set SPCR.SPR0 Off
      Set SPCR.SPR1 Off

      Select Case SPICurrentMode
      Case MasterUltraFast
        Set SPCR.MSTR On
        Set SPSR.SPI2X On

      Case MasterFast
        Set SPCR.MSTR On

      Case Master
        Set SPCR.MSTR On
        Set SPCR.SPR0 On

      Case MasterSlow
        Set SPCR.MSTR On
        Set SPCR.SPR1 On

      'Nothing needed for slave
      'Case Slave
      'Case SlaveSS

      End Select

      'Enable SPI
      Set SPCR.SPE On
    #endif
  #endif

  #if def(CHIPAVRDX)

  #endif

End Sub

'Initialisation sub
'Used to set mode and clock options
'Overloaded: Other version is more compact but doesn't allow clock options to be set
Sub SPI2Mode (In SPICurrentMode, In SPIClockMode)

  #ifdef PIC
    #ifndef Var(SSPCON1)
      #ifdef Var(SSPCON)
        Dim SSPCON1 Alias SSPCON
      #endif
    #endif

    #ifdef Var(SPI2CON0)
        'Supports K mode SPI using the specific SPI module

        'Turn off SPI
        '(Prevents any weird glitches during setup)
        SPI2CON0_EN = 0

        'Clear register
        SPI2CON1 = 0

        If SPIClockMode.0 = Off Then
          SPI2CON1.CKE = 1
        End If

        If SPIClockMode.1 = On Then
          SPI2CON1.CKP = 1
        End If

        #IF BIT(SSP)
          If SPIClockMode.2 = On Then
            SPI2CON1.SSP = 1
          End If
        #ENDIF

        'Transfer
        SPI2CON2 = SPI2CON2 or 3

        #ifdef SPI2_DC
          dir SPI2_DC       out
        #endif

        #ifdef SPI2_CS
          dir SPI2_CS       out
        #endif

        #ifdef SPI2_RESET
          dir SPI2_RESET    out
        #endif

        #ifdef SPI2_DI
          dir SPI2_DI       in
        #endif

        #ifdef SPI2_DO
          dir SPI2_DO       out
        #endif

        #ifdef SPI2_SCK
          dir SPI2_SCK      out
        #endif

        'Select mode and clock
        SPI2CLK = SSP1_FOSC

        Select Case SPICurrentMode

            Case MasterUltraFast
              // assumed that 0 is fastest
              SPI2BAUD = 0
              SPI2CON0.MST = 1
            Case MasterFast
              // see CDF file for SPI2BAUDRATE_SCRIPT
              SPI2BAUD = SPI2BAUDRATE_SCRIPT
              SPI2CON0.MST = 1
            Case Master
              // see CDF file for SPI2BAUDRATE_SCRIPT_MASTER
              SPI2BAUD = SPI2BAUDRATE_SCRIPT_MASTER
              SPI2CON0.MST = 1
            Case MasterSlow
              // see CDF file for SPI2BAUDRATE_SCRIPT_MASTERSLOW
              SPI2BAUD = SPI2BAUDRATE_SCRIPT_MASTERSLOW
              SPI2CON0.MST = 1
            Case Slave
              SPI1CON0.MST = 0
              #ifdef SPI1_SCK
                dir SPI1_SCK      in
              #endif

            Case Slave
              SPI2CON0.MST = 0
            #ifdef SPI2_SCK
              dir SPI2_SCK      in
            #endif

            Case SlaveSS
              SPI2CON0.MST = 0
            #ifdef SPI2_SCK
              dir SPI2_SCK      in
            #endif

        End Select

        #ifdef SPI2_BAUD_RATE_REGISTER
        'override the script calculation
          SPI2BAUD = SPI2_BAUD_RATE_REGISTER
        #endif

        'Enable SPI
        SPI2CON0.EN = 1

    #endif
  #endif


End Sub

#define SPI2Transfer HWSPI2Transfer
'Sub to handle SPI data send/receive
Sub HWSPI2Transfer(In SPITxData, Out SPIRxData)

  #ifdef PIC

    #ifdef Var(SPI2CON0)

      'One byte transfer count
      SPI2TCNTL = 1
      SPI2TXB = SPITxData

      wait while SPI2RXIF = SPI_RX_IN_PROGRESS
      SPIRxData = SPI2RXB

    #endif

  #endif

End Sub

'Sub to handle SPI data send - Master mode only
Sub FastHWSPI2Transfer( In SPITxData )
  'Master mode only
  #ifdef PIC

    #ifdef Var(SPI2CON0)

    dim SPIRxData

      'One byte transfer count
      SPI2TCNTL = 1
      SPI2TXB = SPITxData

      wait while SPI2RXIF = SPI_RX_IN_PROGRESS
      SPIRxData = SPI2RXB

    #endif

  #endif


End Sub
