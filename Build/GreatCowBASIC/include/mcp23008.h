'
'    Copyright Evan R Venn - Oct 2013-2020
'

' 14/08/22 Updated user changeable constants only - no functional change


  #IFNDEF MCP23008_DEVICE_1
    #define MCP23008_DEVICE_1 0x40
  #ENDIF

#define MCP23008_IODIR              0x00  ' default is INput  0= out 1= in
#define MCP23008_IPOL               0x01  ' input polarity 0= normal, 1= inverted
#define MCP23008_GPINTEN            0x02  ' Allow interrupt on change
#define MCP23008_DEFVAL             0x03 ' default value BEFORE interrupt [for easy compare]
#define MCP23008_INTCON             0x04 ' I/O EXPANDER CONFIGURATION .2 1= Open Drain [wire together interrupts]
#define MCP23008_IOCON              0x05
#define MCP23008_GPPU               0x06 ' weak pull up resistors [~100kOhhm] very handy
#define MCP23008_ITF                0x07 ' who done it, the interrupt
#define MCP23008_INTCAP             0x08 ' Saves the event that caused the interrupt
#define MCP23008_GPIO               0x09 ' the IO port
#define MCP23008_OLAT               0x0A ' the LATCH of the IO port see doc


sub mcp23008_sendbyte( in mcp23008_device, in mcp23008_instr , in mcp23008_data )

    ' see the Data sheet for this!!! FIGURE 1-1: MCP23008 I2C DEVICE PROTOCOL
    I2CSTART
    I2CSEND( mcp23008_device )
    I2CSEND( mcp23008_instr )
    I2CSEND( mcp23008_data )
    'initiate Stop Condition
    I2CSTOP

end sub



sub mcp23008_readbyte( in mcp23008_device, in mcp23008_instr , Out mcp23008_data )

    ' see the Data sheet for this!!! FIGURE 1-1: MCP23008 I2C DEVICE PROTOCOL
    I2CSTART
    I2CSEND( mcp23008_device )
    I2CSEND( mcp23008_instr )
    I2CSTART
    I2CSEND( mcp23008_device + 1)
    I2CReceive( mcp23008_data , NACK )
    'initiate Stop Condition
    I2CSTOP
end sub
