'
'    Copyright Evan R Venn - Oct 2013-2024
'

' 14/08/22 Updated user changeable constants only - no functional change


  #IFNDEF MCP23008_DEVICE_1
    #define MCP23008_DEVICE_1 0x40
  #ENDIF

#DEFINE MCP23008_IODIR              0X00  ' DEFAULT IS INPUT  0= OUT 1= IN
#DEFINE MCP23008_IPOL               0X01  ' INPUT POLARITY 0= NORMAL, 1= INVERTED
#DEFINE MCP23008_GPINTEN            0X02  ' ALLOW INTERRUPT ON CHANGE
#DEFINE MCP23008_DEFVAL             0X03 ' DEFAULT VALUE BEFORE INTERRUPT [FOR EASY COMPARE]
#DEFINE MCP23008_INTCON             0X04 ' I/O EXPANDER CONFIGURATION .2 1= OPEN DRAIN [WIRE TOGETHER INTERRUPTS]
#DEFINE MCP23008_IOCON              0X05
#DEFINE MCP23008_GPPU               0X06 ' WEAK PULL UP RESISTORS [~100KOHHM] VERY HANDY
#DEFINE MCP23008_ITF                0X07 ' WHO DONE IT, THE INTERRUPT
#DEFINE MCP23008_INTCAP             0X08 ' SAVES THE EVENT THAT CAUSED THE INTERRUPT
#DEFINE MCP23008_GPIO               0X09 ' THE IO PORT
#DEFINE MCP23008_OLAT               0X0A ' THE LATCH OF THE IO PORT SEE DOC


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
