;    Copyright 2012 - -2026 Evan R.Venn
;    A redirector for LCD to a Serial Terminal
;
;    This code is free software; you can redistribute it and/or
;    modify it under the terms of the GNU Lesser General Public
;    License as published by the Free Software Foundation; either
;    version 2.1 of the License, or (at your option) any later version.

;    This code is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    Lesser General Public License for more details.

;    If you require a copy of the GNU Lesser General Public
;    License along with this code; please write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;    THIS CONFIG OF THE SERIAL PORT WORKS WITH max232 THEN TO A PC TERMINAL
;    USART settings required for LCD2SerialRedirect.h
;         #define USART_BAUD_RATE 9600
;         Dir PORTc.6 Out
;         Dir PORTc.7 In
;         #define USART_DELAY 10 ms
;         #define USART_BLOCKING
;         wait 500 ms
;         HSerSend 7

; add  #include <LCD2SerialRedirect.h> your source file

#define locate ANSI_rev
#define CLS ANSIERASECREEN
#define print hserprint
#define LCDHex hserhex
#define LCDcursor HSerLCDcursor
#define LCDSpace HSerLCDSpace
#define LeadingZeroActive 2


'for LCDCURSOR Sub
#define DisplayON 12
#define LCDON 12

#define FLASHON 9
#define FLASH 9
#define CursorON 10

#define FLASHOFF 14
#define CursorOFF 13

#define DisplayOFF 11
#define LCDOFF 11


; Position cursor on Terminal
Sub ANSI ( IN ANSIxpos, IN ANSIypos )
    HSerSend 27
    HSerPrint "["
    HSerPrint ANSIypos
    HSerSend 59
    HSerPrint ANSIxpos
    HSerPrint "H"
End Sub

; Position cursor on Terminal
Sub ANSI_rev ( IN ANSIypos, IN ANSIxpos )
    ANSIypos = ANSIypos + 1
    HSerSend 27
    HSerPrint "["
    HSerPrint ANSIypos
    HSerSend 59
    HSerPrint ANSIxpos
    HSerPrint "H"
End Sub


; Eraase Terminal
Sub ANSIERASECREEN
    HSerSend 27
    HSerPrint "["
    HSerPrint "2"
    HSerPrint "J"
    Ansi ( 0, 0 )

    HSerSend 27
    HSerPrint "["
    HSerPrint "?"
    HSerPrint "25"
    HSerPrint "l"

end Sub


sub HSerHex (In LCDValue, optional in LCDChar = 1 )

    'Revised 01/26/2014 by William Roth
    'Prints Hex value of ByteVal at current cursor location
    'ByteVal must be in the range of 0 to 255 (Dec)
    'ByteVal can be entered as dec, binary or hex

    'Extract nibbles and convert to ascii values
    HighChar = (LCDValue / 16)  + 48
    LowChar  = (LCDValue and 15) + 48

    'Check for alpha and convert.
    If HighChar > 57 Then HighChar = HighChar + 7
    If LowChar > 57 Then  LowChar = LowChar + 7

    'Write chars to LCD - if user specifies LeadingZeroActive then print the leading char
    IF LCDChar = LeadingZeroActive then
       if LCDValue < 16 then
          HSerSend 48
       end if
    end if

    'Write high char if LCDValue is > 15 (DEC)
    IF LCDValue > 15 then HSerSend HighChar

    HSerSend LowChar


end Sub


' Dummy function to handle sub call
sub HSerLCDcursor(In LCDCRSR)

end sub

' Sub to print a number of spaces - upto 40
sub HSerLCDSpace(in LCDValue)
          if LCDValue > 40 then LCDValue = 40
          do until LCDValue = 0
                    HSerSend 32
                    LCDValue --
          loop
end sub
