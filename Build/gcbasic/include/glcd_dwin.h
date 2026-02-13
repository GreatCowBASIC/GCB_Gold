#option Explicit
#IgnoreCompilation
' ================================================================
    ' Proper Integration for DWIN into GCBASIC GLCD System
    ' This restores full compatibility with #include <glcd.h>
    ' ================================================================

    ' --- Step 1: Add to glcd.dat (at the end) ---
    // 39,glcd_type_dwin,glcd_dwin.h

    ' --- Step 2: Add to glcd.h (in the GLCD_TYPE constants section) ---
    // #define GLCD_TYPE_DWIN 39

    ' --- Step 3: Create file glcd_dwin.h (full library below) ---
    ' ================================================================
    ' glcd_dwin.h - DWIN T5L/DGUS II Driver for GCBASIC GLCD System
    ' Assumes #include <glcd.h> in user program (provides font tables, globals, etc.)
    ' ================================================================

    ' DWIN uses standard 16-bit RGB565 colors (same as ILI9341 etc.)
    ' No new color defines needed - use existing TFT_ / ILI9341_ constants from glcd.h

    ' ================================================================
    ' DWIN Protocol Constants (Optimized for maintainability)
    ' These centralize all hardcoded values sent to/received from DWIN
    ' ================================================================
#script

    ' Intended: Check if the constant GLCD_DWINSerialSend is not defined
    ' If not defined, set it to HSerSend (standard hardware serial send command)
    ' Purpose: Provides a consistent alias for sending data to the DWIN display
    If NODEF(GLCD_DWINSerialSend) Then
        GLCD_DWINSerialSend = HSerSend
    End if

    If NODEF(GLCD_DWINSerialSendSuffix) Then
        GLCD_DWINSerialSendSuffix = ", 2"
    End if
    ' --- Configurable (adjust for your project/firmware) ---
    ' Intended: Check if DWIN_VP_DRAW_BASE is not defined
    ' If not defined, default to 0x1000 (common VP base for "Basic Graphics" runtime commands)
    ' Purpose: Base address for sending drawing primitives (user should verify/override per DGUS project)
    If NODEF(DWIN_VP_DRAW_BASE) Then
        DWIN_VP_DRAW_BASE = 0x1000   ' Common base for runtime drawing commands
    End If

    ' Unconditionally define drawing command opcodes
    ' These are sent to the drawing VP base to trigger primitives
    ' (Rarely need override, but possible if firmware differs)
    DWIN_CMD_POINT       =  0x0001   ' Command code for drawing a single point/pixel
    DWIN_CMD_LINE        =  0x0003   ' Command code for drawing a line
    DWIN_CMD_RECT_FILL   =  0x0005   ' Command code for filled rectangle
    DWIN_CMD_CIRCLE      =  0x0006   ' Command code for unfilled circle
    DWIN_CMD_CIRCLE_FILL =  0x0007   ' Command code for filled circle

    ' Intended: Conditional defaults for standard system VPs
    ' Purpose: Ensure reliable addresses for common functions (almost never changes)
    If NODEF(DWIN_VP_PAGE) Then
        DWIN_VP_PAGE = 0x0014        ' System VP for switching display pages
    End if
    If NODEF(DWIN_VP_BACKLIGHT) Then
        DWIN_VP_BACKLIGHT = 0x0082   ' System VP for backlight brightness (0-100%)
    end if
    If NODEF(DWIN_VP_BUZZER) Then
        DWIN_VP_BUZZER = 0x0086      ' System VP for buzzer beep duration (in 10ms units)
    end if

    ' --- DWIN Protocol Constants ---
    DWIN_PKT_START1         = 0x5A     ' Packet header byte 1 (common to all outbound/inbound packets)
    DWIN_PKT_START1_NIBBLE  = 0xA
    DWIN_PKT_START2         = 0xA5     ' Packet header byte 2 (common to all outbound/inbound packets)
    DWIN_CMD_WRITE_VP    = 0x82     ' Command code for writing to a VP (Variable Pointer) address
    DWIN_CMD_READ_VP     = 0x83
    DWIN_WRITE_VP_LEN_WORD = 0x05   ' Packet length for writing a WORD (2-byte) value to VP (header + cmd + VP + value)
    DWIN_WRITE_VP_LEN_BYTE = 0x04   ' Packet length for writing a BYTE (1-byte) value to VP (header + cmd + VP + value)
    DWIN_TOUCH_PKT_CMD   = 0x01     ' Command code in inbound touch packet indicating touch data

    If NODEF(DWIN_INTRA_SIGNAL_DELAY) Then
      DWIN_INTRA_SIGNAL_DELAY = 0
    end if

    If NODEF(DWIN_INTRA_UPDATE_DELAY) Then
      DWIN_INTRA_UPDATE_DELAY = 10
    Else
      DWIN_INTRA_UPDATE_DELAY = 0
    End If

    #endscript


Sub DWIN_Reset
    // Sequence 5A A5 07 82 00 04 55 AA 5A A5 
    DWIN_WriteLongVP ( [word]0x0004, [long]0x55AA5AA5 )
    wait 2 s
End Sub

// 0 to 100%
Sub DWIN_SetBrightness ( In DWIN_value As Word )

  If DWIN_value > 100 Then DWIN_value = 100
  DWIN_value = Scale ( DWIN_value, 0, 100, 0, 0x7F)
// def hmiBrightness(pBrightness):
// 	data = [0x5A, 0xA5, 0x04, 0x82, 0x00, 0x82, pBrightness]
    // const uint8_t rawCmd1[] = {0x5A, 0xA5, 0x04, 0x82, 0x00, 0x82, 0x00};       // Set display brightness to 0%
    // const uint8_t rawCmd2[] = {0x5A, 0xA5, 0x04, 0x82, 0x00, 0x82, 0x7F};       // Set display brightness to 100%
                                    
    DWIN_WriteByteVP ( [word]0x0082, [Byte]DWIN_value )

End Sub



''' DWIN Show Message: Sends a string message to a specific page on the DGUS display.
''' 
''' Usage Notes:
''' - Writes message to calculated VP address: 0x2000 + (page * 0x10).
''' - Packet: 5A A5 BC 82 VPH VPL [message bytes] (BC = Len(message) + 4; no CRC).
''' - Message: ASCII string; max ~28 chars (buffer limit 32 - header/BC/CMD/addr).
''' - Prerequisites: VP addresses configured in DGUS CFG for text display (e.g., txt_id at addr).
''' - ACK: Display responds "OK" (5A A5 03 82 4F 4B) if successful.
''' - Timing: Send ~1-2ms + (len*10us/bit at 115200); non-blocking.
''' - Example: DWIN_ShowMessage(1, "Hello World")  ' Page 1, addr=0x2010
''' - Error: No ACK = invalid addr/len; check comms or CFG.
''' - Extend for CRC: If R2 bit 4=1, add 2 CRC bytes (BC +=2), compute over payload.
''' 

/*
  Control Location: Set Up Text Widgets in DGUS Tools
  To place messages exactly where you want (e.g., top-left status bar), edit the CFG file:

  Open Project: Launch DGUS Tools V7.6+, load your .DTF project (or create new for TC040C17U00: File → New → 4" 480x272).
  Add Text Widget:
  Drag "Text" icon from toolbar to canvas (e.g., x=10, y=10 for top-left).
  Right-click widget → Properties → ID = "txt_status" (unique name).
  Font/Size: Set to visible (e.g., Arial 16pt, color white).
  Alignment: Left/center as needed.

  Assign VP Address:
  In Properties → "VP Address" = 0x2000 (for page 0; increment by 0x10 for page 1 = 0x2010, etc.).
  "Data Type" = String (ASCII).
  "Max Length" = 20 (matches BC limit).

  Page Control:
  Switch pages via touch buttons: Add "Page Jump" widget, set target page (0-255).
  Or serial: Write to VP 0x0000 = page number (e.g., DWIN_WriteWordVP(0x0000, 1) jumps to page 1).

  Save & Upload:
  File → Generate CFG → Save as T5LCFG0.CFG.
  Copy to FAT32 SD root → Insert in display, power on → "SD END!" (removes SD after).
  Power cycle—project loads with your widget at VP 0x2000.


  3. Test & Debug

  On-Screen Check: After upload, send DWIN_ShowMessage(0, "ROCHDALE TEST")—should appear in your widget.

*/

Sub DWIN_ShowMessage ( in  DWIN_page As Byte, In DWIN_message As String)

  Dim DWIN_pageaddress As Word
  DWIN_pageaddress = 0x2000 + (DWIN_page * 0x10)  ' Calculated VP address (page * 16)
  Dim DWIN_pagemsgLen As Byte 
  DWIN_pagemsgLen = DWIN_message(0)  ' String length
  Dim Idx As Byte

  GLCD_DWINSerialSend DWIN_PKT_START1 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_PKT_START2 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_pagemsgLen+3 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_CMD_WRITE_VP GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_pageaddress_H GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_pageaddress GLCD_DWINSerialSendSuffix
  For Idx = 1 to DWIN_pagemsgLen
    GLCD_DWINSerialSend DWIN_message(Idx) GLCD_DWINSerialSendSuffix
  Next
  wait DWIN_INTRA_SIGNAL_DELAY ms
  
  DWIN_WriteByteVP ( [word]0x5555, [BYTE]=0x55 )
End Sub


Sub DWIN_Sleep()
  // 0x5A, 0xA5, 0x05, 0x82, 0x00, 0x82, 0x01
End Sub

Sub DWIN_Wake()
  // 0x5A, 0xA5, 0x05, 0x82, 0x00, 0x82, 0x00
End Sub




''' DWIN Tone Generator: Writes to T5L DGUSII buzzer VP (0x00A0 = Music_Play_Set) for beep/tone control.
''' 
''' Usage Notes:
''' - This Sub triggers a beep/tone on the DWIN TC040C17U00 (T5L DGUSII-based display).
''' - VP 0x00A0 controls buzzer: Word value = duration in 8ms units (e.g., 0x0006 = 6 * 8ms = 48ms beep).
'''   - Range: 0x0000 = off/no beep; 0x0001 = 8ms chirp; up to 0x00FF = 4080ms (~4s max tone).
'''   - Tone pitch/volume set in CFG file (e.g., BUZZ_Freq_DIV1/2 for ~1kHz; BUZZ_Freq_Duty=0xFF for loud).
''' - Prerequisites: Buzzer enabled in CFG (SD upload): AUX_CFG (0x05) bit 6=0; BUZZ_Set_En (0x27)=0x5A.
''' - ACK: Display responds with "OK" packet (5A A5 03 82 4F 4B) if successful.
''' - Timing:
'''   - Send Time: ~1ms at 115200 baud (8 bytes: header + BC=05 + CMD=82 + VP=00 A0 + value).
'''   - Beep Latency: <10ms (serial parse + trigger); full duration as set (non-blocking).
'''   - Repeat Rate: Min 50ms gap to avoid overlap (e.g., for patterns: beep + Wait(duration + 50)).
'''   - Max Freq: 10-20 beeps/sec (baud limit; use patterns for Morse/error codes).
''' - Error Handling: If no ACK, check comms (baud=115200 8N1) or CFG (read VP 0x0005 for AUX_CFG).
''' - External Buzzer: Wire piezo/speaker to BUZZ pin (pin 4) for louder output (internal faint).
''' - Example Patterns:
'''   - Short chirp: DWIN_Tone(0x0001)  ' 8ms
'''   - 100ms beep: DWIN_Tone(0x000D)  ' 13*8=104ms (as tested)
'''   - 3x error beeps: For i=1 To 3: DWIN_Tone(0x0006): Wait 100: Next
''' - CRC: Disabled by default (R2 bit 4=0); if enabled, extend Sub with CRC calc (BC=07 + 2 bytes).
''' 
Sub DWIN_Tone (In DWIN_value As Word)
  DWIN_WriteWordVP(0x00A0, DWIN_value)  ' Writes duration units to Music_Play_Set
  wait 100 ms
End Sub


''' Plays a simple "TTL-style" chiptune Imperial March (Star Wars Theme) using DWIN_Tone beeps.
''' TTL-style: Short/long durations simulate notes (e.g., 0x0004 = 32ms "quarter note", 0x0002 = 16ms "eighth").
''' Timing: Each note + 50ms gap for rhythm; total ~15s march at 115200 baud.
''' Notes approximated: Higher value = longer duration for rhythm; true pitch via CFG freq (e.g., ~1kHz).
''' Usage: Call DWIN_PlayStarWars() in main (e.g., on button: If buttonPress Then DWIN_PlayStarWars()).
''' Prerequisites: Buzzer CFG enabled (AUX_CFG bit 6=0); test single tones first.
''' Extend: Adjust gaps/values for tempo; add loops for repeat.
''' Rochdale Remix: Imperial beeps marching through the mills—force awakens!
Sub DWIN_PlayTTLSong()
  Dim note As Word
  Dim gapMs As Word = 50  ' Fixed gap between notes (adjust for speed: 30=fast, 100=slow)
  
  ' Imperial March (simplified: G G G Eb Bb G Eb Bb G, then repeat motif)
  ' Structure: Main motif (9 notes) + bridge + repeat; ~2 phrases.
  
  ' Motif 1: Dum dum dum (short short short)
  note = 0x0004  ' G (32ms)
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' G
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' G
  DWIN_Tone(note): Wait gapMs ms
  
  ' Dum da dum (long short long)
  note = 0x0008  ' Eb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0006  ' Bb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' G
  DWIN_Tone(note): Wait gapMs ms
  
  ' Dum da da dum (long short short long)
  note = 0x0008  ' Eb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0006  ' Bb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' G
  DWIN_Tone(note): Wait (gapMs * 2) ms  ' Hold end
  
  ' Bridge pause
  Wait 300 ms
  
  ' Motif 2: Repeat with variation (da dum dum da)
  note = 0x0006  ' G
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' Eb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' Eb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0006  ' F
  DWIN_Tone(note): Wait gapMs ms
  
  ' Dum da dum (long short long)
  note = 0x0008  ' D
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0006  ' Bb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' Bb
  DWIN_Tone(note): Wait gapMs ms
  
  ' Da dum da dum da dum (short long short long short long)
  note = 0x0004  ' G
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0008  ' G
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0004  ' F
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0008  ' Eb
  DWIN_Tone(note): Wait gapMs ms
  note = 0x0006  ' Bb
  DWIN_Tone(note): Wait (gapMs * 2) ms  ' Hold grand end
  
  ' Off tone to stop
  DWIN_Tone(0x0000): Wait 200 ms
End Sub


'''Writes a word value to a specified VP address on the DWIN display
'''@param vp Variable Pointer address (Word)
'''@param value Data value to write (Word)
Sub DWIN_WriteWordVP (In vp As Word, In DWIN_value As Word)
  GLCD_DWINSerialSend   DWIN_PKT_START1 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_PKT_START2 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_WRITE_VP_LEN_WORD GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_CMD_WRITE_VP GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp_H GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value_H GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value GLCD_DWINSerialSendSuffix
  wait DWIN_INTRA_SIGNAL_DELAY ms
End Sub

Sub DWIN_WriteThreeNibblesVP(In vp As Word, In DWIN_value As Byte)
  GLCD_DWINSerialSend DWIN_PKT_START1 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_PKT_START2 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_WRITE_VP_LEN_WORD GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_CMD_WRITE_VP GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp_H GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]0
  wait DWIN_INTRA_SIGNAL_DELAY ms
End Sub

Sub DWIN_WriteLongVP (In vp As Word, In DWIN_value As Long)
  GLCD_DWINSerialSend DWIN_PKT_START1 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_PKT_START2 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_WRITE_VP_LEN_WORD+2 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_CMD_WRITE_VP GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp_H GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value_E GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value_U GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value_H GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value GLCD_DWINSerialSendSuffix
  wait DWIN_INTRA_SIGNAL_DELAY ms
End Sub

'''Writes a byte value to a specified VP address on the DWIN display (optimized variant)
'''@param vp Variable Pointer address (Word)
'''@param value Data value to write (Byte)
Sub DWIN_WriteByteVP (In vp As Word, In DWIN_value As Byte)
  GLCD_DWINSerialSend DWIN_PKT_START1 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_PKT_START2 GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend DWIN_WRITE_VP_LEN_BYTE GLCD_DWINSerialSendSuffix 
  GLCD_DWINSerialSend DWIN_CMD_WRITE_VP GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp_H GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]vp GLCD_DWINSerialSendSuffix
  GLCD_DWINSerialSend [byte]DWIN_value GLCD_DWINSerialSendSuffix
  wait DWIN_INTRA_SIGNAL_DELAY ms
End Sub

'''Clears the entire GLCD screen
'''@param Optional FillColour Colour to fill the screen with (defaults to GLCDBackground)
Sub GLCDCLS_DWIN (Optional In FillColour As Word = GLCDBackground)
  GLCD_yordinate = 0
  FilledBox_DWIN 0, 0, GLCD_WIDTH-1, GLCD_HEIGHT-1, FillColour
End Sub

'''Switches the active page on the DWIN display
'''@param PageID Target page number (Word)
Sub DWIN_ChangePage (In PageID As Byte)
  // [0x5A, 0xA5, 0x07, 0x82, 0x00, 0x84, 0x5A, 0x01, 0x00, pageNO]
  DWIN_WriteLongVP ( [word]0x0084, [Long]0x5A010000 + PageID )
End Sub


Sub DWIN_ReadPage
  // 5A A5 04 83 0014 01
  DWIN_WriteThreeNibblesVP ( [word]0x0083, [Long]0x001401 + PageID )
End Sub



' Globals (add these if not already)
Dim parseStateGlobal As Byte = 0  ' Persist state across calls for partials
' Global packet buffer: Stores up to 12 bytes of data after BC (CMD + addr + len + values; adjust size if CRC included)
Dim VPPacket(12) As Byte  ' Byte array for raw storage; index 0=CMD, 1=VPH, 2=VPL, etc.

'''Reads the last synchronized VP packet.
Sub DWIN_ReadVP (Out vp As Word, Out cmd As Byte, Out DWIN_value As Word)
  Dim isValidPacket As Byte = 0
  Dim clearIdx As Byte

  vp = 0x0000
  cmd = 0x00
  DWIN_value = 0x0000

  If PacketLength = 3 And VPPacket(1) = DWIN_CMD_WRITE_VP Then
    vp = 0xFFFF
    cmd = DWIN_CMD_WRITE_VP
    DWIN_value = 0xFFFF
    isValidPacket = 1

  Else If VPPacket(1) = DWIN_CMD_READ_VP And PacketLength = 6 Then

    [BYTE]vp = VPPacket(3)
    vp_h = (VPPacket(2)) 
    cmd = DWIN_CMD_READ_VP

    DWIN_value = VPPacket(6)
    DWIN_value_h = VPPacket(5)
    isValidPacket = 1
  
  Else
    // Ser1PrintLn "Unknown packet size"
    
  End If

  DWIN_PacketReady = 0
  PacketLength = 0

End Sub


' Globals (add these if not already; assume BUFFER_SIZE = 64)
Dim parseStateGlobal As Byte = 0  ' Persist state across calls for partials
// Dim emptyCallCounter As Byte = 0  ' Throttle empty prints
'''Non-blocking buffer processor: Scans circular buffer for complete DWIN VP packets.
Function ProcessDWINBuffer As Bit
  Dim bufPos As Byte
  Dim tempByte As Byte

  Do 

    tempByte = bgetc
    
    Select Case parseStateGlobal
      Case 0
        If tempByte = DWIN_PKT_START1 or ( tempByte & 0x0F ) = DWIN_PKT_START1_NIBBLE  Then 
          parseStateGlobal = 1

        End If
      Case 1
        If tempByte = DWIN_PKT_START2 Then
          parseStateGlobal = 2
        Else
          parseStateGlobal = 0
        End If
      Case 2  ' Read BC
        
        PacketLength = tempByte
        If  PacketLength > 7 Then
          // If a PacketLength is too big then reset the buffer read operations
          parseStateGlobal = 0
          // Ser1PrintLN "Error PacketLength"
        Else
          parseStateGlobal = 3
        End If
        bufPos = 1

      Case 3
        VPPacket(bufPos) = tempByte
        bufPos += 1
        If bufPos = PacketLength + 1 Then  ' Full

          DWIN_PacketReady = True
          ProcessDWINBuffer = True
          IsValidPacket = True
          parseStateGlobal = 0
          
          Exit Function
        End If

    End Select

  Loop While USARThasBufferData

  ProcessDWINBuffer = False
  IsValidPacket = False
  
End Function

'''Draws a single pixel on the GLCD
'''@param PX X coordinate (Word)
'''@param PY Y coordinate (Word)
'''@param PColour Pixel colour (Word, RGB565)
Sub PSet_DWIN(In PX As Word, In PY As Word, In PColour As Word)
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE, DWIN_CMD_POINT
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+2, PColour
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+4, PX
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+6, PY
End Sub

'''Draws a line on the GLCD
'''@param X1 Start X coordinate (Word)
'''@param Y1 Start Y coordinate (Word)
'''@param X2 End X coordinate (Word)
'''@param Y2 End Y coordinate (Word)
'''@param Optional LColour Line colour (defaults to GLCDForeground)
Sub Line_DWIN(In X1 As Word, In Y1 As Word, In X2 As Word, In Y2 As Word, Optional In LColour As Word = GLCDForeground)
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE, DWIN_CMD_LINE
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+2, LColour
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+4, X1
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+6, Y1
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+8, X2
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+10, Y2
End Sub

'''Draws an unfilled box (rectangle) on the GLCD
'''@param X1 Top-left X coordinate (Word)
'''@param Y1 Top-left Y coordinate (Word)
'''@param X2 Bottom-right X coordinate (Word)
'''@param Y2 Bottom-right Y coordinate (Word)
'''@param Optional BColour Box outline colour (defaults to GLCDForeground)
Sub Box_DWIN(In X1 As Word, In Y1 As Word, In X2 As Word, In Y2 As Word, Optional In BColour As Word = GLCDForeground)
  If X1 > X2 Then Swap X1, X2
  If Y1 > Y2 Then Swap Y1, Y2
  Line_DWIN X1, Y1, X2, Y1, BColour
  Line_DWIN X2, Y1, X2, Y2, BColour
  Line_DWIN X2, Y2, X1, Y2, BColour
  Line_DWIN X1, Y2, X1, Y1, BColour
End Sub

'''Draws a filled box (rectangle) on the GLCD
'''@param X1 Top-left X coordinate (Word)
'''@param Y1 Top-left Y coordinate (Word)
'''@param X2 Bottom-right X coordinate (Word)
'''@param Y2 Bottom-right Y coordinate (Word)
'''@param Optional FColour Fill colour (defaults to GLCDForeground)
Sub FilledBox_DWIN(In X1 As Word, In Y1 As Word, In X2 As Word, In Y2 As Word, Optional In FColour As Word = GLCDForeground)
  Dim BoxW As Word : BoxW = X2 - X1 + 1
  Dim BoxH As Word : BoxH = Y2 - Y1 + 1
  If X1 > X2 Then Swap X1, X2 : BoxW = X2 - X1 + 1
  If Y1 > Y2 Then Swap Y1, Y2 : BoxH = Y2 - Y1 + 1
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE, DWIN_CMD_RECT_FILL
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+2, FColour
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+4, X1
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+6, Y1
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+8, BoxW
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+10, BoxH
End Sub

'''Draws an unfilled circle on the GLCD
'''@param CX Centre X coordinate (Word)
'''@param CY Centre Y coordinate (Word)
'''@param Radius Circle radius (Word)
'''@param Optional CColour Circle colour (defaults to GLCDForeground)
Sub Circle_DWIN (In CX As Word, In CY As Word, In Radius As Word, Optional In CColour As Word = GLCDForeground)
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE, DWIN_CMD_CIRCLE
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+2, CColour
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+4, CX
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+6, CY
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+8, Radius
End Sub

'''Draws a filled circle on the GLCD
'''@param CX Centre X coordinate (Word)
'''@param CY Centre Y coordinate (Word)
'''@param Radius Circle radius (Word)
'''@param Optional CColour Fill colour (defaults to GLCDForeground)
Sub FilledCircle_DWIN (In CX As Word, In CY As Word, In Radius As Word, Optional In CColour As Word = GLCDForeground)
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE, DWIN_CMD_CIRCLE_FILL
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+2, CColour
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+4, CX
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+6, CY
  DWIN_WriteWordVP DWIN_VP_DRAW_BASE+8, Radius
End Sub

'''Draws a single character using the standard GLCD font tables
'''@param CharX X position of character (Word)
'''@param CharY Y position of character (Word)
'''@param CharCode ASCII code of character (Byte)
'''@param Optional TColour Text colour (defaults to GLCDForeground)
Sub GLCDDrawChar_DWIN(In CharX As Word, In CharY As Word, In CharCode As Byte, Optional In TColour As Word = GLCDForeground)
  ' Standard font drawing using PSet_DWIN (full compatibility with other drivers)
  Dim CurrCharCol, CurrCharRow, CurrCharVal,ScaleX, ScaleY As Byte
  CharCode -= 15
  If CharCode >= 178 And CharCode <= 202 Then CharY -= 1

  For CurrCharCol = 1 To 5
    Select Case CurrCharCol
      Case 1: ReadTable GLCDCharCol3, CharCode, CurrCharVal
      Case 2: ReadTable GLCDCharCol4, CharCode, CurrCharVal
      Case 3: ReadTable GLCDCharCol5, CharCode, CurrCharVal
      Case 4: ReadTable GLCDCharCol6, CharCode, CurrCharVal
      Case 5: ReadTable GLCDCharCol7, CharCode, CurrCharVal
    End Select

    For CurrCharRow = 1 To 8
      Dim PixX As Byte : PixX = 0
      Dim PixY As Byte : PixY = 0
      For ScaleX = 1 To GLCDfntDefaultsize
        PixX += 1
        PixY = 0
        For ScaleY = 1 To GLCDfntDefaultsize
          PixY += 1
          If CurrCharVal.0 = 1 Then
            PSet_DWIN CharX + CurrCharCol + PixX - GLCDfntDefaultsize, CharY + CurrCharRow + PixY - GLCDfntDefaultsize, TColour
          Else
            PSet_DWIN CharX + CurrCharCol + PixX - GLCDfntDefaultsize, CharY + CurrCharRow + PixY - GLCDfntDefaultsize, GLCDBackground
          End If
        Next
      Next
      Rotate CurrCharVal Right
    Next
  Next
End Sub

'''Draws a string of characters using the standard GLCD font
'''@param StrX Starting X position (Word)
'''@param StrY Starting Y position (Word)
'''@param TextStr String to draw
'''@param Optional TColour Text colour (defaults to GLCDForeground)
Sub GLCDDrawString_DWIN(In StrX As Word, In StrY As Word, In TextStr As String, Optional In TColour As Word = GLCDForeground)
  Dim Idx As Byte
  For Idx = 1 To TextStr(0)
    GLCDDrawChar_DWIN StrX + ((Idx-1) * (GLCDFontWidth * GLCDfntDefaultsize)), StrY, TextStr(Idx), TColour
  Next
End Sub

'''Prints a string at the specified location (uses GLCDDrawString_DWIN)
'''@param PX X position (Word)
'''@param PY Y position (Word)
'''@param PrintData String to print
'''@param Optional FColour Text colour (defaults to GLCDForeground)
Sub GLCDPrint_DWIN (In PX As Word, In PY As Word, In PrintData As String, Optional In FColour As Word = GLCDForeground)
  GLCDDrawString_DWIN PX, PY, PrintData, FColour
End Sub

'''Prints a numeric value at the specified location (converts to string first)
'''@param PX X position (Word)
'''@param PY Y position (Word)
'''@param PrintData Long value to print
'''@param Optional FColour Text colour (defaults to GLCDForeground)
Sub GLCDPrint_DWIN (In PX As Word, In PY As Word, In PrintData As Long, Optional In FColour As Word = GLCDForeground)
  GLCDDrawString_DWIN PX, PY, Str(PrintData), FColour
End Sub
