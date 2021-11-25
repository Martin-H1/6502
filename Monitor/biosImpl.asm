; -----------------------------------------------------------------------------
; BIOS implemention module which allow programs to call monitor functions via a
; jump table in high ROM.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;

; Character set (ASCII)
.alias AscBell	$07	; bell (Control-G) ASCII character
.alias AscBS	$08	; backspace ASCII character
.alias AscCC	$03	; break (Control-C) ASCII character
.alias AscCR	$0D	; carriage return ASCII character
.alias AscDEL	$7F	; DEL ASCII character
.alias AscESC	$1B	; Escape ASCII character
.alias AscFF	$0C	; Formfeed ASCII character
.alias AscLF	$0A	; line feed ASCII character
.alias AscSP	$20	; space ASCII character
.alias AscTAB	$09	; tab ASCII character
.alias AscVT	$0B	; vertical tab ASCII character

.alias _tibSize [_tibEnd - _tib]

;
; Data segments
;
.data MONDATA
.org $0200

.space _tib	$50	; a line buffer for buffered reads.
.space _tibEnd	$00	; end marker to compute buffer size.
.space _writeIdx $01	; current write position in the buffer.
.space _readIdx	$01	; current read position in the buffer.
.space _STDOUT	2	; pointer to console output routine.
.space _STDIN	2	; pointer to console input routine.
.space _echo	$01	; control echo during line edit mode.

.text

;
; Macros
;
.macro _incIdx
	iny
	cpy #_tibSize
	bne _skip
	ldy #0
_skip:
.macend

.macro _decIdx
	dey
	bpl _skip
	ldy #[_tibSize - 1]
_skip:
.macend

;
; Functions
;

; Initializes I/O vectors and TIB state.
biosInitImpl:
	lda BIOSARG1
	sta _STDIN
	lda BIOSARG1+1
	sta _STDIN+1

	lda BIOSARG2
	sta _STDOUT
	lda BIOSARG2+1
	sta _STDOUT+1
	
	stz _echo
	stz _readIdx
	stz _writeIdx
	rts

; Output ctrl-G to beep speaker.
biosBellImpl:
	lda #AscBell
	jmp biosPutchImpl

; Clears the terminal (if supported).
biosCLSImpl:
	rts

; Go to the next line on terminal.
biosCRLFImpl:
        lda #AscLF
        jmp biosPutchImpl

; cgets is similar to the MSDOS console I/O function that reads an entire
; line from _stdin. A line is terminated by a CR, and backspace deletes
; the previous character in the buffer.
; input - implicit from init function.
; output - implicit in that the line buffer is filled.
biosCgetsImpl:
.scope
	phy
	ldy _writeIdx
_while:
	jsr _getch
	sta _tib,y
	lda _echo
	beq +
	lda _tib,y
	jsr biosPutchImpl
*	lda _tib,y
	cmp #AscBS
	bne +
	`_decIdx
	bra _while
*	`_incIdx
	cmp #AscCR
	beq _end
	cmp #$00
	bne _while
_end:
	sty _writeIdx
	ply
	rts

_getch:
	jmp (_STDIN)
.scend

; cputs is like the MSDOS console I/O function. It prints a null terminated
; string to the console using putch.
biosCputsImpl:
.scope
	phy
	ldy #0
_loop:
	lda (BIOSARG1),y	; get the string via address from zero page
	beq _exit		; if it is a zero, we quit and leave
	jsr biosPutchImpl	; if not, write one character
	iny			; advance to the next character
	bne _loop
	inc BIOSARG1+1		; advance to the next page
	bra _loop
_exit:
	ply
	rts
.scend

; Gets a character from the terminal input buffer, or gets more
; characters if it is empty. Returns it in accumulator.
biosGetchImpl:
.scope
	phy
	ldy _readIdx
	cpy _writeIdx
	bne +
	jsr biosCgetsImpl	; buffer empty, get more characters.
*	lda _tib,y
	`_incIdx
	sty _readIdx		; store next read index.
	ply
	rts
.scend

; Returns terminal cursor to top left.
biosHomeImpl:
	rts

; Positions cursor to row and column.
biosMonitorImpl:
	rts

; Prints the accumulator contents in hex to the terminal.
biosPutHexImpl:
	pha
	lsr
	lsr
	lsr
	lsr
	jsr _print_nybble
	pla
	and #$0f
_print_nybble:
	sed
	clc
	adc #$90	        	; Produce $90-$99 or $00-$05
	adc #$40			; Produce $30-$39 or $41-$46
	cld
	jmp biosPutchImpl

; puts a character in accumulator to stdout by doing an indirect jump to
; that handler. The handler will RTS to our caller.
biosPutchImpl:
.scope
        jmp (_STDOUT)
.scend

; Positions cursor to row and column.
biosSetCursorImpl:
	rts

; Enable or disable character echo during line editing mode.
; input - boolean in accumulator.
; output - none
biosSetEchoImpl:
	sta _echo
	rts

; puts a character in the accumulator back into the terminal input buffer.
biosUngetchImpl:
.scope
	phy
	ldy _readIdx
	`_decIdx
	sta _tib,y
	sty _readIdx
	ply
	rts
.scend

; Calls the reset vetor to reboot.
biosWarmBootImpl:
	rts

.scend
