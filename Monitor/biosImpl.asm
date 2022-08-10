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

.alias _tibSize [_tibEnd - _tib]

;
; Data segments
;

.data MONZP
.org $00fc
.space _tmpPtr 2

.data MONDATA

.space _tib	$50	; a line buffer for buffered reads.
.space _tibEnd	$00	; end marker to compute buffer size.
.space _writeIdx $01	; current write position in the buffer.
.space _readIdx	$01	; current read position in the buffer.
.space _STDOUT	2	; pointer to console output routine.
.space _STDIN	2	; pointer to console input routine.
.space _zpsave	2	; space to hold ZP bytes to be restored from here.
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
.scope
	stz _echo		; Init biosState variables.
	stz _readIdx
	stz _writeIdx

	lda _tmpPtr		; Save the page zero byte during usage
	sta _zpSave
	lda _tmpPtr+1
	sta _zpSave+1
	pla			; pull the return address of the stack.
	sta _tmpPtr
	pla
	sta _tmpPtr+1
	phy			; Save Y and use to index argument bytes.
	ldy #1
	lda (_tmpPtr),y
	sta _STDIN
	iny
	lda (_tmpPtr),y
	sta _STDIN+1
	iny
	lda (_tmpPtr),y
	sta _STDOUT
	iny
	lda (_tmpPtr),y
	sta _STDOUT+1
	tya
	clc
	adc _tmpPtr
	sta _tmpPtr
	bcc _over
	inc _tmpPtr+1
_over:	ply
	lda _tmpPtr+1
	pha
	lda _tmpPtr
	pha
	lda _zpSave		; Restore the page zero byte during usage
	sta _tmpPtr
	lda _zpSave+1
	sta _tmpPtr+1
	rts
.scend

; Output ctrl-G to beep speaker.
biosBellImpl:
	lda #AscBell
	jmp biosPutch

; Clears the terminal (if supported).
biosCLSImpl:
	rts

; Go to the next line on terminal.
biosCRLFImpl:
        lda #AscLF
        jmp biosPutch

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
	jsr biosPutch
*	lda _tib,y
	cmp #AscBS
	bne +
	`_decIdx		; remove from buffer
	lda _echo
	beq +			; if no echo then continue
	lda #AscSP		; otherwise overwrite on terminal.
	jsr biosPutch		; backup one space again
	lda #AscBS
	jsr biosPutch
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
; string to the console using putch. That string is inline in the code and
; is accessed via the return address on the stack.
; input - the return address on the call stack.
; output - updated return address.
biosCputsImpl:
.scope
	pla			; pull the return address of the stack.
	sta _tmpPtr
	pla
	sta _tmpPtr+1
	inc _tmpPtr		; advance to inlined data.
	bne _loop
	inc _tmpPtr+1
	phy
	ldy #0
_loop:
	lda (_tmpPtr),y		; get the string via address from zero page
	beq _exit		; if it is a zero, we quit and leave
	jsr biosPutchImpl	; if not, write one character
	iny			; advance to the next character
	bne _loop
	inc _tmpPtr+1		; advance to the next page
	bra _loop
_exit:
	ply
	lda _tmpPtr+1		; retore the return address
	pha
	lda _tmpPtr
	pha
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
	jsr biosCgets		; buffer empty, get more characters.
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
	jmp biosPutch

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
