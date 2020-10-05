; -----------------------------------------------------------------------------
; conio functions use indirection to allow platform specific implementations.
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
.data ZPDATA
.space _TMPPTR1	2	; working pointer

.data BSS
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

; Routine to initialize console pointers and state.
; input - two pointers on the stack
; output - none
conIoInit:
.scope
	`pop _STDOUT
	`pop _STDIN
	stz _echo
	stz _readIdx
	stz _writeIdx
	rts
.scend

; Enable or disable character echo during line editing mode.
; input - boolean in accumulator.
; output - none
conioSetEcho:
.scope
	sta _echo
	rts
.scend

; cgets is similar to the MSDOS console I/O function that reads an entire
; line from _stdin. A line is terminated by a CR, and backspace deletes
; the previous character in the buffer.
; input - implicit from init function.
; output - implicit in that the line buffer is filled.
cgets:
.scope
	phy
	ldy _writeIdx
_while:
	jsr _getch
	sta _tib,y
	lda _echo
	beq +
	lda _tib,y
	jsr putch
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
cputs:
.scope
	`pop _TMPPTR1
	phy                     ; save Y register
        ldy #$00                ; index

*       lda (_TMPPTR1),y	; get the string via address from zero page
        beq +			; if it is a zero, we quit and leave
        jsr putch	        ; if not, write one character
        iny                     ; get the next byte
        bra -
*       ply
        rts
.scend

; gets a character from the terminal input buffer, or gets more
; characters if it is empty.
getch:
.scope
	phy
	ldy _readIdx
	cpy _writeIdx
	bne +
	jsr cgets		; buffer empty, get more characters.
*	lda _tib,y
	`_incIdx
	sty _readIdx		; store next read index.
	ply
	rts
.scend

; puts a character in the accumulator back into the terminal input buffer.
ungetch:
.scope
	phy
	ldy _readIdx
	`_decIdx
	sta _tib,y
	sty _readIdx
	ply
	rts
.scend

; puts a character in accumulator to stdout by doing an indirect jump to
; that handler. The handler will RTS to our caller.
putch:
.scope
        jmp (_STDOUT)
.scend

.scend
