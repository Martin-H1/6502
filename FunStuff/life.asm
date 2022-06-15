; -----------------------------------------------------------------------------
; Game of life but created to illustrate my Turing Tarpit coding challenge.
; See http://en.wikipedia.org/wiki/Conway's_Game_of_Life
;
; The goal of the challege is to use the least number of instructions and
; towards that end I avoid the X and Y registers, their instructions, as well
; as packed decimal.
; 
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;

; Character set (ASCII)
.alias AscBS	$08	; backspace ASCII character
.alias AscCC	$03	; break (Control-C) ASCII character
.alias AscCR	$0D	; carriage return ASCII character
.alias AscDEL	$7F	; DEL ASCII character
.alias AscESC	$1B	; Escape ASCII character
.alias AscLF	$0A	; line feed ASCII character
.alias AscSP	$20	; space ASCII character

.alias height 24	; constants for board height, width, and size.
.alias width 32
.alias size height * width

.alias negOne $ffff	; precomputed negative quantities to avoid subtraction
.alias negSize [0-size]
.alias negWidth [0-width]
.alias lastCol [width-1]
.alias lastRow [size-width]

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space row 2		; iterators for the row and column.
.space col 2

.space temp 2		; working variables
.space strPtr 2		; pointer used for string processing.

.space rowJmpPtr 2	; ponters to functions for iteration.
.space colJmpPtr 2

.space genCurrPtr 2	; ponters to index into memory.
.space genNextPtr 2

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space gen_curr 768	; allocate arrays to hold current and next gens
.space gen_next 768

.text

;
; Macros
;

; adds two 16 bit quantities and puts the result in the first argument
.macro addConst
	clc
	lda _1
	adc #<_2
	sta _1
	lda _1+1
	adc #>_2
        sta _1+1
.macend

.macro compareConst
	lda _1
	cmp #<_2
	beq _equal

	lda _1+1		; low bytes are not equal, compare MSB
	sbc #>_2
	ora #$01		; Make Zero Flag 0 because we're not equal
	bvs _overflow
	bra _notequal

_equal:				; low bytes are equal, so we compare high bytes
	lda _1+1
	sbc #>_2
	bvc _done

_overflow:			; handle overflow because we use signed numbers
	eor #$80		; complement negative flag

_notequal:
	ora #$01		; if overflow, we can't be equal
_done:
.macend

.macro loadConst
	lda #<_2
	sta _1
	lda #>_2
	sta _1+1
.macend

.macro printWord
	lda _1+1
	jsr printa
	lda _1
	jsr printa
.macend

.org $8000
.outfile "life.rom"
.advance $8000

;
; Functions
;
main:
	`loadConst colJmpPtr, printWelcome
	jsr currErase
	`loadConst col, 30
	jsr colPlus
	`printWord temp
	lda #AscLF
	jsr _putch
	brk

_welcome:
	.byte "Game of Life",0
printWelcome:
	`loadConst strPtr, _welcome
	jsr cputs
	lda #AscLF
	jsr _putch
	rts

; Sets the row offset to zero
rowFirst:
	lda #$00
	sta row
	sta row+1
	rts

; Advances the offset by the width.
rowNext:
	`addConst row, width
	rts

; At end if current offset exceeds array size.
rowAtEnd?:
	`compareConst row, size
	rts

; Iterator used to apply a function to the rows.
rowForEach:
.scope
	jsr rowFirst
_loop:
	jsr _indirectJmp
	jsr rowNext
	jsr rowAtEnd?
	bmi _loop
	rts
_indirectJmp:
	jmp (rowJmpPtr)
.scend

; Returns index of the row after current using wrap around.
rowPlus:
	lda row
	sta temp
	lda row+1
	sta temp+1
	`addConst temp, width
	`compareConst temp, size
	bmi +
	`loadConst temp, 0
*	rts

; Returns index of the column before current using wrap around.
rowMinus:
.scope
	lda row
	sta temp
	lda row+1
	sta temp+1
	`addConst temp, negWidth
	lda temp+1
	bpl +
	`loadConst temp, lastRow
*	rts
.scend

colFirst:
	lda #$00
	sta col
	sta col+1
	rts

colNext:
	`addConst col, 1
	rts

colAtEnd?:
	`compareConst col, width
	rts

; Iterator used to apply a function to the cols.
colForEach:
.scope
	jsr colFirst
_loop:
	jsr _indirectJmp
	jsr colNext
	jsr colAtEnd?
	bmi _loop
	rts
_indirectJmp:
	jmp (colJmpPtr)
.scend

; Returns index of the column after current using wrap around.
colPlus:
.scope
	lda col
	sta temp
	lda col+1
	sta temp+1
	`addConst temp, 1
	`compareConst temp, width
	bmi +
	`loadConst temp, 0
*	rts
.scend

; Returns index of the column before current using wrap around.
colMinus:
	lda col
	sta temp
	lda col+1
	sta temp+1
	`addConst temp, negOne
	lda temp+1
	bpl +
	`loadConst temp, lastCol
*	rts

; moves bytes from next gen to current.
moveCurr:
.scope
	`loadConst genCurrPtr, gen_curr
	`loadConst genNextPtr, gen_next
	`loadConst temp, 0
_loop:
	lda (genNextPtr)
	sta (genCurrPtr)
	`addConst genCurrPtr, 1
	`addConst genNextPtr, 1
	`addConst temp, 1
	`compareConst temp, size
	bmi _loop
	rts
.scend

; clears curr array to clear out junk in ram
currErase:
.scope
	`loadConst genCurrPtr, gen_curr
	`loadConst temp, 0
_loop:
	lda #00
	sta (genCurrPtr)
	`addConst genCurrPtr, 1
	`addConst temp, 1
	`compareConst temp, size
	bmi _loop
	rts
.scend

; retrieve a cell value from the current generation
curr@:
;;    + gen_curr + c@ ;
	rts

; stores a value into a cell from the current generation
curr!:
;; 	+ gen_curr + c!
	rts

; Parses a pattern string into current board.
; This function is unsafe and will over write memory.
currParse:
	jsr currErase
	jsr rowFirst
	jsr colFirst
;    1-
;     for
;        dup c@
;        dup '|' <> if
;            bl <> 1 and
;             col @ row @ curr!
;	    colNext
; 	else
;	    drop
;	    rowNext
; 	    colFirst
;	then
; 	1+
;     next
;     drop ;
	rts

currCell:
;    col @ row @ curr@
;     if '*' else '.' then
;     emit ;
	rts

; prints the row from the current generation to output
currRow:
;	cr ['] .cell colForEach
	rts

; Prints the current board generation to standard output
curr:
;     ['] .currRow rowForEach
;     cr ;
	rts

; retrieve a cell value from the next generation
next@:
;    + gen_next + c@ ;
	rts

; stores a cell into the next generation
next!:
;    + gen_next + c! ;
	rts

; computes the sum of the neigbors of the current cell.
calcSum:
;   col-  row-  curr@
;   col @ row-  curr@ +
;   col+  row-  curr@ +
;   col-  row @ curr@ +
;   col+  row @ curr@ +
;   col-  row+  curr@ +
;   col @ row+  curr@ +
;   col+  row+  curr@ + ;
	rts

calcCell:
	jsr calcSum

	; Unless explicitly marked live, all cells die in the next generation.
	; There are two rules we'll apply to mark a cell live.

	; Is the current cell dead?

	; col @ row @ curr@ 0=
	; if
	; Any dead cell with three live neighbours becomes a live cell.
	; 3 =
	; else
	; Any live cell with two or three live neighbours survives.
	; dup 2 >= swap 3 <= and
	; then
	; 1 and
	; col @ row @ next! ;
	rts

calcRow:
	`loadConst colJmpPtr, calcCell
	jsr colForEach
	rts

calcGen:
	`loadConst rowJmpPtr, calcRow
	jsr rowForEach
	jsr moveCurr
	rts

life:
;    page
;     begin calcGen 0 0 at-xy .curr key? until ;
	rts

; prints the accumulator contents in hex to the console.
printa:
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
	jmp _putch

; cputs is like the MSDOS console I/O function. It prints a null terminated
; string to the console using _putch.
cputs:
.scope
_loop:	lda (strPtr)		; get the string via address from zero page
	beq _exit		; if it is a zero, we quit and leave
	jsr _putch		; if not, write one character
	`addConst strPtr, 1	; get the next byte
	bra _loop
_exit:	rts
.scend

; conio functions unique to each platform.
.alias _py65_putc	$f001	; Definitions for the py65mon emulator
.alias _py65_getc	$f004

_getch:
.scope
*	lda _py65_getc
	beq -
	rts
.scend

_putch:
.scope
	sta _py65_putc
	rts
.scend

; Interrupt handler for RESET button, also boot sequence.
; Note: This doesn't count for the restricted instruction challenge as
; these steps are required to set the hardware to a known state.
.scope
resetv:
	sei		; diable interupts, until interupt vectors are set.
	cld		; clear decimal mode
	ldx #$FF	; reset stack pointer
	txs

	lda #$00	; clear all three registers
	tax
	tay

	pha		; clear all flags
	plp
	jmp main	; go to monitor or main program initialization.
.scend

; redirect the NMI interrupt vector here to be safe, but this 
; should never be reached for py65mon.
irqv:
nmiv:
panic:
	brk

; Interrupt vectors.
.advance $FFFA

.word nmiv    ; NMI vector 
.word resetv  ; RESET vector
.word irqv    ; IRQ vector
