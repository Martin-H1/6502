; -----------------------------------------------------------------------------
; Game of life but I created it to illustrate my least number of instructions
; coding challenge. Toward that end I avoid the X and Y registers and their
; associated instructions, as well as packed decimal.
; See http://en.wikipedia.org/wiki/Conway's_Game_of_Life
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;
.alias height 24	; constants for board height, width, and size.
.alias width 32
.alias size height * width

.alias negOne $ffff	; precomputed negative quantities to avoid subtraction
.alias negWidth [0-width]

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space row 2		; iterators for the row and column.
.space col 2

.space temp 2		; working variables

.space rowJmpPtr 2	; ponters to functions for iteration.
.space colJmpPtr 2

.space genCurrPtr 2	; ponters to index into memory.
.space genNextPtr 2

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space gen_curr size	; allocate arrays to hold current and next gens
.space gen_next size

.text

;
; Macros
;

; adds two 16 bit quantities and puts the result in the first argument
.macro addConstant
	clc
	lda _1
	adc #<_2
	sta _1
	lda _1+1
	adc #>_2
        sta _1+1
.macend

.org $8000
.outfile "life.rom"
.advance $8000

;
; Functions
;
main:
	brk

; Sets the row offset to zero
rowFirst:
	lda #$00
	sta row
	sta row+1
	rts

; Advances the offset by the width.
rowNext:
	`addConstant row width
	rts

; At end if current offset exceeds array size.  
rowAtEnd?:
.scope	
	lda row
	cmp #<size
	beq _else
	bpl _else
	lda #$00
	rts
_else:
	lda #$ff
	rts
.scend

; Iterator used to apply a function to the rows.
rowForEach:
.scope
	jsr rowFirst
_loop:
	jsr _indirectJmp
	jsr rowNext
	jsr rowAtEnd?
	beq _loop
	rts
_indirectJmp:
	jmp (rowJmpPtr)
.scend

; Returns index of the row after current using wrap around.
row+:
	lda row
	sta temp
	lda row+1
	sta temp+1
	`addConstant temp width
	rts

; Returns index of the column before current using wrap around.
row-:
	lda row
	sta temp
	lda row+1
	sta temp+1
	`addConstant temp negWidth
	rts

colFirst:
	lda #$00
	sta col
	sta col+1
	rts

colNext:
	`addConstant col 1

colAtEnd?:
	;;     col @ width >=
	rts

; Iterator used to apply a function to the cols.
colForEach:
.scope
	jsr colFirst
_loop:
	jsr _indirectJmp
	jsr colNext
	jsr colAtEnd?
	beq _loop
	rts
_indirectJmp:
	jmp (colJmpPtr)
.scend

; Returns index of the column after current using wrap around.
col+:
;;     col @ 1 + width mod ;
	rts

; Returns index of the column before current using wrap around.
col-:
;;     col @ 1 - width mod ;
	rts

; moves bytes from next gen to current.
moveCurr:
	;; 	gen_next gen_curr size move ;
	rts

; clears curr array to clear out junk in ram
currErase:
.scope
	lda #<gen_curr
	sta genCurrPtr
	lda #>gen_curr
	sta genCurrPtr+1
_loop:
	lda #00
	sta (genCurrPtr)
	`addConstant genCurrPtr 1
	rts
.scend

; retrieve a cell value from the current generation
curr@:
;;    + gen_curr + c@ ;
	rts

\ stores a value into a cell from the current generation
: curr! ( n col row -- )
    + gen_curr + c! ;

\ Parses a pattern string into current board.
\ This function is unsafe and will over write memory.
: >curr ( addr count -- )
    currErase
    rowFirst colFirst
    1-
    for
        dup c@
        dup '|' <> if
            bl <> 1 and
            col @ row @ curr!
	    colNext
	else
	    drop
	    rowNext
	    colFirst
	then
	1+
    next
    drop ;

: .cell ( -- )
    col @ row @ curr@
    if '*' else '.' then
    emit ;

\ prints the row from the current generation to output
: .currRow ( -- )
    cr ['] .cell colForEach ;

\ Prints the current board generation to standard output
: .curr
    ['] .currRow rowForEach
    cr ;

\ retrieve a cell value from the next generation
: next@ ( col row -- n )
    + gen_next + c@ ;

\ stores a cell into the next generation
: next! ( n col row -- )
    + gen_next + c! ;

\ computes the sum of the neigbors of the current cell.
: calcSum ( -- n )
   col-  row-  curr@
   col @ row-  curr@ +
   col+  row-  curr@ +
   col-  row @ curr@ +
   col+  row @ curr@ +
   col-  row+  curr@ +
   col @ row+  curr@ +
   col+  row+  curr@ + ;

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
	lda #<calcCell
	sta cellJmpPtr
	lda #>calcCell
	sta cellJmpPtr+1
	jsr colForEach
	rts

calcGen:
	lda #<calcRow
	sta rowJmpPtr
	lda #>calcRow
	sta rowJmpPtr+1
	jsr rowForEach
	jsr moveCurr
	rts

: life ( -- )
    page
    begin calcGen 0 0 at-xy .curr key? until ;


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
