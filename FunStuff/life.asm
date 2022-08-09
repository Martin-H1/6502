; -----------------------------------------------------------------------------
; Game of life created to illustrate my Turing Tarpit coding challenge.
; See http://en.wikipedia.org/wiki/Conway's_Game_of_Life and
;     http://forum.6502.org/viewtopic.php?f=2&t=7262
; 
; The goal of the challege is to use a reduced number of instructions as
; described below. The idea is to get a feel for what it would be like to
; program early machines such as the PDP-8.
;
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; Turing Tarpit Challege Overview
; The PDP-8 was tremendously influential to the early computer industry. It was
; cost engineered and Gordon Bell thought through the minimum number of viable
; instructions because it was built from descrete logic, so every gate counted.
; The resulting PDP-8 had eight memory instructions, an I/O instruction, NOP,
; and eight accumulator bit manipulation instructions.
;
; Like the 6502 the PDP-8's accumulator wasn't wide enough to hold an address,
; so the PDP-8 used pages with two addressing modes: PC and zero page. The
; 6502 has the latter, but the closet thing to PC page is PC relative and
; immediate mode. Because the PDP-8 was a word machine it didn't have variable
; length instructions to hold an absolute address as additional operands.
;
; The link register (carry bit) was the only processor status bit, so no zero,
; negative, or overflow bits and associated branch instructions. But there was
; a test for zero in the accumulator and branch instruction.
;
; So for the Turing Tarpit challenge I will restrict myself to the following
; 65c02 instructions and addressing modes.
;
; Accumulator operations: and, ora, eor, adc, clc, sec, inc, ror, rol, and asl
; Memory operations: lda #, lda absolute, lda (), sta absolute, and sta ()
; Flow control instructions: bcc, bcs, beq, bne, jmp absolute, jmp (),
; jsr absolute, rts, and nop
;
; That's 18 instructions which is a notable reduction from the 6502's 56, and
; matches the PDP-8. I'm allowing absolute addressing because the 6502 is a
; byte machine and one or two byte operands are intrinsic to such a design.
; I'm also allowing the zero control bit because the PDP-8 had skip on zero.
; 
; Notable missing instructions are CMP, DEC and SBC. Subtraction is done by
; adding the two's complement, and precompute negative quantities. CMP is
; a nondestructive subtract. DEC is achieved by adding negative one. This was
; actually common on early machines with a discrete logic ALU.

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

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space row 2		; iterators for the row and column.
.space col 2

.space rowM 2		; Row and column minus values
.space colM 2
.space rowP 2		; Row and column plus values
.space colP 2

; General purpose "register" locations for paramets and return values
.space param1 2		; input argument to function.
.space param2 2		; input argument to function.
.space retval 2		; variable for function return values.

.space temp 2		; working variable
.space strPtr 2		; pointer used for string processing.

.space rowJmpPtr 2	; ponters to functions for iteration.
.space colJmpPtr 2

.space genCurrPtr 2	; ponters to index into memory.
.space genNextPtr 2
.space asave 1		; place to store accumulator when needed.

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space gen_curr 768	; allocate arrays to hold current and next gens
.space gen_next 768

.text

;
; Macros
;

.macro incw
        inc _1
        bne _over
        inc _1+1
_over:
.macend

; adds a 16 bit word and a constant and puts the result in first argument.
.macro addwi
	clc
	lda _1
	adc #<_2
	sta _1
	lda _1+1
	adc #>_2
        sta _1+1
.macend

; adds two 16 bit words and puts the result in the first argument
.macro addw
	clc
	lda _1
	adc _2
	sta _1
	lda _1+1
	adc _2+1
        sta _1+1
.macend

; adds the accumulator to a byte sum.
.macro addToSum
	clc
	adc _1
	sta _1
.macend

; branch of word is zero
.macro beqw
	lda _1
	ora _1+1
	beq _2
.macend

; branch if word not equal zero
.macro bnew
	lda _1
	ora _1+1
	bne _2
.macend

; loads a word from a constant.
.macro loadwi
	lda #<_2
	sta _1
	lda #>_2
	sta _1+1
.macend

; loads a word from another word.
.macro loadw
	lda _2
	sta _1
	lda _2+1
	sta _1+1
.macend

.macro printw
	lda _1+1
	jsr printa
	lda _1
	jsr printa
.macend

; Prints a line feed.
.macro printcr
        lda #AscLF
        jsr _putch
.macend

.org $8000
.outfile "life.rom"
.advance $8000

;
; Functions
;
main:
	jsr printWelcome
	`loadwi strPtr, glider
	jsr parseCurr
	jsr printCurr
	jsr calcGen
	jsr printCurr
	brk

_welcome:
	.byte "Game of Life",AscLF,0
printWelcome:
	`loadwi strPtr, _welcome
	jsr cputs
	rts

; Sets the row offset to zero
rowFirst:
	lda #$00
	sta row
	sta row+1
	rts

; Advances the row offset by the width.
rowNext:
	`addwi row, width
	rts

; At end if current offset exceeds array size.
rowAtEnd?:
	`loadw retval, row
	`addwi retval, negSize
	lda retval
	ora retval+1
	rts

; Iterator used to apply a function to the rows.
rowForEach:
.scope
	jsr rowFirst
_loop:
	jsr _indirectJmp
	jsr rowNext
	jsr rowAtEnd?
	bne _loop
	rts
_indirectJmp:
	jmp (rowJmpPtr)
.scend

; Returns index of the row after current using wrap around.
rowPlus:
.scope
	`loadw rowP, row	; calculate the next row
	`addwi rowP, width
	`addwi rowP, negSize	; check if zero?
	`beqw rowP, _else
	`loadw rowP, row	; otherwise redo calculation.
	`addwi rowP, width
_else:	rts			; on wrap around return zero
.scend

; Returns index of the column before current using wrap around.
rowMinus:
.scope
	`beqw row, _else
	`loadw rowM, row
	`addwi rowM, negWidth
	rts
_else:
	`loadwi rowM, [size-width]
	rts
.scend

colFirst:
	lda #$00
	sta col
	sta col+1
	rts

colNext:
	`incw col
	rts

colAtEnd?:
	`loadw retval, col
	`addwi retval, negWidth
	lda retval
	ora retval+1
	rts

; Iterator used to apply a function to the cols.
colForEach:
.scope
	jsr colFirst
_loop:
	jsr _indirectJmp
	jsr colNext
	jsr colAtEnd?
	bne _loop
	rts
_indirectJmp:
	jmp (colJmpPtr)
.scend

; Returns index of the column after current using wrap around.
colPlus:
.scope
	`loadw colP, col	; calculate the next row
	`incw colP
	`addwi colP, negWidth	; check if zero?
	`beqw colP, _else
	`loadw colP, col	; otherwise redo calculation.
	`incw colP
	`addwi colP, width
_else:	rts			; on wrap around return zero
.scend

; Returns index of the column before current using wrap around.
colMinus:
.scope
	`beqw col, _else
	`loadw colM, col
	`addwi colM, negOne
	rts
_else:
	`loadwi colM, [width-1]
	rts
.scend

; moves bytes from next gen to current.
moveCurr:
.scope
	`loadwi genCurrPtr, gen_curr
	`loadwi genNextPtr, gen_next
	`loadwi temp, size
_loop:
	lda (genNextPtr)
	sta (genCurrPtr)
	`incw genCurrPtr
	`incw genNextPtr
	`addwi temp, negOne
	`bnew temp, _loop
	rts
.scend

; clears curr array to clear out junk in ram
eraseCurr:
.scope
	`loadwi genCurrPtr, gen_curr
	`loadwi temp, size
_loop:
	lda #$00
	sta (genCurrPtr)
	`incw genCurrPtr
	`addwi temp, negOne
	`bnew temp, _loop
	rts
.scend

; retrieve a cell value from the current generation
curr@:
	`loadwi genCurrPtr, gen_curr
	`addw genCurrPtr, param1
	`addw genCurrPtr, param2
	lda (genCurrPtr)
	rts

; stores a value into a cell from the current generation
curr!:
	sta asave
	`loadwi genCurrPtr, gen_curr
	`addw genCurrPtr, row
	`addw genCurrPtr, col
	lda asave
	sta (genCurrPtr)
	rts

; stores a value into a cell from the next generation
next!:
	sta asave
	`loadwi genNextPtr, gen_next
	`addw genNextPtr, row
	`addw genNextPtr, col
	lda asave
	sta (genNextPtr)
	rts

; Parses a pattern string referenced by strPtr into current board.
; This function is unsafe and will over write memory.
parseCurr:
.scope
	jsr eraseCurr
	jsr rowFirst
	jsr colFirst
_while:	lda (strPtr)
	beq _exit
	clc
	adc #$84	; two's compliment of 7C | 
	beq _else
	lda (strPtr)
	clc
	adc #$D6	; two's compliment of 2A *
	bne +
	lda #$01
	jsr curr!
*	jsr colNext
	jmp _next
_else:
	jsr rowNext
	jsr colFirst
_next:	`incw strPtr
	jmp _while
_exit:	rts
.scend

printCurrCell:
.scope
	`loadw param1, row
	`loadw param2, col
	jsr curr@
	beq _else
	lda #'\*
	jsr _putch
	rts
_else:
	lda #'\.
	jsr _putch
	rts
.scend

; prints the row from the current generation to output
printCurrRow:
	`loadwi colJmpPtr, printCurrCell
	`printcr
	jsr colForEach
	rts

; Prints the current board generation to standard output
printCurr:
	`loadwi rowJmpPtr, printCurrRow
	jsr rowForEach
	`printcr
	rts

; Computes the sum of the neigbors of the current cell.
; Note: without a stack this is run on function. Macros make it tolerable.
calcSum:
	jsr colMinus		; calculate neighboring cell indecies.
	jsr rowMinus
	jsr colPlus
	jsr rowPlus

	; Sum cell values for all eight neighbors.
	`loadw param1, rowM
	`loadw param2, colM
	jsr curr@
	sta asave

	`loadw param1, rowM
	`loadw param2, col
	jsr curr@
	`addToSum asave

	`loadw param1, rowM
	`loadw param2, colP
	jsr curr@
	`addToSum asave

	`loadw param1, row
	`loadw param2, colM
	jsr curr@
	`addToSum asave

	`loadw param1, row
	`loadw param2, colP
	jsr curr@
	`addToSum asave

	`loadw param1, rowP
	`loadw param2, colM
	jsr curr@
	`addToSum asave

	`loadw param1, rowP
	`loadw param2, col
	jsr curr@
	`addToSum asave

	`loadw param1, rowP
	`loadw param2, colP
	jsr curr@
	`addToSum asave
	rts

calcCell:
.scope
	jsr calcSum

	; Unless explicitly marked live, all cells die in the next generation.
	; There are two rules we'll apply to mark a cell live.
	`loadw param1, rowP
	`loadw param2, colP
	jsr curr@
	sta temp
	bne _live		; Is the current cell dead?
	lda asave
	clc
	adc #$fd		; A dead cell with 3 live neighbors
	beq _markLive		; becomes a live cell.
	rts			; otherwise stay dead!

_markLive:
	lda #$01
	jsr next!
	rts			; else

_live:	; Any live cell with two or three live neighbours survives.
	lda asave
	clc
	adc #$fe
	beq _markLive
	lda asave
	clc
	adc #$fd
	beq _markLive
	rts			; otherwise stay dead!
.scend

calcRow:
	`loadwi colJmpPtr, calcCell
	jsr colForEach
	rts

calcGen:
	`loadwi rowJmpPtr, calcRow
	jsr rowForEach
	jsr moveCurr
	rts

life:
;    page
;     begin calcGen 0 0 at-xy .curr key? until ;
	rts

; Test cases taken from Rosetta code's implementation
blinker:
	.byte "|***",0
toad:
	.byte "***| ***",0
pentomino:
	.byte "**| **| *",0
pi:
	.byte "**| **|**",0
glider:
	.byte " *|  *|***",0
pulsar:
	.byte "*****|*   *",0
ship:
	.byte " ****|*   *|    *|   *",0
pentadecathalon:
	.byte "**********",0
clock:
	.byte " *|  **|**|  *",0

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
	`incw strPtr		; get the next byte
	jmp _loop
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
