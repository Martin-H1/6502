; -----------------------------------------------------------------------------
; Test for conio functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.org $8000
.outfile "tests/conioTest.rom"

.alias RamSize   $7EFF		; def $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../data.asm"

; Main entry point for the interpreter test
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit

	jsr putch_test
	jsr getch_test
	jsr cputs_test
	jsr cgets_test
	jsr echo_test
	jsr ungetch_test
	brk

.scope
_name:	.byte "*** putch test ***",0
putch_test:
	`println _name
	jsr mockConioInit
	lda #'B
	jsr putch
	`printcr
	rts
.scend

.scope
_data:	.byte "A string",0
_name:	.byte "*** getch test ***",0
getch_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	jsr getch
	jsr putch
	jsr getch
	jsr putch
	jsr getch
	jsr putch
	`printcr
	rts
.scend

.scope
_data:	.byte "printing some text.",0
_name:	.byte "*** cputs test ***",0
cputs_test:
	`println _name
	`pushi _data
	jsr cputs
	`printcr
	rts
.scend

.scope
_data:	.byte "This is the first line to buffer.",AscCR,AscLF
	.byte "Now another line to fill the buffer.",AscCR,AscLf
	.byte "This is the final line.",0
_name:	.byte "*** cgets test ***",0
cgets_test:
	`println _name
	`pushi _data
	jsr mockConioInit
_loop:	jsr getch
	cmp #$00
	beq _end
	jsr putch
	bra _loop
_end:	`printcr
	rts
.scend

.scope
_data:	.byte "This is the first line to buffer.",AscCR,AscLF,0
_name:	.byte "*** echo test ***",0
echo_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	lda #$ff
	jsr conioSetEcho
	jsr cgets
	`printcr
	rts
.scend

.scope
_data:	.byte "some data.",0
_name:	.byte "*** ungetch test ***",0
ungetch_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	lda #'B
	jsr ungetch
	lda #'A
	jsr ungetch
	jsr getch
	jsr putch
	jsr getch
	jsr putch
	jsr getch
	jsr putch
	jsr getch
	jsr putch
	`printcr
	rts
.scend

.require "mockConio.asm"

.require "../conio.asm"
.require "../print.asm"
.require "../stack.asm"
.require "../vectors.asm"
