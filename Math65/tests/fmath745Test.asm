; -----------------------------------------------------------------------------
; Test for fmath functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/fmath745Test.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.require "../../Common/data.asm"

.advance $c000

.require "../fmath745.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit

	.invoke pushi neg_pi
	.invoke pushi M_PI
	jsr fadd32
	jsr fprintfpreg1
	jsr fprintfpreg2
	jsr printfloat
	`printcr

	.invoke pushi M_PI
	.invoke pushi M_PI
	jsr fsub32
	jsr fprintfpreg1
	jsr fprintfpreg2
	jsr printfloat

	brk

neg_two:	.byte $c0,$00,$00,$00
neg_one:	.byte $bf,$80,$00,$00
zero:		.byte $00,$00,$00,$00
one_third:	.byte $3e,$aa,$aa,$ab
one:		.byte $3f,$80,$00,$00
two:		.byte $40,$00,$00,$00
infinity:	.byte $7f,$80,$00,$00
neg_pi:		.byte $c0,$49,$0f,$db

.require "../../Common/tests/mockConio.asm"
.require "../../Common/conio.asm"
.require "../../Common/heap.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/string.asm"
.require "../../Common/vectors.asm"
