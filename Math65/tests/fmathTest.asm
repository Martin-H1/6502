; -----------------------------------------------------------------------------
; Test for fmath functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/fmathTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.advance $c000

.require "../fmath32.asm"
.require "../macros.asm"
.require "../print.asm"
.require "../stack.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer

	; convert an integer to float
	.invoke pushi 10
	jsr float32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi one
	.invoke pushi two
	jsr fadd32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi neg_one
	.invoke pushi one
	jsr fadd32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi twenty48
	.invoke pushi ten24
	jsr fsub32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi twenty48
	.invoke pushi fifteen
	jsr fmul32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi onehundred
	.invoke pushi ten
	jsr fdiv32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi ten
	jsr flog32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi onehundred
	jsr fln32
	jsr printfloat
	.invoke printcr

	.invoke drop
	.invoke pushi two
	jsr fexp32
	jsr printfloat
	.invoke printcr

	.invoke pushi one
	.invoke pushi onehundred
	jsr fcompare32
	brk

neg_one:	.byte $80,$C0,$00,$00
zero:		.byte $00,$00,$00,$00
one:		.byte $80,$40,$00,$00
two:		.byte $81,$40,$00,$00
ten:		.byte $83,$50,$00,$00
fifteen:	.byte $83,$78,$00,$00
thirty7:	.byte $85,$4A,$00,$00
onehundred:	.byte $86,$64,$00,$00
ten24:		.byte $8A,$40,$00,$00
twenty48:	.byte $8B,$40,$00,$00

.require "../vectors.asm"
