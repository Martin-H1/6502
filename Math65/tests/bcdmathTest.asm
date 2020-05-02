; -----------------------------------------------------------------------------
; Test for fmath functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.outfile "tests/bcdmathTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.require "../../Common/data.asm"

.text
	.org $c000

.require "../bcdmath.asm"

.macro loadargs
	phx
	`copyReg _1, w1
	`copyReg _2, w2
	plx
	phx
	`pushi w2
	`pushi w1

	`print w1equals
	jsr printbcd
	`print w2equals
	jsr printbcd
	`printcr
.macend

.macro printresult
	plx
	`print result
	`pushi w3
	jsr printbcd
	`printcr
	`printcr
.macend

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit

	`println _enter

	`loadargs _half, _pi
	jsr add
	`printresult

	`loadargs _tst3, _tst4
	jsr sub
	`printresult

	`loadargs _unit, _half
	jsr div
	`printresult

	`loadargs _sixtn, _sixtn
	jsr mul
	`printresult

	`loadargs _two56, _zero
	jsr sqrt
	`printresult

	`loadargs _pio2, _zero
	jsr sin
	`printresult

	`loadargs _pio2, _zero
	jsr cos
	`printresult

	`loadargs _pio4, _zero
	jsr tan
	`printresult
	
	`println _exit
	brk

_zero:	.byte $00,$00,$00,$00,$00,$00,$00,$00
_ln2:	.byte $40,$01,$69,$31,$47,$18,$05,$60
_ln10:	.byte $00,$00,$23,$02,$58,$50,$92,$99
_sqt2:	.byte $00,$00,$14,$14,$21,$35,$62,$37
_pi:	.byte $00,$00,$31,$41,$59,$26,$53,$59
_pio2:	.byte $00,$00,$15,$70,$79,$63,$26,$79
_pio4:	.byte $40,$01,$78,$53,$98,$16,$33,$98
_twopi:	.byte $00,$00,$62,$83,$18,$53,$07,$18
_unit:	.byte $00,$00,$10,$00,$00,$00,$00,$00
_half:	.byte $40,$01,$50,$00,$00,$00,$00,$00
_four:	.byte $00,$00,$40,$00,$00,$00,$00,$00
_sixtn:	.byte $00,$01,$16,$00,$00,$00,$00,$00
_two56:	.byte $00,$02,$25,$60,$00,$00,$00,$00

_tst1:	.byte $00,$01,$80,$00,$00,$00,$00,$00
_tst2:	.byte $00,$01,$80,$00,$00,$00,$00,$01
_tst3:	.byte $80,$01,$80,$00,$00,$00,$00,$00
_tst4:	.byte $80,$00,$12,$30,$00,$00,$00,$00
_tst5:	.byte $40,$01,$33,$00,$00,$00,$00,$00
_tst6:	.byte $40,$02,$20,$00,$00,$00,$00,$00
_tst7:	.byte $C0,$01,$33,$00,$00,$00,$00,$00
_tst8:	.byte $C0,$02,$20,$00,$00,$00,$00,$00

_enter:		.byte "BCD Math Test Enter",0
_exit:		.byte "BCD Math Test Exit",0
w1equals:	.byte "W1= ",0
w2equals:	.byte ", W2= ",0
result:		.byte "Result= ",0

.require "../../Common/tests/mockConio.asm"
.require "../../Common/conio.asm"
.require "../../Common/heap.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/string.asm"
.require "../../Common/vectors.asm"
