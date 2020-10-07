; -----------------------------------------------------------------------------
; Test for sbc 2.7 onboard ACIA. Must be run on real hardware.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.org $8000
.outfile "tests/aciaTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.require "../../Common/data.asm"

.advance $c000

.require "../../Common/conio.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../acia.asm"
.require "../sbc27io.asm"
.require "../via.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	jsr acia1Init
	jsr via1Init
	jsr via2Init
	`pushi acia1Input	; Initialize the console vectors.
	`pushi acia1Output
	jsr conIoInit
	jsr acia1_test
	brk

.scope
_msg:	.byte "*** ACIA 1 test ***",0
acia1_test:
	`println _msg
	rts
.scend

.require "../../Common/vectors.asm"
