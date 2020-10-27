; -----------------------------------------------------------------------------
; Program for running Mandelbrot on sbc 2.7 using onboard ACIA. Must be run on
; real hardware.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.org $8000
.outfile "mandelbrot.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.require "../Common/data.asm"

.advance $c000

.require "../Common/conio.asm"
.require "../Common/math16.asm"
.require "../Common/print.asm"
.require "../Common/stack.asm"
.require "../Hardware/acia.asm"
.require "../Hardware/sbc27io.asm"
.require "../Hardware/via.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	jsr acia1Init
	jsr via1Init
	jsr via2Init
	`pushi acia1Input	; Initialize the console vectors.
	`pushi acia1Output
	jsr conIoInit
	jsr mandelbrot
	brk

.require "mandelbrot.asm"

.require "../Common/vectors.asm"
