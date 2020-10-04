; -----------------------------------------------------------------------------
; Test for sbc 2.7 onboard video. Must be run on real hardware.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/videoTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../../Common/data.asm"

.advance $c000

.require "../../Common/conio.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../sbc27io.asm"
.require "../via.asm"
.require "../video.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	jsr via1Init
	jsr via2Init
	jsr videoInit
	`pushi $00		; Initialize the console vectors.
	`pushi videoOutput
	jsr conIoInit
	jsr video_test
	brk

.scope
_name:	.byte "*** video test ***",0
video_test:
	`println _name
	`printcr
	rts
.scend

.require "../../Common/vectors.asm"
