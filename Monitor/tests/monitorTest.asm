; -----------------------------------------------------------------------------
; Test for array functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/monitorTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../../Common/data.asm"

.advance $c000

.require "../monitor.asm"
.require "../../Common/conio.asm"
.require "../../Common/heap.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/string.asm"
.require "../../Hardware/sbc27io.asm"
.require "../../Hardware/acia.asm"
.require "../../Hardware/via.asm"
.require "../../Hardware/video.asm"
.require "../../Common/tests/mockConio.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConIOInit
	`println _name
	jsr monitorInit
	brk
	nop

_name:	.byte "*** break test ***",0


.require "../../Common/vectors.asm"
