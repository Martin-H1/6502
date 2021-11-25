; -----------------------------------------------------------------------------
; Test for array functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/monitorTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.advance $c000

.require "../bios.asm"
.require "../biosImpl.asm"
.require "mockStdio.asm"
.require "../monitor.asm"

; Main entry point for the test
main:
	jsr mockBiosInit
	`callBiosCputs _name
	brk
	jsr monitorInit
	brk
	nop

_name:	.byte "*** break test ***",0


.require "../vectors.asm"
