; -----------------------------------------------------------------------------
; Test for the garbage collector functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.org $8000
.outfile "tests/gcTest.rom"

.alias RamSize   $7EFF		; def $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../../Common/data.asm"

; Main entry point for the test.
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit
        `pushi heap_size
        `pushi heap_base
	jsr gcInit
	jsr gcPrintProtected
	jsr protect_test
	jsr protect_test
	jsr unprotect_test
	jsr forcegc_test
	brk

.scope
_name:	.byte "*** protect test ***",0
_data:	.byte "A String Cell",0
protect_test:
	`println _name
	`pushi _data
	jsr cellMkString
	`dup
	jsr cellPrint
	`printcr
	`dup
	jsr gcProtect
	jsr gcPrintProtected
	rts
.scend

.scope
_name:	.byte "*** unprotect test ***",0
unprotect_test:
	`println _name
	jsr gcUnprotect
	jsr gcPrintProtected
	jsr gcUnprotect
	jsr gcPrintProtected
	rts
.scend

.scope
_name:	.byte "*** force test ***",0
_data:	.byte "A String Cell\"",0
forcegc_test:
	`println _name
	`pushi _data
	jsr cellMkString
	jsr gcProtect
	jsr gcDoGC
	jsr gcPrintProtected
	rts
.scend

.require "mockConio.asm"

.require "../../Common/array.asm"
.require "../cell.asm"
.require "../../Common/conio.asm"
.require "../gc.asm"
.require "../../Common/heap.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/string.asm"
.require "../../Common/vectors.asm"
