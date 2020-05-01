; -----------------------------------------------------------------------------
; Test for heap functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/heapTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../data.asm"

.advance $c000

.require "../conio.asm"
.require "../heap.asm"
.require "../math16.asm"
.require "../print.asm"
.require "../stack.asm"
.require "../string.asm"
.require "mockConio.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	`pushi getch_impl	; Initialize the console vectors.
	`pushi putch_impl
	jsr conIoInit

	jsr create_test
	jsr alloc_test
	jsr print_test
	jsr realloc_test
	jsr free_test
	jsr defrag_test
	brk

.scope
_name:	.byte "*** create test ***",0
create_test:
	`println _name
        `pushi heap_size
        `pushi heap_base
        jsr hinit
        jsr hvalidate
	`printcr
	rts
.scend

.scope
_name:	.byte "*** alloc test ***",0
alloc_test:
	`println _name
	`pushi $10
	jsr halloc

	`pushi $01		; metadata value
	`pushi $10		; size
	jsr hallocgc

	`print block_ptr
	jsr printstack
	jsr hvalidate

	`pushi $10
	jsr halloc

	jsr printstack
	jsr hvalidate
	rts
.scend

.scope
_name:	.byte "*** print test ***",0
print_test:
	`println _name

	`pushi $beef
	`over
	`store

	`pushi $dead
	`over
	`pushi $08
	jsr add16
	`store

	`dup
	jsr hPrintBlock
	rts
.scend

.scope
_name:	.byte "*** realloc test ***",0
realloc_test:
	`println _name
	`pushi $20
	jsr hrealloc
	`dup
	jsr hPrintBlock

	`print block_ptr
	jsr printstack
	jsr hvalidate
	rts
.scend

.scope
_name:	.byte "*** free test ***",0
free_test:
	`println _name
	`swap
	jsr hfree
	`swap
	jsr hfree
	jsr hvalidate
	rts
.scend

.scope
_name:	.byte "*** defrag test ***",0
defrag_test:
	`println _name
	jsr hdefrag
	jsr hvalidate
	rts
.scend

block_ptr:	.byte "block pointer=",0

.require "../vectors.asm"
