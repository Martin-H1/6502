; -----------------------------------------------------------------------------
; Test for math functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/bigstackTest.rom"

.alias ram_top	 $7fff
.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.
.alias SP0	 $7F
.alias AscLF	$0A	; line feed ASCII character

.require "../data.asm"

.advance $8000

.require "../conio.asm"
.require "../print.asm"
.require "../bigstack.asm"
.require "mockConio.asm"

; Main entry point for the test
main:
	jsr stack'init		; Reset stack pointer

	`pushi getch_impl	; Initialize the console vectors.
	`pushi putch_impl
	jsr initConIO

	`push num1
	`push num2
	jsr stack'print

	jsr stack'swap
	jsr stack'print

	jsr stack'dup
	jsr stack'print
	`drop
	brk

num1:	.word $dead
num2:	.word $beef

.require "../vectors.asm"
