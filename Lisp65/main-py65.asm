; -----------------------------------------------------------------------------
; Lisp for the 65c02.
; Used with the Ophis assembler and the py65mon simulator.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;
.alias RamSize		$7EFF	; default $8000 for 32 kb x 8 bit RAM

.alias _py65_putc	$f001	; Definitions for the py65mon emulator
.alias _py65_getc	$f004

;
; Data segments
;
.require "data.asm"

.data BSS
.space heap_base	$4000	; It's size is 16 KB.
.space heap_top		$0000
.alias heap_size	heap_top - heap_base

.text

;
; Macros
;

;
; Functions
;

.word $8000
.org $8000
.outfile "py65mon.rom"
.advance $8000

main:
	ldx #SP0		; Reset stack pointer
	`pushi _getch_impl	; Initialize the console vectors.
	`pushi _putch_impl
	jsr conioInit
	`pushi heap_size
	`pushi heap_base
	jsr gcInit
	jsr lispInit
	jsr repl
	brk

; conio functions unique to each platform.
_getch_impl:
.scope
*	lda _py65_getc
	beq -
	rts
.scend

_putch_impl:
.scope
	sta _py65_putc
	rts
.scend

.require "array.asm"
.require "cell.asm"
.require "conio.asm"
.require "gc.asm"
.require "heap.asm"
.require "lisp65.asm"
.require "math16.asm"
.require "print.asm"
.require "stack.asm"
.require "string.asm"

.require "vectors.asm"

.scend
