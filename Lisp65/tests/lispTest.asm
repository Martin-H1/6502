; -----------------------------------------------------------------------------
; Test for lisp functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.org $8000
.outfile "tests/lispTest.rom"

.alias RamSize   $7EFF		; def $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../../Common/data.asm"

; Main entry point for the interpreter test
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit

        `pushi heap_size
        `pushi heap_base
	jsr gcInit
	jsr lispInit
	jsr read_string_test
	jsr read_alpha_test
	jsr read_number_test
	jsr read_empty_test
	jsr read_dotted_test
	jsr read_list_test
	jsr evalargs_test
	jsr car_test
	jsr cdr_test
	jsr add_test
	jsr sub_test
	brk

.scope
_data:	.byte "A String Cell\"",0
_name:	.byte "*** read string test ***",0
read_string_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	jsr readString
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "foo",0
_name:	.byte "*** read alpha test ***",0
read_alpha_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	; The first character should be in the accumulator.
	lda #'a
	jsr readAlpha
	jsr cellPrint
	`printcr
	`push INTERNS
	jsr cellPrint
	`printcr
	`pushi _data
	jsr mockConioInit
	lda #'b
	jsr readAlpha
	jsr cellPrint
	`printcr
	`push INTERNS
	jsr cellPrint
	`printcr

	`pushi _data
	jsr mockConioInit
	lda #'b
	jsr readAlpha
	jsr cellPrint
	`printcr
	`push INTERNS
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "1234",0
_name:	.byte "*** read number test ***",0
read_number_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	; The first character should be in the accumulator.
	lda #'1
	jsr readNumber
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "(    )",0
_name:	.byte "*** read empty test ***",0
read_empty_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	jsr getch
	jsr readList
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "( a .  b   )",0
_name:	.byte "*** read dotted ***",0
read_dotted_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	jsr read
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "( aa  bb (1 2 3) \"d\"  )",0
_name:	.byte "*** read list ***",0
read_list_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	jsr read
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data1:	.byte "(1)",0
_data2: .byte "(1 2)",0
_data3: .byte "(1 . 2)",0
_name:	.byte "*** evalargs test ***",0
evalargs_test:
	`println _name
	`push GLOBALS		; push global environment.
	`pushZero		; push null pointer.
	jsr evalargs
	jsr printtos		; we should get null back
	`printcr
	`printcr
	`drop
	`pushi _data1
	jsr mockConioInit
	jsr read
	jsr printtos
	`printcr
	jsr evalargs
	jsr printtos
	`printcr
	jsr cellPrint
	`printcr

	`pushi _data2
	jsr mockConioInit
	jsr read
	`dup
	jsr cellPrint
	`printcr
	jsr evalargs
	jsr cellPrint
	`printcr

	`pushi _data3
	jsr mockConioInit
	jsr read
	`dup
	jsr cellPrint
	`printcr
	jsr evalargs
	jsr cellPrint
	`printcr
	`drop
	rts
.scend

.scope
_data:	.byte "(9 1 3)",0
_func:	.byte "car",0
_name:	.byte "*** car test ***",0
car_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	`pushzero		; null environment
	jsr read
	`pushi _func
	jsr intern
	`push GLOBALS
	`swap
	jsr assq
	jsr cellCdr
	`fetch
	`execute
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "(9 1 3)",0
_func:	.byte "cdr",0
_name:	.byte "*** cdr test ***",0
cdr_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	`pushzero		; null environment
	jsr read
	`pushi _func
	jsr intern
	`push GLOBALS
	`swap
	jsr assq
	jsr cellCdr
	`fetch
	`execute
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "(+ 9 1 3)",0
_name:	.byte "*** add test ***",0
add_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	`push GLOBALS		; global environment
	jsr read
	jsr eval
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "(- 9 1 3)",0
_name:	.byte "*** sub test ***",0
sub_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	`push GLOBALS		; global environment
	jsr read
	jsr eval
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_data:	.byte "  display \"hello world\"\n(define increment (lambda (x) (+ x 1))) ; comment\n",0
_name:	.byte "*** read test ***",0
read_test:
	`println _name
	`pushi _data
	jsr mockConioInit
	jsr read
	jsr printstack
	`push INTERNS
	jsr cellPrint
	`printcr
	rts
.scend

.require "mockConio.asm"

.require "../../Common/array.asm"
.require "../cell.asm"
.require "../../Common/conio.asm"
.require "../gc.asm"
.require "../../Common/heap.asm"
.require "../lisp65.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/string.asm"
.require "../../Common/vectors.asm"
