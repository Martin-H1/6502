; -----------------------------------------------------------------------------
; Test for heap functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/cellTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../../Common/data.asm"

.advance $c000

.require "../../Common/array.asm"
.require "../cell.asm"
.require "../../Common/conio.asm"
.require "../gc.asm"
.require "../../Common/heap.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/string.asm"
.require "mockConio.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	`pushi getch_impl	; Initialize the console vectors.
	`pushi putch_impl
	jsr conIoInit

        `pushi heap_size
        `pushi heap_base
        jsr gcInit

	jsr null_test
	jsr mknumber_test
	jsr mkcons_test
	jsr replace_test
	jsr mkstring_test
	jsr carcdr_test
	jsr mark_test
	brk

.scope
; Do a set of tests that should always return null.
_name:	.byte "*** null test ***",0
_string1Cell:	.byte C_STRING
_string1:	.byte "Cell Test Enter",0
null_test:
	`println _name
	; Push a null and invoke car and cdr on it.
	`pushi 0
	jsr cellCar
	jsr cellCdr

	`pushi _string1
	jsr cellCar
	`pushi _string1
	jsr cellCdr
	jsr printstack
	`drop
	`drop
	`drop
	rts
.scend

.scope
_name:	.byte "*** mknumber test ***",0
mknumber_test:
	`println _name
	; Push a null and invoke car and cdr on it.
	`pushi $7e10
	jsr cellMkNumber
	jsr cellPrint
	`printcr
	rts
.scend

.scope
_name:	.byte "*** cons test ***",0
	.byte C_STRING
_argA:
	.byte "A",0
	.byte C_STRING
_argB:
	.byte "B",0
mkcons_test:
	`println _name
	`pushi _argA
	`pushzero
	jsr cellMkCons
	`dup
	jsr cellPrint
	`printcr

	`pushi _argB
	`pushzero
	jsr cellMkCons
	`dup
	jsr cellPrint
	`printcr

	jsr printstack
	rts
.scend

.scope
_name:	.byte "*** mkstring test ***",0
_string1Cell:	.byte C_STRING
_string1:	.byte "Cell Test Enter",0
_string2:	.byte "Cell Test Exit",0
mkstring_test:
	`println _name

	; Test cellPrint with a hand built cell
	`pushi _string1
	jsr cellPrint
	`printcr

	; Now use a cell constructor to make a cell.
	`pushi _string2
	jsr cellMkString
	jsr cellPrint
	`printcr
	`printcr
	rts
.scend

.scope
_name:	.byte "*** car cdr test ***",0
carcdr_test:
	`println _name
	`dup
	`dup
	jsr cellCar
	`swap
	jsr cellCdr
	jsr printstack
	jsr cellPrint
	`printcr
	jsr cellPrint
	`printcr
	jsr printstack
	rts
.scend

.scope
_name:	.byte "*** replace test ***",0
	.byte C_STRING
_argC:
	.byte "C",0
replace_test:
	`println _name
	`over
	; Test replace cdr operator
	jsr cellRplacd
	`dup
	jsr cellPrint
	`printcr

	`pushi _argC
	`over
	jsr cellRplaca
	`dup
	jsr cellPrint
	`printcr
	jsr printstack
	rts
.scend

.scope
_name:	.byte "*** mark test ***",0
_msg1:	.byte "mark = ",0
mark_test:
	`println _name
	; Call mark and see if it recursively marks other cells.
	`print _msg1
	`cellGetMark
	jsr printa
	`printcr

	`dup
	jsr cellMark
	`print _msg1
	`cellGetMark
	jsr printa
	`printcr

	`dup
	jsr cellCar
	`print _msg1
	`cellGetMark
	jsr printa
	`printcr

	jsr printstack
	`printcr
	rts
.scend

.require "../../Common/vectors.asm"
