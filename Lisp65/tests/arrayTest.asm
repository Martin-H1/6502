; -----------------------------------------------------------------------------
; Test for array functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/arrayTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM
.alias heap_base $0400		; The heap starts on page 4.
.alias heap_size $4000		; It's size is 16 KB.

.require "../data.asm"

.advance $c000

.require "../array.asm"
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
	`pushzero
	jsr mockConIOInit

        `pushi heap_size
        `pushi heap_base
        jsr hinit

	jsr alloc_test
	jsr append_test
	jsr getat_test
	jsr setat_test
	jsr delat_test
	jsr findFirst_test
	jsr findLast_test
	jsr hfree		; dealloc array.
	jsr oneItem_test
	brk

.scope
_name:	.byte "*** alloc test ***",0
alloc_test:
	`println _name
	`pushi $05
	jsr arrayAlloc
	`dup
	jsr arrayPrint
	rts
.scend

.scope
_name:	.byte "*** append test ***",0
append_test:
	`println _name
	`pushi $dead
	jsr arrayAppend		; Append returns the array pointer!
	`dup
	jsr arrayPrint
	`pushTrue
	jsr arrayAppend
	`dup
	jsr arrayPrint

	`pushzero
	jsr arrayAppend		; keep appending to force realloc
	`pushzero
	jsr arrayAppend
	`pushzero
	jsr arrayAppend
	`pushzero
	jsr arrayAppend

	`dup
	jsr arrayPrint
	rts
.scend

.scope
_name:	.byte "*** getAt test ***",0
_msg1:	.byte "item at ",0
_msg2:	.byte " = ",0
getat_test:
	`println _name
	`dup
	`pushi 1
	`print _msg1
	jsr printtos
	jsr arrayGetAt
	`print _msg2
	jsr printtosln
	`drop
	`dup
	`pushi 0
	`print _msg1
	jsr printtos
	jsr arrayGetAt
	`print _msg2
	jsr printtosln
	`drop
	rts
.scend

.scope
_name:	.byte "*** setAt test ***",0
_msg1:	.byte "item at ",0
_msg2:	.byte " = ",0
setat_test:
	`println _name

	`pushi $05
*	`over
	`over
	`dup
	jsr arraySetAt
	`dectos
	`toszero?
	bne -
	`drop

	`dup
	jsr arrayPrint
	rts
.scend

.scope
_name:	.byte "*** delAt test ***",0
_msg1:	.byte "item at ",0
_msg2:	.byte " = ",0
delat_test:
	`println _name
	`dup
	`pushi 1
	jsr arrayDeleteAt

	`dup
	jsr arrayPrint
	rts
.scend

.scope
_name:	.byte "*** findFirst test ***",0
_msg1:	.byte "Found 1 at ",0
_msg2:	.byte "Found 2 at ",0
_msg3:	.byte "Found dead at ",0
findFirst_test:
	`println _name

	`pushi 2
	jsr arrayAppend

	`dup
	jsr arrayPrint

	`dup
	`pushi 01
	jsr arrayFindFirst

	`print _msg1
	jsr printTosln
	`drop

	`dup
	`pushi 02
	jsr arrayFindFirst

	`print _msg2
	jsr printTosln
	`drop

	`dup
	`pushi $dead
	jsr arrayFindFirst
	`print _msg3
	jsr printTosln
	`drop
	rts
.scend

.scope
_name:	.byte "*** findLast test ***",0
_msg1:	.byte "Found 1 at ",0
_msg2:	.byte "Found 2 at ",0
_msg3:	.byte "Found dead at ",0
findLast_test:
	`println _name
	`dup
	jsr arrayPrint

	`dup
	`pushi 01
	jsr arrayFindLast

	`print _msg1
	jsr printTosln
	`drop

	`dup
	`pushi 02
	jsr arrayFindLast

	`print _msg2
	jsr printTosln
	`drop

	`dup
	`pushi $dead
	jsr arrayFindFirst
	`print _msg3
	jsr printTosln
	`drop

	rts
.scend

.scope
_name:	.byte "*** oneItem test ***",0
_msg1:	.byte "Found 1 at ",0
_msg2:	.byte "Found 2 at ",0
_msg3:	.byte "Found $DEAD at ",0
oneItem_test:
	`println _name
	`pushi $05
	jsr arrayAlloc
	`pushi $dead
	jsr arrayAppend
	`dup
	jsr arrayPrint

	`dup
	`pushi 01
	jsr arrayFindLast

	`print _msg1
	jsr printTosln
	`drop

	`dup
	`pushi $dead
	jsr arrayFindLast
	`print _msg3
	jsr printTosln
	`drop
	rts
.scend

.require "../vectors.asm"
