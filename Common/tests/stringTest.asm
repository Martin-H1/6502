; -----------------------------------------------------------------------------
; Test for string functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/stringTest.rom"

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

; Main entry point for the interpreter.
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit

        `pushi heap_size
        `pushi heap_base
        jsr hinit

	jsr alpha_test
	jsr ws_test
	jsr number_test
	jsr operator_test
	jsr sign_test
	jsr strlen_test
	jsr strcmp_test
	jsr strchr_test
	jsr strcpy_test
	jsr strtoi_test
	brk

.scope
_name:	.byte "*** alpha_test ***",0
_data:	.byte " 0AZaz!",0
_is:	.byte "It is alphabetic.",0
_isnot:	.byte "It is not alphabetic!",0

alpha_test:
	`println _name
	`pushi _data
	jsr mockConioInit
_for:	jsr getch
	beq _endfor
	jsr isalpha
	bne +
	`println _is
	bra _for
*	`println _isnot
	bra _for
_endfor:
	rts
.scend

.scope
_name:	.byte "*** number_test ***",0
_data:	.byte " 0123456789A!",0
_is:	.byte "It is numeric.",0
_isnot:	.byte "It is not numeric!",0

number_test:
	`println _name
	`pushi _data
	jsr mockConioInit
_for:	jsr getch
	beq _endfor
	jsr isdigit
	bne +
	`println _is
	bra _for
*	`println _isnot
	bra _for
_endfor:
	rts
.scend

.scope
_name:	.byte "*** ws_test ***",0
_data:	.byte " \t\v\f\rA",0
_is:	.byte "It is white space.",0
_isnot:	.byte "It is not white space.",0

ws_test:
	`println _name
	`pushi _data
	jsr mockConioInit
_for:	jsr getch
	beq _endfor
	jsr isspace
	bne +
	`println _is
	bra _for
*	`println _isnot
	bra _for
_endfor:
	rts
.scend

.scope
_name:	.byte "*** operator test ***",0
_data:	.byte "aa+ - * / = < > <=>=bb",0
_is:	.byte " is an operator.",0
_isnot:	.byte " is not an operator",0

.macro doOperatorTest
.macend

operator_test:
	`println _name
	`pushi _data
	jsr mockConioInit
_while:	jsr getch
	beq _endwhile
	`pushA
	jsr putch
	jsr getch
	sta TOS_MSB,x
	jsr putch
	jsr isoperator
	bne +
	`println _is
	bra _while
*	`println _isnot
	bra _while
_endwhile:
	`printcr
	rts
.scend

.scope
_name:	.byte "*** sign test ***",0
_is:	.byte " is a sign.",0
_isnot:	.byte " is not a sign",0

.macro doSignTest
	lda #_1
	jsr putch
	jsr issign
	bne _else
	`println _is
	bra _endif
_else:
	`println _isnot
_endif:
.macend

sign_test:
	`println _name
	`doSignTest '0
	`doSignTest 'a
	`doSignTest '\+
	`doSignTest '\-
	`printcr
	rts
.scend

.scope
_name:	.byte "*** strlen test ***",0
strlen_test:
	`println _name
	`pushi hello_world
	jsr strlen
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** strcmp test ***",0
strcmp_test:
	`println _name
	`pushi hello_world
	`pushi hello_world
	jsr strcmp
	jsr printa
	`printcr

	`pushi goodbye_world
	`pushi hello_world
	jsr strcmp
	jsr printa
	`printcr

	`pushi hello_world
	`pushi goodbye_world
	jsr strcmp
	jsr printa
	`printcr
	`printcr
	rts
.scend

.scope
_name:	.byte "*** strchr test ***",0
strchr_test:
	`println _name
	`pushi test_buffer
	`pushi test_buffer
	lda #'\-
	jsr strchr
	jsr printstack
	`swap
	jsr sub16
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** strcpy test ***",0
strcpy_test:
	`println _name
	`pushi test_buffer
	`pushi goodbye_world
	jsr printstack
	jsr strcpy
	`dup
	jsr cputs
	`printcr
	jsr printstack

	jsr strdup
	`dup
	jsr cputs
	`printcr
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** strtoi test ***",0
_msg:	 .byte " converts to ",0
strtoi_test:
	`println _name
	`pushi _zero
	jsr strtoi
	`print _zero
	`print _msg
	jsr printtosln
	`drop

	`pushi _one
	jsr strtoi
	`print _one
	`print _msg
	jsr printtosln
	`drop

	`pushi _nine
	jsr strtoi
	`print _nine
	`print _msg
	jsr printtosln
	`drop

	`pushi _ten
	jsr strtoi
	`print _ten
	`print _msg
	jsr printtosln
	`drop

	`pushi _t567
	jsr strtoi
	`print _t567
	`print _msg
	jsr printtosln
	`drop

	`pushi _tn1
	jsr strtoi
	`print _tn1
	`print _msg
	jsr printtosln
	`drop

	rts

_zero:	.byte "0",0
_one:	.byte "1",0
_nine:	.byte "9",0
_ten:	.byte "10",0
_t567:	.byte "+567",0
_tn1:	.byte "-1",0
.scend

goodbye_world: .byte "Goodbye World!",0
hello_world: .byte "Hello World!",0
test_buffer: .byte   "    -         ",0

.require "../vectors.asm"
