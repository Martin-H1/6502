; -----------------------------------------------------------------------------
; Test for math functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.word $8000
.org $8000
.outfile "tests/mathTest.rom"

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

	jsr add_test
	jsr sub_test
	jsr neg_test
	jsr mul_test
	jsr div_test
	jsr mod_test
	jsr greater_test
	jsr less_test
	jsr equals_test
	jsr andor_test
	jsr minmax_test
	jsr divByTwo_test
	jsr arshift_test
	jsr mstar_test
	brk

true: .byte "True!",0
false: .byte "False!",0

.scope
_name:	.byte "*** add test ***",0
_num1:	.word $dead
_num2:	.word $beef
add_test:
	`println _name
	`push _num1
	`push _num2
	jsr printstack

	`swap
	`pushi printstack
	`execute

	jsr add16
	jsr printstack

	rts
.scend

.scope
_name:	.byte "*** sub test ***",0
sub_test:
	`println _name
	`dup
	jsr sub16
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** neg test ***",0
neg_test:
	`println _name
	`pushi 4
	jsr neg16
	`dup
	jsr neg16
	jsr printstack
	`drop
	`drop
	`pushi 2
	`sToD
	jsr printstack
	jsr neg32
	jsr printstack
	`drop
	`drop
	rts
.scend

.scope
_name:	.byte "*** mul test ***",0
mul_test:
	`println _name
	`pushi 2
	`pushi 3
	jsr mul16
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** div test ***",0
div_test:
	`println _name
	`pushi 10
	`pushi 2
	jsr div16
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** mod test ***",0
mod_test:
	`println _name
	`pushi 14
	`pushi 3
	jsr mod16
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** greater test ***",0
greater_test:
	`println _name

	; -1 1 > should be false
        `pushi $ffff
        `pushi $0001
        `if_greater16
            `println true
	    bra _end_if
_else:
            `println false
_end_if:
	rts
.scend

.scope
_name:	.byte "*** less test ***",0
less_test:
	`println _name
	; -1 1 < should be true
	`pushi $ffff
	`pushi $0001

        `if_less16
            `println true
	    bra _end_if
_else:
            `println false
_end_if:
	rts
.scend

.scope
_name:	.byte "*** equals test ***",0
equals_test:
	`println _name
	; 0400 0400 == is true
	`pushi $0400
	`pushi $0400
	.scope
	`if_equals16
	    `println true
	    bra _end_if
_else:
	    `println false
_end_if:
	.scend

	; -1 1 == should be false
	`pushi $ffff
	`pushi $0001

	.scope
	`if_equals16
	    `println true
	    bra _end_if
_else:
	    `println false
_end_if:
	.scend
	rts
.scend

.scope
_name:	.byte "*** andor test ***",0
andor_test:
	`println _name
	`pushi $00f0
	`pushi $0f00
	jsr or16
	jsr printstack

	`pushi $f0f0
	jsr or16
	jsr printstack

	`pushi $f0f0
	jsr and16
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** min max test ***",0
minmax_test:
	`println _name
	`pushi $00f0
	`pushi $0f00
	jsr min16
	jsr printstack

	`pushi $f0f0
	jsr min16
	jsr printstack

	`pushi $00f0
	jsr max16
	jsr printstack

	`pushi $0ff0
	jsr max16
	jsr printstack
	`drop
	rts
.scend

.scope
_name:	.byte "*** div by two test ***",0
divByTwo_test:
	`println _name
	`pushi $ffff
	jsr divByTwo16
	jsr printtosln

	jsr divByTwo16
	jsr printtosln
	`drop

	`pushi $f0f0
	jsr divByTwo16
	jsr printtosln
	`drop

	`pushi $0010
	jsr divByTwo16
	jsr printtosln
	jsr divByTwo16
	jsr printtosln
	jsr divByTwo16
	jsr printtosln
	`drop
	rts
.scend

.scope
_name:	.byte "*** arshift test ***",0
arshift_test:
	`println _name
	`pushi $ffff
	`pushi 0
	jsr arshift16
	jsr printtosln
	`drop

	`pushi $ffff
	`pushi 2
	jsr arshift16
	jsr printtosln
	`drop

	`pushi $f0f0
	`pushi 3
	jsr arshift16
	jsr printtosln
	`drop

	`pushi $0080
	`pushi $0003
	jsr arshift16
	jsr printtosln
	`drop
	jsr printstack
	rts
.scend

.scope
_name:	.byte "*** mstar test ***",0
mstar_test:
	`println _name
	`pushi $ffff
	`pushi 0
	jsr mstar
	jsr printstack
	`drop
	`drop

	`pushi $0ff0
	`pushi $0008
	jsr mstar
	jsr printstack
	`drop
	`drop

	`pushi $000f
	`pushi $0008
	jsr mstar
	jsr printstack
	`drop
	`drop
	rts
.scend

.require "../vectors.asm"
