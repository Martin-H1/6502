; -----------------------------------------------------------------------------
; Test for fmath functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.outfile "tests/itrig16Test.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.require "../../Common/data.asm"

.text
	.org $c000

.require "../itrig16.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit

	jsr sinTest
	jsr cosTest
	jsr tanTest
	jsr asinTest
	jsr acosTest
	brk

_anglesToTest:
	.word [[$ffff^[FULL_ROTATION+ACUTE_ANGLE]]+1]	; -$480
	.word [[$ffff^ACUTE_ANGLE]+1]			; -$80
	.word $0000, ACUTE_ANGLE, RIGHT_ANGLE, RIGHT_ANGLE+ACUTE_ANGLE
	.word STRAIGHT_ANGLE, RIGHT_ANGLE+STRAIGHT_ANGLE
	.word RIGHT_ANGLE+STRAIGHT_ANGLE+ACUTE_ANGLE, FULL_ROTATION
	.word FULL_ROTATION+ACUTE_ANGLE, 2*FULL_ROTATION+ACUTE_ANGLE

.scope
_name:	.byte "*** sin16 test ***",0
_msg1:	.byte "sin of ",0
_msg2:	.byte " = ",0
sinTest:
	`println _name
	ldy #$00
_loop:	lda _anglesToTest,y
	`pusha
	iny
	lda _anglesToTest,y
	sta TOS_MSB,x
	jsr _test
	iny
	cpy #22
	bmi _loop
	`printcr
	rts

_test:	`print _msg1
	jsr printTosSigned
	`print _msg2
	jsr sin16
	jsr printTosSignedln
	`drop
	rts
.scend

.scope
_name:	.byte "*** cos16 test ***",0
_msg1:	.byte "cos of ",0
_msg2:	.byte " = ",0
cosTest:
	`println _name
	ldy #$00
_loop:	lda _anglesToTest,y
	`pusha
	iny
	lda _anglesToTest,y
	sta TOS_MSB,x
	jsr _test
	iny
	cpy #22
	bmi _loop
	`printcr
	rts

_test:	`print _msg1
	jsr printTosSigned
	`print _msg2
	jsr cos16
	jsr printTosSignedln
	`drop
	rts
.scend

.scope
_name:	.byte "*** tan16 test ***",0
_msg1:	.byte "tan of ",0
_msg2:	.byte " = ",0
tanTest:
	`println _name
	`pushi [[$ffff^[ACUTE_ANGLE]]+1]
_loop:	`dup
	`pushi STRAIGHT_ANGLE
	`if_less16
	`dup
	jsr _test
	`pushi [10*DEGREE_ANGLE]
	jsr add16
	bra _loop
_else:	`printcr
	`drop
	rts

_test:	`print _msg1
	jsr printTosSigned
	`print _msg2
	jsr tan16
	jsr printTosSignedln
	`drop
	rts
.scend

_sineValuesToTest:
	.word $A57E, $0000, $0295, $257E, $5A82, $7fff, $8001

.scope
_name:	.byte "*** asin16 test ***",0
_msg1:	.byte "asin of ",0
_msg2:	.byte " = ",0
asinTest:
	`println _name
	ldy #$00
_loop:	lda _sineValuesToTest,y
	`pusha
	iny
	lda _sineValuesToTest,y
	sta TOS_MSB,x
	jsr _test
	iny
	cpy #14
	bmi _loop
	`printcr
	rts

_test:	`print _msg1
	jsr printTosSigned
	`print _msg2
	jsr asin16
	jsr printTosSignedln
	`drop
	rts
.scend

_cosineValuesToTest:
	.word $A57E, $0000, $0295, $257E, $5A82, $7fff, $8001

.scope
_name:	.byte "*** acos16 test ***",0
_msg1:	.byte "asin of ",0
_msg2:	.byte " = ",0
acosTest:
	`println _name
	ldy #$00
_loop:	lda _cosineValuesToTest,y
	`pusha
	iny
	lda _cosineValuesToTest,y
	sta TOS_MSB,x
	jsr _test
	iny
	cpy #14
	bmi _loop
	`printcr
	rts

_test:	`print _msg1
	jsr printTosSigned
	`print _msg2
	jsr acos16
	jsr printTosSignedln
	`drop
	rts
.scend

.require "../../Common/tests/mockConio.asm"
.require "../../Common/conio.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/vectors.asm"
