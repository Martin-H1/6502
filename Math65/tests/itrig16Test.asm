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
	jsr atanTest
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

.scope
_name:	.byte "*** asin16 test ***",0
_msg1:	.byte "asin of ",0
_msg2:	.byte " = ",0
_sineValuesToTest:
	.word $A57E, $0000, $0295, $257E, $5A82, $7fff, $8001

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

.scope
_name:	.byte "*** acos16 test ***",0
_msg1:	.byte "asin of ",0
_msg2:	.byte " = ",0
_cosineValuesToTest:
	.word $A57E, $0000, $0295, $257E, $5A82, $7fff, $8001

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

.scope
_name:	.byte "*** atan test ***",0
_msg1:	.byte "atan2 of [X = ",0
_msg2:	.byte ", Y = ",0
_msg3:	.byte "] = ",0
_xVals:	.byte 3, 3, 4, 0,  4, $fb, $fb,   0, 4
_yVals:	.byte 0, 4, 4, 4, $fc,  0, $fb, $fb, $fb
_yValsEnd:
.alias _yValsCount [_yValsEnd - _yVals]

atanTest:
	`println _name
	ldy #$00
_loop:	`pushZero
	lda _yVals,y
	sta TOS_MSB,x
	`pushZero
	lda _xVals,y
	sta TOS_MSB,x
	jsr _test
	iny
	cpy #_yValsCount
	bmi _loop
_exit:	jsr printstack
	rts

_test:	`print _msg1
	jsr printTosSigned
	`print _msg2
	`swap
	jsr printTosSigned
	`print _msg3
	`swap
	jsr atan216
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
