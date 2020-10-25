; -----------------------------------------------------------------------------
; Test for fmath functions under py65mon.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.outfile "tests/mandelbrotTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.require "../../Common/data.asm"

.text
.org $c000

.require "../mandelbrot.asm"

; Main entry point for the test
main:
	ldx #SP0		; Reset stack pointer
	`pushzero
	jsr mockConioInit

	jsr zrSqTest
	jsr ziSqTest
	jsr toCharTest
	jsr countAndTestTest
	jsr doEscapeTest
	jsr doCellTest
	brk

.scope
_name:	.byte "*** zrSq test ***",0
_msg1:	.byte "zr sq of ",0
_msg2:	.byte " = ",0
_tests:	.word $fe00, $ff00, $ff80, $0000, $0080, $0100, $0200
_testsEnd:
.alias _testsCount [_testsEnd - _tests]
zrSqTest:
	`println _name
	ldy #$00
_loop:	lda _tests,y
	`pushA
	iny
	lda _tests,y
	sta TOS_MSB,x
	jsr _test
	iny
	cpy #_testsCount
	bmi _loop
	`printcr
	rts

_test:	`print _msg1
	jsr printTos
	`pop ZREAL
	`print _msg2
	jsr zrSq
	jsr printTosln
	`drop
	rts
.scend

.scope
_name:	.byte "*** ziSq test ***",0
_msg1:	.byte "zi sq of ",0
_msg2:	.byte " = ",0
_tests:	.word $fe00, $ff00, $ff80, $0000, $0080, $0100, $0200
_testsEnd:
.alias _testsCount [_testsEnd - _tests]
ziSqTest:
	`println _name
	ldy #$00
_loop:	lda _tests,y
	`pushA
	iny
	lda _tests,y
	sta TOS_MSB,x
	jsr _test
	iny
	cpy #_testsCount
	bmi _loop
	`printcr
	rts

_test:	`print _msg1
	jsr printTos
	`pop ZIMAG
	`print _msg2
	jsr ziSq
	jsr printTosln
	`drop
	rts
.scend

.scope
_name:	.byte "*** toChar test ***",0
_msg1:	.byte "toChar of ",0
_msg2:	.byte " = ",0
_tests:	.byte $00, $01, $02, $03, $04, $05, $06, $07, $08
_testsEnd:
.alias _testsCount [_testsEnd - _tests]
toCharTest:
	`println _name
	ldy #$00
_loop:	`print _msg1
	lda _tests,y
	pha
	jsr printa
	`print _msg2
	pla
	jsr toChar
	`printcr
	iny
	cpy #_testsCount
	bmi _loop
	`printcr
	rts
.scend

.scope
_name:	.byte "*** countAndTest test ***",0
_msg1:	.byte "count of ",0
_msg2:	.byte " = ",0
countAndTestTest:
	`println _name
	`pushZero
_loop:	`drop
	`print _msg1
	jsr countAndTest
	lda COUNT
	jsr printa
	`print _msg2
	jsr printtosln
	`tosZero?
	beq _loop
	`printcr
	rts
.scend

.scope
_name:	.byte "*** doEscape test ***",0
_msg1:	.byte "doEscape of ",0
_msg2:	.byte " = ",0
_tests:	.word $fd00, $fe00, $ff00, $0000, $0100, $0200, $0300
_testsEnd:
.alias _testsCount [_testsEnd - _tests]
doEscapeTest:
	`println _name
	stz COUNT
	ldy #$00
_loop:	`print _msg1
	lda _tests,y
	`pusha
	iny
	lda _tests,y
	sta TOS_MSB,x
	jsr printTos
	`pop ZREAL
	`pushZero
	`pop ZIMAG
	jsr doEscape
	`print _msg2
	jsr printtosln
	`drop
	iny
	cpy #_testsCount
	bmi _loop
	`printcr
	rts
.scend

.scope
_name:	.byte "*** doCell test ***",0
_msg1:	.byte "asin of ",0
_msg2:	.byte " = ",0
doCellTest:
	`println _name
	rts
.scend

.require "../../Common/tests/mockConio.asm"
.require "../../Common/conio.asm"
.require "../../Common/math16.asm"
.require "../../Common/print.asm"
.require "../../Common/stack.asm"
.require "../../Common/vectors.asm"
