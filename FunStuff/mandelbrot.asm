; -----------------------------------------------------------------------------
; Mandelbrot in ASCI art. It uses 8.8 fixed point mathematics in a 16 bit cell
; instead of floating point. It depends upon the stack, math, and I/O functions
; defined in the other modules.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;

; Setup constants to remove magic numbers to allow
; for greater zoom with different scale factors.
.alias MAXITER	20
.alias IMAXVAL	$0170
.alias IMINVAL	$FE90
.alias ISTEP	$0010
.alias RMINVAL	$FE00
.alias RMAXVAL	$0110
.alias RSTEP	$000A
.alias ESCAPE	$0400

; Alternate constanst for 4.12 fixed point.
; .alias MINVAL $E000
; .alias MAXVAL $2000
; .alias ISTEP  $01A0
; .alias RSTEP  $00D0
; .alias ESCAPE $4000

;
; Data segments
;
.data BSS
.space CREAL 2			; These variables hold values during the
.space CIMAG 2			; escape calculation.
.space ZREAL 2
.space ZIMAG 2
.space COUNT 1			; current escape count

.text

;
; Macros
;
.macro rescale
	lda NOS_MSB,x
	sta NOS_LSB,x
	lda TOS_LSB,x
	sta NOS_MSB,x
	`drop
.macend

;
; Functions
;

; Main entry point
mandelbrot:
.scope
	`printcr
	`pushi IMAXVAL		; Start iteration at max imaginary
_do:	jsr doRow		; For each row in the set.
	`printcr
	`pushi ISTEP
	jsr sub16
	`dup
	`pushi IMINVAL
	jsr compare16
	bmi _do
	`drop
	rts
.scend

; For each cell in a row.
doRow:
.scope
	`pushi RMINVAL		; Start iteration at max imaginary
_do:	jsr doCell		; For each row in the set.
	`pushi RSTEP
	jsr add16
	`dup
	`pushi RMAXVAL
	jsr compare16
	bpl _do
	`drop
	rts
.scend

; Iterates on a single cell to compute its escape factor.
; input - row and column on stack.
; output - none
doCell:
	jsr initVars
	`pushZero		; set the loop up for repeating
_while:
	`drop			; drop status from last iteration.
	jsr doEscape
	`tosZero?
	beq _while
	`drop
	lda COUNT
	jsr toChar
	rts

; stores the row column values from the stack for the escape calculation.
; input - two cells on stack
; output - none
initVars:
	`over
	`over
	`pop CREAL
	`pop CIMAG
	`pushzero
	`peek ZREAL
	`pop ZIMAG
	lda #$01
	sta COUNT
	rts

; Performs a single iteration of the escape calculation.
; input - implicit from memory.
; ouput - true or false.
doEscape:
.scope
	jsr zrSq
	jsr ziSq
	`over
	`over
	jsr add16
	`pushi ESCAPE		; Numbers >= 4 will always escape
	jsr compare16
	beq +
	bpl _else
*	`drop			; cleanup the stack and return.
	`drop
	`pushTrue
	rts
_else:
	jsr sub16		; Squared i yeilds negative, so subtract
	`push CREAL
	jsr add16		; add real components and leave result on stack
	`push ZREAL		; 2 * ZREAL * ZIMAG
	`push ZIMAG
	jsr mstar
	`rescale
	`pushi 1
	jsr lshift16
	`push CIMAG
	jsr add16
	`pop ZIMAG		; Store stack item into ZIMAG and ZREAL
	`pop ZREAL
	jmp countAndTest
.scend

; Compute squares, but rescale to remove extra scaling factor.
; input - implicit from memory
; output - value on stack
zrSq:
	`push ZREAL
	`dup
	jsr mstar		; rescale by shifting one byte
	`rescale
	rts

; Compute squares, but rescale to remove extra scaling factor.
; input - implicit from memory
; output - value on stack
ziSq:
	`push ZIMAG
	`dup
	jsr mstar		; rescale by shifting one byte
	`rescale
	rts

; Translate escape count to ascii greyscale.
; input - value in accumulator
; output - character to console.
toChar:
	phy
	tay
	lda _charVals,y
	jsr putch
	ply
	rts
_charVals:
	.byte " ..,'~!^:;[/<&?oxOX#  "

; Increment count and compare to max iterations.
; input - implicit from memory
; output - true or false on stack
countAndTest:
.scope
	inc COUNT
	lda COUNT
	`pushA
	`pushi MAXITER
	`if_greater16
	`pushTrue
	rts
_else:	`pushZero
	rts
.scend

; Rescales the 32 bit value into a 16 bit 4.12 fixed point number.
_rescale:
	phy
	ldy #$04		; Right shift 4 bits
*	lda TOS_MSB,x
	asl
	ror TOS_MSB,x
	ror TOS_LSB,x
	ror NOS_MSB,x
	ror NOS_LSB,x
	dey
	bne -
	lda NOS_MSB,x		; right shift one byte
	sta NOS_LSB,x
	lda TOS_LSB,x
	sta NOS_MSB,x
	`drop
	ply
	rts
	
.scend
