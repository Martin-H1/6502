; This module is an implementation of the IEEE 745 floating point code
; for the Binary 32 format. 
;
; This module uses the page zero temporary location and returns output.
;
; Consisting of:
;	fcompare32 - 32 bit compare of floats to set flag bits.
;	fln32 - 32 bit natural log function.
;	flog32 - 32 bit common log function.
;	float32 - converts 16 bit integer to 32 bit float
;	fixed32 - converts 32 bit float to 16 bit int.
;	fadd32 - adds two 32 bit floats
;	fsub32 - subtracts two 32 bit floats
;	fmul32 - multiplies two 32 bit floats
;	fdiv32 - divides two 32 bit floats
;	fexp32 - 32 bit exponential function (e**x)
;	fsin32, fcos32, ftan32, fatn32- tbd

;
; Floating point representation (4-bytes)
;	sign bit - byte 1
;	exponent - byte 1-2
;	significand - bytes 2-4
;
;  Exponent    Significand
;  SEEEEEEE  E.MMMMMMM  MMMMMMMM  MMMMMMMM
;     n         n+1       n+2       n+3
;
; sign:
;	A sign bit in the msb of high-order byte.
;
; exponent:
;	It represents powers of two using the offset-binary representation.
;	0 - means zero
; 	01-ff - subtract 127 to get actual exponent.
;	ff - +-1 infinity.
;
; significand:
;	The sign bit is stored in a separate byte, and normalized so it is
;	always in the range of 1 to 2. This allows inferring the leading 1
;	for 24 bits of precision in 23 bits, so only the mantissa is present.
;
; This allows the representation of decimal numbers in the approximate range of
; 1.17 x 10**(-38) through 3.4 x 10**(38) with 6 to 9 significant digits.

; Use scope operator to hide all internal entry points.
.scope

;
; Aliases to the floating point registers and scratch area in page zero.
;
.alias _fpreg1	FPAREA		; reg1 is at the base of the scratch area.
.alias _fpreg2	FPAREA + $05	; reg2 is five bytes into the scratch area.
.alias _fpreg3	FPAREA + $0A	; reg2 is ten bytes into the scratch area.

;
; Aliases to field offsets within an unpacked float. Once unpacked a float
; contains an eight bit exponent, and 32 bits two compliment significand.
; The extended precision helps reduce truncation errors while normalizing.
;
.alias _exp	$00		; Exponent in binary offset format.
.alias _sgnifcd	$01		; twos compliment significand

;
; Common useful floating point constants.
;

M_E:		.byte $40,$2d,$f8,$54	; 2.718282
M_INFINITY:	.byte $7f,$80,$00,$00	; infinity
M_LOG2E:	.byte $3f,$b8,$aa,$3b	; 1.442695
M_LOG10E:	.byte $3e,$de,$5b,$d9	; 0.434294
M_LN2:		.byte $3f,$31,$72,$18	; 0.693147
M_LN10:		.byte $40,$13,$5d,$8e	; 2.302585
M_NAN:		.byte $7f,$c0,$00,$00	; nan
M_PI:		.byte $40,$49,$0f,$db	; 3.141593
M_PI_2:		.byte $3f,$c9,$0f,$db	; 1.570796
M_PI_4:		.byte $3f,$49,$0f,$db 	; 0.785398
M_1_PI:		.byte $3e,$a2,$f9,$83	; 0.318310
M_2_PI:		.byte $3f,$22,$f9,$83	; 0.636620
M_2_SQRTPI:	.byte $3f,$90,$6e,$bb	; 1.128379
M_SQRT2:	.byte $3f,$b5,$04,$f3	; 1.414214
M_SQRT1_2:	.byte $3f,$35,$04,$f3	; 0.707107

;
; External entry points here.
;
fcompare32:
	rts

fln32:
	rts

flog32:
	rts

;
; Accepts a 16 bit integer on the stack, converts it to a float.
; Returns a pointer to the 32 bit converted value.
;
float32:
	rts

fixed32:
	rts

.macro exitIfInfinity
	lda _1+_exp
	cmp #$ff
	beq _returnInfinity
.macend

_returnInfinity:
	`pushi M_INFINITY
	rts

fadd32:
	jsr _unpack_to_freg1	; Unpack arguments into usable form.
	jsr _swap
	jsr _unpack_to_freg1
	clc
	`exitIfInfinity _fpreg2
	`exitIfInfinity _fpreg1
	cmp _fpreg2		; Compare exponents.
	beq +			; If equal do the add.
	jsr _alignsgnifcd	; Otherwise align significands.
*	jsr _add		; Add the aligned significands
	`pushi _fpreg3		; pack up the results and return
	`savetos tmpptr1
	jsr _pack_freg1
	rts

;
; The exponents aren't equal, so shift the bits to normalize them.
;
_alignsgnifcd:
	rts

;
; Add the significands only
;
_add:
.scope
	clc			; clear carry
	ldy #$03		; index for 4 byte add
*	lda _fpreg1+_sgnifcd,y
	adc _fpreg2+_sgnifcd,y	; add a byte at a time.
	sta _fpreg1+_sgnifcd,y
	dey			; advance index to next more signif.byte
	bpl -			; loop until done.
	rts			; return
.scend

;
; subtracts two floating points by negating the value pointed to by TOS.
; It then calls fadd32 to do the actual computation.
;
fsub32:
	`savetos tmpptr1	; make a copy of the value
	`drop
	`pushi _fpreg3		; use _fpreg3 as working space.
	ldy #$03
*	lda (tmpptr1),y
	sta _fpreg3,y
	dey
	bpl -
	lda _fpreg3
	eor #$80		; compliment sign bit.
	sta _fpreg3
	jsr printfloat
	`printcr
	jmp fadd32

fmul32:
	rts

fdiv32:
	rts

fexp32:
	rts

; takes a pointer to a float on the stack in fully normalized form and
; unpacks it into the 40 bit floating point scratch register. In the
; process it converts the significand to two's compliment.
_unpack_to_freg1:
.scope
	.invoke savetos TMPPTR1
	.invoke drop
	lda (TMPPTR1)		; Load the sign bit and bits 7-1 of exponent.
	asl			; shift sign bit into carry bit.
	sta _fpreg1+_exp	; store bits 7-1 in the exponent 1 field.
	bcs _set_sign
	lda #$01		; clear sign bit and set implied bit 24
	bra _exp0
_set_sign:
	lda #$81		; set sign bit set and set implied bit 24
_exp0:				; Need to handle bit 0 of exponent.
	sta _fpreg1+_sgnifcd
	ldy #01
	lda (TMPPTR1), y	; load high byte of significand which
	bpl _significand	; contains bit 0 of exponent.
	inc _fpreg1+_exp	; sign bit set means low bit of exponent should be set.
_significand:
	sta _fpreg1+_sgnifcd,y
	iny			; process next byte of significand.
	cpy #04
	beq +
	lda (TMPPTR1), y
	bra _significand
*
	lda _fpreg1+_sgnifcd
	bpl _exit
	and #$7f		; clear sign bit as it will be reset.
	sta _fpreg1+_sgnifcd
	bra _twosCompliment	; If negative then twos compliment.
_exit:	rts
.scend

;
; _twosCompliment - changes the significand contents in fpreg1 to twos
; compliment using the sign bit in MSB.
;
_twosCompliment:
.scope
	lda _fpreg1+_sgnifcd
	and #$7f
	sta _fpreg1+_sgnifcd
	clc
	ldy #03
	lda _fpreg1+_sgnifcd,y
	eor #$FF
	adc #$01
	sta _fpreg1+_sgnifcd,y
	dey
_loop:
	lda _fpreg1+_sgnifcd,y
	eor #$FF
	adc #$00
	sta _fpreg1+_sgnifcd,y
	dey
	bpl _loop
	rts
.scend

; packs the 40 bit contents of floating point scratch regsiter freg1 into
; a buffer pointed to by tmpptr1. In the process it converts the significand
; from two's compliment into the 23 bit form.
_pack_freg1:
.scope
	lda #0
	sta (tmpptr1)		; assume a positive sign.
	lda _fpreg1+_sgnifcd	; get the sign bit.
	bpl _significand
	jsr _twosCompliment	; negative requires conversion.
	lda #$80		; set sign bit as well.
	sta (tmpptr1)
_significand:
	ldy #03
*
	lda _fpreg1+_sgnifcd,y
	sta (tmpptr1),y		; copy three significand bytes
	dey
	bne -

	ldy #$01		; merge the exponent with byte 1.
	lda _fpreg1		; load the exponent
	lsr 			; shift bits 7-1 into place.
	ora (tmpptr1)		; and merge with sign bit
	sta (tmpptr1)
	lda #00
	bcc +			; handle bit 0 in carry
	lda #$80
	ora (tmpptr1),y
	bra _exit
*
	lda #$7f
	and (tmpptr1),y
_exit:
	sta (tmpptr1),y
	rts
.scend

;
; swaps the contents of the two floating point registers.
;
_swap:
.scope
	phx
	ldx #$04		; count for 5-byte swap.
_loop:
	lda _fpreg2,x		; A and Y allow swap without a temporary location.
	ldy _fpreg1,x
	sta _fpreg1,x
	sty _fpreg2,x
	dex
	bpl _loop
	plx
	rts
.scend

;
; _lshift - shifts the significand contents pointed to by tmpptr1 to the
; left a number of bits in the accumulator.
;
_lshift:
.scope
	rts
.scend

;
; _rshift - shifts the significand contents pointed to by tmpptr1 to the
; right a number of bits in the accumulator.
;
_rshift:
.scope
	rts
.scend

fprintfpreg1:
	ldy #0
*	lda _fpreg1, y
	jsr printa
	iny
	cpy #05
	bne -
	.invoke printcr
	rts

fprintfpreg2:
	ldy #0
*	lda _fpreg2, y
	jsr printa
	iny
	cpy #05
	bne -
	.invoke printcr
	rts

.scend
