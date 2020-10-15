; -----------------------------------------------------------------------------
; Sine table for 16 bit integer trig functions scaled by 32,767 with 256
; samples per quadrent and indexed using binary radians.
; See https://en.wikipedia.org/wiki/Binary_scaling for more information.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

;
; Aliases
;

; Values for common angles in brad10

.alias DEGREE_ANGLE	$0003	; approximate, the exact value is 2.84
.alias ACUTE_ANGLE	$0080	; 45 degree angle (useful for binary search).
.alias RIGHT_ANGLE	$0100
.alias STRAIGHT_ANGLE	$0200
.alias FULL_ROTATION	$0400

; clampAngle16 takes brads as input and returns an angle that is positive and
; always less than a FULL_ROTATION. That way we can be sure to use the angle
; with the trig functions.
; input - angle to clamp between $0000 to $0400
; output - clamped angle
clampAngle16:
.scope
	lda TOS_MSB,x		; if angle is negative, then make positive
	bpl _positive
	`pushi FULL_ROTATION
	jsr add16
	bra clampAngle16	; continue rotation until angle is positive
_positive:
	`dup
	`pushi FULL_ROTATION
	`if_greater16
	`pushi FULL_ROTATION
	jsr sub16
	bra _positive
_else:
	rts
.scend

; Sine takes brads as input and returns a scaled sine value. However, to
; save memeory the sine table only contains one quadrant, angles in other
; quadrants are mapped onto the first with sign modification if needed.
; Note: be sure to use 32 bit multiply with the return value!
; input - angle to take sine of.
; output - result
sin16:
.scope
	jsr clampAngle16	; make pos and < full rotation
	lda TOS_MSB,x		; determine angle quadrent.
	and #>[RIGHT_ANGLE+STRAIGHT_ANGLE]
	beq _quadrentOne
	cmp #$01
	beq _quadrentTwo
	cmp #$02
	beq _quadrentThree
	bra _quadrentFour

_quadrentOne:
_tableLookup:
	asl TOS_LSB,x		; convert angle to offset into sin table.
	rol TOS_MSB,x
	`pushi sineTable
	jsr add16
	`fetch			; sineTable [ 2 * angle ]
	rts

_quadrentTwo:
	`pushi STRAIGHT_ANGLE	; reflect angle and lookup.
	`swap
	jsr sub16
	bra _tableLookup

_quadrentThree:
	`pushi STRAIGHT_ANGLE	; rotate angle and lookup
	jsr sub16
	jsr _tableLookup
	jmp neg16		; correct sign

_quadrentFour:
	`pushi FULL_ROTATION
	`swap
	jsr sub16
	jsr _tableLookup
	jmp neg16
.scend

; cosine is defined in terms of sine by coordinate rotation.
; input - angle to take cosine of.
; output - result
cos16:
	`pushi RIGHT_ANGLE	; cos = sin ( angle + RIGHT_ANGLE )
	jsr add16
	jsr sin16
	rts

; tangent can be derived from sin / cos. The good news is both sine and
; cosine are fast table lookups, so this is only a division more expensive.
; But both our sin and cos values are scaled quantities, a division result
; in an unscaled quantity. To prevent this, extend to 32 bits, and use bit
; shifting to multiply by the scale factor again.
; input - angle to take tanget of.
; output - result
tan16:
	`peekToR
	jsr sin16
	`pushZero		; extend to 32 bits to set up for multiply
	`swap			; Forth uses little endian, so this is a multiply.
	lda TOS_MSB,x		; Now divide by two and we multiplied by 32,767!
	cmp #$80		; copy sign bit of A into carry
        ror TOS_MSB,x
        ror TOS_LSB,x
        ror NOS_MSB,x
        ror NOS_LSB,x
	`pushFromR
	jsr cos16
	jsr smrem		; 32 bit divided by 16 for 16 bit result.
	`nip
	rts

.alias MAX_SIN RIGHT_ANGLE
.alias MIN_SIN [[$ffff^RIGHT_ANGLE]+1]

; clamps n between max and min to prevent inverse trig function overflow.
; ( n -- clamped_value )
clampSin16:
	`pushi MIN_SIN
	jsr max16
	`pushi MAX_SIN
	jsr min16
	rts

; Compute asin via a binary search. Note: we're shifting angles up two bits
; to allow for greater precision during the search. It will be shifted back
; before return.
asin16:
.scope
	jsr clampSin16
	`pushZero		; push the first approximation and correction
	`pushi ACUTE_ANGLE
;; 	4*
	;; refine approximation and correction iteratively
	ldy #13
_do:	
        `mrot			; move the correction to the bottom of stack.

	`over			; compare the sine of the approximation to the value.
	`over
	;; 2/ 2/
	jsr sin16
	`if_greater16
	`rot			; The approximation is too small, add the correction.
        `dup
	`mrot
	jsr add16
	bra _endif
_else:
	`rot			; The approximation is too large, so decrease it.
	`dup
	`mrot
	jsr sub16
_endif:
	`swap			; half the correction factor for next iterration.
	;;  2/
	dey
	bpl _do
	`drop			; return only the approximation.
	`swap
	`drop
	;; 2/ 2/
	rts
.scend

; acos is defined in terms of asin with coordinate rotation.
acos16:
	jsr asin16
	`pushi RIGHT_ANGLE
	jsr sub16
	jsr neg16

; 13-bit fixed point four-quadrant arctangent. Given Cartesian vector (x, y),
; finds the angle subtended by the vector and the positive x-axis.
; input - 16 bit signed Y at NOS and 16 bit signed X at TOS
; output - binary radian angle
atan216:
.scope
.scope
	`tosZero?		; Handle crossing x axis case
	bne _endif
	`drop
	`pushi RIGHT_ANGLE
	`swap			; adjust sign based upon y's sign.
	lda TOS_MSB,x
	bpl +
	jsr neg16
*	rts
_endif:
.scend
	`dup			; save x to use its sign to adjust results
	`mrot
	`swap
	; 15 lshift swap /	; push the first approximation and correction
	`pushZero
	`pushi ACUTE_ANGLE
	; 4*

	ldy #13			; refine approximation and correction iteratively
_do:
	`mrot			; move the correction to the bottom of stack.
	`over			; compare the sine of the approximation to the value.
	`over
	;; 2/ 2/
	jsr tan16
	;; >
.scope
	`if_greater16
	`rot			; The approximation is too small, add the correction.
	`dup
	`mrot
	jsr add16
	bra _endif
_else:
	`rot			; The approximation is too large, so decrease it.
	`dup
	`mrot
	jsr sub16
_endif:
.scend
	`swap			; half the correction factor for next iterration.
	;; 2/
	dey
	bpl _do
	`drop			; return only the approximation.
	`swap
	`drop
	;; 2/ 2/
	`swap
	lda TOS_MSB,x
	bpl +			; if x's sign was negative, then
	`pushi STRAIGHT_ANGLE	; move results into 3 & 4 quadrants.
	jsr add16
*	rts
.scend

; arcsine can be computed via a binary search of the table and
; then right shfiting the index to get the angle.

sineTable:
	.word $0000, $00C9, $0192, $025B, $0324, $03ED, $04B6, $057E
	.word $0647, $0710, $07D9, $08A1, $096A, $0A32, $0AFB, $0BC3
	.word $0C8B, $0D53, $0E1B, $0EE3, $0FAB, $1072, $1139, $1200
	.word $12C7, $138E, $1455, $151B, $15E1, $16A7, $176D, $1833
	.word $18F8, $19BD, $1A82, $1B46, $1C0B, $1CCF, $1D93, $1E56
	.word $1F19, $1FDC, $209F, $2161, $2223, $22E4, $23A6, $2467
	.word $2527, $25E7, $26A7, $2767, $2826, $28E5, $29A3, $2A61
	.word $2B1E, $2BDB, $2C98, $2D54, $2E10, $2ECC, $2F86, $3041
	.word $30FB, $31B4, $326D, $3326, $33DE, $3496, $354D, $3603
	.word $36B9, $376F, $3824, $38D8, $398C, $3A3F, $3AF2, $3BA4
	.word $3C56, $3D07, $3DB7, $3E67, $3F16, $3FC5, $4073, $4120
	.word $41CD, $4279, $4325, $43D0, $447A, $4523, $45CC, $4674
	.word $471C, $47C3, $4869, $490E, $49B3, $4A57, $4AFA, $4B9D
	.word $4C3F, $4CE0, $4D80, $4E20, $4EBF, $4F5D, $4FFA, $5097
	.word $5133, $51CE, $5268, $5301, $539A, $5432, $54C9, $555F
	.word $55F4, $5689, $571D, $57B0, $5842, $58D3, $5963, $59F3
	.word $5A81, $5B0F, $5B9C, $5C28, $5CB3, $5D3D, $5DC6, $5E4F
	.word $5ED6, $5F5D, $5FE2, $6067, $60EB, $616E, $61F0, $6271
	.word $62F1, $6370, $63EE, $646B, $64E7, $6562, $65DD, $6656
	.word $66CE, $6745, $67BC, $6831, $68A5, $6919, $698B, $69FC
	.word $6A6C, $6ADB, $6B4A, $6BB7, $6C23, $6C8E, $6CF8, $6D61
	.word $6DC9, $6E30, $6E95, $6EFA, $6F5E, $6FC0, $7022, $7082
	.word $70E1, $7140, $719D, $71F9, $7254, $72AE, $7306, $735E
	.word $73B5, $740A, $745E, $74B1, $7503, $7554, $75A4, $75F3
	.word $7640, $768D, $76D8, $7722, $776B, $77B3, $77F9, $783F
	.word $7883, $78C6, $7908, $7949, $7989, $79C7, $7A04, $7A41
	.word $7A7C, $7AB5, $7AEE, $7B25, $7B5C, $7B91, $7BC4, $7BF7
	.word $7C29, $7C59, $7C88, $7CB6, $7CE2, $7D0E, $7D38, $7D61
	.word $7D89, $7DB0, $7DD5, $7DF9, $7E1C, $7E3E, $7E5E, $7E7E
	.word $7E9C, $7EB9, $7ED4, $7EEF, $7F08, $7F20, $7F37, $7F4C
	.word $7F61, $7F74, $7F86, $7F96, $7FA6, $7FB4, $7FC1, $7FCD
	.word $7FD7, $7FE0, $7FE8, $7FEF, $7FF5, $7FF9, $7FFC, $7FFE
	.word $7FFF

.scend
