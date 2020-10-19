; -----------------------------------------------------------------------------
; Sine table for 16 bit integer trig functions scaled by 2^15 with 256
; samples per quadrent and indexed using binary radians.
; See https://en.wikipedia.org/wiki/Binary_scaling for more information.
;
; Copyright (c) 2020 by Martin Heermance <mheermance@gmail.com>
; MIT Licensed
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

;
; Data segments
;
.data BSS
.space _x		2	; CORDIC's adjusted x value.
.space _y		2	; CORDIC's adjusted y value.
.space _loopNum		1	; CORDIC's iteration count.
.text				; revert back to code segment.

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
; 
; Note: Be sure to use 32 bit multiply with the return value and scale
; down by 15 bits.
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

; tangent is done using table lookup for speed rather than sin / cos. In addition,
; tangent goes to infinity as you approach a multiple of a right angle, and that's easier
; to handle without division. This function returns a value with 1 sign bit, 4 whole number,
; and 12 bits fraction. This allows for meaningful values up to 10 brads from the X axis.
; Consumer will need to bit shift after use.
; input - angle to take tanget of.
; output - result
tan16:
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
	`pushi tangentTable
	jsr add16
	`fetch			; sineTable [ 2 * angle ]
	rts

_quadrentTwo:
	`pushi STRAIGHT_ANGLE	; reflect angle and lookup.
	`swap
	jsr sub16
	jsr _tableLookup
	jmp neg16		; correct sign

_quadrentThree:
	`pushi STRAIGHT_ANGLE	; rotate angle and lookup
	jsr sub16
	jmp _tableLookup

_quadrentFour:
	`pushi FULL_ROTATION
	`swap
	jsr sub16
	jsr _tableLookup
	jmp neg16
.scend

.alias MAX_SIN $7fff
.alias MIN_SIN [[$ffff^MAX_SIN]+1]

; clamps n between max and min to prevent inverse trig function overflow.
; Note in this code I used 16 bit values, so this function isn't strictly
; needed. But if you reduce the resolution below 16 bits it is required.
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
	phy
	jsr clampSin16
	`pushZero	    	; keep sine input and approximation on stack
	ldy #$00
_do:	`over			; compare the input to sine of the approximation.
	`over
	jsr divByTwo16		; Signed divide by four
	jsr divByTwo16
	jsr sin16
	`if_greater16		; The approximation is too small, add the correction.
	    lda _asinCorectionsTable,y
	    `pushA
	    iny
	    lda _asinCorectionsTable,y
	    sta TOS_MSB,x
	    jsr add16
	bra _endif
_else:				; The approximation is too large, subtract the correction.
	    lda _asinCorectionsTable,y
	    `pushA
	    iny
	    lda _asinCorectionsTable,y
	    sta TOS_MSB,x
	    jsr sub16
_endif:
	iny
	cpy #_asinCorrectionsCount
	bmi _do			; Continue until the correction is zero.
	`nip			; drop input and return only the approximation.
	jsr divByTwo16		; Signed divide by 4 to return an angle.
	jsr divByTwo16
	ply
	rts

; 11 bit precomputed correction values for binary search
.alias _asinCorrectionsCount [_asinCorectionsTableEnd - _asinCorectionsTable]
_asinCorectionsTable:
	.word $0200, $0100 	; 45, 22.5
	.word $0080, $0040	; 11.25, 5.625
	.word $0020, $0010	; 2.8125, 1.40625
	.word $0008, $0004	; 0.706125, 0.3515625
	.word $0002, $0001	; 0.17578125, 0.087890625
_asinCorectionsTableEnd:
.scend

; acos is defined in terms of asin with coordinate rotation.
acos16:
	jsr asin16
	`pushi RIGHT_ANGLE
	jsr sub16
	jsr neg16
	rts

; Fixed point four-quadrant arctangent implemented using CORDIC. Given Cartesian
; vector (x, y), it finds the angle subtended by the vector and the positive x-axis.
; input - 16 bit signed Y at NOS and 16 bit signed X at TOS
; output - binary radian angle
atan216:
.scope
	phy
	`pop _x
	`pop _y
	`pushZero		; Zero for first approximation of angle at TOS.
	stz _loopNum		; initialize loop and bitshift count.
_do:	jsr _getAngle		; Use stack like an RPM calculator and push args for later.
	`push _x
	jsr _getYShifted	; SumAngle, angle for iteration, _x, (Y >> LoopNum)
	lda _y+1		; if _y is positive
	bmi _else
	    jsr add16		; Calc _x+(Y >> LoopNum) but update _x after _y calcs.
	    `push _y		; _y is done in sequence
	    jsr _getXShifted
	    jsr sub16
	    `pop _y		; update variables with calculations.
	    `pop _x
	    jsr add16		; SumAngle + angle for iteration at TOS
	bra _endif
_else:
	    jsr sub16		; Calc _x-(Y >> LoopNum) but update _x after _y calcs.
	    `push _y		; _y is done in sequence
	    jsr _getXShifted
	    jsr add16
	    `pop _y		; update variables with calculations.
	    `pop _x
	    jsr sub16		; SumAngle - angle for iteration at TOS
_endif:
	inc _loopNum
	lda _loopNum
	cmp #_cordicAnglesCount
	bmi _do
	`pushi 5
	jsr arshift16		; divide SumAngle by 32 to match resolution used
	ply
	rts

_getXShifted:
	`push _x
	lda _loopNum
	`pushA
	jsr arshift16
	rts

_getYShifted:
	`push _y
	lda _loopNum
	`pushA
	jsr arshift16
	rts

_getAngle:
	lda _loopNum
	asl
	tay
	lda _cordicAnglesTable,y
	`pushA
	iny
	lda _cordicAnglesTable,y
	sta TOS_MSB,x
	rts

; 13 bit precomputed tangent values for CORDIC
.alias _cordicAnglesCount [[_cordicAnglesTableEnd - _cordicAnglesTable] / 2]
_cordicAnglesTable:
	.word $1000	; 45
	.word $0972	; 26.565
	.word $04FD	; 14.036
	.word $0288	; 7.125
	.word $0145	; 3.576
	.word $00A2	; 1.79
	.word $0051	; 0.895
	.word $0028	; 0.448
_cordicAnglesTableEnd:
.scend

sineTable:
	.word $0000, $00C9, $0192, $025B, $0324, $03ED, $04B6, $057F
	.word $0647, $0710, $07D9, $08A2, $096A, $0A33, $0AFB, $0BC3
	.word $0C8B, $0D53, $0E1B, $0EE3, $0FAB, $1072, $1139, $1201
	.word $12C8, $138E, $1455, $151B, $15E2, $16A8, $176D, $1833
	.word $18F8, $19BD, $1A82, $1B47, $1C0B, $1CCF, $1D93, $1E56
	.word $1F19, $1FDC, $209F, $2161, $2223, $22E5, $23A6, $2467
	.word $2528, $25E8, $26A8, $2767, $2826, $28E5, $29A3, $2A61
	.word $2B1F, $2BDC, $2C98, $2D55, $2E11, $2ECC, $2F87, $3041
	.word $30FB, $31B5, $326E, $3326, $33DE, $3496, $354D, $3604
	.word $36BA, $376F, $3824, $38D8, $398C, $3A40, $3AF2, $3BA5
	.word $3C56, $3D07, $3DB8, $3E68, $3F17, $3FC5, $4073, $4121
	.word $41CE, $427A, $4325, $43D0, $447A, $4524, $45CD, $4675
	.word $471C, $47C3, $4869, $490F, $49B4, $4A58, $4AFB, $4B9E
	.word $4C3F, $4CE1, $4D81, $4E21, $4EBF, $4F5E, $4FFB, $5097
	.word $5133, $51CE, $5269, $5302, $539B, $5433, $54CA, $5560
	.word $55F5, $568A, $571D, $57B0, $5842, $58D4, $5964, $59F3
	.word $5A82, $5B10, $5B9D, $5C29, $5CB4, $5D3E, $5DC7, $5E50
	.word $5ED7, $5F5E, $5FE3, $6068, $60EC, $616F, $61F1, $6271
	.word $62F2, $6371, $63EF, $646C, $64E8, $6563, $65DD, $6657
	.word $66CF, $6746, $67BD, $6832, $68A6, $6919, $698C, $69FD
	.word $6A6D, $6ADC, $6B4A, $6BB8, $6C24, $6C8F, $6CF9, $6D62
	.word $6DCA, $6E30, $6E96, $6EFB, $6F5F, $6FC1, $7023, $7083
	.word $70E2, $7141, $719E, $71FA, $7255, $72AF, $7307, $735F
	.word $73B5, $740B, $745F, $74B2, $7504, $7555, $75A5, $75F4
	.word $7641, $768E, $76D9, $7723, $776C, $77B4, $77FA, $7840
	.word $7884, $78C7, $7909, $794A, $798A, $79C8, $7A05, $7A42
	.word $7A7D, $7AB6, $7AEF, $7B26, $7B5D, $7B92, $7BC5, $7BF8
	.word $7C29, $7C5A, $7C89, $7CB7, $7CE3, $7D0F, $7D39, $7D62
	.word $7D8A, $7DB0, $7DD6, $7DFA, $7E1D, $7E3F, $7E5F, $7E7F
	.word $7E9D, $7EBA, $7ED5, $7EF0, $7F09, $7F21, $7F38, $7F4D
	.word $7F62, $7F75, $7F87, $7F97, $7FA7, $7FB5, $7FC2, $7FCE
	.word $7FD8, $7FE1, $7FE9, $7FF0, $7FF6, $7FFA, $7FFD, $7FFE
	.word $7FFF

tangentTable:
	.word $0000, $000C, $0019, $0025, $0032, $003E, $004B, $0058
	.word $0064, $0071, $007D, $008A, $0097, $00A3, $00B0, $00BD
	.word $00C9, $00D6, $00E3, $00EF, $00FC, $0109, $0116, $0122
	.word $012F, $013C, $0149, $0156, $0163, $0170, $017D, $018A
	.word $0197, $01A4, $01B1, $01BE, $01CB, $01D9, $01E6, $01F3
	.word $0200, $020E, $021B, $0229, $0236, $0244, $0251, $025F
	.word $026D, $027B, $0288, $0296, $02A4, $02B2, $02C0, $02CE
	.word $02DC, $02EA, $02F9, $0307, $0316, $0324, $0333, $0341
	.word $0350, $035F, $036D, $037C, $038B, $039A, $03AA, $03B9
	.word $03C8, $03D8, $03E7, $03F7, $0406, $0416, $0426, $0436
	.word $0446, $0456, $0467, $0477, $0488, $0498, $04A9, $04BA
	.word $04CB, $04DC, $04ED, $04FF, $0510, $0522, $0534, $0546
	.word $0558, $056A, $057D, $058F, $05A2, $05B5, $05C8, $05DB
	.word $05EE, $0602, $0616, $062A, $063E, $0652, $0667, $067B
	.word $0690, $06A5, $06BB, $06D0, $06E6, $06FC, $0712, $0729
	.word $0740, $0757, $076E, $0786, $079D, $07B5, $07CE, $07E7
	.word $07FF, $0819, $0832, $084C, $0867, $0881, $089C, $08B7
	.word $08D3, $08EF, $090C, $0928, $0946, $0963, $0981, $09A0
	.word $09BF, $09DE, $09FE, $0A1F, $0A40, $0A61, $0A83, $0AA6
	.word $0AC9, $0AED, $0B11, $0B36, $0B5B, $0B82, $0BA9, $0BD0
	.word $0BF9, $0C22, $0C4C, $0C76, $0CA2, $0CCE, $0CFB, $0D29
	.word $0D58, $0D88, $0DB9, $0DEC, $0E1F, $0E53, $0E88, $0EBF
	.word $0EF7, $0F30, $0F6B, $0FA7, $0FE4, $1023, $1064, $10A6
	.word $10EA, $112F, $1177, $11C0, $120C, $1259, $12A9, $12FB
	.word $1350, $13A7, $1401, $145D, $14BD, $151F, $1585, $15EE
	.word $165B, $16CC, $1741, $17B9, $1837, $18B9, $1940, $19CD
	.word $1A5F, $1AF7, $1B96, $1C3B, $1CE8, $1D9D, $1E5A, $1F20
	.word $1FF0, $20CA, $21AF, $22A1, $23A0, $24AD, $25C9, $26F7
	.word $2837, $298C, $2AF7, $2C7B, $2E1A, $2FD8, $31B8, $33BD
	.word $35EE, $384F, $3AE7, $3DBD, $40DC, $444F, $4823, $4C6A
	.word $5139, $56AC, $5CE6, $6414, $6C74, $7658, $7FFF, $7FFF
	.word $7FFF, $7FFF, $7FFF, $7FFF, $7FFF, $7FFF, $7FFF, $7FFF
	.word $7FFF

.scend
