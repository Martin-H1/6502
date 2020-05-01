; -----------------------------------------------------------------------------
; The 6502 is a pure eight bit processor and math beyond eight bits
; requires helper functions. This module uses the stack like an RPN
; calculator or Forth to pass inputs and return outputs.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

;
; Aliases
;

;
; Data segments
;
.data BSS
.space _N	2		; a workspace cell.
.space _CNT	2		; a counter cell.
.text

;
; Macros
;

.macro decw
        lda _1
        bne _over
        dec _1+1
_over:  dec _1
.macend

.macro incw
        inc _1
        bne _over
        inc _1+1
_over:
.macend

.macro if_less16
	jsr compare16

	; for signed numbers, NOS < TOS if C=0
	bcc _else
.macend

.macro if_greater16
	jsr compare16

	; for signed numbers, NOS > TOS if Z=0 and N=1
	beq _else
	bpl _else
.macend

.macro if_equals16
	jsr equals16
	bne _else
.macend

;
; Functions
;

; Takes the absolute value of the input.
; input - number of take absolute value.
; output - result
abs16:
.scope
	lda TOS_MSB,x
	bpl +			; positive number need nothing additional.
	jsr neg16
*	rts
.scend

; Takes the absolute value of a 32 bit value on the stack.
; input - 32 bit number of take absolute value.
; output - result
abs32:
.scope
	lda NOS_MSB,x
	bpl +			; positive number need nothing additional.
	jsr neg32
*	rts
.scend

; adds two 16 bit quantities on the argument stack and puts the result
; on the stack.
add16:
        clc
        lda TOS_LSB,x         ; LSB of lower word
        adc NOS_LSB,x
        sta NOS_LSB,x

        lda TOS_MSB,x         ; MSB of lower word
        adc NOS_MSB,x
        sta NOS_MSB,x
	`drop
	rts

; subtract two 16 bit quantities on the argument stack and puts the result
; on the stack.
sub16:
        sec
        lda NOS_LSB,x		; LSB of lower word
        sbc TOS_LSB,x
        sta NOS_LSB,x

        lda NOS_MSB,x		; MSB of lower word
        sbc TOS_MSB,x
        sta NOS_MSB,x
	`drop
	rts

; Computes two's compliment negation of a 16 bit quantity.
; input - 16 bit number to negate.
; output - result
neg16:
	sec			; calculate 0 - tos
	lda #$00
	sbc TOS_LSB,x
	sta TOS_LSB,x
	lda #$00
	sbc TOS_MSB,x
	sta TOS_MSB,x
	rts

; Computes two's compliment negation of a 32 bit quantity.
; input - 32 bit number to negate.
; output - result
neg32:
	sec			; calculate 0 - nos/tos
	lda #$00
	sbc NOS_LSB,x
	sta NOS_LSB,x
	lda #$00
	sbc NOS_MSB,x
	sta NOS_MSB,x

	lda #$00
	sbc TOS_LSB,x
	sta TOS_LSB,x
	lda #$00
	sbc TOS_MSB,x
	sta TOS_MSB,x
	rts

; Computes signed multiply of two 16 bit quantities.
; input - 16 bit number at NOS and TOS.
; output - 16 result
mul16:
.scope
	lda TOS_MSB,x		; Determine the sign of the result.
	eor NOS_MSB,x
	pha

	jsr abs16		; multiple using absolute values.
	`swap
	jsr abs16
	jsr umul16
	pla			; Apply the sign to the result.
        bpl +
	jsr neg16
*	rts
.scend

; Computes unsigned multiply of two 16 bit quantities.
; input - 16 bit number at NOS and TOS.
; output - 16 result
umul16:
.scope
	lda #0
	sta _N
	phy
	ldy #$10
	lsr NOS_MSB,x
	ror NOS_LSB,x
_loop:	bcc +
	clc
	sta _N+1
	lda _N
	adc TOS_LSB,x
	sta _N
	lda _N+1
	adc TOS_MSB,x
*	ror
	ror _N
	ror NOS_MSB,x
	ror NOS_LSB,x
	dey
	bne _loop
	sta TOS_MSB,x
	lda _N
	sta TOS_LSB,x
	ply
	`drop
	rts
.scend

; UM/MOD ( ud u1 -- u2 u3 ) ("UM/MOD")  32/16 -> 16
; Divide double cell number by single number and return the quotient u3 as
; the TOS and remainder as NOS. All numbers are unsigned. This is the basic
; division operation the others use. Based on FIG Forth code, modified by
; Garth Wilson, see http://6502.org/source/integers/ummodfix/ummodfix.htm
ummod:
.scope
	`toszero?		; prevent division by zero.
	bne _notzero
	brk			; halt for now until error handling firms up.
_notzero:
	lda #$11		; we loop 17 times
	sta _CNT

_loop:
	rol THS_LSB,x		; rotate low cell of dividend left (LSB).
	rol THS_MSB,x
	dec _CNT
	beq _endloop		; loop control

	rol NOS_LSB,x		; rotate high cell of dividend left (MSB).
	rol NOS_MSB,x

	stz _N			; store the bit we got from hi cell MSB.
	rol _N

	sec			; subtract dividend hi cell minus divisor.
	lda NOS_LSB,x
	sbc TOS_LSB,x
	sta _N+1
	lda NOS_MSB,x
	sbc TOS_MSB,x

	tay			; use Y as temporary storage
	lda _N			; include bit carried
	sbc #$00
	bcc _loop

	lda _N+1		; make result new dividend hi cell.
	sta NOS_LSB,x
	sty NOS_MSB,x		; used as temp storage.
	bra _loop
_endloop:
	`drop			; drop unused date from the stack.
	`swap			; swap quotient and remainder.
	rts
.scend

; Symmetic signed division with remainder. Based on F-PC 3.6 by Ulrich Hoffmann
; F-PC is in the public domain. See http://www.xlerb.de/uho/ansi.seq
; input - 32 bit dividend (NOS), 16 bit divisor (TOS).
; output - 16 result remainder (NOS) and quotient (TOS).
smrem:
.scope
	lda NOS_MSB,x		; save sign of nos cell.
	pha

	lda TOS_MSB,x		; XOR the signs of TOS and NOS
	eor NOS_MSB,x
	pha			; save for later.
	jsr abs16		; divide the absolute values.
	`drop
	jsr abs32
	`advance
	jsr ummod		; calculate unsigned quotient and remainder.
	pla			; apply the XOR sign to the quotient.
	bpl +
	jsr neg16
*	pla			; if d was negative, negate the renamainder.
	bpl +
	`drop			; pretend we pushed quotient to R
	jsr neg16
	`advance
*	rts
.scend

; Computes signed division of two 16 bit quantities.
; input - 16 bit dividend at NOS and divisor at TOS.
; output - 16 quotient
div16:
.scope
	`popToR
	`sToD
	`pushFromR
	jsr smrem
	`nip
	rts
.scend

; Computes signed mod of two 16 bit quantities.
; input - 16 bit dividend at NOS and divisor at TOS.
; output - 16 modulus
mod16:
.scope
	`popToR
	`sToD
	`pushFromR
	jsr smrem
	`drop
	rts
.scend

; compares two 16 bit quantities on the argument stack to set the flag bits.
compare16:
.scope
	lda TOS_LSB,x
	cmp NOS_LSB,x
	beq _equal

	lda TOS_MSB,x		; low bytes are not equal, compare MSB
	sbc NOS_MSB,x
	ora #$01		; Make Zero Flag 0 because we're not equal
	bvs _overflow
	bra _notequal

_equal:				; low bytes are equal, so we compare high bytes
	lda TOS_MSB,x
	sbc NOS_MSB,x
	bvc _done

_overflow:			; handle overflow because we use signed numbers
	eor #$80		; complement negative flag

_notequal:
	ora #$01		; if overflow, we can't be equal
_done:
	php
	`drop
	`drop
	plp
	rts
.scend

; determines if two 16 bit quantities are equal and sets the flag bits.
equals16:
.scope
	lda TOS_LSB,x
	cmp NOS_LSB,x
	bne +
	lda TOS_MSB,x
	cmp NOS_MSB,x
*
	php
	`drop
	`drop
	plp
	rts
.scend

; AND's nos and tos, result on stack.
and16:
.scope
	lda TOS_LSB,x
	and NOS_LSB,x
	sta NOS_LSB,x

	lda TOS_MSB,x
	and NOS_MSB,x
	sta NOS_MSB,x
	`drop
	rts
.scend

; OR's nos and tos, result on stack.
or16:
.scope
	lda TOS_LSB,x
	ora NOS_LSB,x
	sta NOS_LSB,x

	lda TOS_MSB,x
	ora NOS_MSB,x
	sta NOS_MSB,x
	`drop
	rts
.scend

; Shift cell u bits to the left. We mask the anything except the lower
; 4 bit of u so we shift maximal of 16 bit
lshift16:
.scope
	lda TOS_LSB,x
	and #%00001111
	beq _done         ; if it is zero, don't do anything
	tay
_while:	asl NOS_LSB,x
	rol NOS_MSB,x
	dey
	bne _while
_done:	`drop
	rts
.scend

; Shift cell u bits to the right. We mask the anything except the lower
; 4 bit of u so we can maximally move 16 bit.
rshift16:
.scope
	lda TOS_LSB,x
	and #%00001111
	beq _done         ; if it is zero, don't do anything
	tay
_while:	lsr NOS_MSB,x
	ror NOS_LSB,x
	dey
	bne _while
_done:	`drop
	rts
.scend

.scend
