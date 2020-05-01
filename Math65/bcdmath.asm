;   bcdmath.asm -- floating point routines for 650X
;
;   (C) 1999 - 2008, C. Bond. All rights reserved.
;
;   v.1
;   This version includes add, subtract, multiply, divide, square
;   root, tangent and arctangent. The tangent and arctangent are
;   implemented as efficient BCD CORDIC algorithms. Sine, cosine
;   arcsine and arccosine are also provided.
;
;   v.2
;   This version is a massive rewrite of the previous version to
;   simplify and improve the algoritms and structure of the trig
;   functions.
;
;   v.3
;   Added remaining trig and inverse trig functions along with
;   natural log (ln) and exponential (exp).
;
;   v.4
;   Improved error reporting and added hyperbolic and inverse
;   hyperbolic functions.
;
;   v.5
;   Added log2, log10 and power (pow) function.
;
;   Todo:
;       1. Provide better documentation for all code,
;       2. Remove unused code and unreferenced labels.
;
;--------------------------------------------------------------------------
;
;   The internal 8-byte floating point format is as follows:
;
;
;       byte:   0   1   2   3   4   5   6   7
;               Â   Â   Â   Â   Â   Â   Â   Â
;               ³   ³   ³   ³   ³   ³   ³   ³
;               ³   ³   ÀÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÄÄ Mantissa (12 BCD DIGITS)
;               ³   ³  D.D  DD  DD  DD  DD  DD
;               ³   ³   ³
;               ³   ³   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ (implied decimal point)
;               ³   ³
;               ³   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÂÄÄÄ Middle and low
;               ³                           ³ ³    exponent digits
;     bits: 7654³3210                       ³ ³
;           ÂÂXXÁÂÂÂÂ                     E E EÄÄÄ Exponent
;           ³³³³ ³³³³                     ³
;           ³³³³ ³³³³                     ³
;           ³³³³ ÀÁÁÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄ- High exponent
;           ³³³³                                   digit
;           ³³ÀÁÄÄ Error reporting flags
;           ³ÀÄÄÄÄ Exponent Sign
;           ÀÄÄÄÄÄ Mantissa Sign
;
;
;   Any number represented in this format can also be represented in
;   the following display format:
;
;           (ñSm)D.DDDDDDDDDDD(ñSe)EEE
;
;   where the 'D's stand for a decimal mantissa digit and the 'E's
;   stand for decimal exponent digits. 'Sm' stands for a single bit
;   mantissa sign ( 0 = +, 1 = - ), and 'Se' stands the sign of the
;   exponent.
;
;   A convention observed in the following code is that a test for zero
;   can be done by simply testing the third byte of the number for
;   zero. This works because for any non-zero number the mantissa is
;   left justified until its two most significant digits are in this
;   location. Only zero (or an equivalent caused by underflow) will
;   have this location empty.
;
;   There are two unused bits in this storage format. Bits 4 and 5 of
;   the first byte (see above) are not assigned. One possible assignment
;   strategy uses these bits to identify invalid arguments. In this
;   version we have begun to implement it. We use the following format
;   for byte 0:
;
;                    x x     x x x x
;           byte 0:  - - 5 4 - - - -
;
;                        0 0            Valid number
;                        0 1            Underflow (not reported)
;                        1 0            Overflow
;                        1 1            Signal  (domain, range, ... error)
;
;   where the 'signal' is expressed by a BCD number in the exponent
;   position. When an invalid argument is detected, the
;   called routine will return with one of the following 8-byte strings:
;
;           $29,$99,$99,$99,$99,$99,$99,$99 : Overflow error
;           $30,1,0,0,0,0,0,0               : Range error
;           $30,2,0,0,0,0,0,0               : Domain error
;           $30,3,0,0,0,0,0,0               : Divide-by-zero error
;
;   The order of the two arguments in binary operations is given by:
;
;
;   add:    w1 + w2 -> w3
;   sub:    w1 - w2 -> w3
;   mul:    w1 * w2 -> w3
;   div:    w1 / w2 -> w3
;   pow:    w1 ^ w2 -> w3
;
;   For unary operations, the argument is in 'w1' and the result in 'w3':
;
;   sqrt:      w1   -> w3
;   int:       w1   -> w3
;   frac:      w1   -> w3
;   abs:       w1   -> w3
;   sine:      w1   -> w3
;    ...
;
;-------------------------------------------------------------------------

.scope

;
;   zero page assignments
;
	.alias tmp1	$c0
	.alias tmp2	$c1
	.alias dadj	$c2
	.alias ctr1	$c3
	.alias ctr2	$c4
	.alias ctr3	$c5
	.alias pctr	$c6
	.alias bctr	$c7
	.alias tnsgn	$c8
	.alias qdrnt	$c9
	.alias cotflg	$ca
	.alias cosflg	$cb
	.alias sinflg	$cc
	.alias expflg	$cd
	.alias mptr	$f0
	.alias ptr3	$f6
	.alias pptr	$f8

;
;   misc equates
;
	.alias reglen	8
	.alias wreglen	16

	.data bcddata
	.org $200
	.space mtbl	16
	.space mtbl1	16
	.space mtbl2	128	; formerly 16*8, but ophis space directive doesn't do expressions.

	.org $300
	.space ra	16	; These registers are for mantissa operations.
	.space rb	16
	.space rc	16
	.space rd	16
	.space re	16
	.space rf	16
;
	.space w1	8	; These registers are for input of numbers in
	.space w2	8	; storage format. 'w1' and/or 'w2' should be
	.space w3	8	; loaded with the operands before calling
	.space w4	8	; F.P. routines.  'w3' is the result.

	.space scalereg	8	; register for scaling parameter
	.space arg	8	; argument register for poly routine
	.space reg1	8	; temporary storage registers
	.space reg2	8
	.space reg3	8
	.space reg4	8

.text

; The scheme used to copy values between registers was overly complex.
; It loaded three registers then jsr-ed to a common copy routine. The
; savings was minimal versus inlining the code via a macro.
.macro copyBytes
	ldx #_3
_loop:	lda _1,x
	sta _2,x
	dex
	bpl _loop
.macend

.macro copyReg
	`copyBytes _1, _2, [reglen-1]
.macend

.macro zeroBytes
	ldx #_2
_loop:	stz _1,x
	dex
	bpl _loop
.macend

.macro zeroReg
	`zeroBytes _1, [reglen-1]
.macend

.macro zeroWideReg
	`zeroBytes _1, [wreglen-1]
.macend

.macro changeSign
	lda _1
	eor #$80
	sta _1
.macend

.macro shiftRegLeft
	ldx #_2
	lda #0
_loop:	ldy _1,x
	ora _tlh,y
	sta _1,x
	lda _thl,y
	dex
	bpl _loop
.macend

.macro shiftRegRight
	ldx #_2
	lda #0
_loop:	ldy _1,x
	ora _tlh,y
	sta _1+1,x
	lda _thl,y
	dex
	bpl _loop
.macend

;
;   main bcd floating point math package
;
sub:	`changeSign w2		; subtract by changing sign of w2 and adding.
add:	sed
	lda w1+2		; filter out case where one number is zero.
	bne +
	`copyReg w2, w3		; w1 is zero -- return with w2 (which may also be zero).
	rts

*	lda w2+2
	bne +
	`copyReg w1, w3		; w2 is zero -- return with w1 (which won't be zero).
	rts

;
;   At this point, additions and subtractions involving '0.0' have
;   been handled. Both operands are now known to be non-zero.
;
*	lda w1			; determine if a sum or difference is required.
	eor w2
	bpl _add		; branch if signs are the same, drop through otherwise

;
;  On entry, we only know that the two numbers have opposite
;  signs (mantissa signs). To subtract, we will determine
;  the larger of the two numbers (in magnitude) and place it
;  in 'ra'. The second number will be decimal aligned and
;  placed in 'rb'.
;
;  The sign of the result will carry the sign of the number
;  in 'ra' (largest in magnitude).
;
_sub:
	bit w1			; now estimate which number is larger from tests
	bvs _est1		;  on the exponents.  If exponent signs are unequal,
	bit w2			;  the positive one goes with the larger magnitude.
	bvs _est2		;
	bvc _sbpe		; both numbers have positive exponents.
_est1:	bit w2
	bvs _sbne		; both numbers have negative exponents.
	jsr _xw12		;
_est2:	jsr _sumexp		; exponents have opposite signs -- e1 is larger.
	lda tmp1
	beq _est3
_est2a:
	`copyReg w1, w3		; just copy e1 to w3...
	rts			; ...and exit.

_est3:	lda tmp2
	cmp #$12
	bcs _est2a		; e1 >> e2
	lda #0			; we now know that a subtraction will take place.
	sta dadj		; initialize decimal adjust.
	jsr _w12rab		; put 'w1' and 'w2' into 'ra' and 'rb' respectively.
	jsr _subrarb
	bcs _est4a
	jsr _xw12
	lda #0
	sta dadj
	jsr _w12rab
	jsr _subrarb
_est4a:
	lda w1			; 'tmp1' is used as a flag to identify the caller...
	eor #$40
	sta tmp1
	jsr rbtow3		; ...of this subroutine.
	rts

_sbpe:
	jsr _diffexp		; carry e1 >= e2.
	bcs +
	jsr _xw12
*	lda tmp1
	cmp #0
	bne _est2a
	jmp _est3

_sbne:
	jsr _diffexp		; exponent diff: tmp1(hi),tmp2(lo), carry: e1 >= e2.
	bcc -
	jsr _xw12
	lda tmp1
	jmp -

;
;   We now have two numbers with the same sign.
;   Add two positive or negative numbers.
;   Handle sign on exit, as it does not affect the addition.
;
;   The following algorithm places the larger of the two
;   numbers in 'ra' and the other in 'rb', but only if a
;   decimal point adjustment is needed before the addition.
;
_add:
	bit w1			; now estimate which number is larger from tests
	bvs _et1		;  on the exponents.  If exponent signs are unequal,
	bit w2			;  the positive one goes with the larger magnitude.
	bvs _et2
	bvc adpe		; both numbers have positive exponents.
_et1:	bit w2
	bvs adne		; both numbers have negative exponents.
	jsr _xw12		; swap w1 and w2 so largest magnitude is in w1.
_et2:	jsr _sumexp		; add the exponents to determine the decimal
				;  point adjustment to be applied to w2.
	lda tmp1		; if tmp1 <> 0 the sum exceeds number of digits
	beq _et3
_et2a:	`copyReg w1, w3		; w1 >> w2. no need to add, just copy to w3...
	rts			; ...and exit

_et3:	lda tmp2
	cmp #$12
	bcs _et2a		; w1 >> w2. copy and exit.
	lda #0			; Summary: w1 is larger than w2, decimal adj in tmp2.
	sta dadj		; initialize adjustments due to potential overflow.
	jsr _w12rab		; copy w1 and w2 to working regs with decimal adj.
	jsr _addrarb		; perform the addition with overflow adjustment.
	jsr rndrb		; (optional) round result register.
	lda w1
	sta tmp1		; 'tmp1' identifies caller to next subroutine.
	jsr rbtow3		; copy result (rb) to w3.
	rts
adpe:
	jsr _diffexp		; exponent diff: 'a'(hi), 'x'(lo), carry: e1 >= e2.
	bcs adp1
	jsr _xw12
adp1:	lda tmp1
	cmp #0
	bne _et2a		; e1 >> e2, just copy w1 to w3 and exit.
	jmp _et3
adne:
	jsr _diffexp		; exponent diff: tmp1(hi), tmp2(lo), carry: e1 >= e2.
	bcc adp1
	jsr _xw12
	jmp adp1

; return with sum of exponents in tmp1(hi),tmp2(lo)
_sumexp:
	lda w1
	and #$0f
	sta tmp1
	lda w2
	and #$0f
	sta tmp2
	clc
	lda w1+1
	adc w2+1
	tax
	lda tmp1
	adc tmp2
	sta tmp1
	stx tmp2
	rts

; return absolute difference of exponents of w1 and w2.
_diffexp:
	lda w1
	and #$0f		; value is in tmp1(hi),tmp2(lo). carry set if e1 >= e2,
	sta tmp1		;  clear if e1 < e2.
	lda w2
	and #$0f
	sta tmp2
	sec			; trial difference assuming e1 >= e2
	lda w1+1		; (It's just as easy to go ahead and perform the
	sbc w2+1		;  subtraction as it is to perform a multibyte
	tax			;  compare. If the numbers were properly ordered,
	lda tmp1		;  we are done. Otherwise, we subtract in reverse
	sbc tmp2		;  order, which we would have done anyway.)
	bcs +			; OK, exit with carry set.
	sec			; Underflow. e1 is smaller than e2, so recalculate
	lda w2+1		;  exponent difference.
	sbc w1+1
	tax
	lda tmp2
	sbc tmp1
	clc
*	sta tmp1
	stx tmp2
	rts

;
;   Copy mantissas of numbers in 'w1' and 'w2' to 'ra' and 'rb'.
;   Align decimal points by shifting 'rb' to the right. Decimal
;   adjustment has been previously placed in 'tmp2'.
;
_w12rab:
	ldy #<wreglen*2-1	;31
	lda #0
clrlp:				; clear ra, rb
	sta ra,y
	dey
	bpl clrlp
	clc			; determine register alignment offset.
	lda tmp2
	adc #$11		; this 'magic' number is 2x5+1.
	tay
	lda _dec2bin,y		; convert to binary...
	lsr			; ... then convert to bytes
	tax			; decimal point adjustment is now contained in 'x'
	ldy #<reglen-3
*	lda w1+2,y		; now copy w1 to ra and w2 to rb (mantissas only)
	sta ra+2,y
	lda w2+2,y
	sta rb+2,x
	dex
	dey
	bpl -
	lda tmp2
	lsr			; set carry if decimal adjustment is odd...
	bcc cxp1
	; ...and shift rb left one digit to normalize.
	`shiftRegLeft rb+2, wreglen-3
cxp1:
	rts

_addrarb:
	ldx #<reglen-2		; add previously aligned 'ra' to 'rb', result in 'rb'
	clc
*	lda ra+1,x
	adc rb+1,x
	sta rb+1,x
	dex
	bpl -
	lda rb+1		; check for overflow...
	beq +
	jsr _rotrgtb		; ...and normalize, if needed
	inc dadj		; and increment decimal adjustment.
*	rts

_subrarb:
	ldx #<reglen
	sec
*	lda ra+1,x
	sbc rb+1,x
	sta rb+1,x
	dex
	bpl -
	bcc sabx		; exit with carry clear (underflow condition).
	lda #12
	sta tmp1
sab1:	lda rb+2		; normalize
	and #$f0
	bne _sabxaa
	jsr _shflftb		; shift mantissa in rb left one digit.
	inc dadj		; this quantity will be subtracted from the exponent.
	dec tmp1
	bne sab1
_sabxaa:
	ldy dadj		; convert dadj to decimal
	lda _bin2dec,y
	sta dadj
	sec			; exit with carry set (normal condition).
sabx:	rts

rndrb:	ldx #7			; round the result in 'rb'
	lda #$50
	clc
*	adc rb+1,x
	sta rb+1,x
	lda #0
	dex
	bpl -
	lda rb+1		; check for overflow...
	beq +
	jsr _rotrgtb		; ...and normalize, if needed...
	clc
	lda dadj		; assume dadj < 99 but (possibly) > 9
	adc #1
	sta dadj		; ... then increment decimal adjustment.
*	rts

rbtow3:	clc			; copy rb to w3.
	`copyBytes rb+2, w3+2, 5 ; first, copy result mantissa from rb to w3.
	lda w3+2
	bne rbta		; check if zero...
	sta w3+1		; ...and exit if so.
	sta w3
	rts
rbta:
	lda w1+1		; now copy exponent from w1 to w3
	sta w3+1
	lda w1
	and #$0f
	sta w3
	bit tmp1		; adjust exponent, assume dadj < 99
	bvc rbtw2		; add adjustment to exponent
	sec
	lda w3+1
	sbc dadj
	tax
	lda w3
	sbc #0
	bcs rbtxa
	sec
	lda dadj
	sbc w3+1
	sta w3+1
	lda #0
	sbc w3
	sta w3
	lda w1
	and #$c0
	eor #$40
	ora w3
	and #$cf
	sta w3
	rts

rbtw2:	clc
	lda w1+1
	adc dadj
	tax
	lda w1
	adc #0
rbtxa:	sta w3			; save exponent
	stx w3+1
	ora w3+1
	bne rbtx
	lda w1			; if exponent is zero...
	and #$80		; ...preserve mantissa sign, suppress exponent sign.
	ora w3
	and #$cf
	sta w3
	rts

rbtx:	lda w1
	and #$c0		; now copy sign bits to w3.
	ora w3
	and #$cf
	sta w3
	rts

_xw12:	ldy #[reglen-1]		; exchange contents of w1 with w2.
*	ldx w1,y
	lda w2,y
	sta w1,y
	txa
	sta w2,y
	dey
	bpl -
	rts

_shflftb:
	; shift mantissa in rb left one digit.
	`shiftRegLeft rb+1, reglen-1
	rts

_rotrgtb:
	`shiftRegRight rb+1, 7
	lda #0
	sta rb+1
	rts

;
;   multiply routines
;
;   Multiplication is performed by a novel algorithm involving a
;   table of multiples which is pre-computed for each call.
;   Instead of repeatedly adding suitably aligned versions of the
;   multiplier to an accumulated sum, a table consisting of
;   multiples of the multiplicand is accessed by indexing on the
;   current multiplier digit.
;
;   The rationale behind the method is that there are basically
;   two major repetitive operations involved in multiplication.
;   One is the addition of one multi-digit register to another,
;   and the other is a single digit shift of a multi-digit
;   register.
;
;   For 8-bit processors using multi-digit packed BCD numbers
;   (two digits per byte) a two digit shift can be done by
;   simply indexing the bytes by 1 (1 byte equals 2 digits).
;
;   Minimizing the number of BCD adds and shifts is the driving
;   force behind the choice of this algorithm.
;
;   The fixed point table for a specific case looks as follows:
;
;   Multiplicand = 23.456...
;
;       Table
;
;   0   0000000...
;   1   0234560...
;   2   0469120...
;   3   0703680...
;   4   0938240...
;   5   1172800...
;   6   1407360...
;   7   1641920...
;   8   1876480...
;   9   2111049...
;
;   The partial product is updated in two phases. All digits on even
;   boundaries are processed in one pass, and the digits on odd boundaries
;   are processed in the second. This improves the performance of BCD
;   computations by eliminating all but one multi-byte register shift.
;   Note that indexing through memory a byte at a time skips 2 BCD
;   digits at a time.
;
;   Suppose the multiplier is:
;
;               1234567890
;
;   Then the partial products are accumulated during the first pass as if
;   the multiplier were:
;
;               0103050709
;
;   Since the non-zero digits are on byte boundaries, the register
;   alignment can be handled by simply incrementing or decrementing
;   the value in an index register. It is not necessary to shift the
;   multi-digit register during this phase.
;
;   The result register is then shifted left one digit, and the second
;   pass accumulates partial products as if the multiplier were:
;
;               0204060800
;
;
;   The algorithm is summarized as follows:
;
;       1. Create the table of multiples of the multiplicand,
;       2. For each digit of the multiplier on even boundaries:
;           A. get next digit of multiplier,
;           B. using digit as an index, access corresponding table entry,
;           C. add table entry to partial product with proper
;               byte alignment,
;           D. if more digits, goto A.
;       3. Shift multiplier one digit,
;       4. Repeat (2.) for remainder of digits.
;
;   To estimate the performance of this strategy, consider a typical
;   repeated addition algorithm for multi-digit BCD numbers. On the
;   average, there will be 4.5 multi-byte additions per digit of the
;   multiplier.
;   After each digit is processed, the partial product register must
;   be shifted one digit. For 10 digit numbers, there will be
;   approximately 45 adds and 9 register shifts.
;
;   For the method used here, there are 8 adds required to construct
;   the table, and one add per digit of the multiplier. There is only
;   one register shift required, so the total operation count will be
;   18 adds and 1 register shift.
;
mul:	sed
	`zeroBytes ra, 47	; clear working registers
	jsr _mulm		; get product of mantissas
	lda w1			; check exponents for same or different signs
	eor w2
	and #$40
	beq ml1s
	jsr _diffexp		; carry set: e1 >= e2
	bcs +
	lda w2
	bcc xsgn
*	lda w1
	bcs xsgn		; branch always
ml1s:	jsr _sumexp		; sum of exponents is in tmp1/tmp2
	lda w1
xsgn:	and #$40
	sta w3			; result exponent sign is set
	lda dadj
	beq cxpn		; branch if no exponent adjustment is needed
	lda w3
	bne xsgna
	jsr incexp
	jmp cxpn
xsgna:	jsr decexp
	jmp cxpn
cxpn:
	lda tmp2
	sta w3+1
	ora tmp1		; if exponent is zero, clear exponent sign
	bne cxp2
	sta w3
cxp2:
	lda tmp1
	ora w3
	sta w3
	lda w1			; now correct mantissa sign
	eor w2
	and #$80
	ora w3
	sta w3
	rts

_mulm:	lda w1+2		; multiply mantissas
	beq _mzro
	lda w2+2
	beq _mzro
	jsr _cw12rab
	jsr _mktbl		; create table of multiples...
	jsr psumh		; ...accumulate partial product...
	jsr _shflft		; ...shift...
	jsr psuml		; ...now accumulate rest of partial product.
	jsr rc2w3
	rts
_mzro:	`zeroReg w3
	rts

_mktbl:	lda #<mtbl		; create table of multiples of multiplier
	sta mptr
	lda #>mtbl
	sta mptr+1
	sta TMPPTR1+1
	sta TMPPTR2+1
	lda #$10
	sta TMPPTR1
	lda #$20
	sta TMPPTR2
	`copyReg ra+9, mtbl+$10	; copy multiplier to 1st multiple
	ldx #8
_mkolp:	ldy #7			; now compute all multiples
	clc
*	lda (TMPPTR1),y
	adc mtbl1,y		; BCD add
	sta (TMPPTR2),y
	dey
	bpl -
	cld
	clc			; bump pointers for next entry
	lda TMPPTR1
	adc #$10
	sta TMPPTR1
	clc
	lda TMPPTR2
	adc #$10
	sta TMPPTR2
	sed
	dex
	bne _mkolp
	rts

_cw12rab:
	ldx #5
*	lda w1+2,x
	sta ra+10,x
	lda w2+2,x
	sta rb+10,x
	dex
	bpl -
	rts

;
;   1st partial sum. add sums of products of high digits from
;   each multiplier byte.
;
psumh:
	lda #>ra		; set up pointers to working registers
	sta TMPPTR1+1
	sta TMPPTR2+1
	lda #$18
	sta TMPPTR1
	lda #$28
	sta TMPPTR2
	ldy #7
psh1:	lda (TMPPTR1),y		; get next high digit
	and #$f0
	beq psh2		; if zero, no need to add
	sta mptr		; update pointer to multiple table for this digit
psh1a:	lda (mptr),y		; copy this multiple to result register
	sta (TMPPTR2),y
	dey
	bpl psh1a
psh2:	ldx #6			; index for number of additional digits in this pass
molp:	dec TMPPTR1		; bump pointers down one byte
	dec TMPPTR2
	ldy #7			; index for register length
	lda (TMPPTR1),y		; get next digit
	and #$f0
	beq noadd		; if zero, no need to add
	sta mptr
	clc
*	lda (mptr),y		; add multiple of current digit to partial sum
	adc (TMPPTR2),y
	sta (TMPPTR2),y
	dey
	bpl -
noadd:	dex
	bpl molp		; next digit
	rts

;
;   2nd partial sum. add sums of products with low digits from
;   each multiplier byte.
;
psuml:	lda #$19
	sta TMPPTR1
	lda #$29
	sta TMPPTR2
	ldx #6
psl1:	dec TMPPTR1
	dec TMPPTR2
	ldy #7
	lda (TMPPTR1),y
	and #$0f
	beq psl3		; if zero, no need to add
	asl
	asl
	asl
	asl
	sta mptr		; update pointer to table of multiples for this byte
	clc
psl2:	lda (mptr),y
	adc (TMPPTR2),y
	sta (TMPPTR2),y
	dey
	bpl psl2
	bcc psl3
	ldy TMPPTR2
	dey
*	lda ra,y
	adc #0
	sta ra,y
	dey
	bcs -

psl3:	dex
	bpl psl1
	rts

_shflft:
	`shiftRegLeft rc, wreglen-1	; shift 'rc' left
	rts

rc2w3:	lda #1			; copy/align 'rc' to 'w3'
	sta dadj
	lda rc+3
	and #$f0
	bne rct1
	jsr shflftc
	dec dadj
rct1:	`copyBytes rc+3, w3+2, 5
	rts

shflftc:
	`shiftRegLeft rc+2, 6	; shift mantissa in 'rc' left one digit.
	rts

incexp:	clc			; increment an exponent
	lda tmp2
	adc #1
	sta tmp2
	lda tmp1
	adc #0
	sta tmp1
	rts

decexp:	sec			; decrement an exponent
	lda tmp2
	sbc #1
	sta tmp2
	lda tmp1
	sbc #0
	sta tmp1
	rts

;
;   divide routines
;
;   The divide algorithm uses tables of multiples, as does the
;   multiply algorithm. In this case each multiple is a candidate
;   for subtraction from the current remainder. A quick scan
;   weeds out multiples which are unsuitable for the trial
;   subtraction and a recovery routine is invoked in the event
;   that an underflow occurs anyway.
;
div:	sed			;   weed out zero divisor or dividend
	lda w2+2
	bne +
	jmp dvzrerr
*	lda w1+2
	bne d1b
	`zeroReg w3		; dividend is zero, return 0.00--e000
	rts
d1b:	`zeroBytes ra, 47	; clear out working area
	ldx #5			; copy mantissas from w1/w2 to ra/rb
d2:	lda w2+2,x		; (ra is divisor, rb is working reg/quotient)
	sta ra+10,x
	lda w1+2,x
	sta rb+2,x
	dex
	bpl d2
	jsr _mktbl		; create table of multiples
	jsr _shftlft		; initialize remainder for 1st digit divide
	lda #15			; digit counter
	sta ctr2
onedig:	jsr decdiv		; divide, one digit at a time
	dec ctr2
	bne onedig
	lda #1			; copy result to w3 mantissa with dadj = exponent adj.
	sta dadj
	lda rb+8
	beq od1
	jsr _shftlft
	dec dadj
	`copyBytes rb+8, w3+2, 5
	bra od4
od1:	`copyBytes rb+9, w3+2, 5

;
;   handle exponent
;
od4:	lda w1
	eor w2
	and #$40		; compare exponent signs
	beq exd
	jsr _sumexp
	lda w1
	jmp dexpadj
exd:	jsr _diffexp		; exponent signs are the same
	lda w2
	bcs dexpadj		; carry: divisor exp >= dividend exp (e1 >= e2)
	eor #$40
dexpadj:
	and #$40
	sta w3
	bit w3
	bvs od5			; branch to negative exponent handler
	lda dadj		; positive exponent
	beq od6
	lda tmp2
	ora tmp1
	bne od5a
	lda #1			; unadjusted exponent is zero and decrement is needed
	sta tmp2
	lda #$40
	ora w3
	sta w3
	jmp od6
od5:	lda dadj
	beq od6
	jsr incexp
	jmp od6
od5a:	jsr decexp
od6:	lda tmp2		; now copy adjusted exponent
	sta w3+1
	lda tmp1
	ora w3
	sta w3
	and #$0f		; test for condition of zero exponent...
	ora w3+1		; ...and clear exponent sign, if so
	bne odx
	sta w3
odx:
	lda w1			; finally, set +/- mantissa sign.
	eor w2
	and #$80
	ora w3
	sta w3
	rts

decdiv:	lda #9			; compute next quotient digit
	sta ctr1
	lda rb
	ldx rb+1
	cmp mtbl+$90		; check 9th multiple for valid subtract
	bcc +
	bne _dsub
	cpx mtbl+$91
	bcs _dsub
*	dec ctr1
	cmp mtbl+$80		; check 8th multiple
	bcc +
	bne _dsub
	cpx mtbl+$81
	bcs _dsub
*	dec ctr1
	cmp mtbl+$70		; check 7th multiple
	bcc +
	bne _dsub
	cpx mtbl+$71
	bcs _dsub
*	dec ctr1
	cmp mtbl+$60		; check 6th multiple
	bcc +
	bne _dsub
	cpx mtbl+$61
	bcs _dsub
*	dec ctr1
	cmp mtbl+$50		; check 5th multiple
	bcc +
	bne _dsub
	cpx mtbl+$51
	bcs _dsub
*	dec ctr1
	cmp mtbl+$40		; check 4th multiple
	bcc +
	bne _dsub
	cpx mtbl+$41
	bcs _dsub
*	dec ctr1
	cmp mtbl+$30		; check 3rd multiple
	bcc +
	bne _dsub
	cpx mtbl+$31
	bcs _dsub
*	dec ctr1
	cmp mtbl+$20		; check 2nd multiple
	bcc +
	bne _dsub
	cpx mtbl+$21
	bcs _dsub
*	dec ctr1
	cmp mtbl+$10		; check 1st multiple
	bcc +
	bne _dsub
	cpx mtbl+$11
	bcs _dsub
*	dec ctr1		; must be zero
	and rb+15
	jmp _shftlft
_dsub:	lda rb+15		; save current digit in result register
	and #$f0
	ora ctr1
	sta rb+15
	asl
	asl
	asl
	asl
	sta TMPPTR1
	ldy #7
	sec
_sulp:	lda rb,y		; shift remainder (and quotient) left one digit
	sbc (TMPPTR1),y
	sta rb,y
	dey
	bpl _sulp
	bcs _shftlft		; branch if no correction is needed
	dec rb+15		; here we recover from underflow caused by
	ldy #7			;  subtracting a too-large multiple
*	lda rb,y		; correct remainder after underflow
	adc mtbl+$10,y		;  by adding 1st multiple back in
	sta rb,y
	dey
	bpl -

_shftlft:
	; shift remainder register in rb left one digit.
	`shiftRegLeft rb, wreglen-1
	rts

;
;   square root routines
;
;   (C) 1999, C. Bond. Finds square root of BCD number by
;       non-restoring pseudo-division.
;
sqrt:	sed
	lda w1+2
	bne +			; check for zero
	`zeroReg w3		; square root of zero, clear result and return
	rts
*	bit w1			; check for negative number...
	bpl +			; ...and return with domain error.
	jmp domnerr
*	`zeroWideReg ra		; clear working register
	`copyReg w1+2, ra+2	; now copy mantissa to ra
	lda w1+1		; check for odd exponent to force grouping of digits
	and #1
	bne +
	`shiftRegRight ra, 7	; even exponent, shift right one digit
*	ldx #7			; multiply mantissa by 5
	clc
*	lda ra+1,x
	adc ra+1,x
	sta rb+1,x
	dex
	bpl -
	ldx #7			; rb = 2 * ra
	clc
*	lda rb+1,x
	adc rb+1,x
	sta rb+1,x
	dex
	bpl -
	ldx #7
	clc
*	lda ra+1,x
	adc rb+1,x
	sta ra+1,x
	dex
	bpl -
				; ra = 5 * ra
	lda #1
	sta TMPPTR1		; index used by pseudo-divisor
	lda #$0
	sta rb
	lda #6			; number of digits counter (x2)
	sta ctr1

_sq2d:	ldx TMPPTR1
	lda #$05
	sta rb,x
	jsr sq3
	dec ctr1
	bmi sqx
	jsr sarlft
	bra _sq2d

sqx:	; shift rb left one digit
	`shiftRegLeft rb+1, reglen-1

	; move mantissa to result register
	`copyBytes rb+1, w3+2, 5

	lda w1+1		; calculate exponent (divide by 2)
	sta w3+1
	lda w1
	and #$0f
	sta w3
	lsr w3
	ror w3+1		; shift exponent right one bit...
	ldy w3
	lda _srtbl,y		; ...and translate to perform BCD correction.
	sta w3
	ldy w3+1
	lda _srtbl,y
	sta w3+1
	bit w1			; is exponent negative?
	bvc sqxx
	lda w1+1		; if so, adjust the exponent...
	and #$1
	beq sqxc
	clc
	adc w3+1
	sta w3+1
	lda w3
	adc #0
	sta w3
sqxc:	lda #$40		; ...and correct the result sign
	ora w3
	sta w3
sqxx:
	rts

sq3:	ldx TMPPTR1
	sec
*	lda ra,x
	sbc rb,x
	sta ra,x
	dex
	bpl -
	bcc sq4
	ldx TMPPTR1
	clc
	lda rb,x
	adc #$10
	sta rb,x
	bcc sq3
sq4:
	ldx TMPPTR1
	lda rb,x
	and #$f0
	ora #$9
	sta rb,x
	lda #$50
	sta rb+1,x
	jsr sarlft
	inc TMPPTR1
sq5:	ldx TMPPTR1
	clc
*	lda ra,x
	adc rb,x
	sta ra,x
	dex
	bpl -
	bcs sq6
	ldx TMPPTR1
	dec rb-1,x
	bcc sq5
sq6:
	rts
sarlft:	`shiftRegLeft ra, wreglen-1
	rts

;
;   poly routines
;
;   Evaluate a polynomial given a list of polynomial coefficients
;    and argument.
;   On entry, the argument must be in register 'arg' and a
;    pointer to the coefficient list must be in 'pptr'. The
;    number of coefficients is in 'ctr1'.
;
;   Coefficients are in high-to-low order.
;
;   This implementation evaluates a polynomial in the argument
;    squared.
;
poly:	sed
	dec pctr
;
;   pre-loop initialization
;

	ldx #[reglen-1]
pl00:	lda arg,x		; get the argument, as given...
	sta w1,x
	sta w2,x
	dex
	bpl pl00
	jsr mul			; ...and square it for this routine
	`copyReg w3, arg	; (replace original argument with squared value)
	ldy #[reglen-1]
pl0:	lda (pptr),y		; move 1st coefficient to w3
	sta w3,y
	dey
	bpl pl0
;
;   now enter main loop
;
pl1:	jsr argtow1		; get argument ...
	jsr w3tow2		; ... and current result
	jsr mul			; w3 <- product
	cld
	clc
	lda pptr		; bump coefficient pointer to next value
	adc #8
	sta pptr
	bcc pl1a
	inc pptr+1
pl1a:	sed
	jsr ctow1		; get next coefficient...
	jsr w3tow2		; ... and add to current result
	jsr add
	dec pctr
	bne pl1
	rts

ctow1:	ldy #[reglen-1]
*	lda (pptr),y
	sta w1,y
	dey
	bpl -
	rts
argtow1:
	`copyReg arg, w1
	rts
w3tow2:
	`copyReg w3, w2
	rts

;
;   secant -- calculates secant function: sec(x) = sqrt(1 + tan^2(x))
;
;   w3 = secant(w1)
;
secant:	jsr tan
	`copyReg w3, w4		; save tangent for (possible) later use
	`copyReg w3, w1
	`copyReg w3, w2
	jsr mul			; w3 contains tan^2
	`copyReg w3, w2
	`copyReg unit, w1	; w3 = 1 + tan^2
	jsr add
	`copyReg w3, w1
	jsr sqrt		; w3 = sqrt(1 + tan^2), w4 = tan
	lda qdrnt
	cmp #1
	beq +
	cmp #4
	beq +
	lda w3
	ora #$80
	sta w3
*	rts

;
;   sin -- calculates sine function:  sin(x) = tan(x)/sec(x)
;
sin:	jsr secant
	`copyReg w3, w2
	`copyReg w4, w1
	jsr div
	lda w3
	and #$7f
	ldx qdrnt
	cpx #3
	bcc +
	ora #$80
*	sta w3
	rts

;
;   cos -- cos(x) = 1/sec(x)
;
cos:	jsr secant
	jmp _reciprocal_w3

;
;   CORDIC routine to calculate tangent of given angle (radians)
;
;   On entry, 'w1' contains the angle, on exit 'w3' contains the tangent.
;
;   'ra' is used for resolving the angle, where 're' accumulates the
;   pseudo-quotient. After this 'ra', 'rb', 'rc' and 'rd' are the
;   working registers for pseudo-multiplication.
;
;   The computation strategy is as follows:
;
;   1. Scale the given argument.
;      . divide the argument by 2pi,
;      . discard the integer part of the result,
;      . multiply the fractional part by 2pi.
;   2. If the result < 0 add 2pi so reference angle is positive.
;   3. Find and set quadrant and tnsgn, quadrant is used by sin, cos, etc.
;   4. If arg > pi subtract pi
;   5. if arg > pi/2 subtract pi
;   6. The scaled argument has the same tangent as the original argument.
;      It is now in quadrant I or IV.
;      . take the absolute value of the argument.
;   7. The argument is now in the range 0 <= arg <= pi/2.
;      . set 'cotflg' = 0
;   8. If the argument exceeds pi/4,
;      . subtract argument from pi
;      . toggle 'cotflg' = 1
;   9. Calculate tangent (see CORDIC description).
;  10. Correct sign of result using 'tnsgn'.
;  11. If 'cotflg' = 1 take reciprocal of result.
;  12. Return F.P. value in w3.
;
;
tan:	lda #0
tan0:	sta cotflg
	sed
	lda #0
	sta tnsgn
	jsr tanscale		; reduce argument to range: 0 <= arg <= pi/4
	`copyReg w3, w1		; move scaled argument to w1
	lda w3+2
	bne +
	rts
*	jsr tanx		; get tangent
	lda w3
	ora tnsgn		; correct sign
	sta w3
	lda cotflg
	beq +
	jsr _reciprocal_w3	; take reciprocal if cotangent
*	rts

cot:	lda #1
	jmp tan0
csc:	jsr sin
	jmp _reciprocal_w3

; main tangent routine
tanx:
	lda w1+2
	bne +
	`zeroReg w3		; argument is zero, clear 'w3' and return.
	rts
*	bit w1
	bvc tn0x1
	lda w1			; exponent is negative, check size
	and #$0f
	bne tn0x1a		; large exponent -- return argument
	lda w1+1
	cmp #8
	bcc tn0x1
tn0x1a:	`copyReg w1, w3		; large neg exponent -- return argument
	rts
tn0x1:	`zeroBytes ra, 3*wreglen-1 ; clear working registers
	lda #<_kxatn		; initialize arctangent table pointer
	sta TMPPTR1
	lda #>_kxatn
	sta TMPPTR1+1
;
	lda w1+1
	lsr
	cld
	adc #5
	sed
	tax
	ldy #5
tn00b:	lda w1+2,y		; move angle to 'ra' with decimal point alignment
	sta ra+2,x
	dex
	dey
	bpl tn00b
	lda w1+1
	and #1
	beq +
	`shiftRegLeft ra+2, 12
*
	jsr tnpdiv		; call tangent pseudo-divide routine
;
;   're' contains pseudo-quotient in extended format.
;       (00 0D 0D 0D 0D 0D ...)
;
;   Note that the first digit is zero because the argument
;   scaling reduces the argument below pi/4 < 1.0.
;   now call routine to create fixed point tangent
;
	jsr tnpmul

;   On return, 'ra' contains a quantity proportional to 'y', and 'rb'
;   contains a quantity proportional to 'x'. First, convert the quantities
;   from the fixed point form to floating point in 'w1' and 'w3'.
;
	lda #0			; clear exponent and sign field of 'w1'
	sta w1
	sta w1+1
	lda #8
	sta bctr		; limit iterations
t0a1:	lda ra+2
	and #$f0		; is the decimal point OK?
	bne t0b
	lda #$40
	sta w1			; exponent is negative
	inc w1+1
	`shiftRegLeft ra+2, 12	; shift 'ra' left and adjust exponent in 'w1'
	dec bctr
	bne t0a1
	`zeroReg w3		; clear 'w3' to zero
	rts
t0b:	`copyBytes ra+2, w1+2, 5 ; move mantissa to 'w1'

;
;   check 'rb' position
;
	lda rb+1
	beq t0b2
	`copyReg w1, w3
	rts
t0b2:	lda #8
	sta bctr
	lda #$40
	sta w2
	lda #0
	sta w2+1		; 'rb' has negative exponent
t0b2a:	lda rb+2		; shift left until aligned
	and #$f0
	bne t0c
	inc w2+1
	`shiftRegLeft rb+2, 10
	dec bctr
	bne t0b2a
	`copyReg pio2, w3
	rts
t0c:	`copyBytes rb+2, w2+2, 5 ; move mantissa to 'w2'
	jsr div
	rts

;
; tangent pseudo-multiply routine
; enter with 'ra' = residual = y
; set 'rb' = 1.0             = x
; copy 'ra' and 'rb' to 'rc' and 'rd'
;
tnpmul:
	ldx #$10
	stx rb+2
	ldx #10
*	lda ra+2,x
	sta rc+2,x
	lda rb+2,x
	sta rd+2,x
	dex
	bpl -
	lda #0
	sta ctr3		; pseudo-digit index
;
; repeat algorithm for each digit of pseudo-quotient
;
;   ra = y2
;   rb = x2
;   rc = y1
;   rd = x1
;
tnpm1:	ldx ctr3
	dec re,x
	bmi tnpm2
	sed
;
;   copy 'ra', 'rb' to 'rc','rd'
;
	ldx #10
*	lda ra+2,x
	sta rc+2,x
	lda rb+2,x
	sta rd+2,x
	dex
	bpl -

;   now shift 'rc' and 'rd' right
;
	lda ctr3
	jsr scdnrt
;
;   now perform pseudo-multiplication
;
	ldx #10
	sec
*	lda rb+2,x		; x2 = x1 - y1*10^j
	sbc rc+2,x
	sta rb+2,x
	dex
	bpl -
	ldx #10
	clc
*	lda ra+2,x		; y2 = y1 + x1*10^j
	adc rd+2,x
	sta ra+2,x
	dex
	bpl -
	jmp tnpm1
tnpm2:
	inc ctr3
	lda #5
	cmp ctr3
	bcs tnpm1
tnpmx:
	rts

;
;   tangent pseudo-divide routine: calculates CORDIC pseudo-quotient
;   for tangent function.
;
tnpdiv:	lda #5			; number of table entries in arctangent table
	sta ctr3
	ldx #0			; index into result register (rd)
	stx re
tpd0:	ldy #7			; (subtract until underflow, then restore)
	sec
tpd1:	lda ra+2,y		; subtract 1 instance of current table value
	sbc (TMPPTR1),y
	sta ra+2,y
	dey
	bpl tpd1
	bcc tpd2a		; underflow? no, do another loop
	inc re,x
	jmp tpd0
tpd2a:
	ldy #7			; restore
	clc
tpd2:	lda ra+2,y
	adc (TMPPTR1),y
	sta ra+2,y
	dey
	bpl tpd2
	inx
	lda #0
	sta re,x
	cld			; ...bump tangent table pointer to next entry
	clc
	lda TMPPTR1
	adc #8
	sta TMPPTR1
	bcc tpd3
	inc TMPPTR1+1
tpd3:	sed
	dec ctr3
	bpl tpd0		; loop until each digit of pseudo-quotient is done
	rts
;
;   x2z: transfer 'x' to 'z' with existing offsets
;
x2z:	`zeroBytes rd, 9	; clear 'z'
	ldy #7
x2z1:	lda (TMPPTR2),y
	sta (ptr3),y
	dey
	bpl x2z1
	rts

;
;   arccos -- calculates arcosine(x): acos(x) = pi/2 - asin(x)
;
acos:	jsr asin
	`copyReg w3, w2
	`copyReg pio2, w1
	jsr sub
	rts

;                                  x
;   arcsine --  asin(x) = atan( ---------)
;                               û 1 - x^2
asin:	lda w1+2
	bne +
	jmp retzr
*
	lda w1
	and #$80		; save argument sign (append to result)
	sta sinflg
	lda #<w1		; make argument positive
	and #$7f
	sta w1
	`copyReg w1, reg1	; save to temporary register
	`copyReg w1, w2
	jsr mul			; form arg^2
	`copyReg unit, w1
	`copyReg w3, w2
	jsr sub			; form 1 - arg^2
;
;   test result of 1 - arg^2
;
;   if zero, argument is +-1.0 and angle is +-pi/2 (sin) or -pi (cos)
;   if negative, argument is invalid.
;
	lda w3+2
	beq argzr
	lda w3
	bpl argpls
	jmp rangerr		; argument > 1.0, return with error
argzr:
	`copyReg pio2, w3
	jmp fxsgn
argpls:
	`copyReg w3, w1
	jsr sqrt		; form û 1 - arg^2
	`copyReg reg1, w1
	`copyReg w3, w2
	jsr div			; form arg / û 1 - arg^2
	`copyReg w3, w1
	jsr atan

fxsgn:	lda w3
	ora sinflg
	sta w3
	rts

;
;   arctangent
;
;   1. Test argument sign,
;       . save sign, make argument positive.
;   2. Test size of argument,
;       . if argument < 1e-6 correct sign and return
;         argument in w3.
;       . if argument > 1.0, set cotflg replace argument
;         with its reciprocal.
;       . else clear cotflg.
;   3. Compute arctangent of argument. (0 <= angle <= pi/4).
;   4. If cotflg, subtract from pi/2.
;   5. Correct sign.
;   6. Return result in w3.
;
atan:	sed
	lda #0
	sta cotflg
	sta tnsgn
	bit w1
	bpl +			; is argument negative ?
	lda #$80		; yes, set 'tnsgn' flag
	sta tnsgn		; and make arg +
	lda #$4f
	and w1
	sta w1
*	bvs argchk
	jsr inv
	`copyReg w3, w1
	inc cotflg		; argument is inverted and cotflg is set
argchk:				; here, 0 <= arg <= 1.0
	lda w1
	and #$0f
	bne toosml
	lda w1+1
	cmp #7
	bcc argok
toosml:	lda #0			; argument is too small to need computation
	cmp cotflg
	beq _tsx
	`copyReg pio2, w2
	jsr div
_tsx:	lda tnsgn
	ora w1
	sta w1
	`copyReg w1, w3
	rts
argok:	; clear working storage: ra, rb, rc, rd and re
	`zeroBytes ra, 5*wreglen-1

;
;   move argument to ra with decimal point alignment
;
	lda w1+1
	lsr
	cld
	clc
	adc #5
	sed
	tax
	ldy #5
*	lda w1+2,y
	sta ra+2,x
	dex
	dey
	bpl -

	lda w1+1
	and #1
	beq +
	`shiftRegRight ra+2, 7	; even exponent, shift right one digit
	sta ra+2
*	ldx #0
	stx ctr3		; storage for pseudo-quotient index
	lda #$10		; 'ra' and 'rb' are now initialized
	sta rb+2
;
;   ra = y2 = arg (0 < arg < pi/4)
;   rb = x2 = 1.0
;
;   1. initialize pseudo-quotient digit counter and index
;       ctr3 <- index = 0, re used for pq
;   2. outer loop
;      (A) inner loop
;        y2 -> y1   (rc)
;        x2 -> x1   (rd)
;        y2 shr index (10^j)  (1st pass, j=0, no shift required)
;        x2 shr index (10^j)    "   "     "        "      "
;        y2 = y2 - x1
;        if y2 < 0 goto (B)
;        x2 = x2 + y1
;        re[index]++
;        goto (A)
;     (B) y2 = y2 + x1  (restore after subtraction underflow)
;        index++
;        if index is LT 7 goto 2.
;
	lda #0
	sta ctr3
atnlp1:
	ldx #6
*	lda ra+2,x		; y2,x2 -> y1,x1
	sta rc+2,x
	lda rb+2,x
	sta rd+2,x
	dex
	bpl -
	lda ctr3
	beq +
	jsr scdnrt		; shift y1,x1 to correct postion
*	sed
	sec
	ldx #6
*	lda ra+2,x
	sbc rd+2,x
	sta ra+2,x
	dex
	bpl -
	bcc rstr		; underflow: do restore
	clc
	ldx #6
*	lda rb+2,x
	adc rc+2,x
	sta rb+2,x
	dex
	bpl -
	ldx ctr3
	inc re,x		; update current pseudo-quotient digit
	jmp atnlp1
rstr:	ldx #6
	clc
*	lda ra+2,x
	adc rd+2,x
	sta ra+2,x
	dex
	bpl -
	inc ctr3
	lda #6			; number of desired pseudo-quotient digits
	cmp ctr3
	bcs atnlp1
;
;   pseudo-quotient digits are now in 're' (6 digits)
;   1. Convert 'ra' to F.P. in w1
;   2. Convert 'rb' to F.P. in w2
;   3. jsr divide, result in w3 is residual starting value for next step
;   4. move w3 to 'ra' with decimal point alignment
;   5. pseudo-multiply. set 'i' to 0.
;       A. for each digit[i] in pseudo-quotient, add _kxatnx[i]
;       B. decrement digit
;       C. digit = 0?
;           no? goto A.
;           yes? increment 'i'
;       D. convert 'ra' to F.P. in w3, return
;
	lda #10
	sta bctr		; guard counter
	lda #0
	sta w1+1
	lda #$40
	sta w1
	lda #0
	sta w1+1		; 'rs' has negative exponent
*	lda ra+2		; shift left until aligned
	and #$f0
	bne atnres
	inc w1+1
	`shiftRegLeft ra+2, 10
	dec bctr
	bne -
atnres:
	`copyBytes ra+2, w1+2, 5
	`copyReg rb, w2
	jsr div
;
;   pseudo-multiply for arctangent
;
;   1. copy w3 to 'ra' with decimal point aligned
;   2. initialize _kxatn table pointer in 'TMPPTR1'
;   3. initialize pseudo-quotient index in 'ctr3'
;   4. pseudo-multiply by repeated addition:
;       A. for count given by each digit of pseudo-quotient,
;           add current table entry to running sum in 'ra',
;       B. increment index (ctr3) and bump table pointer
;           to next entry.
;       C. if index < 7, goto A.
;
	`zeroWideReg ra		; clear 'ra'
	lda w3
	and #$0f
	bne atnpm
	lda w3+1
	cmp #9
	bcs atnpm
	lsr
	clc
	cld
	adc #5
	tax
	ldy #5
*	lda w3+2,y
	sta ra+2,x
	dex
	dey
	bpl -
	lda #<w1+1
	and #1
	beq atnpm
	`shiftRegRight ra+2, 7	; shift right one digit
	sta ra+2
atnpm:	lda #<_kxatn		; setup table point
	sta TMPPTR1
	lda #>_kxatn
	sta TMPPTR1+1
	ldx #0			; initialize pseudo-quotient index
	stx ctr3
apmlp:
	dec re,x
	bmi atnxtd
	ldy #7
	sed
	clc
*	lda ra+2,y
	adc (TMPPTR1),y
	sta ra+2,y
	dey
	bpl -
	bmi apmlp
atnxtd:	inc ctr3
	cld
	clc
	lda TMPPTR1
	adc #8
	sta TMPPTR1
	bcc +
	inc TMPPTR1+1
*	ldx ctr3
	cpx #7			; number of pseudo-quotient digits
	bcc apmlp
	lda #0
	sta w3
	sta w3+1		; clear result exponent field
atn02a:	lda ra+2
	and #$f0
	bne atn03
	lda #$40		; negative exponent
	sta w3
	`shiftRegLeft ra+2, wreglen-1	; shift 'ra' left until normalized
	inc w3+1
	jmp atn02a
atn03:	`copyBytes ra+2, w3+2, 5
	lda cotflg
	beq atnx
	`copyReg pio2, w1
	`copyReg w3, w2
	jsr sub
atnx:
	lda w3
	ora tnsgn
	sta w3
	rts

;
;   arc cosecant -- acsc(x) = asin(1/x)
;
;   w3 = acsc(w1)
;
acsc:
	jsr inv
	jmp asin

;
;   arc secant -- asec(x) = acos(1/x)
;
;   w3 = asec(w1)
;
asec:
	jsr inv
	jmp acos

;
;   arc cotangent -- acot(x) = atan(1/x)
;
;   w3 = acot(w1)
;
acot:
	jsr inv
	jmp atan

;
;   loge
;
;   calculate the natural logarithm (ln) of the argument in 'w1'.
;
;   w3 = ln(w1)
;
loge:	lda w1
	bpl +			; test for negative number
	jmp domnerr
*	lda w1+2
	bne +			; test for zero argument
	jmp rangerr
*	`copyReg w1, reg1	; save argument
	lda w1			; if argument is near unity, use series approximation
	bne +			;  9.9 e-1 < arg < 1.01 e0
	lda w1+1
	bne +
	lda w1+2
	cmp #$10
	bne +
;
;   argument is near unity. test for series solution
;
	`copyReg unit, w2
	jsr sub
	lda w3+2
	bne +			; check for arg = 0.0
	jmp retzr
*	lda w3
	cmp #$40
	bne _lnargrst
	lda w3+1
	cmp #$3
	bcc _lnargrst
	jmp lnser
_lnargrst:
	`copyReg reg1, w1
*	jsr _clr_r		; clear all workspace regs
	`copyBytes w1+2, ra+2, 5 ; copy mantissa to 'ra'
	lda #0
	sta ctr1		; 'j' index for pseudo-divide
	lda #6
	sta ctr3
;
;   main loop
;
_lnlp0:	jsr cpyra2rc		; compute each pseudo-quotient digit
	lda ctr1
	jsr shfrcn
	jsr addrc2ra
	bcs +
	ldx ctr1
	inc rd,x
	bra _lnlp0

*	jsr subrc2ra		; restore remainder after underflow
	inc ctr1
	dec ctr3
	bne _lnlp0		; another iteration?

	ldx #6
	sec
*	lda #0			; compute residual to start pseudo-multiply
	sbc ra+2,x
	sta ra+2,x
	dex
	bpl -
	`shiftRegRight ra+2, 7	; shift right one digit
	sta ra+2
;
;   pseudo-multiply
;
	lda #<_kxlog		; set pointer to table of logs
	sta TMPPTR1
	lda #>_kxlog
	sta TMPPTR1+1
	lda #0			; initialize counters
	sta ctr1
	lda #6
	sta ctr2
lnlp1:	ldx ctr1		; perform pseudo-multiply for each table entry
	dec rd,x
	bmi npsm
	ldy #6
	clc
*	lda ra+2,y
	adc (TMPPTR1),y
	sta ra+2,y
	dey
	bpl -
	bmi lnlp1
npsm:	cld			; bump table pointer to next coefficient
	clc
	lda TMPPTR1
	adc #8
	sta TMPPTR1
	bcc +
	inc TMPPTR1+1
*	sed
	inc ctr1
	dec ctr2
	bne lnlp1
;
;   'ra' contains ln(mantissa). adjust exponent if needed
;
*	lda ra+2		; (ctr2 is now 0)
	and #$f0
	bne ln2fp
	jsr shflfta
	inc ctr2
	bne -
ln2fp:				; save current logarithm as F.P. in reg2
	`copyBytes ra+2, reg2+2, 5
	lda #$80		; sign of mantissa?
	sta reg2
	lda ctr2
	sta reg2+1
	beq +
	lda #$40		; sign of exponent
	ora reg2
	sta reg2
*
	jsr exp2fp
	`copyReg unit, w2
	jsr add
	`copyReg w3, w1
	`copyReg ln10, w2
	jsr mul
	`copyReg reg2, w1
	`copyReg w3, w2
	jsr add
	rts

;
;   exp2fp -- convert exponent in 'w1' to signed F.P. number in 'w1'
;
exp2fp:	`zeroBytes w1+2, 5	; clear mantissa space
	lda w1
	and #$7f
	sta w1
	and #$40
	asl
	tax			; save sign bit
	lda w1
	and #$f
	bne bigexp
	stx w1
	lda w1+1
	and #$f0
	bne medexp
	lda w1+1
	asl
	asl
	asl
	asl
	sta w1+2
	lda #0
	sta w1+1
	stx w1
	rts

medexp:
	sta w1+2
	lda #1
	sta w1+1
	stx w1
	rts

bigexp:
	asl
	asl
	asl
	asl
	sta w1+2
	lda w1+1
	lsr
	lsr
	lsr
	lsr
	ora w1+2
	sta w1+2
	lda w1+1
	asl
	asl
	asl
	asl
	sta w1+3
	lda #2
	sta w1+1
	stx w1
	rts

cpyra2rc:
	ldy #6
*	lda ra+2,y
	sta rc+2,y
	dey
	bpl -
	rts

;
;   add 'rc' to 'ra'. 'rc' is shifted right by ctr1 digits
;
addrc2ra:
	cld
	clc
	ldx #7
	sed
*	lda ra+2,x
	adc rc+2,x
	sta ra+2,x
	dex
	bpl -
	rts

;
;   subtract 'rc' from 'ra'. 'rc' is shifted right by ctr1 digits
;
subrc2ra:
	cld
	sec
	sed
	ldx #7
*	lda ra+2,x
	sbc rc+2,x
	sta ra+2,x
	dex
	bpl -
	rts

;
;   shift 'ra' left one digit
;
shflfta:
	`shiftRegLeft ra+2, 8
	rts

shfrcn:	sta ctr2		; shift rc right n digits ('n' in a_reg)
	and #1			; is it an odd number?
	beq src0		; no, only do byte shifts
	`shiftRegRight rc+2, 7	; yes, do single digit shift
	sta rc+2
src0:	lda ctr2
	lsr			; convert to bytes
	beq srcx		; exit if no further shift required
	cld
	clc
	adc #7
	sed
	tay
	ldx #7
*	lda rc+2,x
	sta rc+2,y
	dey
	dex
	bpl -
	lda #0
*	sta rc+2,y
	dey
	bpl -
srcx:	rts

;
;   lnser -- return logarithm of number near unity by series expansion
;            enter with (arg - 1) in 'w3'.
;
lnser:	lda #<_kln
	sta ptr3
	lda #>_kln
	sta ptr3+1
	lda #3
	sta ctr1
	`copyReg w3, reg2	; save argument (x) to 'reg2'
	ldy #[reglen-1]
*	lda (ptr3),y		; preload 'w3' with 1st coefficient
	sta w3,y
	dey
	bpl -
; main loop
lnslp:	ldy #[reglen-1]
*	lda w3,y
	sta w1,y
	lda reg2,y
	sta w2,y
	dey
	bpl -
	jsr mul			; multiply
	dec ctr1
	bmi lnsrx
	cld
	clc
	lda ptr3
	adc #8
	sta ptr3
	bcc +
	inc ptr3+1
*	sed
	ldy #[reglen-1]
*	lda (ptr3),y
	sta w1,y
	lda w3,y
	sta w2,y
	dey
	bpl -
	jsr add			; add next coefficient
	jmp lnslp
lnsrx:	rts

;
;   exp
;
;   calculate exp(arg) where the argument is in 'w1'.
;
;   First, the argument is checked for sign. If the sign of the
;   argument is negative, the absolute value of the argument
;   is sent to the 'exp' routine and the reciprocal of the result
;   is returned to the caller.
;
;   Next, the integer part of the argument is taken and used to
;   calculate a power-of-ten multiplier (number of decimal places)
;   is found. A residual decimal fraction from this operation is
;   added to the fractional value of the given argument and the
;   'exp' routine is called with this sum as its argument.
;
;   The result is combined with the previously calculated decimal
;   point adjustment and a test for taking the reciprocal is made.
;   After all operations are complete, the result is returned to
;   the caller.
;
;   This implementation takes the mantissa and treats it as if
;   it were in the range: 0.1 <= m <= 0.99999.... instead of
;   1.0 <= m <= 9.99999......
;
;   The algorithm works by finding a pseudo-quotient from the
;   mantissa using a table of natural logs of 2, 1.1, 1.01, 1.001,
;   1.0001, etc. The digits of the pseudo-quotient represent the
;   number of subtractions of the appropriate log before underflow.
;
;   After 6-8 pseudo-quotient digits have been found, 1.0 is added
;   to the residual and the pseudo-multiply algorithm is run.
;
;   q = arg/ln10
;   f = floor(q)
;   r = q - f
;
;
exp:	lda #0
	sta expflg
	lda w1
	and #$0f
	beq +
xpre:	jmp rangerr
*	lda w1+2
	bne +
xrt1:	`copyReg unit, w3
*	bit w1
	bpl +
	lda #$80
	sta expflg
	eor w1
	sta w1
*	bit w1
	bvc +
	lda w1			; check for range problem with exponent
	and #$0f
	bne xrt1		; large negative exponent returns 1.0000...
	lda w1+1
	cmp #$12
	bcs xrt1
	bcc ++
*	lda w1+1
	cmp #4
	bcs xpre
	cmp #3
	bne +
	lda w1+2
	cmp #$23
	bcs xpre
*	`copyReg iln10, w2	; form arg/ln10, w2
	jsr mul
	`copyReg w3, reg1
	`copyReg w3, w1
	jsr int
	`copyReg w3, w2
	`copyReg w3, reg2	; save power of 10 (exponent) in 'reg2'
	jsr sub
	`copyReg w3, w1
	`copyReg ln10, w2
	jsr mul
	lda #0
	sta tmp1		; clear shift count register
	bit w3
	bvc xshft
	lda w3+1
	cmp #$12
	bcc +
	jmp xrt1
*	tax
	lda _dec2bin,x
	sta tmp1
xshft:	jsr xexp
;
;   mantissa of exponential is in 'w1'. get exponent from 'reg2'
;
	ldy reg2+1
	lda #0
	sta reg2+1
xxp:	ldx #3			; move exponent value from mantissa to exponent
*	asl reg2+3
	rol reg2+2
	rol reg2+1
	rol reg2
	dex
	bpl -
	dey
	bpl xxp
	lda reg2		; copy exponent to w1
	sta w3
	lda reg2+1
	sta w3+1
	bit expflg
	bpl +
	jsr _reciprocal_w3
*
	rts

;
;   xexp -- main exponential computation routine
;
xexp:	jsr _clr_r
	ldx #5
	lda tmp1
	lsr			; ignore odd bit for now
	cld
	clc
	adc #5
	tay
*	lda w3+2,x
	sta ra+2,y
	sta rb+2,x
	dey
	dex
	bpl -
	lda tmp1
	sta re
	lsr			; do odd bit shift (if any)
	bcc +
	`shiftRegRight ra+1, 8
	sta ra+2,x
*
;
;   this is the routine which computes the natural logarithm of
;   the number in 'ra'.
;
xnl:	lda #<_kxlog
	ldy #>_kxlog
	sta TMPPTR1
	sty TMPPTR1+1
	lda #7
	sta tmp2		; 'tmp2' is outer loop counter
	lda #0
	sta ctr1
;
;   subtract 'klog' entry from 'ra' until underflow
;
mxlp0:	lda #0
	tax
mxlp:	sed
	sec
	ldy #7
*	lda ra+2,y
	sbc (TMPPTR1),y
	sta ra+2,y
	sta rc+2,y
	dey
	bpl -
	inx
	bcs mxlp
	dex
	txa			; 'x' has current pseudo-quotient digit
	ldx ctr1
	sta rd,x
	inc ctr1
	clc
	ldy #7
*	lda ra+2,y		; restore 'ra' after underflow
	adc (TMPPTR1),y
	sta ra+2,y
	dey
	bpl -
	cld
	clc
	lda TMPPTR1
	adc #8
	sta TMPPTR1
	bcc +
	inc TMPPTR1+1
*	dec tmp2
	bpl mxlp0
;
;   pseudo-quotients are found, now perform pseudo-multiply
;
	lda #0			; 'ctr1' is offset to pseudo-quotient digit
	sta ctr1
	lda #$10		; add '1' to residual in 'ra'
	sta ra+2
	ldx #7
	stx ctr3		; 'ctr3' is outer loop counter (number of digits)
	`copyReg ra+2, rc+2	; copy 'ra' to 'rc'
;
;  multiply 'ra' by 2.0, 1.1, 1.01, etc. iterate each multiplier
;   as controlled by pseudo-digit in 'rd'
;
xml:	ldx ctr1
	dec rd,x
	bmi xbd
;
;   copy 'ra' to 'rc' shifted right
;
	`copyReg ra, rc
	lda ctr1
	jsr shfrcn
	sed
	ldx #5
	clc
*	lda ra+2,x
	adc rc+2,x
	sta ra+2,x
	dex
	bpl -
	jmp xml
xbd:
	inc ctr1
	dec ctr3
	bpl xml
	`copyBytes ra+2, w3+2, 5
	lda #0
	sta w3
	sta w3+1
	rts

;
;   hyberbolic sine -- calculates sinh(x) = (exp(x)-exp(-x))/2
;
;   w3 = sinh(w1)
;
sinh:	jsr exp
	jsr _reciprocal_w3
	`copyReg w3, w1
	lda w1
	ora #$80
	sta w1
	jsr add
	`copyReg w3, w1
	`copyReg half, w2
	jmp mul

;
;   hyperbolic cosine -- calculates cosh(x) = (exp(x) + exp(-x))/2
;
;   w3 = cosh(w1)
;
cosh:	jsr exp
	jsr _reciprocal_w3
	`copyReg w3, w1
	jsr add
	`copyReg w3, w1
	`copyReg half, w2
	jmp mul

;
;   tanh -- compute (exp(x) - exp(-x))/(exp(x) + exp(-x))
;
;   w3 = tanh(w1)
;
tanh:	jsr exp			; w3 = exp(x)
	`copyReg w3, reg2	; save exp(x) in reg2
	jsr _reciprocal_w3	; w3 = exp(-x)
	`copyReg w3, w2
	`copyReg w3, reg3	; save exp(-x) in reg3
	`copyReg reg2, w1
	jsr sub			; w3 = exp(x) - exp(-x)
	`copyReg reg2, w1	; w1 = exp(x)
	`copyReg reg3, w2	; w2 = exp(-x)
	`copyReg w3, reg2	; reg2 = exp(x) - exp(-x)
	jsr add			; w3 = exp(x) + exp(-x)
	`copyReg w3, w2		; w1 = exp(x) + exp(-x)
	`copyReg reg2, w1	; w2 = exp(x) - exp(-x)
	jmp div

;
;   hyperbolic cosecant -- csch(x) = 1/sinh(x)
;
;   w3 = csch(w1)
;
csch:	jsr sinh
	jmp _reciprocal_w3

;
;   hyperbolic secant -- sech(x) = 1/cosh(x)
;
;   w3 = cosh(w1)
;
sech:	jsr cosh
	jmp _reciprocal_w3

;
;   hyperbolic cotangent -- coth(x) = 1/tanh(x)
;
;   w3 = coth(w1)
;
coth:	jsr tanh
	jmp _reciprocal_w3

;
;   hyperbolic arc sine -- asinh(x) = ln(x + sqrt(x^2 + 1))
;
;   w3 = asinh(w1)
;
asinh:	`copyReg w1, reg1	; save argument 'x' in reg1
	`copyReg w1, w2
	jsr mul			; form x^2
	`copyReg w3, w2
	`copyReg unit, w1
	jsr add			; form 1 + x^2
	`copyReg w3, w1
	jsr sqrt		; w3 = sqrt(1 + x^2)
	`copyReg w3, w2
	`copyReg reg1, w1
	jsr add			; w3 = x + sqrt(1 + x^2)
	`copyReg w3, w1
	jmp loge

;
;   hypberbolic arc cosine: acosh(x) = ln(x + sqrt(x^2 - 1))
;
acosh:	`copyReg w1, reg1	; save argument 'x' in reg1
	`copyReg w1, w2
	jsr mul			; form x^2
	`copyReg w3, w1
	`copyReg unit, w2
	jsr sub			; form x^2 - 1
	`copyReg w3, w1
	jsr sqrt		; w3 = sqrt(x^2 - 1)
	`copyReg w3, w2
	`copyReg reg1, w1
	jsr add			; w3 = x + sqrt(x^2 - 1)
	`copyReg w3, w1
	jmp loge

;                                           1 + x
;   hyperbolic arc tangent:  atanh(x) = ln( -----)
;                                           1 - x
atanh:	`copyReg w1, reg1	; save 'x' in reg1
	`copyReg unit, w2
	jsr add			; w3 = 1 + x
	`copyReg unit, w1
	`copyReg reg1, w2
	`copyReg w3, reg1	; save (1 + x) in reg1
	jsr sub			; w3 = 1 - x
	`copyReg w3, w2
	`copyReg reg1, w1
	jsr div			; w3 = (1 + x)/(1 - x)
	`copyReg w3, w1
	jsr loge
	`copyReg w3, w1
	`copyReg half, w2
	jmp mul

;
;   hyperbolic arc cosecant: acsch(x) =  asinh(1/x)
;
acsch:
	jsr inv
	`copyReg w3, w1
	jmp asinh

;
;   hyperbolic arc secant: asech(x) = acosh(1/x)
;
asech:
	jsr inv
	`copyReg w3, w1
	jmp acosh

;
;   hyperbolic arc cotangent: acoth(x) = atanh(1/x)
;
acoth:
	jsr inv
	`copyReg w3, w1
	jmp atanh

;
;   log2(x) = loge(x)/loge(2)
;
log2:	jsr loge
	`copyReg w3, w1
	`copyReg ln2, w2
	jmp div

;
;   log10(x) = loge(x)/loge(10)
;
log10:	jsr loge
	`copyReg w3, w1
	`copyReg ln10, w2
	jmp div

;
;   power(x,y) = x^y = exp(y.loge(x)) = w1^w2
pow:	`copyReg w2, reg3
	jsr loge
	`copyReg w3, w1
	`copyReg reg3, w2
	jsr mul
	`copyReg w3, w1
	jmp exp

;
;   inv -- reciprocal of x = 1/x
;
;   w3 = 1/w1
;
inv:
	`copyReg w1, w2
	`copyReg unit, w1
	jmp div

;
;   abs -- w3 = abs(w1)
;
abs:
	`copyReg w1, w3
	lda w3
	and #$7f
	sta w3
	rts

;
;   chs -- w3 = -w1
;
chs:
	`copyReg w1, w3
	`changeSign w3
	rts

upang:	ldy #7
	clc
*	lda ra+2,y
	adc (TMPPTR1),y
	sta ra+2,y
	dey
	bpl -
	rts

;
;   shift 'rc' and 'rd' right 'n' digits. Enter with 'n' in 'a-reg'
;
scdnrt:	sta ctr2
	and #1			; is it an odd number?
	beq scd0		; no, only do byte shifts
	`shiftRegRight rc+2, 7	; yes, do single digit shift
	sta rc+2
	`shiftRegRight rd+2, 7
	sta rd+2
scd0:	lda ctr2
	lsr			; convert to bytes
	beq scdx		; exit if no further shift required
	cld
	clc
	adc #7
	sed
	tay
	ldx #7
*	lda rc+2,x
	sta rc+2,y
	lda rd+2,x
	sta rd+2,y
	dey
	dex
	bpl -
	lda #0
*	sta rc+2,y
	sta rd+2,y
	dey
	bpl -
scdx:	rts

;
;   int -- returns the integer part of a floating point number
;
;   enter with F.P. number in w1, return with result in w3
;
int:	lda #$0f
	bit w1			; test for sign of exponent and high order exponent digit
	bvs int0		; negative exponent means integer part is zero
	bne int1		; large, positive exponent means number is already integer
	jsr int1		; copy to result register (w3)
	ldx w1+1
	cpx #$12
	bcs intx
	cld
	lda #$b
	sec
	sbc _dec2bin,x		; number of zeros after decimal point
	sed
	lsr
	beq inta
	tax
	ldy #5
	lda #0
*	sta w3+2,y
	dey
	dex
	bne -
inta:	bcc intx
	lda #$f0
	and w3+2,y
	sta w3+2,y
intx:
	rts
int0:	`copyReg zero, w3
	rts
int1:	`copyReg w1, w3
	rts

;
;   frac -- returns with fractional part of floating point number
;
frac:	`copyReg w1, reg3	; save 'x' in reg3
	jsr int			; find integer part of argument in w1
	`copyReg w3, w2		; move integer part to w2
	`copyReg reg3, w1
	jsr sub			; subtract integer part from argument for fraction
	lda w3+2
	bne +
	sta w3
*	rts

tanscale:			; scale argument to range: 0 <= arg <= pi/4
	`copyReg twopi, scalereg ; put 2pi in scale register
	jsr scale		; get   2pi*frac(arg/2pi), -2pi <= arg <= 2pi
	lda w3			; if arg < 0 add 2pi
	bpl +
	`copyReg w3, w1
	`copyReg twopi, w2
	jsr add
*				; now find and save quadrant
	lda #<tpio2
	sta TMPPTR1
	lda #>tpio2		;   TMPPTR1        TMPPTR2        n z
	sta TMPPTR1+1		;          =                x 1
	lda #<w3		;          >                0 0
	sta TMPPTR2		;          <                1 0
	lda #>w3
	sta TMPPTR2+1
	jsr cmpreg		; 3pi/2 | w3
	bpl +
	lda #4			; w3 > 3pi/2
	sta qdrnt
	jmp tsc1
*	lda #<pi
	sta TMPPTR1
	lda #>pi
	sta TMPPTR1+1
	jsr cmpreg		; pi | w3
	bpl +
	lda #3			; w3 > pi
	sta qdrnt
	jmp tsc1
*	lda #<pio2
	sta TMPPTR1
	lda #>pio2
	sta TMPPTR1+1
	jsr cmpreg		; pi/2 | w3
	bpl +
	lda #2			; w3 > pi/2
	sta qdrnt
	jmp tsc1
*	lda #1
	sta qdrnt
tsc1:	lda qdrnt
	sta ra
	cmp #3
	bcc +
	`copyReg w3, w1		; map quadrants III and IV to I and II
	`copyReg pi, w2
	jsr sub
*	lda qdrnt
	and #1
	bne +
	lda #$80
	sta tnsgn
*	lda #<pio2
	sta TMPPTR1
	lda #>pio2
	sta TMPPTR1+1
	jsr cmpreg		; pi/2 | w3
	bpl +
	`copyReg w3, w1		; w3 > pi/2
	`copyReg pi, w2
	jsr sub
*	lda w3			; range is: -pi/2 < arg < pi/2
	and #$7f		; take abs value, tnsgn is already correct
	sta w3
	lda #<w3		; now range is:     0 <= arg <= pi/2.
	sta TMPPTR2		; reduce range to:  0 <= arg <= pi/4
	lda #>w3
	sta TMPPTR2+1
	lda #<pio4
	sta TMPPTR1
	lda #>pio4
	sta TMPPTR1+1
	jsr cmpreg		; pi/4 | w3
	bpl +			; if upper part of range, pi/4 < arg < pi/2, is
	`copyReg pio2, w1	;  mapped to lower part, 0 < arg < pi/4, toggle
	`copyReg w3, w2		;  cotangent flag (cotflg)
	jsr sub
	lda cotflg
	eor #1
	sta cotflg
*	rts			; all scaling is complete.

scale:
	`copyReg scalereg, w2
	jsr div			; perform division
	`copyReg w3, w1		; move result to w1
	jsr frac		; take fractional portion of result
	`copyReg w3, w1
	`copyReg scalereg, w2	; copy scale to w2
	jsr mul			; result is arg mod scale
	rts

;
; compare registers pointed to by TMPPTR1 and TMPPTR2.
; returns the following:
;                                       n z
;   zflag = 1 if numbers are the same   x 1
;   nflag = 0 if TMPPTR1 > TMPPTR2            0 0
;   nflag = 1 if TMPPTR1 < TMPPTR2            1 0
;
cmpreg:
	sed
	ldy #0
	lda (TMPPTR1),y
	eor (TMPPTR2),y
	bpl +
;
;   mantissa signs differ, positive number is larger
;
	lda (TMPPTR1),y		; set nflag using TMPPTR1
	ora #1			; make sure zflag = 0
	rts

;
;   mantissa signs are the same, check exponent signs
;
*	asl			; shift exponent sign into sign bit
	bpl +
;
;   exponent signs differ, larger number has:
;       mantissa sign positive, exponent sign positive
;       mantissa sign negative, exponent sign negative
;
	lda (TMPPTR1),y
	bmi negm
	ldy #2
	lda (TMPPTR1),y
	bne nz1
	lda #$80		; TMPPTR1 = 0 and TMPPTR1 = frac
	rts
nz1:	lda (TMPPTR2),y
	bne nz2
	rts			; TMPPTR2 = 0 and TMPPTR1 = frac
nz2:
	ldy #0
	lda (TMPPTR1),y
	asl
	ora #$1
	rts
negm:
	lda (TMPPTR2),y
	asl
	ora #$1
	rts
;
;   mantissa exponent signs are the same, check exponent magnitude
;
*	lda (TMPPTR1),y
	eor (TMPPTR2),y
	bne +
;
;   first exponent digit matches, check next two
;
	iny
	lda (TMPPTR1),y
	eor (TMPPTR2),y
	bne +
;
;   all signs and exponents match, compare mantissas
;
	ldy #7
	ldx #5
	lda #0
	sta tmp1
	sec
mantst:	lda (TMPPTR2),y
	sbc (TMPPTR1),y
	beq zr
	inc tmp1
zr:
	dey
	dex
	bpl mantst
	bcc p1max
	lda tmp1
	bne p2max
	rts

;
;   all signs match, but exponents differ
;
;   largest number
;       msgn+, esgn+, exponent largest   80.0 >  1.23
;       msgn+, esgn-, exponent smallest  0.33 >  0.02
;       msgn-, esgn+, exponent smallest -1.23 > -80.0
;       msgn-, esgn-, exponent largest  -0.02 > -0.33
;
*	ldy #1
	sec
	lda (TMPPTR2),y
	sbc (TMPPTR1),y
	dey
	lda (TMPPTR2),y
	sbc (TMPPTR1),y
	bcs e2max
;   TMPPTR1 has largest exponent, all signs are the same
	lda (TMPPTR1),y
	and #$c0
	beq p1max
	cmp #$c0
	beq p1max
p2max:	lda #$81
	rts
p1max:	lda #1
	rts

;   TMPPTR2 has largest exponent, all signs are the same
e2max:
	lda (TMPPTR2),y
	and #$c0
	beq p2max
	cmp #$c0
	beq p2max
	bne p1max

;
; clear all 'rn' regs
;
_clr_r:	`zeroBytes ra, wreglen*6-1
	rts

;
; Helper to calculate the reciprocal of the contents of register w3.
;
_reciprocal_w3:
	`copyReg w3, w2
	`copyReg unit, w1
	jmp div

;
;   return with zero
;
;   utility routine to place zero in w3 and return
;
retzr:
	`copyReg zero, w3
	rts

;
;   domain error handler
;
;   returns domain error string with error flags set
;
;       $30,2,0,0,0,0,0,0
;
domnerr:
	`copyReg dmnerr, w3
	jmp _errhndlr

;
;   range error handler
;
;   returns range error string with error flags set
;
;       $30,1,0,0,0,0,0,0
;
rangerr:
	`copyReg rngerr, w3
	jmp _errhndlr

;
;   overflow error handler
;
;   returns overflow error string with error flags set
;
;       $29,$99,$99,$99,$99,$99,$99,$99
;
ovrferr:
	`copyReg ovferr, w3
	jmp _errhndlr

;
;   divide-by-zero error handler
;
;   returns divide-by-zero error string with error flags set
;
;       $30,3,0,0,0,0,0,0
;
dvzrerr:
	`copyReg dvzerr, w3
	jmp _errhndlr

;
;   general error handler
;
;   returns error string in w3 and executes 'brk' instruction
;
_errhndlr:	brk

;
;   The following two tables are used in shifting a BCD register
;   one BCD digit to the right or left.
;
;
;   '_tlh' maps the lower decimal digit of its BCD index into
;   the high digit.
;
;   Example:
;
;           ldy #$35
;           lda _tlh,y
;
;   Now the 'a' register contains #$50; i.e. the '5' in '35'
;   is shifted into the high digit position and the low
;   digit position is cleared.
;
;   This table, and the one following, support high speed
;   shifting of multibyte BCD registers.
;
_tlh:	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
	.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0

;
;   'thl' maps the upper digit of its BCD index into the
;   low digit.
;
;   Example:
;
;           ldy #$35
;           lda thl,y
;
;   Now the 'a' register contains #$03; i.e. the '3' in '35'
;   is shifted to the low digit position and the high digit
;   position is cleared.
;
_thl:	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
	.byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
	.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
	.byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
	.byte 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
	.byte 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
	.byte 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
	.byte 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
	.byte 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9

;
;   '_dec2bin' maps a BCD value into its equivalent binary value.
;
;   Example:
;
;           ldy #$35            ; '35' decimal
;           lda dec2bin,y
;
;   The 'a' register now contains 23h = 0010 0011b = 35 decimal
;
_dec2bin:
	.byte 0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0
	.byte $0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,0,0,0,0,0,0
	.byte $14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,0,0,0,0,0,0
	.byte $1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,0,0,0,0,0,0
	.byte $28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,0,0,0,0,0,0
	.byte $32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,0,0,0,0,0,0
	.byte $3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,0,0,0,0,0,0
	.byte $46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,0,0,0,0,0,0
	.byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,0,0,0,0,0,0
	.byte $5a,$5b,$5c,$5d,$5e,$5f,$60,$61,$62,$63,0,0,0,0,0,0

;
;   '_bin2dec' maps a binary value into its BCD equivalent value.
;
;   Example:
;
;           ldy #$2c            ; '2c' hex = 0010 1100b = 44 decimal
;           lda _bin2dec,y
;
;   The 'a' register now contains #$44.
;
_bin2dec:
	.byte 0,1,2,3,4,5,6,7,8,9
	.byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19
	.byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29
	.byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
	.byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49
	.byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59
	.byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69
	.byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79
	.byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89
	.byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99

;
;   srblt supports the division of BCD numbers by two
;
;   To use, shift the BCD register right one bit and
;   then replace each byte with the value take from
;   this lookup table using the existing byte as an
;   index.
;
_srtbl:		; shift right table
	.byte $00,$01,$02,$03,$04,$05,$06,$07,$05,$06,$07,$08,$09,0,0,0
	.byte $10,$11,$12,$13,$14,$15,$16,$17,$15,$16,$17,$18,$19,0,0,0
	.byte $20,$21,$22,$23,$24,$25,$26,$27,$25,$26,$27,$28,$29,0,0,0
	.byte $30,$31,$32,$33,$34,$35,$36,$37,$35,$36,$37,$38,$39,0,0,0
	.byte $40,$41,$42,$43,$44,$45,$46,$47,$45,$46,$47,$49,$49,0,0,0
	.byte $50,$51,$52,$53,$54,$55,$56,$57,$55,$56,$57,$58,$59,0,0,0
	.byte $60,$61,$62,$63,$64,$65,$66,$67,$65,$66,$67,$68,$69,0,0,0
	.byte $70,$71,$72,$73,$74,$75,$76,$77,$75,$76,$77,$78,$79,0,0,0
	.byte $50,$51,$52,$53,$54,$55,$56,$57,$55,$56,$57,$58,$59,0,0,0
	.byte $60,$61,$62,$63,$64,$65,$66,$67,$65,$66,$67,$68,$69,0,0,0
	.byte $70,$71,$72,$73,$74,$75,$76,$77,$75,$76,$77,$78,$79,0,0,0
	.byte $80,$81,$82,$83,$84,$85,$86,$87,$85,$86,$87,$88,$89,0,0,0
	.byte $90,$91,$92,$93,$94,$95,$96,$97,$95,$96,$97,$98,$99,0,0,0

;
;   miscellaneous constants in floating
;   point format
;
zero:	.byte $00,$00,$00,$00,$00,$00,$00,$00
ln2:	.byte $40,$01,$69,$31,$47,$18,$05,$60
ln10:	.byte $00,$00,$23,$02,$58,$50,$92,$99
iln10:	.byte $40,$01,$43,$42,$94,$48,$19,$03
exp0:	.byte $00,$00,$27,$18,$28,$18,$28,$46
sqt2:	.byte $00,$00,$14,$14,$21,$35,$62,$37
pi:	.byte $00,$00,$31,$41,$59,$26,$53,$59
twopi:	.byte $00,$00,$62,$83,$18,$53,$07,$18
sqp5:	.byte $40,$01,$70,$71,$06,$78,$11,$87
invpi:	.byte $40,$01,$31,$83,$09,$88,$61,$84
i2pi:	.byte $40,$01,$15,$91,$54,$94,$30,$92
pio2:	.byte $00,$00,$15,$70,$79,$63,$26,$79
npio2:	.byte $80,$00,$15,$70,$79,$63,$26,$79
tpio2:	.byte $00,$00,$47,$12,$38,$89,$80,$38
pio4:	.byte $40,$01,$78,$53,$98,$16,$33,$98
unit:	.byte $00,$00,$10,$00,$00,$00,$00,$00
half:	.byte $40,$01,$50,$00,$00,$00,$00,$00
nrpi2:	.byte $00,$15,$70,$79,$00,$00,$00,$00
hnth:	.byte $40,$02,$10,$00,$00,$00,$00,$00
ovferr:	.byte $29,$99,$99,$99,$99,$99,$99,$99
rngerr:	.byte $30,1,0,0,0,0,0,0
dmnerr:	.byte $30,2,0,0,0,0,0,0
dvzerr:	.byte $30,3,0,0,0,0,0,0

;
;   Coefficient table for cosine by polynomial evaluation
;
_kcos:	.byte $40,$05,$23,$15,$39,$31,$67,$00
	.byte $c0,$03,$13,$85,$37,$04,$26,$40
	.byte $40,$02,$41,$66,$35,$84,$67,$69
	.byte $c0,$01,$49,$99,$99,$05,$34,$55
	.byte $40,$01,$99,$99,$99,$95,$34,$64

;
;   atn(1), atn(0.1), atn(0.01), atn(0.001), ...
;
;   This table is in floating point format
;
katn:	.byte $40,$01,$78,$53,$98,$16,$33,$97
	.byte $40,$02,$99,$66,$86,$52,$49,$12
	.byte $40,$03,$99,$99,$66,$66,$86,$67
	.byte $40,$04,$99,$99,$99,$66,$66,$67
	.byte $40,$05,$99,$99,$99,$99,$66,$67
	.byte $40,$06,$99,$99,$99,$99,$99,$67
	.byte $40,$06,$10,0,0,0,0,0
	.byte $40,$07,$10,0,0,0,0,0

;
;   atn(1), atn(0.1), atn(0.01), atn(0.001), ...
;
;   This table is in fixed point format
;
_kxatn:	.byte $07,$85,$39,$81,$63,$39,$74,$48
	.byte $00,$99,$66,$86,$52,$49,$11,$62
	.byte $00,$09,$99,$96,$66,$68,$66,$65
	.byte $00,$00,$99,$99,$99,$66,$66,$67
	.byte $00,$00,$09,$99,$99,$99,$96,$67
	.byte $00,$00,$01,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$10,$00,$00,$00,$00
	.byte $00,$00,$00,$01,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$10,$00,$00,$00
	.byte $00,$00,$00,$00,$01,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$10,$00,$00
	.byte $00,$00,$00,$00,$00,$01,$00,$00

;
;   ln(2), ln(1.1), ln(1.01),ln(1.001),...
;
;   This table is in floating point format
;
_klog:	.byte $40,$01,$69,$31,$47,$18,$05,$60
	.byte $40,$02,$95,$31,$01,$79,$80,$43
	.byte $40,$03,$99,$50,$33,$08,$53,$17
	.byte $40,$04,$99,$95,$00,$33,$30,$84
	.byte $40,$05,$99,$99,$50,$00,$33,$33
	.byte $40,$06,$99,$99,$95,$00,$00,$33
	.byte $40,$07,$99,$99,$99,$50,$00,$00
	.byte $40,$08,$99,$99,$99,$95,$00,$00
	.byte $40,$09,$99,$99,$99,$99,$50,$00
	.byte $40,$10,$99,$99,$99,$99,$95,$00
	.byte $40,$11,$99,$99,$99,$99,$99,$50

;
;   ln(2), ln(1.1), ln(1.01), ln(1.001),...
;
;   This table is in fixed decimal format
;
_kxlog:	.byte $06,$93,$14,$71,$80,$55,$99,$45
	.byte $00,$95,$31,$01,$79,$80,$43,$25
	.byte $00,$09,$95,$03,$30,$85,$31,$68
	.byte $00,$00,$99,$95,$00,$33,$30,$84
	.byte $00,$00,$09,$99,$95,$00,$03,$33
	.byte $00,$00,$00,$99,$99,$95,$00,$00
	.byte $00,$00,$00,$09,$99,$99,$95,$00
	.byte $00,$00,$00,$00,$99,$99,$99,$95
	.byte $00,$00,$00,$00,$10,$00,$00,$00
	.byte $00,$00,$00,$00,$01,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$10,$00,$00
	.byte $00,$00,$00,$00,$00,$01,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$10,$00
	.byte $00,$00,$00,$00,$00,$00,$01,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$10
	.byte $00,$00,$00,$00,$00,$00,$00,$01

_kln:	.byte $C0,$01,$25,$00,$00,$00,$00,$00
	.byte $40,$01,$33,$33,$33,$33,$33,$33
	.byte $C0,$01,$50,$00,$00,$00,$00,$00
	.byte $00,$00,$10,$00,$00,$00,$00,$00
.scend
