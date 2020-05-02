; This module is based upon code from Roy Rankin and Steve Wozniak in
; a 1976 Dr Dobb's article, but reworked for Ophis, the 65c02, and
; compatiblity with a Forth style argument stack.
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
;	exponent byte 1
;	mantissa bytes 2-4
;
;  Exponent    Two's Complement Mantissa
;  SEEEEEEE  SM.MMMMMM  MMMMMMMM  MMMMMMMM
;     n         n+1       n+2       n+3
;
; mantissa:
;	A two's compliment representation with sign in msb of high-order byte.
;	The mantissa is normalized with an assumed decimal point between bits
;	5 and 6 of the high-order byte.  Thus the mantissa is in the range 1
;	to 2. except when the number is less than 2**(-128).
;
; exponent:
;	The exponent represents powers of two.  The representation is a 2's
;	compliment except that the sign bit (bit 7) is complimented. This
;	allows direct comparison of exponents for size since they are stored
;	in increasing numerical sequence ranging from $00 (-128) to $ff (+127)
;
; This allows the representation of decimal numbers in the approximate
; range of 10**(-38) through 10**(38) with 6 to 7 significant digits.

; Use scope operator to hide all internal entry points.
.scope

.alias TMPPTR1	$FC		; working pointer used by heap functions.
.alias TMPPTR2	$FE		; working pointer used by heap functions.

.macro copy4
	ldx #$03
_loop:
	lda _1, x
	sta _2, x
	dex
	bpl _loop
.macend

.macro zero4
	ldx #$03
_loop:
	stz _1, x
	dex
	bpl _loop
.macend

;
; External entry points here.
;
fcompare32:
	jsr _copyargs
	phx
	jsr _compare
	php
	plx
	plp
	rts

fln32:
	jsr _copyarg
	phx
	jsr _log
	plx
	`pushi x1
	rts

flog32:
	jsr _copyarg
	phx
	jsr _log10
	plx
	`pushi x1
	rts

;
; Accepts a 16 bit integer on the stack, converts it to a float.
; Returns a pointer to the 32 bit converted value.
;
float32:
	; copy integer from stack to mantissa
	`peek m1
	`drop
	phx
	jsr _float
	plx
	`pushi x1
	rts

fixed32:
	jsr _copyarg
	phx
	jsr _fix
	plx
	`pushi x1
	rts

fadd32:
	jsr _copyargs
	phx
	jsr _fadd
	plx
	`pushi x1
	rts

fsub32:
	jsr _copyargs
	phx
	jsr _fsub
	plx
	`pushi x1
	rts

fmul32:
	jsr _copyargs
	phx
	jsr _fmul
	plx
	`pushi x1
	rts

fdiv32:
	jsr _copyargs
	phx
	jsr _fdiv
	plx
	`pushi x1
	rts

fexp32:
	jsr _copyarg
	phx
	jsr _exp
	plx
	`pushi x1
	rts

_copyarg:
	`peek TMPPTR1
	`drop
	ldy #$03
*	lda (TMPPTR1), y
	sta x1, y
	dey
	bpl -
	rts

_copyargs:
	`peek TMPPTR1
	`drop
	`peek TMPPTR2
	`drop
	ldy #$03
*	lda (TMPPTR1), y
	sta x1, y
	lda (TMPPTR2), y
	sta x2, y
	dey
	bpl -
	rts

;  prints a float pointed to on the stack
printfloat:
	`peek TMPPTR1
	ldy #0
*	lda (TMPPTR1), y
	jsr printa
	iny
	cpy #04
	bne -
	rts

;
; Rankin and Woz code starts here, but reformated for Ophis assembler.
;

;
; Aliases to page zero transfer locations (total 25 bytes).
;
.alias FPAREA	$D9		; 25 bytes of floating point work space.
.alias sign	FPAREA + $00
.alias x2	FPAREA + $01	; exponent 2
.alias m2	FPAREA + $02	; mantissa 2
.alias x1	FPAREA + $05	; exponent 1
.alias m1	FPAREA + $06	; mantissa 1
.alias e	FPAREA + $09	; scratch region
.alias z	FPAREA + $0D
.alias t	FPAREA + $11
.alias sexp	FPAREA + $15

;
; Natural log of mant/exp1 with result in mant/exp1.
;
_log:
.scope
	lda m1
	beq _error
	bpl +			; if arg>0 ok
_error:	brk			; error arg<=0
;
*	jsr _swap		; move arg to exp/mant2
	ldx #0			; mod 9/76: load x for later
	lda x2			; hold exponent
	ldy #$80
	sty x2		 	; set exponent 2 to 0 ($80)
	eor #$80		; compliment sign bit of original exponent
	sta m1+1		; set exponent into mantissa 1 for float
	bpl +			; mod 9/76: is exponent zero?
	dex			; mod 9/76: yes set x to $ff
*       stx m1			; mod 9/76: set upper byte of exponent
	jsr _float		; convert to floating point
	ldx #3			; 4 byte transfers
*	lda x2,x
	sta z,x			; copy mantissa to z
	lda x1,x
	sta sexp,x		; save exponent in sexp
	lda _r22,x		; load exp/mant1 with sqrt(2)
	sta x1,x
	dex
	bpl -
	jsr _fsub		; z-sqrt(2)
	ldx #3			; 4 byte transfer
*	lda x1,x		; save exp/mant1 as t
	sta t,x
	lda z,x			; load exp/mant1 with z
	sta x1,x
	lda _r22,x		; load exp/mant2 with sqrt(2)
	sta x2,x
	dex
	bpl -
	jsr _fadd		; z+sqrt(2)
	`copy4 t, x2		; load t into exp/mant2
	jsr _fdiv		; t=(z-sqrt(2))/(z+sqrt(2))
	ldx #3			; 4 byte transfer
*	lda x1,x
	sta t,x			; copy exp/mant1 to t and
	sta x2,x		; load exp/mant2 with t
	dex
	bpl -
	jsr _fmul		; t*t
	jsr _swap		; move t*t to exp/mant2
	`copy4 _c, x1		; load exp/mant1 with c
	jsr _fsub		; t*t-c
	`copy4 _mb, x2		; load exp/mant2 with _mb
	jsr _fdiv		; _mb/(t*t-c)
	`copy4 _a1, x2		; load exp/mant2 with _a1
	jsr _fadd		; _mb/(t*t-c)+_a1
	`copy4 t, x2		; load exp/mant2 with t
	jsr _fmul		; (_mb/(t*t-c)+_a1)*t
	`copy4 _mhlf, x2	; load exp/mant2 with _mhlf (.5)
	jsr _fadd		; +.5
	`copy4 sexp, x2		; load exp/mant2 with original exponent
	jsr _fadd		; +expn
	`copy4 _le2, x2		; load exp/mant2 with ln(2)
	jsr _fmul		; *ln(2)
	rts			; return result in mant/exp1
.scend

;
; Common log of mant/exp1 result in mant/exp1
;
_log10:
.scope
	jsr _log		; compute natural log
	`copy4 _ln10, x2	; load exp/mant2 with 1/ln(10)
	jsr _fmul		; log10(x)=ln(x)/ln(10)
	rts
.scend

; pre-computed floating point constants.
_ln10:	.byte $7E,$6F,$2D,$ED	; 0.4342945
_r22:	.byte $80,$5A,$82,$7A	; 1.4142136   sqrt(2)
_le2:	.byte $7F,$58,$B9,$0C	; 0.69314718  log base E of 2
_a1:	.byte $80,$52,$80,$40	; 1.2920074
_mb:	.byte $81,$AB,$86,$49	; -2.6398577
_c:	.byte $80,$6A,$08,$66	; 1.6567626
_mhlf:	.byte $7F,$40,$00,$00	; 0.5

;
;     exp of mant/exp1 result in mant/exp1
;
_exp:
.scope
	`copy4 _l2e, x2		; load exp/mant2 with log base 2 of e
	jsr _fmul		; log2(3)*x
	`copy4 x1, z		; store ln(2)*x in z
	jsr _fix		; convert contents of exp/mant1 to an integer
	lda m1+1
	pha			; save result on stack for later use.
	sec			; set carry for subtraction
	sbc #124		; int-124
	lda m1
	sbc #0
	bpl _ovflw		; overflow int>=124
	clc			; clear carry for add
	lda m1+1
	adc #120		; add 120 to int
	lda m1
	adc #0
	bpl +			; if result positive continue
	`zero4 x1		; int<-120 set exp/mant1 to zero and return
	pla			; drop unused int value
	rts			; return
;
_ovflw:	pla
	brk			; overflow
;
*	jsr _float		; float int
	`copy4 z, x2		; load exp/mant2 with z
	jsr _fsub		; z*z-float(int)
	ldx #3			; 4 byte move
*	lda x1,x
	sta z,x			; save exp/mant1 in z
	sta x2,x		; copy exp/mant1 to exp/mant2
	dex
	bpl -
	jsr _fmul		; z*z
	ldx #3			; 4 byte move
*	lda _a2,x
	sta x2,x		; load exp/mant2 with _a2
	lda x1,x
	sta sexp,x		; save exp/mant1 as sexp
	dex
	bpl -
	jsr _fadd		; z*z+_a2
	`copy4 _b2, x2		; load exp/mant2 with _b2
	jsr _fdiv		; t=b/(z*z+_a2)
	ldx #3			; 4 byte move
*	lda x1,x
	sta t,x			; save exp/mant1 as t
	lda _c2,x
	sta x1,x		; load exp/mant1 with _c2
	lda sexp,x
	sta x2,x		; load exp/mant2 with sexp
	dex
	bpl -
	jsr _fmul		; z*z*c2
	jsr _swap		; move exp/mant1 to exp/mant2
	`copy4 t, x1		; load exp/mant1 with t
	jsr _fsub		; c2*z*z-b2/(z*z+_a2)
	`copy4 _d, x2		; load exp/mant2 with _d
	jsr _fadd		; d+c2*z*z-b2/(z*z+a2)
	jsr _swap		; move exp/mant1 to exp/mant2
	`copy4 z, x1		; load exp/mant1 with z
	jsr _fsub		; -z+d+c2*z*z-b2/(z*z+a2)
	`copy4 z, x2		; load exp/mant2 with z
	jsr _fdiv		; z/(**** )
	`copy4 _mhlf, x2	; load exp/mant2 with .5
	jsr _fadd		; +z/(***)+.5
	sec			; add int to exponent with carry set
	pla			; to multiply by int save on stack.
	adc x1			; 2**(int+1)
	sta x1			; return result to exponent
	rts			; return ans=(.5+z/(-z+d+c2*z*z-b2/(z*z+a2))*2**(int+1)
.scend

; additional pre-computed floating point constants.
_l2e:	.byte $80,$5C,$55,$1E	; 1.4426950409  log base 2 of E
_a2:	.byte $86,$57,$6A,$E1	; 87.417497202
_b2:	.byte $89,$4D,$3F,$1D	; 617.9722695
_c2:	.byte $7B,$46,$4A,$70	; .03465735903
_d:	.byte $83,$4F,$A3,$03	; 9.9545957821

;
; basic floating point routines
;

;
; Add for the mantissas only
;
_add:
.scope
	clc			; clear carry
	ldx #$02		; index for 3-byte add
*	lda m1,x
	adc m2,x		; add a byte of mant2 to mant1
	sta m1,x
	dex			; advance index to next more signif.byte
	bpl -			; loop until done.
	rts			; return
.scend

_md1:
.scope
	asl sign		; clear lsb of sign
	jsr _abswp		; abs val of mant1, then swap mant2
_abswp:	bit m1			; mant1 neg?
	bpl +			; no,swap with mant2 and return
	jsr fcompl		; yes, compliment it.
	inc sign		; incr sign, complementing lsb
*	sec			; set carry for return to mul/div
	;; falls through to _swap!
.scend

;
; swap exp/mant1 with exp/mant2
;
_swap:
.scope
	ldx #$04		; index for 4-byte swap.
*	sty e-1,x
	lda x1-1,x		; swap a byte of exp/mant1 with
	ldy x2-1,x		; exp/mant2 and leavea copy of
	sty x1-1,x		; mant1 in e(3bytes). e+3 used.
	sta x2-1,x
	dex			; advance index to next byte
	bne -			; loop until done.
	rts
.scend

;
; Convert 16 bit integer in m1(high) and m1+1(low) to f.p.
; result in exp/mant1.  exp/mant2 uneffected
;
_float:
.scope
	lda #$8e
	sta x1			; set expn to 14 dec
	lda #0			; clear low order byte
	sta m1+2
	beq _norm		; normalize result
*	dec x1			; decrement exp1
	asl m1+2
	rol m1+1		; shift mant1 (3 bytes) left
	rol m1
.scend
_norm:
.scope
	lda m1			; high order mant1 byte
	asl			; upper two bits unequal?
	eor m1
	bmi +			; yes,return with mant1 normalized
	lda x1			; exp1 zero?
	bne -			; no, continue normalizing
*	rts			; return
.scend

;
; exp/mant2-exp/mant1 result in exp/mant1
;
_fsub:
	jsr fcompl		; cmpl mant1 clears carry unless zero
_swpalg:
	jsr algnsw		; right shift mant1 or swap with mant2 on carry

;
; Add exp/mant1 and exp/mant2 result in exp/mant1
;
_fadd:
.scope
	lda x2
	cmp x1			; compare exp1 with exp2
	bne _swpalg		; if unequal, swap addends or align mantissas
	jsr _add		; add aligned mantissas
.scend
_addend:
.scope
	bvc _norm		; no overflow, normalize results
	bvs _rtlog		; ov: shift mant1 right. note carry is correct sign
algnsw:	bcc _swap		; swap if carry clear, else shift right arith.
rtar:	lda m1			; sign of mant1 into carry for
	asl			; right arith shift
_rtlog:	inc x1			; incr exp1 to compensate for rt shift
	beq _ovfl		; exp1 out of range.
rtlog1:	ldx #$fa		; index for 6 byte right shift
_ror1:	lda #$80
	bcs +
	asl
*	lsr e+3,x		; simulate ror e+3,x
	ora e+3,x
	sta e+3,x
	inx			; next byte of shift
	bne _ror1		; loop until done
	rts			; return
.scend

;
; exp/mant1 x exp/mant2 result in exp/mant1
;
_fmul:
.scope
	jsr _md1		; abs. val of mant1, mant2
	adc x1			; add exp1 to exp2 for product exponent
	jsr _md2		; check product exp and prepare for mul
	clc			; clear carry
_mul1:	jsr rtlog1		; mant1 and e right.(product and mplier)
	bcc +			; if carry clear, skip partial product
	jsr _add		; add multiplican to product
*  	dey			; next mul iteration
	bpl _mul1		; loop until done
mdend:	lsr sign		; test sign (even/odd)
normx:	bcc _norm		; if exen, normalize product, else complement
fcompl:	sec			; set carry for subtract
	ldx #$03		; index for 3 byte subtraction
*	lda #$00		; clear a
	sbc x1,x		; subtract byte of exp1
	sta x1,x		; restore it
	dex			; next more significant byte
	bne -			; loop until done
	beq _addend		; normalize (or shift right if overflow)
.scend

;
; exp/mant2 / exp/mant1 result in exp/mant1
;
_fdiv:
.scope
	jsr _md1		; take abs val of mant1, mant2
	sbc x1			; subtract exp1 from exp2
	jsr _md2		; save as quotient exp
_loop:	sec			; set carry for subtract
	ldx #$02		; index for 3-byte instruction
*	lda m2,x
	sbc e,x			; subtract a byte of e from mant2
	pha			; save on stack
	dex			; next more signif byte
	bpl -			; loop until done
	ldx #$fd		; index for 3-byte conditional move
_loop2:	pla			; pull a byte of difference off stack
	bcc +			; if mant2<e then dont restore mant2
	sta m2+3,x
*	inx			; next less signif byte
	bne _loop2		; loop until done
	rol m1+2
	rol m1+1		; roll quotient left, carry into lsb
	rol m1
	asl m2+2
	rol m2+1		; shift dividend left
	rol m2
	bcs _ovfl		; overflow is due to unnormalized divisor
	dey			; next divide iteration
	bne _loop		; loop until done 23 iterations
	beq mdend		; normalize quotient and correct sign
.scend
_md2:
.scope
	stz m1+2
	stz m1+1		; clr mant1 (3 bytes) for mul/div
	stz m1
	bcs _ovchk		; if exp calc set carry, check for ovfl
	bmi +			; if neg no underflow
	pla			; pop one
	pla			; return level
	bcc normx		; clear x1 and return
*	eor #$80		; compliment sign bit of exp
	sta x1			; store it
	ldy #$17		; count for 24 mul or 23 div iterations
	rts			; return
_ovchk:	bpl -			; if pos exp then no overflow
.scend

_ovfl:	brk

;
; Convert exp/mant1 to integer in m1 (high) and m1+1(low)
; exp/mant2 is uneffected
;
*	jsr rtar		; shift mant1 rt and increment expnt
_fix:	lda x1			; check exponent
	cmp #$8e		; is exponent 14?
	bne -			; no, shift
	rts

;
; Compares exp/mant1 to exp/mant2
; returns result in processor status bits.
;
_compare:
.scope
	;; First compare exponents.
	lda x1
	cmp x2
	beq _equal

	;; if x1 > x2 then exp/mant1 is greater
_greater:

	;; else if x1 < x2 exp/mant1 is less than
_less:

	;; otherwise we must compare the mantissas
_equal:
	ldx #03
	lda m1
	cmp m2

_return:
	rts
.scend

.scend
