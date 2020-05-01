; -----------------------------------------------------------------------------
; C-rtl string functions for 6502 assembly.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

;
; Aliases
;

; Character set (ASCII)
.alias AscBS	$08	; backspace ASCII character
.alias AscCC	$03	; break (Control-C) ASCII character
.alias AscCR	$0D	; carriage return ASCII character
.alias AscDEL	$7F	; DEL ASCII character
.alias AscESC	$1B	; Escape ASCII character
.alias AscFF	$0C	; Formfeed ASCII character
.alias AscLF	$0A	; line feed ASCII character
.alias AscSP	$20	; space ASCII character
.alias AscTAB	$09	; tab ASCII character
.alias AscVT	$0B	; vertical tab ASCII character

;
; Data segments
;
.data ZPDATA
.space _TMPPTR1	2	; pointers used by print and string functions.
.space _TMPPTR2	2

.data BSS
.space _RESULT	2	; Holds the temporary result during number parsing.
.space _SIGN	1	; Holds the sign during number parsing.
.space _TEMP	1	; Useful temporary

.text

;
; Macros
;

;
; Functions
;

; Compares character in accumulator to determine if it is alphabetic.
; input - character in accumulator
; output - processor condition codes.
isalpha:
.scope
	cmp #'A
	bmi _false
	cmp #'z
	bpl _false
	sta _TMPPTR2
	cmp _TMPPTR2
_false:
	rts
.scend

; Compares character in accumulator to determine if it is numeric.
; input - character in accumulator
; output - processor condition codes.
isdigit:
.scope
	cmp #'0
	bmi _false
	cmp #'9
	bpl _false
	sta _TEMP
	cmp _TEMP
_false:
	rts
.scend

; Compares character in TOS to determine if it is an operator.
; "+ ", "- ", "* ", "/ ", "= ", "< ", "> ", "<=", ">="
; input - two characters packed in TOS
; output - processor condition codes, tos is consumed.
isoperator:
.scope
	lda TOS_LSB,x
	cmp #'\+
	beq _isspace
	cmp #'\-
	beq _isspace
	cmp #'\*
	beq _isspace
	cmp #'\/
	beq _isspace
	cmp #'\=
	beq _isspace
	lda TOS_MSB,x		; if second char is =, it must be > or <
	cmp #'\=
	beq _isGtLtEq
	lda TOS_LSB,x		; otherwise check for space
	cmp #'\>
	beq _isspace
	cmp #'\<
	beq _isspace
	`drop
	rts

_isspace:
	; next character must be a space
	lda TOS_MSB,x
	bra +
*	`drop
	cmp #AscSP
	rts

_isGtLtEq:
	lda TOS_LSB,x
	`drop
	cmp #'\>		; otherwise check for > or <.
	beq +
	cmp #'\<
*	rts
.scend

; Compares character in accumulator to determine if it is a sign.
; input - character in accumulator
; output - processor condition codes.
issign:
.scope
	cmp #'\+
	beq +
	cmp #'\-
*	rts
.scend

; Compares character in accumulator to determine if it is a space.
; input - character in accumulator
; output - processor condition codes.
isspace:
.scope
	cmp #AscSP
	beq +
	cmp #AscTAB
	beq +
	cmp #AscVT
	beq +
	cmp #AscFF
	beq +
	cmp #AscCR
*
	rts
.scend

; Copies a block of data from location to another. They must not overlap,
; or unexpected results will happen.
; input - src pointer, dest pointer, bytes to copy.
; output - arguments removed from stack
memcpy:
.scope
*	phy
	`popToR
	`pop _TMPPTR2
	`pop _TMPPTR1
	`pushFromR
	ldy #0
_loop:	lda (_TMPPTR1),y
	sta (_TMPPTR2),y
	iny
	bne +
	inc _TMPPTR2+1
	inc _TMPPTR1+1
*	`decTos
	bne _loop
	ply
	`drop			; return value is under zero.
	rts
.scend

; strchr returns a pointer to the first instance of a character in a string.
; input - a string pointer on the stack and a character in the accumulator.
; output - a pointer or null on the stack
strchr:
.scope
	phy
	sta _TMPPTR2
	`pop _TMPPTR1
	ldy #00
_for:	lda (_TMPPTR1),y
	bne +
	`pushi 0
	ply
	rts
*	cmp _TMPPTR2
	beq _endfor
	iny
	bra _for
_endfor:
	clc			; Advance the string pointer by y
	tya
	adc _TMPPTR2
	sta _TMPPTR2
	lda _TMPPTR2+1
	adc #0
	sta _TMPPTR2+1
	`push _TMPPTR2		; push it on to the stack
	ply			; restore y and return
	rts
.scend

; strcmp takes two string address parameters on the stack.
; It returns the following in the acumulator
; -1 if NOS < TOS
; 0 if TOS = NOS
; 1 fi NOS > TOS
strcmp:
.scope
	; copy the pointers to allow indirect addressing.
	`pop _TMPPTR1
	`pop _TMPPTR2
	ldy	#0

_loop:	lda (_TMPPTR1),y
	cmp (_TMPPTR2),y
	bne _L1
	cmp #0  		; end of strings?
	beq _L3
	iny
	bne _loop
	inc _TMPPTR1+1
	inc _TMPPTR2+1
	bne _loop

_L1:	bcs _L2
	lda #$FF
	rts

_L2:	lda #$01
_L3:	rts
.scend

; strcpy copies a null terminated string.
; TOS - address of string to copy.
; NOS - address of destination.
; returns - pointer to destination buffer.
strcpy:
.scope
	phy
	`pop _TMPPTR1
	`peek _TMPPTR2
	ldy #0
_loop:	lda (_TMPPTR1),y
	bne +
	ply
	rts
*
	sta (_TMPPTR2),y
	iny
	bne _loop
	inc _TMPPTR1+1
	inc _TMPPTR2+1
	bne _loop
.scend

; strdup takes a string address on the stack. It returns the address of
; a duplicate on the stack. The consumer is responsible for retaining the
; original pointer and freeing the new allocation. As this uses raw heap
; pointers, it should not be used if a garbage collector is in use.
strdup:
	`dup
	jsr strlen
	`pushi 1
	jsr add16
	jsr halloc
	`swap
	jsr strcpy
	rts

; strlen takes a string address on the stack. It returns the length in
; bytes of the string (not including the terminating null) on the stack.
strlen:
.scope
	; copy the pointer to allow indirect addressing.
	`peek _TMPPTR1
*
	lda (_TMPPTR1)
	beq _break
	`incw _TMPPTR1
	bra -
_break:
	`push _TMPPTR1		; copy the pointer back to the stack.
	`swap
	jsr sub16		; the difference is the lengh
	rts
.scend

; strtoi takes a string address on the stack and parses it for a number.
; It returns a word value at TOS.
strtoi:
.scope
	phy
	stz _RESULT		; Zero out result
	stz _RESULT+1
	stz _SIGN		; assume positive
	lda (TOS_LSB,x)		; check the sign.
	cmp #'\-
	bne +
	lda #$ff
	sta _SIGN
*	jsr strlen		; TOS contains number of digits.
	`decw _TMPPTR1		; _TMPPTR points to last digit
	`pushi _vals
	`pop _TMPPTR2
_loop:	`toszero?		; Are all digits processed?
	beq _endloop
	lda (_TMPPTR1)
	jsr isDigit
	bne _next
	sec
	sbc #'0
	asl
	tay
	`pushIndy _TMPPTR2
	`push _result
	jsr add16
	`pop _result
_next:	`decTos
	`decw _TMPPTR1
	`push _TMPPTR2
	`pushi $14
	jsr add16
	`pop _TMPPTR2
	bra _loop
_endloop:
	`drop			; drop digit count and push result.
	`push _RESULT
	lda _SIGN
	beq +
	jsr neg16
*	ply
	rts

; Base ten digit values in binary
_vals:	.word $0, $001, $002, $003, $004, $005, $006, $007, $008, $009
	.word $0, $00a, $014, $01e, $028, $032, $03c, $046, $050, $05a
	.word $0, $064, $0c8, $12c, $190, $1f4, $258, $2bc, $320, $384
	.word $0, $3e8, $7d0, $bb8, $fa0, $1388, $1770, $1b58, $1f40, $2328
	.word $0, $2710, $4e20, $7530

.scend

.scend
