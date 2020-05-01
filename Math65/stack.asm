; The argument stack is the first half of zero page. It starts at $7F and
; grows downward towards $00 (128 bytes --> 64 words). This allows for
; over and underlow detection via highest bit being zero. It also reserves
; half of zero page for direct addressing.

.alias NOS_LSB	$03		; offset from X register for NOS and TOS.
.alias NOS_MSB	$04
.alias TOS_LSB	$01
.alias TOS_MSB	$02

; pushes the immediate literal provided as the argument
.macro pushi
        dex			; make room on the argument stack
	dex
        lda #<_1		; LSB
        sta TOS_LSB,x
        lda #>_1		; MSB
        sta TOS_MSB,x
.macend

; pushes the value at the address specified at the argument.
.macro pushv
        dex			; make room on the argument stack
	dex
        lda _1			; LSB
        sta TOS_LSB,x
        lda _1+1		; MSB
        sta TOS_MSB,x
.macend

; pushes zero onto the stack
.macro pushzero
	dex			; make room on the argument stack
	dex
	`zerotos
.macend

; drops a word from the stack
.macro drop
        inx
	inx
.macend

; check for tos equals zero.
.macro toszero?
	lda TOS_LSB, x
	ora TOS_MSB, x
.macend

; saves the word at TOS to the location provided.
.macro savetos
	lda TOS_LSB,x
	sta _1
	lda TOS_MSB,x
	sta _1+1
.macend

; makes the TOS zero
.macro zerotos
	stz TOS_LSB, x
	stz TOS_MSB, x
.macend

; duplicates the value at TOS on the stack.
dup:
        dex			; make room on the stack
        dex
        lda NOS_LSB,x		; copy the word.
        sta TOS_LSB,x
        lda NOS_MSB,x
        sta TOS_MSB,x
	rts

; fetch dereferences the current TOS and replaces the value on TOS.
fetch:
        lda TOS_LSB,x
        sta STRNGPTR		; LSB
        lda TOS_MSB,x
        sta STRNGPTR+1		; MSB
        lda (STRNGPTR)		; LSB of address in memory
	sta TOS_LSB,x

	`incw STRNGPTR

	lda (STRNGPTR)		; MSB of address in memory
        sta TOS_MSB,x
        rts

; stores the value in NOS at the address specified in TOS and drops
; the values from the stack.
store:
        lda NOS_LSB,x		; LSB
        sta (TOS_LSB,x)

	inc TOS_LSB,x
        bne +
	inc TOS_MSB,x
*
	lda NOS_MSB,x		; MSB
        sta (TOS_LSB,x)

	`drop
	`drop
        rts

; swaps top of stack (TOS) to next on stack (NOS)
swap:
        lda NOS_LSB,x		; LSB of both words first
        pha			; use stack as a temporary
        lda TOS_LSB,x
        sta NOS_LSB,x
        pla
        sta TOS_LSB,x

        lda NOS_MSB,x		; MSB next
        pha
        lda TOS_MSB,x
        sta NOS_MSB,x
        pla
        sta TOS_MSB,x

        rts
