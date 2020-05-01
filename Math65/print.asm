; Print related functions that are platform independant, unlike console I/O
; which is platform dependant.

; Syntactic sugar around print to package the argument.
.macro print
        lda #<_1
        sta STRNGPTR
        lda #>_1
        sta STRNGPTR+1
        jsr cputs
.macend

; Prints the string an adds a line feed.
.macro println
	`print
	`printcr
.macend

; Prints a line feed.
.macro printcr
        lda #AscLF
        jsr putch
.macend

; Prints word in hex to the console.
.macro printhex
        lda _1+1
        jsr printa
	lda _1
	jsr printa
.macend

; cputs is like the MSDOS console I/O function. It prints a null terminated
; string to the console using putch.
cputs:
.scope
	phy                     ; save Y register
        ldy #$00                ; index

*       lda (STRNGPTR),y         ; get the string via address from zero page
        beq +			; if it is a zero, we quit and leave
        jsr putch	        ; if not, write one character
        iny                     ; get the next byte
        bra -
*       ply
        rts
.scend

;  prints a float pointed to on the stack
printfloat:
	`savetos TMPPTR1
	ldy #0
*	lda (TMPPTR1), y
	jsr printa
	iny
	cpy #04
	bne -
	rts

; prints the accumulator contents in hex to the console.
printa:
	pha
	lsr
	lsr
	lsr
	lsr
	jsr _print_nybble
	pla
	and #$0f
_print_nybble:
	sed
	clc
	adc #$90	        	; Produce $90-$99 or $00-$05
	adc #$40			; Produce $30-$39 or $41-$46
	cld
	jmp putch

.macro printsign
	bpl _over
	lda #'\-
	jsr putch
_over:
.macend

; prints a BCD floating point number.
; pointer on the stack, no outputs.
printbcd:
	`savetos TMPPTR1
	`drop
	lda (TMPPTR1)		; Load the sign byte
	`printsign
	ldy #2			; Skip over the exponent for now.
	lda (TMPPTR1),y		; The first digit needs special handling.
	pha
	lsr
	lsr
	lsr
	lsr
	jsr _print_nybble
	lda #'.
	jsr putch
	pla
	and #$0f
	jsr _print_nybble
	iny
*	lda (TMPPTR1),y		; Do the remaining digits.
	jsr printa
	iny
	cpy #8
	bne -
	lda #$65		; Print exponent
	jsr putch
	lda (TMPPTR1)		; print the sign of the exponent.
	asl
	`printsign
	lda (TMPPTR1)		; print the exponent.
	and #$0f
	jsr _print_nybble
	ldy #1
	lda (TMPPTR1), y
	jsr printa
	rts

; prints the entire stack.
printstack:
.scope
	phx
_while:
	cpx #SP0
	beq +
	jsr printtos
	`drop
	bra _while
*
	`printcr
	plx
	rts
.scend

; prints the value at the top of stack.
printtos:
	`savetos STRNGPTR
	`printhex STRNGPTR
	`printcr
	rts
