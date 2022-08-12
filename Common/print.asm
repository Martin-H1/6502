; -----------------------------------------------------------------------------
; Print related functions that are platform independant, unlike console I/O
; which is platform dependant.
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
.space _gthan 2		; a counter cell.

.text

;
; Macros
;

; Syntactic sugar around print to package the argument.
.macro print
	`pushi _1
        jsr cputs
.macend

; Prints the string an adds a line feed.
.macro println
	`print _1
	`printcr
.macend

; Prints a line feed.
.macro printcr
        lda #AscLF
        jsr putch
.macend

;
; Functions
;

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

; prints the entire stack.
printstack:
.scope
	phx
_while:
	cpx #SP0
	beq +
	jsr printtosln
	`drop
	bra _while
*
	`printcr
	plx
	rts
.scend

; prints the value at the top of stack.
printtos:
        lda TOS_MSB,x
        jsr printa
        lda TOS_LSB,x
        jsr printa
	rts

; prints the value at the top of stack.
printtosln:
	jsr printtos
	`printcr
	rts

; prints the value at the top of stack.
printTosSigned:
	lda TOS_MSB,x
	bpl printtos		; if positive print as usual.
	lda #$2d
	jsr putch
	`dup			; duplicate and
	sec			; calculate 0 - tos
	lda #$00
	sbc TOS_LSB,x
	sta TOS_LSB,x
	lda #$00
	sbc TOS_MSB,x
	sta TOS_MSB,x
	jsr printtos
	`drop
	rts

; prints the value at the top of stack.
printTosSignedln:
	jsr printTosSigned
	`printcr
	rts

printTosDecS:
	lda TOS_MSB,x
	bpl printTosDec
	lda #$2d		; Print sign
	jsr putch
	sec			; calculate 0 - tos
	lda #$00
	sbc TOS_LSB,x
	sta TOS_LSB,x
	lda #$00
	sbc TOS_MSB,x
	sta TOS_MSB,x		; fall into printTosDec unsigned.

; Print an unsigned decimal number word on data stack.
; I got this off the 6502 forum and I am unsure who wrote it.
printTosDec:
.scope
	lda #0			; null delimiter for print
	pha
_prnum2:			; divide TOS by 10
	lda #0
	sta _gthan+1		; clr BCD
	lda #16
	sta _gthan		; {>} = loop counter
_prdiv1:
	asl TOS_LSB,x		; TOS is gradually replaced
	rol TOS_MSB,x		; with the quotient
	rol _gthan+1		; BCD result is gradually replaced
	lda _gthan+1		; with the remainder
	sec
	sbc #10			; partial BCD >= 10 ?
	bcc _prdiv2
	sta _gthan+1		; yes: update the partial result
	inc TOS_LSB,x		; set low bit in partial quotient
_prdiv2:
	dec _gthan
	bne _prdiv1		; loop 16 times
	lda _gthan+1
	ora #\'0		; convert BCD result to ASCII
	pha			; stack digits in ascending
	lda TOS_LSB,x		; order ('0' for zero)
	ora TOS_MSB,x
	bne _prnum2		; } until TOS is 0
	pla
_prnum3:
	jsr putch		; print digits in descending
	pla			; order until delimiter is
	bne _prnum3		; encountered
	`drop
rts
.scend

.scend
