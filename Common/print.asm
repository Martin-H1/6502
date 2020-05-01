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

.scend
