; Interrupt handler for RESET button, also boot sequence. 
.scope
resetv: 
        ldx #$FF        ; reset stack pointer
        txs

        lda #$00        ; clear all three registers
        tax
        tay
        sta ungetFlg	; clear the ungetc flag.

        pha             ; clear all flags
        plp             
irqv:	jmp main
.scend

; redirect the NMI interrupt vector here to be safe, but this 
; should never be reached for py65mon.
nmiv:
panic:
        jmp resetv

; -----------------------------------------------------------------------------
; Definitions for the py65mon emulator
; -----------------------------------------------------------------------------
.alias py65_putc    $f001
.alias py65_getc    $f004

; conio functions unique to each platform.
getch:
.scope
	lda ungetFlg
	beq _getNew
	lda lastChar
	rts
_getNew:
        lda py65_getc
        rts
.scend

ungetch:
.scope
	sta lastChar
	lda #1
	sta ungetFlg

        lda py65_getc
        rts
.scend

putch: 
.scope
        sta py65_putc
        rts
.scend

; Interrupt vectors.
.advance $FFFA

.word nmiv    ; NMI vector 
.word resetv  ; RESET vector
.word irqv    ; IRQ vector 
