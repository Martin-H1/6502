; -----------------------------------------------------------------------------
; Boot and interrupt handlers.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;

;
; Data segments
;

;
; Macros
;

;
; Functions
;

; Interrupt handler for RESET button, also boot sequence. 
.scope
resetv: 
        ldx #$FF        ; reset stack pointer
        txs

        lda #$00        ; clear all three registers
        tax
        tay

        pha             ; clear all flags
        plp             
irqv:	jmp main
.scend

; redirect the NMI interrupt vector here to be safe, but this 
; should never be reached for py65mon.
nmiv:
panic:
        jmp resetv

; Interrupt vectors.
.advance $FFFA

.word nmiv    ; NMI vector 
.word resetv  ; RESET vector
.word irqv    ; IRQ vector 
