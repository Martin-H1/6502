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
.data BSS
.space INTvector 3		; holds application interrupt vector & checksum
.space NMIvector 3		; holds application NMI vector & checksum
.text

;
; Macros
;

;
; Functions
;

; Interrupt handler for RESET button, also boot sequence. 
.scope
resetv:
	sei		; diable interupts, until interupt vectors are set.
	cld		; clear decimal mode
	ldx #$FF	; reset stack pointer
	txs

	lda #$00	; clear all three registers
	tax
	tay

	pha		; clear all flags
	plp
	jmp main	; go to main program initialization.
.scend

; interrupt vector handlers to be safe, but we only RTI as we have nothing
; to do here.
irqv:	rti

nmiv:
panic:
	rti

; Interrupt vectors.
.advance $FFFA

.word nmiv    ; NMI vector 
.word resetv  ; RESET vector
.word irqv    ; IRQ vector
