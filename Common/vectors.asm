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
.space BRKvector 3		; holds application break vector
.space RESvector 3		; holds application reset vector & checksum
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
	jmp main	; go to monitor or main program initialization.

irqv:	jmp (INTvector)
.scend

; redirect the NMI interrupt vector here to be safe, but this 
; should never be reached for py65mon.
nmiv:
panic:
	jmp (NMIvector)

; Interrupt vectors.
.advance $FFFA

.word nmiv    ; NMI vector 
.word resetv  ; RESET vector
.word irqv    ; IRQ vector
