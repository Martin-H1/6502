; -----------------------------------------------------------------------------
; Boot, Reset, Break, and interrupt handlers.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;

;
; Data segments
;
.data MONDATA
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

.advance $FF00
; Jump table for monitor routines. These locations will not change in future
; versions, and the table will only be appened to. This allows user programs
; to access monitor routines for basic input and output.
jumpTableStart:
	jmp biosInitImpl
	jmp biosBellImpl
	jmp biosCLSImpl
	jmp biosCRLFImpl
	jmp biosCgetsImpl
	jmp biosCputsImpl
	jmp biosGetchImpl
	jmp biosHomeImpl
	jmp biosMonitorImpl
	jmp biosPutHexImpl
	jmp biosPutchImpl
	jmp biosSetCursorImpl
	jmp biosSetEchoImpl
	jmp biosUngetchImpl
	jmp biosWarmBootImpl
jumpTableEnd:

; Interrupt vectors.
.advance $FFFA

.word nmiv    ; NMI vector
.word resetv  ; RESET vector
.word irqv    ; IRQ vector
