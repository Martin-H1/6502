; -----------------------------------------------------------------------------
; Implementation of the Brain F--k virtual machine in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it.
;
; Commands:
;   <	Decrement the data pointer (dptr) to the prior cell.
;   >	Increment the dptr to the next cell.
;   +	Increment the byte at the dptr.
;   -	Decrement the byte at the dptr.
;   .	Output the byte at the dptr.
;   ,	Input a byte and store it in the byte at the dptr.
;   [	If byte at dptr is zero, then jump forward to the matching ].
;   ]	If byte at dptr is nonzero, then loop, otherwise exit the loop.
;
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;

; Character set (ASCII)
.alias AscBS	$08	; backspace ASCII character
.alias AscCC	$03	; break (Control-C) ASCII character
.alias AscCR	$0D	; carriage return ASCII character
.alias AscDEL	$7F	; DEL ASCII character
.alias AscESC	$1B	; Escape ASCII character
.alias AscLF	$0A	; line feed ASCII character
.alias AscSP	$20	; space ASCII character

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space dptr 2		; word to hold the data pointer.
.space iptr 2		; word to hold the instruction pointer.

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space tape 1024	; tape is currently 1K

.text

;
; Macros
;

.org $8000
.outfile "brainf.rom"
.advance $8000

;
; Functions
;
main:
	lda #<tape
	sta dptr
	lda #>tape
	sta dptr+1
	brk

; Decrement the data pointer (dptr) to the prior cell.
decDptr:
.scope
        lda dptr
        bne _over
        dec dptr+1
_over:  dec dptr
	rts
.scend

; Increment the dptr to the next cell.
incDptr:
.scope
	inc dptr
        bne _over
        inc dptr+1
_over:	rts
.scend

; Increment the byte at the dptr.
incCell:
	lda (dptr)
	inc
	sta (dptr)
	rts

; Decrement the byte at the dptr.
decCell:
	lda (dptr)
	dec
	sta (dptr)
	rts

; output the cell value at dptr.
outputCell:
	lda (dptr)
	jsr _putch
	rts

; input a byte and store at dptr.
inputCell:
	jsr _getch
	sta (dptr)
	rts

;   [	If byte at dptr is zero, then jump forward to the matching ].
;   ]	If byte at dptr is nonzero, then loop, otherwise exit the loop.

; conio functions unique to each platform.
.alias _py65_putc	$f001	; Definitions for the py65mon emulator
.alias _py65_getc	$f004

_getch:
.scope
*	lda _py65_getc
	beq -
	rts
.scend

_putch:
.scope
	sta _py65_putc
	rts
.scend

; Interrupt handler for RESET button, also boot sequence.
; Note: This doesn't count for the restricted instruction challenge as
; these steps are required to set the hardware to a known state.
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
.scend

; redirect the NMI interrupt vector here to be safe, but this 
; should never be reached for py65mon.
irqv:
nmiv:
panic:
	brk

; Interrupt vectors.
.advance $FFFA

.word nmiv    ; NMI vector 
.word resetv  ; RESET vector
.word irqv    ; IRQ vector
