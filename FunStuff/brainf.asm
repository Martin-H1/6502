; -----------------------------------------------------------------------------
; Implementation of the Brain F--k virtual machine in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it.
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

.alias AscLT	$3C
.alias AscGT	$3E
.alias AscPlus	$2B
.alias AscComma $2C
.alias AscMinus	$2D
.alias AscDot	$2E
.alias AscRB	$5B
.alias AscLB	$5D

.alias cellsSize [cellsEnd - cells]

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space dptr 2		; word to hold the data pointer.
.space iptr 2		; word to hold the instruction pointer.
.space lidx 2		; word to hold the matching loop token.

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space cells 1024	; cells is currently 1K
.space cellsEnd 0

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
	lda #00
	sta lidx
	sta lidx+1
	jsr initCells
	lda #<helloWorld
	sta iptr
	lda #>helloWorld
	sta iptr+1
	brk
;
; Zero out the cells as per the define start condition.
;
initCells:
.scope
	lda #<cells
	sta dptr
	lda #>cells
	sta dptr+1

_loop:
	lda #00
	sta (dptr)

	inc dptr
	bne _over
	inc dptr+1
_over:	lda dptr+1
	cmp #>cellsEnd
	bne _loop

	; set the dptr back to the start of the cells.
	lda #<cells
	sta dptr
	lda #>cells
	sta dptr+1
	rts
.scend

;
; Interprets a list of commands referenced by iptr.
runProgram:
.scope
_loop:
	lda (dptr)
	beq _exit

	;   <	Decrement the data pointer (dptr) to the prior cell.
decDptr:
	cmp #AscLT
	bne incDptr
.scope
        lda dptr
        bne _over
        dec dptr+1
_over:  dec dptr
	jmp next
.scend

	;   >	Increment the dptr to the next cell.
incDptr:
	cmp #AscGT
	bne incCell
.scope
	inc dptr
        bne _over
        inc dptr+1
_over:	jmp next
.scend

	;   +	Increment the byte at the dptr.
incCell:
*	cmp #AscPlus
	bne decCell

	lda (dptr)
	inc
	sta (dptr)
	jmp next

	;   -	Decrement the byte at the dptr.
decCell:
	cmp #AscMinus
	bne outputCell

	lda (dptr)
	dec
	sta (dptr)
	jmp next

	;   .	Output the byte at the dptr.
outputCell:
	cmp #AscDot
	bne inputCell

	lda (dptr)
	jsr _putch
	jmp next

	;   ,	Input a byte and store it in the byte at the dptr.
inputCell:
	cmp #AscRB
	bne leftBracket

	jsr _getch
	sta (dptr)
	jmp next

	;   [	If byte at dptr is zero, then jump forward to the matching ].
leftBracket:
.scope
	cmp #AscLB
	bne rightBracket

	inc lidx
        bne _over
        inc lidx+1

_over:	lda (dptr)
	bne +
	jsr findMatching
*
	jmp next
.scend

	;   ]	If byte at dptr is nonzero, then loop, otherwise exit the loop.
rightBracket:
	cmp #AscRB
	bne ignoreInput

	lda (dptr)
	bne +

	jmp next

	;  All other characters are ignored.
ignoreInput:

next:	inc iptr
	bne _over
	inc iptr+1
_over:	bra _loop
_exit:	rts
.scend

findMatching:
	rts

helloWorld:
	.byte ">++++++++[<+++++++++>-]<."
	.byte ">++++[<+++++++>-]<+."
	.byte "+++++++.."
	.byte "+++."
	.byte ">>++++++[<+++++++>-]<++."
	.byte "------------."
	.byte ">++++++[<+++++++++>-]<+."
	.byte "<."
	.byte "+++."
	.byte "------."
	.byte "--------."
	.byte ">>>++++[<++++++++>-]<+."
	.byte 0

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
