; -----------------------------------------------------------------------------
; Implementation of the Brain F--k virtual machine in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it. Since Brain f--k is Turing complete you can (in
; theory) compute any problem with just the instructions required to write it.
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

.alias AscLT	$3C	; Character aliases for brain f commands.
.alias AscGT	$3E
.alias AscQues	$3F
.alias AscPlus	$2B
.alias AscComma $2C
.alias AscMinus	$2D
.alias AscDot	$2E
.alias AscLB	$5B
.alias AscRB	$5D

.alias cellsSize [cellsEnd - cells]

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space dptr 2		; word to hold the data pointer.
.space iptr 2		; word to hold the instruction pointer.
.space level 1		; byte to hold current loop level.

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space cells 1024	; cells is currently 1K
.space cellsEnd 0

.text

;
; Macros
;

.macro decw
        lda _1
        bne _over
        dec _1+1
_over:  dec _1
.macend

.macro incw
        inc _1
        bne _over
        inc _1+1
_over:
.macend

.org $8000
.outfile "brainf.rom"
.advance $8000

;
; Functions
;
main:
	; Init the instruction pointer to the test program.
	lda #<sierpinski
	sta iptr
	lda #>sierpinski
	sta iptr+1

	; continue into initCells to zero out RAM and set the dptr.
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
	lda #$00
	sta (dptr)
	`incw dptr
	lda dptr+1
	cmp #>cellsEnd
	bne _loop

	; set the dptr back to the start of the cells.
	lda #<cells
	sta dptr
	lda #>cells
	sta dptr+1
.scend

	; fall into runProgram which interts the brain f commands.
;
; Interprets a list of commands referenced by iptr.
runProgram:
.scope
_while:			; while (*iptr != null)
	lda (iptr)
	bne decDptr
	brk		; Terminate execution on null character.

	;   <	Decrement the data pointer (dptr) to the prior cell.
decDptr:
	cmp #AscLT
	bne incDptr

	`decw dptr
	jmp next

	;   >	Increment the dptr to the next cell.
incDptr:
	cmp #AscGT
	bne incCell

	`incw dptr
	jmp next

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
	cmp #AscComma
	bne leftBracket

	jsr _getch
	sta (dptr)
	jmp next

	;   [	If byte at dptr is zero, then jump forward to the matching ].
leftBracket:
	cmp #AscLB
	bne rightBracket

	lda (dptr)
	bne +
	jsr findMatchForward
*	jmp next

	;   ]	If byte at dptr is nonzero, then loop, otherwise exit the loop.
rightBracket:
	cmp #AscRB
	bne debugOut

	lda (dptr)
	beq +
	jsr findMatchReverse
*	jmp next

	;   ?	Print cells, iptr, and dptr
debugOut:
	cmp #AscQues
	bne ignoreInput

	brk		; unimplemented for now

ignoreInput:		;  All other characters are ignored.

next:	`incw iptr
	bra _while
.scend

; Advances the iptr to the matching bracket.
findMatchForward:
.scope
	lda #01		; Start at nesting level 1.
	sta level
_loop:
	`incw iptr	; Advance to the next character.
	lda (iptr)	; load the instruction looking for match.
	cmp #AscLB	; Is this is another left bracket?
	bne +
	inc level	; Increase nesting level
	bra _loop

*	cmp #AscRB	; Is this a right bracket?
	bne _loop

	dec level	; Decrease nesting level
	bne _loop
	rts 		; We've found a right bracket at matching level
.scend

; Reverses the iptr to the matching bracket.
findMatchReverse:
.scope
	lda #01		; Start at nesting level 1.
	sta level
_loop:
	`decw iptr	; Backup one character
	lda (iptr)	; load the instruction looking for match 
	cmp #AscRB	; Is this is another right bracket?
	bne +
	inc level	; Increase nesting level
	bra _loop

*	cmp #AscLB	; Is this a left bracket?
	bne _loop

	dec level	; Decrease nesting level
	bne _loop
	rts		; We've found a left bracket at matching level
.scend

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

; Fibonacci number generator by Daniel B Cristofani
; This program doesn't terminate; you will have to kill it.
fibonacci:
	.byte ">++++++++++>+>+["
	.byte "[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>["
        .byte "[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
        .byte "[>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>"
	.byte "]<<<"
	.byte "]",0

; Shows an ASCII representation of the Sierpinski triangle
; (c) 2016 Daniel B. Cristofani
sierpinski:
	.byte "++++++++[>+>++++<<-]>++>>+<[-[>>+<<-]+>>]>+["
	.byte "-<<<["
	.byte "->[+[-]+>++>>>-<<]<[<]>>++++++[<<+++++>>-]+<<++.[-]<<"
	.byte "]>.>+[>>]>+"
	.byte "]", 0

; Compute the "golden ratio". Because this number is infinitely long,
; this program doesn't terminate on its own. You will have to kill it.
golden:
	.byte "+>>>>>>>++>+>+>+>++<["
	.byte "  +["
	.byte "    --[++>>--]->--["
	.byte "      +["
	.byte "        +<+[-<<+]++<<[-[->-[>>-]++<[<<]++<<-]+<<]>>>>-<<<<"
	.byte "          <++<-<<++++++[<++++++++>-]<.---<[->.[-]+++++>]>[[-]>>]"
	.byte "          ]+>>--"
	.byte "      ]+<+[-<+<+]++>>"
	.byte "    ]<<<<[[<<]>>[-[+++<<-]+>>-]++[<<]<<<<<+>]"
	.byte "  >[->>[[>>>[>>]+[-[->>+>>>>-[-[+++<<[-]]+>>-]++[<<]]+<<]<-]<]]>>>>>>>"
	.byte "]"

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
