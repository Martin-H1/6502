; -----------------------------------------------------------------------------
; Implementation of the Brain F--k virtual machine in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it. Since Brain f--k is Turing complete you can (in
; theory) compute any problem with just the instructions required to write it.
;
; This version is a pure interpreter and scans the program text repeatedly.
; It serves as the baseline in peformance compared to the faster direct and
; subroutine threaded code versions.
;
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;

; Character set (ASCII)
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
	; Set the instruction pointer to the classic hello world program.
	lda #<helloWorld
	sta iptr
	lda #>helloWorld
	sta iptr+1
	jsr runProgram

	; set the instruction pointer to the Sierpinski triangle program.
	lda #<sierpinski
	sta iptr
	lda #>sierpinski
	sta iptr+1
	jsr runProgram

	; set the instruction pointer to the Golden ratio program.
	lda #<golden
	sta iptr
	lda #>golden
	sta iptr+1
	jsr runProgram
	brk

; runProgram which interprets a list of commands referenced by iptr.
runProgram:
.scope
_initCells:	; Zero out the cells as per the defined start condition.
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

.scope
_while:			; while (*iptr != null)
	lda (iptr)
	bne incCell
	rts		; Terminate execution on null character.

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
	bne decDptr

	lda (dptr)
	dec
	sta (dptr)
	jmp next

	;   <	Decrement the data pointer (dptr) to the prior cell.
decDptr:
	cmp #AscLT
	bne incDptr

	`decw dptr
	jmp next

	;   >	Increment the dptr to the next cell.
incDptr:
	cmp #AscGT
	bne outputCell

	`incw dptr
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
	bne next

	; On zero advance the iptr to the matching bracket.
.scope
_findMatchForward:
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
.scend
	jmp next	; We've found a right bracket at matching level

	;   ]	If byte at dptr is nonzero, then loop, otherwise exit the loop.
rightBracket:
	cmp #AscRB
	bne debugOut

	lda (dptr)
	beq next

	; Reverses the iptr to the matching bracket.
.scope
_findMatchReverse:
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
.scend
	; We've found a left bracket at matching level
	jmp next

	;   ?	Print cells, iptr, and dptr
debugOut:
	cmp #AscQues
	bne ignoreInput
	brk		; unimplemented for now

ignoreInput:		;  All other characters are ignored.

next:	`incw iptr
	jmp _while
.scend

helloWorld:
	.byte "++++++++"
	.byte "[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]"
	.byte ">>.>---.+++++++..+++.>>.<-.<.+++."
	.byte "------.--------.>>+.>++."
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
	cmp #13		; convert CR to LF so as to be compliant on Windows
	bne +
	lda #10
*	rts
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
