; -----------------------------------------------------------------------------
; Implementation of the Brain F--k virtual machine in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it. Since Brain f--k is Turing complete you can (in
; theory) compute any problem with just the instructions required to write it.
;
; This version of the Brain f--k interpreter compiles programs into bytecode
; and operands. When executed the bytecode invoke calls to threaded code.
; The goal is to create a fast Brain f--k runtime than the original intepreted
; version.
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
.alias bytecodeSize [bytecodeEnd - bytecode]

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space dptr 2		; word to hold the data pointer.
.space iptr 2		; word to hold the instruction pointer.
.space temp 2		; word to hold popped PC for bytecode generation.
.space fixup 2		; word to hold popped P to fixup forward branch.
.space jmpvec 2		; vector to jump into threaded code.

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space cells 1024	; cells is currently 1K
.space cellsEnd 0

.space bytecode 2048	; bytecode buffer is currently 2K
.space bytecodeEnd 0

.text

;
; Macros
;

.macro incw
        inc _1
        bne _over
        inc _1+1
_over:
.macend

.macro addTwo
	clc
	lda _1
	adc #2
	sta _1
	bcc _over
	inc _1+1
_over:
.macend

.macro emitBytecode
	lda #<_1
	sta (dptr)
	`incw dptr
	lda #>_1
	sta (dptr)
	`incw dptr
.macend

.macro emitOperand
	lda _1
	sta (dptr)
	`incw dptr
	lda _1+1
	sta (dptr)
	`incw dptr
.macend

.org $8000
.outfile "brainfdtc.rom"
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

runProgram:
	jsr compile		; translate source into bytecode
	lda #<bytecode		; set the instruction pointer to it.
	sta iptr
	lda #>bytecode
	sta iptr+1
	jsr execute		; execute the bytecode
	rts

; compile scans the characters and produces a bytecode token stream.
compile:
.scope
	lda #<bytecode		; use dptr as the index into the bytecode
	sta dptr
	lda #>bytecode
	sta dptr+1

	; All programs start with memory cell initialization.
	`emitBytecode initCells

_while:	lda (iptr)
	bne _incCell

	`emitBytecode endProgram
	rts

_incCell:
	cmp #AscPlus
	bne _decCell

	`emitBytecode incCell
	jmp _next

_decCell:
	cmp #AscMinus
	bne _decDptr

	`emitBytecode decCell
	jmp _next

_decDptr:
	cmp #AscLT
	bne _incDptr

	`emitBytecode decDptr
	jmp _next

_incDptr:
	cmp #AscGT
	bne _outputCell

	`emitBytecode incDptr
	jmp _next

_outputCell:
	cmp #AscDot
	bne _inputCell

	`emitBytecode outputCell
	jmp _next

_inputCell:
	cmp #AscComma
	bne _leftBracket

	`emitBytecode inputCell
	jmp _next

_leftBracket:
	cmp #AscLB
	bne _rightBracket

	lda dptr		; push current PC for later.
	pha
	lda dptr+1
	pha

	`emitBytecode branchForward
	`emitOperand temp	; junk for now, fixup later.
	jmp _next

_rightBracket:
	cmp #AscRB
	bne _debugOut

	pla			; get the return PC off the stack
	sta fixup+1
	sta temp+1
	pla
	sta fixup
	sta temp

	`addTwo fixup
	lda dptr		; to point to current PC
	sta (fixup)
	`incw fixup
	lda dptr+1
	sta (fixup)
	`emitBytecode branchBackward
	`emitOperand temp

	jmp _next

_debugOut:
	cmp #AscQues
	bne _ignoreInput
	`emitBytecode debugOut
	jmp _next

_ignoreInput:		;  All other characters are ignored.

_next:	`incw iptr
	jmp _while
.scend

; executes interprets a list of commands referenced by iptr.
execute:
.scope
next:
	lda (iptr)
	sta jmpvec
	`incw iptr
	lda (iptr)
	sta jmpvec+1
	`incw iptr
	jmp (jmpvec)
.scend

;
; These subroutines function as the threaded code to execute programs.
;

initCells:
	lda #<cells
	sta dptr
	lda #>cells
	sta dptr+1
.scope
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
	jmp next
.scend

incCell:
	lda (dptr)
	inc
	sta (dptr)
	jmp next

decCell:
	lda (dptr)
	dec
	sta (dptr)
	jmp next

decDptr:
	lda dptr
	bne +
	dec dptr+1
*	dec dptr
	jmp next

incDptr:
	`incw dptr
	jmp next

outputCell:
	lda (dptr)
	jsr _putch
	jmp next

inputCell:
	jsr _getch
	sta (dptr)
	jmp next

branchForward:
	lda (dptr)
	beq +		; If zero advance the iptr to the operand.

	`addTwo iptr	; otherwise, consume the unused operand
	jmp next

*	lda (iptr)	; If zero advance the iptr to the operand.
	pha
	`incw iptr
	lda (iptr)
	sta iptr+1
	`incw iptr
	pla
	sta iptr
	jmp next

branchBackward:
	lda (dptr)
	beq +		; If not zero set the iptr to the operand.

	lda (iptr)
	pha
	`incw iptr
	lda (iptr)
	sta iptr+1
	`incw iptr
	pla
	sta iptr
	jmp next

*	`addTwo iptr	; otherwise consume unused operand.
	jmp next

debugOut:
	brk		; unimplemented for now

endProgram:
	rts			; return to calling program.

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
