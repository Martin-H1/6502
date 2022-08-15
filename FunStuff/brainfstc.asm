; -----------------------------------------------------------------------------
; Implementation of the Brain F--k virtual machine in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it. Since Brain f--k is Turing complete you can (in
; theory) compute any problem with just the instructions required to write it.
;
; This version of the Brain f--k interpreter compiles programs into subroutine
; threaded code and arguments. When executed (via a JSR) the threaded code uses
; the underlying hardware as the code threading mechanism. Eliminating the NEXT
; function creates a faster Brain f--k runtime than the prior versions.
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
.alias codeSize [codeEnd - code]

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space dptr 2		; word to hold the data pointer.
.space iptr 2		; word to hold the instruction pointer.
.space temp 2		; word to hold popped PC for code generation.
.space retpc 2		; word to hold return address for code generation.
.space fixup 2		; word to hold popped PC to fixup forward branch.

.data BSS
.org $0300		; page 3 is used for uninitialized data.
.space cells 1024	; cells is currently 1K
.space cellsEnd 0

.space code 3072	; code buffer is currently 3K
.space codeEnd 0

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

.macro addwbi			; add word and byte immediate
	clc
	lda _1
	adc #_2
	sta _1
	bcc _over
	inc _1+1
_over:
.macend

.macro push		; push on stack matching RTS byte order.
	lda _1+1
	pha
	lda _1
	pha
.macend

.macro pushInd		; push indirect on stack matching RTS byte order.
	lda (_1)
	pha
	`decw _1
	lda (_1)
	pha
.macend

.macro pop		; pop from stack matching RTS byte order.
	pla
	sta _1
	pla
	sta _1+1
.macend

.macro emitCode
	lda #$20	; opcode value for JSR
	sta (dptr)
	`incw dptr
	lda #<_1	; address of subroutine
	sta (dptr)
	`incw dptr
	lda #>_1
	sta (dptr)
	`incw dptr
.macend

.macro emitArgument
	lda _1
	sta (dptr)
	`incw dptr
	lda _1+1
	sta (dptr)
	`incw dptr
.macend

.org $8000
.outfile "brainfstc.rom"
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
	jsr compile		; translate source into executable code
	jmp code		; directly execute the code

; compile scans the characters and produces a code token stream.
compile:
.scope
	lda #<code		; use dptr as the index into the code
	sta dptr
	lda #>code
	sta dptr+1

	; All programs start with memory cell initialization.
	`emitCode initCells

_while:	lda (iptr)
	bne _incCell

	`emitCode endProgram
	rts

_incCell:
	cmp #AscPlus
	bne _decCell

	`emitCode incCell
	jmp _next

_decCell:
	cmp #AscMinus
	bne _decDptr

	`emitCode decCell
	jmp _next

_decDptr:
	cmp #AscLT
	bne _incDptr

	`emitCode decDptr
	jmp _next

_incDptr:
	cmp #AscGT
	bne _outputCell

	`emitCode incDptr
	jmp _next

_outputCell:
	cmp #AscDot
	bne _inputCell

	`emitCode outputCell
	jmp _next

_inputCell:
	cmp #AscComma
	bne _leftBracket

	`emitCode inputCell
	jmp _next

_leftBracket:
	cmp #AscLB
	bne _rightBracket

	`push dptr		; push current PC for later.

	`emitCode branchForward
	`emitArgument temp	; junk for now, fixup later.
	jmp _next

_rightBracket:
	cmp #AscRB
	bne _debugOut

	pla			; get the return PC off the stack
	sta fixup
	sta temp
	pla
	sta fixup+1
	sta temp+1

	lda dptr		; current PC - 1 is the address for the fixup
	sta retpc
	lda dptr+1
	sta retpc+1
	`decw retpc		; RTS address is - 1 from desired target

	`addwbi fixup,3
	lda retpc
	sta (fixup)
	`incw fixup
	lda retpc+1
	sta (fixup)

	`emitCode branchBackward
	`decw temp		; RTS address is target-1!
	`emitArgument temp

	jmp _next

_debugOut:
	cmp #AscQues
	bne _ignoreInput
	`emitCode debugOut
	jmp _next

_ignoreInput:		;  All other characters are ignored.

_next:	`incw iptr
	jmp _while
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
	rts
.scend

incCell:
	lda (dptr)
	inc
	sta (dptr)
	rts

decCell:
	lda (dptr)
	dec
	sta (dptr)
	rts

decDptr:
	`decw dptr
	rts

incDptr:
	`incw dptr
	rts

outputCell:
	lda (dptr)
	jsr _putch
	rts

inputCell:
	jsr _getch
	sta (dptr)
	rts

branchForward:
	lda (dptr)
	beq +		; Branch on data cell containing zero

	`pop iptr	; Get return address off stack
	`addwbi iptr,2	; advance past the unused argument.
	`push iptr	; push back on stack
	rts

*	`pop iptr	; Use return address as pointer to argument
	`addwbi iptr,2	; point to argument MSB
	`pushInd iptr	; Use the argument as the new return address.
	rts

branchBackward:
	lda (dptr)
	beq +		; Branch on data cell containing zero

	`pop iptr	; Use return address as pointer to argument
	`addwbi iptr,2
	`pushInd iptr	; Use the argument as the new return address.
	rts

*	`pop iptr	; Get return address off stack
	`addwbi iptr,2	; advance past the unused argument.
	`push iptr	; push back on stack
	rts

debugOut:
	brk		; unimplemented for now

endProgram:
	pla		; pull the threaded code addess off stack.
	pla
	rts		; return to calling program.

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

.word nmiv		; NMI vector 
.word resetv		; RESET vector
.word irqv		; IRQ vector
