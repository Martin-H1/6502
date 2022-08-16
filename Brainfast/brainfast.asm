; -----------------------------------------------------------------------------
; Implementation of the Brain F--k compiler in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it. Since Brain f--k is Turing complete you can (in
; theory) compute any problem with just the instructions required to write it.
;
; This version of the Brain f--k is based upon four prior increasingly faster
; versions. But it is an optimizing compiler that programs directly into 6502
; and inlines all BF operations. It optimized by converting reeated operations
; into their higher level operations (e.g. increment becomes addition). When
; executed the machine code execute quickly and avoids overhead from prior
; threaded code mechanism.
;
; Derived from three prior version by Martin Heermance <mheermance@gmail.com>
; and enhanced by Phil Dennis <pjdennis@gmail.com> into an optimizing compiler.
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

.alias AscLF	$0A	; line feed ASCII character

.alias cellsSize [cellsEnd - cells]
.alias codeSize [codeEnd - code]

;
; Data segments
;
.data ZPDATA		; we'll need to use ZP addressing
.space dptr 2		; word to hold the data pointer.
.space iptr 2		; word to hold the instruction pointer.
.space temp 2		; word to hold popped PC for code generation.
.space fixup 2		; word to hold popped PC to fixup forward branch.
.space cptr 2		; word to hold pointer for code to copy.
.space ccnt 1		; byte to hold count of code to copy.

.data BSS
.space source 4096	; Input source buffer is currently 4K
.space cells 1024	; cells is currently 1K
.space cellsEnd 0

.space code 24576	; code buffer is currently 24K
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

.macro addwbi		; add word and byte immediate
	clc
	lda _1
	adc #_2
	sta _1
	bcc _over
	inc _1+1
_over:
.macend

.macro emitCode
	lda #<_1
	sta cptr
	lda #>_1
	sta cptr + 1
	lda #_2 - _1
	sta ccnt	
_loop:
	lda (cptr)
	sta (dptr)
	`incw cptr
	`incw dptr
	dec ccnt
	bne _loop
.macend

_welcome:
	.byte "Welcome to Brainfast, an optimizing Brainf--- compiler.",AscLF
	.byte "To use: enter BF code at prompt and hit enter.",AscLF
	.byte "Entering an empty line triggers compile and run.",AscLF,0

_prompt:
	.byte "$ ",0

_compile:
	.byte "Compile and run.",AscLF,0

;
; Functions
;
main:
	`print _welcome

repl:
.scope
	; During source entry use dptr to index into source buffer.
	lda #<source
	sta dptr
	lda #>source
	sta dptr+1

	`print _prompt		; Print input prompt.

_loop:	jsr _getch
	jsr _putch		; Echo to console.
	cmp #AscLF
	beq _over  		; Perform return key processing.

_store:	sta (dptr)		; store the character in source code buffer
	`incw dptr		; advance pointer and get next character.
	jmp _loop

_over:
	`print _prompt		; Print input prompt again.

	jsr _getch		; Get a char and check for second return key
	jsr _putch		; Echo to console.
	cmp #AscLF
	bne _store		; not a return key so continue source entry.

	lda #$00		; store a null to terminate source
	sta (dptr)		; store the character in source code buffer
.scend
	`print source
	; fall through into compile and run cycle.
	`print _compile

	; Set the instruction pointer to the source program.
	lda #<source
	sta iptr
	lda #>source
	sta iptr+1
	jsr runProgram

	; Upon termination loop back to repl
	jmp repl

runProgram:
	jsr compile	; translate source into executable code
	jmp code	; directly execute the code

; compile scans the characters and produces a machine code stream.
compile:
.scope
	lda #<code	; use dptr as the index into the code
	sta dptr
	lda #>code
	sta dptr+1

	; All programs start with memory cell initialization.
	`emitCode initCells,initCellsEnd

_while:	lda (iptr)
	bne _incCell

	`emitCode endProgram,endProgramEnd
	rts

_incCell:
	cmp #AscPlus
	bne _decCell

	`emitCode incCell,incCellEnd
	jmp _next

_decCell:
	cmp #AscMinus
	bne _decDptr

	`emitCode decCell,decCellEnd	
	jmp _next

_decDptr:
	cmp #AscLT
	bne _incDptr

	`emitCode decDptr,decDptrEnd
	jmp _next

_incDptr:
	cmp #AscGT
	bne _outputCell

	`emitCode incDptr,incDptrEnd
	jmp _next

_outputCell:
	cmp #AscDot
	bne _inputCell

	`emitCode outputCell,outputCellEnd
	jmp _next

_inputCell:
	cmp #AscComma
	bne _leftBracket

	`emitCode incCell,inputCellEnd
	jmp _next

_leftBracket:
	cmp #AscLB
	bne _rightBracket

	lda dptr+1	; push current PC for later.
	pha
	lda dptr
	pha

	`emitCode branchForward,branchForwardEnd	
	jmp _next

_rightBracket:
	cmp #AscRB
	bne _debugOut

	pla		; get the return PC off the stack
	sta fixup
	sta temp
	pla
	sta fixup+1
	sta temp+1
	
	`addwbi fixup, branchForwardJumpInstruction - branchForward + 1
	
	lda dptr	; fixup jump address for left bracket
	sta (fixup)
	`incw fixup
	lda dptr+1
	sta (fixup)

	`emitCode branchBackward,branchBackwardJumpInstruction + 1
	
	lda temp	; store backwards jump address
	sta (dptr)
	`incw dptr
	lda temp+1
	sta (dptr)
	`incw dptr

	jmp _next

_debugOut:
	cmp #AscQues
	bne _ignoreInput

	`emitCode debugOut,debugOutEnd
	jmp _next

_ignoreInput:		;  All other characters are ignored.

_next:	`incw iptr
	jmp _while
.scend

;
; These secions of code function as the threaded code to execute programs.
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
.scend
initCellsEnd:

incCell:
	lda (dptr)
	inc
	sta (dptr)
incCellEnd:

decCell:
	lda (dptr)
	dec
	sta (dptr)
decCellEnd:

decDptr:
	`decw dptr
decDptrEnd:

incDptr:
	`incw dptr
incDptrEnd:

outputCell:
	lda (dptr)
	jsr _putch
outputCellEnd:

inputCell:
	jsr _getch
	sta (dptr)
inputCellEnd:

branchForward:
	lda (dptr)
	bne +		; Branch on data cell containing zero
branchForwardJumpInstruction:
	jmp 0		; placeholder
*
branchForwardEnd:

branchBackward:
	lda (dptr)
	beq +		; Branch on data cell containing zero
branchBackwardJumpInstruction:
	jmp 0		; placeholder
*

debugOut:
	brk		; unimplemented for now
debugOutEnd:

endProgram:
	rts		; return to calling program.
endProgramEnd:

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
