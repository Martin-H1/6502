; -----------------------------------------------------------------------------
; Implementation of the Brain F--k compiler in 6502 assembly.
;
; The goal of the challege is to create another Turing tarpit using the least
; number of instructions. But this time using the inherent simplicity of the
; Brain f--k VM to enforce it. Since Brain f--k is Turing complete you can (in
; theory) compute any problem with just the instructions required to write it.
;
; This version of the Brain f--k compiler compiles programs into 6502 machine
; code. When executed the machine code uses the underlying hardware as the code
; threading mechanism. Eliminating function calls creates a faster Brain f--k
; impementation than the prior versions.
;
; These optimizations further increase the speed:
; * [ and ] jump to the instruction following the matching brace, instead of
;   jumping to the matching brace which would needlessly retest the value of
;  the current cell
; * Multiple consecutive increments/decrements to cell values or to the data
;   pointer are consolidated into a single addition. The most efficient way of
;   updating the cell value or pointer is chosen based on the magnatude of the
;   consolidated value
; * The compiler keeps track of when the current cell value is reflected in the
;   Z flag so as to avoid reloading the current cell value unnecessarily
;
; Derived from prior version by Martin Heermance <mheermance@gmail.com>
;
; Phil Dennis <pjdennis@gmail.com>
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

.alias StateDefault	$00	; Nothing pending
.alias StateModCell	$01	; Collecting cell increments into delta
.alias StateModDptr	$02	; Collecting pointer increments into delta
.alias StateCellCmp	$03	; Current cell loaded for branch on Z flag

.alias cellsSize [cellsEnd - cells]
.alias codeSize [codeEnd - code]

;
; Data segments
;
.data ZPDATA
.org $0080		; we'll need to use ZP addressing
.space dptr 2		; word to hold the data pointer.
.space iptr 2		; word to hold the instruction pointer.
.space temp 2		; word to hold temporary pointer.
.space fixup 2		; word to hold popped PC to fixup forward branch.
.space cptr 2		; word to hold pointer for code to copy.
.space ccnt 1		; byte to hold count of code to copy.
.space state 1		; current parser state
.space count 2		; count cell or dptr delta

.data BSS
.org $0300		; page 3 is used for uninitialized data.
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
	sta cptr+1
	lda #_2 - _1
	sta ccnt
	jsr copyCode
.macend

.org $8000
.outfile "brainfotc.rom"
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
	jsr compile	; translate source into executable code
	jmp code	; directly execute the code

; compile scans the characters and produces a machine code stream.
compile:
.scope
	lda #<code	; use dptr as the index into the code
	sta dptr
	lda #>code
	sta dptr+1
	
	; Initialize parser state
	lda #StateDefault
	sta state
	lda #0
	sta count
	sta count+1

	; All programs start with memory cell initialization.
	`emitCode initCells,initCellsEnd

_while:	lda (iptr)
	bne _incCell

	jsr processState
	`emitCode endProgram,endProgramEnd
	rts

_incCell:
	cmp #AscPlus
	bne _decCell
	
	lda state
	cmp #StateModCell
	beq +
	jsr processState
	lda #StateModCell
	sta state
*	inc count
	jmp _next

_decCell:
	cmp #AscMinus
	bne _decDptr

	cmp #StateModCell
	beq +
	jsr processState
	lda #StateModCell
	sta state
*	dec count
	jmp _next

_decDptr:
	cmp #AscLT
	bne _incDptr

	lda state
	cmp #StateModDptr
	beq +
	jsr processState
	lda #StateModDptr
	sta state
*	`decw count
	jmp _next

_incDptr:
	cmp #AscGT
	bne _outputCell

	lda state
	cmp #StateModDptr
	beq +
	jsr processState
	lda #StateModDptr
	sta state
*	`incw count
	jmp _next

_outputCell:
	; no longer collecting increments so emit any pending code
	pha
	jsr processState
	pla

	cmp #AscDot
	bne _inputCell

	`emitCode outputCell,outputCellEnd
	lda #StateDefault
	sta state
	jmp _next

_inputCell:
	cmp #AscComma
	bne _leftBracket

	`emitCode incCell,inputCellEnd
	lda #StateDefault
	sta state
	jmp _next

_leftBracket:
	cmp #AscLB
	bne _rightBracket
	
	lda state
	cmp #StateCellCmp
	beq +
	`emitCode branchForward,branchForwardAfterLoad
*	`emitCode branchForwardAfterLoad,branchForwardJumpInstruction+1
	lda dptr+1	; push current PC for later.
	pha
	lda dptr
	pha
	
	`addwbi dptr, 2	; skip past reserved space for jump address

	lda #StateCellCmp
	sta state
	jmp _next

_rightBracket:
	cmp #AscRB
	bne _debugOut

	pla		; get the fixup address off the stack
	sta fixup
	pla
	sta fixup+1

	lda state
	cmp #StateCellCmp
	beq +
	`emitCode branchBackward,branchBackwardAfterLoad
*	`emitCode branchBackwardAfterLoad,branchBackwardJumpInstruction+1

	lda dptr	; address of next instruction into temp
	sta temp
	lda dptr+1
	sta temp+1
	`addwbi temp,2
	
	lda temp	; fixup jump address for left bracket
	sta (fixup)
	`incw fixup
	lda temp+1
	sta (fixup)
	`incw fixup

	lda fixup	; store backwards jump address
	sta (dptr)
	`incw dptr
	lda fixup+1
	sta (dptr)
	`incw dptr

	lda #StateCellCmp
	sta state
	jmp _next

_debugOut:
	cmp #AscQues
	bne _ignoreInput

	`emitCode debugOut,debugOutEnd
	lda #StateDefault
	sta state
	jmp _next

_ignoreInput:		; all other characters are ignored

_next:	`incw iptr
	jmp _while
.scend

processState:
.scope
	lda state
	cmp #StateDefault
	bne _stateCellCmp

	rts

_stateCellCmp:
	cmp #StateCellCmp
	bne _stateModCell

	rts

_stateModCell:
.scope
	cmp #StateModCell
	bne _stateModDptr
	
	lda count
	cmp #$01
	bne _decrement
	; increment current cell
	`emitCode incCell,incCellEnd
	jmp _done
	
_decrement:
	cmp #$ff
	bne _add
	; decrement current cell
	`emitCode decCell,decCellEnd
	jmp _done

_add:
	; add to current cell
	`emitCode modCell, modCellAdd+1
	lda count
	sta (dptr)
	`incw dptr
	`emitCode modCellAdd+2,modCellEnd
	
_done:
	lda #0
	sta count
	lda #StateCellCmp
	sta state
	rts
.scend

_stateModDptr:
.scope
	lda count+1
	bne _decrement

	; Choose most efficient way of modifying data pointer
	lda count
	cmp #$01
	bne _addPosByte
	; increment data pointer
	`emitCode incDptr,incDptrEnd
	jmp _done

_addPosByte:
	; add positive value < 256 to data pointer
	`emitCode addDptrPosByte,addDptrPosByteAdd+1
	lda count
	sta (dptr)
	`incw dptr
	`emitCode addDptrPosByteAdd+2,addDptrPosByteEnd
	jmp _done

_decrement:
	lda count+1
	cmp #$ff
	bne _add

	lda count
	cmp #$ff
	bne _addNegByte
	; decrement data pointer
	`emitCode decDptr,decDptrEnd
	jmp _done

_addNegByte:
	; subract negative value >= -256 from data pointer
	`emitCode addDptrNegByte,addDptrNegByteAdd+1
	lda count
	sta (dptr)
	`incw dptr
	`emitCode addDptrNegByteAdd+2,addDptrNegByteEnd
	jmp _done

_add:
	; add signed value to data pointer
	`emitCode modDptr,modDptrAddLow+1
	lda count
	sta (dptr)
	`incw dptr
	`emitCode modDptrAddLow+2,modDptrAddHigh+1
	lda count+1
	sta (dptr)
	`incw dptr
	`emitCode modDptrAddHigh+2,modDptrEnd

_done:
	lda #0
	sta count
	sta count+1
	lda #StateDefault
	sta state
	rts
.scend
.scend

copyCode:
_loop:
	lda (cptr)
	sta (dptr)
	`incw cptr
	`incw dptr
	dec ccnt
	bne _loop
	
	rts

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

modCell:
	clc
	lda (dptr)
modCellAdd:
	adc #0		; placeholder
	sta (dptr)
modCellEnd:

decDptr:
	`decw dptr
decDptrEnd:

incDptr:
	`incw dptr
incDptrEnd:

addDptrNegByte:
	clc
	lda dptr
addDptrNegByteAdd:
	adc #0		;placeholder
	sta dptr
	bcs +
	dec dptr+1
*
addDptrNegByteEnd:

addDptrPosByte:
	clc
	lda dptr
addDptrPosByteAdd:
	adc #0		; placeholder
	sta dptr
	bcc +
	inc dptr+1
*
addDptrPosByteEnd:

modDptr:
	clc
	lda dptr
modDptrAddLow:
	adc #0		; placeholder
	sta dptr
	lda dptr+1
modDptrAddHigh:
	adc #0		; placeholder
	sta dptr+1
modDptrEnd:

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
branchForwardAfterLoad:
	bne +		; Branch on data cell containing zero
branchForwardJumpInstruction:
	jmp 0		; placeholder
*

branchBackward:
	lda (dptr)
branchBackwardAfterLoad:
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

.word nmiv		; NMI vector 
.word resetv		; RESET vector
.word irqv		; IRQ vector
