; -----------------------------------------------------------------------------
; Main body of the interpreter. It will depend upon the stack, list, and I/O
; functions defined in the other modules.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;

;
; Data segments
;
.data BSS
.space _BUFF	$80		; a working buffer for strings.
.space _CELLPTR	2		; a temporary cell pointer.
.space GLOBALS	2		; pointer to the global scope list.
.space INTERNS	2		; pointer to the list of pooled strings.
.space _XSAVE	1		; holds X reg to allow exception handling.
.space _SSAVE	1		; holds S reg to allow exception handling.
.space _RESULT	2		; math intermediate results storage.
.space _TRUE	2		; pointer to the true value.
.space _FALSE	2		; pointer to the false value.
.text

;
; Macros
;
.macro _globalSubr
	`pushi _1
	jsr intern
	`pushi _2
	jsr cellMkSubr
	jsr cellMkCons
	`push GLOBALS
	jsr cellMkCons
	`pop GLOBALS
.macend

.macro _globalFSubr
	`pushi _1
	jsr intern
	`pushi _2
	jsr cellMkFSubr
	jsr cellMkCons
	`push GLOBALS
	jsr cellMkCons
	`pop GLOBALS
.macend

.macro _globalSymbol
	`pushi _1
	jsr intern
	`peek _2
	`dup
	jsr cellMkCons
	`push GLOBALS
	jsr cellMkCons
	`pop GLOBALS
.macend

.macro _throw
	`pushi _1
	jmp lispCatch
.macend

.macro _throwOnNull
	`tosZero?
	bne _over
	`_throw _1
_over:
.macend

;
; Functions
;

; Main entry point for the interpreter.
lispInit:
	`pushzero
	`pop INTERNS
	`_globalSubr _abs, _absSubr
	`_globalSubr _add, _addSubr
	`_globalSubr _sub, _subSubr
	`_globalSubr _mod, _modSubr
	`_globalSubr _mul, _mulSubr
	`_globalSubr _div, _divSubr
	`_globalSubr _car, _carSubr
	`_globalSubr _cdr, _cdrSubr
	`_globalSubr _exit, _exitSubr
	`_globalFSubr _quote, _quoteSubr
	`_globalSubr _eqls, _eqlsSubr
	`_globalSubr _grtr, _grtrSubr
	`_globalSubr _grtrEq, _grtrEqSubr
	`_globalSubr _less, _lessSubr
	`_globalSubr _lessEq, _lessEqSubr
	`_globalSubr _not, _notSubr

	`_globalSymbol _t, _TRUE
	`_globalSymbol _f, _FALSE
	rts

_t:	.byte "#t",0
_f:	.byte "#f",0

; All subroutines are called with the following arguments.
; input - args (TOS) and environment (NOS)
; output - results of computation.

_car:	.byte "car",0
_carSubr:
.scope
	`nip
	jsr cellCar
	rts
.scend

_cdr:	.byte "cdr",0
_cdrSubr:
.scope
	`nip
	jsr cellCdr
	rts
.scend

_exit:	.byte "exit",0
_exitSubr:
	brk

; Computes the absolute value of a number at TOS using environment at NOS.
; input - 16 bit signed quantity at TOS and environment at NOS.
; output - 16 abosolute value
_abs:	.byte "abs",0
_absSubr:
.scope
	`tosZero?
	beq _exit
	jsr cellCar
	`fetch			; get number value
	jsr abs16
_exit:
	jsr cellMkNumber
	`nip
	rts
.scend

_add:	.byte "+",0
_addSubr:
.scope
	`_throwOnNull _tooFewArgsErr
	`dup
	jsr cellCar
	`fetch
	`pop _RESULT
	jsr cellCdr
_forEach:
	`tosZero?
	beq _endFor
	`dup
	jsr cellCar
	`fetch			; get number value
	`push _RESULT		; add to sum
	jsr add16
	`pop _RESULT
	jsr cellCdr
	bra _forEach
_endFor:
	`drop
	`drop
	`push _RESULT
	jsr cellMkNumber
	rts
.scend

_sub:	.byte "-",0
_subSubr:
.scope
	`_throwOnNull _tooFewArgsErr
	`dup
	jsr cellCar
	`fetch
	`pop _RESULT
	jsr cellCdr
_forEach:
	`tosZero?
	beq _endFor
	`dup
	jsr cellCar
	`fetch			; get number value
	`push _RESULT		; add to sum
	`swap
	jsr sub16
	`pop _RESULT
	jsr cellCdr
	bra _forEach
_endFor:
	`drop
	`drop
	`push _RESULT
	jsr cellMkNumber
	rts
.scend

_mod:	.byte "mod",0
_modSubr:
.scope
	`tosZero?
	beq _exit
	`dup
	jsr cellCar
	`fetch			; get number value
	`pop _RESULT
	jsr cellCdr
	jsr cellCar
	`fetch			; get number value
	`push _RESULT
	jsr mod16
_exit:	jsr cellMkNumber
	`nip
	rts
.scend

_mul:	.byte "*",0
_mulSubr:
.scope
	`_throwOnNull _tooFewArgsErr
	`dup
	jsr cellCar
	`fetch
	`pop _RESULT
	jsr cellCdr
_forEach:
	`tosZero?
	beq _endFor
	`dup
	jsr cellCar
	`fetch			; get number value
	`push _RESULT		; add to sum
	jsr mul16
	`pop _RESULT
	jsr cellCdr
	bra _forEach
_endFor:
	`drop
	`drop
	`push _RESULT
	jsr cellMkNumber
	rts
.scend

_div:	.byte "/",0
_divSubr:
.scope
	`_throwOnNull _tooFewArgsErr
	`dup
	jsr cellCar
	`fetch
	`pop _RESULT
	jsr cellCdr
_forEach:
	`tosZero?
	beq _endFor
	`dup
	jsr cellCar
	`fetch			; get number value
	`push _RESULT		; push the dividend
	`swap
	jsr div16
	`pop _RESULT
	jsr cellCdr
	bra _forEach
_endFor:
	`drop
	`drop
	`push _RESULT
	jsr cellMkNumber
	rts
.scend

_processArgs:
	`dup
	jsr cellCar
	`fetch
	`swap
	jsr cellCdr
	jsr cellCar
	`fetch
	rts

_eqls:	.byte "=",0
_eqlsSubr:
.scope
	jsr _processArgs
	`if_equals16
	`push _TRUE
	rts
_else:	`push _FALSE
	rts
.scend

_grtr:	.byte ">",0
_grtrSubr:
.scope
	jsr _processArgs
	`if_greater16
	`push _TRUE
	rts
_else:	`push _FALSE
	rts
.scend

_grtrEq: .byte ">=",0
_grtrEqSubr:
.scope
	jsr _processArgs
	`if_greater16
	`push _TRUE
	rts
_else:	bne +
	`push _TRUE
	rts
*	`push _FALSE
	rts
.scend

_less:	.byte "<",0
_lessSubr:
.scope
	jsr _processArgs
	`if_less16
	`push _TRUE
	rts
_else:	`push _FALSE
	rts
.scend

_lessEq: .byte "<=",0
_lessEqSubr:
.scope
	jsr _processArgs
	`if_less16
	`push _TRUE
	rts
_else:	bne +
	`push _TRUE
	rts
*	`push _FALSE
	rts
.scend

_not: .byte "not",0
_notSubr:
.scope
	jsr cellCar
	`push _FALSE
	`if_equals16
	`push _TRUE
	rts
_else:	`push _FALSE
	rts
.scend

_quote:	.byte "quote",0
_quoteSubr:
.scope
	jsr cellCar
	rts
.scend

; Locates a string cell to value cell association.
; input - key cell pointer (TOS) and environment (NOS)
; output - a pointer to the association cell or NULL.
assq:
.scope
	`pop _CELLPTR
_for:
	`toszero?
	beq _endfor
	`dup			; duplicate the list pointer.
	jsr cellCar		; The car car points to the  cell.
	jsr cellCar
	`push _CELLPTR
	jsr equals16		; Compare with the string argument.
	bne _next		; No match, so advance to next cell.

	jsr cellCar		; return the association cell.
	rts
_next:
	; Advance to next cell in the environment list.
	jsr cellCdr
	bra _for
_endfor:
	; association is not in list, so return null at TOS.
	rts
.scend

; It is more efficent to retain a single copy of a string and use the
; reference from that point forward. This allows an integer, rather
; than a string compare during execution. So this function locates a
; string in the pool, or creates it if missing.
; input - a null terminated string pointer on the stack.
; output - a pointer to the pooled string cell.
intern:
.scope
	`pop _CELLPTR
	`push INTERNS
_for:
	`toszero?
	beq _endfor
	`dup			; duplicate the list pointer.
	jsr cellCar		; The car points to the symbol cell.
	`push _CELLPTR		; Compare with the string argument.
	jsr strcmp
	bne _next		; No match, so advance to next cell.
	jsr cellCar
	rts
_next:
	; Advance to next cell in INTERNS list.
	jsr cellCdr
	bra _for
_endfor:
	; Symbol is not in INTERNS list, so create it.
	`drop			; pop the null off the stack
	`push _CELLPTR		; and get a pointer to the string
	jsr cellMkSymbol
	`dup			; Now insert it into INTERNS
	jsr gcProtect
	`dup
	`push INTERNS
	jsr cellMkCons		; stack has cons cell and Symbol
	`pop INTERNS		; Make the new cell the head of INTERNS
	`dup
	jsr gcUnprotect
_return:
	rts			; now return a pointer to the cell.
.scend

; Prints the invalid character.
; input - the character in the accumulator.
; output - a null pointer
readIllegal:
.scope
	`print _ignoringIllegal
	jsr putch
	`printcr
	`pushzero
	rts

_ignoringIllegal: .byte "ignoring illegal character ",0
.scend

; Eats leading white space characters.
; input - the character in the accumulator.
; output - first non-blank in accumulator.
readBlanks:
.scope
_while:	jsr getch
	jsr isspace
	bne +
	bra _while
*	rts
.scend

; Reads a sequence of digits.
; input - a character in the accumulator.
; output - a pointer to a cell.
readNumber:
.scope
	phy
	ldy #0
_while:
	sta _BUFF,y
	iny
	jsr getch
	jsr isdigit
	bne _endwhile
	bra _while
_endwhile:
	jsr ungetch
	lda #00
	sta _BUFF,y
	ply
	`pushi _BUFF
	jsr strtoi
	jsr cellMkNumber
	rts
_endif:

.scend
; Reads a scheme operator
; input - a character in the accumulator.
; output - a pointer to a cell.
readOperator:
.scope
	phy
	ldy #0
_while:
	sta _BUFF,y
	iny
	jsr getch
	cmp #'\)
	beq _endwhile
	jsr isspace
	bne _while
_endwhile:
	jsr ungetch
	lda #00
	sta _BUFF,y
	`pushi _BUFF
	jsr intern
	ply
	rts
.scend

; Reads a sequence of characters.
; input - a character in the accumulator.
; output - a pointer to a cell.
readAlpha:
.scope
	phy
	ldy #0
_while:
	sta _BUFF,y
	iny
	jsr getch
	jsr isalpha
	bne _endwhile
	bra _while
_endwhile:
	jsr ungetch
	lda #00
	sta _BUFF,y
	`pushi _BUFF
	jsr intern
	ply
	rts
.scend

; Reads a quoted literal.
; input - a character in the accumulator.
; output - a pointer to a cell.
readQuote:
.scope
	`pushi _quote
	jsr intern
	jsr read
	`pushzero
	jsr cellMkCons
	jsr cellMkCons
	rts
.scend

; Reads a string to end quote
; input - a character in the accumulator.
; output - a pointer to a cell.
readString:
.scope
	phy
	ldy #0
_while:	jsr getch
	cmp #'\"
	beq _endwhile
	sta _BUFF,y
	iny
	bra _while
_endwhile:
	lda #0
	sta _BUFF,y
	ply
	`pushi _BUFF
	jsr cellMkString
	rts
.scend

; Reads a list recursively
; input - a character in the accumulator.
; output - a pointer to a cell.
readList:
.scope
	`pushA			; push list start character onto the stack.
	`inctos			; add one to get terminator
	`pushzero		; push two nulls
	`pushzero
	jsr cellMkCons		; create a placeholder cell.
	`dup
	jsr gcProtect
	`tuck			; save the list head for later.
_while:	jsr readBlanks		; eat leading whitespace
	cmp NOS_LSB,x		; compare character to terminator.
	beq _endwhile
	cmp #'.			; Dotted pair?
	bne +
	jsr read
	`over
	jsr cellRplacd
	bra _endwhile
*	jsr ungetch		; let generic read process this char
	jsr read
	`pushzero
	jsr cellMkCons
	`over
	jsr cellRplacd
	jsr cellCdr		; make the new cell the tail on TOS.
	bra _while
_endwhile:
	`drop			; drop the tail off the stack
	`drop			; drop the list terminator character.
	`dup			; duplicate head and unprotect it.
	jsr gcUnprotect
	jsr cellCdr		; cdr of head is the start of the list.
	rts
.scend

; Reads a comment to end of line
readComment:
.scope
_while:	jsr getch
	beq _endwhile
	cmp #AscLF
	beq _endwhile
	cmp #AscCR
	beq _endwhile
	bra _while
_endwhile:
	rts
.scend

; Reads a sequence of characters from the terminal
; input - implicit via terminal and globals.
; output - a pointer to a cell.
read:
.scope
_while:	jsr readBlanks
	beq _endWhile
	cmp #'\(
	bne +
	jsr readList
	bra _endWhile
*	cmp #'\;
	bne +
	jsr readComment
	bra _endWhile
*	cmp #'\"
	bne +
	jsr readString
	bra _endWhile
*	cmp #'\'
	bne +
	jsr readQuote
	bra _endWhile
*	cmp #'\#
	bne +
	jsr readAlpha
	bra _endWhile
*	jsr isalpha
	bne +
	jsr readAlpha
	bra _endWhile
*	pha
	`pushA
	jsr getch
	sta TOS_MSB,x
	jsr ungetch
	jsr isOperator
	bne +
	pla
	jsr readOperator
	bra _endWhile
*	pla
	jsr isSign
	bne +
	jsr readNumber
	bra _endWhile
*	jsr isdigit
	bne +
	jsr readNumber
	bra _endWhile
*	beq _while
_endWhile:
	rts
.scend

; applies the function subr to the arguments using the environment bindings.
; input - subr pointer (TOS), args (NOS), and environment (THird on S).
; output - arguments consumed, results of evaluation at TOS.
apply:
.scope
	`cellGetType
	cmp #C_FSUBR
	bne +
	`fetch
	`execute
	rts
*	cmp #C_SUBR
	bne +
	`popToR
	jsr evalargs
	`pushFromR
	`fetch
	`execute
	rts
*	`_throw _cannotApplyErr
.scend

; evaluates all the arguments in list using the environment bindings at NOS.
; input - a cell pointer (TOS) and environment (NOS).
; output - evaluated arguments (TOS) and environment (NOS).
evalargs:
.scope
	`tosZero?		; null input check
	bne +
	rts

*	`pushzero		; Make a new first list element.
	`pushzero
	jsr cellMkCons
	`dup
	jsr gcProtect		; protect it from GC.
	`peekToR		; save a working copy on return stack.
	`mrot			; save the working copy under the args.
_while:	`over
	`over
	jsr cellCar
	jsr eval		; evaluate the element.
	`pushFromR
	`over
	`over
	jsr cellRplaca		; replace car with the evaluated element.
	`popToR
	`drop
	jsr cellCdr		; Get the next element
	`tosZero?
	beq _endWhile

	`cellGetType
	cmp #C_CONS
	bne _dottedPair
_next:
	`pushzero		; Make a new next list element.
	`pushzero
	jsr cellMkCons
	`dup
	`pushFromR
	jsr cellRplacd
	`popToR
	bra _while		; process next list element.

; stack contains: cdr (TOS), environment, return ptr
_dottedPair:
	`over
	`over
	jsr eval
	`pushFromR
	`over
	`over
	jsr cellRplacd		; process cdr of dotted pairs.
	`popToR
	`drop
				; fall through to return new list.

; Data stack contains cell pointer (TOS), env (NOS), and return ptr (THS).
; Return stack contains working pointer (TOS) and return addr (NOS).
_endwhile:
	pla			; discard working pointer.
	pla
	`drop			; discard final cell pointer.
	`swap			; return new cell and environment.
	`dup
	jsr gcUnprotect
	rts
.scend

; evaluates the TOS using the environment bindings at NOS.
; input - a cell pointer (TOS) and environment (NOS).
; output - arguments consumed, results of evaluation at TOS.
eval:
.scope
	`toszero?
	beq _exit
	; use the tag to determine how to eval cell.
	`cellGetType

	cmp #C_NUMBER		; numbers are self-evaluating.
	bne +
	`nip
	rts

*	cmp #C_SYMBOL		; if symbol
	bne +
	jsr assq		; lookup in association list
	`_throwOnNull _unboundSymnbolErr
	jsr cellCdr		; and return the association.
	rts
*	cmp #C_CONS		; if list
	bne +
	`over			; get the association for the car
	`over
	jsr cellCar
	jsr eval		; stack contains binding, list, env
	`swap
	jsr cellCdr
	`swap
	jsr apply		; apply function to arguments
*
_exit:	`nip			; drop environment parameter
	rts
.scend

; classic read, eval, print loop.
; input - implicit input stream,
; output - implicit output stream.
repl:
.scope
	`pushTrue
	jsr conioSetEcho
	`println _welcome
	stx _XSAVE		; save X and S to allow exception catching.
	tsx
	stx _SSAVE
	ldx _XSAVE
_while:
	`push GLOBALS
	`print _prompt
	jsr read
	`printcr
	jsr eval
	jsr cellPrint
	`printcr
	bra _while
_endWhile:
	rts
_welcome:
	.byte "Lisp65 Scheme Interpreter 0.5.0.",AscCR,AscLF
	.byte "Type (exit) to halt Scheme.",0
_prompt:
	.byte "> ",0

; During parsing or processing it is possible to generate an exception
; This function restores context to the checkpoint state and resumes.
lispCatch:
        jsr cputs
	`printcr
	ldx _SSAVE
	txs
	ldx _XSAVE
	jmp _while
.scend

_cannotApplyErr:
	.byte "cannot apply",AscCR,AscLF,0

_tooFewArgsErr:
	.byte "too few arguments (at least: 1 got: 0)",0

_unboundSymnbolErr:
	.byte "error: unbound symbol",0
.scend
