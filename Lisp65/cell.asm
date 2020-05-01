; -----------------------------------------------------------------------------
; Lisp cell data stucture implementation.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

; A cell consists of:
; Tag - a byte field that describes the cell data type.
; Payload - a variable length payload described by the tag.

;
; Aliases
;

; Enum of values for tag.
.alias C_NONE		$00	; No payload
.alias C_NUMBER		$01	; Payload is a 32 bit number
.alias C_STRING		$02	; Payload is a string pointer.
.alias C_SYMBOL		$03	; Payload is a string pointer.
.alias C_CONS		$04	; Payload is a pair of cell pointers.
.alias C_SUBR		$05	; Payload is a subroutine pointer.
.alias C_FSUBR		$06
.alias C_EXPR		$07	; Payload is an expression list.
.alias C_FEXPR		$08

;
; Data segments
;
.data ZPDATA
.space _WORKPTR1	$02
.space _WORKPTR2	$02

.text

;
; Macros
;
.macro cellGetType
	`dup
	jsr hGetType
.macend

.macro cellGetMark
	`dup
	jsr hMarkGet
.macend

.macro cellSetMark
	`dup
	jsr hMark
.macend

;
; Functions
;

; Constructor for a number cell.
; A 16 bit number on stack.
; retuns pointer to the cell on stack
cellMkNumber:
.scope
	`pushi C_NUMBER		; metadata tag value
	`pushi CELL		; 16 bits
	jsr gcAlloc
	`peek _WORKPTR1
	`store
	`push _WORKPTR1
	rts
.scend

; Constructor for a string cell.
; string pointer is on argument stack.
cellMkString:
.scope
	lda #C_STRING		; Save the tag type on the stack.
	pha
_MkSCommon:
	`dup
	jsr strlen		; need string size to size cell.
	`incTos			; plus one more for the null.
	pla
	`pushA
	`swap
	jsr gcAlloc
	`peek _WORKPTR1		; Save the returned pointer.
	`swap			; string pointer is at TOS, cell ptr at NOS
	`push _WORKPTR1
	`swap			; strptr, workptr, cell ptr on stack.
	jsr strcpy
	`drop			; only cell ptr remains
	rts

; Constructor for a symbol cell.
; string pointer is on argument stack.
cellMkSymbol:
	lda #C_SYMBOL		; Save the tag into the cell.
	pha
	bra _MkSCommon
.scend

; Constructor for a list cell.
; two cell pointers are on argument stack.
; pointer to cons cell is returned.
cellMkCons:
.scope
	`pushi C_CONS
	`pushi 2*CELL		; Two pointers.
	jsr gcAlloc
	`peek _WORKPTR1		; clone the pointer.

	; Move the second pointer to TOS and store in the cell.
	`swap
	`push _WORKPTR1
	`pushi CELL
	jsr add16
	`store

	; Move the first pointer to TOS and store in the cell.
	`swap
	`push _WORKPTR1
	`store

	;  cell pointer is now at TOS, so return it.
	rts
.scend

; Constructor for a subroutine cell. These cells point to a basic unit
; of interpreter function (e.g. +, -, cons, car, cdr, etc) that are
; written in assembly.
; a pointer to a function is on the argument stack.
; A cons cell is returned.
cellMkSubr:
.scope
	`pushi C_SUBR		; metadata tag value
	`pushi CELL		; A pointer.
_MkSubrCommon:
	jsr gcAlloc
	`peek _WORKPTR1		; clone the pointer.

	; Move the subroutine pointer to TOS and store in the cell.
	`swap
	`push _WORKPTR1
	`store
	rts

; Constructor for a function cell. These cells point to a special form
; of interpreter function (e.g. define, if, lambda, let, setq, etc) that
; are written in assembly.
; a pointer to a function is on the argument stack.
; A cons cell is returned.
cellMkFSubr:
	`pushi C_FSUBR		; the metadata tag value
	`pushi CELL		; a pointer.
	bra _MkSubrCommon
.scend

; Constructor for an expression cell. This cells point to an expression
; and an environment to use while evaluating.
; a pointer to an expression on the argument stack.
; a pointer to an environment on the argument stack.
; A cons cell is returned.
cellMkExpr:
.scope
	`pushi C_EXPR		; The metadata tag value
	`pushi 2*CELL		; Two pointers.
_MkExprCommon:
	jsr gcAlloc
	`peek _WORKPTR1		; clone the pointer.

	; Move the environment pointer to TOS and store in the cell.
	`swap
	`push _WORKPTR1
	`pushi CELL
	jsr add16
	`store

	; Move the expression pointer to TOPS and store in the cell.
	`swap
	`push _WORKPTR1
	`store

	;  cell pointer is now at TOS, so return it.
	rts

; Constructor for an fexpression cell. This cells point to an expression
; whose arguments are not evaluated and an environment to use while
; evaluating.
; a pointer to an fexpression on the argument stack.
; a pointer to an environment on the argument stack.
; A cons cell is returned.
cellMkFExpr:
	`pushi C_EXPR		; The metadata tag value.
	`pushi 2*CELL		; Two pointers.
	bra _MkExprCommon
.scend

; cellCar - returns the car pointer of the cell on the stack.
; A pointer to the cell on the stack.
cellCar:
.scope
	; if cell == null return
	`toszero?
	beq _exit

	; if cell is not a CONS, then return null;
	`cellGetType
	cmp #C_CONS
	beq +
	`putZero
	rts
*
	; Advance past Tag field.
	`fetch
_exit:
	rts
.scend

; cellCdr - returns the cdr pointer of the cell on the stack.
; A pointer to the cell on the stack.
cellCdr:
.scope
	; if cell == null return
	`toszero?
	beq _exit

	; if cell is not a CONS, then return null
	`cellGetType
	cmp #C_CONS
	beq +
	`putZero
	rts
*
	; Advance past the car fields.
	`pushi CELL
	jsr add16
	`fetch
_exit:
	rts
.scend

; cellRplaca - replaces the car of a cell.
; two pointer to the cell on the stack.
; TOS is the cell to alter and NOS is the new Car value.
cellRplaca:
.scope
	; Ensure the TOS is not NULL and points to a cons cell.
	; if cell == null return
	`toszero?
	beq _exit

	; if cell is not a CONS, then return
	`cellGetType
	cmp #C_CONS
	bne _exit
	`store
	rts
_exit:
	`drop
	`drop
	rts
.scend

; cellRplacd - replaces the cdr of a cell.
; two pointer to the cell on the stack.
; TOS is the cell to alter and NOS is the new Cdr value.
cellRplacd:
.scope
	; Ensure the TOS is not NULL and points to a cons cell.
	; if cell == null return
	`toszero?
	beq _exit

	; if cell is not a CONS, then return
	`cellGetType
	cmp #C_CONS
	bne _exit
	`pushi CELL		; advance past the header and car.
	jsr add16
	`store
	rts
_exit:
	`drop
	`drop
	rts
.scend

; cellMark - sets the cell's referenced mark to true, and recursively
; marks any cells referenced by this one.
; input - a pointer to the cell on the stack.
; output - pointer is consumed.
cellMark:
.scope
	`tosZero?
	beq _else
	`cellSetMark
	`cellGetType
	cmp #C_CONS
	bne _else
	`dup			; CONS cells require recursive processing
	jsr cellCar
	jsr cellMark
	jsr cellCdr
	jsr cellMark
	rts
_else:	`drop			; non-cons cells have nothing else to do.
	rts
.scend

; Prints a cell string representation to stdou
; input - a pointer to the cell on the stack.
; output - pointer is consumed.
cellPrint:
.scope
	`toszero?
	beq _exit
	; use the tag as an index into the dispatch table
	`peek _WORKPTR1
	`cellGetType
	asl
	phy
	tay
	lda _dispatch,y
	sta _WORKPTR2
	iny
	lda _dispatch,y
	sta _WORKPTR2+1
	ply
	jmp (_WORKPTR2)

_exit:	`drop
	rts

_dispatch:
	.word _NoneToString, _NumberToString, _StringToString, _SymbolToString
	.word _ConsToString, _SubrToString, _FSubrToString, _ExprToString
	.word _FexprToString, _OperatorToString

_NoneToString:
_NumberToString:
	`fetch
	jsr printtos
	`drop
	rts

_OperatorToString:
	`pop _WORKPTR2
	phy
	ldy #0
	lda (_WORKPTR2),y
	jsr putch
	iny
	lda (_WORKPTR2),y
	jsr putch
	ply
	rts

_StringToString:
	lda #'\"
	jsr putch
	jsr _SymbolToString
	lda #'\"
	jsr putch
	rts
_SymbolToString:
	`push _WORKPTR1
	jsr cputs
	`drop
	rts

_ConsToString:
	lda #'\(
	jsr putch
_while:
	`dup
	jsr cellCar
	jsr cellPrint
	jsr cellCdr
	`toszero?		; do while TOS != NULL
	beq _end_while

	lda #AscSP
	jsr putch

	`cellGetType		; check for dotted pair.
	cmp #C_CONS
	beq _while
	lda #'.
	jsr putch
	lda #AscSP
	jsr putch
	`dup
	jsr cellPrint
_end_while:
	lda #'\)
	jsr putch
	`drop
	rts

_SubrToString:
_FSubrToString:
_ExprToString:
_FexprToString:
	lda #'\$
	jsr putch
	`fetch
	jsr printtos
	`drop
	rts
.scend

.scend
