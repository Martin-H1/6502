; -----------------------------------------------------------------------------
; The origin of this code is Scot W. Stevenson's <scot.stevenson@gmail.com>
; Tali Forth for the 65C02. His ideas were sound, but his code was inline
; and lacked abstraction or resuability.
;
; My goal with these macros is to implement the stack abstract data type to
; make Tali Forth more readable, and create a resuable stack library. Most
; operations align with the classic data structure, but with the addition of
; methods to push to top of stack from different sources (e.g. accumulator,
; RAM, and return stack), and direct access to TOS and NOS for efficiency.
;
; The argument stack is the first half of zero page. It starts at $7F and
; grows downward towards $00 (128 bytes --> 64 words). This allows for
; over and underlow detection via highest bit being zero. It also reserves
; half of zero page for direct addressing.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;

.alias CELL	$02	; Size of word and cell on the stack.

; offset from X register for NOS and TOS.
.alias TOS_LSB	$00
.alias TOS_MSB	$01
.alias NOS_LSB	$02
.alias NOS_MSB	$03
.alias THS_LSB	$04
.alias THS_MSB	$05

;
; Data segments
;
.data ZPSTACK
.space SPMAX	$00		; top of parameter (data) stack
.space _SSPACE	$80
.space SP0	$00		; bottom of parameter (data) stack

.data ZPDATA
.space _INDPTR	$02		; page zero space used for indirection

.text				; revert back to code segment.

;
; Macros
;

; make room on the argument stack.
.macro advance
        dex
	dex
.macend

; drops a word from the stack
.macro drop
        inx
	inx
.macend

; check for tos equals zero.
.macro tosZero?
	lda TOS_LSB, x
	ora TOS_MSB, x
.macend

; decrements the TOS value
.macro decTos
	lda TOS_LSB,x
	bne _over
	dec TOS_MSB,x
_over:	dec TOS_LSB,x
.macend

; increments the TOS value
.macro incTos
        inc TOS_LSB,x
        bne _over
        inc TOS_MSB,x
_over:
.macend

; Nondestructively saves the word at TOS to the location provided.
.macro peek
	lda TOS_LSB,x
	sta _1
	lda TOS_MSB,x
	sta _1+1
.macend

; Nondestructively saves the word at TOS using indirect y addressing.
.macro peekIndY
	lda TOS_LSB,x
	sta (_1),y
	iny
	lda TOS_MSB,x
	sta (_1),y
.macend

; Destructively saves the word at TOS to the location provided.
.macro pop
	`peek _1
	`drop
.macend

; Destructively saves the word at TOS using indirect y addressing.
.macro popIndY
	`peekIndY _1
	`drop
.macend

; pushes the immediate literal provided as the argument
.macro pushi
        `advance
        `puti _1
.macend

; pushes the value in the accumulator onto stack and zero extends it.
.macro pushA
        `advance
        `putA
.macend

; pushes the value at the address specified at the argument.
.macro push
        `advance
	`put _1
.macend

;  pushes the value by dereferencing the pointer at the argument.
.macro pushInd
        `advance
        `putInd _1
.macend

;  pushes the value by using indirect y addressing of the argument.
.macro pushIndY
        `advance
        `putIndY _1
.macend

; pushes true onto the stack
.macro pushTrue
	`advance
	`putTrue
.macend

; pushes zero onto the stack
.macro pushZero
	`advance
	`putZero
.macend

; nondestructively saves the word at NOS to the location provided.
.macro peekNos
	lda NOS_LSB,x
	sta _1
	lda NOS_MSB,x
	sta _1+1
.macend

; loads TOS with the word at location provided
.macro put
	lda _1
	sta TOS_LSB,x
	lda _1+1
	sta TOS_MSB,x
.macend

; loads TOS with the immediate value
.macro puti
        lda #<_1		; LSB
        sta TOS_LSB,x
        lda #>_1		; MSB
        sta TOS_MSB,x
.macend

; loads the value by dereferencing the pointer at the argument.
.macro putInd
	lda (_1)
	sta TOS_LSB,x
	`incw _1
	lda (_1)
	sta TOS_MSB,x
.macend

; loads the value by using indirect y address of the argument.
.macro putIndY
	lda (_1),y
	sta TOS_LSB,x
	iny
	lda (_1),y
	sta TOS_MSB,x
.macend

; loads TOS with the accumulator padded with zeros.
.macro putA
	sta TOS_LSB,x
	stz TOS_MSB,x
.macend

; loads TOS MSB and LSB with the accumulator.
.macro putAA
	sta TOS_LSB,x
	sta TOS_MSB,x
.macend

; loads TOS with true (-1)
.macro putTrue
	lda #$FF
	`putAA
.macend

; makes the TOS zero
.macro putZero
	stz TOS_LSB, x
	stz TOS_MSB, x
.macend

; increments the NOS value
.macro incNos
        inc NOS_LSB,x
        bne _over
        inc NOS_MSB,x
_over:
.macend

; loads the word at location provided to NOS
.macro putNos
	lda _1
	sta NOS_LSB,x
	lda _1+1
	sta NOS_MSB,x
.macend

; loads the value in the accumulator to NOS and zero extends it.
.macro putNosA
	sta NOS_LSB,x
	stz NOS_MSB,x
.macend

; loads the NOS with the value in TOS.
.macro putNosFromTos
	lda TOS_LSB,x
	sta NOS_LSB,x
	lda TOS_MSB,x
	sta NOS_MSB,x
.macend

; loads the TOS with the value in NOS.
.macro putTosFromNos
	lda NOS_LSB,x		; copy the word.
	sta TOS_LSB,x
	lda NOS_MSB,x
	sta TOS_MSB,x
.macend

;
; This set of macros manipulates the return stack.
;

; Moves TOS cell from the data stack to return stack.
.macro peekToR
	lda TOS_MSB,x
	pha
	lda TOS_LSB,x
	pha
.macend

; Moves NOS cell from the data stack to return stack.
.macro peekNosToR
	lda NOS_MSB,x
	pha
	lda NOS_LSB,x
	pha
.macend

; Moves a cell from return stack to memeory.
.macro popFromR
	pla
	sta _1
	pla
	sta _1+1
.macend

; Moves a cell from the data stack to return stack.
.macro popToR
	`peekToR
	`drop
.macend

; Moves a cell from return stack to data stack.
.macro pushFromR
	`advance
	pla
	sta TOS_LSB,x
	pla
	sta TOS_MSB,x
.macend

; Moves a cell from memory to return stack.
.macro pushToR
	lda _1+1
	pha
	lda _1
	pha
.macend

; sign extends a single stack cell to a double stack cell
.macro sToD
	`advance
	lda NOS_MSB,x
	bpl _else
	`putTrue
	bra _endif
_else:	`putZero
_endif:
.macend

;
; Functions
;

; duplicates the value at TOS on the stack.
f_dup:
	`advance
	`putTosFromNos
	rts

; transfers control to the address at TOS
f_execute:
	`pop _INDPTR
	jmp (_INDPTR)

; fetch dereferences the current TOS and replaces the value on TOS.
f_fetch:
        `peek _INDPTR
        `putInd _INDPTR
	rts

; deletes NOS on the stack.
f_nip:
	`putNosFromTos
	`drop
	rts

; allocates a cell and stores the value in THS to TOS
f_over:
	`advance
	lda THS_LSB,x
	sta TOS_LSB,x
	lda THS_MSB,x
	sta TOS_MSB,x
	rts

; Rotate the top three entries upwards
f_mrot:
	`peekToR
	`putTosFromNos
	lda THS_LSB,x
	sta NOS_LSB,x
	lda THS_MSB,x
	sta NOS_MSB,x
	pla
	sta THS_LSB,x
	pla
	sta THS_MSB,x
	rts

; Rotate the top three entries downwards
f_rot:
	lda THS_LSB,x
	pha
	lda THS_MSB,x
	pha
	lda NOS_LSB,x
	sta THS_LSB,x
	lda NOS_MSB,x
	sta THS_MSB,x
	`putNosFromTos
	pla
	sta TOS_MSB,x
	pla
	sta TOS_LSB,x
	rts

; stores the value in NOS at the address specified in TOS and drops
; the values from the stack.
f_store:
        lda NOS_LSB,x		; LSB
        sta (TOS_LSB,x)
	`incTos
	lda NOS_MSB,x		; MSB
        sta (TOS_LSB,x)

	`drop
	`drop
	rts

; swaps top of stack (TOS) to next on stack (NOS)
f_swap:
	`peekNosToR
	`putNosFromTos
	pla
	sta TOS_LSB,x
	pla
	sta TOS_MSB,x
	rts

;
; Syntactic sugar macros for the above.
;
.macro dup
	jsr f_dup
.macend
.macro execute
	jsr f_execute
.macend
.macro fetch
	jsr f_fetch
.macend
.macro nip
	jsr f_nip
.macend
.macro over
	jsr f_over
.macend
.macro mrot
	jsr f_mrot
.macend
.macro rot
	jsr f_rot
.macend
.macro swap
	jsr f_swap
.macend
.macro store
	jsr f_store
.macend
.macro tuck
	`dup
	`mrot
.macend
