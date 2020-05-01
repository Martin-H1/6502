; -----------------------------------------------------------------------------
; Array abstract data type.
; Note: Array's are not cells, they are not managed by the garbage collector.
; But they can be wrapped by a vector cell which acts as a smart pointer.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;
.alias _GROWTH $10		; array expansion factor.
.alias _HEADER_SIZE CELL	; header contains a current size cell.

;
; Data segments
;
.data ZPDATA
.space _TMPPTR		2	; pointer used for indirection.
.space _JMPPTR		2	; pointer used for indirect jump.

.data BSS
.space _CAPACITY	2	; temporaries to hold value for reuse.
.space _COUNT		2
.space _VALUE		2

.text

;
; Macros
;
.macro _idxToBytes
	`pushi 1
	jsr lshift16		; convert index to bytes.
	`pushi _HEADER_SIZE
	jsr add16		; add header to offset
.macend

;
; Functions
;

; Allocates memory for an array and initiaizes the count to zero.
; input - number of cells required on stack.
; output - array pointer
arrayAlloc:
	`_idxToBytes		; convert to byte count.
	jsr halloc
	`pushzero
	`over
	`store
	rts

; Appends a value at the end of the array. It may reallocate, so the
; pointer is returned and consumers should save the value just in case.
; input - array pointer (nos) and value (tos)
; output - array pointer which may be reallocated.
arrayAppend:
.scope
	`pop _VALUE
	`dup
	jsr arrayCapacity
	`peek _CAPACITY
	`over
	jsr arrayCount
	`peek _COUNT
	`if_less16
	`push _CAPACITY
	`_idxToBytes
	`pushi _GROWTH
	jsr add16
	jsr hrealloc
_else:
	`push _COUNT		; ptr, count
	`_idxToBytes
	`over			; ptr, offset, ptr
	jsr add16		; ptr, end_ptr
	`push _VALUE
	`swap			; ptr, value, end_ptr
	`store

	`incw _COUNT		; increment the array count
	`push _COUNT
	`over
	`store
	rts
.scend

; Returns the capacity of the array.
; input - array address (tos).
; output - the max number of cells.
arrayCapacity:
	jsr hsize		; get allocation size.
	`pushi _HEADER_SIZE
	jsr sub16
	`pushi 1
	jsr rshift16
	rts

; Returns the number of array elements.
; input - array address (tos).
; output - the number of filled cells.
arrayCount:
	`fetch			; count is in header cell.
	rts

; Deletes an array item at the specified index and decreases the count
; input - array pointer (nos) and index to delete.
; output - items are droped from the stack
arrayDeleteAt:
.scope
	`peek _VALUE		; save the index
	`_idxToBytes		; convert to byte offset
	`over			; add it to base pointer.
	jsr add16
	`peek _TMPPTR
	`inctos
	`inctos
	`pop _JMPPTR		; we now have a from and to pointer
	`dup
	jsr arrayCount		; compute number of cells to copy
	`dectos
	`peek _COUNT
	`push _VALUE
	jsr sub16
	`pushi 1
	jsr lshift16		; convert to byte count
	phy
	ldy #0
_loop:	`toszero?
	beq _endLoop
	lda (_JMPPTR),y		; copy the bytes
	sta (_TMPPTR),y
	iny
	bne +
	inc _TMPPTR+1
	inc _JMPPTR+1
*	`decTos
	bra _loop
_endloop:
	ply
	`drop
	`push _COUNT
	`swap
	`store
	rts
.scend

; Returns the index of the first instance of a value
; input - array address (nos) and value (tos).
; output - the index containing the value, or -1
arrayFindFirst:
.scope
	`pop _VALUE		; save all input arguments,
	`dup
	jsr arrayCount
	`pop _COUNT		; and loop limit.

	`pushi _HEADER_SIZE	; advance pointer past the header.
	jsr add16
	`pop _TMPPTR
	`pushzero
_for:	`dup
	`push _COUNT
	`if_less16
.scope
	`pushInd _TMPPTR	; retrieve the item value.
	`incw _TMPPTR
	`push _VALUE
	`if_equals16
	rts			; return index on stack.
_else:
	`inctos
	bra _for
.scend
_else:				; end of for loop
	`drop
	`pushi $ffff
	rts
.scend

; Returns the index of the last instance of a value
; input - array address (nos) and value (tos).
; output - the index containing the value, or -1.
arrayFindLast:
.scope
	`pop _VALUE		; save all input arguments,
	`dup
	jsr arrayCount
	`dectos			; decrement count to get an index.
	`peek _COUNT		; and loop limit.
	`_idxToBytes		; Create a pointer to last element.
	jsr add16
	`pop _TMPPTR
	`push _COUNT
	`push _TMPPTR
	`drop
_for:	`dup
	`pushTrue		; true is -1
	`if_greater16
.scope
	`pushInd _TMPPTR	; retrieve the item value.
	`push _VALUE
	`if_equals16
	rts			; return index on stack.
_else:
	`decw _TMPPTR		; move back one item.
	`decw _TMPPTR
	`decw _TMPPTR
	`dectos			; decrement the index
	bra _for
.scend
_else:				; end of for loop
	`drop
	`pushi $ffff
	rts
.scend

; Returns the array element.
; input - array address (nos) and cell index (tos).
; output - the cell value.
arrayGetAt:
	`_idxToBytes		; convert index to a byte offset.
	jsr add16		; add base address to offset
	`fetch			; retrieve cell value.
	rts

; Returns the array element.
; input - array address (third), cell index (nos), and cell value.
; output - elements are consumed
arraySetAt:
	`mrot
	`_idxToBytes
	jsr add16		; add base address to offset
	`store
	rts

; Iterates over an array with the specified function. It pushes the
; array cell value, and calls the function pointer.
; input - pointer to array (nos) and function pointer (tos).
; output - none
arrayForEach:
.scope
	phy
	ldy #0
	`pop _JMPPTR
	`dup
	jsr arrayCount
	`swap
	`pushi _HEADER_SIZE
	jsr add16		; add header to offset
	`pop _TMPPTR
_loop:
	`toszero?
	beq _endLoop
	`pushIndy _TMPPTR
	jsr _indirectJmp
	iny
	bne +
	inc _TMPPTR+1
*	`decTos
	bra _loop
_endLoop:
	`drop
	ply
	rts
_indirectJmp:
	jmp (_JMPPTR)
.scend

; Prints the item capacity, item count, and items.
; input - array pointer
; output - none
arrayPrint:
.scope
	`dup
	`print _addressMsg
	jsr printTosln
	`drop

	`dup
	jsr arrayCapacity
	`print _capacityMsg
	jsr printTosln
	`drop
	`dup
	jsr arrayCount
	`print _countMsg
	jsr printTosln
	`drop
	`print _cellsMsg
	`pushi _printTos
	jsr arrayForEach
	`println _endMsg
	rts
_printTos:
	jsr printTos
	`print _commaMsg
	`drop
	rts
.scend

_addressMsg:	.byte "address = ",0
_capacityMsg:	.byte "capacity = ",0
_countMsg:	.byte "count = ",0
_cellsMsg:	.byte "cells = [ ",0
_commaMsg:	.byte ", ",0
_endMsg:	.byte "]",0

.scend
