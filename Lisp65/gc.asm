; -----------------------------------------------------------------------------
; Garbage collector related functions that managed items that support the
; cell interface. It uses the unmanaged array type to holld collections of
; cells.
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
.space _PROTECTED	2	; pointer to array of protected cell pointers
.space _SIZE		2	; Hold the request size during a GC.
.space _TYPE		2	; Hold the request type during a GC.
.text

;
; Macros
;

;
; Functions
;

; Initializes the garbage collector.
gcInit:
	jsr hInit
	`pushi $10
	jsr arrayAlloc
	`pop _PROTECTED
	rts

; Returns garbage collector managed memory allocations. All allocations
; must contain a cell data type byte, and support methods that the
; garbage collector understands (e.g. cellMark, cellFree).
; input - two stack cells containing type (nos) and size (tos)
; output - a cell pointer of desired size the stack.
gcAlloc:
	`peekNos _TYPE		; retain the inputs
	`peek _SIZE
	jsr hAllocGC		; allocate block
	`tosZero?
	bne _exit		; Exit if allocation worked.
	jsr gcDoGC
	`push _TYPE
	`push _SIZE
	jsr hAllocGC
	`tosZero?
	bne _exit		; Exist if allocation worked after GC.
	`println _GCFailed
	brk
_exit:	rts

gcDoGC:
	`print _GCInProgress
	jsr gcMark
	jsr gcSweep
	jsr printtos
	`drop
	`println _GCComplete
	rts

; Adds a cell to the protected from garbage collection list. It is called to
; protect root cells or new cells from garbage collection.
; input - cell pointer on the stack
; output - cell pointer is droped from the stack
gcProtect:
.scope
	`push _PROTECTED
	`swap
	jsr arrayAppend
	`pop _PROTECTED		; append may reallocate, save returned pointer.
	rts
.scend

; Removes a cell from the protected from garbage collection list. It is called
; on new cells after list insertion when they can now survive a gc.
; input - cell pointer on the stack
; output - cell pointer is droped from the stack
gcUnprotect:
	`push _PROTECTED
	`swap
	jsr arrayFindLast
	lda TOS_MSB,x		; a negative index means not found.
	bmi _done
	`push _PROTECTED
	`swap
	jsr arrayDeleteAt	; delete the value
_done:
	rts

; Starts at the list of protected cells, and teaverses all linked cells.
; They are marked as referenced and inelligible for garbage collection.
; input - none
; output - none
gcMark:
.scope
	`push _PROTECTED
	`pushi cellMark
	jsr arrayForEach
	`pushzero
	rts
.scend

; Starts at the head of the heap and finds all cells. Any unmarked are
; freed and returned to the free list. At the end the heap is defragmented.
; input - none
; output - number of bytes reclaimed.
gcSweep:
.scope
	jsr hStats		; Get the stats and keep the free space.
	`nip
	`pushi _sweepCell
	jsr hForEach
	jsr hdefrag
	jsr hStats		; Get the stats and keep the free space.
	`nip
	`swap
	jsr sub16		; Determine the number of bytes reclaimed.
	rts
_sweepCell:
	`pushi 5		; Advance past headed FIXME
	jsr add16
	`dup
	jsr hGetGC
	beq +			; Return if block is not managed.
	`dup
	jsr hSweep		; Get and reset the mark bit.
	beq +
	jsr hFree		; block was not marked, so free it.
	rts
*	`drop
	rts
.scend

; Prints the current number of protected root cells, and their addresses.
; input - none
; output - none
gcPrintProtected:
.scope
	`push _PROTECTED
	jsr arrayPrint
	rts
.scend

_GCInProgress:	.byte "GC in progress ",0
_GCFailed:	.byte "GC collection unsuccessful.",0
_GCComplete:	.byte " bytes reclaimed.",0

.scend
