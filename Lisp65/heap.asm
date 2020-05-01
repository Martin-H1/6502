; -----------------------------------------------------------------------------
; Heap data stucture implementation.
; Consumer is responsible for avoiding memory leaks, but it will defragment
; on demand if possible.
;
; Note: Hooks for a garbage collector exist and it can be added if consumers
; avoid the use of raw allocations and pointers, and instead access the heap
; via a memory management utility.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

; A heap is a singly linked list of blocks. A block consists of a header
; and a payload. In turn the block header contains length, next pointer,
; and a meta data byte. At the bottom of the module are getter and setter
; functions to manage these elements.
;
; +D7-----------------D0+
; |   block length lsb  |
; +---------------------+
; |   block length msb  |
; +---+---+---+---------+
; |   next pointer lsb  |
; +---------------------+
; |   next pointer msb  |
; +---------------------+
; | block metadata bits |
; +---------------------+
;
; At the present time three metadata bits are used by heap and GC.
; F - free bit, indicates if the block is free or in use.
; G - garbage collector controls block lifecyle.
; M - marked bit during garbage collection phase.
; Bits 0 to 3 are available for heap consumers.

;
; Aliases
;
.alias _ALLOCATED	$00
.alias _FREE		$80
.alias _GC		$40
.alias _MARK		$20
.alias _USER		$0f	; four meta data bits are for heap users.

.alias _GC_MASK		$bf
.alias _MARK_MASK	$df
.alias _USER_MASK	$f0	; four meta data bits are for heap users.

.alias _HEADER_SIZE	CELL*2+1

.alias NULL		$00	; Null used to terminate linked lists.

;
; Data segments
;
.data ZPDATA
.space _TMPPTR	2		; working pointers
.space _JMPPTR	2

.data BSS
.space _HEAPPTR	2		; pointer to heap area
.space _HEAPFREE 2		; size of heap free space
.space _HEAPSIZE 2		; size of heap area
.space _BLKSIZE	2		; desired size of block

.text

;
; Macros
;
.macro _addHdrSize
	`pushi _HEADER_SIZE
	jsr add16
.macend

.macro _subHdrSize
	`pushi _HEADER_SIZE
	jsr sub16
.macend

.macro _ptrToHdr		; back up the pointer to the header.
	`_subHdrSize
.macend

.macro _hdrToPtr		; advance past header to pointer.
	`_addHdrSize
.macend

;
; Functions
;

; Sets up the heap pointer, initial block, and null last pointer
; TOS contains the desired heap size, other pointers (e.g. free list
; are globals near the end of zero page.
hInit:
	`peek _HEAPPTR		; Save the heap address in the heap pointer.
	`peeknos _HEAPSIZE	; Save the total heap size.
	jsr _blockSetSize	; Store heap size in the first block.

	`push _HEAPSIZE		; Initialize heap free space.
	`_subHdrSize
	`pop _HEAPFREE

	`pushi NULL		; Put the null pointer on the stack
	`push _HEAPPTR		; Put the heap pointer on the stack
	jsr _blockSetNext

	`push _HEAPPTR		; Put the heap pointer on the stack
	lda #_FREE		; mark block as free.
	jsr _blockSetMetadata
	rts

; Allocates a block of the size specified in TOS, drops the argument,
; and returns the pointer to the block on TOS, OR null.
; input - desired size of block.
; output - a pointer to the allocated block, or NULL.
hAlloc:
.scope
	; Save the requested size.
	`pop _BLKSIZE
	`pushi _lambdaFunc
	jsr hForEach
	`_hdrToPtr
	rts

; Allocates a gc managed block of the size specified in TOS, drops the
; argument, and returns the pointer to the block on TOS, or null.
; input - tag value (nos), desired size of block (tos).
; output - a pointer to the allocated block, or NULL.
hAllocGC:
	; Save the requested size.
	`pop _BLKSIZE
	`pushi _lambdaFunc
	jsr hForEach
	lda NOS_LSB,x
	and #_USER		; make sure only user bits are set
	ora #_GC		; mark the block as GC managed.
	pha
	`dup
	pla
	jsr _blockSetMetadata
	`swap
	`drop
	`_hdrToPtr
	rts

; Callback that determines if the current block can be used for the
; allocation.
; input - pointer to block.
; output - input dropped, it can also pop the iteration state and push outputs.
_lambdaFunc:
	; Save the current working pointer.
	`peek _TMPPTR

	; if block is free, then exit.
	jsr _blockGetMetadata
	and #_FREE
	beq _exit
	; Is this block large enough to split?
	`push _BLKSIZE
	`push _TMPPTR
	jsr _blockGetSize
	`if_less16
	jsr _blockSplit

	; Stack has iteration context (nos) and return pointer (tos).
	`swap
	`drop
	`pushzero		; terminate the iteration.
	rts
_else:
	; If the next pointer is null, then set up to return null.
	`push _TMPPTR
	jsr _blockGetNext
	`toszero?
	beq +
	`drop			; drop the next pointer.
	rts
*	; Next is NULL, block was too small, so return null.
	`drop
	`pushzero		; return null
	`pushzero		; halt iteration
_exit:
	rts
.scend

; Marks the block specified by the pointer at TOS, and drops the argument.
hFree:
	`_ptrToHdr		; back up the pointer to the header.
	`dup			; Add the free space back to counter
	jsr _blockGetSize
	`push _HEAPFREE
	jsr add16
	`pop _HEAPFREE

	lda #_FREE
	jsr _blockSetMetadata
	rts

; Scan the heap looking for adjacent free blocks that can be joined into
; a single largerblock. Useful after a consumer has freed large numbers
; small objects such as strings.
hDefrag:
.scope
	`pushi _lambdaFunc
	jsr hForEach
	rts
_lambdaFunc:
	; if block is not free, continue.
	`dup
	jsr _blockGetMetadata
	and #_FREE
	beq +
	jsr _blockMerge
*	`drop
	rts
.scend

; Iterates over the heap with the specified function. It pushes the block
; pointer, and calls the function pointer. This is used internally, but
; can also be used by a memory manager.
; Note: The called function can short circuit iteration by doing an extra
; drop and pushing null.
; input - function pointer (tos).
; output - none
hForEach:
.scope
	`pop _JMPPTR

	; push heap ptr on the stack.
	`push _HEAPPTR
_while:
	; while work pointer != null begin
	`toszero?
	beq _end_while
	`dup
	jsr _indirectJmp

	; check if callback halted iteration
	`toszero?
	beq _end_while

	; Get next pointer.
	jsr _blockGetNext
	bra _while
_end_while:
	`drop
	rts
_indirectJmp:
	jmp (_JMPPTR)
.scend

; Returns the value in the GC bit in the block allocation header. This is
; useful for implementing a garbage collector as it indicates a block's
; lifescycle is managed.
; It doesn't use the block level function for to avoid unneeded artihmetic.
; input - pointer to existing block.
; output - metadata bits in acccumulator.
hGetGC:
	`dectos
	lda (TOS_LSB,x)		; get LSB and mask off top nybble
	`drop
	and #_GC
	rts

; Returns the user bits in the block allocation header. This is useful for
; tracking the type of allocation and implementing a simple type system.
; It doesn't use the block level function for to avoid unneeded artihmetic.
; input - pointer to existing block.
; output - metadata bits in acccumulator.
hGetType:
	`dectos
	lda (TOS_LSB,x)		; get LSB and mask off top nybble
	`drop
	and #_USER
	rts

; Sets the mark bit in the allocation header.
; input - pointer to existing block.
; output - input consumed.
hMark:
	`dectos
	lda (TOS_LSB,x)		; get LSB and mask off top nybble
	ora #_MARK
	sta (TOS_LSB,x)		; get LSB and mask off top nybble
	`drop
	rts

; Gets the current value of the mark bit
; input - pointer to existing block.
; output - input consumed, but mark value in accumulator.
hMarkGet:
	`dectos
	lda (TOS_LSB,x)		; get metadata and mask off unneeded bits.
	and #_MARK
	`drop
	rts

; Gets the current value of the mark bit, returns it on the stack
; and clears the mark bit.
; input - pointer to existing block.
; output - input consumed, but mark value in accumulator.
hSweep:
	`dectos
	lda (TOS_LSB,x)		; get metadata and mask off unneeded bits.
	and #_MARK
	pha
	lda (TOS_LSB,x)		; get metadata and clear mark bit
	and #_MARK_MASK
	sta (TOS_LSB,x)		; store back in the header.
	`drop
	pla
	rts

; Allocates a larger block, copies data from the old block, and frees it.
; input - pointer to existing block and new desired size.
; output - pointer to reallocated block.
hRealloc:
.scope
	jsr halloc
	`toszero?
	bne +
	`swap			; reallocation failed!
	`drop			; clean up stack and return null.
	rts
*	`tuck
	`over
	jsr hsize
	jsr memcpy
	rts
.scend

; Returns the user area size of a block.
; input - pointer to existing block.
; output - size of useable area.
hSize:
.scope
	`_ptrToHdr		; back up to header.
	jsr _blockGetSize	; get block size
	`_subHdrSize		; remove header from size.
	rts
.scend

; Returns the statistics of the heap.
; input - none
; output - heap size (nos) amd heap free (tos).
hStats:
	`push _HEAPSIZE
	`push _HEAPFREE
	rts

; Debug function that traverses the heap ensureing it is structually
; sound. It also prints the heap to the console to aid in debugging.
hValidate:
.scope
	`print _heap_base_equals
	`push _HEAPPTR
	jsr printtosln
	`drop
	jsr hStats
	`swap
	`print _heap_size_equals
	jsr printtosln
	`drop
	`print _heap_free_equals
	jsr printtosln
	`drop
	`printcr
	`pushi _blockPrintHdr
	jsr hForEach
	rts
.scend

; Prints the content of the block
; input - address of a data area returned by halloc
; output - pointer droped from stack
hPrintBlock:
	`_ptrToHdr		; back up the pointer to the header.
	`peek _TMPPTR
	`dup
	jsr _blockPrintHdr
	jsr _blockGetSize
	jsr printtosln
	`printcr
	phy
	ldy #0
_loop:	lda (_TMPPTR),y
	jsr printa
	iny
	bne +
	inc _TMPPTR+1
*	`decTos
	bne _loop
	`drop
	`printcr
	ply
	rts

; prints a single block
_blockPrintHdr:
	`print _block_ptr_equals
	jsr printtosln

	`print _block_metadata_equals
	`dup
	jsr _blockGetMetadata
	jsr printa
	`printcr

	`dup
	jsr _blockGetSize
	`print _block_size_equals
	jsr printtosln
	`drop

	jsr _blockGetNext
	`print _block_next_equals
	jsr printtosln
	`printcr
	`drop
	rts

; routine that does the actual mechanics of bit twiddling
_blockGetMetadata:
	`pushi CELL*2
	jsr add16
	lda (TOS_LSB,x)		; get MSB and mask off free bit.
	`drop
	rts

; Takes a pointer to a block at TOS and returns the next pointer
_blockGetNext:
	`pushi CELL
	jsr add16
	`fetch
	rts

; takes a pointer to a block at TOS and returns its size on TOS.
_blockGetSize:
	`fetch
	rts

; Takes a pointer to a block at TOS, the metadata byte in the accumulator
; and stores that value in the header.
_blockSetMetadata:
	pha			; save for later.
	`pushi CELL*2
	jsr add16
	pla
	sta (TOS_LSB,x)		; put back into block
	`drop
	rts

; takes a pointer to a block and returns the next pointer
_blockSetNext:
	`pushi CELL		; Advance one word to the next pointer
	jsr add16
	`store		; store the value at NOS
	rts

; takes a pointer to a block and sets its size.
_blockSetSize:
	`store
	rts

; Takes a pointer to a block and the desired size on the stack.
; Computes a pointer to a new block, sets its next pointer and size,
; then reset the next pointer of the current block.
; Finally return a pointer to the block past the header.
_blockSplit:
.scope
	; push the current block's next pointer on the stack.
	`push _TMPPTR
	jsr _blockGetNext

	;  mark the current block allocated.
	`push _TMPPTR
	lda #_ALLOCATED
	jsr _blockSetMetadata

	; compute the size of the split block
	`push _TMPPTR
	jsr _blockGetSize
	`push _BLKSIZE
	jsr sub16
	`_subHdrSize

	; compute the new next pointer
	`push _TMPPTR
	`_addHdrSize
	`push _BLKSIZE
	jsr add16

	; set this as the current block's next.
	`dup
	`push _TMPPTR
	jsr _blockSetNext

	; mark the new block free
	`dup
	lda #_FREE
	jsr _blockSetMetadata

	; set the new block's size
	jsr _blockSetSize

	; set the new block's next
	`push _TMPPTR
	jsr _blockGetNext
	jsr _blockSetNext

	; Determine the split block's size
	`push _BLKSIZE
	`_addHdrSize

	; Update heap free space
	`push _HEAPFREE
	`over
	jsr sub16
	`pop _HEAPFREE

	; set the split block's size
	`push _TMPPTR
	jsr _blockSetSize

	; return a pointer to the data portion of the split block.
	`push _TMPPTR
	rts
.scend

; Takes a pointer to a block and merges it with the block that follows.
;  _TMPPTR contains the working pointer.
_blockMerge:
.scope
	; get the next block pointer.
	`peek _TMPPTR
	`dup
	jsr _blockGetNext

	; is the next block also free?
	`dup
	jsr _blockGetMetadata
	and #_FREE
	bne +
	; Next block is not free, so clean up stack and return
	`drop
	rts
*
	; get the next block's size and next pointer
	`dup
	jsr _blockGetSize
	`swap
	jsr _blockGetNext

	; set the current block's next pointer.
	`push _TMPPTR
	jsr _blockSetNext

	; get the current block's size
	`push _TMPPTR
	jsr _blockGetSize

	; add the sizes together and store.
	jsr add16
	`push _TMPPTR
	jsr _blockSetSize
	rts
.scend

_heap_base_equals:	.byte "Heap base=",0
_heap_free_equals:	.byte "Heap free=",0
_heap_size_equals:	.byte "Heap size=",0
_block_metadata_equals:	.byte "Block metadata=",0
_block_next_equals:	.byte "Block next=",0
_block_ptr_equals:	.byte "Block ptr=",0
_block_size_equals:	.byte "Block size=",0
.scend
