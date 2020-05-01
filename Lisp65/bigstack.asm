; -----------------------------------------------------------------------------
; This is a large stack that grows from the top of RAM downwards. It allows
; for deep call chains while keeping almost all of page zero free.
;  I am not sure if I am going to need this for the eval function yet.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

;
; Aliases
;
.alias nos_lsb	$03		; offset from _sp register to nos and tos.
.alias nos_msb	$04
.alias tos_lsb	$01
.alias tos_msb	$02

;
; Data segments
;

; Stack page zero allocations
.data zpbig
.org $00
.space _sp      $02
.space _counter $01
.space _temp	$01
.space _scratch	$02
.space TMPPTR1	$02
.space TMPPTR2	$02

;
; Macros
;

; drops a word from the stack
.macro drop
	lda #2
	jsr stack'free
.macend

; check for tos equals zero.
.macro toszero?
	ldy #tos_lsb
	lda (_sp),y
	bne _exit
	ldy #tos_msb
	lda (_sp),y
_exit:
.macend

; increment the tos.
.macro incTos
	ldy #tos_lsb
	lda (_sp),y
	inc
	sta (_sp),y
	bne _exit
	ldy #tos_msb
	lda (_sp),y
	inc
	sta (_sp),y
_exit:
.macend

; saves the word at TOS to the location provided.
.macro peek
	ldy #tos_lsb
	lda (_sp),y
	sta _1
	ldy #tos_msb
	lda (_sp),y
	sta _1+1
.macend

; saves the word at TOS to the location provided.
.macro pop
	`peek _1
	`drop
.macend

; pushes the immediate literal provided as the argument
.macro pushi
	lda #2
	jsr stack'allocate
	lda #<_1		; push the lsb and msb
	ldy #tos_lsb
	sta (_sp),y
	lda #>_1
	ldy #tos_msb
	sta (_sp),y
.macend

; pushes the value at the address specified at the argument.
.macro push
	lda #2
	jsr stack'allocate
	lda _1			; push the lsb and msb
	ldy #tos_lsb
	sta (_sp),y
	lda _1+1
	ldy #tos_msb
	sta (_sp),y
.macend

; pushes zero onto the stack
.macro pushzero
	lda #2
	jsr stack'allocate
	`putZero
.macend

; makes the TOS zero
.macro putZero
	ldy #tos_lsb
	stz (_sp),y
	ldy #tos_msb
	stz (_sp),y
.macend

;
; Functions
;
.text

.scope
stack'init:
        lda     #<ram_top
        sta     _sp
        lda     #>ram_top
        sta     _sp+1
        rts

stack'allocate:
	sta _temp
	sec
	lda _sp
	sbc _temp
	sta _sp
	lda _sp+1
	sbc #0
	sta _sp+1
	rts

stack'empty?:
	lda _sp
	cmp #<ram_top
	bne +
	lda _sp+1
	cmp #>ram_top
*
	rts

stack'free:
	sta _temp
	clc
	lda _sp
	adc _temp
	sta _sp
	lda _sp+1
	adc #0
	sta _sp+1
	rts

stack'print:
.scope
	lda _sp
	sta _scratch
	lda _sp+1
	sta _scratch+1
_while:
	jsr stack'empty?
	beq +
	jsr printtosln
	`drop
	bra _while
*
	`printcr
	lda _scratch
	sta _sp
	lda _scratch+1
	sta _sp+1
	rts
.scend

; duplicates the value at TOS on the stack.
stack'dup:
	lda #2
	jsr stack'allocate	; make room on the stack
	ldy #nos_lsb		; copy the lsb.
        lda (_sp),y
	ldy #tos_lsb
        sta (_sp),y
	ldy #nos_msb		; copy the msb.
        lda (_sp),y
	ldy #tos_msb
        sta (_sp),y
	rts

; fetch dereferences the current TOS and replaces the value on TOS.
stack'fetch:
	`peek _scratch
        lda (_scratch)		; LSB of address in memory
	ldy tos_lsb
	sta (_sp),y
	
	inc _scratch
        bne +
	inc _scratch+1
*
	lda (_scratch)		; MSB of address in memory
	ldy tos_msb
	sta (_sp),y
        rts

; stores the value in NOS at the address specified in TOS and drops
; the values from the stack.
stack'store:
	`peek _scratch
	`drop
	ldy tos_lsb
	lda (_sp),y
	sta (_scratch)

	inc _scratch
        bne +
	inc _scratch+1
*
	ldy tos_msb
	lda (_sp),y
        sta (_scratch)
	`drop
        rts

; swaps top of stack (TOS) to next on stack (NOS)
stack'swap:
	ldy #nos_lsb		; lsb of both works first
        lda (_sp),y
	sta _temp
	ldy #tos_lsb
	lda (_sp),y
	ldy #nos_lsb
        sta (_sp),y
	ldy #tos_lsb
	lda _temp
        sta (_sp),y

	ldy #nos_msb		; msb next
        lda (_sp),y
	sta _temp
	ldy #tos_msb
	lda (_sp),y
	ldy #nos_msb
        sta (_sp),y
	ldy #tos_msb
	lda _temp
        sta (_sp),y

        rts
.scend
