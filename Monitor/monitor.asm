; -----------------------------------------------------------------------------
; Main body of the monitor, losely based on a reverse engineering of Daryl
; Rictor's SBC OS. It acts as a simple REPL and file transfer program. It
; will depend upon the stack, list, and I/O functions defined in the other
; modules.
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
.space _PCH 1
.space _PCL 1
.space _ACC 1
.space _XREG 1
.space _YREG 1
.space _SPTR 1
.space _PREG 1
.text

;
; Macros
;

;
; Functions
;

; Main entry point for the monitor.
monitorInit:
	`pushi monitorBreak
	`pop INTvector
	cli
	rts

monitorCold:
	rts

monitorWarm:
	rts

monitorInput:
	rts

; Break instruction handler.
; Saves machine state and enters monitor.
monitorBreak:
	sta _ACC		; save A
	stx _Xreg		; save X
	sty _Yreg		; save Y
        pla
	sta _Preg		; save P
	pla			; PCL
	plx			; PCH
	sec
	sbc #$02
	sta _PCL		; backup to BRK cmd
	bcs Brk2
	dex
Brk2:	stx _PCH		; save PC
	tsx			; get stack pointer
	stx _SPtr		; save stack pointer
	lda #AscBell
	jsr putch		; Beep speaker
	jsr _printRegln		; dump register contents
	ldx #$FF
	txs			; clear stack
	cli			; enable interrupts again
	jmp monitorWarm		; start the monitor

; Delay loop
_delay:
	rts

; function to print CR, register contents, and crlf.
_printCRRegln:
	`printcr			; Lead with a CR

; function to print register contents, and crlf.
_printRegln:
.scope
	ldx #$ff
	ldy #$ff
_loop:
	iny
	lda _regDataMsg,y
	jsr putch
	cmp #$3D			; "="
	bne _loop
*	inx
	cpx #$07
	beq Printreg3			; done with first 6
	lda _PCH,x
	jsr printa
	cpx #$00
	bne _loop
	bra -
Printreg3:
	dex
	lda _PCH,x			; get Preg
	ldx #$08
Printreg4:
	rol
	tay
	lda #$31
	bcs +
	dec
*	jsr putch
	tya
	dex
	bne Printreg4
	`printcr
	rts

_regDataMsg:
	.byte" PC=  A=  X=  Y=  S=  P= (NVRBDIZC)="
.scend

.scend
