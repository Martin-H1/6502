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
.space PCH 1
.space PCL 1
.space ACC 1
.space XREG 1
.space YREG 1
.space SPTR 1
.space PREG 1
.text

;
; Macros
;

;
; Functions
;

; Main entry point for the monitor.
monitorInit:
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
	sta ACC			; save A
	stx Xreg		; save X
	sty Yreg		; save Y
        pla
	sta Preg		; save P
	pla			; PCL
	plx			; PCH
	sec
	sbc #$02
	sta PCL			; backup to BRK cmd
	bcs Brk2
	dex
Brk2:	stx PCH			; save PC
	tsx			; get stack pointer
	stx SPtr		; save stack pointer
	lda #AscBell
	jsr putch		; Beep speaker
	jsr PrintReg		; dump register contents
	ldx #$FF
	txs			; clear stack
	cli			; enable interrupts again
	jmp Monitor		; start the monitor

; Delay loop
_delay:
	rts

; function to print CR, register contents, and crlf.
_printCRRegln:

; function to print register contents, and crlf.
_printRegln:
	rts
.scend
