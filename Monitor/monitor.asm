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
.data MONDATA
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
; It installs the default interupt handlers.
monitorInit:
	lda #<monitorBreak
	sta INTvector
	lda #>monitorBreak
	sta INTvector+1

	lda #<monitorNmi
	sta NMIvector
	lda #>monitorNmi
	sta NMIvector+1

	lda #$ff
	jsr biosSetEcho

	cli
	rts

monitorMain:
	ldx #$ff
	txs
	lda #AscGT
	jsr biosPutch
	lda #AscSP
	jsr biosPutch
	jsr biosGetch
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
	jsr biosBell		; Beep speaker
	jsr _printRegln		; dump register contents
	ldx #$FF
	txs			; clear stack
	cli			; enable interrupts again
	jmp monitorMain		; start the monitor

; function to print CR, register contents, and crlf.
_printCRRegln:
	jsr biosCRLF		; Lead with a CR

; function to print register contents, and crlf.
_printRegln:
.scope
	ldx #$ff
	ldy #$ff
_loop:
	iny
	lda _regDataMsg,y
	jsr biosPutch
	cmp #$3D			; "="
	bne _loop
*	inx
	cpx #$07
	beq Printreg3			; done with first 6
	lda _PCH,x
	jsr biosPutHex
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
*	jsr biosPutch
	tya
	dex
	bne Printreg4
	jsr biosCRLF
	rts

_regDataMsg:
	.byte" PC=  A=  X=  Y=  S=  P= (NVRBDIZC)="
.scend

;
; NMI handler stub
;
monitorNmi:
	rti

monitorHelp:
.scope
	jsr biosCPuts
_helpText:
	.byte AscCR,AscLF,"Current commands are :",AscCR,AscLF
	.byte "Syntax = {} required, [] optional, HHHH hex address, DD hex data"
	.byte AscCR,AscLF,AscCR,AscLF
	.byte "[HHHH][ HHHH]{Return} - Hex dump address(s)(up to 16 if no address entered)",AscCR,AscLF
	.byte "[HHHH]{.HHHH}{Return} - Hex dump range of addresses (16 per line)",AscCR,AscLF
	.byte "[HHHH]{:DD}[ DD]{Return} - Change data bytes",AscCR,AscLF
	.byte "[HHHH]{G}{Return} - Execute a program (use RTS to return to monitor)",AscCR,AscLF
	.byte "{HHHH.HHHH>HHHH{I}{Return} - move range at 2nd HHHH down to 1st to 3rd HHHH",AscCR,AscLF
	.byte "[HHHH]{L}{Return} - List (disassemble) 20 lines of program",AscCR,AscLF
	.byte "[HHHH]{.HHHH}{L}{Return} - Dissassemble a range",AscCR,AscLF
	.byte "{HHHH.HHHH>HHHH{M}{Return} - Move range at 1st HHHH thru 2nd to 3rd HHHH",AscCR,AscLF
	.byte "[HHHH][ HHHH]{Q}{Return} - Text dump address(s)",AscCR,AscLF
	.byte "[HHHH]{.HHHH}{Q}{Return} - Text dump range of addresses (16 per line)",AscCR,AscLF
	.byte "{R}{Return} - Print register contents from memory locations",AscCR,AscLF
	.byte "{U}{Return} - Upload File (Xmodem/CRC or Intel Hex)",AscCR,AscLF
	.byte "{V}{Return} - Monitor Version",AscCR,AscLF
	.byte "{HHHH.HHHH>HHHH{W}{Return} - Write data in RAM to EEPROM",AscCR,AscLF
	.byte "{!}{Return} - Enter Assembler",AscCR,AscLF
	.byte "{?}{Return} - Print menu of commands",AscCR,AscLF, $00
	rts
.scend

.scend
