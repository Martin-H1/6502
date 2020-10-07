; -----------------------------------------------------------------------------
; ACIA init and I/O functions.
; Borrowed code from Rich Cini's SBC OS and made it generic for an ACIA at
; any address by using macros and dependency injection. The consumer defines
; ACIA1_BASE which compiles into this code when included in their project.
; Additional ACIA's can be defined using the macros and additional user
; defined functions in the consuming module.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

;
; Aliases
;

; I/O Locations for the 6551 ACIA chip registers.
.alias ACIA_DAT		$0
.alias ACIA_STA		$1
.alias ACIA_CMD		$2
.alias ACIA_CTL		$3

;
; Data segments
;

;
; Macros
;

; Set ACIA port properties.
.macro aciaInit
	lda #$1F			; 19.2K/8/1
	sta _1 + ACIA_CTL		; control reg
	lda #$0B			; N parity/echo off/rx int off/ dtr
	sta _1 + ACIA_CMD		; active low to command reg
.macend

;
; input chr from ACIA (blocking)
;
.macro aciaInput
_loop:	lda _1 + ACIA_STA		; Serial port status
	and #$08			; is recvr full
	beq _loop			; no char to get
	lda _1 + ACIA_DAT		; get chr
.macend

;
; input chr from ACIA (non-blocking) uses carry bit as flag
;
.macro aciaInputAsync
	clc
	lda _1 + ACIA_STA		; Serial port status
	and #$08			; mask rcvr full bit
	beq _exit
	lda _1 + ACIA_DAT		; get chr
	sec
_exit:
.macend

;
; output character in accumulator to port
;
.macro aciaOutput
	pha				; save registers
_loop:	lda _1 + ACIA_STA		; serial port status
	and #$10			; is tx buffer empty
	beq _loop			; no
	pla				; get chr
	sta _1 + ACIA_DAT		; put character to Port
.macend

;
; Functions
;

; 6551 I/O Support routines
acia1Init:
	`aciaInit ACIA1_BASE
	rts

acia1Input:
	`aciaInput ACIA1_BASE
	rts

acia1InputAsync:
	`aciaInputAsync ACIA1_BASE
	rts

acia1Output:
	`aciaOutput ACIA1_BASE
	rts
.scend
