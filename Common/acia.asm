; -----------------------------------------------------------------------------
; VIA init functions.
; Took code from Rich Cini's SBC OS and made it generic for an ACIA
; at any address.
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
.data ZPDATA

.data BSS
.space _ACIA_DAT 2		; address of registers
.space _ACIA_STA 2
.space _ACIA_CMD 2
.space _ACIA_CTL 2

.text

;
; Functions
;

;
; 6551 I/O Support Routines
;

; Init pointers to ACIA registers using the base passed on stack.
aciaInit:
	`dup			; dup the base offset of ACIA
	`pop _ACIA_DAT		; save the address of the dat register
	`inctos			; compute the address of the STA register
	`dup
	`pop _ACIA_STA		; save the address of the sta register
	`inctos			; compute the address of the CMD register
	`dup
	`pop _ACIA_CMD		; save the address of the cmd register
	`inctos			; compute the address of the CTL register
	`pop _ACIA_CTL		; save the address of the ctl register
	rts

; Set ACIA port properties.
; Note this uses the stack and indirect addressing to make it generic,
; but serial I/O is slow compared to this, so no big deal.
aciaPortset:
	`push _ACIA_CTL
	lda #$1f			; 19.2K/8/1
	sta (TOS_LSB,x)
	`put _ACIA_CMD
	lda #$0B			; N parity/echo off/rx int off/
	sta (TOS_LSB,X)			; dtr active low to command reg
	rts

;
; input chr from ACIA (blocking)
;
aciaInput:
	`push _ACIA_STA
*	lda (TOS_LSB,x)			; Serial port status
	and #$08			; is recvr full
	beq -				; no char to get
	`put _ACIA_DAT			; dat register offset.
	lda (TOS_LSB,x)			; get chr
	`drop
	rts

;
; input chr from ACIA (non-blocking)
;
aciaInputAsync:
	`push _ACIA_STA
	clc
	lda (TOS_LSB,x)			; Serial port status
	and #$08			; mask rcvr full bit
	beq +
	`put _ACIA_DAT
	lda (TOS_LSB,x)			; get chr
	sec
*	`drop
	rts

;
; output to OutPut Port
;
aciaOutput:
	pha			; save character in A register
	`push _ACIA_STA
*	lda (TOS_LSB,x)		; serial port status
	and #$10		; is tx buffer empty?
	beq -			; no
	`put _ACIA_DAT
	pla			; get chr
	sta (TOS_LSB,x)		; put character to Port
	`drop
	rts

.scend
