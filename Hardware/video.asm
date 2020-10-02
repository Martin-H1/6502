; -----------------------------------------------------------------------------
; Driver for Daryl Richtor's AVR video.
; Original code from Rich Cini's SBC2.7 OS
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;
.alias _VIA2DDRB	VIA2_BASE + VIA_DDRB
.alias _VIA2ACR		VIA2_BASE + VIA_ACR
.alias _VIA2IER		VIA2_BASE + VIA_IER
.alias _VIA2PRB		VIA2_BASE + VIA_PRB
.alias _VIA2SR		VIA2_BASE + VIA_SR

;----------------------------------------------------------------------
; Call this once to initialize the interface
; it sets up Port B, pin 7 and CB1/CB2 for serial mode
; A is changed and Flags are changed.
;----------------------------------------------------------------------
videoInit:
	sei			; disable interrupts
	lda _VIA2DDRB		; get ddr b
	and #$7F		; force bit 7=0
	sta _VIA2DDRB		; set bit 7 to input
	lda _VIA2ACR		; get ACR contents
	and #$E3		; mask bits 2,3,4
	ora #$18		; set Shift out under control of PHI2 mode
	sta _VIA2ACR		; store to acr
	lda #$04		; shift register flag in IER
	sta _VIA2IER		; disable shift register interrupts
	cli			; Enable Interrupts again
	rts

;
; Output contents of A to the Video Display
;  A is preserved, Flags are changed.
;
videoOutput:
	bit _VIA2PRB		;  read handshake byte (pb7)
	bmi videoOutput		;  if pb7=1, wait for AVR to be ready
	sta _VIA2SR		; send to display via shift register
*
	bit _VIA2PRB		; read handshake byte
	bpl -			; if pb7=0, wait for AVR to ack
	rts

.scend
