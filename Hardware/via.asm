; -----------------------------------------------------------------------------
; VIA init functions.
; Took code from Rich Cini's SBC OS and made it generic using macros and
; compile time dependency injection. Since most systems have two VIA's, I
; create two init routines.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

;
; Aliases
;

; I/O Locations for the 6522 VIA chip registers
.alias VIA_PRB		$00
.alias VIA_PRA		$01
.alias VIA_DDRB		$02
.alias VIA_DDRA		$03
.alias VIA_T1CL		$04
.alias VIA_T1CH		$05
.alias VIA_T1LL		$06
.alias VIA_TALH		$07
.alias VIA_T2CL		$08
.alias VIA_T2CH		$09
.alias VIA_SR		$0a
.alias VIA_ACR		$0b
.alias VIA_PCR		$0c
.alias VIA_IFR		$0d
.alias VIA_IER		$0e
.alias VIA_PRA1		$0f

;
; Data segments
;
.data ZPDATA

.data BSS

.text

;
; Macros
;
.macro viaInit
	lda #00
	ldy #VIA_PCR		; zero out lower regsiters
_loop:	sta _1,y
	dey
	bpl _loop 
	lda #$7f		; init two upper registers.
	sta _1 + VIA_IFR
	sta _1 + VIA_IER
.macend

;
; Functions
;
via1Init:
	`viaInit VIA1_BASE
	rts

via2Init:
	`viaInit VIA2_BASE
	rts
.scend
