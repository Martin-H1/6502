; -----------------------------------------------------------------------------
; VIA init functions.
; Took code from Rich Cini's SBC OS and made it generic.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

;
; Aliases
;

; I/O Locations for the 6522 VIA chip
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
; Functions
;

;
; 6522 VIA I/O Support Routines
;
viaInit:
	`pushi _viaInitData
	`swap
	`pushi $0e
	jsr memcpy
	rts

_viaInitData:
	.byte $00		; prb  '00000000'
	.byte $00		; pra  "00000000'
	.byte $00		; ddrb 'iiiiiiii'
	.byte $00		; ddra 'iiiiiiii'
	.byte $00		; tacl  
	.byte $00		; tach  
	.byte $00		; tall  
	.byte $00		; talh  
	.byte $00		; t2cl
	.byte $00		; t2ch
	.byte $00		; sr
	.byte $00		; acr
	.byte $00		; pcr
	.byte $7f		; ifr
	.byte $7f		; ier
.scend
