;*********************************************************************
; ----------------- assembly instructions ---------------------------- 
;
; This is a subroutine library only and must be included in an
; executable source file.
;
; 2003/01/27  RAC  Modified from original with proper starting values
;		    for use with the Serial IEC
; 2005/09/04  RAC  Synched with SBCOS 5/30/05 release
; 2007/07/26  RAC  Changes to intialization values
;
;*** I/O Locations *******************************
; define the i/o address of the Via1 chip
;*** 6522 VIA ************************
Via1Base       =     $7f50
Via1PRB        =     $7f50
Via1PRA        =     $7f51
Via1DDRB       =     $7f52
Via1DDRA       =     $7f53
Via1T1CL       =     $7f54
Via1T1CH       =     $7f55
Via1T1LL       =     $7f56
Via1TALH       =     $7f57
Via1T2CL       =     $7f58
Via1T2CH       =     $7f59
Via1SR         =     $7f5a
Via1ACR        =     $7f5b
Via1PCR        =     $7f5c
Via1IFR        =     $7f5d
Via1IER        =     $7f5e
Via1PRA1       =     $7f5f
;
;***********************************************************************
; 6522 VIA I/O Support Routines
;
Via1_init	LDA   #%01111111	;$7F
		STA   Via1IER		; clear and disable all VIA interrupt sources

		LDA   #%01000000	;$40 T1 free-running (PB7 output disabled)/T2
		sta   Via1ACR		;interval counter/shift registers disabled/
					;port A latch disabled/port B latch disabled

	       	LDA   #%11011110	;$DE CB2 manual L/ CB1 IF set on L->H/ CA2 manual
	    	STA   Via1PCR		;H/ CA1 IF set on H->L

	       	LDX   #$00		;$00 'iiiiiiii'	
	    	STX   Via1DDRB		; save to DDRB
	    	stx   Via1IFR		; clear IFR

	     	LDX   #%10000000	;$80 'oiiiiiii' PA.7=ATN_OUT
	    	STX   Via1DDRA		; save to DDRA

; 2007/07/26 - VIC20 Kernal ROM does not initialize default values for output regs
;		ldx   #$00
;		stx   Via1PRA1		; clear PA output register

		jsr   SCLK1		; set IEC clock line
		lda   #%10000010	; enable CA1 interrupt (RESTOR key)
		sta   Via1IER		; save to IER
		jsr   SCLK0		; reset IEC clock line
		rts                     ; done

AA_end_via1
