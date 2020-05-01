;*********************************************************************
;  2014/02/04	RAC	Created
; ----------------- assembly instructions ---------------------------- 
;
;****************************************************************************
; NMI Vector entry
;	Very simple at this point but other handling for VIA1 interrupts
;	can go here.
;****************************************************************************
NMIVEC				
	PHA			; save regs
	TXA			
	PHA			
	TYA			
	PHA
;	CLD			; some docs show that you should CLD inside the NMI	

	LDA	Via1PRA		; re-arm PANIC interrupt by reading port A
	
	PLA			; restore registers
	TAY			
	PLA			
	TAX			
	PLA			
	CLI			; re-enable interrupts
	JMP	Brk2		; part of BRK handler

AA_end_nmi
