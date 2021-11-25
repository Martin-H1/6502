; -----------------------------------------------------------------------------
; Definitions for the py65mon emulator
; -----------------------------------------------------------------------------
.alias py65_putc    $f001
.alias py65_getc    $f004

;
; Data segments
;

.text
mockBiosInit:
.scope
	`callBiosInit getch_impl, putch_impl
	rts
.scend

getch_impl:
.scope
*	lda py65_getc
	beq -
	rts
.scend

.scope
;	`push GETCHPTR
; 	`peek DBGPTR
;	`inctos
; 	`pop GETCHPTR
;	lda (DBGPTR)
	rts
.scend

putch_impl:
.scope
	sta py65_putc
	rts
.scend
