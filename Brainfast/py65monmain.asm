; -----------------------------------------------------------------------------
; Py65Mon main module to allow for testing under emulation.
;
; -----------------------------------------------------------------------------

;
; Aliases
;

;
; Data segments
;

;
; Macros
;

.org $8000
.outfile "py65mon.rom"
.advance $8000

;
; Functions
;
.require "data.asm"
.require "brainfast.asm"
.require "conio.asm"

; conio core functions unique to each platform.
.alias _py65_putc	$f001	; Definitions for the py65mon emulator
.alias _py65_getc	$f004

_getch:
.scope
*	lda _py65_getc
	beq -
	cmp #13		; convert CR to LF so as to be compliant on Windows
	bne +
	lda #10
*	rts
.scend

_putch:
.scope
	sta _py65_putc
	rts
.scend

.require "vectors.asm"
