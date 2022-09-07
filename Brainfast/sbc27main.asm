; -----------------------------------------------------------------------------
; sbc27 main module to allow for use on real hardware.
; -----------------------------------------------------------------------------

;
; Aliases
;
.alias ACIA1_BASE	$7F70
.alias VIA1_BASE	$7f50
.alias VIA2_BASE	$7f60

;
; Data segments
;

;
; Macros
;

;
; Data segments
;

;
; Macros
;

.org $8000
.outfile "sbc27.rom"
.advance $8000

; Functions
;
.require "data.asm"
.require "brainfast.asm"
.require "conio.asm"
.require "acia.asm"
.require "via.asm"

_ioinit:
	jsr acia1Init
	jsr via1Init
	jmp via2Init

_getch:
	jmp acia1Input

; Write the character in the accumulator to the ACIA.
_putch:
	jmp acia1Output

.require "vectors.asm"
