; -----------------------------------------------------------------------------
; Test for sbc 2.7 onboard video. Must be run on real hardware.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.org $8000
.outfile "tests/viaTest.rom"

.alias RamSize   $7EFF		; default $8000 for 32 kb x 8 bit RAM

.require "../../Common/data.asm"

.advance $c000

.require "../sbc27io.asm"
.require "../via.asm"

; Main entry point for the test
main:
	jsr via1Init
	jsr via2Init
	jsr via2_test
	brk

.scope
; Set VIA2 Port B bit 0 to output and toggle bits.
via2_test:
	ldy #$16
_loop:	lda VIA2_BASE + VIA_DDRB
	ora #$01
	sta VIA2_BASE + VIA_DDRB
	lda VIA2_BASE + VIA_PRB
	eor #$01
	sta VIA2_BASE + VIA_PRB
	jsr _delay
	dey
	bpl _loop
	rts
_delay:	phy
	ldy #$00
	phx
	ldx #$00
*	dex
	bne -
	dey
	bne -
	plx
	ply
	rts
.scend

.require "../../Common/vectors.asm"
