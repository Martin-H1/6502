; -----------------------------------------------------------------------------
; sbc27 main module to allow for use on real hardware.
; -----------------------------------------------------------------------------

;
; Aliases
;

.alias ACIA1_BASE $7F70

;
; Data segments
;

;
; Macros
;

; 65c22 VIA 1 I/O Chip
.alias VIA1base  $7F50          ; VIA1 base address
.alias VIA1orb   VIA1base+0     ; Output register for Port B
.alias VIA1ora   VIA1base+1     ; Output register for Port A with handshake
.alias VIA1ddrb  VIA1base+2     ; Data direction register B
.alias VIA1ddra  VIA1base+3     ; Data direction register A
.alias VIA1rt1l  VIA1base+4     ; Read Timer 1 Counter lo-order byte
.alias VIA1rt1h  VIA1base+5     ; Read Timer 1 Counter hi-order byte
.alias VIA1at1l  VIA1base+6     ; Access Timer 1 Counter lo-order byte
.alias VIA1at1h  VIA1base+7     ; Access Timer 1 Counter hi-order byte
.alias VIA1rt2l  VIA1base+8     ; Read Timer 2 Counter lo-order byte
.alias VIA1rt2h  VIA1base+9     ; Read Timer 2 Counter hi-order byte
.alias VIA1ser   VIA1base+$A    ; Serial I/O shirt register
.alias VIA1acr   VIA1base+$B    ; Auxiliary Control Register
.alias VIA1pcr   VIA1base+$C    ; Peripheral control register
.alias VIA1ifr   VIA1base+$D    ; Interrupt flag register
.alias VIA1ier   VIA1base+$E    ; Interrupt enable register
.alias VIA1orah  VIA1base+$F    ; Output register for Port A without handshake

; 65c22 VIA 2 I/O Chip
.alias VIA2base  $7F60          ; VIA2 base address
.alias VIA2orb   VIA2base+0     ; Output register for Port B
.alias VIA2ora   VIA2base+1     ; Output register for Port A with handshake
.alias VIA2ddrb  VIA2base+2     ; Data direction register B
.alias VIA2ddra  VIA2base+3     ; Data direction register A
.alias VIA2rt1l  VIA2base+4     ; Read Timer 1 Counter lo-order byte
.alias VIA2rt1h  VIA2base+5     ; Read Timer 1 Counter hi-order byte
.alias VIA2at1l  VIA2base+6     ; Access Timer 1 Counter lo-order byte
.alias VIA2at1h  VIA2base+7     ; Access Timer 1 Counter hi-order byte
.alias VIA2rt2l  VIA2base+8     ; Read Timer 2 Counter lo-order byte
.alias VIA2rt2h  VIA2base+9     ; Read Timer 2 Counter hi-order byte
.alias VIA2ser   VIA2base+$A    ; Serial I/O shirt register
.alias VIA2acr   VIA2base+$B    ; Auxiliary Control Register
.alias VIA2pcr   VIA2base+$C    ; Peripheral control register
.alias VIA2ifr   VIA2base+$D    ; Interrupt flag register
.alias VIA2ier   VIA2base+$E    ; Interrupt enable register
.alias VIA2orah  VIA2base+$F    ; Output register for Port A without handshake

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

_ioinit:
	jmp acia1Init

_getch:
	jmp acia1Input

; Write the character in the accumulator to the ACIA.
_putch:
	jmp acia1Output

.require "vectors.asm"





