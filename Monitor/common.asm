; -----------------------------------------------------------------------------
; Common module to define monitor aliases, data segments and macros.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;

; Character set (ASCII)
.alias AscBell	$07	; bell (Control-G) ASCII character
.alias AscBS	$08	; backspace ASCII character
.alias AscCC	$03	; break (Control-C) ASCII character
.alias AscCR	$0D	; carriage return ASCII character
.alias AscDEL	$7F	; DEL ASCII character
.alias AscESC	$1B	; Escape ASCII character
.alias AscFF	$0C	; Form feed ASCII character
.alias AscGT	$3E	; Greater than ASCII character
.alias AscLF	$0A	; line feed ASCII character
.alias AscSP	$20	; space ASCII character
.alias AscTAB	$09	; tab ASCII character
.alias AscVT	$0B	; vertical tab ASCII character

;
; consider adding multiply and divide routines
;

;
; Data segments
;

.data MONDATA
.org $0200

.text

;
; Macros
;

;
; Functions
;

.scend
