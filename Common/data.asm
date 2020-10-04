; -----------------------------------------------------------------------------
; Declaration of common aliases and data segments. This removes cross module
; dependencies by moving these global definitions to a common place.
; For example zero page definitions require defining the program section
; origins before any use. So placing them here ensures no conflict
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

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
.alias AscFF	$0C	; Formfeed ASCII character
.alias AscLF	$0A	; line feed ASCII character
.alias AscSP	$20	; space ASCII character
.alias AscTAB	$09	; tab ASCII character
.alias AscVT	$0B	; vertical tab ASCII character

;
; Data segments
;

.data ZPSTACK
.org $0000		; make stack at bottom of ZP.

.data ZPDATA
.org $008A		; start a few bytes after ZPSTACK as a guard.

.data BSS
.org $0300		; page 3 is used for uninitialized data.

.text
