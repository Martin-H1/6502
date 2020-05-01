; -----------------------------------------------------------------------------
; Declaration of data segments and their origins used by the interpreter.
; For example zero page definitions require defining the program section
; origins before any use. So placing them here ensures no conflict
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.data ZPSTACK
.org $0000		; make stack at bottom of ZP.

.data ZPDATA
.org $008A		; start a few bytes after ZPSTACK as a guard.

.data BSS
.org $0300		; page 3 is used for uninitialized data.

.text
