; -----------------------------------------------------------------------------
; Main body of the monitor. It will depend upon the stack, list, and I/O
; functions defined in the other modules.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;

;
; Data segments
;
.data BSS
	
.text

;
; Macros
;

;
; Functions
;

; Main entry point for the monitor.
monitorInit:
	rts

.scend
