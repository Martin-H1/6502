; -----------------------------------------------------------------------------
; Header for BIOS module which allow programs to call monitor functions via a
; jump table in high ROM. It is losely based on the interface of the MSDOS BIOS,
; SBC OS 2.7, and the APATCO ROM.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

; establish module level scope to hide module locals.
.scope

;
; Aliases
;
.alias biosInit		$FF00		; Initializes I/O vectors and TIB state.
.alias biosBell		$FF03		; Outputs a re ctrl-g to terminal.
.alias biosCLS		$FF06		; Clears the terminal (if supported).
.alias biosCRLF		$FF09		; Go to the next line on terminal.
.alias biosCgets	$FF0C		; Fills the input buffer from terminal.
.alias biosCputs	$FF0F		; Ouputs a null terminated string.
.alias biosGetch	$FF12		; Gets a character from TIB into A.
.alias biosHome		$FF15		; Returns terminal cursor to top left.
.alias biosMonitor	$FF18		; Exits program and enters monitor.
.alias biosPutHex	$FF1B		; Prints A to the terminal as hex.
.alias biosPutch	$FF1E		; Prints A to the terminal as ASCII.
.alias biosSetCursor	$FF21		; Positions cursor to row and column.
.alias biosSetEcho	$FF24		; Turns terminal keyboard echo on/off.
.alias biosUngetch	$FF27		; Returns A to the TIB.
.alias biosWarmBoot	$FF2A		; Calls the reset vetor to reboot.

;
; consider adding multiply and divide routines
;

;
; Data segments
;

;
; Macros
;
.macro callBiosInit
	jsr biosInit
	.word _1, _2
.macend

;
; Functions
;

.scend
