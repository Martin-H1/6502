; -----------------------------------------------------------------------------
; conio functions use indirection to allow platform specific implementations.
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
.data ZPDATA
.space _tmpPtr 2
.space _save 2

.text

;
; Macros
;

.macro print
	lda #>_1		; Let's bury this in a macro
	pha			; I might have a better idea
	lda #<_1		; and I only need to change it here.
	pha
	jsr cputs
.macend

;
; Functions
;

; cputs is like the MSDOS console I/O function. It prints a null terminated
; string to the console using _putch.
; input - string address on return stack.
; output - text to console and string address is consumed.
cputs:
.scope
	pla			; Pass pointer via return stack jiggery pokery
	sta _save		; pull the return address off the stack.
	pla			; and save it for the momment.
	sta _save+1

	pla			; Now pull the argument pointer.
	sta _tmpPtr
	pla
	sta _tmpPtr+1

	lda _save+1		; Retore the return address
	pha
	lda _save
	pha

_loop:	lda (_tmpPtr)		; get the string via address from zero page
	beq _exit		; if it is a zero, we quit and leave
	jsr _putch		; if not, write one character
	inc _tmpPtr		; get the next byte
	bne _loop
	inc _tmpPtr+1
	bra _loop
_exit:	rts
.scend

.scend
