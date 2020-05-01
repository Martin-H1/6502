; Common alias and macros go in this file.
; For example zero page definitions require coodination, so placing
; them here ensures no conflict
.alias CELL $02			; Size of word and cell on the stack.
.alias NULL $00			; Null used to terminate linked lists.

.alias STRNGPTR	$FE		; pointer used by print and string functions.
.alias TMPPTR1	$FC		; working pointer used by heap functions.
.alias TMPPTR2	$FA		; working pointer used by heap functions.
.alias INTERNS	$F8		; pointer to the list of symbols
.alias HEAPPTR	$F6		; pointer to heap area
.alias GLOBALS	$F4		; pointer to the global scope list.
.alias FPAREA	$D9		; 25 bytes of floating point work space.
.alias lastChar	$D8		; the character.
.alias ungetFlg	$D7		; flag indicating a unget char is available
.alias SPMAX    $00		; top of parameter (data) stack
.alias SP0      $7F		; bottom of parameter (data) stack

; Character set (ASCII) 
.alias AscBS  $08               ; backspace ASCII character
.alias AscCC  $03               ; break (Control-C) ASCII character
.alias AscCR  $0D               ; carriage return ASCII character
.alias AscDEL $7F               ; DEL ASCII character
.alias AscESC $1B               ; Escape ASCII character
.alias AscLF  $0A               ; line feed ASCII character
.alias AscSP  $20               ; space ASCII character

.macro incw
        inc _1
        bne _over
        inc _1+1
_over:
.macend

.macro if_less16
	jsr compare16

	; for signed numbers, NOS < TOS if Z=0 and N=0
	beq _else
	bmi _else
.macend

.macro if_greater16
	jsr compare16

	; for signed numbers, NOS > TOS if Z=0 and N=1
	beq _else
	bmi _else
.macend

.macro if_equals16
	jsr equals16
	bne _else
.macend
