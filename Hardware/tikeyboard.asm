; -----------------------------------------------------------------------------
; Driver for TI 99/4a keyboard. This 80's era keyboard has an 8x8 matrix and
; new old stock is ocasionally available on eBay. This code originated from
; Simon Jansen's Orwell computer project and he graciously gave me his code.
; Martin Heermance <mheermance@gmail.com>
; -----------------------------------------------------------------------------

.scope

;
; Aliases
;
.alias _viaOutDdr 0
.alias _viaInDdr 0
.alias _viaOut 0
.alias _viaIn 0

;
; Data segments
;
.data ZPDATA

.data BSS
.space _keyBuffer 1		; Keyboard port buffer.
.space _keyCol 1		; Current keyboard scan column.
.space _keyCounter 1		; Key debounce counter.
.space _keyData 1		; Debounced key value available for users.
.space _keyLastKey 1		; Last value of key pressed
.space _keyPressed 1		; Key pressed (as found in the lookup table).
.space _keyRow 1		; Current keyboard scan row.
.space _keyStatus 1		; bit 0 = funct, 1 = ctrl, 2 = shift, 3 = lock

.text

;
; Functions
;

keyboardInit:
	lda #$00		; Load ACC with b00000000.
	sta _viaInDdr		; Set VIA port to all input.
	lda #$FF		; Load ACC with b11111111.
	sta _viaOutDdr		; Set VIA port to all output.
				; Keyboard reading initialisation.
	lda #$19		; Set the debounce counter.
	sta _keyCounter		; And store it.
	lda #$00		; Zero out all static data.
	sta _keyLastKey		; Set the last key pressed value.
	sta _keyData		; Set the data buffer
	sta _keyPressed		; Set the key_pressed buffer.
	sta _keyStatus		; Set the status.
	rts

;
; Keyboard routine. This routine will set the data available flag in the
; _keyStatus register and return data in the _keyData variable when valid data
; is available from the keyboard. This routine scans the matrix by calling
; the matrix_scan routine then handles debouncing of the value.
;
keyboardScan:
	txa			; Preserve X.
	pha
	tya			; Preserve Y.
	pha
	php			; Preserve status register.
	jsr _matrixScan		; Scan the matrix.
	lda _keyPressed		; Compare detected key to the last key pressed.
	cmp _keyLastKey		; Is this key different?
	bne _keyChange		; The keys are different so it's changing
				; It's the same.
	lda _keyCounter		; If the counter is already at zero this is the same keypress.
	beq _KeyScanDone	; If it's zero we don't do anything.
	dec _keyCounter		; Otherwise decrement the debounce counter.
	beq _keyDebounced	; If it's zero now we have a valid key reading.
	jmp _keyScanDone

_keyChange:
	lda #19			; Reset the counter.
	sta _keyCounter		; And store it.
	lda _keyPressed		; Update the last key pressed value with this new value.
	sta _keyLastKey
	jmp _keyScanDone

_keyDebounced:			; A valid key reading was detected.
	lda _keyPressed		; Load up the key pressed.
	beq _keyScanDone	; If it was zero (no key pressed) we're done.
	sta _keyData		; Make it available now in the data.
	lda #$80		; Set the new data available bit.
	ora _keyStatus
	sta _keyStatus		; Store it in the status.

_keyScanDone:
	plp			; Restore status.
	pla			; Restore Y.
	tay
	pla			; Restore X.
	tax
	rts

; =============================================================================
; Keyboard matrix scan routine. The key_status (bit 6) will indicate if there
; is a key currently pressed.  It also indicated if the function, control,
; shift or alpha lock were on (bits 0, 1, 2 and 3)
; There are 7 rows (0-6) and 8 columns.
; Shift, control and function are on row 0. Alpha is on row 6. We handle
; these as special cases.
; The pressed key is returned in _keyPressed.
; =============================================================================
_matrixScan:
	lda #$00		; Load ACC with 0.
	sta _keyBuffer		; Clear the keyboard character buffer.
	sta _keyPressed		; Clear the key pressed buffer.
	lda #$80		; Load ACC with 10000000.
	and _keyStatus		; Clear the bottom 7 bits of the status.
	sta _keyStatus		; Store it in the status.

_matrixScanSpecial:
	ldx #$00		; Read row 0. Row zero is a special case as
				; it has the shift, control and function keys.
	txa			; Move the row counter to the accumulator.
	sta _viaOut		; Push it to the output VIA to select the row.
	lda _viaIn		; Read VIA input for column data.
	eor #$FF		; XOR to get set bits (keys active low).
	beq _matrixScanAlpha	; If zero no keys are being pressed on this row.
                                ; Just go straight to the next row.
	sta _keyBuffer		; Some key(s) pressed. Store the port in our temp holder.
	lda #$07		; Check if the special keys are pressed.
				; Set up mask for the special keys.
	and _keyBuffer
	ora _keyStatus		; Now set those bits in the status register.
	sta _keyStatus		; Store it in the status.
	lda #$E0		; Now mask off the normal keys and check them.
	and _keyBuffer
	sta _keyBuffer		; Store it in the buffer.

_matrixScanAlpha:
	ldx #$06		; The alpha key on row 6 is also a special case.
	txa			; Move the row counter to the accumulator.
	sta _viaOut		; Push it to the output VIA to select the row.
	lda _viaIn		; Read VIA input for column data.
	eor #$FF		; XOR to get set bits (keys active low).
	beq _matrixScanRows	; If this is non zero the alpha key is pressed.
	lda #$08		; Set the alpha bit.
	ora _keyStatus
	sta _keyStatus		; Store it in the status.

_matrixScanRows:
	ldx #$00		; Reset X back to the first row.
	ldy #$00		; Use Y as the column counter.

_matrixScanCol:
	lda #$80		; Load mask to check the data bit of the buffer.
	and _keyBuffer		; is set to see if key was pressed.
	beq _matrixScanColNext	; Branch if zero since that key wasn't pressed.
	lda #$40		; It's set. Check if we already have a key
				; pressed. Test bit 6 of the keyboard status.
	and _keyStatus		; AND with the keyboard status to test the bit.
	beq _matrixScanColData	; If zero, it's not set, process this new key.

_matrixScanInvalid:		; It's already set so we have an invalid state.
	lda #$BF		; Clear the key pressed flag.
	and _keyStatus
	sta _keyStatus		; Set the status.
	jmp _matrixScanDone	; Now finish since it was invalid.

_matrixScanColData:
	lda #$40		; Set the key pressed bit.
	ora _keyStatus
	sta _keyStatus		; Store it in the status.
	stx _keyRow		; Remember the row.
	sty _keyCol		; Remember the column.

_matrixScanColNext:
	clc			; Clear the carry flag.
	rol _keyBuffer		; Rotate buffer left to check the next column.
	iny			; Increment the column count.
	cpy #$08		; Have we done all eight columns?
	bne _matrixScanCol	; Zero set if equal when we've done all eight.

_matrixScanRowNext:
	inx			; Go onto the next row.
	cpx #$06		; Have we done all remaining six rows?
	beq _matrixScanRowsDone
	txa			; No, move row counter to ACC and read next row.
	sta _viaOut		; Push it to the output VIA to select the row.
	lda _viaIn		; Read VIA input for column data.
	eor #$FF		; XOR to get set bits (keys active low).
	beq _matrixScanRowNext	; If zero no keys are being pressed on this row.
                                ; Just go straight to the next row.
	sta _keyBuffer		; Some key(s) pressed. Store in our buffer.
	ldy #$00		; Use Y as the column counter.
	jmp _matrixScanCol	; Check the columns for this row.

_matrixScanRowsDone:
	lda #$40		; Check if the data flag is set.
	and _keyStatus		; AND with the keyboard status to test the bit.
	beq _matrixScanDone	; If zero we have no keys pressed so can finish.

_matrixScanShift:		; Add to row counter for special keys pressed.
	lda #$04		; Check shift first.
	bit _keyStatus		; Bit three in _keyStatus.
	beq _matrixScanCtrl	; Shift not pressed so test control next.
	lda #$06		; Shift was pressed so add 6 to the row count.
	clc			; Clear carry flag.
	adc _keyRow		; Add to the row count.
	sta _keyRow		; Store the result.
	jmp _matrixScanLookup	; Now go look up the key.

_matrixScanCtrl:
	lda #$02		; Check control next.
	bit _keyStatus		; Bit 2 in _keyStatus.
	beq _matrixScanFctn	; Control not pressed so test function next.
	lda #$0C		; Control was pressed so add 12 to row count.
	clc
	adc _keyRow		; Add to the row count.
	sta _keyRow		; Store the result.
	jmp _matrixScanLookup	; Now go look up the key.

_matrixScanFctn:
	lda  #$01		; Finally check function.
	bit _keyStatus		; Bit one in _keyStatus.
	beq _matrixScanLookup	; Function not pressed so go lookup the key.
	lda #$12		; Function was pressed so add 18 to row count.
	clc			; Clear carry flag.
	adc  _keyRow		; Add to the row count.
	sta  _keyRow		; Store the result.
		
_matrixScanLookup:		; Now lookup the actual character.
	lda _keyRow		; Get the row.
	clc			; Clear the carry flag.
	rol			; Multiple the row counter by 8 (by using rol).
	clc
	rol			; x4
	clc
	rol			; x8
	clc
	adc _keyCol		; Add the column to get offset into the table.
	tax			; Transfer it into X.
	lda _keyboardMap,x	; Use as offset into table. Add offset to
				; table address to convert scan code into
				; ASCII character in the accumulator.
	sta _keyPressed		; Store the new character.

	lda #$08		; Check if Alpha is on.
	bit _keyStatus		; Bit four in _keyStatus.
	beq _matrixScanDone	; Alpha not pressed so just store the key.
				; $61 = a .. $7A = z.

	lda _keyPressed
	cmp #$61		; Alpha pressed, upper case lower case letters.
	bcc _matrixScanDone	; Less than $61.

	cmp #$7B
	bcs _matrixScanDone	; More than than $7A.

	and #$DF		; Make upper case.
	sta _keyPressed		; Store the new character.

_matrixScanDone:
	lda #$00		; Reset the VIA port to all zero.
	sta _viaOut
	rts

; =============================================================================
; Keyboard map.
; The keyboard is 48 keys mapped as 7 rows by 8 columns.
; There are four parts to the table for normal and shifted keys, control
; and function keys.
; =============================================================================

_keyboardMap:
_keyboardMapRowN0: .BYTE "=", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
_keyboardMapRowN1: .BYTE "nhy6bgt5"
_keyboardMapRowN2: .BYTE "mju7vfr4"
_keyboardMapRowN3: .BYTE ",ki8cde3"
_keyboardMapRowN4: .BYTE ".lo9xsw2"
_keyboardMapRowN5: .BYTE "/;p0zaq1"
_keyboardMapRowS0: .BYTE "+", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
_keyboardMapRowS1: .BYTE "NHY^BGT%"
_keyboardMapRowS2: .BYTE "MJU&VFR$"
_keyboardMapRowS3: .BYTE "<KI*CDE#"
_keyboardMapRowS4: .BYTE ">LO(XSW@"
_keyboardMapRowS5: .BYTE "-:P)ZAQ!"
_keyboardMapRowC0: .BYTE "=", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
_keyboardMapRowC1: .BYTE "nhy6b", $07, "t5"           ;$07 = CTRL-G (bell)
_keyboardMapRowC2: .BYTE "mju7vfr4"
_keyboardMapRowC3: .BYTE ",ki8", $03, "de3"           ;$03 = CTRL-C
_keyboardMapRowC4: .BYTE ".lo9xsw2"
_keyboardMapRowC5: .BYTE "/;p0zaq1"
_keyboardMapRowF0: .BYTE "=", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
_keyboardMapRowF1: .BYTE "nhy6b}]5"
_keyboardMapRowF2: .BYTE "mj_7v{[4"
_keyboardMapRowF3: .BYTE ",k?8`", $09, $1B, "3"       ; $09 = TAB, $1B = ESC
_keyboardMapRowF4: .BYTE ".l'9", $0A, $08, "~2"       ; $0A = LF, $08 = BS
_keyboardMapRowF5: .BYTE "/;", $22, "0\|q1"           ; $22 = DOUBLE QUOTE

.scend
