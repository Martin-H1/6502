; -----------------------------------------------------------------------------
; 6502 assembler losely based on a reverse engineering of Daryl Rictor's
; SBC OS. It will depend upon the stack, list, and I/O functions defined
; in the other modules.
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

;
; Macros
;

;
; Functions
;

; Mini assembler code
Assem_Init:
	tsx 		;
	inx 		;
	inx 		;
	inx 		;
	inx 		;
	stz $0100,x		;
	jsr version		;  show version and ? prompt
	jmp Assembler		;
Asm_Help:	lda #<AsmHelptxt		;  lower byte - Menu of Commands
	sta addrptr		;
	lda #>AsmHelptxt		;  upper byte
	sta addrptr+1		;
	bra AsmHelp3		;
ASmHelp4:	cmp #$7e		;  "~"
	beq AsmHelp1		;
	jsr Output		;
	bra AsmHelp2		;
AsmHelp1:	jsr Print_CR		;     
AsmHelp2:	jsr Inc_addrptr		;
AsmHelp3:	lda (addrptr)		;
	bne AsmHelp4		;
	jsr Opcode_List		;  

Assembler:
	LDX #$FF		;
	TXS 			; init stack
	stz HexDigCnt		;
	jsr Input_assem		;
	ldy #$00		; beginning of input line
	lda buffer		;
	cmp #$0d		; Enter = done 
	bne Asm01		;
	JMP Monitor		; exit assembler
Asm01:	cmp #$3f		; "?" Print Help
	beq Asm_Help		;
	cmp #$20		; space
	beq Asm_opfetch		;
	cmp #$3b		;  ";" ignore line
	beq Assembler		;
	cmp #$4C		;  "L" list
	beq Asm_List		;
	cmp #$24		;  "$" ignore this
	bne Asm02		;
	iny 		;
Asm02:	STZ Hexdigits		;  holds parsed hex
	STZ Hexdigits+1		;
	JSR ParseHexDig		;  get Hex Chars 
	LDX Hexdigcnt		;
	Beq Asm_Err		;
	cmp #$4C		; "L" do list               ???
	Beq Asm_List1		;
	cmp #$20		; Space
	Beq Asm_opfetch		;
Asm_Err:	tya 		;  get line pointer
	tax 		;
	lda #$0a		; LF move down one line
	jsr output		;
	jsr PrintXSP		; move to where error occured
	lda #$5E		; "^"                       ???
	jsr Output		; mark it 
	jsr bell		; 
	bra Assembler		;
Asm_list:	stz HexDigcnt		;
Asm_List1:	jsr List_Cmd_1		;
Asm_hop: bra Assembler		;
Asm_opfetch:	lda HexDigCnt		;
	beq Asm_op01		; no address change
	LDX Hexdigits		;
	LDA Hexdigits+1		;
	STX AddrPtr		;
	STA AddrPtr+1		;
	dey 		;
Asm_stripSP:	iny 		;
Asm_op01:	lda buffer,y		;
	cmp #$20		; strip spaces
	beq Asm_stripSP		;
	cmp #$0d		; done
	beq Asm_hop		;
	cmp #$3b		; ";" comment char done
	beq Asm_hop		;
	ldx #$00		;
	stx OpcTxtPtr		;
	sty LineCnt		;
Asm_opclp:	ldy LineCnt		;
	lda OpcTxtPtr		;
	ASL 		;
	adc OpcTxtPtr		;
	tax 		;
	lda buffer,y		;
	iny 		;
	cmp OpcTxtData,x		;
	bne Asm_getnext		;
	lda buffer,y		;
	inx 		;
	iny 		;
	cmp OpcTxtData,x		;
	bne Asm_getnext		;
	lda buffer,y		;
	inx 		;
	iny 		;
	cmp OpcTxtData,x		;
	beq Asm_goodop		;
Asm_getnext:	ldx OpcTxtPtr		;
	inx 		;
	stx OpcTxtPtr		;
	cpx #$4A		; last one? then err
	bne Asm_opclp
Asm_err2:	jmp Asm_err
Asm_goodop:	lda #$00
	sta ModeJmp		; 
	dec ModeJmp		; init to FF for () check
	sta HexDigits		; and Byte holder
	sta HexDigits+1		; 
	sta HexDigCnt		;
	ldx OpcTxtPtr		;
	cpx #$42		; 
	bmi Asm_goodSP		; not a 4 chr opcode
	cpx #$46
	bpl Asm_goodSP		; not a 4 chr opcode
	lda buffer,y		; get next chr
	iny 		; advance pointer
	cmp #$38		; 
	bpl Asm_err2		; not chr "0"-"7"
	cmp #$30
	bmi Asm_err2		; not chr "0"-"7"
	ASL 
	ASL 
	ASL 
	ASL 
	sta startaddr+1		; temp holder for 4th chr opcode
	LDA #$80		; flag for 
Asm_goodSP:	ldx buffer,y		; get next operand char
	iny 		; point to next operand chr
	cpx #$20		;  sp
	bne Asm_GoodSP2
	cmp #$80
	bmi Asm_goodSP
Asm_goodSP1:	ldx OpcTxtPtr		; check if its a BBRx or BBSx opcode
	cpx #$44		; 
	bpl Asm_GoodSP		;
	ldx HexDigCnt		;
	beq Asm_goodSP		;
	cmp #$D0		; already have zp & rel?
	bpl Asm_GoodSP		; we don't care then
	cmp #$C0		; already got a zp address?
	bpl Asm_Err2		; then error
	ldx HexDigits+1
	bne Asm_err2		; not zero page
	ldx HexDigits
	stx startaddr		; temp zp value for BBRx & BBSx cmds 
	ora #$40		; mark zp address fetched
	and #$F7		; mask out zp address found
	bra Asm_goodSP		; get next chr
Asm_goodSp2:	cpx #$0d		;  CR
	bne Asm_eol
Asm_jmp1:	jmp Asm_modeSrch
Asm_eol:	cpx #$3b		;  ";"
	beq Asm_jmp1
	pha 
	lda OpcTxtPtr
	cmp #$46		; normal opcode if <=45h
	bmi Asm_opnd1
	bne Asm_xtra1
	cpx #$24		; $ .db pseudo-opcode
	beq Asm_db1
	dey 
Asm_db1:	jsr ParseHexDig
plx
	ldx HexDigCnt
	beq Asm_err2		; no digits retrieved
	ldy #$00
	lda #$01
	PHA 
	lda HexDigits
	sta (AddrPtr),y
	jmp Asm_save
Asm_xtra1:	cmp #$47		; .dw pseudo-opcode
	bne Asm_xtra2
	cpx #$24		; $
	beq Asm_dw1
	dey 
Asm_dw1:	jsr ParseHexDig
plx
	ldx HexDigCnt
	beq Asm_err1		; no digits retrieved
	ldy #$00
	lda #$02
	PHA 
	lda HexDigits
	sta (AddrPtr),y
	lda HexDigits+1
	iny 
	sta (AddrPtr),y
	jmp Asm_save
Asm_xtra2:	cmp #$48		; .ds pseudo-opcode
	bne Asm_err1
	jmp Asm_txt
Asm_opnd1:	pla 
	cpx #$23		;  #    20
	bne Asm_parse01
	ora #$20
	jmp Asm_goodSP
Asm_parse01:	cpx #$28		;  (   04
	bne Asm_parse02
	ora #$04
	ldx modeJmp
	bpl Asm_err1		;  more than one ( 
	inc ModeJmp
	jmp Asm_goodSP
Asm_parse02:	cpx #$29		;  )
	bne Asm_parse03
	ldx ModeJmp
	bne Asm_err1		;  ) without (
	inc ModeJmp
	jmp Asm_goodSP
Asm_parse03:	cpx #$2C		;  ,
	bne Asm_parse04
	ldx buffer,y
	cpx #$58		;  X        02
	bne Asm_parse31
	ora #$02
	iny 
	jmp Asm_goodSP
Asm_parse31:	cpx #$59		;  Y        01 
	beq Asm_parse32
	cmp #$80		;  is BBRx or BBSx cmd active?
	bmi Asm_err1		;  , without X or Y or 4byte opcode      
	jmp Asm_goodSP1		;  save zp address
Asm_parse32:	ora #$01
	iny 
	jmp Asm_goodSP
Asm_parse04:	cpx #$24		;  $
	beq Asm_parse42		;   
	dey 		; not #$(),X,Y  so try Hexdig, if not err
Asm_parse42:	pha 
	jsr ParseHexDig
	dey 		; adjust input line pointer
	pla 
	ldx HexDigCnt
	beq Asm_err1		; no digits retrieved
	ldx HexDigits+1
	bne Asm_parse41
	ora #$08		; <256               08
	jmp Asm_goodSP
Asm_parse41:	ora #$10		; 2 bytes            10 
	jmp Asm_goodSP
Asm_err1:	jmp Asm_Err
Asm_ModeSrch:	ldx #$0F		; # of modes
Asm_ModeS1:	cmp Asm_ModeLst,x
	beq Asm_ModeFnd
	dex 
	bpl Asm_ModeS1
	bra Asm_Err1		; invalid Mode
Asm_ModeFnd:	stx Memchr		; save mode
	cmp #$80		; is it 4 chr opcode?
	bmi Asm_opcSrch		;no
	txa 
	ora startaddr+1		; adjust the psuedo mode               
	sta Memchr		; set proper mode
Asm_opcSrch:	ldx #$00
Asm_opcSrch1:	lda OpcTxtidx,x
	cmp OpcTxtPtr
	bne Asm_srchNxt
	lda OPCaddmode,x
	cmp Memchr
	beq Asm_OpcFnd
Asm_srchNxt:	inx 
	bne Asm_opcSrch1
	lda Memchr		;
	cmp #$02		; ZP
	bne Asm_srchAlt
	LDA #$01		; ABS
	sta Memchr
	bra Asm_opcSrch
Asm_srchAlt:	cmp #$01		; ABS
	bne Asm_srchA0
	LDA #$0A		; REL
	sta Memchr
	bra Asm_opcSrch
Asm_srchA0:	cmp #$0d		;  ind zp
	bne Asm_srchA1
	LDA #$0b		; ind Abs
	sta Memchr
	bra Asm_opcSrch
Asm_SrchA1:	cmp #$07		; zp,y
	bne Asm_Err1		; no more modes to try, bad mode err
	LDA #$09		; ABS,y
	sta Memchr
	bra Asm_opcSrch
Asm_OpcFnd:	lda Memchr
	and #$0F		; mask out psuedo modes
	sta Memchr		;
	CMP #$0E		; BBR mode?
	bne Asm_opcFnd0		;
	jsr Asm_BRelCalc
	sta HexDigits+1
	lda Startaddr		;
	sta Hexdigits		;
	bra Asm_OpcFnd1		;   
Asm_OpcFnd0:	cmp #$0A		; is Rel Mode?
	bne Asm_OpcFnd1
	jsr Asm_RelCalc		; adjust rel address
Asm_OpcFnd1:	ldy #$00
	txa 
	sta (AddrPtr),y
	iny 
	ldx Memchr		; 
	lda ModeByteCnt,x
	PHA 		; Save # of bytes
	cmp #$01
	beq Asm_EchoL
	lda HexDigits
	sta (AddrPtr),y
	iny 
	lda ModeByteCnt,x
	cmp #$02
	beq Asm_EchoL
	lda HexDigits+1
	sta (AddrPtr),y
Asm_EchoL:	lda AddrPtr
	sta StartAddr
	lda AddrPtr+1
	sta StartAddr+1
	jsr List_One
Asm_Save:	clc 
	PLA 
	adc AddrPtr
	sta AddrPtr
	bcc Asm_done
	inc AddrPtr+1
Asm_done:	jmp Assembler
Asm_BRelCalc:	jsr Asm_relsub
	sbc #$03
	bra Asm_RelC1
Asm_RelSub:	sec 
	lda Hexdigits
	sbc AddrPtr
	sta Memptr
	lda Hexdigits+1
	sbc AddrPtr+1
	sta Memptr+1
	sec 
	lda Memptr
	rts 
Asm_RelCalc:	jsr Asm_relsub
	sbc #$02
Asm_Relc1:	sta Memptr
	bcs Asm_relC2
	dec Memptr+1
Asm_relC2:	lda Memptr+1
	beq Asm_relC4		; positive
	cmp #$FF		; negative
	bne Asm_txtErr
	lda Memptr
	bpl Asm_txtErr
Asm_relC3:	sta HexDigits
	rts 
Asm_relC4:	lda Memptr
	bpl Asm_relC3
Asm_txtErr:	jmp Asm_Err
Asm_txt:plx		; process the .ds pseudo-opcode
	dey 
	tya 
	tax 
	ldy #$fe
Asm_txt1:	iny 
Asm_txt2:	lda buffer,x		; get next operand char
	inx 		; point to next operand chr
	cmp #$0d		;  CR
	beq Asm_txt9
	cmp #$27		; "
	bne Asm_txt3
	cpy #$ff		; opening " found?
	bne Asm_txt9		; no, closing, so done
	bra Asm_txt1		; yes, get first text chr
Asm_txt3:	cpy #$ff		; already found opening "?
	beq Asm_txt4		; 
	sta (AddrPtr),y		; yes, save chr
	bra Asm_txt1
Asm_txt4:	cmp #$20		; no, if not a space, then err
	beq Asm_txt2
	txa 
	tay 
	bra Asm_txtErr
Asm_txt9:	tya 
	pha 
	jmp Asm_save
;
Opcode_List:	ldy #$49		; Number of Opcodes (64)
	ldx #$00		; pointer to characters
Opcode_List1:	txa 		; 
	and #$0F		; Print CR after each 16 opcodes 
	bne Opcode_List2		; not divisible by 16
	jsr Print_CR		;
Opcode_List2:	lda OPCtxtData,x		; get opcode chr data
	jsr Output		; print 1st char
	inx 		;
	lda OPCtxtData,x		; 
	jsr Output		; print 2nd char
	inx 		;
	lda OPCtxtData,x		;
	jsr Output		; print 3rd char
	inx 		;
	cpy #$08		; 
	bpl Opcode_List3		; not 4 byte code
	cpy #$04		;
	bmi Opcode_list3		;
	lda #$78		; add 'x'
	jsr output		; for RMBx, SMBx,BBRx, & BBSx
Opcode_List3:	lda #$20		; print space
	jsr Output		;
	dey 		;
	bne Opcode_List1		; 
	jsr Print_CR		; one last CR-LF
	rts 		;

.scend
