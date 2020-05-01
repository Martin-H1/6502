;*********************************************************************
;  2002/11/18	0.10	Created. Started with Kernal source from 
;			original project.
;  2003/01/17	0.20	First clean compile. Waiting for working test
;			  board in order to do live test.
;  2003/05/22   0.80	Live printer test works! Working on disk drives.
;  2003/05/28   	Added mapping for GETIN
;  2003/06/04		Revised CLRCH to match VIC20 ROM
;  2003/06/06   0.90	Changes to SBIDLE (immediate RTS).
;  2014/02/04	RAC	Removed DOS code and integrated into EhBASIC
;			Changed IOERMS message calling to use EhBASIC
; ----------------- assembly instructions ---------------------------- 
;
;===============================================================
;  Commodore Serial IEC BIOS for Generic 6502-based SBC
;  Copyright (c) 2002-2014 Richard A. Cini
;
;  Derived from the Commodore VIC-20 Kernal ROM
;  Copyright (c) 1980 Commodore Business Machines Ltd.
;
;  Hardware Requirements:
;    Code assumes a single available MOS/CSG 6522 VIA and a buffer/
;    adapter board to provide buffering of the VIA signals as 
;    required by CBM serial IEC peripherals. See schematic for
;    details.
;
;    VIA signals used:	PA.0 - CLK_IN
;			PA.1 - DATA_IN
;			PA.7 - ATN_OUT
;			CA2  - CLK_OUT
;			CB1  - SRQ_IN
;			CB2  - DATA_OUT
;    Other VIA usage:   Timer 2
;
;  Applications Programming Interface:
;    The API is Commodore-compatible and follows the same semantics.
;    It includes both high-level (OPEN, CLOSE, LOAD, SAVE) as well
;    as low-level I/O routines. Console output uses monitor routine
;    OUTPUT. 
;===============================================================
;RAMBOT  =$0400		; Bottom of RAM
;RAMTOP  =$7eff		; Top of RAM
; Note: If location changes in via1.asm, need to change here too
D1ORB	=$7F50		;50 Port B output register
D1ORA  	=D1ORB+1	;51 Port A output register (clk_in, data_in, atn_out)
D1DDRB 	=D1ORB+2	;52 DDRB (data direction register)
D1DDRA 	=D1ORB+3	;53 DDRA (data direction register)
D1TM1L 	=D1ORB+4	;54 Timer 1 latch LSB
D1TM1H 	=D1ORB+5	;55 Timer 1 latch MSB
D1T1CL 	=D1ORB+6	;56 Timer 1 counter LSB
D1T1CH 	=D1ORB+7	;57 Timer 1 counter MSB
D1TM2L 	=D1ORB+8	;58 Timer 2 latch LSB
D1TM2H 	=D1ORB+9	;59 Timer 2 latch MSB
D1SHFR 	=D1ORB+10	;5A shift register
D1ACR  	=D1ORB+11	;5B ACR (aux control register)
D1PCR  	=D1ORB+12	;5C PCR (peripheral control register clk_out, srq_in, data_in)
D1IFR  	=D1ORB+13	;5D IFR (interrupt flag register)
D1IER  	=D1ORB+14	;5E IER (interrupt enable register)
D1ORAH 	=D1ORB+15	;5F Port A output register (unlatched)

;###############################################################
; API -- DO NOT RE-ORDER ENTRIES
_IEC	 JMP IEC_Init	; initialization code

; API routines
_SETTMO  JMP SETTMO	; set IEC bus timeout
_SETNAM	 JMP SETNAM	; set filename parameters
_SETLFS  JMP SETLFS	; set logical file parameters
_MEMBOT  JMP MEMBOT	; set bottom of memory
_OPEN    JMP OPEN	; open logical file
_CLOSE 	 JMP CLOSE	; close logical file
_CLALL   JMP CLOSEALL	; close all open logical files
_CHKIN	 JMP CHKIN	; set channel for input
_CHKOUT	 JMP CHKOUT	; set channel for output
_GETIN	 JMP GETIN	; get character from head of keyboard buffer
_CHRIN	 JMP CHRIN	; get character from channel
_CHROUT	 JMP CHROUT	; send character to IEC
_READST	 JMP READST	; read IEC status variable
_CLRCH	 JMP CLRCH	; clear I/O channels
_SETMS	 JMP SETMS	; set system message mode
_LOAD	 JMP IECLOAD	; load
_SAVE	 JMP IECSAVE	; save

; Internal routines. Used when you need to talk directly to a
; peripheral and don't need a file handle.
_ACPTR	 JMP ACPTR	; get character from IEC (low-level)
_CIOUT	 JMP CIOUT	; output character to IEC (low-level)
_LISTEN  JMP LISTEN	; send LISTEN command
_LSECOND JMP SECOND	; send secondary address
_TALK	 JMP TALK	; send TALK command
_TSECOND JMP TALKSA	; send TALK command with secondary address
_UNLSTN  JMP UNLISTEN	; send UNLISTEN command
_UNTALK  JMP UNTALK	; send UNTALK command
;###############################################################

;===============================================================
; IEC_Init - Initialization routine
;	Called from MonitorBoot in sbcos.asm
;	VIAs are initialized in VIAx.asm
;===============================================================
IEC_Init

	lda #$80
	jsr SETMS	; command mode (80=direct; 0=programmed)
;	jsr SETTMO	; enable timeouts (0=enable; 80=disable)

 	lda #$00	;
	sta CSRMOD	; cursor mode (0=direct; 80=programmed)
	sta D1TM2L
	sta D1TM2H
	jsr SETTMO	; enable timeouts (0=enable; 80=disable)

	ldx #$03	; screen device
	jsr CLR2	; set default devices (in CLRCH)

	ldy #>Ram_base	; MSB
	ldx #<Ram_base	; LSB
	clc
	jsr MEMBOT	; set bottom of RAM to YYXX

	lda #$4c
	sta USRPOK	; set JSR opcode
	rts


;===============================================================
; SETTMO - Set/clear serial bus timeout flag FE6F
;
;  PARAMS   :  .A=bit7 clear to enable serial timeouts
;	       .A=bit7 set to disable serial timeout
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  nothing
;  PRE-CALLS:  none
;  COMMENTS :  Serial device timeout is 64ms. Should be left
;		"enabled" for most uses.
;	BUGBUG: conflicting info in Programmer's Ref Guide
;		lead one to believe that this should be set
;		to #$80 to "enable" timeout testing.
;
;	Found comments in C64 PRG that indicates this is for
;	the C64 IEEE card only. So, who knows...
;===============================================================
SETTMO

	STA STIMOT
	RTS


;===============================================================
; SETNAM - Set pointer to filename string FE49
;
;  PARAMS   :  .A=length of string
;	       .X=LSB of string address
;	       .Y=MSB of string address
;  RETURNS  :  nothing
;  PRE-CALLS:  none
;  COMMENTS :  When setting length and string, string must be
;		null-terminated by the user so that it prints
;		on the screen properly but the value of FNMLEN
;		SHOULD NOT include the null termination.
;		This is different than the CBM API.
;===============================================================
SETNAM
	STA FNMLEN		;set length
	STX FNPTR		;ptr L
	STY FNPTR+1		;ptr H
	RTS


;===============================================================
; SETLFS - Set logical file parameters FE50
;
;  PARAMS   :  .A=logical filenumber to open (LFN $ff reserved
;			for Kernal use).
;	       .X=device to open (4-31 allowed)
;	       .Y=device-speific command (secondary address) or 
;			0xff for none
;  RETURNS  :  none
;  PRE-CALLS:  none
;  COMMENTS :  Devices: 0-3	(reserved - don't use)
;			4-7	printer
;			8-11	disk drive
;			12-31	undefined
;===============================================================
SETLFS
	STA LOGFIL		;file#
	STX CHANNL		;device
	STY SECADR		;secondary address
	RTS


;===============================================================
; MEMBOT - Read/set bottom of memory FE82
;
;  PARAMS   :  .X=LSB of address
;	       .Y=MSB of address
;	       Call with CY=1 (SEC) to read, CY=0 (CLC) to write
;  RETURNS  :  nothing
;  PRE-CALLS:  Setting/clearing Carry Flag
;  COMMENTS :  Used as a pre-call to SAVE to set the bottom
;		memory address used for saving memory.
;===============================================================
MEMBOT
	BCC STOBOT
	LDX OSSTAR		; address L
	LDY OSSTAR+1		; address H

STOBOT
	STX OSSTAR		; address L
	STY OSSTAR+1		; address H
	RTS


;===============================================================
; SETMS - Control OS Messages FE66
;
;  PARAMS   :  .A=message number; BIT7=kernal, BIT6=control, 
;		BIT5-0=message number.
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  none
;  PRE-CALLS:  none
;  COMMENTS :  $80=direct mode; $00=programmed
;===============================================================
SETMS
	STA CMDMOD		;save message #
	LDA CSTAT		;get status

ISETMS1				; set ST bits
	ORA CSTAT		;twiddle bits
	STA CSTAT		;save status
	RTS


;===============================================================
; OPEN - Open logical file for device access  F40A
;
;  PARAMS   :  none
;  RETURNS  :  Errors 1,2,4,5,6
;  PRE-CALLS:  SETNAM, SETLFS
;  COMMENTS :  Devices 4-31 allowed
;===============================================================
OPEN
	LDX LOGFIL		;get file number
	BNE IOPEN_S1		;F411 <>0 not "save"
	JMP IOERMS6		;$F78D "NOT INPUT FILE" error

IOPEN_S1	
	JSR FIND		;locate file# in table, X is free spot
	BNE IOPEN_S2		;F419 not found; any more free spots
	JMP IOERMS2		;$F781 "FILE OPEN" error

IOPEN_S2	
	LDX COPNFL		;get # of open files
	CPX #$0A		;10 files open?
	BCC IOPEN_S3		;F422 no, OK to open it
	JMP IOERMS1		;"TOO MANY FILES" error

IOPEN_S3			; check device# before opening logical file
	LDA CHANNL		; get device#
	CMP #$03
	BCS IOPEN_S4		; device 4 or greater, send SA to IEEE
	jmp IOERMS9		; anything else, ILLEGAL DEVICE error

IOPEN_S4			; All tests passed, so open the logical file
	INC COPNFL		;bump count
	LDA LOGFIL
	STA FILTBL,X		;save file# in table
	LDA SECADR		;flag and save SA
	ORA #%01100000		;$60
	STA SECADR
	STA SECATB,X
	LDA CHANNL		;save device
	STA DEVTBL,X
	JSR SENDSA		;send secondary to IEEE
	CLC
	RTS


;===============================================================
; CLOSE - Close logical device file  F34A
;
;  PARAMS   :  .A=logical filenumber to close
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  none
;  PRE-CALLS:  OPEN
;  COMMENTS :  Devices 4-31 allowed.
;===============================================================
CLOSE
	JSR FIND1		;$F3D4 locate file#
	BEQ ICLSE		;$F351 found it, go to closer
	CLC
	RTS			;not found, return CY=0

ICLSE
	JSR FLATRB		;get file attributes
	TXA			;save table offset
	PHA			;push it
	LDA CHANNL		;get device
	CMP #$03		;screen?
	BCS ICLSE1		;$F3AE device#>3, close IEEE 
	CLC			; invalid device (<3)
	RTS

ICLSE1
	BIT SECADR		; test bit7 of secondary address
	BMI ICLSEX		; if not file-oriented channel, just exit

	LDA CHANNL		;get device number and
	JSR LISTEN		;command it to listen
	LDA SECADR		;get secondary address
	AND #%11101111		;$EF
	ORA #%11100000		;$E0 CLOSE command
	JSR SECOND		;send secondary address
	JSR UNLISTEN		;finally, command device to unlisten
	PLA			;restore table offset
	TAX
	DEC COPNFL		;decrement # of open files
	CPX COPNFL		;no more files open, go to ready
	BEQ ICLSEX		;$F3CD return CY=0
	LDY COPNFL		;delete device by moving last entry
 	LDA FILTBL,Y		;in table into deleted position
	STA FILTBL,X
	LDA DEVTBL,Y
	STA DEVTBL,X
	LDA SECATB,Y
	STA SECATB,X

ICLSEX
	CLC
	RTS			;exit clear


;===============================================================
; CLOSEALL - Close all open logical files F3EF
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  none
;  PRE-CALLS:  OPEN
;  COMMENTS :  none
;===============================================================
CLOSEALL
	LDA #$00
	STA COPNFL		;zero-out count of open files
	JSR CLRCH		; restore I/O channels
	RTS


;===============================================================
; READST- Read status word FE57
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  .A=device status, as follows:
;		bit0 - Write time out
;		bit1 - Read time out
;		bit2 - {reserved}
;		bit3 - {reserved}
;		bit4 - {reserved}
;		bit5 - {reserved}
;		bit6 - EOI
;		bit7 - Device not present
;  PRE-CALLS:  none
;  COMMENTS :  none
;===============================================================
READST
	LDA CSTAT		;get status
	ORA CSTAT		;clear status bits
	STA CSTAT		;save status
	RTS			


;===============================================================
; CHKIN - Set open channel for input F2C7
;
;  PARAMS   :  .A=unused
;	       .X=logical file number from OPEN call
;	       .Y=unused
;  RETURNS  :  Errors 3,5,6
;  PRE-CALLS:  OPEN
;  COMMENTS :  Associated device must be a logical input device
;		(terminal or disk drive). Call automatically sends
;		the talk address and the secondary address (if
;		specified) to the device.
;===============================================================
CHKIN
	JSR FIND		;find file# in tables
	BEQ ICHKI1		;$F2CF found, continue
	JMP IOERMS3		;'FILE NOT OPEN' error

ICHKI1
	JSR FLATRB		;set file# params
	LDA CHANNL		;get device 
	CMP #$03
	BCS ICHKI3		;$F2F0 IEEE? yes, handle IEEE
	jmp IOERMS9		; ILLEGAL DEVICE

ICHKI3				;handle IEEE			
	TAX			;copy device
	JSR TALK		;command device to talk 
	LDA SECADR		;is there a secondary address?
	BPL ICHKI4		;$F2FE yes, send it
	JSR CLKWAIT		;no, send regular talk command
	jmp ICHKI5		;$F301

ICHKI4
	JSR TALKSA		;send secondary address talk

ICHKI5
	TXA			;restore device#
	BIT CSTAT		;BIT7= Dev not present
	BPL ICHKIEX		;all clear, exit
	JMP IOERMS5		;"DEVICE NOT PRESENT" error; Exit

ICHKIEX
	STA INDEV		; save as input device
	CLC
	RTS


;===============================================================
; CHKOUT - Set open channel for output F309
;
;  PARAMS   :  .A=unused
;	       .X=logical file number from OPEN call
;	       .Y=unused
;  RETURNS  :  Errors 3,5,7
;  PRE-CALLS:  OPEN
;  COMMENTS :  Associated device must be an output device (printer
;		or disk drive). Call automatically sends the listen
;		address and the secondary address (if specified)
;		to the device.
;===============================================================
CHKOUT
	JSR FIND		;locate file# in tables
	BEQ ICHKO1		;$F311 found, continue
	JMP IOERMS3		;$F784 "FILE NOT OPEN" error

ICHKO1
	JSR FLATRB		;set file attributes
	LDA CHANNL		;get device
	CMP #$03
	BCS ICHKO2		;$F2F0 IEEE? yes, handle IEEE
	jmp IOERMS9		; "Illegal Device"

ICHKO2
	TAX			;save device#
	JSR LISTEN		;command device to listen
	LDA SECADR		;is there an SA?
	BPL ICHKO_S4		;$F33F yes, send SA

	JSR CLRATN		;no, clear ATN line and make sure
	BNE ICHKO_S5		; device is present

ICHKO_S4
	JSR SECOND		;send SA

ICHKO_S5
	TXA			;restore device#
	BIT CSTAT		;BIT7=Dev not present
	BPL ICHKO_S6		;$F32E
	JMP IOERMS5		;$F78A "DEVICE NOT PRESENT" error

ICHKO_S6
	STA OUTDEV		;set OUTDEV and exit
	CLC
	RTS


;===============================================================
; GETIN - Get a byte from the head of the keyboard queue
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  .A=character returned from buffer
;  COMMENTS :  None
;===============================================================
GETIN
	JSR Scan_Input		; map KERNAL routine to SBCOS
	RTS


;===============================================================
; CHRIN - Read a character from the input channel F20E
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  .A=character returned from channel
;	       Status returned according to READST
;  PRE-CALLS:  OPEN, CHKIN
;  COMMENTS :  Devices 4-31 allowed. Associated device must be
;		a logical input device (disk drive).
;===============================================================
CHRIN
	LDA INDEV		;get input device
	CMP #$03		; allowed IEC device?
	BCS CHINIE		; yes, go get char
	CLC			; return clear
	RTS

CHINIE
	LDA CSTAT		;any IEEE errors? 
	BEQ ACPTR		;no, get next char from device
	LDA #$0D		;yes, return <CR> and exit
	CLC
	RTS


;===============================================================
; ACPTR - Receive a byte over the serial bus EF19
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  .A=byte received from serial device
;	       Status returned according to READST.
;  PRE-CALLS:  TALK, [TALKSA]
;  COMMENTS :  Internal routine used to receive a single buffered
;		data byte on the bus using full handshaking to 
;		a talking device. Also is the back-end input
;		routine to CHRIN.
;===============================================================
ACPTR
	SEI           		;kill interrupts
	LDA #$00			
	STA CNTDN		;zero-out bit count
	JSR SCLK1		;assert CLK

IACPLP1
	JSR SDCLK		;get CLK answer
	BCC IACPLP1		;wait for CLK=H
	JSR SOUT1		; answer with DATA=H
				; not called in c64

IACPTR1
	LDA #$01		; delay for 256us...
	STA D1TM2H

IACPLP2
	LDA D1IFR
	AND #%00100000		;Timer2 time-out flagged?
	BNE IACPTR2

	JSR SDCLK		;...then check CLK again
	BCS IACPLP2		;CLK=H, then EOI (ignore)
	BCC IACPTR3A		;CLK=L, then start clocking data

IACPTR2
	LDA CNTDN		; get bit count
	BEQ IACPTR3		; no more to shift, move on
	JMP FLGER02		; error code 2 (read timeout)

IACPTR3
	JSR SOUT0		; clear DATA
	JSR UNLSTN2		; send unlisten
	LDA #$40		;EOF error
	JSR ISETMS1
	INC CNTDN
	BNE IACPTR1

IACPTR3A
	LDA #$08		;set bit count
	STA CNTDN		

IACPLP4				; clock in bits
	LDA D1ORAH
	CMP D1ORAH
	BNE IACPLP4
	LSR A
	BCC IACPLP4
	LSR A
	ROR CYCLE

IACPLP5
	LDA D1ORAH
	CMP D1ORAH
	BNE IACPLP5
	LSR A
	BCS IACPLP5
	DEC CNTDN
	BNE IACPLP4
	JSR SOUT0		; clear DATA
	LDA CSTAT		; error? 
	BEQ IACPEX		; no, continue shifting
	JSR UNLSTN2		; yes, unlisten and exit.

IACPEX
	LDA CYCLE
	CLI
	CLC
	RTS


;===============================================================
; CHROUT - Write a character to the output channel F27A
;
;  PARAMS   :  .A=character to write to output channel
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST
;  PRE-CALLS:  OPEN, CHKOUT
;  COMMENTS :  Associated device must be a logical output device
;		(printer or disk drive). Care must be taken when
;		using CHROUT since data will be sent to all listening
;		devices (setup with CHKOUT). Unless this is the
;		desired effect, be sure to UNLISTEN all unintended
;		devices. Channel is left open after the call.
;===============================================================
CHROUT
	PHA			; save char
	LDA OUTDEV		; get output device
	CMP #$03		; is it the screen?
	BNE NOTSCR		; no, test other devices

				; SCREEN
	PLA			; restore character
	JSR OUTPUT		; send it to the terminal
	RTS			; and return

NOTSCR				; NOT the SCREEN
	PLA			; restore character; CY should not be effected
	BCS CIOUT		; IEC? Yes, send it.
	JMP IOERMS7		; Otherwise, "Not Output File" error

	; fall-through to CIOUT
	
;===============================================================
; CIOUT - Transmit a byte over the serial bus EEE4
;
;  PARAMS   :  .A=character to write to output channel
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST.
;  PRE-CALLS:  LISTEN, [SECOND]
;  COMMENTS :  Internal routine used to send a single buffered
;		data byte on the bus using full handshaking to 
;		a listening device, such as filename and program
;		address info during SAVE. Also is the back-end
;		output routine to CHROUT.
;
;		When UNLISTEN is called to end the transmission,
;		the byte is sent with EOI set. 
;===============================================================
CIOUT
	BIT C3PO       		;deferred character?
	BMI ICIOUT1		; no, send it now	
	SEC
	ROR C3PO
	BNE ICIOUT2		;send it later

ICIOUT1	
	PHA			;save byte	
	JSR OTDSBU		;send it immediately	
	PLA			;restore it	

ICIOUT2	
	STA BSOUT		;deferred character	
	CLC
	RTS


;===============================================================
; SECOND - Send secondary address to a device commanded to LISTEN EEC0
;
;  PARAMS   :  .A=secondary address (4-31) OR 0x60
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST.
;  PRE-CALLS:  LISTEN
;  COMMENTS :  Device must be an input device. Secondary address
;		must be first ORed with 0x60 before calling this
;		routine.
;===============================================================
SECOND
	STA BSOUT		; save secondary address
	JSR ILISTEX		; output with ATN
				;CLK=0, DATA=1, pause, output

CLRATN				; CLRATN - Clear the ATN line
	LDA D1ORAH		; ATN=0, DATA=1, CLK=1
	AND #%01111111		;$7F
	STA D1ORAH			
	RTS			


;===============================================================
; TALK - Command a device to TALK EE14
;
;  PARAMS   :  .A=device number (4-31)
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST.
;  PRE-CALLS:  none
;  COMMENTS :  Device must be an output device.
;===============================================================
TALK
	ORA #%01000000		;$40 BIT6 = Talk address
	.byte $2c
	; fall through to LISTEN

;===============================================================
; LISTEN - Command a device to LISTEN EE17
;
;  PARAMS   :  .A=device number (4-31)
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST.
;  PRE-CALLS:  none
;  COMMENTS :  Associated device must be an input device. Can 
;		be used to send a single buffered data byte on
;		the bus using full handshaking but without
;		creating a logical file. When UNLISTEN is
;		called to end the transmission, the byte is sent
;		with EOI set.
;===============================================================
LISTEN
	ORA #%00100000		;$20 BIT5 = Listen address
	JSR SBIDLE		; Wait for RS232 idle state

LSNOIDLE
	PHA			;save device address
	BIT C3PO		;char waiting?
	BPL LISN1		;no, branch $EE2B
	SEC			;yes, output bit
	ROR SBITCF		;bit count
	JSR OTDSBU		;output
	LSR C3PO		;go to next
	LSR SBITCF			

LISN1
	PLA			;restore device address
	STA BSOUT		;save to xmit buffer
	JSR SOUT1		;send DATA=1
	CMP #%00111111		;$3F Data=0? 7-5=DATA 4=SRQ 3-1=CLK
	BNE LISN2		;no answer yet, send ATN
	JSR SCLK1		;send SCLK=1 answer

LISN2				; ATN sequence
	LDA D1ORAH			
	ORA #%10000000		;$80 set PA.7=1 Assert ATN (inverted to 0)
	STA D1ORAH		; (LOW at IEC port is "true" and HIGH is "false")

ILISTEX
	JSR SCLK0		;send SCLK=0 (true)
	JSR SOUT1		;send DATA=1 (false)
	JSR DDELAY		;wait 1ms

; Fall-through to OTDSBU

;===============================================================
; OTDSBU- Send one byte on serial bus EE49
;
OTDSBU
	SEI			;kill interrupts
	JSR SOUT1		;send DATA=1
	JSR SDCLK		;Get clk_in/data_in status
	LSR A			;shift to data_in
	BCS FLGERR		;data_in=1, device not present
	JSR SCLK1		;got device ack, ack with CLK=1
	BIT SBITCF		;more bits?
	BPL OTDLP3		;$EE66 yes, skip EOI handler
OTDLP1
	JSR SDCLK		;wait for DIn=H
	LSR A			;roll data into carry
	BCC OTDLP1		;$EE5A
OTDLP2
	JSR SDCLK		;wait for DIn=L
	LSR A			;roll data into carry
	BCS OTDLP2		;$EE60
OTDLP3
	JSR SDCLK		;wait again for DIn=H
	LSR A			;roll data into carry
	BCC OTDLP3		;$EE66
	JSR SCLK0		;send CLK=0
	LDA #$08		;bit counter
	STA CNTDN		;save as bit count-down
OTDLP4
	LDA D1ORAH		;wait for idle
	CMP D1ORAH			
	BNE OTDLP4
	LSR A			
	LSR A			; LSR to data_in 
	BCC FLGER03		; data_in=0 time-out error
	ROR BSOUT		;get bit
	BCS OTDSB1		; if 1, send DATA=1
	JSR SOUT0		;not 1, must be 0; DATA=0
	BNE OTDSB2
OTDSB1
	JSR SOUT1		;send DATA=1
OTDSB2
	JSR SCLK1		;clock it out
	nop			;The NOP instruction is 2 clock cycles,
	nop			; so maybe this is a short delay to let
	nop			; signals settle
	nop
	LDA D1PCR		;get PCR
	AND #%11011111		;$DF CB2=L (DATA=1)
	ORA #%00000010		;$02 CA2=H (CLK=0)
	STA D1PCR		;output it
	DEC CNTDN		;process next bit
	BNE OTDLP4		;loop
	LDA #$04		;set time-out
	STA D1TM2H			
OTDLP5
	LDA D1IFR		;test timer
	AND #%00100000		;$20
	BNE FLGER03		; bus timeout
	JSR SDCLK		;wait for data_in=L
	LSR A
	BCS OTDLP5		;$EEA5
	CLI
	RTS


;===============================================================
; FLGERR - Set CSTAT EEB4
;
FLGERR
	LDA #$80		; Error $80 - device not present
	.byte $2C

FLGER02
	LDA #$02		; Error $02 - read timeout
	.byte $2C

FLGER03
	LDA #$03		; Error $03 - write timeout
	JSR ISETMS1		;$FE6A set status
	CLI
	CLC
	BCC UNLSTN1


;===============================================================
; TALKSA - Send secondary address to a device commanded to TALK EECE
;
;  PARAMS   :  .A=secondary address (4-31)
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST.
;  PRE-CALLS:  TALK
;  COMMENTS :  Device must be an output device.
;===============================================================
TALKSA
	STA BSOUT     		;save secondary address to xmit
	JSR ILISTEX		; output with ATN
				;CLK=0, DATA=1, pause

CLKWAIT				; Wait for CLK
	SEI			;kill interrupts
	JSR SOUT0		;DATA=0
	JSR CLRATN		;clear ATN
	JSR SCLK1		;CLK=1

CLKWAIT1
	JSR SDCLK		;wait for CLK=0
	BCS CLKWAIT1
	CLI			
	RTS


;===============================================================
; UNTALK - Send an UNTALK command EEF6
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST.
;  PRE-CALLS:  none
;  COMMENTS :  All devices previously commanded to TALK will be
;		commanded to UNTALK.
;===============================================================
UNTALK
	JSR SCLK0		;CLK=0
	LDA D1ORAH
	ORA #%10000000		;$80 send ATN
	STA D1ORAH

	LDA #$5F		; %01011111 UNTALK
	.byte $2C		;really BIT $3FA9 to skip EF04
	
	; Fall through to UNLISTEN
;===============================================================
; UNLISTEN - Send an UNLISTEN command EF04
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Status returned according to READST.
;  PRE-CALLS:  none
;  COMMENTS :  All devices previously commanded to LISTEN will
;		be commanded to UNLISTEN.
;===============================================================
UNLISTEN
	LDA #$3F		; %00111111 UNLISTEN
	JSR LSNOIDLE		; send UNLSTN; skip idle test

UNLSTN1
	JSR CLRATN		;clear ATN

UNLSTN2
	TXA			
	LDX #$0B		;pause loop (40ms)

IUNLP1
	DEX			
	BNE IUNLP1		; pause
	TAX			
	JSR SCLK1		;CLK=1
	JMP SOUT1		;DATA=1 EXIT


;===============================================================
; CLRCH - Restore default I/O F3F3
;
;  PARAMS   :  .A=unused
;	       .X=unused
;	       .Y=unused
;  RETURNS  :  Nothing
;  PRE-CALLS:  None
;  COMMENTS :  None
;===============================================================
CLRCH
	LDX #$03		; see if device is output device
	CPX OUTDEV
	BCS CLR1		; No. See if it's an input device then.
	JSR UNLISTEN		; Yes. Send unlisten command to IEC

CLR1				; Not an output device. Maybe it's an input
	CPX INDEV
	BCS CLR2		; No. Restore default devices
	JSR UNTALK		; Yes. Send untalk command to IEC

CLR2	STX OUTDEV
	LDA #$00
	STA INDEV		;restore default I/O devices
	RTS


;===============================================================
; IECLOAD - Load RAM from device
;
;  PARAMS   :  .A = 0 (load) or 1 (verify)
;	       If a "relocated load" is required, set:
;		 .X = LSB of "from" address
;		 .Y = MSB of "from" address
;	       Otherwise, set .X/.Y to $ff for the default
;		starting address (MEMBOT). For SA 0-2, the address
;		is set from the file header. For SA 3, .X/.Y
;		required to be set.
;  RETURNS  :  .X/.Y is LSB/MSB of RAM load address
;  PRE-CALLS:  MEMBOT, SETLFS, SETNAM
;  COMMENTS :  Removed check for STOP key
;===============================================================
IECLOAD
	STX MEMUSS		;save load location from call
	STY MEMUSS+1
	STA IOFLG2		;load/verify flag
	LDA #$00
	STA CSTAT		;clear ST 
	LDA CHANNL		;get device
	CMP #$07		; LOADING from IEC drives allowed
	BCS SERLOAD
	JMP IOERMS9		;exit through "ILLEGAL DEVICE" error

SERLOAD
	LDY FNMLEN		;get filename length
	BNE SERLO_S1		;F563 filename required on IEEE
	JMP IOERMS8		;$F793 "FILENAME MISSING" error

SERLO_S1				
	JSR SSADR		;$E4BC print "Searching"
	LDA #$60
	STA SECADR		;set default SA to 0
	JSR SENDSA		;send it
	LDA CHANNL		;get device
	JSR TALK		;command it to talk
	LDA SECADR		;get SA
	JSR TALKSA		;send SA for talk
	JSR ACPTR		;get char
	STA EAL			;save it as start address L
	LDA CSTAT		;status
	LSR A
	LSR A
	BCS SERLDEX		;$F5C7 timeout? Yes, error
	JSR ACPTR		;get next char
	STA EAL+1		;save as start address H
	JSR SLDPCH		;$E4C1 print "Loading" msg and set load
				;address based on file type
SERLDLP				
	LDA #%11111101		;$FD timeout bit
	AND CSTAT		;clear it
	STA CSTAT		;save ST
;	JSR STOP		;check for STOP key
;	BNE SERLO_S2		;F598 not pressed, continue
;	JMP SAVEXIT1		;$F6CE close file and exit

SERLO_S2	
	JSR ACPTR		;get next char - program byte
	TAX			;save it
	LDA CSTAT		;check status
	LSR A
	LSR A
	BCS SERLDLP		;$F58A error, interrupt process
	TXA			;restore char
	LDY IOFLG2		;check load/verify flag
	BEQ SERLD1		;$F5B3 =0, load
	LDY #$00		;verify comparison
	CMP (EAL),Y
	BEQ SERLD2		;$F5B5 match, continue
	LDA #$10		;not a match, error
	JSR ISETMS1		; $FE6A set status
	.byte $2C		;bit $ae91
SERLD1				
	STA (EAL),Y		;save byte
SERLD2				
	INC EAL			;increment address
	BNE SERLO_S3		;$F5BB
	INC EAL+1			

SERLO_S3				
	BIT CSTAT		;test for EOF
	BVC SERLDLP		;$F58A not EOF, loop
	JSR UNTALK		;EOF, send untalk
	JSR SERSAV1A		;close file
	BCC LOADEX		;$F641 exit no error
SERLDEX				
	JMP IOERMS4		;$F787 "FILE NOT FOUND" error
LOADEX				
	CLC			
	LDX EAL			;return program end address 
	LDY EAL+1			
	RTS			


;===============================================================
; IECSAVE - Save RAM to device
;
;  PARAMS   :  .A = offset to Z-page pointer containing starting
;		    address (calculated as TAX/LDA USRPOK,X). So, 
;		    if USRPOK is at $14 and word pointer is at
;		    $17 (OSSTAR), set .A with $03
;	       .X = LSB of "to" address
;	       .Y = MSB of "to" address
;	       "From" address set by calling MEMBOT with .X/.Y of
;		starting location (MEMBOT sets OSSTAR)
;  RETURNS  :  nothing
;  PRE-CALLS:  MEMBOT, SETLFS, SETNAM
;  COMMENTS :  Removed check for STOP key
;===============================================================
IECSAVE
	STX EAL			;copy "save to" address
	STY EAL+1
	TAX
	LDA USRPOK,X
	STA STAL		;save start address L
	LDA USRVEC,X
	STA STAL+1		;save start address H
	LDA CHANNL		;get device
	CMP #$07		; SAVING to IEC drives allowed
	BCS SERSAV
	JMP IOERMS9		;$F796 "ILLEGAL DEVICE" error
SERSAV
	LDA #$61		;SA=1
	STA SECADR		;set it
	LDY FNMLEN		;get filename length
	BNE SERSAV_S1		;$F69D not 0, continue
	JMP IOERMS8		;$F793 "FILENAME MISSING" error

SERSAV_S1
	JSR SENDSA		;send filename
	JSR SAVEMS		;print "Saving" message
	LDA CHANNL		;get device
	JSR LISTEN		;command it to listen EE17
	LDA SECADR		;get SA
	JSR SECOND		;send it
	LDY #$00
	JSR RSTTPP		;save start address to SAL
	LDA SAL
	JSR CIOUT		;send start address L...
	LDA SAL+1
	JSR CIOUT		;...and start address H

SERSAVLP
	JSR CKWRPT		;reached end yet?
	BCS SERSAV1		;$F6D7 yes, go to ready
	LDA (SAL),Y		;get program byte
	JSR CIOUT		;send it

;	JSR STOP		; SAVE abort test
;	BNE SAVEXIT2
;	JSR SERSAV1A
;	LDA #$00
;	SEC
;	RTS

SAVEXIT2
	JSR INCRDP		;$FD1B bump current address
	BNE SERSAVLP		;$F6BC continue saving
SERSAV1
	JSR UNLISTEN		;send unlisten

SERSAV1A
	BIT SECADR
	BMI SERSAVRC
	LDA CHANNL		;get device
	JSR LISTEN		;command it to listen
	LDA SECADR		;get SA
	AND #%11101111		;$EF twiddle some bits
	ORA #%11100000		;$E0
	JSR SECOND		;send SA
	JSR UNLISTEN		;send unlisten 

SERSAVRC
	CLC
	RTS			;return clear


;***************************************************************
;===============================================================
; Utility Routines
;===============================================================
;***************************************************************

;===============================================================
; SOUT1 - Serial data output "1" ("false") E4A0
;
SOUT1
	LDA D1PCR		;load PCR
	AND #%11011111		;$DF CB2=L
	STA D1PCR		;save change
	RTS
		

;===============================================================
; SOUT0- Serial data output "0" ("true") E4A9
;
SOUT0
	LDA D1PCR		;load PCR
	ORA #%00100000		;$20 CB2=H
	STA D1PCR		;save change
	RTS			


;===============================================================
; SDCLK - Get SDCLK status E4B2
;	Returns status of PA.0 (CLK_IN) in CY
;
SDCLK
	LDA D1ORAH		;load register
	CMP D1ORAH		;any change?
	BNE SDCLK		;yes (unstable), loop
	LSR A			;shift PA.0 (clk_in) to CY
	RTS			


;===============================================================
; SCLK1 - Set Serial CLK_OUT "1" ("false") EF84
;
SCLK1
	LDA D1PCR		;PCR
	AND #%11111101		;$FD CA2=L
	STA D1PCR		;save it
	RTS			


;===============================================================
; SCLK0 - Set Serial CLK_OUT "0" ("true") EF8D
;
SCLK0
	LDA D1PCR		;PCR
	ORA #%00000010		;$02 CA2=H
	STA D1PCR		;save it
	RTS			


;===============================================================
; DDELAY - Delay 1ms EF96
;	Used only by LISTEN
;
DDELAY
	LDA #$04		;set time-out counter
	STA D1TM2H		; IFR cleared on write (was D2)

DLYLOOP
	LDA D1IFR		;IFR
	AND #%00100000		;Timer2 time-out flagged?
	BEQ DLYLOOP		;no, loop
	RTS			


;===============================================================
; SBIDLE - RS232 idle test F160
;	Used only by IEC_LISTEN
;
SBIDLE

;2003/06/06 -- added
;	Theory behind returning with no idle test being performed is
;	that the former RS232/User Port functions on VIA1 were replaced
;	by the VIA2 IEC functions for T1 (IRQ pulse; not implemented)
;	and T2 (IEC timing). WORKS.
	RTS


;===============================================================
; FIND - Look for logical file number in open-file table F3CF
;	On entry to FIND1, .A=file#			
;	On exit, .X=offset in file table			
;
FIND
	LDA #$00			
	STA CSTAT		;clear status
	TXA		
FIND1
	LDX COPNFL		;get #of open files

FINDLOOP
	DEX			
	BMI FEXIT		;reached 0, then exit
	CMP FILTBL,X		;is this the one?
	BNE FINDLOOP		;$F3D6 no, try again
FEXIT
	RTS			;return


;===============================================================
; RSTTPP - Reset memory pointers FBD2
;
RSTTPP
	LDA STAL+1
	STA SAL+1
	LDA STAL
	STA SAL
	RTS


;===============================================================
; CKWRPT - Check read/write pointer for end address FD11
;
CKWRPT
	SEC
	LDA SAL			;SAL is current address
	SBC EAL			;EAL is end address
	LDA SAL+1
	SBC EAL+1
	RTS


;===============================================================
; INCRDP - Increment memory read/write pointer FD1B
;
INCRDP
	INC SAL
	BNE INCRSK
	INC SAL+1
INCRSK
	RTS


;===============================================================
; SSADR - Get SECADR patch for LOAD/VERIFY E4BC
;
SSADR
	LDX SECADR		;get secondary address
	JMP SRCHMS		;print "Searching..."


;===============================================================
; SLDPCH - Relocated patch for serial LOAD/VERIFY E4C1
;
SLDPCH
	TXA
	BNE SLDEXIT		;load location not set in LOAD call, so
				;continue with load

	LDA MEMUSS		;get specified load address from call...
	STA EAL			;and save as program start address
	LDA MEMUSS+1
	STA EAL+1

SLDEXIT	
	JMP LOADMS		;print "Loading"


;===============================================================
; FLATRB - Set file values F3DF
;	On entry, .X = offset in the file tables
;
FLATRB
	LDA FILTBL,X
	STA LOGFIL		;get file#
	LDA DEVTBL,X
	STA CHANNL		;get device
	LDA SECATB,X
	STA SECADR		;get SA
	RTS			


;===============================================================
; SENDSA - Send secondary address F495
;	Called by OPEN, LOAD, SAVE
;
SENDSA
	LDA SECADR		;get SA
	BMI SNDSARC		;$F4C5 neg, exit
	LDY FNMLEN		;get filename length
	BEQ SNDSARC		;$F4C5 0, error	
	LDA CHANNL		;get device
	JSR LISTEN		;command it to listen
	LDA SECADR		;get SA
	ORA #%11110000		;$F0
	JSR SECOND		;$EEC0	sent it
	LDA CSTAT		;status
	BPL SENDSA1		;$F4B2 OK, continue
; *****
;	PLA			;error, set RTS for caller's caller
;	PLA
	JMP IOERMS5		;$F78A "DEVICE NOT PRESENT" error

SENDSA1	
	LDA FNMLEN		;get filename length
	BEQ SNDSARU		;$F4C2 len=0, send unlisten and exit
	LDY #$00

SENDSALP
	LDA (FNPTR),Y		;send filename to IEEE
	JSR CIOUT		;send char
	INY
	CPY FNMLEN
	BNE SENDSALP		;$F4B8 loop

SNDSARU
	JSR UNLISTEN		;done, send unlisten command

SNDSARC
	CLC
	RTS


;===============================================================
; SRCHMS - Print "Searching for [filename]" F647
;
; FIXME -- IEC loading and saving must have filename, so "FOR"
;		must always be printed (this is a leftover from
;		tape processing).
;
SRCHMS
	LDA CMDMOD		;direct mode?
	BPL SRCHEX		;$F669 no, exit
	lda #<LAB_SRCM		; "Searching for "
	ldy #>LAB_SRCM
	jsr LAB_18C3		; print string

; FLNMMS - Print only filename
; not needed for save
FLNMMS				
	LDA FNMLEN		;get filename length
	BEQ SRCHEX		;$F669 no filename, exit
	lda #<FNPTR
	ldy #>FNPTR		; make sure it's null-term
	jsr LAB_18C3		; print string

SRCHEX
	RTS			;exit


;===============================================================
; LOADMS - Print "Loading" or "Verifying" F66A
;	jsr LAB_18C3		; print string
LOADMS

	LDA IOFLG2		;check load/verify flag-0=load
	BEQ DOLMESG		;$F672 load, print message

	lda #<LAB_VFMS		; "Verifying"
	ldy #>LAB_VFMS
	jmp DOMESG

DOLMESG
	lda #<LAB_LDMS		; "Loading"
	ldy #>LAB_LDMS

DOMESG
	jsr LAB_18C3
	jsr LAB_CRLF
	clc
	rts


;===============================================================
; SAVEMS - Print "Saving [filename]"  F728
;
SAVEMS
	pha
	phy
	LDA CMDMOD		;direct mode?
	BPL SVRET		;no, exit

	lda #<LAB_SVMS		; "Saving" low
	ldy #>LAB_SVMS		; "Saving" high
	jsr LAB_18C3
	lda FNPTR		; filename low
	ldy FNPTR+1		; filename high
	jsr LAB_18C3		; print string
SVRET
	ply
	pla
	RTS


;===============================================================
;IOERMS - I/O Error Messages F77E
;
;  PARAMS   :  .A = unused. Error 0 is "Break Error"
;	       .X = unused
;	       .Y = unused
;  RETURNS  :  nothing
;  PRE-CALLS:  none
;  COMMENTS :  Should only be called during a file-oriented operation
;		since it calls CLRCH to clear the I/O channel of errors.
;
IOERMS1
	LDX #$28		;Too Many Files
	.byte $2C	
IOERMS2
	LDX #$2A		;File Already Open 
	.byte $2C 
IOERMS3
	LDX #$2C		;File Not Open
	.byte $2C 
IOERMS4
	LDX #$2E		;File Not Found
	.byte $2C 
IOERMS5
	LDX #$30		;Device Not Present
	.byte $2C 
IOERMS6
	LDX #$32		;Not Input File
	.byte $2C 
IOERMS7
	LDX #$34		;Not Output File
	.byte $2C 
IOERMS8
	LDX #$36		;Missing File Name
	.byte $2C 
IOERMS9
	LDX #$38		;Illegal Device 
	.byte $2C

	JSR CLRCH		; clear I/O channel; uses A
	jmp LAB_XERR		; flag error
	
AA_end_iec
