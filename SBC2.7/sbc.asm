;*********************************************************************
;
;  2005/09/04	RAC	Synched with SBCOS 5/30/05 release
;  2003/09/05	RAC	Reordered system code to enable user to remove
;			EhBASIC easily and to minimize memory holes
;			to maximize user ROM space.
; Assumed Memory Map
; 0000-7EFF - RAM (32K minus 256 bytes of I/O) 
; 7F00-7F4F - Five unused decoded I/O Blocks (16 bytes each) 
; 7F50-7F5F - VIA1 (16 bytes) - SerialIEC
; 7F60-7F6F - VIA2 (16 bytes) - .A=video
; 7F70-7F7F - 65C51 (16 bytes) 
; 7F80-7FFF - undecoded I/O blocks (128 bytes) 
; 8000-FFFF - EPROM (32K) 
;  8000-AFFF -- unused (12.3k)			 } 
;  B000-D8FF -- EhBASIC (10.5k)  		 } 22.8k contiguous space
;  D900-E5FF -- CBM IEC drivers and DOS (3.25k)  } 26.1k contiguous space
;  E600-FFFF -- System BIOS, monitor and assembler (6.5k)
;
; Assumed RAM Map
; Page 0 Z-page
; Page 1 Processor Stack
; Page 2 Free above $026f
; Page 3 Reserved for system monitor and related buffers
; Page 4 Start of free RAM
;
; Compile with TASS: tass /c sbc.asm sbc.bin sbc.lst
;

	*= $8002			; create exact 32k bin image
	.byte "A0",$C3,$C2,$CD		; ROM signature

; System code
	*=$A800	
	.include basic23.asm		; $A800 -  EhBASIC
	.include basldsv.asm	   	; EhBASIC load & save support for sim
	.include cbm_iec.asm		; Serial IEC routines and DOS
; ends around $DC0B


; this is fixed in memory for now
	*=$E500
	.include sbcos.asm        	; $E500 - F737  OS

	*=$F800
	.include VIA1.asm		; $F800 - F901 (without video), F937 (with video)
	.include VIA2.asm
	.include ACIA1.asm
	.include nmi.asm		; NMI code
	.include video.asm		; video BIOS for integrated video on v2.7

	*=$FA00
	.include upload.asm        	; $FA00 - FEFE Intel Hex & Xmodem-CRC uploader

	*=$FF00
	.include reset.asm        	; $FF00  Reset & IRQ handler


