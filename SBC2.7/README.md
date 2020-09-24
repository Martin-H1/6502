# SBC2.7
Rich Cini's additions to Daryl Rictor's 6502 SBC2 ROM. Originally
built using TASS.EXE inside a Dos box using the commands:
mount c c:\...\SBC2.7
c:
tass /c SBC.ASM
But that was too annoying, so I ported it to Ophis using the translator
I created for FigForth. I then diffed the resultant binaries to ensure
they were byte for byte identical. The formatting was messed up in a
few places, and the extension changed to oph, but the resulting binary
is identical.
---Rich's Readme.txt contents---
Here are my SBC-2 Operating System Source Files.

They include:

sbc.asm   - The build file that includes all the other files in the proper order
            (you would assemble this file with TASS)

reset.asm - reset and IRQ initialization
sbcos.asm - monitor & minassembler
acia.asm - 65c51 init and IO support for monitor
via1.asm - via # 1 init
via2.asm - via # 2 init
basic.asm - Enhanced Basic 2.22 (modified slightly from Lee's distribution)
basldsv.asm - load and save routines for EHBASIC.
upload.asm - XMODEM/CRC & Intel-Hex File Upload support

Compiled using TASS using the following command line:
TASS /c /lsbc.lbl sbc.asm sbc.bin sbcos.lst

The compiler list output file from TASS is called:
sbc25.lst

The label file contains all the labels and their addresses and is called:
sbc.lbl

There is 1 compiled file included:
sbc25.rom  -  the 32k raw binary object file (only the upper 20k are used)

Good luck!

Daryl
