# FigForth
The 6502 Fig Forth ported to the Ophis assembler with Py65Mon loading module.

Overview
======
The purpose of this project is to port FIG FORTH to the Ophis assembler,
and create a wrapper module to provide loading and I/O routines.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* .gitignore - prevents target files from getting checked into git.
* asm2oph.py - Python script to convert from ASM to Ophis format.
* Fig6502.oph - the ported source after some tweaking.
* LICENSE - MIT license file.
* Makefile - makefile to issue all of the commands.
* Py65Main.asm - wrapper file to allow executon with Py65Mon.
* README.md - this file.

Usage examples:
======
* make debug - assembles the source and runs it in py65Monbuilds the image
* make FIG6502_ASM.oph - translate original source to Ophis format.

Notes:
======
My code uses Python 3.6, but Ophis depends upon 2.6. But under Windows
Ophis includes its own copy of Python. On Linux you need to install
both
