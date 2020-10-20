# Fun Stuff
A collection of fun odds and ends in 6502 assembly.

Overview
======
This diectory contains more stand alone projects that are for fun.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* .gitignore - prevents target files from getting checked into git.
* LICENSE - MIT license file.
* Makefile - makefile to issue all of the commands.
* README.md - this file.
* mandelbrot.asm - a 6502 assembly mandlebrot rendering program.
* main-py65.asm - main for debug testing.

Usage examples:
======
* make debug - assembles the source and runs it in py65Mon.
* make release - assembles the source and produces a ROM image.
* make tests - runs the unit tests.

Notes:
======
Ophis depends upon Python 2.6. Under Windows Ophis includes its own copy of
Python. On Linux you need to install it.
