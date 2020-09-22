# 6502
A collection of 6502 projects in a monorepo

Overview
======
The purpose of this project is to collect my and others 6502 projects into a single repo, and refactor code into resuable modules when possible.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* .gitignore - prevents target files from getting checked into git.
* common.mk - common make rules used by unit makefiles.
* LICENSE - MIT license file.
* README.md - this file.

Common
======
Reusable data structures and functions, including a simple BIOS.

Hardware
======
Reusable modules to interface with hardware devices such as the 65C22, 65C51, PS/2 Keyboards, etc.

FigForth
======
A port of Fig Forth for Ophis and runs under Py65Mon.

Notes:
======
Ophis depends upon Python 2.6. Under Windows Ophis includes its own copy of
Python. On Linux you need to install it.
