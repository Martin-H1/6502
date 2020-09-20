# Common
A set of common data types and functions written in 6502 assembler.

Overview
======
The purpose of this project is to create a set of reusable modules refactored out of other projects.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* LICENSE - MIT license file.
* Makefile - makefile to issue all of the commands.
* README.md - this file.
* array.asm - a dynamic array of 16 bits values. 
* conio.asm - simple console I/O routines which include redirection.
* data.asm - defines data segments and their origin address.
* heap.asm - a simple heap.
* math16.asm - word sized math routines.
* print.asm - useful print routines.
* stack.asm - small page zero stack macros and routines.
* string.asm - a simple string library.
* vectors.asm - initialization vectors for ROM images.
## tests
* arrayTest.asm - unit tests follow module name with Test.
* conioTest.asm
* heapTest.asm
* mathTest.asm
* mockConio.asm - mock routines for injecting text data.
* stringTest.asm

Usage examples:
======
* make release - assembles the source and produces a ROM image.
* make tests - runs the unit tests.

Notes:
======
Ophis depends upon Python 2.6. Under Windows Ophis includes its own copy of
Python. On Linux you need to install it.
