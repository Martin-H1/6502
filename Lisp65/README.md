# Lisp65
A Lisp interpreter written in 6502 assembler.

Overview
======
The purpose of this project is to create a simple interpreter for the Scheme Lisp dialect.

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
* array.asm - a dynamic array of 16 bits values. 
* bigstack.asm - a 16 bit stack we may need for deep evaluation.
* cell.asm - a single link list of cells.
* conio.asm - simple console I/O routines which include redirection.
* data.asm - defines data segments and their origin address.
* env.bat - sets up environment variables to point to Ophis and Py65mon
* env.sh - sets up environment variables to point to Ophis and Py65mon
* gc.asm - stubs for a garbage collector.
* heap.asm - a simple heap.
* lisp65.asm - the scheme interpreter.
* main-py65.asm - main for debug testing.
* math16.asm - word sized math routines.
* print.asm - useful print routines.
* stack.asm - small page zero stack macros and routines.
* string.asm - a simple string library.
* vectors.asm - initialization vectors for ROM images.
## tests
* arrayTest.asm - unit tests follow module name with Test.
* bigstackTest.asm
* cellTest.asm
* conioTest.asm
* gcTest.asm
* heapTest.asm
* lispTest.asm
* mathTest.asm
* mockConio.asm - mock routines for injecting text data.
* stringTest.asm

Usage examples:
======
* make debug - assembles the source and runs it in py65Mon.
* make release - assembles the source and produces a ROM image.
* make tests - runs the unit tests.

Notes:
======
Ophis depends upon Python 2.6. Under Windows Ophis includes its own copy of
Python. On Linux you need to install it.
