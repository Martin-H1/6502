# Monitor
This project includes a BIOS (accessed via a jump table), and a ROM monitor.
It will be losely a re-write of the SBC2.7 monitor using Ophis.

Overview
======
The purpose is to allow user programs to be written using the BIOS interface
and be portable between machines that implement that BIOS. One BIOS will be
a PyMon debug BIOS, and another for real hardward.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* LICENSE - MIT license file.
* Makefile - makefile to issue all of the commands.
* README.md - this file.
* assembler.asm - embedded assembler for the monitor.
* bios.asm - aliases to the jump table entry points for consumers.
* biosImpl.asm - implmentation of BIOS functions.
* monitor.asm - code for the monitor.
* vectors.asm - high ROM jump table and reset vectors.
## tests
* monitorTest.asm - unit tests follow module name with Test.

Usage examples:
======
* make debug - assembles the source and produces a ROM image.
* make release - assembles the source and produces a ROM image.
* make tests - runs the unit tests.

Notes:
======
Ophis depends upon Python 2.6. Under Windows Ophis includes its own copy of
Python. On Linux you need to install it.
