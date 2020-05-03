# Monitor
A re-write of the SBC2.7 machine language monitor using Ophis and my common data types and functions.

Overview
======
The purpose of this project is to create a machine language montior set of reusable modules refactored out of other projects.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* LICENSE - MIT license file.
* Makefile - makefile to issue all of the commands.
* README.md - this file.
* monitor.asm - code for the monitor.
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
