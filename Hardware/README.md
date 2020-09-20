# Hardware
A set of common drivers for typical 6502 SBC computers.

Overview
======
The purpose of this project is to create a set of reusable modules refactored out of other projects.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* README.md - this file.
* acia.asm - 65C51 driver
* pckeyboard.asm - driver for PS/2 keyboard
* via.asm - 65C22 VIA driver
* video.asm - driver for onboard AVR video

Usage examples:
======

Notes:
======
Ophis depends upon Python 2.6. Under Windows Ophis includes its own copy of
Python. On Linux you need to install it.
