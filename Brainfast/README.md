# Brainfast
A optimizing Brain f--k compilier written in 6502 assembly.

Overview
======
The purpose of this project is to show how a Brainf--k complier can be written
in 6502 assembly, but avoiding the use of the X and Y registers and BCD
instructions. Basically, the 6502 is Turing complete without them, and can
solve any computable problem with just the code in this repo.

Also, this code is the end product of several prior versions. Each of which
improved performance over the previous. The first was a pure interpreter.
The second was a direct thread code interpreter. The third version was a
subroutine threaded interpreter. The final version was a compiler was a
complier that produced directlt executable code.

The performance of the compiler was heads and tails faster than even the
subroutine threaded interpreter. This demonstates the cost of the JSR and
RTS programming constructs.

Prerequisites
======
* Windows or Linux with Python 3.6, Ophis, and Py65Mon installed.
* Git to fetch these files.

Files
======
* acia.asm - a wrapper around the 6551 ACIA.
* brainfast.asm -the interpreter.
* conio.asm - a simple string output subroutine.
* data.asm - program section definitions.
* fibonacci.bf - a sample BF program.
* golden.bf
* helloworld.bf -
* Makefile - used to build object code from source.
* py65monmain.asm - main for debugging purposes.
* sbc27main.asm - main for real hardware.
* sierpinski.bf - another sample BF program.
* vectors.asm - initialization vectors for ROM creation.
* via.asm - a wrapper around the 6522.
* README.md - this file.

Notes:
======
Ophis depends upon Python 2.6. Under Windows Ophis includes its own copy of
Python. On Linux you need to install it.
