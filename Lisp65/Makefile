# Common set of macros to enable Linux versus Windows portability.
ifeq ($(OS),Windows_NT)
    / = $(strip \)
    OPHIS = "C:/Program Files (x86)/Ophis/ophis.exe"
    PY65MON = "%HOMEPATH%\AppData\Local\Programs\Python\Python38\Scripts\py65mon"
    PYTHON = python
    RM = del /f /q
    RMDIR = rmdir /s /q
    SHELL_EXT = bat
    TOUCH = type nul >
else
    / = /
    OPHIS = ~/Ophis-2.1/ophis
    PY65MON = python
    PYTHON = python
    RM = rm -f
    RMDIR = rm -rf
    SHELL_EXT = sh
    TOUCH = touch
endif

SOURCES =  \
	array.asm \
	bigstack.asm \
	cell.asm \
	conio.asm \
	data.asm \
	gc.asm \
	heap.asm \
	lisp65.asm \
	math16.asm \
	print.asm \
	stack.asm \
	string.asm \
	vectors.asm

TESTS =  \
	arrayTest \
	cellTest \
	conioTest \
	gcTest \
	heapTest \
	lispTest \
	mathTest \
	stringTest

py65mon.rom: clean $(SOURCES) main-py65.asm
	${OPHIS} -l py65mon.lst -m py65mon.map --65c02 main-py65.asm

debug: py65mon.rom
	${PY65MON} -m 65C02 -r py65mon.rom

tests: $(TESTS)

arrayTest: tests/arrayTest.rom
	${PY65MON} -m 65C02 -r tests/arrayTest.rom

bigstackTest: tests/bigstackTest.rom
	${PY65MON} -m 65C02 -r tests/bigstackTest.rom

cellTest: tests/cellTest.rom
	${PY65MON} -m 65C02 -r tests/cellTest.rom

conioTest: tests/conioTest.rom
	${PY65MON} -m 65C02 -r tests/conioTest.rom

gcTest: tests/gcTest.rom
	${PY65MON} -m 65C02 -r tests/gcTest.rom

heapTest: tests/heapTest.rom
	${PY65MON} -m 65C02 -r tests/heapTest.rom

lispTest: tests/lispTest.rom
	${PY65MON} -m 65C02 -r tests/lispTest.rom

mathTest: tests/mathTest.rom
	${PY65MON} -m 65C02 -r tests/mathTest.rom

stringTest: tests/stringTest.rom
	${PY65MON} -m 65C02 -r tests/stringTest.rom

tests/%.rom : tests/%.asm $(SOURCES)
	${OPHIS} -l $<.lst -m $<.map --65c02 $<

release:

clean:
	-$(RM) *.rom
	-$(RM) *.lst
	-$(RM) *.map
	-$(RM) tests$/*.lst
	-$(RM) tests$/*.map
	-$(RM) tests$/*.rom
