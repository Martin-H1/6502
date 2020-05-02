include ../common.mk

SOURCES =  \
	bigstack.asm \
	cell.asm \
	gc.asm \
	lisp65.asm

TESTS =  \
	cellTest \
	gcTest \
	lispTest

py65mon.rom: clean $(SOURCES) main-py65.asm
	${OPHIS} -l py65mon.lst -m py65mon.map --65c02 main-py65.asm

debug: py65mon.rom
	${PY65MON} -m 65C02 -r py65mon.rom

tests: $(TESTS)

bigstackTest: tests/bigstackTest.rom
	${PY65MON} -m 65C02 -r tests/bigstackTest.rom

cellTest: tests/cellTest.rom
	${PY65MON} -m 65C02 -r tests/cellTest.rom

gcTest: tests/gcTest.rom
	${PY65MON} -m 65C02 -r tests/gcTest.rom

lispTest: tests/lispTest.rom
	${PY65MON} -m 65C02 -r tests/lispTest.rom

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
