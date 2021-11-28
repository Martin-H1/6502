include ../common.mk

SOURCES =  \
	bios.asm \
	biosImpl.asm \
	monitor.asm \
	vectors.asm

TESTS =  \
	monitorTest

py65mon.rom: clean $(SOURCES) main-py65.asm
	${OPHIS} -l py65mon.lst -m py65mon.map --65c02 main-py65.asm

sbc27.rom: clean $(SOURCES)
	${OPHIS} -l sbc27.lst -m sbc27.map --65c02 sbc.oph

debug: py65mon.rom
	${PY65MON} -m 65C02 -r py65mon.rom

tests: $(TESTS)

monitorTest: tests/monitorTest.rom
	${PY65MON} -m 65C02 -r tests/monitorTest.rom

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
