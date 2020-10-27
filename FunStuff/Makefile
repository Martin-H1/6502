include ../common.mk

ROMS = \
	mandelbrotMain.rom

SOURCES =  \
	mandelbrot.asm \
	mandelbrotMain.asm

TESTS =  \
	mandelbrotTest

py65mon.rom: clean $(SOURCES) main-py65.asm
	${OPHIS} -l py65mon.lst -m py65mon.map --65c02 main-py65.asm

debug: py65mon.rom
	${PY65MON} -m 65C02 -r py65mon.rom

tests: $(TESTS)

mandelbrotTest: tests/mandelbrotTest.rom
	${PY65MON} -m 65C02 -r tests/mandelbrotTest.rom

tests/%.rom : tests/%.asm $(SOURCES)
	${OPHIS} -l $<.lst -m $<.map --65c02 $<

%.rom: %.asm
	${OPHIS} -l $<.lst -m $<.map --65c02 $<

release: $(ROMS)

clean:
	-$(RM) *.rom
	-$(RM) *.lst
	-$(RM) *.map
	-$(RM) tests$/*.lst
	-$(RM) tests$/*.map
	-$(RM) tests$/*.rom
