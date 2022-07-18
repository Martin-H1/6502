include ../common.mk

ROMS = \
	brainf.rom \
	life.rom \
	mandelbrotMain.rom

SOURCES =  \
	brainf.asm \
	life.asm \
	mandelbrot.asm \
	mandelbrotMain.asm

TESTS =  \
	mandelbrotTest

brainf.rom: brainf.asm
	${OPHIS} -l brainf.lst -m brainf.map --65c02 brainf.asm

brainfDebug: brainf.rom
	${PY65MON} -m 65C02 -r brainf.rom

life.rom: life.asm
	${OPHIS} -l life.lst -m life.map --65c02 life.asm

lifeDebug: life.rom
	${PY65MON} -m 65C02 -r life.rom

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
