include ../common.mk

ROMS = \
	brainf.rom \
	brainfdtc.rom \
	brainfstc.rom \
	life.rom \
	mandelbrotMain.rom

SOURCES =  \
	brainf.asm \
	brainfdtc.asm \
	brainfstc.asm \
	life.asm \
	mandelbrot.asm \
	mandelbrotMain.asm

TESTS =  \
	mandelbrotTest

brainf.rom: brainf.asm
	${OPHIS} -l brainf.lst -m brainf.map --65c02 brainf.asm

brainfDebug: brainf.rom
	${PY65MON} -m 65C02 -r brainf.rom

brainfdtc.rom: brainfdtc.asm
	${OPHIS} -l brainfdtc.lst -m brainfdtc.map --65c02 brainfdtc.asm

brainfdtcDebug: brainfdtc.rom
	${PY65MON} -m 65C02 -r brainfdtc.rom

brainfstc.rom: brainfstc.asm
	${OPHIS} -l brainfstc.lst -m brainfstc.map --65c02 brainfstc.asm

brainfstcDebug: brainfstc.rom
	${PY65MON} -m 65C02 -r brainfstc.rom

brainfitc.rom: brainfitc.asm
	${OPHIS} -l brainfitc.lst -m brainfitc.map --65c02 brainfitc.asm

brainfitcDebug: brainfitc.rom
	${PY65MON} -m 65C02 -r brainfitc.rom

brainfotc.rom: brainfotc.asm
	${OPHIS} -l brainfotc.lst -m brainfotc.map --65c02 brainfotc.asm

brainfotcDebug: brainfotc.rom
	${PY65MON} -m 65C02 -r brainfotc.rom

brainfo2tc.rom: brainfo2tc.asm
	${OPHIS} -l brainfo2tc.lst -m brainfo2tc.map --65c02 brainfo2tc.asm

brainfo2tcDebug: brainfo2tc.rom
	${PY65MON} -m 65C02 -r brainfo2tc.rom

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
