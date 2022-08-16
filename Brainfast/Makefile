include ../common.mk

ROMS = \
	brainfast.rom

SOURCES =  \
	brainfast.asm \
	conio.asm \
	data.asm \
	py65monmain.asm \
	vectors.asm

TESTS = 


brainfast.rom: brainfast.asm
	${OPHIS} -l brainfast.lst -m brainfast.map --65c02 brainfast.asm

py65mon.rom: $(SOURCES)
	${OPHIS} -l py65monmain.lst -m py65monmain.map --65c02 py65monmain.asm

release: $(ROMS)

debug: py65mon.rom
	${PY65MON} -m 65C02 -r py65mon.rom

tests: $(TESTS)

tests/%.rom : tests/%.asm $(SOURCES)
	${OPHIS} -l $<.lst -m $<.map --65c02 $<

%.rom: %.asm
	${OPHIS} -l $<.lst -m $<.map --65c02 $<

clean:
	-$(RM) *.rom
	-$(RM) *.lst
	-$(RM) *.map
	-$(RM) tests$/*.lst
	-$(RM) tests$/*.map
	-$(RM) tests$/*.rom
