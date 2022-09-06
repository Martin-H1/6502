include ../common.mk

ROMS = \
	brainfast.rom

SOURCES =  \
	acia.asm \
	brainfast.asm \
	conio.asm \
	data.asm \
	py65monmain.asm \
	sbc27main.asm \
	vectors.asm \
	via.asm

TESTS = 

py65mon.rom: $(SOURCES)
	${OPHIS} -l py65monmain.lst -m py65monmain.map --65c02 py65monmain.asm

sbc27.rom: $(SOURCES)
	${OPHIS} -l sbc27main.lst -m sbc27main.map --65c02 sbc27main.asm

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
