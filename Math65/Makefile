include ../common.mk

SOURCES =  \
	bcdmath.asm \
	fmath32.asm \
	fmath745.asm \
	itrig16.asm

TESTS =  \
	bcdmathTest \
	fmath745Test \
	fmathTest \
	itrig16Test

tests: $(TESTS)

bcdmathTest: tests/bcdmathTest.rom
	${PY65MON} -m 65C02 -r tests/bcdmathTest.rom

fmathTest: tests/fmathTest.rom
	${PY65MON} -m 65C02 -r tests/fmathTest.rom

fmath745Test: tests/fmath745Test.rom
	${PY65MON} -m 65C02 -r tests/fmath745Test.rom

itrig16Test: tests/itrig16Test.rom
	${PY65MON} -m 65C02 -r tests/itrig16Test.rom

tests/%.rom : tests/%.asm $(SOURCES)
	${OPHIS} -l $<.lst -m $<.map --65c02 $<

release:

clean:
	-$(RM) *.rom
	-$(RM) tests$/*.rom
	-$(RM) tests$/*.lst
	-$(RM) tests$/*.map
