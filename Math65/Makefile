SOURCES =  \
	fmath745.asm \
	macros.asm \
	print.asm \
	stack.asm \
	vectors.asm

TESTS =  \
	bcdmathTest \
	fmath745Test \
	fmathTest

tests: $(TESTS)

bcdmathTest: tests/bcdmathTest.rom
	${PY65MON} -m 65C02 -r tests/bcdmathTest.rom

fmathTest: tests/fmathTest.rom
	${PY65MON} -m 65C02 -r tests/fmathTest.rom

fmath745Test: tests/fmath745Test.rom
	${PY65MON} -m 65C02 -r tests/fmath745Test.rom

tests/%.rom : tests/%.asm $(SOURCES)
	${OPHIS} -l $<.lst --65c02 $<

release:

clean:
	-$(RM) *.rom
	-$(RM) tests/*.rom
	-$(RM) tests/*.lst
