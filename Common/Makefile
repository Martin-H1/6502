include ../common.mk

SOURCES =  \
	array.asm \
	conio.asm \
	data.asm \
	heap.asm \
	math16.asm \
	print.asm \
	stack.asm \
	string.asm \
	vectors.asm

TESTS =  \
	conioTest \
	heapTest \
	mathTest \
	stringTest

tests: $(TESTS)

arrayTest: tests/arrayTest.rom
	${PY65MON} -m 65C02 -r tests/arrayTest.rom

conioTest: tests/conioTest.rom
	${PY65MON} -m 65C02 -r tests/conioTest.rom

heapTest: tests/heapTest.rom
	${PY65MON} -m 65C02 -r tests/heapTest.rom

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
