include ../common.mk

SOURCES =  \
	Makefile \
	README.md \
	acia.asm \
	pckeyboard.asm \
	tikeyboard.asm \
	via.asm \
	video.asm

TESTS =  \
	aciaTest \
	viaTest \
	videoTest

tests: $(TESTS)

aciaTest: tests/aciaTest.rom

viaTest: tests/viaTest.rom

videoTest: tests/videoTest.rom

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
