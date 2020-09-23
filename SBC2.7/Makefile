include ../common.mk

SOURCES =  \
	acia1.oph \
	basic23.oph \
	basldsv.oph \
	cbm_iec.oph \
	nmi.oph \
	reset.oph \
	sbc.oph \
	sbcOS.oph \
	upload.oph \
	via1.oph \
	via2.oph \
	video.oph

py65mon.rom: clean $(SOURCES) main-py65.asm
	${OPHIS} -l py65mon.lst -m py65mon.map --65c02 main-py65.asm

sbc27.rom: clean $(SOURCES)
	${OPHIS} -l sbc27.lst -m sbc27.map --65c02 sbc.oph

debug: py65mon.rom
	${PY65MON} -m 65C02 -r py65mon.rom

release: sbc27.rom

clean:
	-$(RM) *.rom
	-$(RM) *.lst
	-$(RM) *.map
