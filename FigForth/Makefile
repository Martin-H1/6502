# Common set of macros to enable Linux versus Windows portability.
ifeq ($(OS),Windows_NT)
    OPHIS = "C:/Program Files (x86)/Ophis/ophis.exe"
    PY65MON = "%HOMEPATH%\AppData\Roaming\Python\Python36\Scripts\py65mon"
    PYTHON = python
    RM = del /f /q
    RMDIR = rmdir /s /q
    SHELL_EXT = bat
    TOUCH = type nul >
else
    OPHIS = ~/Ophis-2.1/ophis
    PY65MON = python 
    PYTHON = python
    RM = rm -f
    RMDIR = rm -rf
    SHELL_EXT = sh
    TOUCH = touch
endif

SOURCES =  \
	Fig6502.oph

PY65MON_SOURCES = \
	$(SOURCES) \
	Py65Main.asm

py65mon.rom: clean $(SOURCES)
	$(OPHIS) --65c02 -l py65mon.lst -m py65mon.map Py65Main.oph

debug: py65mon.rom
	$(PY65MON) -m 65C02 -r py65mon.rom

# This target regenerates the Ophis format file from the original FIG source.
Fig6502_asm.oph: FIG6502.ASM asm2oph.py
	$(PYTHON) asm2oph.py FIG6502.ASM > Fig6502_asm.oph

clean:
	-$(RM) *.bin
	-$(RM) *.lst
	-$(RM) *.map
	-$(RM) *.rom
	-$(RM) *.tmp
