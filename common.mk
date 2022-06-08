# Common set of macros to enable Linux versus Windows portability.
ifeq ($(OS),Windows_NT)
    / = $(strip \)
    OPHIS = "C:/Program Files (x86)/Ophis/ophis.exe"
    PY65MON = "%HOMEPATH%\AppData\Local\Programs\Python\Python38\Scripts\py65mon"
    PYTHON = python
    RM = del /f /q
    RMDIR = rmdir /s /q
    SHELL_EXT = bat
    TOUCH = type nul >
else
    / = /
    OPHIS = ~/Ophis-2.1/ophis
    PY65MON = ~/.local/bin/py65mon
    PYTHON = python3
    RM = rm -f
    RMDIR = rm -rf
    SHELL_EXT = sh
    TOUCH = touch
endif
