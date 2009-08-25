JS2SCHEME_DIR = ..
SOURCE_DIR = .
SOURCES =  repl-scm.scm repl.js
TARGET =  repl

all: $(TARGET).exe

include $(JS2SCHEME_DIR)/build/Makefile.include

.PHONY: all clean

clean:
	rm -f *.o $(TARGET).exe repl.scm $(TARGET).exe.scm $(TARGET).afile
