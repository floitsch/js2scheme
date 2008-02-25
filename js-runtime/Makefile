CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)
TARGETNAMES	= repl

RUNTIME_BGL_MODULES = Array Bool Date Function Math Number Object String \
			conversion Error Eval-env eval Arguments \
			natives object global-object globals globals-tmp \
			primitives runtime operators scope-object \
			RegExp RegExp-parse RegExp-fsm RegExp-classes

RUNTIME_BGL_INCLUDES = macros.sch

REPL_BGL_MODULES = repl

RUNTIME_OBJECTS = $(RUNTIME_BGL_MODULES:%=o/%.o)
RUNTIME_SOURCES = $(RUNTIME_BGL_MODULES:%=%.scm)
REPL_OBJECTS = $(REPL_BGL_MODULES:%=o/%.o)

OBJECTS	= $(RUNTIME_OBJECTS) $(REPL_OBJECTS)
SOURCES		= $(OBJECTS:o/%.o=%.scm)

INCLUDES	= macros.sch

AFILE		= bglafile

all: .afile o
	$(MAKE) targets
	$(MAKE) runtime-variables.sch

targets: $(TARGETNAMES) #j$(TARGETNAME)

.PHONY: build-afile clean

.afile: $(SOURCES)
	$(AFILE) $(SOURCES) > $@

extract-globals: extract-globals.scm
	$(BIGLOO) -g -o $@ $^

runtime-variables.sch: extract-globals $(RUNTIME_SOURCES) macros.sch
	cat $(RUNTIME_SOURCES) macros.sch | ./extract-globals > $@

repl: $(RUNTIME_OBJECTS) $(REPL_OBJECTS)
	$(BIGLOO) -o $@ $^ -L .. -ljs2scheme-comp

o/runtime.o: $(RUNTIME_SOURCES)
o/repl.o: $(RUNTIME_SOURCES)

o:
	mkdir o

o/eval.o: eval.scm $(RUNTIME_BIGL_INCLUDES)
	$(BIGLOO) -c -g  -L .. -library js2scheme-comp -o $@ $<

o/%.o: %.scm $(RUNTIME_BGL_INCLUDES)
	$(BIGLOO) -c -copt -g -copt -O0 -g -o $@ $<

# for js2scm-comp script
%.exe: $(RUNTIME_OBJECTS) o/%.o
	$(BIGLOO) -o $@ $^ -L .. -ljs2scheme-comp

clean:
	rm -f $(OBJECTS) $(REPL_OBJECTS) $(TARGETNAMES) .afile;
