BIGLOO = bigloo
TARGETNAMES	= test repl

RUNTIME_BGL_MODULES = Array Bool Date Function Math Number Object String conversion exceptions\
			natives object global-object globals globals-tmp primitives runtime operators

RUNTIME_BGL_INCLUDES = macros.sch

TEST_BGL_MODULES = test
REPL_BGL_MODULES = repl

RUNTIME_OBJECTS = $(RUNTIME_BGL_MODULES:%=o/%.o)
RUNTIME_SOURCES = $(RUNTIME_BGL_MODULES:%=%.scm)
TEST_OBJECTS = $(TEST_BGL_MODULES:%=o/%.o)
REPL_OBJECTS = $(REPL_BGL_MODULES:%=o/%.o)

OBJECTS	= $(RUNTIME_OBJECTS) $(TEST_OBJECTS)
SOURCES		= $(OBJECTS:o/%.o=%.scm)

INCLUDES	= macros.sch

AFILE		= bglafile

all: .afile o
	$(MAKE) targets

targets: $(TARGETNAMES) #j$(TARGETNAME)

.PHONY: build-afile clean

.afile: $(SOURCES)
	$(AFILE) $(SOURCES) > $@

extract-globals: extract-globals.scm
	$(BIGLOO) -g -o $@ $^

runtime-variables.sch: extract-globals $(RUNTIME_SOURCES)
	cat $(RUNTIME_SOURCES) | ./extract-globals > $@

test: $(RUNTIME_OBJECTS) $(TEST_OBJECTS)
	$(BIGLOO) -o $@ $^ -L .. -ljs2scheme-comp

repl: $(RUNTIME_OBJECTS) $(REPL_OBJECTS)
	$(BIGLOO) -o $@ $^ -L .. -ljs2scheme-comp

o:
	mkdir o

o/globals.o: globals.scm $(RUNTIME_BIGL_INCLUDES)
	$(BIGLOO) -c -g  -L .. -library js2scheme-comp -o $@ $<

o/%.o: %.scm $(RUNTIME_BGL_INCLUDES)
	$(BIGLOO) -c -g -o $@ $<

clean:
	rm -f $(OBJECTS) $(TEST_OBJECTS) $(REPL_OBJECTS) $(TARGETNAMES) .afile;
