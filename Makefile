BIGLOO = bigloo
TARGETNAMES	= js-obfuscator js-pp js2scheme

OBFUSCATOR_BGL_MODULES	= fun-bindings nodes protobject var js-obfuscator js-out \
		 verbose lexer parser symbol statements obfuscate-ids simplify \
		 html

PP_BGL_MODULES	= nodes protobject js-pp js-out var \
		 verbose lexer parser statements

JS2SCHEME_BGL_MODULES = fun-bindings nodes protobject var js2scheme \
		 verbose lexer parser symbol statements simplify expand1 label \
		 label-resolution simplify-labels bind-exit escape liveness let \
		 scm-out expand2 expand3


OBFUSCATOR_OBJECTS = $(OBFUSCATOR_BGL_MODULES:%=o/%.o)
PP_OBJECTS = $(PP_BGL_MODULES:%=o/%.o)
JS2SCHEME_OBJECTS = $(JS2SCHEME_BGL_MODULES:%=o/%.o)

_OBJECTS	= $(_BGL_OBJECTS) $(OBFUSCATOR_BGL_OBJECTS) $(PP_BGL_OBJECTS)

OBJECTS		= $(OBFUSCATOR_OBJECTS) $(PP_OBJECTS) $(JS2SCHEME_OBJECTS)

SOURCES		= $(OBJECTS:o/%.o=%.scm)

INCLUDES	= nodes.sch protobject.sch

AFILE		= bglafile

all: .afile o
	$(MAKE) targets

targets: $(TARGETNAMES) #j$(TARGETNAME)

.PHONY: build-afile clean

.afile: $(SOURCES)
	$(AFILE) $(SOURCES) > $@

js-obfuscator: $(OBFUSCATOR_OBJECTS)
	$(BIGLOO) -o $@ $^

js-pp: $(PP_OBJECTS)
	$(BIGLOO) -o $@ $^

js2scheme: $(JS2SCHEME_OBJECTS)
	$(BIGLOO) -o $@ $^

o:
	mkdir o

o/%.o: %.scm
	$(BIGLOO) -c -g -o $@ $<

clean:
	rm -f $(OBJECTS) $(TARGETNAME) $(BGL_CLASSES) j$(TARGETNAME) *.class .afile;
