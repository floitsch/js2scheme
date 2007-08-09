CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)
TARGETNAMES	= js-obfuscator js-pp js2scheme

OBFUSCATOR_BGL_MODULES	= fun-bindings nodes protobject var js-obfuscator js-out \
		 verbose lexer parser symbol statements obfuscate-ids simplify \
		 html

PP_BGL_MODULES	= nodes protobject js-pp js-out var \
		 verbose lexer parser statements

JS2SCHEME_LIB_MODULES = fun-bindings nodes protobject var \
		 verbose lexer parser symbol statements simplify expand1 label \
		 label-resolution simplify-labels bind-exit escape liveness let \
		 scm-out expand2 expand3 expand4 js2scheme-comp

JS2SCHEME_BGL_MODULES = js2scheme

OBFUSCATOR_OBJECTS = $(OBFUSCATOR_BGL_MODULES:%=o/%.o)
PP_OBJECTS = $(PP_BGL_MODULES:%=o/%.o)
JS2SCHEME_OBJECTS = $(JS2SCHEME_BGL_MODULES:%=o/%.o)
JS2SCHEME_LIB_OBJECTS = $(JS2SCHEME_LIB_MODULES:%=o/%.o)

JS2SCHEME_LIB = js2scheme-comp
JS2SCHEME_HEAP = $(JS2SCHEME_LIB).heap
JS2SCHEME_LIB_A = lib$(JS2SCHEME_LIB).a

_OBJECTS	= $(_BGL_OBJECTS) $(OBFUSCATOR_BGL_OBJECTS) $(PP_BGL_OBJECTS)

OBJECTS		= $(OBFUSCATOR_OBJECTS) $(PP_OBJECTS) $(JS2SCHEME_OBJECTS) $(JS2SCHEME_LIB_OBJECTS)

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

$(JS2SCHEME_HEAP): make-lib.scm
	bigloo -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_LIB) $^ -addheap $@
$(JS2SCHEME_LIB_A): $(JS2SCHEME_HEAP) $(JS2SCHEME_LIB_OBJECTS)
	rm -f $@ && \
	ar qcv $@ $(JS2SCHEME_LIB_OBJECTS) && \
	ranlib $@

js2scheme: $(JS2SCHEME_LIB_A) $(JS2SCHEME_OBJECTS)
	$(BIGLOO) -o $@ $(JS2SCHEME_OBJECTS) -l$(JS2SCHEME_LIB)

o/symbol.o: js-runtime/runtime-variables.sch

o:
	mkdir o

o/%.o: %.scm $(INCLUDES)
	$(BIGLOO) -mkaddlib -c -g -o $@ $<

clean:
	rm -f $(OBJECTS) $(TARGETNAMES) $(JS2SCHEME_LIB_A) $(JS2SCHEME_HEAP) $(BGL_CLASSES) j$(TARGETNAME) *.class .afile;
