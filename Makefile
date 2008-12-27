CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)
TARGETNAMES	= js-obfuscator js-pp js2scheme

OBFUSCATOR_BGL_MODULES	= config fun-bindings nodes protobject var js-obfuscator js-out \
		 verbose lexer parser symbol with ewal statements obfuscate-ids simplify \
		 html symbol-table

PP_BGL_MODULES	= config nodes protobject js-pp js-out var \
		 verbose lexer parser statements

JS2SCHEME_LIB_MODULES = config fun-bindings nodes protobject var \
		 verbose lexer parser symbol with ewal statements simplify \
                 expand1 label label-resolution simplify-labels bind-exit \
		 escape liveness let scm-out js2scheme-comp \
		 symbol-table arguments js-out stmt-result

JS2SCHEME_BGL_MODULES = js2scheme

OBFUSCATOR_OBJECTS = $(OBFUSCATOR_BGL_MODULES:%=o/%.o)
PP_OBJECTS = $(PP_BGL_MODULES:%=o/%.o)
JS2SCHEME_OBJECTS = $(JS2SCHEME_BGL_MODULES:%=o/%.o)
JS2SCHEME_LIB_OBJECTS = $(JS2SCHEME_LIB_MODULES:%=o/%.o)

JS2SCHEME_LIB = js2scheme-comp
JS2SCHEME_HEAP = $(JS2SCHEME_LIB).heap
JS2SCHEME_LIB_VERSION = 1.0
JS2SCHEME_LIB_A = lib$(JS2SCHEME_LIB)_s-$(JS2SCHEME_LIB_VERSION).a
JS2SCHEME_LIB_SO = lib$(JS2SCHEME_LIB)_s-$(JS2SCHEME_LIB_VERSION).so

_OBJECTS	= $(_BGL_OBJECTS) $(OBFUSCATOR_BGL_OBJECTS) $(PP_BGL_OBJECTS)

OBJECTS		= $(OBFUSCATOR_OBJECTS) $(PP_OBJECTS) $(JS2SCHEME_OBJECTS) $(JS2SCHEME_LIB_OBJECTS)

SOURCES		= $(OBJECTS:o/%.o=%.scm)

INCLUDES	= nodes.sch protobject.sch

AFILE		= bglafile

all: $(JS2SCHEME_LIB_A) $(JS2SCHEME_LIB_SO) targets

targets: $(TARGETNAMES) #j$(TARGETNAME)

.PHONY: build-afile clean

.afile: $(SOURCES)
	$(AFILE) -o $@ $(SOURCES)

js-obfuscator: $(OBFUSCATOR_OBJECTS)
	$(BIGLOO) -o $@ $^

js-pp: $(PP_OBJECTS)
	$(BIGLOO) -o $@ $^

$(JS2SCHEME_HEAP): make-lib.scm js2scheme-comp.scm .afile
	bigloo -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_LIB) $< -addheap $@
$(JS2SCHEME_LIB_A): $(JS2SCHEME_HEAP) $(JS2SCHEME_LIB_OBJECTS)
	rm -f $@ && \
	ar qcv $@ $(JS2SCHEME_LIB_OBJECTS) && \
	ranlib $@
$(JS2SCHEME_LIB_SO): $(JS2SCHEME_HEAP) $(JS2SCHEME_LIB_OBJECTS)
	ld -G -o $@ $(JS2SCHEME_LIB_OBJECTS)

js2scheme: $(JS2SCHEME_LIB_A) $(JS2SCHEME_OBJECTS)
	$(BIGLOO) -o $@ $(JS2SCHEME_OBJECTS)

o/symbol.o: js-runtime/runtime-variables.sch

o/.keep:
	mkdir -p o;
	touch $@;

o/%.o: %.scm $(INCLUDES) .afile o/.keep
	$(BIGLOO) -copt -fpic -mkaddlib -c -g -o $@ $<

clean:
	rm -f $(OBJECTS) $(TARGETNAMES) \
	      $(JS2SCHEME_LIB_A) $(JS2SCHEME_LIB_SO) $(JS2SCHEME_HEAP) \
	      $(BGL_CLASSES) j$(TARGETNAME) \
	      *.class .afile;
