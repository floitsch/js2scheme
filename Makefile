CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)

BGL_FLAGS = -copt -fpic -mkaddlib -L js-runtime -L . -L unicode -g
TARGETNAMES	= js-obfuscator js-pp js2scheme

OBFUSCATOR_BGL_MODULES	= config fun-bindings nodes protobject var js-obfuscator js-out \
		 verbose lexer parser symbol with ewal obfuscate-ids simplify \
		 html symbol-table

PP_BGL_MODULES	= config nodes protobject js-pp js-out var \
		 verbose lexer parser

JS2SCHEME_LIB_MODULES = config fun-bindings nodes protobject var \
		 verbose lexer parser symbol with ewal simplify \
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

all: $(JS2SCHEME_LIB_A) $(JS2SCHEME_LIB_SO) targets runtime

targets: $(TARGETNAMES) #j$(TARGETNAME)

.PHONY: build-afile clean runtime js2scheme-runtime-heap

.afile: $(SOURCES)
	$(AFILE) -o $@ $(SOURCES)

js-obfuscator: $(OBFUSCATOR_OBJECTS)
	$(BIGLOO) $(BGL_FLAGS) -o $@ $^

js-pp: $(PP_OBJECTS)
	$(BIGLOO) $(BGL_FLAGS) -o $@ $^

$(JS2SCHEME_HEAP): make-lib.scm js2scheme-comp.scm .afile
	bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_LIB) $< -addheap $@
$(JS2SCHEME_LIB_A): $(JS2SCHEME_HEAP) $(JS2SCHEME_LIB_OBJECTS)
	rm -f $@ && \
	ar qcv $@ $(JS2SCHEME_LIB_OBJECTS) && \
	ranlib $@
$(JS2SCHEME_LIB_SO): $(JS2SCHEME_HEAP) $(JS2SCHEME_LIB_OBJECTS)
	ld -G -o $@ $(JS2SCHEME_LIB_OBJECTS)

js2scheme: $(JS2SCHEME_LIB_A) $(JS2SCHEME_OBJECTS) runtime
	$(BIGLOO) $(BGL_FLAGS) -o $@ $(JS2SCHEME_OBJECTS)

o/symbol.o: js-runtime/runtime-variables.sch

o/.keep:
	mkdir -p o;
	touch $@;

o/%.o: %.scm $(INCLUDES) .afile o/.keep
	$(BIGLOO) $(BGL_FLAGS) -c -o $@ $<

# scm-out requires the runtime-heap.

o/scm-out.o: js2scheme-runtime-heap

js2scheme-runtime-heap:
	$(MAKE) -C js-runtime js2scheme-runtime.heap

js-runtime/runtime-variables.sch:
	$(MAKE) -C js-runtime runtime-variables.sch

runtime: $(JS2SCHEME_HEAP)
	$(MAKE) -C js-runtime

clean:
	$(MAKE) -C js-runtime clean
	rm -f $(OBJECTS) $(TARGETNAMES) \
	      $(JS2SCHEME_LIB_A) $(JS2SCHEME_LIB_SO) $(JS2SCHEME_HEAP) \
	      $(BGL_CLASSES) j$(TARGETNAME) \
	      *.class .afile;
