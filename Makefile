CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)
DEPEND = bgldepend
MCO = bglmco
LD = ld

CLASS_DIR = class

BGL_FLAGS = -copt -fpic -mkaddlib -L js-runtime -L . -L unicode -g
TARGETNAMES = js-obfuscator js-pp js2scheme

OBFUSCATOR_BGL_MODULES = config nodes walk fun-bindings js-obfuscator js-out \
		 verbose lexer parser symbol obfuscate-ids simplify \
		 html symbol-table

PP_BGL_MODULES = config nodes js-pp js-out verbose lexer parser

JS2SCHEME_LIB_MODULES = config nodes walk fun-bindings \
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

OBJECTS = $(OBFUSCATOR_OBJECTS) $(PP_OBJECTS) $(JS2SCHEME_OBJECTS) $(JS2SCHEME_LIB_OBJECTS)

SOURCES = $(OBJECTS:o/%.o=%.scm)

UTF_LIB = utf
UTF_DIR = unicode
UTF_HEAP = $(UTF_DIR)/$(UTF_LIB).heap

INCLUDES =

AFILE = bglafile

all: $(JS2SCHEME_LIB_A) $(JS2SCHEME_LIB_SO) targets runtime unicode

targets: $(TARGETNAMES)

.PHONY: build-afile clean runtime unicode js2scheme-runtime-heap mco-clean dep \
        test

.afile: $(SOURCES)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo $(AFILE) -o $@ $(SOURCES); \
	fi;
	@ $(AFILE) -o $@ $(SOURCES)

js-obfuscator: $(OBFUSCATOR_OBJECTS)
	@ if [ $(VERBOSE) ]; then \
	    echo $(BIGLOO) $(BGL_FLAGS) -o $@ $^; \
	  fi;
	@ $(BIGLOO) $(BGL_FLAGS) -o $@ $^

js-pp: $(PP_OBJECTS)
	@ if [ $(VERBOSE) ]; then \
	    echo $(BIGLOO) $(BGL_FLAGS) -o $@ $^; \
	  fi;
	@ $(BIGLOO) $(BGL_FLAGS) -o $@ $^

$(JS2SCHEME_HEAP): make-lib.scm js2scheme-comp.scm .afile
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo $(BIGLOO) $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_LIB) $< -addheap $@; \
	  fi
	@ $(BIGLOO) $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_LIB) $< -addheap $@
$(JS2SCHEME_LIB_A): $(JS2SCHEME_HEAP) $(JS2SCHEME_LIB_OBJECTS)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo rm -f $@ "&&" \
	         ar qcv $@ $(JS2SCHEME_LIB_OBJECTS) "&&" \
	         ranlib $@; \
	    rm -f $@ && \
	    ar qcv $@ $(JS2SCHEME_LIB_OBJECTS) && \
	    ranlib $@; \
	  else \
	    rm -f $@ && \
	    ar qcv $@ $(JS2SCHEME_LIB_OBJECTS) > /dev/null && \
	    ranlib $@ > /dev/null; \
	  fi

$(JS2SCHEME_LIB_SO): $(JS2SCHEME_HEAP) $(JS2SCHEME_LIB_OBJECTS)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	   echo $(LD) -G -o $@ $(JS2SCHEME_LIB_OBJECTS); \
	  fi
	@ $(LD) -G -o $@ $(JS2SCHEME_LIB_OBJECTS)

js2scheme: $(JS2SCHEME_LIB_A) $(JS2SCHEME_OBJECTS) runtime
	@ if [ $(VERBOSE) ]; then \
	    echo $(BIGLOO) $(BGL_FLAGS) -o $@ $(JS2SCHEME_OBJECTS); \
	  fi;
	@ $(BIGLOO) $(BGL_FLAGS) -o $@ $(JS2SCHEME_OBJECTS)

o/symbol.o: js-runtime/runtime-variables.sch

o/.keep:
	mkdir -p o;
	touch $@;

o/%.o: %.scm $(INCLUDES) .afile o/.keep
	@ echo "[$(PREFIX)$@]";
	@ if [ $$VERBOSE ]; then \
	    echo $(BIGLOO) $(BGL_FLAGS) -c -o $@ $<; \
	  fi;
	@ $(BIGLOO) $(BGL_FLAGS) -c -o $@ $<

# scm-out requires the runtime-heap.
o/scm-out.o: js2scheme-runtime-heap

o/js2scheme.o: $(JS2SCHEME_HEAP) js2scheme-runtime-heap

o/lexer.o: $(UTF_HEAP)

js2scheme-runtime-heap: $(UTF_HEAP)
	@ $(MAKE) PREFIX="$(PREFIX)js-runtime/" -C js-runtime js2scheme-runtime.heap

js-runtime/runtime-variables.sch: $(UTF_HEAP)
	@ $(MAKE) PREFIX="$(PREFIX)js-runtime/" -C js-runtime runtime-variables.sch

runtime: $(JS2SCHEME_HEAP) $(UTF_HEAP)
	@ $(MAKE) PREFIX="$(PREFIX)js-runtime/" -C js-runtime

$(UTF_HEAP): unicode

unicode:
	@$(MAKE) -C $(UTF_DIR)

#*---------------------------------------------------------------------*/
#*    Implicit Rules                                                   */
#*---------------------------------------------------------------------*/
MCOS = $(OBJECTS:%=mco/%.mco)
MCOS2 = $(OBJECTS:%=mco/%.mco2)

.PRECIOUS: $(MCOS)

mco/%.mco: %.scm
	@mkdir -p mco;
	@if [ ! "$@"2 -nt "$^" ]; then \
	  if [ $(VERBOSE) ]; then \
	    echo $(MCO) -s -I . -o $@ $<; \
	  fi; \
	  $(MCO) -s -I . -o $@ $<; \
	fi;
	@touch "$@"2;

mco-clean:
	rm -rf mco

Makefile.deps: $(SOURCES)
	@ echo "Rebuilding $@ file.";
	@ $(DEPEND) -search-path . \
		    -strict-class-dir $(CLASS_DIR) \
		    -strict-mco-dir mco \
                    -strict-obj-dir o \
                    $(SOURCES) > Makefile.deps

clean: mco-clean
	$(MAKE) -C js-runtime clean
	$(MAKE) -C $(UTF_DIR) clean
	rm -f $(OBJECTS) $(TARGETNAMES) \
	      $(JS2SCHEME_LIB_A) $(JS2SCHEME_LIB_SO) $(JS2SCHEME_HEAP) \
	      $(BGL_CLASSES) j$(TARGETNAME) \
	      *.class .afile;

-include Makefile.deps

#*---------------------------------------------------------------------*/
#*    Test                                                             */
#*---------------------------------------------------------------------*/
test:
	cd rhino-tests && \
	   ./jsDriver.pl -e js2scheme -t -l \
	         ecma/Boolean ecma/ExecutionContexts ecma/Array ecma/Date \
	         ecma/Expressions ecma/FunctionObjects ecma/GlobalObject \
	         ecma/Math ecma/LexicalConventions ecma/Number ecma/String \
	         ecma/Statements ecma/TypeConversion
