CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)
LD = ld
MCO = bglmco
AFILE = bglafile
DEPEND = bgldepend
CLASS_DIR = "."

BGL_COMMON_FLAGS = -L .. -I utf -L ../unicode -copt -fpic -mkaddlib -library utf
BGL_UNSAFE =
BGL_FLAGS = $(BGL_COMMON_FLAGS) -g $(BGL_UNSAFE)

# basically all modules are included in 'runtime.scm' and thus exported
# from the library.
RUNTIME_BGL_MODULES = Array Bool Date Function Math Number Object String \
			conversion Error Eval-env eval Arguments \
			undefined base-object base-string \
			global-object globals \
			bitset interval-avl-tree \
			ht-object property-entry operators scope-object \
			RegExp

ALL_RUNTIME_BGL_MODULES = $(RUNTIME_BGL_MODULES) \
			RegExp-parse RegExp-fsm RegExp-classes \
			RegExp-dot RegExp-match RegExp-state RegExp-char-set \
			mset multi-top-level double runtime

RUNTIME_OBJECTS = $(ALL_RUNTIME_BGL_MODULES:%=o/%.o)
RUNTIME_SOURCES = $(RUNTIME_BGL_MODULES:%=%.scm)

STRING_SOURCES = utf/string.sch utf/char.sch

OBJECTS	= $(RUNTIME_OBJECTS)
SOURCES	= $(OBJECTS:o/%.o=%.scm)

JS2SCHEME_RT_LIB = js2scheme-runtime
JS2SCHEME_RT_HEAP = $(JS2SCHEME_RT_LIB).heap
JS2SCHEME_RT_VERSION = 1.0
JS2SCHEME_RT_LIB_A = lib$(JS2SCHEME_RT_LIB)_s-$(JS2SCHEME_RT_VERSION).a
JS2SCHEME_RT_LIB_SO = lib$(JS2SCHEME_RT_LIB)_s-$(JS2SCHEME_RT_VERSION).so
JS2SCHEME_RT_LIB_OBJECTS = $(RUNTIME_OBJECTS)

all: runtime-variables.sch $(JS2SCHEME_RT_LIB).init $(JS2SCHEME_RT_LIB_A) $(JS2SCHEME_RT_LIB_SO)

.PHONY: build-afile clean all

.afile: $(SOURCES)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then echo $(AFILE) -o $@ $(SOURCES); fi
	@ $(AFILE) -o $@ $(SOURCES)

extract-globals: extract-globals.scm
	@ echo "[$(PREFIX)$@]";
	@ if [ $(VERBOSE) ]; then echo $(BIGLOO) -g -o $@ $^; fi
	@ $(BIGLOO) -g -o $@ $^
extract-macros: extract-macros.scm
	@ echo "[$(PREFIX)$@]";
	@ if [ $(VERBOSE) ]; then echo $(BIGLOO) -g -o $@ $^; fi
	@ $(BIGLOO) -g -o $@ $^
string-cache: str-cache.scm
	@ echo "[$(PREFIX)$@]";
	@ if [ $(VERBOSE) ]; then echo $(BIGLOO) -g -o $@ $^; fi
	@ $(BIGLOO) -g -o $@ $^

runtime-variables.sch: extract-globals $(RUNTIME_SOURCES)
	@ echo "[$(PREFIX)$@]";
	@ if [ $(VERBOSE) ]; then \
	    echo ./extract-globals -o $@ $(RUNTIME_SOURCES); \
	    ./extract-globals -o $@ $(RUNTIME_SOURCES); \
	  else \
	    ./extract-globals -o $@ $(RUNTIME_SOURCES) 2> /dev/null; \
	  fi

macros.sch: extract-macros $(RUNTIME_SOURCES) $(STRING_SOURCES)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo ./extract-macros -o $@ $(RUNTIME_SOURCES) $(STRING_SOURCES); \
	    ./extract-macros -o $@ $(RUNTIME_SOURCES) $(STRING_SOURCES); \
	  else \
	    ./extract-macros -o $@ $(RUNTIME_SOURCES) $(STRING_SOURCES) 2> /dev/null; \
	  fi

$(JS2SCHEME_RT_LIB).init: $(JS2SCHEME_RT_LIB).init.in macros.sch
	@ echo "[$@]";
	@ if [ $(VERBOSE) ]; then \
	    echo sed 's/JS2SCHEME_RT_VERSION/$(JS2SCHEME_RT_VERSION)/' $@.in ">" $@; \
	    echo cat macros.sch ">>" $@; \
	  fi
	@ sed 's/JS2SCHEME_RT_VERSION/$(JS2SCHEME_RT_VERSION)/' $@.in > $@
	@ cat macros.sch >> $@

o/runtime.o: $(RUNTIME_SOURCES)

o/.keep:
	@ if [ $(VERBOSE) ]; then \
	    echo mkdir -p o/unicode; \
	    echo touch $@; \
	  fi
	@ mkdir -p o/unicode;
	@ touch $@;

o/%.o: %.scm .afile o/.keep
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then echo $(BIGLOO) $(BGL_FLAGS) -c -o $@ $<; fi
	@ $(BIGLOO) $(BGL_FLAGS) -c -o $@ $<

# must not use -g for match...
#o/RegExp-match.o: RegExp-match.scm
#	$(BIGLOO) -c -O3 -o $@ $<
o/RegExp-match.o: RegExp-match.scm
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo $(BIGLOO) $(BGL_COMMON_FLAGS) -c -o $@ $<; \
	  fi
	@ $(BIGLOO) $(BGL_COMMON_FLAGS) -c -o $@ $<

clean: mco-clean
	rm -f $(OBJECTS) $(REGEXP_OBJECTS) \
              extract-globals extract-globals.o \
	      extract-macros  extract-macros.o \
	      str-cache str-cache.o \
	      $(JS2SCHEME_RT_HEAP) \
	      $(JS2SCHEME_RT_LIB_A) \
	      $(JS2SCHEME_RT_LIB_SO) \
	      $(JS2SCHEME_RT_LIB).init \
	      macros.sch \
	      .afile;

REGEXP_MODULES =  RegExp-main RegExp-parse RegExp-fsm RegExp-classes RegExp-dot \
                  RegExp-match RegExp-state RegExp-char-set mset multi-top-level

REGEXP_OBJECTS = $(REGEXP_MODULES:%=o/%.o)

regexp: .afile $(REGEXP_OBJECTS) RegExp-KMP.scm RegExp-constant-classes.scm
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then echo $(BIGLOO) -o $@ $(REGEXP_OBJECTS); fi
	@ $(BIGLOO) -o $@ $(REGEXP_OBJECTS)

REGEXP_JVM_OBJECTS = $(REGEXP_MODULES:%=%.class)
%.class: %.scm
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then echo $(BIGLOO) $(BGL_FLAGS) -jvm -o $@ $<; fi
	@ $(BIGLOO) $(BGL_FLAGS) -jvm -o $@ $<
regexp.jvm: .afile $(REGEXP_JVM_OBJECTS) RegExp-KMP.scm RegExp-constant-classes.scm
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo $(BIGLOO) -jvm -o $@ $(REGEXP_JVM_OBJECTS); \
	  fi
	@ $(BIGLOO) -jvm -o $@ $(REGEXP_JVM_OBJECTS)

$(JS2SCHEME_RT_HEAP): make-lib.scm runtime.scm .afile
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_RT_LIB) $< -addheap $@; \
	  fi
	@ bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_RT_LIB) $< -addheap $@
$(JS2SCHEME_RT_LIB_A): $(JS2SCHEME_RT_HEAP) $(JS2SCHEME_RT_LIB_OBJECTS)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo rm -f $@ "&&" \
	         ar qcv $@ $(JS2SCHEME_RT_LIB_OBJECTS) "&&" \
		 ranlib $@; \
	    rm -f $@ && \
	    ar qcv $@ $(JS2SCHEME_RT_LIB_OBJECTS) && \
	    ranlib $@; \
	  else \
	    rm -f $@ && \
	    ar qcv $@ $(JS2SCHEME_RT_LIB_OBJECTS) > /dev/null && \
	    ranlib $@ > /dev/null; \
	  fi
$(JS2SCHEME_RT_LIB_SO): $(JS2SCHEME_RT_HEAP) $(JS2SCHEME_RT_LIB_OBJECTS)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo $(LD) -G -o $@ $(JS2SCHEME_RT_LIB_OBJECTS); \
	  fi
	@ $(LD) -G -o $@ $(JS2SCHEME_RT_LIB_OBJECTS)

#-- explicit deps
o/RegExp-match.o: RegExp-KMP.scm RegExp-constant-classes.scm
o/RegExp-main.o: RegExp-KMP.scm RegExp-constant-classes.scm
o/RegExp-fsm.o: RegExp-KMP.scm RegExp-constant-classes.scm
o/RegExp-parse.o: RegExp-KMP.scm RegExp-constant-classes.scm
o/RegExp-classes.o: RegExp-KMP.scm RegExp-constant-classes.scm
o/RegExp-dot.o: RegExp-KMP.scm RegExp-constant-classes.scm
o/Date.o: date-impl.scm
o/eval.o: macros.sch
o/base-string.o: utf/string.sch utf/char.sch

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

Makefile.deps: $(RUNTIME_SOURCES)
	@ echo "Rebuilding $@ file.";
	@ $(DEPEND) -search-path . \
		    -strict-class-dir $(CLASS_DIR) \
		    -strict-mco-dir mco \
                    -strict-obj-dir o \
                    $(RUNTIME_SOURCES) > Makefile.deps

-include Makefile.deps
