CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)
AFILE = bglafile

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
	$(AFILE) -o $@ $(SOURCES)

extract-globals: extract-globals.scm
	$(BIGLOO) -g -o $@ $^
extract-macros: extract-macros.scm
	$(BIGLOO) -g -o $@ $^
string-cache: str-cache.scm
	$(BIGLOO) -g -o $@ $^

runtime-variables.sch: extract-globals $(RUNTIME_SOURCES)
	./extract-globals -o $@ $(RUNTIME_SOURCES)

macros.sch: extract-macros $(RUNTIME_SOURCES) $(STRING_SOURCES)
	./extract-macros -o $@ $(RUNTIME_SOURCES) $(STRING_SOURCES)

$(JS2SCHEME_RT_LIB).init: $(JS2SCHEME_RT_LIB).init.in macros.sch
	sed 's/JS2SCHEME_RT_VERSION/$(JS2SCHEME_RT_VERSION)/' $@.in > $@
	cat macros.sch >> $@

o/runtime.o: $(RUNTIME_SOURCES)

o/.keep:
	mkdir -p o/unicode;
	touch $@;

o/%.o: %.scm .afile o/.keep
	$(BIGLOO) $(BGL_FLAGS) -c -o $@ $<

# must not use -g for match...
#o/RegExp-match.o: RegExp-match.scm
#	$(BIGLOO) -c -O3 -o $@ $<
o/RegExp-match.o: RegExp-match.scm
	$(BIGLOO) $(BGL_COMMON_FLAGS) -c -o $@ $<

clean:
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
	$(BIGLOO) -o $@ $(REGEXP_OBJECTS)

REGEXP_JVM_OBJECTS = $(REGEXP_MODULES:%=%.class)
%.class: %.scm
	$(BIGLOO) $(BGL_FLAGS) -jvm -o $@ $<
regexp.jvm: .afile $(REGEXP_JVM_OBJECTS) RegExp-KMP.scm RegExp-constant-classes.scm
	$(BIGLOO) -jvm -o $@ $(REGEXP_JVM_OBJECTS)

$(JS2SCHEME_RT_HEAP): make-lib.scm runtime.scm .afile
	bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(JS2SCHEME_RT_LIB) $< -addheap $@
$(JS2SCHEME_RT_LIB_A): $(JS2SCHEME_RT_HEAP) $(JS2SCHEME_RT_LIB_OBJECTS)
	rm -f $@ && \
	ar qcv $@ $(JS2SCHEME_RT_LIB_OBJECTS) && \
	ranlib $@
$(JS2SCHEME_RT_LIB_SO): $(JS2SCHEME_RT_HEAP) $(JS2SCHEME_RT_LIB_OBJECTS)
	ld -G -o $@ $(JS2SCHEME_RT_LIB_OBJECTS)

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
