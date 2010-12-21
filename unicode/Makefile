CC = /usr/local/bin/gcc
BIGLOO = bigloo -cc $(CC)
AFILE = bglafile

BGL_COMMON_FLAGS = -L .. -copt -fpic -mkaddlib
BGL_FLAGS = $(BGL_COMMON_FLAGS) -g
BSAFE_FLAGS = -g
BUNSAFE_FLAGS = -unsafe

UTF_BGL_MODULES = utf
UTF_OBJECTS = $(UTF_BGL_MODULES:%=%.o)
UTF_SOURCES = $(UTF_BGL_MODULES:%=%.scm)

OBJECTS_S	= $(UTF_OBJECTS:%=os/%)
OBJECTS_U	= $(UTF_OBJECTS:%=ou/%)
SOURCES	= $(UTF_SOURCES)

UTF_LIB = utf
UTF_HEAP = $(UTF_LIB).heap
UTF_INIT = $(UTF_LIB).init
UTF_VERSION = 1.0
UTF_LIB_A_S = lib$(UTF_LIB)_s-$(UTF_VERSION).a
UTF_LIB_SO_S = lib$(UTF_LIB)_s-$(UTF_VERSION).so
UTF_LIB_A_U = lib$(UTF_LIB)_u-$(UTF_VERSION).a
UTF_LIB_SO_U = lib$(UTF_LIB)_u-$(UTF_VERSION).so
UTF_LIB_SO_E = lib$(UTF_LIB)_e-$(UTF_VERSION).so
UTF_LIB_OBJECTS_S = $(OBJECTS_S)
UTF_LIB_OBJECTS_U = $(OBJECTS_U)
UTF_LIB_OBJECTS_E = make-lib.o

all: $(UTF_INIT) \
	$(UTF_LIB_A_S) $(UTF_LIB_SO_S) \
	$(UTF_LIB_A_U) $(UTF_LIB_SO_U) \
	$(UTF_LIB_SO_E)

.PHONY: build-afile clean all

.afile: $(SOURCES)
	$(AFILE) -o $@ $(SOURCES)

o/runtime.o: $(RUNTIME_SOURCES)

o/unicode/.keep:
	mkdir -p o/unicode;
	touch $@;

%.o: %.scm .afile
	$(BIGLOO) $(BGL_FLAGS) -c -o $@ $<

os/%.o: %.scm .afile
	@ mkdir -p os
	$(BIGLOO) $(BSAFE_FLAGS) $(BGL_FLAGS) -c -o $@ $<

ou/%.o: %.scm .afile
	@ mkdir -p ou
	$(BIGLOO) $(BUNSAFE_FLAGS) $(BGL_FLAGS) -c -o $@ $<

clean:
	rm -f $(OBJECTS_S) \
	      $(OBJECTS_U) \
	      $(UTF_HEAP) \
	      $(UTF_LIB_A_S) \
	      $(UTF_LIB_SO_S) \
	      $(UTF_LIB_A_U) \
	      $(UTF_LIB_SO_U) \
	      $(UTF_LIB_SO_E) \
	      $(UTF_INIT) \
	      unicode-reader.o unicode-reader \
	      utf-data.sch utf-category-data.sch \
	      utf-init-gen utf-init-gen.o \
	      .afile;

$(UTF_INIT): utf.init.in utf-init-gen
	./utf-init-gen -o $@ $<

$(UTF_HEAP): make-lib.scm utf.scm .afile utf-data.sch
	bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(UTF_LIB) $< -addheap $@
$(UTF_LIB_A_S): $(UTF_HEAP) $(UTF_LIB_OBJECTS_S)
	rm -f $@ && \
	ar qcv $@ $(UTF_LIB_OBJECTS_S) && \
	ranlib $@
$(UTF_LIB_SO_S): $(UTF_HEAP) $(UTF_LIB_OBJECTS_S)
	ld -G -o $@ $(UTF_LIB_OBJECTS_S)
$(UTF_LIB_A_U): $(UTF_HEAP) $(UTF_LIB_OBJECTS_U)
	rm -f $@ && \
	ar qcv $@ $(UTF_LIB_OBJECTS_U) && \
	ranlib $@
$(UTF_LIB_SO_U): $(UTF_HEAP) $(UTF_LIB_OBJECTS_U)
	ld -G -o $@ $(UTF_LIB_OBJECTS_U)
$(UTF_LIB_SO_E): $(UTF_HEAP) $(UTF_LIB_OBJECTS_E) $(UTF_LIB_SO_S)
	ld -G -o $@ $(UTF_LIB_OBJECTS_E) -L. -l$(UTF_LIB)_s-$(UTF_VERSION)

# explicit deps
os/utf.o: utf-data.sch utf-category-data.sch
ou/utf.o: utf-data.sch utf-category-data.sch

make-lib.o: utf-data.sch

utf-data.sch: unicode-reader
	./unicode-reader > $@
utf-category-data.sch: unicode-reader
	./unicode-reader --categories > $@

unicode-reader: unicode-reader.scm
	bigloo -Wall -g -o $@ $^

utf-init-gen: utf-init-gen.scm utf-category-data.sch
	bigloo -Wall -g -o $@ $<
