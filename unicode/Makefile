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
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo $(AFILE) -o $@ $(SOURCES); \
	fi;
	@ $(AFILE) -o $@ $(SOURCES)

o/runtime.o: $(RUNTIME_SOURCES)

o/unicode/.keep:
	mkdir -p o/unicode;
	touch $@;

%.o: %.scm .afile
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo $(BIGLOO) $(BGL_FLAGS) -c -o $@ $<; \
	fi;
	@ $(BIGLOO) $(BGL_FLAGS) -c -o $@ $<

os/%.o: %.scm .afile
	@ mkdir -p os
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo $(BIGLOO) $(BSAFE_FLAGS) $(BGL_FLAGS) -c -o $@ $<; \
	fi;
	@ $(BIGLOO) $(BSAFE_FLAGS) $(BGL_FLAGS) -c -o $@ $<

ou/%.o: %.scm .afile
	@ mkdir -p ou
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo $(BIGLOO) $(BUNSAFE_FLAGS) $(BGL_FLAGS) -c -o $@ $<; \
	fi;
	@ $(BIGLOO) $(BUNSAFE_FLAGS) $(BGL_FLAGS) -c -o $@ $<

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
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo ./utf-init-gen -o $@ $<; \
	fi;
	@ ./utf-init-gen -o $@ $<

$(UTF_HEAP): make-lib.scm utf.scm .afile utf-data.sch
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(UTF_LIB) $< -addheap $@; \
	fi;
	@ bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(UTF_LIB) $< -addheap $@
$(UTF_LIB_A_S): $(UTF_HEAP) $(UTF_LIB_OBJECTS_S)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo rm -f $@ "&&" \
	         ar qcv $@ $(UTF_LIB_OBJECTS_S) "&&" \
	         ranlib $@; \
	    rm -f $@ && \
	    ar qcv $@ $(UTF_LIB_OBJECTS_S) && \
	    ranlib $@; \
	  else \
	    rm -f $@ && \
	    ar qcv $@ $(UTF_LIB_OBJECTS_S) > /dev/null && \
	    ranlib $@ > /dev/null; \
	  fi
$(UTF_LIB_SO_S): $(UTF_HEAP) $(UTF_LIB_OBJECTS_S)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	   echo $(LD) -G -o $@ $(UTF_LIB_OBJECTS_S); \
	  fi
	@ ld -G -o $@ $(UTF_LIB_OBJECTS_S)
$(UTF_LIB_A_U): $(UTF_HEAP) $(UTF_LIB_OBJECTS_U)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	    echo rm -f $@ "&&" \
	         ar qcv $@ $(UTF_LIB_OBJECTS_U) "&&" \
	         ranlib $@; \
	    rm -f $@ && \
	    ar qcv $@ $(UTF_LIB_OBJECTS_U) && \
	    ranlib $@; \
	  else \
	    rm -f $@ && \
	    ar qcv $@ $(UTF_LIB_OBJECTS_U) > /dev/null && \
	    ranlib $@ > /dev/null; \
	  fi
$(UTF_LIB_SO_U): $(UTF_HEAP) $(UTF_LIB_OBJECTS_U)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	   echo $(LD) -G -o $@ $(UTF_LIB_OBJECTS_U); \
	  fi;
	@ ld -G -o $@ $(UTF_LIB_OBJECTS_U)
$(UTF_LIB_SO_E): $(UTF_HEAP) $(UTF_LIB_OBJECTS_E) $(UTF_LIB_SO_S)
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo ld -G -o $@ $(UTF_LIB_OBJECTS_E) -L. -l$(UTF_LIB)_s-$(UTF_VERSION); \
	  fi;
	@ ld -G -o $@ $(UTF_LIB_OBJECTS_E) -L. -l$(UTF_LIB)_s-$(UTF_VERSION)

# explicit deps
os/utf.o: utf-data.sch utf-category-data.sch
ou/utf.o: utf-data.sch utf-category-data.sch

make-lib.o: utf-data.sch

utf-data.sch: unicode-reader
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo ./unicode-reader > $@; \
	fi;
	@ ./unicode-reader > $@
utf-category-data.sch: unicode-reader
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo ./unicode-reader --categories > $@; \
	fi;
	@ ./unicode-reader --categories > $@

unicode-reader: unicode-reader.scm
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo $(BIGLOO) -Wall -g -o $@ $^; \
	fi;
	@ $(BIGLOO) -Wall -g -o $@ $^

utf-init-gen: utf-init-gen.scm utf-category-data.sch
	@ echo "[$(PREFIX)$@]"
	@ if [ $(VERBOSE) ]; then \
	  echo $(BIGLOO) -Wall -g -o $@ $<; \
	fi;
	@ $(BIGLOO) -Wall -g -o $@ $<
