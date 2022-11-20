CXX=clang++
CXXFLAGS+=-std=c++11

# If on an M1, M2, or other new Apple Silicon, you have to both compile and run
# using the "arch" tool so that both the final binary and the JIT-compiled code
# can run in x86_64 mode.
UNAME_S:=$(shell uname -s)
UNAME_M:=$(shell uname -m)
ifeq ($(UNAME_S),Darwin)
	ifeq ($(UNAME_M),arm64)
		COMPILEPREFIX=arch -x86_64
	endif
endif

ifneq ($(DEBUG),)
	CFLAGS+=-g -O0
	CXXFLAGS+=-g -O0
endif

ifneq ($(UBSAN),)
	CFLAGS+=-fsanitize=undefined
	CXXFLAGS+=-fsanitize=undefined
endif

ifneq ($(ASAN),)
	CFLAGS+=-fsanitize=address
	CXXFLAGS+=-fsanitize=address
endif

all: interp

# TODO(max): Figure out how to list headers here without including them in $^
interp: interp.o dis.so 
	$(COMPILEPREFIX) $(CXX) $(CXXFLAGS) -Wl,-rpath . interp.o dis.so -o $@ $(LDFLAGS)

interp.o: interp.cpp *.h dis/*.h
	$(COMPILEPREFIX) $(CXX) $(CXXFLAGS) interp.cpp -c -o $@

# TODO(max): Figure out how to list headers here without including them in $^
dis.so: dis/disassembler.cpp dis/disassembler-x64.cpp dis/memory-region.cpp \
	dis/assembler-utils.cpp dis/assembler-x64.cpp \
	dis/dcheck.o *.h dis/*.h
	$(COMPILEPREFIX) $(CXX) $(CXXFLAGS) -fPIC -shared dis/*.cpp dis/dcheck.o -o $@ $(LDFLAGS)

dis/dcheck.o: dis/dcheck.h dis/dcheck.c
	$(COMPILEPREFIX) $(CC) $(CFLAGS) -fPIC -c dis/dcheck.c -o $@

.PHONY: test
test: interp
	$(COMPILEPREFIX) ./interp

.PHONY: clean
clean:
	find . -name '*.o' -delete
	find . -name '*.so' -delete
