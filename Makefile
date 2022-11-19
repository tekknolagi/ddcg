CXX=clang++
CXXFLAGS+=-std=c++11

all: interp

# TODO(max): Figure out how to list headers here without including them in $^
interp: interp.o dis.so *.h dis/*.h
	$(CXX) $(CXXFLAGS) -Wl,-rpath . interp.o dis.so -o $@ $(LDFLAGS)

# TODO(max): Figure out how to list headers here without including them in $^
dis.so: dis/disassembler.cpp dis/disassembler-x64.cpp dis/memory-region.cpp \
	dis/assembler-utils.cpp dis/assembler-x64.cpp \
	dis/dcheck.o *.h dis/*.h
	$(CXX) $(CXXFLAGS) -fPIC -shared dis/*.cpp dis/dcheck.o -o $@ $(LDFLAGS)

dis/dcheck.o: dis/dcheck.h dis/dcheck.c
	$(CC) $(CFLAGS) -fPIC -c dis/dcheck.c -o $@

.PHONY: clean
clean:
	find . -name '*.o' -delete
	find . -name '*.so' -delete
