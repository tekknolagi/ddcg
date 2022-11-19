CXX=clang++

all: interp

# TODO(max): Figure out how to list headers here without including them in $^
interp: interp.o dis.so
	$(CXX) $(CXXFLAGS) -rpath . $^ -o $@ $(LDFLAGS)

# TODO(max): Figure out how to list headers here without including them in $^
dis.so: dis/disassembler.cpp dis/disassembler-x64.cpp dis/memory-region.cpp \
	dis/assembler-utils.cpp dis/assembler-x64.cpp \
	dis/dcheck.c
	$(CXX) $(CXXFLAGS) -fPIC -shared $^ -o $@ $(LDFLAGS)
