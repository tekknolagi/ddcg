CXX=clang++

all: interp

interp: interp.o dis.so | dis.h log.h
	$(CXX) $(CXXFLAGS) -rpath . $^ -o $@ $(LDFLAGS)

dis.so: dis/disassembler.cpp dis/disassembler-x64.cpp dis/memory-region.cpp \
	dis/assembler-utils.cpp dis/assembler-x64.cpp \
	dis/dcheck.c | dis/*.h
	$(CXX) $(CXXFLAGS) -fPIC -shared $^ -o $@ $(LDFLAGS)
