CXX=clang++

all: interp

interp: interp.o dis.so
	$(CXX) $(CXXFLAGS) -rpath . $^ -o $@ $(LDFLAGS)

dis.so: dis/disassembler.cpp dis/disassembler-x64.cpp dis/memory-region.cpp dis/dcheck.c | dis/*.h
	$(CXX) $(CXXFLAGS) -fPIC -shared $^ -o $@ $(LDFLAGS)
