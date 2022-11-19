CXX=clang++

all: interp

interp: interp.o dis.so
	$(CXX) -rpath . $^ -o $@

dis.so: dis/disassembler.cpp dis/disassembler-x64.cpp dis/memory-region.cpp dis/dcheck.c | dis/*.h
	$(CXX) -fPIC -shared $^ -o $@
