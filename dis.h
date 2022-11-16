#pragma once

#include <string>

#include "dis/disassembler.h"
#include "dis/assembler-x64.h"

static const int kMaxDisassemblySize = 4096;

std::string disassembleToString(byte* code, uword length) {
  // Some padding in case it's longer than expected.
  char buffer[kMaxDisassemblySize];
  std::memset(buffer, 0, sizeof buffer);
  dis::DisassembleToMemory formatter(buffer, sizeof buffer);
  dis::Disassembler::disassemble(
      reinterpret_cast<uword>(code),
      reinterpret_cast<uword>(code) + length, &formatter);
  return buffer;
}
