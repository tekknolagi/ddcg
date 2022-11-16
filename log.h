#pragma once

#define LOG(...)                                                               \
  do {                                                                         \
    emitLog(__FILE__, __LINE__, __func__, __VA_ARGS__);                        \
  } while (0)

void emitLog(const char* file, int line, const char* func, ...);
