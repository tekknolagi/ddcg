#include "log.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void emitLog(const char* file, int line, const char* func, ...) {
  // fprintf(stderr, "%s:%d %s: ", file, line, func);
  va_list args;
  va_start(args, func);
  const char* fmt = va_arg(args, const char*);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fputc('\n', stderr);
}
