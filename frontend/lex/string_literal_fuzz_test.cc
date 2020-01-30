#include <cstdint>
#include <string>

#include "frontend/lex/lex.h"
#include "frontend/source/cursor.h"
#include "frontend/source/string.h"
#include "test/fuzz.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  if (length == 0) { return 0; }
  if (data[0] != '"') { return 0; }

  frontend::StringSource src(test::Fuzzy<std::string>(data, length));

  frontend::SourceCursor cursor(
      frontend::SourceLoc(frontend::LineNum(1), frontend::Offset(0)),
      src.ReadUntil('\n').view);

  NextStringLiteral(&cursor, &src);

  return 0;
}
