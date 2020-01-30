#include <cstdint>
#include <string>

#include "diagnostic/consumer/trivial.h"
#include "frontend/lex/lex.h"
#include "frontend/source/cursor.h"
#include "frontend/source/string.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  if (length == 0 or not('0' <= data[0] and data[0] <= '9')) { return 0; }

  frontend::StringSource src(test::Fuzzy<std::string>(data, length));

  frontend::SourceCursor cursor(
      frontend::SourceLoc(frontend::LineNum(1), frontend::Offset(0)),
      src.ReadUntil('\n').view);
  diagnostic::TrivialConsumer diag;
  frontend::NextNumber(&cursor, &src, diag);

  return 0;
}
