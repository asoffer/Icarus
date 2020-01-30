#include <cstdint>
#include <iostream>
#include <string>

#include "diagnostic/consumer/trivial.h"
#include "frontend/lex/lex.h"
#include "frontend/source/string.h"
#include "test/fuzz.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  frontend::StringSource src(test::Fuzzy<std::string>(data, length));

  diagnostic::TrivialConsumer diag;
  frontend::LexState state(&src, diag);

  while (NextToken(&state).tag() != frontend::eof) {}

  return 0;
}
