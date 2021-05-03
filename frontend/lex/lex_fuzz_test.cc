#include <cstdint>
#include <iostream>
#include <string>

#include "diagnostic/consumer/trivial.h"
#include "frontend/lex/lex.h"
#include "frontend/source/buffer.h"
#include "test/fuzz.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  frontend::SourceBuffer buffer(test::Fuzzy<std::string>(data, length));

  diagnostic::TrivialConsumer diag;
  frontend::Lex(buffer, diag);

  return 0;
}
