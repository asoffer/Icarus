#include <string_view>

#include "diagnostic/consumer/trivial.h"
#include "frontend/lex/lex.h"
#include "frontend/parse.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  diagnostic::TrivialConsumer diag;
  auto lex_result = frontend::Lex(
      std::string_view(reinterpret_cast<char const*>(data), length), diag);
  if (not lex_result) { return 0; }
  frontend::ParseModule(lex_result->lexemes_);

  return 0;
}
