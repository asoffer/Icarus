#include <string_view>

#include "diagnostic/consumer/trivial.h"
#include "frontend/lex/lex.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  diagnostic::TrivialConsumer diag;
  frontend::Lex(std::string_view(reinterpret_cast<char const*>(data), length),
                diag);
  return 0;
}
