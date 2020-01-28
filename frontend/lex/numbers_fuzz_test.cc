#include <cstdint>
#include <string_view>

#include "frontend/lex/numbers.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  frontend::ParseNumber(
      std::string_view(reinterpret_cast<char const*>(data), length));

  return 0;
}
