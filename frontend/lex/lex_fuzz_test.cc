#include <cstdint>
#include <string>

#include "error/log.h"
#include "frontend/lex/lex.h"
#include "frontend/lex/lexeme.h"
#include "frontend/source/string.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  frontend::StringSource src(
      std::string(reinterpret_cast<char const*>(data), length));

  error::Log log(&src);
  frontend::LexState state(&src, &log);
  auto lexeme = NextToken(&state);

  return 0;
}
