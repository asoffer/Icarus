#include <cstdint>
#include <string>

#include "error/log.h"
#include "frontend/lex/lex.h"
#include "frontend/source/cursor.h"
#include "frontend/source/string.h"

extern "C" int LLVMFuzzerTestOneInput(uint8_t const* data, size_t length) {
  if (length == 0 or data[0] != '#') { return 0; }
  frontend::StringSource src(
      std::string(reinterpret_cast<char const*>(data), length));

  error::Log log(&src);
  frontend::LexState state(&src, &log);

  frontend::NextHashtag(&state.cursor_, &src);

  return 0;
}
