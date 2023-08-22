#ifndef ICARUS_COMMON_STRING_H
#define ICARUS_COMMON_STRING_H

#include <string>

#include "nth/io/string_printer.h"
#include "nth/strings/format/universal.h"
#include "nth/strings/interpolate.h"

namespace ic {

template <nth::InterpolationString I, typename... Args>
std::string InterpolateString(Args const&... args) {
  nth::universal_formatter f({
      .depth    = 3,
      .fallback = "...",
  });
  std::string s;
  nth::string_printer p(s);
  nth::Interpolate<I>(p, f, args...);
  return s;
}

}  // namespace ic

#endif // ICARUS_COMMON_STRING_H
