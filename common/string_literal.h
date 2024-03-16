#ifndef ICARUS_COMMON_STRING_LITERAL_H
#define ICARUS_COMMON_STRING_LITERAL_H

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

#include "common/internal/constant_handle.h"
#include "nth/debug/debug.h"

namespace ic {

struct StringLiteral : internal_constants::ConstantHandle<StringLiteral> {
  using backing_type = std::string;

  StringLiteral();
  StringLiteral(std::string &&s);
  StringLiteral(std::string const &s);
  StringLiteral(std::string_view s);

  size_t index() const {
    return StrongIdentifierType<StringLiteral, uint32_t>::value();
  }

  explicit operator std::string const &() const;

  static void CompleteGeneration() {
    // TODO
  }

  static std::vector<std::string_view> LatestGeneration() {
    // TODO
    return {};
  }
};

}  // namespace ic

#endif  // ICARUS_COMMON_STRING_LITERAL_H
