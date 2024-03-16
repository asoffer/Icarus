#ifndef ICARUS_COMMON_IDENTIFIER_H
#define ICARUS_COMMON_IDENTIFIER_H

#include <string>
#include <string_view>

#include "common/internal/constant_handle.h"

namespace ic {

// Represents an identifier in a program.
struct Identifier : internal_constants::ConstantHandle<Identifier> {
  using backing_type = std::string;

  static Identifier FromRepresentation(uint32_t n) {
    return Identifier(raw_t{}, n);
  }
  static uint32_t ToRepresentation(Identifier id) { return id.value(); }

  Identifier();
  template <int N>
  Identifier(char const (&s)[N])
      : Identifier(([&] { NTH_REQUIRE(s[N - 1] == '\0'); }(),
                    std::string_view(s, N - 1))) {}
  Identifier(std::string&& s);
  Identifier(std::string const& s);
  Identifier(std::string_view s);

  size_t index() const {
    return StrongIdentifierType<Identifier, uint32_t>::value();
  }

  explicit operator std::string const&() const;

 private:
  struct raw_t {};
  explicit Identifier(raw_t, uint32_t n) : ConstantHandle(n) {}
};

}  // namespace ic

#endif  // ICARUS_COMMON_IDENTIFIER_H
