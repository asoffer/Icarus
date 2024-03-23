#ifndef ICARUS_COMMON_IDENTIFIER_H
#define ICARUS_COMMON_IDENTIFIER_H

#include <string>
#include <string_view>

#include "common/internal/constant_handle.h"

namespace ic {

// Represents an identifier in a program.
struct Identifier : internal_constants::ConstantHandle<Identifier> {
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

  friend void NthPrint(auto& p, auto& f, Identifier id) {
    p.write("`");
    f(p, static_cast<std::string const&>(id));
    p.write("`");
  }

  explicit operator std::string const&() const;

 private:
  friend ConstantHandle;

  explicit Identifier(internal_constants::from_representation_t, uint32_t n)
      : ConstantHandle(n) {}
};

}  // namespace ic

#endif  // ICARUS_COMMON_IDENTIFIER_H
