#ifndef ICARUS_COMMON_STRING_LITERAL_H
#define ICARUS_COMMON_STRING_LITERAL_H

#include <cstddef>
#include <string>
#include <string_view>

#include "common/strong_identifier_type.h"
#include "jasmin/serialize/reader.h"
#include "jasmin/serialize/writer.h"
#include "nth/container/flyweight_set.h"
#include "nth/debug/debug.h"

namespace ic {

struct StringLiteral : private StrongIdentifierType<StringLiteral, uint32_t> {
  StringLiteral();
  StringLiteral(std::string &&s);
  StringLiteral(std::string const &s);
  StringLiteral(std::string_view s);

  size_t index() const {
    return StrongIdentifierType<StringLiteral, uint32_t>::value();
  }

  std::string_view str() const;

  friend void JasminSerialize(jasmin::Writer auto &w, StringLiteral s) {
    std::string_view str = s.str();
    jasmin::WriteInteger(w, str.size());
    w.write(std::span<std::byte const>(
        reinterpret_cast<std::byte const *>(str.data()), str.size()));
  }
  friend bool JasminDeserialize(jasmin::Reader auto &r, StringLiteral &s) {
    size_t n;
    if (not jasmin::ReadInteger(r, n)) { return false; }
    std::string str(n, '\0');
    if (not r.read(std::span<std::byte>(
            reinterpret_cast<std::byte *>(str.data()), str.size()))) {
      return false;
    }
    s = StringLiteral(std::move(s));
    return true;
  }

  friend bool operator==(StringLiteral lhs, StringLiteral rhs) {
    return lhs.value() == rhs.value();
  }

  friend bool operator!=(StringLiteral lhs, StringLiteral rhs) {
    return lhs.value() != rhs.value();
  }

  template <typename H>
  friend H AbslHashValue(H h, StringLiteral s) {
    return H::combine(std::move(h), s.value());
  }

  friend void NthPrint(auto &p, auto &f, StringLiteral s) { f(p, s.str()); }
};

}  // namespace ic

#endif  // ICARUS_COMMON_STRING_LITERAL_H
