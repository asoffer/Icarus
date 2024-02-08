#ifndef ICARUS_COMMON_STRING_LITERAL_H
#define ICARUS_COMMON_STRING_LITERAL_H

#include <cstddef>
#include <string>
#include <string_view>

#include "common/strong_identifier_type.h"
#include "nth/container/flyweight_set.h"
#include "nth/debug/debug.h"
#include "nth/io/serialize/deserialize.h"
#include "nth/io/serialize/serialize.h"

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

  friend bool NthSerialize(auto &s, StringLiteral lit) {
    return nth::io::serialize(s, lit.str());
  }
  friend bool NthDeserialize(auto &d, StringLiteral &s) {
    std::string content;
    if (not nth::io::deserialize(d, content)) { return false; }
    s = StringLiteral(std::move(content));
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

  static nth::flyweight_set<std::string> const &All();

  friend void NthPrint(auto &p, auto &f, StringLiteral s) { f(p, s.str()); }
};

}  // namespace ic

#endif  // ICARUS_COMMON_STRING_LITERAL_H
