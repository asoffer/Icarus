#ifndef ICARUS_TYPE_PATTERN_H
#define ICARUS_TYPE_PATTERN_H

#include "type/type.h"

namespace ic::type {

struct PatternType : Type {
  // A value of `*this` type is a pattern that can be used to match values of
  // type `match_type()`.
  Type match_type() const;

  friend void NthPrint(auto& p, auto& fmt, PatternType pt) {
    p.write("pattern(");
    fmt(p, pt.match_type());
    p.write(")");
  }

 private:
  friend Type;
  friend PatternType Pattern(Type);

  explicit PatternType() = default;
  explicit constexpr PatternType(uint32_t n) : Type(Type::Kind::Pattern, n) {}
};

PatternType Pattern(Type t);

}  // namespace ic::type

#endif  // ICARUS_TYPE_PATTERN_H
