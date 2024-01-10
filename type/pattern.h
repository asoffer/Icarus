#ifndef ICARUS_TYPE_PATTERN_H
#define ICARUS_TYPE_PATTERN_H

#include "type/basic.h"

namespace ic::type {

struct PatternType : internal_type::BasicType {
  explicit PatternType() = default;

  Type match_type() const;

  friend void NthPrint(auto& p, auto& fmt, PatternType pt) {
    p.write("pattern(");
    fmt(p, pt.match_type());
    p.write(")");
  }

 private:
  friend Type;
  friend void SerializeTypeSystem(TypeSystemProto&);
  friend Type Deserialize(TypeProto const&, TypeSystemProto const&);
  friend PatternType Pattern(Type);

  explicit constexpr PatternType(uint64_t n)
      : internal_type::BasicType(Type::Kind::Pattern, n) {}
};

PatternType Pattern(Type t);

}  // namespace ic::type

#endif  // ICARUS_TYPE_PATTERN_H
