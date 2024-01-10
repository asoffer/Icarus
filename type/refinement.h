#ifndef ICARUS_TYPE_REFINEMENT_H
#define ICARUS_TYPE_REFINEMENT_H

#include "common/any_value.h"
#include "common/pattern.h"
#include "type/basic.h"
#include "type/type_system.pb.h"

namespace ic::type {

struct RefinementType : internal_type::BasicType {
  explicit RefinementType() = default;

  Type underlying() const;

  friend void NthPrint(auto& p, auto& fmt, RefinementType r) {
    fmt(p, r.underlying());
    p.write("[??]");
  }

  bool operator()(AnyValue const&) const;

 private:
  friend Type;
  friend void SerializeTypeSystem(TypeSystemProto&);
  friend Type Deserialize(TypeProto const&, TypeSystemProto const&);
  friend RefinementType Refinement(Type, ::ic::Pattern p);

  explicit constexpr RefinementType(uint64_t n)
      : internal_type::BasicType(Type::Kind::Refinement, n) {}
};

// TODO: Accepting an opaque function here is not a viable long-term strategy.
// We want users to be able to provide proofs that an object satisfies the
// refinement without actually invoking it.
RefinementType Refinement(Type t, ::ic::Pattern p);

}  // namespace ic::type

#endif  // ICARUS_TYPE_REFINEMENT_H
