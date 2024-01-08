#ifndef ICARUS_TYPE_REFINEMENT_H
#define ICARUS_TYPE_REFINEMENT_H

#include "ir/function.h"
#include "ir/type_erased_value.h"
#include "type/basic.h"

namespace ic::type {

struct RefinementType : internal_type::BasicType {
  explicit RefinementType() = default;

  Type underlying() const;

  friend void NthPrint(auto& p, auto& fmt, RefinementType r) {
    fmt(p, r.underlying());
    p.write("[??]");
  }

  bool operator()(TypeErasedValue const&) const;

 private:
  friend Type;
  friend void SerializeTypeSystem(TypeSystemProto&);
  friend void DeserializeTypeSystem(TypeSystemProto const&);
  friend RefinementType Refinement(Type, IrFunction const*);

  explicit constexpr RefinementType(uint64_t n)
      : internal_type::BasicType(Type::Kind::Refinement, n) {}
};

// TODO: Accepting an opaque function here is not a viable long-term strategy.
// We want users to be able to provide proofs that an object satisfies the
// refinement without actually invoking it.
RefinementType Refinement(Type t, IrFunction const* f);

}  // namespace ic::type

#endif  // ICARUS_TYPE_REFINEMENT_H
