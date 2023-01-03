#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_CASTING_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_CASTING_H

#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

// Represents the type of cast which can be performed.
enum class CastKind {
  // No cast can be made between the types.
  None,

  // A cast can be made but must be made explicitly.
  Explicit,

  // A cast can be made implicitly.
  Implicit,

  // A reference to the type can be converted to a reference to the other type.
  InPlace,
};

// Returns an enum indicating what kind of cast (if any) is allowed between the
// two types.
CastKind CanCast(QualifiedType from, core::Type to, TypeSystem& type_system);

core::Type CommonType(core::Type lhs, core::Type rhs, TypeSystem& type_system);

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_CASTING_H
