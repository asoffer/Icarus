#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

QualifiedType Constant(core::Type t) {
  return QualifiedType(t, Qualifiers::Constant());
}

QualifiedType Constant(QualifiedType t) {
  return QualifiedType(t.type(), t.qualifiers() | Qualifiers::Constant());
}

}  // namespace semantic_analysis
