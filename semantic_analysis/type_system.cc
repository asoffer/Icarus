#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

QualifiedType Constant(core::Type t) {
  return QualifiedType(t, Qualifiers::Constant());
}

QualifiedType Constant(QualifiedType t) {
  return QualifiedType(t.type(), t.qualifiers() | Qualifiers::Constant());
}

QualifiedType Reference(core::Type t) {
  return QualifiedType(t, Qualifiers::Reference());
}

QualifiedType Reference(QualifiedType t) {
  return QualifiedType(t.type(), t.qualifiers() | Qualifiers::Reference());
}

QualifiedType Error() { return QualifiedType(ErrorType, Qualifiers::Error()); }

QualifiedType Error(QualifiedType t) {
  return QualifiedType(t.type(), t.qualifiers() | Qualifiers::Error());
}

QualifiedType Error(Qualifiers q) {
  return QualifiedType(ErrorType, q | Qualifiers::Error());
}

}  // namespace semantic_analysis
