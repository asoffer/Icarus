#include "semantic_analysis/byte_code/emitter.h"
#include "type/primitive.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Terminal const* t, IrFunction& f) {
  QualifiedType qt = context().qualified_type(t);
  if (qt.type() == Bool) {
    f.append<jasmin::Push>(t->value().get<bool>());
  } else if (qt.type() == Type) {
    type::Type ty = t->value().get<type::Type>();
    if (ty == type::I8) {
      f.append<jasmin::Push>(I(8));
    } else if (ty == type::I16) {
      f.append<jasmin::Push>(I(16));
    } else if (ty == type::I32) {
      f.append<jasmin::Push>(I(32));
    } else if (ty == type::I64) {
      f.append<jasmin::Push>(I(64));
    } else if (ty == type::U8) {
      f.append<jasmin::Push>(U(8));
    } else if (ty == type::U16) {
      f.append<jasmin::Push>(U(16));
    } else if (ty == type::U32) {
      f.append<jasmin::Push>(U(32));
    } else if (ty == type::U64) {
      f.append<jasmin::Push>(U(64));
    } else if (ty == type::Bool) {
      f.append<jasmin::Push>(Bool);
    } else if (ty == type::Char) {
      f.append<jasmin::Push>(Char);
    } else if (ty == type::Type_) {
      f.append<jasmin::Push>(Type);
    }
  } else {
    NOT_YET();
  }
}

}  // namespace semantic_analysis
