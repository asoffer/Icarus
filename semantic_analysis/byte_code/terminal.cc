#include "semantic_analysis/byte_code/emitter.h"
#include "type/primitive.h"
#include "ir/value/slice.h"
#include "type/slice.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Terminal const* node, IrFunction& f) {
  QualifiedType qt = context().qualified_type(node);
  if (qt.type() == Bool) {
    f.append<jasmin::Push>(node->value().get<bool>());
  } else if (qt.type() == SliceType(type_system(), Char)) {
    ir::Slice slice = node->value().get<ir::Slice>();
    f.append<jasmin::Push>(slice.data());
    f.append<jasmin::Push>(slice.length());
  } else if (qt.type() == Integer) {
    // TODO: Store the integer in some shared manager type.
    auto* i = new ir::Integer(node->value().get<ir::Integer>());
    f.append<Construct<ir::Integer>>(i);
  } else if (qt.type() == F64) {
    // TODO: Long-term these won't be doubles, but rather a "rational" type that
    // is arbitrary-precision.
    f.append<jasmin::Push>(node->value().get<double>());
  } else if (qt.type() == Type) {
    type::Type ty = node->value().get<type::Type>();
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
    } else if (ty == type::F32) {
      f.append<jasmin::Push>(F32);
    } else if (ty == type::F64) {
      f.append<jasmin::Push>(F64);
    } else if (ty == type::Type_) {
      f.append<jasmin::Push>(Type);
    } else if (ty == type::Integer) {
      f.append<jasmin::Push>(Integer);
    } else {
      NOT_YET(node->DebugString());
    }
  } else {
    NOT_YET(node->DebugString());
  }
}

}  // namespace semantic_analysis
