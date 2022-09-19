#include "semantic_analysis/byte_code/emitter.h"
#include "type/primitive.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Terminal const* t, IrFunction& f) {
  if (context().qual_types(t)[0].type() == type::Bool) {
    f.append<jasmin::Push>(t->value().get<bool>());
  } else if (context().qual_types(t)[0].type() == type::Type_) {
    type::Type ty = t->value().get<type::Type>();
    if (ty == type::I8) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::I<8>(type_system())));
    } else if (ty == type::I16) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::I<16>(type_system())));
    } else if (ty == type::I32) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::I<32>(type_system())));
    } else if (ty == type::I64) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::I<64>(type_system())));
    } else if (ty == type::U8) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::U<8>(type_system())));
    } else if (ty == type::U16) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::U<16>(type_system())));
    } else if (ty == type::U32) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::U<32>(type_system())));
    } else if (ty == type::U64) {
      f.append<jasmin::Push>(
          core::Type(core::BuiltinType::U<64>(type_system())));
    }
  } else {
    NOT_YET();
  }
}

}  // namespace semantic_analysis
