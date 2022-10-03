#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Call const* node, IrFunction& f) {
  // TODO: Remove this hack. We haven't yet figured out precisely how to
  // represent compile-time-only, or functions whose return type is dependent on
  // a compile-time parameter, but supporting `builtin.foreign` is an important
  // bootstrapping step, so we simply hard-code it here, and poorly.
  if (ast::Access const* access = node->callee()->if_as<ast::Access>();
      access and access->member_name() == "foreign") {
    std::optional fn_type =
        EvaluateAs<core::Type>(&node->arguments()[1].expr());
    if (not fn_type) { NOT_YET(); }

    EmitByteCode(&node->arguments()[0].expr(), f);
    f.append<BuiltinForeign>(*fn_type, &builtin_module());
    return;
  }

  NOT_YET();
}

}  // namespace semantic_analysis
