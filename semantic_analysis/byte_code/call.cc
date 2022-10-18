#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Call const* node, FunctionData data) {
  // TODO: Remove this hack. We haven't yet figured out precisely how to
  // represent compile-time-only, or functions whose return type is dependent on
  // a compile-time parameter, but supporting `builtin.foreign` is an important
  // bootstrapping step, so we simply hard-code it here, and poorly.
  if (ast::Access const* access = node->callee()->if_as<ast::Access>();
      access and access->member_name() == "foreign") {
    core::Type fn_type = EvaluateAs<core::Type>(&node->arguments()[1].expr());
    EmitByteCode(&node->arguments()[0].expr(), data);
    data.function().append<BuiltinForeign>(fn_type, &foreign_function_map());
    return;
  }

  if (not node->named_arguments().empty()) { NOT_YET(); }

  for (auto const& argument : node->positional_arguments()) {
    EmitByteCode(&argument.expr(), data);
  }

  IrFunction const* callee;
  if (node->callee()->is<ast::Identifier>()) {
    IrFunction const* callee =
        compiler_state().function(context().callee_overload(node));
    data.function().append<jasmin::Push>(callee);
  } else {
    EmitByteCode(node->callee(), data);
  }

  data.function().append<jasmin::Call>();
}

}  // namespace semantic_analysis
