#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Call const* node,
                                      FunctionData data) {
  // TODO: Remove this hack. We haven't yet figured out precisely how to
  // represent compile-time-only, or functions whose return type is dependent on
  // a compile-time parameter, but supporting `builtin.foreign` is an important
  // bootstrapping step, so we simply hard-code it here, and poorly.
  if (ast::Access const* access = node->callee()->if_as<ast::Access>();
      access and access->member_name() == "foreign") {
    core::Type fn_type = EvaluateAs<core::Type>(&node->arguments()[1].expr());
    Emit(&node->arguments()[0].expr(), data);
    auto function_type    = fn_type.get<core::FunctionType>(type_system());
    size_t num_parameters = function_type.parameters().size();
    data.function().append<BuiltinForeign>(fn_type, &foreign_function_map(),
                                           &type_system());
    return;
  }

  if (not node->named_arguments().empty()) { NOT_YET(); }

  auto const& callable_identifier = context().callee(node);

  if (auto const* expr = callable_identifier.expression()) {
    auto const& parameters = context()
                                 .qualified_types(expr)[0]
                                 .type()
                                 .get<core::FunctionType>(type_system())
                                 .parameters();

    auto iter = parameters.begin();
    for (auto const& argument : node->positional_arguments()) {
      CastTo(&argument.expr(), QualifiedType(iter->value), data);
      ++iter;
    }

    Emit(expr, data);
  } else {
    data.function().append<PushFunction>(resources().TranslateToPrimary(
        callable_identifier.function().function));
  }

  auto* f = data.function().raw_instructions().back().as<IrFunction const*>();
  data.function().append<jasmin::Call>();
}

void ByteCodeStatementEmitter::operator()(ast::Call const* node,
                                          FunctionData data) {
  this->as<ByteCodeValueEmitter>().Emit(node, data);
  // TODO: Drop any unnecessary return values. Counting is more subtle than
  // this:
  data.function().append<jasmin::Drop>(context().qualified_types(node).size());
}

}  // namespace semantic_analysis
