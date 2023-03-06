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
    core::Type t = EvaluateAs<core::Type>(&node->arguments()[1].expr());
    Emit(&node->arguments()[0].expr(), data);
    if (auto function_type = t.get_if<core::FunctionType>(type_system())) {
      size_t num_parameters = function_type->parameters().size();
      data.function().AppendBuiltinForeignFunction(
          t, &module().function_table(), &module().foreign_symbol_map(),
          &type_system());
    } else {
      data.function().AppendBuiltinForeignPointer(t);
    }
    return;
  }

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

    for (; iter != parameters.end(); ++iter) {
      for (auto const& argument : node->named_arguments()) {
        if (argument.name() != iter->name) { continue; }
        CastTo(&argument.expr(), QualifiedType(iter->value), data);
        goto next_expr;
      }
      
      NOT_YET("Default arguments not yet supported.");
    next_expr:;
    }

    Emit(expr, data);
  } else {
    auto typed_function = callable_identifier.function();

    auto const& parameters =
        typed_function.type.get<core::FunctionType>(type_system()).parameters();

    auto iter = parameters.begin();
    for (auto const& argument : node->positional_arguments()) {
      CastTo(&argument.expr(), QualifiedType(iter->value), data);
      ++iter;
    }

    for (; iter != parameters.end(); ++iter) {
      for (auto const& argument : node->named_arguments()) {
        if (argument.name() != iter->name) { continue; }
        CastTo(&argument.expr(), QualifiedType(iter->value), data);
        goto next_typed_fn;
      }
      NOT_YET("Default arguments not yet supported.");
    next_typed_fn:;
    }

    auto& f = module().function_table().function(typed_function.function);
    data.function().AppendPushFunction(&module().function_table().function(
        callable_identifier.function().function));
  }

  auto* f = data.function().raw_instructions().back().as<vm::Function const*>();
  data.function().AppendCall();
}

void ByteCodeStatementEmitter::operator()(ast::Call const* node,
                                          FunctionData data) {
  this->as<ByteCodeValueEmitter>().Emit(node, data);
  // TODO: Drop any unnecessary return values. Counting is more subtle than
  // this:
  data.function().AppendDrop(context().qualified_types(node).size());
}

}  // namespace semantic_analysis
