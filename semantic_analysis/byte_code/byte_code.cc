#include "semantic_analysis/byte_code/byte_code.h"

#include "nth/container/flyweight_map.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void EmitByteCodeForModule(ast::Module const &ast_module, Context &context,
                           module::Resources &resources) {
  ByteCodeStatementEmitter e(context, resources);
  nth::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  // Populate `variable_offsets`
  core::Bytes offset{};
  ast_module.body_scope().ForEachNonConstantDeclaration(
      [&](ast::Declaration const *decl) {
        for (auto &id : decl->ids()) {
          variable_offsets.try_emplace(&id, offset.value());
          // TODO: Alignment.
          offset += SizeOf(context.qualified_type(&id).type(),
                           resources.primary_module().type_system());
        }
      });
  auto &f = resources.primary_module().initializer();
  f.append<jasmin::StackAllocate>(offset.value());
  e.Emit(&ast_module, FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();
}

IrFunction EmitByteCode(QualifiedType qualified_type,
                        ast::Expression const &expression, Context &context,
                        module::Resources &resources) {
  core::TypeContour contour = ContourOf(
      qualified_type.type(), resources.primary_module().type_system());
  size_t values_needed = contour.bytes().value() / jasmin::ValueSize +
                         (((contour.bytes().value() % jasmin::ValueSize) != 0));
  IrFunction f(0, values_needed);
  ByteCodeValueEmitter e(context, resources);

  // This `variable_offsets` map is intentionally empty. There will never be
  // declarations from which data needs to be loaded. Because `EvaluateConstant`
  // is only to be called on constant expressions, any identifier will refer to
  // a declaration that is constant, and so lookup will happen by loading the
  // value directly rather than adding instructions which load at runtime.
  nth::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  e.Emit(&expression, FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();
  return f;
}

std::span<std::byte const> EvaluateConstant(Context &context,
                                            module::Resources &resources,
                                            ast::Expression const *expr,
                                            QualifiedType qt) {
  ASSERT(qt == context.qualified_type(expr));
  auto [result_ptr, inserted] = context.insert_constant(expr);
  if (inserted) {
    // TODO: Integers are an annoying special case at the moment.
    core::TypeContour contour = ContourOf(qt.type(), resources.primary_module().type_system());
    if (PassInRegister(contour)) {
      IrFunction f = EmitByteCode(qt, *expr, context, resources);

      data_types::IntegerTable table;
      jasmin::ValueStack value_stack;
      jasmin::Execute(f, jasmin::ExecutionState<InstructionSet>{table},
                      value_stack);
      result_ptr->resize(contour.bytes().value());
      std::byte *data = result_ptr->data();
      for (std::byte *ptr = data + result_ptr->size() - jasmin::ValueSize;
           ptr >= data; ptr -= jasmin::ValueSize) {
        jasmin::Value::Store(value_stack.pop_value(), ptr, jasmin::ValueSize);
      }
      return *result_ptr;
    } else {
      NOT_YET(DebugQualifiedType(qt, resources.primary_module().type_system()));
    }
  } else {
    return *result_ptr;
  }
}

}  // namespace semantic_analysis
