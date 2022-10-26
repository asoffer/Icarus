#include "semantic_analysis/byte_code/byte_code.h"

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void EmitByteCodeForModule(ast::Module const &ast_module, Context &context,
                           module::Module &module) {
  ByteCodeStatementEmitter e(context, module);
  base::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  // Populate `variable_offsets`
  core::Bytes offset{};
  ast_module.body_scope().ForEachNonConstantDeclaration(
      [&](ast::Declaration const *decl) {
        for (auto &id : decl->ids()) {
          variable_offsets.try_emplace(&id, offset.value());
          // TODO: Alignment.
          offset +=
              SizeOf(context.qualified_type(&id).type(), module.type_system());
        }
      });
  auto &f = module.initializer();
  f.append<jasmin::StackAllocate>(offset.value());
  e.Emit(&ast_module, FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();
}

IrFunction EmitByteCode(QualifiedType qualified_type,
                        ast::Expression const &expression, Context &context,
                        module::Module &module) {
  IrFunction f = PassInRegister(qualified_type, module.type_system())
                     ? IrFunction(0, 1)
                     : IrFunction(1, 0);
  ByteCodeValueEmitter e(context, module);
  base::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  e.Emit(&expression, FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();
  return f;
}

}  // namespace semantic_analysis
