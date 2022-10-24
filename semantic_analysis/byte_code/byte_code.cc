#include "semantic_analysis/byte_code/byte_code.h"

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void EmitByteCodeForModule(ast::Module const &module, Context &context,
                           CompilerState &compiler_state) {
  ByteCodeStatementEmitter e(context, compiler_state);
  base::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  // Populate `variable_offsets`
  core::Bytes offset{};
  module.body_scope().ForEachNonConstantDeclaration(
      [&](ast::Declaration const *decl) {
        for (auto &id : decl->ids()) {
          variable_offsets.try_emplace(&id, offset.value());
          // TODO: Alignment.
          offset += SizeOf(context.qualified_type(&id).type(),
                           compiler_state.type_system());
        }
      });
  auto &f = compiler_state.module().initializer();
  f.append<jasmin::StackAllocate>(offset.value());

  // size_t parameter_index = 0;
  // module.body_scope().ForEachNonConstantDeclaration(
  //     [&, fn_ptr = fn_ptr](ast::Declaration const* decl) {
  //       if (decl->flags() & ast::Declaration::f_IsFnParam) {
  //         for (auto& id : decl->ids()) {
  //           auto iter = variable_offsets.find(&id);
  //           fn_ptr->append<jasmin::StackOffset>(iter->second);
  //           fn_ptr->append<jasmin::DuplicateAt>(num_parameters -
  //                                               parameter_index);
  //           core::Bytes parameter_size =
  //               SizeOf(function_type.parameters()[parameter_index].value,
  //                      type_system());
  //           fn_ptr->append<jasmin::Store>(parameter_size.value());
  //           ++parameter_index;
  //         }
  //       }
  //     });

  e.Emit(&module, FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();
}

IrFunction EmitByteCode(QualifiedType qualified_type,
                        ast::Expression const &expression, Context &context,
                        CompilerState &compiler_state) {
  IrFunction f = PassInRegister(qualified_type, compiler_state.type_system())
                     ? IrFunction(0, 1)
                     : IrFunction(1, 0);
  ByteCodeValueEmitter e(context, compiler_state);
  base::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  e.Emit(&expression, FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();
  return f;
}

}  // namespace semantic_analysis
