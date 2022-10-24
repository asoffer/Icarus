#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::FunctionLiteral const* node,
                                      FunctionData data) {
  auto const& function_type =
      context().qualified_type(node).type().get<core::FunctionType>(
          type_system());
  size_t num_parameters = function_type.parameters().size();

  auto [fn_id, fn_ptr] = compiler_state().create_function(
      num_parameters, function_type.returns().size());

  base::flyweight_map<ast::Declaration::Id const*, size_t> variable_offsets;
  // Populate `variable_offsets`
  core::Bytes offset{};
  node->body_scope().ForEachNonConstantDeclaration(
      [&](ast::Declaration const* decl) {
        for (auto& id : decl->ids()) {
          variable_offsets.try_emplace(&id, offset.value());
          // TODO: Alignment.
          offset += SizeOf(context().qualified_type(&id).type(), type_system());
        }
      });
  fn_ptr->append<jasmin::StackAllocate>(offset.value());

  size_t parameter_index = 0;
  node->body_scope().ForEachNonConstantDeclaration(
      [&, fn_ptr = fn_ptr](ast::Declaration const* decl) {
        if (decl->flags() & ast::Declaration::f_IsFnParam) {
          for (auto& id : decl->ids()) {
            auto iter = variable_offsets.find(&id);
            fn_ptr->append<jasmin::StackOffset>(iter->second);
            fn_ptr->append<jasmin::DuplicateAt>(num_parameters -
                                                parameter_index);
            core::Bytes parameter_size =
                SizeOf(function_type.parameters()[parameter_index].value,
                       type_system());
            fn_ptr->append<jasmin::Store>(parameter_size.value());
            ++parameter_index;
          }
        }
      });

  FunctionData fn_data(*fn_ptr, variable_offsets);
  for (auto const* stmt : node->stmts()) {
    as<ByteCodeStatementEmitter>().Emit(stmt, fn_data);
  }

  data.function().append<jasmin::Push>(fn_ptr);
}

void ByteCodeStatementEmitter::operator()(ast::FunctionLiteral const*,
                                          FunctionData) {}

}  // namespace semantic_analysis
