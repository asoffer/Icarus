#include "nth/container/flyweight_map.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::FunctionLiteral const* node,
                                      FunctionData data) {
  auto const& function_type =
      context().qualified_type(node).type().get<core::FunctionType>(
          type_system());
  size_t num_parameters = function_type.parameters().size();

  auto [fn_index, fn_ptr] = module().function_table().emplace(
      num_parameters, function_type.returns().size());

  nth::flyweight_map<ast::Declaration::Id const*, size_t> variable_offsets;
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
            core::Bytes parameter_size =
                SizeOf(function_type.parameters()[parameter_index].value,
                       type_system());
            if (parameter_size.value() <= jasmin::ValueSize) {
              fn_ptr->append<jasmin::StackOffset>(iter->second);
              fn_ptr->append<jasmin::DuplicateAt>(num_parameters -
                                                  parameter_index);
              fn_ptr->append<jasmin::Store>(parameter_size.value());
            } else if (function_type.parameters()[parameter_index]
                           .value.is<SliceType>(type_system())) {
              fn_ptr->append<jasmin::StackOffset>(iter->second);
              fn_ptr->append<IncrementPointer>(jasmin::ValueSize);
              fn_ptr->append<jasmin::Swap>();
              fn_ptr->append<jasmin::Store>(jasmin::ValueSize);

              fn_ptr->append<jasmin::StackOffset>(iter->second);
              fn_ptr->append<jasmin::Swap>();
              fn_ptr->append<jasmin::Store>(jasmin::ValueSize);
            } else {
              NOT_YET();
            }
            ++parameter_index;
          }
        }
      });

  FunctionData fn_data(*fn_ptr, variable_offsets);
  for (auto const* stmt : node->stmts()) {
    as<ByteCodeStatementEmitter>().Emit(stmt, fn_data);
  }
  fn_data.function().append<jasmin::Return>();

  data.function().append<jasmin::Push>(fn_ptr);
}

void ByteCodeStatementEmitter::operator()(ast::FunctionLiteral const*,
                                          FunctionData) {}

}  // namespace semantic_analysis
