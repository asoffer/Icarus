#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

void Compiler::CompileModule(ast::Module const &node) {
  std::vector<Compiler::Task> tasks;
  tasks.reserve(node.stmts().size());
  for (auto const *stmt : node.stmts()) {
    tasks.push_back(TaskFor(stmt));
  }
  for (auto &task : tasks) {
    task.get<std::vector<QualifiedType>>();
  }

  nth::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  // Populate `variable_offsets`
  core::Bytes offset{};
  node.body_scope().ForEachNonConstantDeclaration(
      [&](ast::Declaration const *decl) {
        for (auto &id : decl->ids()) {
          variable_offsets.try_emplace(&id, offset.value());
          // TODO: Alignment.
          offset +=
              SizeOf(context().qualified_type(&id).type(), GlobalTypeSystem);
        }
      });
  auto &f = resources().primary_module().initializer();
  f.AppendStackAllocate(offset.value());
  FunctionData data(f, variable_offsets);
  data.set_kind(EmitKind::Statement);
  for (auto &task : tasks) {
    task.send<FunctionData>(data);
    task.complete();
  }
  f.AppendReturn();
}

}  // namespace compiler
