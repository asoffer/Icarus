#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

void Compiler::CompileModule(ast::Module const &node) {
    LOG("", "%s", node.DebugString());
  std::vector<Compiler::Task> tasks;
  tasks.reserve(node.stmts().size());
  for (auto const *stmt : node.stmts()) {
    tasks.push_back(TaskFor(stmt));
  }
  for (auto &task : tasks) {
    LOG("", "%s", node.DebugString());
    task.get<std::vector<QualifiedType>>();
  }

    LOG("", "%s", node.DebugString());
  nth::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  // Populate `variable_offsets`
  core::Bytes offset{};
  node.body_scope().ForEachNonConstantDeclaration(
      [&](ast::Declaration const *decl) {
        for (auto &id : decl->ids()) {
          variable_offsets.try_emplace(&id, offset.value());
          // TODO: Alignment.
          offset += SizeOf(context().qualified_type(&id).type(),
                           resources().primary_module().type_system());
        }
      });
    LOG("", "%s", node.DebugString());
  auto &f = resources().primary_module().initializer();
  f.AppendStackAllocate(offset.value());
  FunctionData data(f, variable_offsets);
  data.set_kind(EmitKind::Statement);
    LOG("", "%s", node.DebugString());
  for (auto &task : tasks) {
    LOG("", "%s", node.DebugString());
    task.send<FunctionData>(data);
    LOG("", "%s", node.DebugString());
    task.complete();
  }
  f.AppendReturn();
}

}  // namespace compiler
