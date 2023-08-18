#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

Compiler::Task Compiler::TaskFor(ast::ReturnStmt const* node) {
  std::vector<QualifiedType> return_types;

  std::vector<Compiler::Task> expr_tasks;
  expr_tasks.reserve(node->exprs().size());
  for (auto const* expr : node->exprs()) {
    expr_tasks.push_back(TaskFor(expr));
  }
  for (auto& task : expr_tasks) {
    auto qts = task.get<std::vector<QualifiedType>>();
    for (QualifiedType qt : qts) {
      if (qt.qualifiers() >= Qualifiers::Error()) {
        NTH_UNIMPLEMENTED();
      } else {
        return_types.push_back(qt);
      }
    }
  }

  context().set_return_types(node, std::move(return_types));
  co_yield std::vector{QualifiedType(NoReturn)};
  std::span function_return_types =
      context()
          .qualified_type(&node->function_literal())
          .type()
          .get<core::FunctionType>(GlobalTypeSystem())
          .returns();
  auto data = co_await nth::type<FunctionData>;
  KindModifier modifier(data, EmitKind::Value);
  auto return_types_iter = function_return_types.begin();
  for (auto& task : expr_tasks) {
    task.send<FunctionData>(data);
    CastTo(expr, QualifiedType(*return_types_iter++), data);
  }
  data.function().AppendReturn();
}

}  // namespace compiler
