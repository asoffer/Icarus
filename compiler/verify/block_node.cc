#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"

namespace compiler {

type::QualType Compiler::VerifyType(ast::BlockNode const *node) {
  for (auto &param : node->params()) { VerifyType(param.value.get()); }
  for (auto *stmt : node->stmts()) { VerifyType(stmt); }

  absl::Span<ast::YieldStmt const *const> yields = context().YieldsTo(node);
  std::vector<core::Arguments<type::Type>> yield_types;
  yield_types.reserve(yields.size());
  for (auto const *yield_stmt : yields) {
    auto &yielded = yield_types.emplace_back();
    for (const auto *expr : yield_stmt->exprs()) {
      // TODO: Support named yields.
      yielded.pos_emplace(ASSERT_NOT_NULL(context().qual_type(expr))->type());
    }
  }
  context().set_yield_types(node, std::move(yield_types));

  return context().set_qual_type(node, type::QualType::Constant(type::Block));
}

}  // namespace compiler
