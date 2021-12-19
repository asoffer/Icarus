#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::ScopeLiteral const *node) {
  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),  //
                   auto params, VerifyParams(node->params()));
  return context().set_qual_type(node,
                                 type::QualType::Constant(type::UnboundScope));
}

// {
//   LOG("ScopeLiteral", "Verifying body of %p: %s", node, node->DebugString());
//   context().set_qual_type(&node->context(),
//                           type::QualType::Constant(type::ScopeContext));
//   ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
//                    auto params, VerifyParams(node->params()));
//   auto qt = type::QualType::Constant(type::Scp(std::move(params)));
// 
//   for (auto const *stmt : node->stmts()) {
//     absl::Span<type::QualType const> stmt_qts = VerifyType(stmt);
//     if (stmt_qts.size() == 1 and not stmt_qts[0].ok()) { qt.MarkError(); }
//   }
// 
//   return context().set_qual_type(node, qt);
// }

}  // namespace compiler
