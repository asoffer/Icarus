#include "ast/ast.h"
#include "compiler/verify/verify.h"

namespace compiler {

absl::Span<type::QualType const> TypeVerifier::VerifyType(ast::Module const *node) {
  for (auto const *stmt : node->stmts()) { VerifyType(stmt); }
  return {};
}

}  // namespace compiler
