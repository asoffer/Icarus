#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(ast::Module const *node) {
  for (auto const *stmt : node->stmts()) { VerifyType(stmt); }
  return {};
}

}  // namespace compiler
