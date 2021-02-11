#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/qual_type.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(ast::BlockLiteral const *node) {
  bool success = true;
  for (auto *b : node->before()) { success &= VerifyType(b)[0].ok(); }
  for (auto *a : node->after()) { success &= VerifyType(a)[0].ok(); }
  auto qt = type::QualType::Constant(type::Block);
  if (not success) { qt.MarkError(); }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
