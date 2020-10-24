#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/qual_type.h"

namespace compiler {

type::QualType Compiler::VerifyType(ast::BlockLiteral const *node) {
  bool success = true;
  for (auto *b : node->before()) { success &= VerifyType(b).ok(); }
  for (auto *a : node->after()) { success &= VerifyType(a).ok(); }
  auto qt = type::QualType::Constant(type::Block);
  if (not success) { qt.MarkError(); }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
