#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"

namespace compiler {

type::QualType Compiler::VerifyType(ast::BlockNode const *node) {
  for (auto &param : node->params()) { VerifyType(param.value.get()); }
  for (auto *stmt : node->stmts()) { VerifyType(stmt); }

  return context().set_qual_type(node, type::QualType::Constant(type::Block));
}

}  // namespace compiler
