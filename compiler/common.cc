#include "compiler/common.h"

#include "ast/scope.h"
#include "base/debug.h"

namespace compiler {

module::BasicModule *ModuleFor(ast::Node const *node) {
  auto &scope     = *ASSERT_NOT_NULL(node->scope());
  auto &mod_scope = *ASSERT_NOT_NULL(scope.Containing<ast::ModuleScope>());
  return ASSERT_NOT_NULL(mod_scope.module());
}

frontend::SourceBuffer const *SourceBufferFor(ast::Node const *node) {
  return &ModuleFor(node)->buffer();
}

frontend::SourceView SourceViewFor(ast::Node const *node) {
  return frontend::SourceView(SourceBufferFor(node), node->range());
}

}  // namespace compiler

