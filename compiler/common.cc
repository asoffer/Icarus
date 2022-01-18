#include "compiler/common.h"

#include "ast/scope.h"
#include "base/debug.h"

namespace compiler {

module::BasicModule *ModuleFor(ast::Node const *node) {
  return &ASSERT_NOT_NULL(ASSERT_NOT_NULL(node)->scope())->module();
}

frontend::SourceBuffer const *SourceBufferFor(ast::Node const *node) {
  return &ModuleFor(node)->buffer();
}

frontend::SourceView SourceViewFor(ast::Node const *node) {
  return frontend::SourceView(SourceBufferFor(node), node->range());
}

}  // namespace compiler
