#include "compiler/common.h"

#include "ast/scope.h"
#include "base/debug.h"
#include "compiler/module.h"

namespace compiler {

module::Module *ModuleFor(ast::Node const *node) {
  return &ASSERT_NOT_NULL(ASSERT_NOT_NULL(node)->scope())->module();
}

}  // namespace compiler
