#include "compiler/module.h"

#include "ast/ast.h"

namespace compiler {

type::Type const *CompiledModule::type_of(ast::Expression const *expr) const {
  auto const *result = data_.result(expr);
  if (result and result->type()) { return result->type(); }

  // TODO embedded modules?
  return nullptr;
}

}  // namespace compiler
