#include "compiler/module.h"

#include "ast/ast.h"

namespace compiler {

type::QualType const *CompiledModule::qual_type_of(
    ast::Expression const *expr) const {
  return data_.result(expr);
}

type::Type const *CompiledModule::type_of(ast::Expression const *expr) const {
  auto const *result = data_.result(expr);
  return result ? result->type() : nullptr;
}

}  // namespace compiler
