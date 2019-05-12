#ifndef ICARUS_AST_INTERFACE_H
#define ICARUS_AST_INTERFACE_H

#include "ast/declaration.h"
#include "ast/expression.h"
#include "core/scope.h"

namespace ast {
struct Interface : public Expression {
  ~Interface() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::vector<Declaration> decls_;
  std::unique_ptr<core::DeclScope> body_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_INTERFACE_H
