#ifndef ICARUS_AST_INTERFACE_H
#define ICARUS_AST_INTERFACE_H

#include "ast/declaration.h"
#include "ast/expression.h"
#include "core/scope.h"

namespace ast {
struct Interface : public Expression {
  ~Interface() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    if (decls_.empty()) { return "interface {}"; }
    std::stringstream ss;
    ss << "interface {\n";
    for (const auto &decl : decls_) {
      ss << std::string(n * 2, ' ') << decl.to_string(n) << "\n";
    }
    ss << "}";
    return ss.str();
  }

  std::vector<Declaration> decls_;
  std::unique_ptr<core::DeclScope> body_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_INTERFACE_H
