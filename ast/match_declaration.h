#ifndef ICARUS_AST_MATCH_DECLARATION_H
#define ICARUS_AST_MATCH_DECLARATION_H

#include "ast/declaration.h"

namespace ast {
struct MatchDeclaration : public Declaration {
  MatchDeclaration() {}
  MatchDeclaration(MatchDeclaration &&) noexcept = default;
  MatchDeclaration &operator=(MatchDeclaration &&) noexcept = default;
  ~MatchDeclaration() override {}

  std::string to_string(size_t n) const override {
    return type_expr->to_string(n) + "`" + id_;
  }
};
}  // namespace ast

#endif  // ICARUS_AST_MATCH_DECLARATION_H
