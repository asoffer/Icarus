#ifndef ICARUS_AST_ENUM_LITERAL_H
#define ICARUS_AST_ENUM_LITERAL_H

#include <string>
#include <vector>

#include "ast/declaration.h"
#include "ast/expression.h"
#include "core/scope.h"

namespace ast {
// TODO rename this because it's for enums and flags.
struct EnumLiteral : public Expression {
  enum Kind : char { Enum, Flags };

  EnumLiteral(std::vector<std::unique_ptr<Expression>> elems, TextSpan span,
              bool is_enum)
      : Expression(std::move(span)),
        elems_(std::move(elems)),
        kind_(is_enum ? Kind::Enum : Kind::Flags) {}

  ~EnumLiteral() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::unique_ptr<core::DeclScope> enum_scope_;
  std::vector<std::unique_ptr<Expression>> elems_;
  Kind kind_;
};

}  // namespace ast

#endif  // ICARUS_AST_ENUM_LITERAL_H
