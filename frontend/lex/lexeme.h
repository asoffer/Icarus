#ifndef ICARUS_FRONTEND_LEX_LEXEME_H
#define ICARUS_FRONTEND_LEX_LEXEME_H

#include <iostream>

#include "ast/ast.h"
#include "ast/node.h"
#include "base/meta.h"
#include "frontend/lex/operators.h"
#include "frontend/lex/syntax.h"
#include "frontend/lex/tag.h"

namespace frontend {

// Represents the results that are produced by the lexer. This is either an
// operator, syntactic information, or an AST node.
struct Lexeme {
  explicit Lexeme(std::unique_ptr<ast::Node>&& n)
      : value_(std::move(n)),
        range_(std::get<std::unique_ptr<ast::Node>>(value_)->range()) {}
  explicit Lexeme(Operator op, SourceRange const& range)
      : value_(op), range_(range) {}
  explicit Lexeme(Syntax s, SourceRange const& range)
      : value_(s), range_(range) {}
  explicit Lexeme(ast::Hashtag h, SourceRange const& range)
      : value_(h), range_(range) {}

  constexpr Operator op() const { return std::get<Operator>(value_); }

  std::variant<std::unique_ptr<ast::Node>, Operator, Syntax, ast::Hashtag>
  get() && {
    return std::move(value_);
  }

  constexpr Tag tag() const {
    return std::visit(
        [](auto&& x) {
          using T = std::decay_t<decltype(x)>;
          if constexpr (std::is_same_v<T, Syntax>) {
            return TagFrom(x);
          } else if constexpr (std::is_same_v<T, Operator>) {
            return TagFrom(x);
          } else if constexpr (std::is_same_v<T, std::unique_ptr<ast::Node>>) {
            if (x->template is<ast::Label>()) {
              return label;
            } else {
              return expr;
            }
          } else if constexpr (std::is_same_v<T, ast::Hashtag>) {
            return hashtag;
          } else {
            static_assert(base::always_false<T>());
          }
        },
        value_);
  }

  SourceRange range() const { return range_; }

 private:
  std::variant<std::unique_ptr<ast::Node>, Operator, Syntax, ast::Hashtag>
      value_;
  SourceRange range_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H
