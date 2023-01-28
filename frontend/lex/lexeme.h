#ifndef ICARUS_FRONTEND_LEX_LEXEME_H
#define ICARUS_FRONTEND_LEX_LEXEME_H

#include <iostream>
#include <memory>

#include "ast/ast.h"
#include "ast/node.h"
#include "frontend/lex/operators.h"
#include "frontend/lex/syntax.h"
#include "frontend/lex/tag.h"
#include "ir/value/hashtag.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

namespace frontend {

// Represents the results that are produced by the lexer. This is either an
// operator, syntactic information, or an AST node.
struct Lexeme {
  explicit Lexeme(std::unique_ptr<ast::Node>&& n)
      : value_(std::move(n)),
        range_(std::get<std::unique_ptr<ast::Node>>(value_)->range()) {}
  explicit Lexeme(Operator op, std::string_view range)
      : value_(op), range_(range) {}
  explicit Lexeme(Syntax s, std::string_view range)
      : value_(s), range_(range) {}
  explicit Lexeme(ir::Hashtag h, std::string_view range)
      : value_(h), range_(range) {}

  constexpr Operator op() const { return std::get<Operator>(value_); }

  std::variant<std::unique_ptr<ast::Node>, Operator, Syntax, ir::Hashtag>
  get() && {
    return std::move(value_);
  }

  constexpr bool eof() const {
    auto const* syntax = std::get_if<Syntax>(&value_);
    return syntax and *syntax == Syntax::EndOfFile;
  }

  constexpr Tag tag() const {
    return std::visit(
        [](auto&& x) {
          using T             = std::decay_t<decltype(x)>;
          constexpr auto type = nth::type<T>;
          if constexpr (type == nth::type<Syntax>) {
            return TagFrom(x);
          } else if constexpr (type == nth::type<Operator>) {
            return TagFrom(x);
          } else if constexpr (type == nth::type<std::unique_ptr<ast::Node>>) {
            if (x->template is<ast::Label>()) {
              return label;
            } else {
              return expr;
            }
          } else if constexpr (type == nth::type<ir::Hashtag>) {
            return hashtag;
          } else {
            static_assert(type.dependent(false));
          }
        },
        value_);
  }

  std::string_view range() const { return range_; }

 private:
  std::variant<std::unique_ptr<ast::Node>, Operator, Syntax, ir::Hashtag>
      value_;
  std::string_view range_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_LEX_H
