#ifndef ICARUS_FRONTEND_LEX_TOKEN_H
#define ICARUS_FRONTEND_LEX_TOKEN_H

#include <string>

#include "ast/node.h"
#include "base/debug.h"
#include "frontend/lex/operators.h"

struct JumpExprs;
namespace frontend {
// ast node used only for holding tokens which have been lexed but not yet
// parsed.
struct Token : ast::Node {
  Token(std::string_view range = "", bool is_hashtag = false)
      : Node(-1, range), token(range) {
    if (is_hashtag) {
      op = Operator::Hashtag;
    } else {
#define OPERATOR_MACRO(name, symbol, tag, prec, assoc)                         \
  if (token == symbol) {                                                       \
    op = Operator::name;                                                       \
    return;                                                                    \
  }
#include "operators.xmacro.h"
#undef OPERATOR_MACRO
      op = Operator::NotAnOperator;
    }
  }

  void DebugStrAppend(std::string *out, size_t indent) const override {
    out->append("[token: ");
    out->append(range());
    out->append("]");
  }

  Operator op;
  std::string_view token;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_TOKEN_H
