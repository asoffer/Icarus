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
struct Token : public ast::Node {
  Token(const SourceRange &span = SourceRange(), std::string str = "",
        bool is_hashtag = false)
      : Node(span), token(std::move(str)) {
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

  ~Token() override {}

  void Accept(ast::VisitorBase *, void *, void *) const override {
    UNREACHABLE();
  }

  void DebugStrAppend(std::string *out, size_t indent) const override {
    out->append("[token: ");
    out->append(token);
    out->append("]");
  }

  std::string token;
  Operator op;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_TOKEN_H
