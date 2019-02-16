#ifndef ICARUS_FRONTEND_TOKEN_H
#define ICARUS_FRONTEND_TOKEN_H

#include <string>
#include "ast/node.h"
#include "frontend/operators.h"

namespace frontend {
// ast node used only for holding tokens which have been lexed but not yet
// parsed.
struct Token : public ast::Node {
  Token(const TextSpan &span = TextSpan(), std::string str = "",
        bool is_hashtag = false)
      : Node(span), token(std::move(str)) {
    if (is_hashtag) {
      op = Language::Operator::Hashtag;
    } else {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  if (token == symbol) {                                                       \
    op = Language::Operator::name;                                             \
    return;                                                                    \
  }
#include "operators.xmacro.h"
#undef OPERATOR_MACRO
      op = Language::Operator::NotAnOperator;
    }
  }

  ~Token() override {}

  std::string to_string(size_t) const override { return "[" + token + "]\n"; }

  void assign_scope(Scope *) override { UNREACHABLE(token); }
  ast::VerifyResult VerifyType(Context *) override { UNREACHABLE(token); }
  void ExtractJumps(ast::JumpExprs *) const override { UNREACHABLE(token); }
  std::vector<ir::Val> EmitIR(Context *) override { UNREACHABLE(token); }
  void DependentDecls(base::Graph<ast::Declaration *> *g,
                      ast::Declaration *d) const {
    UNREACHABLE(token);
  }

  std::string token;
  Language::Operator op;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_TOKEN_H
