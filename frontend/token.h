#ifndef ICARUS_FRONTEND_TOKEN_H
#define ICARUS_FRONTEND_TOKEN_H

#include <string>
#include "ast/node.h"
#include "frontend/operators.h"

struct JumpExprs;
namespace frontend {
// ast node used only for holding tokens which have been lexed but not yet
// parsed.
struct Token : public ast::Node {
  Token(const TextSpan &span = TextSpan(), std::string str = "",
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

  std::string to_string(size_t) const override { return "[" + token + "]\n"; }

  void assign_scope(core::Scope *) override { UNREACHABLE(token); }
  ast::VerifyResult VerifyType(Context *) override { UNREACHABLE(token); }
  void ExtractJumps(JumpExprs *) const override { UNREACHABLE(token); }
  ir::Results EmitIr(Context *) override { UNREACHABLE(token); }
  void DependentDecls(ast::DeclDepGraph *, ast::Declaration *) const {
    UNREACHABLE(token);
  }

  std::string token;
  Operator op;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_TOKEN_H
