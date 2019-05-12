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

  void assign_scope(ast_visitor::AssignScope *, core::Scope *) override {
    UNREACHABLE(token);
  }
  void ExtractJumps(ast_visitor::ExtractJumps *) const override {
    UNREACHABLE(token);
  }

  ast_visitor::VerifyResult VerifyType(ast_visitor::VerifyType const *,
                                       Context *) const override {
    UNREACHABLE(token);
  }
  ir::Results EmitIr(ast_visitor::EmitIr const *, Context *) const override {
    UNREACHABLE(token);
  }

  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(ast_visitor::EmitIr const *,
                                                 Context *) const override {
    UNREACHABLE(token);
  }

  void EmitMoveInit(ast_visitor::EmitIr const *, type::Typed<ir::Reg> reg,
                    Context *ctx) const override {
    UNREACHABLE(token);
  }

  void EmitCopyInit(ast_visitor::EmitIr const *, type::Typed<ir::Reg> reg,
                    Context *ctx) const override {
    UNREACHABLE(token);
  }

  void DependentDecls(ast_visitor::DependentDecls *,
                      ast::Declaration const *) const override {
    UNREACHABLE(token);
  }

  std::string token;
  Operator op;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_TOKEN_H
