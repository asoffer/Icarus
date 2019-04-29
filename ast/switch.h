#ifndef ICARUS_AST_SWITCH_H
#define ICARUS_AST_SWITCH_H

#include "ast/literal.h"

namespace ast {
// TODO consider separating this into two classes given that we know when we
// parse if it has parens or not.
struct Switch : public Literal {
  ~Switch() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(core::Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *rets) const override;
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return false;
  }

  ir::Results EmitIr(Context *) override;

  std::unique_ptr<Expression> expr_;
  std::vector<std::pair<std::unique_ptr<Node>, std::unique_ptr<Expression>>>
      cases_;
};

// Temporary node which never appears in the AST but is useful during parsing to
// distinguish 'when' from other binary operators.
struct SwitchWhen : public Node {
  ~SwitchWhen() override {}
  std::string to_string(size_t n) const override {
    return body->to_string(n) + " when " + cond->to_string(n);
  }
  void assign_scope(core::Scope *scope) override { UNREACHABLE(); }
  VerifyResult VerifyType(Context *) override { UNREACHABLE(); }
  void ExtractJumps(JumpExprs *rets) const override { UNREACHABLE(); }
  void DependentDecls(DeclDepGraph *g, Declaration *d) const override {
    UNREACHABLE();
  }
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return false;
  }

  ir::Results EmitIr(Context *) override { UNREACHABLE(); }

  std::unique_ptr<Node> body;
  std::unique_ptr<Expression> cond;
};

}  // namespace ast

#endif  // ICARUS_AST_SWITCH_H
