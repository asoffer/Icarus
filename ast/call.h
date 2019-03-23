#ifndef ICARUS_AST_CALL_H
#define ICARUS_AST_CALL_H

#include "core/fn_args.h"
#include "ast/literal.h"

namespace ast {
struct Call : public Literal {
  Call() = default;
  explicit Call(std::unique_ptr<Expression> fn,
                core::FnArgs<std::unique_ptr<Expression>> args)
      : fn_(std::move(fn)), args_(std::move(args)) {}

  ~Call() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(core::Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override;

  ir::Results EmitIr(Context *) override;

  std::unique_ptr<Expression> fn_;
  core::FnArgs<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_CALL_H
