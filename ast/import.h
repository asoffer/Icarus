#ifndef ICARUS_AST_IMPORT_H
#define ICARUS_AST_IMPORT_H

#include <memory>
#include <optional>

#include "ast/literal.h"
#include "misc/module.h"

namespace ast {
struct Import : public Literal {
  Import(std::unique_ptr<Expression> expr) : operand_(std::move(expr)) {}
  ~Import() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override {}

  // TODO what if the operand does a result/return thing in a scope? This feels
  // like it should be disallowed but maybe I need this to catch the error!
  void ExtractJumps(JumpExprs *rets) const override {
    operand_->ExtractJumps(rets);
  }

  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override {
    operand_->DependentDecls(g, d);
  }

  std::vector<ir::Val> EmitIR(Context *) override;

  // TODO optimization: if the operand_ is a string literal, schedule it
  // immediately.
  PendingModule module_;
  std::unique_ptr<Expression> operand_;
};
}  // namespace ast
#endif  // ICARUS_AST_IMPORT_H
