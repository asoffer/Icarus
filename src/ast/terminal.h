#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "ast/expression.h"
#include "ast/stages.h"
#include "ir/val.h"

namespace AST {
struct Terminal : public Expression {
  Terminal() = default;
  Terminal(const TextSpan &span, IR::Val val) : Expression(span) {
    stage_range_.low = DoneBodyValidationStage;
    type             = val.type;
    lvalue           = Assign::Const;
    value            = std::move(val);
  }

  ~Terminal() override {}

  std::string to_string(size_t) const override { return value.to_string(); }

  void assign_scope(Scope *scope) override {
    STAGE_CHECK(AssignScopeStage, AssignScopeStage);
    scope_ = scope;
    if (type != type::Type_) { return; }
  }

  void ClearIdDecls() { stage_range_ = StageRange{}; }

  void VerifyType(Context *) override {}
  void Validate(Context *) override {}
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override {}
  void ExtractReturns(std::vector<const Expression *> *) const override {}
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override {}

  Terminal *Clone() const { return new Terminal(*this); }
  IR::Val EmitIR(Context *) { return value; }

  IR::Val value = IR::Val::None();
};
}  // namespace AST

#endif  // ICARUS_AST_TERMINAL_H
