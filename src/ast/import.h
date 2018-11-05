#ifndef ICARUS_AST_IMPORT_H
#define ICARUS_AST_IMPORT_H

#include <memory>
#include <optional>

#include "ast/expression.h"

namespace AST {
struct Import : public Expression {
  Import(std::unique_ptr<Expression> expr) : operand_(std::move(expr)) {}
  ~Import() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override {}
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;

  // TODO what if the operand does a result/return thing in a scope? This feels
  // like it should be disallowed but maybe I need this to catch the error!
  void ExtractJumps(JumpExprs *) const override {}
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;
  Import *Clone() const override;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  // TODO optimization: if the operand_ is a string literal, schedule it
  // immediately.
  std::optional<std::string /* Source::Name */> cache_;
  std::unique_ptr<Expression> operand_;
};
}  // namespace AST
#endif  // ICARUS_AST_IMPORT_H
