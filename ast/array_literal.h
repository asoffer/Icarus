#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/comma_list.h"
#include "ast/literal.h"

namespace ast {
struct ArrayLiteral : public Literal {
  ArrayLiteral(TextSpan const &span) : Literal(span) {}
  ~ArrayLiteral() override {}

  void assign_scope(core::Scope *scope) override { return cl_.assign_scope(scope); }
  std::string to_string(size_t n) const override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *rets) const override {
    return cl_.ExtractJumps(rets);
  }
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return false;
  }

  ir::Results EmitIr(Context *ctx) override;
  void EmitMoveInit(type::Typed<ir::Reg> reg, Context *ctx) override;
  void EmitCopyInit(type::Typed<ir::Reg> reg, Context *ctx) override;

  CommaList cl_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_LITERAL_H
