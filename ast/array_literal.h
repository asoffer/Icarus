#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/literal.h"
#include "ast/comma_list.h"

namespace ast {
struct ArrayLiteral : public Literal {
  ArrayLiteral(TextSpan const& span) : Literal(span) {}
  ~ArrayLiteral() override {}

  void assign_scope(Scope *scope) override { return cl_.assign_scope(scope); }
  std::string to_string(size_t n) const override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context * ctx) override { return cl_.Validate(ctx); }
  void ExtractJumps(JumpExprs *rets) const override {
    return cl_.ExtractJumps(rets);
  }

  std::vector<ir::Val> EmitIR(Context *) override;
  void EmitMoveInit(type::Typed<ir::Register> reg, Context *ctx) override;
  void EmitCopyInit(type::Typed<ir::Register> reg, Context *ctx) override;

  CommaList cl_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_LITERAL_H
