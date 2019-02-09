#ifndef ICARUS_AST_HOLE_H
#define ICARUS_AST_HOLE_H

#include "ast/literal.h"
#include "ir/val.h"

namespace ast {
// TODO currently needs to be an identifier because Declaration::identifier has
// that type. This will change when declarations support commalists too at which
// point this can be replaced with inheriting from Expression.
struct Hole : public Literal {
  Hole() = delete;
  Hole(const TextSpan &span) : Literal(span) {}
  ~Hole() override {}

  void assign_scope(Scope *scope) { scope_ = scope; }
  std::string to_string(size_t n) const override { return "--"; }
  void ExtractJumps(JumpExprs *) const {}
  VerifyResult VerifyType(Context *) { return VerifyResult::Error(); }
  void Validate(Context *ctx) {}

  std::vector<ir::Val> EmitIR(Context *) override { return {ir::Val::None()}; }
};
}  // namespace ast

#endif  // ICARUS_AST_HOLE_H
