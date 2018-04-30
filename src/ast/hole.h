#ifndef ICARUS_AST_HOLE_H
#define ICARUS_AST_HOLE_H

#include "ast/ast.h"

namespace AST {
// TODO currently needs to be an identifier because Declaration::identifier has
// that type. This will change when declarations support commalists too at which
// point this can be replaced with inheriting from Expression.
struct Hole : public Identifier {
  Hole() = delete;
  Hole(const TextSpan &span) : Identifier(span, "--") {
    stage_range_.low = DoneBodyValidationStage;
    type             = nullptr;
    lvalue           = Assign::Const;
  }
  ~Hole() override {}

  void assign_scope(Scope *scope) { scope_ = scope; }
  Hole *Clone() const { return new Hole(*this); }
  void SaveReferences(Scope *, std::vector<IR::Val> *) {}
  void contextualize(const Node *,
                     const std::unordered_map<const Expression *, IR::Val> &) {}
  void ClearIdDecls() { stage_range_ = StageRange{}; }
  void ExtractReturns(std::vector<const Expression *> *) const {}
  void VerifyType(Context *) {}
  void Validate(Context *ctx) {}

  IR::Val EmitIR(Context *) override { return IR::Val::None(); }
  IR::Val EmitLVal(Context *) override { return IR::Val::None(); }
};
}  // namespace AST

#endif  // ICARUS_AST_HOLE_H
