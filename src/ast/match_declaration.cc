#include "ast/match_declaration.h"

#include "backend/eval.h"
#include "type/primitive.h"

namespace ast {
std::string MatchDeclaration::to_string(size_t n) const {
  return type_expr->to_string(n) + "``" + id_;
}

type::Type const *MatchDeclaration::VerifyType(Context *ctx) {
  if (!type_expr->VerifyType(ctx)) { return nullptr; }
  // TODO this is wrong. it's a type satisfying a given interface. does that
  // matter?
  return ctx->set_type(this, type::Interface);
}

void MatchDeclaration::Validate(Context *ctx) { type_expr->Validate(ctx); }

base::vector<ir::Val> MatchDeclaration::EmitIR(Context *ctx) {
  // TODO build it
  return backend::Evaluate(type_expr.get(), ctx);
}
}  // namespace ast
