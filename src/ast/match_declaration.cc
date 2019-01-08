#include "ast/match_declaration.h"

#include "backend/eval.h"
#include "type/primitive.h"

namespace ast {
std::string MatchDeclaration::to_string(size_t n) const {
  return type_expr->to_string(n) + "`" + id_;
}

VerifyResult MatchDeclaration::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), [[maybe_unused]] auto result,
                   type_expr->VerifyType(ctx));
  // TODO this is wrong. it's a type satisfying a given interface. does that
  // matter?
  // TODO is this always constant? does that make sense?
  return VerifyResult::Constant(ctx->set_type(this, type::Interface));
}

void MatchDeclaration::Validate(Context *ctx) { type_expr->Validate(ctx); }

base::vector<ir::Val> MatchDeclaration::EmitIR(Context *ctx) {
  // TODO build it
  return backend::Evaluate(type_expr.get(), ctx);
}
}  // namespace ast
