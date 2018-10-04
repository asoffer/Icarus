#include "ast/match_declaration.h"

#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "type/primitive.h"

namespace AST {
std::string MatchDeclaration::to_string(size_t n) const {
  return type_expr->to_string(n) + "``" + identifier->to_string(n);
}

type::Type const *MatchDeclaration::VerifyType(Context *ctx) {
  {
    VERIFY_STARTING_CHECK_EXPR;
    type_expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(type_expr);
    // TODO this is wrong. it's a type satisfying a given interface. does that
    // matter?
    ctx->mod_->types_.emplace(this, type::Interface);
  }
  identifier->VerifyType(ctx);
  return type::Interface;
}

void MatchDeclaration::Validate(Context *ctx) { type_expr->Validate(ctx); }

MatchDeclaration *MatchDeclaration::Clone() const {
  auto *result = new MatchDeclaration;
  CloneTo(result);
  return result;
}

base::vector<IR::Val> MatchDeclaration::EmitIR(Context *ctx) {
  // TODO build it
  return backend::Evaluate(type_expr.get(), ctx);
}
}  // namespace AST
