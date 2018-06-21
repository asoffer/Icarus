#include "ast/match_declaration.h"

#include "type/primitive.h"
#include "ast/verify_macros.h"

namespace AST {
std::string MatchDeclaration::to_string(size_t n) const {
  return type_expr->to_string(n) + "``" + identifier->to_string(n);
}

void MatchDeclaration::VerifyType(Context *ctx) {
  {
    VERIFY_STARTING_CHECK_EXPR;
    type_expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(type_expr);
    type = type::Type_;
  }
  identifier->VerifyType(ctx);
}

void MatchDeclaration::Validate(Context *ctx) { NOT_YET(); }

MatchDeclaration *MatchDeclaration::Clone() const {
  auto *result = new MatchDeclaration;
  CloneTo(result);
  return result;
}

IR::Val MatchDeclaration::EmitIR(Context *ctx) { UNREACHABLE(); }
}  // namespace AST
