#include "ast/match_declaration.h"

#include "backend/eval.h"
#include "type/interface.h"
#include "type/primitive.h"

namespace ast {
std::string MatchDeclaration::to_string(size_t n) const {
  return type_expr->to_string(n) + "`" + id_;
}

VerifyResult MatchDeclaration::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), [[maybe_unused]] auto result,
                   type_expr->VerifyType(ctx));
  // TODO is this always constant? does that make sense?
  return ctx->set_result(this, VerifyResult::Constant(type::Type_));
}

bool MatchDeclaration::InferType(type::Type const *t,
                                 InferenceState *state) const {
  state->matches_.emplace(this, t);
  return true;
}

ir::Results MatchDeclaration::EmitIr(Context *ctx) {
  if (auto iter = ctx->bound_constants_.constants_.find(this);
      iter != ctx->bound_constants_.constants_.end()) {
    return ir::Results::FromVals({iter->second});
  } else {
    return ir::Results{
        backend::EvaluateAs<type::Interface const *>(type_expr.get(), ctx)};
  }
}
}  // namespace ast
