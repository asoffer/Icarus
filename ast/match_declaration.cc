#include "ast/match_declaration.h"

#include "backend/eval.h"
#include "type/interface.h"
#include "type/primitive.h"

namespace ast {
std::string MatchDeclaration::to_string(size_t n) const {
  return type_expr->to_string(n) + "`" + id_;
}

bool MatchDeclaration::InferType(type::Type const *t,
                                 InferenceState *state) const {
  state->matches_.emplace(this, t);
  return true;
}

ir::Results MatchDeclaration::EmitIr(Context *ctx) {
  auto results = ctx->constants_->first.get_constant(this);
  if (!results.empty()) { return results; }
  return ir::Results{
      backend::EvaluateAs<type::Interface const *>(type_expr.get(), ctx)};
}
}  // namespace ast
