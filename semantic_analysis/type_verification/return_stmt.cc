#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(TypeVerifier& tv,
                                          ast::ReturnStmt const* node) {
  std::vector<core::Type> return_types;
  for (auto const* expr : node->exprs()) {
    absl::Span qts = co_await VerifyTypeOf(expr);
    for (QualifiedType qt : qts) {
      if (qt.qualifiers() >= Qualifiers::Error()) {
        NOT_YET();
      } else {
        return_types.push_back(qt.type());
      }
    }
  }
  tv.context().set_return_types(node, std::move(return_types));
  co_return tv.Completed(node);
}

}  // namespace semantic_analysis
