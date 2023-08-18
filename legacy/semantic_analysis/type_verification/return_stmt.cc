#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(ast::ReturnStmt const* node) {
  std::vector<QualifiedType> return_types;
  for (auto const* expr : node->exprs()) {
    std::span qts = co_await VerifyTypeOf(expr);
    for (QualifiedType qt : qts) {
      if (qt.qualifiers() >= Qualifiers::Error()) {
        NTH_UNIMPLEMENTED();
      } else {
        return_types.push_back(qt);
      }
    }
  }
  context().set_return_types(node, std::move(return_types));
  co_return TypeOf(node, QualifiedType(NoReturn));
}

}  // namespace semantic_analysis
