#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(ast::Module const *node) {
  for (auto const *stmt : node->stmts()) { co_await VerifyTypeOf(stmt); }
  co_return Completed(node);
}

}  // namespace semantic_analysis
