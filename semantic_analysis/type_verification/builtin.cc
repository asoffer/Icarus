#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(TypeVerifier& tv,
                                          ast::Builtin const* node) {
  co_return tv.TypeOf(node, Constant(Module));
}

}  // namespace semantic_analysis
