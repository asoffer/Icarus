#include "ast/ast.h"
#include "compiler/context.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/qual_type.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(TypeVerifier& tv,
                                          ast::Terminal const* node) {
  tv.complete_verification(
      node, type::QualType::Constant(compiler::TerminalType(*node)));
  co_return;
}

}  // namespace semantic_analysis
