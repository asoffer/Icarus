#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Access const *node) {
  std::span operand_qts = co_await VerifyTypeOf(node->operand());
  if (operand_qts.size() != 1) { NOT_YET("log an error"); }
  QualifiedType qt = operand_qts[0];
  if (qt.type() == Module) {
    NOT_YET();
  } else if (qt.type().is<SliceType>(tv.type_system())) {
    if (node->member_name() == "data") {
      co_return tv.TypeOf(
          node, QualifiedType(BufferPointerType(tv.type_system(), Char),
                              qt.qualifiers()));
    } else if (node->member_name() == "length") {
      co_return tv.TypeOf(node, QualifiedType(U(64), qt.qualifiers()));
    } else {
      NOT_YET("Log an error: ", node->member_name());
    }
  } else {
    NOT_YET(DebugQualifiedType(qt, tv.type_system()));
  }
}

}  // namespace semantic_analysis
