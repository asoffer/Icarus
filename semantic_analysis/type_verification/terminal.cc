#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/type.h"

namespace semantic_analysis {

core::Type TypeOf(ast::Terminal const& node) {
  if (node.type() == base::meta<bool>) { return Bool; }
  if (node.type() == base::meta<ir::Char>) { return Char; }
  if (node.type() == base::meta<type::Type>) { return Type; }
  if (node.type() == base::meta<ir::Integer>) { return Integer; }
  if (node.type() == base::meta<double>) { return F64; }
  NOT_YET();
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier& tv,
                                          ast::Terminal const* node) {
  tv.complete_verification(node, Constant(TypeOf(*node)));
  co_return;
}

}  // namespace semantic_analysis
