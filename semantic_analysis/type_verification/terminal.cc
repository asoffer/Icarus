#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/type.h"

namespace semantic_analysis {

core::Type TypeOf(ast::Terminal const& node, TypeSystem& type_system) {
  if (node.type() == base::meta<bool>) { return Bool; }
  if (node.type() == base::meta<ir::Char>) { return Char; }
  if (node.type() == base::meta<type::Type>) { return Type; }
  if (node.type() == base::meta<ir::Integer>) { return Integer; }
  if (node.type() == base::meta<float>) { return F32; }
  if (node.type() == base::meta<double>) { return F64; }
  if (node.type() == base::meta<ir::Slice>) {
    return SliceType(type_system, Char);
  }
  NOT_YET();
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier& tv,
                                          ast::Terminal const* node) {
  tv.complete_verification(node, Constant(TypeOf(*node, tv.type_system())));
  co_return;
}

}  // namespace semantic_analysis
