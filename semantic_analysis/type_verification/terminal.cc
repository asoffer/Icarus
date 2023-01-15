#include "ast/ast.h"
#include "core/type_system/type.h"
#include "ir/value/integer.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

core::Type TypeOfNode(ast::Terminal const& node, TypeSystem& type_system) {
  if (node.type() == nth::type<bool>) { return Bool; }
  if (node.type() == nth::type<ir::Char>) { return Char; }
  if (node.type() == nth::type<core::Type>) { return Type; }
  if (node.type() == nth::type<ir::Integer>) { return Integer; }
  if (node.type() == nth::type<double>) { return F64; }
  if (node.type() == nth::type<std::string>) {
    return SliceType(type_system, Char);
  }
  NOT_YET(node.DebugString());
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier& tv,
                                          ast::Terminal const* node) {
  co_return tv.TypeOf(node, Constant(TypeOfNode(*node, tv.type_system())));
}

}  // namespace semantic_analysis
