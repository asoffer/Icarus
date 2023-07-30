#include "ast/ast.h"
#include "core/type_system/type.h"
#include "absl/numeric/int128.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

core::Type TypeOfNode(ast::Terminal const& node, TypeSystem& type_system) {
  if (node.type() == nth::type<bool>) { return Bool; }
  if (node.type() == nth::type<data_types::Char>) { return Char; }
  if (node.type() == nth::type<core::Type>) { return Type; }
  if (node.type() == nth::type<absl::int128>) { return Integer; }
  if (node.type() == nth::type<double>) { return F64; }
  if (node.type() == nth::type<data_types::addr_t>) { return NullPtr; }
  if (node.type() == nth::type<std::string>) {
    return SliceType(type_system, Char);
  }
  NOT_YET(node.DebugString());
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::Terminal const* node) {
  co_return TypeOf(node, Constant(TypeOfNode(*node, type_system())));
}

}  // namespace semantic_analysis
