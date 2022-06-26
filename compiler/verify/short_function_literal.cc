#include "ast/ast.h"
#include "compiler/resources.h"
#include "compiler/verify/common.h"

namespace compiler {

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ShortFunctionLiteral const *node) {
  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   auto params, VerifyParameters(*this, node->parameters()));
  auto body_qts = VerifyType(node->body());

  std::vector<type::Type> body_types;
  body_types.reserve(body_qts.size());
  for (auto const &qt : body_qts) {
    if (not qt.ok()) {
      return context().set_qual_type(node, type::QualType::Error());
    }
    body_types.push_back(qt.type());
  }

  return context().set_qual_type(
      node, type::QualType::Constant(
                type::Func(std::move(params), std::move(body_types))));
}

}  // namespace compiler
