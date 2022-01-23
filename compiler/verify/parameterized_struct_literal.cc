#include "ast/ast.h"
#include "compiler/instantiate.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "type/generic.h"
#include "type/qual_type.h"
#include "type/struct.h"
#include "type/typed_value.h"

namespace compiler {

bool BodyVerifier::VerifyBody(ast::ParameterizedStructLiteral const *node) {
  LOG("ParameterizedStructLiteral", "%s on %s", node->DebugString(),
      context().DebugString());
  return true;
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ParameterizedStructLiteral const *node) {
  LOG("ParameterizedStructLiteral", "%s on %s", node->DebugString(),
      context().DebugString());
  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   auto params, VerifyParameters(*this, node->params()));
  Enqueue({.kind    = WorkItem::Kind::VerifyParameterizedStructLiteralBody,
           .node    = node,
           .context = &context()});
  return context().set_qual_type(node, type::QualType::Constant(type::EagerFunc(
                                           std::move(params), {type::Type_})));
}

}  // namespace compiler
