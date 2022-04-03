#include "ast/ast.h"
#include "compiler/verify/verify.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

bool BodyVerifier::VerifyBody(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Struct-literal body verification: %p %s", node,
      node->DebugString());

  bool error = false;
  for (auto const &field : node->fields()) {
    if (field.flags() & ast::Declaration::f_IsConst) {
      Enqueue({.kind    = WorkItem::Kind::VerifyType,
               .node    = &field,
               .context = &context()},
              {WorkItem{
                  .kind    = WorkItem::Kind::CompleteStructData,
                  .node    = node,
                  .context = &context(),
              }});
    } else {
      auto field_qt = VerifyType(*this, &field)[0];
      if (not field_qt.ok()) {
        error = true;
      } else if (field_qt.type().get()->completeness() ==
                 type::Completeness::Incomplete) {
        // TODO: This is a bad error message. It'd be nice to correctly diagnose
        // the cylcic dependency.
        LOG("StructLiteral",
            "Setting error due to incomplete field. Diagnostics should have "
            "already been emit.");
        error = true;
      }
    }
  }

  LOG("StructLiteral", "Struct-literal body verification complete: %p", node);
  return not error;
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::StructLiteral const *node) {
  LOG("StructLiteral", "Verify type %p %s", node, node->DebugString());
  Enqueue({.kind    = WorkItem::Kind::VerifyStructBody,
           .node    = node,
           .context = &context()});
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

}  // namespace compiler
