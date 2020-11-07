#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

WorkItem::Result Compiler::VerifyBody(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Struct-literal body verification: %p %s", node,
      node->DebugString());
  bool error = false;
  for (auto const &field : node->fields()) {
    auto field_qt = VerifyType(&field);
    if (not field_qt.ok()) {
      error = true;
    } else if (field_qt.type().get()->completeness() ==
               type::Completeness::Incomplete) {
      LOG("StructLiteral",
          "Setting error due to incomplete field. Diagnostics should have "
          "already been emit.");
      error = true;
    }
  }

  LOG("struct", "Struct-literal body verification complete: %p", node);
  return error ? WorkItem::Result::Failure : WorkItem::Result::Success;
}

type::QualType Compiler::VerifyType(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Verify type %p %s", node, node->DebugString());
  state_.work_queue.Enqueue({
      .kind     = WorkItem::Kind::VerifyStructBody,
      .node     = node,
      .context  = context(),
      .consumer = diag(),
  });
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

}  // namespace compiler
