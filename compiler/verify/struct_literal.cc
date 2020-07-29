#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

WorkItem::Result Compiler::VerifyBody(ast::StructLiteral const *node) {
  DEBUG_LOG("struct")("Struct-literal body verification: ", node, node->DebugString());
  bool error = false;
  for (auto const &field : node->fields()) {
    auto field_qt = VerifyType(&field);
    if (not field_qt.ok()) {
      error = true;
    } else if (field_qt.type()->completeness() ==
               type::Completeness::Incomplete) {
      DEBUG_LOG("struct")
      ("Setting error due to incomplete field. Diagnostics should have already "
       "been emit.");
      error = true;
    }
  }

  DEBUG_LOG("struct")("Struct-literal body verification complete: ", node);
  return error ? WorkItem::Result::Failure : WorkItem::Result::Success;
}

type::QualType Compiler::VerifyType(ast::StructLiteral const *node) {
  DEBUG_LOG("struct")("Verify type ", node, node->DebugString());
  state_.work_queue.Enqueue({
      .kind     = WorkItem::Kind::VerifyStructBody,
      .node     = node,
      .context  = data(),
      .consumer = diag(),
  });
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

}  // namespace compiler
