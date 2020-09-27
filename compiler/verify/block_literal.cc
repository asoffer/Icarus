#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/qual_type.h"

namespace compiler {

WorkItem::Result Compiler::VerifyBody(ast::BlockLiteral const *node) {
  bool success = true;
  // TODO consider not verifying the types of the bodies. They almost certainly
  // contain circular references in the jump statements, and if the functions
  // require verifying the body upfront, things can maybe go wrong?
  for (auto *b : node->before()) { success &= VerifyType(b).ok(); }
  for (auto *a : node->after()) { success &= VerifyType(a).ok(); }
  return WorkItem::Result::Success;
}

type::QualType Compiler::VerifyType(ast::BlockLiteral const *node) {
  LOG("compile-work-queue", "Request work block: %p", node);
  state_.work_queue.Enqueue({
      .kind     = WorkItem::Kind::VerifyBlockBody,
      .node     = node,
      .context  = data(),
      .consumer = diag(),
  });
  return data().set_qual_type(node, type::QualType::Constant(type::Block));
}

}  // namespace compiler
