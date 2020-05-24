#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

bool Compiler::VerifyBody(ast::StructLiteral const *node) {
  bool error = false;
  for (auto const &field : node->fields()) {
    auto field_qt = VerifyType(&field);
    if (not field_qt.ok()) { error = true; }
  }

  return not error;
}

type::QualType Compiler::VerifyType(ast::StructLiteral const *node) {
  state_.work_queue.emplace(node, TransientFunctionState::WorkType::VerifyBody);
  return data().set_qual_type(node, type::QualType::Constant(type::Type_));
}

}  // namespace compiler
