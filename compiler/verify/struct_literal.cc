#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

type::QualType Compiler::VerifyType(ast::StructLiteral const *node) {
  bool error = false;
  for (auto const &field : node->fields()) {
    auto field_qt = VerifyType(&field);
    if (not field_qt.ok()) { error = true; }
  }

  auto qt = type::QualType::Constant(type::Type_);
  if (error) { qt.MarkError(); }
  return data().set_qual_type(node, qt);
}

}  // namespace compiler
