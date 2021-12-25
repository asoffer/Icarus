#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::ScopeLiteral const *node) {
  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),  //
                   auto params, VerifyParams(node->params()));

  VerifyType(&node->context());
  return context().set_qual_type(node,
                                 type::QualType::Constant(type::UnboundScope));
}

}  // namespace compiler
