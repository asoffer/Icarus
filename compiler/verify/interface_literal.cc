#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/primitive.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::InterfaceLiteral const *node) {
  return context().set_qual_type(node,
                                 type::QualType::Constant(type::Interface));
}

}  // namespace compiler
