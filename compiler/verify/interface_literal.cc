#include "ast/ast.h"
#include "compiler/verify/verify.h"
#include "type/primitive.h"

namespace compiler {

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::InterfaceLiteral const *node) {
  return context().set_qual_type(node,
                                 type::QualType::Constant(type::Interface));
}

}  // namespace compiler
