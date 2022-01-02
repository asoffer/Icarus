#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/verify/common.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::ScopeLiteral const *node) {
  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   auto params, VerifyParameters(*this, node->params()));

  VerifyType(&node->context());
  return context().set_qual_type(
      node, type::QualType::Constant(type::Scp(std::move(params))));
}

}  // namespace compiler
