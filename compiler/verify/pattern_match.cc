#include "compiler/compiler.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::PatternMatch const *node) {
  if (node->is_binary()) {
    auto expr_qts = VerifyType(&node->expr());
    VerifyPatternType(&node->pattern(), expr_qts[0].type());
    return context().set_qual_types(node, expr_qts);
  } else {
    VerifyPatternType(&node->pattern(), type::Type_);
    return context().set_qual_type(node, type::QualType::Constant(type::Type_));
  }
}

}  // namespace compiler
