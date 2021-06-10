#include "compiler/compiler.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::PatternMatch const *node) {
  absl::Span<type::QualType const> result;

  if (node->is_binary()) {
    auto expr_qts = VerifyType(&node->expr());
    VerifyPatternType(&node->pattern(), expr_qts[0].type());
    result = context().set_qual_types(node, expr_qts);
  } else {
    VerifyPatternType(&node->pattern(), type::Type_);
    result =
        context().set_qual_type(node, type::QualType::Constant(type::Type_));
  }

  for (auto const &[name, ids] : node->scope()->decls_) {
    for (auto const *id : ids) {
      auto const *d = id->declaration().if_as<ast::BindingDeclaration>();
      if (not d or &d->pattern() != node) { continue; }
      if (context().maybe_qual_type(d).empty()) {
        LOG("", "Blargh");
        context().set_qual_type(d, type::QualType::Error());
      }
      if (context().maybe_qual_type(id).empty()) {
        LOG("", "Blargh");
        context().set_qual_type(id, type::QualType::Error());
      }
    }
  }
  return result;
}

}  // namespace compiler
