#include "compiler/compiler.h"
#include "compiler/verify/verify.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::PatternMatch const *node) {
  absl::Span<type::QualType const> result;

  type::Type match_type;
  if (node->is_binary()) {
    auto expr_qts = VerifyType(&node->expr());
    result = context().set_qual_types(node, expr_qts);
    match_type = expr_qts[0].type();
  } else {
    result =
        context().set_qual_type(node, type::QualType::Constant(type::Type_));
    match_type = type::Type_;
  }

  auto &q         = state().verify_pattern_type_queues.emplace_back();
  absl::Cleanup c = [&] { state().verify_pattern_type_queues.pop_back(); };

  q.emplace(&node->pattern(), match_type);

  while (not q.empty()) {
    auto [n, t] = std::move(q.front());
    q.pop();

    if (not Compiler(*this).VerifyPatternType(n, t)) {
      // TODO: It may not be okay to emit an error because it may just determine
      // an overload set member is not valid.
      return context().set_qual_type(node, type::QualType::Error());
    }
  }

  for (auto const &[name, ids] : node->scope()->decls_) {
    for (auto const *id : ids) {
      auto const *d = id->declaration().if_as<ast::BindingDeclaration>();
      if (not d or &d->pattern() != node) { continue; }
      if (context().maybe_qual_type(d).empty()) {
        context().set_qual_type(d, type::QualType::Error());
      }
      if (context().maybe_qual_type(id).empty()) {
        context().set_qual_type(id, type::QualType::Error());
      }
    }
  }
  return result;
}

}  // namespace compiler
