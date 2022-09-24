#include "semantic_analysis/context.h"

#include "base/debug.h"

namespace semantic_analysis {

absl::Span<QualifiedType const> Context::qualified_types(
    ast::Expression const *expr) const {
  auto iter = type_.find(expr);
  ASSERT(iter != type_.end());
  return iter->second;
}

QualifiedType Context::qualified_type(ast::Expression const *expr) const {
  absl::Span result = qualified_types(expr);
  ASSERT(result.size() == 1);
  return result[0];
}

absl::Span<QualifiedType const> Context::set_qualified_types(
    ast::Expression const *expr, std::vector<QualifiedType> qualified_types) {
  [[maybe_unused]] auto [iter, inserted] =
      type_.try_emplace(expr, std::move(qualified_types));
  ASSERT(inserted == true);
  return iter->second;
}

absl::Span<QualifiedType const> Context::set_qualified_type(
    ast::Expression const *expr, QualifiedType qualified_type) {
  [[maybe_unused]] auto [iter, inserted] =
      type_.try_emplace(expr, 1, qualified_type);
  ASSERT(inserted == true);
  return iter->second;
}

}  // namespace semantic_analysis
