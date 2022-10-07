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

absl::Span<
    absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier> const>
Context::set_parameters(
    ast::Expression const *expr,
    std::vector<absl::flat_hash_map<core::ParameterType, CallableIdentifier>>
        parameters) {
  [[maybe_unused]] auto [iter, inserted] =
      parameters_.try_emplace(expr, std::move(parameters));
  ASSERT(inserted == true);
  return iter->second;
}

void Context::set_callee_overload(ast::Call const *call_expr, ir::Fn f) {
  [[maybe_unused]] auto [iter, inserted] = callees_.try_emplace(call_expr, f);
  ASSERT(inserted == true);
}

ir::Fn Context::callee_overload(ast::Call const *call_expr) const {
  auto iter = callees_.find(call_expr);
  ASSERT(iter != callees_.end());
  return iter->second;
}

}  // namespace semantic_analysis
