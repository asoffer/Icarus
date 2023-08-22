#include "semantic_analysis/context.h"

#include "nth/debug/debug.h"

namespace semantic_analysis {

std::span<QualifiedType const> Context::qualified_types(
    ast::Expression const *expr) const {
  auto iter = type_.find(expr);
  NTH_ASSERT(iter != type_.end());
  return iter->second;
}

QualifiedType Context::qualified_type(ast::Expression const *expr) const {
  std::span result = qualified_types(expr);
  NTH_ASSERT(result.size() == 1);
  return result[0];
}

std::span<QualifiedType const> Context::set_qualified_types(
    ast::Expression const *expr, std::vector<QualifiedType> qualified_types) {
  [[maybe_unused]] auto [iter, inserted] =
      type_.try_emplace(expr, std::move(qualified_types));
  NTH_ASSERT(inserted);
  return iter->second;
}

std::span<QualifiedType const> Context::set_qualified_type(
    ast::Expression const *expr, QualifiedType qualified_type) {
  [[maybe_unused]] auto [iter, inserted] =
      type_.try_emplace(expr, 1, qualified_type);
  return iter->second;
}

std::span<
    absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier> const>
Context::set_parameters(
    ast::Expression const *expr,
    std::vector<absl::flat_hash_map<core::ParameterType, CallableIdentifier>>
        parameters) {
  [[maybe_unused]] auto [iter, inserted] =
      parameters_.try_emplace(expr, std::move(parameters));
  NTH_ASSERT(inserted);
  return iter->second;
}

void Context::set_symbol(ast::Identifier const *id, symbol_ref_type symbol) {
  [[maybe_unused]] auto [iter, inserted] = symbols_.try_emplace(id, symbol);
  NTH_ASSERT(inserted);
}

Context::symbol_ref_type Context::symbol(ast::Identifier const *id) const {
  auto iter = symbols_.find(id);
  NTH_ASSERT(iter != symbols_.end());
  return iter->second;
}

void Context::set_return_types(ast::ReturnStmt const *return_stmt,
                               std::vector<QualifiedType> return_types) {
  [[maybe_unused]] auto [iter, inserted] =
      returns_.try_emplace(return_stmt, std::move(return_types));
  NTH_ASSERT(inserted);
}

std::span<QualifiedType const> Context::return_types(
    ast::ReturnStmt const *return_stmt) const {
  auto iter = returns_.find(return_stmt);
  NTH_ASSERT(iter != returns_.end());
  return iter->second;
}

void Context::set_callee(ast::Call const *node,
                         Context::CallableIdentifier const *identifier) {
  [[maybe_unused]] auto [iter, inserted] =
      callees_.try_emplace(node, identifier);
  NTH_ASSERT(inserted);
}

Context::CallableIdentifier const &Context::callee(ast::Call const *node) {
  auto iter = callees_.find(node);
  NTH_ASSERT(iter != callees_.end());
  return *iter->second;
}

}  // namespace semantic_analysis