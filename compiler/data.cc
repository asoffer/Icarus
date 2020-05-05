#include "compiler/data.h"

namespace compiler {

DependentComputedData::DependentComputedData(module::BasicModule *mod)
    : mod_(mod), bldr_(ir::GetBuilder()) {}

DependentComputedData::~DependentComputedData() {
  // TODO figure out what's being dropped?
  // ASSERT(deferred_work_.lock()->empty() == true);
}

DependentComputedData::InsertDependentResult
DependentComputedData::InsertDependent(
    ast::ParameterizedExpression const *node,
    core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args) {
  auto &[parent, map] = dependent_data_[node];
  parent              = this;
  auto [iter, inserted] =
      map.try_emplace(args, std::piecewise_construct,
                      std::forward_as_tuple(node->params().size()),
                      std::forward_as_tuple(mod_));
  if (inserted) { iter->second.second.parent_ = this; }
  return InsertDependentResult{
      .params   = iter->second.first,
      .data     = iter->second.second,
      .inserted = inserted,
  };
}

DependentComputedData::FindDependentResult DependentComputedData::FindDependent(
    ast::ParameterizedExpression const *node,
    core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args) {
  auto &map = dependent_data_.find(node)->second.map;
  auto iter = map.find(args);
  ASSERT(iter != map.end());
  return FindDependentResult{
      .params = iter->second.first,
      .data   = iter->second.second,
  };
}

ir::Jump *DependentComputedData::jump(ast::Jump const *expr) {
  auto iter = jumps_.find(expr);
  return iter == jumps_.end() ? nullptr : &iter->second;
}

type::QualType const *DependentComputedData::result(
    ast::Expression const *expr) const {
  auto iter = type_verification_results_.find(expr);
  if (iter != type_verification_results_.end()) { return &iter->second; }
  if (parent_) { return parent_->result(expr); }
  return nullptr;
}

type::QualType DependentComputedData::set_result(ast::Expression const *expr,
                                                 type::QualType r) {
  type_verification_results_.emplace(expr, r);
  return r;
}

}  // namespace compiler
