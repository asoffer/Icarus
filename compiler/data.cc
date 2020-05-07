#include "compiler/data.h"

namespace compiler {

DependentComputedData::DependentComputedData(module::BasicModule *mod)
    : mod_(mod), bldr_(ir::GetBuilder()) {}

DependentComputedData::~DependentComputedData() {
  // TODO figure out what's being dropped?
  // ASSERT(deferred_work_.lock()->empty() == true);
}

struct DependentComputedData::DependentDataChild::DataImpl {
  core::Params<type::Type const *> params;
  std::vector<type::Type const *> rets;
  DependentComputedData data;
};

DependentComputedData::InsertDependentResult
DependentComputedData::InsertDependent(
    ast::ParameterizedExpression const *node,
    core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args) {
  auto &[parent, map] = dependent_data_[node];
  parent              = this;
  auto [iter, inserted] = map.try_emplace(args);

  if (inserted) {
    iter->second = std::unique_ptr<DependentDataChild::DataImpl>(
        new DependentDataChild::DataImpl{
            .params = core::Params<type::Type const *>(node->params().size()),
            .rets   = {},
            .data   = DependentComputedData(mod_),
        });
    iter->second->data.parent_ = this;
  }
  auto &[params, rets, data] = *iter->second;
  return InsertDependentResult{
      .params   = params,
      .rets     = rets,
      .data     = data,
      .inserted = inserted,
  };
}

DependentComputedData::FindDependentResult DependentComputedData::FindDependent(
    ast::ParameterizedExpression const *node,
    core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args) {
  auto &map = dependent_data_.find(node)->second.map;
  auto iter = map.find(args);
  ASSERT(iter != map.end());
  auto &[params, rets, data] = *iter->second;
  return FindDependentResult{
      .fn_type = type::Func(params, rets),
      .data    = data,
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
