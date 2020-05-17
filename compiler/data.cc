#include "compiler/data.h"

namespace compiler {

DependentComputedData::DependentComputedData(CompiledModule *mod) : mod_(mod) {}

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
    core::Params<type::Type const *> const &params) {
  auto &[parent, map]   = dependent_data_[node];
  parent                = this;
  auto [iter, inserted] = map.try_emplace(params);

  if (inserted) {
    iter->second = std::unique_ptr<DependentDataChild::DataImpl>(
        new DependentDataChild::DataImpl{
            .params = core::Params<type::Type const *>(node->params().size()),
            .rets   = {},
            .data   = DependentComputedData(mod_),
        });
    iter->second->data.parent_ = this;
    for (size_t i = 0; i < node->params().size(); ++i) {
      auto const *decl = node->params()[i].value.get();
      iter->second->data.set_qual_type(
          decl, decl->flags() & ast::Declaration::f_IsConst
                    ? type::QualType::Constant(params[i].value)
                    : type::QualType::NonConstant(params[i].value));
    }
  }
  auto &[parameters, rets, data] = *iter->second;
  return InsertDependentResult{
      .params   = parameters,
      .rets     = rets,
      .data     = data,
      .inserted = inserted,
  };
}

DependentComputedData::FindDependentResult DependentComputedData::FindDependent(
    ast::ParameterizedExpression const *node,
    core::Params<type::Type const *> const &params) {
  auto &map = dependent_data_.find(node)->second.map;
  auto iter = map.find(params);
  ASSERT(iter != map.end());
  auto &[parameters, rets, data] = *iter->second;
  return FindDependentResult{
      .fn_type = type::Func(parameters, rets),
      .data    = data,
  };
}

ir::Jump *DependentComputedData::jump(ast::Jump const *expr) {
  auto iter = jumps_.find(expr);
  return iter == jumps_.end() ? nullptr : &iter->second;
}

type::QualType const *DependentComputedData::qual_type(
    ast::Expression const *expr) const {
  auto iter = type_verification_results_.find(expr);
  if (iter != type_verification_results_.end()) { return &iter->second; }
  if (parent_) { return parent_->qual_type(expr); }
  return nullptr;
}

type::QualType DependentComputedData::set_qual_type(ast::Expression const *expr,
                                                    type::QualType r) {
  type_verification_results_.emplace(expr, r);
  return r;
}

LibraryModule *DependentComputedData::imported_module(ast::Import const *node) {
  auto iter = imported_modules_.find(node);
  if (iter != imported_modules_.end()) { return iter->second; }
  if (parent_) { return parent_->imported_module(node); }
  return nullptr;
}

void DependentComputedData::set_imported_module(ast::Import const *node,
                                                LibraryModule *module) {
  imported_modules_.emplace(node, module);
}

}  // namespace compiler
