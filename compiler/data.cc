#include "compiler/data.h"

namespace compiler {

DependentComputedData::DependentComputedData(CompiledModule *mod) : mod_(mod) {}

DependentComputedData::~DependentComputedData() {
  // TODO figure out what's being dropped?
  // ASSERT(deferred_work_.lock()->empty() == true);
}

struct DependentComputedData::DependentDataChild::DataImpl {
  core::Params<type::QualType> params;
  std::vector<type::Type const *> rets;
  DependentComputedData data;
};

DependentComputedData::InsertDependentResult
DependentComputedData::InsertDependent(
    ast::ParameterizedExpression const *node,
    core::Params<type::QualType> const &params,
    ConstantBinding const &constants) {
  auto &[parent, map]   = dependent_data_[node];
  parent                = this;
  auto [iter, inserted] = map.try_emplace(params);

  if (inserted) {
    iter->second = std::unique_ptr<DependentDataChild::DataImpl>(
        new DependentDataChild::DataImpl{
            .params = params,
            .rets   = {},
            .data   = DependentComputedData(mod_),
        });

    auto &constant_binding = iter->second->data.constants_;
    constants.ForEach([&](auto const *decl, auto const &binding) {
      constant_binding.reserve_slot(decl, binding.type);
      constant_binding.set_slot(decl, binding.value);
    });

    iter->second->data.parent_ = this;
    for (size_t i = 0; i < node->params().size(); ++i) {
      auto const *decl = node->params()[i].value.get();
      iter->second->data.set_qual_type(decl, params[i].value);
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
    core::Params<type::QualType> const &params) {
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

void DependentComputedData::CompleteType(ast::Expression const *expr,
                                         bool success) {
  if (auto iter = type_verification_results_.find(expr);
      iter != type_verification_results_.end()) {
    if (not success) { iter->second.MarkError(); }
    return;
  }
  // Note: It is possible that we never find the type, because the original
  // verification had an error.
  if (parent_) { parent_->CompleteType(expr, success); }
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

absl::Span<ast::Declaration const *const> DependentComputedData::decls(
    ast::Identifier const *id) const {
  auto iter = decls_.find(id);
  if (iter == decls_.end()) { return ASSERT_NOT_NULL(parent_)->decls(id); }
  return iter->second;
}

void DependentComputedData::set_decls(
    ast::Identifier const *id, std::vector<ast::Declaration const *> decls) {
  decls_.emplace(id, std::move(decls));
}

bool DependentComputedData::cyclic_error(ast::Identifier const *id) const {
  auto iter = cyclic_error_ids_.find(id);
  if (iter != cyclic_error_ids_.end()) { return true; }
  if (not parent_) { return false; }
  return parent_->cyclic_error(id);
}

void DependentComputedData::set_cyclic_error(ast::Identifier const *id) {
  cyclic_error_ids_.insert(id);
}

type::Struct *DependentComputedData::get_struct(ast::StructLiteral const *s) const {
  auto iter = structs_.find(s);
  if (iter != structs_.end()) { return iter->second; }
  if (not parent_) { return nullptr; }
  return parent_->get_struct(s);
}

void DependentComputedData::set_struct(ast::StructLiteral const *sl,
                                       type::Struct *s) {
  structs_.emplace(sl, s);
}

bool DependentComputedData::ShouldVerifyBody(ast::Node const *node) {
  return body_verification_complete_.insert(node).second;
}

void DependentComputedData::ClearVerifyBody(ast::Node const *node) {
  body_verification_complete_.erase(node);
}

}  // namespace compiler
