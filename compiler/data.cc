#include "compiler/data.h"

namespace compiler {

DependentComputedData::DependentComputedData(CompiledModule *mod)
    : mod_(*ASSERT_NOT_NULL(mod)) {}
DependentComputedData::~DependentComputedData() {}

struct DependentComputedData::DependentDataChild::DataImpl {
  core::Params<std::pair<ir::Value, type::QualType>> params;
  std::vector<type::Type const *> rets;
  DependentComputedData data;
};

DependentComputedData::InsertDependentResult
DependentComputedData::InsertDependent(
    ast::ParameterizedExpression const *node,
    core::Params<std::pair<ir::Value, type::QualType>> const &params) {
  auto &[parent, map]   = dependent_data_[node];
  parent                = this;
  auto [iter, inserted] = map.try_emplace(params);

  if (inserted) {
    iter->second = std::unique_ptr<DependentDataChild::DataImpl>(
        new DependentDataChild::DataImpl{
            .params = params,
            .rets   = {},
            .data   = DependentComputedData(&mod_),
        });

    size_t i = 0;
    for (auto const &p : params) {
      if (p.value.first.empty()) { continue; }
      iter->second->data.SetConstant(node->params()[i++].value.get(),
                                     p.value.first);
    }

    iter->second->data.parent_ = this;
    for (size_t i = 0; i < node->params().size(); ++i) {
      auto const *decl = node->params()[i].value.get();
      iter->second->data.set_qual_type(decl, params[i].value.second);
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
    core::Params<std::pair<ir::Value, type::QualType>> const &params) {
  auto &map = dependent_data_.find(node)->second.map;
  auto iter = map.find(params);
  ASSERT(iter != map.end());
  auto &[parameters, rets, data] = *iter->second;
  return FindDependentResult{
      .fn_type = type::Func(
          parameters.Transform([](auto const &p) { return p.second; }), rets),
      .data = data,
  };
}

ir::CompiledJump *DependentComputedData::jump(ast::Jump const *expr) {
  auto iter = jumps_.find(expr);
  return iter == jumps_.end() ? nullptr : &iter->second;
}

type::QualType const *DependentComputedData::qual_type(
    ast::Expression const *expr) const {
  if (auto iter = qual_types_.find(expr); iter != qual_types_.end()) {
    return &iter->second;
  }
  if (parent_) { return parent_->qual_type(expr); }
  return nullptr;
}

type::QualType DependentComputedData::set_qual_type(ast::Expression const *expr,
                                                    type::QualType r) {
  qual_types_.emplace(expr, r);
  return r;
}

void DependentComputedData::CompleteType(ast::Expression const *expr,
                                         bool success) {
  if (auto iter = qual_types_.find(expr); iter != qual_types_.end()) {
    if (not success) { iter->second.MarkError(); }
    return;
  }
  // Note: It is possible that we never find the type, because the original
  // verification had an error.
  if (parent_) { parent_->CompleteType(expr, success); }
}

ir::ModuleId DependentComputedData::imported_module(ast::Import const *node) {
  auto iter = imported_modules_.find(node);
  if (iter != imported_modules_.end()) { return iter->second; }
  if (parent_) { return parent_->imported_module(node); }
  return ir::ModuleId::Invalid();
}

void DependentComputedData::set_imported_module(ast::Import const *node,
                                                ir::ModuleId module_id) {
  imported_modules_.emplace(node, module_id);
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

type::Struct *DependentComputedData::get_struct(
    ast::StructLiteral const *s) const {
  auto iter = structs_.find(s);
  if (iter != structs_.end()) { return iter->second; }
  if (not parent_) { return nullptr; }
  return parent_->get_struct(s);
}

void DependentComputedData::set_struct(ast::StructLiteral const *sl,
                                       type::Struct *s) {
  structs_.emplace(sl, s);
}

type::Struct *DependentComputedData::get_struct(
    ast::ParameterizedStructLiteral const *s) const {
  auto iter = param_structs_.find(s);
  if (iter != param_structs_.end()) { return iter->second; }
  if (not parent_) { return nullptr; }
  return parent_->get_struct(s);
}

void DependentComputedData::set_struct(
    ast::ParameterizedStructLiteral const *sl, type::Struct *s) {
  param_structs_.emplace(sl, s);
}

bool DependentComputedData::ShouldVerifyBody(ast::Node const *node) {
  return body_verification_complete_.insert(node).second;
}

void DependentComputedData::ClearVerifyBody(ast::Node const *node) {
  body_verification_complete_.erase(node);
}

void DependentComputedData::CompleteConstant(ast::Declaration const *decl) {
  auto iter = constants_.find(decl);
  ASSERT(iter != constants_.end());
  iter->second.complete = true;
}

void DependentComputedData::SetConstant(ast::Declaration const *decl,
                                        ir::Value const &value, bool complete) {
  constants_.emplace(decl, ConstantValue{.value = value, .complete = complete});
}

DependentComputedData::ConstantValue const *DependentComputedData::Constant(
    ast::Declaration const *decl) const {
  auto iter = constants_.find(decl);
  return iter != constants_.end() ? &iter->second : nullptr;
}

void DependentComputedData::SetAllOverloads(ast::Expression const *callee,
                                            ast::OverloadSet os) {
  [[maybe_unused]] auto [iter, inserted] =
      all_overloads_.emplace(callee, std::move(os));
  ASSERT(inserted == true);
}

ast::OverloadSet const &DependentComputedData::AllOverloads(
    ast::Expression const *callee) const {
  auto iter = all_overloads_.find(callee);
  if (iter == all_overloads_.end()) {
    if (parent_ == nullptr) {
      UNREACHABLE("Failed to find any overloads for ", callee->DebugString());
    }
    return parent_->AllOverloads(callee);
  } else {
    return iter->second;
  }
}
}  // namespace compiler
