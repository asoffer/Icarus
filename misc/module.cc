#include "misc/module.h"

#include "ast/ast.h"
#include "ast/expression.h"
#include "base/guarded.h"
#include "frontend/source/source.h"
#ifdef ICARUS_VISITOR_EMIT_IR
#include "ir/compiled_fn.h"
#include "ir/scope_def.h"
#endif  // ICARUS_VISITOR_EMIT_IR
#include "type/function.h"
#include "type/jump.h"

// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
Module::Module() : scope_(this) {
#ifdef ICARUS_VISITOR_EMIT_IR
  dep_data_.emplace_back();
#endif  // ICARUS_VISITOR_EMIT_IR
}

Module::~Module() = default;

// TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
ir::CompiledFn *Module::AddFunc(
    type::Function const *fn_type,
    core::FnParams<type::Typed<ast::Expression const *>> params) {
  return fns_
      .emplace_back(
          std::make_unique<ir::CompiledFn>(this, fn_type, std::move(params)))
      .get();
}

ir::ScopeDef *Module::AddScope(
    std::vector<ir::AnyFunc> inits, std::vector<ir::AnyFunc> dones,
    absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks) {
  return scopes_
      .emplace_back(std::make_unique<ir::ScopeDef>(
          this, std::move(inits), std::move(dones), std::move(blocks)))
      .get();
}

ir::CompiledFn *Module::AddJump(
    type::Jump const *jump_type,
    core::FnParams<type::Typed<ast::Expression const *>> params) {
  return fns_
      .emplace_back(std::make_unique<ir::CompiledFn>(
          this, jump_type->ToFunction(), std::move(params)))
      .get();
}

type::Type const *Module::GetType(std::string_view name) const {
  ASSIGN_OR(return nullptr, auto &decl, GetDecl(name));
  return dep_data_.front().second.verify_results_.at(&decl).type_;
}

std::pair<ConstantBinding, DependentData> *Module::insert_constants(
    ConstantBinding const &constant_binding) {
  for (auto iter = dep_data_.begin(); iter != dep_data_.end(); ++iter) {
    auto &[key, val] = *iter;
    if (key == constant_binding) { return &*iter; }
  }
  auto &pair = dep_data_.emplace_back(constant_binding, DependentData{});
  pair.second.constants_ = pair.first;
  return &pair;
}
#endif  //  ICARUS_VISITOR_EMIT_IR

ast::Declaration *Module::GetDecl(std::string_view name) const {
  for (auto const &stmt : statements_) {
    ASSIGN_OR(continue, auto &decl, stmt->if_as<ast::Declaration>());
    if (decl.id() != name) { continue; }
    auto &hashtags = decl.hashtags_;
    bool exported =
        std::any_of(hashtags.begin(), hashtags.end(), [](ast::Hashtag h) {
          return h.kind_ == ast::Hashtag::Builtin::Export;
        });
    if (!exported) { continue; }
    return &decl;
  }
  return nullptr;
}
