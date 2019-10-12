#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/expr_ptr.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/results.h"
#include "type/jump.h"

std::atomic<bool> found_errors = false;

namespace compiler {

std::unique_ptr<module::BasicModule> CompileModule(frontend::Source *src) {
  auto mod = std::make_unique<compiler::CompiledModule>();
  Compiler c(mod.get());
  mod->compiler_ = &c;
  mod->Process(frontend::Parse(src));

  if (c.num_errors() > 0) { found_errors = true; }

  return mod;
}

Compiler::Compiler(module::BasicModule *mod) : mod_(mod), bldr_(ir::GetBuilder()) {
  dep_data_.emplace_back();
  constants_ = &dep_data_.front();
}

Compiler::~Compiler() { ASSERT(deferred_work_.lock()->empty() == true); }

VerifyResult const *Compiler::prior_verification_attempt(
    ast::ExprPtr expr) {
  auto const &map = constants_->second.verify_results_;
  if (auto iter = map.find(expr); iter != map.end()) { return &iter->second; }
  return nullptr;
}

type::Type const *Compiler::type_of(ast::Expression const *expr) const {
  if (auto *decl = expr->if_as<ast::Declaration>()) {
    if (auto *t = current_constants_.type_of(decl)) { return t; }
  }
  if (auto iter = constants_->second.verify_results_.find(expr);
      iter != constants_->second.verify_results_.end()) {
    if (iter->second.type_) { return iter->second.type_; }
  }

  // TODO reenabel once modules are all in core.
  // // When searching in embedded modules we intentionally look with no bound
  // // constants. Across module boundaries, a declaration can't be present anyway.
  // for (module::BasicModule const *mod : mod_->scope_.embedded_modules_) {
  //   // TODO use right constants
  //   if (auto iter = mod->dep_data_.front().second.verify_results_.find(expr);
  //       iter != mod->dep_data_.front().second.verify_results_.end()) {
  //     return iter->second.type_;
  //   }
  // }
  return nullptr;
}

void Compiler::set_addr(ast::Declaration const *decl,
                                      ir::Reg addr) {
  constants_->second.addr_[decl] = addr;
}
compiler::VerifyResult Compiler::set_result(
    ast::ExprPtr expr, compiler::VerifyResult r) {
  constants_->second.verify_results_.emplace(expr, r);
  return r;
}

ir::Reg Compiler::addr(ast::Declaration const *decl) const {
  return constants_->second.addr_.at(decl);
}

void Compiler::set_dispatch_table(ast::ExprPtr expr,
                                                ast::DispatchTable &&table) {
  constants_->second.dispatch_tables_.emplace(expr, std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

std::pair<ConstantBinding, DependentData> *Compiler::insert_constants(
    ConstantBinding const &constant_binding) {
  for (auto iter = dep_data_.begin(); iter != dep_data_.end(); ++iter) {
    auto &[key, val] = *iter;
    if (key == constant_binding) { return &*iter; }
  }
  auto *pair = &dep_data_.emplace_back(constant_binding, DependentData{});
  pair->second.constants_ = pair->first;

  for (auto const &[decl, binding] : constant_binding.keys_) {
    pair->second.verify_results_.emplace(
        ast::ExprPtr(decl), compiler::VerifyResult::Constant(binding.type_));
  }
  return pair;
}

void Compiler::set_jump_table(ast::ExprPtr jump_expr,
                                            ast::ExprPtr node,
                                            ast::DispatchTable &&table) {
  constants_->second.jump_tables_.emplace(std::pair{jump_expr, node},
                                          std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

void Compiler::set_pending_module(ast::Import const *import_node,
                                  module::PendingModule mod) {
  constants_->second.imported_module_.emplace(import_node, std::move(mod));
}

ast::DispatchTable const *Compiler::dispatch_table(ast::ExprPtr expr) const {
  auto &table = constants_->second.dispatch_tables_;
  if (auto iter = table.find(expr); iter != table.end()) {
    return &iter->second;
  }
  return nullptr;
}

module::PendingModule *Compiler::pending_module(
    ast::Import const *import_node) const {
  if (auto iter = constants_->second.imported_module_.find(import_node);
      iter != constants_->second.imported_module_.end()) {
    return &iter->second;
  }
  return nullptr;
}

void Compiler::CompleteDeferredBodies() {
  base::move_func<void()> f;
  while (true) {
    {
      auto handle = deferred_work_.lock();
      if (handle->empty()) { return; }
      auto nh = handle->extract(handle->begin());
      f       = std::move(nh.mapped());
    }
    std::move(f)();
  }
}

ir::CompiledFn *Compiler::AddFunc(
    type::Function const *fn_type,
    core::FnParams<type::Typed<ast::Expression const *>> params) {
  return fns_
      .emplace_back(
          std::make_unique<ir::CompiledFn>(fn_type, std::move(params)))
      .get();
}

ir::CompiledFn *Compiler::AddJump(
    type::Jump const *jump_type,
    core::FnParams<type::Typed<ast::Expression const *>> params) {
  return fns_
      .emplace_back(std::make_unique<ir::CompiledFn>(jump_type->ToFunction(),
                                                     std::move(params)))
      .get();
}

ir::ScopeDef *Compiler::AddScope(
    std::vector<ir::AnyFunc> inits, std::vector<ir::AnyFunc> dones,
    absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks) {
  return scope_defs_
      .emplace_back(std::make_unique<ir::ScopeDef>(
          module(), std::move(inits), std::move(dones), std::move(blocks)))
      .get();
}

ir::BlockDef *Compiler::AddBlock(std::vector<ir::AnyFunc> befores,
                                 std::vector<ir::AnyFunc> afters) {
  return block_defs_
      .emplace_back(
          std::make_unique<ir::BlockDef>(std::move(befores), std::move(afters)))
      .get();
}

}  // namespace compiler
