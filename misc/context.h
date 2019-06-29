#ifndef ICARUS_CONTEXT_H
#define ICARUS_CONTEXT_H

#include <vector>

#include "base/debug.h"
#include "ir/register.h"
#include "ir/results.h"
#include "misc/module.h"

namespace type {
struct Type;
}  // namespace type

namespace ast {
struct Identifier;
struct DispatchTable;
}  // namespace ast

struct Context {
  Context(Module *mod) : mod_(ASSERT_NOT_NULL(mod)) {
    constants_ = &mod_->dep_data_.front();
  }

  error::Log *error_log() { return &mod_->error_log_; }
  size_t num_errors() { return error_log()->size(); }
  void DumpErrors() { error_log()->Dump(); }

  // TODO remove.
  type::Type const *type_of(ast::Expression const *expr) const;

  visitor::VerifyResult const *prior_verification_attempt(ast::ExprPtr expr);
  visitor::VerifyResult set_result(ast::ExprPtr expr,
                                       visitor::VerifyResult r);
  ast::DispatchTable const *dispatch_table(ast::ExprPtr expr) const;
  void set_dispatch_table(ast::ExprPtr expr, ast::DispatchTable &&table);

  ast::DispatchTable const *jump_table(ast::ExprPtr expr, std::string_view s) const;
  void set_jump_table(ast::ExprPtr expr, std::string_view s,
                      ast::DispatchTable &&table);

  ir::Reg addr(ast::Declaration const *decl) const;
  void set_addr(ast::Declaration const *decl, ir::Reg);

  std::pair<ConstantBinding, Module::DependentData> *insert_constants(
      ConstantBinding const &constant_binding);

  core::PendingModule *pending_module(ast::Import const *import_node) const;
  void set_pending_module(ast::Import const *import_node,
                          core::PendingModule mod);

  ir::ScopeDef * scope_def(ast::ScopeLiteral const *scope_lit) const;
  void set_scope_def(ast::ScopeLiteral const *scope_lit,
                     ir::ScopeDef *scope_def);

  Module *mod_ = nullptr;

  std::pair<ConstantBinding, Module::DependentData> *constants_;
  // We only want to generate at most one node for each set of constants in a
  // function literal, but we can't generate them all at once because, for
  // example:
  //   (val :: T, T :: type) -> () { ... }
  // So we need to be able to build them even when there are dependencies
  // between them. To do this, we bulid them here and then move them into the
  // module constants when they're ready.
  ConstantBinding current_constants_;

  // TODO this looks useful in bindings too. maybe give it a better name and
  // use it more frequently?
  struct YieldResult {
    YieldResult(ast::Expression const *expr, ir::Results val)
        : expr_(expr), val_(std::move(val)) {}

    ast::Expression const *expr_;
    ir::Results val_;
  };
  std::vector<std::vector<YieldResult>> yields_stack_;
  bool more_stmts_allowed_ = true;

  absl::flat_hash_map<ir::BlockDef const *, ir::BlockIndex> *block_map;

  // Temporaries need to be destroyed at the end of each statement.
  // This is a pointer to a buffer where temporary allocations can register
  // themselves for deletion.
  std::vector<type::Typed<ir::Reg>> *temporaries_to_destroy_ = nullptr;

  // During validation, when a cyclic dependency is encountered, we write it
  // down here. That way, we can bubble up from the dependency until we see it
  // again, at each step adding the nodes to the error log involved in the
  // dependency. Once complete, we reset this to null
  std::vector<ast::Identifier const *> cyc_deps_;

  // TODO Because you already have arguments, it's perhaps better to just be a
  // pointer into the arguments buffer, to avoid the
  // reallocation/double-storage, but we can deal with this later. Probably
  // requires a deeper refactoring to have things linke ir::ResultView, etc.
  absl::flat_hash_map<ir::Reg, ir::Results> *inline_ = nullptr;
};

#endif  // ICARUS_CONTEXT_H
