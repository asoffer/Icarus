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

struct JumpExprs {
  enum class Kind { Return, Yield, Jump };
  std::vector<ast::Expression const *> &operator[](Kind k) {
    return data_[static_cast<std::underlying_type_t<Kind>>(k)];
  }

 private:
  std::array<std::vector<ast::Expression const *>, 3> data_;
};

struct Context {
  Context(Module *mod) : mod_(ASSERT_NOT_NULL(mod)) {
    constants_ = &mod_->dep_data_.front();
  }

  error::Log *error_log() { return &mod_->error_log_; }
  size_t num_errors() { return error_log()->size(); }
  void DumpErrors() { error_log()->Dump(); }

  // TODO remove.
  type::Type const *type_of(ast::Expression const *expr) const;

  ast::VerifyResult const *prior_verification_attempt(
      ast::Expression const *expr);
  ast::VerifyResult set_result(ast::ExprPtr expr, ast::VerifyResult r);

  ast::DispatchTable const *dispatch_table(ast::ExprPtr expr) const;
  void set_dispatch_table(ast::ExprPtr expr, ast::DispatchTable &&table);
  ast::DispatchTable const *dispatch_table(ast::Node const *node,
                                           size_t index) const;
  void push_dispatch_table(ast::Node const *node, ast::DispatchTable &&table);

  ir::Reg addr(ast::Declaration *decl) const;
  void set_addr(ast::Declaration *decl, ir::Reg);

  std::pair<ConstantBinding, Module::DependentData> *insert_constants(
      ConstantBinding const &constant_binding);


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
    YieldResult(ast::Expression *expr, ir::Results val)
        : expr_(expr), val_(std::move(val)) {}

    ast::Expression *expr_;
    ir::Results val_;
  };
  std::vector<std::vector<YieldResult>> yields_stack_;
  bool more_stmts_allowed_ = true;

  // Temporaries need to be destroyed at the end of each statement.
  // This is a pointer to a buffer where temporary allocations can register
  // themselves for deletion.
  std::vector<type::Typed<ir::Reg>> *temporaries_to_destroy_ = nullptr;

  // During validation, when a cyclic dependency is encountered, we write it
  // down here. That way, we can bubble up from the dependency until we see it
  // again, at each step adding the nodes to the error log involved in the
  // dependency. Once complete, we reset this to null
  std::vector<ast::Identifier *> cyc_deps_;

  // TODO Because you already have arguments, it's perhaps better to just be a
  // pointer into the arguments buffer, to avoid the
  // reallocation/double-storage, but we can deal with this later. Probably
  // requires a deeper refactoring to have things linke ir::ResultView, etc.
  absl::flat_hash_map<ir::Reg, ir::Results> *inline_ = nullptr;
};

#endif  // ICARUS_CONTEXT_H
