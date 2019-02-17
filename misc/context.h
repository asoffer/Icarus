#ifndef ICARUS_CONTEXT_H
#define ICARUS_CONTEXT_H

#include "ast/bound_constants.h"
#include <unordered_map>
#include <vector>
#include "base/debug.h"
#include "ir/register.h"
#include "misc/module.h"

namespace type {
struct Type;
}  // namespace type

namespace ast {
struct Identifier;
struct DispatchTable;
}  // namespace ast

struct Context {
  Context(Context const *parent)
      : parent_(ASSERT_NOT_NULL(parent)), mod_(parent_->mod_) {}

  Context(Module *mod) : mod_(ASSERT_NOT_NULL(mod)) {}

  error::Log *error_log() { return &mod_->error_log_; }
  size_t num_errors() { return error_log()->size(); }
  void DumpErrors() { error_log()->Dump(); }

  type::Type const *type_of(ast::Expression const *expr) const;
  type::Type const *set_type(ast::Expression const *expr, type::Type const *t);

  ast::DispatchTable const *dispatch_table(ast::Expression const *expr) const;
  void set_dispatch_table(ast::Expression const *expr,
                          ast::DispatchTable &&table);
  ast::DispatchTable const *dispatch_table(ast::Node const *node, size_t index) const;
  void push_dispatch_table(ast::Node const *node,
                           ast::DispatchTable &&table);

  std::vector<ast::DispatchTable> const *rep_dispatch_tables(
      ast::Node const *node) const;

  void push_rep_dispatch_table(ast::Node const *node,
                               ast::DispatchTable &&tables);

  ir::Register addr(ast::Declaration *decl) const;
  void set_addr(ast::Declaration *decl, ir::Register);

  Context const *parent_ = nullptr;
  Module *mod_           = nullptr;

  ast::BoundConstants bound_constants_;

  // TODO this looks useful in bindings too. maybe give it a better name and
  // use it more frequently?
  struct YieldResult {
    YieldResult(ast::Expression *expr, ir::Val val)
        : expr_(expr), val_(std::move(val)) {}

    ast::Expression *expr_;
    ir::Val val_;
    };
    std::vector<std::vector<YieldResult>> yields_stack_;
    bool more_stmts_allowed_ = true;

    // Temporaries need to be destroyed at the end of each statement.
    // This is a pointer to a buffer where temporary allocations can register
    // themselves for deletion.
    std::vector<type::Typed<ir::Register>> *temporaries_to_destroy_ = nullptr;

    // During validation, when a cyclic dependency is encountered, we write it
    // down here. That way, we can bubble up from the dependency until we see it
    // again, at each step adding the nodes to the error log involved in the
    // dependency. Once complete, we reset this to null
    std::vector<ast::Identifier *> cyc_deps_;
};

#endif  // ICARUS_CONTEXT_H
