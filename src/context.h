#ifndef ICARUS_CONTEXT_H
#define ICARUS_CONTEXT_H

#include "base/container/vector.h"
#include "base/debug.h"
#include "error/log.h"
#include "ir/register.h"
#include "module.h"

namespace type {
struct Type;
}  // namespace type

namespace ast {
struct Identifier;
}  // namespace ast

struct Context {
  Context(Context const *parent)
      : parent_(ASSERT_NOT_NULL(parent)), mod_(parent_->mod_) {}

  Context(Module *mod) : mod_(ASSERT_NOT_NULL(mod)) {}

  size_t num_errors() { return error_log_.size(); }
  void DumpErrors() { error_log_.Dump(); }

  type::Type const *type_of(ast::Expression const *expr) const;
  type::Type const *set_type(ast::Expression const *expr, type::Type const *t);

  ir::Register addr(ast::Declaration *decl) const;
  void set_addr(ast::Declaration *decl, ir::Register);

  error::Log error_log_;
  Context const *parent_ = nullptr;
  Module *mod_ = nullptr;

  ast::BoundConstants bound_constants_;

  // TODO this looks useful in bindings too. maybe give it a better name and use
  // it more frequently?
  struct YieldResult {
    YieldResult(ast::Expression *expr, ir::Val val)
        : expr_(expr), val_(std::move(val)) {}

    ast::Expression *expr_;
    ir::Val val_;
  };
  base::vector<base::vector<YieldResult>> yields_stack_;

  // During validation, when a cyclic dependency is encountered, we write it
  // down here. That way, we can bubble up from the dependency until we see it
  // again, at each step adding the nodes to the error log involved in the
  // dependency. Once complete, we reset this to null
  base::vector<ast::Identifier *> cyc_deps_;
};

#endif  // ICARUS_CONTEXT_H
