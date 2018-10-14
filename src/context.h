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

struct Context {
  Context(Module *mod) : mod_(ASSERT_NOT_NULL(mod)) {}

  size_t num_errors() { return error_log_.size(); }
  void DumpErrors() { error_log_.Dump(); }

  type::Type const *type_of(AST::Expression const *expr) const;
  void set_type(AST::Expression const *expr, type::Type const *t);

  IR::Register addr(AST::Declaration *decl) const;
  void set_addr(AST::Declaration *decl, IR::Register);

  error::Log error_log_;
  Module *mod_ = nullptr;

  AST::BoundConstants bound_constants_;

  // During validation, when a cyclic dependency is encountered, we write it
  // down here. That way, we can bubble up from the dependency until we see it
  // again, at each step adding the nodes to the error log involved in the
  // dependency. Once complete, we reset this to null
  base::vector<AST::Identifier *> *cyc_dep_vec_ = nullptr;
};

#endif  // ICARUS_CONTEXT_H
