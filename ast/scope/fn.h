#ifndef ICARUS_AST_SCOPE_FN_H
#define ICARUS_AST_SCOPE_FN_H

#include <vector>

#include "ast/ast_fwd.h"
#include "ast/scope/exec.h"
#include "base/bag.h"

namespace ast {

// An executable scope representing the body of a function literal. These scopes
// are of importance and worth separating from ExecScopes in general because
// they need to know about all child scopes so they can stack-allocate enough
// space when they start.
struct FnScope : public ExecScope {
  FnScope(Scope *parent, FunctionLiteral *fn_lit = nullptr)
      : ExecScope(parent), fn_lit_(fn_lit) {
    descendants_.insert(this);
  }
  ~FnScope() override {}

  FunctionLiteral *fn_lit_ = nullptr;
  void insert_descendant(ExecScope *s) { descendants_.insert(s); }
  base::bag<ExecScope *> const &descendants() const { return descendants_; }

 private:
  base::bag<ExecScope *> descendants_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_FN_H
