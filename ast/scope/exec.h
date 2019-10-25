#ifndef ICARUS_AST_SCOPE_EXEC_H
#define ICARUS_AST_SCOPE_EXEC_H

#include "ast/scope/scope.h"

namespace ast {

// A scope that represents a block of executable code. Function bodies, and
// local scope bodies of if-statements or while-loops are examples. This is in
// contrast to declarative scopes which are essentially just collections of
// declarations.
struct ExecScope : public Scope {
  ExecScope(Scope *parent);
  ~ExecScope() override {}
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_EXEC_H
