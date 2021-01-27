#ifndef ICARUS_AST_SCOPE_DECL_H
#define ICARUS_AST_SCOPE_DECL_H

#include "ast/scope/scope.h"

namespace ast {

// Represents a scope whose contents can only be declarations. This is in
// contrast with scopes that can have other types of statements. For example, a
// struct is a declarative scope because all entries inside that scope must be
// declarations.
struct DeclScope : public Scope {
  DeclScope(Scope *parent, bool executable) : Scope(parent, executable) {}
  ~DeclScope() override {}
};

}  // namespace ast

#endif  //  ICARUS_AST_SCOPE_DECL_H
