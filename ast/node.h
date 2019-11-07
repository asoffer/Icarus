#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "ast/visitor_base.h"
#include "base/cast.h"
#include "frontend/source/range.h"

// TODO extract these
#include "ast/methods/dump.h"
#include "module/assign_scope.h"
#include "module/dependent_decls.h"

namespace ast {
struct Declaration;
struct Scope;

struct Node : public base::Cast<Node> {
  Node(frontend::SourceRange span = frontend::SourceRange())
      : span(std::move(span)) {}
  virtual ~Node() {}

  virtual void assign_scope(module::AssignScope *visitor, ast::Scope *scope) {
    (*visitor)(this, scope);
  }
  virtual void Dump(ast::Dump *visitor) const { (*visitor)(this); }
  virtual void DependentDecls(module::DependentDecls *visitor,
                              Declaration const *d) const {
    (*visitor)(this, d);
  }

  virtual void Accept(VisitorBase *visitor, void *ret,
                      void *arg_tuple) const = 0;

  ast::Scope *scope_ = nullptr;
  frontend::SourceRange span;
};

}  // namespace ast

#define METHODS                                                                \
  void assign_scope(module::AssignScope *visitor, ast::Scope *scope)           \
      override {                                                               \
    (*visitor)(this, scope);                                                   \
  }                                                                            \
  void Dump(ast::Dump *visitor) const override { (*visitor)(this); }           \
  void DependentDecls(module::DependentDecls *visitor,                         \
                      ast::Declaration const *d) const override {              \
    (*visitor)(this, d);                                                       \
  }

#endif  // ICARUS_AST_NODE_H
