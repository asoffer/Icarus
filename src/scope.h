#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <iosfwd>
#include <unordered_set>
#include "base/container/unordered_map.h"
#include "base/container/vector.h"

#include "base/util.h"

struct Context;
struct Module;

namespace IR {
struct Val;
}  // namespace IR

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
struct FunctionLiteral;
}  // namespace AST

namespace type {
struct Type;
struct Function;
}  // namespace type

struct DeclScope;
struct ExecScope;
struct FnScope;

struct TypedDecl {
  TypedDecl(type::Type const *type, AST::Declaration *decl)
      : type_(type), decl_(decl) {}
  type::Type const *type_;
  AST::Declaration *decl_;
};

struct Scope : public base::Cast<Scope> {
  Scope() = delete;
  Scope(Scope *parent) : parent(parent) {}
  virtual ~Scope() {}

  template <typename ScopeType>
  std::unique_ptr<ScopeType> add_child() {
    return std::make_unique<ScopeType>(this);
  }

  std::pair<base::vector<TypedDecl>, base::vector<TypedDecl>> AllDeclsWithId(
      const std::string &id, Context *ctx);

  void InsertDecl(AST::Declaration *decl);

  FnScope *ContainingFnScope();
  std::unordered_set<std::string> shadowed_decls_;
  base::unordered_map<std::string, base::vector<AST::Declaration *>> decls_;
  base::unordered_map<std::string, base::vector<AST::Declaration *>>
      child_decls_;

  Scope *parent = nullptr;
};

struct DeclScope : public Scope {
  DeclScope(Scope *parent) : Scope(parent) {}
  ~DeclScope() override {}
  Module *module_;  // Should be only on global scopes?
};

struct ExecScope : public Scope {
  ExecScope(Scope *parent);
  ~ExecScope() override {}
};

struct FnScope : public ExecScope {
  FnScope(Scope *parent) : ExecScope(parent) {}
  ~FnScope() final {}

  void MakeAllStackAllocations(Module *mod);

  type::Function *fn_type      = nullptr;  // TODO deprecate?
  AST::FunctionLiteral *fn_lit = nullptr;
  base::vector<ExecScope *> innards_{1, this};
};

inline FnScope *Scope::ContainingFnScope() {
  Scope *scope = this;
  while (scope && !scope->is<FnScope>()) { scope = scope->parent; }
  // static_cast rather than ->as<FnScope> because it could be null.
  return static_cast<FnScope *>(scope);
}
#endif  // ICARUS_SCOPE_H
