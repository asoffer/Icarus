#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <iosfwd>
#include "base/container/vector.h"
#include <unordered_set>
#include "base/container/unordered_map.h"

#include "base/util.h"

struct Context;
struct Module;

namespace IR {
struct Val;
} // namespace IR

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
struct FuncContent;
} // namespace AST

namespace type {
struct Type;
struct Function;
} // namespace type

struct DeclScope;
struct ExecScope;
struct FnScope;

struct Scope : public base::Cast<Scope> {
  Scope() = delete;
  Scope(Scope *parent) : parent(parent) {}
  virtual ~Scope() {}
  virtual Scope *Clone() const = 0;

  template <typename ScopeType> std::unique_ptr<ScopeType> add_child() {
    return std::make_unique<ScopeType>(this);
  }

  std::pair<base::vector<AST::Declaration *>, base::vector<AST::Declaration *>>
  AllDeclsWithId(const std::string &id, Context *ctx);

  template <typename Fn>
  void ForEachDeclHere(const Fn& fn) const {
    for (const auto &[key, val] : decls_) {
      for (auto *decl : val) { fn(decl); }
    }
  }

  void InsertDecl(AST::Declaration *decl);

  FnScope *ContainingFnScope();
  std::unordered_set<std::string> shadowed_decls_;
  base::unordered_map<std::string, base::vector<AST::Declaration *>> decls_;
  base::unordered_map<std::string, base::vector<AST::Declaration *>> child_decls_;
  Scope *parent = nullptr;
};

struct DeclScope : public Scope {
  DeclScope(Scope *parent) : Scope(parent) {}
  ~DeclScope() override {}
  DeclScope *Clone() const override { return new DeclScope(*this); }
  Module* module_; // Should be only on global scopes?
};

struct ExecScope : public Scope {
  ExecScope(Scope *parent);
  ~ExecScope() override {}

  ExecScope *Clone() const override { return new ExecScope(*this); }
  void Enter(Context *ctx) const;
  void Exit(Context *ctx) const;

  bool can_jump = false;
};

struct FnScope : public ExecScope {
  FnScope(Scope *parent) : ExecScope(parent) {}
  ~FnScope() final {}

  FnScope *Clone() const override { return new FnScope(*this); }
  type::Function *fn_type  = nullptr;
  AST::FuncContent *fn_lit = nullptr;
  base::vector<ExecScope *> innards_{1, this};
};

inline FnScope *Scope::ContainingFnScope() {
  Scope *scope = this;
  while (scope && !scope->is<FnScope>()) { scope = scope->parent; }
  // static_cast rather than ->as<FnScope> because it could be null.
  return static_cast<FnScope *>(scope);
}
#endif // ICARUS_SCOPE_H
