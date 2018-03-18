#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <iosfwd>
#include <vector>
#include <unordered_set>
#include <unordered_map>

#include "base/util.h"

struct Context;

namespace IR {
struct Val;
} // namespace IR

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
struct FunctionLiteral;
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

  std::pair<std::vector<AST::Declaration *>, std::vector<AST::Declaration *>>
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
  std::unordered_map<std::string, std::vector<AST::Declaration *>> decls_;
  std::unordered_map<std::string, std::vector<AST::Declaration *>> child_decls_;
  Scope *parent = nullptr;
};

struct DeclScope : public Scope {
  DeclScope(Scope *parent) : Scope(parent) {}
  ~DeclScope() override {}
  DeclScope *Clone() const override { return new DeclScope(*this); }
};

struct ExecScope : public Scope {
  ExecScope(Scope *parent);
  ~ExecScope() override {}

  ExecScope *Clone() const override { return new ExecScope(*this); }
  void Enter() const;
  void Exit() const;

  bool can_jump = false;
};

struct FnScope : public ExecScope {
  FnScope(Scope *parent) : ExecScope(parent) {}
  ~FnScope() final {}

  FnScope *Clone() const override { return new FnScope(*this); }
  type::Function *fn_type      = nullptr;
  AST::FunctionLiteral *fn_lit = nullptr;
  std::vector<ExecScope *> innards_{1, this};
};

inline FnScope *Scope::ContainingFnScope() {
  Scope *scope = this;
  while (scope && !scope->is<FnScope>()) { scope = scope->parent; }
  // static_cast rather than ->as<FnScope> because it could be null.
  return static_cast<FnScope *>(scope);
}
#endif // ICARUS_SCOPE_H
