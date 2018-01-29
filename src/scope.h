#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <iosfwd>
#include <vector>
#include <unordered_set>
#include <unordered_map>

#include "base/owned_ptr.h"
#include "base/util.h"


namespace IR {
struct Val;
} // namespace IR

namespace AST {
struct Declaration;
struct Expression;
struct Identifier;
struct FunctionLiteral;
} // namespace AST

struct Type;
struct Function;
struct DeclScope;
struct ExecScope;
struct FnScope;

struct Scope : public base::Cast<Scope> {
  Scope() = delete;
  Scope(Scope *parent) : parent(parent) {}
  virtual ~Scope() {}
  virtual Scope *Clone() const = 0;

  template <typename ScopeType> base::owned_ptr<ScopeType> add_child() {
    return base::make_owned<ScopeType>(this);
  }

  // Returns an identifier pointer if there is a declaration of this identifier
  // in this scope. Otherwise it returns nullptr. It does *not* look in parent
  // scopes.
  AST::Identifier *IdHereOrNull(const std::string &name) const;

  // Returns the identifier pointer being referenced by this string name, going
  // up the chaing of scopes as necessary. It returns nullptr if no such
  // identifier can be found.
  AST::Identifier *IdReferencedOrNull(const std::string &name);

  Type *FunctionTypeReferencedOrNull(const std::string &fn_name,
                                     std::vector<Type *> input_type);

  AST::Declaration *DeclHereOrNull(const std::string &name,
                                   Type *declared_type);

  AST::Declaration *DeclReferencedOrNull(const std::string &name,
                                         Type *declared_type);

  std::vector<AST::Declaration *> AllDeclsWithId(const std::string &id);

  template <typename Fn>
  void ForEachDeclHere(const Fn& fn) const {
    for (const auto &[key, val] : decls_) {
      for (auto *decl : val) { fn(decl); }
    }
  }

  template <typename Fn> void ForEachDecl(const Fn &fn) const {
    for (auto *scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
      ForEachDeclHere(fn);
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
  Function *fn_type            = nullptr;
  AST::FunctionLiteral *fn_lit = nullptr;
  std::vector<ExecScope *> innards_{1, this};
};

inline FnScope *Scope::ContainingFnScope() {
  Scope *scope = this;
  while (scope && !scope->is<FnScope>()) { scope = scope->parent; }
  return static_cast<FnScope *>(scope);
}
#endif // ICARUS_SCOPE_H
