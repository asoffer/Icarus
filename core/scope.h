#ifndef ICARUS_CORE_SCOPE_H
#define ICARUS_CORE_SCOPE_H

#include <unordered_set>
#include <unordered_map>
#include <vector>
#include "base/util.h"
#include "type/typed_value.h"

struct Context;
struct Module;

namespace ast {
struct Declaration;
struct FunctionLiteral;
}  // namespace ast

namespace core {

struct DeclScope;
struct ExecScope;
struct FnScope;

struct Scope : public base::Cast<Scope> {
  Scope() = delete;
  Scope(Scope *parent) : parent(parent) {}
  virtual ~Scope() {}

  template <typename ScopeType>
  std::unique_ptr<ScopeType> add_child() {
    return std::make_unique<ScopeType>(this);
  }

  std::vector<type::Typed<ast::Declaration *>> AllDeclsWithId(
      std::string const &id, Context *ctx) const;

  Module const *module() const;

  void InsertDecl(ast::Declaration *decl);

  FnScope *ContainingFnScope();
  std::unordered_map<std::string, std::vector<ast::Declaration *>> decls_;
  std::unordered_map<std::string, std::vector<ast::Declaration *>> child_decls_;

  std::unordered_set<Module const *> embedded_modules_;
  Scope *parent = nullptr;
};

struct DeclScope : public Scope {
  DeclScope(Scope *parent) : Scope(parent) {}
  ~DeclScope() override {}
};

struct ModuleScope : public DeclScope {
  ModuleScope(::Module *mod) : DeclScope(nullptr), module_(mod) {}

 private:
  friend struct Scope;
  Module *module_;
};

struct ExecScope : public Scope {
  ExecScope(Scope *parent);
  ~ExecScope() override {}

  void MakeAllDestructions(Context *ctx);
};

struct FnScope : public ExecScope {
  FnScope(Scope *parent) : ExecScope(parent) {}
  ~FnScope() override {}

  void MakeAllStackAllocations(Context *ctx);

  ast::FunctionLiteral *fn_lit_ = nullptr;
  std::vector<ExecScope *> innards_{1, this};
};

inline FnScope *Scope::ContainingFnScope() {
  Scope *scope = this;
  while (scope && !scope->is<FnScope>()) { scope = scope->parent; }
  // static_cast rather than ->as<FnScope> because it could be null.
  return static_cast<FnScope *>(scope);
}

}  // namespace core

#endif  // ICARUS_CORE_SCOPE_H
