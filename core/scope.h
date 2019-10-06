#ifndef ICARUS_CORE_SCOPE_H
#define ICARUS_CORE_SCOPE_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "ast/ast_fwd.h"
#include "base/util.h"

namespace module {
struct Module;
}  // namespace module

namespace core {

struct Scope : public base::Cast<Scope> {
  Scope() = delete;
  Scope(Scope *parent) : parent(parent) {}
  virtual ~Scope() {}

  template <typename ScopeType, typename ...Args>
  std::unique_ptr<ScopeType> add_child(Args&& ...args) {
    return std::make_unique<ScopeType>(this, std::forward<Args>(args)...);
  }

  std::vector<ast::Declaration const *> AllDeclsWithId(
      std::string_view id) const;

  module::Module const *module() const;
  module::Module *module();

  void InsertDecl(std::string const &id, ast::Declaration *decl);

  template <typename Sc>
  Sc *Containing() {
    Scope *scope = this;
    while (scope && !scope->is<Sc>()) { scope = scope->parent; }
    return static_cast<Sc *>(scope);
  }

  // TODO these ids are already stored on the declaration so it's probably safe
  // to use string_views here. Need to really guarantee that ast nodes are
  // constant after construction.
  absl::flat_hash_map<std::string, std::vector<ast::Declaration *>> decls_;
  absl::flat_hash_map<std::string, std::vector<ast::Declaration *>>
      child_decls_;

  absl::flat_hash_set<module::Module const *> embedded_modules_;
  Scope *parent = nullptr;
};

struct DeclScope : public Scope {
  DeclScope(Scope *parent) : Scope(parent) {}
  ~DeclScope() override {}
};

struct ScopeLitScope : public DeclScope {
  ScopeLitScope(Scope *parent, ast::ScopeLiteral *sl)
      : DeclScope(parent), scope_lit_(sl) {}
  ast::ScopeLiteral *scope_lit_ = nullptr;
};

struct ModuleScope : public DeclScope {
  ModuleScope(module::Module *mod) : DeclScope(nullptr), module_(mod) {}

 private:
  friend struct Scope;
  module::Module *module_;
};

struct ExecScope : public Scope {
  ExecScope(Scope *parent);
  ~ExecScope() override {}
};

struct FnScope : public ExecScope {
  FnScope(Scope *parent) : ExecScope(parent) {}
  ~FnScope() override {}

  ast::FunctionLiteral *fn_lit_ = nullptr;
  std::vector<ExecScope *> innards_{1, this};
};

}  // namespace core

#endif  // ICARUS_CORE_SCOPE_H
