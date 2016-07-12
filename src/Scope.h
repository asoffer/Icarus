#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

enum class ScopeType { While, For, Conditional, Function, Global };

struct Scope {
  static void verify_no_shadowing();
  static BlockScope *Global; // TODO Should this be it's own type
  static std::vector<AST::Declaration *> decl_registry_;

  void set_parent(Scope *parent);

  virtual bool is_block_scope() { return false; }
  virtual bool is_function_scope() { return false; }
  virtual bool is_loop_scope();

  // Returns an identifier pointer if there is a declaration of this identifier
  // in this scope. Otherwise it returns nullptr. It does *not* look in parent
  // scopes.
  AST::Identifier *IdentifierHereOrNull(const std::string &name);

  // Returns the identifier pointer being referenced by this string name, going
  // up the chaing of scopes as necessary. It returns nullptr if no such
  // identifier can be found.
  AST::Identifier *IdentifierBeingReferencedOrNull(const std::string &name);

  AST::Declaration *DeclHereOrNull(const std::string &name,
                                   Type *declared_type);

  AST::Declaration *DeclReferencedOrNull(const std::string &name,
                                         Type *declared_type);


  Scope();
  Scope(const Scope&) = delete;
  Scope(Scope&&) = delete;
  virtual ~Scope() {}

  std::vector<AST::Declaration *> ordered_decls_;
  std::vector<AST::Declaration *> DeclRegistry;

  Scope *parent;
  FnScope *containing_function_;
  std::string name;
};

struct BlockScope : public Scope {
  BlockScope() = delete;
  BlockScope(ScopeType st);
  virtual ~BlockScope(){}
  virtual bool is_block_scope() { return true; }

  void MakeReturn(Type *ret_type, IR::Value val);

  llvm::Value *AllocateLocally(Type *type, const std::string &name);
  llvm::Value *CreateLocalReturn(Type *type);

  void defer_uninit(Type *type, llvm::Value *val);

  ScopeType type;
  IR::Block *entry_block, *exit_block, *land_block;
  std::stack<std::pair<Type *, llvm::Value *>> deferred_uninits;
};

struct FnScope : public BlockScope {
  FnScope();
  virtual bool is_function_scope() { return true; }
  virtual ~FnScope(){}

  void add_scope(Scope *scope);
  void remove_scope(Scope *scope);

  Function *fn_type;
  llvm::Value *return_value, *exit_flag_;
  std::set<Scope *> innards_;

  IR::Value exit_flag, ret_val;
};

#endif // ICARUS_SCOPE_H
