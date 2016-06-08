#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

enum class ScopeType { While, For, Conditional, Function, Global };

struct Scope {
  static std::stack<Scope *> Stack;
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

  void SetCTRV(Context::Value v);
  Context::Value GetCTRV();
  void ClearCTRV();
  bool HasCTRV();

  Scope *parent;
  FnScope *containing_function_;
  std::string name;
};

struct BlockScope : public Scope {
  BlockScope() = delete;
  BlockScope(ScopeType st);
  virtual ~BlockScope(){}
  virtual bool is_block_scope() { return true; }

  virtual void initialize();
  void uninitialize();
  void make_return(llvm::Value *val);

  llvm::Value *AllocateLocally(Type *type, const std::string &name);
  llvm::Value *CreateLocalReturn(Type *type);

  void set_parent_function(llvm::Function *fn);
  void defer_uninit(Type *type, llvm::Value *val);

  ScopeType type;
  llvm::BasicBlock *entry, *exit, *land;
  std::stack<std::pair<Type *, llvm::Value *>> deferred_uninits;
};

struct FnScope : public BlockScope {
  FnScope(llvm::Function *fn = nullptr);
  virtual bool is_function_scope() { return true; }
  virtual ~FnScope(){}

  void add_scope(Scope *scope);
  void remove_scope(Scope *scope);

  virtual void initialize();
  void leave();
  void allocate(Scope *scope);

  // Return the exit flag if it exists. If it doesn't create the flag,
  // initialize it, and return it.
  llvm::Value *ExitFlag();

  Function *fn_type;
  llvm::Function *llvm_fn;
  llvm::Value *return_value, *exit_flag_;
  std::set<Scope *> innards_;

  bool has_ctrv;
  Context::Value ct_ret_val; // Used to store the return value when functions
                             // are evaluated at compile-time
};

// TODO these are not threadsafe! When we access the stack, when compilation is
// multi-threaded, we should probably grab a mutex before getting the top of the
// stack

Scope *CurrentScope();

#endif // ICARUS_SCOPE_H
