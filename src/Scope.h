#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <vector>
#include <map>
#include <set>
#include <string>
#include <type_traits>
#include <stack>

#include "Type.h"
#include "DependencySystem.h"
#include "Context.h"
#include "AST/DeclType.h"

#include <iostream>

struct Type;
struct Function;
struct BlockScope;
struct FnScope;

namespace AST {
struct Declaration;
struct Identifier;
struct FunctionLiteral;
} // namespace AST

extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

enum class ScopeType { While, For, Conditional, Function, Global };

struct Scope {
  static std::stack<Scope *> Stack;
  static void verify_no_shadowing();
  static BlockScope *Global; // TODO Should this be it's own type
  static AST::Declaration *make_declaration(size_t line_num,
                                            AST::DeclType decl_type,
                                            const std::string &id_string,
                                            AST::Expression *type_expr);
  static std::vector<AST::Declaration *> decl_registry_;

  void set_parent(Scope *parent);

  virtual bool is_block_scope() { return false; }
  virtual bool is_function_scope() { return false; }
  virtual bool is_loop_scope();

  AST::Identifier *identifier(AST::Expression *id_as_eptr);

  // Returns an identifier pointer if there is a declaration of this identifier
  // in this scope. Otherwise it returns nullptr. It does *not* look in parent
  // scopes.
  AST::Identifier *IdentifierHereOrNull(const std::string &name);

  // Returns the identifier pointer being referenced by this string name, going
  // up the chaing of scopes as necessary. It returns nullptr if no such
  // identifier can be found.
  AST::Identifier *IdentifierBeingReferencedOrNull(const std::string &name);

  AST::Expression *get_declared_type(AST::Identifier *id_ptr) const;

  Scope();
  Scope(const Scope&) = delete;
  Scope(Scope&&) = delete;
  virtual ~Scope() {}

  std::map<std::string, AST::Identifier *> ids_;
  std::vector<AST::Declaration *> ordered_decls_;

  Scope *parent;
  FnScope *containing_function_;
  Context context;
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

  void set_parent_function(llvm::Function *fn);
  void defer_uninit(TypePtr type, llvm::Value *val);

  ScopeType type;
  llvm::BasicBlock *entry, *exit, *land;
  std::stack<std::pair<TypePtr, llvm::Value *>> deferred_uninits;
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

  Function *fn_type;
  llvm::Function *llvm_fn;
  llvm::Value *return_value, *exit_flag;
  std::set<Scope *> innards_;
};

// TODO these are not threadsafe! When we access the stack, when compilation is
// multi-threaded, we should probably grab a mutex before getting the top of the
// stack

Scope *CurrentScope();
Context &CurrentContext();

#endif // ICARUS_SCOPE_H
