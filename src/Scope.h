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
struct Scope {
  static std::stack<Scope *> Stack;
  static void verify_no_shadowing();
  static BlockScope *Global; // TODO Should this be it's own type
  static AST::Declaration *
  make_declaration(size_t line_num, AST::DeclType decl_type,
                   const std::string &id_string);
  static std::vector<AST::Declaration *> decl_registry_;

  void set_parent(Scope *parent);

  virtual bool is_block_scope() { return false; }
  virtual bool is_function_scope() { return false; }

  AST::Identifier *identifier(AST::Expression *id_as_eptr);
  AST::Identifier *identifier(const std::string &name) const;

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
};

struct BlockScope : public Scope {
  BlockScope();
  virtual ~BlockScope(){}
  virtual bool is_block_scope() { return true; }

  virtual void initialize();
  void uninitialize();
  void make_return(llvm::Value *val);

  void set_parent_function(llvm::Function *fn);

  llvm::BasicBlock *entry, *exit;
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
  llvm::Value *return_value;
  std::set<Scope *> innards_;
};

// TODO these are not threadsafe! When we access the stack, when compilation is
// multi-threaded, we should probably grab a mutex before getting the top of the
// stack

Scope *CurrentScope();
Context &CurrentContext();


/*****************************************************************************

struct Scope {
  static std::stack<Scope *> Stack;
  static GlobalScope *Global;

  static void verify_no_shadowing();

  static AST::Declaration *
  make_declaration(size_t line_num, AST::DeclType decl_type,
                   const std::string &id_string);

  void set_parent(Scope *parent);

  virtual bool is_function_scope() const { return false; }

  AST::Identifier *identifier(AST::Expression *id_as_eptr);
  AST::Identifier *identifier(const std::string &name) const;

  AST::Expression *get_declared_type(AST::Identifier *id_ptr) const;

  void initialize(llvm::BasicBlock *block);
  void uninitialize(llvm::BasicBlock *block);

  Context &context() { return ctx_; }

  virtual void make_return(llvm::Value *val);

  Scope(const Scope &) = delete;
  Scope(Scope &&) = delete;
  virtual ~Scope() {}

  Scope();

  std::map<std::string, AST::Identifier *> ids_;
  std::vector<AST::Declaration *> ordered_decls_;

  Context ctx_;
  Scope *parent;
  FnScope *containing_function_;

  llvm::IRBuilder<> builder;

  static std::vector<AST::Declaration *> decl_registry_;
};

struct TypeScope : public Scope {
  // TODO why are you even bothering making these blocks?
  TypeScope()
      : entry_block_(make_block("entry", nullptr)),
        exit_block_(make_block("exit", nullptr)) {}

  virtual ~TypeScope() {}

  llvm::BasicBlock *entry_block() { return entry_block_; }
  llvm::BasicBlock *exit_block() { return exit_block_; }

  llvm::BasicBlock *entry_block_, *exit_block_;
};

struct FnScope : public Scope {
  virtual bool is_function_scope() const { return true; }
  void set_type(Function *fn_type) { fn_type_ = fn_type; }
  void add_scope(Scope *scope);
  void remove_scope(Scope *scope) { innards_.erase(scope); }

  void set_parent_function(llvm::Function *fn);

  virtual void make_return(llvm::Value *val);
  llvm::Value *return_value() const { return return_val_; }

  virtual llvm::BasicBlock *entry_block() { return entry_block_; }
  virtual llvm::BasicBlock *exit_block() { return exit_block_; }

  virtual void enter();
  virtual void exit();

  FnScope(llvm::Function *fn = nullptr);

  virtual ~FnScope() {}

  std::set<Scope *> innards_;
  Function *fn_type_;
  llvm::Function *llvm_fn_;
  llvm::Value *return_val_;

  llvm::BasicBlock *entry_block_, *exit_block_;
  void allocate(Scope *scope);
};

struct GlobalScope : public Scope {
  virtual llvm::BasicBlock *entry_block() { return the_block_; }
  virtual llvm::BasicBlock *exit_block() { return the_block_; }

  void initialize();

  GlobalScope() : the_block_(make_block("global.block", nullptr)) {
    builder.SetInsertPoint(entry_block());
  }

  virtual ~GlobalScope() {}

  llvm::BasicBlock *the_block_;
};
*/
#endif // ICARUS_SCOPE_H
