#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <vector>
#include <map>
#include <set>
#include <string>
#include <type_traits>

#include "typedefs.h"
#include "Type.h"
#include "DependencySystem.h"
#include "Context.h"

#include <iostream>

class Type;
class Function;
class GlobalScope;
class FnScope;

namespace AST {
  class FunctionLiteral;
}  // namespace AST

extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);

class Scope {
  public:
    friend class AST::FunctionLiteral;
    friend class AST::Declaration;
    friend class FnScope;

    static GlobalScope* Global;

    static void verify_no_shadowing();

    static DeclPtr make_declaration(size_t line_num, const std::string& id_string);

    virtual void enter();
    virtual void exit();

    virtual llvm::BasicBlock* entry_block() = 0;
    virtual llvm::BasicBlock* exit_block() = 0;

    virtual void make_return(llvm::Value* val);

    llvm::IRBuilder<>& builder() { return bldr_; }

    void set_parent(Scope* parent);
    Scope* parent() const { return parent_; }

    virtual bool is_function_scope() const { return false; }
    virtual bool is_loop_scope() const { return false; }

    // While we're actually returning an IdPtr, it's only ever used as an
    // EPtr, so we do the pointer cast inside.
    EPtr identifier(EPtr id_as_eptr);
    EPtr identifier(const std::string& name) const;

    virtual void set_parent_function(llvm::Function* fn);
    EPtr get_declared_type(IdPtr id_ptr) const;

    void uninitialize();

    Context& context() { return ctx_; }

    Scope(const Scope&) = delete;
    Scope(Scope&&) = delete;
    virtual ~Scope() {}

  protected:
    Scope();

    std::map<std::string, IdPtr> ids_;
    std::vector<AST::Declaration*> ordered_decls_;

    Context ctx_;
    Scope* parent_;
    FnScope* containing_function_;

    llvm::IRBuilder<> bldr_;

  private:
    // To each IdPtr we associate a set holding IdPtrs for which it is needed
    static std::vector<DeclPtr> decl_registry_;
    friend void Dependency::traverse_from(Dependency::PtrWithTorV,
        std::map<AST::Expression*, Dependency::Flag>&);
};

class CondScope : public Scope {
  public:
    CondScope() :
      entry_block_(make_block("entry", nullptr)),
      exit_block_(make_block("exit", nullptr)) {}

    virtual ~CondScope() {}

    llvm::BasicBlock* entry_block() { return entry_block_; }
    llvm::BasicBlock* exit_block() { return exit_block_; }

  private:
    llvm::BasicBlock *entry_block_, *exit_block_;
};

class TypeScope : public Scope {
  public:
    TypeScope() :
      entry_block_(make_block("entry", nullptr)),
      exit_block_(make_block("exit", nullptr)) {}

    virtual ~TypeScope() {}

    llvm::BasicBlock* entry_block() { return entry_block_; }
    llvm::BasicBlock* exit_block() { return exit_block_; }
  private:
    llvm::BasicBlock *entry_block_, *exit_block_;
};

class FnScope : public Scope {
  public:
    virtual bool is_function_scope() const { return true; }
    void set_type(Function* fn_type) { fn_type_ = fn_type; }
    void add_scope(Scope* scope);
    void remove_scope(Scope* scope) { innards_.erase(scope); }

    virtual void make_return(llvm::Value* val);
    llvm::Value* return_value() const { return return_val_; }

    virtual llvm::BasicBlock* entry_block() { return entry_block_; }
    virtual llvm::BasicBlock* exit_block() { return exit_block_; }

    virtual void enter();
    virtual void exit();

    virtual void set_parent_function(llvm::Function* fn);

    FnScope(llvm::Function* fn = nullptr);

    virtual ~FnScope() {}
  private:
    std::set<Scope*> innards_;
    Function* fn_type_;
    llvm::Function* llvm_fn_;
    llvm::Value* return_val_;

    llvm::BasicBlock *entry_block_, *exit_block_;
    void allocate(Scope* scope);
};

class WhileScope : public Scope {
  public:
    virtual bool is_loop_scope() const { return true; }

    llvm::BasicBlock* cond_block() const { return cond_block_; }
    llvm::BasicBlock* landing_block() const { return land_block_; }

    virtual void set_parent_function(llvm::Function* fn);

    virtual llvm::BasicBlock* entry_block() { return entry_block_; }
    virtual llvm::BasicBlock* exit_block() { return exit_block_; }

    virtual void enter();
    virtual void exit();

    WhileScope() :
      entry_block_(make_block("while.entry", nullptr)),
      exit_block_(make_block("while.exit", nullptr)),
      cond_block_(make_block("while.cond", nullptr)),
      land_block_(make_block("while.land", nullptr)) {
    }

    virtual ~WhileScope() {}

  private:
    llvm::BasicBlock *entry_block_, *exit_block_, *cond_block_, *land_block_;
};

class GlobalScope : public Scope {
  public:
    virtual llvm::BasicBlock* entry_block() { return the_block_; }
    virtual llvm::BasicBlock* exit_block() { return the_block_; }

    void initialize();

    GlobalScope() : the_block_(make_block("global.block", nullptr)) {
      bldr_.SetInsertPoint(entry_block());
    }

    virtual ~GlobalScope() {}

  private:
    llvm::BasicBlock* the_block_;

};

#endif  // ICARUS_SCOPE_H
