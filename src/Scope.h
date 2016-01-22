#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <vector>
#include <map>
#include <set>
#include <string>
#include <type_traits>

#include "typedefs.h"
#include "Type.h"

#include <iostream>

class Type;
class Function;

class GlobalScope;
class GenericFnScope;
class FnScope;

namespace AST {
  class FunctionLiteral;
}

extern llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn);

class Scope {
  public:
    friend class AST::FunctionLiteral;
    friend class AST::Declaration;
    friend class GenericFnScope;

    // Build a scope of the appropriate type
    template<typename T>
      static typename std::enable_if<std::is_base_of<Scope, T>::value, T*>::type build();

    template<typename T>
      static typename std::enable_if<std::is_base_of<FnScope, T>::value, T*>::type build_fn();


    static std::map<EPtr, std::set<EPtr>> dependencies_;
    static std::map<IdPtr, DeclPtr> decl_of_;

    static void verify_no_shadowing();
    static void determine_declared_types();

    static GlobalScope* build_global();
    static size_t num_scopes();

    static DeclPtr make_declaration(size_t line_num, const std::string& id_string);

    static void fill_db();
    static void assign_type_order();


    static DeclPtr get_declaration(IdPtr id) { return decl_of_[id]; }

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

    virtual void set_parent_function(llvm::Function* fn);
    EPtr get_declared_type(IdPtr id_ptr) const;

    void uninitialize();

    Scope(const Scope&) = delete;
    Scope(Scope&&) = delete;
    virtual ~Scope() {}

  protected:
    Scope() :
      parent_(nullptr),
      containing_function_(nullptr),
      bldr_(llvm::getGlobalContext()) {}

    std::map<std::string, IdPtr> ids_;
    std::vector<DeclPtr> ordered_decls_;

    Scope* parent_;
    GenericFnScope* containing_function_;

    llvm::IRBuilder<> bldr_;

  private:
    // Important invariant: A pointer only ever points to scopes held in
    // higehr indices. The global (root) scope must be the last scope.
    static std::vector<Scope*> registry_;

    // To each IdPtr we associate a set holding IdPtrs for which it is needed
    static std::map<IdPtr, Scope*> scope_containing_;
    static std::vector<DeclPtr> decl_registry_;
};


template<typename T>
typename std::enable_if<std::is_base_of<Scope, T>::value, T*>::type Scope::build() {
  T* new_scope = new T;
  registry_.push_back(new_scope);
  return new_scope;
}

// TODO limit to 
template<typename T>
typename std::enable_if<std::is_base_of<FnScope, T>::value, T*>::type Scope::build_fn() {
  T* new_scope = new T(nullptr);
  registry_.push_back(new_scope);
  return new_scope;
}

template<typename T> class StandardEntryExit {
  public:
    StandardEntryExit() :
      entry_block_(make_block("entry", nullptr)),
      exit_block_(make_block("exit", nullptr)) {}

    virtual ~StandardEntryExit() {}

  protected:
    llvm::BasicBlock* entry_block_;
    llvm::BasicBlock* exit_block_;
};

class CondScope : public Scope, public StandardEntryExit<CondScope> {
  public:
    CondScope() {}

    llvm::BasicBlock* entry_block() { return entry_block_; }
    llvm::BasicBlock* exit_block() { return exit_block_; }
};

class TypeScope : public Scope, public StandardEntryExit<TypeScope> {
  public:
    TypeScope() {}

    llvm::BasicBlock* entry_block() { return entry_block_; }
    llvm::BasicBlock* exit_block() { return exit_block_; }
};

class GenericFnScope : public Scope {
  public:
    virtual llvm::BasicBlock* entry_block() = 0;
    virtual llvm::BasicBlock* exit_block() = 0;

    virtual void enter() = 0;
    virtual void exit() = 0;

    virtual bool is_function_scope() const { return true; }
    void set_type(Function* fn_type) { fn_type_ = fn_type; }
    void add_scope(Scope* scope);
    void remove_scope(Scope* scope) { innards_.erase(scope); }

    virtual void make_return(llvm::Value* val);
    llvm::Value* return_value() const { return return_val_; }

    GenericFnScope(llvm::Function* fn) :
      fn_type_(nullptr),
      llvm_fn_(fn),
      return_val_(nullptr) {}

  virtual ~GenericFnScope() {}

  protected:
    // The inner scopes which contain identifiers that might need to be declared
    std::set<Scope*> innards_;
    Function* fn_type_;
    llvm::Function* llvm_fn_;
    llvm::Value* return_val_;

    void allocate(Scope* scope);
};

class SimpleFnScope : public GenericFnScope {
  public:
    virtual llvm::BasicBlock* entry_block() { return the_block_; }
    virtual llvm::BasicBlock* exit_block() { return the_block_; }

    virtual void enter();
    virtual void exit();

    SimpleFnScope(llvm::Function* fn) :
      GenericFnScope(fn), the_block_(make_block("block", nullptr)) {}

    virtual ~SimpleFnScope() {}

  private:
    llvm::BasicBlock* the_block_;
};


class FnScope : public GenericFnScope, public StandardEntryExit<FnScope> {
  public:
    virtual llvm::BasicBlock* entry_block() { return entry_block_; }
    virtual llvm::BasicBlock* exit_block() { return exit_block_; }

    virtual void enter();
    virtual void exit();

    virtual void set_parent_function(llvm::Function* fn);

    FnScope(llvm::Function* fn) : GenericFnScope(fn) {}

    virtual ~FnScope() {}

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
    llvm::BasicBlock* entry_block_;
    llvm::BasicBlock* exit_block_;
    llvm::BasicBlock* cond_block_;
    llvm::BasicBlock* land_block_;

};

class GlobalScope : public Scope {
  public:
    virtual llvm::BasicBlock* entry_block() { return the_block_; }
    virtual llvm::BasicBlock* exit_block() { return the_block_; }

    void initialize();

    GlobalScope() :
      the_block_(make_block("global.block", nullptr)) {}

    virtual ~GlobalScope() {}

  private:
    llvm::BasicBlock* the_block_;

};

//class NamespaceScope : public Scope {
//};
//
//class TypeScope : public Scope {
//};
//
//
//class AnonScope : public Scope {
//};


#endif  // ICARUS_SCOPE_H
