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
class FnScope;

namespace AST {
  class FunctionLiteral;
}


class Scope {
  public:
    friend class AST::FunctionLiteral;
    friend class AST::Declaration;
    friend class FnScope;

    // Build a scope of the appropriate type
    template<typename T>
      static typename std::enable_if<std::is_base_of<Scope, T>::value, T*>::type build();

    static std::map<EPtr, std::set<EPtr>> dependencies_;
    static std::map<IdPtr, DeclPtr> decl_of_;

    static void verify_no_shadowing();
    static void determine_declared_types();

    static FnScope* build_global();
    static size_t num_scopes();

    static DeclPtr make_declaration(size_t line_num, const std::string& id_string);

    static void fill_db();
    static void assign_type_order();


    static DeclPtr get_declaration(IdPtr id) { return decl_of_[id]; }


    llvm::IRBuilder<>& builder() { return bldr_; }

    void set_parent(Scope* parent);
    Scope* parent() const { return parent_; }

    virtual bool is_function_scope() const { return false; }
    virtual bool is_loop_scope() const { return false; }

    virtual void enter();
    virtual void exit(llvm::BasicBlock* jump_to = nullptr);

    // While we're actually returning an IdPtr, it's only ever used as an
    // EPtr, so we do the pointer cast inside.
    EPtr identifier(EPtr id_as_eptr);

    llvm::BasicBlock* entry_block() const { return entry_block_; }
    llvm::BasicBlock* exit_block() const { return exit_block_; }

    virtual void set_parent_function(llvm::Function* fn);
    EPtr get_declared_type(IdPtr id_ptr) const;

    void uninitialize();

    virtual void make_return_void();
    virtual void make_return(llvm::Value* val);

    Scope(const Scope&) = delete;
    Scope(Scope&&) = delete;
    virtual ~Scope() {}

  protected:
    Scope() :
      parent_(nullptr),
      containing_function_(nullptr),
      entry_block_(llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry")),
      exit_block_(llvm::BasicBlock::Create(llvm::getGlobalContext(), "exit")),
      bldr_(llvm::getGlobalContext()) {}

    std::map<std::string, IdPtr> ids_;
    std::vector<DeclPtr> ordered_decls_;

    Scope* parent_;
    FnScope* containing_function_;
    llvm::BasicBlock* entry_block_;
    llvm::BasicBlock* exit_block_;

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
  T* new_scope = new T();
  registry_.push_back(new_scope);
  return new_scope;
}


class FnScope : public Scope {
  public:
    friend class Scope;

    virtual bool is_function_scope() const { return true; }

    virtual void enter();
    virtual void exit(llvm::BasicBlock* jump_to = nullptr);

    virtual void make_return_void();
    virtual void make_return(llvm::Value* val);

    void set_return_type(Type* ret_type) { return_type_ = ret_type; }
    llvm::Value* return_value() const { return return_val_; }

    FnScope() : return_type_(Type::get_void()), return_val_(nullptr) {
      containing_function_ = this;
    }

    virtual ~FnScope() {}

  private:
    std::set<Scope*> innards_;
    Type* return_type_;
    llvm::Value* return_val_;

    void allocate(Scope* scope);
};

class WhileScope : public Scope {
  public:
    friend class Scope;

    virtual bool is_loop_scope() const { return true; }
    llvm::BasicBlock* cond_block() const { return cond_block_; }
    llvm::BasicBlock* landing_block() const { return land_block_; }

    virtual void enter();
    virtual void exit(llvm::BasicBlock* jump_to = nullptr);

    WhileScope() :
      cond_block_(llvm::BasicBlock::Create(llvm::getGlobalContext(), "while_cond")),
      land_block_(llvm::BasicBlock::Create(llvm::getGlobalContext(), "while_land")) {
    }

    virtual void set_parent_function(llvm::Function* fn);

    virtual ~WhileScope() {}
  private:
    llvm::BasicBlock* cond_block_;
    llvm::BasicBlock* land_block_;
};

class TypeScope : public Scope {
  public:
    TypeScope() {}
};

class CondScope : public Scope {
  public:
    CondScope() {}
};
#endif  // ICARUS_SCOPE_H
