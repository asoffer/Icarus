#ifndef ICARUS_SCOPE_H
#define ICARUS_SCOPE_H

#include <vector>
#include <map>
#include <set>
#include <string>

#include "typedefs.h"
#include "Type.h"

// TODO Figure out what you need from this.
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include <iostream>

class Type;

namespace AST {
  class FunctionLiteral;
}

enum class ScopeType { loop, cond, func, type };

class Scope {
  public:
    friend void fill_db();
    friend void assign_type_order();
    friend class AST::FunctionLiteral;
    friend class AST::Declaration;

    static void verify_no_shadowing();
    static void determine_declared_types();

    static Scope* build(ScopeType st);
    static Scope* build_global();
    static size_t num_scopes();


    // Important invariant: A pointer only ever points to scopes held in
    // higehr indices. The global (root) scope must be the last scope.
    static std::vector<Scope*> registry_;

    // To each IdPtr we associate a set holding IdPtrs for which it is needed
    static std::map<EPtr, std::set<EPtr>> dependencies_;
    static std::map<IdPtr, Scope*> scope_containing_;
    static std::map<IdPtr, DeclPtr> decl_of_;
    static std::vector<DeclPtr> decl_registry_;

    static DeclPtr make_declaration(size_t line_num, const std::string& id_string);

    static void fill_db();
    static void assign_type_order();


    static DeclPtr get_declaration(IdPtr id) { return decl_of_[id]; }


    llvm::IRBuilder<>& builder() { return bldr_; }

    void set_parent(Scope* parent);
    void set_return_type(Type* ret_type) { return_type_ = ret_type; }
    void enter();
    void exit(llvm::BasicBlock* jump_to = nullptr);
    Scope* parent() const { return parent_; }
    llvm::Value* return_value() const { return return_val_; }

    // While we're actually returning an IdPtr, it's only ever used as an
    // EPtr, so we do the pointer cast inside.
    EPtr identifier(EPtr id_as_eptr);


    llvm::BasicBlock* alloc_block() const { return alloc_block_; }
    llvm::BasicBlock* entry_block() const { return entry_block_; }
    llvm::BasicBlock* exit_block() const { return exit_block_; }

    void set_parent_function(llvm::Function* fn);
    void make_return_void();
    void make_return(llvm::Value* val);

    EPtr get_declared_type(IdPtr id_ptr) const;

    void allocate();

    Scope(const Scope&) = delete;
    Scope(Scope&&) = delete;

  private:
    Scope(ScopeType st) :
      parent_(nullptr),
      scope_type_(st),
      entry_block_(llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry")),
      exit_block_(llvm::BasicBlock::Create(llvm::getGlobalContext(), "exit")),
      alloc_block_(scope_type_ == ScopeType::loop
          ? llvm::BasicBlock::Create(llvm::getGlobalContext(), "allocs")
          : entry_block_),
      bldr_(llvm::getGlobalContext())
      {
        bldr_.SetInsertPoint(alloc_block_);

        if (scope_type_ != ScopeType::func) {
          set_return_type(nullptr);
        }
      }

    std::map<std::string, IdPtr> ids_;
    std::vector<DeclPtr> ordered_decls_;

    Scope* parent_;
    ScopeType scope_type_;
    llvm::BasicBlock* entry_block_;
    llvm::BasicBlock* exit_block_;
    llvm::BasicBlock* alloc_block_;

    Type* return_type_;
    llvm::Value* return_val_;


    llvm::IRBuilder<> bldr_;


};

#endif  // ICARUS_SCOPE_H
