#ifndef ICARUS_SCOPE_DB_H
#define ICARUS_SCOPE_DB_H

#include <vector>
#include <map>
#include <set>
#include <string>

#include "typedefs.h"

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

namespace ScopeDB {
  class Scope {
    public:
      friend void fill_db();
      friend void assign_type_order();
      friend class AST::FunctionLiteral;
      friend class AST::Declaration;

      static void verify_no_shadowing();
      static void determine_declared_types();

      static Scope* build();
      static Scope* build_global();
      static size_t num_scopes();

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

      llvm::BasicBlock* entry_block() const { return entry_block_; }
      llvm::BasicBlock* exit_block() const { return exit_block_; }

      void set_parent_function(llvm::Function* fn) {
        if (entry_block_ != nullptr && entry_block_->getParent() != nullptr) {
          entry_block_->removeFromParent();
        }

        if (exit_block_ != nullptr && exit_block_->getParent() != nullptr) {
          exit_block_->removeFromParent();
        }

        entry_block_->insertInto(fn);
        exit_block_->insertInto(fn);
      }

      void make_loop() {
        is_loop_ = true;
        set_return_type(nullptr);
      }

      void make_return_void();
      void make_return(llvm::Value* val);

      EPtr get_declared_type(IdPtr id_ptr) const;

      void allocate(llvm::IRBuilder<>& alloc_builder);

      Scope(const Scope&) = delete;
      Scope(Scope&&) = delete;

    private:
      Scope() :
        parent_(nullptr),
        entry_block_(llvm::BasicBlock::Create(
              llvm::getGlobalContext(), "entry")),
        exit_block_(llvm::BasicBlock::Create(
              llvm::getGlobalContext(), "exit")),
        bldr_(llvm::getGlobalContext()),
        is_loop_(false) {

          bldr_.SetInsertPoint(entry_block());
        }

      std::map<std::string, IdPtr> ids_;
      std::vector<DeclPtr> ordered_decls_;

      Scope* parent_;
      llvm::BasicBlock* entry_block_;
      llvm::BasicBlock* exit_block_;

      Type* return_type_;
      llvm::Value* return_val_;

      llvm::IRBuilder<> bldr_;

      bool is_loop_;

      // Important invariant: A pointer only ever points to scopes held in
      // higehr indices. The global (root) scope must be the last scope.
      static std::vector<Scope*> registry_;
  };

  // To each IdPtr we associate a set holding IdPtrs for which it is needed
  extern std::map<EPtr, std::set<EPtr>> dependencies_;
  extern std::map<IdPtr, Scope*> scope_containing_;
  extern std::map<IdPtr, DeclPtr> decl_of_;
  extern std::vector<DeclPtr> decl_registry_;

  extern DeclPtr make_declaration(size_t line_num, const std::string& id_string);

  extern void fill_db();
  extern void assign_type_order();

}  // namespace ScopeDB

#endif  // ICARUS_SCOPE_DB_H
