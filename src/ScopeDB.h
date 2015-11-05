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

namespace AST {
  class FunctionLiteral;
}

namespace ScopeDB {
  class Scope {
    public:
      friend void fill_db();
      friend void assign_decl_order();
      friend class AST::FunctionLiteral;

      static void verify_no_shadowing();
      static void determine_declared_types();

      static Scope* build();
      static Scope* build_global();
      static size_t num_scopes();

      void set_parent(Scope* parent);
      Scope* parent() { return parent_; }
      IdPtr identifier(const std::string& id_string);
      void allocate();

      llvm::BasicBlock* entry() {
        return entry_block_;
      }

      void set_entry(llvm::BasicBlock* bb) {
        entry_block_ = bb;
      }

    private:
      // TODO Do I need this constructor?
      Scope() : parent_(nullptr), entry_block_(nullptr) {}

      Scope(const Scope&) = delete;
      Scope(Scope&&) = delete;

      Scope* parent_;
      llvm::BasicBlock* entry_block_;

      std::map<std::string, IdPtr> ids_;
      std::vector<DeclPtr> ordered_decls_;

      // Important invariant: A pointer only ever points to scopes held in
      // higehr indices. The global (root) scope must be the last scope.
      static std::vector<Scope*> registry_;
  };

  // To each IdPtr we associate a set holding IdPtrs for which it is needed
  extern std::map<IdPtr, std::set<IdPtr>> dependencies_;
  extern std::map<IdPtr, Scope*> scope_containing_;
  extern std::map<IdPtr, DeclPtr> decl_of_;
  extern std::vector<DeclPtr> decl_registry_;

  extern DeclPtr make_declaration(size_t line_num, const std::string& id_string);

  extern void fill_db();
  extern void assign_decl_order();

}  // namespace ScopeDB

#endif  // ICARUS_SCOPE_DB_H
