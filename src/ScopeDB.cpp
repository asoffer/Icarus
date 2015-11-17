#include "ScopeDB.h"

#include <iostream>
#include <stack>
#include <algorithm>

#include "AST.h"
#include "ErrorLog.h"

extern llvm::Module* global_module;
extern ErrorLog error_log;

namespace ScopeDB {
  Scope* Scope::build() {
    Scope* new_scope = new Scope;
    registry_.push_back(new_scope);

    return new_scope;
  }

  Scope* Scope::build_global() {
    auto scope_ptr = new Scope;
    for (auto& ptr : registry_) {
      ptr->parent_ = scope_ptr;
    }

    registry_.push_back(scope_ptr);
    return registry_.back();
  }

  size_t Scope::num_scopes() {
    return registry_.size();
  }

  void Scope::set_parent(Scope* parent) {
    parent_ = parent;
  }

  void Scope::allocate() {
    llvm::IRBuilder<> temp_builder(entry_block_, entry_block_->begin());

    for (const auto& decl_ptr : ordered_decls_) {
      auto decl_type = decl_ptr->declared_identifier()->type();

      // TODO for now functions are treated as constant, and don't need to be
      // declared in a scope.
      //
      // What happens if you try to reassign? This almost certainly leads to a
      // bug.
      if (decl_type->is_function()) {
        llvm::Function::Create(
            static_cast<llvm::FunctionType*>(decl_type->llvm()),
            llvm::Function::ExternalLinkage, decl_ptr->identifier_string(), global_module);

      } else {
        decl_ptr->declared_identifier()->alloca_ =
          temp_builder.CreateAlloca(
              decl_type->llvm(),
              nullptr, decl_ptr->identifier_string());
      }
    }
  }

  // TODO have a getter-only version for when we know we've passed the
  // verification step
  IdPtr Scope::identifier(const std::string& id_string) {
    Scope* current_scope = this;
    while (current_scope != nullptr) {
      auto iter = current_scope->ids_.find(id_string);
      if (iter != current_scope->ids_.end()) {
        return iter->second;
      }
      current_scope = current_scope->parent();
    }

    // No such identifier has been seen yet, create a new IdPtr and return it.
    IdPtr id_ptr = ids_[id_string] = IdPtr(new AST::Identifier(0, id_string));
    ids_[id_string] = id_ptr;

    scope_containing_[id_ptr] = this;

    return id_ptr;
  }

  void Scope::determine_declared_types() {
    for (auto scope_ptr : registry_) {
      for (auto decl_ptr : scope_ptr->ordered_decls_) {
        if (decl_ptr->infer_type_) {
          // TODO do inference correctly inside this scope in necessary.
          decl_ptr->declared_type()->verify_types();
          decl_ptr->declared_identifier()->expr_type_ =
            decl_ptr->declared_type()->expr_type_;

        } else {
          decl_ptr->declared_identifier()->expr_type_ =
            decl_ptr->declared_type()->interpret_as_type(); // If no type inference
        }
      }
    }
  }

  std::map<IdPtr, DeclPtr> decl_of_;
  std::map<EPtr, std::set<EPtr>> dependencies_;
  std::vector<DeclPtr> decl_registry_;
  std::map<IdPtr, Scope*> scope_containing_;

  std::vector<Scope*> Scope::registry_;

  void Scope::verify_no_shadowing() {
    for (auto decl_ptr1 : decl_registry_) {
      for (auto decl_ptr2 : decl_registry_) {
        if (decl_ptr1 == decl_ptr2) continue;
        if (decl_ptr1->identifier_string() != decl_ptr2->identifier_string()) continue;
        
        auto scope_ptr = decl_ptr1->scope_;
        // If the shadowing occurs in the same scope, we don't need to display
        // the error message twice.
        if (scope_ptr == decl_ptr2->scope_) {
          if (decl_ptr1->line_num() <= decl_ptr2->line_num()) {
            error_log.log(decl_ptr1->line_num(),
                "Identifier `" + decl_ptr1->identifier_string()
                + "` already declared in this scope (on line "
                + std::to_string(decl_ptr2->line_num()) + ").");
          }

          continue;
        }

        while (scope_ptr != nullptr) {
          if (scope_ptr == decl_ptr2->scope_) {
            error_log.log(decl_ptr1->line_num(),
                "Identifier `" + decl_ptr1->identifier_string() + "` shadows identifier declared on line " + std::to_string(decl_ptr2->line_num()) + ".");
            // Do NOT skip out here. It's possible to have many shadows and we
            // might as well catch them all.
          }
          scope_ptr = scope_ptr->parent_;
        }
      }
    }
  }


  DeclPtr make_declaration(size_t line_num, const std::string& id_string) {
    DeclPtr d(new AST::Declaration);
    decl_registry_.push_back(d);
    d->id_ = IdPtr(new AST::Identifier(line_num, id_string));
    d->line_num_ = line_num;

    return d;
  }


  void fill_db() {
    for (auto scope_ptr : Scope::registry_) {
      scope_ptr->ordered_decls_.clear();
    }

    for (const auto& decl_ptr : decl_registry_) {
      IdPtr decl_id = decl_ptr->declared_identifier();
      decl_of_[decl_id] = decl_ptr;

      // Build up dependencies_ starting with empty sets
      dependencies_[std::static_pointer_cast<AST::Expression>(decl_id)] = std::set<EPtr>();
    }

  }

  void assign_type_order() {
    // Counts the number of times a given IdPtr is an immediate dependency of
    // something else. So a value of zero means that nothing depends on it.
    std::map<EPtr, size_t> num_immediate_dep_refs;

    // Char just used as a mask
    std::map<EPtr, char> already_seen;

    std::stack<EPtr> expr_stack;

    // Push back all the sources
    for (const auto& kv : dependencies_) {
      // Ensure each identifier is present in the map
      num_immediate_dep_refs[kv.first];
      already_seen[kv.first] = 0x00;

      for (const auto& dep : kv.second) {
        num_immediate_dep_refs[dep]++;
      }
    }
    
    for (const auto& kv : num_immediate_dep_refs) {
      if (kv.second == 0) {
        expr_stack.push(kv.first);
      }
    }

    // Count the number of EPtrs seen. If at the end this isn't equal to the
    // total number, we know there's a cycle.
    //
    // TODO For better error messages we should write down what the cycle is.
    size_t num_seen = 0;


    // Preallocate a vector of the right size
    std::vector<EPtr> topo_order(already_seen.size(), nullptr);

    // 0x02 means already seen and already popped into topo_order
    // 0x01 means seen but not yet popped
    // 0x00 means not yet seen
    while (!expr_stack.empty()) {
      auto eptr = expr_stack.top();

      if ((already_seen[eptr] & 2) == 2) {
        // Already popped it into topo_order, so just ignore it
        expr_stack.pop();
        continue;
      }

      if ((already_seen[eptr] & 1) == 1) {
        // pop it off and put it in topo_order
        expr_stack.pop();
        topo_order[num_seen] = eptr;

        // mark it as already seen
        already_seen[eptr] = 0x03;
        ++num_seen;
        continue;
      }

      already_seen[eptr] = 0x01;
      for (const auto& dep : dependencies_[eptr]) {
        if ((already_seen[dep] & 2) == 2) continue;

        if ((already_seen[dep] & 1) == 1) {
          error_log.log(dep->line_num(), "Cyclic dependency found.");
          // TODO give information about cycle
          return;
        }

        expr_stack.push(dep);
      }
    }

    if (num_seen != already_seen.size()) {
      error_log.log(0, "A dependency cycle was found.");
      return;
    }

    for (const auto& eptr : topo_order) {
      eptr->verify_types();

      // If it's an identifier, push it into the declarations for the
      // appropriate scope, so they can be allocated correctly
      if (eptr->is_identifier()) {
        auto id_ptr = std::static_pointer_cast<AST::Identifier>(eptr);
        scope_containing_[id_ptr]->ordered_decls_
          .push_back(decl_of_[id_ptr]);
      }
    }
  }

}  // namespace ScopeDB
