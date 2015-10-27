#include "ScopeDB.h"

#include <iostream>
#include <stack>
#include <algorithm>

#include "AST.h"

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
      decl_ptr->declared_identifier()->alloca_ =
        temp_builder.CreateAlloca(
            decl_ptr->declared_identifier()->type()->llvm(),
            nullptr, decl_ptr->identifier_string());
    }
  }

  // TODO have a getter-only version for when we know we've passed the
  // verification step
  IdPtr Scope::identifier(const std::string& id_string) {
    auto iter = ids_.find(id_string);
    if (iter != ids_.end()) {
      return iter->second;
    }

    // No such identifier has been seen yet, create a new IdPtr and return it.
    IdPtr id_ptr = ids_[id_string] = IdPtr(new AST::Identifier(id_string));
    ids_[id_string] = id_ptr;

    scope_containing_[id_ptr] = this;

    return id_ptr;
  }

  void Scope::determine_declared_types() {
    for (auto scope_ptr : registry_) {
      for (auto decl_ptr : scope_ptr->ordered_decls_) {
        decl_ptr->declared_identifier()->expr_type_ =
          decl_ptr->declared_type()->interpret_as_type();
      }
    }
  }

  std::map<IdPtr, DeclPtr> decl_of_;
  std::map<IdPtr, std::set<IdPtr>> dependencies_;
  std::vector<DeclPtr> decl_registry_;
  std::map<IdPtr, Scope*> scope_containing_;

  std::vector<Scope*> Scope::registry_;

  void Scope::verify_no_shadowing() {
    for (auto scope_ptr : registry_) {
      if (scope_ptr->parent_ == nullptr) continue;

      Scope* check_against = scope_ptr;
      do {
        check_against = check_against->parent_;

        for (const auto id : scope_ptr->ids_) {
          auto found_iter = check_against->ids_.find(id.first);
          if (found_iter != check_against->ids_.end()) {
            std::cerr
              << "Identifier shadowed: `" << id.first << "`"
              << std::endl;
          }
        }
      } while (check_against->parent_ != nullptr);
    }
  }


  DeclPtr make_declaration() {
    DeclPtr d(new AST::Declaration);
    decl_registry_.push_back(d);
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
      dependencies_[decl_id] = std::set<IdPtr>();
    }

    // For each declared identifier, look through the identifiers which go into
    // it's type declaration. And add an IdPtr for this to each of there
    // dependencies_ sets

    for (const auto& decl_ptr : decl_registry_) {
      IdPtr decl_id = decl_ptr->declared_identifier();

      EPtr decl_type = decl_ptr->declared_type();

      if (decl_type->is_identifier()) {
        dependencies_[decl_id]
          .insert(std::static_pointer_cast<AST::Identifier>(decl_type));

      } else {
        decl_type->needed_for(decl_id);
      }
    }
  }

  void assign_decl_order() {
    // Counts the number of times a given IdPtr is an immediate dependency of
    // something else. So a value of zero means that nothing depends on it.
    std::map<IdPtr, size_t> num_immediate_dep_refs;

    // Char just used as a mask
    std::map<IdPtr, char> already_seen;

    std::stack<IdPtr> id_stack;

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
        id_stack.push(kv.first);
      }
    }

    // Count the number of idptrs seen. If at the end this isn't equal to the
    // total number, we know there's a cycle.
    //
    // TODO For better error messages we should write down what the cycle is.
    size_t num_seen = 0;


    // Preallocate a vector of the right size
    std::vector<IdPtr> topo_order(already_seen.size(), nullptr);

    while (!id_stack.empty()) {
      auto id_ptr = id_stack.top();
      if ((already_seen[id_ptr] & 2) == 2) {
        continue;
      }

      if ((already_seen[id_ptr] & 1) == 1) {
        id_stack.pop();

        topo_order[num_seen] = id_ptr;

        already_seen[id_ptr] = 0x03;
        ++num_seen;
        continue;
      }

      already_seen[id_ptr] = 0x01;
      for (const auto& dep : dependencies_[id_ptr]) {
        if ((already_seen[dep] & 2) == 2) continue;

        if ((already_seen[dep] & 1) == 1) {
          // TODO give information about cycle
          std::cerr << "Cyclic dependency found." << std::endl;
          return;
        }

        id_stack.push(dep);
      }

    }

    if (num_seen != already_seen.size()) {
      std::cerr << "A dependency cycle was found." << std::endl;
      return;
    }

    for (const auto& id_ptr : topo_order) {
      scope_containing_[id_ptr]->ordered_decls_
        .push_back(decl_of_[id_ptr]);
    }

  }

}  // namespace ScopeDB
