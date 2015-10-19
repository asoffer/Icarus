#include "ScopeDB.h"

#include <iostream>
#include "AST.h"

namespace ScopeDB {

  std::vector<int> parent_;
  std::map<IdPtr, DeclPtr> decl_of_;
  std::map<IdPtr, std::set<IdPtr>> needed_for_;
  std::vector<std::map<std::string, IdPtr>> ids_in_scope_;
  std::vector<DeclPtr> decl_registry_;

  size_t add_scope() {
    parent_.push_back(-1);
    ids_in_scope_.push_back({});
    return parent_.size() - 1;
  }

  void set_parent(size_t child_id, size_t parent_id) {
    // TODO Is it possible to have so many scopes that this cast becomes
    // problematic?
    parent_[child_id] = static_cast<int>(parent_id);
  }

  size_t add_global() {
    // TODO Is it possible to have so many scopes that this cast becomes
    // problematic?
    size_t parent_size = parent_.size();
    for (size_t i = 0; i < parent_size; ++i) {
      if (parent_[i] == -1) {
        parent_[i] = static_cast<int>(parent_size);
      }
    }
    return add_scope();
  }

  IdPtr identifier(size_t scope_id, std::string id_string) {
    auto iter = ids_in_scope_[scope_id].find(id_string);
    if (iter != ids_in_scope_[scope_id].end()) {
      return ids_in_scope_[scope_id][id_string];
    }

    // No such identifier has been seen yet, create a new IdPtr and return it.
    return ids_in_scope_[scope_id][id_string] = IdPtr(new AST::Identifier(id_string));
  }

  void verify_no_shadowing() {
    for (size_t i = 0; i < parent_.size(); ++i) {
      if (parent_[i] == -1) continue;

      // If you get here, parent_[i] is non-negative, so this cast is safe.
      size_t check_against = static_cast<size_t>(i);
      do {
        // If you get here you either just entered the loop, or you came from
        // the while condition. In both cases we have checked that
        // parent_[check_against] != -1.
        check_against = static_cast<size_t>(parent_[check_against]);

        for (const auto id : ids_in_scope_[i]) {
          auto found_iter = ids_in_scope_[check_against].find(id.first);
          if (found_iter != ids_in_scope_[check_against].end()) {
            std::cerr
              << "Identifier shadowed: `" << id.first << "`"
              << std::endl;
          }
        }
      } while(parent_[check_against] != -1);
    }
  }

  void determine_declared_types() {
    for (const auto& foo : decl_registry_) {
      std::cout << foo->identifier_string() << std::endl;
    }
  }

  DeclPtr make_declaration() {
    DeclPtr d(new AST::Declaration);
    decl_registry_.push_back(d);
    return d;
  }

  void fill_db() {
    for (const auto& decl_ptr : decl_registry_) {
      IdPtr decl_id = decl_ptr->declared_identifier();
      decl_of_[decl_id] = decl_ptr;

      // Build up needed_for_ with empty sets
      needed_for_[decl_id] = std::set<IdPtr>();
    }

    // For each declared identifier, look through the identifiers which go into
    // it's type declaration. And add an IdPtr for this to each of there
    // needed_for_ sets
    for (const auto& decl_ptr : decl_registry_) {
      IdPtr decl_id = decl_ptr->declared_identifier();
      EPtr decl_type = decl_ptr->declared_type();

      if (decl_type->is_identifier()) {
        needed_for_[std::static_pointer_cast<AST::Identifier>(decl_type)]
          .insert(decl_id);
      } else {
        decl_type->needed_for(decl_id);
      }
    }
  }

}  // namespace ScopeDB
