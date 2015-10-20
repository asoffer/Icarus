#include "ScopeDB.h"

#include <iostream>
#include <stack>

#include "AST.h"

namespace ScopeDB {

  std::vector<int> parent_;
  std::map<IdPtr, DeclPtr> decl_of_;
  std::map<IdPtr, std::set<IdPtr>> needs_;
  std::vector<std::map<std::string, IdPtr>> ids_in_scope_;
  std::vector<DeclPtr> decl_registry_;
  std::vector<std::vector<DeclPtr>> decl_order_in_scope_;
  std::map<IdPtr, size_t> scope_containing_;

  size_t add_scope() {
    parent_.push_back(-1);
    ids_in_scope_.push_back({});
    return parent_.size() - 1;
  }

  size_t num_scopes() {
    return parent_.size();
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

    IdPtr id_ptr = ids_in_scope_[scope_id][id_string] =
      IdPtr(new AST::Identifier(id_string));
    ids_in_scope_[scope_id][id_string] = id_ptr;

    scope_containing_[id_ptr] = scope_id;

    return id_ptr;
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
    size_t num_of_scopes = num_scopes();
    for (size_t i = 0; i < num_of_scopes; ++i) {
      size_t num_decls = decl_order_in_scope_[i].size();
      for (size_t j = 0; j < num_decls; ++j) {
        decl_order_in_scope_[i][j]->declared_identifier()->expr_type_ =
          decl_order_in_scope_[i][j]->declared_type()->interpret_as_type();

        std::cout << decl_order_in_scope_[i][j]->identifier_string() << std::endl;
      }
      std::cout << "-----" << std::endl;
    }
  }

  DeclPtr make_declaration() {
    DeclPtr d(new AST::Declaration);
    decl_registry_.push_back(d);
    return d;
  }

  void fill_db() {
    decl_order_in_scope_.clear();
    decl_order_in_scope_.resize(num_scopes());

    for (const auto& decl_ptr : decl_registry_) {
      IdPtr decl_id = decl_ptr->declared_identifier();
      decl_of_[decl_id] = decl_ptr;

      // Build up needs_ starting with empty sets
      needs_[decl_id] = std::set<IdPtr>();
    }

    // For each declared identifier, look through the identifiers which go into
    // it's type declaration. And add an IdPtr for this to each of there
    // needs_ sets

    for (const auto& decl_ptr : decl_registry_) {
      IdPtr decl_id = decl_ptr->declared_identifier();

      EPtr decl_type = decl_ptr->declared_type();

      if (decl_type->is_identifier()) {
        needs_[decl_id]
          .insert(std::static_pointer_cast<AST::Identifier>(decl_type));

      } else {
        decl_type->needed_for(decl_id);
      }
    }
  }

  void assign_decl_order() {
    std::map<IdPtr, size_t> num_immediate_dep_refs;

    // Char just used as a mask
    std::map<IdPtr, char> already_seen;


    std::stack<IdPtr> id_stack;

    // Push back all the sources
    for (const auto& kv : needs_) {
      // Ensure each identifier is present
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
      for (const auto& dep : needs_[id_ptr]) {
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
      decl_order_in_scope_[scope_containing_[id_ptr]]
        .push_back(decl_of_[id_ptr]);
    }

  }

}  // namespace ScopeDB
