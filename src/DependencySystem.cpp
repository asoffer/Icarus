// No need to include the corresponding header. Everything here is global and extern

#include <map>
#include <set>
#include <iostream>
#include <stack>

#include "typedefs.h"
#include "Scope.h"

namespace debug {
  extern bool dependency_system;
}  // namespace debug

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

namespace Dependency {
  std::map<AST::Expression*, std::set<AST::Expression*>> type_dependencies_ = {};
  std::map<AST::Expression*, std::set<AST::Expression*>> value_dependencies_ = {};

  void record(AST::Node* node) { node->record_dependencies(); }

  void type_value(AST::Expression* depender, AST::Expression* dependee) {
    value_dependencies_[depender].insert(dependee);
  }

  void type_type(AST::Expression* depender, AST::Expression* dependee) {
    type_dependencies_[depender].insert(dependee);
  }


  void add_to_table(AST::Expression* depender) {
    value_dependencies_[depender];
    type_dependencies_[depender];
  }

  void fill_db() {
    for (auto scope_ptr : Scope::registry_) {
      scope_ptr->ordered_decls_.clear();
    }

    for (const auto& decl_ptr : Scope::decl_registry_) {
      auto decl_id = decl_ptr->declared_identifier();
      if (debug::dependency_system) {
        std::cout << "Make decl_of_: " << decl_id->token() << " (line " << decl_ptr->line_num() << ")" << std::endl;
        std::cout << "\t" << decl_id << " -> " << decl_ptr << std::endl;
        std::cout << "\tRHS: " << decl_ptr->declared_type() << std::endl;
      }
      Scope::decl_of_[decl_id] = decl_ptr;

      // Build up dependencies_ starting with empty sets
      type_dependencies_[decl_id.get()] = {};
    }
  }

  void assign_type_order() {
    // Counts the number of times a given IdPtr is an immediate dependency of
    // something else. So a value of zero means that nothing depends on it.
    std::map<AST::Expression*, size_t> num_immediate_dep_refs;

    // Char just used as a mask
    std::map<const AST::Expression*, char> already_seen;

    std::stack<AST::Expression*> expr_stack;

    // Push back all the sources (i.e., all the ptrs with num_immediate_dep_refs
    // equal to zero
    for (const auto& kv : type_dependencies_) {
      // Ensure each identifier is present in the map
      num_immediate_dep_refs[kv.first];
      already_seen[kv.first] = 0x00;

      for (const auto& dep : kv.second) {
        num_immediate_dep_refs[dep]++;
      }
    }
    // num_immediate_dep_refs[foo] counts the number of things which directly
    // need foo's type in order to determine their own.

    // Start the stack with the sources of the dependency graph.
    // That is, those expressions which no one depends on.
    for (const auto& kv : num_immediate_dep_refs) {
      if (kv.second == 0) {
        expr_stack.push(kv.first);

        if (debug::dependency_system) {
          if (kv.first == nullptr) {
            std::cout << "Found a null pointer!" << std::endl;
          }
        }
      }
    }

    // Count the number of Expression* seen. If at the end this isn't equal to the
    // total number, we know there's a cycle.
    //
    // TODO For better error messages we should write down what the cycle is.
    size_t num_seen = 0;


    // Preallocate a vector of the right size
    std::vector<AST::Expression*> topo_order(already_seen.size(), nullptr);

    if (debug::dependency_system) {
      std::cout << "Expressions seen: " << already_seen.size() << std::endl;
    }

    // 0x02 means already seen and already popped into topo_order
    // 0x01 means seen but not yet popped
    // 0x00 means not yet seen

    // Standard depth-first search
    while (!expr_stack.empty()) {
      auto eptr = expr_stack.top();

      if (debug::dependency_system) {
        if (eptr == nullptr) {
          std::cerr << "Found a null pointer!" << std::endl;
          assert(false);

        } else {
          std::cout << "Looking at:   " << eptr << "\n" << *eptr << std::endl;
        }
      }


      if ((already_seen AT(eptr) & 2) == 2) {
        if (debug::dependency_system) {
          std::cout << "Already done: "  << eptr << "\n" << *eptr << std::endl;
        }
        // Already popped it into topo_order, so just ignore it
        expr_stack.pop();
        continue;

      }

      if ((already_seen AT(eptr) & 1) == 1) {
        // pop it off and put it in topo_order
        if (debug::dependency_system) {
          std::cout << "Adding:       " << eptr << "\n"  << *eptr << std::endl;
        }

        expr_stack.pop();
        topo_order[num_seen] = eptr;

        // mark it as already seen
        already_seen AT(eptr) = 0x03;
        ++num_seen;
        continue;
      }


      already_seen[eptr] = 0x01;

      for (const auto& dep : type_dependencies_[eptr]) {
        if (debug::dependency_system) {
          std::cout << "Found deps:   " << type_dependencies_[eptr].size() << std::endl;

          assert(dep && 
              ("Looking at a null dependency from " + eptr->to_string(0)
               + " seen on line " + std::to_string(eptr->line_num()) + "\n").c_str());

          assert(already_seen.find(dep) != already_seen.end() &&
              ("Dependency has not been seen yet: "
               + dep->to_string(0) + "\nCOMING FROM "
               + eptr->to_string(0) + "\n").c_str());
        }

        if ((already_seen AT(dep) & 2) == 2) {
          if (debug::dependency_system) {
            std::cout << "Skipping:     " << eptr << "\n"  << *eptr << std::endl;
          }
          continue;
        }

        if ((already_seen AT(dep) & 1) == 1) {
          error_log.log(dep->line_num(), "Cyclic dependency found.");
          // TODO give information about cycle
          return;
        }

        if (debug::dependency_system) {
          std::cout << "Pushing dep:  " << eptr << "\n"  << *dep << std::endl;
        }
        expr_stack.push(dep);
      }
    }

    if (num_seen != already_seen.size()) {
      error_log.log(0, "A dependency cycle was found.");
      return;
    }

    for (const auto& ptr : topo_order) {
      ptr->verify_types();

      // If it's an identifier, push it into the declarations for the
      // appropriate scope, so they can be allocated correctly
      if (ptr->is_identifier()) {
        auto id_ptr = static_cast<AST::Identifier*>(ptr)->shared_from_this();
        Scope::scope_containing_[id_ptr]->ordered_decls_.push_back(Scope::decl_of_[id_ptr]);
      }
    }
  }

}  // namespace Dep
