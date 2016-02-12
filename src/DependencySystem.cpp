// No need to include the corresponding header. Everything here is global and extern

#include <map>
#include <set>
#include <iostream>
#include <vector>

#include "typedefs.h"
#include "DependencyTypes.h"
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
  using DepMap = std::map<PtrWithTorV, std::set<PtrWithTorV>>;
  static DepMap dependencies_ = {};

  void record(AST::Node* node) { node->record_dependencies(); }

  void type_type(AST::Expression* depender, AST::Expression* dependee) {
    dependencies_[PtrWithTorV(depender, true)].emplace(dependee, true);
    dependencies_[PtrWithTorV(depender, false)];
  }

  void type_value(AST::Expression* depender, AST::Expression* dependee) {
    dependencies_[PtrWithTorV(depender, true)].emplace(dependee, false);
    dependencies_[PtrWithTorV(depender, false)];
  }

  void value_type(AST::Expression* depender, AST::Expression* dependee) {
    dependencies_[PtrWithTorV(depender, true)];
    dependencies_[PtrWithTorV(depender, false)].emplace(dependee, true);
  }

  void value_value(AST::Expression* depender, AST::Expression* dependee) {
    dependencies_[PtrWithTorV(depender, true)];
    dependencies_[PtrWithTorV(depender, false)].emplace(dependee, false);
  }

  void add_to_table(AST::Expression* depender) {
    dependencies_[PtrWithTorV(depender, true)];
    dependencies_[PtrWithTorV(depender, false)];
  }

  void traverse_from(PtrWithTorV pt, std::map<AST::Expression*, Flag>& already_seen) {
    std::vector<AST::Expression*> expr_stack;
    std::vector<bool> torv_stack;
    expr_stack.push_back(pt.ptr_);
    torv_stack.push_back(pt.torv_);

    while (!expr_stack.empty()) {
      if (debug::dependency_system) {
        std::cin.ignore(1);
        std::cout << "\033[2J\033[1;1H" << std::endl;
      }
      // Ensure stacks match up
      assert(expr_stack.size() == torv_stack.size() && "Stacks sizes don't match!");

      auto ptr = expr_stack.back();
      auto torv = torv_stack.back();

      if (debug::dependency_system) {
        std::cout
          << "+------------------------------------------------" << std::endl;

        for (size_t i = 0; i < expr_stack.size(); ++i) {
          std::cout << "| " << i << ". " << expr_stack[i] << (torv_stack[i] ? " (type)" : " (value)") << std::endl;
        }

        std::cout << *ptr << std::endl;
        std::cout << "Seen flag (on entry): " << already_seen AT(ptr) << std::endl;
      }
 
      Flag done_flag = (torv ? type_done : val_done);
      if ((already_seen AT(ptr) & done_flag) != 0) {
        if (debug::dependency_system) {
          std::cout << "| Already done. Popping." << std::endl;
        }
        expr_stack.pop_back();
        torv_stack.pop_back();
        continue;
      }

      Flag seen_flag = (torv ? type_seen : val_seen);
      if ((already_seen AT(ptr) & seen_flag) != 0) {
        expr_stack.pop_back();
        torv_stack.pop_back();

        if (torv) {
          if (ptr->is_unop()) {
            auto unop = static_cast<AST::Unop*>(ptr);
            if (unop->op() == Language::Operator::At) {
              auto t = unop->operand()->type();
              if (t->is_pointer()) {
                t = static_cast<Pointer*>(t)->pointee_type();
                if (t->is_struct()) {
                  auto struct_type = static_cast<Structure*>(t);
                  PtrWithTorV ptr_with_torv(
                      struct_type->defining_expression(), false);
                  traverse_from(ptr_with_torv, already_seen);
                }
              }
            }
          } else if (ptr->is_binop()) {
            auto binop = static_cast<AST::Binop*>(ptr);
            if (binop->op() == Language::Operator::Access) {
              if (binop->lhs()->type()->is_pointer()) {
                auto t = binop->lhs()->type();
                while (t->is_pointer()) t = static_cast<Pointer*>(t)->pointee_type();
                if (t->is_struct()) {
                  auto struct_type = static_cast<Structure*>(t);
                  PtrWithTorV ptr_with_torv(
                      struct_type->defining_expression(), false);
                  traverse_from(ptr_with_torv, already_seen);
                }
              }
            }
          }

          ptr->verify_types();
        }
        // If it's an identifier, push it into the declarations for the
        // appropriate scope, so they can be allocated correctly
        if (torv && ptr->is_identifier()) {
          auto id_ptr = static_cast<AST::Identifier*>(ptr)->shared_from_this();
          id_ptr->decl_->scope_->ordered_decls_.push_back(id_ptr->decl_);
        }

        if (!torv && ptr->is_type_literal()) {
          auto struct_ptr = static_cast<AST::TypeLiteral*>(ptr);
          struct_ptr->build_llvm_internals();
        }
 
        already_seen AT(ptr) = static_cast<Flag>((seen_flag << 1) | already_seen AT(ptr)); // Mark it as done

        if (debug::dependency_system) {
          std::cout << "Seen flag (now):      " << already_seen AT(ptr) << std::endl;
          std::cout << "| Already seen. Verifying." << std::endl;
        }
        continue;
      }

      // If you get here, this node is totally new.

      // Mark it as seen
      already_seen AT(ptr) = static_cast<Flag>(seen_flag | already_seen AT(ptr));
      if (debug::dependency_system) {
        std::cout << "Seen flag (later):    " << already_seen AT(ptr) << std::endl;
        std::cout << "| Marking as seen." << seen_flag << std::endl;
        std::cout << "| Has "
          << dependencies_ AT( PtrWithTorV(ptr, torv)).size()
          << " dependencies." << std::endl;
      }

      // And follow it's dependencies
      assert((dependencies_.find( PtrWithTorV(ptr, torv) ) != dependencies_.end()) && "Not in dependency table");
      for (const auto& dep : dependencies_ AT( PtrWithTorV(ptr, torv) )) {
        done_flag = (dep.torv_ ? type_done : val_done);
        seen_flag = (dep.torv_ ? type_seen : val_seen);

        if ((already_seen AT(dep.ptr_) & done_flag) != 0) {
          continue;

        } else if ((already_seen AT(dep.ptr_) & seen_flag) != 0) {
          error_log.log(dep.ptr_->line_num(), "Cyclic dependency found.");
          assert(false && "cyclic dep found");

        } else {
          if (debug::dependency_system) {
            std::cout << "| Pushing " << dep.ptr_ << " (" << (dep.torv_ ? "type" : "value") << ")" << std::endl;
          }

          expr_stack.push_back(dep.ptr_);
          torv_stack.push_back(dep.torv_);
        }
      }
    }
  }

  void assign_order() {
    std::map<AST::Expression*, Flag> already_seen;
    for (const auto& kv : dependencies_) {
      already_seen[kv.first.ptr_] = unseen;
    }

    for (const auto& kv : dependencies_) {
      auto ptr = kv.first.ptr_;
      auto torv = kv.first.torv_;
      if (torv && ((already_seen[ptr] & type_seen) != 0)) continue;
      if (!torv && ((already_seen[ptr] & val_seen) != 0)) continue;
      traverse_from(kv.first, already_seen);
    }
  }

}  // namespace Dep
