// No need to include the corresponding header. Everything here is global and extern

#include <map>
#include <set>
#include <iostream>
#include <vector>

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
  // This is terribly wasteful due to poor alignment.
  // Maybe a bottleneck for large programs but probably not.
  // In any event, for your own pride you should pack these neater.
  struct PtrWithTorV {
    PtrWithTorV() = delete;
    PtrWithTorV(AST::Expression *ptr, bool torv) : ptr_(ptr), torv_(torv) {}
    AST::Expression *ptr_;
    bool torv_; // true => type, false => value
  };
}  // namespace Dependency

namespace std {
  template<> struct less<Dependency::PtrWithTorV> {
    bool operator()(const Dependency::PtrWithTorV& lhs, const Dependency::PtrWithTorV& rhs) const {
      if (lhs.ptr_ != rhs.ptr_) return lhs.ptr_ < rhs.ptr_;
      if (lhs.torv_ != rhs.torv_) return lhs.torv_ < rhs.torv_;
      return false;
    }
  };
}  // namespace std

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

  // Gives us a map from identifiers to their declarations
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
      dependencies_[PtrWithTorV(decl_id.get(), true)]  = {};
      dependencies_[PtrWithTorV(decl_id.get(), false)] = {};
    }
  }

  enum Flag : char {
    unseen    = 0x00,
    type_seen = 0x01,
    val_seen  = 0x04,
    tv_seen   = 0x05,
    type_done = 0x02,
    val_done  = 0x08,
    tv_done   = 0x0a
  };

  void assign_order() {
    // Find all sources in the dependency graph. First record for each
    // identifier, how many times something depends on its type and on its
    // value.
    using pair_nums = std::pair<size_t, size_t>;
    std::map<AST::Expression*, pair_nums> num_immediate_dep_refs;
    std::map<AST::Expression*, Flag> already_seen;

    for (const auto& kv : dependencies_) {
      already_seen[kv.first.ptr_] = unseen;

      num_immediate_dep_refs[kv.first.ptr_];

      for (const auto& dep : kv.second) {
        if (dep.torv_) {
          num_immediate_dep_refs[dep.ptr_].first++;
        } else {
          num_immediate_dep_refs[dep.ptr_].second++;
        }
      }
    }

    // Next, pick out all the PtrWithTorVs that are are not depended on by
    // anything. These are all possible sources and get put into the stack to
    // start out with. We store our stack in two parallel chunks. One with the
    // expression pointer and one with the bools representing type/value
    // This saves us all the padding that gets placed at the end of the
    // PtrWithTorV struct.
    std::vector<AST::Expression*> expr_stack;
    std::vector<bool> torv_stack;

    for (const auto& kv : num_immediate_dep_refs) {
      if (kv.second.first == 0) {
        expr_stack.push_back(kv.first);
        torv_stack.push_back(true);
      }

      if (kv.second.second == 0) {
        expr_stack.push_back(kv.first);
        torv_stack.push_back(false);
      }
    }

    assert(expr_stack.size() == torv_stack.size() && "Stacks initialized to be of differet sizes");

    std::vector<PtrWithTorV> topo_order;

    if (debug::dependency_system) {
      std::cout << "+------DEPENDENCY-TABLE------+" << std::endl;
      for (const auto kv : dependencies_) {
        std::cout << "| " << kv.first.ptr_ << (kv.first.torv_ ? " (type)  (" : " (value) (") << kv.second.size() << ") | " << kv.first.ptr_->token() << num_immediate_dep_refs[kv.first.ptr_].first << num_immediate_dep_refs[kv.first.ptr_].second << std::endl;
      }
      std::cout << "+----------------------------+" << std::endl;
    }

    while (!expr_stack.empty()) {
      if (debug::dependency_system) {
        // std::cin.ignore(1);
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
        // pop it off and put it in topo_order
        expr_stack.pop_back();
        torv_stack.pop_back();
        topo_order.emplace_back(ptr, torv);
        already_seen AT(ptr) = static_cast<Flag>((seen_flag << 1) | already_seen AT(ptr)); // Mark it as done

        if (debug::dependency_system) {
          std::cout << "Seen flag (now):      " << already_seen AT(ptr) << std::endl;
          std::cout << "| Already seen. Adding to topo_order." << std::endl;
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

    if (debug::dependency_system) {
      std::cout << "+-----------ORDER:-----------+" << std::endl;
      for (const auto& ptr_with_torv : topo_order) {
        std::cout << "| " << ptr_with_torv.ptr_
          << (ptr_with_torv.torv_ ? " (type) " : " (value) ") << std::endl;
      }
      std::cin.ignore(1);
    }

    // TODO Check for cyclic dependencies that are in components without sources

    for (auto& ptr_with_torv : topo_order) {
      if (ptr_with_torv.torv_) {
        ptr_with_torv.ptr_->verify_types();
      }

      if (debug::dependency_system) {
        std::cout << "\033[2J\033[1;1H" << std::endl;
        std::cout << *ptr_with_torv.ptr_ << std::endl;
        std::cout << "line " << ptr_with_torv.ptr_->line_num() << std::endl;
        std::cout << "== " << (ptr_with_torv.torv_ ? "type" : "value") << " ==" << std::endl;
        if (ptr_with_torv.torv_) {
          std::cout << *ptr_with_torv.ptr_->type() << std::endl;
        } else {
          // std::cout << *ptr_with_torv.ptr_->type() << std::endl;
        }
      }

      // If it's an identifier, push it into the declarations for the
      // appropriate scope, so they can be allocated correctly
      if (ptr_with_torv.torv_ && ptr_with_torv.ptr_->is_identifier()) {
        auto id_ptr = static_cast<AST::Identifier*>(ptr_with_torv.ptr_)->shared_from_this();
        Scope::scope_containing_[id_ptr]->ordered_decls_.push_back(Scope::decl_of_[id_ptr]);
      }

      if (!ptr_with_torv.torv_ && ptr_with_torv.ptr_->is_type_literal()) {
        auto struct_ptr = static_cast<AST::TypeLiteral*>(ptr_with_torv.ptr_);
        struct_ptr->build_llvm_internals();
      }

      if (debug::dependency_system) {
        std::cin.ignore(1);
      }
    }
  }
}  // namespace Dep
