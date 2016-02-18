// No need to include the corresponding header. Everything here is global and extern

#include <map>
#include <set>
#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>

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
  static std::map<AST::Node*, Flag> already_seen_ = {};

  void record(AST::Node* node) { node->record_dependencies(); }

  void type_type(AST::Node* depender, AST::Node* dependee) {
    dependencies_[PtrWithTorV(depender, true)].emplace(dependee, true);
    dependencies_[PtrWithTorV(depender, false)];
  }

  void type_value(AST::Node* depender, AST::Node* dependee) {
    dependencies_[PtrWithTorV(depender, true)].emplace(dependee, false);
    dependencies_[PtrWithTorV(depender, false)];
  }

  void value_type(AST::Node* depender, AST::Node* dependee) {
    dependencies_[PtrWithTorV(depender, true)];
    dependencies_[PtrWithTorV(depender, false)].emplace(dependee, true);
  }

  void value_value(AST::Node* depender, AST::Node* dependee) {
    dependencies_[PtrWithTorV(depender, true)];
    dependencies_[PtrWithTorV(depender, false)].emplace(dependee, false);
  }

  void add_to_table(AST::Node* depender) {
    dependencies_[PtrWithTorV(depender, true)];
    dependencies_[PtrWithTorV(depender, false)];
  }

  void mark_as_done(AST::Node* e) {
    add_to_table(e);
    already_seen_[e] = tv_done;
  }


  void traverse_from(PtrWithTorV pt) {
    std::vector<AST::Node*> node_stack;
    std::vector<bool> torv_stack;
    node_stack.push_back(pt.ptr_);
    torv_stack.push_back(pt.torv_);

    while (!node_stack.empty()) {
      if (debug::dependency_system) {
        std::cin.ignore(1);
        std::cout << "\033[2J\033[1;1H" << std::endl;
      }
      // Ensure stacks match up
      assert(node_stack.size() == torv_stack.size() && "Stacks sizes don't match!");

      auto ptr = node_stack.back();
      auto torv = torv_stack.back();

      if (debug::dependency_system) {
        std::cout
          << "+------------------------------------------------" << std::endl;

        for (size_t i = 0; i < node_stack.size(); ++i) {
          std::cout << "| " << i << ". " << node_stack[i] << (torv_stack[i] ? " (type)" : " (value)") << std::endl;
        }

        std::cout << *ptr << std::endl;
        std::cout << "Seen flag (on entry): " << already_seen_ AT(ptr) << " (vs. seen_flag = " << (torv ? type_seen : val_seen) << ")"<< std::endl;
      }
 
      Flag done_flag = (torv ? type_done : val_done);
      if ((already_seen_ AT(ptr) & done_flag) != 0) {
        if (debug::dependency_system) {
          std::cout << "| Already done. Popping." << std::endl;
        }
        node_stack.pop_back();
        torv_stack.pop_back();
        continue;
      }

      Flag seen_flag = (torv ? type_seen : val_seen);
      if ((already_seen_ AT(ptr) & seen_flag) != 0) {
        node_stack.pop_back();
        torv_stack.pop_back();

        if (torv) {
          if (ptr->is_unop()) {
            auto unop = static_cast<AST::Unop*>(ptr);
            if (unop->op() == Language::Operator::At) {
              auto t = unop->operand()->type();
              while (t->is_pointer()) t = static_cast<Pointer*>(t)->pointee_type();
              if (t->is_pointer()) {
                t = static_cast<Pointer*>(t)->pointee_type();
                if (t->is_struct()) {
                  auto struct_type = static_cast<Structure*>(t);
                  PtrWithTorV ptr_with_torv(
                      struct_type->defining_expression(), false);
                  traverse_from(ptr_with_torv);
                }
              }
            }
          } else if (ptr->is_access()) {
            auto t = static_cast<AST::Access*>(ptr)->expr()->type();
            while (t->is_pointer()) t = static_cast<Pointer*>(t)->pointee_type();
            if (t->is_struct()) {
              auto struct_type = static_cast<Structure*>(t);
              PtrWithTorV ptr_with_torv(
                  struct_type->defining_expression(), false);
              traverse_from(ptr_with_torv);
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
 
        already_seen_ AT(ptr) = static_cast<Flag>((seen_flag << 1) | already_seen_ AT(ptr)); // Mark it as done

        if (debug::dependency_system) {
          std::cout << "Seen flag (now):      " << already_seen_ AT(ptr) << std::endl;
          std::cout << "| Already seen. Verifying." << std::endl;
        }
        continue;
      }

      // If you get here, this node is totally new.

      // Mark it as seen
      already_seen_ AT(ptr) = static_cast<Flag>(seen_flag | already_seen_ AT(ptr));
      if (debug::dependency_system) {
        std::cout << "Seen flag (later):    " << already_seen_ AT(ptr) << std::endl;
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

        if ((already_seen_ AT(dep.ptr_) & done_flag) != 0) {
          continue;

        } else if ((already_seen_ AT(dep.ptr_) & seen_flag) != 0) {
          error_log.log(dep.ptr_->line_num(), "Cyclic dependency found.");
          assert(false && "cyclic dep found");

        } else {
          if (debug::dependency_system) {
            std::cout << "| Pushing " << dep.ptr_ << " (" << (dep.torv_ ? "type" : "value") << ")" << std::endl;
          }

          node_stack.push_back(dep.ptr_);
          torv_stack.push_back(dep.torv_);
        }
      }
    }
  }

  void assign_order() {
    already_seen_.clear();
    for (const auto& kv : dependencies_) {
      already_seen_[kv.first.ptr_] = unseen;
    }

    for (const auto& kv : dependencies_) {
      auto ptr = kv.first.ptr_;
      auto torv = kv.first.torv_;
      if (torv && ((already_seen_[ptr] & type_seen) != 0)) continue;
      if (!torv && ((already_seen_[ptr] & val_seen) != 0)) continue;
      traverse_from(kv.first);
    }
  }

  class GraphVizFile {
    public:
      GraphVizFile(const char* filename) : fout_(filename) {
        fout_ << "digraph {\nrankdir=LR\n";
      }

      GraphVizFile& operator<<(const std::string& str) {
        fout_ << str;
        return *this;
      }

      ~GraphVizFile() {
        {
          fout_ << "}";
          fout_.close();
        }
        system("dot -Tpdf dependencies.dot -o dependencies.pdf");
      }
    private:
      std::ofstream fout_;
  };

  template<typename T> std::string str(T* ptr) {
    std::stringstream ss;
    ss << ptr;
    return ss.str();
  }

  void write_graphviz() {
      GraphVizFile gviz("dependencies.dot");
      for (const auto& node : dependencies_) {
        gviz
          << (node.first.torv_ ? "  t" : "  v") << str(node.first.ptr_)
          << " [label=\"" << node.first.ptr_->graphviz_label()
          << "\", fillcolor=\"" << (node.first.torv_ ? "#88ffaa" : "#88aaff")
          << "\", shape=\"" << (node.first.ptr_->is_identifier() ? "diamond"
              : node.first.ptr_->is_declaration() ? "rectangle" : "ellipse")
          << "\", style=\"filled\"];\n";
      }

      for (const auto& dep : dependencies_) {
        for (const auto& d : dep.second) {
          gviz
            << (dep.first.torv_ ? "  t" : "  v") << str(dep.first.ptr_) << " -> "
            << (d.torv_ ? "t" : "v") << str(d.ptr_) << ";\n";
        }
      }
  }

}  // namespace Dep
