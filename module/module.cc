#include "module/module.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"
#include "frontend/parse.h"

namespace module {
// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
BasicModule::BasicModule() : scope_(this) {}
BasicModule::~BasicModule() {}

void BasicModule::InitializeNodes(base::PtrSpan<ast::Node> nodes) {
  ast::InitializeNodes(nodes, &scope_);
  for (ast::Node const *node : nodes) {
    auto *decl = node->if_as<ast::Declaration>();
    if (not decl) { continue; }

    // TODO: This presumes we don't have conditional exports.
    if (decl->hashtags.contains(ir::Hashtag::Export)) {
      // TODO: Support multiple declarations
      exported_declarations_[decl->ids()[0]].push_back(decl);
    }
  }
}

void BasicModule::ExportsComplete() { exports_complete_.Notify(); }

void BasicModule::AppendNode(std::unique_ptr<ast::Node> node,
                             diagnostic::DiagnosticConsumer &diag,
                             Importer &importer) {
  InitializeNodes(base::PtrSpan<ast::Node>(&node, 1));
  ProcessNodes(base::PtrSpan<ast::Node const>(&node, 1), diag, importer);
  nodes_.push_back(std::move(node));
}

// TODO not sure this is necessary.
void BasicModule::AppendNodes(std::vector<std::unique_ptr<ast::Node>> nodes,
                              diagnostic::DiagnosticConsumer &diag,
                              Importer &importer) {
  InitializeNodes(nodes);
  ProcessNodes(nodes, diag, importer);
  nodes_.insert(nodes_.end(), std::make_move_iterator(nodes.begin()),
                std::make_move_iterator(nodes.end()));
}

absl::Span<ast::Declaration const *const> BasicModule::ExportedDeclarations(
    std::string_view name) const {
  exports_complete_.WaitForNotification();
  auto iter = exported_declarations_.find(name);
  if (iter == exported_declarations_.end()) { return {}; }

  // TODO handle exported embedded modules here too.
  return iter->second;
}

std::vector<ast::Declaration::const_iterator> AllDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id) {
  std::vector<ast::Declaration::const_iterator> decl_iters;
  ForEachDeclTowardsRoot(starting_scope, id,
                         [&](ast::Declaration::const_iterator d) {
                           decl_iters.push_back(d);
                           return true;
                         });
  return decl_iters;
}

// TODO: Add a version of this function that also gives the declarations that
// are inaccessible. Particularly interesting would be the case of an overlaod
// set mixing constant and non-constants. It should also be an error to
// reference that when you're only able to see some of the name.
std::vector<ast::Declaration::const_iterator> AllVisibleDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id) {
  std::vector<ast::Declaration::const_iterator> decl_iters;
  bool only_constants = false;
  for (auto scope_ptr = starting_scope; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    if (auto iter = scope_ptr->decls_.find(id);
        iter != scope_ptr->decls_.end()) {
      // TODO: Support multiple declarations
      for (auto decl_iter : iter->second) {
        if (not only_constants or
            (decl_iter->declaration().flags() & ast::Declaration::f_IsConst)) {
          decl_iters.push_back(decl_iter);
        }
      }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      for (ast::Declaration const *decl : mod->ExportedDeclarations(id)) {
        if (only_constants or (decl->flags() & ast::Declaration::f_IsConst)) {
          for (auto iter : *decl) { decl_iters.push_back(iter); }
        }
      }
    }

    if (scope_ptr->is<ast::FnScope>()) { only_constants = true; }
  }
  return decl_iters;
}

std::vector<ast::Declaration::const_iterator> AllAccessibleDecls(
    ast::Scope const *starting_scope, std::string_view id) {
  std::vector<ast::Declaration::const_iterator> decl_iters =
      module::AllVisibleDeclsTowardsRoot(starting_scope, id);
  auto child_decls = starting_scope->VisibleChildren(id);
  decl_iters.insert(decl_iters.end(), child_decls.begin(), child_decls.end());
  return decl_iters;
}

}  // namespace module
