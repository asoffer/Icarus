#include "module/module.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"

namespace module {
// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
BasicModule::BasicModule() : scope_(this) {}
BasicModule::~BasicModule() {}

void BasicModule::InitializeNodes(base::PtrSpan<ast::Node> nodes) {
  ast::Node::Initializer initializer{.scope = &scope_};
  ast::InitializeNodes(nodes, initializer);
  for (ast::Node const *node : nodes) {
    auto *decl = node->if_as<ast::Declaration>();
    if (not decl) { continue; }

    // TODO: This presumes we don't have conditional exports.
    if (decl->hashtags.contains(ir::Hashtag::Export)) {
      for (auto const &id : decl->ids()) { scope_.insert_exported(&id); }
    }
  }

  done_parsing_.Notify();
}

void BasicModule::AppendNodes(std::vector<std::unique_ptr<ast::Node>> nodes,
                              diagnostic::DiagnosticConsumer &diag,
                              Importer &importer) {
  InitializeNodes(nodes);
  ProcessNodes(nodes, diag, importer);
  nodes_.insert(nodes_.end(), std::make_move_iterator(nodes.begin()),
                std::make_move_iterator(nodes.end()));
}

// TODO: Add a version of this function that also gives the declarations that
// are inaccessible. Particularly interesting would be the case of an overlaod
// set mixing constant and non-constants. It should also be an error to
// reference that when you're only able to see some of the name.
std::vector<ast::Declaration::Id const *> AllVisibleDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id_name) {
  std::vector<ast::Declaration::Id const *> ids;
  bool only_constants = false;

  auto fn = [&](ast::Declaration::Id const *id) {
    if (not only_constants or
        (id->declaration().flags() & ast::Declaration::f_IsConst)) {
      ids.push_back(id);
    }
    return true;
  };

  for (ast::Scope const &s : starting_scope->ancestors()) {
    if (auto iter = s.decls_.find(id_name); iter != s.decls_.end()) {
      // TODO: Support multiple declarations
      for (auto const *id : iter->second) { fn(id); }
    }

    for (auto const *mod_scope : s.embedded_module_scopes()) {
      for (auto const *id : mod_scope->ExportedDeclarationIds(id_name)) {
        if (only_constants or
            (id->declaration().flags() & ast::Declaration::f_IsConst)) {
          ids.push_back(id);
        }
      }
    }

    if (s.is<ast::FnScope>()) { only_constants = true; }
  }
  return ids;
}

std::vector<ast::Declaration::Id const *> AllAccessibleDeclIds(
    ast::Scope const *starting_scope, std::string_view id_name) {
  std::vector<ast::Declaration::Id const *> decl_iters =
      module::AllVisibleDeclsTowardsRoot(starting_scope, id_name);
  auto child_decls = starting_scope->VisibleChildren(id_name);
  decl_iters.insert(decl_iters.end(), child_decls.begin(), child_decls.end());
  return decl_iters;
}

}  // namespace module
