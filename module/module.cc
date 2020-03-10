#include "module/module.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"
#include "ast/initialize.h"
#include "frontend/parse.h"

namespace module {
// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
BasicModule::BasicModule() : scope_(this) {}
BasicModule::~BasicModule() = default;

void BasicModule::InitializeNodes(base::PtrSpan<ast::Node> nodes) {
  ast::InitializeNodes(nodes, &scope_);
  for (ast::Node const *node : nodes) {
    auto *decl = node->if_as<ast::Declaration>();
    if (not decl) { continue; }

    if (not decl->contains_hashtag(
            ast::Hashtag(ast::Hashtag::Builtin::Export))) {
      continue;
    }

    top_level_decls_[decl->id()].push_back(decl);
  }
}

void BasicModule::AppendNode(std::unique_ptr<ast::Node> node,
                             diagnostic::DiagnosticConsumer &diag) {
  InitializeNodes(base::PtrSpan<ast::Node>(&node, 1));
  ProcessNodes(base::PtrSpan<ast::Node const>(&node, 1), diag);
  nodes_.push_back(std::move(node));
}

// TODO not sure this is necessary.
void BasicModule::AppendNodes(std::vector<std::unique_ptr<ast::Node>> nodes,
                              diagnostic::DiagnosticConsumer &diag) {
  InitializeNodes(nodes);
  ProcessNodes(nodes, diag);
  nodes_.insert(nodes_.end(), std::make_move_iterator(nodes.begin()),
                std::make_move_iterator(nodes.end()));
}

void BasicModule::ProcessFromSource(frontend::Source *src,
                                    diagnostic::DiagnosticConsumer &diag) {
  auto nodes = frontend::Parse(src, diag);
  if (diag.num_consumed() > 0) { return; }
  AppendNodes(std::move(nodes), diag);
}

absl::Span<ast::Declaration const *const> BasicModule::declarations(
    std::string_view name) const {
  auto iter = top_level_decls_.find(name);
  if (iter == top_level_decls_.end()) { return {}; }

  // TODO handle exported embedded modules here too.
  return iter->second;
}

std::vector<ast::Declaration const *> AllDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id) {
  std::vector<ast::Declaration const *> decls;
  for (auto scope_ptr = starting_scope; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    if (auto iter = scope_ptr->decls_.find(id);
        iter != scope_ptr->decls_.end()) {
      for (auto *decl : iter->second) { decls.push_back(decl); }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      DEBUG_LOG("AllDeclsTowardsRoot")(starting_scope, " ", mod);
      // TODO use the right bound constants? or kill bound constants?
      for (auto *decl : mod->declarations(id)) {
        DEBUG_LOG("AllDeclsTowardsRoot")("   ", id);
        // TODO what about transitivity for embedded modules?
        // New context will lookup with no constants.
        decls.push_back(decl);
      }
    }
  }

  return decls;
}

std::vector<ast::Declaration const *> AllAccessibleDecls(
    ast::Scope const *starting_scope, std::string_view id) {
  std::vector<ast::Declaration const *> decls =
      module::AllDeclsTowardsRoot(starting_scope, id);
  auto child_decls = starting_scope->children_with_id(id);
  decls.insert(decls.end(), child_decls.begin(), child_decls.end());
  return decls;
}

}  // namespace module
