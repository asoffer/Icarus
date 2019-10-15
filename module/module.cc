#include "module/module.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"
#include "module/assign_scope.h"

namespace module {
// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
BasicModule::BasicModule() : scope_(this) {}
BasicModule::~BasicModule() = default;

void BasicModule::InitializeNodes(base::PtrSpan<ast::Node> nodes) {
  AssignScope visitor;
  for (ast::Node *node : nodes) { node->assign_scope(&visitor, &scope_); }
  for (ast::Node const *node : nodes) {
    auto *decl = node->if_as<ast::Declaration>();
    if (not decl) { continue; }

    bool exported = absl::c_any_of(decl->hashtags_, [](ast::Hashtag h) {
      return h.kind_ == ast::Hashtag::Builtin::Export;
    });
    if (not exported) { continue; }

    top_level_decls_[decl->id()].push_back(decl);
  }
}

absl::Span<ast::Declaration const *const> BasicModule::declarations(
    std::string_view name) const {
  auto iter = top_level_decls_.find(name);
  if (iter == top_level_decls_.end()) { return {}; }

  // TODO handle exported embedded modules here too.
  return iter->second;
}

}  // namespace module
