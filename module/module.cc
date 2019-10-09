#include "module/module.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"
#include "module/assign_scope.h"

namespace module {
// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
BasicModule::BasicModule() : scope_(this) {}
BasicModule::~BasicModule() = default;

void BasicModule::AppendStatements(
    std::vector<std::unique_ptr<ast::Node>> stmts) {
  AssignScope visitor;
  for (auto &stmt : stmts) { stmt->assign_scope(&visitor, &scope_); }

  unprocessed_.insert(unprocessed_.end(),
                      std::make_move_iterator(stmts.begin()),
                      std::make_move_iterator(stmts.end()));
}

void BasicModule::Append(std::unique_ptr<ast::Node> node) {
  AssignScope visitor;
  node->assign_scope(&visitor, &scope_);
  unprocessed_.push_back(std::move(node));
}

void BasicModule::IndexDeclarations(base::PtrSpan<ast::Node const> nodes) {
  for (ast::Node const *node : nodes) {
    auto *decl = node->if_as<ast::Declaration>();
    if (!decl) { continue; }

    bool exported = absl::c_any_of(decl->hashtags_, [](ast::Hashtag h) {
      return h.kind_ == ast::Hashtag::Builtin::Export;
    });
    if (!exported) { continue; }

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
