#include "module/module.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"
#include "module/assign_scope.h"

namespace module {
// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
Module::Module() : scope_(this) {}

Module::~Module() = default;

void Module::AppendStatements(std::vector<std::unique_ptr<ast::Node>> stmts) {
  AssignScope visitor;
  for (auto &stmt : stmts) { stmt->assign_scope(&visitor, &scope_); }

  unprocessed_.insert(unprocessed_.end(),
                      std::make_move_iterator(stmts.begin()),
                      std::make_move_iterator(stmts.end()));
}

void Module::Append(std::unique_ptr<ast::Node> node) {
  AssignScope visitor;
  node->assign_scope(&visitor, &scope_);
  unprocessed_.push_back(std::move(node));
}

base::PtrSpan<ast::Node> Module::unprocessed() { return unprocessed_; };
base::PtrSpan<ast::Node const> Module::unprocessed() const {
  return unprocessed_;
};

void Module::process() {
  processed_.insert(processed_.end(),
                    std::make_move_iterator(unprocessed_.begin()),
                    std::make_move_iterator(unprocessed_.end()));
  unprocessed_.clear();
}

ast::Declaration *Module::GetDecl(std::string_view name) const {
  for (auto const &stmt : unprocessed_) {
    ASSIGN_OR(continue, auto &decl, stmt->if_as<ast::Declaration>());
    if (decl.id() != name) { continue; }
    bool exported = absl::c_any_of(decl.hashtags_, [](ast::Hashtag h) {
      return h.kind_ == ast::Hashtag::Builtin::Export;
    });
    if (!exported) { continue; }
    return &decl;
  }
  return nullptr;
}
}  // namespace module
