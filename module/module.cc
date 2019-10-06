#include "module/module.h"

#include "ast/ast.h"

namespace module {
// Can't declare this in header because unique_ptr's destructor needs to know
// the size of ir::CompiledFn which we want to forward declare.
Module::Module(std::vector<std::unique_ptr<ast::Node>> stmts)
    : scope_(this), statements_(std::move(stmts)) {}

Module::~Module() = default;

void Module::AppendStatements(std::vector<std::unique_ptr<ast::Node>> stmts) {
  statements_.insert(statements_.end(), std::make_move_iterator(stmts.begin()),
                     std::make_move_iterator(stmts.end()));
}

ast::Declaration *Module::GetDecl(std::string_view name) const {
  for (auto const &stmt : statements_) {
    ASSIGN_OR(continue, auto &decl, stmt->if_as<ast::Declaration>());
    if (decl.id() != name) { continue; }
    auto &hashtags = decl.hashtags_;
    bool exported =
        std::any_of(hashtags.begin(), hashtags.end(), [](ast::Hashtag h) {
          return h.kind_ == ast::Hashtag::Builtin::Export;
        });
    if (!exported) { continue; }
    return &decl;
  }
  return nullptr;
}
}  // namespace module
