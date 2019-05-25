#ifndef ICARUS_AST_STATEMENTS_H
#define ICARUS_AST_STATEMENTS_H

#include <memory>
#include <vector>

#include "ast/node.h"

namespace ast {
struct Statements : public Node {
  Statements() {}
  ~Statements() override {}
  Statements(Statements &&) noexcept = default;
  Statements &operator=(Statements &&) noexcept = default;

#include "visitor/visitors.xmacro.h"

  size_t size() const { return content_.size(); }
  void append(std::unique_ptr<Node> &&node) {
    if (auto *stmts = node->if_as<Statements>()) {
      content_.insert(content_.end(),
                      std::make_move_iterator(stmts->content_.begin()),
                      std::make_move_iterator(stmts->content_.end()));
    } else {
      content_.push_back(std::move(node));
    }
  }

  std::vector<std::unique_ptr<Node>> content_;
};

}  // namespace ast
#endif  // ICARUS_AST_STATEMENTS_H
