#include "ast/statements.h"

#include "misc/context.h"

namespace ast {
std::string Statements::to_string(size_t n) const {
  if (content_.empty()) { return ""; }

  std::stringstream ss;
  for (const auto &stmt : content_) {
    ss << std::string(n * 2, ' ') << stmt->to_string(n) << "\n";
  }
  return ss.str();
}

void Statements::append(std::unique_ptr<Node> &&node) {
  if (node->is<Statements>()) {
    content_.insert(
        content_.end(),
        std::make_move_iterator(node->as<Statements>().content_.begin()),
        std::make_move_iterator(node->as<Statements>().content_.end()));
  } else {
    content_.push_back(std::move(node));
  }
}

}  // namespace ast
