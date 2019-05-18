#ifndef ICARUS_AST_SWITCH_H
#define ICARUS_AST_SWITCH_H

#include "ast/expression.h"

namespace ast {
// TODO consider separating this into two classes given that we know when we
// parse if it has parens or not.
struct Switch : public Expression {
  ~Switch() override {}

#include "visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << "switch ";
    if (expr_) { ss << "(" << expr_->to_string(n) << ") {\n"; }
    for (const auto& [body, cond] : cases_) {
      ss << std::string((n + 1) * 2, ' ') << body->to_string(n + 1) << " when "
         << cond->to_string(n + 1) << "\n";
    }
    ss << std::string(2 * n, ' ') << "}";
    return ss.str();
  }

  std::unique_ptr<Expression> expr_;
  std::vector<std::pair<std::unique_ptr<Node>, std::unique_ptr<Expression>>>
      cases_;
};

// Temporary node which never appears in the AST but is useful during parsing to
// distinguish 'when' from other binary operators.
struct SwitchWhen : public Node {
  ~SwitchWhen() override {}

#include "visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    return body->to_string(n) + " when " + cond->to_string(n);
  }

  std::unique_ptr<Node> body;
  std::unique_ptr<Expression> cond;
};

}  // namespace ast

#endif  // ICARUS_AST_SWITCH_H
