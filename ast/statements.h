#ifndef ICARUS_AST_STATEMENTS_H
#define ICARUS_AST_STATEMENTS_H

#include <vector>

#include "ast/node.h"

namespace ast {
struct Statements : public Node {
  Statements() {}
  ~Statements() override {}
  Statements(Statements &&) noexcept = default;
  Statements &operator=(Statements &&) noexcept = default;

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return false;
  }

  ir::Results EmitIr(Context *) override;

  inline size_t size() const { return content_.size(); }

  void append(std::unique_ptr<Node> &&node);

  std::vector<std::unique_ptr<Node>> content_;
};

}  // namespace ast
#endif  // ICARUS_AST_STATEMENTS_H
