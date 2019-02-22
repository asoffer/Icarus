#ifndef ICARUS_AST_STATEMENTS_H
#define ICARUS_AST_STATEMENTS_H

#include <vector>

#include "node.h"

namespace ast {
struct Statements : public Node {
  Statements() {}
  ~Statements() override {}
  Statements(Statements &&) noexcept = default;
  Statements &operator=(Statements &&) noexcept = default;

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  ir::Results EmitIr(Context *) override;

  inline size_t size() const { return content_.size(); }

  void append(std::unique_ptr<Node> &&node);

  std::vector<std::unique_ptr<Node>> content_;
};

}  // namespace ast
#endif  // ICARUS_AST_STATEMENTS_H
