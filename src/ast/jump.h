#ifndef ICARUS_AST_JUMP_H
#define ICARUS_AST_JUMP_H

#include "ast/node.h"

namespace AST {
struct Jump : public Node {
  Jump(const TextSpan &span, JumpKind jump_type)
      : Node(span), jump_type(jump_type) {}
  ~Jump() override {}

  std::string to_string(size_t n) const override {
    switch (jump_type) {
      case JumpKind::Return: return "return";
      case JumpKind::Yield: return "yield";
    }
    UNREACHABLE();
  }

  void assign_scope(Scope *scope) override { scope_ = scope; }
  type::Type const *VerifyType(Context *) override { return nullptr; }
  void Validate(Context *) override {}
  void SaveReferences(Scope *scope, base::vector<IR::Val> *) override {}
  void ExtractJumps(JumpExprs *) const override{};
  void contextualize(
      const Node *,
      const base::unordered_map<const Expression *, IR::Val> &) override {}

  Jump *Clone() const override { return new Jump(*this); }
  base::vector<IR::Val> EmitIR(Context *) override { return {}; }

  ExecScope *scope;
  JumpKind jump_type;
};
}  // namespace AST

#endif  // ICARUS_AST_JUMP_H
