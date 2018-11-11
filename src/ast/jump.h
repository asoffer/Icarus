#ifndef ICARUS_AST_JUMP_H
#define ICARUS_AST_JUMP_H

#include "ast/node.h"

namespace ast {
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
  void ExtractJumps(JumpExprs *) const override{};

  base::vector<ir::Val> EmitIR(Context *) override { return {}; }

  ExecScope *scope;
  JumpKind jump_type;
};
}  // namespace ast

#endif  // ICARUS_AST_JUMP_H
