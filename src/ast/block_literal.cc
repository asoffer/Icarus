#include "ast/block_literal.h"

#include "context.h"
#include "ir/val.h"
#include "scope.h"
#include "type/function.h"
#include "type/primitive.h"

namespace ast {
BlockLiteral::BlockLiteral(bool required) : required_(required) {}

std::string BlockLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "block {\n";
  for (auto &b : before_) {
    ss << std::string(2 * (n + 1), ' ') << b->to_string(n + 1) << "\n";
  }
  for (auto &a : after_) {
    ss << std::string(2 * (n + 1), ' ') << a->to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void BlockLiteral::assign_scope(Scope *scope) {
  scope_      = scope;
  body_scope_ = scope->add_child<DeclScope>();
  for (auto &b : before_) { b->assign_scope(body_scope_.get()); }
  for (auto &a : after_) { a->assign_scope(body_scope_.get()); }
}

type::Type const *BlockLiteral::VerifyType(Context *ctx) {
  return required_ ? type::Block : type::OptBlock;
}

void BlockLiteral::Validate(Context *ctx) {
  // Because this returns void, we need to ignore the return value. Wrapping in
  // an immediately invoked lambda.
  [&]() -> type::Type const * {
    ctx->set_type(this,
                        required_ ? type::Block : type::OptBlock);
    std::vector<type::Type const *> before_types, after_types;
    before_types.reserve(before_.size());
    for (auto &b : before_) {
      auto *before_type = b->VerifyType(ctx);
      if (before_type == nullptr) { return nullptr; }
      before_types.push_back(before_type);
    }
    for (auto &a : after_) {
      auto *after_type = a->VerifyType(ctx);
      if (after_type == nullptr) { return nullptr; }
      after_types.push_back(after_type);
    }

    // TODO type-check before/after functions.
    for (auto &b : before_) { b->Validate(ctx); }
    for (auto &a : after_) { a->Validate(ctx); }
    return required_ ? type::Block : type::OptBlock;
  }();
}

void BlockLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &b : before_) { b->ExtractJumps(rets); }
  for (auto &a : after_) { a->ExtractJumps(rets); }
}

base::vector<ir::Val> ast::BlockLiteral::EmitIR(Context *ctx) {
  for (auto& b : before_) { b->EmitIR(ctx); }
  for (auto& a : after_) { a->EmitIR(ctx); }
  return {ir::Val::Block(this)};
}

base::vector<ir::Register> BlockLiteral::EmitLVal(Context *) {
  UNREACHABLE(this);
}

}  // namespace ast
