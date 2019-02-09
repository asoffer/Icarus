#include "ast/block_literal.h"

#include "misc/context.h"
#include "ir/val.h"
#include "misc/scope.h"
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

VerifyResult BlockLiteral::VerifyType(Context *ctx) {
  return VerifyResult::Constant(required_ ? type::Block : type::OptBlock);
}

void BlockLiteral::Validate(Context *ctx) {
  ctx->set_type(this, required_ ? type::Block : type::OptBlock);
  bool err = false;
  for (auto &b : before_) { err |= !b->VerifyType(ctx).ok(); }
  for (auto &a : after_) { err |= !a->VerifyType(ctx).ok(); }
  if (err) { return; }

  // TODO type-check before/after functions.
  for (auto &b : before_) { b->Validate(ctx); }
  for (auto &a : after_) { a->Validate(ctx); }
}

void BlockLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &b : before_) { b->ExtractJumps(rets); }
  for (auto &a : after_) { a->ExtractJumps(rets); }
}

std::vector<ir::Val> ast::BlockLiteral::EmitIR(Context *ctx) {
  for (auto& b : before_) { b->EmitIR(ctx); }
  for (auto& a : after_) { a->EmitIR(ctx); }
  return {ir::Val::Block(this)};
}

}  // namespace ast
