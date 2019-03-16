#include "ast/block_literal.h"

#include "ir/val.h"
#include "misc/context.h"
#include "core/scope.h"
#include "type/function.h"
#include "type/primitive.h"

namespace ast {
BlockLiteral::BlockLiteral(bool required) : required_(required) {}

std::string BlockLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "block {\n";
  for (auto const &b : before_) {
    ss << std::string(2 * (n + 1), ' ') << b.to_string(n + 1) << "\n";
  }
  for (auto const &a : after_) {
    ss << std::string(2 * (n + 1), ' ') << a.to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void BlockLiteral::assign_scope(core::Scope *scope) {
  scope_      = scope;
  body_scope_ = scope->add_child<core::DeclScope>();
  for (auto &b : before_) { b.assign_scope(body_scope_.get()); }
  for (auto &a : after_) { a.assign_scope(body_scope_.get()); }
}

void BlockLiteral::DependentDecls(base::Graph<Declaration *> *g,
                                  Declaration *d) const {
  for (auto const &b : before_) { b.DependentDecls(g, d); }
  for (auto const &a : after_) { a.DependentDecls(g, d); }
}

VerifyResult BlockLiteral::VerifyType(Context *ctx) {
  for (auto &b : before_) { b.VerifyType(ctx); }
  for (auto &a : after_) { a.VerifyType(ctx); }

  return ctx->set_result(
      this, VerifyResult::Constant(required_ ? type::Blk() : type::OptBlock));
}

void BlockLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &b : before_) { b.ExtractJumps(rets); }
  for (auto &a : after_) { a.ExtractJumps(rets); }
}

ir::Results BlockLiteral::EmitIr(Context *ctx) {
  for (auto &b : before_) { b.EmitIr(ctx); }
  for (auto &a : after_) { a.EmitIr(ctx); }
  return ir::Results::FromVals({ir::Val::Block(this)});
}

}  // namespace ast
