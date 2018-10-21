#include "ast/block_literal.h"

#include "ast/verify_macros.h"
#include "ir/val.h"
#include "scope.h"
#include "type/function.h"
#include "type/primitive.h"

namespace AST {
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
    ctx->mod_->set_type(ctx->bound_constants_, this,
                        required_ ? type::Block : type::OptBlock);
    std::vector<type::Type const *> before_types, after_types;
    before_types.reserve(before_.size());
    for (auto &b : before_) {
      VERIFY_OR_RETURN(before_type, b);
      before_types.push_back(before_type);
    }
    for (auto &a : after_) {
      VERIFY_OR_RETURN(after_type, a);
      after_types.push_back(after_type);
    }

    // TODO type-check before/after functions.
    for (auto &b : before_) { b->Validate(ctx); }
    for (auto &a : after_) { a->Validate(ctx); }
    return required_ ? type::Block : type::OptBlock;
  }();
}

void BlockLiteral::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &b : before_) { b->SaveReferences(scope, args); }
  for (auto &a : after_) { a->SaveReferences(scope, args); }
}

void BlockLiteral::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < before_.size(); ++i) {
    before_[i]->contextualize(
        correspondant->as<BlockLiteral>().before_[i].get(), replacements);
  }
  for (size_t  i = 0; i < after_.size(); ++i) {
    after_[i]->contextualize(correspondant->as<BlockLiteral>().after_[i].get(),
                             replacements);
  }
}

void BlockLiteral::ExtractReturns(
    base::vector<const Expression *> *rets) const {
  for (auto &b : before_) { b->ExtractReturns(rets); }
  for (auto &a : after_) { a->ExtractReturns(rets); }
}

BlockLiteral *BlockLiteral::Clone() const {
  auto *result = new BlockLiteral(required_);
  result->span = span;
  for (auto &b : before_) { result->before_.emplace_back(b->Clone()); }
  for (auto &a : after_) { result->after_.emplace_back(a->Clone()); }
  return result;
}

base::vector<IR::Val> AST::BlockLiteral::EmitIR(Context *ctx) {
  return {IR::Val::Block(this)};
}

base::vector<IR::Register> BlockLiteral::EmitLVal(Context *) {
  UNREACHABLE(this);
}

}  // namespace AST
