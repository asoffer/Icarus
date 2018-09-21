#include "ast/block_literal.h"

#include "ast/verify_macros.h"
#include "ir/val.h"
#include "scope.h"
#include "type/function.h"
#include "type/primitive.h"

namespace AST {
BlockLiteral::BlockLiteral(bool required) {
  type = required ? type::Block : type::OptBlock;
}

std::string BlockLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "block {\n"
     << std::string(2 * (n + 1), ' ') << before_->to_string(n + 1) << "\n"
     << std::string(2 * (n + 1), ' ') << after_->to_string(n + 1) << "\n"
     << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void BlockLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_     = scope;
  body_scope_ = scope->add_child<DeclScope>();
  before_->assign_scope(body_scope_.get());
  after_->assign_scope(body_scope_.get());
}

type::Type const *BlockLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  return type;
}

void BlockLiteral::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  // Because this returns void, we need to ignore the return value. Wrapping in
  // an immediately invoked lambda.
  [&]() -> type::Type const * {
    ctx->types_.buffered_emplace(this, type);
    [[maybe_unused]] VERIFY_OR_RETURN(before_type, before_);
    [[maybe_unused]] VERIFY_OR_RETURN(after_type, after_);

    // TODO type-check before/after functions.

    before_->Validate(ctx);
    after_->Validate(ctx);
    return type;
  }();
}

void BlockLiteral::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  before_->SaveReferences(scope, args);
  after_->SaveReferences(scope, args);
}

void BlockLiteral::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  before_->contextualize(correspondant->as<BlockLiteral>().before_.get(),
                          replacements);
  after_->contextualize(correspondant->as<BlockLiteral>().after_.get(),
                         replacements);
}

void BlockLiteral::ExtractReturns(base::vector<const Expression *> *rets) const {
  before_->ExtractReturns(rets);
  after_->ExtractReturns(rets);
}

BlockLiteral *BlockLiteral::Clone() const {
  auto *result     = new BlockLiteral(type == type::Block);
  result->span     = span;
  result->before_ = base::wrap_unique(before_->Clone());
  result->after_  = base::wrap_unique(after_->Clone());
  return result;
}

base::vector<IR::Val> AST::BlockLiteral::EmitIR(Context *ctx) {
  return {IR::Val::Block(this)};
}

base::vector<IR::Register> BlockLiteral::EmitLVal(Context *) { UNREACHABLE(this); }

}  // namespace AST
