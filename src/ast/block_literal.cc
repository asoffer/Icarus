#include "ast/block_literal.h"

#include "ast/verify_macros.h"
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

void BlockLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::Const;
}

void BlockLiteral::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);

  VERIFY_AND_RETURN_ON_ERROR(before_);
  VERIFY_AND_RETURN_ON_ERROR(after_);

  // TODO type-check before/after functions.

  before_->Validate(ctx);
  after_->Validate(ctx);
}

void BlockLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  before_->SaveReferences(scope, args);
  after_->SaveReferences(scope, args);
}

void BlockLiteral::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  before_->contextualize(correspondant->as<BlockLiteral>().before_.get(),
                          replacements);
  after_->contextualize(correspondant->as<BlockLiteral>().after_.get(),
                         replacements);
}

void BlockLiteral::ExtractReturns(std::vector<const Expression *> *rets) const {
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

IR::Val AST::BlockLiteral::EmitIR(Context *ctx) {
  return IR::Val::Block(this);
}

IR::Val BlockLiteral::EmitLVal(Context *) { UNREACHABLE(this); }

}  // namespace AST
