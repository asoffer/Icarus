#include "ast/array_type.h"

#include <sstream>
#include "ast/hole.h"
#include "ast/verify_macros.h"
#include "ir/cmd.h"

namespace AST {
std::string ArrayType::to_string(size_t n) const {
  ASSERT(length_ != nullptr);
  std::stringstream ss;
  ss << "[" << length_->to_string(n) << "; " << data_type_->to_string(n) << "]";
  return ss.str();
}

void ArrayType::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  length_->assign_scope(scope);
  data_type_->assign_scope(scope);
}

void ArrayType::ClearIdDecls() {
  stage_range_ = StageRange{};
  length_->ClearIdDecls();
  data_type_->ClearIdDecls();
}

void ArrayType::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::Const;

  length_->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  limit_to(length_);
  data_type_->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  limit_to(data_type_);

  type = type::Type_;

  if (length_->is<Hole>()) { return; }
  if (length_->lvalue != Assign::Const || data_type_->lvalue != Assign::Const) {
    lvalue = Assign::RVal;
  }

  if (length_->type != type::Int) {
    ctx->error_log_.ArrayIndexType(span);
    limit_to(StageRange::NoEmitIR());
  }
}

void ArrayType::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  length_->Validate(ctx);
  data_type_->Validate(ctx);
}

void ArrayType::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  length_->SaveReferences(scope, args);
  data_type_->SaveReferences(scope, args);
}

void ArrayType::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  length_->contextualize(
      correspondant->as<std::decay_t<ArrayType>>().length_.get(), replacements);
  data_type_->contextualize(
      correspondant->as<std::decay_t<ArrayType>>().data_type_.get(),
      replacements);
}

ArrayType *ArrayType::Clone() const {
  auto *result       = new ArrayType;
  result->span       = span;
  result->length_    = base::wrap_unique(length_->Clone());
  result->data_type_ = base::wrap_unique(data_type_->Clone());
  return result;
}

IR::Val AST::ArrayType::EmitIR(Context *ctx) {
  return IR::Array(length_->EmitIR(ctx), data_type_->EmitIR(ctx));
}

void ArrayType::ExtractReturns(std::vector<const Expression *> *rets) const {
  // TODO length_ needs to be constexpr so maybe we're safe here? and don't need
  // to check it? This happens in other places too!
  length_->ExtractReturns(rets);
  data_type_->ExtractReturns(rets);
}
}  // namespace AST
