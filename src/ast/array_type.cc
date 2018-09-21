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

type::Type const *ArrayType::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  length_->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  limit_to(length_);
  data_type_->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  limit_to(data_type_);

  type = type::Type_;
  ctx->types_.buffered_emplace(this, type::Type_);

  if (!length_->is<Hole>() && length_->type != type::Int) {
    ctx->error_log_.ArrayIndexType(span);
    limit_to(StageRange::NoEmitIR());
  }

  return type::Type_;
}

void ArrayType::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  length_->Validate(ctx);
  data_type_->Validate(ctx);
}

void ArrayType::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  length_->SaveReferences(scope, args);
  data_type_->SaveReferences(scope, args);
}

void ArrayType::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
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

base::vector<IR::Val> ArrayType::EmitIR(Context *ctx) {
  auto len_val       = length_->EmitIR(ctx)[0];
  auto data_type_reg = data_type_->EmitIR(ctx)[0].reg_or<type::Type const *>();
  IR::RegisterOr<type::Type const *> result =
      (len_val == IR::Val::None())
          ? IR::Array(data_type_reg)
          : IR::Array(len_val.reg_or<i32>(), data_type_reg);
  return {IR::ValFrom(result)};
}

base::vector<IR::Register> ArrayType::EmitLVal(Context *ct) { UNREACHABLE(*this); }

void ArrayType::ExtractReturns(base::vector<const Expression *> *rets) const {
  // TODO length_ needs to be constexpr so maybe we're safe here? and don't need
  // to check it? This happens in other places too!
  length_->ExtractReturns(rets);
  data_type_->ExtractReturns(rets);
}
}  // namespace AST
