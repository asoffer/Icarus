#include "ast/array_type.h"

#include <sstream>
#include "ast/hole.h"
#include "ir/cmd.h"

namespace ast {
std::string ArrayType::to_string(size_t n) const {
  ASSERT(length_ != nullptr);
  std::stringstream ss;
  ss << "[" << length_->to_string(n) << "; " << data_type_->to_string(n) << "]";
  return ss.str();
}

void ArrayType::assign_scope(Scope *scope) {
  scope_ = scope;
  length_->assign_scope(scope);
  data_type_->assign_scope(scope);
}

type::Type const *ArrayType::VerifyType(Context *ctx) {
  bool failed       = false;
  auto *length_type = length_->VerifyType(ctx);
  if (length_type != type::Int64) {
    ctx->error_log_.ArrayIndexType(span);
    failed = true;
  }

  auto *data_type_type = data_type_->VerifyType(ctx);
  if (data_type_type != type::Type_) {
    ctx->error_log_.ArrayDataTypeNotAType(data_type_->span);
    failed = true;
  }

  if (failed) return type::Type_;
  return ctx->set_type(this, type::Type_);
}

void ArrayType::Validate(Context *ctx) {
  length_->Validate(ctx);
  data_type_->Validate(ctx);
}

base::vector<ir::Val> ArrayType::EmitIR(Context *ctx) {
  return {ir::ValFrom(
      ir::Array(length_->EmitIR(ctx)[0].reg_or<i32>(),
                data_type_->EmitIR(ctx)[0].reg_or<type::Type const *>()))};
}

base::vector<ir::Register> ArrayType::EmitLVal(Context *) {
  UNREACHABLE(*this);
}

void ArrayType::ExtractJumps(JumpExprs *rets) const {
  // TODO length_ needs to be constexpr so maybe we're safe here? and don't need
  // to check it? This happens in other places too!
  length_->ExtractJumps(rets);
  data_type_->ExtractJumps(rets);
}
}  // namespace ast
