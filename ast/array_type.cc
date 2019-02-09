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

VerifyResult ArrayType::VerifyType(Context *ctx) {
  auto length_result = length_->VerifyType(ctx);
  if (length_result.type_ != type::Int64) { ctx->error_log_.ArrayIndexType(span); }

  auto data_type_result = data_type_->VerifyType(ctx);
  if (data_type_result.type_ != type::Type_) {
    ctx->error_log_.ArrayDataTypeNotAType(data_type_->span);
  }

  return VerifyResult(ctx->set_type(this, type::Type_),
                      data_type_result.const_ && length_result.const_);
}

void ArrayType::Validate(Context *ctx) {
  length_->Validate(ctx);
  data_type_->Validate(ctx);
}

std::vector<ir::Val> ArrayType::EmitIR(Context *ctx) {
  return {ir::ValFrom(
      ir::Array(length_->EmitIR(ctx)[0].reg_or<int64_t>(),
                data_type_->EmitIR(ctx)[0].reg_or<type::Type const *>()))};
}

std::vector<ir::RegisterOr<ir::Addr>> ArrayType::EmitLVal(Context *) {
  UNREACHABLE(*this);
}

void ArrayType::ExtractJumps(JumpExprs *rets) const {
  length_->ExtractJumps(rets);
  data_type_->ExtractJumps(rets);
}
}  // namespace ast
