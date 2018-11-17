#include "ast/array_type.h"

#include <sstream>
#include "ast/hole.h"
#include "ast/verify_macros.h"
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
  auto *length_type = length_->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  data_type_->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;

  ctx->set_type(this, type::Type_);

  if (!length_->is<Hole>() && length_type != type::Int64) {
    ctx->error_log_.ArrayIndexType(span);
  }

  return type::Type_;
}

void ArrayType::Validate(Context *ctx) {
  length_->Validate(ctx);
  data_type_->Validate(ctx);
}

base::vector<ir::Val> ArrayType::EmitIR(Context *ctx) {
  auto len_val       = length_->EmitIR(ctx)[0];
  auto data_type_reg = data_type_->EmitIR(ctx)[0].reg_or<type::Type const *>();
  ir::RegisterOr<type::Type const *> result =
      (len_val == ir::Val::None())
          ? ir::Array(data_type_reg)
          : ir::Array(len_val.reg_or<i32>(), data_type_reg);
  return {ir::ValFrom(result)};
}

base::vector<ir::Register> ArrayType::EmitLVal(Context *ct) {
  UNREACHABLE(*this);
}

void ArrayType::ExtractJumps(JumpExprs *rets) const {
  // TODO length_ needs to be constexpr so maybe we're safe here? and don't need
  // to check it? This happens in other places too!
  length_->ExtractJumps(rets);
  data_type_->ExtractJumps(rets);
}
}  // namespace ast
