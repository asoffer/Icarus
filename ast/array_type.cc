#include "ast/array_type.h"

#include <sstream>
#include "ir/cmd.h"

namespace ast {
std::string ArrayType::to_string(size_t n) const {
  ASSERT(length_ != nullptr);
  std::stringstream ss;
  ss << "[" << length_->to_string(n) << "; " << data_type_->to_string(n) << "]";
  return ss.str();
}

void ArrayType::assign_scope(core::Scope *scope) {
  scope_ = scope;
  length_->assign_scope(scope);
  data_type_->assign_scope(scope);
}

void ArrayType::DependentDecls(DeclDepGraph *g,
                               Declaration *d) const {
  length_->DependentDecls(g, d);
  data_type_->DependentDecls(g, d);
}

bool ArrayType::InferType(type::Type const *t, InferenceState *state) const {
  auto *a = t->if_as<type::Array>();
  return a && data_type_->InferType(a->data_type, state);
}

VerifyResult ArrayType::VerifyType(Context *ctx) {
  auto length_result = length_->VerifyType(ctx);
  if (length_result.type_ != type::Int64) {
    ctx->error_log()->ArrayIndexType(span);
  }

  auto data_type_result = data_type_->VerifyType(ctx);
  if (data_type_result.type_ != type::Type_) {
    ctx->error_log()->ArrayDataTypeNotAType(data_type_->span);
  }

  return ctx->set_result(
      this, VerifyResult(type::Type_,
                         data_type_result.const_ && length_result.const_));
}

ir::Results ArrayType::EmitIr(Context *ctx) {
  return ir::Results{
      ir::Array(length_->EmitIr(ctx).get<int64_t>(0),
                data_type_->EmitIr(ctx).get<type::Type const *>(0))};
}

void ArrayType::ExtractJumps(JumpExprs *rets) const {
  length_->ExtractJumps(rets);
  data_type_->ExtractJumps(rets);
}
}  // namespace ast
