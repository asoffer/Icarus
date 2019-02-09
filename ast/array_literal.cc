#include "ast/array_literal.h"

#include "misc/context.h"
#include "error/log.h"
#include "ir/cmd.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/pointer.h"

namespace ast {
std::string ArrayLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "[";
  if (!cl_.exprs_.empty()) {
    auto iter = cl_.exprs_.begin();
    ss << (*iter)->to_string(n);
    ++iter;
    while (iter != cl_.exprs_.end()) {
      ss << ", " << (*iter)->to_string(n);
      ++iter;
    }
  }
  ss << "]";
  return ss.str();
}

VerifyResult ArrayLiteral::VerifyType(Context *ctx) {
  if (cl_.exprs_.empty()) {
    return VerifyResult::Constant(ctx->set_type(this, type::EmptyArray));
  }

  ASSIGN_OR(return VerifyResult::Error(), auto expr_results,
                   cl_.VerifyWithoutSetting(ctx));
  VerifyResult result;
  auto *t      = expr_results.front().type_;
  result.type_ = type::Arr(t, expr_results.size());
  for (auto expr_result : expr_results) {
    result.const_ &= expr_result.const_;
    if (expr_result.type_ != t) {
      ctx->error_log_.InconsistentArrayType(span);
      return VerifyResult::Error();
    }
  }
  ctx->set_type(this, result.type_);
  return result;
}

std::vector<ir::Val> ArrayLiteral::EmitIR(Context *ctx) {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = ctx->type_of(this);
  auto alloc      = ir::TmpAlloca(this_type, ctx);
  auto array_val  = ir::Val::Reg(alloc, type::Ptr(this_type));
  if (!cl_.exprs_.empty()) {
    auto *data_type = this_type->as<type::Array>().data_type;
    for (size_t i = 0; i < cl_.exprs_.size(); ++i) {
      type::EmitMoveInit(
          data_type, cl_.exprs_[i]->EmitIR(ctx)[0],
          type::Typed<ir::Register>(
              ir::Index(type::Ptr(this_type), alloc, static_cast<int32_t>(i)),
              type::Ptr(data_type)),
          ctx);
    }
  }
  return {array_val};
}

void ArrayLiteral::EmitMoveInit(type::Typed<ir::Register> reg, Context *ctx) {
  type::Array const &array_type = ctx->type_of(this)->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type);
  auto elem = ir::Index(type::Ptr(&array_type), reg.get(), 0);
  for (size_t i = 0; i + 1 < array_type.len; ++i) {
    cl_.exprs_.at(i)->EmitMoveInit(
        type::Typed<ir::Register>(elem, data_type_ptr), ctx);
    elem = ir::PtrIncr(elem, 1, data_type_ptr);
  }
  cl_.exprs_.back()->EmitMoveInit(
      type::Typed<ir::Register>(elem, data_type_ptr), ctx);
}

void ArrayLiteral::EmitCopyInit(type::Typed<ir::Register> reg, Context *ctx) {
  type::Array const &array_type = ctx->type_of(this)->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type);
  auto elem = ir::Index(type::Ptr(&array_type), reg.get(), 0);
  for (size_t i = 0; i + 1 < array_type.len; ++i) {
    cl_.exprs_.at(i)->EmitCopyInit(
        type::Typed<ir::Register>(elem, data_type_ptr), ctx);
    elem = ir::PtrIncr(elem, 1, data_type_ptr);
  }
  cl_.exprs_.back()->EmitCopyInit(
      type::Typed<ir::Register>(elem, data_type_ptr), ctx);
}

}  // namespace ast
