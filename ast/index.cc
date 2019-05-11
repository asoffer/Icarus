#include "ast/index.h"

#include "backend/eval.h"
#include "ir/components.h"
#include "ir/str.h"
#include "core/arch.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/tuple.h"

namespace ast {

std::string Index::to_string(size_t n) const {
  return lhs_->to_string(n) + "[" + rhs_->to_string(n) + "]";
}

ir::Results Index::EmitIr(Context *ctx) {
  return ir::Results{ir::PtrFix(EmitLVal(ctx)[0].reg_, ctx->type_of(this))};
}

std::vector<ir::RegisterOr<ir::Addr>> Index::EmitLVal(Context *ctx) {
  auto *lhs_type = ctx->type_of(lhs_.get());
  auto *rhs_type = ctx->type_of(rhs_.get());

  if (lhs_type->is<type::Array>()) {
    auto index =
        ir::Cast(rhs_type, type::Int64, rhs_->EmitIr(ctx)).get<int64_t>(0);

    auto lval = lhs_->EmitLVal(ctx)[0];
    if (!lval.is_reg_) { NOT_YET(this, ctx->type_of(this)); }
    return {ir::Index(type::Ptr(ctx->type_of(lhs_.get())), lval.reg_, index)};
  } else if (auto *buf_ptr_type = lhs_type->if_as<type::BufferPointer>()) {
    auto index =
        ir::Cast(rhs_type, type::Int64, rhs_->EmitIr(ctx)).get<int64_t>(0);

    return {ir::PtrIncr(lhs_->EmitIr(ctx).get<ir::Reg>(0), index,
                        type::Ptr(buf_ptr_type->pointee))};
  } else if (lhs_type == type::ByteView) {
    // TODO interim until you remove string_view and replace it with Addr
    // entirely.
    auto index =
        ir::Cast(rhs_type, type::Int64, rhs_->EmitIr(ctx)).get<int64_t>(0);
    return {ir::PtrIncr(
        ir::GetString(lhs_->EmitIr(ctx).get<std::string_view>(0).val_), index,
        type::Ptr(type::Nat8))};
  } else if (auto *tup = lhs_type->if_as<type::Tuple>()) {
    auto index =
        ir::Cast(rhs_type, type::Int64, backend::Evaluate(rhs_.get(), ctx))
            .get<int64_t>(0)
            .val_;
    return {ir::Field(lhs_->EmitLVal(ctx)[0], tup, index).get()};
  }
  UNREACHABLE(*this);
}

}  // namespace ast
