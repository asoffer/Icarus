#include "ast/index.h"

#include "backend/eval.h"
#include "ir/components.h"
#include "ir/val.h"
#include "layout/arch.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/tuple.h"

namespace ast {

std::string Index::to_string(size_t n) const {
  return lhs_->to_string(n) + "[" + rhs_->to_string(n) + "]";
}

void Index::assign_scope(core::Scope *scope) {
  scope_ = scope;
  lhs_->assign_scope(scope);
  rhs_->assign_scope(scope);
}

void Index::DependentDecls(DeclDepGraph *g,
                           Declaration *d) const {
  lhs_->DependentDecls(g, d);
  rhs_->DependentDecls(g, d);
}

VerifyResult Index::VerifyType(Context *ctx) {
  auto lhs_result = lhs_->VerifyType(ctx);
  auto rhs_result = rhs_->VerifyType(ctx);
  if (!lhs_result.ok() || !rhs_result.ok()) { return VerifyResult::Error(); }

  auto *index_type = rhs_result.type_->if_as<type::Primitive>();
  if (!index_type || !index_type->is_integral()) {
    ctx->error_log()->InvalidIndexType(span, lhs_result.type_, lhs_result.type_);
  }

  if (lhs_result.type_ == type::ByteView) {
    // TODO is nat8 what I want?
    return ctx->set_result(this, VerifyResult(type::Nat8, rhs_result.const_));
  } else if (auto *lhs_array_type = lhs_result.type_->if_as<type::Array>()) {
    return ctx->set_result(
        this, VerifyResult(lhs_array_type->data_type, rhs_result.const_));
  } else if (auto *lhs_buf_type =
                 lhs_result.type_->if_as<type::BufferPointer>()) {
    return ctx->set_result(
        this, VerifyResult(lhs_buf_type->pointee, rhs_result.const_));
  } else if (auto *tup = lhs_result.type_->if_as<type::Tuple>()) {
    if (!rhs_result.const_) {
      NOT_YET("log an error");
      return VerifyResult::Error();
    }

    // TODO Will this cast appropriately?
    int64_t index = type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                     uint16_t, uint32_t, uint64_t>(
        index_type, [&](auto type_holder) -> int64_t {
          using T = typename decltype(type_holder)::type;
          return backend::EvaluateAs<T>(rhs_.get(), ctx);
        });

    if (index < 0 || index >= static_cast<int64_t>(tup->size())) {
      ctx->error_log()->IndexingTupleOutOfBounds(span, tup, index);
      return VerifyResult::Error();
    }

    return ctx->set_result(
        this, VerifyResult(tup->entries_.at(index), lhs_result.const_));

  } else {
    ctx->error_log()->InvalidIndexing(span, lhs_result.type_);
    return VerifyResult::Error();
  }
}

void Index::ExtractJumps(JumpExprs *rets) const {
  lhs_->ExtractJumps(rets);
  rhs_->ExtractJumps(rets);
}

ir::Results Index::EmitIr(Context *ctx) {
  // TODO is indexing overloadable?
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
    return {ir::PtrIncr(ir::GetString(std::string(
                            lhs_->EmitIr(ctx).get<std::string_view>(0).val_)),
                        index, type::Ptr(type::Nat8))};
  } else if (auto *tup = lhs_type->if_as<type::Tuple>()) {
    auto index =
        ir::Cast(rhs_type, type::Int64,
                 ir::Results::FromVals(backend::Evaluate(rhs_.get(), ctx)))
            .get<int64_t>(0).val_;
    return {ir::Field(lhs_->EmitLVal(ctx)[0], tup, index).get()};
  }
  UNREACHABLE(*this);
}

}  // namespace ast
