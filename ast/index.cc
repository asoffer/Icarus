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

void Index::assign_scope(core::Scope *scope) {
  scope_ = scope;
  lhs_->assign_scope(scope);
  rhs_->assign_scope(scope);
}

void Index::DependentDecls(DeclDepGraph *g, Declaration *d) const {
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
      ctx->error_log()->NonConstantTupleIndex(span);
      return VerifyResult::Error();
    }

    int64_t index = [&]() -> int64_t {
      auto results = backend::Evaluate(rhs_.get(), ctx);
      if (index_type == type::Int8) { return results.get<int8_t>(0).val_; }
      if (index_type == type::Int16) { return results.get<int16_t>(0).val_; }
      if (index_type == type::Int32) { return results.get<int32_t>(0).val_; }
      if (index_type == type::Int64) { return results.get<int64_t>(0).val_; }
      if (index_type == type::Nat8) { return results.get<uint8_t>(0).val_; }
      if (index_type == type::Nat16) { return results.get<uint16_t>(0).val_; }
      if (index_type == type::Nat32) { return results.get<uint32_t>(0).val_; }
      if (index_type == type::Nat64) { return results.get<uint64_t>(0).val_; }
      UNREACHABLE();
    }();

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
