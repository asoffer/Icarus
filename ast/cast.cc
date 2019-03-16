#include "ast/cast.h"

#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "misc/context.h"
#include "type/cast.h"
#include "type/tuple.h"

namespace ast {

std::string Cast::to_string(size_t n) const {
  return "(" + expr_->to_string(n) + ") as (" + type_->to_string(n) + ")";
}

void Cast::assign_scope(core::Scope *scope) {
  scope_ = scope;
  expr_->assign_scope(scope);
  type_->assign_scope(scope);
}

void Cast::DependentDecls(base::Graph<Declaration *> *g,
                          Declaration *d) const {
  expr_->DependentDecls(g, d);
  type_->DependentDecls(g, d);
}

VerifyResult Cast::VerifyType(Context *ctx) {
  auto expr_result = expr_->VerifyType(ctx);
  auto type_result = type_->VerifyType(ctx);
  if (!expr_result.ok() || !type_result.ok()) { return VerifyResult::Error(); }

  if (type_result.type_ != type::Type_) {
    ctx->error_log()->CastToNonType(span);
    return VerifyResult::Error();
  }
  if (!type_result.const_) {
    ctx->error_log()->CastToNonConstantType(span);
    return VerifyResult::Error();
  }
  auto *t = ASSERT_NOT_NULL(
      backend::EvaluateAs<type::Type const *>(type_.get(), ctx));
  if (t->is<type::Struct>()) {
    OverloadSet os(scope_, "as", ctx);
    os.add_adl("as", t);
    os.add_adl("as", expr_result.type_);
    os.keep_return(t);

    auto *ret_type = DispatchTable::MakeOrLogError(
        this, core::FnArgs<Expression *>({expr_.get()}, {}), os, ctx);
    if (ret_type == nullptr) { return VerifyResult::Error(); }
    ASSERT(t == ret_type);
    return ctx->set_result(this, VerifyResult(ret_type, expr_result.const_));

  } else {
    if (!type::CanCast(expr_result.type_, t)) {
      NOT_YET("log an error", expr_result.type_, t);
    }
    return VerifyResult(t, expr_result.const_);
  }
}

void Cast::ExtractJumps(JumpExprs *rets) const {
  expr_->ExtractJumps(rets);
  type_->ExtractJumps(rets);
}

ir::Results Cast::EmitIr(Context *ctx) {
  if (auto *dispatch_table = ctx->dispatch_table(this)) {
    // TODO struct is not exactly right. we really mean user-defined
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<Expression *, ir::Results>>(
            {std::pair(expr_.get(), expr_->EmitIr(ctx)),
             std::pair(type_.get(), type_->EmitIr(ctx))},
            {}),
        ASSERT_NOT_NULL(ctx->type_of(this)), ctx);
  }

  auto *this_type = ASSERT_NOT_NULL(ctx->type_of(this));
  auto results    = expr_->EmitIr(ctx);
  if (this_type == type::Type_) {
    std::vector<type::Type const *> entries;
    entries.reserve(results.size());
    for (size_t i = 0; i < results.size(); ++i) {
      // TODO what about incomplete structs?
      entries.push_back(results.GetResult(i).get<type::Type const *>(0).val_);
    }
    return ir::Results{type::Tup(entries)};
  }
  return ir::Cast(ctx->type_of(expr_.get()), this_type, results);
}

std::vector<ir::RegisterOr<ir::Addr>> Cast::EmitLVal(Context *ctx) {
  NOT_YET();
}

}  // namespace ast
