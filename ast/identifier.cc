#include "ast/identifier.h"

#include "ast/declaration.h"
#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "backend/eval.h"
#include "error/log.h"
#include "ir/components.h"
#include "misc/context.h"
#include "misc/module.h"
#include "core/scope.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {

ir::Results Identifier::EmitIr(Context *ctx) {
  ASSERT(decl_ != nullptr) << this->to_string(0);
  if (decl_->const_) { return const_cast<Declaration *>(decl_)->EmitIr(ctx); }
  if (decl_->is_fn_param_) {
    auto *t     = ctx->type_of(this);
    ir::Reg reg = ctx->addr(decl_);
    if (ctx->inline_) {
      ir::Results reg_results = (*ctx->inline_)[reg];
      if (!reg_results.is_reg(0)) { return reg_results; }
      reg = reg_results.get<ir::Reg>(0);
    }

    return ir::Results{decl_->is_output_ && !t->is_big() ? ir::Load(reg, t)
                                                         : reg};
  } else if (decl_->is<MatchDeclaration>()) {
    // TODO is there a better way to do look up? look up in parent too?
    UNREACHABLE(decl_);

  } else {
    auto *t   = ASSERT_NOT_NULL(ctx->type_of(this));
    auto lval = EmitLVal(ctx)[0];
    if (!lval.is_reg_) { NOT_YET(); }
    return ir::Results{ir::PtrFix(lval.reg_, t)};
  }
}

std::vector<ir::RegisterOr<ir::Addr>> Identifier::EmitLVal(Context *ctx) {
  ASSERT(decl_ != nullptr);
  ASSERT(decl_->const_ == false);
  ASSERT(decl_->is_fn_param_ == false);
  return {ctx->addr(decl_)};
}

}  // namespace ast
