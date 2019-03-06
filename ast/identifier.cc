#include "ast/identifier.h"

#include "ast/declaration.h"
#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "backend/eval.h"
#include "error/log.h"
#include "ir/components.h"
#include "ir/val.h"
#include "misc/context.h"
#include "misc/module.h"
#include "core/scope.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
void Identifier::assign_scope(core::Scope *scope) { scope_ = scope; }

VerifyResult Identifier::VerifyType(Context *ctx) {
  for (auto iter = ctx->cyc_deps_.begin(); iter != ctx->cyc_deps_.end();
       ++iter) {
    if (*iter == this) {
      ctx->error_log()->CyclicDependency(
          std::vector<Identifier const *>(iter, ctx->cyc_deps_.end()));
      return VerifyResult::Error();
    }
  }
  ctx->cyc_deps_.push_back(this);
  base::defer d([&] { ctx->cyc_deps_.pop_back(); });

  // `decl_` is not necessarily null. Because we may call VerifyType many times
  // in multiple contexts, it is null the first time, but not on future
  // iterations.
  //
  // TODO that means we should probably resolve identifiers ahead of
  // type verification, but I think we rely on type information to figure it out
  // for now so you'll have to undo that first.
  if (decl_ == nullptr) {
    auto potential_decls = scope_->AllDeclsWithId(token, ctx);
    switch (potential_decls.size()) {
      case 1: {
        // TODO could it be that evn though there is only one declaration,
        // there's a bound constant of the same name? If so, we need to deal
        // with this case.
        decl_ = potential_decls[0].get();
      } break;
      case 0:
        // TODO what if you find a bound constant and some errror decls?
        for (auto const &[d, v] :
             ctx->mod_->constants_[ctx->bound_constants_].constants_) {
          if (d->id_ == token) {
            return VerifyResult(ctx->set_type(this, v.type), d->const_);
          }
        }

        ctx->error_log()->UndeclaredIdentifier(this);
        return VerifyResult::Error();
      default:
        // TODO Should we allow the overload?
        ctx->error_log()->UnspecifiedOverload(span);
        return VerifyResult::Error();
    }
  }

  if (!decl_->const_ && (span.start.line_num < decl_->span.start.line_num ||
                         (span.start.line_num == decl_->span.start.line_num &&
                          span.start.offset < decl_->span.start.offset))) {
    ctx->error_log()->DeclOutOfOrder(decl_, this);
  }

  // TODO: This needs cleanup. I'm sure there's a bunch of inefficiencies here
  // due to me not totally understanding what's going on.
  if (auto iter = ctx->bound_constants_.constants_.find(decl_);
      iter != ctx->bound_constants_.constants_.end()) {
    return VerifyResult(ctx->set_type(this, iter->second.type), decl_->const_);
  }

  // TODO this is because we may have determined the declartaion previously with
  // a different generic setup but not bound the type for this context. But this
  // is wrong in the sense that the declaration bound is possibly dependent on
  // the context.
  type::Type const *t = ctx->type_of(decl_);

  if (t == nullptr) { return VerifyResult::Error(); }
  return VerifyResult(ctx->set_type(this, t), decl_->const_);
}

ir::Results Identifier::EmitIr(Context *ctx) {
  ASSERT(decl_ != nullptr) << this->to_string(0);
  if (decl_->const_) { return decl_->EmitIr(ctx); }
  if (decl_->is_fn_param_) {
    auto *t = ctx->type_of(this);
    return ir::Results{decl_->is_output_ && !t->is_big()
                           ? ir::Load(ctx->addr(decl_), t)
                           : ctx->addr(decl_)};
  } else if (decl_->is<MatchDeclaration>()) {
    // TODO is there a better way to do look up? look up in parent too?
    if (auto iter = ctx->bound_constants_.constants_.find(decl_);
        iter != ctx->bound_constants_.constants_.end()) {
      return ir::Results::FromVals({iter->second});
    } else {
      UNREACHABLE(decl_);
    }
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
  return {ctx->addr(decl_)};
}

}  // namespace ast
