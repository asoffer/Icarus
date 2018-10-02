#include "ast/identifier.h"

#include "ast/declaration.h"
#include "ast/function_literal.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "context.h"
#include "error/log.h"
#include "ir/components.h"
#include "ir/val.h"
#include "module.h"
#include "scope.h"
#include "type/pointer.h"
#include "type/type.h"

namespace AST {
void Identifier::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
}

Identifier *Identifier::Clone() const {
  auto *result  = new Identifier;
  result->span  = span;
  result->token = token;
  return result;
}

type::Type const *Identifier::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  type::Type const *t = nullptr;
  if (decl == nullptr) {
    auto[potential_decls, potential_error_decls] =
        scope_->AllDeclsWithId(token, ctx);
    switch (potential_decls.size()) {
      case 1: {
        // TODO could it be that evn though there is only one declaration,
        // there's a bound constant of the same name? If so, we need to deal
        // with this case.
        auto const &typed_decl = potential_decls[0];
        t                      = typed_decl.type_;
        decl                   = typed_decl.decl_;
      } break;
      case 0:
        // TODO what if you find a bound constant and some errror decls?
        for (auto const & [ d, v ] : ctx->mod_->bound_constants_.constants_) {
          if (d->identifier->token == token) {
            ctx->mod_->types_.emplace(this, v.type);
            return v.type;
          }
        }

        switch (potential_error_decls.size()) {
          case 0: ctx->error_log_.UndeclaredIdentifier(this); break;
          case 1: {
            auto const &typed_decl = potential_decls[0];
            t                      = typed_decl.type_;
            decl                   = typed_decl.decl_;
            HANDLE_CYCLIC_DEPENDENCIES;
          } break;
          default: NOT_YET();
        }
        limit_to(StageRange::Nothing());
        return nullptr;
      default:
        // TODO Should we allow the overload?
        ctx->error_log_.UnspecifiedOverload(span);
        limit_to(StageRange::Nothing());
        return nullptr;
    }
  }

  if (!decl->const_ && (span.start.line_num < decl->span.start.line_num ||
                        (span.start.line_num == decl->span.start.line_num &&
                         span.start.offset < decl->span.start.offset))) {
    ctx->error_log_.DeclOutOfOrder(decl, this);
    limit_to(StageRange::NoEmitIR());
  }

  if (t == nullptr) { return nullptr; }
  ctx->mod_->types_.emplace(this, t);
  return t;
}

void Identifier::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
}

base::vector<IR::Val> AST::Identifier::EmitIR(Context *ctx) {
  if (ASSERT_NOT_NULL(decl)->const_) {
    return decl->EmitIR(ctx);
  } else if (decl->arg_val) {
    return {IR::Val::Reg(decl->addr_, ctx->mod_->types_.at(this))};
  } else {
    auto *t = ASSERT_NOT_NULL(ctx->mod_->types_.at(this));
    return {IR::Val::Reg(IR::PtrFix(EmitLVal(ctx)[0], t), t)};
  }
}

base::vector<IR::Register> AST::Identifier::EmitLVal(Context *ctx) {
  ASSERT(decl != nullptr);
  ASSERT(!decl->const_);
  return {decl->addr_};
}

}  // namespace AST
