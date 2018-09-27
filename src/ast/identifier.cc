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

  if (decl == nullptr) {
    auto[potential_decls, potential_error_decls] =
        scope_->AllDeclsWithId(token, ctx);
    switch (potential_decls.size()) {
      case 1:
        // TODO could it be that evn though there is only one declaration,
        // there's a bound constant of the same name? If so, we need to deal
        // with this case.
        decl = potential_decls[0];
        break;
      case 0:
        // TODO what if you find a bound constant and some errror decls?
        for (auto const & [ decl, v ] : ctx->mod_->bound_constants_.constants_) {
          if (decl->identifier->token == token) { 
            // TODO Note that you're not assigning a declaration here. Is that
            // required? You shouldn't be relying on it... that's what the
            // staging system is for.
            type = type::Type_;
            ctx->mod_->types_.emplace(this, type::Type_);
            return type::Type_;
          }
        }

        switch (potential_error_decls.size()) {
          case 0: ctx->error_log_.UndeclaredIdentifier(this); break;
          case 1:
            decl = potential_error_decls[0];
            HANDLE_CYCLIC_DEPENDENCIES;
            break;
          default: NOT_YET();
        }
        type = type::Err;
        limit_to(StageRange::Nothing());
        return nullptr;
      default:
        // TODO Should we allow the overload?
        ctx->error_log_.UnspecifiedOverload(span);
        type = type::Err;
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

  // No guarantee the declaration has been validated yet.
  auto *decl_type = decl->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  type = decl_type;
  ctx->mod_->types_.emplace(this, decl_type);
  return decl_type;
}

void Identifier::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
}

base::vector<IR::Val> AST::Identifier::EmitIR(Context *ctx) {
  if (ASSERT_NOT_NULL(decl)->const_) {
    return decl->EmitIR(ctx);
  } else if (decl->arg_val) {
    return {IR::Val::Reg(decl->addr_, type)};
  } else {
    return {IR::Val::Reg(IR::PtrFix(EmitLVal(ctx)[0], type), type)};
  }
}

base::vector<IR::Register> AST::Identifier::EmitLVal(Context *ctx) {
  ASSERT(decl != nullptr);
  ASSERT(!decl->const_);
  return {decl->addr_};
}

}  // namespace AST
