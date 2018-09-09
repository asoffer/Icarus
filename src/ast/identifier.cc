#include "ast/identifier.h"

#include "ast/declaration.h"
#include "ast/function_literal.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "context.h"
#include "error/log.h"
#include "ir/val.h"
#include "module.h"
#include "scope.h"
#include "type/type.h"
#include "type/pointer.h"

IR::Val PtrCallFix(const IR::Val& v);

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

void Identifier::VerifyType(Context *ctx) {
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
        for (const auto & [ tk, v ] : ctx->bound_constants_->constants_) {
          if (tk == token) { 
            // TODO Note that you're not assigning a declaration here. Is that
            // required? You shouldn't be relying on it... that's what the
            // staging system is for.
            type   = type::Type_;
            lvalue = Assign::Const;
            return;
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
        return;
      default:
        // TODO Should we allow the overload?
        ctx->error_log_.UnspecifiedOverload(span);
        type = type::Err;
        limit_to(StageRange::Nothing());
        return;
    }
  }

  if (!decl->const_ && (span.start.line_num < decl->span.start.line_num ||
                        (span.start.line_num == decl->span.start.line_num &&
                         span.start.offset < decl->span.start.offset))) {
    ctx->error_log_.DeclOutOfOrder(decl, this);
    limit_to(StageRange::NoEmitIR());
  }

  // No guarantee the declaration has been validated yet.
  decl->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  type = decl->type;
  lvalue = decl->lvalue == Assign::Const ? Assign::Const : Assign::LVal;
}

void Identifier::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
}

base::vector<IR::Val> AST::Identifier::EmitIR(Context *ctx) {
  auto *val = AST::find(ctx->bound_constants_, token);
  if (decl == nullptr) { return {val ? *val : IR::Val::None()}; }

  if (decl->const_) {
    if (val) { return {*val}; }

    if (decl->IsCustomInitialized()) {
      return backend::Evaluate(decl->init_val.get(), ctx);

    } else {
      NOT_YET(this->to_string(0));
    }
  }

  // TODO checking for const isn't really what we want to do. we'd rather just
  // have addr not be tied to anything if it's const.
  if (decl->const_ && decl->addr == IR::Val::None()) { decl->EmitIR(ctx); }
  return {decl->arg_val
              ? decl->addr
              : PtrCallFix(IR::Val::Reg(EmitLVal(ctx)[0], type::Ptr(type)))};
}

base::vector<IR::Register> AST::Identifier::EmitLVal(Context *ctx) {
  ASSERT(decl != nullptr);
  if (decl->const_ && decl->addr == IR::Val::None()) { decl->EmitIR(ctx); }
  return {std::get<IR::Register>(decl->addr.value)};
}

}  // namespace AST
