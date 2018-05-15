#include "ast/identifier.h"

#include "ast/declaration.h"
#include "ast/function_literal.h"
#include "ast/verify_macros.h"
#include "context.h"
#include "error/log.h"
#include "ir/val.h"
#include "module.h"
#include "scope.h"
#include "type/type.h"

#include "ir/cmd.h"

IR::Val PtrCallFix(const IR::Val& v);
std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

namespace AST {
void Identifier::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
}

void Identifier::ClearIdDecls() {
  stage_range_ = StageRange{};
  decl         = nullptr;
}

Identifier *Identifier::Clone() const {
  auto *result      = new Identifier;
  result->span     = span;
  result->token     = token;
  Declaration *decl = nullptr;
  return result;
}

void Identifier::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  if (decl == nullptr) {
    auto[potential_decls, potential_error_decls] =
        scope_->AllDeclsWithId(token, ctx);

    if (potential_decls.size() != 1) {
      if (potential_decls.empty()) {
        switch (potential_error_decls.size()) {
          case 0: ctx->error_log_.UndeclaredIdentifier(this); break;
          case 1:
            decl = potential_error_decls[0];
            HANDLE_CYCLIC_DEPENDENCIES;
            break;
          default: NOT_YET();
        }
      } else {
        // TODO is this reachable? Or does shadowing cover this case?
        NOT_YET("log an error");
      }
      type = type::Err;
      limit_to(StageRange::Nothing());
      return;
    }

    if (potential_decls[0]->const_ && potential_decls[0]->arg_val != nullptr &&
        potential_decls[0]->arg_val->is<GenericFunctionLiteral>()) {
      if (auto iter = ctx->bound_constants_->find(
              potential_decls[0]->identifier->token);
          iter != ctx->bound_constants_->end()) {
        potential_decls[0]->arg_val = scope_->ContainingFnScope()->fn_lit;
      } else {
        ctx->error_log_.UndeclaredIdentifier(this);
        type = type::Err;
        limit_to(StageRange::Nothing());
        return;
      }
    }
    decl = potential_decls[0];
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

IR::Val AST::Identifier::EmitIR(Context *ctx) {
  auto *val = AST::find(ctx->bound_constants_, token);
  if (decl == nullptr) { return val ? *val : IR::Val::None(); }

  if (decl->const_) {
    if (val) { return *val; }

    if (decl->IsCustomInitialized()) {
      return Evaluate(decl->init_val.get(), ctx) AT(0);

    } else {
      NOT_YET();
    }
  }

  // TODO checking for const isn't really what we want to do. we'd rather just
  // have addr not be tied to anything if it's const.
  if (decl->const_ && decl->addr == IR::Val::None()) { decl->EmitIR(ctx); }
  return decl->arg_val ? decl->addr : PtrCallFix(EmitLVal(ctx));
}

IR::Val AST::Identifier::EmitLVal(Context *ctx) {
  ASSERT(decl != nullptr);
  if (decl->const_ && decl->addr == IR::Val::None()) { decl->EmitIR(ctx); }
  return decl->addr;
}

}  // namespace AST
