#include "ast/identifier.h"

#include "ast/ast.h"  // TODO change this to declaration when that is moved out.
#include "ast/verify_macros.h"
#include "context.h"
#include "error/log.h"
#include "ir/val.h"
#include "module.h"
#include "scope.h"
#include "type/type.h"

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
        ErrorLog::LogGeneric(
            this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
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
  if (!decl) { return val ? *val : IR::Val::None(); }

  if (decl->const_) {
    if (val) { return *val; }

    if (decl->IsCustomInitialized()) {
      return Evaluate(decl->init_val.get(), ctx) AT(0);

    } else {
      // TODO this may be wrong for types that require nontrivial construction.
      return decl->type->EmitInitialValue(ctx);
    }
  }

  // TODO this global scope thing is probably wrong.
  if (decl->scope_ == ctx->mod_->global_.get()) { decl->EmitIR(ctx); }
  return decl->arg_val ? decl->addr : PtrCallFix(EmitLVal(ctx));
}

IR::Val AST::Identifier::EmitLVal(Context *ctx) {
  ASSERT(decl != nullptr);
  if (decl->addr == IR::Val::None()) { decl->EmitIR(ctx); }
  return decl->addr;
}

}  // namespace AST
