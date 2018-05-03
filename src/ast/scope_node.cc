#include "ast/scope_node.h"

#include <sstream>
#include "ast/function_literal.h"
#include "ast/scope_literal.h"
#include "ast/stages.h"
#include "ast/verify_macros.h"
#include "context.h"
#include "ir/func.h"
#include "scope.h"
#include "type/scope.h"
#include "type/type.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

namespace AST {
using base::check::Is; 

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << scope_expr->to_string(n);
  if (expr) { ss << " " << expr->to_string(n); }
  ss << " {\n" << stmts->to_string(n) << std::string(n * 2, ' ') << "}";
  return ss.str();
}

void ScopeNode::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  if (!internal) { internal = scope_->add_child<ExecScope>(); }
  scope_expr->assign_scope(scope);
  if (expr) { expr->assign_scope(scope); }
  stmts->assign_scope(internal.get());
}

void ScopeNode::ClearIdDecls() {
  stage_range_ = StageRange{};
  scope_expr->ClearIdDecls();
  if (expr) { expr->ClearIdDecls(); }
  stmts->ClearIdDecls();
}

void ScopeNode::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  lvalue = Assign::RVal;

  scope_expr->VerifyType(ctx);
  limit_to(scope_expr);
  if (expr != nullptr) {
    expr->VerifyType(ctx);
    limit_to(expr);
  }
  stmts->VerifyType(ctx);
  limit_to(stmts);

  if (!scope_expr->type->is<type::Scope>()) {
    ErrorLog::InvalidScope(scope_expr->span, scope_expr->type);
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO verify it uses the fields correctly
  type = type::Void;  // TODO can this evaluate to anything?
}

void ScopeNode::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  // TODO
}

void ScopeNode::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  expr->SaveReferences(scope, args);
  scope_expr->SaveReferences(scope, args);
  stmts->SaveReferences(scope, args);
}

void ScopeNode::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  expr->contextualize(correspondant->as<ScopeNode>().expr.get(), replacements);
  scope_expr->contextualize(correspondant->as<ScopeNode>().scope_expr.get(),
                            replacements);
  stmts->contextualize(correspondant->as<ScopeNode>().stmts.get(),
                       replacements);
}

void ScopeNode::ExtractReturns(std::vector<const Expression *> *rets) const {
  scope_expr->ExtractReturns(rets);
  if (expr) { expr->ExtractReturns(rets); }
  stmts->ExtractReturns(rets);
}

ScopeNode *ScopeNode::Clone() const {
  auto *result       = new ScopeNode;
  result->span       = span;
  result->expr       = base::wrap_unique(expr->Clone());
  result->scope_expr = base::wrap_unique(scope_expr->Clone());
  result->stmts      = base::wrap_unique(stmts->Clone());
  return result;
}

IR::Val AST::ScopeNode::EmitIR(Context *ctx) {
  IR::Val scope_expr_val = Evaluate(scope_expr.get(), ctx)[0];
  ASSERT(scope_expr_val.type, Is<type::Scope>());

  auto enter_fn =
      std::get<ScopeLiteral *>(scope_expr_val.value)->enter_fn->init_val.get();
  enter_fn->EmitIR(ctx);

  auto exit_fn =
      std::get<ScopeLiteral *>(scope_expr_val.value)->exit_fn->init_val.get();
  exit_fn->EmitIR(ctx);

  auto enter_block = IR::Func::Current->AddBlock();
  IR::UncondJump(enter_block);
  IR::Block::Current = enter_block;

  auto call_enter_result = IR::Call(
      IR::Val::Func(enter_fn->as<FunctionLiteral>().ir_func_),
      expr ? std::vector<IR::Val>{expr->EmitIR(ctx)} : std::vector<IR::Val>{},
      {});
  auto land_block       = IR::Func::Current->AddBlock();
  auto body_start_block = IR::Func::Current->AddBlock();

  IR::CondJump(call_enter_result, body_start_block, land_block);

  IR::Block::Current = body_start_block;
  stmts->EmitIR(ctx);

  auto call_exit_result =
      IR::Call(IR::Val::Func(exit_fn->as<FunctionLiteral>().ir_func_), {}, {});
  IR::CondJump(call_exit_result, enter_block, land_block);

  IR::Block::Current = land_block;
  return IR::Val::None();
}

IR::Val ScopeNode::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
