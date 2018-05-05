#include "ast/scope_node.h"

#include <sstream>
#include "ast/block_literal.h"
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
  auto *scope_lit           = std::get<ScopeLiteral *>(scope_expr_val.value);

  struct BlockData {
    IR::BlockIndex before, body, after;
  };
  std::unordered_map<AST::BlockLiteral*, BlockData> block_map;

  for (const auto &decl : scope_lit->decls_) {
    if (decl.type != type::Block) { NOT_YET("local variables"); }
    auto block_val  = Evaluate(decl.init_val.get(), ctx)[0];
    auto block_lit  = std::get<AST::BlockLiteral *>(block_val.value);
    auto block_data = BlockData{
        IR::Func::Current->AddBlock(),
        IR::Func::Current->AddBlock(),
        IR::Func::Current->AddBlock(),
    };
    block_map[block_lit] = block_data;

    if (decl.identifier->token == "self") {
      // TODO check constness as part of type-checking
      IR::UncondJump(block_data.before);
    }
  }

  for (auto& [block_lit, block_data] : block_map) {
    IR::BasicBlock::Current = block_data.before;
    // TODO the context for the before_ and after_ functions is wrong. Bound
    // constants should not be for those in this scope but in the block literal
    // scope.
    auto call_enter_result = IR::Call(
        block_lit->before_->EmitIR(ctx),
        expr ? std::vector<IR::Val>{expr->EmitIR(ctx)} : std::vector<IR::Val>{},
        {});
    for (auto & [ jump_block_lit, jump_block_data ] : block_map) {
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          jump_block_data.body,
          IR::Eq(call_enter_result, IR::Val::Block(jump_block_lit)));
    }
    // TODO what to do with this last block? probably won't compile if llvm is
    // enabled.

    IR::BasicBlock::Current = block_data.body;
    stmts->EmitIR(ctx);
    IR::UncondJump(block_data.after);

    IR::BasicBlock::Current = block_data.after;
    auto call_exit_result = IR::Call(block_lit->after_->EmitIR(ctx), {}, {});
    for (auto & [ jump_block_lit, jump_block_data ] : block_map) {
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          jump_block_data.before,
          IR::Eq(call_exit_result, IR::Val::Block(jump_block_lit)));
    }
    // TODO what to do with this last block? probably won't compile if llvm is
    // enabled.

  }

  return IR::Val::None();
}

IR::Val ScopeNode::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
