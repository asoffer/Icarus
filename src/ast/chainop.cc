#include "ast/chainop.h"

#include "ast/fn_args.h"
#include "ast/verify_macros.h"
#include "base/check.h"
#include "context.h"
#include "ir/func.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/struct.h"
#include "type/tuple.h"

IR::Val PtrCallFix(const IR::Val& v);
namespace IR {

IR::Val MakeBlockSeq(const std::vector<IR::Val>&);
}  // namespace IR

std::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

namespace AST {
std::vector<Expression *> FunctionOptions(const std::string &token,
                                          Scope *scope, Context *ctx);
const type::Type *SetDispatchTable(const FnArgs<Expression *> &args,
                                   std::vector<Expression *> fn_options,
                                   AST::DispatchTable *dispatch_table,
                                   Context *ctx);

namespace {
using base::check::Is;
using base::check::Not;

IR::Val EmitChainOpPair(AST::ChainOp *chain_op, size_t index,
                        const IR::Val &lhs_ir, const IR::Val &rhs_ir,
                        Context *ctx) {
  const type::Type *lhs_type = chain_op->exprs[index]->type;
  const type::Type *rhs_type = chain_op->exprs[index + 1]->type;
  auto op                    = chain_op->ops[index];

  if (lhs_type->is<type::Array>() && rhs_type->is<type::Array>()) {
    ASSERT(op == Language::Operator::Eq || op == Language::Operator::Ne);
    return type::Array::Compare(&lhs_type->as<type::Array>(), lhs_ir,
                                &rhs_type->as<type::Array>(), rhs_ir,
                                op == Language::Operator::Eq, ctx);
  } else if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
    FnArgs<std::pair<Expression *, IR::Val>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(
        chain_op->exprs[index].get(),
        chain_op->exprs[index]->type->is_big() ? PtrCallFix(lhs_ir) : lhs_ir);
    args.pos_.emplace_back(chain_op->exprs[index + 1].get(),
                           chain_op->exprs[index + 1]->type->is_big()
                               ? PtrCallFix(rhs_ir)
                               : rhs_ir);

    auto results = EmitCallDispatch(args, chain_op->dispatch_tables_[index],
                                    type::Bool, ctx);
    ASSERT(results.size() == 1u);
    return results[0];

  } else {
    switch (op) {
      case Language::Operator::Lt: return IR::Lt(lhs_ir, rhs_ir);
      case Language::Operator::Le: return IR::Le(lhs_ir, rhs_ir);
      case Language::Operator::Eq: return IR::Eq(lhs_ir, rhs_ir);
      case Language::Operator::Ne: return IR::Ne(lhs_ir, rhs_ir);
      case Language::Operator::Ge: return IR::Ge(lhs_ir, rhs_ir);
      case Language::Operator::Gt:
        return IR::Gt(lhs_ir, rhs_ir);
        // TODO case Language::Operator::And: cmp = lhs_ir; break;

      default: UNREACHABLE();
    }
  }
}

}  // namespace

std::string ChainOp::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  for (size_t i = 0; i < ops.size(); ++i) {
    ss << exprs[i]->to_string(n);
    switch (ops[i]) {
      case Language::Operator::Or: ss << " | "; break;
      case Language::Operator::Xor: ss << " ^ "; break;
      case Language::Operator::And: ss << " & "; break;
      case Language::Operator::Lt: ss << " < "; break;
      case Language::Operator::Le: ss << " <= "; break;
      case Language::Operator::Eq: ss << " == "; break;
      case Language::Operator::Ne: ss << " != "; break;
      case Language::Operator::Ge: ss << " >= "; break;
      case Language::Operator::Gt: ss << " > "; break;
      default: UNREACHABLE();
    }
  }
  ss << exprs.back()->to_string(n) << ")";
  return ss.str();
}

void ChainOp::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void ChainOp::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto &expr : exprs) { expr->ClearIdDecls(); }
}

void ChainOp::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  bool found_err = false;

  lvalue = Assign::Const;
  for (auto &expr : exprs) {
    expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr->type == type::Err) { found_err = true; }
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  if (found_err) {
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  if (ops[0] == Language::Operator::Or) {
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      if (exprs[i]->type == type::Block) {
        NOT_YET("log an error");
      } else if (exprs[i]->type == type::OptBlock) {
        continue;
      } else {
        goto not_blocks;
      }
    }
    if (exprs.back()->type != type::Block &&
        exprs.back()->type != type::OptBlock) {
      NOT_YET("log an error");
      type = type::Err;
    } else {
      type = exprs.back()->type;
      return;
    }
  }
  not_blocks:

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  switch (ops[0]) {
    case Language::Operator::Or:
    case Language::Operator::And:
    case Language::Operator::Xor: {
      bool failed = false;
      for (const auto &expr : exprs) {
        if (expr->type != exprs[0]->type) {
          NOT_YET("log an error");
          failed = true;
        }
      }

      type = exprs[0]->type;

      if (exprs[0]->type != type::Bool &&
          !(exprs[0]->type == type::Type_ &&
            ops[0] == Language::Operator::Or) &&
          (!exprs[0]->type->is<type::Flags>())) {
        NOT_YET("log an error");
        if (failed) {
          limit_to(StageRange::Nothing());
          return;
        }
      }

      return;
    } break;
    default: {
      ASSERT(exprs.size() >= 2u);
      for (size_t i = 0; i < exprs.size() - 1; ++i) {
        const type::Type *lhs_type = exprs[i]->type;
        const type::Type *rhs_type = exprs[i + 1]->type;
        if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
          // TODO struct is wrong. generally user-defined (could be array of
          // struct too, or perhaps a variant containing a struct?) need to
          // figure out the details here.
          const char *token = nullptr;
          switch (ops[i]) {
            case Language::Operator::Lt: token = "<"; break;
            case Language::Operator::Le: token = "<="; break;
            case Language::Operator::Eq: token = "=="; break;
            case Language::Operator::Ne: token = "!="; break;
            case Language::Operator::Ge: token = ">="; break;
            case Language::Operator::Gt: token = ">"; break;
            default: UNREACHABLE();
          }

          FnArgs<Expression *> args;
          args.pos_ = std::vector{exprs[i].get(), exprs[i + 1].get()};
          type = SetDispatchTable(args, FunctionOptions(token, scope_, ctx),
                                  &dispatch_tables_[i], ctx);
          ASSERT(type, Not(Is<type::Tuple>()));
          if (type == type::Err) { limit_to(StageRange::Nothing()); }
        } else {
          if (lhs_type != rhs_type) {
            NOT_YET(lhs_type, " ", rhs_type);
          } else {
            auto cmp = lhs_type->Comparator();

            switch (ops[i]) {
              case Language::Operator::Eq:
              case Language::Operator::Ne: {
                switch (cmp) {
                  case type::Cmp::Order:
                  case type::Cmp::Equality: continue;
                  case type::Cmp::None:
                    type = type::Err;
                    NOT_YET("log an error");
                }
              } break;
              case Language::Operator::Lt:
              case Language::Operator::Le:
              case Language::Operator::Ge:
              case Language::Operator::Gt: {
                switch (cmp) {
                  case type::Cmp::Order: continue;
                  case type::Cmp::Equality:
                  case type::Cmp::None:
                    type = type::Err;
                    NOT_YET("log an error");
                }
              } break;
              default: UNREACHABLE("Expecting a ChainOp operator type.");
            }
          }
        }
      }

      if (type == type::Err) { limit_to(StageRange::Nothing()); }
      type = type::Bool;
    }
  }
}

void ChainOp::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &expr : exprs) { expr->Validate(ctx); }
}

void ChainOp::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->SaveReferences(scope, args); }
}

void ChainOp::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) {
    exprs[i]->contextualize(correspondant->as<ChainOp>().exprs[i].get(),
                            replacements);
  }
}

void ChainOp::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

IR::Val ChainOp::EmitIR(Context *ctx) {
  if (ops[0] == Language::Operator::Xor) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::Xor(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::Or && type->is<type::Enum>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::Or(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::And && type->is<type::Enum>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::And(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::Or && type == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<IR::Val> args;
    args.reserve(exprs.size());
    for (const auto &expr : exprs) { args.push_back(expr->EmitIR(ctx)); }
    return IR::Variant(std::move(args));
  } else if (ops[0] == Language::Operator::Or &&
             (type == type::Block || type == type::OptBlock)) {
    std::vector<IR::Val> vals;
    vals.reserve(exprs.size());
    for (auto &expr : exprs) { vals.push_back(expr->EmitIR(ctx)); }
    return IR::MakeBlockSeq(vals);
  } else if (ops[0] == Language::Operator::And ||
             ops[0] == Language::Operator::Or) {
    auto land_block = IR::Func::Current->AddBlock();
    std::vector<IR::Val> phi_args;
    phi_args.reserve(2 * exprs.size());
    bool is_or = (ops[0] == Language::Operator::Or);
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      auto val = exprs[i]->EmitIR(ctx);

      auto next_block = IR::Func::Current->AddBlock();
      IR::CondJump(val, is_or ? land_block : next_block,
                   is_or ? next_block : land_block);
      phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
      phi_args.push_back(IR::Val::Bool(is_or));

      IR::BasicBlock::Current = next_block;
    }

    phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
    phi_args.push_back(exprs.back()->EmitIR(ctx));
    IR::UncondJump(land_block);

    IR::BasicBlock::Current = land_block;
    auto phi           = IR::Phi(type::Bool);
    IR::Func::Current->SetArgs(phi, std::move(phi_args));
    return IR::Func::Current->Command(phi).reg();

  } else {
    if (ops.size() == 1) {
      auto lhs_ir = exprs[0]->EmitIR(ctx);
      auto rhs_ir = exprs[1]->EmitIR(ctx);
      auto val    = EmitChainOpPair(this, 0, lhs_ir, rhs_ir, ctx);
      return val;

    } else {
      std::vector<IR::Val> phi_args;
      auto lhs_ir     = exprs.front()->EmitIR(ctx);
      auto land_block = IR::Func::Current->AddBlock();
      for (size_t i = 0; i < ops.size() - 1; ++i) {
        auto rhs_ir = exprs[i + 1]->EmitIR(ctx);
        IR::Val cmp = EmitChainOpPair(this, i, lhs_ir, rhs_ir, ctx);

        phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
        phi_args.push_back(IR::Val::Bool(false));
        auto next_block = IR::Func::Current->AddBlock();
        IR::CondJump(cmp, next_block, land_block);
        IR::BasicBlock::Current = next_block;
        lhs_ir             = rhs_ir;
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = exprs.back()->EmitIR(ctx);
      auto last_cmp =
          EmitChainOpPair(this, exprs.size() - 2, lhs_ir, rhs_ir, ctx);
      phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
      phi_args.push_back(last_cmp);
      IR::UncondJump(land_block);

      IR::BasicBlock::Current = land_block;
      auto phi           = IR::Phi(type::Bool);
      IR::Func::Current->SetArgs(phi, std::move(phi_args));
      return IR::Func::Current->Command(phi).reg();
    }
  }
}

IR::Val ChainOp::EmitLVal(Context *ctx) { UNREACHABLE(this); }

ChainOp *ChainOp::Clone() const {
  auto *result = new ChainOp;
  result->span = span;
  result->ops  = ops;
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) { result->exprs.emplace_back(expr->Clone()); }
  return result;
}

}  // namespace AST
