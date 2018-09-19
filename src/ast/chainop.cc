#include "ast/chainop.h"

#include <numeric>

#include "ast/fn_args.h"
#include "ast/verify_macros.h"
#include "base/check.h"
#include "context.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"

base::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

namespace IR {
Val BlockSeq(base::vector<Val> const &blocks);
RegisterOr<type::Type const *> Variant(base::vector<Val> const &vals);
}  // namespace IR

namespace AST {
namespace {
using base::check::Is;
using base::check::Not;

IR::RegisterOr<bool> EmitChainOpPair(AST::ChainOp *chain_op, size_t index,
                                     IR::Val const &lhs_ir,
                                     IR::Val const &rhs_ir, Context *ctx) {
  const type::Type *lhs_type = chain_op->exprs[index]->type;
  const type::Type *rhs_type = chain_op->exprs[index + 1]->type;
  auto op                    = chain_op->ops[index];

  if (lhs_type->is<type::Array>() && rhs_type->is<type::Array>()) {
    ASSERT(op == Language::Operator::Eq || op == Language::Operator::Ne);
    return type::Array::Compare(&lhs_type->as<type::Array>(), lhs_ir,
                                &rhs_type->as<type::Array>(), rhs_ir,
                                op == Language::Operator::Eq, ctx)
        .reg_or<bool>();
  } else if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
    FnArgs<std::pair<Expression *, IR::Val>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(chain_op->exprs[index].get(), lhs_ir);
    args.pos_.emplace_back(chain_op->exprs[index + 1].get(), rhs_ir);

    auto results = EmitCallDispatch(args, chain_op->dispatch_tables_[index],
                                    type::Bool, ctx);
    ASSERT(results.size() == 1u);
    return results[0].reg_or<bool>();

  } else {
#define MAKE_OP(cpp_type, IcTypeCheck, OpName)                                 \
  if (lhs_ir.type IcTypeCheck) {                                               \
    return OpName(lhs_ir.reg_or<cpp_type>(), rhs_ir.reg_or<cpp_type>());       \
  }
    switch (op) {
      case Language::Operator::Lt:
        MAKE_OP(i32, == type::Int, IR::LtInt);
        MAKE_OP(double, == type::Real, IR::LtReal);
        MAKE_OP(IR::FlagsVal, ->is<type::Flags>(), IR::LtFlags);
        UNREACHABLE();
      case Language::Operator::Le:
        MAKE_OP(i32, == type::Int, IR::LeInt);
        MAKE_OP(double, == type::Real, IR::LeReal);
        MAKE_OP(IR::FlagsVal, ->is<type::Flags>(), IR::LeFlags);
        UNREACHABLE();
      case Language::Operator::Eq:
        MAKE_OP(bool, == type::Bool, IR::EqBool);
        MAKE_OP(char, == type::Char, IR::EqChar);
        MAKE_OP(i32, == type::Int, IR::EqInt);
        MAKE_OP(double, == type::Real, IR::EqReal);
        MAKE_OP(type::Type const *, == type::Type_, IR::EqType);
        MAKE_OP(IR::EnumVal, ->is<type::Enum>(), IR::EqEnum);
        MAKE_OP(IR::FlagsVal, ->is<type::Flags>(), IR::EqFlags);
        MAKE_OP(IR::Addr, ->is<type::Pointer>(), IR::EqAddr);
        {
          IR::BlockSequence const *val1 =
              std::get_if<IR::BlockSequence>(&lhs_ir.value);
          IR::BlockSequence const *val2 =
              std::get_if<IR::BlockSequence>(&rhs_ir.value);
          if (val1 != nullptr && val2 != nullptr) { return *val1 == *val2; }
        }
        UNREACHABLE();
      case Language::Operator::Ne:
        MAKE_OP(bool, == type::Bool, IR::XorBool);
        MAKE_OP(char, == type::Char, IR::NeChar);
        MAKE_OP(i32, == type::Int, IR::NeInt);
        MAKE_OP(double, == type::Real, IR::NeReal);
        MAKE_OP(type::Type const *, == type::Type_, IR::NeType);
        MAKE_OP(IR::EnumVal, ->is<type::Enum>(), IR::NeEnum);
        MAKE_OP(IR::FlagsVal, ->is<type::Flags>(), IR::NeFlags);
        MAKE_OP(IR::Addr, ->is<type::Pointer>(), IR::NeAddr);
        {
          IR::BlockSequence const *val1 =
              std::get_if<IR::BlockSequence>(&lhs_ir.value);
          IR::BlockSequence const *val2 =
              std::get_if<IR::BlockSequence>(&rhs_ir.value);
          if (val1 != nullptr && val2 != nullptr) { return *val1 == *val2; }
        }
        UNREACHABLE();
      case Language::Operator::Ge:
        MAKE_OP(i32, == type::Int, IR::GeInt);
        MAKE_OP(double, == type::Real, IR::GeReal);
        MAKE_OP(IR::FlagsVal, ->is<type::Flags>(), IR::GeFlags);
        UNREACHABLE();
      case Language::Operator::Gt:
        MAKE_OP(i32, == type::Int, IR::GtInt);
        MAKE_OP(double, == type::Real, IR::GtReal);
        MAKE_OP(IR::FlagsVal, ->is<type::Flags>(), IR::GtFlags);
        UNREACHABLE();
        // TODO case Language::Operator::And: cmp = lhs_ir; break;
      default: UNREACHABLE();
    }
#undef MAKE_OP
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

void ChainOp::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  bool found_err = false;

  for (auto &expr : exprs) {
    expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr->type == type::Err) { found_err = true; }
  }
  if (found_err) {
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  if (ops[0] == Language::Operator::Or) {
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      if (exprs[i]->type == type::Block) {
        ctx->error_log_.EarlyRequiredBlock(exprs[i]->span);
        type = type::Err;
      } else if (exprs[i]->type == type::OptBlock) {
        continue;
      } else {
        goto not_blocks;
      }
    }
    if (exprs.back()->type != type::Block &&
        exprs.back()->type != type::OptBlock) {
      goto not_blocks;
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

        if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
          FnArgs<Expression *> args;
          args.pos_ =
              base::vector<Expression *>{{exprs[i].get(), exprs[i + 1].get()}};
          // TODO overwriting type a bunch of times?
          std::tie(dispatch_tables_.at(i), type) =
              DispatchTable::Make(args, token, scope_, ctx);
          ASSERT(type, Not(Is<type::Tuple>()));
          if (type == type::Err) { limit_to(StageRange::Nothing()); }
        } else {
          if (lhs_type != rhs_type) {
            // TODO better error.
            ctx->error_log_.NoMatchingOperator(token, lhs_type, rhs_type, span);
            limit_to(StageRange::NoEmitIR());

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

void ChainOp::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->SaveReferences(scope, args); }
}

void ChainOp::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) {
    exprs[i]->contextualize(correspondant->as<ChainOp>().exprs[i].get(),
                            replacements);
  }
}

void ChainOp::ExtractReturns(base::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

base::vector<IR::Val> ChainOp::EmitIR(Context *ctx) {
  if (ops[0] == Language::Operator::Xor) {
    if (type == type::Bool) {
      return {IR::ValFrom(std::accumulate(
          exprs.begin(), exprs.end(), IR::RegisterOr<bool>(false),
          [&](IR::RegisterOr<bool> acc, auto &expr) {
            return IR::XorBool(acc,
                               expr->EmitIR(ctx)[0].template reg_or<bool>());
          }))};
    } else if (type->is<type::Flags>()) {
      return {IR::ValFrom(
          std::accumulate(
              exprs.begin(), exprs.end(),
              IR::RegisterOr<IR::FlagsVal>(IR::FlagsVal{0}),
              [&](IR::RegisterOr<IR::FlagsVal> acc, auto &expr) {
                return IR::XorFlags(
                    &type->as<type::Flags>(), acc,
                    expr->EmitIR(ctx)[0].template reg_or<IR::FlagsVal>());
              }),
          &type->as<type::Flags>())};
    } else {
      UNREACHABLE();
    }

  } else if (ops[0] == Language::Operator::Or && type->is<type::Flags>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIR(ctx)[0].reg_or<IR::FlagsVal>();
    while (++iter != exprs.end()) {
      val = IR::OrFlags(&type->as<type::Flags>(), val,
                        (*iter)->EmitIR(ctx)[0].reg_or<IR::FlagsVal>());
    }
    return {IR::ValFrom(val, &type->as<type::Flags>())};
  } else if (ops[0] == Language::Operator::And && type->is<type::Flags>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIR(ctx)[0].reg_or<IR::FlagsVal>();
    while (++iter != exprs.end()) {
      val = IR::AndFlags(&type->as<type::Flags>(), val,
                         (*iter)->EmitIR(ctx)[0].reg_or<IR::FlagsVal>());
    }
    return {IR::ValFrom(val, &type->as<type::Flags>())};
  } else if (ops[0] == Language::Operator::Or && type == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    base::vector<IR::Val> args;
    args.reserve(exprs.size());
    for (const auto &expr : exprs) { args.push_back(expr->EmitIR(ctx)[0]); }
    auto reg_or_type = IR::Variant(args);
    return {IR::ValFrom(reg_or_type)};
  } else if (ops[0] == Language::Operator::Or &&
             (type == type::Block || type == type::OptBlock)) {
    base::vector<IR::Val> vals;
    vals.reserve(exprs.size());
    for (auto &expr : exprs) { vals.push_back(expr->EmitIR(ctx)[0]); }
    return {IR::BlockSeq(vals)};
  } else if (ops[0] == Language::Operator::And ||
             ops[0] == Language::Operator::Or) {
    ASSERT(exprs[0]->type == type::Bool);

    auto land_block = IR::Func::Current->AddBlock();

    base::unordered_map<IR::BlockIndex, IR::RegisterOr<bool>> phi_args;
    bool is_or = (ops[0] == Language::Operator::Or);
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      auto val = exprs[i]->EmitIR(ctx)[0].reg_or<bool>();

      auto next_block = IR::Func::Current->AddBlock();
      IR::CondJump(val, is_or ? land_block : next_block,
                   is_or ? next_block : land_block);
      phi_args.emplace(IR::BasicBlock::Current, is_or);

      IR::BasicBlock::Current = next_block;
    }

    phi_args.emplace(IR::BasicBlock::Current,
                     exprs.back()->EmitIR(ctx)[0].reg_or<bool>());
    IR::UncondJump(land_block);

    IR::BasicBlock::Current = land_block;

    return {IR::ValFrom(IR::MakePhi<bool>(IR::Phi(type::Bool), phi_args))};

  } else {
    if (ops.size() == 1) {
      auto lhs_ir = exprs[0]->EmitIR(ctx)[0];
      auto rhs_ir = exprs[1]->EmitIR(ctx)[0];
      return {IR::ValFrom(EmitChainOpPair(this, 0, lhs_ir, rhs_ir, ctx))};

    } else {
      base::unordered_map<IR::BlockIndex, IR::RegisterOr<bool>> phi_args;
      auto lhs_ir     = exprs.front()->EmitIR(ctx)[0];
      auto land_block = IR::Func::Current->AddBlock();
      for (size_t i = 0; i < ops.size() - 1; ++i) {
        auto rhs_ir = exprs[i + 1]->EmitIR(ctx)[0];
        auto cmp = EmitChainOpPair(this, i, lhs_ir, rhs_ir, ctx);

        phi_args.emplace(IR::BasicBlock::Current, false);
        auto next_block = IR::Func::Current->AddBlock();
        IR::CondJump(cmp, next_block, land_block);
        IR::BasicBlock::Current = next_block;
        lhs_ir                  = rhs_ir;
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = exprs.back()->EmitIR(ctx)[0];
      phi_args.emplace(
          IR::BasicBlock::Current,
          EmitChainOpPair(this, exprs.size() - 2, lhs_ir, rhs_ir, ctx));
      IR::UncondJump(land_block);

      IR::BasicBlock::Current = land_block;

      return {IR::ValFrom(IR::MakePhi<bool>(IR::Phi(type::Bool), phi_args))};
    }
  }
}

base::vector<IR::Register> ChainOp::EmitLVal(Context *ctx) { UNREACHABLE(this); }

ChainOp *ChainOp::Clone() const {
  auto *result             = new ChainOp;
  result->span             = span;
  result->ops              = ops;
  result->dispatch_tables_ = dispatch_tables_;
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) { result->exprs.emplace_back(expr->Clone()); }
  return result;
}

}  // namespace AST
