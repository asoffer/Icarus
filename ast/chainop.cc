#include "ast/chainop.h"

#include <numeric>

#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"

namespace ir {
Val BlockSeq(std::vector<RegisterOr<BlockSequence>> const &blocks);
RegisterOr<type::Type const *> Variant(
    std::vector<RegisterOr<type::Type const *>> const &vals);
}  // namespace ir

namespace ast {
namespace {

ir::RegisterOr<bool> EmitChainOpPair(ast::ChainOp *chain_op, size_t index,
                                     ir::Results const &lhs_ir,
                                     ir::Results const &rhs_ir, Context *ctx) {
  auto *lhs_type = ctx->type_of(chain_op->exprs[index].get());
  auto *rhs_type = ctx->type_of(chain_op->exprs[index + 1].get());
  auto op        = chain_op->ops[index];

  if (lhs_type->is<type::Array>() && rhs_type->is<type::Array>()) {
    using ::matcher::Eq;
    ASSERT(op, Eq(frontend::Operator::Eq) || Eq(frontend::Operator::Ne));
    return type::Array::Compare(&lhs_type->as<type::Array>(), lhs_ir,
                                &rhs_type->as<type::Array>(), rhs_ir,
                                op == frontend::Operator::Eq, ctx)
        .get<bool>(0);
  } else if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
    auto results =
        ASSERT_NOT_NULL(ctx->rep_dispatch_tables(chain_op))
            ->at(index)
            .EmitCall(core::FnArgs<std::pair<Expression *, ir::Results>>(
                          {std::pair(chain_op->exprs[index].get(), lhs_ir),
                           std::pair(chain_op->exprs[index + 1].get(), rhs_ir)},
                          {}),
                      type::Bool, ctx);
    ASSERT(results.size() == 1u);
    return results.get<bool>(0);

  } else {
    switch (op) {
      case frontend::Operator::Lt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Lt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Le:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Le(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Eq:
        if (lhs_type->is<type::Block>() || lhs_type == type::OptBlock ||
            lhs_type == type::RepBlock) {
          auto val1 = lhs_ir.get<ir::BlockSequence>(0);
          auto val2 = rhs_ir.get<ir::BlockSequence>(0);
          if (!val1.is_reg_ && !val2.is_reg_) { return val1.val_ == val2.val_; }
        }
        return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto type_holder) {
              using T = typename decltype(type_holder)::type;
              return ir::Eq(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ne:
        if (lhs_type->is<type::Block>() || lhs_type == type::OptBlock ||
            lhs_type == type::RepBlock) {
          auto val1 = lhs_ir.get<ir::BlockSequence>(0);
          auto val2 = rhs_ir.get<ir::BlockSequence>(0);
          if (!val1.is_reg_ && !val2.is_reg_) { return val1.val_ == val2.val_; }
      }
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                type::Type const *, ir::EnumVal, ir::FlagsVal,
                                ir::Addr>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Ne(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Ge:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Ge(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Gt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Gt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
        // TODO case frontend::Operator::And: cmp = lhs_ir; break;
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
      case frontend::Operator::Or: ss << " | "; break;
      case frontend::Operator::Xor: ss << " ^ "; break;
      case frontend::Operator::And: ss << " & "; break;
      case frontend::Operator::Lt: ss << " < "; break;
      case frontend::Operator::Le: ss << " <= "; break;
      case frontend::Operator::Eq: ss << " == "; break;
      case frontend::Operator::Ne: ss << " != "; break;
      case frontend::Operator::Ge: ss << " >= "; break;
      case frontend::Operator::Gt: ss << " > "; break;
      default: UNREACHABLE();
    }
  }
  ss << exprs.back()->to_string(n) << ")";
  return ss.str();
}

void ChainOp::assign_scope(core::Scope *scope) {
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void ChainOp::DependentDecls(base::Graph<Declaration *> *g,
                             Declaration *d) const {
  for (auto const &expr : exprs) { expr->DependentDecls(g, d); }
}

VerifyResult ChainOp::VerifyType(Context *ctx) {
  std::vector<VerifyResult> results;
  results.reserve(exprs.size());
  for (auto &expr : exprs) { results.push_back(expr->VerifyType(ctx)); }
  if (std::any_of(results.begin(), results.end(),
                  [](VerifyResult const &v) { return !v.ok(); })) {
    return VerifyResult::Error();
  }

  if (ops[0] == frontend::Operator::Or) {
    bool found_err = false;
    for (size_t i = 0; i < results.size() - 1; ++i) {
      if (results[i].type_->is<type::Block>()) {
        if (!results[i].const_) { NOT_YET("log an error: non const block"); }

        ctx->error_log()->EarlyRequiredBlock(exprs[i]->span);
        found_err = true;
      } else if (results[i].type_ == type::OptBlock) {
        if (!results[i].const_) { NOT_YET("log an error: non const block"); }

        continue;
      } else {
        goto not_blocks;
      }
    }
    if (found_err) { return VerifyResult::Error(); }
    auto &last = results.back();
    if (!last.type_->is<type::Block>() && last.type_ != type::OptBlock) {
      goto not_blocks;
    } else if (!results.back().const_) {
      NOT_YET("log an error: non const block");
    } else {
      return ctx->set_result(this, VerifyResult::Constant(last.type_));
    }
  }
not_blocks:

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  switch (ops[0]) {
    case frontend::Operator::Or:
    case frontend::Operator::And:
    case frontend::Operator::Xor: {
      bool failed                       = false;
      bool is_const                     = true;
      type::Type const *first_expr_type = results[0].type_;

      for (auto &result : results) {
        // TODO this collection of error messages could be greatly improved.
        if (result.type_ != first_expr_type) {
          auto op_str = [this] {
            switch (ops[0]) {
              case frontend::Operator::Or: return "|";
              case frontend::Operator::And: return "&";
              case frontend::Operator::Xor: return "^";
              default: UNREACHABLE();
            }
          }();

          NOT_YET("Log an error");
          is_const &= result.const_;
          failed = true;
        }
      }

      if (failed) { return VerifyResult::Error(); }
      return ctx->set_result(this, VerifyResult(first_expr_type, is_const));
    } break;
    default: {
      bool is_const = results[0].const_;
      ASSERT(exprs.size() >= 2u);
      for (size_t i = 0; i < exprs.size() - 1; ++i) {
        VerifyResult const &lhs_result = results[i];
        VerifyResult const &rhs_result = results[i + 1];
        is_const &= rhs_result.const_;

        // TODO struct is wrong. generally user-defined (could be array of
        // struct too, or perhaps a variant containing a struct?) need to
        // figure out the details here.
        const char *token = nullptr;
        switch (ops[i]) {
          case frontend::Operator::Lt: token = "<"; break;
          case frontend::Operator::Le: token = "<="; break;
          case frontend::Operator::Eq: token = "=="; break;
          case frontend::Operator::Ne: token = "!="; break;
          case frontend::Operator::Ge: token = ">="; break;
          case frontend::Operator::Gt: token = ">"; break;
          default: UNREACHABLE();
        }

        if (lhs_result.type_->is<type::Struct>() ||
            lhs_result.type_->is<type::Struct>()) {
          // TODO overwriting type a bunch of times?
          OverloadSet os(scope_, token, ctx);
          os.add_adl(token, lhs_result.type_);
          os.add_adl(token, rhs_result.type_);

          auto *ret_type = DispatchTable::MakeOrLogError(
              this,
              core::FnArgs<Expression *>({exprs[i].get(), exprs[i + 1].get()}, {}),
              os, ctx, true);
          if (ret_type == nullptr) { return VerifyResult::Error(); }
          if (ret_type->is<type::Tuple>()) { NOT_YET(); }
          // TODO check that ret_type is a bool?
        } else {
          if (lhs_result.type_ != rhs_result.type_) {
            NOT_YET("Log an error", lhs_result.type_, rhs_result.type_, this);

          } else {
            auto cmp = lhs_result.type_->Comparator();

            switch (ops[i]) {
              case frontend::Operator::Eq:
              case frontend::Operator::Ne: {
                switch (cmp) {
                  case type::Cmp::Order:
                  case type::Cmp::Equality: continue;
                  case type::Cmp::None:
                    ctx->error_log()->ComparingIncomparables(
                        lhs_result.type_, rhs_result.type_,
                        TextSpan(exprs[i]->span, exprs[i + 1]->span));
                    return VerifyResult::Error();
                }
              } break;
              case frontend::Operator::Lt:
              case frontend::Operator::Le:
              case frontend::Operator::Ge:
              case frontend::Operator::Gt: {
                switch (cmp) {
                  case type::Cmp::Order: continue;
                  case type::Cmp::Equality:
                  case type::Cmp::None:
                    ctx->error_log()->ComparingIncomparables(
                        lhs_result.type_, rhs_result.type_,
                        TextSpan(exprs[i]->span, exprs[i + 1]->span));
                    return VerifyResult::Error();
                }
              } break;
              default: UNREACHABLE("Expecting a ChainOp operator type.");
            }
          }
        }
      }

      return ctx->set_result(this, VerifyResult(type::Bool, is_const));
    }
  }
}

void ChainOp::ExtractJumps(JumpExprs *rets) const {
  for (auto &expr : exprs) { expr->ExtractJumps(rets); }
}

ir::Results ChainOp::EmitIr(Context *ctx) {
  auto *t = ctx->type_of(this);
  if (ops[0] == frontend::Operator::Xor) {
    if (t == type::Bool) {
      return ir::Results{std::accumulate(
          exprs.begin(), exprs.end(), ir::RegisterOr<bool>(false),
          [&](ir::RegisterOr<bool> acc, auto &expr) {
            return ir::XorBool(acc, expr->EmitIr(ctx).template get<bool>(0));
          })};
    } else if (t->is<type::Flags>()) {
      return ir::Results{
          std::accumulate(exprs.begin(), exprs.end(),
                          ir::RegisterOr<ir::FlagsVal>(ir::FlagsVal{0}),
                          [&](ir::RegisterOr<ir::FlagsVal> acc, auto &expr) {
                            return ir::XorFlags(
                                &t->as<type::Flags>(), acc,
                                expr->EmitIr(ctx).template get<ir::FlagsVal>(
                                    0));
                          })};
    } else {
      UNREACHABLE();
    }

  } else if (ops[0] == frontend::Operator::Or && t->is<type::Flags>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0);
    while (++iter != exprs.end()) {
      val = ir::OrFlags(&t->as<type::Flags>(), val,
                        (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (ops[0] == frontend::Operator::And && t->is<type::Flags>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0);
    while (++iter != exprs.end()) {
      val = ir::AndFlags(&t->as<type::Flags>(), val,
                         (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (ops[0] == frontend::Operator::Or && t == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<ir::RegisterOr<type::Type const *>> args;
    args.reserve(exprs.size());
    for (const auto &expr : exprs) {
      args.push_back(expr->EmitIr(ctx).get<type::Type const *>(0));
    }
    auto reg_or_type = ir::Variant(args);
    return ir::Results{reg_or_type};
  } else if (ops[0] == frontend::Operator::Or &&
             (t->is<type::Block>() || t == type::OptBlock)) {
    std::vector<ir::RegisterOr<ir::BlockSequence>> vals;
    vals.reserve(exprs.size());
    for (auto &expr : exprs) {
      vals.push_back(expr->EmitIr(ctx).get<ir::BlockSequence>(0));
    }
    return ir::Results{ir::BlockSeq(vals)};
  } else if (ops[0] == frontend::Operator::And ||
             ops[0] == frontend::Operator::Or) {
    auto land_block = ir::Func::Current->AddBlock();

    absl::flat_hash_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_args;
    bool is_or = (ops[0] == frontend::Operator::Or);
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      auto val = exprs[i]->EmitIr(ctx).get<bool>(0);

      auto next_block = ir::Func::Current->AddBlock();
      ir::CondJump(val, is_or ? land_block : next_block,
                   is_or ? next_block : land_block);
      phi_args.emplace(ir::BasicBlock::Current, is_or);

      ir::BasicBlock::Current = next_block;
    }

    phi_args.emplace(ir::BasicBlock::Current,
                     exprs.back()->EmitIr(ctx).get<bool>(0));
    ir::UncondJump(land_block);

    ir::BasicBlock::Current = land_block;

    return ir::Results{ir::MakePhi<bool>(ir::Phi(type::Bool), phi_args)};

  } else {
    if (ops.size() == 1) {
      auto lhs_ir = exprs[0]->EmitIr(ctx);
      auto rhs_ir = exprs[1]->EmitIr(ctx);
      return ir::Results{EmitChainOpPair(this, 0, lhs_ir, rhs_ir, ctx)};

    } else {
      absl::flat_hash_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_args;
      auto lhs_ir     = exprs.front()->EmitIr(ctx);
      auto land_block = ir::Func::Current->AddBlock();
      for (size_t i = 0; i < ops.size() - 1; ++i) {
        auto rhs_ir = exprs[i + 1]->EmitIr(ctx);
        auto cmp    = EmitChainOpPair(this, i, lhs_ir, rhs_ir, ctx);

        phi_args.emplace(ir::BasicBlock::Current, false);
        auto next_block = ir::Func::Current->AddBlock();
        ir::CondJump(cmp, next_block, land_block);
        ir::BasicBlock::Current = next_block;
        lhs_ir                  = std::move(rhs_ir);
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = exprs.back()->EmitIr(ctx);
      phi_args.emplace(
          ir::BasicBlock::Current,
          EmitChainOpPair(this, exprs.size() - 2, lhs_ir, rhs_ir, ctx));
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return ir::Results{ir::MakePhi<bool>(ir::Phi(type::Bool), phi_args)};
    }
  }
}

}  // namespace ast
