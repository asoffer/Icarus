#include "ast/binop.h"

#include "ast/comma_list.h"
#include "ast/fn_args.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "base/check.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "type/array.h"
#include "type/char_buffer.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace IR {
Register Tup(base::vector<Val> const &entries);
}  // namespace IR

namespace type {
const Pointer *Ptr(const Type *);
}  // namespace type

namespace {
bool IsTypeOrTupleOfTypes(type::Type const *t) {
  if (t == type::Type_) { return true; }
  if (!t->is<type::Tuple>()) { return false; }
  auto &entries = t->as<type::Tuple>().entries_;
  return std::all_of(entries.begin(), entries.end(),
                     +[](type::Type const *ty) { return ty == type::Type_; });
}
}  // namespace

base::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

// TODO move this to some weird util lib?
void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn) {
  if (expr->is<AST::CommaList>()) {
    const auto &exprs = expr->as<AST::CommaList>().exprs;
    for (size_t i = 0; i < exprs.size(); ++i) { fn(i, exprs[i].get()); }
  } else {
    fn(0, expr);
  }
}

namespace AST {
using base::check::Not;
using base::check::Is;

std::string Binop::to_string(size_t n) const {
  std::stringstream ss;
  if (op == Language::Operator::Index) {
    ss << lhs->to_string(n) << "[" << rhs->to_string(n) << "]";
    return ss.str();
  }

  ss << "(" << lhs->to_string(n) << ")";
  switch (op) {
  case Language::Operator::Arrow: ss << " -> "; break;
  case Language::Operator::Add: ss << " + "; break;
  case Language::Operator::Sub: ss << " - "; break;
  case Language::Operator::Mul: ss << " * "; break;
  case Language::Operator::Div: ss << " / "; break;
  case Language::Operator::Mod: ss << " % "; break;
  case Language::Operator::Assign: ss << " <<:=>> "; break;
  case Language::Operator::OrEq: ss << " |= "; break;
  case Language::Operator::XorEq: ss << " ^= "; break;
  case Language::Operator::AndEq: ss << " &= "; break;
  case Language::Operator::AddEq: ss << " += "; break;
  case Language::Operator::SubEq: ss << " -= "; break;
  case Language::Operator::MulEq: ss << " *= "; break;
  case Language::Operator::DivEq: ss << " /= "; break;
  case Language::Operator::ModEq: ss << " %= "; break;
  case Language::Operator::As: ss << " as "; break;
  case Language::Operator::When: ss << " when "; break;
  default: UNREACHABLE();
  }
  ss << "(" << rhs->to_string(n) << ")";

  return ss.str();
}

void Binop::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  lhs->assign_scope(scope);
  rhs->assign_scope(scope);
}

void Binop::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  lhs->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  rhs->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  if (lhs->type == type::Err || rhs->type == type::Err) {
    type = type::Err;
    limit_to(lhs);
    limit_to(rhs);
    return;
  }

  using Language::Operator;
  // TODO if lhs is reserved?
  if (op == Operator::Assign) {
    if (lhs->type->is<type::Tuple>()) {
      if (rhs->type->is<type::Tuple>()) {
        const auto &lhs_entries_ = lhs->type->as<type::Tuple>().entries_;
        const auto &rhs_entries_ = rhs->type->as<type::Tuple>().entries_;

        if (lhs_entries_.size() != rhs_entries_.size()) {
          NOT_YET("error message");
        } else {
          for (size_t i = 0; i < lhs_entries_.size(); ++i) {
            if (!type::CanCastImplicitly(rhs_entries_[i], lhs_entries_[i])) {
              NOT_YET("log an error");
              limit_to(StageRange::NoEmitIR());
            }
          }
        }
      } else {
        LOG << lhs;
        LOG << rhs;
        NOT_YET("error message");
      }
    } else {
      if (rhs->type->is<type::Tuple>()){
        LOG << lhs;
        LOG << rhs;
        NOT_YET("error message");
      } else {
        if (!type::CanCastImplicitly(rhs->type, lhs->type)) {
          NOT_YET("log an error");
          limit_to(StageRange::NoEmitIR());
        }
      }
    }

    return;
  }

  switch (op) {
    case Operator::Index: {
      type = type::Err;
      if (lhs->type->is<type::CharBuffer>()) {
        if (rhs->type != type::Int) {
          ctx->error_log_.InvalidCharBufIndex(span, rhs->type);
          limit_to(StageRange::NoEmitIR());
        }
        type = type::Char;  // Assuming it's a char, even if the index type was
                            // wrong.
        return;
      } else if (!lhs->type->is<type::Array>()) {
        ctx->error_log_.IndexingNonArray(span, lhs->type);
        limit_to(StageRange::NoEmitIR());
        return;
      } else {
        type = lhs->type->as<type::Array>().data_type;

        if (rhs->type == type::Int) { break; }
        ctx->error_log_.NonIntegralArrayIndex(span, rhs->type);
        limit_to(StageRange::NoEmitIR());
        return;
      }
    } break;
    case Operator::As: {
      // TODO check that the type actually can be cast
      // correctly.
      type   = backend::EvaluateAs<const type::Type *>(rhs.get(), ctx);
    } break;
    case Operator::XorEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Flags>() && rhs->type == lhs->type) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.XorEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;
    case Operator::AndEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Flags>() && rhs->type == lhs->type) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.AndEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;
    case Operator::OrEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Flags>() && rhs->type == lhs->type) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.OrEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;

#define CASE(OpName, symbol, ret_type)                                         \
  case Operator::OpName: {                                                     \
    if ((lhs->type == type::Int && rhs->type == type::Int) ||                  \
        (lhs->type == type::Real && rhs->type == type::Real)) {                \
      type = ret_type;                                                         \
    } else {                                                                   \
      FnArgs<Expression *> args;                                               \
      args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};          \
      std::tie(dispatch_table_, type) =                                        \
          DispatchTable::Make(args, symbol, scope_, ctx);                      \
      ASSERT(type, Not(Is<type::Tuple>()));                                    \
      if (type == type::Err) {                                                 \
        ctx->error_log_.NoMatchingOperator(symbol, lhs->type, rhs->type,       \
                                           span);                              \
        limit_to(StageRange::Nothing());                                       \
      }                                                                        \
    }                                                                          \
  } break;

      CASE(Sub, "-", lhs->type);
      CASE(Div, "/", lhs->type);
      CASE(Mod, "%", lhs->type);
      CASE(SubEq, "-=", type::Void());
      CASE(MulEq, "*=", type::Void());
      CASE(DivEq, "/=", type::Void());
      CASE(ModEq, "%=", type::Void());
#undef CASE
    case Operator::Add: {
      if ((lhs->type == type::Int && rhs->type == type::Int) ||
          (lhs->type == type::Real && rhs->type == type::Real) ||
          (lhs->type == type::Code && rhs->type == type::Code)) {
        type = lhs->type;
      } else if (lhs->type->is<type::CharBuffer>() &&
                 rhs->type->is<type::CharBuffer>()) {
        type = type::CharBuf(lhs->type->as<type::CharBuffer>().length_ +
                             rhs->type->as<type::CharBuffer>().length_);
      } else {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};
        std::tie(dispatch_table_, type) =
            DispatchTable::Make(args, "+", scope_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) {
          ctx->error_log_.NoMatchingOperator("+", lhs->type, rhs->type, span);
          limit_to(StageRange::Nothing());
        }
      }
    } break;
    case Operator::AddEq: {
      if ((lhs->type == type::Int && rhs->type == type::Int) ||
          (lhs->type == type::Real && rhs->type == type::Real) ||
          (lhs->type == type::Code &&
           rhs->type == type::Code)) { /* TODO type::Code should only be valid
                                          for Add, not Sub, etc */
        type = type::Void();
      } else {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression*>{{lhs.get(), rhs.get()}};
        std::tie(dispatch_table_, type) =
            DispatchTable::Make(args, "+=", scope_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) { limit_to(StageRange::Nothing()); }
      }
    } break;
    // Mul is done separately because of the function composition
    case Operator::Mul: {
      if ((lhs->type == type::Int && rhs->type == type::Int) ||
          (lhs->type == type::Real && rhs->type == type::Real)) {
        type = lhs->type;

      } else if (lhs->type->is<type::Function>() &&
                 rhs->type->is<type::Function>()) {
        auto *lhs_fn = &lhs->type->as<type::Function>();
        auto *rhs_fn = &rhs->type->as<type::Function>();
        if (rhs_fn->output == lhs_fn->input) {
          type = type::Func({rhs_fn->input}, {lhs_fn->output});

        } else {
          type = type::Err;
          ctx->error_log_.NonComposableFunctions(span);
          limit_to(StageRange::Nothing());
          return;
        }

      } else {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression*>{{lhs.get(), rhs.get()}};
        std::tie(dispatch_table_, type) =
            DispatchTable::Make(args, "*", scope_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) {
          ctx->error_log_.NoMatchingOperator("+", lhs->type, rhs->type, span);
          limit_to(StageRange::Nothing());
        }
      }
    } break;
    case Operator::Arrow: {
      if (!IsTypeOrTupleOfTypes(lhs->type)) {
        type = type::Err;
        ctx->error_log_.NonTypeFunctionInput(span);
        limit_to(StageRange::Nothing());
        return;
      } else {
        type = type::Type_;
      }

      if (!IsTypeOrTupleOfTypes(rhs->type)) {
        type = type::Err;
        ctx->error_log_.NonTypeFunctionOutput(span);
        limit_to(StageRange::Nothing());
        return;
      } else {
        type = type::Type_;
      }

      if (type != type::Err) { type = type::Type_; }
    } break;
    default: UNREACHABLE();
  }
}

void Binop::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  lhs->Validate(ctx);
  rhs->Validate(ctx);
}

void Binop::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  lhs->SaveReferences(scope, args);
  rhs->SaveReferences(scope, args);
}

void Binop::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  lhs->contextualize(correspondant->as<Binop>().lhs.get(), replacements);
  rhs->contextualize(correspondant->as<Binop>().rhs.get(), replacements);
}

void Binop::ExtractReturns(base::vector<const Expression *> *rets) const {
  lhs->ExtractReturns(rets);
  rhs->ExtractReturns(rets);
}

base::vector<IR::Val> AST::Binop::EmitIR(Context *ctx) {
  if (op != Language::Operator::Assign &&
      (lhs->type->is<type::Struct>() || rhs->type->is<type::Struct>())) {
    // TODO struct is not exactly right. we really mean user-defined
    AST::FnArgs<std::pair<AST::Expression *, IR::Val>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(lhs.get(), lhs->EmitIR(ctx)[0]);
    args.pos_.emplace_back(rhs.get(), rhs->EmitIR(ctx)[0]);

    ASSERT(type != nullptr);
    return EmitCallDispatch(args, dispatch_table_, type, ctx);
  }

  switch (op) {
    case Language::Operator::Add: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        return {IR::ValFrom(
            IR::AddInt(lhs_ir.reg_or<i32>(), rhs_ir.reg_or<i32>()))};
      } else if (rhs_ir.type == type::Real) {
        return {IR::ValFrom(
            IR::AddReal(lhs_ir.reg_or<double>(), rhs_ir.reg_or<double>()))};
      } else if (rhs_ir.type->is<type::CharBuffer>()) {
        NOT_YET();
      } else {
        UNREACHABLE(rhs_ir.type);
      }
    } break;
    case Language::Operator::Sub: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        return {IR::ValFrom(
            IR::SubInt(lhs_ir.reg_or<i32>(), rhs_ir.reg_or<i32>()))};
      } else if (rhs_ir.type == type::Real) {
        return {IR::ValFrom(
            IR::SubReal(lhs_ir.reg_or<double>(), rhs_ir.reg_or<double>()))};
      } else {
        UNREACHABLE(rhs_ir.type);
      }
    } break;
    case Language::Operator::Mul: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        return {IR::ValFrom(
            IR::MulInt(lhs_ir.reg_or<i32>(), rhs_ir.reg_or<i32>()))};
      } else if (rhs_ir.type == type::Real) {
        return {IR::ValFrom(
            IR::MulReal(lhs_ir.reg_or<double>(), rhs_ir.reg_or<double>()))};
      } else {
        UNREACHABLE(rhs_ir.type);
      }
    } break;
    case Language::Operator::Div: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        return {IR::ValFrom(
            IR::DivInt(lhs_ir.reg_or<i32>(), rhs_ir.reg_or<i32>()))};
      } else if (rhs_ir.type == type::Real) {
        return {IR::ValFrom(
            IR::DivReal(lhs_ir.reg_or<double>(), rhs_ir.reg_or<double>()))};
      } else {
        UNREACHABLE(rhs_ir.type);
      }
    } break;
    case Language::Operator::Mod: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        return {IR::ValFrom(
            IR::ModInt(lhs_ir.reg_or<i32>(), rhs_ir.reg_or<i32>()))};
      } else if (rhs_ir.type == type::Real) {
        return {IR::ValFrom(
            IR::ModReal(lhs_ir.reg_or<double>(), rhs_ir.reg_or<double>()))};
      } else {
        UNREACHABLE(rhs_ir.type);
      }
    } break;
    case Language::Operator::As: {
      auto val = lhs->EmitIR(ctx)[0];
      if (val.type == type) {
        return {val};
      } else if (i32 const *n = std::get_if<i32>(&val.value);
                 n && type == type::Real) {
        return {IR::Val(static_cast<double>(*n))};
      } else if (type->is<type::Variant>()) {
        auto alloc = IR::Alloca(type);
        type->EmitAssign(val.type, std::move(val), alloc, ctx);
        return {IR::Val::Reg(alloc, type)};
      } else if (val.type->is<type::Pointer>()) {
        auto *ptee_type = val.type->as<type::Pointer>().pointee;
        if (ptee_type->is<type::Array>()) {
          auto &array_type = ptee_type->as<type::Array>();
          if (array_type.fixed_length &&
              type::Ptr(array_type.data_type) == type) {
            IR::Val v_copy  = val;
            v_copy.type = type;
            return {v_copy};
          }
        }
      }

      if (type == type::Real && val.type == type::Int) {
        return {IR::ValFrom(IR::CastIntToReal(val.reg_or<i32>()))};
      } else if (type->is<type::Pointer>()) {
        return {IR::Val::Reg(IR::CastPtr(std::get<IR::Register>(val.value),
                                         &type->as<type::Pointer>()),
                             type)};

      } else {
        UNREACHABLE();
      }

    } break;
    case Language::Operator::Arrow: {
      auto reg_or_type =
          IR::Arrow(IR::Tup(lhs->EmitIR(ctx)), IR::Tup(rhs->EmitIR(ctx)));
      return {IR::ValFrom(reg_or_type)};
    } break;
    case Language::Operator::Assign: {
      base::vector<const type::Type *> lhs_types, rhs_types;
      ForEachExpr(rhs.get(), [&ctx, &rhs_types](size_t, Expression *expr) {
        if (expr->type->is<type::Tuple>()) {
          rhs_types.insert(rhs_types.end(),
                           expr->type->as<type::Tuple>().entries_.begin(),
                           expr->type->as<type::Tuple>().entries_.end());
        } else {
          rhs_types.push_back(expr->type);
        }
      });
      auto rhs_vals = rhs->EmitIR(ctx);

      // TODO types can be retrieved from the values?
      ForEachExpr(lhs.get(), [&ctx, &lhs_types](size_t, AST::Expression *expr) {
        lhs_types.push_back(expr->type);
      });
      auto lhs_lvals = lhs->EmitLVal(ctx);

      ASSERT(lhs_lvals.size() == rhs_vals.size());
      ASSERT(lhs_lvals.size() == lhs_types.size());
      ASSERT(lhs_lvals.size() == rhs_types.size());
      for (size_t i = 0; i < lhs_lvals.size(); ++i) {
        lhs_types[i]->EmitAssign(rhs_types[i], rhs_vals[i], lhs_lvals[i], ctx);
      }
      return {};
    } break;
    case Language::Operator::OrEq: {
      if (type->is<type::Flags>()) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        IR::StoreFlags(
            IR::OrFlags(&type->as<type::Flags>(), IR::LoadFlags(lhs_lval, type),
                        rhs->EmitIR(ctx)[0].reg_or<IR::FlagsVal>()),
            lhs_lval);
        return {};
      }
      auto land_block = IR::Func::Current->AddBlock();
      auto more_block = IR::Func::Current->AddBlock();

      auto lhs_val       = lhs->EmitIR(ctx)[0].reg_or<bool>();
      auto lhs_end_block = IR::BasicBlock::Current;
      IR::CondJump(lhs_val, land_block, more_block);

      IR::BasicBlock::Current = more_block;
      auto rhs_val            = rhs->EmitIR(ctx)[0].reg_or<bool>();
      auto rhs_end_block      = IR::BasicBlock::Current;
      IR::UncondJump(land_block);

      IR::BasicBlock::Current = land_block;

      return {IR::ValFrom(IR::MakePhi<bool>(
          IR::Phi(type::Bool),
          {{lhs_end_block, true}, {rhs_end_block, rhs_val}}))};
    } break;
    case Language::Operator::AndEq: {
      if (type->is<type::Flags>()) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        IR::StoreFlags(IR::AndFlags(&type->as<type::Flags>(),
                                    IR::LoadFlags(lhs_lval, type),
                                    rhs->EmitIR(ctx)[0].reg_or<IR::FlagsVal>()),
                       lhs_lval);
        return {};
      }

      auto land_block = IR::Func::Current->AddBlock();
      auto more_block = IR::Func::Current->AddBlock();

      auto lhs_val       = lhs->EmitIR(ctx)[0].reg_or<bool>();
      auto lhs_end_block = IR::BasicBlock::Current;
      IR::CondJump(lhs_val, more_block, land_block);

      IR::BasicBlock::Current = more_block;
      auto rhs_val            = rhs->EmitIR(ctx)[0].reg_or<bool>();
      auto rhs_end_block      = IR::BasicBlock::Current;
      IR::UncondJump(land_block);

      IR::BasicBlock::Current = land_block;

      return {IR::ValFrom(IR::MakePhi<bool>(
          IR::Phi(type::Bool),
          {{lhs_end_block, rhs_val}, {rhs_end_block, false}}))};
    } break;
    case Language::Operator::AddEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        IR::StoreInt(IR::AddInt(IR::LoadInt(lhs_lval), rhs_ir.reg_or<i32>()),
                     lhs_lval);
      } else if (rhs_ir.type == type::Real) {
        IR::StoreReal(
            IR::AddReal(IR::LoadReal(lhs_lval), rhs_ir.reg_or<double>()),
            lhs_lval);
      } else if (rhs_ir.type->is<type::CharBuffer>()) {
        NOT_YET();
      } else {
        UNREACHABLE(rhs_ir.type);
      }
      return {};
    } break;
    case Language::Operator::SubEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        IR::StoreInt(IR::SubInt(IR::LoadInt(lhs_lval), rhs_ir.reg_or<i32>()),
                     lhs_lval);
      } else if (rhs_ir.type == type::Real) {
        IR::StoreReal(
            IR::SubReal(IR::LoadReal(lhs_lval), rhs_ir.reg_or<double>()),
            lhs_lval);
      } else {
        UNREACHABLE(rhs_ir.type);
      }
      return {};
    } break;
    case Language::Operator::DivEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        IR::StoreInt(IR::DivInt(IR::LoadInt(lhs_lval), rhs_ir.reg_or<i32>()),
                     lhs_lval);
      } else if (rhs_ir.type == type::Real) {
        IR::StoreReal(
            IR::DivReal(IR::LoadReal(lhs_lval), rhs_ir.reg_or<double>()),
            lhs_lval);
      } else {
        UNREACHABLE(rhs_ir.type);
      }
      return {};
    } break;
    case Language::Operator::ModEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        IR::StoreInt(IR::ModInt(IR::LoadInt(lhs_lval), rhs_ir.reg_or<i32>()),
                     lhs_lval);
      } else if (rhs_ir.type == type::Real) {
        IR::StoreReal(
            IR::ModReal(IR::LoadReal(lhs_lval), rhs_ir.reg_or<double>()),
            lhs_lval);
      } else {
        UNREACHABLE(rhs_ir.type);
      }
      return {};
    } break;
    case Language::Operator::MulEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      if (rhs_ir.type == type::Int) {
        IR::StoreInt(IR::MulInt(IR::LoadInt(lhs_lval), rhs_ir.reg_or<i32>()),
                     lhs_lval);
      } else if (rhs_ir.type == type::Real) {
        IR::StoreReal(
            IR::MulReal(IR::LoadReal(lhs_lval), rhs_ir.reg_or<double>()),
            lhs_lval);
      } else {
        UNREACHABLE(rhs_ir.type);
      }
      return {};
    } break;
    case Language::Operator::XorEq: {
      if (lhs->type == type::Bool) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        auto rhs_ir   = rhs->EmitIR(ctx)[0].reg_or<bool>();
        IR::StoreBool(IR::XorBool(IR::LoadBool(lhs_lval), rhs_ir), lhs_lval);
      } else if (lhs->type->is<type::Flags>()) {
        auto *flags_type = &lhs->type->as<type::Flags>();
        auto lhs_lval    = lhs->EmitLVal(ctx)[0];
        auto rhs_ir      = rhs->EmitIR(ctx)[0].reg_or<IR::FlagsVal>();
        IR::StoreFlags(
            IR::XorFlags(flags_type, IR::LoadFlags(lhs_lval, flags_type),
                         rhs_ir),
            lhs_lval);
      } else {
        UNREACHABLE(lhs->type);
      }
      return {};
    } break;
    case Language::Operator::Index:
      return {IR::Val::Reg(IR::PtrFix(EmitLVal(ctx)[0], type), type)};
    default: UNREACHABLE(*this);
  }
}

base::vector<IR::Register> AST::Binop::EmitLVal(Context *ctx) {
  switch (op) {
    case Language::Operator::As: NOT_YET();
    case Language::Operator::Index:
      if (lhs->type->is<type::Array>()) {
        return {IR::Index(type::Ptr(this->type), lhs->EmitLVal(ctx)[0],
                          rhs->EmitIR(ctx)[0].reg_or<i32>())};
      }
      [[fallthrough]];
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

Binop *Binop::Clone() const {
  auto *result = new Binop;
  result->span = span;
  result->lhs  = base::wrap_unique(lhs->Clone());
  result->rhs  = base::wrap_unique(rhs->Clone());
  result->op   = op;
  return result;
}


}  // namespace AST
