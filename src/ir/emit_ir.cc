#include "func.h"

#include <algorithm>
#include <optional>

#include "../ast/stages.h"
#include "../context.h"
#include "../error/log.h"
#include "../type/all.h"
#include "ast/ast.h"
#include "ast/declaration.h"
#include "module.h"

using base::check::Is;

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

// If the expression is a CommaList, apply the function to each expr. Otherwise
// call it on the expression itself.
void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn) {
  if (expr->is<AST::CommaList>()) {
    const auto &exprs = expr->as<AST::CommaList>().exprs;
    for (size_t i = 0; i < exprs.size(); ++i) { fn(i, exprs[i].get()); }
  } else {
    fn(0, expr);
  }
}

// TODO using nullptr for module. Is that safe here? Is it correct?

IR::Val AST::Access::EmitLVal(Context *ctx) {
  auto val = operand->EmitLVal(ctx);
  while (val.type->is<type::Pointer>() &&
         !val.type->as<type::Pointer>().pointee->is_big()) {
    val = IR::Load(val);
  }

  ASSERT(val.type, Is<type::Pointer>());
  ASSERT(val.type->as<type::Pointer>().pointee, Is<type::Struct>());

  auto *struct_type =
      &val.type->as<type::Pointer>().pointee->as<type::Struct>();
  return IR::Field(val, struct_type->field_indices_ AT(member_name));
}

IR::Val AST::Node::EmitIR(Context *) { UNREACHABLE(*this); }
IR::Val AST::Expression::EmitIR(Context *) { UNREACHABLE(*this); }
IR::Val AST::Expression::EmitLVal(Context *) { UNREACHABLE(*this); }

std::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

IR::Val AST::Access::EmitIR(Context *ctx) {
  if (operand->type == type::Module) {
    auto mod =
        std::get<const Module *>(Evaluate(operand.get(), ctx) AT(0).value);
    return mod->GetDecl(member_name)->EmitIR(ctx);
  } else if (type->is<type::Enum>()) {
    return type->as<type::Enum>().EmitLiteral(member_name);
  } else {
    return PtrCallFix(EmitLVal(ctx));
  }
  return IR::Val::None();
}

IR::Val AST::Binop::EmitIR(Context *ctx) {
  if (op != Language::Operator::Assign &&
      (lhs->type->is<type::Struct>() || rhs->type->is<type::Struct>())) {
    // TODO struct is not exactly right. we really mean user-defined
    AST::FnArgs<std::pair<AST::Expression *, IR::Val>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(lhs.get(), PtrCallFix(lhs->EmitIR(ctx)));
    args.pos_.emplace_back(rhs.get(), PtrCallFix(rhs->EmitIR(ctx)));

    ASSERT(type != nullptr);
    auto results = EmitCallDispatch(args, dispatch_table_, type, ctx);
    ASSERT(results.size() == 1u);
    return results[0];
  }

  switch (op) {
#define CASE(op_name)                                                          \
  case Language::Operator::op_name: {                                          \
    auto lhs_ir = lhs->EmitIR(ctx);                                            \
    auto rhs_ir = rhs->EmitIR(ctx);                                            \
    return IR::op_name(lhs_ir, rhs_ir);                                        \
  } break
    CASE(Add);
    CASE(Sub);
    CASE(Mul);
    CASE(Div);
    CASE(Mod);
    CASE(Arrow);
#undef CASE
    case Language::Operator::Assign: {
      std::vector<const type::Type *> lhs_types, rhs_types;
      std::vector<IR::Val> rhs_vals;
      ForEachExpr(rhs.get(),
                  [&ctx, &rhs_vals, &rhs_types](size_t, AST::Expression *expr) {
                    rhs_vals.push_back(expr->EmitIR(ctx));
                    rhs_types.push_back(expr->type);
                  });
      if (rhs_vals.size() == 1) {
        if (auto many_ptr =
                std::get_if<std::vector<IR::Val>>(&rhs_vals[0].value)) {
          rhs_vals  = std::move(*many_ptr);
          rhs_types = rhs->type->as<type::Tuple>().entries_;
        }
      }

      std::vector<IR::Val> lhs_lvals;
      ForEachExpr(lhs.get(), [&ctx, &lhs_lvals, &lhs_types](
                                 size_t, AST::Expression *expr) {
        lhs_lvals.push_back(expr->EmitLVal(ctx));
        lhs_types.push_back(expr->type);
      });

      ASSERT(lhs_lvals.size() == rhs_vals.size());
      for (size_t i = 0; i < lhs_lvals.size(); ++i) {
        lhs_types[i]->EmitAssign(rhs_types[i], PtrCallFix(rhs_vals[i]),
                                 lhs_lvals[i], ctx);
      }
      return IR::Val::None();
    } break;
    case Language::Operator::OrEq: {
      if (type->is<type::Enum>()) {
        auto lhs_lval = lhs->EmitLVal(ctx);
        IR::Store(IR::Or(IR::Load(lhs_lval), rhs->EmitIR(ctx)), lhs_lval);
        return IR::Val::None();
      }
      auto land_block = IR::Func::Current->AddBlock();
      auto more_block = IR::Func::Current->AddBlock();

      auto lhs_val       = lhs->EmitIR(ctx);
      auto lhs_end_block = IR::Block::Current;
      IR::CondJump(lhs_val, land_block, more_block);

      IR::Block::Current = more_block;
      auto rhs_val       = rhs->EmitIR(ctx);
      auto rhs_end_block = IR::Block::Current;
      IR::UncondJump(land_block);

      IR::Block::Current = land_block;

      auto phi = IR::Phi(type::Bool);
      IR::Func::Current->SetArgs(
          phi, {IR::Val::Block(lhs_end_block), IR::Val::Bool(true),
                IR::Val::Block(rhs_end_block), rhs_val});
      return IR::Func::Current->Command(phi).reg();
    } break;
    case Language::Operator::AndEq: {
      if (type->is<type::Enum>()) {
        auto lhs_lval = lhs->EmitLVal(ctx);
        IR::Store(IR::And(IR::Load(lhs_lval), rhs->EmitIR(ctx)), lhs_lval);
        return IR::Val::None();
      }

      auto land_block = IR::Func::Current->AddBlock();
      auto more_block = IR::Func::Current->AddBlock();

      auto lhs_val       = lhs->EmitIR(ctx);
      auto lhs_end_block = IR::Block::Current;
      IR::CondJump(lhs_val, more_block, land_block);

      IR::Block::Current = more_block;
      auto rhs_val       = rhs->EmitIR(ctx);
      auto rhs_end_block = IR::Block::Current;
      IR::UncondJump(land_block);

      IR::Block::Current = land_block;

      auto phi = IR::Phi(type::Bool);
      IR::Func::Current->SetArgs(
          phi, {IR::Val::Block(lhs_end_block), IR::Val::Bool(false),
                IR::Val::Block(rhs_end_block), rhs_val});
      return IR::Func::Current->Command(phi).reg();
    } break;
#define CASE_ASSIGN_EQ(op_name)                                                \
  case Language::Operator::op_name##Eq: {                                      \
    auto lhs_lval = lhs->EmitLVal(ctx);                                        \
    auto rhs_ir   = rhs->EmitIR(ctx);                                          \
    IR::Store(IR::op_name(PtrCallFix(lhs_lval), rhs_ir), lhs_lval);            \
    return IR::Val::None();                                                    \
  } break
      CASE_ASSIGN_EQ(Xor);
      CASE_ASSIGN_EQ(Add);
      CASE_ASSIGN_EQ(Sub);
      CASE_ASSIGN_EQ(Mul);
      CASE_ASSIGN_EQ(Div);
      CASE_ASSIGN_EQ(Mod);
#undef CASE_ASSIGN_EQ
    case Language::Operator::Index: return PtrCallFix(EmitLVal(ctx));
    default: UNREACHABLE(*this);
  }
}

static IR::Val EmitChainOpPair(AST::ChainOp *chain_op, size_t index,
                               const IR::Val &lhs_ir, const IR::Val &rhs_ir,
                               Context *ctx) {
  const type::Type *lhs_type = chain_op->exprs[index]->type;
  const type::Type *rhs_type = chain_op->exprs[index + 1]->type;
  auto op = chain_op->ops[index];

  if (lhs_type->is<type::Array>() && rhs_type->is<type::Array>()) {
    ASSERT(op == Language::Operator::Eq || op == Language::Operator::Ne);
    return type::Array::Compare(&lhs_type->as<type::Array>(), lhs_ir,
                                &rhs_type->as<type::Array>(), rhs_ir,
                                op == Language::Operator::Eq, ctx);
  } else if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
    AST::FnArgs<std::pair<AST::Expression *, IR::Val>> args;
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

IR::Val AST::ChainOp::EmitIR(Context *ctx) {
  if (ops[0] == Language::Operator::Xor) {
    auto iter = exprs.begin();
    auto val = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::Xor(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::Or && type->is<type::Enum>()) {
    auto iter = exprs.begin();
    auto val = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::Or(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::And && type->is<type::Enum>()) {
    auto iter = exprs.begin();
    auto val = (*iter)->EmitIR(ctx);
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
      phi_args.push_back(IR::Val::Block(IR::Block::Current));
      phi_args.push_back(IR::Val::Bool(is_or));

      IR::Block::Current = next_block;
    }

    phi_args.push_back(IR::Val::Block(IR::Block::Current));
    phi_args.push_back(exprs.back()->EmitIR(ctx));
    IR::UncondJump(land_block);

    IR::Block::Current = land_block;
    auto phi = IR::Phi(type::Bool);
    IR::Func::Current->SetArgs(phi, std::move(phi_args));
    return IR::Func::Current->Command(phi).reg();

  } else {
    if (ops.size() == 1) {
      auto lhs_ir = exprs[0]->EmitIR(ctx);
      auto rhs_ir = exprs[1]->EmitIR(ctx);
      auto val = EmitChainOpPair(this, 0, lhs_ir, rhs_ir, ctx);
      return val;

    } else {
      std::vector<IR::Val> phi_args;
      auto lhs_ir = exprs.front()->EmitIR(ctx);
      auto land_block = IR::Func::Current->AddBlock();
      for (size_t i = 0; i < ops.size() - 1; ++i) {
        auto rhs_ir = exprs[i + 1]->EmitIR(ctx);
        IR::Val cmp = EmitChainOpPair(this, i, lhs_ir, rhs_ir, ctx);

        phi_args.push_back(IR::Val::Block(IR::Block::Current));
        phi_args.push_back(IR::Val::Bool(false));
        auto next_block = IR::Func::Current->AddBlock();
        IR::CondJump(cmp, next_block, land_block);
        IR::Block::Current = next_block;
        lhs_ir = rhs_ir;
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = exprs.back()->EmitIR(ctx);
      auto last_cmp =
          EmitChainOpPair(this, exprs.size() - 2, lhs_ir, rhs_ir, ctx);
      phi_args.push_back(IR::Val::Block(IR::Block::Current));
      phi_args.push_back(last_cmp);
      IR::UncondJump(land_block);

      IR::Block::Current = land_block;
      auto phi = IR::Phi(type::Bool);
      IR::Func::Current->SetArgs(phi, std::move(phi_args));
      return IR::Func::Current->Command(phi).reg();
    }
  }
}

IR::Val AST::CommaList::EmitIR(Context *) { UNREACHABLE(this); }
IR::Val AST::CommaList::EmitLVal(Context *) { NOT_YET(); }

IR::Val AST::Binop::EmitLVal(Context *ctx) {
  switch (op) {
    case Language::Operator::Index:
      if (lhs->type->is<type::Array>()) {
        return IR::Index(lhs->EmitLVal(ctx), rhs->EmitIR(ctx));
      }
      [[fallthrough]];
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}
