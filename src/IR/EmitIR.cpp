#include "IR.h"
#include "Type/Type.h"
#include "Scope.h"

#define NOT_YET assert(false && "Not yet implemented")

extern llvm::Value *GetFunctionReferencedIn(Scope *scope,
                                            const std::string &fn_name,
                                            Type *input_type);

namespace AST {
IR::Value Terminal::EmitIR() {
  switch (terminal_type) {
  case Language::Terminal::ASCII: NOT_YET;
  case Language::Terminal::Char: NOT_YET;
  case Language::Terminal::Else: return IR::Value(true);
  case Language::Terminal::False: return IR::Value(false);
  case Language::Terminal::Hole: NOT_YET;
  case Language::Terminal::Int: NOT_YET;
  case Language::Terminal::Null: NOT_YET;
  case Language::Terminal::Ord: NOT_YET;
  case Language::Terminal::Real: NOT_YET;
  case Language::Terminal::Return: NOT_YET;
  case Language::Terminal::StringLiteral: NOT_YET;
  case Language::Terminal::True: return IR::Value(true);
  case Language::Terminal::Type: NOT_YET; 
  case Language::Terminal::Uint: NOT_YET; 
  }
}

IR::Value Unop::EmitIR() {
  switch (op) {
  case Language::Operator::Import: NOT_YET;
  case Language::Operator::Return: {
    auto result = operand->EmitIR();
    IR::Block::Current->exit.SetReturn(result);
    return IR::Value();
  } break;
  case Language::Operator::Break: NOT_YET;
  case Language::Operator::Continue: NOT_YET;
  case Language::Operator::Repeat: NOT_YET;
  case Language::Operator::Restart: NOT_YET;
  case Language::Operator::Free: NOT_YET;
  case Language::Operator::Print: NOT_YET;
  case Language::Operator::And: NOT_YET;
  case Language::Operator::Sub: {
    auto val = operand->EmitIR();
    if (operand->type == Int) {
      return IR::INeg(val);

    } else if (operand->type == Real) {
      return IR::FNeg(val);

    } else {
      auto neg_fn = GetFunctionReferencedIn(scope_, "__neg__", operand->type);
      assert(neg_fn && "No 'not' function available.");
      NOT_YET; // return IR::Call(neg_fn, {val});
    }
  } break;
  case Language::Operator::Not: {
    auto val = operand->EmitIR();
    if (operand->type == Bool) {
      return BNot(val);

    } else {
      auto not_fn = GetFunctionReferencedIn(scope_, "__not__", operand->type);
      assert(not_fn && "No 'not' function available.");
      NOT_YET; // return IR::Call(not_fn, {val});
    }
  } break;
  case Language::Operator::At: {
    return IR::Load(operand->EmitIR());
  } break;
  case Language::Operator::Call: {
                                   NOT_YET;
    // return IRB.AddCall(neg_fn->ir);
  } break;
  default: NOT_YET;
  }
}

IR::Value Binop::EmitIR() {
  switch (op) {
  case Language::Operator::Assign: NOT_YET;
  case Language::Operator::Cast: NOT_YET;
  case Language::Operator::OrEq: NOT_YET;
  case Language::Operator::XorEq: NOT_YET;
  case Language::Operator::AndEq: NOT_YET;
  case Language::Operator::AddEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IAdd(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UAdd(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FAdd(lhs_val, rhs_val), lval);

    } else {
      auto add_eq_fn = GetFunctionReferencedIn(scope_, "__add_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(add_eq_fn && "No 'add_eq' function available");
      NOT_YET; // return IR::Call(add_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::SubEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::ISub(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::USub(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FSub(lhs_val, rhs_val), lval);

    } else {
      auto sub_eq_fn = GetFunctionReferencedIn(scope_, "__sub_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(sub_eq_fn && "No 'sub_eq' function available");
      NOT_YET; // return IR::Call(sub_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::MulEq: {
    auto lval    = lhs->EmitIR(); // EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IMul(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UMul(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FMul(lhs_val, rhs_val), lval);

    } else {
      auto mul_eq_fn = GetFunctionReferencedIn(scope_, "__mul_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(mul_eq_fn && "No 'mul_eq' function available");
      NOT_YET; // return IR::Call(mul_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::DivEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IDiv(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UDiv(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FDiv(lhs_val, rhs_val), lval);

    } else {
      auto div_eq_fn = GetFunctionReferencedIn(scope_, "__div_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(div_eq_fn && "No 'div_eq' function available");
      NOT_YET; // return IR::Call(div_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::ModEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IMod(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UMod(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FMod(lhs_val, rhs_val), lval);

    } else {
      auto mod_eq_fn = GetFunctionReferencedIn(scope_, "__mod_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(mod_eq_fn && "No 'mod_eq' function available");
      NOT_YET; // return IR::Call(mod_eq_fn, {lhs_val, rhs_val});
    }
  } break;

  default: NOT_YET;
  }
}

IR::Value ChainOp::EmitIR() {
  assert(!ops.empty());

  if (ops[0] == Language::Operator::Xor) {
    std::vector<IR::Value> vals;
    for (auto e : exprs) { vals.push_back(e->EmitIR()); }

    IR::Value v = vals[0];
    for (size_t i = 1; i < vals.size(); ++i) { v = BXor(v, vals[i]); }
    return v;

  } else if (ops[0] == Language::Operator::And ||
             ops[0] == Language::Operator::Or) {
    std::vector<IR::Block *> blocks(exprs.size(), nullptr);
    // If it's an or, an early exit is because we already know the value is true.
    // If it's an and, an early exit is beacause we already know the value is false.
    IR::Value early_exit_value = IR::Value(ops[0] == Language::Operator::Or);

    for (auto &b: blocks) {
      // TODO roll this all into one function
      b = new IR::Block(IR::Func::Current->blocks.size());
      IR::Func::Current->blocks.push_back(b);
    }

    IR::Block::Current->exit.SetUnconditional(blocks.front());

    // Create the landing block
    IR::Block *landing_block = new IR::Block(IR::Func::Current->blocks.size());
    IR::Func::Current->blocks.push_back(landing_block);
    auto phi = IR::Phi();

    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      IR::Block::Current = blocks[i];
      auto result = exprs[i]->EmitIR();
      IR::Block::Current->exit.SetConditional(result, blocks[i + 1],
                                              landing_block);

      phi.AddIncoming(IR::Block::Current, early_exit_value);
    }

    IR::Block::Current = blocks.back();
    auto last_result = exprs.back()->EmitIR();
    IR::Block::Current->exit.SetUnconditional(landing_block);
    phi.AddIncoming(IR::Block::Current, last_result);

    IR::Block::Current = landing_block;
    landing_block->cmds.push_back(phi);

    return phi;
  }
  NOT_YET;
}

IR::Value FunctionLiteral::EmitIR() {
  if (ir_func) { return IR::Value(ir_func); }
  ir_func            = new IR::Func;
  IR::Func::Current  = ir_func;
  IR::Block::Current = ir_func->entry();
  statements->EmitIR();
  ir_func->dump();
  return IR::Value(ir_func);
}

IR::Value Statements::EmitIR() {
  for (auto stmt : statements) { stmt->EmitIR(); }
  return IR::Value();
}

IR::Value Conditional::EmitIR() { NOT_YET; }
IR::Value For::EmitIR() { NOT_YET; }
IR::Value While::EmitIR() { NOT_YET; }
IR::Value Jump::EmitIR() { NOT_YET; }
IR::Value Identifier::EmitIR() { NOT_YET; }
IR::Value Declaration::EmitIR() { NOT_YET; }
IR::Value Generic::EmitIR() { NOT_YET; }
IR::Value InDecl::EmitIR() { NOT_YET; }
IR::Value ParametricStructLiteral::EmitIR() { NOT_YET; }
IR::Value StructLiteral::EmitIR() { NOT_YET; }
IR::Value Case::EmitIR() { NOT_YET; }
IR::Value Access::EmitIR() { NOT_YET; }
IR::Value ArrayLiteral::EmitIR() { NOT_YET; }
IR::Value EnumLiteral::EmitIR() { NOT_YET; }
IR::Value DummyTypeExpr::EmitIR() { NOT_YET; }
IR::Value ArrayType::EmitIR() { NOT_YET; }
} // namespace AST

#undef NOT_YET
