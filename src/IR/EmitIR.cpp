#include "IR.h"
#include "Type/Type.h"
#include "Scope.h"

extern llvm::Value *GetFunctionReferencedIn(Scope *scope,
                                            const std::string &fn_name,
                                            Type *input_type);
namespace debug {
extern bool ct_eval;
} // namespace debug

namespace AST {
IR::Value Terminal::EmitIR() {
  // TODO translation from Context::Value to IR::Value should be removed
  switch (terminal_type) {
  case Language::Terminal::ASCII: NOT_YET;
  case Language::Terminal::Char: return IR::Value(value.as_char);
  case Language::Terminal::Else: return IR::Value(true);
  case Language::Terminal::False: return IR::Value(false);
  case Language::Terminal::Hole: UNREACHABLE;
  case Language::Terminal::Int: return IR::Value(value.as_int);
  case Language::Terminal::Null: return IR::Value(nullptr);
  case Language::Terminal::Ord: NOT_YET;
  case Language::Terminal::Real: return IR::Value(value.as_real);
  case Language::Terminal::Return: {
    IR::Block::Current->exit.SetReturnVoid();
    return IR::Value();
  } break;
  case Language::Terminal::StringLiteral: NOT_YET;
  case Language::Terminal::True: return IR::Value(true);
  case Language::Terminal::Type: return IR::Value(value.as_type);
  case Language::Terminal::Uint: return IR::Value(value.as_uint);
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
  case Language::Operator::And: {
    auto val = operand->EmitIR();
    if (operand->type == Type_) {
      return IR::TC_Ptr(val);
    } else {
      NOT_YET;
    }
  } break;
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
  case Language::Operator::Arrow: {
    return IR::TC_Arrow(lhs->EmitIR(), rhs->EmitIR());
  } break;
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
static IR::Value EmitComparison(Type *op_type, Language::Operator op,
                                IR::Value lhs, IR::Value rhs) {
  if (op == Language::Operator::LT) {
    if (op_type == Int) {
      return ILT(lhs, rhs);
    } else if (op_type == Real) {
      return FLT(lhs, rhs);
    } else if (op_type == Uint) {
      return ULT(lhs, rhs);
    }

  } else if (op == Language::Operator::LE) {
    if (op_type == Int) {
      return ILE(lhs, rhs);
    } else if (op_type == Real) {
      return FLE(lhs, rhs);
    } else if (op_type == Uint) {
      return ULE(lhs, rhs);
    }

  } else if (op == Language::Operator::EQ) {
    if (op_type == Bool) {
      return BEQ(lhs, rhs);
    } else if (op_type == Char) {
      return CEQ(lhs, rhs);
    } else if (op_type == Int) {
      return IEQ(lhs, rhs);
    } else if (op_type == Real) {
      return FEQ(lhs, rhs);
    } else if (op_type == Uint) {
      return UEQ(lhs, rhs);
    } else if (op_type == Type_) {
      return TEQ(lhs, rhs);
    } else if (op_type->is_function()) {
      return FnEQ(lhs, rhs);
    }

  } else if (op == Language::Operator::NE) {
    if (op_type == Bool) {
      return BNE(lhs, rhs);
    } else if (op_type == Char) {
      return CNE(lhs, rhs);
    } else if (op_type == Int) {
      return INE(lhs, rhs);
    } else if (op_type == Real) {
      return FNE(lhs, rhs);
    } else if (op_type == Uint) {
      return UNE(lhs, rhs);
    } else if (op_type == Type_) {
      return TNE(lhs, rhs);
    } else if (op_type->is_function()) {
      return FnNE(lhs, rhs);
    }

  } else if (op == Language::Operator::GE) {
    if (op_type == Int) {
      return IGE(lhs, rhs);
    } else if (op_type == Real) {
      return FGE(lhs, rhs);
    } else if (op_type == Uint) {
      return UGE(lhs, rhs);
    }

  } else if (op == Language::Operator::GT) {
    if (op_type == Int) {
      return IGT(lhs, rhs);
    } else if (op_type == Real) {
      return FGT(lhs, rhs);
    } else if (op_type == Uint) {
      return UGT(lhs, rhs);
    }
  }
  assert(false);
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
    bool using_or = (ops[0] == Language::Operator::Or);
    IR::Value early_exit_value = IR::Value(using_or);

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
      if (using_or) {
        IR::Block::Current->exit.SetConditional(result, landing_block,
                                                blocks[i + 1]);
      } else {
        IR::Block::Current->exit.SetConditional(result, blocks[i + 1],
                                                landing_block);
      }
      phi.AddIncoming(IR::Block::Current, early_exit_value);
    }

    IR::Block::Current = blocks.back();
    auto last_result = exprs.back()->EmitIR();
    IR::Block::Current->exit.SetUnconditional(landing_block);
    phi.AddIncoming(IR::Block::Current, last_result);

    IR::Block::Current = landing_block;
    landing_block->cmds.push_back(phi);

    return phi;
  } else if (Language::precedence(ops.front()) ==
             Language::precedence(Language::Operator::EQ)) {
    // Operators here can be <, <=, ==, !=, >=, or >.
    std::vector<IR::Block *> blocks(exprs.size() - 1, nullptr);

    for (auto &b: blocks) {
      // TODO roll this all into one function
      b = new IR::Block(IR::Func::Current->blocks.size());
      IR::Func::Current->blocks.push_back(b);
    }

    IR::Block *landing_block = new IR::Block(IR::Func::Current->blocks.size());
    IR::Func::Current->blocks.push_back(landing_block);
    auto phi = IR::Phi();

    IR::Value result, lhs;
    IR::Value rhs = exprs[0]->EmitIR();
    IR::Block::Current->exit.SetUnconditional(blocks.front());
    assert(exprs.size() >= 2);
    for (size_t i = 0; i < exprs.size() - 2; ++i) {
      IR::Block::Current = blocks[i];
      lhs                = rhs;
      rhs                = exprs[i + 1]->EmitIR();
      result             = EmitComparison(exprs[i]->type, ops[i], lhs, rhs);

      // Early exit
      IR::Block::Current->exit.SetConditional(result, blocks[i + 1],
                                              landing_block);
      phi.AddIncoming(IR::Block::Current, IR::Value(false));
    }

    IR::Block::Current = blocks.back();

    lhs              = rhs;
    rhs              = exprs.back()->EmitIR();
    auto last_result = EmitComparison(exprs.back()->type, ops.back(), lhs, rhs);
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

  if (debug::ct_eval) { ir_func->dump(); }

  return IR::Value(ir_func);
}

IR::Value Statements::EmitIR() {
  for (auto stmt : statements) { stmt->EmitIR(); }
  return IR::Value();
}

IR::Value Identifier::EmitIR() {
  if (arg_val && arg_val->is_function_literal()) {
    // TODO Iterating through linearly is probably not smart.
    auto fn = (FunctionLiteral *)arg_val;
    size_t arg_num = 0;
    for (auto in : fn->inputs) {
      if (this != in->identifier) {
        ++arg_num;
        continue;
      }

      // Here you found a match
      IR::Value v;
      v.flag       = IR::ValType::Arg;
      v.val.as_arg = arg_num;
      return v;
    }
    assert(false && "Failed to match argument");
  }
  NOT_YET;
}

IR::Value ArrayType::EmitIR() {
  if (length->is_hole()) {
    return IR::TC_Arr1(data_type->EmitIR());
  } else {
    return IR::TC_Arr2(length->EmitIR(), data_type->EmitIR());
  }
}

IR::Value Conditional::EmitIR() { NOT_YET; }
IR::Value For::EmitIR() { NOT_YET; }
IR::Value While::EmitIR() { NOT_YET; }
IR::Value Jump::EmitIR() { NOT_YET; }
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
} // namespace AST
