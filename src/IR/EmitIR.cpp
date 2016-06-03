#include "IR.h"
#include "Type/Type.h"
#include "Scope.h"

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

void EmitPrintExpr(Expression *expr) {
  if (expr->type->is_primitive()) {
    IR::Print(expr->EmitIR());
  } else {
    NOT_YET;
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
  case Language::Operator::Print: {
    if (operand->is_comma_list()) {
      auto operand_as_chainop = (ChainOp *)operand;
      for (auto op : operand_as_chainop->exprs) { EmitPrintExpr(op); }
    } else {
      EmitPrintExpr(operand);
    }
    return IR::Value();
  } break;
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
      NOT_YET;
    }
  } break;
  case Language::Operator::Not: {
    auto val = operand->EmitIR();
    if (operand->type == Bool) {
      return BNot(val);

    } else {
      NOT_YET;
    }
  } break;
  case Language::Operator::At: {
    return IR::Load(operand->EmitIR());
  } break;
  default: NOT_YET;
  }
}

IR::Value Binop::EmitIR() {
  switch (op) {
  case Language::Operator::Assign: {
    return IR::Store(rhs->EmitIR(), lhs->EmitLVal());
  } break;
  case Language::Operator::Cast: NOT_YET;
  case Language::Operator::Arrow: {
    return IR::TC_Arrow(lhs->EmitIR(), rhs->EmitIR());
  } break;
  case Language::Operator::OrEq: NOT_YET;
  case Language::Operator::XorEq: NOT_YET;
  case Language::Operator::AndEq: NOT_YET;

#define ARITHMETIC_EQ_CASE(Op, op)                                             \
  case Language::Operator::Op##Eq: {                                           \
    auto lval    = lhs->EmitLVal();                                            \
    auto lhs_val = IR::Load(lval);                                             \
    auto rhs_val = rhs->EmitIR();                                              \
                                                                               \
    if (lhs->type == Int && rhs->type == Int) {                                \
      return IR::Store(IR::I##Op(lhs_val, rhs_val), lval);                     \
                                                                               \
    } else if (lhs->type == Uint && rhs->type == Uint) {                       \
      return IR::Store(IR::U##Op(lhs_val, rhs_val), lval);                     \
                                                                               \
    } else if (lhs->type == Real && rhs->type == Real) {                       \
      return IR::Store(IR::F##Op(lhs_val, rhs_val), lval);                     \
                                                                               \
    } else {                                                                   \
      NOT_YET;                                                                 \
    }                                                                          \
  } break

    ARITHMETIC_EQ_CASE(Add, add);
    ARITHMETIC_EQ_CASE(Sub, sub);
    ARITHMETIC_EQ_CASE(Mul, mul);
    ARITHMETIC_EQ_CASE(Div, div);
    ARITHMETIC_EQ_CASE(Mod, mod);

#undef ARITHMETIC_EQ_CASE

#define ARITHMETIC_CASE(Op, op)                                                \
  case Language::Operator::Op: {                                               \
    auto lhs_val = lhs->EmitIR();                                              \
    auto rhs_val = rhs->EmitIR();                                              \
    if (lhs->type == Int && rhs->type == Int) {                                \
      return IR::I##Op(lhs_val, rhs_val);                                      \
                                                                               \
    } else if (lhs->type == Uint && rhs->type == Uint) {                       \
      return IR::U##Op(lhs_val, rhs_val);                                      \
                                                                               \
    } else if (lhs->type == Real && rhs->type == Real) {                       \
      return IR::F##Op(lhs_val, rhs_val);                                      \
                                                                               \
    } else {                                                                   \
      NOT_YET;                                                                 \
    }                                                                          \
  } break

    ARITHMETIC_CASE(Add, add);
    ARITHMETIC_CASE(Sub, sub);
    ARITHMETIC_CASE(Mul, mul);
    ARITHMETIC_CASE(Div, div);
    ARITHMETIC_CASE(Mod, mod);

#undef ARITHMETIC_CASE

  case Language::Operator::Call: {
    auto result = IR::CallCmd(lhs->EmitIR());
    if (rhs) {
      if (rhs->is_comma_list()) {
        for (auto expr : ((ChainOp *)rhs)->exprs) {
          result.args.push_back(expr->EmitIR());
        }
      } else {
        result.args.push_back(rhs->EmitIR());
      }
    }

    IR::Block::Current->cmds.push_back(result);

    return result;
  }
  default: std::cerr << *this << std::endl; NOT_YET;
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
  if (ir_func) { return IR::Value(ir_func); } // Cache
  ir_func            = new IR::Func;
  IR::Func::Current  = ir_func;
  IR::Block::Current = ir_func->entry();

  size_t frame_alignment_mask = 0;
  for (auto decl : fn_scope->ordered_decls_) {
    if (decl->identifier->arg_val) { continue; }

    // Set new alignment
    size_t align_mask = decl->type->alignment() - 1;
    ir_func->frame_size = ((ir_func->frame_size - 1) | align_mask) + 1;

    ir_func->frame_map[decl->identifier] = ir_func->frame_size;
    ir_func->frame_size += decl->type->bytes();

    frame_alignment_mask |= align_mask;
  }

  ir_func->frame_alignment = frame_alignment_mask + 1;

  statements->verify_types();
  statements->EmitIR();

  if (debug::ct_eval) { ir_func->dump(); }

  return IR::Value(ir_func);
}

IR::Value Statements::EmitIR() {
  for (auto stmt : statements) {
    if (stmt->is_declaration()) { continue; }
    stmt->EmitIR();
  }
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

  } else if (type->is_function()) {
    Ctx ctx;
    evaluate(ctx);
    assert(value.as_expr);
    auto current_func  = IR::Func::Current;
    auto current_block = IR::Block::Current;
    auto func_to_call  = value.as_expr->EmitIR();
    IR::Block::Current = current_block;
    IR::Func::Current  = current_func;
    return func_to_call;

  } else {
    return IR::Load(IR::Value::Alloc(IR::Func::Current->frame_map.at(this)));
  }
  std::cerr << *this << std::endl;
  NOT_YET;
}

IR::Value ArrayType::EmitIR() {
  if (length->is_hole()) {
    return IR::TC_Arr1(data_type->EmitIR());
  } else {
    return IR::TC_Arr2(length->EmitIR(), data_type->EmitIR());
  }
}

IR::Value Declaration::EmitIR() { NOT_YET; }

IR::Value DummyTypeExpr::EmitIR() { return IR::Value(value.as_type); }

IR::Value Conditional::EmitIR() { NOT_YET; }
IR::Value For::EmitIR() { NOT_YET; }
IR::Value While::EmitIR() { NOT_YET; }
IR::Value Jump::EmitIR() { NOT_YET; }
IR::Value Generic::EmitIR() { NOT_YET; }
IR::Value InDecl::EmitIR() { NOT_YET; }
IR::Value ParametricStructLiteral::EmitIR() { NOT_YET; }
IR::Value StructLiteral::EmitIR() { NOT_YET; }
IR::Value Case::EmitIR() { NOT_YET; }
IR::Value Access::EmitIR() { NOT_YET; }
IR::Value ArrayLiteral::EmitIR() { NOT_YET; }
IR::Value EnumLiteral::EmitIR() { NOT_YET; }
} // namespace AST
