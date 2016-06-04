#include "IR.h"
#include "Type/Type.h"
#include "Scope.h"
#include "Stack.h"

namespace debug {
extern bool ct_eval;
} // namespace debug

namespace IR {
static Value PtrCallFix(Type *t, IR::Value v) {
  return t->is_big() ? v : IR::Load(t, v);
}
} // namespace IR

void IR::Func::PushLocal(AST::Declaration *decl) {
  size_t alignment = decl->type->alignment();
  size_t bytes     = decl->type->bytes();

  // Compile-time variables actually take up space in the IR!
  if (bytes == 0) { bytes = sizeof(Type *); }
  if (alignment == 0) { alignment = sizeof(Type *); }

  frame_size = MoveForwardToAlignment(frame_size, alignment);

  frame_map[decl->identifier] = frame_size;
  frame_size += bytes;
}

namespace AST {
IR::Value Terminal::EmitIR() {
  // TODO translation from Context::Value to IR::Value should be removed
  switch (terminal_type) {
  case Language::Terminal::ASCII: NOT_YET;
  case Language::Terminal::Char: return IR::Value(value.as_char);
  case Language::Terminal::Else: UNREACHABLE;
  case Language::Terminal::False: return IR::Value(false);
  case Language::Terminal::Hole: UNREACHABLE;
  case Language::Terminal::Int:
    return IR::Value(
        (int)value.as_int); // TODO Context::Value shouldn't use longs
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
    return IR::Load(operand->type, operand->EmitIR());
  } break;
  default: NOT_YET;
  }
}

IR::Value Binop::EmitIR() {
  switch (op) {
  case Language::Operator::Assign: {
    return IR::Store(rhs->type, rhs->EmitIR(), lhs->EmitLVal());
  } break;
  case Language::Operator::Cast: NOT_YET;
  case Language::Operator::Arrow: {
    return IR::TC_Arrow(lhs->EmitIR(), rhs->EmitIR());
  } break;
  case Language::Operator::OrEq:
  case Language::Operator::AndEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      auto lval    = lhs->EmitLVal();
      auto lhs_val = IR::Load(lhs->type, lval);

      auto load_rhs_block = IR::Func::Current->AddBlock();
      auto land_block     = IR::Func::Current->AddBlock();

      IR::Block::Current->exit.SetConditional(
          lhs_val,
          (op == Language::Operator::OrEq) ? land_block : load_rhs_block,
          (op == Language::Operator::AndEq) ? land_block : load_rhs_block);

      IR::Block::Current = load_rhs_block;
      auto rhs_val       = rhs->EmitIR();
      IR::Store(Bool, rhs_val, lval);

      IR::Block::Current->exit.SetUnconditional(land_block);

      IR::Block::Current = load_rhs_block;
      return IR::Value();
    } else {
      NOT_YET;
    }
  } break;
  case Language::Operator::XorEq: {
    auto lval    = lhs->EmitLVal();
    auto lhs_val = IR::Load(lhs->type, lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Bool && rhs->type == Bool) {
      return IR::Store(Bool, IR::BXor(lhs_val, rhs_val), lval);
    } else {
      NOT_YET;
    }
  } break;
#define ARITHMETIC_EQ_CASE(Op, op)                                             \
  case Language::Operator::Op##Eq: {                                           \
    auto lval    = lhs->EmitLVal();                                            \
    auto lhs_val = IR::Load(lhs->type, lval);                                  \
    auto rhs_val = rhs->EmitIR();                                              \
                                                                               \
    if (lhs->type == Int && rhs->type == Int) {                                \
      return IR::Store(Int, IR::I##Op(lhs_val, rhs_val), lval);                \
                                                                               \
    } else if (lhs->type == Uint && rhs->type == Uint) {                       \
      return IR::Store(Uint, IR::U##Op(lhs_val, rhs_val), lval);               \
                                                                               \
    } else if (lhs->type == Real && rhs->type == Real) {                       \
      return IR::Store(Real, IR::F##Op(lhs_val, rhs_val), lval);               \
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

  case Language::Operator::Index: {
    NOT_YET;
  } break;
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

    for (auto &b : blocks) { b = IR::Func::Current->AddBlock(); }

    IR::Block::Current->exit.SetUnconditional(blocks.front());

    // Create the landing block
    IR::Block *landing_block = IR::Func::Current->AddBlock();
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

    for (auto &b : blocks) { b = IR::Func::Current->AddBlock(); }

    IR::Block *landing_block = IR::Func::Current->AddBlock();
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

  statements->verify_types();
  if(error_log.num_errors() != 0) {
    std::cerr << error_log << std::endl;
  }

  for (auto decl : fn_scope->ordered_decls_) {
    if (decl->identifier->arg_val) { continue; }
    ir_func->PushLocal(decl);
  }

  for (auto scope : fn_scope->innards_) {
    for (auto decl : scope->ordered_decls_) {
      if (decl->identifier->arg_val) { continue; }
      ir_func->PushLocal(decl);
    }
  }

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
      return IR::Value::Arg(arg_num);
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
    return IR::PtrCallFix(
        type, IR::Value::Alloc(IR::Func::Current->frame_map.at(this)));
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

static void EmitAssignment(Scope *scope, Type *lhs_type, Type *rhs_type,
                           IR::Value lhs_ptr, IR::Value rhs) {
  assert(scope);
  if (lhs_type == rhs_type) {
    if (lhs_type->is_primitive() || lhs_type->is_pointer() ||
        lhs_type->is_enum()) {
      IR::Store(rhs_type, rhs, lhs_ptr);
    }
  } else {
    NOT_YET;
  }
}

IR::Value Declaration::EmitIR() {
  if (IsUninitialized()) {
    return IR::Value();

  } else if (IsDefaultInitialized()) {
    type->EmitInit(identifier->EmitLVal());

  } else {
    auto id_val  = identifier->EmitLVal();
    auto rhs_val = init_val->EmitIR();

    EmitAssignment(scope_, identifier->type, init_val->type, id_val, rhs_val);
  }
  return IR::Value();
}

IR::Value DummyTypeExpr::EmitIR() { return IR::Value(value.as_type); }

IR::Value Case::EmitIR() {
  std::vector<IR::Block *> key_blocks(key_vals.size(), nullptr);

  for (auto &b : key_blocks) { b = IR::Func::Current->AddBlock(); }

  // Create the landing block
  IR::Block *landing_block = IR::Func::Current->AddBlock();
  auto phi = IR::Phi();

  IR::Block::Current->exit.SetUnconditional(key_blocks.front());

  IR::Value result;
  for (size_t i = 0; i < key_vals.size() - 1; ++i) {
    auto compute_block = IR::Func::Current->AddBlock();

    IR::Block::Current = key_blocks[i];
    result = key_vals[i].first->EmitIR();
    IR::Block::Current->exit.SetConditional(result, compute_block,
                                            key_blocks[i + 1]);

    IR::Block::Current = compute_block;
    result = key_vals[i].second->EmitIR();
    IR::Block::Current->exit.SetUnconditional(landing_block);
    phi.AddIncoming(IR::Block::Current, result);
  }

  // Assume last entry is "else => ___".
  IR::Block::Current = key_blocks.back();
  result = key_vals.back().second->EmitIR();
  IR::Block::Current->exit.SetUnconditional(landing_block);
  phi.AddIncoming(IR::Block::Current, result);

  IR::Block::Current = landing_block;
  landing_block->cmds.push_back(phi);

  return phi;
}

IR::Value Access::EmitIR() {
  // TODO we don't allow pointers to types?
  if (operand->type == Type_) {
    if (member_name == "bytes") {
      return IR::Bytes(operand->EmitIR());

    } else if (member_name == "alignment") {
      return IR::Alignment(operand->EmitIR());
    }
  }

  // Pass through pointers
  auto eval      = operand->EmitIR();
  auto base_type = operand->type;
  while (base_type->is_pointer()) {
    base_type = ((Pointer *)base_type)->pointee;
    if (!base_type->is_big()) { eval = IR::Load(base_type, eval); }
  }

  // Array size
  if (base_type->is_array() && member_name == "size") {
    auto array_type = (Array *)base_type;
    if (array_type->fixed_length) {
      return IR::Value(array_type->len);
    } else {
      auto gep = IR::GEP(array_type, eval, {0, 0});
      IR::Block::Current->push(gep);
      return IR::Load(array_type->data_type, gep);
    }
  }

  if (base_type->is_struct()) {
    auto struct_type = (Structure *)base_type;

    if (!type->stores_data()) { NOT_YET; }

    auto elem_ptr =
        IR::GEP(struct_type, eval,
                {0, (int)(struct_type->field_name_to_num AT(member_name))});
    IR::Block::Current->push(elem_ptr);

    return IR::PtrCallFix(type, elem_ptr);
  }

  NOT_YET;
}

IR::Value While::EmitIR() {
  auto cond_block = IR::Func::Current->AddBlock();
  auto body_block = IR::Func::Current->AddBlock();
  auto land_block = IR::Func::Current->AddBlock();

  IR::Block::Current->exit.SetUnconditional(cond_block);

  IR::Block::Current = cond_block;
  auto cond_val = condition->EmitIR();
  IR::Block::Current->exit.SetConditional(cond_val, body_block, land_block);

  IR::Block::Current = body_block;
  // for (auto decl : while_scope->ordered_decls_) { /* TODO Initialize */ }
  statements->EmitIR();
  // TODO Exit/cleanup

  // TODO Exit flag. For now, there are no breaks/continues
  IR::Block::Current->exit.SetUnconditional(cond_block);

  IR::Block::Current = land_block;
  return IR::Value();
}

IR::Value Conditional::EmitIR() {
  std::vector<IR::Block *> cond_blocks(conditions.size(), nullptr);
  std::vector<IR::Block *> body_blocks(body_scopes.size(), nullptr);

  for (auto &b : cond_blocks) { b = IR::Func::Current->AddBlock(); }
  for (auto &b : body_blocks) { b = IR::Func::Current->AddBlock(); }
  auto land_block = IR::Func::Current->AddBlock();

  IR::Block::Current->exit.SetUnconditional(cond_blocks[0]);

  for (size_t i = 0; i < conditions.size() - 1; ++i) {
    IR::Block::Current = cond_blocks[i];
    auto cond_val = conditions[i]->EmitIR();
    IR::Block::Current->exit.SetConditional(cond_val, body_blocks[i],
                                            cond_blocks[i + 1]);
  }

  IR::Block::Current = cond_blocks.back();
  auto cond_val      = conditions.back()->EmitIR();
  IR::Block::Current->exit.SetConditional(
      cond_val, body_blocks[conditions.size() - 1],
      has_else() ? body_blocks.back() : land_block);

  for (size_t i = 0; i < body_scopes.size(); ++i) {
    IR::Block::Current = body_blocks[i];
    statements[i]->EmitIR();
    // TODO break, return, etc flags? Destruction?
    IR::Block::Current->exit.SetUnconditional(land_block);
  }

  IR::Block::Current = land_block;
  return IR::Value();
}

IR::Value ArrayLiteral::EmitIR() { NOT_YET; }
IR::Value For::EmitIR() { NOT_YET; }
IR::Value Jump::EmitIR() { NOT_YET; }
IR::Value Generic::EmitIR() { NOT_YET; }
IR::Value InDecl::EmitIR() { NOT_YET; }
IR::Value ParametricStructLiteral::EmitIR() { NOT_YET; }
IR::Value StructLiteral::EmitIR() { NOT_YET; }
IR::Value EnumLiteral::EmitIR() { NOT_YET; }
} // namespace AST
