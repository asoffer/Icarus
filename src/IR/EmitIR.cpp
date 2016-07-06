#include "IR.h"
#include "Type/Type.h"
#include "Scope.h"
#include "Stack.h"

namespace debug {
extern bool ct_eval;
} // namespace debug

namespace IR {
extern Func *AsciiFunc();
extern Func *OrdFunc();
} // namespace IR

extern std::string Mangle(const Function *f, AST::Expression *expr,
                          Scope *starting_scope);
extern IR::Value PtrCallFix(Type *t, IR::Value v);
extern IR::Func *GetFuncReferencedIn(Scope *scope, const std::string &fn_name,
                                     Function *fn_type);

size_t IR::Func::PushSpace(Type *t) {
  return PushSpace(t->bytes(), t->alignment());
}

size_t IR::Func::PushSpace(size_t bytes, size_t alignment) {
  // Compile-time variables actually take up space in the IR!
  if (bytes == 0) { bytes = sizeof(Type *); }
  if (alignment == 0) { alignment = sizeof(Type *); }

  frame_size = MoveForwardToAlignment(frame_size, alignment);

  auto result = frame_size;
  frame_size += bytes;

  return result;
}

void IR::Func::PushLocal(AST::Declaration *decl) {
  decl->stack_loc = IR::Value::FrameAddr(PushSpace(decl->type));
}

namespace AST {
IR::Value Terminal::EmitIR() {
  // TODO translation from Context::Value to IR::Value should be removed
  switch (terminal_type) {
  case Language::Terminal::ASCII: return IR::Value(IR::AsciiFunc());
  case Language::Terminal::Char: return IR::Value(value.as_char);
  case Language::Terminal::Else: UNREACHABLE;
  case Language::Terminal::False: return IR::Value(false);
  case Language::Terminal::Hole: UNREACHABLE;
  case Language::Terminal::Int:
    return IR::Value(
        (long)value.as_int); // TODO Context::Value shouldn't use longs
  case Language::Terminal::Null: return IR::Value::HeapAddr(0);
  case Language::Terminal::Ord: return IR::Value(IR::OrdFunc());
  case Language::Terminal::Real: return IR::Value(value.as_real);
  case Language::Terminal::Return: {
    IR::Block::Current->exit.SetReturnVoid();
    return IR::Value();
  } break;
  case Language::Terminal::StringLiteral: return IR::Value(value.as_cstr);
  case Language::Terminal::True: return IR::Value(true);
  case Language::Terminal::Type: return IR::Value(value.as_type);
  case Language::Terminal::Uint: return IR::Value(value.as_uint);
  }
}

static void EmitPrintExpr(Expression *expr) {
  // TODO make string primitive
  if (expr->type->is_primitive() || expr->type == String || expr->type->is_enum()) {
    IR::Print(IR::Value(expr->type), expr->EmitIR());

  } else if (expr->type->is_function() || expr->type->is_array()) {
    expr->type->EmitRepr(expr->EmitIR());

  } else {
    auto fn =
        GetFuncReferencedIn(expr->scope_, "__print__", Func(expr->type, Void));
    assert(fn);
    IR::Call(Void, IR::Value(fn), {expr->EmitIR()});
  }
}

IR::Value Unop::EmitIR() {
  switch (op) {
  // NOTE: Not sure if Import will always be disallowed (in which case we need
  // TODO stricter checking earlier) or if we allow it to some degree
  // (hopefully, but details need to be worked out). For now it is deemed
  // illegal.
  case Language::Operator::Import: UNREACHABLE;
  case Language::Operator::Return: {
    // Note: Because we're loading the current block, and the EmitIR call can
    // change that, we must first compute the ret then set the return. The order
    // here is important!
    auto ret = operand->EmitIR();
    assert(scope_->is_block_scope());
    auto block_scope = (BlockScope *)scope_;

    block_scope->MakeReturn(operand->type, ret);
    return IR::Value();
  } break;
  case Language::Operator::Free: NOT_YET;
  case Language::Operator::Eval: {
    auto fn_ptr                = new FunctionLiteral;
    fn_ptr->type               = Func(Void, operand->type);
    fn_ptr->fn_scope->fn_type  = (Function *)fn_ptr->type;
    fn_ptr->scope_             = scope_;
    fn_ptr->statements         = new Statements;
    fn_ptr->statements->scope_ = fn_ptr->fn_scope;
    fn_ptr->return_type_expr   = new DummyTypeExpr(operand->loc, type);
    Unop *ret                  = nullptr;

    if (operand->type != Void) {
      ret             = new Unop;
      ret->scope_     = fn_ptr->fn_scope;
      ret->operand    = operand;
      ret->op         = Language::Operator::Return;
      ret->precedence = Language::precedence(ret->op);
      fn_ptr->statements->statements.push_back(ret);
    } else {
      fn_ptr->statements->statements.push_back(operand);
    }

    auto local_stack = new IR::LocalStack;
    IR::Func *func   = fn_ptr->EmitIR().as_func;
    func->name       = "anonymous-func";

    if (operand->type == Void) {
      // Assumptions:
      //  * There's only one exit block.
      //  * The current block is pointing to it. This is NOT threadsafe.
      IR::Block::Current->exit.SetReturnVoid();
    }

    auto result = IR::Call(func, local_stack, {});
    delete local_stack;

    // Cleanup
    // ret->operand = nullptr;
    delete fn_ptr; // Because delete is called recursively, this is all we need
                   // to do. The Statements and Unop nodes are owned by the
                   // function.
    return result;
  } break;
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
      auto fn =
          GetFuncReferencedIn(scope_, "__addr__", Func(operand->type, type));
      assert(fn);
      IR::Call(Void, IR::Value(fn), {operand->EmitIR()});
      return IR::Value();
    }
  } break;
  case Language::Operator::Sub: {
    auto val = operand->EmitIR();
    if (operand->type == Int) {
      return IR::INeg(val);
    } else if (operand->type == Real) {
      return IR::FNeg(val);
    } else {
      auto fn =
          GetFuncReferencedIn(scope_, "__neg__", Func(operand->type, type));
      return IR::Call(type, IR::Value(fn), {val});
    }
  } break;
  case Language::Operator::Not: {
    auto val = operand->EmitIR();
    if (operand->type == Bool) {
      return IR::BNot(val);
    } else {
      auto fn =
          GetFuncReferencedIn(scope_, "__not__", Func(operand->type, type));
      return IR::Call(type, IR::Value(fn), {val});
    }
  } break;
  case Language::Operator::At: {
    return IR::Load(operand->type, operand->EmitIR());
  } break;
  default: UNREACHABLE;
  }
}

IR::Value Binop::EmitIR() {
  switch (op) {
  case Language::Operator::Assign: {
    if (rhs->type->is_primitive()) {
      return IR::Store(rhs->type, rhs->EmitIR(), lhs->EmitLVal());
    } else {
      Type::IR_CallAssignment(scope_, lhs->type, rhs->type, rhs->EmitIR(),
                              lhs->EmitLVal());
      return IR::Value();
    }
  } break;
  case Language::Operator::Cast: {
    auto rhs_val = rhs->EmitIR();
    // NOTE: Demand knowing the type now rather than at eval time
    assert(rhs_val.flag == IR::ValType::T);
    return IR::Cast(lhs->type, rhs_val.as_type, lhs->EmitIR());
  };
  case Language::Operator::Arrow: {
    return IR::TC_Arrow(lhs->EmitIR(), rhs->EmitIR());
  } break;
  case Language::Operator::OrEq:
  case Language::Operator::AndEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      auto lval    = lhs->EmitLVal();
      auto lhs_val = IR::Load(lhs->type, lval);

      auto load_rhs_block = IR::Func::Current->AddBlock("load-rhs");
      auto land_block     = IR::Func::Current->AddBlock("binop-land");

      IR::Block::Current->exit.SetConditional(
          lhs_val,
          (op == Language::Operator::OrEq) ? land_block : load_rhs_block,
          (op == Language::Operator::AndEq) ? land_block : load_rhs_block);

      IR::Block::Current = load_rhs_block;
      auto rhs_val = rhs->EmitIR();
      IR::Store(Bool, rhs_val, lval);

      IR::Block::Current->exit.SetUnconditional(land_block);

      IR::Block::Current = land_block;
      return IR::Value();
    } else {
      auto fn = GetFuncReferencedIn(
          scope_, op == Language::Operator::AndEq ? "__and_eq__" : "__or_eq__",
          Func(Tup({lhs->type, rhs->type}), type));
      return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});
    }
  } break;
  case Language::Operator::XorEq: {
    auto lval    = lhs->EmitLVal();
    auto lhs_val = IR::Load(lhs->type, lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Bool && rhs->type == Bool) {
      return IR::Store(Bool, IR::BXor(lhs_val, rhs_val), lval);
    } else {
      auto fn = GetFuncReferencedIn(scope_, "__xor_eq__",
                                    Func(Tup({lhs->type, rhs->type}), type));
      return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});
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
    }                                                                          \
    if (lhs->type == Uint && rhs->type == Uint) {                              \
      return IR::Store(Uint, IR::U##Op(lhs_val, rhs_val), lval);               \
    }                                                                          \
    if (lhs->type == Real && rhs->type == Real) {                              \
      return IR::Store(Real, IR::F##Op(lhs_val, rhs_val), lval);               \
    }                                                                          \
    auto fn =                                                                  \
        GetFuncReferencedIn(scope_, "__" #op "_eq__",                          \
                            Func(Tup({Ptr(lhs->type), rhs->type}), type));     \
    return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});      \
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
    }                                                                          \
    if (lhs->type == Uint && rhs->type == Uint) {                              \
      return IR::U##Op(lhs_val, rhs_val);                                      \
    }                                                                          \
    if (lhs->type == Real && rhs->type == Real) {                              \
      return IR::F##Op(lhs_val, rhs_val);                                      \
    }                                                                          \
    auto fn = GetFuncReferencedIn(scope_, "__" #op "__",                       \
                                  Func(Tup({lhs->type, rhs->type}), type));    \
    return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});      \
  } break

    ARITHMETIC_CASE(Add, add);
    ARITHMETIC_CASE(Sub, sub);
    ARITHMETIC_CASE(Mul, mul);
    ARITHMETIC_CASE(Div, div);
    ARITHMETIC_CASE(Mod, mod);
#undef ARITHMETIC_CASE

  case Language::Operator::Index: {
    if (lhs->type->is_array()) {
      return PtrCallFix(type, EmitLVal());

    } else {
      auto fn = GetFuncReferencedIn(scope_, "__bracket__",
                                    Func(Tup({lhs->type, rhs->type}), type));
      return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});
    }
  } break;

  case Language::Operator::Call: {
    if (rhs) {
      if (rhs->is_comma_list()) {
        std::vector<IR::Value> args;
        for (auto v : ((ChainOp *)rhs)->exprs) { args.push_back(v->EmitIR()); }
        return IR::Call(type, lhs->EmitIR(), args);

      } else {
        return IR::Call(type, lhs->EmitIR(), {rhs->EmitIR()});
      }
    } else {
      return IR::Call(type, lhs->EmitIR(), {});
    }
  }
  default: UNREACHABLE;
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
    if (op_type == Int) { return ILE(lhs, rhs); }
    if (op_type == Real) { return FLE(lhs, rhs); }
    if (op_type == Uint) { return ULE(lhs, rhs); }

  } else if (op == Language::Operator::EQ) {
    if (op_type == Bool) { return BEQ(lhs, rhs); }
    if (op_type == Char) { return CEQ(lhs, rhs); }
    if (op_type == Int) { return IEQ(lhs, rhs); }
    if (op_type == Real) { return FEQ(lhs, rhs); }
    if (op_type == Uint) { return UEQ(lhs, rhs); }
    if (op_type == Type_) { return TEQ(lhs, rhs); }
    if (op_type->is_function()) { return FnEQ(lhs, rhs); }

  } else if (op == Language::Operator::NE) {
    if (op_type == Bool) { return BNE(lhs, rhs); }
    if (op_type == Char) { return CNE(lhs, rhs); }
    if (op_type == Int) { return INE(lhs, rhs); }
    if (op_type == Real) { return FNE(lhs, rhs); }
    if (op_type == Uint) { return UNE(lhs, rhs); }
    if (op_type == Type_) { return TNE(lhs, rhs); }
    if (op_type->is_function()) { return FnNE(lhs, rhs); }

  } else if (op == Language::Operator::GE) {
    if (op_type == Int) { return IGE(lhs, rhs); }
    if (op_type == Real) { return FGE(lhs, rhs); }
    if (op_type == Uint) { return UGE(lhs, rhs); }

  } else if (op == Language::Operator::GT) {
    if (op_type == Int) { return IGT(lhs, rhs); }
    if (op_type == Real) { return FGT(lhs, rhs); }
    if (op_type == Uint) { return UGT(lhs, rhs); }
  }
  UNREACHABLE;
}

IR::Value ChainOp::EmitIR() {
  assert(!ops.empty());

  switch (ops[0]) {
  case Language::Operator::Xor: {
    std::vector<IR::Value> vals;
    for (auto e : exprs) { vals.push_back(e->EmitIR()); }

    IR::Value v = vals[0];
    for (size_t i = 1; i < vals.size(); ++i) { v = IR::BXor(v, vals[i]); }
    return v;
  } break;
  case Language::Operator::And:
  case Language::Operator::Or: {
    std::vector<IR::Block *> blocks(exprs.size(), nullptr);
    // If it's an or, an early exit is because we already know the value is
    // true. If it's an and, an early exit is beacause we already know the value
    // is false.
    bool using_or              = (ops[0] == Language::Operator::Or);
    IR::Value early_exit_value = IR::Value(using_or);

    for (auto &b : blocks) { b = IR::Func::Current->AddBlock("chainop-block"); }

    IR::Block::Current->exit.SetUnconditional(blocks.front());

    // Create the landing block
    IR::Block *landing_block = IR::Func::Current->AddBlock("chainop-landing");
    auto phi                 = IR::Phi(Bool);

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
      phi.args.emplace_back(IR::Block::Current);
      phi.args.emplace_back(early_exit_value);
    }

    IR::Block::Current = blocks.back();
    auto last_result = exprs.back()->EmitIR();
    IR::Block::Current->exit.SetUnconditional(landing_block);
    phi.args.emplace_back(IR::Block::Current);
    phi.args.emplace_back(last_result);

    IR::Block::Current = landing_block;
    IR::Block::Current->push(phi);

    return IR::Value(phi.result.reg);
  } break;
  case Language::Operator::LT:
  case Language::Operator::LE:
  case Language::Operator::EQ:
  case Language::Operator::NE:
  case Language::Operator::GE:
  case Language::Operator::GT: {
    // Operators here can be <, <=, ==, !=, >=, or >.
    std::vector<IR::Block *> blocks(exprs.size() - 1, nullptr);

    for (auto &b : blocks) { b = IR::Func::Current->AddBlock("eq-block"); }

    IR::Block *landing_block = IR::Func::Current->AddBlock("eq-land");
    auto phi                 = IR::Phi(Bool);

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
      phi.args.emplace_back(IR::Block::Current);
      phi.args.emplace_back(false);
    }

    IR::Block::Current = blocks.back();

    lhs              = rhs;
    rhs              = exprs.back()->EmitIR();
    auto last_result = EmitComparison(exprs.back()->type, ops.back(), lhs, rhs);
    IR::Block::Current->exit.SetUnconditional(landing_block);
    phi.args.emplace_back(IR::Block::Current);
    phi.args.emplace_back(last_result);

    IR::Block::Current = landing_block;
    IR::Block::Current->push(phi);

    return IR::Value::Reg(phi.result.reg);
  } break;
  case Language::Operator::Comma: UNREACHABLE;
  default: UNREACHABLE;
  }
}

IR::Value FunctionLiteral::EmitIR() {
  if (ir_func) { return IR::Value(ir_func); } // Cache
  ir_func = new IR::Func;

  fn_scope->entry_block = ir_func->entry();
  fn_scope->exit_block  = ir_func->AddBlock("func-exit");

  assert(type);
  assert(type->is_function());
  fn_scope->ret_val =
      IR::Value::FrameAddr(ir_func->PushSpace(((Function *)type)->output));
  IR::Func::Current  = ir_func;
  IR::Block::Current = ir_func->entry();

  statements->verify_types();

  for (auto decl : fn_scope->ordered_decls_) {
    if (decl->arg_val) {
      if (decl->arg_val->is_function_literal()) {
        auto fn_lit = (FunctionLiteral *)decl->arg_val;

        size_t arg_num = 0;
        for (const auto& d : fn_lit->inputs) {
          if (decl == d) {
            decl->stack_loc = IR::Value::Arg(arg_num);
            goto success_fn;
          }
          ++arg_num;
        }

        UNREACHABLE;

      success_fn:
        continue;

      } else if (decl->arg_val->is_parametric_struct_literal()) {
        auto param_struct = (ParametricStructLiteral *)decl->arg_val;
        size_t param_num = 0;
        for (const auto &p : param_struct->params) {
          if (decl == p) {
            decl->stack_loc = IR::Value::Arg(param_num);
            goto success_param_struct;
          }
          ++param_num;
        }

        UNREACHABLE;

      success_param_struct:
        continue;
      }
      UNREACHABLE;
    }
    ir_func->PushLocal(decl);
  }

  for (auto scope : fn_scope->innards_) {
    for (auto decl : scope->ordered_decls_) {
      if (decl->arg_val) { continue; }
      ir_func->PushLocal(decl);
    }
  }

  fn_scope->IR_Init();

  statements->EmitIR();

  IR::Block::Current->exit.SetUnconditional(fn_scope->exit_block);
  IR::Block::Current = fn_scope->exit_block;
  if (((Function *)type)->output == Void) {
    IR::Block::Current->exit.SetReturnVoid();
  } else {
    IR::Block::Current->exit.SetReturn(
        IR::Load(((Function *)type)->output, fn_scope->ret_val));
  }

  return IR::Value(ir_func);
}

IR::Value Statements::EmitIR() {
  for (auto stmt : statements) { stmt->EmitIR(); }
  return IR::Value();
}

IR::Value Identifier::EmitIR() {
  if (decl->arg_val && decl->arg_val->is_function_literal()) {
    // TODO Iterating through linearly is probably not smart.
    auto fn = (FunctionLiteral *)decl->arg_val;
    size_t arg_num = 0;
    for (auto in : fn->inputs) {
      if (decl != in) {
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
    auto fn            = value.as_expr->EmitIR();
    fn.as_func->name   = Mangle((Function *)type, this, decl->scope_);
    IR::Block::Current = current_block;
    IR::Func::Current  = current_func;
    return fn;

  } else {
    return PtrCallFix(type, decl->stack_loc);
  }
}

IR::Value ArrayType::EmitIR() {
  return (length->is_hole()
              ? IR::TC_Arr1(data_type->EmitIR())
              : IR::TC_Arr2(length->EmitIR(), data_type->EmitIR()));
}

IR::Value Declaration::EmitIR() {
  /*
  if (IsUninitialized()) {
    return IR::Value();

  } else if (IsDefaultInitialized()) {
    type->EmitInit(identifier->EmitLVal());

  } else {
    auto id_val  = identifier->EmitLVal();
    auto rhs_val = init_val->EmitIR();

    Type::IR_CallAssignment(scope_, identifier->type, init_val->type,
  rhs_val, id_val);
  }
  */
  return IR::Value();
}

IR::Value DummyTypeExpr::EmitIR() { return IR::Value(value.as_type); }

IR::Value Case::EmitIR() {

  std::vector<IR::Block *> key_blocks(key_vals.size(), nullptr);

  for (auto &b : key_blocks) { b = IR::Func::Current->AddBlock("key-block"); }
  IR::Block::Current->exit.SetUnconditional(key_blocks.front());

  // Create the landing block
  IR::Block *landing_block = IR::Func::Current->AddBlock("case-land");
  auto phi                 = IR::Phi(Bool);

  IR::Value result;
  for (size_t i = 0; i < key_vals.size() - 1; ++i) {
    auto compute_block = IR::Func::Current->AddBlock("case-compute");

    IR::Block::Current = key_blocks[i];
    result = key_vals[i].first->EmitIR();
    IR::Block::Current->exit.SetConditional(result, compute_block,
                                            key_blocks[i + 1]);

    IR::Block::Current = compute_block;
    result = key_vals[i].second->EmitIR();
    IR::Block::Current->exit.SetUnconditional(landing_block);
    phi.args.emplace_back(IR::Block::Current);
    phi.args.emplace_back(result);
  }

  // Assume last entry is "else => ___".
  IR::Block::Current = key_blocks.back();
  result = key_vals.back().second->EmitIR();
  IR::Block::Current->exit.SetUnconditional(landing_block);
  phi.args.emplace_back(IR::Block::Current);
  phi.args.emplace_back(result);

  IR::Block::Current = landing_block;
  IR::Block::Current->push(phi);

  return IR::Value::Reg(phi.result.reg);
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
    return array_type->fixed_length ? IR::Value(array_type->len)
                                    : IR::ArrayLength(eval);
  }

  if (base_type->is_struct()) {
    auto struct_type = (Structure *)base_type;

    if (!type->stores_data()) { NOT_YET; }
    auto index = struct_type->field_name_to_num AT(member_name);
    return PtrCallFix(struct_type->field_type AT(index),
                      IR::Field(struct_type, eval, index));
  }

  UNREACHABLE;
}

IR::Value While::EmitIR() {
  auto cond_block = IR::Func::Current->AddBlock("while-cond");
  auto body_block = IR::Func::Current->AddBlock("while-body");
  auto land_block = IR::Func::Current->AddBlock("while-land");

  IR::Block::Current->exit.SetUnconditional(cond_block);

  IR::Block::Current = cond_block;
  auto cond_val = condition->EmitIR();
  IR::Block::Current->exit.SetConditional(cond_val, body_block, land_block);

  IR::Block::Current = body_block;
//  for (auto decl : while_scope->ordered_decls_) { /*TODO Initialize*/
//  }
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

  auto land_block = IR::Func::Current->AddBlock("cond-land");

  for (auto &b : cond_blocks) { b = IR::Func::Current->AddBlock("cond-cond"); }
  for (auto &b : body_blocks) { b = IR::Func::Current->AddBlock("cond-body"); }
  for (auto &s : body_scopes) {
    s->land_block  = land_block;
    s->entry_block = IR::Func::Current->AddBlock("cond-entry");
    s->exit_block  = IR::Func::Current->AddBlock("cond-exit");
  }

  assert(!cond_blocks.empty());
  IR::Block::Current->exit.SetUnconditional(cond_blocks.front());

  for (size_t i = 0; i < conditions.size() - 1; ++i) {
    IR::Block::Current = cond_blocks[i];
    auto cond_val = conditions[i]->EmitIR();
    IR::Block::Current->exit.SetConditional(
        cond_val, body_scopes[i]->entry_block, cond_blocks[i + 1]);
  }

  // Last step
  IR::Block::Current = cond_blocks.back();
  auto cond_val      = conditions.back()->EmitIR();
  IR::Block::Current->exit.SetConditional(
      cond_val, body_scopes[conditions.size() - 1]->entry_block,
      has_else() ? body_scopes.back()->entry_block : land_block);

  for (size_t i = 0; i < body_scopes.size(); ++i) {
    IR::Block::Current = body_scopes[i]->entry_block;
    // TODO Initialize!
    IR::Block::Current->exit.SetUnconditional(body_blocks[i]);
    IR::Block::Current = body_blocks[i];

    statements[i]->EmitIR();

    if (IR::Block::Current->exit.flag == IR::Exit::Strategy::Unset) {
      IR::Block::Current->exit.SetUnconditional(body_scopes[i]->exit_block);
    }

    IR::Block::Current = body_scopes[i]->exit_block;
    // TODO Destroy!
    body_scopes[i]->exit_block->exit.SetUnconditional(land_block);
    // TODO get exit flag
  }

  IR::Block::Current = land_block;
  return IR::Value();
}

IR::Value ArrayLiteral::EmitIR() {
  // TODO delete allocation
  auto tmp_addr  = IR::Value::FrameAddr(IR::Func::Current->PushSpace(type));
  auto num_elems = elems.size();

  assert(type->is_array());
  auto data_type = ((Array *)type)->data_type;
  for (size_t i = 0; i < num_elems; ++i) {
    auto ptr = IR::Access(data_type, IR::Value(i), tmp_addr);
    Type::IR_CallAssignment(scope_, data_type, elems[i]->type,
                            elems[i]->EmitIR(), ptr);
  }

  return tmp_addr;
}
static void EmitSliceOrArray(IR::Block *init_block, IR::Block *phi_block,
                             IR::Block *cond_block, IR::Block *incr_block,
                             Array *array, InDecl *indecl, IR::Cmd *phi,
                             IR::Value *done_cmp, IR::Value start_ptr,
                             IR::Value end_ptr) {
  IR::Block::Current = phi_block;
  *phi               = IR::Phi(Ptr(indecl->type));
  auto phi_result = IR::Value::Reg(phi->result.reg);
  phi->args.emplace_back(init_block);
  phi->args.emplace_back(start_ptr);

  IR::Block::Current = cond_block;
  indecl->stack_loc  = phi_result;
  *done_cmp          = IR::BOr(*done_cmp, IR::PtrEQ(phi_result, end_ptr));

  IR::Block::Current = incr_block;
  auto next = IR::Access(array->data_type, IR::Value(1ul), phi_result);
  phi->args.emplace_back(incr_block);
  phi->args.emplace_back(next);
}

IR::Value For::EmitIR() {
  auto init_block = IR::Func::Current->AddBlock("for-init");
  auto incr_block = IR::Func::Current->AddBlock("for-incr");
  auto phi_block  = IR::Func::Current->AddBlock("for-phi");
  auto cond_block = IR::Func::Current->AddBlock("for-cond");

  IR::Block::Current->exit.SetUnconditional(init_block);

  auto done_cmp = IR::Value(false);

  auto num_iters = iterators.size();
  for (size_t i = 0; i < num_iters; ++i) {
    auto iter      = iterators[i];
    auto container = iter->container;

    IR::Cmd phi = IR::NOp();

    if (container->type->is_range()) {
      IR::Block::Current = init_block;
      auto start_val     = ((Binop *)container)->lhs->EmitIR();

      IR::Block::Current = phi_block;
      phi                = IR::Phi(iter->type);
      phi.args.emplace_back(init_block);
      phi.args.emplace_back(start_val);

      IR::Block::Current = cond_block;
      IR::Store(((Binop *)container)->lhs->type, IR::Value::Reg(phi.result.reg),
                iter->identifier->EmitLVal());

      if (container->is_binop()) {
        auto end_val = ((Binop *)container)->rhs->EmitIR();
        if (iter->type == Int) {
          done_cmp = IR::BOr(done_cmp,
                             IR::IGT(IR::Value::Reg(phi.result.reg), end_val));
        } else if (iter->type == Uint) {
          done_cmp = IR::BOr(done_cmp,
                             IR::UGT(IR::Value::Reg(phi.result.reg), end_val));
        } else if (iter->type == Char) {
          done_cmp = IR::BOr(done_cmp,
                             IR::CGT(IR::Value::Reg(phi.result.reg), end_val));
        } else {
          UNREACHABLE;
        }
      }

      IR::Block::Current = incr_block;
      IR::Value next;
      if (iter->identifier->type == Uint) {
        next = IR::UAdd(IR::Load(Uint, iter->identifier->EmitLVal()),
                        IR::Value(1ul));
      } else if (iter->identifier->type == Int) {
        next = IR::IAdd(IR::Load(Int, iter->identifier->EmitLVal()),
                        IR::Value(1l));

      } else if (iter->identifier->type == Char) {
        next = IR::CAdd(IR::Load(Char, iter->identifier->EmitLVal()),
                        IR::Value('\01'));

      } else {
        UNREACHABLE;
      }

      phi.args.emplace_back(incr_block);
      phi.args.emplace_back(next);

    } else if (container->type->is_slice()) {
      assert(container->is_binop());
      IR::Block::Current   = init_block;
      auto array_container = ((Binop *)container)->lhs;
      auto container_val   = array_container->EmitIR();

      IR::Value start_ptr, head_ptr, start_offset, end_offset, end_ptr;

      auto array_type = (Array *)array_container->type;
      if (array_type->fixed_length) {
        head_ptr = container_val;
      } else {
        head_ptr = IR::ArrayData(array_type, container_val);
      }

      auto range = ((Binop *)container)->rhs;
      assert(range->type->is_range());

      if (range->is_binop()) {
        start_offset = ((Binop *)range)->lhs->EmitIR();
        end_offset   = ((Binop *)range)->rhs->EmitIR();

        assert(((Binop *)range)->rhs->type == Int ||
               ((Binop *)range)->rhs->type == Uint);
        if (((Binop *)range)->rhs->type == Int) {
          end_offset = IR::UAdd(end_offset, IR::Value(1ul));
        } else {
          end_offset = IR::IAdd(end_offset, IR::Value(1l));
        }

      } else if (range->is_unop()) {
        start_offset = ((Unop *)range)->operand->EmitIR();
        end_offset = array_type->fixed_length ? IR::Value(array_type->len)
                                              : IR::ArrayLength(container_val);
      } else {
        UNREACHABLE;
      }

      start_ptr = IR::Access(array_type->data_type, start_offset, head_ptr);
      end_ptr   = IR::Access(array_type->data_type, end_offset, head_ptr);

      EmitSliceOrArray(init_block, phi_block, cond_block, incr_block,
                       array_type, iter, &phi, &done_cmp, start_ptr, end_ptr);

    } else if (container->type->is_array()) {
      IR::Block::Current = init_block;
      auto container_val = container->EmitIR();

      IR::Value start_ptr, len_val, end_ptr;

      auto array_type = (Array *)container->type;
      if (array_type->fixed_length) {
        start_ptr = container_val;
        len_val   = IR::Value(array_type->len);
      } else {
        start_ptr = IR::ArrayData(array_type, container_val);
        len_val   = IR::ArrayLength(container_val);
      }
      end_ptr = IR::Access(array_type->data_type, len_val, start_ptr);

      EmitSliceOrArray(init_block, phi_block, cond_block, incr_block,
                       array_type, iter, &phi, &done_cmp, start_ptr, end_ptr);

    } else if (container->type == Type_) {
      Ctx ctx;
      auto container_type = container->evaluate(ctx).as_type;
      assert(container_type->is_enum());
      auto enum_type = (Enumeration *)container_type;

      IR::Block::Current = phi_block;
      phi                = IR::Phi(Uint);
      phi.args.emplace_back(init_block);
      phi.args.emplace_back(IR::Value(0ul));


      IR::Block::Current = cond_block;
      auto phi_result    = IR::Value::Reg(phi.result.reg);
      done_cmp =
          IR::BOr(done_cmp,
                  IR::UEQ(phi_result, IR::Value(enum_type->int_values.size())));
      IR::Store(Uint, phi_result, iter->identifier->EmitLVal());

      IR::Block::Current = incr_block;
      auto next          = IR::UAdd(iter->identifier->EmitIR(), IR::Value(1ul));
      phi.args.emplace_back(incr_block);
      phi.args.emplace_back(next);

    } else {
      UNREACHABLE;
    }

    phi_block->push(phi);
  }

  assert(!for_scope->land_block);
  for_scope->land_block = IR::Func::Current->AddBlock("for-land");

  assert(!for_scope->entry_block);
  for_scope->entry_block = IR::Func::Current->AddBlock("for-entry");

  assert(!for_scope->exit_block);
  for_scope->exit_block = IR::Func::Current->AddBlock("for-exit");

  // Link blocks
  init_block->exit.SetUnconditional(phi_block);
  incr_block->exit.SetUnconditional(phi_block);
  phi_block->exit.SetUnconditional(cond_block);
  cond_block->exit.SetConditional(done_cmp, for_scope->land_block,
                                  for_scope->entry_block);


  auto body_block = IR::Func::Current->AddBlock("for-body");
  for_scope->IR_Init();
  for_scope->entry_block->exit.SetUnconditional(body_block);

  IR::Block::Current = body_block;
  statements->EmitIR();
  IR::Block::Current->exit.SetUnconditional(for_scope->exit_block);

  // TODO  for_scope->IR_destroy();

  // auto exit_flag = IR::Load(Char, ___);
  // FIXME for now, ...
  IR::Block::Current->exit.SetUnconditional(incr_block);

  IR::Block::Current = for_scope->land_block;

  return IR::Value();
}

IR::Value Jump::EmitIR() {
  // TODO not correct. Must call destructors
  IR::Block::Current->exit.SetReturnVoid();
  return IR::Value();
}
IR::Value Generic::EmitIR() { NOT_YET; }
IR::Value InDecl::EmitIR() { NOT_YET; }

IR::Value ParametricStructLiteral::EmitIR() { return IR::Value(value.as_type); }
IR::Value StructLiteral::EmitIR() { return IR::Value(value.as_type); }
IR::Value EnumLiteral::EmitIR() { return IR::Value(value.as_type); }
} // namespace AST
