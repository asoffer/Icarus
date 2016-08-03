#include "IR.h"
#include "Type/Type.h"
#include "Scope.h"
#include "Stack.h"

extern llvm::IRBuilder<> builder;

extern std::vector<IR::Func *> all_functions;
extern llvm::Module *global_module;
extern FileType file_type;

AST::FunctionLiteral *WrapExprIntoFunction(AST::Expression *expr) {
  auto fn_ptr = new AST::FunctionLiteral;

  fn_ptr->type               = Func(Void, expr->type);
  fn_ptr->fn_scope->fn_type  = (Function *)fn_ptr->type;
  fn_ptr->scope_             = expr->scope_;
  fn_ptr->statements         = new AST::Statements;
  fn_ptr->statements->scope_ = fn_ptr->fn_scope;
  fn_ptr->return_type_expr   = new AST::DummyTypeExpr(expr->loc, expr->type);
  AST::Unop *ret             = nullptr;

  if (expr->type != Void) {
    ret             = new AST::Unop;
    ret->scope_     = fn_ptr->fn_scope;
    ret->operand    = expr;
    ret->op         = Language::Operator::Return;
    ret->precedence = Language::precedence(ret->op);
    fn_ptr->statements->statements.push_back(ret);
  } else {
    fn_ptr->statements->statements.push_back(expr);
  }

  fn_ptr->verify_types();

  return fn_ptr;
}

IR::Value Evaluate(AST::Expression *expr) {
  if (expr->value != IR::Value::None()) { return expr->value; }
  auto old_func  = IR::Func::Current;
  auto old_block = IR::Block::Current;

  auto fn_ptr      = WrapExprIntoFunction(expr);
  auto local_stack = new IR::LocalStack;
  IR::Func *func   = fn_ptr->EmitAnonymousIR().as_func;

  func->SetName("anonymous-func");

  expr->verify_types();
  IR::Func::Current  = old_func;
  IR::Block::Current = old_block;

  auto result = func->Call(local_stack, {});
  delete local_stack;

  if (fn_ptr->type == Func(Void, Void)) {
    fn_ptr->statements->statements[0] = nullptr;
  } else {
    auto ret = fn_ptr->statements->statements.front();
    assert(ret->is_unop() && ((AST::Unop *)ret)->op == Language::Operator::Return);
    ((AST::Unop *)ret)->operand = nullptr;
  }
  delete fn_ptr;

  return result;
}

IR::Func *AsciiFunc() {
  static IR::Func *ascii_ = nullptr;

  if (ascii_) { return ascii_; }
  ascii_ = new IR::Func(Func(Uint, Char));
  ascii_->SetName("ascii");

  auto saved_func  = IR::Func::Current;
  auto saved_block = IR::Block::Current;

  IR::Func::Current  = ascii_;
  IR::Block::Current = ascii_->entry();

  auto val = IR::Trunc(IR::Value::Arg(0));

  IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
  IR::Block::Current = IR::Func::Current->exit();
  IR::Block::Current->SetReturn(val);

  IR::Func::Current  = saved_func;
  IR::Block::Current = saved_block;

  return ascii_;
}

IR::Func *OrdFunc() {
  static IR::Func *ord_ = nullptr;

  if (ord_) { return ord_; }
  ord_ = new IR::Func(Func(Char, Uint));
  ord_->SetName("ord");

  auto saved_func  = IR::Func::Current;
  auto saved_block = IR::Block::Current;

  IR::Func::Current  = ord_;
  IR::Block::Current = ord_->entry();

  auto val = IR::ZExt(IR::Value::Arg(0));

  IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
  IR::Block::Current = IR::Func::Current->exit();
  IR::Block::Current->SetReturn(val);

  IR::Func::Current  = saved_func;
  IR::Block::Current = saved_block;

  return ord_;
}

extern std::string Mangle(const Function *f, AST::Expression *expr,
                          Scope *starting_scope);
extern IR::Value PtrCallFix(Type *t, IR::Value v);
extern IR::Func *GetFuncReferencedIn(Scope *scope, const std::string &fn_name,
                                     Function *fn_type);

size_t IR::Func::PushSpace(Type *t) {
  size_t bytes     = t->bytes();
  size_t alignment = t->alignment();

  frame_size = MoveForwardToAlignment(frame_size, alignment);

  auto result = frame_size;
  frame_size += bytes;

  if (file_type != FileType::None) {
    auto ip = builder.saveIP();
    builder.SetInsertPoint(alloc_block);
    if (t != Void && t != Type_) {
      frame_map[result] =
          builder.CreateAlloca(*(t->is_function() ? Ptr(t) : t));
    }

    builder.restoreIP(ip);
  }

  return result;
}

void IR::Func::PushLocal(AST::Declaration *decl) {
  decl->addr = IR::Value::FrameAddr(PushSpace(decl->type));
}

static std::vector<const char *> const_strs_;
const char *GetGlobalStringNumbered(size_t index) {
  assert(index < const_strs_.size());
  return const_strs_[index];
}

static IR::Value FindOrInsertGlobalCStr(const char *cstr) {
  assert(cstr);
  assert(*cstr == '\1');

  auto num_strs = const_strs_.size();
  for (size_t i = 0; i < num_strs; ++i) {
    // + 1 because we're starting with either a 0 or 1 to tell us if we own the
    // memory associated with the string.
    if (strcmp(const_strs_[i] + 1, cstr + 1) == 0) {
      return IR::Value::GlobalCStr(i);
    }
  }
  size_t len = strlen(cstr) + 1;
  char *new_cstr = new char[len];
  memcpy(new_cstr, cstr, len);
  // The + 1 is to skip the first entry telling us if we own the memory or not.
  const_strs_.push_back(new_cstr + 1);
  return IR::Value::GlobalCStr(num_strs);
}

namespace AST {
IR::Value Terminal::EmitIR() {
  switch (terminal_type) {
  case Language::Terminal::ASCII: return IR::Value(AsciiFunc());
  case Language::Terminal::Null: return IR::Value::Null(type);
  case Language::Terminal::Ord: return IR::Value(OrdFunc());
  case Language::Terminal::Return:
    IR::Store(Char, RETURN_FLAG, scope_->GetFnScope()->exit_flag);
    assert(scope_->is_block_scope() || scope_->is_function_scope());
    IR::Block::Current->SetUnconditional(((BlockScope *)scope_)->exit_block);
    return IR::Value::None();
  case Language::Terminal::StringLiteral:
    return FindOrInsertGlobalCStr(value.as_cstr);
  case Language::Terminal::True: return IR::Value(true);
  case Language::Terminal::False: return IR::Value(false);
  case Language::Terminal::Char:
  case Language::Terminal::Int:
  case Language::Terminal::Type:
  case Language::Terminal::Real:
  case Language::Terminal::Uint: return value;
  default: UNREACHABLE;
  }
}

static void EmitPrintExpr(Expression *expr) {
  if (expr->type->is_primitive() || expr->type->is_enum() ||
      expr->type->is_pointer()) {
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
    return IR::Value::None();
  } break;
  case Language::Operator::Free: {
    IR::Free(operand->EmitIR());
    return IR::Value::None();
  } break;
  case Language::Operator::Eval: {
    auto old_func  = IR::Func::Current;
    auto old_block = IR::Block::Current;

    auto fn_ptr      = WrapExprIntoFunction(operand);
    auto local_stack = new IR::LocalStack;
    IR::Func *func = fn_ptr->EmitAnonymousIR().as_func;
    func->SetName("anonymous-func");

    if (operand->type == Void) {
      // Assumptions:
      //  * There's only one exit block.
      //  * The current block is pointing to it. This is NOT threadsafe.
      IR::Block::Current->SetReturnVoid();
    }

    IR::Func::Current  = old_func;
    IR::Block::Current = old_block;

    auto result = func->Call(local_stack, {});
    delete local_stack;

    // Cleanup (Need to set ret->operand to null so it isn't cleaned up by
    // deleting the fn_ptr). TODO
    // ret->operand = nullptr;
    delete fn_ptr;
    return result;
  } break;
  case Language::Operator::Print: {
    if (operand->is_comma_list()) {
      auto operand_as_chainop = (ChainOp *)operand;
      for (auto op : operand_as_chainop->exprs) { EmitPrintExpr(op); }
    } else {
      EmitPrintExpr(operand);
    }
    return IR::Value::None();
  } break;
  case Language::Operator::And:
    return (operand->type == Type_) ? IR::TC_Ptr(operand->EmitIR())
                                    : operand->EmitLVal();
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
  case Language::Operator::At: return PtrCallFix(type, operand->EmitIR());
  default: UNREACHABLE;
  }
}

IR::Value Binop::EmitIR() {
  switch (op) {
  case Language::Operator::Assign: {
    Type::CallAssignment(scope_, lhs->type, rhs->type, rhs->EmitIR(),
                         lhs->EmitLVal());
    return IR::Value::None();
  } break;
  case Language::Operator::Cast: {
    return IR::Cast(lhs->type, Evaluate(rhs).as_type, lhs->EmitIR());
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

      IR::Block::Current->SetConditional(
          lhs_val,
          (op == Language::Operator::OrEq) ? land_block : load_rhs_block,
          (op == Language::Operator::AndEq) ? land_block : load_rhs_block);

      IR::Block::Current = load_rhs_block;
      auto rhs_val = rhs->EmitIR();
      IR::Store(Bool, rhs_val, lval);

      IR::Block::Current->SetUnconditional(land_block);

      IR::Block::Current = land_block;
      return IR::Value::None();
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

#define ARITHMETIC_EQ_CASE(Op, op_str)                                         \
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
    auto fn = GetFuncReferencedIn(                                             \
        scope_, op_str, Func(Tup({Ptr(lhs->type), rhs->type}), type));         \
    return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});      \
  } break

    ARITHMETIC_EQ_CASE(Add, "__add__");
    ARITHMETIC_EQ_CASE(Sub, "__sub__");
    ARITHMETIC_EQ_CASE(Mul, "__mul__");
    ARITHMETIC_EQ_CASE(Div, "__div__");
    ARITHMETIC_EQ_CASE(Mod, "__mod__");
#undef ARITHMETIC_EQ_CASE

#define ARITHMETIC_CASE(Op, op_str)                                            \
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
    auto fn = GetFuncReferencedIn(scope_, op_str,                              \
                                  Func(Tup({lhs->type, rhs->type}), type));    \
    return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});      \
  } break

    ARITHMETIC_CASE(Add, "__add__");
    ARITHMETIC_CASE(Sub, "__sub__");
    ARITHMETIC_CASE(Div, "__div__");
    ARITHMETIC_CASE(Mod, "__mod__");
#undef ARITHMETIC_CASE

  case Language::Operator::Mul: {
    auto lhs_val = lhs->EmitIR();
    auto rhs_val = rhs->EmitIR();
    if (lhs->type == Int && rhs->type == Int) {
      return IR::IMul(lhs_val, rhs_val);
    }
    if (lhs->type == Uint && rhs->type == Uint) {
      return IR::UMul(lhs_val, rhs_val);
    }
    if (lhs->type == Real && rhs->type == Real) {
      return IR::FMul(lhs_val, rhs_val);
    }
    if (lhs->type->is_function() && rhs->type->is_function() &&
        ((Function *)lhs)->input == ((Function *)lhs)->input) {
      assert(type->is_function());
      auto fn_type   = (Function *)type;
      auto composite = new IR::Func(fn_type);

      auto saved_func  = IR::Func::Current;
      auto saved_block = IR::Block::Current;

      IR::Func::Current  = composite;
      IR::Block::Current = composite->entry();

      std::vector<IR::Value> args = {};
      auto num_entries = fn_type->input->is_tuple()
                             ? ((Tuple *)fn_type->input)->entries.size()
                             : 1;
      for (size_t i = 0; i < num_entries; ++i) {
        args.push_back(IR::Value::Arg(i));
      }

      auto intermediate_val =
          IR::Call(((Function *)rhs->type)->output, rhs->EmitIR(), args);

      auto result =
          IR::Call(fn_type->output, lhs->EmitIR(), {intermediate_val});

      IR::Block::Current->SetUnconditional(IR::Func::Current->exit());
      IR::Block::Current = IR::Func::Current->exit();
      IR::Block::Current->SetReturn(result);

      IR::Func::Current  = saved_func;
      IR::Block::Current = saved_block;

      return IR::Value(composite);
    }
    auto fn = GetFuncReferencedIn(scope_, "__mul__",
                                  Func(Tup({lhs->type, rhs->type}), type));
    return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});
  } break;

  case Language::Operator::Index: {
    if (lhs->type->is_array()) {
      return PtrCallFix(type, EmitLVal());

    } else if (lhs->type == String) {
      return IR::Load(Char, EmitLVal());

    } else {
      auto fn = GetFuncReferencedIn(scope_, "__bracket__",
                                    Func(Tup({lhs->type, rhs->type}), type));
      return IR::Call(type, IR::Value(fn), {lhs->EmitIR(), rhs->EmitIR()});
    }
  } break;

  case Language::Operator::Call: {
    std::vector<IR::Value> args;
    if (rhs) {
      if (rhs->is_comma_list()) {
        for (auto v : ((ChainOp *)rhs)->exprs) { args.push_back(v->EmitIR()); }
      } else {
        args = {rhs->EmitIR()};
      }
    }

    if (lhs->type == Type_) {
      auto t = Evaluate(lhs).as_type;
      assert(t->is_parametric_struct());
      return IR::Call(type, IR::Value(((ParamStruct *)t)->IRFunc()), args);
    } else {
      return IR::Call(type, lhs->EmitIR(), args);
    }
  } break;
  default: UNREACHABLE;
  }
}

static IR::Value EmitComparison(Scope *scope, Type *op_type,
                                Language::Operator op, IR::Value lhs,
                                IR::Value rhs) {
  // TODO pass in lhs and rhs types and output type
  if (op == Language::Operator::LT) {
    if (op_type == Int) { return ILT(lhs, rhs); }
    if (op_type == Real) { return FLT(lhs, rhs); }
    if (op_type == Uint) { return ULT(lhs, rhs); }

    auto fn = GetFuncReferencedIn(scope, "__lt__",
                                  Func(Tup({op_type, op_type}), Bool));
    return IR::Call(Bool, IR::Value(fn), {lhs, rhs});

  } else if (op == Language::Operator::LE) {
    if (op_type == Int) { return ILE(lhs, rhs); }
    if (op_type == Real) { return FLE(lhs, rhs); }
    if (op_type == Uint) { return ULE(lhs, rhs); }

    auto fn = GetFuncReferencedIn(scope, "__le__",
                                  Func(Tup({op_type, op_type}), Bool));
    return IR::Call(Bool, IR::Value(fn), {lhs, rhs});

  } else if (op == Language::Operator::EQ) {
    if (op_type == Bool) { return BEQ(lhs, rhs); }
    if (op_type == Char) { return CEQ(lhs, rhs); }
    if (op_type == Int) { return IEQ(lhs, rhs); }
    if (op_type == Real) { return FEQ(lhs, rhs); }
    if (op_type == Uint) { return UEQ(lhs, rhs); }
    if (op_type->is_enum()) { return UEQ(lhs, rhs); }
    if (op_type == Type_) { return TEQ(lhs, rhs); }
    if (op_type->is_function()) { return FnEQ(lhs, rhs); }

    auto fn = GetFuncReferencedIn(scope, "__eq__",
                                  Func(Tup({op_type, op_type}), Bool));
    return IR::Call(Bool, IR::Value(fn), {lhs, rhs});

  } else if (op == Language::Operator::NE) {
    if (op_type == Bool) { return BNE(lhs, rhs); }
    if (op_type == Char) { return CNE(lhs, rhs); }
    if (op_type == Int) { return INE(lhs, rhs); }
    if (op_type == Real) { return FNE(lhs, rhs); }
    if (op_type == Uint) { return UNE(lhs, rhs); }
    if (op_type->is_enum()) { return UNE(lhs, rhs); }
    if (op_type == Type_) { return TNE(lhs, rhs); }
    if (op_type->is_function()) { return FnNE(lhs, rhs); }

    auto fn = GetFuncReferencedIn(scope, "__ne__",
                                  Func(Tup({op_type, op_type}), Bool));
    return IR::Call(Bool, IR::Value(fn), {lhs, rhs});

  } else if (op == Language::Operator::GE) {
    if (op_type == Int) { return IGE(lhs, rhs); }
    if (op_type == Real) { return FGE(lhs, rhs); }
    if (op_type == Uint) { return UGE(lhs, rhs); }

    auto fn = GetFuncReferencedIn(scope, "__ge__",
                                  Func(Tup({op_type, op_type}), Bool));
    return IR::Call(Bool, IR::Value(fn), {lhs, rhs});

  } else if (op == Language::Operator::GT) {
    if (op_type == Int) { return IGT(lhs, rhs); }
    if (op_type == Real) { return FGT(lhs, rhs); }
    if (op_type == Uint) { return UGT(lhs, rhs); }

    auto fn = GetFuncReferencedIn(scope, "__gt__",
                                  Func(Tup({op_type, op_type}), Bool));
    return IR::Call(Bool, IR::Value(fn), {lhs, rhs});
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

    IR::Block::Current->SetUnconditional(blocks.front());

    std::vector<IR::Value> phi_args;

    // Where we end from after emitting the element in the chain. We later go
    // through and set the appropriate field to point to the landing_block. We
    // don't declare the landing block too early because it starts with a phi
    // node and phi nodes must receive from lower numbered blocks.
    std::vector<IR::Block *> end_blocks;

    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      IR::Block::Current = blocks[i];
      auto result = exprs[i]->EmitIR();
      end_blocks.push_back(IR::Block::Current);
      if (using_or) {
        IR::Block::Current->SetConditional(result, nullptr, blocks[i + 1]);
      } else {
        IR::Block::Current->SetConditional(result, blocks[i + 1], nullptr);
      }
      phi_args.emplace_back(IR::Block::Current);
      phi_args.emplace_back(early_exit_value);
    }

    IR::Block::Current = blocks.back();
    auto last_result   = exprs.back()->EmitIR();

    // Create the landing block
    IR::Block *landing_block = IR::Func::Current->AddBlock("chainop-landing");

    IR::Block::Current->SetUnconditional(landing_block);
    phi_args.emplace_back(IR::Block::Current);
    phi_args.emplace_back(last_result);

    for (auto &b : end_blocks) {
      assert(b->exit->is_conditional());
      if (using_or) {
        ((IR::Exit::Conditional *)b->exit)->true_block = landing_block;
      } else {
        ((IR::Exit::Conditional *)b->exit)->false_block = landing_block;
      }
    }

    // TODO clean up API.
    auto phi           = IR::Phi(Bool);
    phi.args           = std::move(phi_args);
    IR::Block::Current = landing_block;
    IR::Block::Current->push(phi);

    return IR::Value::Reg(phi.result.reg);
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
    IR::Block::Current->SetUnconditional(blocks.front());
    assert(exprs.size() >= 2);
    for (size_t i = 0; i < exprs.size() - 2; ++i) {
      IR::Block::Current = blocks[i];
      lhs                = rhs;
      rhs                = exprs[i + 1]->EmitIR();
      result             = EmitComparison(scope_, exprs[i]->type, ops[i], lhs, rhs);

      // Early exit
      IR::Block::Current->SetConditional(result, blocks[i + 1], landing_block);
      phi.args.emplace_back(IR::Block::Current);
      phi.args.emplace_back(false);
    }

    IR::Block::Current = blocks.back();

    lhs = rhs;
    rhs = exprs.back()->EmitIR();
    auto last_result =
        EmitComparison(scope_, exprs.back()->type, ops.back(), lhs, rhs);
    IR::Block::Current->SetUnconditional(landing_block);
    phi.args.emplace_back(IR::Block::Current);
    phi.args.emplace_back(last_result);

    IR::Block::Current = landing_block;
    IR::Block::Current->push(phi);
    return IR::Value::Reg(phi.result.reg);
  } break;
  case Language::Operator::Comma: {
    // TODO should this always be a tuple of types? Definitely not.
    std::vector<IR::Value> vals;
    for (auto expr : exprs) { vals.push_back(expr->EmitIR()); }
    return IR::TC_Tup(vals);
  } break;
  default: UNREACHABLE;
  }
}

IR::Value FunctionLiteral::EmitIR() { return Emit(true); }

IR::Value FunctionLiteral::Emit(bool should_gen) {
  if (ir_func) { return IR::Value(ir_func); } // Cache
  if (type->has_vars()) { return IR::Value::None(); }

  auto saved_func  = IR::Func::Current;
  auto saved_block = IR::Block::Current;

  assert(type->is_function());
  ir_func = new IR::Func((Function *)type, should_gen);

  fn_scope->entry_block = ir_func->entry();
  fn_scope->exit_block  = ir_func->exit();

  assert(type);
  assert(type->is_function());

  fn_scope->ret_val =
      IR::Value::FrameAddr(ir_func->PushSpace(((Function *)type)->output));
  fn_scope->exit_flag = IR::Value::FrameAddr(ir_func->PushSpace(Char));

  IR::Func::Current  = ir_func;
  IR::Block::Current = ir_func->entry();

  statements->verify_types();

  for (auto decl : fn_scope->DeclRegistry) {
    if (decl->type->has_vars()) { continue; }
    if (decl->arg_val) {
      if (decl->arg_val->is_function_literal()) {
        auto fn_lit = (FunctionLiteral *)decl->arg_val;

        size_t arg_num = 0;
        for (const auto &d : fn_lit->inputs) {
          if (decl == d) {
            decl->addr = IR::Value::Arg(arg_num);
            goto success;
          }
          ++arg_num;
        }

        UNREACHABLE;
      } else if (decl->arg_val->is_dummy() &&
                 decl->arg_val->value.as_type->is_parametric_struct()) {
        auto param_struct = (ParamStruct *)decl->arg_val->value.as_type;
        size_t param_num = 0;
        for (const auto &p : param_struct->params) {
          if (decl == p) {
            decl->addr = IR::Value::Arg(param_num);
            goto success;
          }
          ++param_num;
        }

        UNREACHABLE;
      }

    } else {
      ir_func->PushLocal(decl);
    }
  success:;
  }

  for (auto scope : fn_scope->innards_) {
    if (!scope->is_block_scope() || scope->is_function_scope()) { continue; }
    for (auto decl : scope->DeclRegistry) {
      if (decl->arg_val || decl->is_in_decl() ||
          decl->type->time() == Time::compile) {
        continue;
      }
      ir_func->PushLocal(decl);
    }
  }

  IR::Block::Current->SetUnconditional(fn_scope->entry_block);

  statements->EmitIR();

  IR::Block::Current->SetUnconditional(fn_scope->exit_block);
  fn_scope->InsertDestroy();

  if (((Function *)type)->output == Void) {
    IR::Block::Current->SetReturnVoid();
  } else {
    IR::Block::Current->SetReturn(
        IR::Load(((Function *)type)->output, fn_scope->ret_val));
  }

  IR::Func::Current  = saved_func;
  IR::Block::Current = saved_block;

  return IR::Value(ir_func);
}

IR::Value Statements::EmitIR() {
  for (auto stmt : statements) { stmt->EmitIR(); }
  return IR::Value::None();
}

IR::Value Identifier::EmitIR() {
  if (type == Type_) { // TODO move this to wherever it should be
    if (decl->arg_val) {
      if (decl->arg_val->is_function_literal()) {
        auto fn_lit = (FunctionLiteral *)decl->arg_val;
        for (size_t i = 0; i < fn_lit->inputs.size(); ++i) {
          if (decl == fn_lit->inputs[i]) { return IR::Value::Arg(i); }
        }
      } else {
        assert(decl->arg_val->is_dummy() &&
               decl->arg_val->value.as_type->is_parametric_struct());
        auto param_struct = (ParamStruct *)decl->arg_val->value.as_type;
        for (size_t i = 0; i < param_struct->params.size(); ++i) {
          if (decl == param_struct->params[i]) { return IR::Value::Arg(i); }
        }
      }
      UNREACHABLE;
    } else {
      if (!decl->init_val) {
        assert(decl->is_generic());
        return decl->EmitIR();
      }

      if (!value.as_type) {
        value = IR::Value(Evaluate(decl->init_val).as_type);
        // TODO this should be cached in the evaluate method
      }
      return IR::Value(value.as_type);
    }
  }

  if (decl->is_in_decl()) { return decl->addr; }

  if (decl->arg_val && decl->arg_val->is_function_literal()) {
    // TODO Iterating through linearly is probably not smart.
    auto fn        = (FunctionLiteral *)decl->arg_val;
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
    if (decl->addr != IR::Value::None()) { return decl->addr; }
    assert(!decl->type->has_vars());

    auto fn_type = (Function *)type;

    // TODO Is this really the place to name the function? Shouldn't that be
    // done in its declaration?

    auto old_func  = IR::Func::Current;
    auto old_block = IR::Block::Current;

    if (decl->init_val) {
      decl->addr = decl->init_val->EmitIR();
      decl->addr.as_func->SetName(
          Mangle(fn_type, decl->identifier, decl->scope_));
    } else if (decl->HasHashtag("cstdlib")) {
      auto fn = new IR::Func(fn_type, false);
      // TODO call mangle even though it's known to be cstdlib?
      fn->SetName(token);
      fn->generated                    = IR::Func::Gen::ToLink;
      all_functions.push_back(fn);

      if (file_type != FileType::None) {
        llvm::FunctionType *llvm_fn_type = *fn_type;
        fn->llvm_fn = (llvm::Function *)global_module->getOrInsertFunction(
            fn->GetName(), llvm_fn_type);
      }

      decl->addr = IR::Value(fn);
    } else if (decl->type->is_function()) {
      return IR::Load(type, decl->addr);

    } else {
      std::cerr << *decl << '\n';
      NOT_YET;
    }

    IR::Func::Current  = old_func;
    IR::Block::Current = old_block;

    return decl->addr;

  } else {
    return PtrCallFix(type, decl->addr);
  }
}

IR::Value ArrayType::EmitIR() {
  return (length->is_hole()
              ? IR::TC_Arr1(data_type->EmitIR())
              : IR::TC_Arr2(length->EmitIR(), data_type->EmitIR()));
}

IR::Value Declaration::EmitIR() {
  if (IsUninitialized()) {
    return IR::Value::None();

  } else if (IsDefaultInitialized()) {
    type->EmitInit(identifier->EmitLVal());

  } else {
    if (type != Type_) {
      Type::CallAssignment(scope_, identifier->type, init_val->type,
                           init_val->EmitIR(), identifier->EmitLVal());
    }
  }

  return IR::Value::None();
}

IR::Value Case::EmitIR() {

  std::vector<IR::Block *> key_blocks(key_vals.size(), nullptr);

  for (auto &b : key_blocks) { b = IR::Func::Current->AddBlock("key-block"); }
  IR::Block::Current->SetUnconditional(key_blocks.front());

  // Create the landing block
  IR::Block *landing_block = IR::Func::Current->AddBlock("case-land");
  auto phi                 = IR::Phi(type);

  IR::Value result;
  for (size_t i = 0; i < key_vals.size() - 1; ++i) {
    auto compute_block = IR::Func::Current->AddBlock("case-compute");

    IR::Block::Current = key_blocks[i];
    result = key_vals[i].first->EmitIR();
    IR::Block::Current->SetConditional(result, compute_block,
                                       key_blocks[i + 1]);

    IR::Block::Current = compute_block;
    result = key_vals[i].second->EmitIR();
    IR::Block::Current->SetUnconditional(landing_block);
    phi.args.emplace_back(IR::Block::Current);
    phi.args.emplace_back(result);
  }

  // Assume last entry is "else => ___".
  IR::Block::Current = key_blocks.back();
  result = key_vals.back().second->EmitIR();
  IR::Block::Current->SetUnconditional(landing_block);
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
                                    : IR::Load(Uint, IR::ArrayLength(eval));
  }

  if (base_type->is_struct()) {
    auto struct_type = (Struct *)base_type;

    if (!type->stores_data()) { NOT_YET; }
    auto index = struct_type->field_name_to_num AT(member_name);
    return PtrCallFix(struct_type->field_type AT(index),
                      IR::Field(struct_type, eval, index));
  }

  if (base_type == Type_) {
    auto ty = Evaluate(operand).as_type;
    if (ty->is_enum()) {
      return ((Enum *)ty)->EmitLiteral(member_name);

    } else {
      UNREACHABLE;
    }
  }

  std::cerr << *this << std::endl;
  UNREACHABLE;
}

IR::Value While::EmitIR() {
  auto cond_block = IR::Func::Current->AddBlock("while-cond");
  auto body_block = IR::Func::Current->AddBlock("while-body");
  auto land_block = IR::Func::Current->AddBlock("while-land");

  while_scope->entry_block = IR::Func::Current->AddBlock("while-entry");
  while_scope->exit_block  = IR::Func::Current->AddBlock("while-exit");

  IR::Block::Current->SetUnconditional(cond_block);

  IR::Block::Current = cond_block;
  auto cond_val = condition->EmitIR();
  IR::Block::Current->SetConditional(cond_val, while_scope->entry_block,
                                     land_block);

  IR::Block::Current = while_scope->entry_block;
  IR::Store(Char, CONTINUE_FLAG, while_scope->GetFnScope()->exit_flag);
  IR::Block::Current->SetUnconditional(body_block);
  IR::Block::Current = body_block;
  statements->EmitIR();

  IR::Block::Current->SetUnconditional(while_scope->exit_block);
  while_scope->InsertDestroy();
  IR::Block::Current->SetSwitch(
      IR::Load(Char, while_scope->GetFnScope()->exit_flag), cond_block);
  IR::Exit::Switch *while_exit_switch =
      (IR::Exit::Switch *)IR::Block::Current->exit;
  // TODO what about RESTART_FLAG?
  while_exit_switch->AddEntry(CONTINUE_FLAG, cond_block);
  while_exit_switch->AddEntry(REPEAT_FLAG, while_scope->entry_block);
  while_exit_switch->AddEntry(BREAK_FLAG, land_block);
  assert(while_scope->parent->is_block_scope() ||
         while_scope->parent->is_function_scope());
  while_exit_switch->AddEntry(RETURN_FLAG,
                              ((BlockScope *)while_scope->parent)->exit_block);

  IR::Block::Current = land_block;

  return IR::Value::None();
}

IR::Value Conditional::EmitIR() {
  std::vector<IR::Block *> cond_blocks(conditions.size(), nullptr);
  std::vector<IR::Block *> body_blocks(body_scopes.size(), nullptr);

  auto land_block = IR::Func::Current->AddBlock("cond-land");

  for (auto &b : cond_blocks) { b = IR::Func::Current->AddBlock("cond-cond"); }
  for (auto &b : body_blocks) { b = IR::Func::Current->AddBlock("cond-body"); }
  for (auto &s : body_scopes) {
    s->entry_block = IR::Func::Current->AddBlock("cond-entry");
    s->exit_block  = IR::Func::Current->AddBlock("cond-exit");
  }

  assert(!cond_blocks.empty());
  IR::Block::Current->SetUnconditional(cond_blocks.front());

  for (size_t i = 0; i < conditions.size() - 1; ++i) {
    IR::Block::Current = cond_blocks[i];
    auto cond_val = conditions[i]->EmitIR();
    IR::Block::Current->SetConditional(cond_val, body_scopes[i]->entry_block,
                                       cond_blocks[i + 1]);
  }

  // Last step
  IR::Block::Current = cond_blocks.back();
  auto cond_val = conditions.back()->EmitIR();
  IR::Block::Current->SetConditional(
      cond_val, body_scopes[conditions.size() - 1]->entry_block,
      has_else() ? body_scopes.back()->entry_block : land_block);

  for (size_t i = 0; i < body_scopes.size(); ++i) {
    IR::Block::Current = body_scopes[i]->entry_block;
    IR::Block::Current->SetUnconditional(body_blocks[i]);
    IR::Block::Current = body_blocks[i];

    statements[i]->EmitIR();

    IR::Block::Current->SetUnconditional(body_scopes[i]->exit_block);
    body_scopes[i]->InsertDestroy();

    assert(body_scopes[i]->parent && body_scopes[i]->parent->is_block_scope() &&
           ((BlockScope *)body_scopes[i]->parent)->exit_block);
    IR::Block::Current->SetConditional(
        IR::CEQ(IR::Load(Char, body_scopes[i]->GetFnScope()->exit_flag),
                NORMAL_FLAG),
        land_block, ((BlockScope *)body_scopes[i]->parent)->exit_block);
  }

  IR::Block::Current = land_block;
  return IR::Value::None();
}

IR::Value ArrayLiteral::EmitIR() {
  // TODO delete allocation
  auto tmp_addr  = IR::Value::FrameAddr(IR::Func::Current->PushSpace(type));
  auto num_elems = elems.size();

  assert(type->is_array());
  auto data_type = ((Array *)type)->data_type;
  // TODO doing this incrementally instead of accessing from the head. This will
  // likely make register allocation better.
  for (size_t i = 0; i < num_elems; ++i) {
    auto ptr = IR::Access(data_type, IR::Value(i), tmp_addr);
    Type::CallAssignment(scope_, data_type, elems[i]->type, elems[i]->EmitIR(),
                         ptr);
  }

  return tmp_addr;
}

static void ComputeAndStoreRangeValues(Expression *range, IR::Value &left,
                                       IR::Value &right) {
  assert(range->is_binop() || range->is_unop());

  if (range->is_binop()) {
    left  = ((Binop *)range)->lhs->EmitIR();
    right = ((Binop *)range)->rhs->EmitIR();
  } else {
    left  = ((Unop *)range)->operand->EmitIR();
    right = IR::Value::None();
  }
}

static void ComputeAndStoreArrayBounds(Array *array_type,
                                       IR::Value container_val,
                                       IR::Value &head_ptr,
                                       IR::Value &len_val) {
  if (array_type->fixed_length) {
    head_ptr = IR::Access(array_type->data_type, IR::Value(0ul), container_val);
    len_val  = IR::Value(array_type->len);
  } else {
    head_ptr = IR::Load(Ptr(array_type->data_type),
                        IR::ArrayData(array_type, container_val));
    len_val = IR::Load(Uint, IR::ArrayLength(container_val));
  }
}

IR::Value For::EmitIR() {
  auto num_iters = iterators.size();

  auto init_block = IR::Func::Current->AddBlock("for-init");
  IR::Block::Current->SetUnconditional(init_block);
  IR::Block::Current = init_block;

  std::vector<IR::Value> start_vals(num_iters);
  std::vector<IR::Value> end_vals(num_iters);

  for (size_t i = 0; i < num_iters; ++i) {
    auto iter = iterators[i];

    if (iter->container->type->is_array()) {
      auto container_val = iter->container->EmitIR();
      auto array_type    = (Array *)iter->container->type;

      IR::Value head_ptr, len_val;
      ComputeAndStoreArrayBounds(array_type, container_val, head_ptr, len_val);

      start_vals[i] =
          IR::PtrIncr(Ptr(array_type->data_type), head_ptr, IR::Value(0ul));
      end_vals[i] = IR::PtrIncr(Ptr(array_type->data_type), head_ptr, len_val);

    } else if (iter->container->type->is_range()) {
      ComputeAndStoreRangeValues(iter->container, start_vals[i], end_vals[i]);

    } else if (iter->container->type->is_slice()) {
      assert(iter->container->is_binop());
      auto array_container = ((Binop *)iter->container)->lhs;
      auto container_val   = array_container->EmitIR();
      auto array_type      = (Array *)array_container->type;

      // TODO we're computing the length value here (emiting instructions to do
      // so) even though it's unnecessary.
      IR::Value head_ptr, len_val, start_offset, end_offset;

      auto range = ((Binop *)iter->container)->rhs;
      ComputeAndStoreArrayBounds(array_type, container_val, head_ptr, len_val);
      ComputeAndStoreRangeValues(range, start_offset, end_offset);
      assert(((Binop *)range)->rhs->type == Int ||
             ((Binop *)range)->rhs->type == Uint);
      if (((Binop *)range)->rhs->type == Int) {
        end_offset = IR::UAdd(end_offset, IR::Value(1ul));
      } else {
        end_offset = IR::IAdd(end_offset, IR::Value(1l));
      }

      start_vals[i] =
          IR::PtrIncr(Ptr(array_type->data_type), head_ptr, start_offset);
      end_vals[i] =
          IR::PtrIncr(Ptr(array_type->data_type), head_ptr, end_offset);

    } else if (iter->container->type == Type_) {
      auto container_type = Evaluate(iter->container).as_type;
      assert(container_type->is_enum());
      auto enum_type = (Enum *)container_type;

      start_vals[i] = IR::Value(0ul);
      end_vals[i]   = IR::Value(enum_type->int_values.size());
    } else {
      UNREACHABLE;
    }
  }

  auto end_init = IR::Block::Current;

  auto phi_block = IR::Func::Current->AddBlock("for-phi");
  IR::Block::Current->SetUnconditional(phi_block);
  IR::Block::Current = phi_block;

  // Fill in the phi values later.
  std::vector<IR::Value> phi_vals;
  for (size_t i = 0; i < num_iters; ++i) {
    auto iter = iterators[i];

    Type *phi_type = nullptr;
    if (iter->container->type->is_array() ||
        iter->container->type->is_slice()) {
      phi_type = Ptr(iter->type);
    } else if (iter->container->type->is_range()) {
      phi_type = iter->type;
    } else if (iter->container->type == Type_) {
      phi_type = Uint;
    } else {
      UNREACHABLE;
    }

    auto phi = IR::Phi(phi_type);
    phi_block->push(phi);
    phi_vals.push_back(IR::Value::Reg(phi.result.reg));
    iter->addr = IR::Value::Reg(phi.result.reg);
    if (iter->container->type->is_array() ||
        iter->container->type->is_slice()) {
      iter->addr = PtrCallFix(iter->type, iter->addr);
    }
  }

  auto land_block = IR::Func::Current->AddBlock("for-land");

  for (size_t i = 0; i < num_iters; ++i) {
    auto iter = iterators[i];
    IR::Value result;

    if (iter->container->type->is_array()) {
      result = IR::PtrEQ(phi_vals[i], end_vals[i]);

    } else if (iter->container->type->is_range()) {
      if (iter->type == Int) {
        result = IR::IGT(phi_vals[i], end_vals[i]);
      } else if (iter->type == Uint) {
        result = IR::UGT(phi_vals[i], end_vals[i]);
      } else if (iter->type == Char) {
        result = IR::CGT(phi_vals[i], end_vals[i]);
      } else {
        UNREACHABLE;
      }

    } else if (iter->container->type->is_slice()) {
      result = IR::PtrEQ(phi_vals[i], end_vals[i]);

    } else if (iter->container->type == Type_) {
      result = IR::UEQ(phi_vals[i], end_vals[i]);

    } else {
      UNREACHABLE;
    }

    auto cond_block = IR::Func::Current->AddBlock("for-cond");
    IR::Block::Current->SetConditional(result, land_block, cond_block);
    IR::Block::Current = cond_block;
  }

  // Rename the last block. Use it as a jumping off point to the entry.
  IR::Block::Current->block_name = "for-cond-true";

  assert(!for_scope->entry_block);
  for_scope->entry_block = IR::Func::Current->AddBlock("for-entry");

  assert(!for_scope->exit_block);
  for_scope->exit_block = IR::Func::Current->AddBlock("for-exit");

  IR::Block::Current->SetUnconditional(for_scope->entry_block);

  IR::Block::Current = for_scope->entry_block;
  IR::Store(Char, CONTINUE_FLAG, for_scope->GetFnScope()->exit_flag);
  statements->EmitIR();

  auto incr_block = IR::Func::Current->AddBlock("for-incr");

  IR::Block::Current->SetUnconditional(for_scope->exit_block);
  for_scope->InsertDestroy();
  IR::Block::Current->SetSwitch(
      IR::Load(Char, for_scope->GetFnScope()->exit_flag), incr_block);
  IR::Exit::Switch *for_exit_switch =
      (IR::Exit::Switch *)IR::Block::Current->exit;
  for_exit_switch->AddEntry(RESTART_FLAG, init_block);
  for_exit_switch->AddEntry(CONTINUE_FLAG, incr_block);
  for_exit_switch->AddEntry(REPEAT_FLAG, phi_block);
  for_exit_switch->AddEntry(BREAK_FLAG, land_block);
  assert(for_scope->parent->is_block_scope() ||
         for_scope->parent->is_function_scope());
  for_exit_switch->AddEntry(RETURN_FLAG,
                              ((BlockScope *)for_scope->parent)->exit_block);
  auto for_exit = IR::Block::Current;

  IR::Block::Current = incr_block;
  std::vector<IR::Value> incr_vals;
  for (size_t i = 0; i < num_iters; ++i) {
    auto iter = iterators[i];

    if (iter->container->type->is_array()) {
      auto array_type = (Array *)iter->container->type;
      incr_vals.push_back(
          IR::PtrIncr(Ptr(array_type->data_type), phi_vals[i], IR::Value(1ul)));

    } else if (iter->container->type->is_slice()) {
      auto array_type = (Array *)((Binop *)iter->container)->lhs->type;
      incr_vals.push_back(
          IR::PtrIncr(Ptr(array_type->data_type), phi_vals[i], IR::Value(1ul)));

    } else if (iter->container->type->is_range()) {
      if (iter->type == Int) {
        incr_vals.push_back(IR::IAdd(phi_vals[i], IR::Value(1l)));
      } else if (iter->type == Uint) {
        incr_vals.push_back(IR::UAdd(phi_vals[i], IR::Value(1ul)));
      } else if (iter->type == Char) {
        incr_vals.push_back(IR::CAdd(phi_vals[i], IR::Value('\01')));
      } else {
        UNREACHABLE;
      }
    } else if (iter->container->type == Type_) {
      incr_vals.push_back(IR::UAdd(phi_vals[i], IR::Value(1ul)));

    } else {
      UNREACHABLE;
    }
  }
  IR::Block::Current->SetUnconditional(phi_block);

  auto end_incr = IR::Block::Current;

  IR::Block::Current = phi_block;
  assert(start_vals.size() == num_iters);
  assert(incr_vals.size() == num_iters);
  for (size_t i = 0; i < num_iters; ++i) {
    assert(phi_block->cmds[i].op_code == IR::Op::Phi);
    phi_block->cmds[i].args.emplace_back(end_init);
    phi_block->cmds[i].args.emplace_back(start_vals[i]);

    phi_block->cmds[i].args.emplace_back(for_exit);
    phi_block->cmds[i].args.emplace_back(phi_vals[i]);

    phi_block->cmds[i].args.emplace_back(end_incr);
    phi_block->cmds[i].args.emplace_back(incr_vals[i]);
  }

  IR::Block::Current = land_block;

  return IR::Value::None();
}

IR::Value Jump::EmitIR() {
  IR::Value flag;
  switch (jump_type) {
  case JumpType::Restart: flag  = RESTART_FLAG; break;
  case JumpType::Continue: flag = CONTINUE_FLAG; break;
  case JumpType::Repeat: flag   = REPEAT_FLAG; break;
  case JumpType::Break: flag    = BREAK_FLAG; break;
  case JumpType::Return: flag   = RETURN_FLAG; break;
  }

  IR::Store(Char, flag, scope_->GetFnScope()->exit_flag);
  assert(scope_->is_block_scope() || scope_->is_function_scope());
  IR::Block::Current->SetUnconditional(((BlockScope *)scope_)->exit_block);

  // TODO set current block to be unreachable. access to it should trigger an
  // error that no code there will ever be executed.
  return IR::Value::None();
}

IR::Value Generic::EmitIR() {
  if (!value.as_type) { value = IR::Value(TypeVar(identifier, test_fn)); }
  return IR::Value(value.as_type);
}

IR::Value InDecl::EmitIR() { UNREACHABLE; }

IR::Value DummyTypeExpr::EmitIR() { return IR::Value(value.as_type); }
} // namespace AST
