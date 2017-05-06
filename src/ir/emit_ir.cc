#include "ir.h"

#include "../ast/ast.h"
#include "../scope.h"
#include "../type/type.h"

static IR::Val AsciiFunc() {
  static IR::Func *ascii_func_ = []() {
    auto fn = new IR::Func(Func(Uint, Char));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(0, IR::Trunc(IR::Val::Arg(::Uint, 0)));
      IR::Jump::Unconditional(fn->exit());

      IR::Block::Current = fn->exit();
      IR::Jump::Return();
    }
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

static IR::Val OrdFunc() {
  static IR::Func *ord_func_ = []() {
    auto fn = new IR::Func(Func(Char, Uint));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(0, IR::Extend(IR::Val::Arg(::Uint, 0)));
      IR::Jump::Unconditional(fn->exit());

      IR::Block::Current = fn->exit();
      IR::Jump::Return();
    }
    return fn;
  }();
  return IR::Val::Func(ord_func_);
}

IR::Val AST::Terminal::EmitIR() {
  verify_types();

  switch (terminal_type) {
  case Language::Terminal::Char:
  case Language::Terminal::Int:
  case Language::Terminal::Real:
  case Language::Terminal::Type:
  case Language::Terminal::Uint:
  case Language::Terminal::True:
  case Language::Terminal::False: return value;
  case Language::Terminal::ASCII: return AsciiFunc();
  case Language::Terminal::Ord: return OrdFunc();
  case Language::Terminal::Return:
    IR::Jump::Return();
    return IR::Val::None();
  default: NOT_YET;
  }
}

IR::Val AST::Identifier::EmitIR() {
  verify_types();
  NOT_YET;
}

IR::Val AST::Unop::EmitIR() {
  verify_types();
  switch (op) {
  case Language::Operator::Not:
  case Language::Operator::Sub: return IR::Neg(operand->EmitIR());
  case Language::Operator::Return: {
    auto val = operand->EmitIR();
    IR::SetReturn(0, val);

    ASSERT(scope_->is_exec(), "");
    // static_cast<BlockScope *>(scope_)->MakeReturn(operand->EmitIR());
    IR::Jump::Unconditional(IR::BlockIndex{1});

    // TODO this is the right number but not implemented correctly.
    IR::Block::Current = IR::BlockIndex{1};
    IR::Jump::Return();
    return IR::Val::None();
  }

  default: { UNREACHABLE; }
  }
}

IR::Val AST::Binop::EmitIR() {
  verify_types();
  switch (op) {
#define CASE(op_name)                                                          \
  case Language::Operator::op_name: {                                          \
    auto lhs_ir = lhs->EmitIR();                                               \
    auto rhs_ir = rhs->EmitIR();                                               \
    return IR::op_name(lhs_ir, rhs_ir);                                        \
  } break
    CASE(Add);
    CASE(Sub);
    CASE(Mul);
    CASE(Div);
    CASE(Mod);
    CASE(Arrow);
#undef CASE
  case Language::Operator::Cast: {
    ASSERT (!rhs->is_comma_list(), "");
    auto lhs_ir = lhs->EmitIR();
    auto rhs_ir = rhs->EmitIR();
    return IR::Cast(lhs_ir, rhs_ir);
  } break;
  case Language::Operator::Call: {
    auto lhs_ir = lhs->EmitIR();
    std::vector<IR::Val> args;
    if (rhs->is_comma_list()) {
      NOT_YET;
    } else {
      args.push_back(rhs->EmitIR());
    }
    return IR::Call(lhs_ir, std::move(args));
  } break;
  default: {
    std::cerr << *this << std::endl;
    UNREACHABLE;
  }
  }
}

IR::Val AST::ArrayType::EmitIR(){
  verify_types();
  return IR::Array(length->EmitIR(), data_type->EmitIR());
}

IR::Val AST::ChainOp::EmitIR(){
  verify_types();
  if (ops[0] == Language::Operator::Xor) {
    return std::accumulate(exprs.begin(), exprs.end(), IR::Val::Bool(false),
                           [](IR::Val lhs, AST::Expression *expr) {
                             return IR::Xor(lhs, expr->EmitIR());
                           });
  }
  NOT_YET;
}

IR::Val AST::FunctionLiteral::Emit(bool) {
  verify_types();

  CURRENT_FUNC(ir_func = new IR::Func(type)) {
    IR::Block::Current = IR::BlockIndex{0};
    statements->EmitIR();
  }

  return IR::Val::Func(ir_func);
}

IR::Val AST::Statements::EmitIR() {
  for (auto stmt : statements) { stmt->EmitIR(); }
  return IR::Val::None();
}

