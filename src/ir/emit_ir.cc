#include "ir.h"

#include "../ast/ast.h"
#include "../scope.h"
#include "../type/type.h"

#define VERIFY_OR_EXIT_EARLY                                                   \
  do {                                                                         \
    verify_types(errors);                                                      \
    if (!errors->empty()) { return IR::Val::None(); }                          \
  } while (false)

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

IR::Val AST::Terminal::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;

  switch (terminal_type) {
  case Language::Terminal::Char:
  case Language::Terminal::Int:
  case Language::Terminal::Real:
  case Language::Terminal::Type:
  case Language::Terminal::Uint:
  case Language::Terminal::True:
  case Language::Terminal::False:
    return value;
  case Language::Terminal::ASCII:
    return AsciiFunc();
  case Language::Terminal::Ord:
    return OrdFunc();
  case Language::Terminal::Return:
    IR::Jump::Return();
    return IR::Val::None();
  default:
    NOT_YET;
  }
}

IR::Val AST::Identifier::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  ASSERT(decl, "");
  ASSERT(decl->addr != IR::Val::None(), "");
  return IR::Load(decl->addr);
}

IR::Val AST::Unop::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  switch (op) {
  case Language::Operator::Not:
  case Language::Operator::Sub:
    return IR::Neg(operand->EmitIR(errors));
  case Language::Operator::Return: {
    auto val = operand->EmitIR(errors);
    IR::SetReturn(0, val);

    ASSERT(scope_->is_exec(), "");
    // static_cast<BlockScope *>(scope_)->MakeReturn(operand->EmitIR(errors));
    IR::Jump::Unconditional(IR::BlockIndex{1});

    // TODO this is the right number but not implemented correctly.
    IR::Block::Current = IR::BlockIndex{1};
    IR::Jump::Return();
    return IR::Val::None();
  }

  default: { UNREACHABLE; }
  }
}

IR::Val AST::Binop::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  switch (op) {
#define CASE(op_name)                                                          \
  case Language::Operator::op_name: {                                          \
    auto lhs_ir = lhs->EmitIR(errors);                                         \
    auto rhs_ir = rhs->EmitIR(errors);                                         \
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
    ASSERT(!rhs->is_comma_list(), "");
    auto lhs_ir = lhs->EmitIR(errors);
    auto rhs_ir = rhs->EmitIR(errors);
    return IR::Cast(lhs_ir, rhs_ir);
  } break;
  case Language::Operator::Call: {
    auto lhs_ir = lhs->EmitIR(errors);
    std::vector<IR::Val> args;
    if (rhs->is_comma_list()) {
      NOT_YET;
    } else {
      args.push_back(rhs->EmitIR(errors));
    }
    return IR::Call(lhs_ir, std::move(args));
  } break;
  default: {
    std::cerr << *this << std::endl;
    UNREACHABLE;
  }
  }
}

IR::Val AST::ArrayType::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  return IR::Array(length->EmitIR(errors), data_type->EmitIR(errors));
}

IR::Val AST::ChainOp::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  if (ops[0] == Language::Operator::Xor) {
    return std::accumulate(exprs.begin(), exprs.end(), IR::Val::Bool(false),
                           [errors](IR::Val lhs, AST::Expression *expr) {
                             return IR::Xor(lhs, expr->EmitIR(errors));
                           });
  }
  NOT_YET;
}

IR::Val AST::FunctionLiteral::Emit(bool, std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;

  CURRENT_FUNC(ir_func = new IR::Func(type)) {
    IR::Block::Current = IR::BlockIndex{0};
    statements->EmitIR(errors);
  }

  return IR::Val::Func(ir_func);
}

IR::Val AST::Statements::EmitIR(std::vector<Error> *errors) {
  for (auto stmt : statements) { stmt->EmitIR(errors); }
  return IR::Val::None();
}

