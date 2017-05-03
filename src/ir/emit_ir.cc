#include "ir.h"
#include "../ast/ast.h"
#include "../scope.h"

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
  default: NOT_YET;
  }
}

IR::Val AST::Unop::EmitIR() {
  verify_types();
  switch (op) {
  case Language::Operator::Sub: return IR::Neg(operand->EmitIR());
  case Language::Operator::Return: {
    auto val = operand->EmitIR();
    IR::SetReturn(0, val);

    ASSERT(scope_->is_block_scope(), "");
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
#undef CASE
  default: { UNREACHABLE; }
  }
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

