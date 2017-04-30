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
  case Language::Operator::Add: {
    auto lhs_ir = lhs->EmitIR();
    auto rhs_ir = rhs->EmitIR();
    return IR::Add(lhs_ir, rhs_ir);
  }
  case Language::Operator::Sub: {
    auto lhs_ir = lhs->EmitIR();
    auto rhs_ir = rhs->EmitIR();
    return IR::Sub(lhs_ir, rhs_ir);
  }
  case Language::Operator::Mul: {
    auto lhs_ir = lhs->EmitIR();
    auto rhs_ir = rhs->EmitIR();
    return IR::Mul(lhs_ir, rhs_ir);
  }
  case Language::Operator::Div: {
    auto lhs_ir = lhs->EmitIR();
    auto rhs_ir = rhs->EmitIR();
    return IR::Div(lhs_ir, rhs_ir);
  }
  case Language::Operator::Mod: {
    auto lhs_ir = lhs->EmitIR();
    auto rhs_ir = rhs->EmitIR();
    return IR::Mod(lhs_ir, rhs_ir);
  }
  default: { UNREACHABLE; }
  }
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

