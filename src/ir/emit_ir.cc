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
    ASSERT(scope_->is_block_scope(), "");
    static_cast<BlockScope *>(scope_)->MakeReturn(operand->EmitIR());
    return IR::Val::None();
  }

  default: { UNREACHABLE; }
  }
}

IR::Val AST::FunctionLiteral::Emit(bool) {
  verify_types();

  ir_func = new IR::Func(type);

  CURRENT_FUNC(ir_func) {
    IR::Block::Current = ir_func->entry();
    statements->EmitIR();
  }

  return IR::Val::Func(ir_func);
}

IR::Val AST::Statements::EmitIR() {
  for (auto stmt : statements) { stmt->EmitIR(); }
  return IR::Val::None();
}

