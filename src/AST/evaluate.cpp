#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type/Type.h"
#endif

#include "IR/IR.h"
#include "IR/Stack.h"

extern AST::FunctionLiteral *WrapExprIntoFunction(AST::Expression *expr);
extern std::stack<Scope *> ScopeStack;

namespace TypeSystem {
void initialize();
extern Type *get(const std::string &name);
} // namespace TypeSystem

IR::Value Evaluate(AST::Expression *expr) {
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
