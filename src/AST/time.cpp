#ifndef ICARUS_UNITY
#include "Type.h"
#endif

namespace AST {
Time::Eval Unop::determine_time() {
  time_ = operand->determine_time();
  if (op == Language::Operator::At || op == Language::Operator::And ||
      op == Language::Operator::Free || op == Language::Operator::Print) {
    time_ |= Time::run;
  }
  return time_;
}

Time::Eval Access::determine_time() {
  if (type->is_enum()) { return time_ = Time::either; }
  return time_ = operand->determine_time();
}

Time::Eval Binop::determine_time() {
  if (op == Language::Operator::Cast) {
    return time_ = lhs->determine_time();

  } else if (op == Language::Operator::Call) {
    return time_ = rhs->determine_time();
  }
  return time_ = lhs->determine_time() | rhs->determine_time();
}

Time::Eval ChainOp::determine_time() {
  time_ = Time::either;
  for (const auto &expr : exprs) { time_ |= expr->determine_time(); }
  return time_;
}

Time::Eval ArrayLiteral::determine_time() { return time_ = type->time(); }

Time::Eval ArrayType::determine_time() {
  length->determine_time();
  data_type->determine_time();
  return time_ = Time::compile;
}

Time::Eval Terminal::determine_time() {
  return time_ = (terminal_type == Language::Terminal::Type) ? Time::compile
                                                             : Time::either;
}

Time::Eval Identifier::determine_time() { return time_ = type->time(); }

Time::Eval InDecl::determine_time() {
  container->determine_time();
  return time_ = identifier->determine_time();
}

Time::Eval Declaration::determine_time() {
  expr->determine_time();
  return time_ = identifier->determine_time();
}

Time::Eval Case::determine_time() {
  time_ = Time::either;
  for (auto &kv : key_vals) {
    time_ |= kv.first->determine_time();
    time_ |= kv.second->determine_time();
  }
  return time_;
}

Time::Eval Statements::determine_time() {
  for (auto &stmt : statements) { time_ |= stmt->determine_time(); }
  return time_;
}

Time::Eval FunctionLiteral::determine_time() {
  time_ = Time::either;
  for (auto &in : inputs) { time_ |= in->determine_time(); }

  return (time_ |= statements->determine_time());
}

Time::Eval Conditional::determine_time() {
  time_ = Time::either;
  for (auto &cond : conditions) { time_ |= cond->determine_time(); }
  for (auto &stmt : statements) { time_ |= stmt->determine_time(); }

  return time_;
}

Time::Eval While::determine_time() {
  return time_ = condition->determine_time() | statements->determine_time();
}

Time::Eval For::determine_time() {
  time_ = statements->determine_time();
  for (auto iter : iterators) { time_ |= iter->determine_time(); }
  return time_;
}

Time::Eval StructLiteral::determine_time() {
  time_ = Time::either;
  for (auto &d : declarations) { time_ |= d->determine_time(); }
  return time_;
}

Time::Eval EnumLiteral::determine_time() { return time_ = Time::run; }

Time::Eval Jump::determine_time() { return time_ = Time::either; }
Time::Eval DummyTypeExpr::determine_time() { return time_ = Time::compile; }
} // namespace AST
