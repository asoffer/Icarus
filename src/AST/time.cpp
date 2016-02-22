#include "AST.h"

namespace AST {
Time::Eval Unop::determine_time() {
  time_ = operand->determine_time();
  if (op == Language::Operator::At || op == Language::Operator::And ||
      op == Language::Operator::Free || op == Language::Operator::Print) {
    time_ |= Time::run;
  }
  return time_;
}

Time::Eval Access::determine_time() { return time_ = operand->determine_time(); }

Time::Eval Binop::determine_time() {
  if (op == Language::Operator::Cast) {
    return time_ = lhs->determine_time();
  }
  return time_ = lhs->determine_time() | rhs->determine_time();
}

Time::Eval ChainOp::determine_time() {
  time_ = Time::either;
  for (const auto &expr : exprs) {
    time_ |= expr->determine_time();
  }
  return time_;
}

Time::Eval ArrayLiteral::determine_time() { return time_ = type->time(); }

Time::Eval ArrayType::determine_time() { return time_ = Time::compile; }

Time::Eval Terminal::determine_time() {
  return time_ = (terminal_type == Language::Terminal::Type) ? Time::compile
                                                             : Time::run;
}

Time::Eval Identifier::determine_time() { return time_ = type->time(); }

Time::Eval Declaration::determine_time() {
  type_expr->determine_time();
  return time_ = identifier->determine_time();
}

Time::Eval KVPairList::determine_time() {
  time_ = Time::either;
  for (auto &kv_pair : kv_pairs_) {
    time_ |= kv_pair.first->determine_time();
    time_ |= kv_pair.second->determine_time();
  }
  return time_;
}

Time::Eval Case::determine_time() { return time_ = pairs_->determine_time(); }

Time::Eval Statements::determine_time() {
  for (auto &stmt : statements_) {
    time_ |= stmt->determine_time();
  }
  return time_;
}

Time::Eval FunctionLiteral::determine_time() {
  time_ = Time::either;
  for (auto &in : inputs_) {
    time_ |= in->determine_time();
  }

  return (time_ |= statements_->determine_time());
}

Time::Eval Conditional::determine_time() {
  time_ = Time::either;
  for (auto &cond : conds_) {
    time_ |= cond->determine_time();
  }
  for (auto &stmt : statements_) {
    time_ |= stmt->determine_time();
  }

  return time_;
}

Time::Eval While::determine_time() {
  return time_ = cond_->determine_time() | statements_->determine_time();
}

Time::Eval TypeLiteral::determine_time() {
  time_ = Time::either;
  for (auto &d : decls_) {
    time_ |= d->determine_time();
  }
  return time_;
}

Time::Eval EnumLiteral::determine_time() { return time_ = Time::run; }

Time::Eval Break::determine_time() { return time_ = Time::either; }
} // namespace AST
