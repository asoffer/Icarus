#ifndef ICARUS_UNITY
#include "Type.h"
#endif

namespace AST {
void Identifier::lrvalue_check() { lvalue = true; }

void Unop::lrvalue_check() {
  operand->lrvalue_check();
  if (op == Language::Operator::And) {
    if (!operand->lvalue && operand->type != Type_) {
      error_log.log(loc, "Cannot take address");
    }
  }
  lvalue = (op == Language::Operator::At || op == Language::Operator::And) &&
           operand->lvalue;
}

void Access::lrvalue_check() {
  operand->lrvalue_check();
  if (operand->type == Type_ &&
      (member_name == "bytes" || member_name == "alignment")) {
    lvalue = false;
  } else if (operand->type->is_array() &&
             static_cast<Array *>(operand->type)->fixed_length &&
             member_name == "size") {
    lvalue = false;
  } else {
    lvalue = operand->lvalue;
  }
}

void Binop::lrvalue_check() {
  lhs->lrvalue_check();
  if (rhs) { rhs->lrvalue_check(); }

  if (is_assignment() && !lhs->lvalue) {
    // TODO better error message.
    error_log.log(loc, "Invalid assignment (to rvalue)");

  } else {
    lvalue =
        (op == Language::Operator::Cast || op == Language::Operator::Index) &&
        lhs->lvalue;
  }
}

void ChainOp::lrvalue_check() {
  lvalue = false;
  for (auto e : exprs) { e->lrvalue_check(); }
}

void FunctionLiteral::lrvalue_check() {
  lvalue = false;
  return_type_expr->lrvalue_check();
  statements->lrvalue_check();
  for (auto decl : inputs) { decl->lrvalue_check(); }
}

void InDecl::lrvalue_check() {
  lvalue = false;
  identifier->lrvalue_check();
  container->lrvalue_check();
}

void Declaration::lrvalue_check() {
  lvalue = (decl_type == DeclType::Std);
  identifier->lrvalue_check();
  expr->lrvalue_check();
}

void ArrayType::lrvalue_check() { lvalue = false;
  if (length) { length->lrvalue_check(); }
  data_type->lrvalue_check();
}

void ArrayLiteral::lrvalue_check() {
  lvalue = false;
  for (auto e : elems) {
    e->lrvalue_check();
  }
}

void Case::lrvalue_check() {
  lvalue = false;
  for (auto kv : key_vals) {
    kv.first->lrvalue_check();
    kv.second->lrvalue_check();
  }
}

void Statements::lrvalue_check() {
  for (auto stmt : statements) {
    stmt->lrvalue_check();
  }
}

void Jump::lrvalue_check() {}

void Conditional::lrvalue_check() {
  for (auto cond : conditions) { cond->lrvalue_check(); }
  for (auto stmt : statements) { stmt->lrvalue_check(); }
}


void For::lrvalue_check() {
  statements->lrvalue_check();
  for (auto decl : iterators) { decl->lrvalue_check(); }
}

void While::lrvalue_check(){
  condition->lrvalue_check();
  statements->lrvalue_check();
}

void ParametricStructLiteral::lrvalue_check() {
  lvalue = false;
  for (auto decl : params) { decl->lrvalue_check(); }
  for (auto decl : declarations) { decl->lrvalue_check(); }
}

void StructLiteral::lrvalue_check() {
  lvalue = false;
  for (auto decl : declarations) { decl->lrvalue_check(); }
}

void Terminal::lrvalue_check() { lvalue = false; }
void EnumLiteral::lrvalue_check() { lvalue = false; }
void DummyTypeExpr::lrvalue_check() { lvalue = false; }

} // namespace AST
