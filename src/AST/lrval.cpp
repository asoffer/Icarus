#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

namespace AST {
void Identifier::lrvalue_check() {
  lvalue = decl->HasHashtag("const") ? Assign::Const : Assign::LVal;
}

void Unop::lrvalue_check() {
  operand->lrvalue_check();
  if (op == Language::Operator::And) {
    if (operand->type == Type_) {
      lvalue = Assign::RVal;
      return;
    }

    switch (operand->lvalue) {
    case Assign::Const: // Intentionally falling through
    case Assign::RVal: Error::Log::InvalidAddress(loc, operand->lvalue); break;
    case Assign::LVal: break;
    case Assign::Unset: UNREACHABLE;
    }
  }
  lvalue = (op == Language::Operator::At) ? Assign::LVal : Assign::RVal;
}

void Access::lrvalue_check() {
  operand->lrvalue_check();
  if (operand->type == Type_ &&
      (member_name == "bytes" || member_name == "alignment")) {
    lvalue = Assign::Const;
  } else if (operand->type->is_array() &&
             ((Array *)operand->type)->fixed_length &&
             member_name == "size") {
    lvalue = Assign::Const;
  } else {
    lvalue = operand->lvalue;
  }
}

void Binop::lrvalue_check() {
  lhs->lrvalue_check();
  if (rhs) { rhs->lrvalue_check(); }

  if (is_assignment() && lhs->lvalue != Assign::LVal) {
    Error::Log::InvalidAssignment(loc, lhs->lvalue);

  } else {
    lvalue = (op == Language::Operator::Cast || op == Language::Operator::Index)
                 ? lhs->lvalue
                 : Assign::RVal;
  }
}

void ChainOp::lrvalue_check() {
  lvalue = Assign::RVal;
  for (auto e : exprs) { e->lrvalue_check(); }
}

void FunctionLiteral::lrvalue_check() {
  lvalue = Assign::RVal;
  return_type_expr->lrvalue_check();
  bool input_has_vars = false;
  for (auto in : inputs) {
    in->lrvalue_check();
    input_has_vars |= in->type->has_vars;
  }

  if (!input_has_vars) { statements->lrvalue_check(); }
}

void Generic::lrvalue_check() {
  lvalue = Assign::RVal;
  identifier->lrvalue_check();
  test_fn->lrvalue_check();
}

void InDecl::lrvalue_check() {
  lvalue = Assign::RVal;
  identifier->lrvalue_check();
  container->lrvalue_check();
}

void Declaration::lrvalue_check() {
  lvalue = Assign::RVal;
  identifier->lrvalue_check();
  if (type_expr) { type_expr->lrvalue_check(); }
  if (init_val) { init_val->lrvalue_check(); }
}

void ArrayType::lrvalue_check() {
  lvalue = Assign::RVal;
  if (length) { length->lrvalue_check(); }
  data_type->lrvalue_check();
}

void ArrayLiteral::lrvalue_check() {
  lvalue = Assign::RVal;
  for (auto e : elems) {
    e->lrvalue_check();
  }
}

void Case::lrvalue_check() {
  lvalue = Assign::RVal;
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

void Terminal::lrvalue_check() { lvalue = Assign::RVal; }
void DummyTypeExpr::lrvalue_check() { lvalue = Assign::RVal; }

} // namespace AST
