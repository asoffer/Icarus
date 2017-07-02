#include "ast.h"
#include "../type/type.h"
#include "../error_log.h"

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
    case Assign::RVal: ErrorLog::InvalidAddress(loc, operand->lvalue); break;
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
  } else if (operand->type->is<Array>() &&
             ((Array *)operand->type)->fixed_length && member_name == "size") {
    lvalue = Assign::Const;
  } else {
    lvalue = operand->lvalue;
  }
}

void Binop::lrvalue_check() {
  lhs->lrvalue_check();
  if (rhs) { rhs->lrvalue_check(); }

  if (is_assignment() && lhs->lvalue != Assign::LVal) {
    ErrorLog::InvalidAssignment(loc, lhs->lvalue);

  } else {
    lvalue = (op == Language::Operator::Cast || op == Language::Operator::Index)
                 ? lhs->lvalue
                 : Assign::RVal;
  }
}

void ChainOp::lrvalue_check() {
  lvalue = Assign::RVal;
  for (auto& expr : exprs) { expr->lrvalue_check(); }
}

void FunctionLiteral::lrvalue_check() {
  lvalue = Assign::RVal;
  return_type_expr->lrvalue_check();
  statements->lrvalue_check();
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
  for (auto& elem : elems) {
    elem->lrvalue_check();
  }
}

void Case::lrvalue_check() {
  lvalue = Assign::RVal;
  for (auto& kv : key_vals) {
    kv.first->lrvalue_check();
    kv.second->lrvalue_check();
  }
}

void Statements::lrvalue_check() {
  for (auto& stmt : statements) {
    stmt->lrvalue_check();
  }
}

void Jump::lrvalue_check() {}
void CodeBlock::lrvalue_check() {}

void For::lrvalue_check() {
  statements->lrvalue_check();
  for (auto& decl : iterators) { decl->lrvalue_check(); }
}

void ScopeNode::lrvalue_check(){
  scope_expr->lrvalue_check();
  if (expr) { expr->lrvalue_check(); }
  stmts->lrvalue_check();
}

void ScopeLiteral::lrvalue_check() {
  lvalue = Assign::RVal;
  enter_fn->lrvalue_check();
  exit_fn->lrvalue_check();
}
void Terminal::lrvalue_check() { lvalue = Assign::RVal; }
} // namespace AST
