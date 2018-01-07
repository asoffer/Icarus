#include "ast.h"
#include "../type/type.h"
#include "../error_log.h"

namespace AST {
bool Identifier::lrvalue_check() {
  ASSERT_NE(decl, nullptr);
  lvalue = decl->const_ ? Assign::Const : Assign::LVal;
  return true;
}

bool Unop::lrvalue_check() {
  bool result = operand->lrvalue_check();
  if (op == Language::Operator::And) {
    if (operand->type == Type_) {
      lvalue = Assign::RVal;
      return true;
    }

    switch (operand->lvalue) {
    case Assign::Const: // Intentionally falling through
    case Assign::RVal:
      ErrorLog::InvalidAddress(span, operand->lvalue);
      return false;
    case Assign::LVal: break;
    case Assign::Unset: UNREACHABLE();
    }
  }
  lvalue = (op == Language::Operator::At) ? Assign::LVal : Assign::RVal;
  return result;
}

bool Call::lrvalue_check() {
  bool result = true;
  for (auto &pos : pos_) { result &= pos->lrvalue_check(); }
  for (auto & [ key, val ] : named_) { result &= val->lrvalue_check(); }
  return result;
}

bool Access::lrvalue_check() {
  operand->lrvalue_check();
  if (operand->type == Type_ &&
      (member_name == "bytes" || member_name == "alignment")) {
    lvalue = Assign::Const;
  } else if (operand->type->is<Array>() &&
             operand->type->as<Array>().fixed_length && member_name == "size") {
    lvalue = Assign::Const;
  } else {
    lvalue = operand->lvalue;
  }
  return true;
}

bool Binop::lrvalue_check() {
  bool result = true;
  lhs->lrvalue_check();
  if (rhs) { result &= rhs->lrvalue_check(); }

  if (is_assignment() && lhs->lvalue != Assign::LVal) {
    ErrorLog::InvalidAssignment(span, lhs->lvalue);
    result = false;

  } else {
    lvalue = (op == Language::Operator::Cast || op == Language::Operator::Index)
                 ? lhs->lvalue
                 : Assign::RVal;
  }
  return result;
}

bool ChainOp::lrvalue_check() {
  bool result = true;
  lvalue = Assign::RVal;
  for (auto& expr : exprs) { result &= expr->lrvalue_check(); }
  return result;
}

bool CommaList::lrvalue_check() {
  bool result = true;
  lvalue = Assign::RVal;
  for (auto &expr : exprs) { result &= expr->lrvalue_check(); }
  return result;
}

bool FunctionLiteral::lrvalue_check() {
  lvalue = Assign::RVal;
  return return_type_expr->lrvalue_check();
}

bool InDecl::lrvalue_check() {
  lvalue = Assign::RVal;
  return identifier->lrvalue_check() & container->lrvalue_check();
}

bool Declaration::lrvalue_check() {
  bool result = true;
  lvalue      = Assign::RVal;
  result &= identifier->lrvalue_check();
  if (type_expr) { result &= type_expr->lrvalue_check(); }
  if (init_val) { result &= init_val->lrvalue_check(); }
  return result;
}

bool ArrayType::lrvalue_check() {
  bool result = true;
  lvalue = Assign::RVal;
  if (length) { result &= length->lrvalue_check(); }
  result &= data_type->lrvalue_check();
  return result;
}

bool ArrayLiteral::lrvalue_check() {
  bool result = true;
  lvalue      = Assign::RVal;
  for (auto &elem : elems) { result &= elem->lrvalue_check(); }
  return result;
}

bool Case::lrvalue_check() {
  bool result = true;
  lvalue      = Assign::RVal;
  for (auto & [ key, val ] : key_vals) {
    result &= key->lrvalue_check() & val->lrvalue_check();
  }
  return result;
}

bool Statements::lrvalue_check() {
  bool result =true;
  for (auto &stmt : statements) { result &= stmt->lrvalue_check(); }
  return result;
}

bool Jump::lrvalue_check() {return true;}
bool CodeBlock::lrvalue_check() {return true;}
bool Hole::lrvalue_check(){return true;}

bool For::lrvalue_check() {
  bool result = statements->lrvalue_check();
  for (auto &decl : iterators) { result &= decl->lrvalue_check(); }
  return result;
}

bool ScopeNode::lrvalue_check(){
  bool result = scope_expr->lrvalue_check();
  if (expr) { result &= expr->lrvalue_check(); }
  return result & stmts->lrvalue_check();
}

bool ScopeLiteral::lrvalue_check() {
  lvalue = Assign::RVal;
  return enter_fn->lrvalue_check() & exit_fn->lrvalue_check();
}
bool Terminal::lrvalue_check() {
  lvalue = Assign::RVal;
  return true;
}
} // namespace AST
