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
  switch (op) {
    case Language::Operator::And:
      switch (operand->lvalue) {
      case Assign::Const: [[fallthrough]];
      case Assign::RVal:
        ErrorLog::InvalidAddress(span, operand->lvalue);
        lvalue = Assign::RVal;
        return false;
      case Assign::LVal: break;
      case Assign::Unset: UNREACHABLE();
      }
      return result;
    case Language::Operator::At: lvalue = Assign::LVal; return true;
    default:
      lvalue = operand->lvalue == Assign::Const ? Assign::Const : Assign::LVal;
      return true;
  }
}

bool Call::lrvalue_check() {
  bool result = true;
  bool all_const = true;
  for (auto &pos : pos_) {
    result &= pos->lrvalue_check();
    all_const &= pos->lvalue == Assign::Const;
  }
  for (auto & [ key, val ] : named_) {
    result &= val->lrvalue_check();
    all_const &= val->lvalue == Assign::Const;
  }
  lvalue = all_const ? Assign::Const : Assign::RVal;
  return result;
}

bool Access::lrvalue_check() {
  bool result = operand->lrvalue_check();
  lvalue = (operand->type->is<Array>() &&
            operand->type->as<Array>().fixed_length && member_name == "size")
               ? Assign::Const
               : operand->lvalue;
  return result;
}

bool Binop::lrvalue_check() {
  bool result = lhs->lrvalue_check() & rhs->lrvalue_check();

  if (is_assignment() && lhs->lvalue != Assign::LVal) {
    ErrorLog::InvalidAssignment(span, lhs->lvalue);
    result = false;

  } else if (op == Language::Operator::Cast ||
             op == Language::Operator::Index) {
    lvalue = rhs->lvalue;
  } else if (lhs->lvalue == Assign::Const && rhs->lvalue == Assign::Const) {
    lvalue = Assign::Const;
  } else {
    lvalue = Assign::RVal;
  }
  return result;
}

bool ChainOp::lrvalue_check() {
  bool result = true;
  lvalue      = Assign::Const;
  for (auto &expr : exprs) {
    result &= expr->lrvalue_check();
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  return result;
}

bool CommaList::lrvalue_check() {
  bool result = true;
  lvalue      = Assign::Const;
  for (auto &expr : exprs) {
    result &= expr->lrvalue_check();
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  return result;
}

bool FunctionLiteral::lrvalue_check() {
  lvalue = Assign::Const;
  return return_type_expr->lrvalue_check();
}

bool InDecl::lrvalue_check() {
  lvalue = Assign::RVal;
  return identifier->lrvalue_check() & container->lrvalue_check();
}

bool Declaration::lrvalue_check() {
  bool result = true;
  lvalue      = const_ ? Assign::Const : Assign::RVal;
  result &= identifier->lrvalue_check();
  if (type_expr) { result &= type_expr->lrvalue_check(); }
  if (init_val) {
    result &= init_val->lrvalue_check();
    if (init_val->lvalue != Assign::Const && const_) {
      ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + "\n");
      result = false;
    }
  }
  return result;
}

bool ArrayType::lrvalue_check() {
  bool result = true;
  lvalue = Assign::Const;
  if (length) {
    result &= length->lrvalue_check();
    if (length->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  result &= data_type->lrvalue_check();
  if (data_type->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  return result;
}

bool ArrayLiteral::lrvalue_check() {
  bool result = true;
  lvalue      = Assign::Const;
  for (auto &elem : elems) { 
    result &= elem->lrvalue_check();
    if (elem->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  return result;
}

bool Case::lrvalue_check() {
  bool result = true;
  lvalue      = Assign::Const;
  for (auto & [ key, val ] : key_vals) {
   result &= key->lrvalue_check() & val->lrvalue_check();
   // TODO do you want case-statements to be usable as lvalues? Currently they
   // are not.
   if (key->lvalue != Assign::Const || val->lvalue != Assign::Const) {
     lvalue = Assign::RVal;
   }
  }
  return result;
}

bool Statements::lrvalue_check() {
  bool result =true;
  for (auto &stmt : statements) { result &= stmt->lrvalue_check(); }
  return result;
}

bool Jump::lrvalue_check() { return true; }
bool CodeBlock::lrvalue_check() { return true; }
bool Hole::lrvalue_check() { return true; }

bool For::lrvalue_check() {
  bool result = statements->lrvalue_check();
  for (auto &decl : iterators) { result &= decl->lrvalue_check(); }
  return result;
}

bool ScopeNode::lrvalue_check(){
  bool result = scope_expr->lrvalue_check();
  if (expr) { result &= expr->lrvalue_check(); }
  lvalue = Assign::RVal;
  // TODO Support better constant-ness.
  return result & stmts->lrvalue_check();
}

bool ScopeLiteral::lrvalue_check() {
  lvalue = Assign::Const;
  return enter_fn->lrvalue_check() & exit_fn->lrvalue_check();
}
bool Terminal::lrvalue_check() {
  lvalue = Assign::Const;
  return true;
}
} // namespace AST
