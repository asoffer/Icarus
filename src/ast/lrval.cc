#include "ast.h"

#include "../error_log.h"
#include "../type/type.h"

static constexpr int ThisStage() { return 2; }
namespace AST {
void Identifier::lrvalue_check() {
  STAGE_CHECK;
  ASSERT_NE(decl, nullptr);
  lvalue = decl->const_ ? Assign::Const : Assign::LVal;
}

void Unop::lrvalue_check() {
  STAGE_CHECK;
  operand->lrvalue_check();
  limit_to(operand);
  switch (op) {
  case Language::Operator::And:
    switch (operand->lvalue) {
    case Assign::Const: [[fallthrough]];
    case Assign::RVal:
      ErrorLog::InvalidAddress(span, operand->lvalue);
      lvalue = Assign::RVal;
      break;
    case Assign::LVal: break;
    case Assign::Unset: UNREACHABLE();
    }
    break;
  case Language::Operator::At: lvalue = Assign::LVal; break;
  default:
    lvalue = operand->lvalue == Assign::Const ? Assign::Const : Assign::LVal;
    break;
  }
}

void Call::lrvalue_check() {
  STAGE_CHECK;
  bool all_const = true;
  for (auto &pos : pos_) {
    pos->lrvalue_check();
    limit_to(pos);
    all_const &= pos->lvalue == Assign::Const;
  }
  for (auto & [ key, val ] : named_) {
    val->lrvalue_check();
    limit_to(val);
    all_const &= val->lvalue == Assign::Const;
  }
  lvalue = all_const ? Assign::Const : Assign::RVal;
}

void Access::lrvalue_check() {
  STAGE_CHECK;
  operand->lrvalue_check();
  limit_to(operand);
  lvalue = (operand->type->is<Array>() &&
            operand->type->as<Array>().fixed_length && member_name == "size")
               ? Assign::Const
               : operand->lvalue;
}

void Binop::lrvalue_check() {
  STAGE_CHECK;
  lhs->lrvalue_check();
  rhs->lrvalue_check();
  limit_to(lhs);
  limit_to(lhs);

  if (is_assignment() && lhs->lvalue != Assign::LVal) {
    ErrorLog::InvalidAssignment(span, lhs->lvalue);
    limit_to(StageRange::Nothing());

  } else if (op == Language::Operator::Cast ||
             op == Language::Operator::Index) {
    lvalue = rhs->lvalue;
  } else if (lhs->lvalue == Assign::Const && rhs->lvalue == Assign::Const) {
    lvalue = Assign::Const;
  } else {
    lvalue = Assign::RVal;
  }
}

void ChainOp::lrvalue_check() {
  STAGE_CHECK;
  bool result = true;
  lvalue      = Assign::Const;
  for (auto &expr : exprs) {
    expr->lrvalue_check();
    limit_to(expr);
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
}

void CommaList::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::Const;
  for (auto &expr : exprs) {
    expr->lrvalue_check();
    limit_to(expr);
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
}

void FunctionLiteral::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::Const;
  return_type_expr->lrvalue_check();
  limit_to(return_type_expr);
}

void InDecl::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::RVal;
  identifier->lrvalue_check();
  container->lrvalue_check();
  limit_to(identifier);
  limit_to(container);
}

void Declaration::lrvalue_check() {
  STAGE_CHECK;
  lvalue = const_ ? Assign::Const : Assign::RVal;
  identifier->lrvalue_check();
  limit_to(identifier);
  if (type_expr) {
    type_expr->lrvalue_check();
    limit_to(type_expr);
  }
  if (init_val) {
    init_val->lrvalue_check();
    limit_to(init_val);
    if (init_val->lvalue != Assign::Const && const_) {
      ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + "\n");
      limit_to(StageRange::Nothing());
    }
  }
}

void ArrayType::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::Const;
  if (length != nullptr) {
    length->lrvalue_check();
    limit_to(length);
    if (length->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  data_type->lrvalue_check();
  limit_to(data_type);
  if (data_type->lvalue != Assign::Const) { lvalue = Assign::RVal; }
}

void ArrayLiteral::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::Const;
  for (auto &elem : elems) {
    elem->lrvalue_check();
    limit_to(elem);
    if (elem->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
}

void Case::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::Const;
  for (auto & [ key, val ] : key_vals) {
    key->lrvalue_check();
    val->lrvalue_check();
    limit_to(key);
    limit_to(val);

    // TODO do you want case-statements to be usable as lvalues? Currently they
    // are not.
    if (key->lvalue != Assign::Const || val->lvalue != Assign::Const) {
      lvalue = Assign::RVal;
    }
  }
}

void Statements::lrvalue_check() {
  STAGE_CHECK;
  for (auto &stmt : statements) {
    stmt->lrvalue_check();
    limit_to(stmt);
  }
}

void Jump::lrvalue_check() { STAGE_CHECK; }
void CodeBlock::lrvalue_check() { STAGE_CHECK; }
void Hole::lrvalue_check() { STAGE_CHECK; }

void For::lrvalue_check() {
  STAGE_CHECK;
  statements->lrvalue_check();
  limit_to(statements);
  for (auto &decl : iterators) {
    decl->lrvalue_check();
    limit_to(decl);
  }
}

void ScopeNode::lrvalue_check() {
  STAGE_CHECK;
  scope_expr->lrvalue_check();
  if (expr != nullptr) {
    expr->lrvalue_check();
    limit_to(expr);
  }
  lvalue = Assign::RVal;
  // TODO Support better constant-ness.
  stmts->lrvalue_check();
  limit_to(stmts);
}

void ScopeLiteral::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::Const;
  enter_fn->lrvalue_check();
  exit_fn->lrvalue_check();
  limit_to(enter_fn);
  limit_to(exit_fn);
}
void Terminal::lrvalue_check() {
  STAGE_CHECK;
  lvalue = Assign::Const;
}
} // namespace AST
