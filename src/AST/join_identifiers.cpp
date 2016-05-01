#include "Scope.h"

void set_or_recurse(AST::Expression *&eptr) {
  // TODO What happens to the line number?
  if (eptr->is_identifier()) {
    eptr = CurrentScope()->identifier(eptr);
  } else {
    eptr->join_identifiers();
  }
}

namespace AST {
void DummyTypeExpr::join_identifiers(bool) {}

void Unop::join_identifiers(bool) { set_or_recurse(operand); }

void While::join_identifiers(bool) {
  Scope::Stack.push(while_scope);
  condition->join_identifiers();
  statements->join_identifiers();
  Scope::Stack.pop();
}

void For::join_identifiers(bool) {
  Scope::Stack.push(for_scope);
  for (auto &iter : iterators) {
    iter->identifier = CurrentScope()->identifier(iter->identifier);
    auto expr = static_cast<Expression *>(iter);
    set_or_recurse(expr);
  }

  statements->join_identifiers();
  Scope::Stack.pop();
}

void ArrayLiteral::join_identifiers(bool) {
  for (auto &el : elems) { set_or_recurse(el); }
}

void Terminal::join_identifiers(bool) {}
void Jump::join_identifiers(bool) {}

void Identifier::join_identifiers(bool) { Terminal::join_identifiers(); }

void Conditional::join_identifiers(bool) {
  for (size_t i = 0; i < conditions.size(); ++i) {
    Scope::Stack.push(body_scopes[i]);
    conditions[i]->join_identifiers();
    Scope::Stack.pop();
  }
  for (size_t i = 0; i < statements.size(); ++i) {
    Scope::Stack.push(body_scopes[i]);
    statements[i]->join_identifiers();
    Scope::Stack.pop();
  }
}

void Access::join_identifiers(bool) { set_or_recurse(operand); }

void Binop::join_identifiers(bool) {
  set_or_recurse(lhs);
  set_or_recurse(rhs);
}

void ArrayType::join_identifiers(bool) {
  if (length != nullptr) { set_or_recurse(length); }
  set_or_recurse(data_type);
}

void Declaration::join_identifiers(bool is_arg) {
  identifier = CurrentScope()->identifier(identifier);
  if (is_arg) {
    assert(identifier);
    identifier->is_arg = true;
  }

  set_or_recurse(type_expr);
}

void ChainOp::join_identifiers(bool is_arg) {
  for (auto &expr : exprs) { set_or_recurse(expr); }
}

void Case::join_identifiers(bool is_arg) { kv->join_identifiers(); }

void KVPairList::join_identifiers(bool is_arg) {
  for (auto &pair : pairs) {
    set_or_recurse(pair.first);
    set_or_recurse(pair.second);
  }
}

void Statements::join_identifiers(bool is_arg) {
  for (auto &ptr : statements) {
    if (ptr->is_identifier()) {
      ptr = CurrentScope()->identifier(static_cast<Expression *>(ptr));
    } else {
      ptr->join_identifiers();
    }
  }
}

void FunctionLiteral::join_identifiers(bool is_arg) {
  Scope::Stack.push(fn_scope);
  for (auto &in : inputs) { in->join_identifiers(true); }

  set_or_recurse(return_type_expr);
  statements->join_identifiers();

  Scope::Stack.pop();
}

void StructLiteral::join_identifiers(bool) {
  Scope::Stack.push(type_scope);

  for (auto &param : params) { param->join_identifiers(true); }

  for (auto &decl : declarations) { decl->join_identifiers(); }

  Scope::Stack.pop();
}

void EnumLiteral::join_identifiers(bool) {}
} // namespace AST
