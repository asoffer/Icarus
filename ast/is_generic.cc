#include "ast/ast.h"

namespace ast {

bool Access::IsGeneric() const { return operand()->IsGeneric(); }

bool ArgumentType::IsGeneric() const { return true; }

bool ArrayLiteral::IsGeneric() const {
  for (auto const *elem : elems()) {
    if (elem->IsGeneric()) { return true; }
  }
  return false;
}

bool ArrayType::IsGeneric() const {
  if (data_type()->IsGeneric()) { return true; }
  for (auto const *length : lengths()) {
    if (length->IsGeneric()) { return true; }
  }
  return false;
}

bool Binop::IsGeneric() const {
  return lhs()->IsGeneric() or rhs()->IsGeneric();
}

bool BlockLiteral::IsGeneric() const {
  for (auto const *b : before()) {
    if (b->IsGeneric()) { return true; }
  }
  for (auto const *a : after()) {
    if (a->IsGeneric()) { return true; }
  }
  return false;
}

bool BlockNode::IsGeneric() const {
  // TODO
  return false;
}

bool Jump::IsGeneric() const {
  // TODO
  return false;
}

bool BuiltinFn::IsGeneric() const { return false; }

bool Call::IsGeneric() const {
  if (callee()->IsGeneric()) { return true; }
  for (auto const *arg : args().pos()) {
    if (arg->IsGeneric()) { return true; }
  }
  for (auto [name, arg] : args().named()) {
    if (arg->IsGeneric()) { return true; }
  }
  return false;
}

bool Cast::IsGeneric() const {
  return expr()->IsGeneric() or type()->IsGeneric();
}

bool ChainOp::IsGeneric() const {
  for (auto const *expr : exprs()) {
    if (expr->IsGeneric()) { return true; }
  }
  return false;
}

bool CommaList::IsGeneric() const {
  // TODO
  return false;
}

bool Declaration::IsGeneric() const {
  if (auto *t = type_expr(); t and t->IsGeneric()) { return true; }
  if (auto *i = init_val(); i and i->IsGeneric()) { return true; }
  return false;
}

bool DesignatedInitializer::IsGeneric() const {
  // TODO
  return type()->IsGeneric();
}

bool EnumLiteral::IsGeneric() const {
  // TODO
  return false;
}

bool FunctionLiteral::IsGeneric() const {
  // TODO
  return false;
}

bool Identifier::IsGeneric() const {
  // TODO
  return false;
}

bool Import::IsGeneric() const {
  return operand()->IsGeneric();
}

bool Index::IsGeneric() const {
  return lhs()->IsGeneric() or rhs()->IsGeneric();
}

bool Goto::IsGeneric() const {
  // TODO
  return false;
}

bool Label::IsGeneric() const { return false; }

bool ReturnStmt::IsGeneric() const {
  // TODO
  return false;
}

bool YieldStmt::IsGeneric() const {
  // TODO
  return false;
}

bool ScopeLiteral::IsGeneric() const {
  // TODO
  return false;
}

bool ScopeNode::IsGeneric() const {
  // TODO
  return false;
}

bool StructLiteral::IsGeneric() const {
  // TODO
  return false;
}

bool ParameterizedStructLiteral::IsGeneric() const {
  // TODO
  return false;
}

bool StructType::IsGeneric() const {
  // TODO
  return false;
}

bool Switch::IsGeneric() const {
  // TODO
  return false;
}

bool Terminal::IsGeneric() const { return false; }

bool Unop::IsGeneric() const { return operand()->IsGeneric(); }

}  // namespace ast
