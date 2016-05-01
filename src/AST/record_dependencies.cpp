#include "Scope.h"

namespace debug {
  extern bool dependency_system;
}  // namespace debug

namespace AST {
void Terminal::record_dependencies() {
  Dependency::value_type(this, this);
  if (terminal_type == Language::Terminal::StringLiteral) {
    Dependency::type_value(this, Scope::Global->IdentifierHereOrNull("string"));
  }
}

void Identifier::record_dependencies() {
  Dependency::value_type(this, this);
  for (const auto decl : decls) {
    Dependency::value_value(this, decl);
    Dependency::type_type(this, decl);
  }

  // Also depend on the identifier at higher scopes
  auto scope_ptr = scope_->parent;
  while (scope_ptr) {
    auto id_ptr = scope_ptr->IdentifierHereOrNull(token());
    if (id_ptr) {
      Dependency::value_value(this, id_ptr);
      Dependency::type_type(this, id_ptr);
      break;
    }
    scope_ptr = scope_ptr->parent;
  }
}

void Access::record_dependencies() {
  Dependency::value_type(this, this);
  Dependency::value_value(this, operand);
  Dependency::type_type(this, operand);
  operand->record_dependencies();
}

void Unop::record_dependencies() {
  Dependency::value_type(this, this);
  Dependency::type_type(this, operand);
  if (op == Language::Operator::Return) {
    Dependency::value_value(this, operand);
  } else if (op == Language::Operator::Sub) {
    auto scope_ptr = scope_;
    while (scope_ptr) {
      auto id_ptr = scope_ptr->IdentifierHereOrNull(token());
      if (id_ptr) {
        Dependency::value_value(this, id_ptr);
        Dependency::type_type(this, id_ptr);
      }
      scope_ptr = scope_ptr->parent;
    }
  }

  operand->record_dependencies();
}

void ArrayLiteral::record_dependencies() {
  for (const auto &el : elems) {
    Dependency::type_type(this, el);
    Dependency::value_type(this, this);
    el->record_dependencies();
  }
}

void Binop::record_dependencies() {
  Dependency::value_type(this, this);

  Dependency::type_type(this, lhs);
  lhs->record_dependencies();

  Dependency::type_type(this, rhs);
  rhs->record_dependencies();

  Dependency::value_value(this, lhs);
  Dependency::value_value(this, rhs);
}

void ArrayType::record_dependencies() {

  if (length != nullptr) {
    Dependency::value_value(this, length);

    // Maybe later when we have length dependency?
    Dependency::type_type(this, length);

    length->record_dependencies();
  }

  Dependency::value_type(this, data_type);
  Dependency::type_type(this, data_type);
  data_type->record_dependencies();
}

void ChainOp::record_dependencies() {
  Dependency::value_type(this, this);
  for (auto &e : exprs) {
    Dependency::type_type(this, e);
    Dependency::value_value(this, e);
    e->record_dependencies();
  }
}

void Declaration::record_dependencies() {
  switch (decl_type) {
  case DeclType::Std: {
    Dependency::value_type(this, this);
    Dependency::type_value(this, type_expr);
  } break;
  case DeclType::In:
    Dependency::type_value(this, type_expr);
    // Note: Intentionally falling through
  case DeclType::Infer: {
    Dependency::type_type(this, type_expr);
    Dependency::value_value(this, type_expr);
    Dependency::value_type(this, this);
  } break;
  case DeclType::Tick: {
    Dependency::type_type(this, type_expr);
    Dependency::type_value(this, type_expr);
    Dependency::value_type(this, this);
    Dependency::value_value(this, type_expr);
  } break;
  }

  identifier->record_dependencies();
  type_expr->record_dependencies();
}

void Case::record_dependencies() {
  Dependency::value_type(this, this);
  for (const auto &kv : kv->pairs) {
    Dependency::type_type(this, kv.first);
    Dependency::type_type(this, kv.second);
  }

  for (const auto &kv : kv->pairs) {
    kv.first->record_dependencies();
    kv.second->record_dependencies();
  }
}

void FunctionLiteral::record_dependencies() {
  Dependency::value_type(this, this);
  for (const auto &in : inputs) { Dependency::type_type(this, in); }

  Dependency::type_type(this, return_type_expr);
  Dependency::type_value(this, return_type_expr);

  for (const auto &in : inputs) { in->record_dependencies(); }

  Dependency::value_value(this, statements);
  Dependency::value_type(this, statements);

  Dependency::value_value(this, return_type_expr);
  Dependency::value_type(this, return_type_expr);

  return_type_expr->record_dependencies();
  statements->record_dependencies();
}

void KVPairList::record_dependencies() {
  assert(false && "KVPairList::record_dependencies() should never be called");
}

void Statements::record_dependencies() {
  for (const auto &stmt : statements) {
    Dependency::value_value(this, stmt);
    stmt->record_dependencies();
  }
}

void Conditional::record_dependencies() {
  for (auto &stmt : statements) {
    Dependency::value_value(this, stmt);
    stmt->record_dependencies();
  }
  for (auto &cond : conditions) {
    Dependency::value_value(this, cond);
    cond->record_dependencies();
  }
}

void While::record_dependencies() {
  // NOTE: We don't depend on internal values
  // because this isn't allowed at compile-time
  // TODO check evaluate
  Dependency::value_type(this, this);
  Dependency::type_type(this, condition);
  Dependency::type_type(this, statements);
  statements->record_dependencies();
  condition->record_dependencies();
}

void For::record_dependencies() {
  // NOTE: We don't depend on internal values
  // because this isn't allowed at compile-time
  // TODO check evaluate
  Dependency::value_type(this, this);
  for (auto iter : iterators) {
    Dependency::type_type(this, iter);
    Dependency::value_value(this, iter);
  }
  Dependency::type_type(this, statements);

  statements->record_dependencies();
  for (auto iter : iterators) { iter->record_dependencies(); }

}

void StructLiteral::record_dependencies() {
  Dependency::value_type(this, this);
  for (const auto &param : params) {
    Dependency::value_type(this, param->identifier);
    param->record_dependencies();
  }

  for (const auto &decl : declarations) {
    Dependency::value_type(this, decl->identifier);
    decl->record_dependencies();
  }
}

void EnumLiteral::record_dependencies() {
  Dependency::value_type(this, this);
  Dependency::add_to_table(this);
}

void Jump::record_dependencies() { Dependency::add_to_table(this); }
void DummyTypeExpr::record_dependencies() { Dependency::add_to_table(this); }
} // namespace AST
 
