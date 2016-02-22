#include "AST.h"
#include "DependencySystem.h"

#include <cassert>

namespace debug {
  extern bool dependency_system;
}  // namespace debug

namespace AST {
  void Terminal::record_dependencies() {
    Dependency::value_type(this, this);
    if (terminal_type == Language::Terminal::StringLiteral) {
      Dependency::type_value(this, Scope::Global->identifier("string").get());
    }
  }

  void Identifier::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::value_value(this, decl);
    Dependency::type_type(this, decl);
  }

  void Access::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::value_value(this, operand.get());
    Dependency::type_type(this, operand.get());
    operand->record_dependencies();
  }

  void Unop::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::type_type(this, operand.get());
    if (op == Language::Operator::Return) {
      Dependency::value_value(this, operand.get());
    }
    operand->record_dependencies();
  }

  void ArrayLiteral::record_dependencies() {
    for (const auto& el : elems) {
      Dependency::type_type(this, el.get());
      Dependency::value_type(this, this);
      el->record_dependencies();
    }
  }

  void Binop::record_dependencies() {
    Dependency::value_type(this, this);

    Dependency::type_type(this, lhs.get());
    lhs->record_dependencies();

    if (op != Language::Operator::Access) {
      Dependency::type_type(this, rhs.get());
      rhs->record_dependencies();
    }

    Dependency::value_value(this, lhs.get());
    Dependency::value_value(this, rhs.get());
  }

  void ArrayType::record_dependencies() {

    if (length != nullptr) {
      Dependency::value_value(this, length.get());

      // Maybe later when we have length dependency?
      Dependency::type_type(this, length.get());

      length->record_dependencies();
    }

    Dependency::value_type(this, data_type.get());
    Dependency::type_type(this, data_type.get());
    data_type->record_dependencies();
  }

  void ChainOp::record_dependencies() {
    Dependency::value_type(this, this);
    for (auto& e : exprs) {
      Dependency::type_type(this, e.get());
      e->record_dependencies();
    }
  }

  void Declaration::record_dependencies() {
    if (is_inferred) {
      Dependency::type_type(this, type_expr.get());
      Dependency::value_value(this, type_expr.get());
      Dependency::value_type(this, this);
    } else {
      Dependency::value_type(this, this);
      Dependency::type_value(this, type_expr.get());
    }

    identifier->record_dependencies();
    type_expr->record_dependencies();
  }

  void Case::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& kv : kv->pairs) {
      Dependency::type_type(this, kv.first.get());
      Dependency::type_type(this, kv.second.get());
    }

    for (const auto& kv : kv->pairs) {
      kv.first->record_dependencies();
      kv.second->record_dependencies();
    } 
  }

  void FunctionLiteral::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& in : inputs) {
      Dependency::type_type(this, in.get());
    }

     Dependency::type_type(this, return_type_expr.get());
     Dependency::type_value(this, return_type_expr.get());

    for (const auto& in : inputs) {
      in->record_dependencies();
    }

    Dependency::value_value(this, statements.get());
    Dependency::value_type(this, statements.get());

    Dependency::value_value(this, return_type_expr.get());
    Dependency::value_type(this, return_type_expr.get());

    return_type_expr->record_dependencies();
    statements->record_dependencies();
  }

  void KVPairList::record_dependencies() {
    assert(false && "KVPairList::record_dependencies() should never be called");
  }

  void Statements::record_dependencies() {
    for (const auto& stmt : statements) {
      Dependency::value_value(this, stmt.get());
      stmt->record_dependencies();
    }
  }

  void Conditional::record_dependencies() {
    for (auto& stmt : statements) {
      Dependency::value_value(this, stmt.get());
      stmt->record_dependencies();
    }
    for (auto& cond : conditions) {
      Dependency::value_value(this, cond.get());
      cond->record_dependencies();
    }
  }

  void While::record_dependencies() {
    // NOTE: We don't depend on internal values
    // because this isn't allowed at compile-time
    // TODO check evaluate
    Dependency::value_type(this, this);
    Dependency::type_type(this, condition.get());
    Dependency::type_type(this, statements.get());
    statements->record_dependencies();
    condition->record_dependencies();
  }

  void TypeLiteral::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& decl : declarations) {
      // NOTE: Assuming only : and no :=. TODO Fix this when you allow :=
      Dependency::value_type(this, decl->identifier.get());
      decl->record_dependencies();
    }
  }

  void EnumLiteral::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::add_to_table(this);
  }

  void Break::record_dependencies() {
    Dependency::add_to_table(this);
  }
}  // namespace AST
