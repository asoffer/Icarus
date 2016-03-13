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
      Dependency::type_value(this, Scope::Global->identifier("string"));
    }
  }

  void Identifier::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::value_value(this, decl);
    Dependency::type_type(this, decl);
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
    }
    operand->record_dependencies();
  }

  void ArrayLiteral::record_dependencies() {
    for (const auto& el : elems) {
      Dependency::type_type(this, el);
      Dependency::value_type(this, this);
      el->record_dependencies();
    }
  }

  void Binop::record_dependencies() {
    Dependency::value_type(this, this);

    Dependency::type_type(this, lhs);
    lhs->record_dependencies();

    if (op != Language::Operator::Access) {
      Dependency::type_type(this, rhs);
      rhs->record_dependencies();
    }

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
    for (auto& e : exprs) {
      Dependency::type_type(this, e);
      e->record_dependencies();
    }
  }

  void Declaration::record_dependencies() {
    switch (decl_type) {
    case DeclType::Std: {
      Dependency::value_type(this, this);
      Dependency::type_value(this, type_expr);
    } break;
    case DeclType::Infer:
    case DeclType::In: {
      Dependency::type_type(this, type_expr);
      Dependency::value_value(this, type_expr);
      Dependency::value_type(this, this);
    } break;
    }

    identifier->record_dependencies();
    type_expr->record_dependencies();
  }

  void Case::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& kv : kv->pairs) {
      Dependency::type_type(this, kv.first);
      Dependency::type_type(this, kv.second);
    }

    for (const auto& kv : kv->pairs) {
      kv.first->record_dependencies();
      kv.second->record_dependencies();
    } 
  }

  void FunctionLiteral::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& in : inputs) {
      Dependency::type_type(this, in);
    }

     Dependency::type_type(this, return_type_expr);
     Dependency::type_value(this, return_type_expr);

    for (const auto& in : inputs) {
      in->record_dependencies();
    }

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
    for (const auto& stmt : statements) {
      Dependency::value_value(this, stmt);
      stmt->record_dependencies();
    }
  }

  void Conditional::record_dependencies() {
    for (auto& stmt : statements) {
      Dependency::value_value(this, stmt);
      stmt->record_dependencies();
    }
    for (auto& cond : conditions) {
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
    Dependency::type_value(this, container);
    Dependency::type_type(this, container);
    Dependency::type_type(this, statements);

    // Iterator doesn't have a declaration specifically. It's implicitly
    // declared here

    statements->record_dependencies();
    iterator->record_dependencies();
    container->record_dependencies();

    Dependency::type_type(iterator, this);
  }

  void TypeLiteral::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& decl : declarations) {
      Dependency::value_type(this, decl->identifier);
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
