#include "AST.h"
#include "DependencySystem.h"

#include <cassert>

namespace debug {
  extern bool dependency_system;
}  // namespace debug

namespace AST {
  void Terminal::record_dependencies() {
    Dependency::value_type(this, this);
    if (terminal_type_ == Language::Terminal::StringLiteral) {
      Dependency::type_value(this, Scope::Global->identifier("string").get());
    }
  }

  void Identifier::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::value_value(this, decl_);
    Dependency::type_type(this, decl_);
  }

  void Access::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::value_value(this, expr_.get());
    Dependency::type_type(this, expr_.get());
    expr_->record_dependencies();
  }

  void Unop::record_dependencies() {
    Dependency::value_type(this, this);
    Dependency::value_type(this, expr_.get());
    Dependency::type_type(this, expr_.get());
    if (op_ == Language::Operator::Return) {
      Dependency::value_value(this, expr_.get());
    }
    expr_->record_dependencies();
  }

  void ArrayLiteral::record_dependencies() {
    for (const auto& el : elems_) {
      Dependency::type_type(this, el.get());
      Dependency::value_type(this, this);
      el->record_dependencies();
    }
  }

  void Binop::record_dependencies() {
    Dependency::value_type(this, this);

    Dependency::type_type(this, lhs_.get());
    lhs_->record_dependencies();

    if (op_ != Language::Operator::Access) {
      Dependency::type_type(this, rhs_.get());
      rhs_->record_dependencies();
    }

    Dependency::value_value(this, lhs_.get());
    Dependency::value_value(this, rhs_.get());
  }

  void ArrayType::record_dependencies() {

    if (len_ != nullptr) {
      Dependency::value_value(this, len_.get());

      // Maybe later when we have length dependency?
      Dependency::type_type(this, len_.get());

      len_->record_dependencies();
    }

    Dependency::value_type(this, array_type_.get());
    Dependency::type_type(this, array_type_.get());
    array_type_->record_dependencies();
  }

  void ChainOp::record_dependencies() {
    Dependency::value_type(this, this);
    for (auto& e : exprs_) {
      Dependency::type_type(this, e.get());
      e->record_dependencies();
    }
  }

  void Declaration::record_dependencies() {
    if (type_is_inferred()) {
      Dependency::type_type(this, decl_type_.get());
      Dependency::value_value(this, decl_type_.get());
      Dependency::value_type(this, this);
    } else {
      Dependency::type_value(this, decl_type_.get());
    }

    id_->record_dependencies();
    decl_type_->record_dependencies();
  }

  void Case::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& kv : pairs_->kv_pairs_) {
      Dependency::type_type(this, kv.first.get());
      Dependency::type_type(this, kv.second.get());
    }

    for (const auto& kv : pairs_->kv_pairs_) {
      kv.first->record_dependencies();
      kv.second->record_dependencies();
    } 
  }

  void FunctionLiteral::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& in : inputs_) {
      Dependency::type_type(this, in.get());
    }

    Dependency::type_type(this, return_type_.get());

    for (const auto& in : inputs_) {
      in->record_dependencies();
    }

    Dependency::value_value(this, statements_.get());
    Dependency::value_type(this, statements_.get());

    Dependency::value_value(this, return_type_.get());
    Dependency::value_type(this, return_type_.get());

    return_type_->record_dependencies();
    statements_->record_dependencies();
  }

  void KVPairList::record_dependencies() {
    assert(false && "KVPairList::record_dependencies() should never be called");
  }

  void Statements::record_dependencies() {
    for (const auto& stmt : statements_) {
      Dependency::value_value(this, stmt.get());
      stmt->record_dependencies();
    }
  }

  void Conditional::record_dependencies() {
    for (auto& stmt : statements_) {
      Dependency::value_value(this, stmt.get());
      stmt->record_dependencies();
    }
    for (auto& cond : conds_) {
      Dependency::value_value(this, cond.get());
      cond->record_dependencies();
    }
  }

  void While::record_dependencies() {
    // NOTE: We don't depend on internal values
    // because this isn't allowed at compile-time
    // TODO check evaluate
    Dependency::value_type(this, this);
    Dependency::type_type(this, cond_.get());
    Dependency::type_type(this, statements_.get());
    statements_->record_dependencies();
    cond_->record_dependencies();
  }

  void TypeLiteral::record_dependencies() {
    Dependency::value_type(this, this);
    for (const auto& decl : decls_) {
      // NOTE: Assuming only : and no :=. TODO Fix this when you allow :=
      Dependency::value_type(this, decl->declared_identifier().get());
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
