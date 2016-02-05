#include "AST.h"
#include "DependencySystem.h"

#include <cassert>

namespace debug {
  extern bool dependency_system;
}  // namespace debug

#define DEBUG_KILL(x) if (debug::dependency_system) assert(x);

namespace AST {
  void Unop::record_dependencies() {
    DEBUG_KILL(expr_);
    Dependency::type_type(this, expr_.get());
    expr_->record_dependencies();
  }

  void ArrayLiteral::record_dependencies() {
    for (const auto& el : elems_) {
      DEBUG_KILL(el);

      Dependency::type_type(this, el.get());
      el->record_dependencies();
    }
  }

  void Binop::record_dependencies() {
    DEBUG_KILL(lhs_);

    Dependency::type_type(this, lhs_.get());
    lhs_->record_dependencies();

    if (op_ != Language::Operator::Access) {
      DEBUG_KILL(rhs_);

      Dependency::type_type(this, rhs_.get());
      rhs_->record_dependencies();
    }
  }

  void ArrayType::record_dependencies() {
    if (len_ != nullptr) {
      DEBUG_KILL(len_);
      Dependency::type_type(this, len_.get());
    }

    DEBUG_KILL(array_type_);
    Dependency::type_type(this, array_type_.get());

    if (len_ != nullptr) {
      len_->record_dependencies();
    }
    array_type_->record_dependencies();
  }

  void ChainOp::record_dependencies() {
    for (auto& e : exprs_) {
      DEBUG_KILL(e);
      Dependency::type_type(this, e.get());
      e->record_dependencies();
    }
  }

  void Declaration::record_dependencies() {
    DEBUG_KILL(decl_type_);

    Dependency::type_type(id_.get(), this);
    Dependency::value_type(id_.get(), this);
    if (infer_type_) {
      Dependency::type_type(this, decl_type_.get());
    } else {
      Dependency::type_value(this, decl_type_.get());
    }

    decl_type_->record_dependencies();
  }

  void Case::record_dependencies() {
    for (const auto& kv : pairs_->kv_pairs_) {
      DEBUG_KILL(kv.first);
      Dependency::type_type(this, kv.first.get());
      DEBUG_KILL(kv.second);
      Dependency::type_type(this, kv.second.get());
    }

    for (const auto& kv : pairs_->kv_pairs_) {
      kv.first->record_dependencies();
      kv.second->record_dependencies();
    } 
  }

  void FunctionLiteral::record_dependencies() {
    for (const auto& in : inputs_) {
      DEBUG_KILL(in);
      Dependency::type_type(this, in.get());
    }

    DEBUG_KILL(return_type_);
    Dependency::type_type(this, return_type_.get());

    for (const auto& in : inputs_) {
      in->record_dependencies();
    }

    return_type_->record_dependencies();
    statements_->record_dependencies();
  }

  void Terminal::record_dependencies() {
    DEBUG_KILL(this);
    Dependency::add_to_table(this);
  }

  void Identifier::record_dependencies() {
    Dependency::type_type(this, Scope::decl_of_[shared_from_this()].get());
  }

  void KVPairList::record_dependencies() {
    assert(false && "KVPairList::record_dependencies() should never be called");
  }

  void Statements::record_dependencies() {
    for (const auto& stmt : statements_) {
      stmt->record_dependencies();
    }
  }

  void Conditional::record_dependencies() {
    for (auto& stmt : statements_) stmt->record_dependencies();
    for (auto& cond : conds_)      cond->record_dependencies();
  }

  void While::record_dependencies() {
    statements_->record_dependencies();
    cond_->record_dependencies();
  }

  void TypeLiteral::record_dependencies() {
    for (const auto& decl : decls_) {
      DEBUG_KILL(decl);
      Dependency::type_type(this, decl.get());
      decl->record_dependencies();
    }
  }

  void EnumLiteral::record_dependencies() {
    DEBUG_KILL(this);
    Dependency::add_to_table(this);
  }
}  // namespace AST

#undef DEBUG_KILL
