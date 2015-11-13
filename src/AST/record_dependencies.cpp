#include "AST.h"

// These functions are confusing because we're fighting an annoyance of
// shared_ptr. Each function is supposed to record its dependencies for type
// computation. The problem is that `this` points to the object itself rather
// than the shared_ptr to itself. As far as I know these needn't be the same
// address, so we need to also carry information about the shared pointer to
// itself. We pass this in as an argument.
//
// One may consider having it as a member variable, but then the reference
// counting would be useless. Maybe this is a situation for weak_ptr.
//
// TODO look in to weak_ptrs.
namespace AST {

  void Unop::record_dependencies(EPtr eptr) const {
      ScopeDB::dependencies_[eptr].insert(expr_);
      expr_->record_dependencies(expr_);
  }

  void Binop::record_dependencies(EPtr eptr) const {
      ScopeDB::dependencies_[eptr].insert(lhs_);
      ScopeDB::dependencies_[eptr].insert(rhs_);

      lhs_->record_dependencies(lhs_);
      rhs_->record_dependencies(rhs_);
  }

  void ArrayType::record_dependencies(EPtr eptr) const {
      ScopeDB::dependencies_[eptr].insert(len_);
      ScopeDB::dependencies_[eptr].insert(array_type_);

      len_->record_dependencies(len_);
      array_type_->record_dependencies(array_type_);
  }

  void ChainOp::record_dependencies(EPtr eptr) const {
    for (auto& e : exprs_) {
      ScopeDB::dependencies_[eptr].insert(e);
    }

    for (auto& e : exprs_) {
      e->record_dependencies(e);
    }
  }

  void Declaration::record_dependencies(EPtr eptr) const {
    ScopeDB::dependencies_[eptr].insert(decl_type_);
    decl_type_->record_dependencies(decl_type_);
  }

  void Case::record_dependencies(EPtr eptr) const {
    for (const auto& kv : pairs_->kv_pairs_) {
      ScopeDB::dependencies_[eptr].insert(kv.first);
      ScopeDB::dependencies_[eptr].insert(kv.second);
    }

    for (const auto& kv : pairs_->kv_pairs_) {
      kv.first->record_dependencies(kv.first);
      kv.second->record_dependencies(kv.second);
    } 
  }

  void FunctionLiteral::record_dependencies(EPtr eptr) const {
    for (const auto& in : inputs_) {
      ScopeDB::dependencies_[eptr].insert(in);
    }

    ScopeDB::dependencies_[eptr].insert(return_type_);

    for (const auto& in : inputs_) {
      in->record_dependencies(in);
    }

    return_type_->record_dependencies(return_type_);
    statements_->record_dependencies(nullptr);
  }

  void Terminal::record_dependencies(EPtr eptr) const {
    ScopeDB::dependencies_[eptr];
  }

  void Identifier::record_dependencies(EPtr eptr) const {
    ScopeDB::dependencies_[eptr].insert(std::static_pointer_cast<Expression>(
        ScopeDB::decl_of_[std::static_pointer_cast<Identifier>(eptr)]));
  }

  void KVPairList::record_dependencies(EPtr /* nullptr */) const {
#ifdef DEBUG
    std::cerr << "KVPairList::record_dependencies(EPtr) should never be called" << std::endl;
#endif
  }

  void Statements::record_dependencies(EPtr /* nullptr */) const {
    for (const auto& stmt : statements_) {
      if (stmt->is_expression()) {
        stmt->record_dependencies(std::static_pointer_cast<Expression>(stmt));

      } else {
        stmt->record_dependencies(nullptr);
      }
    }
  }

  void While::record_dependencies(EPtr /* nullptr */) const {
    // TODO implement this
  }
}  // namespace AST
