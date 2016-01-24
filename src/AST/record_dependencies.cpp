#include "AST.h"

#include <cassert>

namespace debug {
  extern bool dependency_system;
}  // namespace debug


#define DEBUG_KILL(x, where)          \
  if (debug::dependency_system) {     \
    if ( (x) == nullptr ) {           \
      std::cout << "-> " << where << std::endl; \
      assert(false);                  \
    }                                 \
  }

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
    DEBUG_KILL(expr_, "unop");

    Scope::dependencies_[eptr].insert(expr_);
    expr_->record_dependencies(expr_);
  }

  void ArrayLiteral::record_dependencies(EPtr eptr) const {
    for (const auto& el : elems_) {
      DEBUG_KILL(el, "array_lit");

      Scope::dependencies_[eptr].insert(el);
      el->record_dependencies(el);
    }
  }

  void Binop::record_dependencies(EPtr eptr) const {
    DEBUG_KILL(lhs_, "binop lhs");

    Scope::dependencies_[eptr].insert(lhs_);
    lhs_->record_dependencies(lhs_);

    if (op_ != Language::Operator::Access) {
      DEBUG_KILL(rhs_, "binop rhs");

      Scope::dependencies_[eptr].insert(rhs_);
      rhs_->record_dependencies(rhs_);
    }
  }

  void ArrayType::record_dependencies(EPtr eptr) const {
    if (len_ != nullptr) {
      DEBUG_KILL(len_, "arraytype len");
      Scope::dependencies_[eptr].insert(len_);
    }

    DEBUG_KILL(array_type_, "ararytype");
    Scope::dependencies_[eptr].insert(array_type_);

    if (len_ != nullptr) {
      len_->record_dependencies(len_);
    }
    array_type_->record_dependencies(array_type_);
  }

  void ChainOp::record_dependencies(EPtr eptr) const {
    for (auto& e : exprs_) {
      DEBUG_KILL(e, "chain");
      Scope::dependencies_[eptr].insert(e);
      e->record_dependencies(e);
    }
  }

  void Declaration::record_dependencies(EPtr eptr) const {
    DEBUG_KILL(decl_type_, "decl");
    Scope::dependencies_[eptr].insert(decl_type_);
    decl_type_->record_dependencies(decl_type_);
  }

  void Case::record_dependencies(EPtr eptr) const {
    for (const auto& kv : pairs_->kv_pairs_) {
      DEBUG_KILL(kv.first, "case 1");
      Scope::dependencies_[eptr].insert(kv.first);
      DEBUG_KILL(kv.second, "case 2");
      Scope::dependencies_[eptr].insert(kv.second);
    }

    for (const auto& kv : pairs_->kv_pairs_) {
      kv.first->record_dependencies(kv.first);
      kv.second->record_dependencies(kv.second);
    } 
  }

  void FunctionLiteral::record_dependencies(EPtr eptr) const {
    for (const auto& in : inputs_) {
      DEBUG_KILL(in, "fnlit in");
      Scope::dependencies_[eptr].insert(in);
    }

    DEBUG_KILL(return_type_, "fnlit rettype");
    Scope::dependencies_[eptr].insert(return_type_);

    for (const auto& in : inputs_) {
      in->record_dependencies(in);
    }

    return_type_->record_dependencies(return_type_);
    statements_->record_dependencies(nullptr);
  }

  void Terminal::record_dependencies(EPtr eptr) const {
    DEBUG_KILL(eptr, "terminal");
    // NOTE: Need to ensure that eptr is a node in the dependency graph,
    // so even though we aren't assigning anything to it, we must access
    // it here.
    Scope::dependencies_[eptr];
  }

  void Identifier::record_dependencies(EPtr eptr) const {
    DEBUG_KILL(Scope::decl_of_[std::static_pointer_cast<Identifier>(eptr)], "id");
    Scope::dependencies_[eptr].insert(std::static_pointer_cast<Expression>(
          Scope::decl_of_[std::static_pointer_cast<Identifier>(eptr)]));
  }

  void KVPairList::record_dependencies(EPtr /* nullptr */) const {
#ifdef DEBUG
    std::cerr << "FATAL: KVPairList::record_dependencies(EPtr) should never be called" << std::endl;
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

  void Conditional::record_dependencies(EPtr eptr) const {
    for (size_t i = 0; i < statements_.size(); ++i) {
      statements_[i]->record_dependencies(nullptr);
    }

    for (size_t i = 0; i < conds_.size(); ++i) {
      conds_[i]->record_dependencies(conds_[i]);
    }
  }

  void While::record_dependencies(EPtr eptr) const {
    statements_->record_dependencies(nullptr);
    cond_->record_dependencies(cond_);
  }

  void TypeLiteral::record_dependencies(EPtr eptr) const {
    for (const auto& decl : decls_) {
      DEBUG_KILL(decl, "typelit");
      Scope::dependencies_[eptr].insert(decl);
      decl->record_dependencies(decl);
    }
  }

  void EnumLiteral::record_dependencies(EPtr eptr) const {
    for (const auto& val : vals_) {
      DEBUG_KILL(val, "enum");
      Scope::dependencies_[eptr].insert(val);
      val->record_dependencies(val);
    }
  }
}  // namespace AST

#undef DEBUG_KILL
