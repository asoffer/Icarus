#include "AST.h"

namespace AST {
  void Unop::needed_for(IdPtr id_ptr) const {
    if (expr_->is_identifier()) {
      ScopeDB::needed_for_[std::static_pointer_cast<Identifier>(expr_)]
        .insert(id_ptr);

    } else {
      expr_->needed_for(id_ptr);
    }
  }

  void Binop::needed_for(IdPtr id_ptr) const {
    if (lhs_->is_identifier()) {
        ScopeDB::needed_for_[std::static_pointer_cast<Identifier>(lhs_)]
          .insert(id_ptr);

    } else {
      lhs_->needed_for(id_ptr);
    }

    if (rhs_->is_identifier()) {
        ScopeDB::needed_for_[std::static_pointer_cast<Identifier>(rhs_)]
          .insert(id_ptr);

    } else {
      rhs_->needed_for(id_ptr);
    }
  }

  void ChainOp::needed_for(IdPtr id_ptr) const {
    for (auto& expr : exprs_) {
      if (expr->is_identifier()) {

        ScopeDB::needed_for_[std::static_pointer_cast<Identifier>(expr)]
          .insert(id_ptr);

      } else {
        expr->needed_for(id_ptr);
      }
    }
  }

  void Declaration::needed_for(IdPtr id_ptr) const {
    // TODO
  }

  void Case::needed_for(IdPtr id_ptr) const {
    // TODO
  }

  void FunctionLiteral::needed_for(IdPtr id_ptr) const {
    // TODO
  }

}  // namespace AST
