#include "AST.h"

namespace AST {
  void Unop::needed_for(IdPtr id_ptr) const {
    if (expr_->is_identifier()) {
      ScopeDB::dependencies_[id_ptr]
        .insert(std::static_pointer_cast<Identifier>(expr_));

    } else {
      expr_->needed_for(id_ptr);
    }
  }

  void Binop::needed_for(IdPtr id_ptr) const {
    if (lhs_->is_identifier()) {
        ScopeDB::dependencies_[id_ptr]
          .insert(std::static_pointer_cast<Identifier>(lhs_));

    } else {
      lhs_->needed_for(id_ptr);
    }

    if (rhs_->is_identifier()) {
        ScopeDB::dependencies_[id_ptr]
          .insert(std::static_pointer_cast<Identifier>(rhs_));

    } else {
      rhs_->needed_for(id_ptr);
    }
  }

  void ChainOp::needed_for(IdPtr id_ptr) const {
    for (auto& expr : exprs_) {
      if (expr->is_identifier()) {
        ScopeDB::dependencies_[id_ptr]
          .insert(std::static_pointer_cast<Identifier>(expr));

      } else {
        expr->needed_for(id_ptr);
      }
    }
  }

  void Declaration::needed_for(IdPtr id_ptr) const {
    if (infer_type_) {
      decl_type_->needed_for(id_ptr);
    } else {
      // TODO
    }
  }

  void Case::needed_for(IdPtr id_ptr) const {
    std::cout << "*?*?*?" << std::endl;
    pairs_->needed_for(id_ptr);
  }

  void KVPairList::needed_for(IdPtr id_ptr) const {
    for (const auto& kv : kv_pairs_) {
      if (kv.first->is_identifier()) {
        ScopeDB::dependencies_[id_ptr]
          .insert(std::static_pointer_cast<Identifier>(kv.first));
      } else {
        kv.first->needed_for(id_ptr);
      }

      if (kv.second->is_identifier()) {
        ScopeDB::dependencies_[id_ptr]
          .insert(std::static_pointer_cast<Identifier>(kv.second));
        std::cout << "*?*?*?" << std::endl;
      } else {
        kv.second->needed_for(id_ptr);
      }
    }
  }


  void FunctionLiteral::needed_for(IdPtr id_ptr) const {
    // TODO
  }

}  // namespace AST
