#include "AST.h"

void set_or_recurse(EPtr& eptr, Scope* scope) {
  if (eptr->is_identifier()) {
    eptr = scope->identifier(eptr);
  } else {
    eptr->join_identifiers(scope);
  }
}

namespace AST {
  void Unop::join_identifiers(Scope* scope, bool is_arg) {
    set_or_recurse(operand, scope);
  }

  void While::join_identifiers(Scope* scope, bool is_arg) {
    condition->join_identifiers(while_scope);
    statements->join_identifiers(while_scope);
  }

  void ArrayLiteral::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& el : elems) {
      set_or_recurse(el, scope);
    }
  }

  void Terminal::join_identifiers(Scope* scope, bool is_arg) {}

  void Identifier::join_identifiers(Scope* scope, bool is_arg) {
    Terminal::join_identifiers(scope);
  }

  void Conditional::join_identifiers(Scope* scope, bool is_arg) {
    for (size_t i = 0; i < conds_.size(); ++i) {
      conds_[i]->join_identifiers(body_scopes_[i]);
    }

    for (size_t i = 0; i < statements.size(); ++i) {
      statements[i]->join_identifiers(body_scopes_[i]);
    }
  }

  void Access::join_identifiers(Scope* scope, bool is_arg) {
    set_or_recurse(operand, scope);
  }

  void Binop::join_identifiers(Scope* scope, bool is_arg) {
    set_or_recurse(lhs, scope);

    // Ignore the RHS of a dot operator
    // TODO Access should be looking in a different scope
    // Should it be looking at all?
    // Should this even be a binary operator?
    if (op == Language::Operator::Access) return;

    set_or_recurse(rhs, scope);
  }

  void ArrayType::join_identifiers(Scope* scope, bool is_arg) {
    if (length != nullptr) {
      set_or_recurse(length, scope);
    }

    set_or_recurse(data_type, scope);
  }

  void Declaration::join_identifiers(Scope* scope, bool is_arg) {
    identifier = std::static_pointer_cast<Identifier>(
        scope->identifier(identifier));
    if (is_arg) {
      identifier->is_function_arg = true;
    }
    identifier->line_num = line_num; // Hacky and probably wrong TODO FIXME

    set_or_recurse(type_expr, scope);
  }

  void ChainOp::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& expr : exprs) {
      set_or_recurse(expr, scope);
    }
  }

  void Case::join_identifiers(Scope* scope, bool is_arg) {
    kv->join_identifiers(scope);
  }

  void KVPairList::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& pair : pairs) {
      set_or_recurse(pair.first, scope);
      set_or_recurse(pair.second, scope);
    }
  }

  void Statements::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& ptr : statements) {
      if (ptr->is_identifier()) {
        ptr = std::static_pointer_cast<Node>(
            scope->identifier(std::static_pointer_cast<Expression>(ptr)));
      } else {
        ptr->join_identifiers(scope);
      }
    }
  }

  void FunctionLiteral::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& in : inputs_) {
      in->join_identifiers(fn_scope_, true);
    }

    set_or_recurse(return_type_, fn_scope_);
    statements->join_identifiers(fn_scope_);
  }

  void TypeLiteral::join_identifiers(Scope*, bool) {
    for (auto& decl : declarations) {
      decl->join_identifiers(type_scope);
    }
  }

  void EnumLiteral::join_identifiers(Scope* scope, bool is_arg) {}
}  // namespace AST
