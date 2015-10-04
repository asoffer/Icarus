#include "AST/Scope.h"
#include "AST/Identifier.h"
#include "AST/Declaration.h"


namespace AST {
  std::vector<Scope*> Scope::all_scopes = {};

  IdPtr Scope::identifier(const std::string& token_string) {

    auto iter = id_map_.find(token_string);
    if (iter != id_map_.end()) {
      return iter->second;
    }

    return id_map_[token_string] = IdPtr(new Identifier(token_string));
  }

  void Scope::verify_scope() {
    join_identifiers(this);
    find_all_decls(this);

    // Find and log all undeclared identifers. If you find any, log them and
    // stop checking the current scope. There is no more useful information to
    // be found.
    if (log_undeclared_identifiers()) return;

    verify_types();
  }


  void Scope::show_identifiers() const {
    for (const auto& ids : id_map_) {
      std::cout << ids.first << std::endl;
    }
  }

  void Scope::register_declaration(Declaration* decl) {
    auto id_ptr = id_map_[decl->lhs_->token()];
    if (id_ptr->expr_type_ != Type::Unknown) {
      std::cerr << "Identifier redeclared in scope: `" << id_ptr->token() << "`" << std::endl;
    }

    id_ptr->expr_type_ =
      Type::Literals.at(decl->rhs_->token());
  }

  bool Scope::log_undeclared_identifiers() const {
    bool found_undeclared_ident = false;
    for (const auto& ident : id_map_) {
      if (ident.second->expr_type_ == Type::Unknown) {
        std::cerr << "Undeclared identifier: `" << ident.first << "`" << std::endl;
        found_undeclared_ident = true;
      }
    }
    return found_undeclared_ident;
  }
}  // namespace AST
