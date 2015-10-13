#include "AST.h"

namespace AST {
  /****************************************
   *          NAMESPACES GLOBALS          *
   ****************************************/
  std::vector<Scope*> Scope::scope_registry;


  /****************************************
   *            MISCELLANEOUS             *
   ****************************************/


  /* ANONYMOUS SCOPE */
  void AnonymousScope::add_statements(NPtr&& stmts_ptr) {
    auto stmts = std::unique_ptr<Statements>(
        static_cast<Statements*>(stmts_ptr.release()));

    statements_->statements_.reserve( statements_->size() + stmts->size() );

    for (size_t i = 0; i < stmts->size(); ++i) {
      statements_->statements_.push_back(std::move(stmts->statements_[i]));
    }
  }

  void AnonymousScope::collect_return_types(std::set<Type>* return_exprs) const {
    for (const auto& stmt : statements_->statements_) {
      // TODO When we start having loops/conditionals, this won't cut it. We'll
      // need to dive deeper into the scopes
      if (!stmt->is_return()) continue;

      // Safe because, to pass is_return(), it must be a pointer to a Unop.
      auto unop_ptr = static_cast<Unop*>(stmt.get());
      return_exprs->insert(unop_ptr->expr_->type());
    }
  }

  /* SCOPE */

  IdPtr Scope::get_identifier(const std::string& token_string) {
    Scope* search_scope = this;
    while (search_scope != nullptr) {
      auto iter = search_scope->id_map_.find(token_string);
      if (iter != search_scope->id_map_.end()) {
        return iter->second;
      }
      search_scope = search_scope->parent_;
    }

    std::cerr
      << "Undeclared identifier `" << token_string << "`."
      << std::endl;
    
    // Do I really want to return something in this insatnce?
    std::cout << "***** THIS IS BAD *****" << std::endl;
    return id_map_[token_string] = IdPtr(new Identifier(token_string));
  }


  void Scope::verify_scope() {
    find_all_decls(this);
    verify_no_shadowing();
    join_identifiers(this);

    // Determine types for each declared variable
    // Current working assumption is that a type declaration doesn't depend on
    // any other.
    // TODO: remove that assumption
    for (const auto& kv : decl_registry_) {
      id_map_[kv.first]->expr_type_ = kv.second->interpret_as_type();
    }
  }


  void Scope::show_identifiers() const {
    for (const auto& ids : id_map_) {
      std::cout << ids.first << std::endl;
    }
  }

  void Scope::register_declaration(Declaration* decl) {
    std::string str = decl->identifier();
    auto id_ptr = id_map_.find(str);
    if (id_ptr != id_map_.end()) {
      std::cerr
        << "Identifier already declared in scope: `" << id_ptr->first << "`"
        << std::endl;
    }

    decl_registry_[str] = decl->declared_type();
    id_map_[str] = IdPtr(new Identifier(str));
  }

  void Scope::verify_no_shadowing() {
    if (parent_ == nullptr) return;
    
    for (const auto& id : id_map_) {
      Scope* check_against = parent_;
      while (check_against != nullptr) {
        auto id_ptr = check_against->id_map_.find(id.first);
        if (id_ptr != check_against->id_map_.end()) {
          std::cerr
            << "Identifier shadowed: `" << id.first << "`"
            << std::endl;
        }

        check_against = check_against->parent_;
      }
    }
  }

}  // namespace AST
