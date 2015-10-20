#include "AST.h"
/*
namespace AST {
  Scope Scope::Global;

  void Scope::init_global_scope(AST::Statements* stmts) {
    stmts->add_to_scope(&Global);
  }

  void Scope::attach_to_parent(Scope* parent_scope) {
    if (this != parent_scope) {
      parent_ = parent_scope;
      parent_scope->children_.push_back(this);
    }
  }

  void Scope::register_input(Declaration* decl) {
    std::string str = decl->identifier_string();
    auto id_ptr = decl_registry_.find(str);
    if (id_ptr != decl_registry_.end()) {
      std::cerr
        << "Identifier already declared in scope: `" << id_ptr->first << "`"
        << std::endl;
    }

    decl_registry_[str] = decl->declared_type();
    inputs_[str] = IdPtr(new Identifier(str));
  }

  void Scope::register_local(Declaration* decl) {
    std::string str = decl->identifier_string();
    auto id_ptr = decl_registry_.find(str);
    if (id_ptr != decl_registry_.end()) {
      std::cerr
        << "Identifier already declared in scope: `" << id_ptr->first << "`"
        << std::endl;
    }

    // TODO should shadowing be checked here? Seems easy enough.
    // If a you do shadow, we can build a table which shifts the names so we can
    // continue. If they meant to use the old one sometimes and the new one
    // other times, all bets are off. I don't know how to deal with that
    // situation.
    
    decl_registry_[str] = decl->declared_type();
    locals_[str] = IdPtr(new Identifier(str));
  }

  void Scope::verify_no_shadowing() const {
    for (const auto& scope_ptr : children_) {
      scope_ptr->verify_no_shadowing();
    }

    if (parent_ == nullptr) return;

    for (const auto& id : decl_registry_) {

      Scope* check_against = parent_;
      while (check_against != nullptr) {
        auto id_ptr = check_against->decl_registry_.find(id.first);
        if (id_ptr != check_against->decl_registry_.end()) {
          std::cerr
            << "Identifier shadowed: `" << id.first << "`"
            << std::endl;
        }

        check_against = check_against->parent_;
      }
    }
  }

  // TODO Make this iterative instead of recursive
  void Scope::determine_declared_types() {
    for (const auto& scope_ptr : children_) {
      scope_ptr->determine_declared_types();
    }

    // Determine types for each declared variable
    // Current working assumption is that a type declaration doesn't depend on
    // any other.
    // TODO: remove that assumption
    for (auto& kv : inputs_) {
      kv.second->expr_type_ = decl_registry_[kv.first]->interpret_as_type();
    }

    for (auto& kv : locals_) {
      kv.second->expr_type_ = decl_registry_[kv.first]->interpret_as_type();
    }
  }

  IdPtr Scope::get_identifier(const std::string& token_string) {
    Scope* search_scope = this;
    while (search_scope != nullptr) {
      auto iter = search_scope->locals_.find(token_string);
      if (iter != search_scope->locals_.end()) {
        return iter->second;
      }
      // Reusing iter, same type
      iter = search_scope->inputs_.find(token_string);
      if (iter != search_scope->inputs_.end()) {
        return iter->second;
      }
      search_scope = search_scope->parent_;
    }

    std::cerr
      << "Undeclared identifier `" << token_string << "`."
      << std::endl;
    
    // Do I really want to return something in this insatnce?
    std::cout << "***** THIS IS BAD *****" << std::endl;
    return IdPtr(nullptr);
  }

  void Scope::generate_stack_variables(llvm::Function* fn) {
    // TODO declare the correct type (currently always a real)
    for (auto& arg : fn->args()) {
      
      inputs_[arg.getName()]->val_ = builder.CreateAlloca(
          llvm::Type::getDoubleTy(llvm::getGlobalContext()),
          nullptr,
          arg.getName());


      builder.CreateStore(&arg, inputs_[arg.getName()]->val_);

    }

    for (const auto& kv : locals_) {
      kv.second->val_ = builder.CreateAlloca(
          llvm::Type::getDoubleTy(llvm::getGlobalContext()),
          nullptr,
          kv.first.c_str());
    }
  }

}  // namespace AST
*/
