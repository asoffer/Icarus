#include "AST.h"

namespace AST {
  /****************************************
   *       PRINTING AND TO_STRING()       *
   ****************************************/

  std::string tabs(size_t n) {
    // Tabs are two spaces
    return std::string(n << 1, ' ');
  }

  std::string Node::to_string(size_t n) const {
    std::string output =
      tabs(n) + "[" + Language::show_name.at(type_);

    if (!token_.empty())
      output += ": " + token_;

    return output + "]\n";
  }

  std::string Binop::to_string(size_t n) const {
    std::string output = 
      tabs(n) + "<Binop (" + expr_type_.to_string() + "): '"
      + (token_ == "" ? Language::show_name.at(type_) : token_)
      + "', prec: " + std::to_string(precedence_) + ">\n";

    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }

  std::string Terminal::to_string(size_t n) const {
    return tabs(n) + "<Terminal (" + expr_type_.to_string() + "): " + token_ + ">\n";
  }

  std::string AnonymousScope::to_string(size_t n) const {
    return tabs(n) + "<AnonymousScope>\n" + statements_->to_string(n + 1);
  }

  std::string Declaration::to_string(size_t n) const {
    return tabs(n)
      + "<Declaration (" + expr_type_.to_string() + ")>\n"
      + lhs_->to_string(n + 1)
      + rhs_->to_string(n + 1);
  }

  std::string Assignment::to_string(size_t n) const {
    return tabs(n)
      + "<Assignment (" + expr_type_.to_string() + ")>\n"
      + lhs_->to_string(n + 1)
      + rhs_->to_string(n + 1);
  }

  std::string Case::to_string(size_t n) const {
    return tabs(n) + "<Case>\n" + pairs_->to_string(n + 1);
  }

  std::string KVPairList::to_string(size_t n) const {
    std::string indent = tabs(n);
    std::string output;

    for (const auto& kv : kv_pairs_) {
      output += indent + "[=>]\n";
      output += kv.first->to_string(n + 1);
      output += kv.second->to_string(n + 1);
    }

    return output;
  }

  std::string Statements::to_string(size_t n) const {
    std::string output = tabs(n) + "<Statements>\n";

    for (const auto& exprs : statements_) {
      output += exprs->to_string(n + 1);
    }

    return output;
  }



  /****************************************
   *           JOIN IDENTIFIERS           *
   ****************************************/

  void Binop::join_identifiers(Scope* scope) {
    if (lhs_->is_identifier()) {
      auto id_ptr = scope->identifier(lhs_->token());
      lhs_ = std::static_pointer_cast<Expression>(id_ptr);
    } else {
      lhs_->join_identifiers(scope);
    }

    if (rhs_->is_identifier()) {
      auto id_ptr = scope->identifier(rhs_->token());
      rhs_ = std::static_pointer_cast<Expression>(id_ptr);
    } else {
      rhs_->join_identifiers(scope);
    }
  }

  void AnonymousScope::join_identifiers(Scope* scope) {
    statements_->join_identifiers(scope);
  }

  void Case::join_identifiers(Scope* scope) {
    pairs_->join_identifiers(scope);
  }

  void KVPairList::join_identifiers(Scope* scope) {
    for (const auto& pair : kv_pairs_) {
      pair.first->join_identifiers(scope);
      pair.second->join_identifiers(scope);
    }
  }

  void Statements::join_identifiers(Scope* scope) {
    for (auto& eptr : statements_) {
      if (eptr->is_identifier()) {
        auto id_ptr = scope->identifier(eptr->token());
        eptr = std::static_pointer_cast<Expression>(id_ptr);
      }

      eptr->join_identifiers(scope);
    }
  }



  /****************************************
   *            FIND ALL DECLS            *
   ****************************************/

  void Binop::find_all_decls(Scope* scope) {
    lhs_->find_all_decls(scope);
    rhs_->find_all_decls(scope);
  }

  void AnonymousScope::find_all_decls(Scope* scope) {
    statements_->find_all_decls(scope);
  } 

  void Declaration::find_all_decls(Scope* scope) {
    scope->register_declaration(this);
  }

  void Statements::find_all_decls(Scope* scope) {
    for (auto& eptr : statements_) {
      eptr->find_all_decls(scope);
    }
  }



  /* BINOP */
  void Binop::verify_types() {
    // FIXME this is ugly, but "worse is better"
    // TODO make this better

    lhs_->verify_types();
    rhs_->verify_types();

    if (lhs_->expr_type_ == Type::TypeError || rhs_->expr_type_ == Type::TypeError) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Type::TypeError;
      return;
    }

    if (token_ == "=>") {
      if (lhs_->expr_type_ != Type::Bool)
        expr_type_ = Type::TypeError;

    } else if (token_ == ":>") {
      // TODO verify that this cast is possible
      expr_type_ = Type::Literals.at(rhs_->token());

    } else if (token_ == "<" || token_ == ">" || token_ == "<=" ||
        token_ == ">=" || token_ == "==" || token_ == "!=") {
      if (lhs_->expr_type_ != rhs_->expr_type_) {
        // If the types don't match give an error message. We can continue
        // because the result must be a bool
        std::cerr
          << "Type mismatch for comparison operator" << token_ << " ("
          << lhs_->expr_type_.to_string() << " and "
          << rhs_->expr_type_.to_string() << ")" << std::endl;
      }

      expr_type_ = Type::Bool;

    } else if (lhs_->expr_type_ == rhs_->expr_type_) {
      //Otherwise it's an arithmetic operator
      expr_type_ = lhs_->expr_type_;

    }
    else {
      // TODO give a type-mismatch error here
      expr_type_ = Type::TypeError;
    }
  }


  /* TERMINAL */
  void Terminal::verify_types() {
  }


  /* ANONYMOUS SCOPE */
  void AnonymousScope::add_statements(NPtr&& stmts_ptr) {
    auto stmts = std::unique_ptr<Statements>(static_cast<Statements*>(stmts_ptr.release()));
    statements_->statements_.reserve( statements_->size() + stmts->size() );

    for (size_t i = 0; i < stmts->size(); ++i) {
      statements_->statements_.push_back(std::move(stmts->statements_[i]));
    }
  }

  void AnonymousScope::verify_types() {
    statements_->verify_types();
  }
  /* DECLARATION */
  void Declaration::verify_types() {
    expr_type_ = lhs_->expr_type_;
  }


  /* ASSIGNMENT */
  void Assignment::verify_types() {
    Binop::verify_types();

    if (lhs_->expr_type_ == Type::TypeError) return;
    if (rhs_->expr_type_ == Type::TypeError) return;

    if (lhs_->expr_type_ != rhs_->expr_type_) {
      std::cout << "!!!" << std::endl;
      // TODO Give some error about assignment type-mismatch
    }
    expr_type_ = Type::Void;
  }

  /* CASE */
  void Case::verify_types() {
    pairs_->verify_types_with_key(Type::Bool);
  }

  /* KVPAIRLIST */

  // Verifies that all keys have the same given type `key_type` and that all
  // values have the same (but unspecified type.
  Type KVPairList::verify_types_with_key(Type key_type) {
    std::set<Type> value_types;

    for (const auto& kv : kv_pairs_) {
      kv.first->verify_type_is(key_type);
      kv.second->verify_types();

      //value_types.insert(kv->verify_value_type());
    }


    // TODO guess what type was intended
    if (value_types.size() != 1) {
      std::cout << "Type error: Value do not match in key-value pairs" << std::endl;
      return Type::TypeError;
    }

    // FIXME this paradigm fits really well with Case statements but not
    // KVPairLists so much
    return Type::Unknown;//*value_types.begin();
  }

  /* SCOPE */
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

  /* STATEMENTS */
  void Statements::verify_types() {
    for (auto& eptr : statements_) {
      eptr->verify_types();
    }
  }

}  // namespace AST
