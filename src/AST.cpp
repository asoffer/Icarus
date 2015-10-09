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

  std::string ChainOp::to_string(size_t n) const {
    std::string output = tabs(n) + "<Chain: ";
    for (const auto& op : ops_) {
      output += op->token() + " ";
    }

    output += ", prec: " + std::to_string(precedence()) + ">\n";

    for (const auto& expr : exprs_) {
      output += expr->to_string(n + 1);
    }

    return output;
  }

  std::string Terminal::to_string(size_t n) const {
    return tabs(n) + "<Terminal (" + expr_type_.to_string() + "): "
      + token_ + ">\n";
  }

  std::string Identifier::to_string(size_t n) const {
    return tabs(n) + "<Identifier (" + expr_type_.to_string() + "): "
      + token() + ">\n";
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


  std::string FunctionLiteral::to_string(size_t n) const {
    std::string output = tabs(n) + "<FunctionLiteral>\n";
    for (const auto& decl : inputs_) {
      output += decl->to_string(n + 1);
    }
    return output + tabs(n + 1) + "Body:\n" + statements_->to_string(n + 2);
  }

  /****************************************
   *           JOIN IDENTIFIERS           *
   ****************************************/

  void Binop::join_identifiers(Scope* scope) {
    if (lhs_->is_identifier()) {
      auto id_ptr = scope->get_identifier(lhs_->token());
      lhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      lhs_->join_identifiers(scope);
    }

    if (rhs_->is_identifier()) {
      auto id_ptr = scope->get_identifier(rhs_->token());
      rhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      rhs_->join_identifiers(scope);
    }
  }


  void ChainOp::join_identifiers(Scope* scope) {
    for (auto& expr : exprs_) {
      if (expr->is_identifier()) {
        auto id_ptr = scope->get_identifier(expr->token());
        expr = std::static_pointer_cast<Expression>(id_ptr);

      } else {
        expr->join_identifiers(scope);
      }
    }
  }

  void AnonymousScope::join_identifiers(Scope* scope) {
    // Do not allow higher scopes to see inside
    if (scope != this) return;

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
        auto id_ptr = scope->get_identifier(eptr->token());
        eptr = std::static_pointer_cast<Expression>(id_ptr);
      }

      eptr->join_identifiers(scope);
    }
  }

  void FunctionLiteral::join_identifiers(Scope* scope) {
    // Do not allow higher scopes to see inside
    if (scope != this) return;

    for (const auto decl : inputs_) {
      decl->join_identifiers(scope);
    }
    statements_->join_identifiers(scope);
  }



   /****************************************
   *            REGISTER SCOPE            *
   ****************************************/
  void Scope::register_scopes(Scope* parent_scope) {
    // TODO is this correct?
    if (this != parent_scope) {
      parent_ = parent_scope;
    }
    scope_registry.push_back(this);
  }

  void Binop::register_scopes(Scope* parent_scope) {
    lhs_->register_scopes(parent_scope);
    rhs_->register_scopes(parent_scope);
  }

  void ChainOp::register_scopes(Scope* parent_scope) {
    for (auto& expr : exprs_) {
      expr->register_scopes(parent_scope);
    }
  }

  void Case::register_scopes(Scope* parent_scope) {
    pairs_->register_scopes(parent_scope);
  }

  void KVPairList::register_scopes(Scope* parent_scope) {
    for (const auto& kv : kv_pairs_) {
      kv.first->register_scopes(parent_scope);
      kv.second->register_scopes(parent_scope);
    }
  }

  void AnonymousScope::register_scopes(Scope* parent_scope) {
    Scope::register_scopes(parent_scope);
    statements_->register_scopes(this);
  }

  
  void Statements::register_scopes(Scope* parent_scope) {
    for (const auto& stmt : statements_) {
      stmt->register_scopes(parent_scope);
    }
  }

  void FunctionLiteral::register_scopes(Scope* parent_scope) {
    // This calls Scope::register_scope
    AnonymousScope::register_scopes(parent_scope);

    // FIXME This is almost certainly unnecessary as scopes probably cannot be
    // contained inside a declaration.
    for (const auto& decl : inputs_) {
      decl->register_scopes(this);
    }

  }


  /****************************************
   *            FIND ALL DECLS            *
   ****************************************/

  void Binop::find_all_decls(Scope* scope) {
    lhs_->find_all_decls(scope);
    rhs_->find_all_decls(scope);
  }

  void ChainOp::find_all_decls(Scope* scope) {
    for (auto& expr : exprs_) {
      expr->find_all_decls(scope);
    }
  }

  void AnonymousScope::find_all_decls(Scope* scope) {
    statements_->find_all_decls(scope);
  } 

  void FunctionLiteral::find_all_decls(Scope* scope) {
    // Do not allow higher scopes to see inside
    if (scope != this) return;

    for (const auto & decl : inputs_) {
      decl->find_all_decls(scope);
    }

    // Call parent
    AnonymousScope::find_all_decls(scope);
  } 

  void Declaration::find_all_decls(Scope* scope) {
    scope->register_declaration(this);
  }

  void Statements::find_all_decls(Scope* scope) {
    for (auto& eptr : statements_) {
      eptr->find_all_decls(scope);
    }
  }

  void Case::find_all_decls(Scope*) { // TODO
  }

  /****************************************
   *           INTERPRET AS TYPE          *
   ****************************************/

  Type Binop::interpret_as_type() const {
    if (token() == "->") {
      return Type::Function(
          lhs_->interpret_as_type(),
          rhs_->interpret_as_type());
    }

    // TODO more cases here probably

    return Type::TypeError;
  }


  Type ChainOp::interpret_as_type() const {
    return Type::TypeError;
  }


  Type Terminal::interpret_as_type() const {
    if (expr_type_ == Type::Type_) {

      if (token() == "bool") return Type::Bool;
      if (token() == "char") return Type::Char;
      if (token() == "int") return Type::Int;
      if (token() == "real") return Type::Real;
      if (token() == "string") return Type::String;
      if (token() == "type") return Type::Type_;
      if (token() == "uint") return Type::UInt;
      if (token() == "void") return Type::Void;

      std::cerr
        << "I don't think " << token()
        << " is a type!" << std::endl;

      return Type::TypeError;
    }

    std::cerr << token() + " is not a type!" << std::endl;

    return Type::TypeError;
  }

  Type AnonymousScope::interpret_as_type() const {
    throw "Stub, this shouldn't be possible";
    return Type::TypeError;
  }

  Type FunctionLiteral::interpret_as_type() const {
    throw "Stub, this shouldn't be possible";
    return Type::TypeError;
  }

  Type Case::interpret_as_type() const {
    throw "Stub, this shouldn't be possible";
    return Type::TypeError;
  }


  /****************************************
   *             VERIFY TYPES             *
   ****************************************/
 
  void Binop::verify_types() {
    // FIXME this is ugly, but "worse is better"
    // TODO make this better

    lhs_->verify_types();
    rhs_->verify_types();

    if (lhs_->expr_type_ == Type::TypeError
        || rhs_->expr_type_ == Type::TypeError) {
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

    } else if (token_ == "()") {
      expr_type_ = Type::TypeError;
      if (!lhs_->expr_type_.is_function()) {
        std::cerr
          << "Identifier `" << token_
          << "` is not a function." << std::endl;
        return;
      }

      Type in_type = lhs_->expr_type_.input_type();

      if (in_type != rhs_->expr_type_) {
        std::cerr << "Type mismatch on function arguments" << std::endl;
        return;
      }

      expr_type_ = lhs_->expr_type_.return_type();
      
      return;

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


  void ChainOp::verify_types() {
    std::set<Type> expr_types;

    for (const auto& expr : exprs_) {
      expr_types.insert(expr->expr_type_);
    }

    if (expr_types.size() == 1) {
      // FIXME assuming this is only used for booleans currently.
      expr_type_ = Type::Bool;
    }

    // TODO guess what type was intended
    std::cerr
      << "Type error: Values do not match in ChainOp"
      << std::endl;
  }

  void Declaration::verify_types() {
    lhs_->verify_types();
    rhs_->verify_types();
    expr_type_ = lhs_->expr_type_;
  }

  void Terminal::verify_types() {}

  void Identifier::verify_types() {
    // TODO id verify
  }

  void AnonymousScope::verify_types() {
    statements_->verify_types();
  }

  void FunctionLiteral::verify_types() {
    for (const auto& decl : inputs_) {
      decl->verify_types();
    }
    AnonymousScope::verify_types();

    // FIXME if there are many inputs, we just take the first one. Obviously
    // wrong
    expr_type_ = Type::Function(
        inputs_.front()->expr_type_,
        return_type_->interpret_as_type());
  }

  void Assignment::verify_types() {
    Binop::verify_types();
    expr_type_ = Type::TypeError;

    if (lhs_->expr_type_ == Type::TypeError) return;
    if (rhs_->expr_type_ == Type::TypeError) return;

    if (lhs_->expr_type_ != rhs_->expr_type_) {
      std::cerr
        << "Type mismatch:"
        << lhs_->expr_type_.to_string() << " and"
        << rhs_->expr_type_.to_string() << std::endl;
    }
    expr_type_ = Type::Void;
  }

  void Case::verify_types() {
    pairs_->verify_types_with_key(Type::Bool);
  }

  // Verifies that all keys have the same given type `key_type` and that all
  // values have the same (but unspecified) type.
  Type KVPairList::verify_types_with_key(Type key_type) {
    std::set<Type> value_types;

    for (const auto& kv : kv_pairs_) {
      kv.first->verify_type_is(key_type);
      kv.second->verify_types();

      value_types.insert(kv.second->expr_type_);
    }


    // TODO guess what type was intended
    if (value_types.size() != 1) {
      std::cerr
        << "Type error: Values do not match in key-value pairs" << std::endl;
      return Type::TypeError;
    }

    // FIXME this paradigm fits really well with Case statements but not
    // KVPairLists so much
    return Type::Unknown;//*value_types.begin();
  }

  void Statements::verify_types() {
    for (auto& eptr : statements_) {
      eptr->verify_types();
    }
  }



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

  /* SCOPE */
  std::vector<Scope*> Scope::scope_registry;

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
