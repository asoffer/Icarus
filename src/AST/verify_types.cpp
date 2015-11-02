#include "AST.h"

namespace AST {
  void Unop::verify_types() {
    expr_->verify_types();

    // Even if there was previously a type_error on the return line, we still
    // know that `return foo` should have void type
    //
    // TODO is it safe to check one of the enums instead of strings?
    // TODO print can only take a real right now
    if (is_return() || is_print()) {
      expr_type_ = Type::get_void();
      return;
    }

    if (expr_->expr_type_ == Type::get_type_error()) {
      expr_type_ = Type::get_type_error();
      return;
    }

    if (token_ == "-") {
      if (expr_->expr_type_ == Type::get_uint()) {
        // TODO Warning/Error? signed conversion cast?
        expr_type_ = Type::get_int();

      } else if (expr_->expr_type_ == Type::get_int()) {
        expr_type_ = Type::get_int();

      } else if(expr_->expr_type_ == Type::get_real()) {
        expr_type_ = Type::get_real();

      } else {
        // TODO there are probably more cases here
        expr_type_ = Type::get_type_error();
      }

    } else if (token_ == "!") {
      if (expr_->expr_type_ != Type::get_bool()) {
        expr_type_ = Type::get_type_error();

      } else {
        expr_type_ = Type::get_bool();
      }
    }


    // TODO there are more unary operators to verify.
    // @ - dereferencing
    // & - indirection
    //
  }


  void Binop::verify_types() {
    // FIXME this is ugly, but "worse is better"
    // TODO make this better

    lhs_->verify_types();
    rhs_->verify_types();

    if (lhs_->expr_type_ == Type::get_type_error()
        || rhs_->expr_type_ == Type::get_type_error()) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Type::get_type_error();
      return;
    }

    if (token_ == "=>") {
      if (lhs_->expr_type_ != Type::get_bool())
        expr_type_ = Type::get_type_error();

    } else if (token_ == "()") {
      expr_type_ = Type::get_type_error();
      if (!lhs_->expr_type_->is_function()) {
        std::cerr
          << "Identifier `" << lhs_->token()
          << "` is not a function." << std::endl;
        return;
      }

      Type* in_type = static_cast<Function*>(lhs_->expr_type_)->argument_type();

      if (in_type != rhs_->expr_type_) {
        std::cerr << "Type mismatch on function arguments" << std::endl;
        return;
      }

      expr_type_ = static_cast<Function*>(lhs_->expr_type_)->return_type();
      
      return;

    } else if (token_ == "<" || token_ == ">" || token_ == "<=" ||
        token_ == ">=" || token_ == "==" || token_ == "!=") {
      // TODO is this else-if block necessary anymore??
 
      if (lhs_->expr_type_ != rhs_->expr_type_) {
        // If the types don't match give an error message. We can continue
        // because the result must be a bool
        std::cerr
          << "Type mismatch for comparison operator" << token_ << " ("
          << lhs_->expr_type_->to_string() << " and "
          << rhs_->expr_type_->to_string() << ")" << std::endl;
      }

      expr_type_ = Type::get_bool();

    } else if (lhs_->expr_type_ == rhs_->expr_type_) {
      //Otherwise it's an arithmetic operator
      expr_type_ = lhs_->expr_type_;

    }
    else {
      // TODO give a type-mismatch error here
      expr_type_ = Type::get_type_error();
    }
  }


  void ChainOp::verify_types() {
    std::set<Type*> expr_types;

    for (const auto& expr : exprs_) {
      expr->verify_types();
      expr_types.insert(expr->expr_type_);
    }

    if (expr_types.size() == 1) {
      // FIXME assuming this is only used for booleans currently.
      expr_type_ = Type::get_bool();

    } else {
      // TODO guess what type was intended
      std::cerr
        << "Type error: Values do not match in ChainOp"
        << std::endl;
    }
  }

  void Declaration::verify_types() {
    id_->verify_types();
    decl_type_->verify_types();
    expr_type_ = id_->expr_type_;
  }

  void Terminal::verify_types() {}

  void Identifier::verify_types() {
    // TODO id verify
  }

  void FunctionLiteral::verify_types() {
    for (const auto& kv : inputs_) {
      kv->verify_types();
    }
    statements_->verify_types();

    // FIXME if there are many inputs, we just take the first one. Obviously
    // wrong
    Type* return_type_as_type = return_type_->interpret_as_type();

    expr_type_ = Type::get_function(
        inputs_.front()->type(),
        return_type_as_type);

    std::set<Type*> return_types;
    statements_->collect_return_types(&return_types);

    if (return_type_as_type == Type::get_void()) {
      if (!return_types.empty()) {
        std::cerr << "Function declared void but returns a value." << std::endl;
      }
      return;
    }

    if (return_types.empty()) {
      // If you get here, the return type isn't void so no return statements is
      // an error.
      //
      // TODO better error message. Repalec 'non-void' with some information
      // about the type.
      std::cerr << "Non-void function has no return statement." << std::endl;

    } else if (return_types.size() > 1) {
      std::cerr << "Too many return types" << std::endl;

    } else if (*return_types.begin() != return_type_as_type) {
      std::cerr
        << "Return type does not match function declared return type: "
        << (*return_types.begin())->to_string()
        << " vs. "
        << return_type_as_type->to_string()
        << std::endl;
    }
  }

  void Statements::collect_return_types(std::set<Type*>* return_exprs) const {
    for (const auto& stmt : statements_) {
      // TODO When we start having loops/conditionals, this won't cut it. We'll
      // need to dive deeper into the scopes
      if (!stmt->is_return()) continue;

      // Safe because, to pass is_return(), it must be a pointer to a Unop.
      auto unop_ptr = static_cast<Unop*>(stmt.get());
      return_exprs->insert(unop_ptr->expr_->type());
    }
  }



  void Assignment::verify_types() {
    Binop::verify_types();
    expr_type_ = Type::get_type_error();

    if (lhs_->expr_type_ == Type::get_type_error() ||
        rhs_->expr_type_ == Type::get_type_error()) return;

    if (lhs_->expr_type_ != rhs_->expr_type_) {
      std::cerr
        << "Type mismatch: "
        << lhs_->expr_type_->to_string() << " and "
        << rhs_->expr_type_->to_string() << std::endl;
    }
    expr_type_ = Type::get_void();
  }

  void Case::verify_types() {
    expr_type_ = pairs_->verify_types_with_key(Type::get_bool());
  }

  // Verifies that all keys have the same given type `key_type` and that all
  // values have the same (but unspecified) type.
  Type* KVPairList::verify_types_with_key(Type* key_type) {
    std::set<Type*> value_types;

    for (const auto& kv : kv_pairs_) {
      kv.first->verify_type_is(key_type);
      kv.second->verify_types();

      value_types.insert(kv.second->expr_type_);
    }


    // TODO guess what type was intended
    if (value_types.size() != 1) {
      std::cerr
        << "Type error: Values do not match in key-value pairs" << std::endl;
      return Type::get_type_error();
    }

    // FIXME this paradigm fits really well with Case statements but not
    // KVPairLists so much
    return *value_types.begin();
  }

  void Statements::verify_types() {
    for (auto& eptr : statements_) {
      eptr->verify_types();
    }
  }


  void While::verify_types() {
    cond_->verify_type_is(Type::get_bool());
    statements_->verify_types();
  }
}  // namespace AST
