#include "AST.h"

namespace AST {
  void Unop::verify_types() {
    expr_->verify_types();

    // Even if there was previously a type_error on the return line, we still
    // know that `return foo` should have void type
    if (token_ == "return") {
      expr_type_ = Type::Void;
      return;
    }

    if (expr_->expr_type_ == Type::TypeError) {
      expr_type_ = Type::TypeError;
      return;
    }

    if (token_ == "-") {
      if (expr_->expr_type_ == Type::UInt) {
        // TODO Warning/Error? signed conversion cast?
        expr_type_ = Type::Int;

      } else if (expr_->expr_type_ == Type::Int) {
        expr_type_ = Type::Int;

      } else if(expr_->expr_type_ == Type::Real) {
        expr_type_ = Type::Real;

      } else {
        // TODO there are probably more cases here
        expr_type_ = Type::TypeError;
      }

    } else if (token_ == "!") {
      if (expr_->expr_type_ != Type::Bool) {
        expr_type_ = Type::TypeError;

      } else {
        expr_type_ = Type::Bool;
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
      // TODO is this else-if block necessary anymore??
 
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
    Type return_type_as_type = return_type_->interpret_as_type();

    expr_type_ = Type::Function(
        inputs_.front()->expr_type_,
        return_type_as_type);

    std::set<Type> return_types;
    collect_return_types(&return_types);

    if (return_type_as_type == Type::Void) {
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
        << "Return type does not match function declared return type"
        << std::endl;
    }
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

}  // namespace AST
