#include "AST.h"
#include "ErrorLog.h"

extern ErrorLog error_log;

namespace AST {
  void Unop::verify_types() {
    // Even if there was previously a type_error on the return line, we still
    // know that `return foo` should have void type
    //
    // TODO is it safe to check one of the enums instead of strings?
    // TODO print can only take a real right now
    if (is_return() || is_print()) {
      expr_type_ = Type::get_void();
      return;

    } else if (token() == "()") {
      if (!expr_->expr_type_->is_function()) {
        expr_type_ = Type::get_type_error();
        return;
      }

      auto fn_type = static_cast<Function*>(expr_->expr_type_);
      expr_type_ = (fn_type->argument_type() == Type::get_void() ?
          fn_type->return_type() : Type::get_type_error());

      return;

    } else if (expr_->expr_type_ == Type::get_type_error()) {
      expr_type_ = Type::get_type_error();
      return;
    }

    if (token() == "-") {
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
    if (lhs_->expr_type_ == Type::get_type_error()
        || rhs_->expr_type_ == Type::get_type_error()) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Type::get_type_error();
      return;
    }


    if (token_ == ":>") {
      expr_type_ = rhs_->interpret_as_type();
      if (lhs_->expr_type_ == expr_type_
          || (lhs_->expr_type_ == Type::get_bool()
            &&
            (expr_type_ == Type::get_int()
             || expr_type_ == Type::get_uint()
             || expr_type_ == Type::get_real()))
          || (lhs_->expr_type_ == Type::get_int() && expr_type_ == Type::get_real())
          || (lhs_->expr_type_ == Type::get_uint() && expr_type_ == Type::get_real())
          ) return;

      error_log.log(line_num_, "Invalid cast from " + lhs_->expr_type_->to_string() + " to " + expr_type_->to_string());
    } else if (token_ == "=>") {
      if (lhs_->expr_type_ != Type::get_bool())
        expr_type_ = Type::get_type_error();

    } else if (token_ == "()") {
      expr_type_ = Type::get_type_error();
      if (!lhs_->expr_type_->is_function()) {
        error_log.log(line_num_, "Identifier `" + lhs_->token() +"` does not name a function.");
        return;
      }

      Type* in_type = static_cast<Function*>(lhs_->expr_type_)->argument_type();

      if (in_type != rhs_->expr_type_) {
        error_log.log(line_num_, "Type mismatch on function arguments.");
        return;
      }

      expr_type_ = static_cast<Function*>(lhs_->expr_type_)->return_type();
      
      return;

   } else if (token_ == "[]") {
      expr_type_ = Type::get_type_error();
      if (!lhs_->expr_type_->is_array()) {
        error_log.log(line_num_, "Identifier `" + lhs_->token() +"` does not name an array.");
        return;
      }


      expr_type_ = static_cast<Array*>(lhs_->expr_type_)->data_type();

      // TODO make this allow uint maybe?
      // TODO allow slice indexing
      if (rhs_->expr_type_ != Type::get_int()) {
        error_log.log(line_num_, "Arary must be indexed by an integer.");
        return;
      }
      
      return;

    } else if (token_ == "<" || token_ == ">" || token_ == "<=" ||
        token_ == ">=" || token_ == "==" || token_ == "!=") {
      // TODO is this else-if block necessary anymore??
 
      if (lhs_->expr_type_ != rhs_->expr_type_) {
        // If the types don't match give an error message. We can continue
        // because the result must be a bool
        error_log.log(line_num_,
            "Type mismatch for comparison operator " + token_ + " ("
            + lhs_->expr_type_->to_string() + " and "
            + rhs_->expr_type_->to_string() + ")");
      }

      expr_type_ = Type::get_bool();

    } else if (lhs_->expr_type_ == rhs_->expr_type_) {
      //Otherwise it's an arithmetic operator
      expr_type_ = lhs_->expr_type_;

    } else if (lhs_->expr_type_ == Type::get_unknown()) {
      // TODO is this reachable?
      error_log.log(line_num_, "Undeclared identifier `" + lhs_->token() + "`");
      expr_type_ = Type::get_type_error();

    } else if (rhs_->expr_type_ == Type::get_unknown()) {
      // TODO is this reachable?
      error_log.log(line_num_, "Undeclared identifier `" + rhs_->token() + "`");
      expr_type_ = Type::get_type_error();

    } else {
      error_log.log(line_num_,
          "Type mismatch: "
          + lhs_->expr_type_->to_string() + " and "
          + rhs_->expr_type_->to_string() + ")");

      // TODO give a type-mismatch error here
      expr_type_ = Type::get_type_error();
    }
  }

  void ArrayType::verify_types() {
    expr_type_ = Type::get_type();

    // TODO implement uint and change this to uint
    if (len_ != nullptr && len_->expr_type_ != Type::get_int()) {
      error_log.log(line_num_, "Array length indexed by non-integral type");
    }

    if (array_type_->expr_type_ != Type::get_type()) {
      error_log.log(line_num_, "Base for array must be a type but " + array_type_->type()->to_string() + " given.");
    }
  }

  void ArrayLiteral::verify_types() {
    auto type_to_match = elems_.front()->expr_type_;

    expr_type_ = Type::get_array(type_to_match);
    for (const auto& el : elems_) {
      if (el->expr_type_ != type_to_match) {
        error_log.log(line_num_, "Type error: Array literal must have consistent type");
        expr_type_ = Type::get_type_error();
        return;
      }
    }
  }

  void ChainOp::verify_types() {
    if (is_comma_list()) {
      std::vector<Type*> type_vec(exprs_.size(), nullptr);

      size_t position = 0;
      for (const auto& eptr : exprs_) {
        type_vec[position] = eptr->type();
        ++position;
      }
      expr_type_ = Type::get_tuple(type_vec);
      return;
    } 

    // All other chain ops need to take arguments of the same type and the
    // expr_type_ is that one type
    std::set<Type*> expr_types;

    for (const auto& expr : exprs_) {
      expr_types.insert(expr->expr_type_);
    }

    if (expr_types.size() == 1) {
      // FIXME assuming this is only used for booleans currently.
      expr_type_ = Type::get_bool();

    } else {
      // TODO guess what type was intended
      error_log.log(line_num_, "Type error: Values do not have matching types in ChainOp");
    }
  }

  void Declaration::verify_types() {

    if (decl_type_->expr_type_ == Type::get_void()) {
      error_log.log(line_num_, "Void types cannot be assigned.");
      return;
    }

    if (decl_type_->is_type_literal() && infer_type_) {
      auto type_lit_ = std::static_pointer_cast<TypeLiteral>(decl_type_);
      Type::make_user_defined(type_lit_->decls_, id_->token());
    }


    id_->expr_type_ = (infer_type_
        ? decl_type_->expr_type_
        : decl_type_->interpret_as_type());

    expr_type_ = id_->expr_type_;
  }

  void Terminal::verify_types() {}

  void Identifier::verify_types() {
    // TODO id verify. Does anything need to be done here?
  }

  void FunctionLiteral::verify_types() {

    // FIXME if there are many inputs, we just take the first one. Obviously
    // wrong
    Type* return_type_as_type = return_type_->interpret_as_type();

    Type* input_type;
    size_t inputs_size = inputs_.size();
    switch (inputs_size) {
      case 0:
        input_type = Type::get_void();
        break;
      case 1:
        input_type = inputs_.front()->type();
        break;
      default:
        {
          std::vector<Type*> input_type_vec(inputs_size, nullptr);
          size_t i = 0;
          for (const auto& input : inputs_) {
            input_type_vec[i] = input->type();
            ++i;
          }

          input_type = Type::get_tuple(input_type_vec);
        }
    }

    expr_type_ = Type::get_function(input_type, return_type_as_type);

    // NOTE: You cannot verify the return type in the body is correct here.
    // The reason is that knowing the type of the function literal doesn't
    // require knowing the internals of the function, and so the body may show
    // up later. You'll have to verify this separately.

    /*
    std::set<Type*> return_types;
    statements_->collect_return_types(&return_types);

    if (return_type_as_type == Type::get_void()) {
      if (!return_types.empty()) {
        error_log.log(line_num_, "Function declared void but returns a value.");
      }
      return;
    }

    if (return_types.empty()) {
      // If you get here, the return type isn't void so no return statements is
      // an error.
      //
      // TODO better error message. Repalec 'non-void' with some information
      // about the type.
      error_log.log(line_num_, "Non-void function has no return statement.");

    } else if (return_types.size() > 1) {
      error_log.log(line_num_, "Too many return types.");

    } else if (*return_types.begin() != return_type_as_type) {
      error_log.log(line_num_, "Return type does not match function declared return type: "
          + (*return_types.begin())->to_string()
          + " vs. "
          + return_type_as_type->to_string());
    }*/
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
    if (lhs_->expr_type_ == Type::get_type_error() ||
        rhs_->expr_type_ == Type::get_type_error()) {
      // An error was already found in the types, so just pass silently

    } else if (rhs_->expr_type_ == Type::get_void()) {
      error_log.log(line_num_, "Void types cannot be assigned.");
      expr_type_ = Type::get_type_error();

    } else if (lhs_->expr_type_ != rhs_->expr_type_) {
      if (lhs_->expr_type_ == Type::get_unknown()) {
        // TODO is this reachable?
        error_log.log(line_num_, "Undeclared identifier `" + lhs_->token() + "`");

      } else if (rhs_->expr_type_ == Type::get_unknown()) {
        // TODO is this reachable?
        error_log.log(line_num_, "Undeclared identifier `" + rhs_->token() + "`");

      } else {
        error_log.log(line_num_, "Type mismatch: "
            + lhs_->expr_type_->to_string() + " and "
            + rhs_->expr_type_->to_string());
      }
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
      if (kv.first->expr_type_ != key_type) {
        // TODO: give some context for this error message. Why must this be the
        // type?  So far the only instance where this is called is for case
        // statements,
        error_log.log(line_num_, "Type of `____` must be "
            + key_type->to_string() + ", but "
            + kv.first->expr_type_->to_string() + " found instead.");
        kv.first->expr_type_ = key_type;
      }

      value_types.insert(kv.second->expr_type_);
    }

    // TODO guess what type was intended
    if (value_types.size() != 1) {
      error_log.log(line_num_, "Type error: Values do not match in key-value pairs");
      return Type::get_type_error();
    }

    // FIXME this paradigm fits really well with Case statements but not
    // KVPairLists so much
    return *value_types.begin();
  }

  void Statements::verify_types() {}

  void While::verify_types() {
    if (cond_->type() != Type::get_bool()) {
      error_log.log(cond_->line_num(), "Type error: Condition in while loop must be a boolean expression, but a type of " + cond_->type()->to_string() + " was found.");
    }
  }

  void Conditional::verify_types() {
    for (const auto& cond : conds_) {
      if (cond->type() != Type::get_bool()) {
        error_log.log(cond->line_num(), "Type error: Condition in if statement must be a boolean expression, but a type of " + cond->type()->to_string() + " was found.");
      }
    }
  }

  void TypeLiteral::verify_types() {
    // TODO
  }
}  // namespace AST
