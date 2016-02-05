#include "AST.h"
#include "ErrorLog.h"
#include "Type.h"

extern ErrorLog error_log;

namespace AST {
  Type* operator_lookup(size_t line_num, Language::Operator op, Type* lhs_type, Type* rhs_type) {
    auto ret_type = TypeSystem::get_operator(op, Tup({ lhs_type, rhs_type }));
    if (ret_type == nullptr) {
      std::string tok;
      switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc) \
        case Language::Operator::name: tok = #symbol; break;
#include "config/operator.conf"
#undef OPERATOR_MACRO
      }

      error_log.log(line_num, "No known operator overload for `" + tok + "` with types " + lhs_type->to_string() + " and " + rhs_type->to_string());
      return Error;
    } else {
      //Otherwise it's an arithmetic operator
      return ret_type;
    }
  }

  void Terminal::verify_types() {
    using Language::Terminal;
    switch (terminal_type_) {
      case Terminal::ASCII:         expr_type_ = Func(Uint, Char);  break;
      case Terminal::Return:        expr_type_ = Void;              break;
      case Terminal::True:          expr_type_ = Bool;              break;
      case Terminal::False:         expr_type_ = Bool;              break;
      case Terminal::Char:          expr_type_ = Char;              break;
      case Terminal::Int:           expr_type_ = Int;               break;
      case Terminal::Real:          expr_type_ = Real;              break;
      case Terminal::Type:          expr_type_ = Type_;             break;
      case Terminal::UInt:          expr_type_ = Uint;              break;
      case Terminal::StringLiteral: expr_type_ = String;            break;
    }
  }

  void Identifier::verify_types() {
    auto decl_ptr = Scope::decl_of_[shared_from_this()];
    if (decl_ptr->type_is_inferred()) {
      expr_type_ = decl_ptr->type();

      if (expr_type_ == Type_) {
        Scope::Global->context().bind(Context::Value(
              decl_ptr->declared_type()->evaluate(Scope::Global->context()).as_type),
            shared_from_this());
        assert(Scope::Global->context().get(shared_from_this()).as_type
            && "Bound type was a nullptr");
      }
    } else {
      expr_type_ = decl_ptr->declared_type()->evaluate(Scope::Global->context()).as_type;

    }
    assert(expr_type_ && "Expression type is nullptr in Identifier::verify_types()");
  }
  void Unop::verify_types() {
    using Language::Operator;
    if (op_ == Operator::Print) {
      if (expr_->type() == Void) {
        error_log.log(line_num(), "Void types cannot be printed");
      }
      expr_type_ = Void;
    
    } else if (op_ == Operator::And) {
      // TODO disallow pointers to goofy things (address of rvalue, e.g.)
      expr_type_ = (expr_->type() == Type_
          ? static_cast<Type*>(Type_)
          : static_cast<Type*>(Ptr(expr_->type())));

    } else if (op_ == Operator::Sub) {
      if (expr_->type() == Uint) {
        error_log.log(line_num(), "Negation applied to unsigned integer");
        expr_type_ = Uint;

      } else if (expr_->type() == Int) {
        expr_type_ = Int;

      } else if(expr_->type() == Real) {
        expr_type_ = Real;

      } else {
        error_log.log(line_num(), type()->to_string() + " has no negation operator.");
        expr_type_ = Error;
      }
    } else {
      assert(false && "Died in Unop::verify_types");
    }
    return;
  }

  void Binop::verify_types() {
    using Language::Operator;
    if (lhs_->type() == Error || rhs_->type() == Error) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Error;
      return;
    }

    if (op_ == Operator::Access) {
      if (!rhs_->is_identifier()) {
        error_log.log(line_num(), "Member access (`.`) must access an identifier.");
        expr_type_ = Error;
        return;
      }

      // Access passes through pointers
      auto lhs_type = lhs_->type();
      while (lhs_type->is_pointer()) {
        lhs_type = static_cast<Pointer*>(lhs_type)->pointee_type();
      }

      if (lhs_type->is_struct()) {
        auto struct_type = static_cast<Structure*>(lhs_type);
        auto member_type = struct_type->field(rhs_->token());
        if (member_type == nullptr) {
          error_log.log(line_num(),
              "Objects of type " + lhs_type->to_string() + " has no member named `" + rhs_->token() + "`.");
        } else {
          rhs_->expr_type_ = member_type;
          expr_type_ = member_type;
        }
      }
      assert(expr_type_ && "expr_type_ is nullptr in binop access");
      return;
    }

    assert(false && "Died in Binop::verify_types");
  }

  void ChainOp::verify_types() {
    assert(false && "Died in ChainOp::verify_types");
  }

  void Declaration::verify_types() {
    if (decl_type_->type() == Void) {
      expr_type_ = Error;
      error_log.log(line_num(), "Void types cannot be assigned.");
      return;
    }

    expr_type_ = (type_is_inferred()
        ? decl_type_->type()
        : decl_type_->evaluate(Scope::Global->context()).as_type);
    return;
  }
 
  void ArrayType::verify_types() {
    assert(false && "Died in ArrayType::verify_types");
  }

  void ArrayLiteral::verify_types() {
    assert(false && "Died in ArrayLiteral::verify_types");
  }

  void FunctionLiteral::verify_types() {
    Type* ret_type = return_type_->evaluate(Scope::Global->context()).as_type;
    assert(ret_type && "Return type is a nullptr");
    Type* input_type;
    size_t inputs_size = inputs_.size();
    if (inputs_size == 0) {
      input_type = Void;

    } else if (inputs_size == 1) {
      input_type = inputs_.front()->type();

    } else {
      std::vector<Type*> input_type_vec;
      for (const auto& input : inputs_) {
        input_type_vec.push_back(input->type());
      }

      input_type = Tup(input_type_vec);
    }
    expr_type_ = Func(input_type, ret_type);
    assert(expr_type_ && "FunctionLiteral type is nullptr");
  }

  void Assignment::verify_types() {
    if (lhs_->type() == Error || rhs_->type() == Error) {
      expr_type_ = Error;
      return;
    }

    if (op_ == Language::Operator::Assign) {
      if (lhs_->type() != rhs_->type()) {
        error_log.log(line_num(), "Invalid assignment. Left-hand side has type " + lhs_->type()->to_string() + ", but right-hand side has type " + rhs_->type()->to_string());
      }
      expr_type_ = Void;
      return;
    }

    // expr_type_ = operator_lookup(line_num(), op_, lhs_->type(), rhs_->type());

    assert(false && "Died in Assignment::verify_types");
  }

  void Case::verify_types() {
    assert(false && "Died in Case::verify_types");
  }

  void KVPairList::verify_types() {
    assert(false && "Died in KVPairList::verify_types");
  }
  void Statements::verify_types() {
    assert(false && "Died in Statements::verify_types");
  }
  void While::verify_types() {
    assert(false && "Died in While::verify_types");
  }
  void Conditional::verify_types() {
    assert(false && "Died in Conditional::verify_types");
  }

  void EnumLiteral::verify_types() {
    assert(false && "Died in EnumLiteral::verify_types");
  }

  void TypeLiteral::verify_types() {
    expr_type_ = Type_;
    type_value_ = Struct("__anon.type"); 
  }

  Type* KVPairList::verify_types_with_key(Type* key_type) {
    assert(false && "Died in KVPairList::verify_types_with_key");
  }
/*
  void Unop::verify_types() {
    // Even if there was previously a type_error on the return line, we still
    // know that `return foo` should have void type
    //
    // TODO is it safe to check one of the enums instead of strings?
    if (op_ == Language::Operator::At) {
      if (expr_->type()->is_pointer()) {
        expr_type_ = static_cast<Pointer*>(expr_->type())->pointee_type();

      } else {
        error_log.log(line_num(), "Dereferencing object of type " + expr_->type()->to_string() + ", which is not a pointer.");
      }

    } else if (op_ == Language::Operator::Print) {
      expr_type_ = Void;

    } else if (op_ == Language::Operator::Return) {
      expr_type_ = Void;

    } else if (op_ == Language::Operator::And) { // Indirection '&'
      // TODO disallow pointers to goofy things (address of rvalue, e.g.)
      expr_type_ = (expr_->type() == Type_
          ? static_cast<Type*>(Type_)
          : static_cast<Type*>(Ptr(expr_->type())));
      return;

    } else if (op_ == Language::Operator::Call) {
      if (!expr_->type()->is_function()) {
        expr_type_ = Error;
        return;
      }

      auto fn_type = static_cast<Function*>(expr_->type());
      if (fn_type->argument_type() == Void) {
        // TODO multiple return values. For now just taking one
        expr_type_ = fn_type->return_type();

      } else {
        error_log.log(line_num(), "Call to a function of type "
            + fn_type->to_string() + " with no arguments provided.");
        expr_type_ = Error;
      }
      return;

    } else if (expr_->type() == Error) {

      expr_type_ = Error;
      return;

    } else if (op_ == Language::Operator::Sub) {

      if (expr_->type() == Uint) {
        error_log.log(line_num(), "Negation applied to unsigned integer");
        expr_type_ = Uint;

      } else if (expr_->type() == Int) {
        expr_type_ = Int;

      } else if(expr_->type() == Real) {
        expr_type_ = Real;

      } else {
        error_log.log(line_num(), type()->to_string() + " has no negation operator.");
        expr_type_ = Error;
      }

    } else if (op_ == Language::Operator::Not) {
      if (expr_->type() != Bool) {
        expr_type_ = Error;

      } else {
        expr_type_ = Bool;
      }
    }
  }


  void Binop::verify_types() {
    if (lhs_->type() == Error || rhs_->type() == Error) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Error;
      return;
    }

    // TODO casting check should be part of the type interface
    if (op_ == Language::Operator::Cast) {
      expr_type_ = rhs_->interpret_as_type();
      if (lhs_->type() == type()
          || (lhs_->type() == Bool
            && (type() == Int || type() == Uint || type() == Real))
          || (lhs_->type() == Int && type() == Real)
          || (lhs_->type() == Int && type() == Uint)
          || (lhs_->type() == Uint && type() == Real)
          || (lhs_->type() == Uint && type() == Int)
          ) return;

      error_log.log(line_num(), "Invalid cast from " + lhs_->type()->to_string() + " to " + type()->to_string());

    } else if (op_ == Language::Operator::Access) {
      if (!rhs_->is_identifier()) {
        error_log.log(line_num(), "Member access (`.`) must access an identifier.");
      }

      auto lhs_type = lhs_->type();
      while (lhs_type->is_pointer()) {
        lhs_type = static_cast<Pointer*>(lhs_type)->pointee_type();
      }

      if (lhs_type->is_struct()) {
        auto struct_type = static_cast<Structure*>(lhs_type);
        // TODO TOKENREMOVAL
        // rhs_ must be and identifier in this case
        auto member_type = struct_type->field(rhs_->token());
        if (member_type == nullptr) {
          // TODO better error message
          // TODO TOKENREMOVAL
          // rhs_ must be and identifier in this case
          error_log.log(line_num(),
              "Objects of type " + lhs_type->to_string() + " has no member named `" + rhs_->token() + "`.");
        } else {
          rhs_->expr_type_ = member_type;
          expr_type_ = member_type;
        }

      } else if (lhs_->interpret_as_type()->is_enum()) {
        // TODO verify that the rhs is actually a member
        expr_type_ = lhs_->interpret_as_type();
        rhs_->expr_type_ = expr_type_;

      } else {
        // TODO better error message
        error_log.log(line_num(),
            "Objects of type " + lhs_type->to_string() + " have no members.");
      }
      return;

    } else if (op_ == Language::Operator::Rocket) {
      if (lhs_->type() != Bool) {
        expr_type_ = Error;
      }

    } else if (op_ == Language::Operator::Call) {
      expr_type_ = Error;
      if (!lhs_->type()->is_function()) {
        // TODO TOKENREMOVAL
        // TODO lhs might not have a precise token
        error_log.log(line_num(), "Identifier `" + lhs_->token() +"` does not name a function.");
        return;
      }

      auto in_types = static_cast<Function*>(lhs_->type())->argument_type();

      // TODO If rhs is a comma-list, is it's type given by a tuple?
      if (in_types != rhs_->type()) {
        // TODO segfault happenning here because rhs_ is not totally initialized always.
        error_log.log(line_num(), "Type mismatch on function arguments.");
        return;
      }

      // TODO multiple return values. For now just taking the first
      expr_type_ = static_cast<Function*>(lhs_->type())->return_type();
      
      return;

   } else if (op_ == Language::Operator::Index) {
      expr_type_ = Error;
      if (!lhs_->type()->is_array()) {
        // TODO TOKENREMOVAL
        // TODO lhs might not have a precise token
        error_log.log(line_num(), "Identifier `" + lhs_->token() +"` does not name an array.");
        return;
      }


      expr_type_ = static_cast<Array*>(lhs_->type())->data_type();

      // TODO make this allow uint maybe?
      // TODO allow slice indexing
      if (rhs_->type() != Int) {
        error_log.log(line_num(), "Arary must be indexed by an integer.");
        return;
      }
      
      return;

    } else {
      expr_type_ = operator_lookup(line_num(), op_, lhs_->type(), rhs_->type());
    }
  }

  void ArrayType::verify_types() {
    // TODO implement uint and change this to uint
    if (len_ != nullptr && len_->type() != Int) {
      error_log.log(line_num(), "Array length indexed by non-integral type");
    }

    if (array_type_->type() == Type_) {
      expr_type_ = Type_;
    } else {
      expr_type_ = Arr(array_type_->type());
    }
//    if (array_type_->type() != Type_) {
//      error_log.log(line_num(), "Base for array must be a type but " + array_type_->type()->to_string() + " given.");
//    }
  }

  void ArrayLiteral::verify_types() {
    auto type_to_match = elems_.front()->type();

    expr_type_ = Arr(type_to_match);
    for (const auto& el : elems_) {
      if (el->type() != type_to_match) {
        error_log.log(line_num(), "Type error: Array literal must have consistent type");
        expr_type_ = Error;
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
      expr_type_ = Tup(type_vec);
      return;
    } 

    // All other chain ops need to take arguments of the same type and the
    // expr_type_ is that one type
    std::set<Type*> expr_types;

    for (const auto& expr : exprs_) {
      expr_types.insert(expr->type());
    }

    if (expr_types.size() == 1) {
      // TODO must it always be bool?
      expr_type_ = Bool;

    } else {
      // TODO guess what type was intended
      error_log.log(line_num(), "Type error: Values do not have matching types in ChainOp");
    }
  }

  void Declaration::verify_types() {
    if (decl_type_->type() == Void) {
      error_log.log(line_num(), "Void types cannot be assigned.");
      return;
    }

    if (decl_type_->is_type_literal()) {
      auto type_lit = std::static_pointer_cast<TypeLiteral>(decl_type_);
      type_lit->type_value_ = 
        Struct(infer_type_ ? identifier_string() : "__anon.type", type_lit->decls_); 

    } else if (decl_type_->is_enum_literal()) {
      auto enum_lit = std::static_pointer_cast<EnumLiteral>(decl_type_);
      enum_lit->type_value_ =
        Enum(infer_type_ ? identifier_string() : "__anon.enum", enum_lit.get());
    }

    // TODO get the scope context
    expr_type_ = (infer_type_ ?
        decl_type_->type() : decl_type_->evaluate(Scope::Global->context()).as_type);

    // TODO unnecessary, I think
    // id_->expr_type_ = expr_type_;

    if (infer_type_) {
      // TODO for now all functions are bound in the global context. This is
      // probably incorrect. However, it may be safe anyways, because we've
      // already verified access.
      Scope::Global->context().bind(Context::Value(decl_type_.get()), id_);
    }
  }

  void Terminal::verify_types() {
    if (terminal_type_ == Language::Terminal::StringLiteral) {
      expr_type_ = String;
    }
  }

  void Identifier::verify_types() {
    auto decl_ptr = Scope::decl_of_[shared_from_this()];
    // TODO use the right context
    if (decl_ptr->type_is_inferred()) {
      expr_type_ = decl_ptr->type();

    } else {
      // TODO use the correct context
      expr_type_ = decl_ptr->declared_type()->evaluate(Scope::Global->context()).as_type;
    }

    if (expr_type_ == Type_) {
      // TODO Bind it in the correct context
      // For now, bind it in the global context
      Scope::Global->context().bind(
          decl_ptr->declared_type()->evaluate(Scope::Global->context()),
          shared_from_this());
    }
  }

  void FunctionLiteral::verify_types() {
    Type* return_type_as_type = return_type_->interpret_as_type();

    Type* input_type;
    size_t inputs_size = inputs_.size();
    switch (inputs_size) {
      case 0:
        input_type = Void;
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

          input_type = Tup(input_type_vec);
        }
    }

    expr_type_ = Func(input_type, return_type_as_type);
  }

  void Assignment::verify_types() {
    if (lhs_->type() == Error || rhs_->type() == Error) {
      expr_type_ = Error;
      return;
    }

    // TODO put this in the lookup with a type matching system
    if (op_ == Language::Operator::Assign) {
      if (lhs_->type() != rhs_->type()) {
        error_log.log(line_num(), "Invalid assignment. Left-hand side has type " + lhs_->type()->to_string() + ", but right-hand side has type " + rhs_->type()->to_string());
      }
      expr_type_ = Void;
      return;
    }

    expr_type_ = operator_lookup(line_num(), op_, lhs_->type(), rhs_->type());
  }

  void Case::verify_types() {
    expr_type_ = pairs_->verify_types_with_key(Bool);
  }

  void KVPairList::verify_types() {}

  // Verifies that all keys have the same given type `key_type` and that all
  // values have the same (but unspecified) type.
  Type* KVPairList::verify_types_with_key(Type* key_type) {
    std::set<Type*> value_types;

    for (const auto& kv : kv_pairs_) {
      if (kv.first->type() != key_type) {
        // TODO: give some context for this error message. Why must this be the
        // type?  So far the only instance where this is called is for case
        // statements,
        error_log.log(line_num(), "Type of `____` must be "
            + key_type->to_string() + ", but "
            + kv.first->type()->to_string() + " found instead.");
        kv.first->expr_type_ = key_type;
      }

      value_types.insert(kv.second->type());
    }

    // TODO guess what type was intended
    if (value_types.size() != 1) {
      error_log.log(line_num(), "Type error: Values do not match in key-value pairs");
      return Error;
    }

    // FIXME this paradigm fits really well with Case statements but not
    // KVPairLists so much
    return *value_types.begin();
  }

  void Statements::verify_types() {}

  void While::verify_types() {
    if (cond_->type() != Bool) {
      error_log.log(cond_->line_num(), "Type error: Condition in while loop must be a boolean expression, but a type of " + cond_->type()->to_string() + " was found.");
    }
  }

  void Conditional::verify_types() {
    for (const auto& cond : conds_) {
      if (cond->type() != Bool) {
        error_log.log(cond->line_num(), "Type error: Condition in if statement must be a boolean expression, but a type of " + cond->type()->to_string() + " was found.");
      }
    }
  }

  void TypeLiteral::verify_types() {
    // TODO
  }

  void EnumLiteral::verify_types() {
    // TODO
  }
*/
}  // namespace AST
