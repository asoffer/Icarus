#include "AST.h"
#include "ErrorLog.h"
#include "Type.h"
#include "DependencySystem.h"
#include <sstream>

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
      case Terminal::Else:          expr_type_ = Bool;              break;
      case Terminal::True:          expr_type_ = Bool;              break;
      case Terminal::False:         expr_type_ = Bool;              break;
      case Terminal::Char:          expr_type_ = Char;              break;
      case Terminal::Int:           expr_type_ = Int;               break;
      case Terminal::Real:          expr_type_ = Real;              break;
      case Terminal::Type:          expr_type_ = Type_;             break;
      case Terminal::UInt:          expr_type_ = Uint;              break;
      case Terminal::StringLiteral: expr_type_ = String;            break;
      case Terminal::Alloc:         /* Already set */               break;
      case Terminal::Null:          /* Already set */               break;
    }
  }

  void Identifier::verify_types() {
    if (decl_->type_is_inferred()) {
      expr_type_ = decl_->type();

      if (decl_->declared_type()->is_type_literal()) {
        auto tlit_type_val =
          static_cast<TypeLiteral*>(decl_->declared_type().get())->type_value_;
        scope_->context().bind(Context::Value(tlit_type_val), shared_from_this());

      } else if (decl_->declared_type()->is_function_literal()) {
        auto flit = static_cast<FunctionLiteral*>(decl_->declared_type().get());
        scope_->context().bind(Context::Value(flit), shared_from_this());
      }
      assert(expr_type_ && "decl_->type() is nullptr");

    } else {
      expr_type_ = decl_->declared_type()->evaluate(scope_->context()).as_type;
      assert(expr_type_ && "eval with context operandptr is nullptr");
      if (expr_type_ == Type_) {
        scope_->context().bind(
            Context::Value(TypeVar(shared_from_this())), shared_from_this());
      }
    }
    assert(expr_type_ && "Expression type is nullptr in Identifier::verify_types()");
  }

  void Unop::verify_types() {
    using Language::Operator;
    if (op == Operator::Free) {
      if (!operand->type()->is_pointer()) {
        error_log.log(line_num, "Free can only be called on pointer types");
      }
      expr_type_ = Void;
 
    } else if (op == Operator::Print) {
      if (operand->type() == Void) {
        error_log.log(line_num, "Void types cannot be printed");
      }
      expr_type_ = Void;

    } else if (op == Operator::Return) {
      if (operand->type() == Void) {
        error_log.log(line_num, "Void types cannot be returned");
      }
 
      expr_type_ = Void;

    } else if (op == Operator::At) {
      if (operand->type()->is_pointer()) {
        expr_type_ = static_cast<Pointer*>(operand->type())->pointee_type();

      } else {
        error_log.log(line_num, "Dereferencing object of type "
            + operand->type()->to_string() + ", which is not a pointer.");
        expr_type_ = Error;
      }

    } else if (op == Operator::Call) {
      if (!operand->type()->is_function()) {
        error_log.log(line_num,
            "Identifier `" + operand->token() + "` is not a function.");
        expr_type_ = Error;
        return;
      }

      auto fn = static_cast<Function*>(operand->type());
      if (fn->argument_type() != Void) {
        error_log.log(line_num,
            "Calling function `" + operand->token() + "` with no arguments.");
        expr_type_ = Error;
      } else {
        expr_type_ = fn->return_type();
        assert(expr_type_ && "fn return type is nullptr");
      }

    } else if (op == Operator::And) {
      // TODO disallow pointers to goofy things (address of rvalue, e.g.)
      if (operand->type() == Type_) expr_type_ = Type_;
      else                        expr_type_ = Ptr(operand->type());
      assert(expr_type_ && "&expr_type_ is null");

    } else if (op == Operator::Sub) {
      if (operand->type() == Uint) {
        error_log.log(line_num, "Negation applied to unsigned integer");
        expr_type_ = Int;

      } else if (operand->type() == Int) {
        expr_type_ = Int;

      } else if(operand->type() == Real) {
        expr_type_ = Real;

      } else {
        error_log.log(line_num, type()->to_string() + " has no negation operator.");
        expr_type_ = Error;
      }
    } else {
      assert(false && "Died in Unop::verify_types");
    }
  }

  void Access::verify_types() {
    if (operand->type() == Error) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Error;
      return;
    }
    auto etype = operand->type();
    if (etype == Type_) {
      Dependency::traverse_from(Dependency::PtrWithTorV(operand.get(), false));
      auto etypename = operand->evaluate(scope_->context()).as_type;
      if (etypename->is_enum()) {
        auto enum_type = static_cast<Enumeration*>(etypename);
        // If you can get the value,
        if (enum_type->get_value(member_name_)) {
          // TODO use correct context
          expr_type_ = operand->evaluate(scope_->context()).as_type;

        } else {
          error_log.log(line_num, etypename->to_string() + " has no member " + member_name_ + ".");
          expr_type_ = Error;
        }
        return;
      }
    }

    // Access passes through pointers
    while (etype->is_pointer()) {
      etype = static_cast<Pointer*>(etype)->pointee_type();
    }

    if (etype->is_struct()) {
      auto struct_type = static_cast<Structure*>(etype);
      auto member_type = struct_type->field(member_name_);
      if (member_type) {
        expr_type_ = member_type;
      } else {
        error_log.log(line_num, "Objects of type " + etype->to_string()
            + " have no member named `" + member_name_ + "`.");
        expr_type_ = Error;
      }
    }

    assert(expr_type_ && "expr_type_ is nullptr in access");
    return;
  }

  void Binop::verify_types() {
    using Language::Operator;
    if (lhs_->type() == Error || rhs_->type() == Error) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Error;
      return;
    }

    if (op == Language::Operator::Rocket) {
      if (lhs_->type() != Bool) {
        error_log.log(line_num, "LHS of rocket must be a bool");
        expr_type_ = Error;
      }
      return;

    } else if (op == Language::Operator::Call) {
      expr_type_ = Error;

      if (lhs_->type()->is_dependent_type()) {
        // TODO treat dependent types as functions
        auto dep_type = static_cast<DependentType*>(lhs_->type());
        auto result_type =
          (*dep_type)(rhs_->evaluate(scope_->context()).as_type);
        expr_type_ = result_type;
        return;
      }


      if (!lhs_->type()->is_function()) {
        // TODO TOKENREMOVAL
        // TODO lhs might not have a precise token
        error_log.log(line_num, "Identifier `" + lhs_->token() +"` does not name a function.");
        return;
      }

      auto in_types = static_cast<Function*>(lhs_->type())->argument_type();

      // TODO If rhs is a comma-list, is it's type given by a tuple?
      if (in_types != rhs_->type()) {
        // TODO segfault happenning here because rhs_ is not totally initialized always.
        error_log.log(line_num, "Type mismatch on function arguments.");
        return;
      }

      // TODO multiple return values. For now just taking the first
      expr_type_ = static_cast<Function*>(lhs_->type())->return_type();
      assert(expr_type_ && "return type is null");

      return;

    } else if (op == Language::Operator::Index) {
      expr_type_ = Error;
      if (!lhs_->type()->is_array()) {
        // TODO TOKENREMOVAL
        // TODO lhs might not have a precise token
        error_log.log(line_num, "Identifier `" + lhs_->token() + "` does not name an array.");
        return;
      }


      expr_type_ = static_cast<Array*>(lhs_->type())->data_type();
      assert(expr_type_ && "array data type is nullptr");
      // TODO allow slice indexing
      if (rhs_->type() != Int && rhs_->type() != Uint) {
        error_log.log(line_num, "Arary must be indexed by an int or uint. You supplied a " + rhs_->type()->to_string());
        return;
      }

      return;
    } else if (op == Language::Operator::Cast) {
      // TODO use correct scope
      expr_type_ = rhs_->evaluate(scope_->context()).as_type;
      if (expr_type_ == Error) return;
      assert(expr_type_ && "cast to nullptr?");

      if (lhs_->type() == type()
          || (lhs_->type() == Bool
            && (type() == Int || type() == Uint || type() == Real))
          || (lhs_->type() == Int && type() == Real)
          || (lhs_->type() == Int && type() == Uint)
          || (lhs_->type() == Uint && type() == Real)
          || (lhs_->type() == Uint && type() == Int)
         ) return;

      error_log.log(line_num,
          "Invalid cast from " + lhs_->type()->to_string()
          + " to " + type()->to_string());

    } else {
      expr_type_ = operator_lookup(line_num, op, lhs_->type(), rhs_->type());
      assert(expr_type_ && "operator_lookup yields nullptr");
      return;
    }

    assert(false && "Died in Binop::verify_types");
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
      assert(expr_type_ && "tuple yields nullptr");
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
      std::stringstream ss;
      ss << "Type error: Types do not all match. Found the following types:\n";
      for (const auto& t : expr_types) {
        ss << "\t" << *t << "\n";
      }

      error_log.log(line_num, ss.str());
      expr_type_ = Error;
    }
  }

  void Declaration::verify_types() {
    if (decl_type_->type() == Void) {
      expr_type_ = Error;
      error_log.log(line_num, "Void types cannot be assigned.");
      return;
    }

    expr_type_ = (type_is_inferred()
        ? decl_type_->type()
        : decl_type_->evaluate(scope_->context()).as_type);

    // TODO if RHS is not a type give a nice message instead of segfaulting

    if (decl_type_->is_terminal()) {
      auto term = std::static_pointer_cast<Terminal>(decl_type_);
      if (term->terminal_type_ == Language::Terminal::Null) {
        error_log.log(line_num, "Cannot infer the type of `null`.");
      }
    }

    assert(expr_type_ && "decl expr is nullptr");
  }

  void ArrayType::verify_types() {
    // TODO change this to uint
    if (len_ != nullptr && len_->type() != Int) {
      error_log.log(line_num, "Array length indexed by non-integral type");
    }

    if (array_type_->type() == Type_) {
      expr_type_ = Type_;
    } else {
      expr_type_ = Arr(array_type_->type());
      assert(expr_type_ && "arrayType nullptr");
    }
  }

  void ArrayLiteral::verify_types() {
    auto type_to_match = elems_.front()->type();
    assert(type_to_match && "type to match is nullptr");
    if (type_to_match == Error) {
      expr_type_ = Error;
      return;
    }

    expr_type_ = Arr(type_to_match);
    for (const auto& el : elems_) {
      if (el->type() != type_to_match) {
        error_log.log(line_num, "Type error: Array literal must have consistent type");
        expr_type_ = Error;
      }
    }
  }

  void FunctionLiteral::verify_types() {
    Type* ret_type = return_type_->evaluate(scope_->context()).as_type;
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

    // TODO if lhs is reserved?
    if (op == Language::Operator::Assign) {
      if (rhs_->is_terminal()) {
        auto term = std::static_pointer_cast<Terminal>(rhs_);
        if (term->terminal_type_ == Language::Terminal::Null) {
          term->expr_type_ = lhs_->type();
          expr_type_ = Void;
          return;
        }
      }

      if (lhs_->type() != rhs_->type()) {
        error_log.log(line_num, "Invalid assignment. Left-hand side has type " + lhs_->type()->to_string() + ", but right-hand side has type " + rhs_->type()->to_string());
      }
      expr_type_ = Void;
    } else {
      expr_type_ = operator_lookup(line_num, op, lhs_->type(), rhs_->type());
      assert(expr_type_ && "operator_lookup");
    }
  }

  void Case::verify_types() {
    expr_type_ = pairs_->verify_types_with_key(Bool);
    assert(expr_type_ && "case is nullptr");
  }

  void KVPairList::verify_types() {
    assert(false && "Died in KVPairList::verify_types");
  }

  void Statements::verify_types() {
    // TODO Verify that a return statement, if present, is the last thing
  }

  void While::verify_types() {
    if (cond_->type() != Bool) {
      error_log.log(line_num, "While loop condition must be a bool, but "
          + cond_->type()->to_string() + " given.");
    }
  }

  void Conditional::verify_types() {
    for (const auto& cond : conds_) {
      if (cond->type() != Bool) {
        error_log.log(line_num, "Conditional must be a bool, but "
            + cond->type()->to_string() + " given.");
      }
    }
  }

  void EnumLiteral::verify_types() {
    static size_t anon_enum_counter = 0;
    expr_type_ = Type_;
    type_value_ = Enum("__anon.enum" + std::to_string(anon_enum_counter), this);
    ++anon_enum_counter;
  }

  void TypeLiteral::verify_types() {
    static size_t anon_type_counter = 0;
    expr_type_ = Type_;
    type_value_ = Struct("__anon.struct" + std::to_string(anon_type_counter), this); 
    ++anon_type_counter;
  }

  // Verifies that all keys have the same given type `key_type` and that all
  // values have the same (but unspecified) type.
  Type* KVPairList::verify_types_with_key(Type* key_type) {
    std::set<Type*> value_types;

    for (const auto& kv : kv_pairs_) {
      if (kv.first->type() != key_type) {
        // TODO: give some context for this error message. Why must this be the
        // type?  So far the only instance where this is called is for case
        // statements,
        error_log.log(line_num, "Type of `____` must be "
            + key_type->to_string() + ", but "
            + kv.first->type()->to_string() + " found instead.");
        kv.first->expr_type_ = key_type;
        assert(kv.first->expr_type_ && "keytype");
      }

      value_types.insert(kv.second->type());
    }

    // TODO guess what type was intended
    if (value_types.size() != 1) {
      error_log.log(line_num, "Type error: Values do not match in key-value pairs");
      return Error;
    }

    // FIXME this paradigm fits really well with Case statements but not
    // KVPairLists so much
    return *value_types.begin();
  }
}  // namespace AST
