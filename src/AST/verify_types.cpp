#include "AST.h"
#include "ErrorLog.h"
#include "Type.h"
#include "DependencySystem.h"
#include <sstream>

extern ErrorLog error_log;

namespace AST {
TypePtr operator_lookup(size_t line_num, Language::Operator op, TypePtr lhs_type,
                      TypePtr rhs_type) {
  // TODO move this into the get_operator function
  if (lhs_type.is_function() && rhs_type.is_function()) {
    auto lhs_fn = static_cast<Function *>(lhs_type.get);
    auto rhs_fn = static_cast<Function *>(rhs_type.get);
    if (rhs_fn->output == lhs_fn->input) {
      return Func(rhs_fn->input, lhs_fn->output);
    }
  }

  auto ret_type = TypeSystem::get_operator(op, Tup({lhs_type, rhs_type}));
  if (ret_type.get == nullptr) {
    std::string tok;
    switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  case Language::Operator::name:                                               \
    tok = #symbol;                                                             \
    break;
#include "config/operator.conf"
#undef OPERATOR_MACRO
    }

    error_log.log(line_num, "No known operator overload for `" + tok +
                                "` with types " + lhs_type.to_string() +
                                " and " + rhs_type.to_string());
    return Error;
  } else {
    // Otherwise it's an arithmetic operator
    return ret_type;
  }
}

void Terminal::verify_types() {
  using Language::Terminal;
  switch (terminal_type) {
  case Terminal::ASCII: type         = Func(Uint, Char); break;
  case Terminal::Return: type        = Void; break;
  case Terminal::Else: type          = Bool; break;
  case Terminal::True: type          = Bool; break;
  case Terminal::False: type         = Bool; break;
  case Terminal::Char: type          = Char; break;
  case Terminal::Int: type           = Int; break;
  case Terminal::Real: type          = Real; break;
  case Terminal::Type: type          = Type_; break;
  case Terminal::UInt: type          = Uint; break;
  case Terminal::StringLiteral: type = String; break;
  case Terminal::Alloc: /* Already set */ break;
  case Terminal::Null: /* Already set */ break;
  }
}

void Identifier::verify_types() {
  // We have already checked in Declaration::verify_types that either there is
  // one type, or there are functions
  if (decls.size() == 1) {
    type = decls[0]->type;
    assert(type && "type is null");
  } else {
    std::vector<TypePtr> vec;
    for (auto decl : decls) {
      vec.push_back(decl->type);
    }

    type = new QuantumType(vec);
  }

  for (const auto decl : decls) {
    switch (decl->decl_type) {
    case DeclType::Std: {
      if (type == Type_) {
        scope_->context.bind(Context::Value(TypeVar(this)), this);
      }
    } break;

    case DeclType::Infer: {
      if (decl->type_expr->is_struct_literal()) {
        //      auto tlit_type_val =
        //          static_cast<StructLiteral *>(decl->type_expr)->type_value;
        //      scope_->context.bind(Context::Value(tlit_type_val), this);
      } else if (decl->type_expr->is_function_literal()) {
        auto flit = static_cast<FunctionLiteral *>(decl->type_expr);
        scope_->context.bind(Context::Value(flit), this);
      }
    } break;
    case DeclType::In: {
      // TODO does anything need to happen here?
    } break;
    case DeclType::Tick: {
      // TODO do we need to do anything here?
    } break;
    }
  }

  assert(type && "Expression type is nullptr in Identifier::verify_types()");
}

void Unop::verify_types() {
  using Language::Operator;
  switch (op) {
  case Operator::Free: {
    if (!operand->type.is_pointer()) {
      error_log.log(line_num, "Free can only be called on pointer types");
    }
    type = Void;
  } break;
  case Operator::Print: {
    if (operand->type == Void) {
      error_log.log(line_num, "Void types cannot be printed");
    }
    type = Void;
  } break;
  case Operator::Return: {
    if (operand->type == Void) {
      error_log.log(line_num, "Void types cannot be returned");
    }

    type = Void;
  } break;
  case Operator::At: {
    if (operand->type.is_pointer()) {
      type = static_cast<Pointer *>(operand->type.get)->pointee;

    } else {
      error_log.log(line_num, "Dereferencing object of type " +
                                  operand->type.to_string() +
                                  ", which is not a pointer.");
      type = Error;
    }
  } break;
  case Operator::Call: {
    if (!operand->type.is_function()) {
      error_log.log(line_num,
                    "Identifier `" + operand->token() + "` is not a function.");
      type = Error;
      return;
    }

    auto fn = static_cast<Function *>(operand->type.get);
    if (fn->input != Void) {
      error_log.log(line_num, "Calling function `" + operand->token() +
                                  "` with no arguments.");
      type = Error;
    } else {
      type = fn->output;
      assert(type && "fn return type is nullptr");
    }
  } break;
  case Operator::And: {
    // TODO disallow pointers to goofy things (address of rvalue, e.g.)
    if (operand->type == Type_)
      type = Type_;
    else
      type = Ptr(operand->type);
    assert(type && "&type is null");
  } break;
  case Operator::Sub: {
    if (operand->type == Uint) {
      error_log.log(line_num, "Negation applied to unsigned integer");
      type = Int;

    } else if (operand->type == Int) {
      type = Int;

    } else if (operand->type == Real) {
      type = Real;

    } else {
      error_log.log(line_num, type.to_string() + " has no negation operator.");
      type = Error;
    }
  } break;
  case Operator::Dots: {
    if (operand->type == Uint || operand->type == Int || operand->type == Char) {
      type = Range(operand->type);
    } else {
      error_log.log(line_num, type.to_string() + " cannot be part of a range");
      type = Error;
    }
  } break;
  case Operator::Not: {
    if (operand->type == Bool) {
      type = Bool;
    } else {
      error_log.log(
          line_num,
          "The logical negation operator `!` only applies to boolean values");
      type = Error;
    }
  } break;
  default: assert(false && "Died in Unop::verify_types");
  }
}

void Access::verify_types() {
  if (operand->type == Error) {
    // An error was already found in the types, so just pass silently
    type = Error;
    return;
  }

  auto etype = operand->type;

  if (etype == Type_) {
    Dependency::traverse_from(Dependency::PtrWithTorV(operand, false));
    auto etypename = operand->evaluate(scope_->context).as_type;
    if (etypename->is_enum()) {
      auto enum_type = static_cast<Enumeration *>(etypename);
      // If you can get the value,
      if (enum_type->get_value(member_name)) {
        type = operand->evaluate(scope_->context).as_type;

      } else {
        error_log.log(line_num, etypename->to_string() + " has no member " +
                                    member_name + ".");
        type = Error;
      }
      return;
    }
  }

  // Access passes through pointers
  while (etype.is_pointer()) {
    etype = static_cast<Pointer *>(etype.get)->pointee;
  }

  if (etype.is_array() && member_name == "size") {
    type = Uint;
    return;
  }

  if (etype.is_struct()) {
    assert(static_cast<Structure *>(etype.get)->field_type.size());

    auto member_type = static_cast<Structure *>(etype.get)->field(member_name);
    if (member_type) {
      type = member_type;

    } else {
      error_log.log(line_num, "Objects of type " + etype.to_string() +
                                  " have no member named `" + member_name +
                                  "`.");
      type = Error;
    }
  }

  assert(type && "type is nullptr in access");
  return;
}

void Binop::verify_types() {
  using Language::Operator;
  if (lhs->type == Error || rhs->type == Error) {
    // An error was already found in the types, so just pass silently
    type = Error;
    return;
  }

  switch (op) {
  case Operator::Rocket: {
    if (lhs->type != Bool) {
      error_log.log(line_num, "LHS of rocket must be a bool");
      type = Error;
    }
    return;
  }
  case Operator::Call: {
    type = Error;

    if (lhs->type.is_dependent_type()) {
      // TODO treat dependent types as functions
      auto dep_type    = static_cast<DependentType *>(lhs->type.get);
      auto result_type = (*dep_type)(rhs->evaluate(scope_->context).as_type);
      type             = result_type;
      return;
    }

    if (lhs->type == Type_) {
      // TODO (remove this assumption)
      //
      // Assume you are not a dufus, and if you're passing a struct to be
      // "called" that it's a parametric struct. (So we use .as_expr).
      //
      // auto lhs_as_expr = lhs->evaluate(scope_->context).as_expr;

      // If it isn't parametric, terribleness will ensue.
      type = Type_;
      return;
    }

    if (lhs->type.is_quantum()) {
      // In the binary operator (), store the index for which quantum type value
      // you're actually calling.
      for (auto opt : static_cast<QuantumType *>(lhs->type.get)->options) {
        auto fn_type = static_cast<Function *>(opt.get);
        if (fn_type->input != rhs->type) continue;

        type = fn_type->output;
        return;
      }
      error_log.log(line_num, "Could not match function type");
      return;
    } else if (!lhs->type.is_function()) {
      // TODO TOKENREMOVAL
      // TODO lhs might not have a precise token
      error_log.log(line_num, "Identifier `" + lhs->token() +
                                  "` does not name a function.");
      return;
    }

    auto in_types = static_cast<Function *>(lhs->type.get)->input;

    // TODO Check if it takes any type variables at all.
    if (in_types.is_type_variable()) {
      std::cout << "I need to test a function with the input " << rhs->type
                << std::endl
                << "Then I need to clone the function " << *lhs << std::endl;
    }

    // TODO If rhs is a comma-list, is it's type given by a tuple?
    if (in_types != rhs->type) {
      error_log.log(line_num, "Type mismatch on function arguments.");
      return;
    }

    // TODO multiple return values. For now just taking the first
    type = static_cast<Function *>(lhs->type.get)->output;
    assert(type && "return type is null");

    return;
  }
  case Operator::Index: {
    type = Error;
    if (!lhs->type.is_array()) {
      // TODO TOKENREMOVAL
      // TODO lhs might not have a precise token
      error_log.log(line_num, "Identifier `" + lhs->token() +
                                  "` does not name an array.");
      return;
    }

    type = static_cast<Array *>(lhs->type.get)->data_type;
    assert(type && "array data type is nullptr");
    // TODO allow slice indexing
    if (rhs->type != Int && rhs->type != Uint) {
      error_log.log(line_num,
                    "Arary must be indexed by an int or uint. You supplied a " +
                        rhs->type.to_string());
      return;
    }

    return;
  }
  case Operator::Cast: {
    // TODO use correct scope
    type = rhs->evaluate(scope_->context).as_type;
    if (type == Error) return;
    assert(type && "cast to nullptr?");

    if (lhs->type == type ||
        (lhs->type == Bool && (type == Int || type == Uint || type == Real)) ||
        (lhs->type == Int && type == Real) ||
        (lhs->type == Int && type == Uint) ||
        (lhs->type == Uint && type == Real) ||
        (lhs->type == Uint && type == Int)) {
      return;
    }

    error_log.log(line_num, "Invalid cast from " + lhs->type.to_string() +
                                " to " + type.to_string());
    return;
  }
  case Operator::Dots: {
    if (lhs->type == Int && rhs->type == Int) {
      type = Range(Int);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      type = Range(Uint);

    } else if (lhs->type == Char && rhs->type == Char) {
      type = Range(Char);

    } else {
      error_log.log(line_num, "No known range construction for types" +
                                  lhs->type.to_string() + " .. " +
                                  rhs->type.to_string());
    }
    return;
  }
  default: {
    type = operator_lookup(line_num, op, lhs->type, rhs->type);
    assert(type && "operator_lookup yields nullptr");
    return;
  }
  }

  assert(false && "Died in Binop::verify_types");
}

void ChainOp::verify_types() {
  if (is_comma_list()) {
    std::vector<TypePtr> type_vec(exprs.size(), nullptr);

    size_t position = 0;
    for (const auto &eptr : exprs) {
      type_vec[position] = eptr->type;
      ++position;
    }
    type = Tup(type_vec);
    assert(type && "tuple yields nullptr");
    return;
  }

  // All other chain ops need to take arguments of the same type and the
  // type is that one type
  std::set<TypePtr> expr_types;

  for (const auto &expr : exprs) { expr_types.insert(expr->type); }

  if (expr_types.size() == 1) {
    // TODO must it always be bool?
    type = Bool;

  } else {

    // TODO guess what type was intended
    std::stringstream ss;
    ss << "Type error: Types do not all match. Found the following types:\n";
    for (const auto &t : expr_types) { ss << "\t" << *t.get << "\n"; }

    error_log.log(line_num, ss.str());
    type = Error;
  }
}

void Declaration::verify_types() {
  if (type_expr->type == Void) {
    type = Error;
    error_log.log(line_num, "Void types cannot be assigned.");
    return;
  }

  switch (decl_type) {
  case DeclType::Std: {
    type = type_expr->evaluate(scope_->context).as_type;
  } break;
  case DeclType::Infer: {
    type = type_expr->type;

    // TODO if it's compile-time
    if (type == Type_) {
      if (type_expr->is_struct_literal()) {
        auto type_expr_as_struct = static_cast<StructLiteral *>(type_expr);
        assert(type_expr_as_struct->type_value);
        scope_->context.bind(Context::Value(type_expr_as_struct->type_value),
                             identifier);
      }
    }
  } break;
  case DeclType::In: {
    if (type_expr->type.is_array()) {
      type = static_cast<Array *>(type_expr->type.get)->data_type;

    } else if (type_expr->type.is_range()) {
      type = static_cast<RangeType *>(type_expr->type.get)->end_type;

    } else if (type_expr->type == Type_) {
      auto t = type_expr->evaluate(scope_->context).as_type;
      if (t->is_enum()) { type = t; }

    } else {
      error_log.log(line_num, "Cannot determine type from in declaration.");
      type = Error;
    }
  } break;
  case DeclType::Tick: {
    if (!type_expr->type.is_function()) {
      // TODO Need a way better
      error_log.log(
          line_num,
          "Cannot generate a type where the tester is not a function");
      type = Error;
      return;
    }

    auto test_func = static_cast<Function *>(type_expr->type.get);
    if (test_func->output != Bool) {
      // TODO What about implicitly cast-able to bool via a user-defined cast?
      error_log.log(line_num, "Test function must return a bool");
      type = Error;
      return;
    }

    type = Type_;

    // TODO can't continue with type verification immediately

  } break;
  }

  // TODO if RHS is not a type give a nice message instead of segfaulting

  if (type_expr->is_terminal()) {
    auto term = static_cast<Terminal *>(type_expr);
    if (term->terminal_type == Language::Terminal::Null) {
      error_log.log(line_num, "Cannot infer the type of `null`.");
    }
  }

  assert(type && "decl expr is nullptr");
}

void ArrayType::verify_types() {
  // TODO change this to uint
  if (length != nullptr && length->type != Int) {
    error_log.log(line_num, "Array length indexed by non-integral type");
  }

  if (data_type->type == Type_) {
    type = Type_;
  } else {
    type = Arr(data_type->type);
    assert(type && "arrayType nullptr");
  }
}

void ArrayLiteral::verify_types() {
  auto type_to_match = elems.front()->type;
  assert(type_to_match && "type to match is nullptr");
  if (type_to_match == Error) {
    type = Error;
    return;
  }

  type = Arr(type_to_match);
  for (const auto &el : elems) {
    if (el->type != type_to_match) {
      error_log.log(line_num,
                    "Type error: Array literal must have consistent type");
      type = Error;
    }
  }
}

void FunctionLiteral::verify_types() {
  TypePtr ret_type = return_type_expr->evaluate(scope_->context).as_type;
  assert(ret_type && "Return type is a nullptr");
  TypePtr input_type;
  size_t inputssize = inputs.size();
  if (inputssize == 0) {
    input_type = Void;

  } else if (inputssize == 1) {
    input_type = inputs.front()->type;

  } else {
    std::vector<TypePtr> input_type_vec;
    for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

    input_type = Tup(input_type_vec);
  }

  // TODO what if just one argument is a type variable?
  if (input_type.is_type_variable()) {
    type = DepType([](TypePtr t){ Ptr(t) });
  } else {
    type = Func(input_type, ret_type);
  }

  assert(type && "FunctionLiteral type is nullptr");
}

void Assignment::verify_types() {
  if (lhs->type == Error || rhs->type == Error) {
    type = Error;
    return;
  }

  // TODO if lhs is reserved?
  if (op == Language::Operator::Assign) {
    if (rhs->is_terminal()) {
      auto term = static_cast<Terminal *>(rhs);
      if (term->terminal_type == Language::Terminal::Null) {
        term->type = lhs->type;
        type       = Void;
        return;
      }
    }

    if (lhs->type != rhs->type) {
      error_log.log(line_num, "Invalid assignment. Left-hand side has type " +
                                  lhs->type.to_string() +
                                  ", but right-hand side has type " +
                                  rhs->type.to_string());
    }
    type = Void;
  } else {
    type = operator_lookup(line_num, op, lhs->type, rhs->type);
    assert(type && "operator_lookup");
  }
}

void Case::verify_types() {
  type = kv->verify_types_with_key(Bool);
  assert(type && "case is nullptr");
}

void KVPairList::verify_types() {
  assert(false && "Died in KVPairList::verify_types");
}

void Statements::verify_types() {
  // TODO Verify that a return statement, if present, is the last thing
}

void While::verify_types() {
  if (condition->type != Bool) {
    error_log.log(line_num, "While loop condition must be a bool, but " +
                                condition->type.to_string() + " given.");
  }
}

void For::verify_types() {
  /*
                              */
}

void Conditional::verify_types() {
  for (const auto &cond : conditions) {
    if (cond->type != Bool) {
      error_log.log(line_num, "Conditional must be a bool, but " +
                                  cond->type.to_string() + " given.");
    }
  }
}

void EnumLiteral::verify_types() {
  static size_t anon_enum_counter = 0;

  type       = Type_;
  type_value = Enum("__anon.enum" + std::to_string(anon_enum_counter), this);
  ++anon_enum_counter;
}

void StructLiteral::verify_types() {
  static size_t anon_struct_counter = 0;
  type                              = Type_;

  if (!type_value) {
    if (params.empty()) {
      type_value =
          Struct("__anon.struct" + std::to_string(anon_struct_counter), this);
    } else {
      type_value = ParamStruct(
          "__anon.param.struct" + std::to_string(anon_struct_counter), this);
    }
    ++anon_struct_counter;
  }
}

// Verifies that all keys have the same given type `key_type` and that all
// values have the same (but unspecified) type.
TypePtr KVPairList::verify_types_with_key(TypePtr key_type) {
  std::set<TypePtr> value_types;

  for (const auto &kv : pairs) {
    if (kv.first->type != key_type) {
      // TODO: give some context for this error message. Why must this be the
      // type?  So far the only instance where this is called is for case
      // statements,
      error_log.log(line_num, "Type of `____` must be " +
                                  key_type.to_string() + ", but " +
                                  kv.first->type.to_string() +
                                  " found instead.");
      kv.first->type = key_type;
      assert(kv.first->type && "keytype");
    }

    value_types.insert(kv.second->type);
  }

  // TODO guess what type was intended
  if (value_types.size() != 1) {
    error_log.log(line_num,
                  "Type error: Values do not match in key-value pairs");
    return Error;
  }

  // FIXME this paradigm fits really well with Case statements but not
  // KVPairLists so much
  return *value_types.begin();
}

void Jump::verify_types() {
  auto scope_ptr = scope_;
  while (scope_ptr) {
    assert(scope_ptr->is_block_scope());
    auto block_scope_ptr = static_cast<BlockScope *>(scope_ptr);
    if (block_scope_ptr->type == ScopeType::Function) {
      if (jump_type != JumpType::Return) {
        error_log.log(line_num, "statement must be contained inside a loop.");
      }
      return;
    }

    if (block_scope_ptr->is_loop_scope()) {
      scope = block_scope_ptr;
      return;
    }

    scope_ptr = block_scope_ptr->parent;
  }

  assert(false && "How did you get to here?");


}

void DummyTypeExpr::verify_types() {}
} // namespace AST
