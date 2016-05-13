#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

extern Type *GetFunctionTypeReferencedIn(Scope *scope,
                                         const std::string &fn_name,
                                         Type *input_type);

extern AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr);

AST::FunctionLiteral *GenerateSpecifiedFunction(AST::FunctionLiteral *fn_lit,
                                                TypeVariable *input_type,
                                                Type *fn_type) {
  auto lookup_key = new TypeVariable *[1];
  lookup_key[0]   = input_type;
  auto lookup_val = new Type *[1];
  lookup_val[0]   = fn_type;

  auto cloned_func =
      (AST::FunctionLiteral *)fn_lit->clone(1, lookup_key, lookup_val);

  auto old_stack_size = Scope::Stack.size();
  Scope::Stack.push(fn_lit->scope_);

  cloned_func->assign_scope();
  cloned_func->join_identifiers();
  Dependency::record(cloned_func);
  Dependency::rebuild_already_seen();

  Scope::Stack.pop();
  assert(Scope::Stack.size() == old_stack_size);

  delete[] lookup_key;
  delete[] lookup_val;

  return cloned_func;
}

Type *CallResolutionMatch(Type *lhs_type, AST::Expression *lhs,
                          AST::Expression *rhs) {
  // TODO log information about failures to match?
  //
  // lhs_type cannot be quantum because we first break apart quantum types at
  // the calling site of this function.
  assert(!lhs_type->is_quantum());

  if (lhs_type->is_function()) {
    auto in_types = static_cast<Function *>(lhs_type)->input;

    // TODO Check if it takes any type variables at all.
    if (in_types->has_vars) {
      auto test_func = static_cast<TypeVariable *>(in_types)->test;

      bool success;
      {
        auto call_binop = new AST::Binop();
        call_binop->op  = Language::Operator::Call;
        call_binop->lhs = test_func;
        auto dummy      = new AST::DummyTypeExpr(rhs->loc, rhs->type);
        call_binop->rhs = dummy;

        success = call_binop->evaluate(lhs->scope_->context).as_bool;

        dummy->type_value = nullptr;
        delete dummy;

        call_binop->lhs = nullptr;
        call_binop->rhs = nullptr;
        delete call_binop;
      }

      if (!success) return nullptr;
      // In the same scope that this type was declared, make a new declare
      // with the type specified.

      auto fn_expr = GetFunctionLiteral(lhs);

      // look in cache to see if the function has already been chosen
      for (auto &gen : fn_expr->cache) {
        if (gen.first == rhs->type) {
          // TODO what if T is in the return type?
          return static_cast<Function *>(lhs_type)->output;
        }
      }

      // Cache the function
      fn_expr->cache[rhs->type] = GenerateSpecifiedFunction(
          (AST::FunctionLiteral *)fn_expr, (TypeVariable *)in_types, rhs->type);

      // TODO what if T is in the return type?
      return static_cast<Function *>(lhs_type)->output;

    } else {

      if (in_types != rhs->type) { return nullptr; }

      // TODO multiple return values. For now just taking the first
      auto ret_type = ((Function *)lhs_type)->output;
      if (ret_type->is_type_variable()) {
        // TODO more generically, if it has a variable
        auto tv        = (TypeVariable *)ret_type;
        auto new_scope = lhs->scope_->context.spawn();

        // TODO tv->identifier isn't in this scope. is this at all reasonable?
        auto rhs_eval = rhs->evaluate(lhs->scope_->context).as_type;
        new_scope.bind(Context::Value(rhs_eval), tv->identifier);

        ret_type = tv->identifier->evaluate(new_scope).as_type;
      }
      return ret_type;
    }

  } else if (lhs_type == Type_) {
    // Assume you are not a dufus, and if you're passing a struct to be
    // "called" that it's a parametric struct. (So we use .as_expr).
    //
    // auto lhs_as_expr = lhs->evaluate(scope_->context).as_expr;

    // If it isn't parametric, terribleness will ensue.
    return Type_;

  } else if (lhs_type->is_dependent_type()) {
    // TODO treat dependent types as functions
    auto dep_type = static_cast<DependentType *>(lhs->type);
    return (*dep_type)(rhs->evaluate(lhs->scope_->context).as_type);

  } else {
    return nullptr;
  }
}

namespace AST {
void Terminal::verify_types() {
  // Anything other than a string is done when the terminal is created.
  // TODO Do string literal and then set the values later.
  if (terminal_type == Language::Terminal::StringLiteral) { type = String; }
}

void Identifier::verify_types() {
  if (type != Unknown) { return; }

  // We have already checked in Declaration::verify_types that either there is
  // one type, or there are functions
  if (decls.size() == 1) {
    type = decls[0]->type;
    assert(type && "type is null");
  } else {
    std::vector<Type *> vec;
    for (auto decl : decls) { vec.push_back(decl->type); }

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
    if (!operand->type->is_pointer()) {
      error_log.log(loc, "Free can only be called on pointer types");
    }
    type = Void;
  } break;
  case Operator::Print: {
    if (operand->type == Void) {
      error_log.log(loc, "Void types cannot be printed");
    }
    type = Void;
  } break;
  case Operator::Return: {
    if (operand->type == Void) {
      error_log.log(loc, "Void types cannot be returned");
    }

    type = Void;
  } break;
  case Operator::At: {
    if (operand->type->is_pointer()) {
      type = static_cast<Pointer *>(operand->type)->pointee;

    } else {
      error_log.log(loc, "Dereferencing object of type " +
                             operand->type->to_string() +
                             ", which is not a pointer.");
      type = Error;
    }
  } break;
  case Operator::Call: {
    // TODO quantum types
    if (!operand->type->is_function()) {
      error_log.log(loc,
                    "Identifier `" + operand->token() + "` is not a function.");
      type = Error;
      return;
    }

    auto fn = static_cast<Function *>(operand->type);
    if (fn->input != Void) {
      error_log.log(loc, "Calling function `" + operand->token() +
                             "` with no arguments.");
      type = Error;
    } else {
      type = fn->output;
      assert(type && "fn return type is nullptr");
    }
  } break;
  case Operator::And: {
    type = (operand->type == Type_) ? Type_ : Ptr(operand->type);
    assert(type && "&type is null");
  } break;
  case Operator::Sub: {
    if (operand->type == Uint) {
      error_log.log(loc, "Negation applied to unsigned integer");
      type = Int;

    } else if (operand->type == Int) {
      type = Int;

    } else if (operand->type == Real) {
      type = Real;

    } else if (operand->type->is_struct()) {
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdentifierHereOrNull("__neg__");
        if (!id_ptr) { continue; }

        Dependency::traverse_from(Dependency::PtrWithTorV(id_ptr, false));
      }

      auto t = GetFunctionTypeReferencedIn(scope_, "__neg__", operand->type);
      if (t) {
        type = static_cast<Function *>(t)->output;
      } else {
        error_log.log(loc, type->to_string() + " has no negation operator.");
        type = Error;
      }

    } else {
      error_log.log(loc, type->to_string() + " has no negation operator.");
      type = Error;
    }
  } break;
  case Operator::Dots: {
    if (operand->type == Uint || operand->type == Int ||
        operand->type == Char) {
      type = Range(operand->type);
    } else {
      error_log.log(loc, type->to_string() + " cannot be part of a range");
      type = Error;
    }
  } break;
  case Operator::Not: {
    if (operand->type == Bool) {
      type = Bool;
    } else {
      error_log.log(
          loc,
          "The logical negation operator `!` only applies to boolean values");
      type = Error;
    }
  } break;
  case Operator::Import: {
    type = Void;
  } break;
  default: assert(false && "Died in Unop::verify_types");
  }
}

void Access::verify_types() {
  auto etype = operand->type;

  if (etype == Error) {
    // An error was already found in the types, so just pass silently
    type = Error;
    return;
  }

  // Access passes through pointers
  while (etype->is_pointer()) {
    etype = static_cast<Pointer *>(etype)->pointee;
  }

  if (etype->is_array() && member_name == "size") {
    type = Uint;
    return;

  } else if (etype == Type_) {
    Dependency::traverse_from(Dependency::PtrWithTorV(operand, false));

    if (member_name == "bytes" || member_name == "alignment") {
      type = Uint;
      return;
    }

    auto etypename = operand->evaluate(scope_->context).as_type;
    if (etypename->is_enum()) {
      auto enum_type = static_cast<Enumeration *>(etypename);
      // If you can get the value,
      if (enum_type->get_value(member_name)) {
        type = operand->evaluate(scope_->context).as_type;

      } else {
        error_log.log(loc, etypename->to_string() + " has no member " +
                               member_name + ".");
        type = Error;
      }
      return;
    }
  }

  if (etype->is_struct()) {
    assert(static_cast<Structure *>(etype)->field_type.size());

    auto member_type = static_cast<Structure *>(etype)->field(member_name);
    if (member_type) {
      type = member_type;

    } else {
      error_log.log(loc, "Objects of type " + etype->to_string() +
                             " have no member named `" + member_name + "`.");
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
      if (lhs->type->is_array() && rhs->type->is_array()) {
        auto lhs_array_type = (Array *)lhs->type;
        auto rhs_array_type = (Array *)rhs->type;
        if (lhs_array_type->data_type != rhs_array_type->data_type) {
          error_log.log(
              loc,
              "Invalid assignment. Data in arrays are of different types.");
        } else if (lhs_array_type->fixed_length &&
                   rhs_array_type->fixed_length) {
          error_log.log(loc,
                        "Invalid assignment. Arrays are of different lengths.");

        } else if (lhs_array_type->fixed_length) {
          error_log.log(loc, "Invalid assignment. Array on right-hand side has "
                             "unknown length, but lhs is known to be of "
                             "length " +
                                 std::to_string(lhs_array_type->len));
        } else {
          assert(rhs_array_type->fixed_length);
          return;
        }

      } else {
        error_log.log(loc, "Invalid assignment. Left-hand side has type " +
                               lhs->type->to_string() +
                               ", but right-hand side has type " +
                               rhs->type->to_string());
      }
    }
    type = Void;
    return;
  }

  switch (op) {
  case Operator::Rocket: {
    if (lhs->type != Bool) {
      error_log.log(loc, "LHS of rocket must be a bool");
      type = Error;
    }
  } break;
  case Operator::Call: {
    if (lhs->is_identifier()) {
      size_t num_matches = 0;
      Type *resulting_type;

      auto id_token = lhs->token();
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdentifierHereOrNull(id_token);
        if (!id_ptr) { continue; }

        if (id_ptr->type->is_quantum()) {
          // If the LHS has a quantum type, test all possibilities to see which
          // one works. Verify that exactly one works.
          for (auto opt : static_cast<QuantumType *>(id_ptr->type)->options) {
            auto t = CallResolutionMatch(opt, id_ptr, rhs);
            if (t) {
              ++num_matches;
              resulting_type = t;
            }
          }

        } else {
          resulting_type = CallResolutionMatch(id_ptr->type, id_ptr, rhs);
          if (resulting_type) { ++num_matches; }
        }
      }

      if (num_matches != 1) {
        type = Error;
        error_log.log(loc, num_matches == 0
                               ? "No function overload matches call."
                               : "Multiple function overloads match call.");
      } else {
        type = resulting_type;
      }

    } else {
      size_t num_matches = 0;
      Type *resulting_type;

      if (lhs->type->is_quantum()) {
        // If the LHS has a quantum type, test all possibilities to see which
        // one works. Verify that exactly one works.
        for (auto opt : static_cast<QuantumType *>(lhs->type)->options) {
          auto t = CallResolutionMatch(opt, lhs, rhs);
          if (t) {
            ++num_matches;
            resulting_type = t;
          }
        }

      } else {
        resulting_type = CallResolutionMatch(lhs->type, lhs, rhs);
        if (resulting_type) { ++num_matches; }
      }

      if (num_matches != 1) {
        type = Error;
        error_log.log(loc, num_matches == 0
                               ? "No function overload matches call."
                               : "Multiple function overloads match call.");
      } else {
        type = resulting_type;
      }
    }
  } break;
  case Operator::Index: {
    type = Error;
    if (!lhs->type->is_array()) {
      // TODO TOKENREMOVAL
      // TODO lhs might not have a precise token
      error_log.log(loc, "Identifier `" + lhs->token() +
                             "` does not name an array.");
      return;
    }

    if (rhs->type->is_range()) {
      type = Slice(static_cast<Array *>(lhs->type));
      break;
    }

    type = static_cast<Array *>(lhs->type)->data_type;
    assert(type && "array data type is nullptr");
    // TODO allow slice indexing
    if (rhs->type == Int) { break; }
    if (rhs->type == Uint) { break; }

    error_log.log(loc,
                  "Array must be indexed by an int or uint. You supplied a " +
                      rhs->type->to_string());
    return;

  } break;
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

    error_log.log(loc, "Invalid cast from " + lhs->type->to_string() + " to " +
                           type->to_string());
  } break;
  case Operator::Dots: {
    if (lhs->type == Int && rhs->type == Int) {
      type = Range(Int);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      type = Range(Uint);

    } else if (lhs->type == Char && rhs->type == Char) {
      type = Range(Char);

    } else {
      error_log.log(loc, "No known range construction for types" +
                             lhs->type->to_string() + " .. " +
                             rhs->type->to_string());
    }
  } break;
  case Language::Operator::XorEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Error;
      error_log.log(loc, "Operator ^= must take arguments of type bool");
    }
  } break;
  case Language::Operator::AndEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Error;
      error_log.log(loc, "Operator &= must take arguments of type bool");
    }
  } break;
  case Language::Operator::OrEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Error;
      error_log.log(loc, "Operator |= must take arguments of type bool");
    }
  } break;

#define CASE(OpName, op_name, symbol, ret_type)                                \
  case Language::Operator::OpName: {                                           \
    if ((lhs->type == Int && rhs->type == Int) ||                              \
        (lhs->type == Uint && rhs->type == Uint) ||                            \
        (lhs->type == Real && rhs->type == Real)) {                            \
      type = ret_type;                                                         \
    } else {                                                                   \
      for (auto scope_ptr = scope_; scope_ptr;                                 \
           scope_ptr = scope_ptr->parent) {                                    \
        auto id_ptr = scope_ptr->IdentifierHereOrNull("__" op_name "__");      \
        if (!id_ptr) { continue; }                                             \
        Dependency::traverse_from(Dependency::PtrWithTorV(id_ptr, false));     \
      }                                                                        \
      auto fn_type = GetFunctionTypeReferencedIn(scope_, "__" op_name "__",    \
                                                 Tup({lhs->type, rhs->type})); \
      if (fn_type) {                                                           \
        type = static_cast<Function *>(fn_type)->output;                       \
      } else {                                                                 \
        type = Error;                                                          \
        error_log.log(loc, "No known operator overload for `" symbol           \
                           "` with types " +                                   \
                               lhs->type->to_string() + " and " +              \
                               rhs->type->to_string());                        \
      }                                                                        \
    }                                                                          \
  } break

    CASE(Add, "add", "+", lhs->type);
    CASE(Sub, "sub", "-", lhs->type);
    CASE(Div, "div", "/", lhs->type);
    CASE(Mod, "mod", "%", lhs->type);
    CASE(AddEq, "add_eq", "+=", Void);
    CASE(SubEq, "sub_eq", "-=", Void);
    CASE(MulEq, "mul_eq", "*=", Void);
    CASE(DivEq, "div_eq", "/=", Void);
    CASE(ModEq, "mod_eq", "%=", Void);

#undef CASE

  // Mul is done separately because of the function composition
  case Operator::Mul: {
    if ((lhs->type == Int && rhs->type == Int) ||
        (lhs->type == Uint && rhs->type == Uint) ||
        (lhs->type == Real && rhs->type == Real)) {
      type = lhs->type;

    } else if (lhs->type->is_function() && rhs->type->is_function()) {
      auto lhs_fn = (Function *)lhs->type;
      auto rhs_fn = (Function *)rhs->type;
      if (rhs_fn->output == lhs_fn->input) {
        type = Func(rhs_fn->input, lhs_fn->output);

      } else {
        type = Error;
        error_log.log(loc, "Functions cannot be composed.");
      }

    } else {
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdentifierHereOrNull("__mul__");
        if (!id_ptr) { continue; }

        Dependency::traverse_from(Dependency::PtrWithTorV(id_ptr, false));
      }

      auto fn_type = GetFunctionTypeReferencedIn(scope_, "__mul__",
                                                 Tup({lhs->type, rhs->type}));
      if (fn_type) {
        type = static_cast<Function *>(fn_type)->output;
      } else {
        type = Error;
        error_log.log(loc, "No known operator overload for `*` with types " +
                               lhs->type->to_string() + " and " +
                               rhs->type->to_string());
      }
    }
  } break;
  case Operator::Arrow: {
    if (lhs->type != Type_) {
      type = Error;
      error_log.log(loc, "From-type for a function must be a type.");
    }
    if (rhs->type != Type_) {
      type = Error;
      error_log.log(loc, "To-type for a function must be a type.");
    }

    if (type != Error) { type = Type_; }

  } break;
  default: {
    assert(false);
  }
  }
}

void ChainOp::verify_types() {
  if (is_comma_list()) {
    std::vector<Type *> type_vec(exprs.size(), nullptr);

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
  std::set<Type *> expr_types;

  for (const auto &expr : exprs) { expr_types.insert(expr->type); }

  if (expr_types.size() == 1) {
    // TODO must it always be bool?
    type = Bool;

  } else {

    // TODO guess what type was intended
    std::stringstream ss;
    ss << "Type error: Types do not all match. Found the following types:\n";
    for (const auto &t : expr_types) { ss << "\t" << *t << "\n"; }

    error_log.log(loc, ss.str());
    type = Error;
  }
}

void InDecl::verify_types() {
  if (container->type == Void) {
    type = Error;
    error_log.log(loc, "Cannot iterate over a void type.");
    return;
  }

  if (container->type->is_array()) {
    type = static_cast<Array *>(container->type)->data_type;

  } else if (container->type->is_slice()) {
    type = static_cast<SliceType *>(container->type)->array_type->data_type;

  } else if (container->type->is_range()) {
    type = static_cast<RangeType *>(container->type)->end_type;

  } else if (container->type == Type_) {
    auto t = container->evaluate(scope_->context).as_type;
    if (t->is_enum()) { type = t; }

  } else {
    error_log.log(loc, "Cannot determine type from in declaration.");
    type = Error;
  }

  identifier->type = type;
}

void Declaration::verify_types() {
  if (type_expr->type == Void) {
    type = Error;
    error_log.log(loc, "Void types cannot be assigned.");
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
  case DeclType::Tick: {
    if (!type_expr->type->is_function()) {
      // TODO Need a way better
      error_log.log(
          loc, "Cannot generate a type where the tester is not a function");
      type = Error;
      return;
    }

    auto test_func = static_cast<Function *>(type_expr->type);
    if (test_func->output != Bool) {
      // TODO What about implicitly cast-able to bool via a user-defined cast?
      error_log.log(loc, "Test function must return a bool");
      type = Error;
      return;
    }

    type = Type_;

    // TODO can't continue with type verification immediately

  } break;
  }

  if (identifier->token() == "__print__") {
    if (!type->is_function()) {
      error_log.log(loc, "Print must be defined to be a function.");
      return;
    }

    bool error_raised = false;
    auto fn_type = (Function *)type;
    if (!fn_type->input->is_struct()) {
      error_log.log(loc, "Cannot define print function for " +
                             fn_type->input->to_string());
      error_raised = true;
    }

    if (fn_type->output != Void) {
      error_log.log(loc, "print function must return void");
      error_raised = true;
    }

    if (error_raised) { return; }
  } else if (identifier->token() == "__assign__") {
    if (!type->is_function()) {
      error_log.log(loc, "Assign must be defined to be a function");
      return;
    }

    bool error_raised = false;
    auto fn_type = (Function *)type;
    if (!fn_type->input->is_tuple()) {
      error_log.log(loc, "Cannot define assign function for " +
                             fn_type->input->to_string());
      error_raised = true;
    } else {
      auto in = static_cast<Tuple *>(fn_type->input);
      if (in->entries.size() != 2) {
        error_log.log(loc, "Assignment must be a binary operator, but " +
                               std::to_string(in->entries.size()) + "argument" +
                               (in->entries.size() != 1 ? "s" : "") +
                               " given.");
        error_raised = true;
      }
      // TODO more checking.
    }

    if (fn_type->output != Void) {
      error_log.log(loc, "assignment must return void");
      error_raised = true;
    }

    if (error_raised) { return; }

  } else if (identifier->token() == "__destroy__") {
    if (!type->is_function()) {
      error_log.log(loc, "Destructor must be defined to be a function.");
      return;
    }

    bool error_raised = false;
    auto fn_type = (Function *)type;
    if (!fn_type->input->is_pointer()) {
      error_log.log(loc, "Destructor must take one pointer argument.");
      error_raised = true;
    } else {
      auto ptee = static_cast<Pointer *>(fn_type->input)->pointee;
      if (!ptee->is_struct()) {
        error_log.log(loc, "Destructor must take a pointer to a struct.");
        error_raised = true;
      }
    }

    if (fn_type->output != Void) {
      error_log.log(loc, "Destructor must return void");
      error_raised = true;
    }

    if (error_raised) { return; }
  }

  // TODO if RHS is not a type give a nice message instead of segfaulting

  if (type_expr->is_terminal()) {
    auto term = static_cast<Terminal *>(type_expr);
    if (term->terminal_type == Language::Terminal::Null) {
      error_log.log(loc, "Cannot infer the type of `null`.");
    }
  }

  assert(type && "decl expr is nullptr");
}

void ArrayType::verify_types() {
  assert(length && data_type->type == Type_);
  type = Type_;

  // TODO have a Hole type primitive.
  if (length->is_terminal() &&
      static_cast<Terminal *>(length)->terminal_type ==
          Language::Terminal::Hole) {
    return;
  }

  // TODO change this to just uint
  if (length->type != Int && length->type != Uint) {
    error_log.log(loc, "Array length indexed by non-integral type");
  }
}

void ArrayLiteral::verify_types() {
  if (elems.empty()) {
    type = Error;
    error_log.log(loc, "Cannot infer the type of an empty array.");
    return;
  }

  auto type_to_match = elems.front()->type;
  assert(type_to_match && "type to match is nullptr");
  if (type_to_match == Error) {
    type = Error;
    return;
  }

  type = Arr(type_to_match, elems.size());
  for (const auto &el : elems) {
    if (el->type != type_to_match) {
      error_log.log(loc, "Type error: Array literal must have consistent type");
      type = Error;
    }
  }
}

void FunctionLiteral::verify_types() {
  Type *ret_type = return_type_expr->evaluate(fn_scope->context).as_type;
  assert(ret_type && "Return type is a nullptr");
  Type *input_type;
  size_t inputssize = inputs.size();
  if (inputssize == 0) {
    input_type = Void;

  } else if (inputssize == 1) {
    input_type = inputs.front()->type;

  } else {
    std::vector<Type *> input_type_vec;
    for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

    input_type = Tup(input_type_vec);
  }

  // TODO generics?
  type = Func(input_type, ret_type);

  assert(type && "FunctionLiteral type is nullptr");
}

void Case::verify_types() {
  std::set<Type *> value_types;
  for (const auto &kv : key_vals) {
    if (kv.first->type != Bool) {
      // TODO: give some context for this error message. Why must this be the
      // type?  So far the only instance where this is called is for case
      // statements,
      error_log.log(loc, "Type of `____` must be bool, but " +
                             kv.first->type->to_string() + " found instead.");
      kv.first->type = Bool;
      assert(kv.first->type && "keytype");
    }

    value_types.insert(kv.second->type);
  }

  // TODO guess what type was intended
  if (value_types.size() != 1) {
    error_log.log(loc, "Type error: Values do not match in key-value pairs");
    type = Error;
  } else {
    type = *value_types.begin();
  }
}

void Statements::verify_types() {
  // TODO Verify that a return statement, if present, is the last thing
}

void While::verify_types() {
  if (condition->type != Bool) {
    error_log.log(loc, "While loop condition must be a bool, but " +
                           condition->type->to_string() + " given.");
  }
}

void For::verify_types() {
  /*
                              */
}

void Conditional::verify_types() {
  for (const auto &cond : conditions) {
    if (cond->type != Bool) {
      error_log.log(loc, "Conditional must be a bool, but " +
                             cond->type->to_string() + " given.");
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

void Jump::verify_types() {
  auto scope_ptr = scope_;
  while (scope_ptr) {
    assert(scope_ptr->is_block_scope());
    auto block_scope_ptr = static_cast<BlockScope *>(scope_ptr);
    if (block_scope_ptr->type == ScopeType::Function) {
      if (jump_type != JumpType::Return) {
        error_log.log(loc, "statement must be contained inside a loop.");
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

void DummyTypeExpr::verify_types() { type = Type_; }
} // namespace AST
