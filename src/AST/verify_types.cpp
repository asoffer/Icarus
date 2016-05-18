#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

extern std::queue<AST::Node *> VerificationQueue;
extern Type *GetFunctionTypeReferencedIn(Scope *scope,
                                         const std::string &fn_name,
                                         Type *input_type);

extern AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr);

static AST::FunctionLiteral *
GenerateSpecifiedFunction(AST::FunctionLiteral *fn_lit,
                          const std::map<TypeVariable *, Type *> &matches) {

  auto num_matches = matches.size();
  auto lookup_key  = new TypeVariable *[num_matches];
  auto lookup_val  = new Type *[num_matches];

  size_t i = 0;
  for (auto kv : matches) {
    lookup_key[i] = kv.first;
    lookup_val[i] = kv.second;
    ++i;
  }

  auto cloned_func =
      (AST::FunctionLiteral *)fn_lit->clone(num_matches, lookup_key, lookup_val);

  auto old_stack_size = Scope::Stack.size();
  Scope::Stack.push(fn_lit->scope_);

  cloned_func->assign_scope();
  cloned_func->join_identifiers();
  cloned_func->verify_types();

  Scope::Stack.pop();
  assert(Scope::Stack.size() == old_stack_size);

  delete[] lookup_key;
  delete[] lookup_val;

  return cloned_func;
}

// TODO:
//  * Send back error messages to be logged if not exactly one match occurs
//  * Log locations?
static bool MatchCall(Type *lhs, Type *rhs,
                      std::map<TypeVariable *, Type *> &matches) {

  if (!lhs->has_vars) { return lhs == rhs; }

  if (lhs->is_type_variable()) {
    auto lhs_var = (TypeVariable *)lhs;
    assert(lhs_var->test);
    auto test_fn_expr =
        lhs_var->test->evaluate(lhs_var->identifier->scope_->context).as_expr;
    assert(test_fn_expr->is_function_literal());
    auto test_fn = (AST::FunctionLiteral *)test_fn_expr;

    assert(test_fn->inputs.size() == 1 && test_fn->inputs[0]->type == Type_);

    // Do a function call
    test_fn->inputs[0]->identifier->value = Context::Value(rhs);
    assert(test_fn->type->is_function() &&
           static_cast<Function *>(test_fn->type)->output == Bool);
    bool test_result = test_fn->evaluate(test_fn->scope_->context).as_bool;

    if (test_result) {
      // TODO check if you've already inserted lhs_var and make sure you have
      // valid matches. Track errors to log if not.
      matches[lhs_var] = rhs;
      return true;
    }
    return false;
  }

  if (lhs->is_pointer()) {
    return (rhs->is_pointer()) &&
           MatchCall(static_cast<Pointer *>(lhs)->pointee,
                     static_cast<Pointer *>(rhs)->pointee, matches);
  }

  if (lhs->is_array()) {
    if (!rhs->is_array()) { return false; }

    auto lhs_array = static_cast<Array *>(lhs);
    auto rhs_array = static_cast<Array *>(rhs);

    if (lhs_array->fixed_length != rhs_array->fixed_length) { return false; }

    if (lhs_array->fixed_length) {
      return (lhs_array->len == rhs_array->len) &&
             MatchCall(lhs_array->data_type, rhs_array->data_type, matches);
    } else {
      return MatchCall(lhs_array->data_type, rhs_array->data_type, matches);
    }
  }

  if (lhs->is_function()) {
    if (!rhs->is_function()) { return false; }

    auto lhs_in  = static_cast<Function *>(lhs)->input;
    auto rhs_in  = static_cast<Function *>(rhs)->input;
    auto lhs_out = static_cast<Function *>(lhs)->output;
    auto rhs_out = static_cast<Function *>(rhs)->output;

    return MatchCall(lhs_in, rhs_in, matches) &&
           MatchCall(lhs_out, rhs_out, matches);
  }

  if (lhs->is_struct()) {
    std::cout << *lhs << std::endl;
    assert(false && "Not yet implemented");
  }

  if (lhs->is_tuple()) {
    auto lhs_tuple = (Tuple *)lhs;
    auto rhs_tuple = (Tuple *)rhs;
    if (lhs_tuple->entries.size() != rhs_tuple->entries.size()) {
      return false;
    }

    size_t num_entries = lhs_tuple->entries.size();

    for (size_t i = 0; i < num_entries; ++i) {
      if (!MatchCall(lhs_tuple->entries[i], rhs_tuple->entries[i], matches)) {
        return false;
      }
    }
    return true;
  }

  assert(false);
}

static Type *EvalWithVars(Type *type,
                          const std::map<TypeVariable *, Type *> &lookup) {
  if (!type->has_vars) { return type; }

  if (type->is_type_variable())  {
    auto iter =lookup.find((TypeVariable *)type);
    return (iter == lookup.end()) ? type : iter->second;
  }

  if (type->is_pointer()) {
    auto ptr_type = static_cast<Pointer *>(type);
    return Ptr(EvalWithVars(ptr_type->pointee, lookup));
  }

  if (type->is_array()) {
    auto array_type = (Array *)type;
    if (array_type->fixed_length) {
      return Arr(EvalWithVars(array_type->data_type, lookup), array_type->len);
    } else {
      return Arr(EvalWithVars(array_type->data_type, lookup));
    }
  }

  if (type->is_function()) {
    auto func_type = (Function *)type;
    return Func(EvalWithVars(func_type->input, lookup),
                EvalWithVars(func_type->output, lookup));
  }

  if (type->is_tuple()) {
    auto tup_type = (Tuple*)type;
    std::vector<Type *> entries;
    entries.reserve(tup_type->entries.size());

    for (auto entry : tup_type->entries) {
      entries.push_back(EvalWithVars(entry, lookup));
    }

    return Tup(entries);
  }


  std::cout << *type << std::endl;
  assert(false);
}

static Type *CallResolutionMatch(Type *lhs_type, AST::Expression *lhs,
                                 AST::Expression *rhs, TokenLocation rhs_loc) {
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
        auto dummy      = new AST::DummyTypeExpr(rhs_loc, rhs->type);
        call_binop->rhs = dummy;

        success = call_binop->evaluate(lhs->scope_->context).as_bool;

        dummy->value = nullptr;
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
//      fn_expr->cache[rhs->type] = GenerateSpecifiedFunction(
//          (AST::FunctionLiteral *)fn_expr, (TypeVariable *)in_types, rhs->type);

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
        auto rhs_eval         = rhs->evaluate(lhs->scope_->context).as_type;
        tv->identifier->value = Context::Value(rhs_eval);

        ret_type = tv->identifier->evaluate(new_scope).as_type;
      }
      return ret_type;
    }

  } else if (lhs_type == Type_) {
    if (!lhs->value.as_type->is_parametric_struct()) {
      error_log.log(lhs->loc, "Invalid call of () operator on a type. LHS is "
                              "not a parametric struct");
      return nullptr;
    }

    return Type_;

  } else if (lhs_type->is_dependent_type()) {
    // TODO treat dependent types as functions
    auto dep_type = static_cast<DependentType *>(lhs->type);
    return (*dep_type)(rhs->evaluate(lhs->scope_->context).as_type);

  } else {
    return nullptr;
  }
}

#define STARTING_CHECK                                                         \
  assert(type != Unknown && "Cyclic dependency");                              \
  if (type) { return; }                                                        \
  type = Unknown

namespace AST {
// TODO In what file should this be placed?
// TODO this should take a context because flushing it out depends on the context.
void StructLiteral::FlushOut() {
  assert(value.as_type && value.as_type->is_struct());

  auto tval = static_cast<Structure *>(value.as_type);
  if (!tval->field_num_to_name.empty()) { return; }

  for (auto d : declarations) {
    d->verify_types();
    tval->insert_field(d->identifier->token, d->identifier->type,
                       d->decl_type == DeclType::Infer ? d->expr : nullptr);
  }
}

void Terminal::verify_types() {
  // Anything other than a string is done when the terminal is created.
  // TODO Do string literal and then set the values later.
  if (terminal_type == Language::Terminal::StringLiteral) {
    auto string_decl = Scope::Global->IdentifierHereOrNull("string");
    string_decl->verify_types();
    type = String;
  }
}

void Identifier::AppendType(Type *t) {
  if (!type) {
    type = t;
    return;
  } else {
    if (type->is_quantum()) {
      auto q = static_cast<QuantumType *>(type);
      q->options.push_back(t);
    } else {
      type = Quantum({type, t});
    }
  }
}

void Identifier::verify_types() {
  for (auto decl : decls) { decl->verify_types(); }
}

void Unop::verify_types() {
  STARTING_CHECK;
  operand->verify_types();


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
      error_log.log(loc, "Operand is not a function.");
      type = Error;
      return;
    }

    auto fn = static_cast<Function *>(operand->type);
    if (fn->input != Void) {
      error_log.log(loc, "Calling function with no arguments.");
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

        id_ptr->verify_types();
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
  STARTING_CHECK;
  operand->verify_types();
  auto base_type = operand->type;

  // Propogate errors silently.
  if (base_type == Error) {
    type = Error;
    return;
  }

  // Access passes through pointers
  while (base_type->is_pointer()) {
    base_type = static_cast<Pointer *>(base_type)->pointee;
  }

  if (base_type->is_array()) {
    if (member_name == "size") {
      type = Uint;
      return;
    } else if (member_name == "resize") {
      auto array_base_type = static_cast<Array *>(base_type);
      if (array_base_type->fixed_length) {
        error_log.log(loc, "Cannot resize a fixed-length array.");
        type = Error;
        return;
      }
      
      // TODO Kinda hacky, because the type should really take the array into
      // account, but we're just dealing with it at code-gen time?
      type = Func(/*Ptr(operand->type), */Uint, Void);
      return;
    }

  } else if (base_type == Type_) {
    if (member_name == "bytes" || member_name == "alignment") {
      if (!operand->value.as_type) { operand->evaluate(scope_->context); }
      assert(operand->value.as_type);
      type = Uint;
      return;
    }

    auto evaled_type = operand->evaluate(scope_->context).as_type;
    if (evaled_type->is_enum()) {
      auto enum_type = (Enumeration *)evaled_type;
      // If you can get the value,
      if (enum_type->get_value(member_name)) {
        type = operand->evaluate(scope_->context).as_type;

      } else {
        error_log.log(loc, evaled_type->to_string() + " has no member " +
                               member_name + ".");
        type = Error;
      }
      return;
    }
  }

  if (base_type->is_struct()) {
    auto member_type = static_cast<Structure *>(base_type)->field(member_name);
    if (member_type) {
      type = member_type;

    } else {
      error_log.log(loc, "Objects of type " + base_type->to_string() +
                             " have no member named `" + member_name + "`.");
      type = Error;
    }
  }

  if (base_type->is_primitive() || base_type->is_array() ||
      base_type->is_function()) {
    error_log.log(loc, base_type->to_string() + " has no field named '" +
                           member_name + "'.");
    type = Error;
    return;
  }

  assert(type && "type is nullptr in access");
  return;
}

void Binop::verify_types() {
  STARTING_CHECK;
  lhs->verify_types();
  rhs->verify_types();

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
      std::vector<std::map<TypeVariable *, Type *>> match_vec;
      Identifier *matched_id = nullptr;
      Type *matched_type;

      auto id_token = static_cast<AST::Identifier *>(lhs)->token;

      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdentifierHereOrNull(id_token);

        if (!id_ptr) { continue; }

        if (id_ptr->type->is_quantum()) {
          // If the LHS has a quantum type, test all possibilities to see which
          // one works. Verify that exactly one works.
          for (auto opt : static_cast<QuantumType *>(id_ptr->type)->options) {
            std::map<TypeVariable *, Type *> matches;
            assert(opt->is_function());
            auto in_type = static_cast<Function *>(opt)->input;
            if (MatchCall(in_type, rhs->type, matches)) {
              ++num_matches;
              match_vec.push_back(matches);
              matched_type = opt;
              matched_id   = id_ptr;
            }
          }

        } else {
          if (id_ptr->type->is_function()) {
            std::map<TypeVariable *, Type *> matches;
            auto in_type = static_cast<Function *>(id_ptr->type)->input;

            if (MatchCall(in_type, rhs->type, matches)) {
              ++num_matches;
              match_vec.push_back(matches);
              matched_type = id_ptr->type;
              matched_id   = id_ptr;
            }
          } else {
            if (id_ptr->type == Type_) {
              assert(id_ptr->value.as_type->is_parametric_struct());
              ++num_matches;
              match_vec.push_back({});
              matched_type = id_ptr->type;
              matched_id   = id_ptr;

            } else {
              assert(false);
            }
          }
        }
      }

      assert(match_vec.size() == num_matches);

      if (num_matches != 1) {
        type = Error;
        error_log.log(loc, num_matches == 0
                               ? "No function overload matches call."
                               : "Multiple function overloads match call.");
      } else {
        auto evaled_type = EvalWithVars(matched_type, match_vec[0]);

        if (evaled_type->is_function()) {
          type = static_cast<Function *>(evaled_type)->output;

          if (matched_id->type->has_vars && !matched_id->is_arg) {
            auto fn_expr = GetFunctionLiteral(lhs);

            // look in cache to see if the function has already been chosen
            for (auto &gen : fn_expr->cache) {
              if (gen.first == static_cast<Function *>(evaled_type)->input) {
                return;
              }
            }

            // Cache the function
            fn_expr->cache[rhs->type] =
                GenerateSpecifiedFunction(fn_expr, match_vec[0]);
          }
        } else {
          type = evaled_type;
          return;
        }
      }

    } else {
      size_t num_matches = 0;
      Type *resulting_type;

      if (lhs->type->is_quantum()) {
        // If the LHS has a quantum type, test all possibilities to see which
        // one works. Verify that exactly one works.
        for (auto opt : static_cast<QuantumType *>(lhs->type)->options) {
          auto t = CallResolutionMatch(opt, lhs, rhs, rhs->loc);
          if (t) {
            ++num_matches;
            resulting_type = t;
          }
        }

      } else {
        resulting_type = CallResolutionMatch(lhs->type, lhs, rhs, rhs->loc);
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
      error_log.log(loc, "LHS does not name an array.");
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
        id_ptr->verify_types();                                                \
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

        // Dependency::traverse_from(Dependency::PtrWithTorV(id_ptr, false));
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
  STARTING_CHECK;
  for (auto e : exprs) { e->verify_types(); }

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
  STARTING_CHECK;
  container->verify_types();

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
  STARTING_CHECK;
  expr->verify_types();

  scope_->ordered_decls_.push_back(this);

  if (expr->type == Void) {
    type = Error;
    error_log.log(loc, "Void types cannot be assigned.");
    return;
  }

  switch (decl_type) {
  case DeclType::Std: {
    type = expr->evaluate(scope_->context).as_type;
    if (type->is_struct()) {
      static_cast<Structure *>(type)->ast_expression->FlushOut();
    }
  } break;
  case DeclType::Infer: {
    type = expr->type;

    if (type == Type_) {
      if (expr->is_struct_literal()) {
        assert(expr->value.as_type && expr->value.as_type->is_struct());

        // TODO mangle the name correctly
        static_cast<Structure *>(expr->value.as_type)
            ->set_name(identifier->token);

      } else if (expr->is_parametric_struct_literal()) {
        assert(expr->value.as_type &&
               expr->value.as_type->is_parametric_struct());
        // TODO mangle the name correctly
        static_cast<ParametricStructure *>(expr->value.as_type)
            ->set_name(identifier->token);

      } else if (expr->is_enum_literal()) {
        expr->evaluate(scope_->context); // TODO do we need to evaluate here?
        assert(expr->value.as_type);
        // TODO mangle name properly.
        static_cast<Enumeration *>(expr->value.as_type)->bound_name =
            identifier->token;
      }

      identifier->value = expr->value;

    } else if (expr->is_function_literal()) {
      identifier->verify_types();
      identifier->value = Context::Value(expr);
    }

  } break;
  case DeclType::Tick: {
    if (!expr->type->is_function()) {
      // TODO Need a way better
      error_log.log(
          loc, "Cannot generate a type where the tester is not a function");
      type = Error;
      return;
    }

    auto test_func = static_cast<Function *>(expr->type);
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

  identifier->AppendType(type);

  if (identifier->token == "__print__") {
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
  } else if (identifier->token == "__assign__") {
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

  } else if (identifier->token == "__destroy__") {
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

  if (expr->is_terminal()) {
    auto term = (Terminal *)expr;
    if (term->terminal_type == Language::Terminal::Null) {
      error_log.log(loc, "Cannot infer the type of `null`.");
    }
  }

  assert(type && "decl expr is nullptr");
}

void ArrayType::verify_types() {
  STARTING_CHECK;
  length->verify_types();
  data_type->verify_types();

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
  STARTING_CHECK;
  for (auto e : elems) { e->verify_types(); }

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
  STARTING_CHECK;
  for (auto in : inputs) { in->verify_types(); }
  VerificationQueue.push(statements);
  return_type_expr->verify_types();

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
  STARTING_CHECK;
  for (auto kv : key_vals) {
    kv.first->verify_types();
    kv.second->verify_types();
  }

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
  for (auto stmt : statements) { stmt->verify_types(); }

  // TODO Verify that a return statement, if present, is the last thing
}

void While::verify_types() {
  condition->verify_types();
  statements->verify_types();

  if (condition->type != Bool) {
    error_log.log(loc, "While loop condition must be a bool, but " +
                           condition->type->to_string() + " given.");
  }
}

void For::verify_types() {
  for (auto iter : iterators) { iter->verify_types(); }
  statements->verify_types();
}

void Conditional::verify_types() {
  for (auto cond : conditions) { cond->verify_types(); }
  for (auto stmts : statements) { stmts->verify_types(); }

  for (const auto &cond : conditions) {
    if (cond->type != Bool) {
      error_log.log(loc, "Conditional must be a bool, but " +
                             cond->type->to_string() + " given.");
    }
  }
}

void EnumLiteral::verify_types() {
  static size_t anon_enum_counter = 0;

  type  = Type_;
  value = Context::Value(
      Enum("__anon.enum" + std::to_string(anon_enum_counter), this));
  ++anon_enum_counter;
}

void ParametricStructLiteral::verify_types() {}

void StructLiteral::verify_types() {
  for (auto decl : declarations) { VerificationQueue.push(decl); }
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

void DummyTypeExpr::verify_types() {
  STARTING_CHECK;
  type = Type_;
}
} // namespace AST

#undef STARTING_CHECK
#undef AT
