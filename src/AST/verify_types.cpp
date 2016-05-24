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

  auto cloned_func = (AST::FunctionLiteral *)fn_lit->clone(
      num_matches, lookup_key, lookup_val);

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

// This function is handed two types. The left-hand side argument is the input
// to a function, and the right-hand side argument is the arguments a function
// is called with. This function attempts to match the inputs to determine if
// this particular call is possible.
//
// This function is not concerned with overload resolution. Rather, for each
// possible overload, this function is called and is used to determine whether a
// particular option is viable. At another place in the code, we ensure that
// exactly one overload is viable.
//
// Instead, this function is tasked with attempting to match the arguments
// provided to a potential overload. While at first glance, it seems like we
// could just test for the equality of the types, this is more complicated for
// two reasons.
//
// First, to give good error messages, we need to know why the
// match failed. For example, we want to know if we provided a pointer instead
// of a value, or if we provided an array with a run-time length instead of a
// fixed-length array.
//
// Second, with generic declarations (the ` operator), we need to (if a match
// exists) log a map for how to match provided argument types to the generic
// ones.
static bool MatchCall(Type *lhs, Type *rhs,
                      std::map<TypeVariable *, Type *> &matches,
                      std::string &error_message) {
  if (!lhs->has_vars) {
    if (lhs == rhs) { return true; }
    error_message +=
        rhs->to_string() + " does not match " + lhs->to_string() + ".\n";
    return false;
  }

  if (lhs->is_type_variable()) {
    auto lhs_var = (TypeVariable *)lhs;
    assert(lhs_var->test);

    auto test_fn_expr = lhs_var->test->evaluate().as_expr;
    assert(test_fn_expr->is_function_literal());

    auto test_fn = (AST::FunctionLiteral *)test_fn_expr;

    assert(test_fn->type == Func(Type_, Bool));

    // Do a function call
    test_fn->inputs[0]->identifier->value = Context::Value(rhs);
    bool test_result                      = test_fn->evaluate().as_bool;
    test_fn->inputs[0]->identifier->value = nullptr;

    if (test_result) {
      auto iter = matches.find(lhs_var);
      if (iter == matches.end()) {
        matches[lhs_var] = rhs;
        return true;

      } else if (iter->second == rhs) {
        return true;
      } else {
        // TODO better message. Log locations of other options and explain that
        // those positions are wrong because this one is authoritative.
        error_message +=
            "Failure to match parameter " + lhs_var->to_string() + ".\n";
        return false;
      }
    } else {
      // Test result failed
      error_message += "Type " + rhs->to_string() + " failed test for " +
                       lhs_var->identifier->token + ".";

      if (lhs_var->test->is_identifier()) {
        auto id_test = (AST::Identifier *)(lhs_var->test);
        assert (id_test->decls.size() == 1);
        error_message += " (See line " +
                         std::to_string(id_test->decls[0]->loc.line_num) + ")";
      }
      error_message += "\n";
      return false;
    }
  }

  if (lhs->is_pointer()) {
    if (!rhs->is_pointer()) {
      error_message +=
          "Expected pointer, but received " + rhs->to_string() + ".\n";
      return false;
    }

    return MatchCall(static_cast<Pointer *>(lhs)->pointee,
                     static_cast<Pointer *>(rhs)->pointee, matches,
                     error_message);
  }

  if (lhs->is_array()) {
    if (!rhs->is_array()) {
      error_message += "Expected array, but received" + rhs->to_string() + ".\n";
      return false;
    }

    auto lhs_array = (Array *)lhs;
    auto rhs_array = (Array *)rhs;

    if (lhs_array->fixed_length != rhs_array->fixed_length) { return false; }

    if (lhs_array->fixed_length) {
      return (lhs_array->len == rhs_array->len) &&
             MatchCall(lhs_array->data_type, rhs_array->data_type, matches,
                       error_message);
    } else {
      return MatchCall(lhs_array->data_type, rhs_array->data_type, matches,
                       error_message);
    }
  }

  if (lhs->is_function()) {
    if (!rhs->is_function()) {
      error_message +=
          "Expected function, but received" + rhs->to_string() + ".\n";
      return false;
    }

    auto lhs_in  = static_cast<Function *>(lhs)->input;
    auto rhs_in  = static_cast<Function *>(rhs)->input;
    auto lhs_out = static_cast<Function *>(lhs)->output;
    auto rhs_out = static_cast<Function *>(rhs)->output;

    return MatchCall(lhs_in, rhs_in, matches, error_message) &&
           MatchCall(lhs_out, rhs_out, matches, error_message);
  }

  if (lhs->is_struct()) {
    if (!rhs->is_struct()) { return false; }

    // TODO parameters are not necessarily a collection of types.

    auto lhs_struct = (Structure *)lhs;
    auto rhs_struct = (Structure *)rhs;
    if (lhs_struct->creator != rhs_struct->creator) { return false; }

    auto lhs_params =
        lhs_struct->creator->reverse_cache[lhs_struct->ast_expression];
    auto rhs_params =
        rhs_struct->creator->reverse_cache[rhs_struct->ast_expression];
    if (lhs_params.size() != rhs_params.size()) { return false; }

    auto num_params = lhs_params.size();
    bool coherent_matches = true;
    for (size_t i = 0; i < num_params; ++i) {
      coherent_matches &=
          MatchCall(lhs_params[i], rhs_params[i], matches, error_message);
    }
    return coherent_matches;
  }

  if (lhs->is_tuple()) {
    auto lhs_tuple = (Tuple *)lhs;
    auto rhs_tuple = (Tuple *)rhs;
    if (lhs_tuple->entries.size() != rhs_tuple->entries.size()) {
      return false;
    }

    size_t num_entries = lhs_tuple->entries.size();

    for (size_t i = 0; i < num_entries; ++i) {
      if (!MatchCall(lhs_tuple->entries[i], rhs_tuple->entries[i], matches,
                     error_message)) {
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

  if (type->is_type_variable()) {
    auto iter = lookup.find((TypeVariable *)type);
    return (iter == lookup.end()) ? type : iter->second;
  }

  if (type->is_pointer()) {
    auto ptr_type = (Pointer *)type;
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
    auto tup_type = (Tuple *)type;
    std::vector<Type *> entries;
    entries.reserve(tup_type->entries.size());

    for (auto entry : tup_type->entries) {
      entries.push_back(EvalWithVars(entry, lookup));
    }

    return Tup(entries);
  }

  if (type->is_struct()) {
    auto struct_type = (Structure *)type;
    assert(struct_type->creator);
    assert(
        struct_type->creator->reverse_cache.find(struct_type->ast_expression) !=
        struct_type->creator->reverse_cache.end());

    auto params =
        struct_type->creator->reverse_cache[struct_type->ast_expression];

    auto evaled_params = std::vector<Context::Value>();

    for (auto p : params) {
      // TODO not all parameters have to be types (but to be fixed elsewhere)
      // Also, we're wrapping these in a Walue, only to be unwrapped in the
      // CreateOrGetCached method? Hopefully the above generalization will fix
      // this awfulness, and everything will just be a Value.
      evaled_params.push_back(Context::Value(EvalWithVars(p, lookup)));
    }

    return struct_type->creator->CreateOrGetCached(evaled_params).as_type;
  }

  std::cerr << *type << std::endl;
  assert(false);
}

#define STARTING_CHECK                                                         \
  assert(type != Unknown && "Cyclic dependency");                              \
  if (type) { return; }                                                        \
  /*std::cout << "\033[2J\033[1;1H" << std::endl;                                \
  std::cout << *this << std::endl;                                             \
  std::cin.ignore(1);                                                          */\
  type = Unknown

#define ENDING_SHOW                                                            \
  /*std::cout << "\033[2J\033[1;1H" << std::endl;                                \
  std::cout << "COMPLETE:\n" << *this << std::endl;                            \
  std::cin.ignore(1)*/

namespace AST {
// TODO In what file should this be placed?
// TODO this should take a context because flushing it out depends on the
// context.
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
  if (operand->type == Error) {
    type = Error;
    return;
  }

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
  Verify(true);
}

void Access::Verify(bool emit_errors) {
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
      type = Func(/*Ptr(operand->type), */ Uint, Void);
      return;
    }

  } else if (base_type == Type_) {
    if (member_name == "bytes" || member_name == "alignment") {
      if (!operand->value.as_type) { operand->evaluate(); }
      assert(operand->value.as_type);
      type = Uint;
      return;
    }

    auto evaled_type = operand->evaluate().as_type;
    if (evaled_type->is_enum()) {
      auto enum_type = (Enumeration *)evaled_type;
      // If you can get the value,
      if (enum_type->get_value(member_name)) {
        type = operand->evaluate().as_type;

      } else {
        error_log.log(loc, evaled_type->to_string() + " has no member " +
                               member_name + ".");
        type = Error;
      }
      return;
    }
  }

  if (base_type->is_struct()) {
    auto struct_type = static_cast<Structure *>(base_type);
    struct_type->ast_expression->FlushOut();

    auto member_type = struct_type->field(member_name);
    if (member_type) {
      type = member_type;

    } else {
      if (emit_errors) {
        error_log.log(loc, "Objects of type " + base_type->to_string() +
                               " have no member named `" + member_name + "`.");
      }
      type = Error;
    }
  }

  if (base_type->is_primitive() || base_type->is_array() ||
      base_type->is_function()) {
    if (emit_errors) {
      error_log.log(loc, base_type->to_string() + " has no field named '" +
                             member_name + "'.");
    }
    type = Error;
    return;
  }

  assert(type && "type is nullptr in access");
}

struct MatchData {
  Type *match;
  Expression *expr;
  std::string err;
  MatchData(Type *t = nullptr, Expression *e = nullptr) : match(t), expr(e) {}
};

static void AddToPotentialCallInterpretations(
    Expression *expr, std::vector<MatchData> &potential_match_options) {
  if (expr->type->is_quantum()) {
    // If the LHS has a quantum type, test all possibilities to see which
    // one works. Verify that exactly one works.
    for (auto opt : static_cast<QuantumType *>(expr->type)->options) {
      assert(opt->is_function());
      // TODO better line number logging.
      potential_match_options.emplace_back(opt, expr);
    }

  } else if (expr->type->is_function() || expr->type == Type_) {
    // Assert that if it's a type, then it's a parametric struct.
    assert(expr->type != Type_ || expr->value.as_type->is_parametric_struct());

    potential_match_options.emplace_back(expr->type, expr);
  } else {
    assert(false);
  }
}

void Binop::verify_types() {
  STARTING_CHECK;

  if (op == Language::Operator::Call && lhs->is_access()){
    // This has a lot in common with rhs access
    auto lhs_access = (Access *)lhs;
    lhs_access->Verify(false);

    // If the field doesn't exist, it's meant to be UFCS. Modify the AST to make
    // that correct.
    if (lhs_access->type == Error && lhs_access->operand->type != Error) {
      auto new_lhs =
          scope_->IdentifierBeingReferencedOrNull(lhs_access->member_name);
      if (!new_lhs) {
        assert(false); // TODO log error
      }

      // TODO What if it's indirected >= 2 times? This only deals with 0 or 1
      // indirections
      Expression *ufcs_ptr;
      if (lhs_access->operand->type->is_pointer()) {
        ufcs_ptr = lhs_access->operand;
      } else {
        auto unop     = new Unop;
        unop->op      = Language::Operator::And;
        unop->operand = lhs_access->operand;
        unop->type    = Ptr(unop->operand->type);
        ufcs_ptr      = unop;
        // TODO line number?
      }

      ChainOp* new_rhs;
      if (!rhs) {
        rhs = ufcs_ptr;
      } else {
        if (rhs->is_comma_list()) {
          auto rhs_chainop = (ChainOp *)rhs;
          rhs_chainop->ops.push_back(Language::Operator::Comma);
          rhs_chainop->exprs.insert(rhs_chainop->exprs.begin(), ufcs_ptr);
          new_rhs = rhs_chainop;
        } else {
          // TODO line number?
          new_rhs = new ChainOp;
          new_rhs->ops.push_back(Language::Operator::Comma);
          new_rhs->exprs.push_back(ufcs_ptr);
          new_rhs->exprs.push_back(rhs); // Pointer to rhs?
        }

        rhs = new_rhs;
      }
      lhs = new_lhs;
    }
  }

  lhs->verify_types();

  if (rhs) {
    rhs->verify_types();
  } else {
    // If rhs == 0x0, this means it's the unary call operator.
    assert(op == Language::Operator::Call);

    // TODO quantum types
    if (!lhs->type->is_function()) {
      error_log.log(loc, "Operand is not a function.");
      type = Error;
      return;
    }

    auto fn = static_cast<Function *>(lhs->type);
    if (fn->input != Void) {
      error_log.log(loc, "Calling function with no arguments.");
      type = Error;
    } else {
      type = fn->output;
      assert(type && "fn return type is nullptr");
    }
    return;
  }

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
    std::vector<std::map<TypeVariable *, Type *>> match_vec;

    std::vector<MatchData> potential_match_options;

    if (lhs->is_identifier()) {
      auto id_token = static_cast<AST::Identifier *>(lhs)->token;

      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdentifierHereOrNull(id_token);
        if (!id_ptr) { continue; }

        AddToPotentialCallInterpretations(id_ptr, potential_match_options);
      }

    } else {
      AddToPotentialCallInterpretations(lhs, potential_match_options);
    }

    MatchData matched_data;
    bool has_vars_flag = false;

    for (auto& opt : potential_match_options) {
      std::map<TypeVariable *, Type *> matches;
      // Receiving either a function or a parametric struct as the opt.match
      // parameter
      if (opt.match->is_function()) {
        if (MatchCall(static_cast<Function *>(opt.match)->input, rhs->type,
                      matches, opt.err)) {

          match_vec.push_back(matches);
          matched_data = opt;

          has_vars_flag = matched_data.expr->type->has_vars;
          if (matched_data.expr->is_identifier()) {
            // TODO why is this condition necessary?
            has_vars_flag &=
                !static_cast<Identifier *>(matched_data.expr)->is_arg;
          }
        }
      } else {
        assert(opt.expr->value.as_type->is_parametric_struct());
        match_vec.push_back(matches);
        matched_data  = opt;
        has_vars_flag = false; // TODO FIXME WHAT SHOULD THIS BE?!
      }
    }

    if (match_vec.size() != 1) {
      type = Error;

      if (match_vec.empty()) {
        std::stringstream msg;
        msg << "No function overload matches call.\n";
        for (const auto& pmo : potential_match_options) {
          // TODO stringstream
          // TODO file if it's not in the same file.
          msg << "      "
              << "Line " << pmo.expr->loc.line_num << ": " << pmo.err;
          /*
                 static_cast<Function *>(pmo.match)->input->to_string() +
                 " vs. " + rhs->type->to_string() + " on line " +
                 std::to_string(pmo.expr->loc.line_num);
                 */
        }
        error_log.log(loc, msg.str());
      } else {
        error_log.log(loc, "Multiple function overloads match call.");
      }
      return;
    }

    auto evaled_type = EvalWithVars(matched_data.match, match_vec[0]);

    if (evaled_type->is_function()) {
      type = static_cast<Function *>(evaled_type)->output;

      if (has_vars_flag) {
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
    type = rhs->evaluate().as_type;
    if (type == Error) { return; }
    assert(type && "cast to nullptr?");

    if (lhs->type == type ||
        (lhs->type == Bool && (type == Int || type == Uint || type == Real)) ||
        (lhs->type == Int && type == Real) ||
        (lhs->type == Int && type == Uint) ||
        (lhs->type == Uint && type == Real) ||
        (lhs->type == Uint && type == Int)) {
      return;
    }

    if (lhs->type->is_pointer() && type->is_pointer()) { return; }

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
      error_log.log(loc, "No known range construction for types " +
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
  default: { assert(false); }
  }
}

void ChainOp::verify_types() {
  STARTING_CHECK;
  for (auto e : exprs) { e->verify_types(); }

  if (is_comma_list()) {
    // If the tuple consists of a list of types, it should be interpretted as a
    // type itself rather than a tuple. This is a limitation in your support of
    // full tuples.
    bool all_types = true;

    std::vector<Type *> type_vec(exprs.size(), nullptr);

    size_t position = 0;
    for (const auto &eptr : exprs) {
      type_vec[position] = eptr->type;
      all_types &= (eptr->type == Type_);
      ++position;
    }
    type = all_types ? Type_ : Tup(type_vec);
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
    auto t = container->evaluate().as_type;
    if (t->is_enum()) { type = t; }

  } else {
    error_log.log(loc, "Cannot determine type from in declaration.");
    type = Error;
  }

  identifier->type = type;
}

// TODO Declaration is responsible for the type verification of it's identifier?
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
    type = expr->evaluate().as_type;
    if (type->is_struct() /* TODO && !type->has_vars */) {
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
        expr->evaluate(); // TODO do we need to evaluate here?
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
  ENDING_SHOW;
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
  bool input_has_vars = false;
  for (auto in : inputs) {
    in->verify_types();
    input_has_vars |= in->type->has_vars;
  }

  if (!input_has_vars) { VerificationQueue.push(statements); }

  return_type_expr->verify_types();

  Type *ret_type = return_type_expr->evaluate().as_type;
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
  ENDING_SHOW;
}

void Case::verify_types() {
  STARTING_CHECK;
  for (auto kv : key_vals) {
    kv.first->verify_types();
    kv.second->verify_types();
  }

  std::set<Type *> value_types;

  for (auto &kv : key_vals) {
    if (kv.first->type == Error) {
      kv.first->type = Bool;
      if (kv.second->type == Error) { continue; }

    } else if (kv.first->type != Bool) {
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
    if (!value_types.empty()) {
      error_log.log(loc, "Type error: Values do not match in key-value pairs");
    }
    type = Error;
  } else {
    type = *value_types.begin();
  }
  ENDING_SHOW;
}

void Statements::verify_types() {
  for (auto stmt : statements) { stmt->verify_types(); }

  // TODO Verify that a return statement, if present, is the last thing
}

void While::verify_types() {
  condition->verify_types();
  statements->verify_types();

  if (condition->type == Error) { return; }

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
    if (cond->type == Error) { continue; }
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
