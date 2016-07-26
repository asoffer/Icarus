#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

#include "IR/Stack.h"

extern IR::Value Evaluate(AST::Expression *expr);
extern std::queue<AST::Node *> VerificationQueue;
extern Type *GetFunctionTypeReferencedIn(Scope *scope,
                                         const std::string &fn_name,
                                         Type *input_type);

extern AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr);
extern std::stack<Scope *> ScopeStack;

static Scope *CurrentScope() {
  return ScopeStack.empty() ? nullptr : ScopeStack.top();
}

static AST::Declaration *
GenerateSpecifiedFunctionDecl(const std::string &name,
                              AST::FunctionLiteral *fn_lit,
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

  auto old_stack_size = ScopeStack.size();
  ScopeStack.push(fn_lit->scope_);

  auto new_id      = new AST::Identifier(fn_lit->loc, name);
  auto decl        = new AST::Declaration;
  new_id->decl     = decl;
  new_id->type     = cloned_func->type;
  new_id->scope_   = fn_lit->scope_;
  decl->loc        = fn_lit->loc;
  decl->scope_     = fn_lit->scope_;
  decl->identifier = new_id;
  decl->init_val   = cloned_func;
  decl->alloc      = nullptr;
  decl->stack_loc  = IR::Value(~0ul);
  decl->arg_val    = nullptr;

  // We don't want to run decl->assign_scope() because that automatically adds
  // it to the scopes DeclRegistry. This will mean it can be looked up in type
  // verification. We want this function to be matched by its generic form and
  // then looked up in the cache rather than matching outright.

  decl->scope_= CurrentScope();
  decl->identifier->assign_scope();
  if (decl->type_expr) { decl->type_expr->assign_scope(); }
  if (decl->init_val) { decl->init_val->assign_scope(); }
  decl->verify_types();

  ScopeStack.pop();
  assert(ScopeStack.size() == old_stack_size);

  delete[] lookup_key;
  delete[] lookup_val;

  return decl;
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
    assert(lhs_var->test->type == Func(Type_, Bool));

    // Do a function call
    auto local_stack = new IR::LocalStack;
    auto f = lhs_var->test->EmitIR();
    assert(f.flag == IR::ValType::F);
    auto test_result = f.as_func->Call(local_stack, {IR::Value(rhs)});
    delete local_stack;
    assert(test_result.flag == IR::ValType::B);

    if (test_result.as_bool) {
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
        error_message +=
            " (See line " + std::to_string(id_test->decl->loc.line_num) + ")";
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

    return MatchCall(((Pointer *)lhs)->pointee, ((Pointer *)rhs)->pointee,
                     matches, error_message);
  }

  if (lhs->is_array()) {
    if (!rhs->is_array()) {
      error_message +=
          "Expected array, but received" + rhs->to_string() + ".\n";
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

    auto lhs_in  = ((Function *)lhs)->input;
    auto rhs_in  = ((Function *)rhs)->input;
    auto lhs_out = ((Function *)lhs)->output;
    auto rhs_out = ((Function *)rhs)->output;

    return MatchCall(lhs_in, rhs_in, matches, error_message) &&
           MatchCall(lhs_out, rhs_out, matches, error_message);
  }

  if (lhs->is_struct()) {
    if (!rhs->is_struct()) { return false; }

    auto lhs_struct = (Structure *)lhs;
    auto rhs_struct = (Structure *)rhs;

    // We know that LHS has a creator because it has variables. Thus, passing
    // this test means that these are instances of the same parametric struct.
    if (lhs_struct->creator != rhs_struct->creator) { return false; }

    auto lhs_params =
        lhs_struct->creator->reverse_cache[lhs_struct->ast_expression];
    auto rhs_params =
        rhs_struct->creator->reverse_cache[rhs_struct->ast_expression];
    if (lhs_params.size() != rhs_params.size()) { return false; }

    auto num_params       = lhs_params.size();
    bool coherent_matches = true;
    for (size_t i = 0; i < num_params; ++i) {
      // TODO what if params aren't types
      coherent_matches &= MatchCall(
          lhs_params[i].as_type, rhs_params[i].as_type, matches, error_message);
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

  UNREACHABLE;
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

  std::cerr << *type << std::endl;
  UNREACHABLE;
}

#define STARTING_CHECK                                                         \
  assert(type != Unknown && "Cyclic dependency");                              \
  if (type) { return; }                                                        \
  type = Unknown

#define VERIFY_AND_RETURN_ON_ERROR(expr)                                       \
  do {                                                                         \
    expr->verify_types();                                                      \
    if (expr->type == Err) {                                                   \
      type = Err;                                                              \
      return;                                                                  \
    }                                                                          \
  } while (false)

namespace AST {
// TODO In what file should this be placed?
// TODO this should take a context because flushing it out depends on the
// context.
void StructLiteral::CompleteDefinition() {
  assert(value.as_type && value.as_type->is_struct());

  auto tval = (Structure *)value.as_type;
  if (!tval->field_num_to_name.empty()) { return; }

  for (size_t i = 0; i < decls.size(); ++i) {
    decls[i]->verify_types();

    Type *decl_type;
    if (decls[i]->type_expr) {
      if (decls[i]->type_expr->type == Err ||
          decls[i]->type_expr->type == Void ||
          decls[i]->type_expr->type->is_parametric_struct()) {
        decl_type = Err;
      } else {
        decl_type = Evaluate(decls[i]->type_expr).as_type;
      }
    } else {
      decl_type = decls[i]->init_val->type;
    }

    tval->insert_field(decls[i]->identifier->token, decl_type,
                       decls[i]->init_val);
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

#define LOOP_OVER_DECLS_FROM(s, d)                                             \
  for (auto scope_ptr = s; scope_ptr; scope_ptr = scope_ptr->parent)           \
    for (auto d : scope_ptr->DeclRegistry)

void Identifier::verify_types() {
  STARTING_CHECK;

  if (decl) {
    decl->verify_types();
    type = decl->type;
    return;
  }

  std::vector<Declaration *> potential_decls;

  LOOP_OVER_DECLS_FROM(scope_, d) {
    if (token != d->identifier->token) { continue; }
    potential_decls.push_back(d);
  }

  if (potential_decls.empty()) {
    type = Err;
    Error::Log::UndeclaredIdentifier(loc, token.c_str());
    return;
  }

  if (potential_decls.size() > 1) {
    type = Err;
    Error::Log::AmbiguousIdentifier(loc, token.c_str());
    return;
  }

  decl = potential_decls[0];
  decl->verify_types();
  type = decl->type;

  // You are allowed to capture, functions, globals, and const objects
  // TODO what about #const pointers?
  if (type == Type_ || type->is_function() || decl->scope_ == Scope::Global ||
      decl->HasHashtag("const")) {
    return;
  }

  for (auto scope_ptr = scope_; scope_ptr != potential_decls[0]->scope_;
       scope_ptr = scope_ptr->parent) {
    if (scope_ptr->is_function_scope()) {
      Error::Log::InvalidCapture(loc, decl);
      return;
    }
  }
}

void Unop::verify_types() {
  STARTING_CHECK;

  VERIFY_AND_RETURN_ON_ERROR(operand);

  using Language::Operator;
  switch (op) {
  case Operator::Eval: {
    type = operand->type;
  } break;
  case Operator::Free: {
    if (!operand->type->is_pointer()) {
      std::string msg = "Attempting to free an object of type `" +
                        operand->type->to_string() + "`.";
      Error::Log::UnopTypeFail(msg, this);
    }
    type = Void;
  } break;
  case Operator::Print: {
   if (operand->type == Void) {
     Error::Log::UnopTypeFail(
         "Attempting to print an expression with type `void`.", this);
   }
    type = Void;
  } break;
  case Operator::Return: {
    if (operand->type == Void) {
      Error::Log::UnopTypeFail("Attempting to return an expression which has type `void`.", this);
    }

    type = Void;
  } break;
  case Operator::At: {
    if (operand->type->is_pointer()) {
      type = ((Pointer *)operand->type)->pointee;

    } else {
      std::string msg = "Attempting to dereference an expression of type `" +
                        operand->type->to_string() + "`.";
      Error::Log::UnopTypeFail(msg, this);
      type = Err;
    }
  } break;
  case Operator::And: {
    type = (operand->type == Type_) ? Type_ : Ptr(operand->type);
    assert(type && "&type is null");
  } break;
  case Operator::Sub: {
    if (operand->type == Uint) {
      Error::Log::UnopTypeFail(
          "Attempting to negate an unsigned integer (uint).", this);
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
        type = ((Function *)t)->output;
      } else {
        Error::Log::UnopTypeFail("Type `" + operand->type->to_string() +
                                     "` has no unary negation operator.",
                                 this);
        type = Err;
      }

    } else {
      Error::Log::UnopTypeFail("Type `" + operand->type->to_string() +
                                   "` has no unary negation operator.",
                               this);
      type = Err;
    }
  } break;
  case Operator::Dots: {
    if (operand->type == Uint || operand->type == Int ||
        operand->type == Char) {
      type = Range(operand->type);
    } else {

      Error::Log::Log(loc, type->to_string() + " cannot be part of a range");
      type = Err;
    }
  } break;
  case Operator::Not: {
    if (operand->type == Bool) {
      type = Bool;
    } else {
      Error::Log::UnopTypeFail("Attempting to apply the logical negation "
                               "operator (!) to an expression of type `" +
                                   operand->type->to_string() + "`.",
                               this);
      type = Err;
    }
  } break;
  case Operator::Import: {
    type = Void;
  } break;
  default: UNREACHABLE;
  }
}

void Access::verify_types() {
  STARTING_CHECK;
  Verify(true);
}

void Access::Verify(bool emit_errors) {
  operand->verify_types();
  VERIFY_AND_RETURN_ON_ERROR(operand);
  auto base_type = operand->type;

  // Access passes through pointers
  while (base_type->is_pointer()) {
    base_type = ((Pointer *)base_type)->pointee;
  }

  if (base_type->is_array()) {
    if (member_name == "size") {
      type = Uint;
      return;
    } else if (member_name == "resize") {
      auto array_base_type = (Array *)base_type;

      if (array_base_type->fixed_length) {
        Error::Log::ResizingFixedArray(loc);
      }

      // TODO Can we have this without a member?
      type = Void;
      return;
    }
  } else if (base_type == Type_) {
    if (member_name == "bytes" || member_name == "alignment") {
      type = Uint;
      return;
    }

    auto evaled_type = Evaluate(operand).as_type;
    if (evaled_type->is_enum()) {
      auto enum_type = (Enumeration *)evaled_type;
      // If you can get the value,
      if (enum_type->get_value(member_name)) {
        type = Evaluate(operand).as_type;

      } else {
        Error::Log::MissingMember(loc, member_name, evaled_type);
        type = Err;
      }
      return;
    }
  }

  if (base_type->is_struct()) {
    auto struct_type = (Structure *)base_type;
    struct_type->ast_expression->CompleteDefinition();

    auto member_type = struct_type->field(member_name);
    if (member_type) {
      type = member_type;

    } else {
      if (emit_errors) {
        Error::Log::MissingMember(loc, member_name, base_type);
      }
      type = Err;
    }
  }

  if (base_type->is_primitive() || base_type->is_array() ||
      base_type->is_function()) {
    if (emit_errors) { Error::Log::MissingMember(loc, member_name, base_type); }
    type = Err;
    return;
  }

  assert(type && "type is nullptr in access");
}

static std::vector<Declaration *> AllDeclsInScopeWithId(Scope *scope,
                                                        const std::string &id) {
  std::vector<Declaration *> matching_decls;
  LOOP_OVER_DECLS_FROM(scope, d) {
    if (d->identifier->token != id) { continue; }
    d->verify_types();
    if (d->type == Err) { continue; }
    matching_decls.push_back(d);
  }
  return matching_decls;
}


void Binop::verify_types() {
  STARTING_CHECK;
  if (op == Language::Operator::Call && lhs->is_access()) {
    // This has a lot in common with rhs access
    auto lhs_access = (Access *)lhs;
    lhs_access->Verify(false);

    // If the field doesn't exist, it's meant to be UFCS. Modify the AST to make
    // that correct.
    if (lhs_access->type == Err && lhs_access->operand->type != Err) {

      // TODO log Error in this case
      auto ufcs_func =
          scope_->IdentifierBeingReferencedOrNull(lhs_access->member_name);
      assert(ufcs_func);

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

      ChainOp *new_rhs;
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
      lhs = new Identifier(ufcs_func->loc, lhs_access->member_name);
    }
  }

  if (op == Language::Operator::Call) {
    std::string err_msg; // TODO for now we mostly ignore this.
    std::map<TypeVariable *, Type *> matches;

    if (lhs->is_identifier()) {
      auto lhs_id = (Identifier *)lhs;
      auto matching_decls = AllDeclsInScopeWithId(scope_, lhs_id->token);
      if (rhs) { VERIFY_AND_RETURN_ON_ERROR(rhs); }

      // Look for valid matches by looking at any declaration which has a
      // matching token.
      std::vector<Declaration *> valid_matches;
      for (auto decl : matching_decls) {
        if (decl->type->is_function()) {
          auto fn_type = (Function *)decl->type;
          // If there is no input, and the function takes Void as its input, or
          // if the types just match, then add it to your list of matches.
          if ((!rhs && fn_type->input == Void) ||
              MatchCall(fn_type->input, rhs->type, matches, err_msg)) {
            valid_matches.emplace_back(decl);
          }
        } else {
          assert(decl->type == Type_);

          if (decl->IsInferred() || decl->IsCustomInitialized()) {
            if (decl->init_val->type->is_function()) {
              decl->value = Context::Value(decl->init_val);
            } else {
              decl->value = Context::Value(Evaluate(decl->init_val).as_type);

              if (decl->init_val->is_struct_literal()) {
                assert(decl->identifier->value.as_type->is_struct());
                ((Structure *)(decl->identifier->value.as_type))
                    ->set_name(decl->identifier->token);

              } else if (decl->init_val->is_parametric_struct_literal()) {
                assert(decl->identifier->value.as_type->is_parametric_struct());
                ((ParametricStructure *)(decl->identifier->value.as_type))
                    ->set_name(decl->identifier->token);

              } else if (decl->init_val->is_enum_literal()) {
                assert(decl->identifier->value.as_type->is_enum());
                ((Enumeration *)(decl->identifier->value.as_type))->bound_name =
                    decl->identifier->token;
              }
            }

          } else if (decl->IsUninitialized()) {
            NOT_YET;
          }

          auto decl_type = decl->value.as_type;

          if (!decl_type->is_parametric_struct()) { continue; }
          auto param_expr =
              ((ParametricStructure *)decl_type)->ast_expression;

          // Get the types of parameter entries
          std::vector<Type *> param_types;
          for (auto p : param_expr->params) { param_types.push_back(p->type); }
          Type *param_type =
              param_types.size() == 1 ? param_types[0] : Tup(param_types);

          // Get the input types we're trying to match
          std::vector<Type *> input_type_vec;
          Type *input_type = nullptr;
          if (rhs->is_chain_op()) {
            for (auto elem : ((ChainOp *)rhs)->exprs) {
              input_type_vec.push_back(elem->type);
            }
            input_type = Tup(input_type_vec);
          } else {
            input_type = rhs->type;
          }

          if (MatchCall(param_type, input_type, matches, err_msg)) {
            valid_matches.emplace_back(decl);
          }
        }
      } // End of decl loop

      if (valid_matches.size() != 1) {
        UNREACHABLE;
        Error::Log::Log(loc, valid_matches.empty() ? "No valid matches"
                                                   : "Ambiguous call");
        type = lhs->type = Err;
        return;
      }

      lhs->type = valid_matches[0]->type;
      ((Identifier *)lhs)->decl = valid_matches[0];

    } else {
      VERIFY_AND_RETURN_ON_ERROR(lhs);
      if (rhs) { VERIFY_AND_RETURN_ON_ERROR(rhs); }

      if (lhs->type->is_function()) {
        if (!MatchCall(((Function *)lhs->type)->input, (rhs ? rhs->type : Void),
                       matches, err_msg)) {
          Error::Log::Log(loc, err_msg);
          type      = Err;
          lhs->type = Err;
          return;
        }
      } else {
        assert(lhs->type == Type_ &&
               "Should have caught the bad-type of lhs earlier");

        if (lhs->value.as_type->is_parametric_struct()) {
          auto param_ast_expr =
              ((ParametricStructure *)lhs->value.as_type)->ast_expression;

          // Get the types of parameter entries
          std::vector<Type *> param_type_vec;
          for (auto p : param_ast_expr->params) {
            param_type_vec.push_back(p->type);
          }
          Type *param_type = param_type_vec.size() == 1 ? param_type_vec[0]
                                                        : Tup(param_type_vec);

          VERIFY_AND_RETURN_ON_ERROR(rhs);

          // TODO can you have a parametric struct taking no arguments? If so,
          // rhs could be empty and we have a bug!

          // Get the input types we're trying to match
          std::vector<Type *> input_type_vec;
          Type *input_type = nullptr;
          if (rhs->is_chain_op()) {
            for (auto elem : ((ChainOp *)rhs)->exprs) {
              input_type_vec.push_back(elem->type);
            }
            input_type = Tup(input_type_vec);
          }
          input_type = rhs->type;

          if (!MatchCall(param_type, input_type, matches, err_msg)) {
            Error::Log::Log(loc, err_msg);
            type      = Err;
            lhs->type = Err;
            return;
          }
        } else {
          Error::Log::Log(loc, "Object is not callable");
          if (rhs) { rhs->verify_types(); }
          type = Err;
        }
      }
    }

    if (rhs) { VERIFY_AND_RETURN_ON_ERROR(rhs); }

    // If you get here, you know the types all match. We just need to compute
    // the type of the call.
    if (lhs->type->is_function()) {
      auto evaled_type = EvalWithVars(lhs->type, matches);
      type             = ((Function *)evaled_type)->output;

      // Generate if you need to
      if (lhs->type->has_vars) {
        auto fn_expr = GetFunctionLiteral(lhs);

        auto in_type = ((Function *)evaled_type)->input;
        for (auto &cached_fn : fn_expr->cache) {
          if (cached_fn.first != in_type) { continue; }
          if (lhs->is_identifier()) {
            ((Identifier *)lhs)->decl = cached_fn.second;
          }
          return;
        }

        // If you have variables, the input cannot be void.
        assert(rhs);

        // If you can't find it in the cache, generate it.
        if (lhs->is_identifier()) {
          auto lhs_id  = (Identifier *)lhs;
          lhs_id->decl = fn_expr->cache[rhs->type] =
              GenerateSpecifiedFunctionDecl(lhs_id->token, fn_expr, matches);
          lhs_id->type = lhs_id->decl->type; // TODO I need to do something like
                                             // this, but maybe this needs to be
                                             // done more generally?
        } else {
          fn_expr->cache[rhs->type] =
              GenerateSpecifiedFunctionDecl("anon-fn", fn_expr, matches);
        }

      }

    } else {
      assert(lhs->type == Type_ &&
             "Should have caught the bad-type of lhs earlier");

      type = EvalWithVars(lhs->type, matches);
    }

    assert(type);
    assert(type != Unknown);
    return;
  }

  assert(rhs);
  lhs->verify_types();
  rhs->verify_types();
  if (lhs->type == Err || rhs->type == Err) { type = Err; return; }

  using Language::Operator;
  // TODO if lhs is reserved?
  if (op == Language::Operator::Assign) {
    if (rhs->is_terminal()) {
      auto term = (Terminal *)rhs;
      if (term->terminal_type == Language::Terminal::Null) {
        term->type = lhs->type;
        type       = Void;
        return;
      }

      if (term->terminal_type == Language::Terminal::Hole) {
        // TODO this should become a noop
        term->type = lhs->type;
        type       = Void;
        // if (lhs->is_declaration()) { ((Declaration *)lhs)->init = false; }

        return;
      }
    }

    if (lhs->type != rhs->type) {
      if (lhs->type->is_array() && rhs->type->is_array()) {
        auto lhs_array_type = (Array *)lhs->type;
        auto rhs_array_type = (Array *)rhs->type;
        if (lhs_array_type->data_type != rhs_array_type->data_type) {
          Error::Log::Log(
              loc,
              "Invalid assignment. Data in arrays are of different types.");
        } else if (lhs_array_type->fixed_length &&
                   rhs_array_type->fixed_length) {
          Error::Log::Log(loc,
                        "Invalid assignment. Arrays are of different lengths.");

        } else if (lhs_array_type->fixed_length) {
          Error::Log::Log(loc, "Invalid assignment. Array on right-hand side has "
                             "unknown length, but lhs is known to be of "
                             "length " +
                                 std::to_string(lhs_array_type->len));
        } else {
          assert(rhs_array_type->fixed_length);
          return;
        }

      } else {
        Error::Log::Log(loc, "Invalid assignment. Left-hand side has type " +
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
    // TODO rocket encountered outside case statement.
    UNREACHABLE;
  } break;
  case Operator::Index:
    type = Err;
    if (!lhs->type->is_array()) {
      if (rhs->type->is_range()) {
        Error::Log::SlicingNonArray(loc, lhs->type);
      } else {
        Error::Log::IndexingNonArray(loc, lhs->type);
      }
    } else if (rhs->type->is_range()) {
      type = Slice((Array *)lhs->type);
      break;
    } else {
      type = ((Array *)lhs->type)->data_type;
      assert(type && "array data type is nullptr");
      // TODO allow slice indexing
      if (rhs->type == Int || rhs->type == Uint) { break; }
      Error::Log::NonIntegralArrayIndex(loc, rhs->type);
    }
    return;
  case Operator::Cast: {
    type = Evaluate(rhs).as_type;
    if (type == Err) { return; }
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

    Error::Log::InvalidCast(loc, lhs->type, type);
  } break;
  case Operator::Dots: {
    if (lhs->type == Int && rhs->type == Int) {
      type = Range(Int);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      type = Range(Uint);

    } else if (lhs->type == Char && rhs->type == Char) {
      type = Range(Char);

    } else {
      Error::Log::Log(loc, "No known range construction for types " +
                             lhs->type->to_string() + " .. " +
                             rhs->type->to_string());
    }
  } break;
  case Language::Operator::XorEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      Error::Log::Log(loc, "Operator ^= must take arguments of type bool");
    }
  } break;
  case Language::Operator::AndEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      Error::Log::Log(loc, "Operator &= must take arguments of type bool");
    }
  } break;
  case Language::Operator::OrEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      Error::Log::Log(loc, "Operator |= must take arguments of type bool");
    }
  } break;

#define CASE(OpName, op_name, symbol, ret_type)                                \
  case Language::Operator::OpName: {                                           \
    if ((lhs->type == Int && rhs->type == Int) ||                              \
        (lhs->type == Uint && rhs->type == Uint) ||                            \
        (lhs->type == Real && rhs->type == Real)) {                            \
      type = ret_type;                                                         \
    } else {                                                                   \
      /* Store a vector containing the valid matches */                        \
      std::vector<Declaration *> matched_op_name;                              \
                                                                               \
      /* TODO this linear search is probably not ideal.   */                   \
      LOOP_OVER_DECLS_FROM(scope_, decl) {                                     \
        if (decl->identifier->token == "__" op_name "__") {                    \
          decl->verify_types();                                                \
          matched_op_name.push_back(decl);                                     \
        }                                                                      \
      }                                                                        \
                                                                               \
      Declaration *correct_decl = nullptr;                                     \
      for (auto decl : matched_op_name) {                                      \
        if (!decl->type->is_function()) { continue; }                          \
        auto fn_type = (Function *)decl->type;                                 \
        if (fn_type->input != Tup({lhs->type, rhs->type})) { continue; }       \
        /* If you get here, you've found a match. Hope there is only one       \
         * TODO if there is more than one, log them all and give a good        \
         * *error message. For now, we just fail */                            \
        if (correct_decl) {                                                    \
          Error::Log::Log(loc, "Already found a match for operator `" symbol   \
                               "` with types " +                               \
                                   lhs->type->to_string() + " and " +          \
                                   rhs->type->to_string());                    \
          type = Err;                                                          \
        } else {                                                               \
          correct_decl = decl;                                                 \
        }                                                                      \
      }                                                                        \
      if (!correct_decl) {                                                     \
        type = Err;                                                            \
        Error::Log::Log(loc, "No known operator overload for `" symbol         \
                             "` with types " +                                 \
                                 lhs->type->to_string() + " and " +            \
                                 rhs->type->to_string());                      \
      } else if (type != Err) {                                                \
        type = ((Function *)correct_decl->type)->output;                       \
      }                                                                        \
    }                                                                          \
  } break;

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
        type = Err;
        Error::Log::Log(loc, "Functions cannot be composed.");
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
        type = ((Function *)fn_type)->output;
      } else {
        type = Err;
        Error::Log::Log(loc, "No known operator overload for `*` with types " +
                               lhs->type->to_string() + " and " +
                               rhs->type->to_string());
      }
    }
  } break;
  case Operator::Arrow: {
    if (lhs->type != Type_) {
      type = Err;
      Error::Log::Log(loc, "From-type for a function must be a type.");
    }
    if (rhs->type != Type_) {
      type = Err;
      Error::Log::Log(loc, "To-type for a function must be a type.");
    }

    if (type != Err) { type = Type_; }

  } break;
  default: UNREACHABLE;
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

    Error::Log::Log(loc, ss.str());
    type = Err;
  }
}

void Generic::verify_types() {
  STARTING_CHECK;
  test_fn->verify_types();

  bool has_err = false;

  if (!test_fn->type->is_function()) {
    // TODO Need a way better
    Error::Log::Log(loc,
                  "Cannot generate a type where the tester is not a function");
    type             = Err;
    identifier->type = Err;
    return;
  }

  auto test_func_type = (Function *)(test_fn->type);
  if (test_func_type->output != Bool) {
    // TODO What about implicitly cast-able to bool via a user-defined cast?
    Error::Log::Log(loc, "Test function must return a bool");
    type    = Err;
    has_err = true;
  }

  if (test_func_type->input != Type_) {
    // TODO will this always be true?
    Error::Log::Log(loc, "Test function must take a type");
    type    = Err;
    has_err = true;
  }

  if (!has_err) { type = Type_; }
  identifier->type = type;
}

void InDecl::verify_types() {
  STARTING_CHECK;
  container->verify_types();

  if (container->type == Void) {
    type             = Err;
    identifier->type = Err;
    Error::Log::Log(loc, "Cannot iterate over a void type.");
    return;
  }

  if (container->type->is_array()) {
    type = ((Array *)container->type)->data_type;

  } else if (container->type->is_slice()) {
    type = ((SliceType *)container->type)->array_type->data_type;

  } else if (container->type->is_range()) {
    type = ((RangeType *)container->type)->end_type;

  } else if (container->type == Type_) {
    auto t = Evaluate(container).as_type;
    if (t->is_enum()) { type = t; }

  } else {
    Error::Log::Log(loc, "Cannot determine type from in declaration.");
    type = Err;
  }

  identifier->type = type;
}

Type *Expression::VerifyTypeForDeclaration(const std::string &id_tok) {
  assert(type && type != Unknown);

  if (type != Type_) {
    Error::Log::NotAType(loc, id_tok);
    return Err;
  }

  Type *t = Evaluate(this).as_type;

  if (t == Void) {
    Error::Log::DeclaredVoidType(loc, id_tok);
    return Err;
  } 
  
  if (t->is_parametric_struct()) {
    Error::Log::DeclaredParametricType(loc, id_tok);
    return Err;
  }

  return t;
}

// TODO refactor this and VerifyTypeForDeclaration because they have extreme
// commonalities.
Type *Expression::VerifyValueForDeclaration(const std::string &id_tok) {
  assert(type && type != Unknown);

  if (type == Void) {
    Error::Log::Log(loc, "Identifier being declared as having void type.");
    return Err;

  } else if (type->is_parametric_struct()) {
    // TODO is this actually what we want?
    Error::Log::Log(loc,
                  "Identifier being declared as having a parametric type.");
    return Err;
  }
  return type;
}

static void VerifyDeclarationForMagicPrint(Type *type, const Cursor &loc) {
  if (!type->is_function()) {
    Error::Log::Log(loc, "Print must be defined to be a function.");
    return;
  }

  auto fn_type = (Function *)type;
  if (!fn_type->input->is_struct()) {
    Error::Log::Log(loc, "Cannot define print function for " +
                           fn_type->input->to_string());
  }

  if (fn_type->output != Void) {
    Error::Log::Log(loc, "print function must return void");
  }
}

static void VerifyDeclarationForMagicAssign(Type *type, const Cursor &loc) {
  if (!type->is_function()) {
    Error::Log::Log(loc, "Assign must be defined to be a function");
    return;
  }

  auto fn_type = (Function *)type;
  if (!fn_type->input->is_tuple()) {
    Error::Log::Log(loc, "Cannot define assign function for " +
                           fn_type->input->to_string());
  } else {
    auto in = (Tuple *)(fn_type->input);
    if (in->entries.size() != 2) {
      Error::Log::Log(loc, "Assignment must be a binary operator, but " +
                             std::to_string(in->entries.size()) + "argument" +
                             (in->entries.size() != 1 ? "s" : "") + " given.");
    }
    // TODO more checking.
  }

  if (fn_type->output != Void) {
    Error::Log::Log(loc, "assignment must return void");
  }
}

// TODO Declaration is responsible for the type verification of it's identifier?
// TODO rewrite/simplify
void Declaration::verify_types() {
  STARTING_CHECK;

  if (type_expr) { type_expr->verify_types(); }
  if (init_val) { init_val->verify_types(); }

  // TODO figure out what's going on with this.
  assert(scope_);

  // There are four cases for the form of a declaration.
  //   1. I: T
  //   2. I := V
  //   3. I: T = V
  //   4. I: T = --
  //
  // Here 'I' stands for "identifier". This is the identifier being declared.
  // 'T' stands for "type", the type of the identifier being declared.
  // 'V' stands for "value", the initial value of the identifier being declared.

  if (IsDefaultInitialized()) {
    identifier->type = type =
        type_expr->VerifyTypeForDeclaration(identifier->token);
  } else if (IsInferred()) {
    identifier->type = type =
        init_val->VerifyValueForDeclaration(identifier->token);

    if (type == NullPtr) {
      Error::Log::Log(loc, "Cannot initialize a declaration with 'null'.");
      type = Err;
    }

  } else if (IsCustomInitialized()) {
    identifier->type = type =
        type_expr->VerifyTypeForDeclaration(identifier->token);
    auto t           = init_val->VerifyValueForDeclaration(identifier->token);

    if (type == Err) {
      type             = t;
      identifier->type = t;

      if (type != t) {
        Error::Log::Log(
            loc,
            "Initial value does not have a type that matches declaration.");
      }

    } else if (t == NullPtr) {
      if (type->is_pointer()) {
        identifier->type = type;
        init_val->type   = type;
      } else {
        auto new_type = Ptr(type);
        Error::Log::Log(loc, "Cannot initialize an identifier of type " +
                               type->to_string() +
                               " with null. Did you mean to declare it as " +
                               new_type->to_string() + "?");
        type             = new_type;
        identifier->type = new_type;
        init_val->type   = new_type;
      }
    }

  } else if (IsUninitialized()) {
    type             = type_expr->VerifyTypeForDeclaration(identifier->token);
    identifier->type = type;
    init_val->type   = type;

  } else {
    UNREACHABLE;
  }

  if (type == Err) {
    identifier->type = Err;
    return;
  }

  if (type->is_struct()) {
    ((Structure *)type)->ast_expression->CompleteDefinition();
  }

  // TODO this section is also in Declaration::evaluate. It makes more sense
  // there. You need to decide on how to deal with this. Do you wait to call it?
  // Do you call it here? Probably that one.
  if (type == Type_ && IsInferred()) {
    if (init_val->is_struct_literal()) {
      assert(init_val->value.as_type && init_val->value.as_type->is_struct());
      // Declaration looks like
      //
      // foo := struct { ... }

      // Set the name of the struct.
      // TODO mangle the name correctly (Where should this be done?)
      ((Structure *)(init_val->value.as_type))->set_name(identifier->token);

    } else if (init_val->is_parametric_struct_literal()) {
      // Declarations look like
      //
      // foo := struct (...) { ... }
      assert(init_val->value.as_type &&
             init_val->value.as_type->is_parametric_struct());

      // Set the name of the parametric struct.
      // TODO mangle the name correctly (Where should this be done?)
      ((ParametricStructure *)(init_val->value.as_type))
          ->set_name(identifier->token);

    } else if (init_val->is_enum_literal()) {
      assert(init_val->value.as_type);

      // Set the name of the parametric struct.
      // TODO mangle the name correctly (Where should this be done?)
      ((Enumeration *)(init_val->value.as_type))->bound_name =
          identifier->token;
    }

    identifier->value = init_val->value;
  }

  // TODO determine type of null. In particular, log an error for
  //
  // foo := null
  //
  // and set the type of the null terminal for something like
  //
  // foo: T = null
  //
  // (If T is not a pointer, we should log an error in that case too).

  // If you get here, you can be assured that the type is valid. So we add it to
  // the identifier.

  if (identifier->token == "__print__") {
    VerifyDeclarationForMagicPrint(type, loc);

  } else if (identifier->token == "__assign__") {
    VerifyDeclarationForMagicAssign(type, loc);
  }
}

void ArrayType::verify_types() {
  STARTING_CHECK;
  length->verify_types();
  data_type->verify_types();

  assert(length && data_type->type == Type_);
  type = Type_;

  // TODO have a Hole type primitive.
  if (length->is_terminal() &&
      ((Terminal *)length)->terminal_type == Language::Terminal::Hole) {
    return;
  }

  // TODO change this to just uint
  if (length->type != Int && length->type != Uint) {
    Error::Log::Log(loc, "Array length indexed by non-integral type");
  }
}

void ArrayLiteral::verify_types() {
  STARTING_CHECK;
  for (auto e : elems) { e->verify_types(); }

  if (elems.empty()) {
    type = Err;
    Error::Log::EmptyArrayLit(loc);
    return;
  }

  auto type_to_match = elems.front()->type;
  assert(type_to_match && "type to match is nullptr");
  if (type_to_match == Err) {
    type = Err;
    return;
  }

  type = Arr(type_to_match, elems.size());
  for (const auto &el : elems) {
    if (el->type != type_to_match) {
      Error::Log::Log(loc, "Type error: Array literal must have consistent type");
      type = Err;
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

  Type *ret_type = Evaluate(return_type_expr).as_type;
  assert(ret_type && "Return type is a nullptr");
  Type *input_type;
  size_t num_inputs = inputs.size();
  if (num_inputs == 0) {
    input_type = Void;

  } else if (num_inputs == 1) {
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

  for (auto &kv : key_vals) {
    if (kv.first->type == Err) {
      kv.first->type = Bool;
      if (kv.second->type == Err) { continue; }

    } else if (kv.first->type != Bool) {
      Error::Log::CaseLHSBool(loc, kv.first->loc, kv.first->type);
      kv.first->type = Bool;
    }

    value_types.insert(kv.second->type);
  }

  // TODO guess what type was intended

  if (value_types.size() != 1) {
    if (!value_types.empty()) {
      Error::Log::Log(loc, "Type error: Values do not match in key-value pairs");
    }
    type = Err;
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

  if (condition->type == Err) { return; }

  if (condition->type != Bool) {

    Error::Log::Log(loc, "While loop condition must be a bool, but " +
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
    if (cond->type == Err) { continue; }
    if (cond->type != Bool) {
      Error::Log::Log(loc, "Conditional must be a bool, but " +
                             cond->type->to_string() + " given.");
    }
  }
}

void EnumLiteral::verify_types() {
  static size_t anon_enum_counter = 0;

  type = Type_;

  value = Context::Value(
      Enum("__anon.enum" + std::to_string(anon_enum_counter), this));
  ++anon_enum_counter;
}

void ParametricStructLiteral::verify_types() {
  type = Type_;
  for (auto p : params) { p->verify_types(); }
//  for (auto d : decls) {
//    d->identifier->decl = d;
//    if (d->type_expr) { d->type_expr->verify_types(); }
//  }
}

void StructLiteral::verify_types() {
  type = Type_;
  for (auto d : decls) { VerificationQueue.push(d); }
}

void Jump::verify_types() {
  auto scope_ptr = scope_;
  while (scope_ptr) {
    assert(scope_ptr->is_block_scope());
    auto block_scope_ptr = (BlockScope *)scope_ptr;
    if (block_scope_ptr->type == ScopeType::Function) {
      if (jump_type != JumpType::Return) {
        Error::Log::Log(loc, "statement must be contained inside a loop.");
      }
      return;
    }

    if (block_scope_ptr->is_loop_scope()) {
      scope = block_scope_ptr;
      return;
    }

    scope_ptr = block_scope_ptr->parent;
  }

  UNREACHABLE;
}

void DummyTypeExpr::verify_types() {
  STARTING_CHECK;
  type = Type_;
}
} // namespace AST

#undef VERIFY_AND_RETURN_ON_ERROR
#undef LOOP_OVER_DECLS_FROM
#undef STARTING_CHECK
