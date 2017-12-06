#include "ast.h"

#include <queue>

#include "../error_log.h"
#include "../ir/ir.h"
#include "../scope.h"
#include "../type/type.h"

// TODO catch functions that don't return along all paths.

extern IR::Val Evaluate(AST::Expression *expr);
std::queue<AST::Node *> VerificationQueue;
std::queue<std::pair<Type *, AST::Statements *>> FuncInnardsVerificationQueue;
extern AST::FunctionLiteral *GetFunctionLiteral(AST::Expression *expr);

enum class SourceLocationOrder { Unordered, InOrder, OutOfOrder, Same };
static SourceLocationOrder GetOrder(const TextSpan &lhs, const TextSpan &rhs) {
  if (lhs.source->name != rhs.source->name) {
    return SourceLocationOrder::Unordered;
  }
  if (lhs.start.line_num < rhs.start.line_num) {
    return SourceLocationOrder::InOrder;
  }
  if (lhs.start.line_num > rhs.start.line_num) {
    return SourceLocationOrder::OutOfOrder;
  }
  if (lhs.start.offset < rhs.start.offset) {
    return SourceLocationOrder::InOrder;
  }
  if (lhs.start.offset > rhs.start.offset) {
    return SourceLocationOrder::OutOfOrder;
  }
  return SourceLocationOrder::Same;
}

static std::vector<AST::Identifier *> all_ids;
void VerifyDeclBeforeUsage() {
  for (auto &id : all_ids) {
    if (id->type == Err || id->type == Type_) { continue; }
    if (id->decl->scope_ == Scope::Global) { continue; }
    if (GetOrder(id->decl->span, id->span) == SourceLocationOrder::OutOfOrder) {
      // ErrorLog::DeclOutOfOrder(id->decl, id);
    }
  }
}

static bool CanCastImplicitly(Type *from, Type *to) {
  if (from == to || (from == NullPtr && to->is<Pointer>())) { return true; }

  if (from->is<Array>() && to->is<Array>()) {
    if (to->as<Array>().fixed_length &&
        (!from->as<Array>().fixed_length ||
         to->as<Array>().len != from->as<Array>().len)) {
      return false;
    }
    return CanCastImplicitly(from->as<Array>().data_type,
                             to->as<Array>().data_type);
  }

  return false;
}

#define STARTING_CHECK                                                         \
  ASSERT(scope_, "Need to first call assign_scope()");                         \
  if (type == Unknown) {                                                       \
    ErrorLog::CyclicDependency(this);                                          \
    type = Err;                                                                \
  }                                                                            \
  if (type) { return; }                                                        \
  type = Unknown

#define VERIFY_AND_RETURN_ON_ERROR(expr)                                       \
  do {                                                                         \
    (expr)->verify_types();                                                    \
    if ((expr)->type == Err) {                                                 \
      type = Err;                                                              \
      return;                                                                  \
    }                                                                          \
  } while (false)

namespace AST {
// TODO: This algorithm is sufficiently complicated you should combine it with
// proof of correctness and good explanation of what it does.
static bool
CommonAmbiguousFunctionCall(const std::vector<ArgumentMetaData> &data1,
                            const std::vector<ArgumentMetaData> &data2) {
  // TODO Don't need to reprocess this each time
  std::unordered_map<std::string, size_t> index2;
  for (size_t i = 0; i < data2.size(); ++i) { index2[data2[i].name] = i; }

  std::vector<int> delta_fwd_matches(std::max(data1.size(), data2.size()), 0);
  for (size_t i = 0; i < data1.size(); ++i) {
    auto iter = index2.find(data1[i].name);
    if (iter == index2.end()) { continue; }
    size_t j = iter->second;
    delta_fwd_matches[std::min(i, j)]++;
    delta_fwd_matches[std::max(i, j)]--;
  }

  std::vector<size_t> indices = {0};
  // One useful invariant here is that accumulating delta_fwd_matches always
  // yields a non-negative integer. This is because any subtraction that occurs
  // is always preceeded by an addition.
  size_t accumulator = 0;
  for (size_t i = 0; i < delta_fwd_matches.size(); ++i) {
    if (data1[i].type != data2[i].type) { break; }
    accumulator += delta_fwd_matches[i];
    if (accumulator == 0) { indices.push_back(i + 1); }
  }

  // TODO working backwards through indices should allow you to avoid having to
  // copy index2 each time and redo the same work repeatedly.
  for (auto index : indices) {
    // Everything after this index would have to be named or defaulted. named
    // values that match but with different types would have to be defaulted.
    auto index2_copy = index2;
    for (size_t i = index; i < data1.size(); ++i) {
      auto iter = index2_copy.find(data1[i].name);
      if (iter == index2_copy.end()) {
        if (!data1[i].has_default) { goto next_option; }
      } else {
        size_t j = iter->second;
        // TODO not just equal but if there exists something convertible to
        // both.
        if (data1[i].type != data2[j].type &&
            (!data1[i].has_default || !data2[j].has_default)) {
          goto next_option;
        }

        // These two parameters can both be named explicitly. Remove it from
        // index2_copy so what we're left with are all those elements in the
        // second function which haven't been named by anything in the first.
        index2_copy.erase(iter);
      }
    }

    for (const auto &entry : index2) {
      if (entry.second < index) {
        // Ignore entries which preceed the index where we start using named
        // arguments.
        continue;
      }
      // Each of these must have a default if there's a call to both.
      if (!data2[entry.second].has_default) { goto next_option; }
    }

    return true;

  next_option:;
  }
  return false;
}

bool Shadow(Declaration *decl1, Declaration *decl2) {
  if (!decl1->type->is<Function>() || !decl2->type->is<Function>()) {
    return true;
  }

  // If they're both functions, we have more work to do because we allow
  // overloading so long as there are no ambiguous calls.

  // TODO can we store the data in this format to begin with?
  // TODO I don't need to fully generate code here, just the heading
  // information.
  // TODO check const-decl or not.

  auto *fn1 = Evaluate(decl1->init_val.get()).value.as<IR::Func *>();
  std::vector<Type *> arg_types1 = fn1->type->input->is<Tuple>()
                                       ? fn1->type->input->as<Tuple>().entries
                                       : std::vector<Type *>{fn1->type->input};
  std::vector<ArgumentMetaData> metadata1;
  metadata1.reserve(fn1->args_.size());
  for (size_t i = 0; i < fn1->args_.size(); ++i) {
    metadata1.push_back(
        ArgumentMetaData{/*        type = */ arg_types1[i],
                         /*        name = */ fn1->args_[i].first,
                         /* has_default = */ fn1->args_[i].second != nullptr});
  }

  auto *fn2 = Evaluate(decl2->init_val.get()).value.as<IR::Func *>();
  std::vector<Type *> arg_types2 = fn2->type->input->is<Tuple>()
                                       ? fn2->type->input->as<Tuple>().entries
                                       : std::vector<Type *>{fn2->type->input};
  std::vector<ArgumentMetaData> metadata2;
  metadata2.reserve(fn2->args_.size());
  for (size_t i = 0; i < fn2->args_.size(); ++i) {
    metadata2.push_back(
        ArgumentMetaData{/*        type = */ arg_types2[i],
                         /*        name = */ fn2->args_[i].first,
                         /* has_default = */ fn2->args_[i].second != nullptr});
  }

  return CommonAmbiguousFunctionCall(metadata1, metadata2);
}

void Terminal::verify_types() {}

void Identifier::verify_types() {
  STARTING_CHECK;

  all_ids.push_back(this); // Save this identifier for later checks (see
                           // VerifyDeclBeforeUsage)
  // TODO is it true that decl==nullptr if and only if we haven't yet called
  // verify_types on this identifier? That would make my life much easier

  // 'decl' is the (non-owning) pointer to the declaration node representing
  // where this identifier was declared. If it's null, that means we haven't yet
  // determined where this was declared.
  if (decl == nullptr) {
    auto potential_decls = scope_->AllDeclsWithId(token);
    if (potential_decls.size() != 1) {
      (potential_decls.size() == 0 ? LogError::UndeclaredIdentifier
                                   : LogError::AmbiguousIdentifier)(this);
      type = Err;
      return;
    }

    decl = potential_decls[0];
  }

  type = decl->type;

  // Verify whether or not this identifier was captured validly.

  // TODO the code below here looks wrong to me as of 11/28/17.
  // Compile-time constants may be captured implicitly.
  if (decl->const_) { return; }

  // For everything else we iterate from the scope of this identifier up to the
  // scope in which it was declared checking that along the way that it's a
  // block scope.
  for (auto scope_ptr = scope_; scope_ptr != decl->scope_;
       scope_ptr      = scope_ptr->parent) {
    if (scope_ptr->is<FnScope>()) {
      scope_ptr->as<FnScope>().fn_lit->captures.insert(decl);
    } else if (scope_ptr->is<ExecScope>()) {
      continue;
    } else {
      LogError::ImplicitCapture(this);
      return;
    }
  }
}

void Unop::verify_types() {
  STARTING_CHECK;
  VERIFY_AND_RETURN_ON_ERROR(operand);

  using Language::Operator;
  switch (op) {
  case Operator::Eval: type     = operand->type; break;
  case Operator::Require: type  = Void; break;
  case Operator::Generate: type = Void; break;
  case Operator::Free: {
    if (!operand->type->is<Pointer>()) {
      std::string msg = "Attempting to free an object of type `" +
                        operand->type->to_string() + "`.";
      ErrorLog::UnopTypeFail(msg, this);
    }
    type = Void;
  } break;
  case Operator::Print: {
    if (operand->type == Void) {
      ErrorLog::UnopTypeFail(
          "Attempting to print an expression with type `void`.", this);
    }
    type = Void;
  } break;
  case Operator::Return: {
    if (operand->type == Void) {
      ErrorLog::UnopTypeFail(
          "Attempting to return an expression which has type `void`.", this);
    }

    type = Void;
  } break;
  case Operator::At: {
    if (operand->type->is<Pointer>()) {
      type = operand->type->as<Pointer>().pointee;

    } else {
      std::string msg = "Attempting to dereference an expression of type `" +
                        operand->type->to_string() + "`.";
      ErrorLog::UnopTypeFail(msg, this);
      type = Err;
    }
  } break;
  case Operator::And: {
    type = Ptr(operand->type);
  } break;
  case Operator::Mul: {
    if (operand->type != Type_) {
      ErrorLog::LogGeneric(this->span,
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
    } else {
      type = Type_;
    }
  } break;
  case Operator::Sub: {
    if (operand->type == Uint) {
      ErrorLog::UnopTypeFail("Attempting to negate an unsigned integer (uint).",
                             this);
      type = Int;

    } else if (operand->type == Int || operand->type == Real) {
      type = operand->type;

    } else if (operand->type->is<Struct>()) {
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdHereOrNull("__neg__");
        if (!id_ptr) { continue; }

        id_ptr->verify_types();
      }

      auto t = scope_->FunctionTypeReferencedOrNull("__neg__", operand->type);
      if (t) {
        type = ptr_cast<Function>(t)->output;
      } else {
        ErrorLog::UnopTypeFail("Type `" + operand->type->to_string() +
                                   "` has no unary negation operator.",
                               this);
        type = Err;
      }

    } else {
      ErrorLog::UnopTypeFail("Type `" + operand->type->to_string() +
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
      ErrorLog::InvalidRangeType(span, type);
      type = Err;
    }
  } break;
  case Operator::Not: {
    if (operand->type == Bool) {
      type = Bool;
    } else {
      ErrorLog::UnopTypeFail("Attempting to apply the logical negation "
                             "operator (!) to an expression of type `" +
                                 operand->type->to_string() + "`.",
                             this);
      type = Err;
    }
  } break;
  case Operator::Needs: {
    type = Void;
    if (operand->type != Bool) { LogError::PreconditionNeedsBool(this); }
  } break;
  case Operator::Ensure: {
    type = Void;
    if (operand->type != Bool) { LogError::EnsureNeedsBool(this); }
  } break;
  default: UNREACHABLE(*this);
  }
}

static Type *DereferenceAll(Type *t) {
  while (t->is<Pointer>()) { t = static_cast<Pointer *>(t)->pointee; }
  return t;
}

void Access::verify_types() {
  STARTING_CHECK;
  VERIFY_AND_RETURN_ON_ERROR(operand);

  auto base_type = DereferenceAll(operand->type);
  if (base_type->is<Array>()) {
    if (member_name == "size") {
      type = Uint;
    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = Err;
    }
  } else if (base_type == Type_) {
    if (member_name == "bytes" || member_name == "alignment") {
      type = Uint;
    } else {
      Type *evaled_type = Evaluate(operand.get()).value.as<Type *>();
      if (evaled_type->is<Enum>()) {
        // Regardless of whether we can get the value, it's clear that this is
        // supposed to be a member so we should emit an error but carry on
        // assuming that this is an element of that enum type.
        type = evaled_type;
        if (ptr_cast<Enum>(evaled_type)->IndexOrFail(member_name) ==
            std::numeric_limits<size_t>::max()) {
          ErrorLog::MissingMember(span, member_name, evaled_type);
        }
      }
    }
  } else if (base_type->is<Struct>()) {
    auto struct_type = static_cast<Struct *>(base_type);
    struct_type->CompleteDefinition();

    auto member_type = struct_type->field(member_name);
    if (member_type != nullptr) {
      type = member_type;

    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = Err;
    }
  } else if (base_type->is<Primitive>() || base_type->is<Function>()) {
    ErrorLog::MissingMember(span, member_name, base_type);
    type = Err;
  }
}

// We already know there can be at most one match (multiple matches would have
// been caught by shadowing), so we just return a pointer to it if it exists,
// and null otherwise.
static Declaration *FindFunctionCallMatch(const Identifier &id, Scope *scope,
                                          CallArgs *args) {
  for (auto *decl : scope->AllDeclsWithId(id.token)) {
    if (decl->type->is<Function>()) {
      // TODO what about const vs. non-const declarations?

      auto *fn = Evaluate(decl->init_val.get()).value.as<IR::Func *>();

      // Compute name -> argument number map.
      // TODO this should be done when the function is generated instead of
      // ad-hoc
      std::unordered_map<std::string, size_t> index_lookup;
      for (size_t i = 0; i < fn->args_.size(); ++i) {
        index_lookup[fn->args_[i].first] = i;
      }

      // Match the ordered unnamed arguments
      std::vector<Expression *> bindings(fn->args_.size(), nullptr);
      for (size_t i = 0; i < args->numbered_.size(); ++i) {
        bindings[i] = args->numbered_[i].get();
      }

      // Match the named arguments
      for (const auto &name_and_expr : args->named_) {
        auto iter = index_lookup.find(name_and_expr.first);
        if (iter == index_lookup.end()) {
          // TODO. Think more about this situation. You obviously need to pass
          // on this function match because it doesn't have this named argument,
          // but is that an error? It seems strange to be an error but also
          // strange to pass on it silently. Maybe it's not weird because you
          // already verify that there is some non-name-based (type-based) way
          // to distinguish the function calls. You'll probably go with this but
          // should write a good explanation about what it is and why it's okay.
          // For now passing silently.
          goto next_option;
        } else {
          bindings[iter->second] = name_and_expr.second.get();
        }
      }

      std::vector<Type *> inputs = fn->type->input->is<Tuple>()
                                       ? fn->type->input->as<Tuple>().entries
                                       : std::vector<Type *>{fn->type->input};
      for (size_t i = 0; i < bindings.size(); ++i) {
        if (bindings[i] == nullptr) {
          if (fn->args_[i].second == nullptr) {
            goto next_option; // No default for this parameter
          }
        } else {
          if (bindings[i]->type != inputs[i]) {
            goto next_option; // No match, but not an error.
          }
        }
      }
      args->bindings_ = std::move(bindings);
      return decl;
    } else {
      // TODO type casts can be called like functions.
    }
  next_option:;
  }
  return nullptr;
}

void CallArgs::verify_types() {
  STARTING_CHECK;
  for (auto &num : numbered_) {
    num->verify_types();
    if (num->type == Err) { type = Err; }
  }
  for (auto &name_and_val : named_) {
    name_and_val.second->verify_types();
    if (name_and_val.second->type == Err) { type = Err; }
  }
  if (type == Err) { return; }

  // We actually don't care what the type of this is and specifying it precisely
  // is hard due to not understanding the ordering of named arguments. It just
  // needs to be not Unknown or nullptr.
  type = Void;
}

void Binop::verify_types() {
  STARTING_CHECK;

  if (op == Language::Operator::Call) {
    if (lhs->is<Identifier>()) {
      auto &lhs_id = lhs->as<Identifier>();
      // It suffices to find just the first match because we are guarnateed that
      // there can only be one.
      Declaration *valid_match =
          FindFunctionCallMatch(lhs_id, scope_, &rhs->as<CallArgs>());

      if (valid_match == nullptr) {
        ErrorLog::NoValidMatches(span);
        type = lhs->type = Err;
        return;
      }

      lhs_id.decl = valid_match;
      lhs_id.verify_types();

    } else {
      VERIFY_AND_RETURN_ON_ERROR(lhs);

      if (lhs->type->is<Function>()) {
        if (ptr_cast<Function>(lhs->type)->input == (rhs ? rhs->type : Void)) {
          ErrorLog::LogGeneric(span,
                               "TODO " __FILE__ ":" + std::to_string(__LINE__));
          type      = Err;
          lhs->type = Err;
          return;
        }
      } else {
        ASSERT_EQ(lhs->type, Type_);

        // Cast
        op   = Language::Operator::Cast;
        type = lhs->value.value.as<Type *>();
        if (type == Err) { return; }

        if (rhs->type == type ||
            (rhs->type == Bool &&
             (type == Int || type == Uint || type == Real)) ||
            (rhs->type == Int && type == Real) ||
            (rhs->type == Int && type == Uint) ||
            (rhs->type == Uint && type == Real) ||
            (rhs->type == Uint && type == Int)) {
          return;
        }

        if (lhs->type->is<Pointer>() && type->is<Pointer>()) { return; }
        ErrorLog::InvalidCast(span, lhs->type, type);

        if (rhs) { rhs->verify_types(); }
        type = Err;
      }
    }

    if (rhs) { VERIFY_AND_RETURN_ON_ERROR(rhs); }

    // If you get here, you know the types all match. We just need to compute
    // the type of the call.
    if (lhs->type->is<Function>()) {
      type = lhs->type->as<Function>().output;

    } else {
      ASSERT_EQ(lhs->type, Type_);
      type = lhs->type;
    }

    return;
  }

  lhs->verify_types();
  rhs->verify_types();
  if (lhs->type == Err || rhs->type == Err) {
    type = Err;
    return;
  }

  using Language::Operator;
  // TODO if lhs is reserved?
  if (op == Language::Operator::Assign) {
    if (CanCastImplicitly(rhs->type, lhs->type)) {
      type = Void;
    } else {
      ErrorLog::LogGeneric(this->span,
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
    }
    return;
  }

  switch (op) {
  case Operator::Rocket: {
    // TODO rocket encountered outside case statement.
    UNREACHABLE();
  } break;
  case Operator::Index:
    type = Err;
    if (lhs->type == String) {
      if (rhs->type == Int || rhs->type == Uint) {
        type = Char;
        break;
      } else {
        ErrorLog::InvalidStringIndex(span, rhs->type);
      }
    } else if (!lhs->type->is<Array>()) {
      if (rhs->type->is<RangeType>()) {
        ErrorLog::SlicingNonArray(span, lhs->type);
      } else {
        ErrorLog::IndexingNonArray(span, lhs->type);
      }
    } else if (rhs->type->is<RangeType>()) {
      type = Slice(ptr_cast<Array>(lhs->type));
      break;
    } else {
      type = ptr_cast<Array>(lhs->type)->data_type;

      // TODO allow slice indexing
      if (rhs->type == Int || rhs->type == Uint) { break; }
      ErrorLog::NonIntegralArrayIndex(span, rhs->type);
    }
    return;
  case Operator::Dots: {
    if (lhs->type == Int && rhs->type == Int) {
      type = Range(Int);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      type = Range(Uint);

    } else if (lhs->type == Char && rhs->type == Char) {
      type = Range(Char);

    } else {
      ErrorLog::InvalidRangeTypes(span, lhs->type, rhs->type);
    }
  } break;
  case Language::Operator::XorEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      ErrorLog::XorEqNeedsBool(span);
    }
  } break;
  case Language::Operator::AndEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      ErrorLog::AndEqNeedsBool(span);
    }
  } break;
  case Language::Operator::OrEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      ErrorLog::OrEqNeedsBool(span);
    }
  } break;

#define CASE(OpName, op_name, symbol, ret_type)                                \
  case Language::Operator::OpName: {                                           \
    if ((lhs->type == Int && rhs->type == Int) ||                              \
        (lhs->type == Uint && rhs->type == Uint) ||                            \
        (lhs->type == Real && rhs->type == Real) ||                            \
        (lhs->type == Code && rhs->type == Code)) {                            \
      /* TODO Code should only be valid for Add, not Sub, etc */               \
      type = ret_type;                                                         \
    } else {                                                                   \
      /* Store a vector containing the valid matches */                        \
      std::vector<Declaration *> matched_op_name;                              \
                                                                               \
      /* TODO this linear search is probably not ideal.   */                   \
      scope_->ForEachDecl([&matched_op_name](Declaration *decl) {              \
        if (decl->identifier->token == "__" op_name "__") {                    \
          decl->verify_types();                                                \
          matched_op_name.push_back(decl);                                     \
        }                                                                      \
      });                                                                      \
                                                                               \
      Declaration *correct_decl = nullptr;                                     \
      for (auto &decl : matched_op_name) {                                     \
        if (!decl->type->is<Function>()) { continue; }                         \
        auto fn_type = ptr_cast<Function>(decl->type);                         \
        if (fn_type->input != Tup({lhs->type, rhs->type})) { continue; }       \
        /* If you get here, you've found a match. Hope there is only one       \
         * TODO if there is more than one, log them all and give a good        \
         * *error message. For now, we just fail */                            \
        if (correct_decl) {                                                    \
          ErrorLog::AlreadyFoundMatch(span, symbol, lhs->type, rhs->type);     \
          type = Err;                                                          \
        } else {                                                               \
          correct_decl = decl;                                                 \
        }                                                                      \
      }                                                                        \
      if (!correct_decl) {                                                     \
        type = Err;                                                            \
        ErrorLog::NoKnownOverload(span, symbol, lhs->type, rhs->type);         \
      } else if (type != Err) {                                                \
        type = ptr_cast<Function>(correct_decl->type)->output;                 \
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

    } else if (lhs->type->is<Function>() && rhs->type->is<Function>()) {
      auto lhs_fn = ptr_cast<Function>(lhs->type);
      auto rhs_fn = ptr_cast<Function>(rhs->type);
      if (rhs_fn->output == lhs_fn->input) {
        type = Func(rhs_fn->input, lhs_fn->output);

      } else {
        type = Err;
        ErrorLog::NonComposableFunctions(span);
      }

    } else {
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdHereOrNull("__mul__");
        if (!id_ptr) { continue; }
      }

      auto fn_type = scope_->FunctionTypeReferencedOrNull(
          "__mul__", Tup({lhs->type, rhs->type}));
      if (fn_type) {
        type = ptr_cast<Function>(fn_type)->output;
      } else {
        type = Err;
        ErrorLog::NoKnownOverload(span, "*", lhs->type, rhs->type);
      }
    }
  } break;
  case Operator::Arrow: {
    if (lhs->type != Type_) {
      type = Err;
      ErrorLog::NonTypeFunctionInput(span);
    }
    if (rhs->type != Type_) {
      type = Err;
      ErrorLog::NonTypeFunctionOutput(span);
    }

    if (type != Err) { type = Type_; }

  } break;
  default: UNREACHABLE();
  }
}

static bool ValidateComparisonType(Language::Operator op, Type *lhs_type,
                                   Type *rhs_type) {
  ASSERT(op == Language::Operator::Lt || op == Language::Operator::Le ||
             op == Language::Operator::Eq || op == Language::Operator::Ne ||
             op == Language::Operator::Ge || op == Language::Operator::Gt,
         "Expecting a ChainOp operator type.");

  if (lhs_type->is<Primitive>() || rhs_type->is<Primitive>()) {
    if (lhs_type != rhs_type) {
      ErrorLog::LogGeneric(TextSpan(),
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
      return false;
    }

    if (lhs_type == Int || lhs_type == Uint || lhs_type == Real) {
      return true;
    }

    if (lhs_type == Code || lhs_type == Void) { return false; }
    // TODO NullPtr, String types?

    if (lhs_type == Bool || lhs_type == Char || lhs_type == Type_) {
      if (op == Language::Operator::Eq || op == Language::Operator::Ne) {
        return true;
      } else {
        ErrorLog::LogGeneric(TextSpan(),
                             "TODO " __FILE__ ":" + std::to_string(__LINE__));
        return false;
      }
    }
  }

  if (lhs_type->is<Enum>() || lhs_type->is<Pointer>()) {
    if (lhs_type != rhs_type) {
      ErrorLog::LogGeneric(TextSpan(),
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
      return false;
    } else if (op != Language::Operator::Eq && op != Language::Operator::Ne) {
      ErrorLog::LogGeneric(TextSpan(),
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
      return false;
    } else {
      return true;
    }
  }

  // TODO there are many errors that might occur here and we should export all
  // of them. For instance, I might try to compare two arrays of differing fixed
  // lengths with '<'. We should not exit early, but rather say that you can't
  // use '<' and that the lengths are different.
  if (lhs_type->is<Array>()) {
    if (rhs_type->is<Array>()) {
      if (op != Language::Operator::Eq && op != Language::Operator::Ne) {
        ErrorLog::LogGeneric(TextSpan(),
                             "TODO " __FILE__ ":" + std::to_string(__LINE__));
        return false;
      }

      Array *lhs_array = ptr_cast<Array>(lhs_type);
      Array *rhs_array = ptr_cast<Array>(rhs_type);

      // TODO what if data types are equality comparable but not equal?
      if (lhs_array->data_type != rhs_array->data_type) {
        ErrorLog::LogGeneric(TextSpan(),
                             "TODO " __FILE__ ":" + std::to_string(__LINE__));
        return false;
      }

      if (lhs_array->fixed_length && rhs_array->fixed_length) {
        if (lhs_array->len == rhs_array->len) {
          return true;
        } else {
          ErrorLog::LogGeneric(TextSpan(),
                               "TODO " __FILE__ ":" + std::to_string(__LINE__));
          return false;
        }
      } else {
        return true; // If at least one array length is variable, we should be
                     // allowed to compare them.
      }
    } else {
      ErrorLog::LogGeneric(TextSpan(),
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
      return false;
    }
  }

  ErrorLog::LogGeneric(TextSpan(),
                       "TODO " __FILE__ ":" + std::to_string(__LINE__));
  return false;
}

void ChainOp::verify_types() {
  STARTING_CHECK;
  bool found_err = false;
  for (auto &expr : exprs) {
    expr->verify_types();
    if (expr->type == Err) { found_err = true; }
  }
  if (found_err) {
    type = Err;
    return;
  }

  // TODO if some expressions have type errors we can and should still do some
  // validation here
  type = Bool;

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  if (ops[0] == Language::Operator::And || ops[0] == Language::Operator::Or ||
      ops[0] == Language::Operator::Xor) {
    for (const auto &expr : exprs) {
      if (expr->type != Bool) {
        ErrorLog::LogGeneric(TextSpan(),
                             "TODO " __FILE__ ":" + std::to_string(__LINE__));
      }
    }
  } else {
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      if (!ValidateComparisonType(ops[i], exprs[i]->type, exprs[i + 1]->type)) {
        type = Err; // Errors exported by ValidateComparisonType
      }
    }
  }
}

void CommaList::verify_types() {
  STARTING_CHECK;
  bool found_err = false;
  for (auto &expr : exprs) {
    expr->verify_types();
    if (expr->type == Err) { found_err = true; }
  }
  if (found_err) {
    type = Err;
    return;
  }

  // TODO this is probably not a good way to do it.  If the tuple consists of a
  // list of types, it should be interpretted as a
  // type itself rather than a tuple. This is a limitation in your support of
  // full tuples.
  bool all_types = true;
  std::vector<Type *> type_vec(exprs.size(), nullptr);

  size_t position = 0;
  for (const auto &expr : exprs) {
    type_vec[position] = expr->type;
    all_types &= (expr->type == Type_);
    ++position;
  }
  // TODO got to have a better way to make tuple types i think
  type = all_types ? Type_ : Tup(type_vec);
}

void InDecl::verify_types() {
  STARTING_CHECK;
  container->verify_types();

  if (container->type == Void) {
    type             = Err;
    identifier->type = Err;
    ErrorLog::TypeIteration(span);
    return;
  }

  if (container->type->is<Array>()) {
    type = ptr_cast<Array>(container->type)->data_type;

  } else if (container->type->is<SliceType>()) {
    type = ptr_cast<SliceType>(container->type)->array_type->data_type;

  } else if (container->type->is<RangeType>()) {
    type = ptr_cast<RangeType>(container->type)->end_type;

  } else if (container->type == Type_) {
    auto t = Evaluate(container.get()).value.as<Type *>();
    if (t->is<Enum>()) { type = t; }

  } else {
    ErrorLog::IndeterminantType(span);
    type = Err;
  }

  identifier->type = type;
}

Type *Expression::VerifyTypeForDeclaration(const std::string &id_tok) {
  ASSERT_NE(type, nullptr);
  if (type != Type_) {
    ErrorLog::NotAType(span, id_tok);
    return Err;
  }

  Type *t = Evaluate(this).value.as<Type *>();

  if (t == Void) {
    ErrorLog::DeclaredVoidType(span, id_tok);
    return Err;
  }

  return t;
}

Type *Expression::VerifyValueForDeclaration(const std::string &) {
  if (type == Void) {
    ErrorLog::VoidDeclaration(span);
    return Err;
  }
  return type;
}

static void VerifyDeclarationForMagic(const std::string &magic_method_name,
                                      Type *type, const TextSpan &span) {
  if (!type->is<Function>()) {
    const static std::map<std::string, void (*)(const TextSpan &)>
        error_log_to_call = {{"__print__", ErrorLog::NonFunctionPrint},
                             {"__assign__", ErrorLog::NonFunctionAssign}};

    auto iter = error_log_to_call.find(magic_method_name);
    if (iter == error_log_to_call.end()) { return; }
    ErrorLog::LogGeneric(TextSpan(),
                         "TODO " __FILE__ ":" + std::to_string(__LINE__));
    iter->second(span);
  }

  auto fn_type = static_cast<Function *>(type);
  if (magic_method_name == "__print__") {
    if (!fn_type->input->is<Struct>()) {
      ErrorLog::InvalidPrintDefinition(span, fn_type->input);
    }

    if (fn_type->output != Void) { ErrorLog::NonVoidPrintReturn(span); }
  } else if (magic_method_name == "__assign__") {
    if (!fn_type->input->is<Tuple>()) {
      ErrorLog::InvalidAssignDefinition(span, fn_type->input);
    } else {
      auto in = static_cast<Tuple *>(fn_type->input);
      if (in->entries.size() != 2) {
        ErrorLog::NonBinaryAssignment(span, in->entries.size());
      }
      // TODO more checking.
    }

    if (fn_type->output != Void) { ErrorLog::NonVoidAssignReturn(span); }
  }
}

static bool Inferrable(Type *t) {
  if (t == NullPtr) {
    return false;
  } else if (t->is<Array>()) {
    return Inferrable(t->as<Array>().data_type);
  } else if (t->is<Pointer>()) {
    return Inferrable(t->as<Pointer>().pointee);
  } else if (t->is<Tuple>()) {
    for (auto *entry : t->as<Tuple>().entries) {
      if (!Inferrable(entry)) { return false; }
    }
  } else if (t->is<Function>()) {
    return Inferrable(t->as<Function>().input) &&
           Inferrable(t->as<Function>().output);
  }
  // TODO higher order types?
  return true;
}

static Type* Unify(Type* lhs, Type* rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == Err) { return rhs; } // Ignore errors
  if (rhs == Err) { return lhs; } // Ignore errors
  if (lhs == NullPtr && rhs->is<Pointer>()) { return rhs; }
  if (rhs == NullPtr && lhs->is<Pointer>()) { return lhs; }
  if (lhs->is<Pointer>() && rhs->is<Pointer>()) {
    return Unify(lhs->as<Pointer>().pointee, rhs->as<Pointer>().pointee);
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    Type *result = nullptr;
    if (lhs->as<Array>().fixed_length && rhs->as<Array>().fixed_length) {
      if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
      result = Unify(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result, lhs->as<Array>().len) : result;
    } else {
      result = Unify(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result) : result;
    }
  }
  // TODO, arrays, etc.
  return nullptr;
}

// TODO Declaration is responsible for the type verification of it's identifier?
// TODO rewrite/simplify
void Declaration::verify_types() {
  STARTING_CHECK;

  if (type_expr) { type_expr->verify_types(); }
  if (init_val) { init_val->verify_types(); }

  // There are eight cases for the form of a declaration.
  //   1a. I: T         or    1b. I :: T
  //   2a. I := V       or    2b. I ::= V
  //   3a. I: T = V     or    3b. I :: T = V
  //   4a. I: T = --    or    4b. I :: T = -- (illegal)
  //
  // Here 'I' stands for "identifier". This is the identifier being declared.
  // 'T' stands for "type", the type of the identifier being declared.
  // 'V' stands for "value", the initial value of the identifier being declared.
  // The double-colon (b) cases are those where the values being declared must
  // be known at compile-time. The single-colon (a) cases the values being
  // declared need only be known at run-time. In case 4a and 4b, the --
  // indicates that the value is not specified. The only valid operations to
  // perform on such an identifier is to assign to it. This might be useful in
  // 4a for optimzation purposes. In the case of 4b it is outright illegal and
  // an error is thrown by the compiler.

  if (IsDefaultInitialized()) {
    type = type_expr->VerifyTypeForDeclaration(identifier->token);
  } else if (IsInferred()) {
    type = init_val->VerifyValueForDeclaration(identifier->token);

    if (!Inferrable(type)) {
      // Some types are only present in the type-system to make handling easier
      // for us, but really cannot be inferred. For example, almost dealing
      // with 'null':
      //   foo := [null, null]  // Not valid
      //   foo: [2; &int] = [null, null] // Okay. Type explicitly stated.
      ErrorLog::UninferrableType(span);
      type = Err;
    }

  } else if (IsCustomInitialized()) {
    type               = type_expr->VerifyTypeForDeclaration(identifier->token);
    auto init_val_type = init_val->VerifyValueForDeclaration(identifier->token);

    if (type == Err) {
      type = init_val_type;
    } else if (!CanCastImplicitly(init_val_type, type)) {
      ErrorLog::AssignmentTypeMismatch(span, type, init_val_type);
    }

    if (!type->is<Pointer>() && init_val_type == NullPtr) {
      ErrorLog::LogGeneric(this->span,
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
    }

  } else if (IsUninitialized()) {
    if (const_) {
      ErrorLog::LogGeneric(this->span,
                           "TODO " __FILE__ ":" + std::to_string(__LINE__));
    }

    type           = type_expr->VerifyTypeForDeclaration(identifier->token);
    init_val->type = type;

  } else {
    UNREACHABLE();
  }

  identifier->verify_types();

  if (type == Err) { return; }

  // TODO is this the right time to complete the struct definition?
  if (type->is<Struct>()) { type->as<Struct>().CompleteDefinition(); }

  if (type == Type_ && IsInferred()) {
    // TODO Declaring a type must be a compile-time constant? No... what if I'm
    // writing a function that modifies a type? Something here needs fixing.

    if (init_val->is<Terminal>()) {
      auto t = init_val->value.value.as<Type *>();
      // TODO mangle the name correctly (Where should this be done?)
      if (t->is<Struct>()) {
        t->as<Struct>().bound_name = identifier->token;
      } else if (t->is<Enum>()) {
        t->as<Enum>().bound_name = identifier->token;
      } else if (t->is<Scope_Type>()) {
        t->as<Scope_Type>().bound_name = identifier->token;
      }
    }

    identifier->value = init_val->value;
  }

  // TODO Either guarantee that higher scopes have all declarations declared and
  // verified first, or check both in the upwards and downwards direction for
  // shadowing.
  for (auto *decl : scope_->AllDeclsWithId(identifier->token)) {
    if (decl == this) { continue; }
    if (this < decl) {
      // Pick one arbitrary but consistent ordering of the pair to check because
      // at each Declaration verification, we look both up and down the scope
      // tree.
      if (Shadow(this, decl)) { ErrorLog::ShadowingDeclaration(*this, *decl); }
    }
  }
  auto iter = scope_->child_decls_.find(identifier->token);
  if (iter != scope_->child_decls_.end()) {
    for (auto *decl : iter->second) {
      if (this < decl) {
        // Pick one arbitrary but consistent ordering of the pair to check
        // because at each Declaration verification, we look both up and down
        // the scope tree.
        if (Shadow(this, decl)) {
          ErrorLog::ShadowingDeclaration(*this, *decl);
        }
      }
    }
  }

  // TODO I hope this won't last very long. I don't love the syntax/approach
  VerifyDeclarationForMagic(identifier->token, type, span);
}

void ArrayType::verify_types() {
  STARTING_CHECK;
  length->verify_types();
  data_type->verify_types();

  type = Type_;

  if (length->is_hole()) { return; }

  // TODO change this to just uint
  if (length->type != Int && length->type != Uint) {
    ErrorLog::ArrayIndexType(span);
  }
}

void ArrayLiteral::verify_types() {
  STARTING_CHECK;

  // TODO this should be allowed in the same vein as 'null'?
  if (elems.empty()) {
    type = Err;
    ErrorLog::EmptyArrayLit(span);
    return;
  }

  for (auto &elem : elems) { elem->verify_types(); }
  Type *unified = Err;
  for (auto &elem : elems) {
    unified = Unify(unified, elem->type);
    if (unified == nullptr) { break; }
  }

  if (unified == nullptr) {
    // Types couldn't be unified. Emit an error
    ErrorLog::InconsistentArrayType(span);
    type = Err;
  } else if (unified == Err) {
    type = Err; // There were no valid types anywhere in the array
  } else {
    type = Arr(unified, elems.size());
  }
}

void FunctionLiteral::verify_types() {
  STARTING_CHECK;

  VerificationQueue.push(statements.get());

  return_type_expr->verify_types();
  if (ErrorLog::num_errs_ > 0) {
    type = Err;
    return;
  }

  // TODO should named return types be required?
  auto ret_type_val = Evaluate([&]() {
    if (!return_type_expr->is<Declaration>()) { return return_type_expr.get(); }

    auto *decl_return = &return_type_expr->as<Declaration>();
    if (decl_return->IsInferred()) { NOT_YET(); }

    return decl_return->type_expr.get();
  }());

  if (ret_type_val == IR::Val::None()) {
    type = Err;
    return;
  }

  // TODO must this really be undeclared?
  if (ret_type_val == IR::Val::None() /* TODO Error() */) {
    ErrorLog::IndeterminantType(return_type_expr.get());
    type = Err;
  } else if (ret_type_val.type != Type_) {
    ErrorLog::NotAType(return_type_expr.get(), return_type_expr->type);
    type = Err;
    return;
  } else if (ret_type_val.value.as<Type *>() == Err) {
    type = Err;
    return;
  }

  for (auto &input : inputs) { input->verify_types(); }

  // TODO don't do early exists on input or return type errors.

  Type *ret_type = ret_type_val.value.as<Type *>();
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

  FuncInnardsVerificationQueue.emplace(ret_type, statements.get());

  // TODO generics?
  type = Func(input_type, ret_type);
}

void Case::verify_types() {
  STARTING_CHECK;
  for (auto &kv : key_vals) {
    kv.first->verify_types();
    kv.second->verify_types();
  }

  std::map<Type *, size_t> value_types;

  for (auto &kv : key_vals) {
    if (kv.first->type == Err) {
      kv.first->type = Bool;

    } else if (kv.first->type != Bool) {
      ErrorLog::CaseLHSBool(span, kv.first->span, kv.first->type);
      kv.first->type = Bool;
    }

    if (kv.second->type == Err) {
      type = Err;
      return;
    }
    ++value_types[kv.second->type];
  }

  if (value_types.size() != 1) {

    // In order to give a message saying that a particular type is incorrect, we
    // need either
    // * 1/2 of them have the same type
    // * 1/4 of them have the same type, and no other type hits > 1/8
    //
    // NOTE: These numbers were chosen somewhat arbitrarily.

    size_t max_size = 0;
    size_t min_size = key_vals.size();
    Type *max_type  = nullptr;
    for (const auto &kv : value_types) {
      if (kv.second > max_size) {
        max_size = kv.second;
        max_type = kv.first;
      }

      if (kv.second < min_size) { min_size = kv.second; }
    }

    if (2 * max_size > key_vals.size() ||
        (4 * max_size > key_vals.size() && 8 * min_size < key_vals.size())) {
      ErrorLog::CaseTypeMismatch(this, max_type);
      type = max_type;
    } else {
      ErrorLog::CaseTypeMismatch(this);
      type = Err;
    }
  } else {
    type = value_types.begin()->first;
  }
}

void Statements::verify_types() {
  for (auto &stmt : statements) { stmt->verify_types(); }
}

void For::verify_types() {
  for (auto &iter : iterators) { iter->verify_types(); }
  statements->verify_types();
}

void Jump::verify_types() {
  // TODO made this slightly wrong
  auto scope_ptr = scope_;
  while (scope_ptr && scope_ptr->is<ExecScope>()) {
    auto exec_scope_ptr = &scope_ptr->as<ExecScope>();
    if (exec_scope_ptr->can_jump) {
      scope = exec_scope_ptr;
      return;
    }
    scope_ptr = exec_scope_ptr->parent;
  }
  ErrorLog::JumpOutsideLoop(span);
}

// Intentionally do not verify anything internal
void CodeBlock::verify_types() { type = Code; }

void ScopeNode::verify_types() {
  STARTING_CHECK;

  scope_expr->verify_types();
  if (expr) { expr->verify_types(); }
  stmts->verify_types();

  if (!scope_expr->type->is<Scope_Type>()) {
    ErrorLog::InvalidScope(scope_expr->span, scope_expr->type);
    type = Err;
    return;
  }

  // TODO verify it uses the fields correctly
  //
  // ScopeLiteral *lit = Evaluate(scope_expr).as_scope;
  // if (!type->is<Scope_Type>()) {
  //   if (scope_expr->type != ScopeType(expr ? expr->type : Void)) {
  //     ErrorLog::InvalidScope(scope_expr->span, scope_expr->type);
  //     type = Err;
  //     return;
  //   }
  // }
  type = Void;
}

void ScopeLiteral::verify_types() {
  STARTING_CHECK;
  bool cannot_proceed_due_to_errors = false;
  if (!enter_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span,
                         "TODO " __FILE__ ":" + std::to_string(__LINE__));
  }

  if (!exit_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span,
                         "TODO " __FILE__ ":" + std::to_string(__LINE__));
  }

  if (cannot_proceed_due_to_errors) { return; }

  VERIFY_AND_RETURN_ON_ERROR(enter_fn);
  VERIFY_AND_RETURN_ON_ERROR(exit_fn);

  if (!enter_fn->type->is<Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span,
                         "TODO " __FILE__ ":" + std::to_string(__LINE__));
  }

  if (!exit_fn->type->is<Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span,
                         "TODO " __FILE__ ":" + std::to_string(__LINE__));
  }

  if (cannot_proceed_due_to_errors) { return; }

  type = ScopeType(enter_fn->type->as<Function>().input);
}

void Unop::VerifyReturnTypes(Type *ret_type) {
  if (type == Err) { return; }

  if (op == Language::Operator::Return) {
    if (operand->type == Err) { return; } // Error already logged
    if (operand->type != ret_type) {
      ErrorLog::InvalidReturnType(span, operand->type, ret_type);
    }
  }
}

void Statements::VerifyReturnTypes(Type *ret_type) {
  for (auto &stmt : statements) { stmt->VerifyReturnTypes(ret_type); }
}

void Jump::VerifyReturnTypes(Type *ret_type) {
  if (jump_type == JumpType::Return && ret_type != Void) {
    ErrorLog::InvalidReturnType(span, Void, ret_type);
  }
}

void For::VerifyReturnTypes(Type *ret_type) {
  statements->VerifyReturnTypes(ret_type);
}
} // namespace AST

void CompletelyVerify(AST::Node *node) {
  VerificationQueue.push(node);

  while (!VerificationQueue.empty()) {
    auto node_to_verify = VerificationQueue.front();
    node_to_verify->verify_types();
    VerificationQueue.pop();
  }

  while (!FuncInnardsVerificationQueue.empty()) {
    auto pair     = FuncInnardsVerificationQueue.front();
    auto ret_type = pair.first;
    auto stmts    = pair.second;
    stmts->VerifyReturnTypes(ret_type);
    FuncInnardsVerificationQueue.pop();
  }
}

#undef VERIFY_AND_RETURN_ON_ERROR
#undef STARTING_CHECK
