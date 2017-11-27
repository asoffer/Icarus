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
extern std::vector<Error> errors;
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
    auto *array_from = ptr_cast<Array>(from);
    auto *array_to   = ptr_cast<Array>(to);

    if (array_to->fixed_length &&
        (!array_from->fixed_length || array_to->len != array_from->len)) {
      return false;
    }
    return CanCastImplicitly(array_from->data_type, array_to->data_type);
  }

  return false;
}

#define STARTING_CHECK                                                         \
  ASSERT(scope_, "Need to first call assign_scope()");                         \
  if (type == Unknown) {                                                       \
    errors.emplace_back(Error::Code::CyclicDependency);                        \
    /* ErrorLog::CyclicDependency(this); */                                    \
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
    switch (potential_decls.size()) {
    case 1: decl = potential_decls[0]; break;
    case 0:
      LogError::UndeclaredIdentifier(this);
      type = Err;
      return;
    default:
      LogError::AmbiguousIdentifier(this);
      type = Err;
      return;
    }
  }

  type = decl->type;

  // Verify whether or not this identifier was captured validly.

  // Compile-time constants may be captured implicitly.
  if (decl->const_) { return; }

  // For everything else we iterate from the scope of this identifier up to the
  // scope in which it was declared checking that along the way that it's a
  // block scope.
  for (auto scope_ptr = scope_; scope_ptr != decl->scope_;
       scope_ptr      = scope_ptr->parent) {
    if (scope_ptr->is<FnScope>()) {
      ptr_cast<FnScope>(scope_ptr)->fn_lit->captures.insert(decl);
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
      errors.emplace_back(Error::Code::FreeNonPtr);
      // ErrorLog::UnopTypeFail(msg, this);
    }
    type = Void;
  } break;
  case Operator::Print: {
    if (operand->type == Void) {
      errors.emplace_back(Error::Code::PrintVoid);
      // ErrorLog::UnopTypeFail(
      // "Attempting to print an expression with type `void`.", this);
    }
    type = Void;
  } break;
  case Operator::Return: {
    if (operand->type == Void) {
      errors.emplace_back(Error::Code::ReturnVoid);
      // ErrorLog::UnopTypeFail(
      //  "Attempting to return an expression which has type `void`.", this);
    }

    type = Void;
  } break;
  case Operator::At: {
    if (operand->type->is<Pointer>()) {
      type = ptr_cast<Pointer>(operand->type)->pointee;

    } else {
      std::string msg = "Attempting to dereference an expression of type `" +
                        operand->type->to_string() + "`.";
      errors.emplace_back(Error::Code::DerefNonPtr);
      // ErrorLog::UnopTypeFail(msg, this);
      type = Err;
    }
  } break;
  case Operator::And: {
    type = Ptr(operand->type);
  } break;
  case Operator::Mul: {
    if (operand->type != Type_) {
      errors.emplace_back(Error::Code::Other); // TODO correct error code
    } else {
      type = Type_;
    }
  } break;
  case Operator::Sub: {
    if (operand->type == Uint) {
      errors.emplace_back(Error::Code::UnsignedNegation);
      // ErrorLog::UnopTypeFail("Attempting to negate an unsigned integer
      // (uint).",
      //                    this);
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
        errors.emplace_back(Error::Code::MissingOperator);
        // ErrorLog::UnopTypeFail("Type `" + operand->type->to_string() +
        //                        "` has no unary negation operator.",
        //                   this);
        type = Err;
      }

    } else {
      errors.emplace_back(Error::Code::MissingOperator);
      // ErrorLog::UnopTypeFail("Type `" + operand->type->to_string() +
      //                        "` has no unary negation operator.",
      //                   this);
      type = Err;
    }
  } break;
  case Operator::Dots: {
    if (operand->type == Uint || operand->type == Int ||
        operand->type == Char) {
      type = Range(operand->type);
    } else {
      errors.emplace_back(Error::Code::InvalidRangeType);
      // ErrorLog::InvalidRangeType(loc, type);
      type = Err;
    }
  } break;
  case Operator::Not: {
    if (operand->type == Bool) {
      type = Bool;
    } else {
      errors.emplace_back(Error::Code::LogicalNegationOfNonBool);
      // ErrorLog::UnopTypeFail("Attempting to apply the logical negation "
      //                    "operator (!) to an expression of type `" +
      //                       operand->type->to_string() + "`.",
      //                  this);
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
      errors.emplace_back(Error::Code::MissingMember);
      // ErrorLog::MissingMember(loc, member_name, base_type);
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
          errors.emplace_back(Error::Code::MissingMember);
          // ErrorLog::MissingMember(loc, member_name, evaled_type);
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
      errors.emplace_back(Error::Code::MissingMember);
      // ErrorLog::MissingMember(loc, member_name, base_type);
      type = Err;
    }
  } else if (base_type->is<Primitive>() || base_type->is<Function>()) {
    errors.emplace_back(Error::Code::MissingMember);
    // ErrorLog::MissingMember(loc, member_name, base_type);
    type = Err;
  }
}

void Binop::verify_types() {
  STARTING_CHECK;

  if (op == Language::Operator::Call) {
    if (rhs) { VERIFY_AND_RETURN_ON_ERROR(rhs); }

    if (lhs->is<Identifier>()) {
      auto *lhs_id = ptr_cast<Identifier>(lhs.get());

      // Look for valid matches by looking at any declaration which has a
      // matching token.
      std::vector<Declaration *> valid_matches;
      for (auto &decl : scope_->AllDeclsWithId(lhs_id->token)) {
        if (decl->type->is<Function>()) {
          auto fn_type = ptr_cast<Function>(decl->type);
          // If there is no input, and the function takes Void as its input, or
          // if the types just match, then add it to your list of matches.
          if ((rhs.get() == nullptr && fn_type->input == Void) ||
              (rhs.get() != nullptr && rhs->type == fn_type->input)) {
            valid_matches.push_back(decl);
          }
        } else {
          if (decl->IsInferred() || decl->IsCustomInitialized()) {
            if (decl->init_val->type->is<Function>()) {
              UNREACHABLE(); // TODO WTF??? HOW DID THIS EVEN COMPILE?
              // decl->value = IR::Val(decl->init_val);
            } else {
              decl->value = IR::Val::Type(
                  Evaluate(decl->init_val.get()).value.as<Type *>());

              if (decl->init_val->is<Terminal>()) {
                auto t = decl->init_val->value.value.as<Type *>();
                if (t->is<Struct>()) {
                  ptr_cast<Struct>(decl->identifier->value.value.as<Type *>())
                      ->bound_name = decl->identifier->token;
                } else if (t->is<Enum>()) {
                  ptr_cast<Enum>(decl->identifier->value.value.as<Type *>())
                      ->bound_name = decl->identifier->token;
                }
              }
            }

          } else if (decl->IsUninitialized()) {
            NOT_YET();
          }
        }
      } // End of decl loop

      if (valid_matches.size() != 1) {
        // TODO provide more information
        if (valid_matches.empty()) {
          errors.emplace_back(Error::Code::NoCallMatches);
          // ErrorLog::NoValidMatches(loc);
        } else {
          errors.emplace_back(Error::Code::AmbiguousCall);
          // ErrorLog::AmbiguousCall(loc);
        }
        type = lhs->type = Err;
        return;
      }

      lhs_id->decl = valid_matches[0];
      lhs_id->verify_types();

    } else {
      VERIFY_AND_RETURN_ON_ERROR(lhs);

      if (lhs->type->is<Function>()) {
        if (ptr_cast<Function>(lhs->type)->input == (rhs ? rhs->type : Void)) {
          errors.emplace_back(Error::Code::Other);
          // ErrorLog::LogGeneric(loc, err_msg);
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
        errors.emplace_back(Error::Code::InvalidCast);
        // ErrorLog::InvalidCast(loc, lhs->type, type);

        if (rhs) { rhs->verify_types(); }
        type = Err;
      }
    }

    if (rhs) { VERIFY_AND_RETURN_ON_ERROR(rhs); }

    // If you get here, you know the types all match. We just need to compute
    // the type of the call.
    if (lhs->type->is<Function>()) {
      type = ptr_cast<Function>(lhs->type)->output;

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
      errors.emplace_back(Error::Code::Other);
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
        errors.emplace_back(Error::Code::InvalidStringIndexType);
        // ErrorLog::InvalidStringIndex(loc, rhs->type);
      }
    } else if (!lhs->type->is<Array>()) {
      if (rhs->type->is<RangeType>()) {
        errors.emplace_back(Error::Code::SlicingNonArray);
        // ErrorLog::SlicingNonArray(loc, lhs->type);
      } else {
        errors.emplace_back(Error::Code::IndexingNonArray);
        // ErrorLog::IndexingNonArray(loc, lhs->type);
      }
    } else if (rhs->type->is<RangeType>()) {
      type = Slice(ptr_cast<Array>(lhs->type));
      break;
    } else {
      type = ptr_cast<Array>(lhs->type)->data_type;

      // TODO allow slice indexing
      if (rhs->type == Int || rhs->type == Uint) { break; }
      errors.emplace_back(Error::Code::NonIntegralArrayIndex);
      // ErrorLog::NonIntegralArrayIndex(loc, rhs->type);
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
      errors.emplace_back(Error::Code::InvalidRangeTypes);
      // ErrorLog::InvalidRangeTypes(loc, lhs->type, rhs->type);
    }
  } break;
  case Language::Operator::XorEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      errors.emplace_back(Error::Code::NonBooleanLogicalAssignemnt);
      // ErrorLog::XorEqNeedsBool(loc);
    }
  } break;
  case Language::Operator::AndEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      errors.emplace_back(Error::Code::NonBooleanLogicalAssignemnt);
      // ErrorLog::AndEqNeedsBool(loc);
    }
  } break;
  case Language::Operator::OrEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else {
      type = Err;
      errors.emplace_back(Error::Code::NonBooleanLogicalAssignemnt);
      // ErrorLog::OrEqNeedsBool(loc);
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
      for (auto scope_ptr = scope_; scope_ptr;                                 \
           scope_ptr      = scope_ptr->parent) {                               \
        for (auto &decl : scope_ptr->decls_) {                                 \
          if (decl->identifier->token == "__" op_name "__") {                  \
            decl->verify_types();                                              \
            matched_op_name.push_back(decl);                                   \
          }                                                                    \
        }                                                                      \
      }                                                                        \
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
          errors.emplace_back(Error::Code::AlreadyFoundMatch);                 \
          /* ErrorLog::AlreadyFoundMatch(loc, symbol, lhs->type, rhs->type);   \
           */                                                                  \
          type = Err;                                                          \
        } else {                                                               \
          correct_decl = decl;                                                 \
        }                                                                      \
      }                                                                        \
      if (!correct_decl) {                                                     \
        type = Err;                                                            \
        errors.emplace_back(Error::Code::NoKnownOverload);                     \
        /* ErrorLog::NoKnownOverload(loc, symbol, lhs->type, rhs->type); */    \
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
        errors.emplace_back(Error::Code::NonComposableFns);
        // ErrorLog::NonComposableFunctions(loc);
      }

    } else {
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdHereOrNull("__mul__");
        if (!id_ptr) { continue; }

        // Dependency::traverse_from(Dependency::PtrWithTorV(id_ptr, false));
      }

      auto fn_type = scope_->FunctionTypeReferencedOrNull(
          "__mul__", Tup({lhs->type, rhs->type}));
      if (fn_type) {
        type = ptr_cast<Function>(fn_type)->output;
      } else {
        type = Err;
        errors.emplace_back(Error::Code::NoKnownOverload);
        // ErrorLog::NoKnownOverload(loc, "*", lhs->type, rhs->type);
      }
    }
  } break;
  case Operator::Arrow: {
    if (lhs->type != Type_) {
      type = Err;
      errors.emplace_back(Error::Code::NonTypeFnInput);
      // ErrorLog::NonTypeFunctionInput(loc);
    }
    if (rhs->type != Type_) {
      type = Err;
      errors.emplace_back(Error::Code::NonTypeFnOutput);
      // ErrorLog::NonTypeFunctionOutput(loc);
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
      // TODO Log a better error
      errors.emplace_back(Error::Code::Other);
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
        // TODO Log a better error
        errors.emplace_back(Error::Code::Other);
        return false;
      }
    }
  }

  if (lhs_type->is<Enum>() || lhs_type->is<Pointer>()) {
    if (lhs_type != rhs_type) {
      // TODO Log a better error
      errors.emplace_back(Error::Code::Other);
      return false;
    } else if (op != Language::Operator::Eq && op != Language::Operator::Ne) {
      // TODO Log a better error
      errors.emplace_back(Error::Code::Other);
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
        // TODO Log a better error
        errors.emplace_back(Error::Code::Other);
        return false;
      }

      Array *lhs_array = ptr_cast<Array>(lhs_type);
      Array *rhs_array = ptr_cast<Array>(rhs_type);

      // TODO what if data types are equality comparable but not equal?
      if (lhs_array->data_type != rhs_array->data_type) {
        // TODO Log a better error
        errors.emplace_back(Error::Code::Other);
        return false;
      }

      if (lhs_array->fixed_length && rhs_array->fixed_length) {
        if (lhs_array->len == rhs_array->len) {
          return true;
        } else {
          // TODO Log a better error
          errors.emplace_back(Error::Code::Other);
          return false;
        }
      } else {
        return true; // If at least one array length is variable, we should be
                     // allowed to compare them.
      }
    } else {
      // TODO Log a better error
      errors.emplace_back(Error::Code::Other);
      return false;
    }
  }

  // TODO Log a better error
  errors.emplace_back(Error::Code::Other);
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
        // TODO log an error
        LOG << "Type error.";
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
    errors.emplace_back(Error::Code::TypeIteration);
    // ErrorLog::TypeIteration(loc);
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
    errors.emplace_back(Error::Code::IndeterminantType);
    // ErrorLog::IndeterminantType(loc);
    type = Err;
  }

  identifier->type = type;
}

Type *Expression::VerifyTypeForDeclaration(const std::string & /*id_tok*/) {

  if (type != Type_) {
    errors.emplace_back(Error::Code::NotAType);
    // ErrorLog::NotAType(loc, id_tok);
    return Err;
  }

  Type *t = Evaluate(this).value.as<Type *>();

  if (t == Void) {
    errors.emplace_back(Error::Code::VoidDeclaration);
    // ErrorLog::DeclaredVoidType(loc, id_tok);
    return Err;
  }

  return t;
}

// TODO refactor this and VerifyTypeForDeclaration because they have extreme
// commonalities.
Type *Expression::VerifyValueForDeclaration(const std::string &) {
  if (type == Void) {
    errors.emplace_back(Error::Code::VoidDeclaration);
    // ErrorLog::VoidDeclaration(loc);
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
    errors.emplace_back(Error::Code::Other);
    iter->second(span);
  }

  auto fn_type = static_cast<Function *>(type);
  if (magic_method_name == "__print__") {
    if (!fn_type->input->is<Struct>()) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::InvalidPrintDefinition(span, fn_type->input);
    }

    if (fn_type->output != Void) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::NonVoidPrintReturn(span);
    }
  } else if (magic_method_name == "__assign__") {
    if (!fn_type->input->is<Tuple>()) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::InvalidAssignDefinition(span, fn_type->input);
    } else {
      auto in = static_cast<Tuple *>(fn_type->input);
      if (in->entries.size() != 2) {
        errors.emplace_back(Error::Code::Other);
        // ErrorLog::NonBinaryAssignment(span, in->entries.size());
      }
      // TODO more checking.
    }

    if (fn_type->output != Void) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::NonVoidAssignReturn(span);
    }
  }
}

// TODO Declaration is responsible for the type verification of it's identifier?
// TODO rewrite/simplify
void Declaration::verify_types() {
  STARTING_CHECK;

  if (type_expr) { type_expr->verify_types(); }
  if (init_val) { init_val->verify_types(); }

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
    type = type_expr->VerifyTypeForDeclaration(identifier->token);
  } else if (IsInferred()) {
    type = init_val->VerifyValueForDeclaration(identifier->token);

    if (type == NullPtr) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::NullDeclInit(span);
      type = Err;
    }

  } else if (IsCustomInitialized()) {
    type               = type_expr->VerifyTypeForDeclaration(identifier->token);
    auto init_val_type = init_val->VerifyValueForDeclaration(identifier->token);

    if (type == Err) {
      type = init_val_type;
    } else if (!CanCastImplicitly(init_val_type, type)) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::AssignmentTypeMismatch(span, type, init_val_type);
    }

  } else if (IsUninitialized()) {
    type           = type_expr->VerifyTypeForDeclaration(identifier->token);
    init_val->type = type;

  } else {
    UNREACHABLE();
  }

  identifier->verify_types();

  if (type == Err) { return; }

  if (type->is<Struct>()) { ptr_cast<Struct>(type)->CompleteDefinition(); }

  if (type == Type_ && IsInferred()) {
    if (init_val->is<Terminal>()) {
      auto t = init_val->value.value.as<Type *>();
      // TODO mangle the name correctly (Where should this be done?)
      if (t->is<Struct>()) {
        ptr_cast<Struct>(t)->bound_name = identifier->token;
      } else if (t->is<Enum>()) {
        ptr_cast<Enum>(t)->bound_name = identifier->token;
      } else if (t->is<Scope_Type>()) {
        ptr_cast<Scope_Type>(t)->bound_name = identifier->token;
      }
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
    errors.emplace_back(Error::Code::Other);
    // ErrorLog::ArrayIndexType(span);
  }
}

void ArrayLiteral::verify_types() {
  STARTING_CHECK;
  for (auto &elem : elems) { elem->verify_types(); }

  // TODO this should be allowed in the same vein as 'null'?
  if (elems.empty()) {
    type = Err;
    errors.emplace_back(Error::Code::Other);
    // ErrorLog::EmptyArrayLit(span);
    return;
  }

  // TODO create a collection of all the types in the array literal. go on if
  // there's only one otherwise, attempt to determine where the mistakes are.
  auto type_to_match = elems.front()->type;

  if (type_to_match == Err) {
    type = Err;
    return;
  }

  type = Arr(type_to_match, elems.size());
  for (const auto &el : elems) {
    if (el->type != type_to_match) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::InconsistentArrayType(span);
      type = Err;
    }
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
    errors.emplace_back(Error::Code::Other);
    // ErrorLog::IndeterminantType(return_type_expr);
    type = Err;
  } else if (ret_type_val.type != Type_) {
    errors.emplace_back(Error::Code::Other);
    // ErrorLog::NotAType(return_type_expr, return_type_expr->type);
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
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::CaseLHSBool(span, kv.first->span, kv.first->type);
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
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::CaseTypeMismatch(this, max_type);
      type = max_type;
    } else {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::CaseTypeMismatch(this);
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
    auto exec_scope_ptr = static_cast<ExecScope *>(scope_ptr);
    if (exec_scope_ptr->can_jump) {
      scope = exec_scope_ptr;
      return;
    }
    scope_ptr = exec_scope_ptr->parent;
  }
  errors.emplace_back(Error::Code::Other);
  // ErrorLog::JumpOutsideLoop(span);
}

// Intentionally do not verify anything internal
void CodeBlock::verify_types() { type = Code; }

void ScopeNode::verify_types() {
  STARTING_CHECK;

  scope_expr->verify_types();
  if (expr) { expr->verify_types(); }
  stmts->verify_types();

  if (!scope_expr->type->is<Scope_Type>()) {
    errors.emplace_back(Error::Code::Other);
    // ErrorLog::InvalidScope(scope_expr->span, scope_expr->type);
    type = Err;
    return;
  }

  // TODO verify it uses the fields correctly
  //
  // ScopeLiteral *lit = Evaluate(scope_expr).as_scope;
  // if (!type->is<Scope_Type>()) {
  //   if (scope_expr->type != ScopeType(expr ? expr->type : Void)) {
  //     errors.emplace_back(Error::Code::Other);
  //     // ErrorLog::InvalidScope(scope_expr->span, scope_expr->type);
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
    errors.emplace_back(Error::Code::Other);
  }

  if (!exit_fn) {
    cannot_proceed_due_to_errors = true;
    errors.emplace_back(Error::Code::Other);
  }

  if (cannot_proceed_due_to_errors) { return; }

  VERIFY_AND_RETURN_ON_ERROR(enter_fn);
  VERIFY_AND_RETURN_ON_ERROR(exit_fn);

  if (!enter_fn->type->is<Function>()) {
    cannot_proceed_due_to_errors = true;
    errors.emplace_back(Error::Code::Other);
  }

  if (!exit_fn->type->is<Function>()) {
    cannot_proceed_due_to_errors = true;
    errors.emplace_back(Error::Code::Other);
  }

  if (cannot_proceed_due_to_errors) { return; }

  type = ScopeType(ptr_cast<Function>(enter_fn->type)->input);
}

void Unop::VerifyReturnTypes(Type *ret_type) {
  if (type == Err) { return; }

  if (op == Language::Operator::Return) {
    if (operand->type == Err) { return; } // Error already logged
    if (operand->type != ret_type) {
      errors.emplace_back(Error::Code::Other);
      // ErrorLog::InvalidReturnType(span, operand->type, ret_type);
    }
  }
}

void Statements::VerifyReturnTypes(Type *ret_type) {
  for (auto &stmt : statements) { stmt->VerifyReturnTypes(ret_type); }
}

void Jump::VerifyReturnTypes(Type *ret_type) {
  if (jump_type == JumpType::Return && ret_type != Void) {
    errors.emplace_back(Error::Code::Other);
    // ErrorLog::InvalidReturnType(span, Void, ret_type);
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
