#include "ast.h"

#include <algorithm>

#include "ast/hole.h"
#include "error/log.h"
#include "ir/func.h"
#include "type/all.h"
#include "module.h"
#include "verify_macros.h"

// TODO catch functions that don't return along all paths.
// TODO Join/Meet for type::EmptyArray <-> [0; T] (explicitly empty)? Why!?

IR::Val ErrorFunc();
IR::Val AsciiFunc();
IR::Val OrdFunc();

using base::check::Is; 
using base::check::Not; 

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

namespace AST {
static const type::Type *DereferenceAll(const type::Type *t) {
  while (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
  return t;
}

std::optional<Binding> Binding::MakeUntyped(
    AST::Expression *fn_expr, const FnArgs<Expression *> &args,
    const std::unordered_map<std::string, size_t> &index_lookup) {
  Binding result(fn_expr, index_lookup.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    result.exprs_[i] = std::pair(nullptr, args.pos_[i]);
  }

  // Match the named arguments
  for (const auto & [ name, expr ] : args.named_) {
    // TODO emit an error explaining why we couldn't use this one if there
    // was a missing named argument.
    auto iter = index_lookup.find(name);
    if (iter == index_lookup.end()) { return std::nullopt; }
    result.exprs_[iter->second] = std::pair(nullptr, expr);
  }
  return result;
}

Binding::Binding(AST::Expression *fn_expr, size_t n)
    : fn_expr_(fn_expr),
      exprs_(n, std::pair<type::Type *, Expression *>(nullptr, nullptr)) {}

void DispatchTable::insert(FnArgs<const type::Type *> call_arg_types,
                           Binding binding, size_t expanded_size) {
  if (expanded_size == std::numeric_limits<size_t>::max()) {
    expanded_size = 1;
    call_arg_types.Apply([&expanded_size](const type::Type *t) {
      if (t->is<type::Variant>()) {
        expanded_size *= t->as<type::Variant>().size();
      }
    });
  }

  total_size_ += expanded_size;
  bindings_.emplace(std::move(call_arg_types), std::move(binding));
}

std::vector<Expression *> FunctionOptions(const std::string &token,
                                          Scope *scope, Context *ctx);

const type::Type *SetDispatchTable(const FnArgs<Expression *> &args,
                                   std::vector<Expression *> fn_options,
                                   AST::DispatchTable *dispatch_table,
                                   Context *ctx);

void Binop::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  lhs->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  rhs->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  if (lhs->type == type::Err || rhs->type == type::Err) {
    type = type::Err;
    limit_to(lhs);
    limit_to(rhs);
    return;
  }

  using Language::Operator;
  if (lhs->lvalue != Assign::LVal &&
      (op == Operator::Assign || op == Operator::OrEq ||
       op == Operator::XorEq || op == Operator::AndEq ||
       op == Operator::AddEq || op == Operator::SubEq ||
       op == Operator::MulEq || op == Operator::DivEq ||
       op == Operator::ModEq)) {
    switch (lhs->lvalue) {
      case Assign::Unset: UNREACHABLE();
      case Assign::Const: ctx->error_log_.AssigningToConstant(span); break;
      case Assign::RVal: ctx->error_log_.AssigningToTemporary(span); break;
      case Assign::LVal: UNREACHABLE();
    }
    limit_to(StageRange::Nothing());
  } else if (op == Operator::Index) {
    lvalue = rhs->lvalue;
  } else if (lhs->lvalue == Assign::Const && rhs->lvalue == Assign::Const) {
    lvalue = Assign::Const;
  } else {
    lvalue = Assign::RVal;
  }

  // TODO if lhs is reserved?
  if (op == Operator::Assign) {
    if (lhs->type->is<type::Tuple>()) {
      if (rhs->type->is<type::Tuple>()) {
        const auto &lhs_entries_ = lhs->type->as<type::Tuple>().entries_;
        const auto &rhs_entries_ = rhs->type->as<type::Tuple>().entries_;

        if (lhs_entries_.size() != rhs_entries_.size()) {
          NOT_YET("error message");
        } else {
          for (size_t i = 0; i < lhs_entries_.size(); ++i) {
            if (!type::CanCastImplicitly(rhs_entries_[i], lhs_entries_[i])) {
              ErrorLog::LogGeneric(
                  this->span,
                  "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
              limit_to(StageRange::NoEmitIR());
            }
          }
        }
      } else {
        LOG << lhs;
        LOG << rhs;
        NOT_YET("error message");
      }
    } else {
      if (rhs->type->is<type::Tuple>()){
        LOG << lhs;
        LOG << rhs;
        NOT_YET("error message");
      } else {
        if (!type::CanCastImplicitly(rhs->type, lhs->type)) {
          ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                               std::to_string(__LINE__) + ": ");
          limit_to(StageRange::NoEmitIR());
        }
      }
    }

    return;
  }

  switch (op) {
    case Operator::Index: {
      type = type::Err;
      if (lhs->type == type::String) {
        if (rhs->type != type::Int) {
          ErrorLog::InvalidStringIndex(span, rhs->type);
          limit_to(StageRange::NoEmitIR());
        }
        type = type::Char;  // Assuming it's a char, even if the index type was
                            // wrong.
        return;
      } else if (!lhs->type->is<type::Array>()) {
        ErrorLog::IndexingNonArray(span, lhs->type);
        limit_to(StageRange::NoEmitIR());
        return;
      } else {
        type = lhs->type->as<type::Array>().data_type;

        if (rhs->type == type::Int) { break; }
        ErrorLog::NonIntegralArrayIndex(span, rhs->type);
        limit_to(StageRange::NoEmitIR());
        return;
      }
    } break;
    case Operator::Dots: NOT_YET();
    case Operator::XorEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Enum>() && rhs->type == lhs->type &&
                 !lhs->type->as<type::Enum>().is_enum_) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.XorEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;
    case Operator::AndEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Enum>() && rhs->type == lhs->type &&
                 !lhs->type->as<type::Enum>().is_enum_) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.AndEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;
    case Operator::OrEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Enum>() && rhs->type == lhs->type &&
                 !lhs->type->as<type::Enum>().is_enum_) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.OrEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;

#define CASE(OpName, symbol, ret_type)                                         \
  case Operator::OpName: {                                                     \
    if ((lhs->type == type::Int && rhs->type == type::Int) ||                  \
        (lhs->type == type::Real && rhs->type == type::Real) ||                \
        (lhs->type == type::Code && rhs->type == type::Code)) {                \
      /* TODO type::Code should only be valid for Add, not Sub, etc */         \
      type = ret_type;                                                         \
    } else {                                                                   \
      FnArgs<Expression *> args;                                               \
      args.pos_ = std::vector{lhs.get(), rhs.get()};                           \
      type      = SetDispatchTable(args, FunctionOptions(symbol, scope_, ctx), \
                              &dispatch_table_, ctx);                          \
      ASSERT(type, Not(Is<type::Tuple>()));                                    \
      if (type == type::Err) { limit_to(StageRange::Nothing()); }              \
    }                                                                          \
  } break;

      CASE(Add, "+", lhs->type);
      CASE(Sub, "-", lhs->type);
      CASE(Div, "/", lhs->type);
      CASE(Mod, "%", lhs->type);
      CASE(AddEq, "+=", type::Void);
      CASE(SubEq, "-=", type::Void);
      CASE(MulEq, "*=", type::Void);
      CASE(DivEq, "/=", type::Void);
      CASE(ModEq, "%=", type::Void);
#undef CASE

    // Mul is done separately because of the function composition
    case Operator::Mul: {
      if ((lhs->type == type::Int && rhs->type == type::Int) ||
          (lhs->type == type::Real && rhs->type == type::Real)) {
        type = lhs->type;

      } else if (lhs->type->is<type::Function>() &&
                 rhs->type->is<type::Function>()) {
        auto *lhs_fn = &lhs->type->as<type::Function>();
        auto *rhs_fn = &rhs->type->as<type::Function>();
        if (rhs_fn->output == lhs_fn->input) {
          type = type::Func({rhs_fn->input}, {lhs_fn->output});

        } else {
          type = type::Err;
          ctx->error_log_.NonComposableFunctions(span);
          limit_to(StageRange::Nothing());
          return;
        }

      } else {
        FnArgs<Expression *> args;
        args.pos_  = std::vector{lhs.get(), rhs.get()};
        type       = SetDispatchTable(args, FunctionOptions("*", scope_, ctx),
                                &dispatch_table_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) { limit_to(StageRange::Nothing()); }
      }
    } break;
    case Operator::Arrow: {
      if (lhs->type != type::Type_) {
        type = type::Err;
        ctx->error_log_.NonTypeFunctionInput(span);
        limit_to(StageRange::Nothing());
        return;
      }
      if (rhs->type != type::Type_) {
        type = type::Err;
        ctx->error_log_.NonTypeFunctionOutput(span);
        limit_to(StageRange::Nothing());
        return;
      }

      if (type != type::Err) { type = type::Type_; }

    } break;
    default: UNREACHABLE();
  }
}

void Access::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  VERIFY_AND_RETURN_ON_ERROR(operand);
  lvalue =
      (operand->type->is<type::Array>() &&
       operand->type->as<type::Array>().fixed_length && member_name == "size")
          ? Assign::Const
          : operand->lvalue;

  auto base_type = DereferenceAll(operand->type);
  if (base_type->is<type::Array>()) {
    if (member_name == "size") {
      type = type::Int;
    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = type::Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type == type::Type_) {
    auto *evaled_type =
        std::get<const type::Type *>(Evaluate(operand.get(), ctx)[0].value);
    if (evaled_type->is<type::Enum>()) {
      // Regardless of whether we can get the value, it's clear that this is
      // supposed to be a member so we should emit an error but carry on
      // assuming that this is an element of that enum type.
      type = evaled_type;
      if (evaled_type->as<type::Enum>().IntValueOrFail(member_name) ==
          std::numeric_limits<size_t>::max()) {
        ErrorLog::MissingMember(span, member_name, evaled_type);
        limit_to(StageRange::NoEmitIR());
      }
    }
  } else if (base_type->is<type::Struct>()) {
    const auto *member = base_type->as<type::Struct>().field(member_name);
    if (member != nullptr) {
      type = member->type;

    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = type::Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type == type::Module) {
    auto module =
        std::get<const Module *>(Evaluate(operand.get(), ctx)[0].value);
    type = module->GetType(member_name);
    if (type == nullptr) {
      ErrorLog::LogGeneric(
          TextSpan(), "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
      type = type::Err;
      limit_to(StageRange::Nothing());
    }

  } else if (base_type->is<type::Primitive>() ||
             base_type->is<type::Function>()) {
    ErrorLog::MissingMember(span, member_name, base_type);
    type = type::Err;
    limit_to(StageRange::Nothing());
  }
}

void ChainOp::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  bool found_err = false;

  lvalue = Assign::Const;
  for (auto &expr : exprs) {
    expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr->type == type::Err) { found_err = true; }
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  if (found_err) {
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  switch (ops[0]) {
    case Language::Operator::Or:
    case Language::Operator::And:
    case Language::Operator::Xor: {
      bool failed = false;
      for (const auto &expr : exprs) {
        if (expr->type != exprs[0]->type) {
          ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                               std::to_string(__LINE__) + ": ");
          failed = true;
        }
      }

      type = exprs[0]->type;

      if (exprs[0]->type != type::Bool &&
          !(exprs[0]->type == type::Type_ &&
            ops[0] == Language::Operator::Or) &&
          (!exprs[0]->type->is<type::Enum>() ||
           exprs[0]->type->as<type::Enum>().is_enum_)) {
        ErrorLog::LogGeneric(
            TextSpan(), "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
        if (failed) {
          limit_to(StageRange::Nothing());
          return;
        }
      }

      return;
    } break;
    default: {
      ASSERT(exprs.size() >= 2u);
      for (size_t i = 0; i < exprs.size() - 1; ++i) {
        const type::Type *lhs_type = exprs[i]->type;
        const type::Type *rhs_type = exprs[i + 1]->type;
        if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
          // TODO struct is wrong. generally user-defined (could be array of
          // struct too, or perhaps a variant containing a struct?) need to
          // figure out the details here.
          const char *token = nullptr;
          switch (ops[i]) {
            case Language::Operator::Lt: token = "<"; break;
            case Language::Operator::Le: token = "<="; break;
            case Language::Operator::Eq: token = "=="; break;
            case Language::Operator::Ne: token = "!="; break;
            case Language::Operator::Ge: token = ">="; break;
            case Language::Operator::Gt: token = ">"; break;
            default: UNREACHABLE();
          }

          FnArgs<Expression *> args;
          args.pos_ = std::vector{exprs[i].get(), exprs[i + 1].get()};
          type = SetDispatchTable(args, FunctionOptions(token, scope_, ctx),
                                  &dispatch_tables_[i], ctx);
          ASSERT(type, Not(Is<type::Tuple>()));
          if (type == type::Err) { limit_to(StageRange::Nothing()); }
        } else {
          if (lhs_type != rhs_type) {
            NOT_YET(lhs_type, " ", rhs_type);
          } else {
            auto cmp = lhs_type->Comparator();

            switch (ops[i]) {
              case Language::Operator::Eq:
              case Language::Operator::Ne: {
                switch (cmp) {
                  case type::Cmp::Order:
                  case type::Cmp::Equality: continue;
                  case type::Cmp::None:
                    type = type::Err;
                    ErrorLog::LogGeneric(
                        TextSpan(),
                        "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
                }
              } break;
              case Language::Operator::Lt:
              case Language::Operator::Le:
              case Language::Operator::Ge:
              case Language::Operator::Gt: {
                switch (cmp) {
                  case type::Cmp::Order: continue;
                  case type::Cmp::Equality:
                  case type::Cmp::None:
                    type = type::Err;
                    ErrorLog::LogGeneric(
                        TextSpan(),
                        "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
                }
              } break;
              default: UNREACHABLE("Expecting a ChainOp operator type.");
            }
          }
        }
      }

      if (type == type::Err) { limit_to(StageRange::Nothing()); }
      type = type::Bool;
    }
  }
}

void CommaList::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  // TODO actually compute value category
  lvalue = Assign::LVal;
  for (auto &expr : exprs) {
    expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr->type == type::Err) { type = type::Err; }
  }
  if (type == type::Err) {
    limit_to(StageRange::Nothing());
    return;
  } else {
    std::vector<const type::Type *> entries;
    entries.reserve(exprs.size());
    for (const auto &expr : exprs) { entries.push_back(expr->type); }
    type = type::Tup(std::move(entries));
  }
}

}  // namespace AST
