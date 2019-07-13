#include "visitor/verify_type.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "error/inference_failure_reason.h"
#include "frontend/operators.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "type/cast.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/parameter_pack.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/util.h"
#include "visitor/dump_ast.h"

Module *CompileModule(Module *mod, std::filesystem::path const *path);

namespace ir {
// TODO Duplicated in emit_ir_val.h
static type::Type const *BuiltinType(core::Builtin b) {
  switch (b) {
#define ICARUS_CORE_BUILTIN_X(enumerator, str, t)                              \
  case core::Builtin::enumerator:                                              \
    return t;
#include "core/builtin.xmacro.h"
#undef ICARUS_CORE_BUILTIN_X
  }
  UNREACHABLE();
}
}  // namespace ir

namespace visitor {
namespace {

// NOTE: the order of these enumerators is meaningful and relied upon! They are
// ordered from strongest relation to weakest.
enum class Cmp { Order, Equality, None };

Cmp Comparator(type::Type const *t);

template <typename TypeContainer>
Cmp MinCmp(TypeContainer const &c) {
  using cmp_t = std::underlying_type_t<Cmp>;
  return static_cast<Cmp>(absl::c_accumulate(
      c, static_cast<cmp_t>(Cmp::Equality), [](cmp_t val, type::Type const *t) {
        return std::min(val, static_cast<cmp_t>(Comparator(t)));
      }));
}

Cmp Comparator(type::Type const *t) {
  using cmp_t = std::underlying_type_t<Cmp>;
  if (auto *v = t->if_as<type::Variant>()) { return MinCmp(v->variants_); }
  if (auto *tup = t->if_as<type::Tuple>()) { return MinCmp(tup->entries_); }
  if (auto *a = t->if_as<type::Array>()) {
    return static_cast<Cmp>(
        std::min(static_cast<cmp_t>(Comparator(a->data_type)),
                 static_cast<cmp_t>(Cmp::Equality)));
  } else if (auto *p = t->if_as<type::Primitive>()) {
    return type::IsNumeric(p) ? Cmp::Order : Cmp::Equality;
  } else if (t->is<type::Flags>() || t->is<type::BufferPointer>()) {
    return Cmp::Order;
  } else if (t->is<type::Enum>() || t->is<type::Pointer>()) {
    return Cmp::Equality;
  } else {
    return Cmp::None;
  }
}

bool VerifyAssignment(TextSpan const &span, type::Type const *to,
                      type::Type const *from, Context *ctx) {
  if (to == from && to->is<type::GenericStruct>()) { return true; }

  // TODO this feels like the semantics are iffy. It works fine if we assign
  // to/from the same type, but we really care if you can assign to a type
  // rather than copy from another, I think.
  if (!from->IsMovable()) {
    ctx->error_log()->NotMovable(span, from->to_string());
    return false;
  }

  if (to == from) { return true; }
  auto *to_tup   = to->if_as<type::Tuple>();
  auto *from_tup = from->if_as<type::Tuple>();
  if (to_tup && from_tup) {
    if (to_tup->size() != from_tup->size()) {
      ctx->error_log()->MismatchedAssignmentSize(span, to_tup->size(),
                                                 from_tup->size());
      return false;
    }

    bool result = true;
    for (size_t i = 0; i < to_tup->size(); ++i) {
      result &= VerifyAssignment(span, to_tup->entries_.at(i),
                                 from_tup->entries_.at(i), ctx);
    }
    return result;
  }

  if (auto *to_var = to->if_as<type::Variant>()) {
    if (auto *from_var = from->if_as<type::Variant>()) {
      for (auto fvar : from_var->variants_) {
        if (!to_var->contains(fvar)) {
          NOT_YET("log an error", from, to);
          return false;
        }
      }
      return true;
    } else {
      if (!to_var->contains(from)) {
        NOT_YET("log an error", from, to);
        return false;
      }

      return true;
    }
  }

  if (auto *to_ptr = to->if_as<type::Pointer>()) {
    if (from == type::NullPtr) { return true; }
    NOT_YET("log an error", from, to);
    return false;
  }

  NOT_YET("log an error: no cast from ", from->to_string(), " to ",
          to->to_string());
}

type::Type const *DereferenceAll(type::Type const *t) {
  while (auto *p = t->if_as<type::Pointer>()) { t = p->pointee; }
  return t;
}

}  // namespace

std::ostream &operator<<(std::ostream &os, VerifyResult r) {
  if (!r.ok()) { return os << "error"; }
  return os << (r.const_ ? "const[" : "non-const[") << r.type_->to_string()
            << "]";
}

VerifyResult VerifyType::operator()(ast::Access const *node, Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto operand_result,
                   node->operand()->VerifyType(this, ctx));

  auto base_type = DereferenceAll(operand_result.type_);
  if (base_type == type::Type_) {
    if (!operand_result.const_) {
      ctx->error_log()->NonConstantTypeMemberAccess(node->span);
      return VerifyResult::Error();
    }
    // TODO We may not be allowed to evaluate node:
    //    f ::= (T: type) => T.key
    // We need to know that T is const
    auto *t           = ctx->type_of(node->operand());
    auto *evaled_type = backend::EvaluateAs<type::Type const *>(
        type::Typed{node->operand(), t}, ctx);

    // For enums and flags, regardless of whether we can get the value, it's
    // clear that node is supposed to be a member so we should emit an error but
    // carry on assuming that node is an element of that enum type.
    if (auto *e = evaled_type->if_as<type::Enum>()) {
      if (!e->Get(node->member_name()).has_value()) {
        ctx->error_log()->MissingMember(node->span, node->member_name(),
                                        evaled_type->to_string());
      }
      return ctx->set_result(node, VerifyResult::Constant(evaled_type));
    } else if (auto *f = evaled_type->if_as<type::Flags>()) {
      if (!f->Get(node->member_name()).has_value()) {
        ctx->error_log()->MissingMember(node->span, node->member_name(),
                                        evaled_type->to_string());
      }
      return ctx->set_result(node, VerifyResult::Constant(evaled_type));
    } else {
      // TODO what about structs? Can structs have constant members we're
      // allowed to access?
      ctx->error_log()->TypeHasNoMembers(node->span);
      return VerifyResult::Error();
    }

  } else if (auto *s = base_type->if_as<type::Struct>()) {
    auto const *member = s->field(node->member_name());
    if (member == nullptr) {
      ctx->error_log()->MissingMember(node->span, node->member_name(),
                                      s->to_string());
      return VerifyResult::Error();
    }

    if (ctx->mod_ != s->defining_module() &&
        absl::c_none_of(member->hashtags_, [](ast::Hashtag h) {
          return h.kind_ == ast::Hashtag::Builtin::Export;
        })) {
      ctx->error_log()->NonExportedMember(node->span, node->member_name(),
                                          s->to_string());
    }

    return ctx->set_result(node,
                           VerifyResult(member->type, operand_result.const_));

  } else if (base_type == type::Module) {
    if (!operand_result.const_) {
      ctx->error_log()->NonConstantModuleMemberAccess(node->span);
      return VerifyResult::Error();
    }

    auto *t = backend::EvaluateAs<Module const *>(
                  type::Typed{node->operand(), operand_result.type_}, ctx)
                  ->GetType(node->member_name());
    if (t == nullptr) {
      ctx->error_log()->NoExportedSymbol(node->span);
      return VerifyResult::Error();
    }

    // TODO is node right?
    return ctx->set_result(node, VerifyResult::Constant(t));
  } else {
    ctx->error_log()->MissingMember(node->span, node->member_name(),
                                    base_type->to_string());
    return VerifyResult::Error();
  }
}

static std::optional<std::vector<VerifyResult>> VerifyWithoutSetting(
    VerifyType *visitor, ast::NodeSpan<ast::Expression const> exprs,
    Context *ctx) {
  std::vector<VerifyResult> results;
  results.reserve(exprs.size());
  for (auto const &expr : exprs) {
    auto r = expr->VerifyType(visitor, ctx);
    if (expr->needs_expansion()) {
      auto &entries = r.type_->as<type::Tuple>().entries_;
      for (auto *t : entries) { results.emplace_back(t, r.const_); }
    } else {
      results.push_back(r);
    }
  }
  if (absl::c_any_of(results, [](VerifyResult const &r) { return !r.ok(); })) {
    return std::nullopt;
  }
  return results;
}

VerifyResult VerifyType::operator()(ast::ArrayLiteral const *node,
                                    Context *ctx) {
  if (node->empty()) {
    return ctx->set_result(node, VerifyResult::Constant(type::EmptyArray));
  }

  ASSIGN_OR(return VerifyResult::Error(), auto expr_results,
                   VerifyWithoutSetting(this, node->elems(), ctx));
  VerifyResult result;
  auto *t      = expr_results.front().type_;
  result.type_ = type::Arr(expr_results.size(), t);
  for (auto expr_result : expr_results) {
    result.const_ &= expr_result.const_;
    if (expr_result.type_ != t) {
      ctx->error_log()->InconsistentArrayType(node->span);
      return VerifyResult::Error();
    }
  }
  return ctx->set_result(node, result);
}

VerifyResult VerifyType::operator()(ast::ArrayType const *node, Context *ctx) {
  std::vector<VerifyResult> length_results;
  length_results.reserve(node->lengths().size());
  bool is_const = true;
  for (auto const &len : node->lengths()) {
    auto result = len->VerifyType(this, ctx);
    is_const &= result.const_;
    length_results.push_back(result);
    if (result.type_ != type::Int64) {
      ctx->error_log()->ArrayIndexType(node->span);
    }
  }

  auto data_type_result = node->data_type()->VerifyType(this, ctx);
  if (data_type_result.type_ != type::Type_) {
    ctx->error_log()->ArrayDataTypeNotAType(node->data_type()->span);
  }

  return ctx->set_result(
      node, VerifyResult(type::Type_, data_type_result.const_ && is_const));
}

static bool IsTypeOrTupleOfTypes(type::Type const *t) {
  if (t == type::Type_) { return true; }
  if (!t->is<type::Tuple>()) { return false; }
  auto &entries = t->as<type::Tuple>().entries_;
  return absl::c_all_of(entries,
                        [](type::Type const *ty) { return ty == type::Type_; });
}

VerifyResult VerifyType::operator()(ast::Binop const *node, Context *ctx) {
  auto lhs_result = node->lhs()->VerifyType(this, ctx);
  auto rhs_result = node->rhs()->VerifyType(this, ctx);
  if (!lhs_result.ok() || !rhs_result.ok()) { return VerifyResult::Error(); }

  using frontend::Operator;
  switch (node->op()) {
    case Operator::Assign: {
      // TODO if lhs is reserved?
      if (!VerifyAssignment(node->span, lhs_result.type_, rhs_result.type_,
                            ctx)) {
        return VerifyResult::Error();
      }
      return VerifyResult::NonConstant(type::Void());
    } break;
    case Operator::XorEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        return ctx->set_result(node, lhs_result);
      } else {
        ctx->error_log()->XorEqNeedsBoolOrFlags(node->span);
        return VerifyResult::Error();
      }
    case Operator::AndEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        return ctx->set_result(node, lhs_result);
      } else {
        ctx->error_log()->AndEqNeedsBoolOrFlags(node->span);
        return VerifyResult::Error();
      }
    case Operator::OrEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        return ctx->set_result(node, lhs_result);
      } else {
        ctx->error_log()->OrEqNeedsBoolOrFlags(node->span);
        return VerifyResult::Error();
      }

#define CASE(OpName, symbol, return_type)                                      \
  case Operator::OpName: {                                                     \
    bool is_const = lhs_result.const_ && rhs_result.const_;                    \
    if (type::IsNumeric(lhs_result.type_) &&                                   \
        type::IsNumeric(rhs_result.type_)) {                                   \
      if (lhs_result.type_ == rhs_result.type_) {                              \
        return ctx->set_result(node, VerifyResult((return_type), is_const));   \
      } else {                                                                 \
        ctx->error_log()->MismatchedBinopArithmeticType(                       \
            lhs_result.type_->to_string(), rhs_result.type_->to_string(),      \
            node->span);                                                       \
        return VerifyResult::Error();                                          \
      }                                                                        \
    } else {                                                                   \
      ast::OverloadSet os(node->scope_, symbol, ctx);                          \
      os.add_adl(symbol, lhs_result.type_);                                    \
      os.add_adl(symbol, rhs_result.type_);                                    \
      return ast::VerifyDispatch(                                              \
          node, os,                                                            \
          core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(      \
              {std::pair{node->lhs(), lhs_result},                             \
               std::pair{node->rhs(), rhs_result}},                            \
              {}),                                                             \
          ctx);                                                                \
    }                                                                          \
  } break;
      CASE(Sub, "-", lhs_result.type_);
      CASE(Mul, "*", lhs_result.type_);
      CASE(Div, "/", lhs_result.type_);
      CASE(Mod, "%", lhs_result.type_);
      CASE(SubEq, "-=", type::Void());
      CASE(MulEq, "*=", type::Void());
      CASE(DivEq, "/=", type::Void());
      CASE(ModEq, "%=", type::Void());
#undef CASE
    case Operator::Add: {
      bool is_const = lhs_result.const_ && rhs_result.const_;
      if (type::IsNumeric(lhs_result.type_) &&
          type::IsNumeric(rhs_result.type_)) {
        if (lhs_result.type_ == rhs_result.type_) {
          return ctx->set_result(node,
                                 VerifyResult(lhs_result.type_, is_const));
        } else {
          ctx->error_log()->MismatchedBinopArithmeticType(
              lhs_result.type_->to_string(), rhs_result.type_->to_string(),
              node->span);
          return VerifyResult::Error();
        }
      } else {
        ast::OverloadSet os(node->scope_, "+", ctx);
        os.add_adl("+", lhs_result.type_);
        os.add_adl("+", rhs_result.type_);
        return ast::VerifyDispatch(
            node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair{node->lhs(), lhs_result},
                 std::pair{node->rhs(), rhs_result}},
                {}),
            ctx);
      }
    } break;
    case Operator::AddEq: {
      bool is_const = lhs_result.const_ && rhs_result.const_;
      if (type::IsNumeric(lhs_result.type_) &&
          type::IsNumeric(rhs_result.type_)) {
        if (lhs_result.type_ == rhs_result.type_) {
          return ctx->set_result(node, VerifyResult(type::Void(), is_const));
        } else {
          ctx->error_log()->MismatchedBinopArithmeticType(
              lhs_result.type_->to_string(), rhs_result.type_->to_string(),
              node->span);
          return VerifyResult::Error();
        }
      } else {
        ast::OverloadSet os(node->scope_, "+=", ctx);
        os.add_adl("+=", lhs_result.type_);
        os.add_adl("+=", rhs_result.type_);
        return ast::VerifyDispatch(
            node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair{node->lhs(), lhs_result},
                 std::pair{node->rhs(), rhs_result}},
                {}),
            ctx);
      }
    } break;
    case Operator::Arrow: {
      type::Type const *t = type::Type_;
      if (!IsTypeOrTupleOfTypes(lhs_result.type_)) {
        t = nullptr;
        ctx->error_log()->NonTypeFunctionInput(node->span);
      }

      if (!IsTypeOrTupleOfTypes(rhs_result.type_)) {
        t = nullptr;
        ctx->error_log()->NonTypeFunctionOutput(node->span);
      }

      if (t == nullptr) { return VerifyResult::Error(); }

      return ctx->set_result(
          node,
          VerifyResult(type::Type_, lhs_result.const_ && rhs_result.const_));
    }
    default: UNREACHABLE();
  }
  UNREACHABLE(static_cast<int>(node->op()));
}

VerifyResult VerifyType::operator()(ast::BlockLiteral const *node,
                                    Context *ctx) {
  // TODO consider not verifying the types of the bodies. They almost certainly
  // contain circular references in the jump statements, and if the functions
  // require verifying the body upfront, things can maybe go wrong?
  for (auto *b : node->before()) { b->VerifyType(this, ctx); }
  for (auto *a : node->after()) { a->VerifyType(this, ctx); }

  return ctx->set_result(node, VerifyResult::Constant(type::Block));
}

VerifyResult VerifyType::operator()(ast::BlockNode const *node, Context *ctx) {
  for (auto *arg : node->args()) { arg->VerifyType(this, ctx); }
  for (auto *stmt : node->stmts()) { stmt->VerifyType(this, ctx); }
  return ctx->set_result(node, VerifyResult::Constant(type::Block));
}

VerifyResult VerifyType::operator()(ast::BuiltinFn const *node, Context *ctx) {
  return ctx->set_result(
      node, VerifyResult::Constant(ir::BuiltinType(node->value())));
}

static ast::OverloadSet FindOverloads(
    core::Scope *scope, std::string_view token,
    core::FnArgs<type::Type const *> arg_types, Context *ctx) {
  ast::OverloadSet os(scope, token, ctx);
  arg_types.Apply([&](type::Type const *t) { os.add_adl(token, t); });
  return os;
}

template <typename EPtr, typename StrType>
static VerifyResult VerifyCall(ast::BuiltinFn const *b,
                               core::FnArgs<EPtr, StrType> const &args,
                               core::FnArgs<VerifyResult> const &arg_results,
                               Context *ctx) {
  switch (b->value()) {
    case core::Builtin::Foreign: {
      bool err = false;
      if (!arg_results.named().empty()) {
        ctx->error_log()->BuiltinError(b->span,
                                       "Built-in function `foreign` cannot be "
                                       "called with named arguments.");
        err = true;
      }

      size_t size = arg_results.size();
      if (size != 2u) {
        ctx->error_log()->BuiltinError(b->span,
                                       "Built-in function `foreign` takes "
                                       "exactly two arguments (You provided " +
                                           std::to_string(size) + ").");
        err = true;
      }

      if (!err) {
        if (arg_results.at(0).type_ != type::ByteView) {
          ctx->error_log()->BuiltinError(
              b->span,
              "First argument to `foreign` must be a byte-view (You provided "
              "a(n) " +
                  arg_results.at(0).type_->to_string() + ").");
        }
        if (!arg_results.at(0).const_) {
          ctx->error_log()->BuiltinError(
              b->span, "First argument to `foreign` must be a constant.");
        }
        if (arg_results.at(1).type_ != type::Type_) {
          ctx->error_log()->BuiltinError(
              b->span,
              "Second argument to `foreign` must be a type (You provided "
              "a(n) " +
                  arg_results.at(0).type_->to_string() + ").");
        }
        if (!arg_results.at(1).const_) {
          ctx->error_log()->BuiltinError(
              b->span, "Second argument to `foreign` must be a constant.");
        }
      }
      return VerifyResult::Constant(backend::EvaluateAs<type::Type const *>(
          type::Typed<ast::Expression const *>{args.at(1), type::Type_}, ctx));
    } break;
    case core::Builtin::Opaque:
      if (!arg_results.empty()) {
        ctx->error_log()->BuiltinError(
            b->span, "Built-in function `opaque` takes no arguments.");
      }
      return VerifyResult::Constant(ir::BuiltinType(core::Builtin::Opaque)
                                        ->as<type::Function>()
                                        .output[0]);

    case core::Builtin::Bytes: {
      size_t size = arg_results.size();
      if (!arg_results.named().empty()) {
        ctx->error_log()->BuiltinError(b->span,
                                       "Built-in function `bytes` cannot be "
                                       "called with named arguments.");
      } else if (size != 1u) {
        ctx->error_log()->BuiltinError(b->span,
                                       "Built-in function `bytes` takes "
                                       "exactly one argument (You provided " +
                                           std::to_string(size) + ").");
      } else if (arg_results.at(0).type_ != type::Type_) {
        ctx->error_log()->BuiltinError(
            b->span,
            "Built-in function `bytes` must take a single argument of type "
            "`type` (You provided a(n) " +
                arg_results.at(0).type_->to_string() + ").");
      }
      return VerifyResult::Constant(ir::BuiltinType(core::Builtin::Bytes)
                                        ->as<type::Function>()
                                        .output[0]);
    }
    case core::Builtin::Alignment: {
      size_t size = arg_results.size();
      if (!arg_results.named().empty()) {
        ctx->error_log()->BuiltinError(b->span,
                                       "Built-in function `alignment` cannot "
                                       "be called with named arguments.");
      }
      if (size != 1u) {
        ctx->error_log()->BuiltinError(b->span,
                                       "Built-in function `alignment` takes "
                                       "exactly one argument (You provided " +
                                           std::to_string(size) + ").");

      } else if (arg_results.at(0).type_ != type::Type_) {
        ctx->error_log()->BuiltinError(
            b->span,
            "Built-in function `alignment` must take a single argument of "
            "type `type` (you provided a(n) " +
                arg_results.at(0).type_->to_string() + ")");
      }
      return VerifyResult::Constant(ir::BuiltinType(core::Builtin::Alignment)
                                        ->as<type::Function>()
                                        .output[0]);
    }
#ifdef DBG
    case core::Builtin::DebugIr:
      // This is for debugging the compiler only, so there's no need to write
      // decent errors here.
      ASSERT(arg_results, matcher::IsEmpty());
      return VerifyResult::Constant(type::Void());
#endif  // DBG
  }
  UNREACHABLE();
}

template <typename EPtr, typename StrType>
static std::pair<core::FnArgs<VerifyResult, StrType>, bool> VerifyFnArgs(
    VerifyType *visitor, core::FnArgs<EPtr, StrType> const &args,
    Context *ctx) {
  bool err         = false;
  auto arg_results = args.Transform([&](EPtr const &expr) {
    auto expr_result = expr->VerifyType(visitor, ctx);
    err |= !expr_result.ok();
    return expr_result;
  });

  return std::pair{std::move(arg_results), err};
}

VerifyResult VerifyType::operator()(ast::Call const *node, Context *ctx) {
  auto [arg_results, err] = VerifyFnArgs(this, node->args(), ctx);
  // TODO handle cyclic dependencies in call arguments.
  if (err) { return VerifyResult::Error(); }

  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    // TODO: Should we allow these to be overloaded?
    ASSIGN_OR(return VerifyResult::Error(), auto result,
                     VerifyCall(b, node->args(), arg_results, ctx));
    return ctx->set_result(node, VerifyResult(result.type_, result.const_));
  }

  ast::OverloadSet overload_set = [&]() {
    if (auto *id = node->callee()->if_as<ast::Identifier>()) {
      return FindOverloads(
          node->scope_, id->token(),
          arg_results.Transform([](VerifyResult const &p) { return p.type_; }),
          ctx);
    } else {
      auto results = node->callee()->VerifyType(this, ctx);
      ast::OverloadSet os;
      os.emplace(node->callee(), results);
      // TODO ADL for node?
      return os;
    }
  }();

  core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>
      arg_expr_result;
  for (size_t i = 0; i < arg_results.pos().size(); ++i) {
    arg_expr_result.pos_emplace(node->args().at(i), arg_results.at(i));
  }
  for (auto const &[name, res] : arg_results.named()) {
    arg_expr_result.named_emplace(
        std::piecewise_construct, std::forward_as_tuple(name),
        std::forward_as_tuple(node->args().at(name), res));
  }

  return ast::VerifyDispatch(node, overload_set, arg_expr_result, ctx);
}

VerifyResult VerifyType::operator()(ast::Cast const *node, Context *ctx) {
  auto expr_result = node->expr()->VerifyType(this, ctx);
  auto type_result = node->type()->VerifyType(this, ctx);
  if (!expr_result.ok() || !type_result.ok()) { return VerifyResult::Error(); }

  if (type_result.type_ != type::Type_) {
    ctx->error_log()->CastToNonType(node->span);
    return VerifyResult::Error();
  }
  if (!type_result.const_) {
    ctx->error_log()->CastToNonConstantType(node->span);
    return VerifyResult::Error();
  }
  auto *t = ASSERT_NOT_NULL(backend::EvaluateAs<type::Type const *>(
      type::Typed<ast::Expression const *>(node->type(), type::Type_), ctx));
  if (t->is<type::Struct>()) {
    ast::OverloadSet os(node->scope_, "as", ctx);
    os.add_adl("as", t);
    os.add_adl("as", expr_result.type_);
    os.keep([t](ast::Overload const &o) { return o.result.type_ == t; });
    return ast::VerifyDispatch(
        node, os,
        core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
            {std::pair(node->expr(), expr_result)}, {}),
        ctx);
  } else {
    if (!type::CanCast(expr_result.type_, t)) {
      ctx->error_log()->InvalidCast(expr_result.type_->to_string(),
                                    t->to_string(), node->span);
      NOT_YET("log an error", expr_result.type_, t);
    }
    return VerifyResult(t, expr_result.const_);
  }
}

VerifyResult VerifyType::operator()(ast::ChainOp const *node, Context *ctx) {
  std::vector<VerifyResult> results;
  results.reserve(node->exprs().size());
  for (auto *expr : node->exprs()) {
    results.push_back(expr->VerifyType(this, ctx));
  }
  if (absl::c_any_of(results, [](VerifyResult const &v) { return !v.ok(); })) {
    return VerifyResult::Error();
  }

  if (node->ops()[0] == frontend::Operator::Or) {
    bool found_err = false;
    for (size_t i = 0; i < results.size() - 1; ++i) {
      if (results[i].type_ == type::Block) {
        if (!results[i].const_) { NOT_YET("log an error: non const block"); }

        ctx->error_log()->EarlyRequiredBlock(node->exprs()[i]->span);
        found_err = true;
      } else {
        goto not_blocks;
      }
    }
    if (found_err) { return VerifyResult::Error(); }
    auto &last = results.back();
    if (last.type_ != type::Block) {
      goto not_blocks;
    } else if (!results.back().const_) {
      NOT_YET("log an error: non const block");
    } else {
      return ctx->set_result(node, VerifyResult::Constant(last.type_));
    }
  }
not_blocks:

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  switch (node->ops()[0]) {
    case frontend::Operator::Or:
    case frontend::Operator::And:
    case frontend::Operator::Xor: {
      bool failed                       = false;
      bool is_const                     = true;
      type::Type const *first_expr_type = results[0].type_;

      for (auto &result : results) {
        // TODO node collection of error messages could be greatly improved.
        if (result.type_ != first_expr_type) {
          auto op_str = [node] {
            switch (node->ops()[0]) {
              case frontend::Operator::Or: return "|";
              case frontend::Operator::And: return "&";
              case frontend::Operator::Xor: return "^";
              default: UNREACHABLE();
            }
          }();

          NOT_YET("Log an error");
          is_const &= result.const_;
          failed = true;
        }
      }

      if (failed) { return VerifyResult::Error(); }
      return ctx->set_result(node, VerifyResult(first_expr_type, is_const));
    } break;
    default: {
      bool is_const = results[0].const_;
      ASSERT(node->exprs().size() >= 2u);
      for (size_t i = 0; i + 1 < node->exprs().size(); ++i) {
        VerifyResult const &lhs_result = results[i];
        VerifyResult const &rhs_result = results[i + 1];
        is_const &= rhs_result.const_;

        // TODO struct is wrong. generally user-defined (could be array of
        // struct too, or perhaps a variant containing a struct?) need to
        // figure out the details here.
        const char *token = nullptr;
        switch (node->ops()[i]) {
          case frontend::Operator::Lt: token = "<"; break;
          case frontend::Operator::Le: token = "<="; break;
          case frontend::Operator::Eq: token = "=="; break;
          case frontend::Operator::Ne: token = "!="; break;
          case frontend::Operator::Ge: token = ">="; break;
          case frontend::Operator::Gt: token = ">"; break;
          default: UNREACHABLE();
        }

        if (lhs_result.type_->is<type::Struct>() ||
            lhs_result.type_->is<type::Struct>()) {
          // TODO overwriting type a bunch of times?
          ast::OverloadSet os(node->scope_, token, ctx);
          os.add_adl(token, lhs_result.type_);
          os.add_adl(token, rhs_result.type_);
          return ast::VerifyDispatch(
              ast::ExprPtr{node->exprs()[i], 0x01}, os,
              core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                  {std::pair(node->exprs()[i], lhs_result),
                   std::pair(node->exprs()[i + 1], rhs_result)},
                  {}),
              ctx);
        }

        if (lhs_result.type_ != rhs_result.type_ &&
            !(lhs_result.type_->is<type::Pointer>() &&
              rhs_result.type_ == type::NullPtr) &&
            !(rhs_result.type_->is<type::Pointer>() &&
              lhs_result.type_ == type::NullPtr)) {
          NOT_YET("Log an error", lhs_result.type_->to_string(),
                  rhs_result.type_->to_string(), node);

        } else {
          auto cmp = Comparator(lhs_result.type_);

          switch (node->ops()[i]) {
            case frontend::Operator::Eq:
            case frontend::Operator::Ne: {
              switch (cmp) {
                case Cmp::Order:
                case Cmp::Equality: continue;
                case Cmp::None:
                  ctx->error_log()->ComparingIncomparables(
                      lhs_result.type_->to_string(),
                      rhs_result.type_->to_string(),
                      TextSpan(node->exprs()[i]->span,
                               node->exprs()[i + 1]->span));
                  return VerifyResult::Error();
              }
            } break;
            case frontend::Operator::Lt:
            case frontend::Operator::Le:
            case frontend::Operator::Ge:
            case frontend::Operator::Gt: {
              switch (cmp) {
                case Cmp::Order: continue;
                case Cmp::Equality:
                case Cmp::None:
                  ctx->error_log()->ComparingIncomparables(
                      lhs_result.type_->to_string(),
                      rhs_result.type_->to_string(),
                      TextSpan(node->exprs()[i]->span,
                               node->exprs()[i + 1]->span));
                  return VerifyResult::Error();
              }
            } break;
            default: UNREACHABLE("Expecting a ChainOp operator type.");
          }
        }
      }

      return ctx->set_result(node, VerifyResult(type::Bool, is_const));
    }
  }
}

VerifyResult VerifyType::operator()(ast::CommaList const *node, Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto results,
                   VerifyWithoutSetting(
                       this, ast::NodeSpan<ast::Expression const>(node->exprs_),
                       ctx));
  std::vector<type::Type const *> ts;
  ts.reserve(results.size());
  bool is_const = true;
  for (auto const &r : results) {
    ts.push_back(r.type_);
    is_const &= r.const_;
  }
  return ctx->set_result(node,
                         VerifyResult(type::Tup(std::move(ts)), is_const));
}

static VerifyResult VerifySpecialFunctions(ast::Declaration const *decl,
                                           type::Type const *decl_type,
                                           Context *ctx) {
  bool error = false;
  if (decl->id() == "copy") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (!f->output.empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->input.size() != 2 || f->input.at(0) != f->input.at(1) ||
          !f->input.at(0)->is<type::Pointer>() ||
          !f->input.at(0)->as<type::Pointer>().pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect input type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s =
            f->input.at(0)->as<type::Pointer>().pointee->as<type::Struct>();

        if (decl->scope_ != s.scope_) {
          error = true;
          NOT_YET(
              "(copy) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(ast::Hashtag::Builtin::Uncopyable)) {
          NOT_YET("defined (copy) on a non-copyable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (copy) must be a function.");
    }
  } else if (decl->id() == "move") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (!f->output.empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->input.size() != 2 || f->input.at(0) != f->input.at(1) ||
          !f->input.at(0)->is<type::Pointer>() ||
          !f->input.at(0)->as<type::Pointer>().pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect input type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s =
            f->input.at(0)->as<type::Pointer>().pointee->as<type::Struct>();

        if (decl->scope_ != s.scope_) {
          error = true;
          NOT_YET(
              "(move) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(ast::Hashtag::Builtin::Immovable)) {
          error = true;
          NOT_YET("defined (move) for an immovable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (move) must be a function.");
    }
  }
  if (error) { ctx->set_result(decl, VerifyResult::Error()); }

  return ctx->set_result(
      decl,
      VerifyResult(decl_type, decl->flags() & ast::Declaration::f_IsConst));
}

// TODO what about shadowing of symbols across module boundaries imported with
// -- ::= ?
// Or when you import two modules verifying that symbols don't conflict.
bool Shadow(type::Typed<ast::Declaration const *> decl1,
            type::Typed<ast::Declaration const *> decl2, Context *ctx) {
  // TODO Don't worry about generic shadowing? It'll be checked later?
  if (decl1.type() == type::Generic || decl2.type() == type::Generic) {
    return false;
  }

  auto ExtractParams = +[](ast::Declaration const *decl,
                           Context *ctx) -> core::FnParams<type::Type const *> {
    bool is_const               = (decl->flags() & ast::Declaration::f_IsConst);
    ast::Expression const *expr = decl->init_val();
    if (!is_const) {
      return ctx->type_of(expr)
          ->as<type::Function>()
          .AnonymousFnParams()
          .Transform([](type::Typed<ast::Expression const *> expr) {
            return expr.type();
          });
    } else if (auto *fn_lit = expr->if_as<ast::FunctionLiteral>()) {
      return fn_lit->inputs_.Transform(
          [ctx](std::unique_ptr<ast::Declaration> const &decl) {
            return ctx->type_of(decl.get());
          });
    }
    NOT_YET();
  };
  return core::AmbiguouslyCallable(
      ExtractParams(*decl1, ctx), ExtractParams(*decl2, ctx),
      [](type::Type const *lhs, type::Type const *rhs) {
        return type::Meet(lhs, rhs) != nullptr;
      });
}

enum DeclKind { INFER = 1, CUSTOM_INIT = 2, UNINITIALIZED = 4 };

static InferenceFailureReason Inferrable(type::Type const *t) {
  if (t == type::NullPtr) { return InferenceFailureReason::NullPtr; }
  if (t == type::EmptyArray) { return InferenceFailureReason::EmptyArray; }
  if (auto *a = t->if_as<type::Array>()) { return Inferrable(a->data_type); }
  if (auto *p = t->if_as<type::Pointer>()) { return Inferrable(p->pointee); }
  if (auto *v = t->if_as<type::Variant>()) {
    // TODO only returning the first failure here and not even givving a good
    // explanation of precisely what the problem is. Fix here and below.
    for (auto const *var : v->variants_) {
      auto reason = Inferrable(var);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  } else if (auto *tup = t->if_as<type::Tuple>()) {
    for (auto const *entry : tup->entries_) {
      auto reason = Inferrable(entry);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  } else if (auto *f = t->if_as<type::Function>()) {
    for (auto const *t : f->input) {
      auto reason = Inferrable(t);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
    for (auto const *t : f->output) {
      auto reason = Inferrable(t);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  }
  // TODO higher order types?
  return InferenceFailureReason::Inferrable;
}

VerifyResult VerifyType::operator()(ast::Declaration const *node,
                                    Context *ctx) {
  bool swap_bc    = ctx->mod_ != node->module();
  Module *old_mod = std::exchange(ctx->mod_, node->module());
  if (swap_bc) {
    // TODO constants
  }
  base::defer d([&] {
    ctx->mod_ = old_mod;
    if (swap_bc) {
      // TODO constants
    }
  });

  // Declarations may have already been computed. Essentially the first time we
  // see an identifier (either a real identifier node, or a declaration, we need
  // to verify the type, but we only want to do node once.
  if (auto *attempt = ctx->prior_verification_attempt(node)) {
    return *attempt;
  }

  int dk = 0;
  if (node->IsInferred()) { dk = INFER; }
  if (node->IsUninitialized()) {
    dk |= UNINITIALIZED;
  } else if (node->IsCustomInitialized()) {
    dk |= CUSTOM_INIT;
  }
  type::Type const *node_type = nullptr;
  switch (dk) {
    case 0 /* Default initailization */: {
      ASSIGN_OR(return ctx->set_result(node, VerifyResult::Error()),
                       auto type_expr_result,
                       node->type_expr()->VerifyType(this, ctx));
      if (!type_expr_result.const_) {
        // Hmm, not necessarily an error. Example (not necessarily minimal):
        //
        //   S ::= (x: any`T) => struct {
        //     _val: T = x
        //   }
        //
        NOT_YET("log an error", DumpAst::ToString(node));
        return ctx->set_result(node, VerifyResult::Error());
      }
      auto *type_expr_type = type_expr_result.type_;
      if (type_expr_type == type::Type_) {
        node_type = ASSERT_NOT_NULL(
            ctx->set_result(node,
                            VerifyResult::Constant(
                                backend::EvaluateAs<type::Type const *>(
                                    type::Typed<ast::Expression const *>(
                                        node->type_expr(), type_expr_type),
                                    ctx)))
                .type_);

        if (!(node->flags() & ast::Declaration::f_IsFnParam) &&
            !node_type->IsDefaultInitializable()) {
          ctx->error_log()->TypeMustBeInitialized(node->span,
                                                  node_type->to_string());
        }

      } else if (type_expr_type == type::Intf) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          return ctx->set_result(node, VerifyResult::Error());
        } else {
          node_type = ctx->set_result(
                             node, VerifyResult::Constant(
                                       backend::EvaluateAs<type::Type const *>(
                                           type::Typed<ast::Expression const *>(
                                               node->type_expr(), type::Type_),
                                           ctx)))
                          .type_;
        }
      } else {
        ctx->error_log()->NotAType(node->type_expr()->span,
                                   type_expr_type->to_string());
        return ctx->set_result(node, VerifyResult::Error());
      }
    } break;
    case INFER: UNREACHABLE(); break;
    case INFER | CUSTOM_INIT: {
      ASSIGN_OR(return ctx->set_result(node, VerifyResult::Error()),
                       auto init_val_result,
                       node->init_val()->VerifyType(this, ctx));

      auto reason = Inferrable(init_val_result.type_);
      if (reason != InferenceFailureReason::Inferrable) {
        ctx->error_log()->UninferrableType(reason, node->init_val()->span);
        return ctx->set_result(node, VerifyResult::Error());
      }

      // TODO initialization, not assignment.
      if (!VerifyAssignment(node->span, init_val_result.type_,
                            init_val_result.type_, ctx)) {
        return ctx->set_result(node, VerifyResult::Error());
      }

      node_type =
          ctx->set_result(node, VerifyResult(init_val_result.type_,
                                             (node->flags() &
                                              ast::Declaration::f_IsConst)))
              .type_;
    } break;
    case INFER | UNINITIALIZED: {
      ctx->error_log()->UninferrableType(InferenceFailureReason::Hole,
                                         node->init_val()->span);
      if (node->flags() & ast::Declaration::f_IsConst) {
        ctx->error_log()->UninitializedConstant(node->span);
      }
      return ctx->set_result(node, VerifyResult::Error());
    } break;
    case CUSTOM_INIT: {
      auto init_val_result = node->init_val()->VerifyType(this, ctx);
      bool error           = !init_val_result.ok();

      auto *init_val_type   = node->init_val()->VerifyType(this, ctx).type_;
      auto type_expr_result = node->type_expr()->VerifyType(this, ctx);
      auto *type_expr_type  = type_expr_result.type_;

      if (type_expr_type == nullptr) {
        error = true;
      } else if (type_expr_type == type::Type_) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          error = true;
        } else {
          node_type = ctx->set_result(
                             node, VerifyResult::Constant(
                                       backend::EvaluateAs<type::Type const *>(
                                           type::Typed<ast::Expression const *>(
                                               node->type_expr(), type::Type_),
                                           ctx)))
                          .type_;
        }

        // TODO initialization, not assignment. Error messages will be
        // wrong.
        if (node_type != nullptr && init_val_type != nullptr) {
          error |= !VerifyAssignment(node->span, node_type, init_val_type, ctx);
        }
      } else if (type_expr_type == type::Intf) {
        node_type =
            ctx->set_result(node, VerifyResult::Constant(type::Generic)).type_;
      } else {
        ctx->error_log()->NotAType(node->type_expr()->span,
                                   type_expr_type->to_string());
        error = true;
      }

      if (error) { return ctx->set_result(node, VerifyResult::Error()); }
    } break;
    case UNINITIALIZED: {
      ASSIGN_OR(return ctx->set_result(node, VerifyResult::Error()),
                       auto type_expr_result,
                       node->type_expr()->VerifyType(this, ctx));
      auto *type_expr_type = type_expr_result.type_;
      if (type_expr_type == type::Type_) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          return ctx->set_result(node, VerifyResult::Error());
        }
        node_type =
            ctx->set_result(node, VerifyResult::Constant(
                                      backend::EvaluateAs<type::Type const *>(
                                          type::Typed<ast::Expression const *>(
                                              node->type_expr(), type::Type_),
                                          ctx)))
                .type_;
      } else if (type_expr_type == type::Intf) {
        node_type =
            ctx->set_result(node, VerifyResult::Constant(type::Generic)).type_;
      } else {
        ctx->error_log()->NotAType(node->type_expr()->span,
                                   type_expr_type->to_string());
        return ctx->set_result(node, VerifyResult::Error());
      }

      if (node->flags() & ast::Declaration::f_IsConst) {
        ctx->error_log()->UninitializedConstant(node->span);
        return ctx->set_result(node, VerifyResult::Error());
      }

    } break;
    default: UNREACHABLE(dk);
  }

  if (node->id().empty()) {
    if (node_type == type::Module) {
      // TODO check shadowing against other modules?
      // TODO what if no init val is provded? what if not constant?
      node->scope_->embedded_modules_.insert(
          backend::EvaluateAs<Module const *>(
              type::Typed<ast::Expression const *>(node->init_val(),
                                                   type::Module),
              ctx));
      return ctx->set_result(node, VerifyResult::Constant(type::Module));
    } else if (node_type->is<type::Tuple>()) {
      NOT_YET(node_type, DumpAst::ToString(node));
    } else {
      NOT_YET(node_type, DumpAst::ToString(node));
    }
  }

  // TODO simplify now that you don't have error decls.
  ASSERT(node_type != nullptr) << DumpAst::ToString(node);
  std::vector<type::Typed<ast::Declaration const *>> decls_to_check;
  {
    auto good_decls_to_check = node->scope_->AllDeclsWithId(node->id());
    size_t num_total         = good_decls_to_check.size();
    auto iter                = node->scope_->child_decls_.find(node->id());

    bool has_children = (iter != node->scope_->child_decls_.end());
    if (has_children) { num_total += iter->second.size(); }

    decls_to_check.reserve(num_total);
    for (auto *decl : good_decls_to_check) {
      decls_to_check.emplace_back(decl, ctx->type_of(decl));
    }

    if (has_children) {
      for (auto *decl : iter->second) {
        decls_to_check.emplace_back(decl, ctx->type_of(decl));
      }
    }
  }

  auto iter = std::partition(decls_to_check.begin(), decls_to_check.end(),
                             [node](type::Typed<ast::Declaration const *> td) {
                               return node >= td.get();
                             });
  bool failed_shadowing = false;
  while (iter != decls_to_check.end()) {
    auto typed_decl = *iter;
    if (Shadow(type::Typed(node, node_type), typed_decl, ctx)) {
      failed_shadowing = true;
      ctx->error_log()->ShadowingDeclaration(node->span, (*typed_decl)->span);
    }
    ++iter;
  }

  if (failed_shadowing) {
    // TODO node may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else on
    // a different branch could find it unambiguously. It's also just a hack
    // from the get-go so maybe we should just do it the right way.
    return ctx->set_result(node, VerifyResult::Error());
  }

  return VerifySpecialFunctions(node, node_type, ctx);
}

VerifyResult VerifyType::operator()(ast::EnumLiteral const *node,
                                    Context *ctx) {
  for (auto const &elem : node->elems()) {
    if (auto *decl = elem->if_as<ast::Declaration>()) {
      auto *t = decl->init_val()->VerifyType(this, ctx).type_;
      ASSERT(t == type::Int32);
      // TODO determine what is allowed here and how to generate errors.
    }
  }

  return ctx->set_result(node, VerifyResult::Constant(type::Type_));
}

// TODO there's not that much shared between the inferred and uninferred cases,
// so probably break them out.
VerifyResult VerifyBody(VerifyType *visitor, ast::FunctionLiteral const *node,
                        Context *ctx) {
  for (auto const &stmt : node->statements_) { stmt->VerifyType(visitor, ctx); }
  // TODO propogate cyclic dependencies.

  ExtractJumps extract_visitor;
  for (auto const &stmt : node->statements_) {
    stmt->ExtractJumps(&extract_visitor);
  }

  // TODO we can have yields and returns, or yields and jumps, but not jumps and
  // returns. Check this.
  absl::flat_hash_set<type::Type const *> types;
  absl::flat_hash_map<ast::ReturnStmt const *, type::Type const *>
      saved_ret_types;
  for (auto const *n : extract_visitor.jumps(ExtractJumps::Kind::Return)) {
    if (auto const *ret_node = n->if_as<ast::ReturnStmt>()) {
      std::vector<type::Type const *> ret_types;
      for (auto const *expr : ret_node->exprs()) {
        ret_types.push_back(ctx->type_of(expr));
      }
      auto *t = Tup(std::move(ret_types));
      types.emplace(t);
      saved_ret_types.emplace(ret_node, t);
    } else {
      UNREACHABLE();  // TODO
    }
  }

  std::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(node->inputs_.size());
  for (auto &input : node->inputs_) {
    input_type_vec.push_back(ASSERT_NOT_NULL(ctx->type_of(input.value.get())));
  }

  if (!node->outputs_) {
    std::vector<type::Type const *> output_type_vec(
        std::make_move_iterator(types.begin()),
        std::make_move_iterator(types.end()));

    if (types.size() > 1) { NOT_YET("log an error"); }
    auto f = type::Func(std::move(input_type_vec), std::move(output_type_vec));
    return ctx->set_result(node, VerifyResult::Constant(f));

  } else {
    auto *node_type  = ctx->type_of(node);
    auto const &outs = ASSERT_NOT_NULL(node_type)->as<type::Function>().output;
    switch (outs.size()) {
      case 0: {
        bool err = false;
        for (auto *n : extract_visitor.jumps(ExtractJumps::Kind::Return)) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            if (!ret_node->exprs().empty()) {
              ctx->error_log()->NoReturnTypes(ret_node);
              err = true;
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? VerifyResult::Error() : VerifyResult::Constant(node_type);
      } break;
      case 1: {
        bool err = false;
        for (auto *n : extract_visitor.jumps(ExtractJumps::Kind::Return)) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *t = ASSERT_NOT_NULL(saved_ret_types.at(ret_node));
            if (t == outs[0]) { continue; }
            ctx->error_log()->ReturnTypeMismatch(
                outs[0]->to_string(), t->to_string(), ret_node->span);
            err = true;
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return err ? VerifyResult::Error() : VerifyResult::Constant(node_type);
      } break;
      default: {
        for (auto *n : extract_visitor.jumps(ExtractJumps::Kind::Return)) {
          if (auto *ret_node = n->if_as<ast::ReturnStmt>()) {
            auto *expr_type = ASSERT_NOT_NULL(saved_ret_types.at(ret_node));
            if (expr_type->is<type::Tuple>()) {
              auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
              if (tup_entries.size() != outs.size()) {
                ctx->error_log()->ReturningWrongNumber(
                    ret_node->span,
                    (expr_type->is<type::Tuple>()
                         ? expr_type->as<type::Tuple>().size()
                         : 1),
                    outs.size());
                return VerifyResult::Error();
              } else {
                bool err = false;
                for (size_t i = 0; i < tup_entries.size(); ++i) {
                  if (tup_entries.at(i) != outs.at(i)) {
                    // TODO if this is a commalist we can point to it more
                    // carefully but if we're just passing on multiple return
                    // values it's harder.
                    //
                    // TODO point the span to the correct entry which may be
                    // hard if it's splatted.
                    ctx->error_log()->IndexedReturnTypeMismatch(
                        outs.at(i)->to_string(), tup_entries.at(i)->to_string(),
                        ret_node->span, i);
                    err = true;
                  }
                }
                if (err) { return VerifyResult::Error(); }
              }
            } else {
              ctx->error_log()->ReturningWrongNumber(
                  ret_node->span,
                  (expr_type->is<type::Tuple>()
                       ? expr_type->as<type::Tuple>().size()
                       : 1),
                  outs.size());
              return VerifyResult::Error();
            }
          } else {
            UNREACHABLE();  // TODO
          }
        }
        return VerifyResult::Constant(node_type);
      } break;
    }
  }
}

void VerifyBody(VerifyType *visitor, ast::JumpHandler const *node,
                Context *ctx) {
  DEBUG_LOG("JumpHandler")(DumpAst::ToString(node));
  ExtractJumps extract_visitor;
  for (auto const *stmt : node->stmts()) {
    stmt->VerifyType(visitor, ctx);
    stmt->ExtractJumps(&extract_visitor);
  }

  auto jumps = extract_visitor.jumps(ExtractJumps::Kind::Jump);
  for (auto const *jump : jumps) {
    DEBUG_LOG("JumpHandler")(DumpAst::ToString(&jump->as<ast::Jump>()));
    for (auto const &jump_opt : jump->as<ast::Jump>().options_) {
      DEBUG_LOG("JumpHandler")
      (jump_opt.block, " args=(", jump_opt.args, ")");
    }
  }
}

VerifyResult VerifyType::ConcreteFnLit(ast::FunctionLiteral const *node,
                                       Context *ctx) {
  std::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(node->inputs_.size());
  for (auto &d : node->inputs_) {
    ASSIGN_OR(return _, auto result, d.value->VerifyType(this, ctx));
    input_type_vec.push_back(result.type_);
  }

  std::vector<type::Type const *> output_type_vec;
  bool error = false;
  if (node->outputs_) {
    output_type_vec.reserve(node->outputs_->size());
    for (auto &output : *node->outputs_) {
      auto result = output->VerifyType(this, ctx);
      output_type_vec.push_back(result.type_);
      if (result.type_ != nullptr && !result.const_) {
        // TODO this feels wrong because output could be a decl. And that decl
        // being a const decl isn't what I care about.
        NOT_YET("log an error");
        error = true;
      }
    }
  }

  if (error ||
      absl::c_any_of(input_type_vec,
                     [](type::Type const *t) { return t == nullptr; }) ||
      absl::c_any_of(output_type_vec,
                     [](type::Type const *t) { return t == nullptr; })) {
    return VerifyResult::Error();
  }

  // TODO need a better way to say if there was an error recorded in a
  // particular section of compilation. Right now we just have the grad total
  // count.
  if (ctx->num_errors() > 0) {
    ctx->error_log()->Dump();
    return VerifyResult::Error();
  }

  if (node->outputs_) {
    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (auto *decl = node->outputs_->at(i)->if_as<ast::Declaration>()) {
        output_type_vec.at(i) = ctx->type_of(decl);
      } else {
        ASSERT(output_type_vec.at(i) == type::Type_);
        output_type_vec.at(i) = backend::EvaluateAs<type::Type const *>(
            type::Typed<ast::Expression const *>(node->outputs_->at(i).get(),
                                                 type::Type_),
            ctx);
      }
    }

    return ctx->set_result(
        node, VerifyResult::Constant(type::Func(std::move(input_type_vec),
                                                std::move(output_type_vec))));
  } else {
    return VerifyBody(this, node, ctx);
  }
}

VerifyResult VerifyType::operator()(ast::FunctionLiteral const *node,
                                    Context *ctx) {
  for (auto const &p : node->inputs_) {
    if ((p.value->flags() & ast::Declaration::f_IsConst) ||
        !node->param_dep_graph_.at(p.value.get()).empty()) {
      return ctx->set_result(node, VerifyResult::Constant(type::Generic));
    }
  }

  return ConcreteFnLit(node, ctx);
}

VerifyResult VerifyType::operator()(ast::Identifier const *node, Context *ctx) {
  for (auto iter = ctx->cyc_deps_.begin(); iter != ctx->cyc_deps_.end();
       ++iter) {
    if (*iter == node) {
      ctx->error_log()->CyclicDependency(
          std::vector<ast::Identifier const *>(iter, ctx->cyc_deps_.end()));
      return VerifyResult::Error();
    }
  }
  ctx->cyc_deps_.push_back(node);
  base::defer d([&] { ctx->cyc_deps_.pop_back(); });

  // `node->decl()` is not necessarily null. Because we may call VerifyType many
  // times in multiple contexts, it is null the first time, but not on future
  // iterations.
  //
  // TODO that means we should probably resolve identifiers ahead of
  // type verification, but I think we rely on type information to figure it out
  // for now so you'll have to undo that first.
  if (node->decl() == nullptr) {
    auto potential_decls = node->scope_->AllDeclsWithId(node->token());
    switch (potential_decls.size()) {
      case 1: {
        // TODO could it be that evn though there is only one declaration,
        // there's a bound constant of the same name? If so, we need to deal
        // with node case.
        const_cast<ast::Identifier *>(node)->set_decl(potential_decls[0]);
        if (node->decl() == nullptr) { return VerifyResult::Error(); }
      } break;
      case 0:
        ctx->error_log()->UndeclaredIdentifier(node);
        return VerifyResult::Error();
      default:
        // TODO Should we allow the overload?
        ctx->error_log()->UnspecifiedOverload(node->span);
        return VerifyResult::Error();
    }

    if (!(node->decl()->flags() & ast::Declaration::f_IsConst) &&
        (node->span.start.line_num < node->decl()->span.start.line_num ||
         (node->span.start.line_num == node->decl()->span.start.line_num &&
          node->span.start.offset < node->decl()->span.start.offset))) {
      ctx->error_log()->DeclOutOfOrder(node->decl(), node);
    }
  }

  // TODO node is because we may have determined the declartaion previously with
  // a different generic setup but not bound the type for node context. But node
  // is wrong in the sense that the declaration bound is possibly dependent on
  // the context.
  type::Type const *t = ctx->type_of(node->decl());

  if (t == nullptr) { return VerifyResult::Error(); }
  return ctx->set_result(
      node,
      VerifyResult(t, node->decl()->flags() & ast::Declaration::f_IsConst));
}

VerifyResult VerifyType::operator()(ast::Import const *node, Context *ctx) {
  ASSIGN_OR(return _, auto result, node->operand()->VerifyType(this, ctx));
  bool err = false;
  if (result.type_ != type::ByteView) {
    // TODO allow (import) overload
    ctx->error_log()->InvalidImport(node->operand()->span);
    err = true;
  }

  if (!result.const_) {
    ctx->error_log()->NonConstantImport(node->operand()->span);
    err = true;
  }

  if (err) { return VerifyResult::Error(); }
  // TODO storing node might not be safe.
  auto src = backend::EvaluateAs<std::string_view>(
      type::Typed<ast::Expression const *>(node->operand(), type::ByteView),
      ctx);
  ASSIGN_OR(ctx->error_log()->MissingModule(src, *ctx->mod_->path_);
            return VerifyResult::Error(),  //
                   auto pending_mod,
                   core::ImportModule(std::filesystem::path{src},
                                      *ctx->mod_->path_, CompileModule));

  if (!pending_mod.valid()) { return VerifyResult::Error(); }
  ctx->set_pending_module(node, pending_mod);
  return ctx->set_result(node, VerifyResult::Constant(type::Module));
}

VerifyResult VerifyType::operator()(ast::Index const *node, Context *ctx) {
  auto lhs_result = node->lhs()->VerifyType(this, ctx);
  auto rhs_result = node->rhs()->VerifyType(this, ctx);
  if (!lhs_result.ok() || !rhs_result.ok()) { return VerifyResult::Error(); }

  auto *index_type = rhs_result.type_->if_as<type::Primitive>();
  if (!index_type || !index_type->is_integral()) {
    ctx->error_log()->InvalidIndexType(node->span,
                                       lhs_result.type_->to_string(),
                                       lhs_result.type_->to_string());
  }

  if (lhs_result.type_ == type::ByteView) {
    return ctx->set_result(node, VerifyResult(type::Nat8, rhs_result.const_));
  } else if (auto *lhs_array_type = lhs_result.type_->if_as<type::Array>()) {
    return ctx->set_result(
        node, VerifyResult(lhs_array_type->data_type, rhs_result.const_));
  } else if (auto *lhs_buf_type =
                 lhs_result.type_->if_as<type::BufferPointer>()) {
    return ctx->set_result(
        node, VerifyResult(lhs_buf_type->pointee, rhs_result.const_));
  } else if (auto *tup = lhs_result.type_->if_as<type::Tuple>()) {
    if (!rhs_result.const_) {
      ctx->error_log()->NonConstantTupleIndex(node->span);
      return VerifyResult::Error();
    }

    int64_t index = [&]() -> int64_t {
      auto results =
          backend::Evaluate(type::Typed{node->rhs(), index_type}, ctx);
      if (index_type == type::Int8) { return results.get<int8_t>(0).val_; }
      if (index_type == type::Int16) { return results.get<int16_t>(0).val_; }
      if (index_type == type::Int32) { return results.get<int32_t>(0).val_; }
      if (index_type == type::Int64) { return results.get<int64_t>(0).val_; }
      if (index_type == type::Nat8) { return results.get<uint8_t>(0).val_; }
      if (index_type == type::Nat16) { return results.get<uint16_t>(0).val_; }
      if (index_type == type::Nat32) { return results.get<uint32_t>(0).val_; }
      if (index_type == type::Nat64) { return results.get<uint64_t>(0).val_; }
      UNREACHABLE();
    }();

    if (index < 0 || index >= static_cast<int64_t>(tup->size())) {
      ctx->error_log()->IndexingTupleOutOfBounds(node->span, tup->to_string(),
                                                 tup->size(), index);
      return VerifyResult::Error();
    }

    return ctx->set_result(
        node, VerifyResult(tup->entries_.at(index), lhs_result.const_));

  } else {
    ctx->error_log()->InvalidIndexing(node->span,
                                      lhs_result.type_->to_string());
    return VerifyResult::Error();
  }
}

VerifyResult VerifyType::operator()(ast::Interface const *node, Context *ctx) {
  for (auto const *decl : node->decls()) {
    decl->VerifyType(this, ctx);
    if (decl->init_val() != nullptr) { NOT_YET(); }
  }
  return ctx->set_result(node, VerifyResult::Constant(type::Intf));
}

VerifyResult VerifyType::operator()(ast::Jump const *node, Context *ctx) {
  DEBUG_LOG("JumpHandler")(DumpAst::ToString(node));
  std::vector<core::FnArgs<VerifyResult>> arg_results;
  arg_results.reserve(node->options_.size());
  bool err = false;
  for (auto const &opt : node->options_) {
    auto [arg_result, arg_err] = VerifyFnArgs(this, opt.args, ctx);
    err |= arg_err;
    arg_results.push_back(std::move(arg_result));
  }
  if (err) { NOT_YET(); }
  ir::ScopeDef *scope_def = ctx->scope_def(
      node->scope_->Containing<core::ScopeLitScope>()->scope_lit_);
  DEBUG_LOG("JumpHandler")(scope_def->blocks_);
  for (auto const &opt : node->options_) {
    if (opt.block == "start") {
    } else if (opt.block == "exit") {
    } else {
      auto iter = scope_def->blocks_.find(opt.block);
      if (iter == scope_def->blocks_.end()) { NOT_YET(opt.block); }
      auto block_def = &iter->second;
      // TODO you're re-verifying each unnecessarily.
      auto args = opt.args.Transform(
          [ctx, this](std::unique_ptr<ast::Expression> const &arg)
              -> std::pair<ast::Expression const *, VerifyResult> {
            return std::pair{arg.get(), arg->VerifyType(this, ctx)};
          });
      auto *lit = block_def->parent_;
      ast::VerifyDispatch(ast::ExprPtr{lit, 0x01}, block_def->before_, args,
                          ctx);
    }
  }
  return VerifyResult::Constant(type::Void());
}

VerifyResult VerifyType::operator()(ast::JumpHandler const *node,
                                    Context *ctx) {
  DEBUG_LOG("JumpHandler")(DumpAst::ToString(node));
  bool err = false;
  std::vector<type::Type const *> arg_types;
  arg_types.reserve(node->input().size());
  for (auto const &input : node->input()) {
    auto v = input->VerifyType(this, ctx);
    if (!v.ok()) {
      err = true;
    } else {
      arg_types.push_back(v.type_);
    }
  }

  if (err) {
    return ctx->set_result(node, VerifyResult::Error());
  } else {
    return ctx->set_result(node, VerifyResult::Constant(type::Jmp(arg_types)));
  }
}

static std::optional<
    std::vector<std::pair<ast::Expression const *, VerifyResult>>>
VerifySpan(visitor::VerifyType *v, ast::NodeSpan<ast::Expression const> exprs,
           Context *ctx) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, VerifyResult>> results;
  bool err = false;
  for (auto *expr : exprs) {
    results.emplace_back(expr, expr->VerifyType(v, ctx));
    err |= !results.back().second.ok();
  }
  if (err) { return std::nullopt; }
  return results;
}

enum class Constness { Error, Const, NonConst };
static Constness VerifyAndGetConstness(
    visitor::VerifyType *v, ast::NodeSpan<ast::Expression const> exprs,
    Context *ctx) {
  bool err      = false;
  bool is_const = true;
  for (auto *expr : exprs) {
    auto r = expr->VerifyType(v, ctx);
    err |= !r.ok();
    if (!err) { is_const &= r.const_; }
  }
  if (err) { return Constness::Error; }
  return is_const ? Constness::Const : Constness::NonConst;
}

VerifyResult VerifyType::operator()(ast::PrintStmt const *node, Context *ctx) {
  auto verify_results = VerifySpan(this, node->exprs(), ctx);
  if (!verify_results) { return VerifyResult::Error(); }
  for (auto &verify_result : *verify_results) {
    auto *expr_type = verify_result.second.type_;
    // TODO print arrays?
    if (expr_type->is<type::Primitive>() || expr_type->is<type::Pointer>() ||
        expr_type == type::ByteView || expr_type->is<type::Enum>() ||
        expr_type->is<type::Flags>()) {
      continue;
    } else {
      ast::OverloadSet os(node->scope_, "print", ctx);
      os.add_adl("print", expr_type);
      // TODO using expr.get() for the dispatch table is super janky. node is
      // used so we don't collide with the table for the actual expression as
      // `print f(x)` needs a table both for the printing and for the call to
      // `f`. Test node thoroughly.
      auto dispatch_result = ast::VerifyDispatch(
          ast::ExprPtr{verify_result.first, 0x01}, os,
          core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
              {verify_result}, {}),
          ctx);
      if (dispatch_result.type_ && dispatch_result.type_ != type::Void()) {
        ctx->error_log()->PrintMustReturnVoid(
            dispatch_result.type_->to_string(), node->span);
        return VerifyResult::Error();
      }
    }
  }
  return VerifyResult::NonConstant(type::Void());
}

VerifyResult VerifyType::operator()(ast::ReturnStmt const *node, Context *ctx) {
  auto c = VerifyAndGetConstness(this, node->exprs(), ctx);
  if (c == Constness::Error) { return VerifyResult::Error(); }
  return c == Constness::Const ? VerifyResult::Constant(type::Void())
                               : VerifyResult::NonConstant(type::Void());
}

VerifyResult VerifyType::operator()(ast::YieldStmt const *node, Context *ctx) {
  auto c = VerifyAndGetConstness(this, node->exprs(), ctx);
  if (c == Constness::Error) { return VerifyResult::Error(); }
  return c == Constness::Const ? VerifyResult::Constant(type::Void())
                               : VerifyResult::NonConstant(type::Void());
}

VerifyResult VerifyType::operator()(ast::ScopeLiteral const *node,
                                    Context *ctx) {
  auto verify_result =
      ctx->set_result(node, VerifyResult::Constant(type::Scope));
  bool error = false;
  for (auto const *decl : node->decls()) {
    auto result = decl->VerifyType(this, ctx);
    if (!result.const_) {
      error = true;
      NOT_YET("log an error");
    }
  }
  if (error) { return VerifyResult::Error(); }
  return verify_result;
}

static std::vector<
    core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>>
VerifyBlockNode(VerifyType *visitor, ast::BlockNode const *node, Context *ctx) {
  node->VerifyType(visitor, ctx);

  ExtractJumps extract_visitor;
  for (auto const *stmt : node->stmts()) {
    stmt->ExtractJumps(&extract_visitor);
  }

  auto yields = extract_visitor.jumps(ExtractJumps::Kind::Yield);
  // TODO this setup is definitely wrong because it doesn't account for
  // multiple yields correctly. For example,
  //
  // ```
  //  result: int32 | bool = if (cond) then {
  //    yield 3
  //  } else if (other_cond) then {
  //    yield 4
  //  } else {
  //    yield true
  //  }
  //  ```
  std::vector<core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>>
      result;
  for (auto *yield : yields) {
    auto &back = result.emplace_back();
    // TODO actually fill a fnargs
    std::vector<std::pair<ast::Expression const *, VerifyResult>>
        local_pos_yields;
    for (auto *yield_expr : yields[0]->as<ast::YieldStmt>().exprs()) {
      back.pos_emplace(
          yield_expr,
          *ASSERT_NOT_NULL(ctx->prior_verification_attempt(yield_expr)));
    }
  }
  return result;
}

VerifyResult VerifyType::operator()(ast::ScopeNode const *node, Context *ctx) {
  // TODO how do you determine the type of this?
  ASSIGN_OR(return _, auto name_result, node->name()->VerifyType(this, ctx));
  static_cast<void>(name_result);

  auto arg_results =
      node->args().Transform([ctx, this](ast::Expression const *arg) {
        return std::pair{arg, arg->VerifyType(this, ctx)};
      });

  // TODO later on you'll want to allow dynamic dispatch. Calling a scope on an
  // argument of type `A | B` where there are two scope objects (one of type `A`
  // and one of type `B`) should work. But this necessitates evaluating not as a
  // ScopeDef but as an overload set.
  //
  // Then on each possibility, you should first check that all the block names
  // are available on all possible scopes.
  //
  // TODO you may not be able to compute this ahead of time. It relies on
  // computing all the block handlers which may be generics.
  //
  // TODO is the scope type correct here?
  auto *scope_def = backend::EvaluateAs<ir::ScopeDef *>(
      type::Typed<ast::Expression const *>{node->name(), type::Scope}, ctx);
  if (scope_def->work_item && *scope_def->work_item) {
    (std::move(*scope_def->work_item))();
  }

  bool err = false;
  std::vector<ir::BlockDef const *> block_defs;
  for (auto const &block : node->blocks()) {
    DEBUG_LOG("ScopeNode")("Verifying dispatch for block ", block.name());
    auto block_results    = VerifyBlockNode(this, &block, ctx);
    auto const &block_def = scope_def->blocks_.at(block.name());
    DEBUG_LOG("ScopeNode")("    ", block_results);
    if (block_results.empty()) {
      DEBUG_LOG("ScopeNode")("    ... empty block results");
      auto result =
          ast::VerifyJumpDispatch(node, block_def.after_, {}, ctx, &block_defs);
      DEBUG_LOG("ScopeNode")("    ... dispatch result = ", result);
    } else {
      for (auto const &fn_args : block_results) {
        auto result = ast::VerifyDispatch(node, block_def.after_, fn_args, ctx);
        DEBUG_LOG("ScopeNode")("    ... dispatch result = ", result);
      }
    }
    DEBUG_LOG("ScopeNode")("    ... done.");
  }
  auto init_result = ast::VerifyJumpDispatch(node, scope_def->inits_,
                                             arg_results, ctx, &block_defs);
  DEBUG_LOG("ScopeNode")("    ... init_result = ", init_result);
  DEBUG_LOG("ScopeNode")("    ... block_defs = ", block_defs);
  return VerifyResult::Constant(type::Void());
}

VerifyResult VerifyType::operator()(ast::StructLiteral const *node,
                                    Context *ctx) {
  std::vector<type::Type const *> ts;
  ts.reserve(node->args_.size());
  for (auto &a : node->args_) { ts.push_back(a.VerifyType(this, ctx).type_); }
  if (absl::c_any_of(ts, [](type::Type const *t) { return t == nullptr; })) {
    return VerifyResult::Error();
  }

  if (node->args_.empty()) {
    bool is_const = true;
    bool err      = false;
    for (auto const &field : node->fields_) {
      if (!field.type_expr()) { continue; }
      auto result = field.type_expr()->VerifyType(this, ctx);
      if (!result) {
        err = true;
        continue;
      }
      is_const &= result.const_;
      if (field.init_val()) {
        if (result.const_) {
          auto init_val_result = field.init_val()->VerifyType(this, ctx);
          if (!init_val_result.const_) {
            ctx->error_log()->NonConstantStructFieldDefaultValue(
                field.init_val()->span);
            err = true;
          }

          if (init_val_result.type_ != result.type_) {
            NOT_YET("type mismatch");
          }

        } else {
          NOT_YET(
              "can't have an initial value set if the type is also present and "
              "non-constant");
        }
      }
    }
    if (err) { return ctx->set_result(node, VerifyResult::Error()); }
    return ctx->set_result(node, VerifyResult::Constant(type::Type_));
    // TODO, we need to verify the body of this struct at some point, but it may
    // be dependent on things we can't evaluate yet. For example,
    //
    // wrapper ::= (T: type) => struct { val: T }
    //
    // I'm yet unsure exactly how we should handle this.
    /*
    bool ok = absl::c_all_of(node->fields_, [this, ctx](
                                                ast::Declaration const &field) {
      // TODO you should verify each field no matter what.
      field.VerifyType(this, ctx);
      if (!field.init_val() ||
          ASSERT_NOT_NULL(ctx->prior_verification_attempt(field.init_val()))
              ->const_) {
        return true;
      }
      ctx->error_log()->NonConstantStructFieldDefaultValue(
          field.init_val()->span);
      return false;
    });
    // TODO so in fact we could recover here and just not emit ir but we're no
    // longer set up to do that.
    if (ok) {
      return ctx->set_result(node, VerifyResult::Constant(type::Type_));
    } else {
      return VerifyResult::Error();
    }
    */
  } else {
    return ctx->set_result(node, VerifyResult::Constant(type::GenStruct(
                                     node->scope_, std::move(ts))));
  }
}

VerifyResult VerifyType::operator()(ast::StructType const *node, Context *ctx) {
  for (auto &arg : node->args_) { arg->VerifyType(this, ctx); }
  return ctx->set_result(node, VerifyResult::Constant(type::Type_));
}

VerifyResult VerifyType::operator()(ast::Switch const *node, Context *ctx) {
  bool is_const               = true;
  type::Type const *expr_type = nullptr;
  if (node->expr_) {
    ASSIGN_OR(return _, auto result, node->expr_->VerifyType(this, ctx));
    is_const &= result.const_;
    expr_type = result.type_;
  }

  absl::flat_hash_set<type::Type const *> types;
  bool err = false;
  for (auto &[body, cond] : node->cases_) {
    auto cond_result = cond->VerifyType(this, ctx);
    auto body_result = body->VerifyType(this, ctx);
    err |= !cond_result || !body_result;
    if (err) {
      DEBUG_LOG()(DumpAst::ToString(body.get()));
      NOT_YET();
      continue;
    }

    is_const &= cond_result.const_ && body_result.const_;
    if (node->expr_) {
      static_cast<void>(expr_type);
      // TODO dispatch table
    } else {
      if (cond_result.type_ != type::Bool) {
        ctx->error_log()->SwitchConditionNeedsBool(
            cond_result.type_->to_string(), node->span);
      }
    }
    // TODO if there's an error, an unorderded_set is not helpful for giving
    // good error messages.
    if (body->is<ast::Expression>()) {
      // TODO check that it's actually a jump
      types.insert(body_result.type_);
    }
  }
  if (err) { return VerifyResult::Error(); }

  // TODO check to ensure that the type is either exhaustable or has a default.

  if (types.empty()) {
    return ctx->set_result(node, VerifyResult(type::Void(), is_const));
  }
  auto some_type = *types.begin();
  if (absl::c_all_of(types,
                     [&](type::Type const *t) { return t == some_type; })) {
    // TODO node might be a constant.
    return ctx->set_result(node, VerifyResult(some_type, is_const));
  } else {
    for (auto &t : types) { DEBUG_LOG()(!t ? "<>" : t->to_string()); }
    NOT_YET("handle type error");
    return VerifyResult::Error();
  }
}

VerifyResult VerifyType::operator()(ast::Terminal const *node, Context *ctx) {
  return ctx->set_result(node, VerifyResult::Constant(node->type()));
}

VerifyResult VerifyType::operator()(ast::Unop const *node, Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto result,
                   node->operand->VerifyType(this, ctx));
  auto *operand_type = result.type_;

  switch (node->op) {
    case frontend::Operator::Copy:
      if (!operand_type->IsCopyable()) {
        NOT_YET("log an error. not copyable");
      }
      // TODO Are copies always consts?
      return ctx->set_result(node, VerifyResult(operand_type, result.const_));
    case frontend::Operator::Move:
      if (!operand_type->IsMovable()) { NOT_YET("log an error. not movable"); }
      // TODO Are copies always consts?
      return ctx->set_result(node, VerifyResult(operand_type, result.const_));
    case frontend::Operator::BufPtr:
      return ctx->set_result(node, VerifyResult(type::Type_, result.const_));
    case frontend::Operator::TypeOf:
      return ctx->set_result(node, VerifyResult(type::Type_, result.const_));
    case frontend::Operator::Eval:
      if (!result.const_) {
        // TODO here you could return a correct type and just have there
        // be an error regarding constness. When you do node probably worth a
        // full pass over all verification code.
        ctx->error_log()->NonConstantEvaluation(node->operand->span);
        return VerifyResult::Error();
      } else {
        return ctx->set_result(node, VerifyResult(operand_type, result.const_));
      }
    case frontend::Operator::Which:
      if (!operand_type->is<type::Variant>()) {
        ctx->error_log()->WhichNonVariant(operand_type->to_string(),
                                          node->span);
      }
      return ctx->set_result(node, VerifyResult(type::Type_, result.const_));
    case frontend::Operator::At:
      if (operand_type->is<type::Pointer>()) {
        return ctx->set_result(
            node, VerifyResult(operand_type->as<type::Pointer>().pointee,
                               result.const_));
      } else {
        ctx->error_log()->DereferencingNonPointer(operand_type->to_string(),
                                                  node->span);
        return VerifyResult::Error();
      }
    case frontend::Operator::And:
      // TODO  does it make sense to take the address of a constant? I think it
      // has to but it also has to have some special meaning. Things we take the
      // address of in run-time code need to be made available at run-time.
      return ctx->set_result(
          node, VerifyResult(type::Ptr(operand_type), result.const_));
    case frontend::Operator::Mul:
      if (operand_type != type::Type_) {
        NOT_YET("log an error, ", operand_type, node);
        return VerifyResult::Error();
      } else {
        return ctx->set_result(node, VerifyResult(type::Type_, result.const_));
      }
    case frontend::Operator::Sub:
      if (type::IsNumeric(operand_type)) {
        return ctx->set_result(node, VerifyResult(operand_type, result.const_));
      } else if (operand_type->is<type::Struct>()) {
        ast::OverloadSet os(node->scope_, "-", ctx);
        os.add_adl("-", operand_type);
        return ast::VerifyDispatch(
            node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair(node->operand.get(), result)}, {}),
            ctx);
      }
      NOT_YET();
      return VerifyResult::Error();
    case frontend::Operator::Expand:
      // NOTE: It doesn't really make sense to ask for the type of an expanded
      // argument, but since we consider the type of the result of a function
      // call returning multiple arguments to be a tuple, we do the same here.
      //
      if (operand_type->is<type::Tuple>()) {
        // TODO there should be a way to avoid copying over any of entire type
        return ctx->set_result(node, VerifyResult(operand_type, result.const_));
      } else {
        NOT_YET();  // Log an error. can't expand a non-tuple.
      }
    case frontend::Operator::Not:
      if (operand_type == type::Bool || operand_type->is<type::Enum>() ||
          operand_type->is<type::Flags>()) {
        return ctx->set_result(node, VerifyResult(operand_type, result.const_));
      }
      if (operand_type->is<type::Struct>()) {
        ast::OverloadSet os(node->scope_, "!", ctx);
        os.add_adl("!", operand_type);
        return ast::VerifyDispatch(
            node, os,
            core::FnArgs<std::pair<ast::Expression const *, VerifyResult>>(
                {std::pair(node->operand.get(), result)}, {}),
            ctx);
      } else {
        NOT_YET("log an error");
        return VerifyResult::Error();
      }
    case frontend::Operator::Needs:
      if (operand_type != type::Bool) {
        ctx->error_log()->PreconditionNeedsBool(node->operand->span,
                                                operand_type->to_string());
      }
      if (!result.const_) { NOT_YET(); }
      return ctx->set_result(node, VerifyResult::Constant(type::Void()));
    case frontend::Operator::Ensure:
      if (operand_type != type::Bool) {
        ctx->error_log()->PostconditionNeedsBool(node->operand->span,
                                                 operand_type->to_string());
      }
      if (!result.const_) { NOT_YET(); }
      return ctx->set_result(node, VerifyResult::Constant(type::Void()));
    case frontend::Operator::VariadicPack:  {
      if (!result.const_) { NOT_YET("Log an error"); }
      // TODO could be a type, or a function returning a ty
      if (result.type_ == type::Type_) {
        return ctx->set_result(node, VerifyResult::Constant(type::Type_));
      } else if (result.type_->is<type::Function>()) {
        NOT_YET();
      }
      NOT_YET(*node);
    }
    default: UNREACHABLE(*node);
  }
}

}  // namespace visitor
