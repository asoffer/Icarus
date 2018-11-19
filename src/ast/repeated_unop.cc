#include "ast/repeated_unop.h"

#include "ast/fn_args.h"
#include "ast/function_literal.h"
#include "ast/overload_set.h"
#include "ast/verify_macros.h"
#include "context.h"
#include "ir/func.h"
#include "scope.h"
#include "type/all.h"

base::vector<ir::Val> EmitCallDispatch(
    const ast::FnArgs<std::pair<ast::Expression *, ir::Val>> &args,
    const ast::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

void ForEachExpr(ast::Expression *expr,
                 const std::function<void(size_t, ast::Expression *)> &fn);

namespace ast {
std::string RepeatedUnop::to_string(size_t n) const {
  switch (op_) {
    case Language::Operator::Return: return "return " + args_.to_string(n);
    case Language::Operator::Yield: return "yield " + args_.to_string(n);
    case Language::Operator::Print: return "print " + args_.to_string(n);
    default: { UNREACHABLE(); }
  }
}

RepeatedUnop::RepeatedUnop(TextSpan const &text_span) {
  span = args_.span = text_span;
}

void RepeatedUnop::assign_scope(Scope *scope) {
  scope_ = scope;
  args_.assign_scope(scope);
}

void RepeatedUnop::Validate(Context *ctx) { args_.Validate(ctx); }

void RepeatedUnop::ExtractJumps(JumpExprs *rets) const {
  args_.ExtractJumps(rets);
  // TODO yield as well?
  switch (op_) {
    case Language::Operator::Return:
      (*rets)[JumpKind::Return].push_back(&args_);
      break;
    case Language::Operator::Yield:
      (*rets)[JumpKind::Yield].push_back(&args_);
      break;
    default: break;
  }
}

type::Type const *RepeatedUnop::VerifyType(Context *ctx) {
  // TODO don't make an assertion. propogate nulls (errors) out.
  auto *t = ASSERT_NOT_NULL(args_.VerifyType(ctx));
  std::vector<type::Type const *> arg_types =
      t->is<type::Tuple>() ? t->as<type::Tuple>().entries_
                           : base::vector<type::Type const *>{t};

  if (op_ == Language::Operator::Print) {
    ASSERT(dispatch_tables_.size() == args_.exprs.size());
    for (size_t i = 0; i < args_.exprs.size(); ++i) {
      auto &arg      = args_.exprs[i];
      auto *arg_type = arg_types[i];
      if (arg_type->is<type::Primitive>() || arg_type->is<type::Pointer>() ||
          arg_type->is<type::CharBuffer>() || arg_type->is<type::Enum>() ||
          arg_type->is<type::Flags>()) {
        continue;
      } else if (arg_type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_.push_back(arg.get());
        const type::Type *ret_type = nullptr;
        std::tie(dispatch_tables_[i], ret_type) =
            DispatchTable::Make(args, OverloadSet(scope_, "print", ctx), ctx);
        if (ret_type != type::Void()) {
          NOT_YET("log an error: ", ret_type);
        }
      } else if (arg_type->is<type::Variant>()) {
        // TODO
      } else {
        NOT_YET(arg_type);
      }
    }
  }
  return nullptr;
}

base::vector<ir::Val> RepeatedUnop::EmitIR(Context *ctx) {
  auto arg_vals = args_.EmitIR(ctx);
  switch (op_) {
    case Language::Operator::Return: {
      size_t offset  = 0;
      auto *fn_scope = ASSERT_NOT_NULL(scope_->ContainingFnScope());
      auto *fn_lit   = ASSERT_NOT_NULL(fn_scope->fn_lit);

      auto *fn_type =
          &ASSERT_NOT_NULL(ctx->type_of(fn_lit))->as<type::Function>();
      for (size_t i = 0; i < args_.exprs.size(); ++i) {
        // TODO return type maybe not the same as type actually returned?
        ir::SetRet(i, arg_vals[i]);
      }
      ir::ReturnJump();
      return {};
    }
    case Language::Operator::Yield: {
      // Can't return these because we need to pass them up at least through the
      // containing statements node and maybe further if we allow labelling
      // scopes to be yielded to.
      ctx->yields_stack_.back().clear();
      ctx->yields_stack_.back().reserve(arg_vals.size());
      // TODO one problem with this setup is that we look things up in a context
      // after returning, so the `after` method has access to a different
      // (smaller) collection of bound constants. This can change the meaning of
      // things or at least make them not compile if the `after` function takes
      // a compile-time constant argument.
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        ctx->yields_stack_.back().emplace_back(args_.exprs[i].get(), arg_vals[i]);
      }
      return {};
    }
    case Language::Operator::Print:
      for (size_t i = 0; i < args_.exprs.size(); ++i) {
        // TODO unify with repr. is repr even a good idea?
        auto *t = ASSERT_NOT_NULL(ctx->type_of(args_.exprs[i].get()));
        if (t == type::Char) {
          ir::Print(arg_vals[i].reg_or<char>());
        } else if (t->is<type::Struct>()) {
          ASSERT(dispatch_tables_[i].total_size_ != 0u);
          // TODO struct is not exactly right. we really mean user-defined
          FnArgs<std::pair<Expression *, ir::Val>> args;
          args.pos_ = {std::pair(args_.exprs[i].get(), arg_vals[i])};
          EmitCallDispatch(args, dispatch_tables_[i], type::Void(), ctx);
        } else {
          t->EmitRepr(arg_vals[i], ctx);
        }
      }
      return {};
    default: UNREACHABLE("Operator is ", static_cast<int>(op_));
  }
}
}  // namespace ast
