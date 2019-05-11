#include "ast/repeated_unop.h"

#include "ast/block_literal.h"
#include "ast/call.h"
#include "ast/function_literal.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "core/fn_args.h"
#include "core/scope.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"

namespace ast {
using ::matcher::InheritsFrom;

std::string RepeatedUnop::to_string(size_t n) const {
  switch (op_) {
    case frontend::Operator::Jump: return "jump " + args_.to_string(n);
    case frontend::Operator::Return: return "return " + args_.to_string(n);
    case frontend::Operator::Yield: return "yield " + args_.to_string(n);
    case frontend::Operator::Print: return "print " + args_.to_string(n);
    default: { UNREACHABLE(); }
  }
}

RepeatedUnop::RepeatedUnop(TextSpan const &text_span) {
  span = args_.span = text_span;
}

ir::Results RepeatedUnop::EmitIr(Context *ctx) {
  if (op_ == frontend::Operator::Jump) {
    ASSERT(args_.exprs_.size() == 1u);
    ASSERT(args_.exprs_[0].get(), InheritsFrom<Call>());
    auto &call = args_.exprs_[0]->as<Call>();
    auto *called_expr = call.fn_.get();

    // TODO stop calculating this so many times.
    auto block_seq = backend::EvaluateAs<ir::BlockSequence>(
        type::Typed<Expression const *>(call.fn_.get(), type::Block), ctx);
    auto block = block_seq.at(0);
    if (block == ir::Block::Start()) {
      // Do nothing. You'll just end up jumping to this location and the body
      // will be emit elsewhere.
    } else if (block == ir::Block::Exit()) {
    } else {
      ASSERT_NOT_NULL(ctx->dispatch_table(ExprPtr{&call, 0x01}))
          ->EmitInlineCall({}, {}, ctx);
    }
    ir::JumpPlaceholder(backend::EvaluateAs<ir::BlockSequence>(
        type::Typed<Expression const *>(called_expr, type::Block), ctx));
    return ir::Results{};
  }

  std::vector<ir::Results> arg_vals;
  if (args_.needs_expansion()) {
    for (auto &expr : args_.exprs_) {
      auto vals = expr->EmitIr(ctx);
      for (size_t i = 0; i < vals.size(); ++i) {
        arg_vals.push_back(vals.GetResult(i));
      }
    }
  } else {
    auto vals = args_.EmitIr(ctx);
    for (size_t i = 0; i < vals.size(); ++i) {
      arg_vals.push_back(vals.GetResult(i));
    }
  }

  switch (op_) {
    case frontend::Operator::Return: {
      size_t offset  = 0;
      auto *fn_scope = ASSERT_NOT_NULL(scope_->Containing<core::FnScope>());
      auto *fn_lit   = ASSERT_NOT_NULL(fn_scope->fn_lit_);

      auto *fn_type =
          &ASSERT_NOT_NULL(ctx->type_of(fn_lit))->as<type::Function>();
      for (size_t i = 0; i < arg_vals.size(); ++i) {
        // TODO return type maybe not the same as type actually returned?
        ir::SetRet(i, type::Typed{arg_vals[i], fn_type->output.at(i)}, ctx);
      }

      // Rather than doing this on each block it'd be better to have each
      // scope's destructors jump you to the correct next block for destruction.
      auto *scope = scope_;
      while (auto *exec = scope->if_as<core::ExecScope>()) {
        exec->MakeAllDestructions(ctx);
        scope = exec->parent;
      }

      ctx->more_stmts_allowed_ = false;
      ir::ReturnJump();
      return ir::Results{};
    }
    case frontend::Operator::Yield: {
      // TODO store this as an exec_scope.
      scope_->as<core::ExecScope>().MakeAllDestructions(ctx);
      // TODO pretty sure this is all wrong.

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
        ctx->yields_stack_.back().emplace_back(args_.exprs_[i].get(),
                                               arg_vals[i]);
      }
      ctx->more_stmts_allowed_ = false;
      return ir::Results{};
    }
    case frontend::Operator::Print: {
      size_t index                = 0;
      // TODO this is wrong if you use the <<(...) spread operator.
      for (auto &val : arg_vals) {
        if (auto const *dispatch_table =
                ctx->dispatch_table(ExprPtr{args_.exprs_[index].get(), 0x01})) {
          dispatch_table->EmitCall(
              core::FnArgs<std::pair<Expression const *, ir::Results>>(
                  {std::pair(args_.exprs_[index].get(), std::move(val))}, {}),
              ctx);
        } else {
          auto *t = ctx->type_of(args_.exprs_.at(index).get());
          t->EmitRepr(val, ctx);
        }
        ++index;
      }
      return ir::Results{};
    } break;
    default: UNREACHABLE("Operator is ", static_cast<int>(op_));
  }
}
}  // namespace ast
