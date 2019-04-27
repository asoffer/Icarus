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

void RepeatedUnop::assign_scope(core::Scope *scope) {
  scope_ = scope;
  args_.assign_scope(scope);
}

void RepeatedUnop::DependentDecls(DeclDepGraph *g,
                                  Declaration *d) const {
  args_.DependentDecls(g, d);
}

void RepeatedUnop::ExtractJumps(JumpExprs *rets) const {
  args_.ExtractJumps(rets);
  // TODO yield as well?
  switch (op_) {
    case frontend::Operator::Jump:
      (*rets)[JumpExprs::Kind::Jump].push_back(&args_);
      break;
    case frontend::Operator::Return:
      (*rets)[JumpExprs::Kind::Return].push_back(&args_);
      break;
    case frontend::Operator::Yield:
      (*rets)[JumpExprs::Kind::Yield].push_back(&args_);
      break;
    default: break;
  }
}

VerifyResult RepeatedUnop::VerifyType(Context *ctx) {
  ASSIGN_OR(return _, auto result, args_.VerifyType(ctx));

  std::vector<type::Type const *> arg_types =
      result.type_->is<type::Tuple>()
          ? result.type_->as<type::Tuple>().entries_
          : std::vector<type::Type const *>{result.type_};

  if (op_ == frontend::Operator::Print) {
    // TODO what's the actual size given expansion of tuples and stuff?
    for (size_t i = 0; i < args_.exprs_.size(); ++i) {
      auto &arg      = args_.exprs_[i];
      auto *arg_type = arg_types[i];
      if (arg_type->is<type::Primitive>() || arg_type->is<type::Pointer>() ||
          arg_type == type::ByteView || arg_type->is<type::Enum>() ||
          arg_type->is<type::Flags>() || arg_type->is<type::Array>()) {
        continue;
      } else {
        OverloadSet os(scope_, "print", ctx);
        os.add_adl("print", arg_type);

        // TODO I need finer-grained const-ness here: Currently all members are
        // const or all are non-const.
        //
        // TODO using arg.get() for the dispatch table is super janky. This is
        // used so we don't collide with the table for the actual expression as
        // `print f(x)` needs a table both for the printing and for the call to
        // `f`. Test this thoroughly.
        auto dispatch_result = VerifyDispatch(
            ExprPtr{arg.get(), true}, os,
            core::FnArgs<std::pair<Expression *, VerifyResult>>(
                {std::pair(arg.get(), VerifyResult(arg_type, result.const_))},
                {}),
            ctx);
        if (dispatch_result.type_ && dispatch_result.type_ != type::Void()) {
          NOT_YET("log an error. must return void: ",
                  dispatch_result.type_->to_string());
        }
      }
    }
  } else if (op_ == frontend::Operator::Jump) {
    ASSERT(args_.exprs_[0].get(), InheritsFrom<Call>());
    // Note: We're not verifying the type of the call but instead the callable
    // and its args.
    auto &call = args_.exprs_[0]->as<Call>();
    call.fn_->VerifyType(ctx);

    auto block = backend::EvaluateAs<ir::Block>(
        type::Typed<Expression *>(call.fn_.get(), type::Blk()), ctx);
    if (block != ir::Block::Start() && block != ir::Block::Exit()) {
      auto args =
          call.args_.Transform([ctx](std::unique_ptr<Expression> const &arg)
                                   -> std::pair<Expression *, VerifyResult> {
            auto *arg_ptr =
                const_cast<std::unique_ptr<Expression> &>(arg).get();
            return std::pair{arg_ptr, arg_ptr->VerifyType(ctx)};
          });
      OverloadSet before_os(block.get()->body_scope_.get(), "before", ctx);
      VerifyDispatch(block.get(), before_os, args, ctx);

      OverloadSet after_os(block.get()->body_scope_.get(), "after", ctx);
      VerifyDispatch(ExprPtr{block.get(), true}, after_os, args, ctx);
    }
  }

  return VerifyResult(type::Void(), result.const_);
}

ir::Results RepeatedUnop::EmitIr(Context *ctx) {
  if (op_ == frontend::Operator::Jump) {
    ASSERT(args_.exprs_.size() == 1u);
    ASSERT(args_.exprs_[0].get(), InheritsFrom<Call>());
    auto *called_expr = args_.exprs_[0]->as<Call>().fn_.get();
    ir::JumpPlaceholder(backend::EvaluateAs<ir::Block>(
        type::Typed<Expression *>(args_.exprs_[0]->as<Call>().fn_.get(),
                                  type::Blk()),
        ctx));
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
      auto *fn_scope = ASSERT_NOT_NULL(scope_->ContainingFnScope());
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
                ctx->dispatch_table(ExprPtr{args_.exprs_[index].get(), true})) {
          dispatch_table->EmitCall(
              core::FnArgs<std::pair<Expression *, ir::Results>>(
                  {std::pair(args_.exprs_[index].get(), std::move(val))}, {}),
              type::Void(), ctx);
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
