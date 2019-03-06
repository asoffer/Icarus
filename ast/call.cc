#include "ast/call.h"

#include <sstream>

#include "ast/block_literal.h"
#include "ast/builtin_fn.h"
#include "ast/dispatch/table.h"
#include "ast/fn_params.h"
#include "ast/function_literal.h"
#include "ast/struct_literal.h"
#include "ast/unop.h"
#include "backend/eval.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "misc/scope.h"
#include "type/array.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ast {
std::string Call::to_string(size_t n) const {
  std::stringstream ss;
  ss << fn_->to_string(n) << "(";
  bool seen_one = false;
  args_.ApplyWithIndex(
      [&](auto &&index, std::unique_ptr<Expression> const &expr) {
        ss << (seen_one ? ", " : "");
        if constexpr (!std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
          ss << index << " = ";
        }
        ss << expr->to_string(n);
        seen_one = true;
      });
  ss << ")";
  return ss.str();
}

void Call::assign_scope(Scope *scope) {
  scope_ = scope;
  fn_->assign_scope(scope);
  args_.Apply([scope](auto &expr) { expr->assign_scope(scope); });
}

static OverloadSet FindOverloads(Scope *scope, std::string const &token,
                                 FnArgs<type::Type const *> arg_types,
                                 Context *ctx) {
  OverloadSet os(scope, token, ctx);
  arg_types.Apply([&](type::Type const *t) { os.add_adl(token, t); });
  return os;
}

bool Call::InferType(type::Type const *t, InferenceState *state) const {
  auto *s = t->if_as<type::Struct>();
  if (!s) { return false; }

  if (auto *id = fn_->if_as<Identifier>()) {
    // TODO this is probably the wrong approach. It'd be nice to stuff more data
    // we know into the inference state.
    auto os = FindOverloads(scope_, id->token, FnArgs<type::Type const *>{},
                            state->ctx_);
    if (os.size() != 1) { NOT_YET("only handle no overloading right now."); }
    if (auto *gs = os[0].type()->if_as<type::GenericStruct>()) {
      auto iter = gs->mod_->generic_struct_cache_.find(s->parent_);
      if (iter == gs->mod_->generic_struct_cache_.end()) { return false; }

      auto backward_iter = iter->second.back_.find(s);
      if (backward_iter == iter->second.back_.end()) { 
        // TODO This is impossible? Just use .at if so.
        return false;
      }

      // TODO only if this is the right dispatch?
      // TODO named args too.
      ASSERT(backward_iter->second->size() == args_.num_pos());
      for (size_t i = 0; i < args_.num_pos(); ++i) {
        state->match_queue_.emplace(args_.at(i).get(),
                                    backward_iter->second->at(i));
      }

      return true;
    } else {
      NOT_YET(this);
    }

  } else {
    NOT_YET(this);
  }
  return false;
}

void Call::DependentDecls(base::Graph<Declaration *> *g,
                          Declaration *d) const {
  fn_->DependentDecls(g, d);
  args_.Apply([g, d](auto const &expr) { expr->DependentDecls(g, d); });
}

VerifyResult Call::VerifyType(Context *ctx) {
  std::vector<VerifyResult> pos_results;
  std::unordered_map<std::string, VerifyResult> named_results;

  bool err = false;

  // TODO this could be TransformWithIndex
  args_.ApplyWithIndex(
      [&](auto &&index, std::unique_ptr<Expression> const &expr) {
        if constexpr (std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
          auto expr_result = expr->VerifyType(ctx);
          if (!expr->parenthesized_ && expr->is<Unop>() &&
              expr->as<Unop>().op == frontend::Operator::Expand &&
              expr_result.type_->is<type::Tuple>()) {
            auto const &entries = expr_result.type_->as<type::Tuple>().entries_;
            for (type::Type const *entry : entries) {
              pos_results.emplace_back(entry, expr_result.const_);
              err |= !pos_results.back().ok();
            }
          } else {
            pos_results.push_back(expr_result);
            err |= !pos_results.back().ok();
          }
        } else {
          auto iter = named_results.emplace(index, expr->VerifyType(ctx)).first;
          err |= !iter->second.ok();
        }
      });

  FnArgs<VerifyResult> arg_results(std::move(pos_results),
                                   std::move(named_results));

  // TODO handle cyclic dependencies in call arguments.
  if (err) { return VerifyResult::Error(); }

  if (auto *b = fn_->if_as<BuiltinFn>()) {
    // TODO: Should we allow these to be overloaded?
    ASSIGN_OR(return VerifyResult::Error(), auto result,
                     b->VerifyCall(args_, arg_results, ctx));
    return VerifyResult(ctx->set_type(this, result.type_), result.const_);
  }  

  FnArgs<Expression *> args = args_.Transform(
      [](std::unique_ptr<Expression> const &arg) { return arg.get(); });

  OverloadSet overload_set = [&]() {
    if (auto *id = fn_->if_as<Identifier>()) {
      return FindOverloads(
          scope_, id->token,
          arg_results.Transform([](VerifyResult const &v) { return v.type_; }),
          ctx);
    } else {
      auto t = fn_->VerifyType(ctx).type_;
      OverloadSet os;
      os.emplace_back(fn_.get(), t);
      // TODO ADL for this?
      return os;
    }
  }();

  auto *ret_type = DispatchTable::MakeOrLogError(this, args, overload_set, ctx);
  if (ret_type == nullptr) { return VerifyResult::Error(); }

  // TODO returning const is overly optimistic. This should be const if and only
  // if all arguments are const, all possible dispatched-to functions are const.
  // Default arguments are already required to be const, so we don't need to
  // check those.

  return VerifyResult::Constant(ret_type);
}

void Call::ExtractJumps(JumpExprs *rets) const {
  fn_->ExtractJumps(rets);
  args_.Apply([rets](std::unique_ptr<Expression> const &expr) {
    expr->ExtractJumps(rets);
  });
}

ir::Results Call::EmitIr(Context *ctx) {
  if (auto *b = fn_->if_as<BuiltinFn>()) {
    switch (b->b_) {
      case ir::Builtin::Foreign: {
        auto name =
            backend::EvaluateAs<std::string_view>(args_.at(0).get(), ctx);
        auto *foreign_type =
            backend::EvaluateAs<type::Type const *>(args_.at(1).get(), ctx);
        return ir::Results{ir::LoadSymbol(name, foreign_type)};
      } break;

      case ir::Builtin::Opaque:
        return ir::Results{static_cast<ir::Reg>(ir::NewOpaqueType(ctx->mod_))};

      case ir::Builtin::Bytes: {
        auto const &fn_type =
            ir::BuiltinType(ir::Builtin::Bytes)->as<type::Function>();
        ir::Arguments call_args{&fn_type, args_.at(0)->EmitIr(ctx)};

        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        ir::Call(ir::BytesFn(), std::move(call_args), std::move(outs));

        return ir::Results{reg};
      } break;

      case ir::Builtin::Alignment: {
        auto const &fn_type =
            ir::BuiltinType(ir::Builtin::Alignment)->as<type::Function>();
        ir::Arguments call_args{&fn_type, args_.at(0)->EmitIr(ctx)};

        ir::OutParams outs;
        auto reg = outs.AppendReg(fn_type.output.at(0));
        ir::Call(ir::AlignmentFn(), std::move(call_args), std::move(outs));

        return ir::Results{reg};
      } break;

#ifdef DBG
      case ir::Builtin::DebugIr: ir::DebugIr(); return ir::Results{};
#endif  // DBG
    }
    UNREACHABLE();
    //} else if (auto *t = fn->if_as<Terminal>()) {
    //  UNREACHABLE(this);
    //  if (auto *bs = std::get_if<ir::BlockSequence>(&fn_val.value)) {
    //    // TODO might be optional.
    //    return ir::Results{*bs};
    //  }
  }

  auto const &dispatch_table = *ASSERT_NOT_NULL(ctx->dispatch_table(this));
  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching
  // insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.

  return dispatch_table.EmitCall(
      args_.Transform([ctx](std::unique_ptr<Expression> const &expr) {
        return std::pair(const_cast<Expression *>(expr.get()),
                         expr->EmitIr(ctx));
      }),
      ASSERT_NOT_NULL(ctx->type_of(this)), ctx);
}

}  // namespace ast
