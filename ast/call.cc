#include "ast/call.h"

#include <sstream>

#include "ast/fn_params.h"
#include "ast/function_literal.h"
#include "ast/struct_literal.h"
#include "ast/terminal.h"
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

int32_t ForeignFuncIndex = 0;
int32_t OpaqueFuncIndex  = 1;

ir::Val DebugIrFunc() {
  auto *fn_type = type::Func({}, {});
  static ir::Func *debug_ir_func_ =
      new ir::Func(nullptr, fn_type, ast::FnParams<ast::Expression *>{});
  return ir::Val::Func(fn_type, debug_ir_func_);
}

ir::Val BytesFunc() {
  auto *fn_type                = type::Func({type::Type_}, {type::Int64});
  static ir::Func *bytes_func_ = [&]() {
    ast::FnParams<ast::Expression *> params;
    params.append("", nullptr);
    auto fn = new ir::Func(nullptr, fn_type, std::move(params));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      ir::SetRet(0, Bytes(fn->Argument(0)));
      ir::ReturnJump();
    }
    return fn;
  }();
  return ir::Val::Func(fn_type, bytes_func_);
}

ir::Val AlignFunc() {
  auto *fn_type                = type::Func({type::Type_}, {type::Int64});
  static ir::Func *bytes_func_ = [&]() {
    ast::FnParams<ast::Expression *> params;
    params.append("", nullptr);
    auto fn = new ir::Func(nullptr, fn_type, std::move(params));
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      ir::SetRet(0, Align(fn->Argument(0)));
      ir::ReturnJump();
    }
    return fn;
  }();
  return ir::Val::Func(fn_type, bytes_func_);
}

namespace ast {
std::string Call::to_string(size_t n) const {
  std::stringstream ss;
  ss << fn_->to_string(n) << "(";
  bool seen_one = false;
  for (const auto &pos : args_.pos_) {
    ss << (seen_one ? ", " : "") << pos->to_string(n);
    seen_one = true;
  }
  for (const auto &[key, val] : args_.named_) {
    ss << (seen_one ? ", " : "") << key << " = " << val->to_string(n) << ", ";
    seen_one = true;
  }
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
    auto os = FindOverloads(scope_, id->token, {}, state->ctx_);
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
      ASSERT(backward_iter->second->size() == args_.pos_.size());
      for (size_t i = 0; i < args_.pos_.size(); ++i) {
        state->match_queue_.emplace(args_.pos_[i].get(),
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
  FnArgs<VerifyResult> arg_results;
  for (auto const &expr : args_.pos_) {
    auto expr_result = expr->VerifyType(ctx);
    if (!expr->parenthesized_ && expr->is<Unop>() &&
        expr->as<Unop>().op == frontend::Operator::Expand &&
        expr_result.type_->is<type::Tuple>()) {
      auto const &entries = expr_result.type_->as<type::Tuple>().entries_;
      for (type::Type const *entry : entries) {
        arg_results.pos_.emplace_back(entry, expr_result.const_);
      }
    } else {
      arg_results.pos_.push_back(expr_result);
    }
  }

  for (auto const &[name, expr] : args_.named_) {
    arg_results.named_.emplace(name, expr->VerifyType(ctx));
  }

  // TODO handle cyclic dependencies in call arguments.

  if (std::any_of(arg_results.pos_.begin(), arg_results.pos_.end(),
                  [](VerifyResult const &v) { return !v.ok(); }) ||
      std::any_of(arg_results.named_.begin(), arg_results.named_.end(),
                  [](std::pair<std::string, VerifyResult> const &p) {
                    return !p.second.ok();
                  })) {
    return VerifyResult::Error();
  }

  if (fn_->is<Terminal>()) {
    // Special case for error, etc.
    // TODO can these be overloaded?
    auto fn_val = fn_->as<Terminal>().value;
    if (fn_val == BytesFunc() || fn_val == AlignFunc()) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 1u);
      ASSERT(arg_results.pos_[0].type_ == type::Type_);
      return VerifyResult(ctx->set_type(this, type::Int64),
                          arg_results.pos_[0].const_);
#ifdef DBG
    } else if (fn_val == DebugIrFunc()) {
      return VerifyResult::Constant(type::Func({}, {}));
#endif  // DBG
    } else if (fn_val == ir::Val::BuiltinGeneric(ForeignFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 2u);
      // TODO should turn this into some sort of interface requiremnet of being
      // string-like
      ASSERT(arg_results.pos_[0].type_ == type::ByteView);
      ASSERT(arg_results.pos_[0].const_ == true);
      ASSERT(arg_results.pos_[1].type_ == type::Type_);
      ASSERT(arg_results.pos_[1].const_ == true);
      return VerifyResult(
          ctx->set_type(this, backend::EvaluateAs<type::Type const *>(
                                  args_.pos_[1].get(), ctx)),
          arg_results.pos_[0].const_ && arg_results.pos_[1].const_);
    } else if (fn_val == ir::Val::BuiltinGeneric(OpaqueFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 0u);
      return VerifyResult::Constant(ctx->set_type(this, type::Type_));
    } else if (std::holds_alternative<ir::BlockSequence>(fn_val.value)) {
      // TODO might be optional.
      // TODO what about set_type?
      return VerifyResult::Constant(type::Block);
    } else {
      UNREACHABLE();
    }
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
  for (const auto &val : args_.pos_) { val->ExtractJumps(rets); }
  for (const auto &[key, val] : args_.named_) { val->ExtractJumps(rets); }
}

std::vector<ir::Val> Call::EmitIR(Context *ctx) {
  if (fn_->is<Terminal>()) {
    auto fn_val = fn_->as<Terminal>().value;
#ifdef DBG
    if (fn_val == DebugIrFunc()) {
      ir::DebugIr();
      return {};
    }
#endif  // DBG
    if (fn_val == BytesFunc() || fn_val == AlignFunc()) {
      ir::Arguments call_args;
      for (const auto &arg : args_.pos_[0]->EmitIR(ctx)) {
        call_args.append(arg);
      }
      call_args.type_ = &fn_val.type->as<type::Function>();

      auto *out_type = fn_val.type->as<type::Function>().output.at(0);
      ASSERT(out_type->is_big() == false);

      ir::OutParams outs;
      auto reg = outs.AppendReg(out_type);
      ir::Call(std::get<ir::AnyFunc>(fn_val.value), std::move(call_args),
               std::move(outs));

      return {ir::Val::Reg(reg, out_type)};

    } else if (fn_val == ir::Val::BuiltinGeneric(ForeignFuncIndex)) {
      // Note: verified as constants in VerifyType.
      auto name =
          backend::EvaluateAs<std::string_view>(args_.pos_[0].get(), ctx);
      // TODO can I evaluate as type::Function const *?
      auto *foreign_type =
          backend::EvaluateAs<type::Type const *>(args_.pos_[1].get(), ctx);
      return {ir::Val(ir::LoadSymbol(name, foreign_type))};
    } else if (fn_val == ir::Val::BuiltinGeneric(OpaqueFuncIndex)) {
      return {ir::Val(ir::NewOpaqueType(ctx->mod_))};
    } else if (std::holds_alternative<ir::BlockSequence>(fn_val.value)) {
      // TODO might be optional.
      return {fn_val};
    }

    UNREACHABLE();
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
                         expr->EmitIR(ctx));
      }),
      ASSERT_NOT_NULL(ctx->type_of(this)), ctx);
}

}  // namespace ast
