#include "ast/call.h"

#include <sstream>

#include "ast/fn_params.h"
#include "ast/function_literal.h"
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

using base::check::Is;

i32 ForeignFuncIndex = 0;
i32 OpaqueFuncIndex = 1;

ir::Val DebugIrFunc() {
  auto *fn_type                   = type::Func({}, {});
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
  for (const auto & [ key, val ] : args_.named_) {
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

VerifyResult Call::VerifyType(Context *ctx) {
  FnArgs<VerifyResult> arg_results;
  for (auto const &expr : args_.pos_) {
    auto expr_result = expr->VerifyType(ctx);
    if (!expr->parenthesized_ && expr->is<Unop>() &&
        expr->as<Unop>().op == Language::Operator::Expand &&
        expr_result.type_->is<type::Tuple>()) {
      auto const &entries = expr_result.type_->as<type::Tuple>().entries_;
      for (type::Type const *entry : entries) {
        arg_results.pos_.emplace_back(entry, expr_result.const_);
      }
    } else {
      arg_results.pos_.push_back(expr_result);
    }
  }

  for (auto const& [name, expr] : args_.named_) {
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
      ASSERT(arg_results.pos_[0].const_);
      ASSERT(arg_results.pos_[1].type_ == type::Type_);
      ASSERT(arg_results.pos_[1].const_);
      return VerifyResult(
          ctx->set_type(this, backend::EvaluateAs<type::Type const *>(
                                  args_.pos_[1].get(), ctx)),
          arg_results.pos_[0].const_ && arg_results.pos_[1].const_);
    } else if (fn_val == ir::Val::BuiltinGeneric(OpaqueFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.empty());
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
    if (fn_->is<Identifier>()) {
      auto &token = fn_->as<Identifier>().token;
      OverloadSet os(scope_, token, ctx);
      arg_results.Apply(
          [&](VerifyResult const &v) { os.add_adl(token, v.type_); });
      return os;
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

void Call::Validate(Context *ctx) {
  fn_->Validate(ctx);
  args_.Apply([ctx](auto &arg) { arg->Validate(ctx); });
}

void Call::ExtractJumps(JumpExprs *rets) const {
  fn_->ExtractJumps(rets);
  for (const auto &val : args_.pos_) { val->ExtractJumps(rets); }
  for (const auto & [ key, val ] : args_.named_) { val->ExtractJumps(rets); }
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
      ASSERT(!out_type->is_big());

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

std::vector<ir::RegisterOr<ir::Addr>> Call::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace ast
