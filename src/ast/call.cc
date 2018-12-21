#include "ast/call.h"

#include <sstream>

#include "ast/function_literal.h"
#include "ast/terminal.h"
#include "ast/unop.h"
#include "backend/eval.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "scope.h"
#include "type/array.h"
#include "type/char_buffer.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

using base::check::Is;

i32 ForeignFuncIndex = 0;

ir::Val DebugIrFunc() {
  auto *fn_type                   = type::Func({}, {});
  static ir::Func *debug_ir_func_ = new ir::Func(nullptr, fn_type, {});
  return ir::Val::Func(fn_type, debug_ir_func_);
}

ir::Val BytesFunc() {
  auto *fn_type                = type::Func({type::Type_}, {type::Int64});
  static ir::Func *bytes_func_ = [&]() {
    auto fn = new ir::Func(nullptr, fn_type, {{"", nullptr}});
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
    auto fn = new ir::Func(nullptr, fn_type, {{"", nullptr}});
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

type::Type const *Call::VerifyType(Context *ctx) {
  FnArgs<type::Type const *> arg_types;
  for (auto const &expr : args_.pos_) {
    type::Type const *expr_type = expr->VerifyType(ctx);
    if (!expr->parenthesized_ && expr->is<Unop>() &&
        expr->as<Unop>().op == Language::Operator::Expand &&
        expr_type->is<type::Tuple>()) {
      auto &entries = expr_type->as<type::Tuple>().entries_;
      arg_types.pos_.insert(arg_types.pos_.end(),
                            std::make_move_iterator(entries.begin()),
                            std::make_move_iterator(entries.end()));
    } else {
      arg_types.pos_.push_back(expr_type);
    }
  }

  for (auto const& [name, expr] : args_.named_) {
    arg_types.named_.emplace(name, expr->VerifyType(ctx));
  }

  // TODO handle cyclic dependencies in call arguments.

  if (std::any_of(arg_types.pos_.begin(), arg_types.pos_.end(),
                  [](type::Type const *t) { return t == nullptr; }) ||
      std::any_of(arg_types.named_.begin(), arg_types.named_.end(),
                  [](std::pair<std::string, type::Type const *> const &p) {
                    return p.second == nullptr;
                  })) {
    return nullptr;
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
      ASSERT(arg_types.pos_[0] == type::Type_);
      return ctx->set_type(this, type::Int64);
#ifdef DBG
    } else if (fn_val == DebugIrFunc()) {
      return type::Func({}, {});
#endif  // DBG
    } else if (fn_val == ir::Val::BuiltinGeneric(ForeignFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 2u);
      ASSERT(arg_types.pos_[0], Is<type::CharBuffer>());
      ASSERT(arg_types.pos_[1] == type::Type_);
      auto *t =
          backend::EvaluateAs<type::Type const *>(args_.pos_[1].get(), ctx);
      return ctx->set_type(this, t);
    } else if (std::holds_alternative<ir::BlockSequence>(fn_val.value)) {
      // TODO might be optional.
      return type::Block;
    } else {
      UNREACHABLE();
    }
  }

  FnArgs<Expression *> args =
      args_.Transform([](std::unique_ptr<Expression> const &arg) {
        return const_cast<Expression *>(arg.get());
      });

  type::Type const *ret_type = nullptr;

  OverloadSet overload_set = [&]() {
    if (fn_->is<Identifier>()) {
      return OverloadSet(scope_, fn_->as<Identifier>().token, ctx);
    } else {
      auto t = fn_->VerifyType(ctx);
      OverloadSet os;
      os.emplace_back(fn_.get(), t);
      return os;
    }
  }();

  std::tie(dispatch_table_, ret_type) =
      DispatchTable::Make(args, overload_set, ctx);

  u64 expanded_size = 1;
  arg_types.Apply([&expanded_size](type::Type const *arg_type) {
    if (arg_type->is<type::Variant>()) {
      expanded_size *= arg_type->as<type::Variant>().size();
    }
  });

  if (dispatch_table_.total_size_ != expanded_size) {
    ctx->error_log_.NoCallMatch(span, dispatch_table_.failure_reasons_);
    return nullptr;
  }

  return ctx->set_type(this, ret_type);
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

base::vector<ir::Val> Call::EmitIR(Context *ctx) {
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
      auto name =
          backend::EvaluateAs<std::string_view>(args_.pos_[0].get(), ctx);
      // TODO can I evaluate as type::Function const *?
      auto *fn_type =
          backend::EvaluateAs<type::Type const *>(args_.pos_[1].get(), ctx);
      return {ir::Val::Func(
          fn_type, ir::ForeignFn{name, this, &fn_type->as<type::Function>()})};
    } else if (std::holds_alternative<ir::BlockSequence>(fn_val.value)) {
      // TODO might be optional.
      return {fn_val};
    }

    UNREACHABLE();
  }

  ASSERT(dispatch_table_.bindings_.size() > 0u);
  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching
  // insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.

  return dispatch_table_.EmitCall(
      args_.Transform([ctx](const std::unique_ptr<Expression> &expr) {
        return std::pair(const_cast<Expression *>(expr.get()),
                         expr->EmitIR(ctx)[0]);
      }),
      ASSERT_NOT_NULL(ctx->type_of(this)), ctx);
}

base::vector<ir::RegisterOr<ir::Addr>> Call::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace ast
