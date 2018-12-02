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
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

using base::check::Is;

namespace type {
extern Type const *Code, *Int, *Char;
}  // namespace type

i32 ResizeFuncIndex  = 0;
i32 ForeignFuncIndex = 1;

ir::Val AsciiFunc() {
  auto *fn_type                = type::Func({type::Int8}, {type::Char});
  static ir::Func *ascii_func_ = [&]() {
    auto fn = new ir::Func(nullptr, fn_type, {{"", nullptr}});
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      ir::SetRet(0, Trunc(fn->Argument(0)));
      ir::ReturnJump();
    }
    return fn;
  }();
  return ir::Val::Func(fn_type, ascii_func_);
}

ir::Val DebugIrFunc() {
  auto *fn_type                   = type::Func({}, {});
  static ir::Func *debug_ir_func_ = new ir::Func(nullptr, fn_type, {});
  return ir::Val::Func(fn_type, debug_ir_func_);
}

ir::Val OrdFunc() {
  auto *fn_type              = type::Func({type::Char}, {type::Int8});
  static ir::Func *ord_func_ = [&]() {
    auto fn = new ir::Func(nullptr, fn_type, {{"", nullptr}});
    CURRENT_FUNC(fn) {
      ir::BasicBlock::Current = fn->entry();
      ir::SetRet(0, Extend(fn->Argument(0)));
      ir::ReturnJump();
    }
    return fn;
  }();
  return ir::Val::Func(fn_type, ord_func_);
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

static ir::RegisterOr<bool> EmitVariantMatch(ir::Register needle,
                                             const type::Type *haystack) {
  auto runtime_type = ir::Load<type::Type const *>(ir::VariantType(needle));

  if (haystack->is<type::Variant>()) {
    // TODO I'm fairly confident this will work, but it's also overkill because
    // we may already know this type matches if one variant is a subset of the
    // other.
    auto landing = ir::Func::Current->AddBlock();

    base::unordered_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_map;
    for (type::Type const *v : haystack->as<type::Variant>().variants_) {
      phi_map.emplace(ir::BasicBlock::Current, true);

      ir::BasicBlock::Current =
          ir::EarlyExitOn<true>(landing, ir::Eq(v, runtime_type));
    }

    phi_map.emplace(ir::BasicBlock::Current, false);

    ir::UncondJump(landing);

    ir::BasicBlock::Current = landing;
    return ir::MakePhi<bool>(ir::Phi(type::Bool), phi_map);

  } else {
    // TODO actually just implicitly convertible to haystack
    return ir::Eq(haystack, runtime_type);
  }
}

static ir::BlockIndex CallLookupTest(
    const ast::FnArgs<std::pair<ast::Expression *, ir::Val>> &args,
    const ast::FnArgs<const type::Type *> &call_arg_type, Context *ctx) {
  // Generate code that attempts to match the types on each argument (only
  // check the ones at the call-site that could be variants).
  auto next_binding = ir::Func::Current->AddBlock();
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!ctx->type_of(args.pos_[i].first)->is<type::Variant>()) { continue; }
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(std::get<ir::Register>(args.pos_.at(i).second.value),
                         call_arg_type.pos_[i]));
  }

  for (const auto & [ name, expr_and_val ] : args.named_) {
    auto iter = call_arg_type.find(name);
    if (iter == call_arg_type.named_.end()) { continue; }
    if (!ctx->type_of(expr_and_val.first)->is<type::Variant>()) { continue; }
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(
            std::get<ir::Register>(args.named_.at(iter->first).second.value),
            iter->second));
  }

  return next_binding;
}

// We allow overwriting outgoing_regs slots. This will only happen with locally
// declared registers which means they're all simple and this works as a nice
// return value.
static void EmitOneCallDispatch(
    const type::Type *ret_type, base::vector<ir::Val> *outgoing_regs,
    const base::unordered_map<ast::Expression *, const ir::Val *> &expr_map,
    const ast::Binding &binding, Context *ctx) {
  auto callee = [&] {
    Context fn_ctx(ctx->mod_);  // TODO this might be the wrong module.
    fn_ctx.bound_constants_ = binding.bound_constants_;
    return binding.fn_.get()->EmitIR(&fn_ctx)[0];
  }();

  if (!binding.const_) {
    if (!binding.fn_.get()->is<ast::Declaration>() ||
        !binding.fn_.get()->as<ast::Declaration>().is_arg_) {
      if (auto *reg = std::get_if<ir::Register>(&callee.value)) {
        callee = ir::Val::Reg(ir::Load<ir::AnyFunc>(*reg, binding.fn_.type()),
                              binding.fn_.type());
      }
    }
  }
  ASSERT(callee.type, Is<type::Function>());

  // After the last check, if you pass, you should dispatch
  base::vector<std::pair<std::string, ast::Expression *>> *const_args = nullptr;
  if (auto *fn_to_call = std::get_if<ir::AnyFunc>(&callee.value)) {
    if (fn_to_call->is_fn()) { const_args = &(fn_to_call->func()->args_); }
  }

  base::vector<ir::Val> args;
  args.resize(binding.exprs_.size());
  for (size_t i = 0; i < args.size(); ++i) {
    auto typed_expr = binding.exprs_[i];
    if (typed_expr.get() == nullptr) {
      auto default_expr = (*ASSERT_NOT_NULL(const_args))[i].second;
      args[i] = ASSERT_NOT_NULL(typed_expr.type())
                    ->PrepareArgument(ctx->type_of(default_expr),
                                      default_expr->EmitIR(ctx)[0], ctx);
    } else {
      args[i] = ASSERT_NOT_NULL(typed_expr.type())
                    ->PrepareArgument(ctx->type_of(typed_expr.get()),
                                      *expr_map.at(typed_expr.get()), ctx);
    }
  }

  ir::Arguments call_args;
  call_args.type_ = &callee.type->as<type::Function>();
  for (const auto &arg : args) { call_args.append(arg); }

  base::vector<ir::Val> results;
  ir::OutParams outs;
  ASSERT(binding.fn_.type(), Is<type::Function>());
  if (!binding.fn_.type()->as<type::Function>().output.empty()) {
    auto MakeRegister = [&](type::Type const *return_type,
                            type::Type const *expected_return_type,
                            ir::Val *out_reg) {
      // Cases:
      // 1. I return a small value, and am expected to return the same
      //    reg return
      //
      // 2. I return a big value and am expected to return the same
      //    pass in a return
      // 3. I return a variant and am expected to return a variant
      //    pass in a return
      //
      // 4. I return a small value but am expected to return a variant
      //    pass in a return and fix
      // 5. I return a big value but am expected to return a variant
      //    pass in a return and fix
      //
      // TODO: This is a lot like PrepareArgument.
      if (!return_type->is_big() && !expected_return_type->is_big()) {
        *out_reg = ir::Val::Reg(outs.AppendReg(expected_return_type),
                                expected_return_type);
        return;
      }

      if (return_type == expected_return_type ||
          return_type->is<type::Variant>()) {
        outs.AppendLoc(std::get<ir::Register>(out_reg->value));
        return;
      }

      ASSERT(expected_return_type, Is<type::Variant>());
      ir::Store(return_type,
                    ir::VariantType(std::get<ir::Register>(out_reg->value)));
      outs.AppendLoc(ir::VariantValue(return_type,
                                      std::get<ir::Register>(out_reg->value)));
    };

    if (ret_type->is<type::Tuple>()) {
      ASSERT(binding.fn_.type(), Is<type::Function>());
      ASSERT(ret_type->as<type::Tuple>().entries_.size() ==
             binding.fn_.type()->as<type::Function>().output.size());
      for (size_t i = 0;
           i < binding.fn_.type()->as<type::Function>().output.size(); ++i) {
        MakeRegister(binding.fn_.type()->as<type::Function>().output.at(i),
                     ret_type->as<type::Tuple>().entries_.at(i),
                     &outgoing_regs->at(i));
      }
    } else {
      MakeRegister(binding.fn_.type()->as<type::Function>().output.at(0),
                   ret_type, &outgoing_regs->at(0));
    }
  }

  ASSERT(std::holds_alternative<ir::Register>(callee.value) ||
         std::holds_alternative<ir::AnyFunc>(callee.value));
  ir::Call(callee.reg_or<ir::AnyFunc>(), std::move(call_args), std::move(outs));
}

base::vector<ir::Val> EmitCallDispatch(
    const ast::FnArgs<std::pair<ast::Expression *, ir::Val>> &args,
    const ast::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx) {
  ASSERT(dispatch_table.bindings_.size() != 0u);
  base::unordered_map<ast::Expression *, const ir::Val *> expr_map;
  args.Apply([&expr_map](const std::pair<ast::Expression *, ir::Val> &arg) {
    expr_map[arg.first] = &arg.second;
  });

  base::vector<ir::Val> out_regs;
  if (ret_type->is<type::Tuple>()) {
    out_regs.reserve(ret_type->as<type::Tuple>().entries_.size());
    for (auto *entry : ret_type->as<type::Tuple>().entries_) {
      out_regs.push_back(entry->is_big()
                             ? ir::Val::Reg(ir::Alloca(entry), type::Ptr(entry))
                             : ir::Val::None());
    }
  } else {
    out_regs.push_back(ret_type->is_big() ? ir::Val::Reg(ir::Alloca(ret_type),
                                                         type::Ptr(ret_type))
                                          : ir::Val::None());
  }

  if (dispatch_table.bindings_.size() == 1) {
    const auto & [ call_arg_type, binding ] = *dispatch_table.bindings_.begin();
    EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
    return out_regs;
  }

  // TODO push void out of here.
  size_t num_rets = ret_type->is<type::Tuple>()
                        ? ret_type->as<type::Tuple>().entries_.size()
                        : 1;

  base::vector<base::unordered_map<ir::BlockIndex, ir::Val>> result_phi_args(
      num_rets);

  auto landing_block = ir::Func::Current->AddBlock();

  auto iter = dispatch_table.bindings_.begin();
  ASSERT(iter != dispatch_table.bindings_.end());
  for (size_t i = 0; i < dispatch_table.bindings_.size() - 1; ++i, ++iter) {
    const auto & [ call_arg_type, binding ] = *iter;
    auto next_binding = CallLookupTest(args, call_arg_type, ctx);
    size_t j          = 0;

    EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
    for (const auto &result : out_regs) {
      result_phi_args.at(j)[ir::BasicBlock::Current] = result;
      ++j;
    }
    ASSERT(j == num_rets);

    ir::UncondJump(landing_block);
    ir::BasicBlock::Current = next_binding;
  }
  
  const auto & [ call_arg_type, binding ] = *iter;
  size_t j                                = 0;
  EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
  for (const auto &result : out_regs) {
    result_phi_args.at(j)[ir::BasicBlock::Current] = result;
    ++j;
  }
  ASSERT(j == num_rets);

  ir::UncondJump(landing_block);
  ir::BasicBlock::Current = landing_block;

  switch (num_rets) {
    case 0: return {};
    case 1:
      if (ret_type == type::Void()) {
        return {ir::Val::None()};
      } else {
        return {
            ir::MakePhi(ir::Phi(ret_type->is_big() ? Ptr(ret_type) : ret_type),
                        result_phi_args[0])};
      }
      break;
    default: {
      base::vector<ir::Val> results;
      results.reserve(num_rets);
      const auto &tup_entries = ret_type->as<type::Tuple>().entries_;
      for (size_t i = 0; i < num_rets; ++i) {
        const type::Type *single_ret_type = tup_entries[i];
        if (single_ret_type == type::Void()) {
          results.push_back(ir::Val::None());
        } else {
          results.push_back(ir::MakePhi(
              ir::Phi(single_ret_type->is_big() ? Ptr(single_ret_type)
                                                : single_ret_type),
              result_phi_args[i]));
        }
      }
      return results;
    } break;
  }
  UNREACHABLE();
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
    // Special case for error/ord/ascii/etc.
    // TODO can these be overloaded?
    auto fn_val = fn_->as<Terminal>().value;
    if (fn_val == OrdFunc()) {
      return fn_val.type;
    } else if (fn_val == AsciiFunc()) {
      return fn_val.type;
#ifdef DBG
    } else if (fn_val == DebugIrFunc()) {
      return type::Func({}, {});
#endif  // DBG
    } else if (fn_val == BytesFunc() || fn_val == AlignFunc()) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 1u);
      ASSERT(arg_types.pos_[0] == type::Type_);
      return ctx->set_type(this, type::Int64);
    } else if (fn_val == ir::Val::BuiltinGeneric(ResizeFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 2u);
      ASSERT(arg_types.pos_[0], Is<type::Pointer>());
      ASSERT(arg_types.pos_[0]->as<type::Pointer>().pointee, Is<type::Array>());
      ASSERT(arg_types.pos_[1] == type::Int64);
      return ctx->set_type(this, type::Void());
    } else if (fn_val == ir::Val::BuiltinGeneric(ForeignFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 2u);
      ASSERT(arg_types.pos_[0], Is<type::CharBuffer>());
      ASSERT(arg_types.pos_[1] == type::Type_);
      auto *t =
          backend::EvaluateAs<const type::Type *>(args_.pos_[1].get(), ctx);
      return ctx->set_type(this, t);
      return t;
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
    // Special case for error/ord/ascii
    auto fn_val = fn_->as<Terminal>().value;
#ifdef DBG
    if (fn_val == DebugIrFunc()) {
      ir::DebugIr();
      return {};
    }
#endif  // DBG
    if (fn_val == OrdFunc() || fn_val == AsciiFunc() || fn_val == BytesFunc() ||
        fn_val == AlignFunc()) {
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

    } else if (fn_val == ir::Val::BuiltinGeneric(ResizeFuncIndex)) {
      ctx->type_of(args_.pos_[0].get())
          ->as<type::Pointer>()
          .pointee->as<type::Array>()
          .EmitResize(args_.pos_[0]->EmitIR(ctx)[0],
                      args_.pos_[1]->EmitIR(ctx)[0], ctx);

      return {};
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

  return EmitCallDispatch(
      args_.Transform([ctx](const std::unique_ptr<Expression> &expr) {
        return std::pair(const_cast<Expression *>(expr.get()),
                         expr->EmitIR(ctx)[0]);
      }),
      dispatch_table_, ASSERT_NOT_NULL(ctx->type_of(this)), ctx);
}

base::vector<ir::Register> Call::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace ast
