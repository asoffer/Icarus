#include "ast/call.h"

#include <sstream>

#include "ast/function_literal.h"
#include "ast/terminal.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
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

IR::Val ErrorFunc() {
  // TODO implement me
  return IR::Val(123.456);
}

IR::Val AsciiFunc() {
  static IR::Func *ascii_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::Int}, {type::Char}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      IR::SetReturnInt(0, Trunc(fn->Argument(0)).reg_);
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

IR::Val DebugIrFunc() {
  static IR::Func *debug_ir_func_ =
      new IR::Func(nullptr, type::Func({}, {}), {});
  return IR::Val::Func(debug_ir_func_);
}

IR::Val OrdFunc() {
  static IR::Func *ord_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::Char}, {type::Int}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      IR::SetReturnInt(0, Extend(fn->Argument(0)).reg_);
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(ord_func_);
}

IR::Val BytesFunc() {
  static IR::Func *bytes_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::Type_}, {type::Int}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      IR::SetReturnInt(0, Bytes(fn->Argument(0)));
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(bytes_func_);
}

IR::Val AlignFunc() {
  static IR::Func *bytes_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::Type_}, {type::Int}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      IR::SetReturnInt(0, Align(fn->Argument(0)));
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(bytes_func_);
}

static IR::RegisterOr<bool> EmitVariantMatch(IR::Register needle,
                                             const type::Type *haystack) {
  auto runtime_type = IR::LoadType(IR::VariantType(needle));

  if (haystack->is<type::Variant>()) {
    // TODO I'm fairly confident this will work, but it's also overkill because
    // we may already know this type matches if one variant is a subset of the
    // other.
    auto landing = IR::Func::Current->AddBlock();

    base::unordered_map<IR::BlockIndex, IR::RegisterOr<bool>> phi_map;
    for (const type::Type *v : haystack->as<type::Variant>().variants_) {
      phi_map.emplace(IR::BasicBlock::Current, true);

      IR::BasicBlock::Current =
          IR::EarlyExitOn<true>(landing, IR::EqType(v, runtime_type));
    }

    phi_map.emplace(IR::BasicBlock::Current, false);

    IR::UncondJump(landing);

    IR::BasicBlock::Current = landing;
    return IR::MakePhi<bool>(IR::Phi(type::Bool), phi_map);

  } else {
    // TODO actually just implicitly convertible to haystack
    return IR::EqType(haystack, runtime_type);
  }
}

static IR::BlockIndex CallLookupTest(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::FnArgs<const type::Type *> &call_arg_type, Context *ctx) {
  // Generate code that attempts to match the types on each argument (only
  // check the ones at the call-site that could be variants).
  auto next_binding = IR::Func::Current->AddBlock();
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!ctx->mod_->types_.at(args.pos_[i].first)->is<type::Variant>()) {
      continue;
    }
    IR::BasicBlock::Current = IR::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(std::get<IR::Register>(args.pos_.at(i).second.value),
                         call_arg_type.pos_[i]));
  }

  for (const auto & [ name, expr_and_val ] : args.named_) {
    auto iter = call_arg_type.find(name);
    if (iter == call_arg_type.named_.end()) { continue; }
    if (!ctx->mod_->types_.at(expr_and_val.first)->is<type::Variant>()) {
      continue;
    }
    IR::BasicBlock::Current = IR::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(
            std::get<IR::Register>(args.named_.at(iter->first).second.value),
            iter->second));
  }

  return next_binding;
}

// We allow overwriting outgoing_regs slots. This will only happen with locally
// declared registers which means they're all simple and this works as a nice
// return value.
static void EmitOneCallDispatch(
    const type::Type *ret_type, base::vector<IR::Val> *outgoing_regs,
    const base::unordered_map<AST::Expression *, const IR::Val *> &expr_map,
    const AST::Binding &binding, Context *ctx) {
  auto callee = binding.fn_expr_->EmitIR(ctx)[0];
  ASSERT(callee.type, Is<type::Function>());

  // After the last check, if you pass, you should dispatch
  base::vector<std::pair<std::string, AST::Expression *>> *const_args = nullptr;
  if (auto **fn_to_call = std::get_if<IR::Func *>(&callee.value)) {
    const_args = &((**fn_to_call).args_);
  }

  base::vector<IR::Val> args;
  args.resize(binding.exprs_.size());
  for (size_t i = 0; i < args.size(); ++i) {
    auto[bound_type, expr] = binding.exprs_[i];
    if (expr == nullptr) {
      ASSERT(bound_type != nullptr);
      auto default_expr = (*ASSERT_NOT_NULL(const_args))[i].second;
      args[i] = bound_type->PrepareArgument(ctx->mod_->types_.at(default_expr),
                                            default_expr->EmitIR(ctx)[0], ctx);
    } else {
      args[i] = bound_type->PrepareArgument(ctx->mod_->types_.at(expr),
                                            *expr_map.at(expr), ctx);
    }
  }

  IR::LongArgs call_args;
  call_args.type_ = &callee.type->as<type::Function>();
  for (const auto &arg : args) { call_args.append(arg); }

  base::vector<IR::Val> results;
  IR::OutParams outs;
  if (!binding.fn_type_->output.empty()) {
    auto MakeRegister = [&](type::Type const *return_type,
                            type::Type const *expected_return_type,
                            IR::Val *out_reg) {
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
        *out_reg = IR::Val::Reg(outs.AppendReg(expected_return_type),
                                expected_return_type);
        return;
      }

      if (return_type == expected_return_type ||
          return_type->is<type::Variant>()) {
        outs.AppendLoc(std::get<IR::Register>(out_reg->value));
        return;
      }

      ASSERT(expected_return_type, Is<type::Variant>());
      IR::StoreType(return_type,
                    IR::VariantType(std::get<IR::Register>(out_reg->value)));
      outs.AppendLoc(IR::VariantValue(return_type,
                                      std::get<IR::Register>(out_reg->value)));
    };

    if (ret_type->is<type::Tuple>()) {
      ASSERT(ret_type->as<type::Tuple>().entries_.size() ==
             binding.fn_type_->output.size());
      for (size_t i = 0; i < binding.fn_type_->output.size(); ++i) {
        MakeRegister(binding.fn_type_->output.at(i),
                     ret_type->as<type::Tuple>().entries_.at(i),
                     &outgoing_regs->at(i));
      }
    } else {
      MakeRegister(binding.fn_type_->output.at(0), ret_type,
                   &outgoing_regs->at(0));
    }
  }

  std::visit(
      [&](auto &val) {
        using val_t = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<val_t, IR::Func *> ||
                      std::is_same_v<val_t, IR::ForeignFn>) {
          IR::Call(IR::AnyFunc{val}, std::move(call_args), std::move(outs));
        } else if constexpr (std::is_same_v<val_t, IR::Register>) {
          IR::Call(val, std::move(call_args), std::move(outs));
        } else {
          UNREACHABLE(val);
        }
      },
      callee.value);
}

base::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx) {
  ASSERT(dispatch_table.bindings_.size() != 0u);
  base::unordered_map<AST::Expression *, const IR::Val *> expr_map;
  args.Apply([&expr_map](const std::pair<AST::Expression *, IR::Val> &arg) {
    expr_map[arg.first] = &arg.second;
  });

  base::vector<IR::Val> out_regs;
  if (ret_type->is<type::Tuple>()) {
    out_regs.reserve(ret_type->as<type::Tuple>().entries_.size());
    for (auto *entry : ret_type->as<type::Tuple>().entries_) {
      out_regs.push_back(entry->is_big()
                             ? IR::Val::Reg(IR::Alloca(entry), type::Ptr(entry))
                             : IR::Val::None());
    }
  } else {
    out_regs.push_back(ret_type->is_big() ? IR::Val::Reg(IR::Alloca(ret_type),
                                                         type::Ptr(ret_type))
                                          : IR::Val::None());
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

  base::vector<base::unordered_map<IR::BlockIndex, IR::Val>> result_phi_args(num_rets);

  auto landing_block = IR::Func::Current->AddBlock();

  auto iter = dispatch_table.bindings_.begin();
  ASSERT(iter != dispatch_table.bindings_.end());
  for (size_t i = 0; i < dispatch_table.bindings_.size() - 1; ++i, ++iter) {
    const auto & [ call_arg_type, binding ] = *iter;
    auto next_binding = CallLookupTest(args, call_arg_type, ctx);
    size_t j          = 0;

    EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
    for (const auto &result : out_regs) {
      result_phi_args.at(j)[IR::BasicBlock::Current] = result;
      ++j;
    }
    ASSERT(j == num_rets);

    IR::UncondJump(landing_block);
    IR::BasicBlock::Current = next_binding;
  }

  const auto & [ call_arg_type, binding ] = *iter;
  size_t j                                = 0;
  EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
  for (const auto &result : out_regs) {
    result_phi_args.at(j)[IR::BasicBlock::Current] = result;
    ++j;
  }
  ASSERT(j == num_rets);

  IR::UncondJump(landing_block);
  IR::BasicBlock::Current = landing_block;

  switch (num_rets) {
    case 0: return {};
    case 1:
      if (ret_type == type::Void()) {
        return {IR::Val::None()};
      } else {
        return {
            IR::MakePhi(IR::Phi(ret_type->is_big() ? Ptr(ret_type) : ret_type),
                        result_phi_args[0])};
      }
      break;
    default: {
      base::vector<IR::Val> results;
      results.reserve(num_rets);
      const auto &tup_entries = ret_type->as<type::Tuple>().entries_;
      for (size_t i = 0; i < num_rets; ++i) {
        const type::Type *single_ret_type = tup_entries[i];
        if (single_ret_type == type::Void()) {
          results.push_back(IR::Val::None());
        } else {
          results.push_back(IR::MakePhi(
              IR::Phi(single_ret_type->is_big() ? Ptr(single_ret_type)
                                                : single_ret_type),
              result_phi_args[i]));
        }
      }
      return results;
    } break;
  }
  UNREACHABLE();
}

namespace AST {
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
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  fn_->assign_scope(scope);
  args_.Apply([scope](auto &expr) { expr->assign_scope(scope); });
}

type::Type const *Call::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  auto arg_types =
      args_.Transform([ctx, this](auto &arg) { return arg->VerifyType(ctx); });
  // TODO handle cyclic dependencies in call arguments.

  if (std::any_of(arg_types.pos_.begin(), arg_types.pos_.end(),
                  +[](type::Type const *t) { return t == nullptr; }) ||
      std::any_of(arg_types.named_.begin(), arg_types.named_.end(),
                  [](std::pair<std::string, type::Type const *> const &p) {
                    return p.second == nullptr;
                  })) {
    limit_to(StageRange::Nothing());
    return nullptr;
  }

  if (fn_->is<Terminal>()) {
    // Special case for error/ord/ascii/etc.
    // TODO can these be overloaded?
    auto fn_val = fn_->as<Terminal>().value;
    if (fn_val == OrdFunc()) {
      NOT_YET();
    } else if (fn_val == AsciiFunc()) {
      NOT_YET();
#ifdef DBG
    } else if (fn_val == DebugIrFunc()) {
      return type::Func({}, {});
#endif  // DBG
    } else if (fn_val == ErrorFunc()) {
      NOT_YET();
    } else if (fn_val == BytesFunc() || fn_val == AlignFunc()) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 1u);
      ASSERT(arg_types.pos_[0] == type::Type_);
      ctx->mod_->types_.buffered_emplace(this, type::Int);
      return type::Int;
    } else if (fn_val == IR::Val::BuiltinGeneric(ResizeFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 2u);
      ASSERT(arg_types.pos_[0], Is<type::Pointer>());
      ASSERT(arg_types.pos_[0]->as<type::Pointer>().pointee, Is<type::Array>());
      ASSERT(arg_types.pos_[1] == type::Int);
      ctx->mod_->types_.buffered_emplace(this, type::Void());
      return type::Void();
    } else if (fn_val == IR::Val::BuiltinGeneric(ForeignFuncIndex)) {
      // TODO turn assert into actual checks with error logging. Or maybe allow
      // named args here?
      ASSERT(args_.named_.size() == 0u);
      ASSERT(args_.pos_.size() == 2u);
      ASSERT(arg_types.pos_[0], Is<type::CharBuffer>());
      ASSERT(arg_types.pos_[1] == type::Type_);
      auto *t =
          backend::EvaluateAs<const type::Type *>(args_.pos_[1].get(), ctx);
      ctx->mod_->types_.buffered_emplace(this, t);
      ASSERT(t, Is<type::Function>());
      return t;
    } else {
      UNREACHABLE();
    }
  } else {
    fn_->VerifyType(ctx);
  }

  FnArgs<Expression *> args =
      args_.Transform([](const std::unique_ptr<Expression> &arg) {
        return const_cast<Expression *>(arg.get());
      });

  type::Type const *ret_type = nullptr;
  std::tie(dispatch_table_, ret_type) =
      !fn_->is<Identifier>()
          ? DispatchTable::Make(args, fn_.get(), ctx)
          : DispatchTable::Make(args, fn_->as<Identifier>().token, scope_, ctx);
  ctx->mod_->types_.buffered_emplace(this, ret_type);

  if (ret_type == nullptr) { limit_to(StageRange::Nothing()); }

  u64 expanded_size = 1;
  arg_types.Apply([&expanded_size](type::Type const *arg_type) {
    if (arg_type->is<type::Variant>()) {
      expanded_size *= arg_type->as<type::Variant>().size();
    }
  });

  if (dispatch_table_.total_size_ != expanded_size) {
    // TODO give a better error message here.
    ctx->error_log_.NoCallMatch(span);
    limit_to(StageRange::Nothing());
    return nullptr;
  }

  return ret_type;
}

void Call::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  fn_->Validate(ctx);
  args_.Apply([ctx](auto &arg) { arg->Validate(ctx); });
}

void Call::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &pos : args_.pos_) { pos->SaveReferences(scope, args); }
  for (auto & [ name, expr ] : args_.named_) {
    expr->SaveReferences(scope, args);
  }
}

void Call::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  fn_->contextualize(correspondant->as<Call>().fn_.get(), replacements);

  for (size_t i = 0; i < args_.pos_.size(); ++i) {
    args_.pos_[i]->contextualize(correspondant->as<Call>().args_.pos_[i].get(),
                                 replacements);
  }
  for (auto && [ name, expr ] : args_.named_) {
    expr->contextualize(
        correspondant->as<Call>().args_.named_.find(name)->second.get(),
        replacements);
  }
}

void Call::ExtractReturns(base::vector<const Expression *> *rets) const {
  fn_->ExtractReturns(rets);
  for (const auto &val : args_.pos_) { val->ExtractReturns(rets); }
  for (const auto & [ key, val ] : args_.named_) { val->ExtractReturns(rets); }
}

Call *Call::Clone() const {
  auto *result = new Call;
  result->span = span;
  result->fn_  = base::wrap_unique(fn_->Clone());
  result->args_.pos_.reserve(args_.pos_.size());
  for (const auto &val : args_.pos_) {
    result->args_.pos_.emplace_back(val->Clone());
  }
  for (const auto & [ key, val ] : args_.named_) {
    result->args_.named_.emplace(key, base::wrap_unique(val->Clone()));
  }

  result->dispatch_table_ = dispatch_table_;
  return result;
}

base::vector<IR::Val> Call::EmitIR(Context *ctx) {
  if (fn_->is<Terminal>()) {
    // Special case for error/ord/ascii
    auto fn_val = fn_->as<Terminal>().value;
#ifdef DBG
    if (fn_val == DebugIrFunc()) {
      IR::DebugIr();
      return {};
    }
#endif  // DBG
    if (fn_val == OrdFunc() || fn_val == AsciiFunc() || fn_val == ErrorFunc() ||
        fn_val == BytesFunc() || fn_val == AlignFunc()) {
      IR::LongArgs call_args;
      for (const auto &arg : args_.pos_[0]->EmitIR(ctx)) {
        call_args.append(arg);
      }
      call_args.type_ = &fn_val.type->as<type::Function>();

      auto *out_type = fn_val.type->as<type::Function>().output.at(0);
      ASSERT(!out_type->is_big());

      IR::OutParams outs;
      auto reg = outs.AppendReg(out_type);
      IR::Call(IR::AnyFunc{std::get<IR::Func *>(fn_val.value)},
               std::move(call_args), std::move(outs));

      return {IR::Val::Reg(reg, out_type)};

    } else if (fn_val == IR::Val::BuiltinGeneric(ResizeFuncIndex)) {
      ctx->mod_->types_.at(args_.pos_[0].get())
          ->as<type::Pointer>()
          .pointee->as<type::Array>()
          .EmitResize(args_.pos_[0]->EmitIR(ctx)[0],
                      args_.pos_[1]->EmitIR(ctx)[0], ctx);

      return {};
    } else if (fn_val == IR::Val::BuiltinGeneric(ForeignFuncIndex)) {
      return {IR::Val::Foreign(
          backend::EvaluateAs<const type::Type *>(args_.pos_[1].get(), ctx),
          IR::ForeignFn{
              backend::EvaluateAs<std::string_view>(args_.pos_[0].get(), ctx),
              this})};
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
      dispatch_table_, ctx->mod_->types_.at(this), ctx);
}

base::vector<IR::Register> Call::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
