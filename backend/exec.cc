#include "backend/exec.h"

#include <dlfcn.h>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <future>
#include <iostream>
#include <memory>
#include <thread>

#include "ast/ast.h"
#include "backend/eval.h"
#include "base/util.h"
#include "core/arch.h"
#include "error/log.h"
#include "ir/arguments.h"
#include "ir/compiled_fn.h"
#include "misc/module.h"
#include "type/incomplete_enum.h"
#include "type/incomplete_flags.h"
#include "type/type.h"
#include "type/util.h"

// TODO CreateStruct, CreateEnum, etc should register a deleter so if we exit
// early we don't leak them. FinalizeStruct, FinalizeEnum, etc should dergeister
// the deleter without calling it.

// TODO compile-time failure. dump the stack trace and abort for Null address
// kinds

namespace ast {
struct Expression;
}  // namespace ast

namespace backend {
base::untyped_buffer ReadOnlyData(0);

void Execute(ir::CompiledFn *fn, const base::untyped_buffer &arguments,
             const std::vector<ir::Addr> &ret_slots, ExecContext *exec_ctx) {
  // TODO: Understand why and how work-items may not be complete and add an
  // explanation here. I'm quite confident this is really possible with the
  // generics model I have, but I can't quite articulate exactly why it only
  // happens for generics and nothing else.
  if (fn->work_item != nullptr) { (*fn->work_item)(); }
  ASSERT(fn->work_item == nullptr);

  // TODO what about bound constants?
  exec_ctx->call_stack.emplace(fn, arguments);

  // TODO log an error if you're asked to execute a function that had an
  // error.

  auto arch     = core::Interpretter();
  auto offset = core::Bytes{0};
  for (auto *t : fn->type_->output) {
    offset = core::FwdAlign(offset, t->alignment(arch)) + t->bytes(arch);
  }
  base::untyped_buffer ret_buffer(offset.value());

  while (true) {
    auto block_index = exec_ctx->ExecuteBlock(ret_slots);
    if (block_index.is_default()) {
      exec_ctx->call_stack.pop();
      return;
    } else {
      exec_ctx->call_stack.top().MoveTo(block_index);
    }
  }
}

template <typename T>
T ExecContext::resolve(ir::Reg r) const {
  return call_stack.top().regs_.get<T>(
      call_stack.top().fn_->compiler_reg_to_offset_.at(r));
}

ExecContext::ExecContext() : stack_(50u) {}

ir::BasicBlock &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(ir::CompiledFn *fn, const base::untyped_buffer &arguments)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(base::untyped_buffer::MakeFull(fn_->reg_size_.value())) {
  regs_.write(0, arguments);
}

ir::BlockIndex ExecContext::ExecuteBlock(
    const std::vector<ir::Addr> &ret_slots) {
  ir::BlockIndex result;
  ASSERT(current_block().cmds_.size() > 0u)
      << call_stack.top().current_ << "  " << *call_stack.top().fn_;
  auto cmd_iter = current_block().cmds_.begin();
  do {
    result = ExecuteCmd(*cmd_iter++, ret_slots);
  } while (result == ir::BlockIndex{-2});
  return result;
}

template <typename T>
static T LoadValue(ir::Addr addr, base::untyped_buffer const &stack) {
  switch (addr.kind) {
    case ir::Addr::Kind::Heap:
      return *ASSERT_NOT_NULL(static_cast<T *>(addr.as_heap));
    case ir::Addr::Kind::Stack: return stack.get<T>(addr.as_stack);
    case ir::Addr::Kind::ReadOnly: return ReadOnlyData.get<T>(addr.as_rodata);
  }
  UNREACHABLE(DUMP(static_cast<int>(addr.kind)));
}

template <typename T>
static void StoreValue(T val, ir::Addr addr, base::untyped_buffer *stack) {
  if constexpr (std::is_same_v<std::decay_t<T>, void *>) {
    auto start  = reinterpret_cast<uintptr_t>(stack->raw(0));
    auto end    = reinterpret_cast<uintptr_t>(stack->raw(stack->size()));
    auto result = reinterpret_cast<uintptr_t>(val);
    if (start <= result && result < end) {
      StoreValue(ir::Addr::Stack(result - start), addr, stack);
    } else {
      StoreValue(ir::Addr::Heap(val), addr, stack);
    }
    // TODO readonly data
  } else {
    switch (addr.kind) {
      case ir::Addr::Kind::Stack: stack->set(addr.as_stack, val); return;
      case ir::Addr::Kind::ReadOnly:
        ReadOnlyData.set(addr.as_rodata, val);
        return;
      case ir::Addr::Kind::Heap:
        *ASSERT_NOT_NULL(static_cast<T *>(addr.as_heap)) = val;
    }
  }
}


template <size_t N, typename... Ts>
struct RetrieveArgs;

template <size_t N, typename T, typename... Ts>
struct RetrieveArgs<N, T, Ts...> {
  template <typename OutTup>
  void operator()(base::untyped_buffer const &arguments, size_t *index,
                  OutTup *out_tup) {
    if constexpr (std::is_same_v<T, void *>) {
      // Any pointer-type alignment is fine. (TODO they're always the same,
      // right?)
      *index = ((*index - 1) | (alignof(ir::Addr) - 1)) + 1;
      auto addr = arguments.get<ir::Addr>(*index);
      void *ptr = nullptr;
      switch (addr.kind) {
        case ir::Addr::Kind::Stack: NOT_YET();
        case ir::Addr::Kind::Heap:
          ptr = static_cast<void *>(addr.as_heap);
          break;
        case ir::Addr::Kind::ReadOnly:
          ptr = static_cast<void *>(ReadOnlyData.raw(addr.as_rodata));
          break;
        default: UNREACHABLE(static_cast<int>(addr.kind));
      }
      std::get<N>(*out_tup) = ptr;
      *index += sizeof(ir::Addr);
      RetrieveArgs<N + 1, Ts...>{}(arguments, index, out_tup);
    } else {
      auto arch = core::Interpretter();
      auto t    = type::Get<T>();
      *index =
          core::FwdAlign(core::Bytes{*index}, t->alignment(arch)).value();
      std::get<N>(*out_tup) = arguments.get<T>(*index);
      *index += t->bytes(arch).value();
      RetrieveArgs<N + 1, Ts...>{}(arguments, index, out_tup);
    }
  }
};

template <size_t N>
struct RetrieveArgs<N> {
  template <typename OutTup>
  void operator()(base::untyped_buffer const &arguments, size_t *index,
                  OutTup *out_tup) {}
};

template <typename... Ts>
std::tuple<Ts...> MakeTupleArgs(base::untyped_buffer const &arguments) {
  std::tuple<Ts...> results;
  size_t index = 0;
  RetrieveArgs<0, Ts...>{}(arguments, &index, &results);
  return results;
}

// TODO Generalize this based on calling convention.
template <typename Out, typename... Ins>
void FfiCall(ir::Foreign const &f, base::untyped_buffer const &arguments,
             std::vector<ir::Addr> *ret_slots, base::untyped_buffer *stack) {
  using fn_t = Out (*)(Ins...);
  fn_t fn    = (fn_t)(f.get());

  if constexpr (std::is_same_v<Out, void>) {
    std::apply(fn, MakeTupleArgs<Ins...>(arguments));
  } else {
    StoreValue(std::apply(fn, MakeTupleArgs<Ins...>(arguments)),
               ret_slots->at(0), stack);
  }
}

void CallForeignFn(ir::Foreign const &f, base::untyped_buffer const &arguments,
                   std::vector<ir::Addr> ret_slots,
                   base::untyped_buffer *stack) {
  // TODO we can catch locally or in the same lexical scope stack if a
  // redeclaration of the same foreign symbol has a different type.
  // TODO handle failures gracefully.
  // TODO Consider caching these.
  // TODO Handle a bunch more function types in a coherent way.
  auto *fn_type = &f.type()->as<type::Function>();
  if (fn_type == type::Func({type::Int64}, {type::Int64})) {
    FfiCall<int64_t, int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Int64}, {})) {
    FfiCall<void, int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Float64}, {type::Float64})) {
    FfiCall<double, double>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Float32}, {type::Float32})) {
    FfiCall<float, float>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({}, {type::Int64})) {
    FfiCall<int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type == type::Func({type::Nat8}, {type::Int64})) {
    FfiCall<int64_t, uint8_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 &&
             fn_type->input[0]->is<type::Pointer>() &&
             fn_type->output.size() == 1 && fn_type->output[0] == type::Int64) {
    FfiCall<int64_t, void *>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 2 &&
             fn_type->input[0]->is<type::Pointer>() &&
             fn_type->input[1]->is<type::Pointer>() &&
             fn_type->output.size() == 1 &&
             fn_type->output[0]->is<type::Pointer>()) {
    FfiCall<void *, void *, void *>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 2 && fn_type->input[0] == type::Int64 &&
             fn_type->input[1]->is<type::Pointer>() &&
             fn_type->output.size() == 1 && fn_type->output[0] == type::Int64) {
    FfiCall<int64_t, int64_t, void *>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 && fn_type->input[0] == type::Int64 &&
             fn_type->output.size() == 1 &&
             fn_type->output[0]->is<type::Pointer>()) {
    FfiCall<void *, int64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 && fn_type->input[0] == type::Nat64 &&
             fn_type->output.size() == 1 &&
             fn_type->output[0]->is<type::Pointer>()) {
    FfiCall<void *, uint64_t>(f, arguments, &ret_slots, stack);
  } else if (fn_type->input.size() == 1 && fn_type->output.empty() &&
             fn_type->input[0]->is<type::Pointer>()) {
    FfiCall<void, void *>(f, arguments, &ret_slots, stack);
  } else {
    UNREACHABLE(fn_type);
  }
}

ir::BlockIndex ExecContext::ExecuteCmd(
    const ir::Cmd &cmd, const std::vector<ir::Addr> &ret_slots) {
  auto save = [&](auto val) {
    call_stack.top().regs_.set(
        call_stack.top().fn_->compiler_reg_to_offset_.at(cmd.result), val);
  };

  switch (cmd.op_code_) {
    case ir::Op::Death: UNREACHABLE(call_stack.top().fn_);
    case ir::Op::Bytes:
      save(resolve(cmd.type_arg_)->bytes(core::Interpretter()));
      break;
    case ir::Op::Align:
      save(resolve(cmd.type_arg_)->alignment(core::Interpretter()));
      break;
    case ir::Op::NotBool: save(!resolve<bool>(cmd.reg_)); break;
    case ir::Op::NotFlags: {
      save(ir::NotFlags(resolve<ir::FlagsVal>(cmd.typed_reg_.get()),
                        &cmd.typed_reg_.type()->as<type::Flags>()));
    } break;
    case ir::Op::NegInt8: save(-resolve<int8_t>(cmd.reg_)); break;
    case ir::Op::NegInt16: save(-resolve<int16_t>(cmd.reg_)); break;
    case ir::Op::NegInt32: save(-resolve<int32_t>(cmd.reg_)); break;
    case ir::Op::NegInt64: save(-resolve<int64_t>(cmd.reg_)); break;
    case ir::Op::NegFloat32: save(-resolve<double>(cmd.reg_)); break;
    case ir::Op::NegFloat64: save(-resolve<double>(cmd.reg_)); break;
#define CASE(op, ty)                                                           \
  case op: save(LoadValue<ty>(resolve<ir::Addr>(cmd.reg_), stack_)); break
      CASE(ir::Op::LoadBool, bool);
      CASE(ir::Op::LoadInt8, int8_t);
      CASE(ir::Op::LoadInt16, int16_t);
      CASE(ir::Op::LoadInt32, int32_t);
      CASE(ir::Op::LoadInt64, int64_t);
      CASE(ir::Op::LoadNat8, uint8_t);
      CASE(ir::Op::LoadNat16, uint16_t);
      CASE(ir::Op::LoadNat32, uint32_t);
      CASE(ir::Op::LoadNat64, uint64_t);
      CASE(ir::Op::LoadFloat32, float);
      CASE(ir::Op::LoadFloat64, double);
      CASE(ir::Op::LoadType, type::Type const *);
      CASE(ir::Op::LoadEnum, size_t);
      CASE(ir::Op::LoadFlags, size_t);
      CASE(ir::Op::LoadAddr, ir::Addr);
      CASE(ir::Op::LoadFunc, ir::AnyFunc);
      CASE(ir::Op::LoadInterface, type::Interface const *);
#undef CASE

#define CASE(op, member, fn)                                                   \
  case op: {                                                                   \
    save(fn(resolve(cmd.member.args_[0]), resolve(cmd.member.args_[1])));      \
  } break
      CASE(ir::Op::AddInt8, i8_args_, std::plus<int8_t>{});
      CASE(ir::Op::AddInt16, i16_args_, std::plus<int16_t>{});
      CASE(ir::Op::AddInt32, i32_args_, std::plus<int32_t>{});
      CASE(ir::Op::AddInt64, i64_args_, std::plus<int64_t>{});
      CASE(ir::Op::AddNat8, u8_args_, std::plus<uint8_t>{});
      CASE(ir::Op::AddNat16, u16_args_, std::plus<uint16_t>{});
      CASE(ir::Op::AddNat32, u32_args_, std::plus<uint32_t>{});
      CASE(ir::Op::AddNat64, u64_args_, std::plus<uint64_t>{});
      CASE(ir::Op::AddFloat32, float32_args_, std::plus<float>{});
      CASE(ir::Op::AddFloat64, float64_args_, std::plus<double>{});

      CASE(ir::Op::SubInt8, i8_args_, std::minus<int8_t>{});
      CASE(ir::Op::SubInt16, i16_args_, std::minus<int16_t>{});
      CASE(ir::Op::SubInt32, i32_args_, std::minus<int32_t>{});
      CASE(ir::Op::SubInt64, i64_args_, std::minus<int64_t>{});
      CASE(ir::Op::SubNat8, u8_args_, std::minus<uint8_t>{});
      CASE(ir::Op::SubNat16, u16_args_, std::minus<uint16_t>{});
      CASE(ir::Op::SubNat32, u32_args_, std::minus<uint32_t>{});
      CASE(ir::Op::SubNat64, u64_args_, std::minus<uint64_t>{});
      CASE(ir::Op::SubFloat32, float32_args_, std::minus<float>{});
      CASE(ir::Op::SubFloat64, float64_args_, std::minus<double>{});

      CASE(ir::Op::MulInt8, i8_args_, std::multiplies<int8_t>{});
      CASE(ir::Op::MulInt16, i16_args_, std::multiplies<int16_t>{});
      CASE(ir::Op::MulInt32, i32_args_, std::multiplies<int32_t>{});
      CASE(ir::Op::MulInt64, i64_args_, std::multiplies<int64_t>{});
      CASE(ir::Op::MulNat8, u8_args_, std::multiplies<uint8_t>{});
      CASE(ir::Op::MulNat16, u16_args_, std::multiplies<uint16_t>{});
      CASE(ir::Op::MulNat32, u32_args_, std::multiplies<uint32_t>{});
      CASE(ir::Op::MulNat64, u64_args_, std::multiplies<uint64_t>{});
      CASE(ir::Op::MulFloat32, float32_args_, std::multiplies<float>{});
      CASE(ir::Op::MulFloat64, float64_args_, std::multiplies<double>{});

      CASE(ir::Op::DivInt8, i8_args_, std::divides<int8_t>{});
      CASE(ir::Op::DivInt16, i16_args_, std::divides<int16_t>{});
      CASE(ir::Op::DivInt32, i32_args_, std::divides<int32_t>{});
      CASE(ir::Op::DivInt64, i64_args_, std::divides<int64_t>{});
      CASE(ir::Op::DivNat8, u8_args_, std::divides<uint8_t>{});
      CASE(ir::Op::DivNat16, u16_args_, std::divides<uint16_t>{});
      CASE(ir::Op::DivNat32, u32_args_, std::divides<uint32_t>{});
      CASE(ir::Op::DivNat64, u64_args_, std::divides<uint64_t>{});
      CASE(ir::Op::DivFloat32, float32_args_, std::divides<float>{});
      CASE(ir::Op::DivFloat64, float64_args_, std::divides<double>{});

      CASE(ir::Op::ModInt8, i8_args_, std::modulus<int8_t>{});
      CASE(ir::Op::ModInt16, i16_args_, std::modulus<int16_t>{});
      CASE(ir::Op::ModInt32, i32_args_, std::modulus<int32_t>{});
      CASE(ir::Op::ModInt64, i64_args_, std::modulus<int64_t>{});
      CASE(ir::Op::ModNat8, u8_args_, std::modulus<uint8_t>{});
      CASE(ir::Op::ModNat16, u16_args_, std::modulus<uint16_t>{});
      CASE(ir::Op::ModNat32, u32_args_, std::modulus<uint32_t>{});
      CASE(ir::Op::ModNat64, u64_args_, std::modulus<uint64_t>{});

      CASE(ir::Op::LtInt8, i8_args_, std::less<int8_t>{});
      CASE(ir::Op::LtInt16, i16_args_, std::less<int16_t>{});
      CASE(ir::Op::LtInt32, i32_args_, std::less<int32_t>{});
      CASE(ir::Op::LtInt64, i64_args_, std::less<int64_t>{});
      CASE(ir::Op::LtNat8, u8_args_, std::less<uint8_t>{});
      CASE(ir::Op::LtNat16, u16_args_, std::less<uint16_t>{});
      CASE(ir::Op::LtNat32, u32_args_, std::less<uint32_t>{});
      CASE(ir::Op::LtNat64, u64_args_, std::less<uint64_t>{});
      CASE(ir::Op::LtFloat32, float32_args_, std::less<float>{});
      CASE(ir::Op::LtFloat64, float64_args_, std::less<double>{});
      CASE(ir::Op::LtFlags, flags_args_, std::less<ir::FlagsVal>{});

      CASE(ir::Op::LeInt8, i8_args_, std::less_equal<int8_t>{});
      CASE(ir::Op::LeInt16, i16_args_, std::less_equal<int16_t>{});
      CASE(ir::Op::LeInt32, i32_args_, std::less_equal<int32_t>{});
      CASE(ir::Op::LeInt64, i64_args_, std::less_equal<int64_t>{});
      CASE(ir::Op::LeNat8, u8_args_, std::less_equal<uint8_t>{});
      CASE(ir::Op::LeNat16, u16_args_, std::less_equal<uint16_t>{});
      CASE(ir::Op::LeNat32, u32_args_, std::less_equal<uint32_t>{});
      CASE(ir::Op::LeNat64, u64_args_, std::less_equal<uint64_t>{});
      CASE(ir::Op::LeFloat32, float32_args_, std::less_equal<float>{});
      CASE(ir::Op::LeFloat64, float64_args_, std::less_equal<double>{});
      CASE(ir::Op::LeFlags, flags_args_, std::less_equal<ir::FlagsVal>{});

      CASE(ir::Op::GtInt8, i8_args_, std::greater<int8_t>{});
      CASE(ir::Op::GtInt16, i16_args_, std::greater<int16_t>{});
      CASE(ir::Op::GtInt32, i32_args_, std::greater<int32_t>{});
      CASE(ir::Op::GtInt64, i64_args_, std::greater<int64_t>{});
      CASE(ir::Op::GtNat8, u8_args_, std::greater<uint8_t>{});
      CASE(ir::Op::GtNat16, u16_args_, std::greater<uint16_t>{});
      CASE(ir::Op::GtNat32, u32_args_, std::greater<uint32_t>{});
      CASE(ir::Op::GtNat64, u64_args_, std::greater<uint64_t>{});
      CASE(ir::Op::GtFloat32, float32_args_, std::greater<float>{});
      CASE(ir::Op::GtFloat64, float64_args_, std::greater<double>{});
      CASE(ir::Op::GtFlags, flags_args_, std::greater<ir::FlagsVal>{});

      CASE(ir::Op::GeInt8, i8_args_, std::greater_equal<int8_t>{});
      CASE(ir::Op::GeInt16, i16_args_, std::greater_equal<int16_t>{});
      CASE(ir::Op::GeInt32, i32_args_, std::greater_equal<int32_t>{});
      CASE(ir::Op::GeInt64, i64_args_, std::greater_equal<int64_t>{});
      CASE(ir::Op::GeNat8, u8_args_, std::greater_equal<uint8_t>{});
      CASE(ir::Op::GeNat16, u16_args_, std::greater_equal<uint16_t>{});
      CASE(ir::Op::GeNat32, u32_args_, std::greater_equal<uint32_t>{});
      CASE(ir::Op::GeNat64, u64_args_, std::greater_equal<uint64_t>{});
      CASE(ir::Op::GeFloat32, float32_args_, std::greater_equal<float>{});
      CASE(ir::Op::GeFloat64, float64_args_, std::greater_equal<double>{});
      CASE(ir::Op::GeFlags, flags_args_, std::greater_equal<ir::FlagsVal>{});

      CASE(ir::Op::EqBool, bool_args_, std::equal_to<bool>{});
      CASE(ir::Op::EqInt8, i8_args_, std::equal_to<int8_t>{});
      CASE(ir::Op::EqInt16, i16_args_, std::equal_to<int16_t>{});
      CASE(ir::Op::EqInt32, i32_args_, std::equal_to<int32_t>{});
      CASE(ir::Op::EqInt64, i64_args_, std::equal_to<int64_t>{});
      CASE(ir::Op::EqNat8, u8_args_, std::equal_to<uint8_t>{});
      CASE(ir::Op::EqNat16, u16_args_, std::equal_to<uint16_t>{});
      CASE(ir::Op::EqNat32, u32_args_, std::equal_to<uint32_t>{});
      CASE(ir::Op::EqNat64, u64_args_, std::equal_to<uint64_t>{});
      CASE(ir::Op::EqFloat32, float32_args_, std::equal_to<float>{});
      CASE(ir::Op::EqFloat64, float64_args_, std::equal_to<double>{});
      CASE(ir::Op::EqEnum, enum_args_, std::equal_to<ir::EnumVal>{});
      CASE(ir::Op::EqFlags, flags_args_, std::equal_to<ir::FlagsVal>{});
      CASE(ir::Op::EqType, type_args_, std::equal_to<type::Type const *>{});
      CASE(ir::Op::EqAddr, addr_args_, std::equal_to<ir::Addr>{});

      CASE(ir::Op::XorBool, bool_args_, std::not_equal_to<bool>{});
      CASE(ir::Op::NeInt8, i8_args_, std::not_equal_to<int8_t>{});
      CASE(ir::Op::NeInt16, i16_args_, std::not_equal_to<int16_t>{});
      CASE(ir::Op::NeInt32, i32_args_, std::not_equal_to<int32_t>{});
      CASE(ir::Op::NeInt64, i64_args_, std::not_equal_to<int64_t>{});
      CASE(ir::Op::NeNat8, u8_args_, std::not_equal_to<uint8_t>{});
      CASE(ir::Op::NeNat16, u16_args_, std::not_equal_to<uint16_t>{});
      CASE(ir::Op::NeNat32, u32_args_, std::not_equal_to<uint32_t>{});
      CASE(ir::Op::NeNat64, u64_args_, std::not_equal_to<uint64_t>{});
      CASE(ir::Op::NeFloat32, float32_args_, std::not_equal_to<float>{});
      CASE(ir::Op::NeFloat64, float64_args_, std::not_equal_to<double>{});
      CASE(ir::Op::NeEnum, enum_args_, std::not_equal_to<ir::EnumVal>{});
      CASE(ir::Op::NeFlags, flags_args_, std::not_equal_to<ir::FlagsVal>{});
      CASE(ir::Op::NeType, type_args_, std::not_equal_to<type::Type const *>{});
      CASE(ir::Op::NeAddr, addr_args_, std::not_equal_to<ir::Addr>{});

      CASE(ir::Op::XorFlags, flags_args_, std::bit_xor{});
      CASE(ir::Op::OrFlags, flags_args_, std::bit_or{});
      CASE(ir::Op::AndFlags, flags_args_, std::bit_and{});
      CASE(ir::Op::Arrow, type_args_, [](auto &&lhs, auto &&rhs) {
        return type::Func({std::forward<std::decay_t<decltype(lhs)>>(lhs)},
                          {std::forward<std::decay_t<decltype(rhs)>>(rhs)});
      });
#undef CASE
    case ir::Op::Move: {
      auto *t = cmd.special2_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr) * 2);
      call_buf.append(resolve(cmd.special2_.regs_[0]));
      call_buf.append(resolve(cmd.special2_.regs_[1]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->move_assign_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->move_assign_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->move_assign_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::Copy: {
      auto *t = cmd.special2_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr) * 2);
      call_buf.append(resolve(cmd.special2_.regs_[0]));
      call_buf.append(resolve(cmd.special2_.regs_[1]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->copy_assign_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->copy_assign_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->copy_assign_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::Init: {
      auto *t = cmd.special1_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr));
      call_buf.append(resolve(cmd.special1_.regs_[0]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
       f = s->init_func_;
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->init_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->init_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::Destroy: {
      auto *t = cmd.special1_.type_;
      std::vector<ir::Addr> return_slots;
      base::untyped_buffer call_buf(sizeof(ir::Addr));
      call_buf.append(resolve(cmd.special1_.regs_[0]));

      ir::AnyFunc f;
      if (auto *s = t->if_as<type::Struct>()) {
        f = s->destroy_func_.get();
      } else if (auto *tup = t->if_as<type::Tuple>()) {
        f = tup->destroy_func_.get();
      } else if (auto *a = t->if_as<type::Array>()) {
        f = a->destroy_func_.get();
      } else {
        NOT_YET();
      }

      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::CreateStruct:
      save(new type::Struct(cmd.create_struct_.scope_,
                            cmd.create_struct_.scope_->module(),
                            cmd.create_struct_.parent_));
      break;
    case ir::Op::CreateStructField: {
      auto *struct_to_modify = ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.create_struct_field_.struct_));
      struct_to_modify->add_field(resolve(cmd.create_struct_field_.type_));
    } break;
    case ir::Op::SetStructFieldName: {
      ASSERT_NOT_NULL(
          resolve<type::Struct *>(cmd.set_struct_field_name_.struct_))
          ->set_last_name(cmd.set_struct_field_name_.name_);
    } break;
    case ir::Op::AddHashtagToField: {
      ASSERT_NOT_NULL(resolve<type::Struct *>(cmd.add_hashtag_.struct_))
          ->add_hashtag_to_last_field(cmd.add_hashtag_.hashtag_);
    } break;
    case ir::Op::AddHashtagToStruct: {
      ASSERT_NOT_NULL(resolve<type::Struct *>(cmd.add_hashtag_.struct_))
          ->add_hashtag(cmd.add_hashtag_.hashtag_);
    } break;
    case ir::Op::FinalizeStruct: {

      auto *s = resolve<type::Struct *>(cmd.reg_);
      auto &cache = s->mod_->generic_struct_cache_[s->parent_];

      // TODO this is horrendous. Find a better way to populate the back_ map.
      std::vector<type::Type const *> const *input_vals = nullptr;
      for (auto const &[vals, t] : cache.fwd_) {
        if (t != s) { continue; }
        input_vals = &vals;
        break;
      }
      if (input_vals != nullptr) {
        bool inserted = cache.back_.emplace(s, input_vals).second;
        ASSERT(inserted == true);
      }

      s->init_func_ = s->mod_->AddFunc(
          type::Func({type::Ptr(s)}, {}),
          core::FnParams(core::Param{"", type::Typed<ast::Expression const *>{
                                             nullptr, type::Ptr(s)}}));
      CURRENT_FUNC(s->init_func_.func()) {
        // TODO this context gets no constants which is not what we want.
        // Probably need to store the correct bound constants pointer in the
        // struct type when we create create it initially.
        Context ctx(s->mod_);

        ir::BasicBlock::Current = ir::CompiledFn::Current->entry();
        auto var                = ir::Reg::Arg(0);
        size_t i                = 0;
        for (auto const &field : s->parent_->fields_) {
          auto ir_field = ir::Field(var, s, i);
          visitor::EmitIr visitor;
          if (field.init_val) {
            field.init_val->EmitCopyInit(&visitor, ir_field, &ctx);
          } else {
            s->fields_.at(i).type->EmitDefaultInit(&visitor, ir_field.get(),
                                                   &ctx);
          }
          ++i;
        }

        ir::ReturnJump();
      }

      save(s);

      // TODO set backwards map.
    } break;
    case ir::Op::DebugIr: {
      std::stringstream ss;
      ss << *call_stack.top().fn_;
      base::Log() << ss.str();
    } break;
    case ir::Op::Alloca: {
      auto arch = core::Interpretter();

      save(ir::Addr::Stack(core::FwdAlign(core::Bytes{stack_.size()},
                                            cmd.type_->alignment(arch))
                               .value()));
      // TODO simplify: just say how big you want the stack to be after this.
      stack_.append_bytes(cmd.type_->bytes(arch).value(),
                          cmd.type_->alignment(arch).value());

    } break;
    case ir::Op::Ptr:
      save(type::Ptr(resolve<type::Type const *>(cmd.reg_)));
      break;
    case ir::Op::BufPtr:
      save(type::BufPtr(resolve<type::Type const *>(cmd.reg_)));
      break;
    case ir::Op::Array: {
      save(type::Arr(resolve(cmd.array_.len_), resolve(cmd.array_.type_)));
    } break;
    case ir::Op::VariantType: save(resolve(cmd.addr_arg_)); break;
    case ir::Op::VariantValue: {
      auto arch      = core::Interpretter();
      auto bytes     = type::Type_->bytes(arch);
      auto bytes_fwd = core::FwdAlign(bytes, type::Type_->alignment(arch));
      auto addr      = resolve(cmd.addr_arg_);
      switch (addr.kind) {
        case ir::Addr::Kind::Stack:
          addr.as_stack += bytes_fwd.value();
          save(addr);
          break;
        case ir::Addr::Kind::Heap:
          addr.as_heap = static_cast<void *>(static_cast<char *>(addr.as_heap) +
                                             bytes_fwd.value());
          save(addr);
          break;
        case ir::Addr::Kind::ReadOnly:
          addr.as_rodata += bytes_fwd.value();
          save(addr);
          break;
      }
    } break;
    case ir::Op::PtrIncr: {
      auto addr = resolve(cmd.ptr_incr_.ptr_);
      auto bytes_fwd =
          type::Array(resolve(cmd.ptr_incr_.incr_), cmd.ptr_incr_.pointee_type_)
              .bytes(core::Interpretter());
      switch (addr.kind) {
        case ir::Addr::Kind::Stack: addr.as_stack += bytes_fwd.value(); break;
        case ir::Addr::Kind::Heap:
          addr.as_heap = static_cast<char *>(addr.as_heap) + bytes_fwd.value();
          break;
        case ir::Addr::Kind::ReadOnly:
          addr.as_rodata += bytes_fwd.value();
          break;
      }
      save(addr);
    } break;
    case ir::Op::Field: {
      auto addr   = resolve(cmd.field_.ptr_);
      auto offset = core::Bytes{0};
      auto arch   = core::Interpretter();
      if (cmd.field_.type_->is<type::Struct>()) {
        offset =
            cmd.field_.type_->as<type::Struct>().offset(cmd.field_.num_, arch);
      } else if (cmd.field_.type_->is<type::Tuple>()) {
        offset =
            cmd.field_.type_->as<type::Tuple>().offset(cmd.field_.num_, arch);
      } else {
        UNREACHABLE();
      }
      if (addr.kind == ir::Addr::Kind::Stack) {
        addr.as_stack += offset.value();
      } else {
        addr.as_heap =
            static_cast<void *>(static_cast<char *>(addr.as_heap) + offset.value());
      }
      save(addr);
    } break;
    case ir::Op::PrintBool:
      std::cerr << (resolve(cmd.bool_arg_) ? "true" : "false");
      break;
    case ir::Op::PrintInt8:
      std::cerr << static_cast<int32_t>(resolve(cmd.i8_arg_));
      break;
    case ir::Op::PrintInt16: std::cerr << resolve(cmd.i16_arg_); break;
    case ir::Op::PrintInt32: std::cerr << resolve(cmd.i32_arg_); break;
    case ir::Op::PrintInt64: std::cerr << resolve(cmd.i64_arg_); break;
    case ir::Op::PrintNat8:
      std::cerr << static_cast<int32_t>(resolve(cmd.u8_arg_));
      break;
    case ir::Op::PrintNat16: std::cerr << resolve(cmd.u16_arg_); break;
    case ir::Op::PrintNat32: std::cerr << resolve(cmd.u32_arg_); break;
    case ir::Op::PrintNat64: std::cerr << resolve(cmd.u64_arg_); break;
    case ir::Op::PrintFloat32: std::cerr << resolve(cmd.float32_arg_); break;
    case ir::Op::PrintFloat64: std::cerr << resolve(cmd.float64_arg_); break;
    case ir::Op::PrintType:
      std::cerr << resolve(cmd.type_arg_)->to_string();
      break;
    case ir::Op::PrintEnum: {
      auto numeric_value = resolve(cmd.print_enum_.arg_).value;
      auto iter = cmd.print_enum_.enum_type_->members_.find(numeric_value);
      if (iter == cmd.print_enum_.enum_type_->members_.end()) {
        std::cerr << numeric_value;
      } else {
        std::cerr << iter->second;
      }
    } break;
    case ir::Op::PrintFlags: {
      size_t val = resolve(cmd.print_flags_.arg_).value;
      std::vector<std::string> vals;

      auto const &members = cmd.print_flags_.flags_type_->members_;

      while (val != 0) {
        size_t mask = (val & ((~val) + 1));
        val -= mask;
        auto iter = members.find(mask);
        if (iter == members.end()) {
          vals.emplace_back(std::to_string(mask));
        } else {
          vals.emplace_back(iter->second);
        }
      }

      if (vals.empty()) {
        std::cerr << "(empty)";
      } else {
        auto iter = vals.begin();
        std::cerr << *iter++;
        while (iter != vals.end()) { std::cerr << " | " << *iter++; }
      }
    } break;
    case ir::Op::PrintAddr:
      std::cerr << resolve(cmd.addr_arg_).to_string();
      break;
    case ir::Op::PrintByteView:
      std::cerr << resolve(cmd.byte_view_arg_);
      break;
    case ir::Op::PrintInterface: std::cerr << resolve(cmd.intf_arg_); break;
    case ir::Op::Call: {
      // NOTE: This is a hack using heap address slots to represent registers
      // since they are both void* and are used identically in the
      // interpretter.
      std::vector<ir::Addr> return_slots;
      if (cmd.call_.outs_ != nullptr) {
        return_slots.reserve(cmd.call_.outs_->size());
        for (size_t i = 0; i < cmd.call_.outs_->size(); ++i) {
          if (cmd.call_.outs_->is_loc_[i]) {
            return_slots.push_back(resolve<ir::Addr>(cmd.call_.outs_->regs_[i]));
          } else {
            auto addr = ir::Addr::Heap(call_stack.top().regs_.raw(
                call_stack.top().fn_->compiler_reg_to_offset_.at(
                    cmd.call_.outs_->regs_[i])));
            return_slots.push_back(addr);
          }
        }
      }

      auto call_buf = cmd.call_.arguments_->PrepareCallBuffer(
          call_stack.top().fn_->compiler_reg_to_offset_,
          call_stack.top().regs_);
      ir::AnyFunc f = resolve(cmd.call_.fn_);

      // TODO you need to be able to determine how many args there are
      if (f.is_fn()) {
        Execute(f.func(), call_buf, return_slots, this);
      } else {
        CallForeignFn(f.foreign(), call_buf, return_slots, &stack_);
      }
    } break;
    case ir::Op::NewOpaqueType: save(new type::Opaque(cmd.mod_)); break;
    case ir::Op::LoadSymbol: {
      void *sym = [&]() -> void * {
        // TODO: this is a hack for now untill we figure out why we can load
        // stderr as a symbol but not write to it.
        if (cmd.load_sym_.name_ == "stderr") { return stderr; }
        if (cmd.load_sym_.name_ == "stdout") { return stdout; }
        return ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, cmd.load_sym_.name_.data()));
      }();
      if (cmd.load_sym_.type_->is<type::Function>()) {
        save(ir::AnyFunc{ir::Foreign(sym, cmd.load_sym_.type_)});
      } else if (cmd.load_sym_.type_->is<type::Pointer>()) {
        save(ir::Addr::Heap(sym));
      } else {
        NOT_YET(cmd.load_sym_.type_);
      }
    } break;
    case ir::Op::CreateEnum: save(new type::IncompleteEnum(cmd.mod_)); break;
    case ir::Op::AddEnumerator: {
      resolve<type::IncompleteEnum *>(cmd.add_enumerator_.enum_)
          ->add(cmd.add_enumerator_.name_);
    } break;
    case ir::Op::SetEnumerator: {
      resolve<type::IncompleteEnum *>(cmd.set_enumerator_.enum_)
          ->set_last_value(resolve(cmd.set_enumerator_.val_));
    } break;
    case ir::Op::FinalizeEnum: {
      auto *e = resolve<type::IncompleteEnum *>(cmd.reg_);
      save(std::move(*e).finalize());
      delete e;
    } break;
    case ir::Op::CreateFlags: save(new type::IncompleteFlags(cmd.mod_)); break;
    case ir::Op::AddFlag: {
      resolve<type::IncompleteFlags *>(cmd.add_enumerator_.enum_)
          ->add(cmd.add_enumerator_.name_);
    } break;
    case ir::Op::SetFlag: {
      resolve<type::IncompleteFlags *>(cmd.add_enumerator_.enum_)
          ->set_last_value(resolve(cmd.set_enumerator_.val_));
    } break;
    case ir::Op::FinalizeFlags: {
      auto *f = resolve<type::IncompleteFlags *>(cmd.reg_);
      save(std::move(*f).finalize());
      delete f;
    } break;
    case ir::Op::CreateInterface: {
      save(new type::Interface(cmd.scope_, cmd.scope_->module()));
    } break;
    case ir::Op::FinalizeInterface: {
      save(resolve<type::Interface *>(cmd.reg_));
    } break;
    case ir::Op::CreateTuple: {
      save(new type::Tuple(std::vector<type::Type const *>{}));
    } break;
    case ir::Op::AppendToTuple: {
      auto *tuple_to_modify =
          ASSERT_NOT_NULL(resolve<type::Tuple *>(cmd.store_type_.addr_.reg_));
      tuple_to_modify->entries_.push_back(resolve(cmd.store_type_.val_));
    } break;
    case ir::Op::FinalizeTuple:
      save(resolve<type::Tuple *>(cmd.reg_)->finalize());
      break;
    case ir::Op::CreateVariant: {
      save(new type::Variant(std::vector<type::Type const *>{}));
    } break;
    case ir::Op::AppendToVariant: {
      auto *variant_to_modify =
          ASSERT_NOT_NULL(resolve<type::Variant *>(cmd.store_type_.addr_.reg_));
      variant_to_modify->variants_.push_back(resolve(cmd.store_type_.val_));
    } break;
    case ir::Op::FinalizeVariant:
      save(resolve<type::Variant *>(cmd.reg_)->finalize());
      break;
    case ir::Op::CastToInt8: {
      save(static_cast<int8_t>(resolve<int8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToNat8: {
      save(static_cast<uint8_t>(resolve<int8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToInt16: {
      save(static_cast<uint16_t>(resolve<int8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToNat16: {
      save(static_cast<uint16_t>(resolve<uint8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToInt32: {
      save(static_cast<int32_t>(resolve<int8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToNat32: {
      save(static_cast<uint32_t>(resolve<uint8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToInt64: {
      save(static_cast<int64_t>(resolve<int8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToNat64: {
      save(static_cast<uint64_t>(resolve<uint8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToFloat32: {
      save(static_cast<float>(resolve<int8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToFloat64: {
      save(static_cast<double>(resolve<int8_t>(cmd.typed_reg_.get())));
    } break;
    case ir::Op::CastToEnum: save(ir::EnumVal(resolve<int32_t>(cmd.reg_))); break;
    case ir::Op::CastToFlags: save(ir::FlagsVal(resolve<int32_t>(cmd.reg_))); break;
    case ir::Op::CastPtr: save(resolve<ir::Addr>(cmd.typed_reg_.get())); break;
    case ir::Op::GetRet: save(ret_slots.at(cmd.get_ret_)); break;
    case ir::Op::SetRetBool:
      StoreValue(resolve(cmd.set_ret_bool_.val_),
                 ret_slots.at(cmd.set_ret_bool_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt8:
      StoreValue(resolve(cmd.set_ret_i8_.val_),
                 ret_slots.at(cmd.set_ret_i8_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt16:
      StoreValue(resolve(cmd.set_ret_i16_.val_),
                 ret_slots.at(cmd.set_ret_i16_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt32:
      StoreValue(resolve(cmd.set_ret_i32_.val_),
                 ret_slots.at(cmd.set_ret_i32_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInt64:
      StoreValue(resolve(cmd.set_ret_i64_.val_),
                 ret_slots.at(cmd.set_ret_i64_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat8:
      StoreValue(resolve(cmd.set_ret_u8_.val_),
                 ret_slots.at(cmd.set_ret_u8_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat16:
      StoreValue(resolve(cmd.set_ret_u16_.val_),
                 ret_slots.at(cmd.set_ret_u16_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat32:
      StoreValue(resolve(cmd.set_ret_u32_.val_),
                 ret_slots.at(cmd.set_ret_u32_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetNat64:
      StoreValue(resolve(cmd.set_ret_u64_.val_),
                 ret_slots.at(cmd.set_ret_u64_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFloat32:
      StoreValue(resolve(cmd.set_ret_float32_.val_),
                 ret_slots.at(cmd.set_ret_float32_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFloat64:
      StoreValue(resolve(cmd.set_ret_float64_.val_),
                 ret_slots.at(cmd.set_ret_float64_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetType:
      StoreValue(resolve(cmd.set_ret_type_.val_),
                 ret_slots.at(cmd.set_ret_type_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetByteView:
      StoreValue(resolve(cmd.set_ret_byte_view_.val_),
                 ret_slots.at(cmd.set_ret_byte_view_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetAddr:
      StoreValue(resolve(cmd.set_ret_addr_.val_),
                 ret_slots.at(cmd.set_ret_addr_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetEnum:
      StoreValue(resolve(cmd.set_ret_enum_.val_),
                 ret_slots.at(cmd.set_ret_enum_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFlags:
      StoreValue(resolve(cmd.set_ret_flags_.val_),
                 ret_slots.at(cmd.set_ret_flags_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetFunc:
      StoreValue(resolve(cmd.set_ret_func_.val_),
                 ret_slots.at(cmd.set_ret_func_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetScope:
      StoreValue(resolve(cmd.set_ret_scope_.val_),
                 ret_slots.at(cmd.set_ret_scope_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetGeneric:
      StoreValue(resolve(cmd.set_ret_generic_.val_),
                 ret_slots.at(cmd.set_ret_generic_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetModule:
      StoreValue(resolve(cmd.set_ret_module_.val_),
                 ret_slots.at(cmd.set_ret_module_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetBlock:
      StoreValue(resolve(cmd.set_ret_block_.val_),
                 ret_slots.at(cmd.set_ret_block_.ret_num_), &stack_);
      break;
    case ir::Op::SetRetInterface:
      StoreValue(resolve(cmd.set_ret_intf_.val_),
                 ret_slots.at(cmd.set_ret_intf_.ret_num_), &stack_);
      break;
    case ir::Op::StoreBool:
      StoreValue(resolve(cmd.store_bool_.val_),
                 resolve<ir::Addr>(cmd.store_bool_.addr_), &stack_);
      break;
    case ir::Op::StoreInt8:
      StoreValue(resolve(cmd.store_i8_.val_),
                 resolve<ir::Addr>(cmd.store_i8_.addr_), &stack_);
      break;
    case ir::Op::StoreInt16:
      StoreValue(resolve(cmd.store_i16_.val_),
                 resolve<ir::Addr>(cmd.store_i16_.addr_), &stack_);
      break;
    case ir::Op::StoreInt32:
      StoreValue(resolve(cmd.store_i32_.val_),
                 resolve<ir::Addr>(cmd.store_i32_.addr_), &stack_);
      break;
    case ir::Op::StoreInt64:
      StoreValue(resolve(cmd.store_i64_.val_),
                 resolve<ir::Addr>(cmd.store_i64_.addr_), &stack_);
      break;
    case ir::Op::StoreNat8:
      StoreValue(resolve(cmd.store_u8_.val_),
                 resolve<ir::Addr>(cmd.store_u8_.addr_), &stack_);
      break;
    case ir::Op::StoreNat16:
      StoreValue(resolve(cmd.store_u16_.val_),
                 resolve<ir::Addr>(cmd.store_u16_.addr_), &stack_);
      break;
    case ir::Op::StoreNat32:
      StoreValue(resolve(cmd.store_u32_.val_),
                 resolve<ir::Addr>(cmd.store_u32_.addr_), &stack_);
      break;
    case ir::Op::StoreNat64:
      StoreValue(resolve(cmd.store_u64_.val_),
                 resolve<ir::Addr>(cmd.store_u64_.addr_), &stack_);
      break;
    case ir::Op::StoreFloat32:
      StoreValue(resolve(cmd.store_float32_.val_),
                 resolve<ir::Addr>(cmd.store_float32_.addr_), &stack_);
      break;
    case ir::Op::StoreFloat64:
      StoreValue(resolve(cmd.store_float64_.val_),
                 resolve<ir::Addr>(cmd.store_float64_.addr_), &stack_);
      break;
    case ir::Op::StoreType:
      StoreValue(resolve(cmd.store_type_.val_),
                 resolve<ir::Addr>(cmd.store_type_.addr_), &stack_);
      break;
    case ir::Op::StoreEnum:
      StoreValue(resolve(cmd.store_enum_.val_),
                 resolve<ir::Addr>(cmd.store_enum_.addr_), &stack_);
      break;
    case ir::Op::StoreFunc:
      StoreValue(resolve(cmd.store_func_.val_),
                 resolve<ir::Addr>(cmd.store_func_.addr_), &stack_);
      break;
    case ir::Op::StoreFlags:
      StoreValue(resolve(cmd.store_flags_.val_),
                 resolve<ir::Addr>(cmd.store_flags_.addr_), &stack_);
      break;
    case ir::Op::StoreAddr:
      StoreValue(resolve(cmd.store_addr_.val_),
                 resolve<ir::Addr>(cmd.store_addr_.addr_), &stack_);
      break;
    case ir::Op::PhiBool:
      save(resolve(cmd.phi_bool_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiInt8:
      save(resolve(cmd.phi_i8_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiInt16:
      save(resolve(cmd.phi_i16_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiInt32:
      save(resolve(cmd.phi_i32_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiInt64:
      save(resolve(cmd.phi_i64_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat8:
      save(resolve(cmd.phi_u8_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat16:
      save(resolve(cmd.phi_u16_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat32:
      save(resolve(cmd.phi_u32_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiNat64:
      save(resolve(cmd.phi_u64_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFloat32:
      save(resolve(cmd.phi_float32_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFloat64:
      save(resolve(cmd.phi_float64_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiType:
      save(resolve(cmd.phi_type_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiAddr:
      save(resolve(cmd.phi_addr_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiBlock:
      save(resolve(cmd.phi_block_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiEnum:
      save(resolve(cmd.phi_enum_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFlags:
      save(resolve(cmd.phi_flags_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::PhiFunc:
      save(resolve(cmd.phi_func_->map_.at(call_stack.top().prev_)));
      break;
    case ir::Op::ArgumentCache: {
      // TODO currently cache is dependetn on all args but requires that they be
      // types.
      std::vector<type::Type const*> cached_vals;
      cached_vals.reserve(cmd.sl_->args_.size());
      for (uint64_t i = 0; i < cmd.sl_->args_.size(); ++i) {
        cached_vals.push_back(resolve<type::Type const *>(ir::Reg{i}));
      }
      auto &cache = cmd.sl_->mod_->generic_struct_cache_[cmd.sl_];
      auto iter = cache.fwd_.try_emplace(std::move(cached_vals), nullptr).first;
      type::Type const **cache_slot = &iter->second;
      // Backwards direction set in FinalizeStruct
      save(ir::Addr::Heap(cache_slot));
    } break;
    case ir::Op::CreateContext: save(new Context(cmd.mod_)); break;
    case ir::Op::AddBoundConstant: {
      NOT_YET();
    } break;
    case ir::Op::DestroyContext: delete resolve<Context *>(cmd.reg_); break;
    case ir::Op::VerifyType: {
      visitor::VerifyType visitor;
      cmd.ast_.node_->VerifyType(&visitor, resolve<Context *>(cmd.ast_.ctx_));
    } break;
    case ir::Op::EvaluateAsType: {
      // TODO, you don't have a parent context... that could be problematic.
      save(EvaluateAs<type::Type const *>(
          type::Typed(&cmd.ast_.node_->as<ast::Expression const>(), type::Type_),
          resolve<Context *>(cmd.ast_.ctx_)));
    } break;
    case ir::Op::CondJump:
      return cmd.cond_jump_.blocks_[resolve<bool>(cmd.cond_jump_.cond_)];
    case ir::Op::UncondJump: return cmd.block_index_;
    case ir::Op::ReturnJump: return ir::BlockIndex{-1};
    case ir::Op::JumpPlaceholder: UNREACHABLE(call_stack.top().fn_);
    case ir::Op::BlockSeqJump: NOT_YET(); break;
    case ir::Op::CreateScopeDef:
      // TODO consider the implications of leaking this. I'm not convinced there
      // exist scenarios where we're leaking and could clean it up. I.e., every
      // time we create one it's because we expect it to live forever. I suppose
      // we could ref-count the results of functions and delete these if they're
      // unused?
      save(new ir::ScopeDef(cmd.create_scope_def_.sl_));
      break;
    case ir::Op::AddScopeDefInit:
      resolve<ir::ScopeDef *>(cmd.add_scope_def_init_.reg_)
          ->AddInit(resolve(cmd.add_scope_def_init_.f_));
      break;
    case ir::Op::AddScopeDefDone:
      resolve<ir::ScopeDef *>(cmd.add_scope_def_done_.reg_)
          ->AddDone(resolve(cmd.add_scope_def_done_.f_));
      break;
    case ir::Op::AddBlockDef:
      // TODO implement me
      break;
  }

  return ir::BlockIndex{-2};
}
}  // namespace backend
