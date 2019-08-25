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
#include "ir/basic_block.h"
#include "ir/cmd/jumps.h"
#include "ir/compiled_fn.h"
#include "misc/module.h"
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

void CallForeignFn(ir::Foreign const &f, base::untyped_buffer const &arguments,
                   std::vector<ir::Addr> const &ret_slots,
                   base::untyped_buffer *stack);

void Execute(ir::AnyFunc fn, base::untyped_buffer const &arguments,
             std::vector<ir::Addr> const &ret_slots, ExecContext *ctx) {
  if (fn.is_fn()) {
    Execute(fn.func(), arguments, ret_slots, ctx);
  } else {
    CallForeignFn(fn.foreign(), arguments, ret_slots, &ctx->stack_);
  }
}

void Execute(ir::CompiledFn *fn, const base::untyped_buffer &arguments,
             const std::vector<ir::Addr> &ret_slots, ExecContext *ctx) {
  // TODO: Understand why and how work-items may not be complete and add an
  // explanation here. I'm quite confident this is really possible with the
  // generics model I have, but I can't quite articulate exactly why it only
  // happens for generics and nothing else.
  if (fn->work_item && *fn->work_item) { (std::move(*fn->work_item))(); }

  // TODO what about bound constants?
  ctx->call_stack.emplace(fn, arguments, ctx);

  // TODO log an error if you're asked to execute a function that had an
  // error.

  auto arch   = core::Interpretter();
  auto offset = core::Bytes{0};
  for (auto *t : fn->type_->output) {
    offset = core::FwdAlign(offset, t->alignment(arch)) + t->bytes(arch);
  }
  base::untyped_buffer ret_buffer(offset.value());

  while (true) {
    auto block_index = ctx->ExecuteBlock(ret_slots);
    if (block_index.is_default()) {
      ctx->call_stack.pop();
      return;
    } else {
      ctx->call_stack.top().MoveTo(block_index);
    }
  }
}

ExecContext::ExecContext() : stack_(50u) {}

ir::BasicBlock &ExecContext::current_block() {
  return call_stack.top().fn_->block(call_stack.top().current_);
}

ExecContext::Frame::Frame(ir::CompiledFn *fn,
                          const base::untyped_buffer &arguments,
                          ExecContext *ctx)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(base::untyped_buffer::MakeFull(fn_->reg_size_.value())) {
  regs_.write(0, arguments);

  auto arch = core::Interpretter();
  fn->allocs().for_each([&](type::Type const *t, ir::Reg r) {
    DEBUG_LOG("allocs")
    ("Allocating type = ", t->to_string(), ", reg = ", r,
     ", offset = ", fn_->compiler_reg_to_offset_.at(r));
    regs_.set(fn_->compiler_reg_to_offset_.at(r),
              ir::Addr::Stack(core::FwdAlign(core::Bytes{ctx->stack_.size()},
                                             t->alignment(arch))
                                  .value()));

    ctx->stack_.append_bytes(t->bytes(arch).value(),
                             t->alignment(arch).value());
  });
}

ir::BlockIndex ExecContext::ExecuteBlock(
    const std::vector<ir::Addr> &ret_slots) {
  return current_block().cmd_buffer_.Execute(ret_slots, this);
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
        case ir::Addr::Kind::Stack: NOT_YET(addr.as_stack);
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
             std::vector<ir::Addr> const *ret_slots,
             base::untyped_buffer *stack) {
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
                   std::vector<ir::Addr> const& ret_slots,
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
    UNREACHABLE(fn_type->to_string());
  }
}

}  // namespace backend
