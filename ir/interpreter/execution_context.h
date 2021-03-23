#ifndef ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H
#define ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H

#include <cstdlib>
#include <vector>

#include "absl/cleanup/cleanup.h"
#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/instruction/core.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/foreign.h"
#include "ir/interpreter/stack_frame.h"
#include "ir/read_only_data.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"

namespace interpreter {
namespace internal_execution {

// clang-format off
template <typename T>
concept HasResolveMemberFunction = requires(T t) {
  // TODO: Check that the type is supported in ir::Value::supported_types
  { (void)t.Resolve() } -> std::same_as<void>;
};
// clang-format on

struct StackFrameIterator {
  StackFrameIterator(ir::NativeFn fn, StackFrame &frame)
      : byte_code_iter_(fn->byte_code().begin()),
        begin_(fn->byte_code().begin()),
        prev_index_(0),
        current_index_(0) {}

  void MoveTo(uintptr_t offset) {
    prev_index_     = std::exchange(current_index_, offset);
    byte_code_iter_ = begin_;
    byte_code_iter_.skip(offset);
  }

  // Reads exactly `num` uintptr_t's starting at `iter` and returns the index of
  // the one whose value was the previous index. Behavior is undefined if zero
  // or more than one such value matches.
  uintptr_t IndexMatchingPrevious(base::untyped_buffer::const_iterator &iter,
                                  size_t num) {
    uint64_t index = std::numeric_limits<uint64_t>::max();
    for (size_t i = 0; i < num; ++i) {
      if (prev_index_ == iter.read<uintptr_t>()) {
        ASSERT(index == std::numeric_limits<uint64_t>::max());
        index = i;
      }
    }
    ASSERT(index != std::numeric_limits<uint64_t>::max());
    return index;
  }

  base::untyped_buffer::const_iterator &byte_code_iterator() {
    return byte_code_iter_;
  }

 private:
  base::untyped_buffer::const_iterator byte_code_iter_;
  base::untyped_buffer::const_iterator begin_;
  uintptr_t prev_index_    = 0;
  uintptr_t current_index_ = 0;
};

}  // namespace internal_execution

// An ExecutionContext holds all the required state needed during the execution
// of an `ir::Fn`. This includes references to stack frames, the state of all
// registers, etc.
struct ExecutionContext {
  StackFrame const &current_frame() const {
    return *ASSERT_NOT_NULL(current_frame_);
  }
  StackFrame &current_frame() { return *ASSERT_NOT_NULL(current_frame_); }

  // Stores the given `value` into the given location expressed by `addr`. The
  // value must be register-sized.
  template <typename T>
  void Store(ir::Addr addr, T const &value) {
    switch (addr.kind()) {
      case ir::Addr::Kind::Stack: stack_.set(addr.stack(), value); break;
      case ir::Addr::Kind::ReadOnly:
        NOT_YET(
            "Storing into read-only data seems suspect. Is it just for "
            "initialization?");
        break;
      case ir::Addr::Kind::Heap:
        *ASSERT_NOT_NULL(static_cast<T *>(addr.heap())) = value;
    }
  }

  // Loads `num_bytes` bytes starting at `addr` and stores the result into
  // `result`.
  void Load(ir::Reg result, ir::Addr addr, core::Bytes num_bytes) {
    switch (addr.kind()) {
      case ir::Addr::Kind::Stack: {
        current_frame().regs_.set_raw(result, stack_.raw(addr.stack()),
                                      num_bytes.value());
      } break;
      case ir::Addr::Kind::ReadOnly: {
        auto handle = ir::ReadOnlyData.lock();
        current_frame().regs_.set_raw(result, handle->raw(addr.rodata()),
                                      num_bytes.value());
      } break;
      case ir::Addr::Kind::Heap: {
        current_frame().regs_.set_raw(result, addr.heap(), num_bytes.value());
      } break;
    }
  }

  // TODO: Deprecate. This doesn't feel particularly great API.
  // Copies `length` bytes stored in `reg` to `dst`.
  void MemCpyRegisterBytes(void *dst, ir::Reg reg, size_t length) {
    std::memcpy(dst, current_frame().regs_.raw(reg), length);
  }

  // Reads the value stored in `r` assuming it has type `T`. Behavior is
  // undefined if the value stored in the register is of another type.
  template <typename T>
  T resolve(ir::Reg r) const {
    return current_frame().regs_.get<T>(r);
  }

  // If `val` is already holding a `T`, returns that value. Otherwise, loads the
  // value stored in the register `val.reg()` assuming it has type `T`. Behavior
  // is undefined if the value stored in the register is of another type.
  template <typename T>
  T resolve(ir::RegOr<T> val) const {
    return val.resolve([&](ir::Reg r) { return resolve<T>(r); });
  }

  template <typename... Args>
  StackFrame MakeStackFrame(ir::Fn fn, Args &&... args) {
    return StackFrame(fn, std::forward<Args>(args)..., &stack_);
  }

  template <typename InstSet>
  void Execute(ir::Fn fn, StackFrame &frame) {
    switch (fn.kind()) {
      case ir::Fn::Kind::Native: {
        CallFn<InstSet>(fn.native(), frame);
      } break;
      case ir::Fn::Kind::Builtin: {
        CallFn(fn.builtin(), frame);
      } break;
      case ir::Fn::Kind::Foreign: {
        CallFn(fn.foreign(), frame, stack());
      } break;
    }
  }

  base::untyped_buffer_view stack() const { return stack_; }

 private:
  template <typename InstSet>
  void CallFn(ir::NativeFn fn, StackFrame &frame) {
    StackFrame *old = std::exchange(current_frame_, &frame);
    absl::Cleanup c = [&] { current_frame_ = old; };
    ExecuteBlocks<InstSet>();
  }

  static void CallFn(ir::BuiltinFn fn, StackFrame &frame);

  static void CallFn(ir::ForeignFn f, StackFrame &frame,
                     base::untyped_buffer_view arguments);

  template <typename InstSet>
  void ExecuteBlocks() {
    internal_execution::StackFrameIterator frame_iter(
        current_frame_->fn().native(), *current_frame_);
    auto &iter = frame_iter.byte_code_iterator();
    while (true) {
      ir::cmd_index_t cmd_index = iter.read<ir::cmd_index_t>();
      switch (cmd_index) {
        case ir::internal::kReturnInstruction: return;
        case ir::internal::kUncondJumpInstruction: {
          uintptr_t offset = iter.read<uintptr_t>();
          frame_iter.MoveTo(offset);
        } break;
        case ir::internal::kCondJumpInstruction: {
          ir::Reg r             = iter.read<ir::Reg>();
          uintptr_t true_block  = iter.read<uintptr_t>();
          uintptr_t false_block = iter.read<uintptr_t>();
          uintptr_t offset      = resolve<bool>(r) ? true_block : false_block;
          frame_iter.MoveTo(offset);
        } break;
        case ir::LoadInstruction::kIndex: {
          uint16_t num_bytes = iter.read<uint16_t>();
          ir::Addr addr      = resolve(iter.read<ir::RegOr<ir::Addr>>().get());
          auto result_reg    = iter.read<ir::Reg>().get();
          Load(result_reg, addr, core::Bytes(num_bytes));
        } break;

        default: {
          static constexpr std::array ExecutionArray =
              MakeExecuteFunctions<InstSet>(typename InstSet::instructions_t{});
          ExecutionArray[cmd_index](*this, frame_iter);
        }
      }
    }
  }

  template <typename T>
  void ResolveField(T &field) {
    if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
      field = resolve(field);
    } else if constexpr (base::meta<T>.template is_a<std::vector>() or
                         base::meta<T>.template is_a<absl::flat_hash_set>() or
                         base::meta<T>.template is_a<absl::flat_hash_map>()) {
      for (auto &f : field) { ResolveField(f); }
    } else if constexpr (base::meta<T>.template is_a<std::pair>()) {
      ResolveField(field.second);
      ResolveField(field.first);
    }
  }

  using exec_t = void (*)(interpreter::ExecutionContext &,
                          internal_execution::StackFrameIterator &);

  template <typename InstSet, typename Inst>
  static constexpr exec_t GetInstruction() {
    return [](interpreter::ExecutionContext &ctx,
              internal_execution::StackFrameIterator &frame_iter) {
      auto *iter = &frame_iter.byte_code_iterator();

      if constexpr (base::meta<Inst> == base::meta<ir::CallInstruction>) {
        ir::Fn f = ctx.resolve(iter->read<ir::RegOr<ir::Fn>>().get());

        type::Function const *fn_type = f.type();
        LOG("CallInstruction", "%s: %s", f, fn_type->to_string());

        StackFrame frame = ctx.MakeStackFrame(f);

        // TODO: you probably want interpreter::Arguments or something.
        size_t num_inputs = fn_type->params().size();
        for (size_t i = 0; i < num_inputs; ++i) {
          if (iter->read<bool>()) {
            ir::Reg reg = iter->read<ir::Reg>();
            frame.regs_.set_raw(ir::Reg::Arg(i),
                                ctx.current_frame().regs_.raw(reg),
                                ir::Value::value_size_v);
            LOG("CallInstruction", "  %s: [%s]", ir::Reg::Arg(i), reg);
          } else {
            type::Type t = fn_type->params()[i].value.type();
            frame.regs_.set_raw(ir::Reg::Arg(i), iter->raw(),
                                ir::Value::value_size_v);
            LOG("CallInstruction", "  %s: ???]", ir::Reg::Arg(i));
            iter->skip((t.is_big()
                            ? interpreter::kArchitecture.pointer().bytes()
                            : t.bytes(interpreter::kArchitecture))
                           .value());
          }
        }

        uint16_t num_rets = iter->read<uint16_t>();

        for (uint16_t i = 0; i < num_rets; ++i) {
          ir::Reg reg  = iter->read<ir::Reg>();
          type::Type t = fn_type->output()[i];
          ir::Addr out_addr =
              t.is_big() ? ctx.resolve<ir::Addr>(reg)
                         : ir::Addr::Heap(ctx.current_frame().regs_.raw(reg));
          frame.regs_.set(ir::Reg::Out(i), out_addr);
        }

        ctx.Execute<InstSet>(f, frame);

      } else if constexpr (
          base::meta<Inst>.template is_a<ir::PhiInstruction>()) {
        uint16_t num   = iter->read<uint16_t>();
        uint64_t index = frame_iter.IndexMatchingPrevious(*iter, num);

        using type = typename Inst::type;

        type result;
        for (uint16_t i = 0; i < num; ++i) {
          if (i == index) {
            result = ctx.resolve(iter->read<ir::RegOr<type>>().get());
          } else {
            iter->read<ir::RegOr<type>>();
          }
        }

        ctx.current_frame().regs_.set(iter->read<ir::Reg>(), result);
      } else if constexpr (
          base::meta<Inst>.template is_a<ir::SetReturnInstruction>()) {
        using type        = typename Inst::type;
        uint16_t n        = iter->read<uint16_t>();
        ir::Addr ret_slot = ctx.resolve<ir::Addr>(ir::Reg::Out(n));
        type val          = ctx.resolve(iter->read<ir::RegOr<type>>().get());
        ASSERT(ret_slot.kind() == ir::Addr::Kind::Heap);
        *ASSERT_NOT_NULL(static_cast<type *>(ret_slot.heap())) = val;
      } else if constexpr (internal_execution::HasResolveMemberFunction<Inst>) {
        auto inst = Inst::ReadFromByteCode(iter);
        std::apply([&](auto &... fields) { (ctx.ResolveField(fields), ...); },
                   inst.field_refs());

        ctx.current_frame().regs_.set(inst.result, inst.Resolve());

      } else if constexpr (base::meta<decltype(std::declval<Inst>().Apply(
                               ctx))> == base::meta<void>) {
        Inst::ReadFromByteCode(iter).Apply(ctx);
      } else {
        StackFrame frame = Inst::ReadFromByteCode(iter).Apply(ctx);
        ctx.CallFn<InstSet>(frame.fn().native(), frame);
      }
    };
  }

  template <typename InstSet, typename... Insts>
  static constexpr std::array<exec_t, sizeof...(Insts)> MakeExecuteFunctions(
      base::type_list<Insts...>) {
    return {GetInstruction<InstSet, Insts>()...};
  }

  base::untyped_buffer stack_;
  StackFrame *current_frame_ = nullptr;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H
