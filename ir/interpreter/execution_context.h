#ifndef ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H
#define ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H

#include <cstdlib>
#include <vector>

#include "absl/cleanup/cleanup.h"
#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/byte_code/reader.h"
#include "ir/instruction/core.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/stack_frame.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope.h"
#include "module/shared_context.h"

namespace interpreter {
namespace internal_execution {

// clang-format off
template <typename T>
concept HasResolveMemberFunction = requires(T t) {
  { (void)t.Resolve() } -> std::same_as<void>;
};
// clang-format on

struct StackFrameIterator {
  StackFrameIterator(base::untyped_buffer::const_iterator byte_code_iter,
                     StackFrame &frame)
      : byte_code_iter_(byte_code_iter),
        begin_(byte_code_iter),
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
  explicit ExecutionContext(module::SharedContext const *context)
      : shared_context_(*ASSERT_NOT_NULL(context)) {}

  StackFrame const &current_frame() const {
    return *ASSERT_NOT_NULL(current_frame_);
  }
  StackFrame &current_frame() { return *ASSERT_NOT_NULL(current_frame_); }

  // Stores the given `value` into the given location expressed by `addr`. The
  // value must be register-sized.
  template <typename T>
  void Store(ir::addr_t addr, T const &value) {
    *ASSERT_NOT_NULL(reinterpret_cast<T *>(addr)) = value;
  }

  // Loads `num_bytes` bytes starting at `addr` and stores the result into
  // `result`.
  void Load(ir::Reg result, ir::addr_t addr, core::Bytes num_bytes);

  // Reads the value stored in `r` assuming it has type `T`. Behavior is
  // undefined if the value stored in the register is of another type.
  template <typename T>
  T resolve(ir::Reg r) const {
    return current_frame().get<T>(r);
  }

  // If `val` is already holding a `T`, returns that value. Otherwise, loads the
  // value stored in the register `val.reg()` assuming it has type `T`. Behavior
  // is undefined if the value stored in the register is of another type.
  template <typename T>
  T resolve(ir::RegOr<T> val) const {
    return val.resolve([&](ir::Reg r) { return resolve<T>(r); });
  }

  template <typename InstSet>
  void Execute(ir::Scope, StackFrame &frame) {
    StackFrame *old = std::exchange(current_frame_, &frame);
    absl::Cleanup c = [&] { current_frame_ = old; };
    ExecuteBlocks<InstSet>();
  }

  template <typename InstSet>
  void Execute(ir::Fn fn, StackFrame &frame) {
    switch (fn.kind()) {
      case ir::Fn::Kind::Native: CallNative<InstSet>(frame); break;
      case ir::Fn::Kind::Foreign: CallForeignFunction(fn, frame); break;
    }
  }

  Stack const &stack() const & { return stack_; }
  Stack &stack() & { return stack_; }

 private:
  template <typename InstSet>
  void CallNative(StackFrame &frame) {
    StackFrame *old = std::exchange(current_frame_, &frame);
    absl::Cleanup c = [&] { current_frame_ = old; };
    ExecuteBlocks<InstSet>();
  }

  void CallForeignFunction(ir::Fn f, StackFrame &frame);

  template <typename InstSet>
  void ExecuteBlocks() {
    internal_execution::StackFrameIterator frame_iter(
        current_frame_->byte_code_begin(), *current_frame_);
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
          type::Type t          = iter.read<type::Type>();
          core::Bytes num_bytes = t.bytes(interpreter::kArchitecture);
          ir::addr_t addr = resolve(iter.read<ir::RegOr<ir::addr_t>>().get());
          ir::Reg result_reg = iter.read<ir::Reg>();
          Load(result_reg, addr, num_bytes);
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
        auto inst = ir::ByteCodeReader::DeserializeTo<Inst>(*iter);
        LOG("CallInstruction", "%s", inst);
        ir::Fn f                      = ctx.resolve(inst.func());
        type::Function const *fn_type = f.type();
        LOG("CallInstruction", "%s: %s", f, fn_type->to_string());

        StackFrame frame =
            (f.kind() == ir::Fn::Kind::Native)
                ? StackFrame(&f.native().byte_code(), ctx.stack())
                : StackFrame({.num_parameters = fn_type->params().size(),
                              .num_outputs    = fn_type->return_types().size()},
                             ctx.stack());

        for (size_t i = 0; i < inst.arguments().num_entries(); ++i) {
          ir::PartialResultRef argument = inst.arguments()[i];
          if (argument.is_register()) {
            ir::Reg reg = argument.get<ir::Reg>();
            frame.set_raw(ir::Reg::Parameter(i), ctx.current_frame().raw(reg),
                          StackFrame::register_value_size);
            LOG("CallInstruction", "  %s: [%s]", ir::Reg::Parameter(i), reg);
          } else {
            type::Type t = fn_type->params()[i].value.type();
            core::Bytes size =
                t.is_big() ? interpreter::kArchitecture.pointer().bytes()
                           : t.bytes(interpreter::kArchitecture);
            frame.set_raw(ir::Reg::Parameter(i), argument.raw().data(), size.value());

            LOG("CallInstruction", "  %s: [%s]", ir::Reg::Parameter(i),
                argument.raw());
          }
        }

        for (size_t i = 0; i < inst.outputs().size(); ++i) {
          ir::Reg reg         = inst.outputs()[i];
          type::Type t        = fn_type->return_types()[i];
          ir::addr_t out_addr = t.is_big() ? ctx.resolve<ir::addr_t>(reg)
                                           : ctx.current_frame().raw(reg);
          LOG("CallInstruction", "  %s: [%p]", ir::Reg::Output(i), out_addr);
          frame.set(ir::Reg::Output(i), out_addr);
        }

        ctx.Execute<InstSet>(f, frame);

      } else if constexpr (
          base::meta<Inst>.template is_a<ir::PhiInstruction>()) {
        uint16_t num   = iter->read<uint16_t>();
        uint64_t index = frame_iter.IndexMatchingPrevious(*iter, num);

        using type = typename Inst::type;

        alignas(type) char result_buffer[sizeof(type)];
        type *result = reinterpret_cast<type *>(result_buffer);
        for (uint16_t i = 0; i < num; ++i) {
          if (i == index) {
            new (result) type(ctx.resolve(iter->read<ir::RegOr<type>>().get()));
          } else {
            iter->read<ir::RegOr<type>>();
          }
        }

        ctx.current_frame().set(iter->read<ir::Reg>(), *result);
      } else if constexpr (
          base::meta<Inst>.template is_a<ir::SetReturnInstruction>()) {
        using type          = typename Inst::type;
        auto inst           = ir::ByteCodeReader::DeserializeTo<Inst>(*iter);
        ir::addr_t ret_slot = ctx.resolve<ir::addr_t>(ir::Reg::Output(inst.index));
        type value          = ctx.resolve(inst.value);
        *ASSERT_NOT_NULL(reinterpret_cast<type *>(ret_slot)) = value;
      } else if constexpr (internal_execution::HasResolveMemberFunction<Inst>) {
        Inst inst = ir::ByteCodeReader::DeserializeTo<Inst>(*iter);
        std::apply([&](auto &... fields) { (ctx.ResolveField(fields), ...); },
                   inst.field_refs());

        ctx.current_frame().set(inst.result, inst.Resolve());

      } else if constexpr (base::meta<decltype(std::declval<Inst>().Apply(
                               ctx))> == base::meta<void>) {
        ir::ByteCodeReader::DeserializeTo<Inst>(*iter).Apply(ctx);
      } else {
        StackFrame frame =
            ir::ByteCodeReader::DeserializeTo<Inst>(*iter).Apply(ctx);
        ctx.CallNative<InstSet>(frame);
      }
    };
  }

  template <typename InstSet, typename... Insts>
  static constexpr std::array<exec_t, sizeof...(Insts)> MakeExecuteFunctions(
      base::type_list<Insts...>) {
    return {GetInstruction<InstSet, Insts>()...};
  }

  module::SharedContext const&shared_context_;
  Stack stack_;
  StackFrame *current_frame_ = nullptr;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H
