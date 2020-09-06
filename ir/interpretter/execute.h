#ifndef ICARUS_IR_INTERPRETTER_EXECUTE_H
#define ICARUS_IR_INTERPRETTER_EXECUTE_H

#include <vector>

#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "ir/interpretter/architecture.h"
#include "ir/interpretter/foreign.h"
#include "ir/interpretter/stack_frame.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace interpretter {

// An ExecutionContext holds all the required state needed during the execution
// of a `Fn`.
struct ExecutionContext {
  StackFrame const *current_frame() const { return current_frame_; }
  StackFrame *current_frame() { return current_frame_; }

  // Copies `length` bytes stored in `reg` to `dst`.
  void MemCpyRegisterBytes(void *dst, ir::Reg reg, size_t length);

  template <typename T>
  T resolve(ir::Reg r) const {
    return current_frame()->regs_.get<T>(r);
  }

  // TODO determine if this is actually used and if not, remove the #include
  // "ir/value/reg_or.h".
  template <typename T>
  T resolve(ir::RegOr<T> val) const {
    return val.resolve([&](ir::Reg r) { return resolve<T>(r); });
  }

  template <typename T>
  inline T ReadAndResolve(bool is_reg,
                          base::untyped_buffer::const_iterator *iter) {
    if (is_reg) {
      ir::Reg r = iter->read<ir::Reg>();
      return this->resolve<T>(r);
    } else {
      return iter->read<T>();
    }
  }

  StackFrame *current_frame_ = nullptr;
  base::untyped_buffer stack_;
};

// TODO: fix dependency/layering here.
template <typename InstSet>
void CallFn(ir::NativeFn fn, StackFrame *frame,
            absl::Span<ir::Addr const> ret_slots,
            interpretter::ExecutionContext *ctx);

inline void CallFn(ir::BuiltinFn fn, base::untyped_buffer_view arguments,
                   absl::Span<ir::Addr const> ret_slots, ExecutionContext *) {
  switch (fn.which()) {
    case ir::BuiltinFn::Which::Alignment: {
      type::Type const *type = arguments.get<type::Type const *>(0);
      *static_cast<uint64_t *>(ASSERT_NOT_NULL(ret_slots[0].heap())) =
          type->alignment(kArchitecture).value();
    } break;
    case ir::BuiltinFn::Which::Bytes: {
      type::Type const *type = arguments.get<type::Type const *>(0);
      *static_cast<uint64_t *>(ASSERT_NOT_NULL(ret_slots[0].heap())) =
          type->bytes(kArchitecture).value();
    } break;
    default: NOT_YET();
  }

  // size_t i = 0;
  // for (auto const &result : fn.Call(arguments)) {
  //   ASSERT(ret_slot.kind() == ir::Addr::Kind::Heap);
  //   *ASSERT_NOT_NULL(static_cast<type *>(ret_slot.heap())) = val;
  // }
}



// TODO rename the `arguments` parameter. It actually should be arguments and
// space for registers.
template <typename InstSet>
void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx) {
  switch (fn.kind()) {
    case ir::Fn::Kind::Native: {
      StackFrame frame(fn.native(), std::move(arguments), &ctx->stack_);
      CallFn<InstSet>(fn.native(), &frame, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Builtin: {
      CallFn(fn.builtin(), arguments, ret_slots, ctx);
    } break;
    case ir::Fn::Kind::Foreign: {
      CallFn(fn.foreign(), arguments, ret_slots, &ctx->stack_);
    } break;
  }
}

}  // namespace interpretter

#endif  // ICARUS_IR_INTERPRETTER_EXECUTE_H
