#ifndef ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H
#define ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H

#include <cstdlib>
#include <vector>

#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/interpreter/stack_frame.h"
#include "ir/read_only_data.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"

namespace interpreter {

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
  StackFrame MakeStackFrame(ir::NativeFn fn, Args &&... args) {
    return StackFrame(fn, std::forward<Args>(args)..., &stack_);
  }

  // `RestoreFrameToken` is returned by calls to `push()` so that previous stack
  // frame can be restored when the frame token is destroyed.
  struct RestoreFrameToken {
    RestoreFrameToken(ExecutionContext *ctx, StackFrame *old_frame)
        : ctx_(ctx), old_frame_(old_frame) {}
    ~RestoreFrameToken() { ctx_->current_frame_ = old_frame_; }

   private:
    ExecutionContext *ctx_;
    StackFrame *old_frame_;
  };
  RestoreFrameToken PushFrame(StackFrame *frame) {
    return RestoreFrameToken(this, std::exchange(current_frame_, frame));
  }

  base::untyped_buffer_view stack() const { return stack_; }

 private:
  base::untyped_buffer stack_;
  StackFrame *current_frame_ = nullptr;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EXECUTION_CONETXT_H
