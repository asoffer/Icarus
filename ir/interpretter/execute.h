#ifndef ICARUS_IR_INTERPRETTER_EXECUTE_H
#define ICARUS_IR_INTERPRETTER_EXECUTE_H

#include <cstdlib>
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
// of an `ir::Fn`. This includes references to stack frames, the state of all
// registers, etc.
struct ExecutionContext {
  StackFrame const &current_frame() const {
    return *ASSERT_NOT_NULL(current_frame_);
  }
  StackFrame &current_frame() { return *ASSERT_NOT_NULL(current_frame_); }

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

  // TODO: Replace with something simpler.
  template <typename T>
  T ReadAndResolve(bool is_reg, base::untyped_buffer::const_iterator *iter) {
    if (is_reg) {
      ir::Reg r = iter->read<ir::Reg>();
      return this->resolve<T>(r);
    } else {
      return iter->read<T>();
    }
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

  // TODO: Make these private.
  base::untyped_buffer stack_;

 private:
  StackFrame *current_frame_ = nullptr;
};

}  // namespace interpretter

#endif  // ICARUS_IR_INTERPRETTER_EXECUTE_H
