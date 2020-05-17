#ifndef ICARUS_INTERPRETTER_EXECUTE_H
#define ICARUS_INTERPRETTER_EXECUTE_H

#include <vector>

#include "absl/types/span.h"
#include "base/untyped_buffer.h"
#include "interpretter/stack_frame.h"
#include "ir/value/addr.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace interpretter {

struct ExecutionContext {
  ExecutionContext(std::ostream &os = std::cout) : os_(os) {}
  void ExecuteBlocks(absl::Span<ir::Addr const> ret_slots);

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

  std::ostream &os_;
  StackFrame *current_frame_ = nullptr;
  base::untyped_buffer stack_;
};

void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots, ExecutionContext *ctx);

}  // namespace interpretter

#endif  // ICARUS_INTERPRETTER_EXECUTE_H
