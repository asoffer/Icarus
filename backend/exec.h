#ifndef ICARUS_BACKEND_EXEC_H
#define ICARUS_BACKEND_EXEC_H
#include <cstddef>
#include <stack>

#include "base/untyped_buffer.h"
#include "ir/basic_block.h"
#include "ir/cmd.h"
#include "ir/val.h"

namespace ir {
struct Func;
}  // namespace ir

namespace backend {
struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(ir::Func *fn, const base::untyped_buffer &arguments);

    void MoveTo(ir::BlockIndex block_index) {
      ASSERT(block_index.value >= 0);
      prev_    = current_;
      current_ = block_index;
    }

    ir::Func *fn_ = nullptr;
    ir::BlockIndex current_;
    ir::BlockIndex prev_;

    base::untyped_buffer regs_;
  };

  ir::BasicBlock &current_block();

  std::stack<Frame> call_stack;

  ir::BlockIndex ExecuteBlock(const base::vector<ir::Addr> &ret_slots);
  ir::BlockIndex ExecuteCmd(const ir::Cmd &cmd,
                            const base::vector<ir::Addr> &ret_slots);

  template <typename T>
  T resolve(ir::Register val) const;

  template <typename T>
  T resolve(ir::RegisterOr<T> val) const {
    return val.is_reg_ ? resolve<T>(val.reg_) : val.val_;
  }

  base::untyped_buffer stack_;
};

void Execute(ir::Func *fn, const base::untyped_buffer &arguments,
             const base::vector<ir::Addr> &ret_slots,
             backend::ExecContext *ctx);
}  // namespace backend
#endif  // ICARUS_BACKEND_EXEC_H
